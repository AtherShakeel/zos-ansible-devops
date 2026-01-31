pipeline {
  agent { label 'linux' }

  options {
    buildDiscarder(logRotator(numToKeepStr: '20', artifactNumToKeepStr: '10'))
    timeout(time: 30, unit: 'MINUTES')
    ansiColor('xterm')
    timestamps()

    // Guardrail: prevent two builds from deploying at the same time
    disableConcurrentBuilds()
  }

  parameters {
    // Keep your existing knobs
    booleanParam(name: 'FORCE_PRIME', defaultValue: false, description: 'Force VSAM priming even if MASTER already exists')
    booleanParam(name: 'DEBUG', defaultValue: false, description: 'Debug mode (shows more Ansible detail)')

    // Optional escape hatch: override env if you really need to (rare)
    choice(name: 'ENV_OVERRIDE', choices: ['', 'dev', 'int', 'prod'], description: 'Optional: override inferred environment (blank = auto)')
  }

  environment {
    ANSIBLE_STDOUT_CALLBACK = "yaml"
    ANSIBLE_FORCE_COLOR = "true"
    PYTHONUNBUFFERED = "1"
    ANSIBLE_HOST_KEY_CHECKING = "False"

    VENV_DIR = "${WORKSPACE}/.venv"
    ZOS_SSH_CRED = "zos-ssh-key"

    // KEEP EXACTLY your Phase-1 artifact behavior
    ARTIFACTS_DIR = "${WORKSPACE}/artifacts/build-${BUILD_NUMBER}"
  }

  stages {

    stage('Init (Branch/PR → Env)') {
      steps {
        script {
          // Multibranch sets BRANCH_NAME / CHANGE_ID automatically.
          // Non-multibranch may not; this is still safe.
          def branch = env.BRANCH_NAME ?: "unknown"
          def isPR = (env.CHANGE_ID != null && env.CHANGE_ID.trim() != "")

          // Infer env from branch
          def inferredEnv = "dev"
          if (branch == "develop") inferredEnv = "int"
          if (branch == "main" || branch == "master") inferredEnv = "prod"

          // Optional override
          def finalEnv = inferredEnv
          if (params.ENV_OVERRIDE?.trim()) {
            finalEnv = params.ENV_OVERRIDE.trim()
          }

          // Safe mode: PR builds should never deploy to z/OS by default
          def safeMode = isPR

          env.DEPLOY_ENV = finalEnv
          env.SAFE_MODE = safeMode.toString()
          env.GIT_BRANCH_EFFECTIVE = branch
          env.IS_PR = isPR.toString()

          echo """
          Branch: ${branch}
          PR Build: ${isPR}
          DEPLOY_ENV: ${env.DEPLOY_ENV}
          SAFE_MODE: ${env.SAFE_MODE}
          """
        }
      }
    }

    stage('Clean Workspace') {
      steps {
        deleteDir()
      }
    }

    stage('Checkout') {
      steps {
        // Multibranch: this checks out the current branch/PR automatically
        checkout scm
      }
    }

    stage('Preflight (Agent)') {
      steps {
        sh '''
          set -e
          echo "User: $(whoami)"
          echo "PWD: $(pwd)"
          echo "WORKSPACE: $WORKSPACE"
          which git
          git --version
          ls -ld .
          ls -ld "$WORKSPACE"
        '''
      }
    }

    stage('Setup Ansible Environment') {
      steps {
        sh '''
          set -e
          python3 -V
          python3 -m venv "$VENV_DIR"
          . "$VENV_DIR/bin/activate"
          python -m pip install --upgrade pip

          # Pin later if you want reproducibility; for now keep it simple
          pip install ansible ansible-lint

          ansible --version
          ansible-playbook --version
          ansible-lint --version || true
        '''
      }
    }

    stage('Install z/OS Collections') {
      steps {
        sh '''
          set -e
          . "$VENV_DIR/bin/activate"
          cd ansible
          ansible-galaxy collection install -r requirements.yml
        '''
      }
    }

    stage('Lint / Syntax Check (safe)') {
      steps {
        sh '''
          set -e
          . "$VENV_DIR/bin/activate"
          cd ansible

          # Basic playbook syntax check
          ansible-playbook --syntax-check playbooks/deploy.yml

          # Lint (non-fatal if you’re still iterating)
          ansible-lint -q playbooks/deploy.yml || true
        '''
      }
    }

    stage('Approval Gate (prod)') {
      when {
        expression { return env.DEPLOY_ENV == 'prod' && env.SAFE_MODE != 'true' }
      }
      steps {
        script {
          // Manual approval before touching prod-like environment
          input message: "Approve PROD deployment for build #${env.BUILD_NUMBER} (branch: ${env.GIT_BRANCH_EFFECTIVE})?",
                ok: "Deploy to PROD"
        }
      }
    }

    stage('Execute Deployment') {
      when {
        expression { return env.SAFE_MODE != 'true' }
      }
      steps {
        echo "Deploying to z/OS via Ansible (env=${env.DEPLOY_ENV})..."
        sshagent(credentials: [env.ZOS_SSH_CRED]) {
          sh '''
            set -e
            . "$VENV_DIR/bin/activate"
            cd ansible

            INV="inventories/${DEPLOY_ENV}/hosts.ini"
            VARS="group_vars/${DEPLOY_ENV}.yml"

            if [ ! -f "$INV" ]; then
              echo "ERROR: Missing inventory: $INV"
              exit 2
            fi
            if [ ! -f "$VARS" ]; then
              echo "ERROR: Missing vars file: $VARS"
              exit 2
            fi

            EXTRA_VARS=""
            if [ "${FORCE_PRIME}" = "true" ]; then
              EXTRA_VARS="$EXTRA_VARS -e force_prime=true"
            fi
            if [ "${DEBUG}" = "true" ]; then
              EXTRA_VARS="$EXTRA_VARS -e debug=true"
            fi

            mkdir -p "${ARTIFACTS_DIR}"
            echo "Artifacts will be written to: ${ARTIFACTS_DIR}"

            ansible-playbook -i "$INV" playbooks/deploy.yml \
              -e "@${VARS}" \
              -e "env=${DEPLOY_ENV}" \
              -e "artifacts_dir=${ARTIFACTS_DIR}" \
              $EXTRA_VARS
          '''
        }
      }
    }

    stage('PR Safe Mode Notice') {
      when {
        expression { return env.SAFE_MODE == 'true' }
      }
      steps {
        echo "SAFE_MODE=true (PR build). Skipping z/OS deployment. Ran lint/syntax checks only."
      }
    }
  }

  post {
    always {
      echo 'Normalizing spool artifacts (convert literal \\n to real newlines)...'
      sh '''
        set -e
        if [ -d "${ARTIFACTS_DIR}" ]; then
          find "${ARTIFACTS_DIR}" -type f -name "*.spool.txt" -print0 2>/dev/null | \
            xargs -0 -r perl -0777 -pe 's/\\\\n/\\n/g' -i
        fi
      '''

      echo 'Archiving artifacts (spools/output)...'
      archiveArtifacts artifacts: "artifacts/build-${BUILD_NUMBER}/**", allowEmptyArchive: true, fingerprint: true
    }
    failure {
      echo 'FAILURE: Check artifacts (spool files) in the Build Artifacts section.'
    }
  }
}
