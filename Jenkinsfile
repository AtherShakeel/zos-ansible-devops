pipeline {
  agent { label 'linux' }

  tools {
    git 'Default'
  }
  
  options {
    skipDefaultCheckout(true)
    buildDiscarder(logRotator(numToKeepStr: '20', artifactNumToKeepStr: '10'))
    timeout(time: 30, unit: 'MINUTES')
    ansiColor('xterm')
    timestamps()
    disableConcurrentBuilds()
  }

  parameters {
    booleanParam(name: 'USE_DOCKER', defaultValue: false, description: 'Run Ansible inside Docker image (zos-ansible-ci)')
    booleanParam(name: 'REBUILD_DOCKER', defaultValue: false, description: 'Force rebuild Docker CI image even if it exists on this agent')
    booleanParam(name: 'FORCE_PRIME', defaultValue: false, description: 'Force VSAM priming even if MASTER already exists')
    booleanParam(name: 'DEBUG', defaultValue: false, description: 'Debug mode (shows more Ansible detail)')
    choice(name: 'ENV_OVERRIDE', choices: ['', 'dev', 'int', 'prod'], description: 'Optional: override inferred environment (blank = auto)')
  }

  environment {
    ANSIBLE_STDOUT_CALLBACK = "yaml"
    ANSIBLE_FORCE_COLOR = "true"
    PYTHONUNBUFFERED = "1"
    ANSIBLE_HOST_KEY_CHECKING = "False"

    VENV_DIR = "${WORKSPACE}/.venv"
    ZOS_SSH_CRED = "zos-ssh-key"

    // Phase-1 artifact behavior
    ARTIFACTS_DIR = "${WORKSPACE}/artifacts/build-${BUILD_NUMBER}"
  }

  stages {

    stage('Stage 1- Init (Branch â†’ Env)') {
      steps {
        script {
          def branch = (env.BRANCH_NAME ?: "unknown").trim()
          def isPR = (env.CHANGE_ID != null && env.CHANGE_ID.trim() != "")

          def inferredEnv = "dev"
          if (branch == "int") inferredEnv = "int"
          if (branch == "dev") inferredEnv = "dev"
          if (branch == "main" || branch == "master" || branch == "prod") inferredEnv = "prod"

          def finalEnv = inferredEnv
          if (params.ENV_OVERRIDE?.trim()) {
            finalEnv = params.ENV_OVERRIDE.trim()
          }

          def isProdBranch = (branch == "main" || branch == "master" || branch == "prod")
          if (finalEnv == "prod" && !isProdBranch) {
            error("Refusing PROD deploy from branch '${branch}'. Only 'main'/'master'/'prod' can deploy to prod.")
          }

          def allowedDeployBranches = ['dev', 'int', 'main', 'master', 'prod', 'phase2-multibranch']
          def safeMode = isPR || !allowedDeployBranches.contains(branch)

          env.DEPLOY_ENV = finalEnv
          env.SAFE_MODE = safeMode.toString()
          env.GIT_BRANCH_EFFECTIVE = branch
          env.IS_PR = isPR.toString()

          currentBuild.displayName = "#${env.BUILD_NUMBER} ${env.DEPLOY_ENV} ${branch}"

          echo """
          Branch: ${branch}
          PR Build: ${isPR}
          DEPLOY_ENV: ${env.DEPLOY_ENV}
          SAFE_MODE: ${env.SAFE_MODE}
          ENV_OVERRIDE: ${params.ENV_OVERRIDE}
          USE_DOCKER: ${params.USE_DOCKER}
          """
        }
      }
    }

    stage('Stage 2- PR Guardrail') {
      when { expression { return env.IS_PR == 'true' } }
      steps {
        echo """
        PR build detected (CHANGE_ID=${env.CHANGE_ID}).
        Running checks only â€” deployment is blocked by design.
        Target branch: ${env.CHANGE_TARGET}
        Source branch: ${env.CHANGE_BRANCH}
        """
      }
    }

    stage('Stage 3- Clean Workspace') {
      steps {
          sh 'rm -rf "$WORKSPACE/artifacts" || true'
          deleteDir()
      }
    }

    stage('Stage 4- Checkout') {
      steps {
        checkout scm
      }
    }

    stage('Stage 5- Preflight (Agent)') {
      steps {
        sh '''
          set -e
          echo "User: $(whoami)"
          echo "PWD: $(pwd)"
          echo "WORKSPACE: $WORKSPACE"
          which git
          git --version
        '''
      }
    }

    stage('Stage 6- Setup Ansible Environment (host venv)') {
      when { expression { return !params.USE_DOCKER } }
      steps {
        sh '''
          set -e
          python3 -V
          python3 -m venv "$VENV_DIR"
          . "$VENV_DIR/bin/activate"
          python -m pip install --upgrade pip
          pip install ansible ansible-lint
          ansible --version
          ansible-playbook --version
          ansible-lint --version || true
        '''
      }
    }

    stage('Stage 7- Install z/OS Collections (host venv)') {
      when { expression { return !params.USE_DOCKER } }
      steps {
        sh '''
          set -e
          . "$VENV_DIR/bin/activate"
          cd ansible
          ansible-galaxy collection install -r requirements.yml
        '''
      }
    }

    stage('Stage 8- Build Docker CI Image (optional)') {
      when { expression { return params.USE_DOCKER } }
      steps {
        sh '''
          set -e
          docker version

          IMAGE="zos-ansible-ci"

          if docker image inspect "$IMAGE" >/dev/null 2>&1; then
            echo "Image '$IMAGE' exists."
            if [ "${REBUILD_DOCKER}" = "true" ]; then
              echo "REBUILD_DOCKER=true -> rebuilding..."
              docker build -t "$IMAGE" -f ci/docker/Dockerfile .
            else
              echo "Skipping rebuild."
            fi
          else
            echo "Image '$IMAGE' not found -> building..."
            docker build -t "$IMAGE" -f ci/docker/Dockerfile .
          fi

          docker images "$IMAGE" | head -n 5
        '''
      }
    }

    stage('Stage 9- Lint / Syntax Check (safe)') {
      steps {
        sh '''
          set -e

          cd ansible
          INV="inventories/${DEPLOY_ENV}/hosts.ini"
          VARS="group_vars/${DEPLOY_ENV}.yml"
          
          echo "Syntax-check using inventory: $INV and vars: $VARS"

          if [ "${USE_DOCKER}" = "true" ]; then
            docker run --rm \
              -v "$PWD:/workspace" -w /workspace \
              zos-ansible-ci \
              bash -lc "
                set -e
                cd ansible
                ansible-playbook --syntax-check -i '$INV' playbooks/deploy.yml -e '@$VARS'
                ansible-lint -q playbooks/deploy.yml || true
              "
          else
            . "$VENV_DIR/bin/activate"
            ansible-playbook --syntax-check -i "$INV" playbooks/deploy.yml -e "@$VARS"
            ansible-lint -q playbooks/deploy.yml || true
          fi
        '''
      }
    }

    stage('Stage 10- Approval Gate (prod)') {
      when { expression { return env.DEPLOY_ENV == 'prod' && env.SAFE_MODE != 'true' } }
      steps {
        script {
          input message: "Approve PROD deployment for build #${env.BUILD_NUMBER} (branch: ${env.GIT_BRANCH_EFFECTIVE})?",
                ok: "Deploy to PROD"
        }
      }
    }

    stage('Stage 11- Execute Deployment') {
      when { expression { return env.SAFE_MODE != 'true' } }
      steps {
        echo "Deploying to z/OS via Ansible (env=${env.DEPLOY_ENV})..."
        sshagent(credentials: [env.ZOS_SSH_CRED]) {
          sh '''
            set -e
            
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
            echo "Artifacts (host) dir: ${ARTIFACTS_DIR}"

            if [ "${USE_DOCKER}" = "true" ]; then
              echo "USE_DOCKER=true -> running inside Docker (zos-ansible-ci)"

              # IMPORTANT: container artifacts path MUST be under /workspace
              CONTAINER_ART_DIR="/workspace/artifacts/build-${BUILD_NUMBER}"
              mkdir -p "artifacts/build-${BUILD_NUMBER}"
              echo "Artifacts (container) dir: ${CONTAINER_ART_DIR}"

              docker run --rm \
                -v "$PWD:/workspace" -w /workspace \
                -e SSH_AUTH_SOCK="$SSH_AUTH_SOCK" \
                -v "$SSH_AUTH_SOCK:$SSH_AUTH_SOCK" \
                zos-ansible-ci \
                bash -lc "
                  set -e
                  cd ansible
                  ansible-playbook -i '$INV' playbooks/deploy.yml \
                    -e '@$VARS' \
                    -e 'env=${DEPLOY_ENV}' \
                    -e 'artifacts_dir=${CONTAINER_ART_DIR}' \
                    $EXTRA_VARS
                "
            else
              echo "USE_DOCKER=false -> running using host venv"
              . "$VENV_DIR/bin/activate"
              
              ansible-playbook -i "$INV" playbooks/deploy.yml \
                -e "@${VARS}" \
                -e "env=${DEPLOY_ENV}" \
                -e "artifacts_dir=${ARTIFACTS_DIR}" \
                $EXTRA_VARS
            fi
          '''
        }
      }
    }

    stage('Stage 11- PR Safe Mode Notice') {
      when { expression { return env.SAFE_MODE == 'true' } }
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
