pipeline {
    agent { label 'linux' }

    options {
        // Keep last N builds and artifacts on the controller (Windows)
        buildDiscarder(logRotator(numToKeepStr: '20', artifactNumToKeepStr: '10'))

        timeout(time: 30, unit: 'MINUTES')
        ansiColor('xterm')
        timestamps()
    }

    parameters {
        booleanParam(name: 'FORCE_PRIME', defaultValue: false, description: 'Force VSAM priming even if MASTER already exists')
        booleanParam(name: 'DEBUG', defaultValue: false, description: 'Debug mode (shows more Ansible detail)')
    }

    environment {
        ANSIBLE_STDOUT_CALLBACK = "yaml"
        ANSIBLE_FORCE_COLOR = "true"
        PYTHONUNBUFFERED = "1"
        ANSIBLE_HOST_KEY_CHECKING = "False"
        VENV_DIR = "${WORKSPACE}/.venv"
        ZOS_SSH_CRED = "zos-ssh-key"
        ARTIFACT_DIR = "artifacts/build-${BUILD_NUMBER}"
    }

    stages {

        stage('Clean Workspace') {
            steps {
                // Built-in: wipes the agent workspace folder contents
                deleteDir()
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

        stage('Pull from GitHub') {
            steps {
                echo 'Pulling fresh zos-ansible-devops code...'
                checkout([$class: 'GitSCM',
                    branches: [[name: '*/main']],
                    userRemoteConfigs: [[
                        url: 'https://github.com/AtherShakeel/zos-ansible-devops',
                        credentialsId: 'github-auth'
                    ]],
                    extensions: [[$class: 'CleanBeforeCheckout']]
                ])
            }
        }

        stage('Setup Ansible Environment') {
            steps {
                echo 'Creating Python venv and installing Ansible...'
                sh '''
                    set -e
                    python3 -V
                    python3 -m venv "$VENV_DIR"
                    . "$VENV_DIR/bin/activate"
                    python -m pip install --upgrade pip
                    pip install ansible
                    ansible --version
                    ansible-playbook --version
                '''
            }
        }

        stage('Install z/OS Collections') {
            steps {
                echo 'Installing Ansible collections from ansible/requirements.yml...'
                sh '''
                    set -e
                    . "$VENV_DIR/bin/activate"
                    cd ansible
                    ansible-galaxy collection install -r requirements.yml
                '''
            }
        }

        stage('Execute Deployment') {
            steps {
                echo 'Starting z/OS deployment via Ansible (datasets → uploads → JCL chain → spool artifacts)...'

                sshagent(credentials: [env.ZOS_SSH_CRED]) {
                    sh '''
                        set -e
                        . "$VENV_DIR/bin/activate"
                        cd ansible

                        EXTRA_VARS=""
                        if [ "${FORCE_PRIME}" = "true" ]; then
                          EXTRA_VARS="$EXTRA_VARS -e force_prime=true"
                        fi
                        if [ "${DEBUG}" = "true" ]; then
                          EXTRA_VARS="$EXTRA_VARS -e debug=true"
                        fi

                        ansible-playbook -i hosts.ini playbooks/deploy.yml \
                          -e artifact_dir=${ARTIFACT_DIR} \
                          $EXTRA_VARS
                    '''
                }
            }
        }
    }

    post {
        always {
            echo 'Normalizing spool artifacts (convert literal \\n to real newlines)...'
            sh '''
                set -e
                if [ -d "artifacts" ]; then
                  find artifacts -type f -name "*.spool.txt" -print0 2>/dev/null | \
                    xargs -0 -r perl -0777 -pe 's/\\\\n/\\n/g' -i
                fi
            '''

            echo 'Pipeline finished. Archiving artifacts (spools/output)...'
            archiveArtifacts artifacts: 'artifacts/**', allowEmptyArchive: true, fingerprint: true
        }
        failure {
            echo 'FAILURE: Check artifacts (spool files) in the Build Artifacts section.'
        }
    }
}
