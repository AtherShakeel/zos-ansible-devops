pipeline {
    agent {label 'linux'}

    options {
        timeout(time: 10, unit: 'MINUTES')
        ansiColor('xterm')
        timestamps()
    }

    parameters {
        booleanParam(name: 'FORCE_PRIME', defaultValue: false, description: 'Force VSAM priming even if MASTER already exists')
        booleanParam(name: 'DEBUG', defaultValue: false, description: 'Debug mode (shows more Ansible detail)')
    }

    environment {
        // Keep Ansible output readable in Jenkins
        ANSIBLE_STDOUT_CALLBACK = "yaml"
        ANSIBLE_FORCE_COLOR = "true"
        PYTHONUNBUFFERED = "1"
        ANSIBLE_HOST_KEY_CHECKING = "False"

        // venv inside workspace (reproducible, no dependency on node global state)
        VENV_DIR = "${WORKSPACE}/.venv"

        // Jenkins credentials:
        // - github-auth: used for pulling repo (you already have this)
        // - zos-ssh-key: SSH Username with private key for z/OS host access
        ZOS_SSH_CRED = "zos-ssh-key"
    }

    stages {
        stage('Pull from GitHub') {
            steps {
                echo 'Pulling fresh zos-ansible-devops code...'
                git branch: 'main',
                    credentialsId: 'github-auth',
                    url: 'https://github.com/AtherShakeel/zos-ansible-devops'
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

                // Uses SSH key stored in Jenkins
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

                        ansible-playbook -i hosts.ini playbooks/deploy.yml $EXTRA_VARS
                    '''
                }
            }
        }
    }

    post {
        always {
            echo 'Pipeline finished. Archiving artifacts (spools/output)...'
            archiveArtifacts artifacts: 'artifacts/**', allowEmptyArchive: true, fingerprint: true
        }
        failure {
            echo 'FAILURE: Check artifacts (spool files) in the Build Artifacts section.'
        }
    }
}
