#!/usr/bin/env bash
set -e

if [ -z "$1" ]; then
  echo "Usage: $0 <dev|int|prod>"
  exit 1
fi

DEPLOY_ENV="$1"

INV="ansible/inventories/${DEPLOY_ENV}/hosts.ini"
VARS="ansible/group_vars/${DEPLOY_ENV}.yml"
ARTIFACTS_DIR="/workspace/artifacts/docker-${DEPLOY_ENV}"

if [ ! -f "$INV" ]; then
  echo "ERROR: Missing inventory: $INV"
  exit 2
fi

if [ ! -f "$VARS" ]; then
  echo "ERROR: Missing vars file: $VARS"
  exit 2
fi

echo "Running Docker deploy for environment: ${DEPLOY_ENV}"
echo "Inventory  : ${INV}"
echo "Vars file  : ${VARS}"
echo "Artifacts  : artifacts/docker-${DEPLOY_ENV}"

docker run --rm \
  -v "$PWD:/workspace" -w /workspace \
  -v "$HOME/.ssh:/root/.ssh:ro" \
  zos-ansible-ci \
  bash -lc "
    set -e
    cd ansible
    mkdir -p '${ARTIFACTS_DIR}'
    ansible-playbook -i '${INV#ansible/}' playbooks/deploy.yml \
      -e '@${VARS#ansible/}' \
      -e 'env=${DEPLOY_ENV}' \
      -e 'artifacts_dir=${ARTIFACTS_DIR}'
  "
