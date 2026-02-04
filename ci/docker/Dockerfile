FROM python:3.11-slim

# Keep it light
ENV PIP_NO_CACHE_DIR=1 \
    PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN apt-get update && apt-get install -y --no-install-recommends \
    openssh-client \
    git \
    ca-certificates \
  && rm -rf /var/lib/apt/lists/*

# Ansible + lint
RUN pip install ansible ansible-lint

# IBM z/OS core collection
RUN ansible-galaxy collection install ibm.ibm_zos_core

WORKDIR /workspace
