🚀 z/OS Ansible DevOps CI/CD Pipeline 

![COBOL](https://img.shields.io/badge/Language-COBOL-blue)
![JCL](https://img.shields.io/badge/Language-JCL-blue)
![Ansible](https://img.shields.io/badge/Automation-Ansible-red)
![Jenkins](https://img.shields.io/badge/CI-Jenkins-orange)
![zOS](https://img.shields.io/badge/Platform-IBM%20z%2FOS-green)
![VSAM](https://img.shields.io/badge/VSAM-Validation-purple)
![Contributors](https://img.shields.io/github/contributors/AtherShakeel/zos-ansible-devops)
![Last commit](https://img.shields.io/github/last-commit/AtherShakeel/zos-ansible-devops)


Enterprise-grade Mainframe DevOps & Modernization pipeline built on IBM z/OS, Ansible, Jenkins Multibranch, and Docker (optional).

This project demonstrates how traditional z/OS workloads (COBOL, JCL, VSAM) can be integrated into a modern CI/CD workflow with environment isolation, automation, and reproducibility.

🎯 What This Project Demonstrates

🧠 Mainframe expertise (z/OS datasets, VSAM, JES spool handling)

🔁 End-to-end CI/CD across dev → int → prod

🔀 Branch-based environment promotion

🧪 PR safe-mode (no accidental deployments)

📦 Artifact capture (JESMSGLG, JESJCL, job output)

🐳 Dockerized Ansible execution (optional, reproducible)

🛡️ Enterprise guardrails (approvals, branch protections)

This is not a demo script — it mirrors how real enterprises modernize mainframe delivery pipelines.

🧱 Tech Stack

IBM z/OS

Ansible + ibm.ibm_zos_core

Jenkins Multibranch Pipelines

Docker (WSL2 backend, optional)

GitHub

COBOL / JCL / VSAM

ZOAU / SSH-based automation

📁 Repository Structure
.
├── ansible/
│   ├── playbooks/        # deploy.yml (end-to-end orchestration)
│   ├── roles/            # datasets, deploy_sources, compile, run, spool_artifacts
│   ├── inventories/      # dev / int / prod
│   ├── group_vars/       # env-specific variables
│   └── requirements.yml # z/OS Ansible collections
│
├── ci/
│   ├── Dockerfile        # zos-ansible-ci image
│   ├── run_docker.sh     # Run deployment via Docker
│
├── artifacts/
│   └── build-<n>/        # Jenkins build outputs (spool files, JCL)
│
└── Jenkinsfile           # Full multibranch CI/CD pipeline

🔀 Branch → Environment Mapping
Branch	      Environment
dev	          dev
int	          int
main/master	  prod
PR branches	  SAFE MODE (no deploy)

🧪 SAFE MODE (PR Builds)

Pull Requests never deploy to z/OS.

PR builds:

Run syntax checks

Run Ansible lint

Validate inventories & vars

Block deployment by design

This mirrors enterprise change-control policies.

🐳 Docker (Optional Execution Mode)

The pipeline supports two execution models:

🔹 Option 1: Native (No Docker)

Python virtualenv

Ansible installed on Jenkins agent

🔹 Option 2: Dockerized (Recommended)

Prebuilt zos-ansible-ci image

Fully reproducible Ansible runtime

No Python/Ansible required on host

Docker is optional — both modes coexist cleanly.

▶️ Run Locally (WSL / Linux)
Without Docker
cd ansible
ansible-playbook \
  -i inventories/dev/hosts.ini \
  playbooks/deploy.yml \
  -e "@group_vars/dev.yml" \
  -e "env=dev"

With Docker
ci/run_docker.sh dev

🤖 Jenkins CI/CD Highlights

Multibranch pipeline (auto-discovery)

Environment inference from branch name

PROD approval gate

Optional Docker-based execution (USE_DOCKER=true)

Build artifacts archived per build

Artifacts example:

artifacts/build-27/
├── COMPDYN_JOB03276_compile_calc.spool.txt
├── COMPMAIN_JOB03278_compile_main.spool.txt
├── PRNTVASM_JOB03281_util_print_vsam.spool.txt
└── rendered_jcl/

📦 Artifact Management

JESMSGLG

JESJCL

Job output spools

Rendered JCL

Normalized text for readability

Artifacts are archived in Jenkins and traceable per build.

⚠️ Real-World Problems Solved

This project intentionally addresses non-trivial enterprise issues:

Root-owned files from Docker runs

Jenkins workspace permission conflicts

z/OS spool retrieval timing & retries

Environment variable scoping

Inventory & group_vars resolution

PR vs non-PR deployment safety

These are the actual problems teams face — and they’re handled here.

🧭 Why This Project Exists

Mainframe modernization is not about replacing COBOL.

It’s about:

Bringing DevOps discipline to z/OS

Reducing deployment risk

Improving traceability

Enabling faster, safer change delivery

This repo shows how that looks in practice.

👤 Author

Ather Shakeel
Mainframe Engineer | z/OS DevOps | CI/CD Modernization
