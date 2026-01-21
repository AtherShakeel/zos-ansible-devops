# zos-ansible-devops  

![COBOL](https://img.shields.io/badge/Language-COBOL-blue)
![JCL](https://img.shields.io/badge/Language-JCL-blue)
![Ansible](https://img.shields.io/badge/Automation-Ansible-red)
![Jenkins](https://img.shields.io/badge/CI-Jenkins-orange)
![zOS](https://img.shields.io/badge/Platform-IBM%20z%2FOS-green)


### Enterprise-style z/OS CI/CD using Ansible and ZOAU

---

## 1. What is this project?

**zos-ansible-devops** is an end-to-end DevOps automation project for IBM z/OS using **Ansible** and the **ibm.ibm_zos_core** collection over **SSH + ZOAU**.

It automates the complete mainframe application lifecycle:

- Create and manage z/OS datasets (PDS, PDSE, PS, VSAM)
- Upload COBOL programs, copybooks, subprograms, and input data
- Submit a controlled JCL job chain (compile, link, run)
- Capture JES spool output for every job as build artifacts
- Fail the pipeline when jobs return unacceptable return codes
- Integrate cleanly with Jenkins for CI/CD

This project replaces an earlier **Python + Zowe CLI** pipeline with a **modern, Ansible-based** approach focused on:

- idempotency  
- structured job results  
- reproducible environments  
- clean CI logs with full diagnostics via artifacts  

---

## 2. Why this project exists

- To modernize mainframe workflows using DevOps practices  
- To replace Zowe CLI scripting with native Ansible automation  
- To learn and demonstrate:
  - inventories & group_vars  
  - reusable roles  
  - SSH + ZOAU integration  
  - Jenkins-based CI/CD  

---

## 3. High-level workflow

1. Controller (WSL / Jenkins) runs Ansible  
2. Ansible connects to **z/OS via SSH**  
3. ZOAU utilities perform:
   - dataset allocation  
   - JCL submission  
   - JES spool retrieval  
4. Spool is saved locally under `artifacts/`  
5. Pipeline fails **after** artifacts are preserved  

---

## 4. Environment & prerequisites

### Controller
- Linux / WSL Ubuntu  
- Python 3.9+  
- Ansible (via pip)  
- SSH key access to z/OS  
- Internet access for Ansible Galaxy  

### Target system (z/OS)
- IBM z/OS (IBM Z Xplore)  
- SSH enabled  
- ZOAU installed  
- USS Python:

/usr/lpp/IBM/cyp/v3r9/pyz/bin/python3

yaml
Copy code

### Critical: ZOAU environment

`ansible/group_vars/zos_ssh.yml` exports:

- ZOAU_HOME  
- PATH  
- LIBPATH  
- PYTHONPATH  

Injected via:

```yaml
environment: "{{ zoau_env }}"


5. Dataset model (source of truth)
Defined in:
ansible/group_vars/all.yml

Purpose	Type	Attributes
SOURCE	PDS	FB, LRECL=80
SUBSRC	PDS	FB, LRECL=80
COPYLIB	PDS	FB, LRECL=80
OBJLIB	PDS	RECFM=U
LOADLIB	PDSE	RECFM=U
TRANS	PS	FB, LRECL=80
MASTER	VSAM KSDS	Keylen=10

⚠ If a dataset was created with wrong attributes,
state: present will NOT correct it — it must be recreated.


6. VSAM priming & force_prime
VSAM is primed using setup_env.jcl.

Default:

Prime only when newly created

Override:

bash
Copy code
ansible-playbook -i hosts.ini playbooks/dply.yml -e force_prime=true
Meaning:

“Reset VSAM contents without redefining it”


7. Spool & artifacts
Captured for every job:

JESMSGLG

JESJCL

JESYSMSG

Extra:

PRNTVASM → SYSPRINT

RUNORCH → SYSOUT

Artifacts saved to:

Copy code
artifacts/
Spool is saved even when the pipeline fails.


8. Console output philosophy
Job submits use no_log: true

Jenkins logs stay clean

Full details live in artifacts

Debug mode:

bash
Copy code
ansible-playbook -i hosts.ini playbooks/dply.yml -e debug=true


9. ACTUAL Project Structure
powershell
Copy code
zos-ansible-devops/
├── ansible/
│   ├── ansible.cfg
│   ├── hosts.ini
│   ├── requirements.yml
│   ├── group_vars/
│   │   ├── all.yml
│   │   └── zos_ssh.yml
│   ├── playbooks/
│   │   ├── deploy.yml             # MAIN orchestrator
│   │   ├── deploy_test.yml        # experimental
│   │   └── labs/                  # experimental tests
│   │   └── tasks/                 # experimental tasks
│   └── roles/
│       ├── datasets/
│       ├── deploy_sources/
│       ├── run_jobs/
│       └── spool_artifacts/
│
├── cobol/
├── copy/
├── subprogs/
├── jcl/
├── data/
│   └── transactions.txt           # Input file to be read in main cobol pgm and finally loaded in VSAM as per the business logic
│
├── zapp.yaml                      # experimental / not used
├── 1.1.0                          # UNKNOWN – gitignored
│
├── artifacts/                     # GENERATED – gitignored
├── Jenkinsfile
├── .gitignore
└── README.md

Status of folders
CORE (used by Ansible pipeline)

ansible/

cobol/ copy/ subprogs/ jcl/ data/

Jenkinsfile

EXPERIMENTAL / LEGACY

playbooks/labs/ – test plays

deploy_test.yml – experiments

zapp.yaml – not used


10. Dependencies
ansible/requirements.yml

yaml
Copy code
collections:
  - name: ibm.ibm_zos_core
Install:

bash
Copy code
cd ansible
ansible-galaxy collection install -r requirements.yml


11. Quick Start (Run locally)
bash
Copy code
cd ansible
ansible-galaxy collection install -r requirements.yml
ansible-playbook -i hosts.ini playbooks/deploy.yml

Force VSAM prime:
ansible-playbook -i hosts.ini playbooks/dply.yml -e force_prime=true
Debug mode:
ansible-playbook -i hosts.ini playbooks/dply.yml -e debug=true


12. Jenkins
Jenkins performs:

Checkout GitHub

Create Python venv

Install Ansible + collections

Run playbook

Archive artifacts/**

Pipeline defined in:

nginx
Copy code
Jenkinsfile


13. Design principles
Idempotent

Artifacts-first

Clean logs

Single source of truth

CI/CD ready


14. Status
✅ End-to-end working
✅ Roles implemented
✅ Spool capture reliable
✅ Jenkins-ready
