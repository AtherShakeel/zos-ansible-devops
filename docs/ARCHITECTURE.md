# Architecture Overview

## Components

1. Controller (Jenkins/WSL)
   - Ansible engine
   - SSH connectivity
   - Artifact storage

2. Target z/OS
   - ZOAU utilities
   - JES
   - COBOL compiler
   - VSAM

## Flow Diagram

Controller → Ansible → SSH → z/OS
       ↓
  artifacts/
