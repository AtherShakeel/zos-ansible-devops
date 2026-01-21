# Pipeline Workflow

1. Create datasets (idempotent)
2. Upload:
   - COBOL
   - Copybooks
   - Subprograms
   - Input data

3. Submit JCL chain
   - compile
   - link
   - run

4. Capture spool:
   - JESMSGLG
   - JESJCL
   - JESYSMSG
   - SYSPRINT / SYSOUT

5. Fail only after artifacts saved
