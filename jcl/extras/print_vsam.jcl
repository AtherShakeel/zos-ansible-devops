//PRNTVSAM JOB (ACCT),'VERIFY VSAM',CLASS=A,MSGCLASS=X
//*****************************************************************
//* PRINT THE FIRST 20 RECORDS TO VERIFY STATUS AND AMOUNTS
//*****************************************************************
//STEP01   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  PRINT -
    IDS(Z88264.AZBRIDGE.MASTER) -
    CHARACTER -
    COUNT(20)
/*
