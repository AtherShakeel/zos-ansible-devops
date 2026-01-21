//PRIMEKSD JOB (ACCT),'PRIME VSAM',CLASS=A,MSGCLASS=X
//*****************************************************************
//* THIS JOB FORCES ONE RECORD INTO THE VSAM KSDS TO INITIALIZE IT
//*****************************************************************
//STEP01   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//DUMMYIN  DD *
0000000000DUMMY RECORD - INITIALIZED                000000000
/*
//VSAMOUT  DD DSN=Z88264.ZBRIDGE.MASTER.KSDS,DISP=SHR
//SYSIN    DD *
  REPRO INFILE(DUMMYIN) OUTFILE(VSAMOUT)
/*
//*****************************************************************
//* VERIFY THE RECORD WAS WRITTEN
//*****************************************************************
//STEP02   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  PRINT INDATASET(Z88264.ZBRIDGE.MASTER.KSDS) CHARACTER
/*
