//ZBRSETUP JOB (ACCT),'ENV SETUP',CLASS=A,MSGCLASS=X
//*****************************************************************
//* STEP 1: DEFINE THE VSAM MASTER FILE
//*****************************************************************
//*DEFVSAM  EXEC PGM=IDCAMS
//*SYSPRINT DD SYSOUT=*
//*SYSIN    DD *
//*DEFINE CLUSTER (NAME(Z88264.AZBRIDGE.MASTER) -
//*         INDEXED -
//*         RECORDS(100 50) -
//*         RECORDSIZE(80 80) -
//*         KEYS(10 0) -
//*         VOLUMES(VPWRKB))
/*
//*****************************************************************
//* STEP 4: PRIME THE VSAM FILE (PREVENT FILE STATUS 35/39)
//*****************************************************************
//PRIME    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//DUMMYIN  DD *
0000000000DUMMY RECORD FOR INITIALIZATION
/*
//SYSIN    DD *
  REPRO REPLACE INFILE(DUMMYIN) OUTDATASET(Z88264.AZBRIDGE.MASTER)
/*