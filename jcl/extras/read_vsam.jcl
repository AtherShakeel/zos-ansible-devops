//READVSAM  JOB (ACCT),'VIEW VSAM',CLASS=A,MSGCLASS=X
//*------------------------------------------------------------------*
//* DELETE THE PREVIOUS FLAT FILE IF IT EXISTS
//*------------------------------------------------------------------*
//DELOLD   EXEC PGM=IEFBR14
//OLDDATA  DD DSN=Z88264.AZBRIDGE.MASTER.VIEW,
//            DISP=(MOD,DELETE,DELETE),UNIT=SYSDA,SPACE=(TRK,0)
//*------------------------------------------------------------------*
//* COPY VSAM TO FLAT FILE
//*------------------------------------------------------------------*
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//VSAMFILE DD DSN=Z88264.AZBRIDGE.MASTER,DISP=SHR
//FLATFILE DD DSN=Z88264.AZBRIDGE.MASTER.VIEW,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(1,1)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//SYSIN    DD *
  REPRO INFILE(VSAMFILE) OUTFILE(FLATFILE)
/*