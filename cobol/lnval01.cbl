       IDENTIFICATION DIVISION.
       PROGRAM-ID. LNVAL01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-FILE ASSIGN TO TRANSIN.
           SELECT MASTER-FILE ASSIGN TO MASTVSAM
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS L-LOAN-ID
                  FILE STATUS  IS WS-VSAM-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANS-FILE.
       01  TRANS-REC.
           05 T-LOAN-ID-IN         PIC X(10).
           05 T-BORROWER-IN        PIC X(30).
           05 T-AMOUNT-IN          PIC 9(07)V99.
           05 T-STATUS-IN          PIC X(01).
           05 T-FILLER-IN          PIC X(29).
           05 T-CODE               PIC X(01).


       FD  MASTER-FILE.
       COPY LOANREC.

       WORKING-STORAGE SECTION.

       01  WS-VSAM-STATUS          PIC X(02) VALUE "00".
       01  WS-EOF-SW               PIC X(01) VALUE "N".
           88  EOF-REACHED         VALUE "Y".

      * DYNAMIC CALL VARIABLE
       01  WS-DYNAMIC-PROG         PIC X(08) VALUE SPACES.


       PROCEDURE DIVISION.
       0000-MAIN.
           CALL "LNUTIL01". *> Static Call

           OPEN INPUT TRANS-FILE
                I-O   MASTER-FILE.
           display "vsam open :"  WS-VSAM-STATUS

           PERFORM 1000-PROCESS UNTIL EOF-REACHED.

           CLOSE TRANS-FILE MASTER-FILE.
           display "vsam closed :" WS-VSAM-STATUS.
           GOBACK.

       1000-PROCESS.
           READ TRANS-FILE AT END MOVE "Y" TO WS-EOF-SW
              NOT AT END
                 MOVE SPACES TO LOAN-RECORD
                 MOVE ZERO   TO L-LOAN-AMOUNT
      * Map input to our Copybook structure
                 MOVE T-LOAN-ID-IN   TO L-LOAN-ID
                 MOVE T-BORROWER-IN  TO L-BORROWER-NAME
                 MOVE T-AMOUNT-IN    TO L-LOAN-AMOUNT
                 MOVE T-STATUS-IN    TO L-LOAN-STATUS

      * --- CONDITIONAL DYNAMIC CALL LOGIC ---
                 EVALUATE T-CODE
                 WHEN "C"
                    MOVE "LNCALC01" TO WS-DYNAMIC-PROG
                    CALL WS-DYNAMIC-PROG USING LOAN-RECORD
                 WHEN "A"
                    MOVE "LNALRT01" TO WS-DYNAMIC-PROG
                    CALL WS-DYNAMIC-PROG USING LOAN-RECORD
                 WHEN "B"
                    MOVE "LNCALC01" TO WS-DYNAMIC-PROG
                    CALL WS-DYNAMIC-PROG USING LOAN-RECORD
                    MOVE "LNALRT01" TO WS-DYNAMIC-PROG
                    CALL WS-DYNAMIC-PROG USING LOAN-RECORD
                 WHEN OTHER
                    DISPLAY "INVALID TRANS CODE: " T-CODE
                 END-EVALUATE

                 WRITE LOAN-RECORD
                    INVALID KEY REWRITE LOAN-RECORD
                 END-WRITE
           END-READ.
