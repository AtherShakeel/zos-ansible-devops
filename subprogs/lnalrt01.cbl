       IDENTIFICATION DIVISION.
       PROGRAM-ID. LNALRT01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-LIMIT               PIC 9(07)V99 VALUE 500000.00.
       LINKAGE SECTION.
       COPY LOANREC.
       PROCEDURE DIVISION USING LOAN-RECORD.
      * Professional Rule: If amount > 500k, flag for manual review
           IF L-LOAN-AMOUNT > WS-LIMIT
              MOVE "S" TO L-LOAN-STATUS
              DISPLAY "AUDIT: ID " L-LOAN-ID " FLAGGED - OVER LIMIT"
           END-IF.
           GOBACK.