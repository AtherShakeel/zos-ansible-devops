       IDENTIFICATION DIVISION.
       PROGRAM-ID. LNCALC01.
      *================================================================*
      * SUBPROGRAM: CALCULATES LOAN VALIDATION STATUS
      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
      * We use the same copybook here to define the passed data
       COPY LOANREC.

       PROCEDURE DIVISION USING LOAN-RECORD.
       0000-MAIN.
      * SIMPLE BUSINESS RULE: If Amount > 500,000, mark as 'R'eview
           IF L-LOAN-AMOUNT > 500000.00
              MOVE 'R' TO L-LOAN-STATUS
           ELSE
              MOVE 'A' TO L-LOAN-STATUS
           END-IF.

           GOBACK.