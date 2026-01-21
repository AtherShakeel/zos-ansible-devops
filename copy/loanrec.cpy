      * LOAN RECORD LAYOUT (80 BYTES)
       01  LOAN-RECORD.
           05  L-LOAN-ID           PIC X(10). *> OFFSET 0.LENGTH 10
           05  L-BORROWER-NAME     PIC X(30).  *> OFFSET 10.LENGTH 30
           05  L-LOAN-AMOUNT       PIC 9(07)V99. *> OFFSET 40.LENGTH 9
           05  L-LOAN-STATUS       PIC X(01).  *> OFFSET 49.LENGTH 1
           05  FILLER              PIC X(30).  *> OFFSET 50.LENGTH 30