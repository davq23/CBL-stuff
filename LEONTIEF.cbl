      *****************************************************************
      * Program name:    LEONTIEF
      * Original author: DAVID QUINTERO
      *
      * Maintenence Log
      * Date      Author        Maintenance Requirement
      * --------- ------------  ---------------------------------------
      * 03/05/2020 DAVID QUINTERO  Created for COBOL class
      *
      *
      * Description: Calculates the Leontief inverse of an 
      *              open Input-Output model of up to 100 sectors
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  LEONTIEF.
       AUTHOR. DAVID QUINTERO.
       INSTALLATION. COBOL DEVELOPMENT CENTER.
       DATE-WRITTEN. 05/05/2020.
       DATE-COMPILED. 05/05/2020.
       SECURITY. NON-CONFIDENTIAL.
      ***
      ***
      ***
       ENVIRONMENT DIVISION.
      **
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-3081.
       OBJECT-COMPUTER. IBM-3081.
      **
      **
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN  TO INDD
            ORGANIZATION IS SEQUENTIAL.
      ***
      ***
      ***
       DATA DIVISION.
      **
      **
       FILE SECTION.
       FD  INFILE
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           BLOCK  CONTAINS 0 RECORDS
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS FS-ELM.
       01  FS-ELM.
           88 EOF     VALUE HIGH-VALUES.
           02 ROW     PIC 9(03).
           02 FILLER  PIC X.
           02 COLMN   PIC 9(03).
           02 FILLER  PIC X.
           02 NUM     PIC -9(08)V9(10).
           02 FILLER  PIC X(53).
      **
      **
       WORKING-STORAGE SECTION.
      *--------------------------INDICES-----------------------------*
       77  ROW-IND    PIC S9(03) USAGE IS COMP.
       77  TABL-SIZE  PIC S9(03) USAGE IS COMP VALUE 0.
       77  COL-IND    PIC S9(03) USAGE IS COMP.
       77  FIXED-IND  PIC S9(03) USAGE IS COMP.
      *-----------------------SINGLE VARIABLES-----------------------*
       77  FIXED-ELM  PIC S9(08)V9(10) USAGE IS COMP-3 VALUE 0.
      *------------------------NUMBER FORMAT-------------------------*
       77  DEC-BUF    PIC -ZZZZZZZ9.9999999999.
       77  INT-BUF    PIC -ZZ9.
      *---------------------------TABLES-----------------------------*
       01  IO-MTRX.
           05  ROW    OCCURS 100 TIMES.
           10  COLMN  OCCURS 100 TIMES.
           15  IO-ELM PIC S9(08)V9(10) USAGE IS COMP-3 VALUE 0.
       01  LTF-INRS.
           05  ROW    OCCURS 100 TIMES.
           10  COLMN  OCCURS 100 TIMES.
           15  LTF-ELM PIC S9(08)V9(10) USAGE IS COMP-3 VALUE 0.
       01  FINAL-DEMAND.
           05  ROW    OCCURS 50 TIMES.
           10  TOTAL   PIC S9(08)V9(10) USAGE IS COMP-3 VALUE 0.
      ***
      ***
      ***
       PROCEDURE DIVISION.
      **
      **
       MAIN-PROCEDURE.
           OPEN INPUT INFILE
           PERFORM READ-INPUT-FILE UNTIL EOF
           CLOSE INFILE
      *
           IF TABL-SIZE > 0
      *
             PERFORM DIVIDE-BY-TOTAL VARYING ROW-IND FROM 1 BY 1
               UNTIL ROW-IND > TABL-SIZE
      *
             DISPLAY 'INPUT COEFFICIENTS'
             PERFORM DISPLAY-IO VARYING ROW-IND FROM 1 BY 1
               UNTIL ROW-IND > TABL-SIZE
      *
             PERFORM FILL-IDENTITY VARYING ROW-IND FROM 1 BY 1
               UNTIL ROW-IND > TABL-SIZE
      *
             PERFORM SUBTRACT-IDENTITY VARYING ROW-IND FROM 1 BY 1
               UNTIL ROW-IND > TABL-SIZE
      *
             PERFORM DIVIDE-ROW THRU START-PIVOTING
              VARYING FIXED-IND  FROM 1 BY 1
              UNTIL FIXED-IND > TABL-SIZE
      *
             DISPLAY 'LEONTIEF INVERSE'
             PERFORM DISPLAY-LTF VARYING ROW-IND FROM 1 BY 1
              UNTIL ROW-IND > TABL-SIZE
      *
           END-IF.
      **
      **
       PROGRAM-END.
           GOBACK.
      **
      **
       READ-INPUT-FILE.
            READ INFILE
      *
             AT END
              SET EOF TO TRUE
      *
             NOT AT END
               IF COLMN OF FS-ELM NOT = 000
      *
                 IF TABL-SIZE < ROW OF FS-ELM
                   MOVE ROW OF FS-ELM TO TABL-SIZE
                 END-IF
      *
                 MOVE NUM TO IO-ELM (ROW OF FS-ELM, COLMN OF FS-ELM)
      *
                 ADD IO-ELM (ROW OF FS-ELM, COLMN OF FS-ELM)
                   TO TOTAL (ROW OF FS-ELM)
               ELSE
      *
                 MOVE NUM TO  FIXED-ELM

                 ADD  FIXED-ELM TO TOTAL (ROW OF FS-ELM)
      *
               END-IF
            END-READ.
      **
      **
       DIVIDE-BY-TOTAL.
           PERFORM VARYING COL-IND FROM 1 BY 1
            UNTIL COL-IND > TABL-SIZE
      *
               COMPUTE IO-ELM (ROW-IND, COL-IND) =
                 IO-ELM (ROW-IND, COL-IND) / TOTAL (COL-IND)
               END-COMPUTE
      *
           END-PERFORM.
      **
      **
       FILL-IDENTITY.
           PERFORM VARYING COL-IND FROM 1 BY 1
            UNTIL COL-IND > TABL-SIZE
      *
               IF COL-IND = ROW-IND
      *
                   MOVE 1 TO LTF-ELM (ROW-IND, COL-IND)
      *
               ELSE
      *
                   MOVE 0 TO LTF-ELM (ROW-IND, COL-IND)
      *
               END-IF
      *
           END-PERFORM.
      **
      **
       SUBTRACT-IDENTITY.

           PERFORM VARYING COL-IND FROM 1 BY 1
            UNTIL COL-IND > TABL-SIZE
      *
               SUBTRACT IO-ELM (ROW-IND, COL-IND)
                FROM LTF-ELM (ROW-IND, COL-IND)
                GIVING IO-ELM (ROW-IND, COL-IND)
      *
           END-PERFORM.
      **
      **
       DISPLAY-LTF.
           MOVE ROW-IND TO INT-BUF
           DISPLAY 'ROW ', INT-BUF
      *
           PERFORM VARYING COL-IND FROM 1 BY 1
            UNTIL COL-IND > TABL-SIZE
      *
               MOVE COL-IND TO INT-BUF
               DISPLAY 'COLUMN ', INT-BUF
      *
               MOVE LTF-ELM (ROW-IND, COL-IND) TO DEC-BUF
               DISPLAY DEC-BUF
      *
           END-PERFORM
      *
           DISPLAY ' '.
      **
      **
       DISPLAY-IO.
           MOVE ROW-IND TO INT-BUF
           DISPLAY 'ROW ', INT-BUF
      *
           PERFORM VARYING COL-IND FROM 1 BY 1
            UNTIL COL-IND > TABL-SIZE
      *
               MOVE COL-IND TO INT-BUF
               DISPLAY 'COLUMN ', INT-BUF
      *
               MOVE IO-ELM (ROW-IND, COL-IND) TO DEC-BUF
               DISPLAY DEC-BUF
      *
           END-PERFORM
           DISPLAY ' '.
      **
      **
       DIVIDE-ROW.
           MOVE IO-ELM (FIXED-IND, FIXED-IND) TO FIXED-ELM
      *
           IF FIXED-ELM = 0
      *
               DISPLAY 'MULTIPLE SOLUTIONS FOR ECONOMY'
               GO TO PROGRAM-END
      *
           END-IF
      *
           PERFORM VARYING COL-IND FROM 1 BY 1
             UNTIL COL-IND > TABL-SIZE
      *
               DIVIDE FIXED-ELM INTO IO-ELM (FIXED-IND, COL-IND)
                LTF-ELM (FIXED-IND, COL-IND) ROUNDED
      *
           END-PERFORM.
      **
      **
       START-PIVOTING.
           PERFORM PIVOTING-ROW VARYING ROW-IND FROM 1 BY 1 UNTIL
             ROW-IND > TABL-SIZE.
      **
      **
       PIVOTING-ROW.
           IF ROW-IND NOT = FIXED-IND
      *
            MOVE IO-ELM (ROW-IND, FIXED-IND) TO FIXED-ELM
      *
               PERFORM  VARYING COL-IND FROM 1 BY 1 UNTIL
                COL-IND > TABL-SIZE
      *
                COMPUTE
                   IO-ELM (ROW-IND, COL-IND) = IO-ELM (ROW-IND, COL-IND)
                 - (IO-ELM (FIXED-IND, COL-IND) * FIXED-ELM)
                END-COMPUTE
      *
                COMPUTE
                 LTF-ELM (ROW-IND, COL-IND) = LTF-ELM (ROW-IND, COL-IND)
                 - (LTF-ELM (FIXED-IND, COL-IND) * FIXED-ELM)
                END-COMPUTE
      *
               END-PERFORM
      *
           END-IF.
