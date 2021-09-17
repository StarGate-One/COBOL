       IDENTIFICATION DIVISION.
       PROGRAM-ID. randomck.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  result-tally.
           03  result-tallies                  PIC 9(7) OCCURS 6 TIMES
                                                  INDEXED BY tally-idx.

       01  random-number                       PIC 9 COMP.

       PROCEDURE DIVISION.
           MOVE FUNCTION RANDOM(FUNCTION SECONDS-PAST-MIDNIGHT)
             TO random-number

           PERFORM 1000000 TIMES
              COMPUTE random-number = FUNCTION RANDOM * 6
              ADD 1 TO random-number
              ADD 1 TO result-tallies (random-number)
           END-PERFORM.

           PERFORM VARYING tally-idx FROM 1 BY 1 UNTIL tally-idx > 6
              DISPLAY result-tallies (tally-idx)
           END-PERFORM.
           END PROGRAM randomck.
