      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. SAMPLE.
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. POSIX WITH DEBUGGING MODE.
       OBJECT-COMPUTER. POSIX.

       SPECIAL-NAMES.

       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STANDARD-IN
           ASSIGN TO KEYBOARD
           ORGANIZATION IS LINE SEQUENTIAL
           STATUS IS STDIN-FILE-STATUS.

           SELECT STANDARD-OUT
           ASSIGN TO DISPLAY
           ORGANIZATION IS LINE SEQUENTIAL
           STATUS IS STDOUT-FILE-STATUS.

           SELECT STANDARD-ERR
           ASSIGN TO DISPLAY
           ORGANIZATION IS LINE SEQUENTIAL
           STATUS IS STDERR-FILE-STATUS.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
       FD STANDARD-IN.
           01 STDIN-LINE       PIC X(32768).
       FD STANDARD-OUT.
           01 STDOUT-LINE      PIC X(32768).
       FD STANDARD-ERR.
           01 STDERR-LINE      PIC X(32768).

       WORKING-STORAGE SECTION.
       01 STDIN-FILE-STATUS.
          05 STDIN-STATUS      PIC 99.
          05 STDIN-SUBSTATUS REDEFINES
             STDIN-STATUS.
             07 STDIN-STATUS-1 PIC 9.
             07 STDIN-STATUS-2 PIC 9.

       01 STDOUT-FILE-STATUS.
          05 STDOUT-STATUS     PIC 99.
          05 STDOUT-SUBSTATUS REDEFINES
             STDOUT-STATUS.
             07 STDOUT-STATUS-1 PIC 9.
             07 STDOUT-STATUS-2 PIC 9.

       01 STDERR-FILE-STATUS.
          05 STDERR-STATUS     PIC 99.
          05 STDERR-SUBSTATUS REDEFINES
             STDERR-STATUS.
             07 STDERR-STATUS-1 PIC 9.
             07 STDERR-STATUS-2 PIC 9.

       01 COUNTDOWN            PIC 99.
       01 DISPLAY-COUNT        PIC Z9.
       01 JOKE-LIMITER         PIC X     VALUE LOW-VALUE.
          88 REFRAIN                     VALUE HIGH-VALUE.

       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       REPORT SECTION.
       SCREEN SECTION.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       DECLARATIVES.

       HELPFUL-DEBUG SECTION.
           USE FOR DEBUGGING ON CLEANSE.
       CLEANSE-DEBUG.
           DISPLAY
               "DEBUG: CLEANSING INPUT: " TRIM(STDIN-LINE TRAILING)
               UPON SYSERR.

       BARK-ON-STDIN-ERRORS SECTION.
           USE AFTER STANDARD ERROR ON STANDARD-IN.
       BARK-STDIN.
           DISPLAY
               "SOMETHING BAD HAPPENED ON KEYBOARD" UPON SYSERR.

       BARK-ON-STDOUT-ERRORS SECTION.
           USE AFTER STANDARD ERROR ON STANDARD-OUT.
       BARK-STDOUT.
           DISPLAY
               "SOMETHING BAD HAPPENED ON DISPLAY" UPON SYSERR.

       BARK-ON-STDERR-ERRORS SECTION.
           USE AFTER STANDARD ERROR ON STANDARD-ERR.
       BARK-STDERR.
           DISPLAY
               "SOMETHING BAD HAPPENED ON DISPLAY" UPON SYSERR.

       END DECLARATIVES.

       MAINLINE SECTION.

      *  TURN ON STATEMENT TRACER LINES  *
           READY TRACE.

           OPEN INPUT STANDARD-IN
           IF STDIN-STATUS GREATER THAN 10
              PERFORM SOFT-EXCEPTION
           END-IF.

           OPEN OUTPUT STANDARD-OUT.
           IF STDOUT-STATUS GREATER THAN 10
              PERFORM SOFT-EXCEPTION
           END-IF.

           OPEN OUTPUT STANDARD-ERR.
           IF STDERR-STATUS GREATER THAN 10
              PERFORM SOFT-EXCEPTION
           END-IF.


      *  TURN OFF STATEMENT TRACER LINES  *
           RESET TRACE.

           PERFORM UNTIL STDIN-STATUS GREATER THAN 9
               MOVE "WHAT IS YOUR COMMAND? " TO STDOUT-LINE
               WRITE STDOUT-LINE END-WRITE
               IF STDOUT-STATUS GREATER THAN 10
                   PERFORM SOFT-EXCEPTION
               END-IF

               READ STANDARD-IN
                   AT END
                       EXIT PERFORM
               END-READ
               IF STDIN-STATUS GREATER THAN 10
                   PERFORM SOFT-EXCEPTION
               END-IF

               PERFORM CLEANSE

               EVALUATE STDIN-LINE ALSO TRUE
                   WHEN "HELP"         ALSO ANY
                       DISPLAY "WE ALL WANT A LITTLE HELP"
                       DISPLAY "HELP, QUIT OR EXIT EXIT"
                   WHEN "QUIT"         ALSO ANY
                       DISPLAY
                          "I KNOW YOU WANT TO QUIT, BUT I'M BEING"
                           " UNFRIENDLY; TYPE 'EXIT', YOU USER YOU"
                   WHEN "EXIT"         ALSO REFRAIN
                       DISPLAY "FINE, LEAVING NOW"
                       EXIT PERFORM
                   WHEN "EXIT"         ALSO ANY
                       DISPLAY "HA!  NO QUIT FOR YOU"
                       DISPLAY
                           "WASTING YOUR TIME FOR "
                       END-DISPLAY
                       PERFORM VARYING COUNTDOWN FROM 10 BY -1
                           UNTIL COUNTDOWN EQUAL ZERO
                           MOVE COUNTDOWN TO DISPLAY-COUNT
                           DISPLAY
                               DISPLAY-COUNT "... " WITH NO ADVANCING
                           CALL
                               "FFLUSH" USING NULL
                               ON EXCEPTION CONTINUE
                           END-CALL
                           CALL "C$SLEEP" USING 1 END-CALL
                       END-PERFORM
                       DISPLAY "KEEP TRYING"
                       SET REFRAIN TO TRUE
                   WHEN OTHER
                       DISPLAY "TRY 'HELP'"
               END-EVALUATE
           END-PERFORM.

           GOBACK.

      *  ***************************************************************
       HELPER SECTION.

      *  RUDIMENTARY CHANGES TO STDIN, SHOW OFF A FEW FUNCTIONS <*
       CLEANSE.
           MOVE TRIM(SUBSTITUTE(LOWER-CASE(STDIN-LINE),
               "'", SPACE, '"', SPACE))
             TO STDIN-LINE.

       SOFT-EXCEPTION.
           DISPLAY "EXCEPTION-FILE:      " EXCEPTION-FILE 
                   UPON SYSERR.
           DISPLAY "EXCEPTION-STATUS:    " EXCEPTION-STATUS 
                   UPON SYSERR.
           DISPLAY "EXCEPTION-LOCATION:  " EXCEPTION-LOCATION 
                   UPON SYSERR.
           DISPLAY "EXCEPTION-STATEMENT: " EXCEPTION-STATEMENT 
                   UPON SYSERR.

       HARD-EXCEPTION.
           PERFORM SOFT-EXCEPTION.
           STOP RUN RETURNING 127.
         
           END PROGRAM SAMPLE.
       