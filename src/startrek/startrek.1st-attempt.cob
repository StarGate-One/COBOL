      * ****************************************************************
      *                     Begin Program STARTREK
      * ****************************************************************

       IDENTIFICATION DIVISION.

       PROGRAM-ID.    STARTREK.

      * AUTHOR.        Kurt Wilhem.
       
      * INSTALLATION.  Oakland University.
                       
       DATE-COMPILED. FUNCTION WHEN-COMPILED.
        
      * DATE-MODIFIED. September 04, 2021.
       
      * DATE-WRITTEN.  Completed September 1, 1979.
       
      * ****************************************************************
      *                  Program Description
      * ****************************************************************
        
      * Star Trek simulates an outer space adventure game on a remote
      * terminal. The user commands the U.S.S. Enterprise, and thru
      * various offensive and defensive commands, travels throughout
      * the galaxy on a mission to destroy all Klingons, Romulans, and
      * other challenges which also maneuver and fire on the Enterprise.
        
      * ----------------------------------------------------------------
      *                         Program History
       
      *   Date   Int                    Reason
      * -------- --- ---------------------------------------------------
      
      * 20210902 Q   Admiral Q ported source from OpenCobol 1.1 to 
      *              GnuCobol 3.2.
      *              Modernized to use new GnuCobol 3.2 abilities.
      *              Basically a rewrite adding my own changes.

      * 20100324 BT  <btiffin@users.sf.net> (Brian Tiffin),
      *              btiffin updates, it actually works now, when
      *              compiled perform-osvs:yes This allows thru with
      *              overlapping exits.
      *                
      * 20100324 HA  <harald@skogtun.org> (Harald Arnesen),
      *              Ported to OpenCobol 1.1 by changing the comments
      *              to free format, as some lines are too long for
      *              fixed format.
      *              I haven't played it, just that it compiles and
      *              runs!
      *          Original source:
      *          <http://dunnington.u-net.com/public/STARTREK/ctrek.cob>
       
      * 19790901 KW  (Kurt Wilhelm), Original program written.
                                                              
      * ----------------------------------------------------------------

      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

       ENVIRONMENT DIVISION.

      * ----------------------------------------------------------------

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. IBM-PC.
       
      * SOURCE-COMPUTER. IBM-PC WITH DEBUGGING MODE.

      * Set Environment Variable COB_SET_DEBUG= Y, y or 1 to activate
      *     debugging lines during runtime (those with D in column 7)
      *     provide the program was compiled with the -fdebugging-line
      *     and the -d or -debug switch.

       OBJECT-COMPUTER. IBM-PC.
   
       SPECIAL-NAMES.
 
       REPOSITORY.
       FUNCTION ALL INTRINSIC.
 
      * ----------------------------------------------------------------

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT STANDARD-IN
       ASSIGN TO KEYBOARD
       ORGANIZATION LINE SEQUENTIAL
       STATUS WS-STDIN-FILE-STATUS.
  
       SELECT STANDARD-OUT
       ASSIGN TO DISPLAY
       ORGANIZATION LINE SEQUENTIAL
       STATUS WS-STDOUT-FILE-STATUS. 

       SELECT STANDARD-ERR
       ASSIGN TO DISPLAY
       ORGANIZATION LINE SEQUENTIAL
       STATUS WS-STDERR-FILE-STATUS. 

      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

       DATA DIVISION.

      * ----------------------------------------------------------------

       FILE SECTION.

       FD STANDARD-IN.
          01 STDIN-LINE       PIC X(32768).

       FD STANDARD-OUT.
          01 STDOUT-LINE      PIC X(32768).
 
       FD STANDARD-ERR.
          01 STDERR-LINE      PIC X(32768).
 
      * ----------------------------------------------------------------

       WORKING-STORAGE SECTION.

       01 WS-STD-FILE-STATUSES.
          03 WS-STDIN-FILE-STATUS       PIC   99 VALUE ZEROES.
          03 WS-STDOUT-FILE-STATUS      PIC   99 VALUE ZEROES.
          03 WS-STDERR-FILE-STATUS      PIC   99 VALUE ZEROES.
          03 WS-STD-FILE-STATUS.
             05 WS-STD-FILE-STATUS-1    PIC    9.
             05 WS-STD-FILE-STATUS-2    PIC    9.
             05 WS-STD-FILE-STATUS-2-RED REDEFINES
                WS-STD-FILE-STATUS-2    PIC   99 BINARY.
          03 WS-STD-FILE-ACTION         PIC    X(05) VALUE LOW-VALUES.
             88 WS-STD-FILE-ACTION-CLOSE VALUE "CLOSE".
             88 WS-STD-FILE-ACTION-OPEN  VALUE "OPEN".
             88 WS-STD-FILE-ACTION-READ  VALUE "READ".
             88 WS-STD-FILE-ACTION-IO    VALUE "I-O".
             88 WS-STD-FILE-ACTION-WRITE VALUE "WRITE".
          03 WS-STD-FILE-NAME           PIC    X(12) VALUE LOW-VALUES.
             88 WS-STD-FILE-STD-ERR      VALUE "STANDARD-ERR".
             88 WS-STD-FILE-STD-IN       VALUE "STANDARD-IN".
             88 WS-STD-FILE-STD-OUT      VALUE "STANDARD-OUT".
          03 WS-STD-FILE-INFO           PIC    X(60) VALUE LOW-VALUES.

       01 WS-FLAGS-SWITCHES.
          03 WS-END-OF-FILE       BINARY PIC    9 VALUE 0.
             88 WS-NOT-EOF            VALUE 0.
             88 WS-AT-EOF             VALUE 1.

       01 WS-COUNTERS.
      *    03 WS-GALAXY-X-AXIS-CTR           PIC   9(02)  VALUE ZEROES.
      *    03 WS-GALAXY-Y-AXIS-CTR           PIC   9(02)  VALUE ZEROES.
      *    03 WS-GALAXY-Z-AXIS-CTR           PIC   9(02)  VALUE ZEROES.
      *    03 WS-SECTOR-X-CELL-CTR           PIC   9(02)  VALUE ZEROES.
      *    03 WS-SECTOR-Y-CELL-CTR           PIC   9(02)  VALUE ZEROES.
      *    03 WS-SECTOR-Z-CELL-CTR           PIC   9(02)  VALUE ZEROES.
      *    03 WS-GALAXY-MAX                  USAGE INDEX  VALUE 50.
      *    03 WS-SECTOR-MAX                  USAGE INDEX  VALUE 20.
          03 WS-GALAXY-MAX                  USAGE INDEX  VALUE 0.
             88 WS-GALAXY-TRAINEE          VALUE  1.
             88 WS-GALAXY-CW02             VALUE  2.
             88 WS-GALAXY-CW03             VALUE  3.
             88 WS-GALAXY-CW04             VALUE  4.
             88 WS-GALAXY-CW05             VALUE  5.
             88 WS-GALAXY-ENS-O1           VALUE  6.
             88 WS-GALAXY-LTJG-O2          VALUE  7.
             88 WS-GALAXY-LT-O3            VALUE  8.
             88 WS-GALAXY-LCDR-O4          VALUE 10.
             88 WS-GALAXY-CDR-O5           VALUE 15.
             88 WS-GALAXY-CAPT-O6          VALUE 20.
             88 WS-GALAXY-RDML-O7          VALUE 25.
             88 WS-GALAXY-RADM-O8          VALUE 30.
             88 WS-GALAXY-VADM-O9          VALUE 35.
             88 WS-GALAXY-ADM-O10          VALUE 40.
             88 WS-GALAXY-FL-ADM-O11       VALUE 45.
             88 WS-GALAXY-CMDR-SF-O12      VALUE 50.
          03 WS-SECTOR-MAX                  USAGE INDEX  VALUE 0.
             88 WS-SECTOR-TRAINEE          VALUE  1.
             88 WS-SECTOR-CW02             VALUE  2.
             88 WS-SECTOR-CW03             VALUE  3.
             88 WS-SECTOR-CW04             VALUE  4.
             88 WS-SECTOR-CW05             VALUE  5.
             88 WS-SECTOR-ENS-O1           VALUE  6.
             88 WS-SECTOR-LTJG-O2          VALUE  7.
             88 WS-SECTOR-LT-O3            VALUE  8.
             88 WS-SECTOR-LCDR-O4          VALUE  9.
             88 WS-SECTOR-CDR-O5           VALUE 10.
             88 WS-SECTOR-CAPT-O6          VALUE 11.
             88 WS-SECTOR-RDML-O7          VALUE 12.
             88 WS-SECTOR-RADM-O8          VALUE 13.
             88 WS-SECTOR-VADM-O9          VALUE 14.
             88 WS-SECTOR-ADM-O10          VALUE 15.
             88 WS-SECTOR-FL-ADM-O11       VALUE 16.
             88 WS-SECTOR-CMDR-SF-O12      VALUE 20.

       01 WS-MISC.
          03 WS-BLANK-SCREEN                 PIC X(2080) VALUE SPACES.
          03 WS-CURRENT-DATE-TIME-OFFSET     PIC X(21).
          03 WS-CURRENT-DATE-TIME-OFFSET-RED REDEFINES
             WS-CURRENT-DATE-TIME-OFFSET.
             05 WS-CURRENT-DATE.
                07 WS-CURRENT-DATE-CC        PIC   9(02).
                07 WS-CURRENT-DATE-YY        PIC   9(02).
                07 WS-CURRENT-DATE-MM        PIC   9(02).
                07 WS-CURRENT-DATE-DD        PIC   9(02).
             05 WS-CURRENT-TIME.
                07 WS-CURRENT-TIME-HH        PIC   9(02).
                07 WS-CURRENT-TIME-MM        PIC   9(02).
                07 WS-CURRENT-TIME-SS        PIC   9(02).
                07 WS-CURRENT-TIME-MS        PIC   9(02).
             05 WS-CURRENT-OFFSET.
                07 WS-CURRENT-OFFSET         PIC   X(01).
                07 WS-CURRENT-OFFSET-HH      PIC   9(02).
                07 WS-CURRENT-OFFSET-MM      PIC   9(02).
          03 WS-RANK-STRUCTURE               PIC   9(02).
                88 WS-VALUE-TRAINEE          VALUE  1.
                88 WS-VALUE-CW02             VALUE  2.
                88 WS-VALUE-CW03             VALUE  3.
                88 WS-VALUE-CW04             VALUE  4.
                88 WS-VALUE-CW05             VALUE  5.
                88 WS-VALUE-ENS-O1           VALUE  6.
                88 WS-VALUE-LTJG-O2          VALUE  7.
                88 WS-VALUE-LT-O3            VALUE  8.
                88 WS-VALUE-LCDR-O4          VALUE  9.
                88 WS-VALUE-CDR-O5           VALUE 10.
                88 WS-VALUE-CAPT-O6          VALUE 11.
                88 WS-VALUE-RDML-O7          VALUE 12.
                88 WS-VALUE-RADM-O8          VALUE 13.
                88 WS-VALUE-VADM-O9          VALUE 14.
                88 WS-VALUE-ADM-O10          VALUE 15.
                88 WS-VALUE-FL-ADM-O11       VALUE 16.
                88 WS-VALUE-CMDR-SF-O12      VALUE 17.
         03 WS-RANK-DESCRIPTION              PIC X(25).
                88 WS-TRAINEE      VALUE  "Trainee                  ".
                88 WS-CW02         VALUE  "Warrant Officer 2        ".
                88 WS-CW03         VALUE  "Warrant Officer 3        ".
                88 WS-CW04         VALUE  "Warrant Officer 4        ".
                88 WS-CW05         VALUE  "Warrant Officer 5        ".
                88 WS-ENS-O1       VALUE  "Ensign                   ".
                88 WS-LTJG-O2      VALUE  "Lt., Junior Grade        ".
                88 WS-LT-O3        VALUE  "Lieutenant               ".
                88 WS-LCDR-O4      VALUE  "Lt. Commander            ".
                88 WS-CDR-O5       VALUE  "Commander                ".
                88 WS-CAPT-O6      VALUE  "Captain                  ".
                88 WS-RDML-O7      VALUE  "Rear Admiral Lower Half  ".
                88 WS-RADM-O8      VALUE  "Read Admiral Upper Half  ".
                88 WS-VADM-O9      VALUE  "Vice Admiral             ".
                88 WS-ADM-O10      VALUE  "Admiral                  ".
                88 WS-FADM-O11     VALUE  "Fleet Admiral            ".
                88 WS-CMDR-SF-O12  VALUE  "Commander, Star Fleet    ".
          03 WS-RANDOM-NUMBER                PIC 9(19)V9(19).
          03 WS-RANDOM-NBR REDEFINES
             WS-RANDOM-NUMBER.
             05 WS-RANDOM-NUMBER-INTEGER     PIC 9(19).
             05 WS-RANDOM-NUMBER-DECIMAL     PIC 9(19).
          03 WS-RANDOM-SEED                  PIC 9(19)V9(19).
          03 WS-RANDOM-TEMP                  PIC 9(19)V9(19).
          03 WS-SECTOR-CONTENTS              PIC X(03).
             88 WS-SECTOR-ANOMALLY           VALUE "?A?".
             88 WS-SECTOR-ASTEROID           VALUE "(^)".
             88 WS-SECTOR-BLACK-HOLE         VALUE "(B)".
             88 WS-SECTOR-COLONY             VALUE "{@}".
             88 WS-SECTOR-COMET              VALUE "(C)".
             88 WS-SECTOR-EMPTY              VALUE SPACES.
             88 WS-SECTOR-ENTERPRISE         VALUE "/E\".
             88 WS-SECTOR-GALAXY-BARRIER     VALUE "|+|".
             88 WS-SECTOR-KLINGON            VALUE "[K]".
             88 WS-SECTOR-MOON               VALUE "(M)".
             88 WS-SECTOR-NEUTRAL-ZONE       VALUE "|-|".
             88 WS-SECTOR-NOVA               VALUE "(N)".
             88 WS-SECTOR-PLANET             VALUE "(P)".
             88 WS-SECTOR-ROMULAN            VALUE "[R]".
             88 WS-SECTOR-STAR               VALUE "(*)".
             88 WS-SECTOR-STAR-BASE          VALUE "{#}".
             88 WS-SECTOR-STAR-FLEET         VALUE "SFC".
             88 WS-SECTOR-STAR-SHIP          VALUE "<S>".
             88 WS-SECTOR-UNKNOWN            VALUE "?U?".
             88 WS-SECTOR-VULCAN             VALUE "<V>".
             88 WS-SECTOR-WORMHOLE           VALUE "(W)".
          03 WS-SECTOR-CONTENTS-COUNT-MAXIMUMS.
             05 WS-SECTOR-ANOMALLY-MAX       PIC  9(04).
                88 ANOMALLY-TRAINEE          VALUE    5.
                88 ANOMALLY-CW02             VALUE   10.
                88 ANOMALLY-CW03             VALUE   20.
                88 ANOMALLY-CW04             VALUE   30.
                88 ANOMALLY-CW05             VALUE   40.
                88 ANOMALLY-ENS-O1           VALUE   50.
                88 ANOMALLY-LTJG-O2          VALUE   60.
                88 ANOMALLY-LT-O3            VALUE   70.
                88 ANOMALLY-LCDR-O4          VALUE   80.
                88 ANOMALLY-CDR-O5           VALUE   90.
                88 ANOMALLY-CAPT-O6          VALUE  100.
                88 ANOMALLY-RDML-O7          VALUE  200.
                88 ANOMALLY-RADM-O8          VALUE  400.
                88 ANOMALLY-VADM-O9          VALUE  800.
                88 ANOMALLY-ADM-O10          VALUE 1600.
                88 ANOMALLY-FADM-O11         VALUE 2400.
                88 ANOMALLY-CMDR-SF-O12      VALUE 3200.
             05 WS-SECTOR-ASTEROID-MAX       PIC  9(04).
             05 WS-SECTOR-BLACK-HOLE-MAX     PIC  9(04).
             05 WS-SECTOR-COLONY-MAX         PIC  9(04).
             05 WS-SECTOR-COMET-MAX          PIC  9(04).
             05 WS-SECTOR-KLINGON-MAX        PIC  9(04).
             05 WS-SECTOR-MOON-MAX           PIC  9(04).
             05 WS-SECTOR-NOVA-MAX           PIC  9(04).
             05 WS-SECTOR-PLANET-MAX         PIC  9(04).
             05 WS-SECTOR-ROMULAN-MAX        PIC  9(04).
             05 WS-SECTOR-STAR-MAX           PIC  9(04).
             05 WS-SECTOR-STAR-BASE-MAX      PIC  9(04).
             05 WS-SECTOR-STAR-SHIP-MAX      PIC  9(04).
             05 WS-SECTOR-UNKNOWN-MAX        PIC  9(04).
             05 WS-SECTOR-VULCAN-MAX         PIC  9(04).
             05 WS-SECTOR-WORMHOLE-MAX       PIC  9(04).
          03 WS-SECTOR-CONTENTS-COUNTS.
             05 WS-SECTOR-ANOMALLY-CNT       PIC  9(04).
             05 WS-SECTOR-ASTEROID-CNT       PIC  9(04).
             05 WS-SECTOR-BLACK-HOLE-CNT     PIC  9(04).
             05 WS-SECTOR-COLONY-CNT         PIC  9(04).
             05 WS-SECTOR-COMET-CNT          PIC  9(04).
             05 WS-SECTOR-KLINGON-CNT        PIC  9(04).
             05 WS-SECTOR-MOON-CNT           PIC  9(04).
             05 WS-SECTOR-NOVA-CNT           PIC  9(04).
             05 WS-SECTOR-PLANET-CNT         PIC  9(04).
             05 WS-SECTOR-ROMULAN-CNT        PIC  9(04).
             05 WS-SECTOR-STAR-CNT           PIC  9(04).
             05 WS-SECTOR-STAR-BASE-CNT      PIC  9(04).
             05 WS-SECTOR-STAR-SHIP-CNT      PIC  9(04).
             05 WS-SECTOR-UNKNOWN-CNT        PIC  9(04).
             05 WS-SECTOR-VULCAN-CNT         PIC  9(04).
             05 WS-SECTOR-WORMHOLE-CNT       PIC  9(04).

       01 WS-GALAXY-TABLE-ARRAY.
          03 WS-GALAXY-X-AXIS
             OCCURS 50 TIMES INDEXED BY WS-GALAXY-X-IDX.
             05 WS-GALAXY-Y-AXIS
                OCCURS 50 TIMES INDEXED BY WS-GALAXY-Y-IDX.
                07 WS-GALAXY-Z-AXIS
                   OCCURS 50 TIMES INDEXED BY WS-GALAXY-Z-IDX.
                   09 WS-GALAXY-SECTOR               PIC X.

       01 WS-SECTOR-TABLE-ARRAY.
          03 WS-SECTOR-X-AXIS
             OCCURS 20 TIMES INDEXED BY WS-SECTOR-X-IDX.
             05 WS-SECTOR-Y-AX
                OCCURS 20 TIMES INDEXED BY WS-SECTOR-Y-IDX.
               07 WS-SECTOR-Z-AXIS
                  OCCURS 20 TIMES INDEXED BY WS-SECTOR-Z-IDX.
                  09 WS-SECTOR-GALAXY-ADDRESS.
      *               11 WS-GALAXY-X-AXIS-IDX       USAGE INDEX.
      *               11 WS-GALAXY-Y-AXIS-IDX       USAGE INDEX.
      *               11 WS-GALAXY-Z-AXIS-IDX       USAGE INDEX.
      *               11 WS-SECTOR-X-AXIS-IDX       USAGE INDEX.
      *               11 WS-SECTOR-Y-AXIS-IDX       USAGE INDEX.
      *               11 WS-SECTOR-Z-AXIS-IDX       USAGE INDEX.
                     11 WS-GALAXY-X-AXIS-IDX       PIC 9(04).
                     11 WS-GALAXY-Y-AXIS-IDX       PIC 9(04).
                     11 WS-GALAXY-Z-AXIS-IDX       PIC 9(04).
                     11 WS-SECTOR-X-AXIS-IDX       PIC 9(04).
                     11 WS-SECTOR-Y-AXIS-IDX       PIC 9(04).
                     11 WS-SECTOR-Z-AXIS-IDX       PIC 9(04).
                  09 WS-SECTOR-CELL-CONTENTS       PIC X(03).
                     88 SECTOR-ANOMALLY            VALUE "?A?".
                     88 SECTOR-ASTEROID            VALUE "(^)".
                     88 SECTOR-BLACK-HOLE          VALUE "(B)".
                     88 SECTOR-COLONY              VALUE "{@}".
                     88 SECTOR-COMET               VALUE "(C)".
                     88 SECTOR-EMPTY               VALUE SPACES.
                     88 SECTOR-ENTERPRISE          VALUE "/E\".
                     88 SECTOR-GALAXY-BARRIER      VALUE "|+|".
                     88 SECTOR-KLINGON             VALUE "[K]".
                     88 SECTOR-MOON                VALUE "(M)".
                     88 SECTOR-NEUTRAL-ZONE        VALUE "|-|".
                     88 SECTOR-NOVA                VALUE "(N)".
                     88 SECTOR-PLANET              VALUE "(P)".
                     88 SECTOR-ROMULAN             VALUE "[R]".
                     88 SECTOR-STAR                VALUE "(*)".
                     88 SECTOR-STAR-BASE           VALUE "{#}".
                     88 SECTOR-STAR-FLEET          VALUE "SFC".
                     88 SECTOR-STAR-SHIP           VALUE "<S>".
                     88 SECTOR-UNKNOWN             VALUE "?U?".
                     88 SECTOR-VULCAN              VALUE "<V>".
                     88 SECTOR-WORMHOLE            VALUE "(W)".

       01 WS-SCREEN-STORAGE-VARIABLES.
          03 WS-RESPONSE-INTRO      PIC X     VALUE SPACES.
             88 WS-CONTINUE         VALUE "C" "c".
             88 WS-QUIT             VALUE "Q" "q".
          03 WS-RESPONSE-LAST-NAME  PIC X(30) VALUE SPACES.
          

      *LOCAL-STORAGE SECTION.
      *LINKAGE SECTION.
      *REPORT SECTION.
      * ----------------------------------------------------------------

       SCREEN SECTION.

       01 MAIN-SCREEN.
          03 INTRO-SECTION.
             05 VALUE "Star Trek Game Introduction" BLANK SCREEN
                      LINE 1 COL 26.
             05 VALUE "Space, the final frontier...."
                      LINE 3 COL 1.
             05 VALUE "These are the voyages of the starship, "
                      LINE 4 COL 1.
             05 VALUE "Enterprise..."
                      LINE 4 COL 40.
             05 VALUE "Her ongoing mission, "
                      LINE 5 COL 1.
             05 VALUE "To explore strange new worlds..."
                      LINE 6 COL 1.
             05 VALUE "To seek out new life and new civilizations..."
                      LINE 7 COL 1.
             05 VALUE "To boldly go where no one has gone before..."
                      LINE 8 COL 1.
             05 VALUE "Are you bold, brave and savey enough?"
                      LINE 10 COL 2.
          03 INTRO-RESPONSE.
             05 VALUE "Are you bold, brave and savey enough?"
                      LINE 10 COL 2.
             05 VALUE "C - to Continue"
                      LINE 12 COL 10.
             05 VALUE "Q - to Quit"
                      LINE 13 COL 10.
             05 VALUE "Enter Choice:"
                      LINE 14 COL 10.
             05 RESPONSE-INTRO 
                      LINE 14 COL 24
                      PIC X TO WS-RESPONSE-INTRO.
          03 CONTINUE-GAME.
             05 VALUE "Get Game Information" BLANK SCREEN
                      LINE 1 COL 30.
             05 VALUE "Last Name: "
                      LINE 3 COL 1.
             05 RESPONSE-LAST-NAME
                      LINE 3 COL 12
                      PIC X(30) TO WS-RESPONSE-LAST-NAME.
          03 CONTINUE-GET-RANK.
             05 VALUE "Enter your rank (1 thru 17): " 
                      LINE 5 COL 1.
             05 RESPONSE-RANK
                      LINE 5 COL 30
                      PIC 99 TO WS-RANK-STRUCTURE.
          03 CONTINUE-GREET.
             05 VALUE "Welcome aboard " 
                      LINE 7 COL 10.
             05 USING WS-RANK-DESCRIPTION
                      LINE 7 COL 27.
             05 USING WS-RESPONSE-LAST-NAME
                      LINE 7 COL 53.
             05 VALUE "C - to Continue"
                      LINE 12 COL 10.
             05 VALUE "Q - to Quit"
                      LINE 13 COL 10.
             05 VALUE "Enter Choice:"
                      LINE 14 COL 10.
             05 RESPONSE-GREET 
                      LINE 14 COL 24
                      PIC X TO WS-RESPONSE-INTRO.
      * COPY SCREENIO.
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

       PROCEDURE DIVISION.

      * ----------------------------------------------------------------

       0000-MAIN-PROGRAM SECTION.
    
           DISPLAY INTRO-SECTION.
           DISPLAY INTRO-RESPONSE.
           ACCEPT RESPONSE-INTRO.

           IF WS-CONTINUE
              PERFORM UNTIL WS-RESPONSE-LAST-NAME NOT = SPACES
                 DISPLAY CONTINUE-GAME
                 ACCEPT RESPONSE-LAST-NAME
              END-PERFORM

              MOVE SPACES TO RESPONSE-INTRO

              PERFORM UNTIL WS-RANK-STRUCTURE > 0
                        AND WS-RANK-STRUCTURE < 18
                 DISPLAY CONTINUE-GET-RANK
                 ACCEPT RESPONSE-RANK
              END-PERFORM

              EVALUATE WS-RANK-STRUCTURE
                  WHEN 1
                       SET WS-TRAINEE TO TRUE
                  WHEN 2
                       SET WS-CW02 TO TRUE
                  WHEN 3
                       SET WS-CW03 TO TRUE
                  WHEN 4
                       SET WS-CW04 TO TRUE
                  WHEN 5
                       SET WS-CW05 TO TRUE
                  WHEN 6
                       SET WS-ENS-O1 TO TRUE
                  WHEN 7
                       SET WS-LTJG-O2 TO TRUE
                  WHEN 8
                       SET WS-LT-O3 TO TRUE
                  WHEN 9
                       SET WS-LCDR-O4 TO TRUE
                  WHEN 10
                       SET WS-CDR-O5 TO TRUE
                  WHEN 11
                       SET WS-CAPT-O6 TO TRUE
                  WHEN 12
                       SET WS-RDML-O7 TO TRUE
                  WHEN 13
                       SET WS-RADM-O8 TO TRUE
                  WHEN 14
                       SET WS-VADM-O9 TO TRUE
                  WHEN 15
                       SET WS-ADM-O10 TO TRUE
                  WHEN 16
                       SET WS-FADM-O11 TO TRUE
                  WHEN 17
                       SET WS-CMDR-SF-O12 TO TRUE
              END-EVALUATE
              
              DISPLAY CONTINUE-GREET
              ACCEPT RESPONSE-GREET
              
              IF WS-CONTINUE
      D          DISPLAY "Start of game STAR TREK"

                 PERFORM 0010-START-PROGRAM THRU
                         0010-END

                 PERFORM 9000-STOP-PROGRAM THRU
                         9000-END
              END-IF
           END-IF.

           STOP RUN RETURNING 0.
            
       0000-END.
           EXIT.

      * ----------------------------------------------------------------

       0010-START-PROGRAM SECTION.

            SET WS-STD-FILE-ACTION-OPEN TO TRUE.
            OPEN INPUT STANDARD-IN.
            SET WS-STD-FILE-STD-IN TO TRUE.
            MOVE WS-STDIN-FILE-STATUS TO WS-STD-FILE-STATUS.
            PERFORM 9010-CHECK-FILE-STATUS THRU
                    9010-END.
            
            OPEN OUTPUT STANDARD-OUT.
            SET WS-STD-FILE-STD-OUT TO TRUE.
            MOVE WS-STDOUT-FILE-STATUS TO WS-STD-FILE-STATUS.
            PERFORM 9010-CHECK-FILE-STATUS THRU
                    9010-END.
            
            OPEN OUTPUT STANDARD-ERR.
            SET WS-STD-FILE-STD-ERR TO TRUE.
            MOVE WS-STDOUT-FILE-STATUS TO WS-STD-FILE-STATUS.
            PERFORM 9010-CHECK-FILE-STATUS THRU
                    9010-END.
                    
            MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME-OFFSET.

            PERFORM 0020-INTRO THRU
                    0020-END.

            PERFORM 0030-GET-NAME-RANK THRU
                    0030-END.

            PERFORM 0040-COMPUTE-RANDOM-SEED THRU
                    0040-END.

            PERFORM 1010-BUILD-GALAXY-ARRAY THRU
                    1010-END.

       0010-END.
            EXIT.

      * ----------------------------------------------------------------
       
       0020-INTRO SECTION.
       
       0020-END.
            EXIT.

      * ----------------------------------------------------------------
       
       0030-GET-NAME-RANK SECTION.
       
       0030-END.
            EXIT.

      * ----------------------------------------------------------------
      
       0040-COMPUTE-RANDOM-SEED SECTION.

            MOVE 1.0000000000000000001 TO WS-RANDOM-TEMP.
      D     DISPLAY "WS-RANDOM-TEMP->"WS-RANDOM-TEMP.

            IF WS-CURRENT-DATE-CC NOT = ZEROES
               COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP *
                       WS-CURRENT-DATE-CC
            END-IF.
      D     DISPLAY "WS-CURRENT-DATE-CC->" WS-CURRENT-DATE-CC.
      D     DISPLAY "WS-RANDOM-TEMP->"WS-RANDOM-TEMP.

            IF WS-CURRENT-TIME-HH NOT = ZEROES
               COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP *
                       WS-CURRENT-TIME-HH
            END-IF.
      D     DISPLAY "WS-CURRENT-TIME-HH->" WS-CURRENT-TIME-HH.
      D     DISPLAY "WS-RANDOM-TEMP->"WS-RANDOM-TEMP. 

            IF WS-CURRENT-DATE-YY NOT = ZEROES
               COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP *
                       WS-CURRENT-DATE-YY
            END-IF.
      D     DISPLAY "WS-CURRENT-DATE-YY->" WS-CURRENT-DATE-YY.
      D     DISPLAY "WS-RANDOM-TEMP->"WS-RANDOM-TEMP.

            IF WS-CURRENT-TIME-MM NOT = ZEROES
               COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP *
                       WS-CURRENT-TIME-MM
            END-IF.
      D     DISPLAY "WS-CURRENT-TIME-MM->" WS-CURRENT-TIME-MM.
      D     DISPLAY "WS-RANDOM-TEMP->"WS-RANDOM-TEMP.

            IF WS-CURRENT-DATE-MM NOT = ZEROES
               COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP *
                       WS-CURRENT-DATE-MM
            END-IF.
      D     DISPLAY "WS-CURRENT-DATE-MM->" WS-CURRENT-DATE-MM.
      D     DISPLAY "WS-RANDOM-TEMP->"WS-RANDOM-TEMP.

            IF WS-CURRENT-TIME-SS NOT = ZEROES
               COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP *
                       WS-CURRENT-TIME-SS
            END-IF.
      D     DISPLAY "WS-CURRENT-TIME-SS->" WS-CURRENT-TIME-SS.
      D     DISPLAY "WS-RANDOM-TEMP->"WS-RANDOM-TEMP.

            IF WS-CURRENT-DATE-DD NOT = ZEROES
               COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP *
                       WS-CURRENT-DATE-DD
            END-IF.
      D     DISPLAY "WS-CURRENT-DATE-DD->" WS-CURRENT-DATE-DD.
      D     DISPLAY "WS-RANDOM-TEMP->"WS-RANDOM-TEMP.

            IF WS-CURRENT-TIME-MS NOT = ZEROES
               COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP *
                       WS-CURRENT-TIME-MS
            END-IF.
      D     DISPLAY "WS-CURRENT-TIME-MS->" WS-CURRENT-TIME-MS.
      D     DISPLAY "WS-RANDOM-TEMP->"WS-RANDOM-TEMP.

            IF WS-CURRENT-OFFSET-HH NOT = ZEROES
               COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP /
                       WS-CURRENT-OFFSET-HH
            END-IF.
      D     DISPLAY "WS-CURRENT-OFFSET-HH->" WS-CURRENT-OFFSET-HH.
      D     DISPLAY "WS-RANDOM-TEMP->"WS-RANDOM-TEMP.

            IF WS-CURRENT-OFFSET-MM NOT = ZEROES
               COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP /
                       WS-CURRENT-OFFSET-MM
            END-IF.
      D     DISPLAY "WS-CURRENT-OFFSET-MM->" WS-CURRENT-OFFSET-MM.
      D     DISPLAY "WS-RANDOM-TEMP->"WS-RANDOM-TEMP.

            COMPUTE WS-RANDOM-SEED = 
                    FUNCTION SQRT(WS-RANDOM-TEMP).
            COMPUTE WS-RANDOM-NUMBER = 
                    FUNCTION RANDOM (WS-RANDOM-SEED).

      D     DISPLAY "WS-RANDOM-SEED->" WS-RANDOM-SEED.
      D     DISPLAY "WS-RANDOM-NUMBER->" WS-RANDOM-NUMBER.

       0040-END.
            EXIT.

      * ----------------------------------------------------------------

       1010-BUILD-GALAXY-ARRAY SECTION.

            MOVE ALL ZEROES TO WS-SECTOR-CONTENTS-COUNTS.

            MOVE HIGH-VALUES TO WS-GALAXY-TABLE-ARRAY.

            PERFORM VARYING WS-GALAXY-X-IDX FROM 1 BY 1
                    UNTIL WS-GALAXY-X-IDX GREATER WS-GALAXY-MAX
                  PERFORM VARYING WS-GALAXY-Y-IDX FROM 1 BY 1
                          UNTIL WS-GALAXY-Y-IDX GREATER WS-GALAXY-MAX
                        PERFORM VARYING WS-GALAXY-Z-IDX FROM 1 BY 1
                                UNTIL WS-GALAXY-Z-IDX GREATER 
                                      WS-GALAXY-MAX
                               PERFORM 1020-BUILD-SECTOR-ARRAY THRU
                                       1020-END
                        END-PERFORM
                  END-PERFORM
            END-PERFORM.

      D     DISPLAY "WS-SECTOR-ANOMALLY-CNT   ->"
      D              WS-SECTOR-ANOMALLY-CNT
      D             " WS-SECTOR-ANOMALLY-MAX   ->"
      D               WS-SECTOR-ANOMALLY-MAX.

      D     DISPLAY "WS-SECTOR-ASTEROID-CNT   ->"
      D              WS-SECTOR-ASTEROID-CNT
      D             " WS-SECTOR-ASTEROID-MAX   ->"
      D               WS-SECTOR-ASTEROID-MAX.

      D     DISPLAY "WS-SECTOR-BLACK-HOLE-CNT ->"
      D              WS-SECTOR-BLACK-HOLE-CNT
      D             " WS-SECTOR-BLACK-HOLE-MAX ->"
      D               WS-SECTOR-BLACK-HOLE-MAX.

      D     DISPLAY "WS-SECTOR-COLONY-CNT     ->"
      D              WS-SECTOR-COLONY-CNT
      D             " WS-SECTOR-COLONY-MAX     ->"
      D               WS-SECTOR-COLONY-MAX.

      D     DISPLAY "WS-SECTOR-COMET-CNT      ->"
      D              WS-SECTOR-COMET-CNT
      D             " WS-SECTOR-COMET-MAX      ->"
      D               WS-SECTOR-COMET-MAX.

      D     DISPLAY "WS-SECTOR-KLINGON-CNT    ->"
      D              WS-SECTOR-KLINGON-CNT
      D             " WS-SECTOR-KLINGON-MAX    ->"
      D               WS-SECTOR-KLINGON-MAX

      D     DISPLAY "WS-SECTOR-MOON-CNT       ->"
      D              WS-SECTOR-MOON-CNT
      D             " WS-SECTOR-MOON-MAX       ->"
      D               WS-SECTOR-MOON-MAX.

      D     DISPLAY "WS-SECTOR-NOVA-CNT       ->"
      D              WS-SECTOR-NOVA-CNT
      D             " WS-SECTOR-NOVA-MAX       ->"
      D               WS-SECTOR-NOVA-MAX.

      D     DISPLAY "WS-SECTOR-PLANET-CNT     ->"
      D              WS-SECTOR-PLANET-CNT
      D             " WS-SECTOR-PLANET-MAX     ->"
      D               WS-SECTOR-PLANET-MAX.

      D     DISPLAY "WS-SECTOR-ROMULAN-CNT    ->"
      D              WS-SECTOR-ROMULAN-CNT
      D             " WS-SECTOR-ROMULAN-MAX    ->"
      D               WS-SECTOR-ROMULAN-MAX.

      D     DISPLAY "WS-SECTOR-STAR-CNT       ->"
      D              WS-SECTOR-STAR-CNT
      D             " WS-SECTOR-STAR-MAX       ->"
      D               WS-SECTOR-STAR-MAX.

      D     DISPLAY "WS-SECTOR-STAR-BASE-CNT  ->"
      D              WS-SECTOR-STAR-BASE-CNT
      D             " WS-SECTOR-STAR-BASE-MAX  ->"
      D               WS-SECTOR-STAR-BASE-MAX.

      D     DISPLAY "WS-SECTOR-STAR-SHIP-CNT  ->"
      D              WS-SECTOR-STAR-SHIP-CNT
      D             " WS-SECTOR-STAR-SHIP-MAX  ->"
      D               WS-SECTOR-STAR-SHIP-MAX.

      D     DISPLAY "WS-SECTOR-UNKNOWN-CNT    ->"
      D               WS-SECTOR-UNKNOWN-CNT
      D             " WS-SECTOR-UNKNOWN-MAX    ->"
      D               WS-SECTOR-UNKNOWN-MAX.

      D     DISPLAY "WS-SECTOR-VULCAN-CNT     ->"
      D              WS-SECTOR-VULCAN-CNT
      D             " WS-SECTOR-VULCAN-MAX     ->"
      D              WS-SECTOR-VULCAN-MAX.

      D     DISPLAY "WS-SECTOR-WORMHOLE-CNT   ->"
      D              WS-SECTOR-WORMHOLE-CNT
      D             " WS-SECTOR-WORMHOLE-MAX   ->"
      D               WS-SECTOR-WORMHOLE-MAX.

       1010-END.
            EXIT.

      * ----------------------------------------------------------------

       1020-BUILD-SECTOR-ARRAY SECTION.

            MOVE HIGH-VALUES TO WS-SECTOR-TABLE-ARRAY.

            PERFORM VARYING WS-SECTOR-X-IDX FROM 1 BY 1
                    UNTIL WS-SECTOR-X-IDX GREATER WS-SECTOR-MAX
                  PERFORM VARYING WS-SECTOR-Y-IDX FROM 1 BY 1
                          UNTIL WS-SECTOR-Y-IDX GREATER WS-SECTOR-MAX
                        PERFORM VARYING WS-SECTOR-Z-IDX FROM 1 BY 1
                                UNTIL WS-SECTOR-Z-IDX GREATER 
                                      WS-SECTOR-MAX
                               PERFORM 1030-POPULATE-SECTOR-CELL THRU
                                       1030-END
                        END-PERFORM
                  END-PERFORM 
            END-PERFORM.

       1020-END.
            EXIT.

      * ----------------------------------------------------------------

       1030-POPULATE-SECTOR-CELL SECTION.
       
            MOVE WS-GALAXY-X-IDX TO WS-GALAXY-X-AXIS-IDX
                (WS-SECTOR-X-IDX, WS-SECTOR-Y-IDX, WS-SECTOR-Z-IDX).
            MOVE WS-GALAXY-Y-IDX TO WS-GALAXY-Y-AXIS-IDX
                (WS-SECTOR-X-IDX, WS-SECTOR-Y-IDX, WS-SECTOR-Z-IDX).
            MOVE WS-GALAXY-Z-IDX TO WS-GALAXY-Z-AXIS-IDX
                (WS-SECTOR-X-IDX, WS-SECTOR-Y-IDX, WS-SECTOR-Z-IDX).

            MOVE WS-SECTOR-X-IDX TO WS-SECTOR-X-AXIS-IDX
                (WS-SECTOR-X-IDX, WS-SECTOR-Y-IDX, WS-SECTOR-Z-IDX).
            MOVE WS-SECTOR-Y-IDX TO WS-SECTOR-Y-AXIS-IDX
                (WS-SECTOR-X-IDX, WS-SECTOR-Y-IDX, WS-SECTOR-Z-IDX).
            MOVE WS-SECTOR-Z-IDX TO WS-SECTOR-Z-AXIS-IDX
                (WS-SECTOR-X-IDX, WS-SECTOR-Y-IDX, WS-SECTOR-Z-IDX).

            PERFORM 1100-PLACE-STATIC-ITEMS THRU
                    1100-END.

            IF WS-SECTOR-CELL-CONTENTS
              (WS-SECTOR-X-IDX, WS-SECTOR-Y-IDX, WS-SECTOR-Z-IDX) =
               HIGH-VALUES
               PERFORM 1200-RANDOMIZE-SECTOR-CONTENTS THRU
                       1200-END
            END-IF.

       1030-END.
            EXIT.

      * ----------------------------------------------------------------

       1100-PLACE-STATIC-ITEMS SECTION.

      *      SECTOR-ENTERPRISE
      *      SECTOR-GALAXY-BARRIER
      *      SECTOR-NEUTRAL-ZONE
      *      SECTOR-STAR-FLEET


       1100-END.
            EXIT.

      * ----------------------------------------------------------------

       1200-RANDOMIZE-SECTOR-CONTENTS SECTION.

            COMPUTE WS-RANDOM-NUMBER = FUNCTION RANDOM * 50 + 1.

            EVALUATE WS-RANDOM-NUMBER-INTEGER
                WHEN 1
                     SET WS-SECTOR-ANOMALLY TO TRUE
                     ADD 1 TO WS-SECTOR-ANOMALLY-CNT
                WHEN 2
                     SET WS-SECTOR-ASTEROID TO TRUE
                     ADD 1 TO WS-SECTOR-ASTEROID-CNT
                WHEN 3
                     SET WS-SECTOR-BLACK-HOLE TO TRUE
                     ADD 1 TO WS-SECTOR-BLACK-HOLE-CNT
                WHEN 4
                     SET WS-SECTOR-COLONY TO TRUE
                     ADD 1 TO WS-SECTOR-COLONY-CNT
                WHEN 5
                     SET WS-SECTOR-COMET TO TRUE
                     ADD 1 TO WS-SECTOR-COMET-CNT
                WHEN 6
                     SET WS-SECTOR-KLINGON TO TRUE
                     ADD 1 TO WS-SECTOR-KLINGON-CNT
                WHEN 7
                     SET WS-SECTOR-MOON TO TRUE
                     ADD 1 TO WS-SECTOR-MOON-CNT
                WHEN 8
                     SET WS-SECTOR-NOVA TO TRUE
                     ADD 1 TO WS-SECTOR-NOVA-CNT
                WHEN 9
                     SET WS-SECTOR-PLANET TO TRUE
                     ADD 1 TO WS-SECTOR-PLANET-CNT
                WHEN 10
                     SET WS-SECTOR-ROMULAN TO TRUE
                     ADD 1 TO WS-SECTOR-ROMULAN-CNT
                WHEN 11
                     SET WS-SECTOR-STAR TO TRUE
                     ADD 1 TO WS-SECTOR-STAR-CNT
                WHEN 12
                     SET WS-SECTOR-STAR-BASE TO TRUE
                     ADD 1 TO WS-SECTOR-STAR-BASE-CNT
                WHEN 13
                     SET WS-SECTOR-STAR-SHIP TO TRUE
                     ADD 1 TO WS-SECTOR-STAR-SHIP-CNT
                WHEN 14
                     SET WS-SECTOR-UNKNOWN TO TRUE
                     ADD 1 TO WS-SECTOR-UNKNOWN-CNT
                WHEN 16
                     SET WS-SECTOR-VULCAN TO TRUE
                     ADD 1 TO WS-SECTOR-VULCAN-CNT
                WHEN 17
                     SET WS-SECTOR-WORMHOLE TO TRUE
                     ADD 1 TO WS-SECTOR-WORMHOLE-CNT
                WHEN OTHER
                     SET WS-SECTOR-EMPTY TO TRUE
            END-EVALUATE.

            MOVE WS-SECTOR-CONTENTS TO WS-SECTOR-CELL-CONTENTS
                   (WS-SECTOR-X-IDX, WS-SECTOR-Y-IDX, WS-SECTOR-Z-IDX).

      D     DISPLAY "WS-RANDOM-NUMBER->" WS-RANDOM-NUMBER
      D          " WS-RANDOM-NUMBER-INTEGER->" WS-RANDOM-NUMBER-INTEGER
      D          " WS-SECTOR-Z-AXIS->" WS-SECTOR-Z-AXIS
      D          (WS-SECTOR-X-IDX, WS-SECTOR-Y-IDX, WS-SECTOR-Z-IDX).

       1200-END.
            EXIT.

      * ----------------------------------------------------------------

       9000-STOP-PROGRAM SECTION.

            SET WS-STD-FILE-ACTION-CLOSE TO TRUE.
            CLOSE STANDARD-IN.
            SET WS-STD-FILE-STD-IN TO TRUE.
            MOVE WS-STDIN-FILE-STATUS TO WS-STD-FILE-STATUS.
            PERFORM 9010-CHECK-FILE-STATUS THRU
                    9010-END.
            
            CLOSE STANDARD-OUT.
            SET WS-STD-FILE-STD-OUT TO TRUE.
            MOVE WS-STDOUT-FILE-STATUS TO WS-STD-FILE-STATUS.
            PERFORM 9010-CHECK-FILE-STATUS THRU
                    9010-END.
            
            CLOSE STANDARD-ERR.
            SET WS-STD-FILE-STD-ERR TO TRUE.
            MOVE WS-STDERR-FILE-STATUS TO WS-STD-FILE-STATUS.
            PERFORM 9010-CHECK-FILE-STATUS THRU
                    9010-END.


       9000-END.
            EXIT.
      * ----------------------------------------------------------------

       9010-CHECK-FILE-STATUS SECTION.

            STRING WS-STD-FILE-NAME DELIMITED BY SPACES
                  " " DELIMITED BY SIZE
                   WS-STD-FILE-ACTION DELIMITED BY SPACES 
                  " " DELIMITED BY SIZE
                   WS-STD-FILE-STATUS DELIMITED BY SIZE
              INTO WS-STD-FILE-INFO.
            
      D     DISPLAY WS-STD-FILE-INFO.
            




       9010-END.
            EXIT.       
      * ----------------------------------------------------------------
        

            END PROGRAM STARTREK.

      * ****************************************************************
      *                      End Program STARTREK
      * ****************************************************************
