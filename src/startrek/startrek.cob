      * ************************************************************** *
      *                    START OF PROGRAM STARTREK                   *
      * ************************************************************** *
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    STARTREK.
       AUTHOR.        STAR FLEET ADMIRAL Q.
       INSTALLATION.  HOME PC DEVELOPER.
       DATE-WRITTEN.  SUNDAY SEPTEMBER 05, 2021.
       DATE-COMPILED. FUNCTION WHEN-COMPILED.
      *
      * ************************************************************** *
      *
      * ************************************************************** *
      *                       PROGRAM DESCRIPTION                      *
      * -------------------------------------------------------------- *
      * STARTREK IS AN OUTER SPACE SIMULATION GAME WRITTEN IN COBOL.   *
      *  YOU WILL BE SELECTED USING VARIOUS COMMANDS TO PILOT THE      *
      *  FEDERATION BATTLE CRUISER U.S.S. ENTERPRISE, AND THROUGH      *
      *  VARIOUS OFFENSIVE AND DEFENSIVE COMMANDS, TRAVEL THROUGHOUT   *
      *  THE GALAXY ON A MISSION TO DESTROY ANY ENEMIES THREATENING    *
      *  THE UNITED FEDERATION OF PLANETS, EXPLORE NEW WORLDS AND GAIN *
      *  NEW ALLIES IN A SEARCH FOR GALACITC PEACE.                    *
      * -------------------------------------------------------------- *
      *                         PROGRAM HISTORY                        *
      *                                                                *
      *   DATE   INT                    REASON                         *
      * -------- --- ------------------------------------------------- *
      * 20210905 Q   PROGRAM WRITTEN.                                  *
      *                                                                *
      * ************************************************************** *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. IBM-PC.
      * SET ENVIRONMENT VARIABLE COB_SET_DEBUG=Y, y OR 1 TO ACTIVATE
      *  DEBUGGING LINES DURING RUNTIME (THOSE WITH D IN COLUMN 7)
      *  PROVIDED THE PROGRAM WAS COMPILED WITH THE -FDEBUGGING-LINE
      *  AND THE -D OR -DEBUG SWITCH.
       SOURCE-COMPUTER. IBM-PC DEBUGGING MODE.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.
       REPOSITORY.
       FUNCTION ALL INTRINSIC.
      *
      * -------------------------------------------------------------- *
      *
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
       ASSIGN TO STARTREK-ERROR
       ORGANIZATION LINE SEQUENTIAL
       STATUS WS-STDERR-FILE-STATUS.
      *
      * ************************************************************** *
      *
       DATA DIVISION.

       FILE SECTION.

       FD STANDARD-IN.
          01 STDIN-LINE             PIC X(80).

       FD STANDARD-OUT.
          01 STDOUT-LINE            PIC X(80).

       FD STANDARD-ERR.
          01 STDERR-LINE            PIC X(80).
      *
      * -------------------------------------------------------------- *
      *
       WORKING-STORAGE SECTION.

       01 WS-BLANK-LINE             PIC  X(80)  VALUE SPACES.
       01 WS-PAUSE                  PIC  X(01)  VALUE SPACES.

       01 WS-RANDOM-NUMBERS.
          03 WS-RANDOM-NUMBER       PIC 9(19)V9(19).
          03 WS-RANDOM-NBR REDEFINES
             WS-RANDOM-NUMBER.
             05 WS-RDM-NBR-INT      PIC 9(19).
             05 WS-RDM-NBR-DEC      PIC 9(19).
          03 WS-RANDOM-SEED         PIC 9(19)V9(19).
          03 WS-RANDOM-TEMP         PIC 9(19)V9(19).

       01 WS-STD-FILE-STATUSES.
          03 WS-STDIN-FILE-STATUS.
             05 WS-STDIN-STATUS-1   PIC  9(01)  VALUE ZEROES.
             05 WS-STDIN-STATUS-2   PIC  9(01)  VALUE ZEROES.
             05 WS-STDIN-STATUS-2-RED REDEFINES
                WS-STDIN-STATUS-2   PIC  9(02)  COMP-5.
          03 WS-STDOUT-FILE-STATUS.
             05 WS-STDOUT-STATUS-1  PIC  9(01)  VALUE ZEROES.
             05 WS-STDOUT-STATUS-2  PIC  9(01)  VALUE ZEROES.
             05 WS-STDOUT-STATUS-2-RED REDEFINES
                WS-STDOUT-STATUS-2  PIC  9(02)  COMP-5.
          03 WS-STDERR-FILE-STATUS.
             05 WS-STDERR-STATUS-1  PIC  9(01)  VALUE ZEROES.
             05 WS-STDERR-STATUS-2  PIC  9(01)  VALUE ZEROES.
             05 WS-STDERR-STATUS-2-RED REDEFINES
                WS-STDERR-STATUS-2  PIC  9(02)  COMP-5.

       01 WS-NAME                   PIC  X(30)  VALUE ZEROES.

       01 WS-RANK-CHOICES           PIC  9(02)  VALUE ZEROES.
          88 WS-RANK-CHOICE-0       VALUE  0.
          88 WS-RANK-CHOICE-1       VALUE  1.
          88 WS-RANK-CHOICE-2       VALUE  2.
          88 WS-RANK-CHOICE-3       VALUE  3.
          88 WS-RANK-CHOICE-4       VALUE  4.
          88 WS-RANK-CHOICE-5       VALUE  5.
          88 WS-RANK-CHOICE-6       VALUE  6.
          88 WS-RANK-CHOICE-7       VALUE  7.
          88 WS-RANK-CHOICE-8       VALUE  8.
          88 WS-RANK-CHOICE-9       VALUE  9.
          88 WS-RANK-CHOICE-10      VALUE 10.
          88 WS-RANK-CHOICE-11      VALUE 11.
          88 WS-RANK-CHOICE-12      VALUE 12.
          88 WS-RANK-CHOICE-13      VALUE 13.
          88 WS-RANK-CHOICE-14      VALUE 14.
          88 WS-RANK-CHOICE-15      VALUE 15.
          88 WS-RANK-CHOICE-16      VALUE 16.
          88 WS-RANK-CHOICE-17      VALUE 17.
          88 WS-RANK-CHOICE-VALID   VALUES 0 THRU 17.

       01 WS-RANK                   PIC  X(25)  VALUE SPACES.
          88 WS-RANK-TRAINEE        VALUE 'Trainee                  '.
          88 WS-RANK-WARRANT-2      VALUE 'Warrant Officer 2        '.
          88 WS-RANK-WARRANT-3      VALUE 'Warrant Officer 3        '.
          88 WS-RANK-WARRANT-4      VALUE 'Warrant Officer 4        '.
          88 WS-RANK-WARRANT-5      VALUE 'Warrant Officer 5        '.
          88 WS-RANK-ENSIGN         VALUE 'Ensign                   '.
          88 WS-RANK-LT-JR          VALUE 'Lt. Junior Grade         '.
          88 WS-RANK-LT             VALUE 'Lieutenant               '.
          88 WS-RANK-LT-CMDR        VALUE 'Lt. Commander            '.
          88 WS-RANK-CMDR           VALUE 'Commander                '.
          88 WS-RANK-CAPT           VALUE 'Captain                  '.
          88 WS-RANK-COMMODORE      VALUE 'Commodore                '.
          88 WS-RANK-REAR-LOWER     VALUE 'Rear Admiral Lower Half  '.
          88 WS-RANK-REAR-UPPER     VALUE 'Rear Admiral Upper Half  '.
          88 WS-RANK-VICE           VALUE 'Vice Admiral             '.
          88 WS-RANK-ADMIRAL        VALUE 'Admiral                  '.
          88 WS-RANK-FLEET          VALUE 'Fleet Admiral            '.
          88 WS-RANK-SF-CMD         VALUE 'Commander, Star Fleet    '.

       01 WS-RANK-NAME              PIC  X(55)  VALUE SPACES.
       01 WS-DIFFICULTY-FACTOR      PIC  9(02)  VALUE ZEROES.
       01 WS-DIFFICULTY-FACTOR-SQD  PIC  9(04)  VALUE ZEROES.

       01 WS-SECTOR-ITEMS           PIC  X(01)  VALUE SPACES.
          88 WS-IS-ANOMALLY         VALUE '.'.
          88 WS-IS-ASTEROID         VALUE 'a'.
          88 WS-IS-BLACK-HOLE       VALUE 'b'.
          88 WS-IS-COMET            VALUE 'c'.
          88 WS-IS-EMPTY-SPACE      VALUE '0'.
          88 WS-IS-ENTERPRISE       VALUE '@'.
          88 WS-IS-GALAXY-BARRIER   VALUE '+'.
          88 WS-IS-MOON             VALUE 'm'.
          88 WS-IS-NEUTRAL-ZONE     VALUE '-'.
          88 WS-IS-NEXUS            VALUE 'x'.
          88 WS-IS-NOVA             VALUE 'n'.
          88 WS-IS-PLANET           VALUE 'p'.
          88 WS-IS-STAR             VALUE '*'.
          88 WS-IS-STAR-BASE        VALUE '#'.
          88 WS-IS-STAR-SHIP        VALUE '^'.
          88 WS-IS-STAR-FLEET       VALUE '$'.
          88 WS-IS-SUPER-NOVA       VALUE 's'.
          88 WS-IS-WORM-HOLE        VALUE 'w'.
          88 WS-IS-UNKNOWN          VALUE 'u'.

       01 WS-IS-ENEMY REDEFINES
          WS-SECTOR-ITEMS           PIC  X(01).
          88 WS-IS-BORG             VALUE 'B'.
          88 WS-IS-CARD             VALUE 'C'.
          88 WS-IS-ENEMY-1          VALUE '1'.
          88 WS-IS-ENEMY-2          VALUE '2'.
          88 WS-IS-KLINGON          VALUE 'K'.
          88 WS-IS-NO-ENEMY         VALUE '0'.
          88 WS-IS-Q                VALUE 'Q'.
          88 WS-IS-ROMULAN          VALUE 'R'.
          88 WS-IS-VGER             VALUE 'V'.

       01 WS-IS-ALLIED REDEFINES
          WS-SECTOR-ITEMS           PIC  X(01).
          88 WS-IS-FERI             VALUE 'f'.
          88 WS-IS-KOBIASHI         VALUE '!'.
          88 WS-IS-NO-ALLIED        VALUE '0'.
          88 WS-IS-VULCAN           VALUE 'v'.

       01 WS-STAR-SHIPS.
          03 WS-STARSHIP-REGISTER   PIC X(08)  VALUE SPACES.
          03 FILLER                 PIC X(01)  VALUE SPACES.
          03 WS-STARSHIP-NAME       PIC X(16)  VALUE SPACES.

       01 WS-CONSTELLATION-SHIPS REDEFINES
          WS-STAR-SHIPS             PIC X(25).
          88 WS-SHIP-CONSTELLATION1 VALUE 'NCC-1974 Constellation   '.
          88 WS-SHIP-GETTYSBURG     VALUE 'NCC-3890 Gettysburg      '.
          88 WS-SHIP-HATHAWAY       VALUE 'NCC-2593 Hathaway        '.
          88 WS-SHIP-MAGELLAN       VALUE 'NCC-3069 Magellan        '.
          88 WS-SHIP-STARGAZER      VALUE 'NCC-2893 Stargazer       '.
          88 WS-SHIP-VICTORY        VALUE 'NCC-9754 Victory         '.

       01 WS-CONSITUTION-SHIPS REDEFINES
          WS-STAR-SHIPS             PIC X(25).
          88 WS-SHIP-CONSTELLATION2 VALUE 'NCC-1017 Constellation   '.
          88 WS-SHIP-CONSTITUTION   VALUE 'NCC-1700 Constitution    '.
          88 WS-SHIP-DEFIANT        VALUE 'NCC-1764 Defiant         '.
          88 WS-SHIP-ENTEPRISE      VALUE 'NCC-1701 Enterprise      '.
          88 WS-SHIP-EXCALIBUR      VALUE 'NCC-1664 Excalibur       '.
          88 WS-SHIP-EXETER         VALUE 'NCC-1706 Exeter          '.
          88 WS-SHIP-FARRAGUT       VALUE 'NCC-1647 Farragut        '.
          88 WS-SHIP-KONGO          VALUE 'NCC-1710 Kongo           '.
          88 WS-SHIP-LEXINGTON      VALUE 'NCC-1709 Lexington       '.
          88 WS-SHIP-POTEMKIN       VALUE 'NCC-1657 Potemkin        '.
          88 WS-SHIP-YORKTOWN       VALUE 'NCC-1717 Yorktown        '.

       01 WS-EXCELSIOR-SHIPS REDEFINES
          WS-STAR-SHIPS             PIC X(25).
          88 WS-SHIP-EXCELSIOR      VALUE 'NCC-2000 Excelsior       '.

       01 WS-MIRANDA-SHIPS REDEFINES
          WS-STAR-SHIPS             PIC X(25).
          88 WS-SHIP-HELIN          VALUE 'NCC-1692 Helin           '.
          88 WS-SHIP-LANTREE        VALUE 'NCC-1837 Lantree         '.
          88 WS-SHIP-RELIANT        VALUE 'NCC-1864 Reliant         '.
          88 WS-SHIP-SARATOGA       VALUE 'NCC-1867 Saratoga        '.
          88 WS-SHIP-SITAK          VALUE 'NCC-1924 Sitak           '.

       01 WS-SOYUZ-SHIPS REDEFINES
          WS-STAR-SHIPS             PIC X(25).
          88 WS-SHIP-BOZEMAN        VALUE 'NCC-1941 Bozeman         '.

       01 WS-SYDNEY-SHIPS REDEFINES
          WS-STAR-SHIPS             PIC X(25) .
          88 WS-SHIP-JENOLAN        VALUE 'NCC-2010 Jenolan         '.

       01 WS-FLAG-SWITCHES.
          03 WS-EOF          BINARY PIC  9(01)  VALUE ZEROES.
             88 WS-EOF-FALSE         VALUE 0.
             88 WS-EOF-TRUE          VALUE 1.

       01 WS-GALAXY-ARRAY.
          03 WS-QUADRANT            OCCURS  4 TIMES.
             05 WS-QUAD-X-AXIS      OCCURS 40 TIMES.
                07 WS-QUAD-Y-AXIS   OCCURS 40 TIMES.
                   09 WS-QUAD-Z-AXIS
                                    OCCURS 40 TIMES.
                      11 WS-QUAD-SECTOR-ID.
                         13 WS-QUAD-ID
                                    PIC  9(01)         VALUE ZEROES.
                         13 WS-QUAD-X-ID
                                    PIC  9(02)         VALUE ZEROES.
                         13 WS-QUAD-Y-ID
                                    PIC  9(02)         VALUE ZEROES.
                         13 WS-QUAD-Z-ID
                                    PIC  9(02)         VALUE ZEROES.
                      11 WS-QUAD-SECTOR-CONTENTS
                                    PIC  X(1000)       VALUE SPACES.

        01 WS-SECTOR-ARRAY.
           03 WS-SECTOR-ID.
              05 WS-SECTOR-QUAD     PIC  9(01)         VALUE ZEROES.
                 88 WS-ALPHA-QUAD                      VALUE 1.
                 88 WS-BETA-QUAD                       VALUE 2.
                 88 WS-GAMMA-QUAD                      VALUE 3.
                 88 WS-DELTA-QUAD                      VALUE 4.
              05 WS-SECTOR-QUAD-X   PIC  9(02)         VALUE ZEROES.
              05 WS-SECTOR-QUAD-Y   PIC  9(02)         VALUE ZEROES.
              05 WS-SECTOR-QUAD-Z   PIC  9(02)         VALUE ZEROES.
           03 WS-SECTOR-CONTENTS    PIC  X(1000)       VALUE SPACES.
           03 WS-SECTOR-CONTENTS-RED REDEFINES
              WS-SECTOR-CONTENTS.
              05 WS-SECTOR-X-AXIS   OCCURS 10 TIMES.
                 07 WS-SECTOR-Y-AXIS
                                    OCCURS 10 TIMES.
                    09 WS-SECTOR-Z-AXIS
                                    OCCURS 10 TIMES.
                       11 WS-SECTOR-LOCATION
                                    PIC  X(01).

       01 WS-COUNTERS.
          03 WS-QUAD-CTR            PIC  9(01)         VALUE ZEROES.
             88 WS-QUAD-ALPHA       VALUE 1.
             88 WS-QUAD-BETA        VALUE 2.
             88 WS-QUAD-GAMMA       VALUE 3.
             88 WS-QUAD-DELTA       VALUE 4.
          03 WS-QUAD-X-CTR          PIC  9(02)         VALUE ZEROES.
          03 WS-QUAD-Y-CTR          PIC  9(02)         VALUE ZEROES.
          03 WS-QUAD-Z-CTR          PIC  9(02)         VALUE ZEROES.
          03 WS-SECTOR-X-CTR        PIC  9(02)         VALUE ZEROES.
          03 WS-SECTOR-Y-CTR        PIC  9(02)         VALUE ZEROES.
          03 WS-SECTOR-Z-CTR        PIC  9(02)         VALUE ZEROES.
          03 WS-ITEM-COUNTER.
             05 WS-IS-ANOMALLY-CTR     PIC  9(09)      VALUE ZEROES.
             05 WS-IS-ASTEROID-CTR     PIC  9(09)      VALUE ZEROES.
             05 WS-IS-BLACK-HOLE-CTR   PIC  9(09)      VALUE ZEROES.
             05 WS-IS-BORG-CTR         PIC  9(09)      VALUE ZEROES.
             05 WS-IS-CARD-CTR         PIC  9(09)      VALUE ZEROES.
             05 WS-IS-COMET-CTR        PIC  9(09)      VALUE ZEROES.
             05 WS-IS-ENEMY-1-CTR      PIC  9(09)      VALUE ZEROES.
             05 WS-IS-ENEMY-2-CTR      PIC  9(09)      VALUE ZEROES.
             05 WS-IS-FERI-CTR         PIC  9(09)      VALUE ZEROES.
             05 WS-IS-KLINGON-CTR      PIC  9(09)      VALUE ZEROES.
             05 WS-IS-KOBIASHI-CTR     PIC  9(09)      VALUE ZEROES.
             05 WS-IS-MOON-CTR         PIC  9(09)      VALUE ZEROES.
             05 WS-IS-NEXUS-CTR        PIC  9(09)      VALUE ZEROES.
             05 WS-IS-NOVA-CTR         PIC  9(09)      VALUE ZEROES.
             05 WS-IS-PLANET-CTR       PIC  9(09)      VALUE ZEROES.
             05 WS-IS-Q-CTR            PIC  9(09)      VALUE ZEROES.
             05 WS-IS-ROMULAN-CTR      PIC  9(09)      VALUE ZEROES.
             05 WS-IS-STAR-CTR         PIC  9(09)      VALUE ZEROES.
             05 WS-IS-STAR-BASE-CTR    PIC  9(09)      VALUE ZEROES.
             05 WS-IS-STAR-SHIP-CTR    PIC  9(09)      VALUE ZEROES.
             05 WS-IS-SUPER-NOVA-CTR   PIC  9(09)      VALUE ZEROES.
             05 WS-IS-UNKNOWN-CTR      PIC  9(09)      VALUE ZEROES.
             05 WS-IS-VGER-CTR         PIC  9(09)      VALUE ZEROES.
             05 WS-IS-VULCAN-CTR       PIC  9(09)      VALUE ZEROES.
             05 WS-IS-WORM-HOLE-CTR    PIC  9(09)      VALUE ZEROES.
          03 WS-ITEM-MAX-COUNTER.
             05 WS-IS-ANOMALLY-MAX     PIC  9(09)      VALUE ZEROES.
             05 WS-IS-ASTEROID-MAX     PIC  9(09)      VALUE ZEROES.
             05 WS-IS-BLACK-HOLE-MAX   PIC  9(09)      VALUE ZEROES.
             05 WS-IS-BORG-MAX         PIC  9(09)      VALUE ZEROES.
             05 WS-IS-CARD-MAX         PIC  9(09)      VALUE ZEROES.
             05 WS-IS-COMET-MAX        PIC  9(09)      VALUE ZEROES.
             05 WS-IS-ENEMY-1-MAX      PIC  9(09)      VALUE ZEROES.
             05 WS-IS-ENEMY-2-MAX      PIC  9(09)      VALUE ZEROES.
             05 WS-IS-FERI-MAX         PIC  9(09)      VALUE ZEROES.
             05 WS-IS-KLINGON-MAX      PIC  9(09)      VALUE ZEROES.
             05 WS-IS-KOBIASHI-MAX     PIC  9(09)      VALUE ZEROES.
             05 WS-IS-MOON-MAX         PIC  9(09)      VALUE ZEROES.
             05 WS-IS-NEXUS-MAX        PIC  9(09)      VALUE ZEROES.
             05 WS-IS-NOVA-MAX         PIC  9(09)      VALUE ZEROES.
             05 WS-IS-PLANET-MAX       PIC  9(09)      VALUE ZEROES.
             05 WS-IS-Q-MAX            PIC  9(09)      VALUE ZEROES.
             05 WS-IS-ROMULAN-MAX      PIC  9(09)      VALUE ZEROES.
             05 WS-IS-STAR-MAX         PIC  9(09)      VALUE ZEROES.
             05 WS-IS-STAR-BASE-MAX    PIC  9(09)      VALUE ZEROES.
             05 WS-IS-STAR-SHIP-MAX    PIC  9(09)      VALUE ZEROES.
             05 WS-IS-SUPER-NOVA-MAX   PIC  9(09)      VALUE ZEROES.
             05 WS-IS-UNKNOWN-MAX      PIC  9(09)      VALUE ZEROES.
             05 WS-IS-VGER-MAX         PIC  9(09)      VALUE ZEROES.
             05 WS-IS-VULCAN-MAX       PIC  9(09)      VALUE ZEROES.
             05 WS-IS-WORM-HOLE-MAX    PIC  9(09)      VALUE ZEROES.

       01 WS-CONSTANTS.
          03 WS-QUAD-MAX            PIC  9(01)         VALUE 4.
          03 WS-SECTOR-MAX          PIC  9(02)         VALUE 10.
          03 WS-CONTENT-ID          PIC  9(02)         VALUE 90.

       01 WS-CURRENT-DATE-TIME-OFFSET
                                    PIC X(21)          VALUE SPACES.
       01 WS-CURRENT-DATE-TIME-OFFSET-RED REDEFINES
          WS-CURRENT-DATE-TIME-OFFSET.
          03 WS-CURRENT-DATE.
             05 WS-CURRENT-DATE-CC   PIC   9(02).
             05 WS-CURRENT-DATE-YY   PIC   9(02).
             05 WS-CURRENT-DATE-MM   PIC   9(02).
             05 WS-CURRENT-DATE-DD   PIC   9(02).
          03 WS-CURRENT-TIME.
             05 WS-CURRENT-TIME-HH   PIC   9(02).
             05 WS-CURRENT-TIME-MM   PIC   9(02).
             05 WS-CURRENT-TIME-SS   PIC   9(02).
             05 WS-CURRENT-TIME-MS   PIC   9(02).
          03 WS-CURRENT-OFFSET.
             05 WS-CURRENT-OFFSET-PP-MM
                                     PIC   X(01).
             05 WS-CURRENT-OFFSET-HH PIC   9(02).
             05 WS-CURRENT-OFFSET-MM PIC   9(02).
      *
      * -------------------------------------------------------------- *
      *
       LOCAL-STORAGE SECTION.
      *
      * -------------------------------------------------------------- *
      *
       LINKAGE SECTION.
      *
      * -------------------------------------------------------------- *
      *
       REPORT SECTION.
      *
      * -------------------------------------------------------------- *
      *
       SCREEN SECTION.
      *
      * ************************************************************** *
      *
       PROCEDURE DIVISION.

       1000-START-PROGRAM SECTION.

           DISPLAY 'Space, the final frontier!!!'.
           DISPLAY 'These are the voyages of the starship Enterprise.'.
           DISPLAY 'An ongoing mission, to explore strange new worlds.'.
           DISPLAY 'To seek out new life and new civilizations.'.
           DISPLAY 'To boldly go where no one has gone before!!!'.
           DISPLAY WS-BLANK-LINE.
           DISPLAY WS-BLANK-LINE.

           DISPLAY 'Please enter your name: '.
           ACCEPT WS-NAME FROM CONSOLE.

       1000-RANK-LOOP.
           DISPLAY WS-BLANK-LINE.
           DISPLAY WS-BLANK-LINE.
           DISPLAY 'Please enter your rank: '.
           DISPLAY ' 0 Trainee'.
           DISPLAY ' 1 Warrant Officer 2'.
           DISPLAY ' 2 Warrant Officer 3'.
           DISPLAY ' 3 Warrant Officer 4'.
           DISPLAY ' 4 Warrant Officer 5'.
           DISPLAY ' 5 Ensign'.
           DISPLAY ' 6 Lieutenant, Junior Grade'.
           DISPLAY ' 7 Lieutenant'.
           DISPLAY ' 8 Lieutenant Commander'.
           DISPLAY ' 9 Commander'.
           DISPLAY '10 Captain'.
           DISPLAY '11 Commodore'.
           DISPLAY '12 Rear Admiral Lower Half'.
           DISPLAY '13 Admiral Upper Half'.
           DISPLAY '14 Vice Admiral'.
           DISPLAY '15 Admiral'.
           DISPLAY '16 Fleet Admiral'.
           DISPLAY '17 Commander, Star Fleet'.
           DISPLAY 'Note: the higher your rank the more difficult the '.
           DISPLAY 'missions become. ...'.
           DISPLAY WS-BLANK-LINE.
           DISPLAY 'Please enter your choice:'.
           ACCEPT WS-RANK-CHOICES FROM CONSOLE.

           IF NOT WS-RANK-CHOICE-VALID
              DISPLAY WS-BLANK-LINE
              DISPLAY WS-BLANK-LINE
              DISPLAY 'Invalid rank choice: ' WS-RANK
              MOVE LOW-VALUES TO WS-RANK
              DISPLAY 'Please select a valid rank value (0 through 17)!'
              GO TO 1000-RANK-LOOP
           END-IF.

           PERFORM 1010-FORMAT-RANK-NAME
              THRU 1010-END.

           DISPLAY WS-BLANK-LINE.
           DISPLAY WS-BLANK-LINE.
           DISPLAY 'Welcome aboard ' WS-RANK-NAME.
           DISPLAY WS-BLANK-LINE.
           DISPLAY 'Congratulations - you have been appointed'.
           DISPLAY 'to command the U.S.S. Enterprise,'.
           DISPLAY 'registration number NCC-1701 refit'.
           DISPLAY 'from Star Trek The Motion Picture'.
           DISPLAY "after the encounter with V'Ger (Voyager 6).".
           DISPLAY WS-BLANK-LINE.
           DISPLAY 'Your mission is to perform the duties assigned'.
           DISPLAY 'as directed by those appointed above you. ...'.

           DISPLAY WS-BLANK-LINE.
           DISPLAY WS-BLANK-LINE.
           DISPLAY 'Press <ENTER> key to continue. ...'.
           ACCEPT WS-PAUSE FROM CONSOLE.

           PERFORM 1020-COMPUTE-RANDOM-SEED
              THRU 1020-END.

           PERFORM 1040-BUILD-GALAXY
              THRU 1040-END.

           PERFORM 9990-STOP-PROGRAM
              THRU 9990-END.

       1000-END.
           EXIT.
      *
      * -------------------------------------------------------------- *
      *
       1010-FORMAT-RANK-NAME SECTION.

           EVALUATE TRUE
               WHEN WS-RANK-CHOICE-0
                SET WS-RANK-TRAINEE TO TRUE
               WHEN WS-RANK-CHOICE-1
                SET WS-RANK-WARRANT-2 TO TRUE
               WHEN WS-RANK-CHOICE-2
                SET WS-RANK-WARRANT-3 TO TRUE
               WHEN WS-RANK-CHOICE-3
                SET WS-RANK-WARRANT-4 TO TRUE
               WHEN WS-RANK-CHOICE-4
                SET WS-RANK-WARRANT-5 TO TRUE
               WHEN WS-RANK-CHOICE-5
                SET WS-RANK-ENSIGN TO TRUE
               WHEN WS-RANK-CHOICE-6
                SET WS-RANK-LT-JR TO TRUE
               WHEN WS-RANK-CHOICE-7
                SET WS-RANK-LT TO TRUE
               WHEN WS-RANK-CHOICE-8
                SET WS-RANK-LT-CMDR TO TRUE
               WHEN WS-RANK-CHOICE-9
                SET WS-RANK-CMDR TO TRUE
               WHEN WS-RANK-CHOICE-10
                SET WS-RANK-CAPT TO TRUE
               WHEN WS-RANK-CHOICE-11
                SET WS-RANK-COMMODORE TO TRUE
               WHEN WS-RANK-CHOICE-12
                SET WS-RANK-REAR-LOWER TO TRUE
               WHEN WS-RANK-CHOICE-13
                SET WS-RANK-REAR-UPPER TO TRUE
               WHEN WS-RANK-CHOICE-14
                SET WS-RANK-VICE TO TRUE
               WHEN WS-RANK-CHOICE-15
                SET WS-RANK-ADMIRAL TO TRUE
               WHEN WS-RANK-CHOICE-16
                SET WS-RANK-FLEET TO TRUE
               WHEN WS-RANK-CHOICE-17
                SET WS-RANK-SF-CMD TO TRUE
           END-EVALUATE.

           INSPECT WS-RANK REPLACING TRAILING SPACES BY LOW-VALUES.
           INSPECT WS-NAME REPLACING TRAILING SPACES BY LOW-VALUES.

           STRING WS-RANK DELIMITED BY LOW-VALUES
                    SPACE DELIMITED BY SIZE
                  WS-NAME DELIMITED BY LOW-VALUES
                 ' ! ! !' DELIMITED BY SIZE
             INTO WS-RANK-NAME.

           COMPUTE WS-DIFFICULTY-FACTOR =
                 ((WS-RANK-CHOICES + 1) * 2) + 4.
           COMPUTE WS-DIFFICULTY-FACTOR-SQD =
                   WS-DIFFICULTY-FACTOR * WS-DIFFICULTY-FACTOR.

      D    DISPLAY "WS-DIFFICULTY-FACTOR->" WS-DIFFICULTY-FACTOR.
      D    DISPLAY "WS-DIFFICULTY-FACTOR-SQD->"
      D             WS-DIFFICULTY-FACTOR-SQD.

       1010-END.
           EXIT.
      *
      * -------------------------------------------------------------- *
      *
       1020-COMPUTE-RANDOM-SEED SECTION.

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME-OFFSET.

           MOVE 1.0000000000000000001 TO WS-RANDOM-TEMP.
      D    DISPLAY 'WS-RANDOM-TEMP->'WS-RANDOM-TEMP.

           IF WS-CURRENT-OFFSET-PP-MM  = '+'
              COMPUTE WS-RANDOM-TEMP =
                     (WS-RANK-CHOICES + WS-RANDOM-TEMP) *
                      WS-DIFFICULTY-FACTOR
             ELSE
              COMPUTE WS-RANDOM-TEMP =
                     (WS-RANK-CHOICES - WS-RANDOM-TEMP) *
                      WS-DIFFICULTY-FACTOR
           END-IF.
      D    DISPLAY 'WS-CURRENT-OFFSET-PP-MM->' WS-CURRENT-OFFSET-PP-MM.
      D    DISPLAY 'WS-RANK-CHOICES->' WS-RANK-CHOICES.
      D    DISPLAY 'WS-DIFFICULTY-FACTOR->' WS-DIFFICULTY-FACTOR.
      D    DISPLAY 'WS-RANDOM-TEMP->'WS-RANDOM-TEMP.

           IF WS-CURRENT-DATE-CC NOT = ZEROES
              COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP /
                      WS-CURRENT-DATE-CC
           END-IF.
      D    DISPLAY 'WS-CURRENT-DATE-CC->' WS-CURRENT-DATE-CC.
      D    DISPLAY 'WS-RANDOM-TEMP->'WS-RANDOM-TEMP.

           IF WS-CURRENT-TIME-HH NOT = ZEROES
              COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP *
                      WS-CURRENT-TIME-HH
           END-IF.
      D    DISPLAY 'WS-CURRENT-TIME-HH->' WS-CURRENT-TIME-HH.
      D    DISPLAY 'WS-RANDOM-TEMP->'WS-RANDOM-TEMP.

           IF WS-CURRENT-DATE-YY NOT = ZEROES
              COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP /
                      WS-CURRENT-DATE-YY
           END-IF.
      D    DISPLAY 'WS-CURRENT-DATE-YY->' WS-CURRENT-DATE-YY.
      D    DISPLAY 'WS-RANDOM-TEMP->'WS-RANDOM-TEMP.

           IF WS-CURRENT-TIME-MM NOT = ZEROES
              COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP *
                      WS-CURRENT-TIME-MM
           END-IF.
      D    DISPLAY 'WS-CURRENT-TIME-MM->' WS-CURRENT-TIME-MM.
      D    DISPLAY 'WS-RANDOM-TEMP->'WS-RANDOM-TEMP.

           IF WS-CURRENT-DATE-MM NOT = ZEROES
              COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP /
                      WS-CURRENT-DATE-MM
           END-IF.
      D    DISPLAY 'WS-CURRENT-DATE-MM->' WS-CURRENT-DATE-MM.
      D    DISPLAY 'WS-RANDOM-TEMP->'WS-RANDOM-TEMP.

           IF WS-CURRENT-TIME-SS NOT = ZEROES
              COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP *
                      WS-CURRENT-TIME-SS
           END-IF.
      D    DISPLAY 'WS-CURRENT-TIME-SS->' WS-CURRENT-TIME-SS.
      D    DISPLAY 'WS-RANDOM-TEMP->'WS-RANDOM-TEMP.

           IF WS-CURRENT-DATE-DD NOT = ZEROES
              COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP /
                      WS-CURRENT-DATE-DD
           END-IF.
      D    DISPLAY 'WS-CURRENT-DATE-DD->' WS-CURRENT-DATE-DD.
      D    DISPLAY 'WS-RANDOM-TEMP->'WS-RANDOM-TEMP.

           IF WS-CURRENT-TIME-MS NOT = ZEROES
              COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP *
                      WS-CURRENT-TIME-MS
           END-IF.
      D    DISPLAY 'WS-CURRENT-TIME-MS->' WS-CURRENT-TIME-MS.
      D    DISPLAY 'WS-RANDOM-TEMP->'WS-RANDOM-TEMP.

           IF WS-CURRENT-OFFSET-HH NOT = ZEROES
              COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP /
                      WS-CURRENT-OFFSET-HH
           END-IF.
      D    DISPLAY 'WS-CURRENT-OFFSET-HH->' WS-CURRENT-OFFSET-HH.
      D    DISPLAY 'WS-RANDOM-TEMP->'WS-RANDOM-TEMP.

           IF WS-CURRENT-OFFSET-MM NOT = ZEROES
              COMPUTE WS-RANDOM-TEMP = WS-RANDOM-TEMP /
                      WS-CURRENT-OFFSET-MM
           END-IF.
      D    DISPLAY 'WS-CURRENT-OFFSET-MM->' WS-CURRENT-OFFSET-MM.
      D    DISPLAY 'WS-RANDOM-TEMP->'WS-RANDOM-TEMP.

           COMPUTE WS-RANDOM-SEED =
                   FUNCTION SQRT(WS-RANDOM-TEMP).
           COMPUTE WS-RANDOM-NUMBER =
                   FUNCTION RANDOM (WS-RANDOM-SEED).

      D    DISPLAY 'WS-RANDOM-SEED->' WS-RANDOM-SEED.
      D    DISPLAY 'WS-RANDOM-NUMBER->' WS-RANDOM-NUMBER.

       1020-END.
           EXIT.
      *
      * -------------------------------------------------------------- *
      *
       1040-BUILD-GALAXY SECTION.

           MOVE ZEROES TO WS-GALAXY-ARRAY.

           PERFORM VARYING WS-QUAD-CTR FROM 1 BY 1
             UNTIL WS-QUAD-CTR > WS-QUAD-MAX

             PERFORM 1050-CALCULATE-MAX-ITEMS
                THRU 1050-END

             PERFORM VARYING WS-QUAD-X-CTR FROM 1 BY 1
               UNTIL WS-QUAD-X-CTR > WS-DIFFICULTY-FACTOR

               PERFORM VARYING WS-QUAD-Y-CTR FROM 1 BY 1
                 UNTIL WS-QUAD-Y-CTR > WS-DIFFICULTY-FACTOR

                 PERFORM VARYING WS-QUAD-Z-CTR FROM 1 BY 1
                   UNTIL WS-QUAD-Z-CTR > WS-DIFFICULTY-FACTOR

                   MOVE WS-QUAD-CTR
                     TO WS-QUAD-ID (WS-QUAD-CTR WS-QUAD-X-CTR
                                    WS-QUAD-Y-CTR WS-QUAD-Z-CTR)
                   MOVE WS-QUAD-X-CTR
                     TO WS-QUAD-X-ID (WS-QUAD-CTR WS-QUAD-X-CTR
                                      WS-QUAD-Y-CTR WS-QUAD-Z-CTR)
                   MOVE WS-QUAD-Y-CTR
                     TO WS-QUAD-Y-ID (WS-QUAD-CTR WS-QUAD-X-CTR
                                      WS-QUAD-Y-CTR WS-QUAD-Z-CTR)
                   MOVE WS-QUAD-Z-CTR
                     TO WS-QUAD-Z-ID (WS-QUAD-CTR WS-QUAD-X-CTR
                                      WS-QUAD-Y-CTR WS-QUAD-Z-CTR)

                   PERFORM 1060-POPULATE-SECTOR
                      THRU 1060-END

                 END-PERFORM

               END-PERFORM

             END-PERFORM

      D      DISPLAY WS-QUAD-CTR "WS-ITEM-COUNTER-> "
                     WS-ITEM-COUNTER

      D      DISPLAY WS-QUAD-CTR
      D              "WS-IS-ANOMALLY-CTR->"   WS-IS-ANOMALLY-CTR
      D              "WS-IS-ASTEROID-CTR->"   WS-IS-ASTEROID-CTR
      D              "WS-IS-BLACK-HOLE-CTR->" WS-IS-BLACK-HOLE-CTR
      D              "WS-IS-BORG-CTR->"       WS-IS-BORG-CTR
      D              "WS-IS-CARD-CTR->"       WS-IS-CARD-CTR
      D              "WS-IS-COMET-CTR->"      WS-IS-COMET-CTR
      D              "WS-IS-ENEMY-1-CTR->"    WS-IS-ENEMY-1-CTR
      D              "WS-IS-ENEMY-2-CTR->"    WS-IS-ENEMY-2-CTR
      D              "WS-IS-FERI-CTR->"       WS-IS-FERI-CTR
      D              "WS-IS-KLINGON-CTR->"    WS-IS-KLINGON-CTR
      D              "WS-IS-KOBIASHI-CTR->"   WS-IS-KOBIASHI-CTR
      D              "WS-IS-MOON-CTR->"       WS-IS-MOON-CTR
      D              "WS-IS-NEXUS-CTR->"      WS-IS-NEXUS-CTR
      D              "WS-IS-NOVA-CTR->"       WS-IS-NOVA-CTR
      D              "WS-IS-PLANET-CTR->"     WS-IS-PLANET-CTR
      D              "WS-IS-Q-CTR->"          WS-IS-Q-CTR
      D              "WS-IS-ROMULAN-CTR->"    WS-IS-ROMULAN-CTR
      D              "WS-IS-STAR-CTR->"       WS-IS-STAR-CTR
      D              "WS-IS-STAR-BASE-CTR->"  WS-IS-STAR-BASE-CTR
      D              "WS-IS-STAR-SHIP-CTR->"  WS-IS-STAR-SHIP-CTR
      D              "WS-IS-SUPER-NOVA-CTR->" WS-IS-SUPER-NOVA-CTR
      D              "WS-IS-UNKNOWN-CTR->"    WS-IS-UNKNOWN-CTR
      D              "WS-IS-VGER-CTR->"       WS-IS-VGER-CTR
      D              "WS-IS-VULCAN-CTR->"     WS-IS-VULCAN-CTR
      D              "WS-IS-WORM-HOLE-CTR->"  WS-IS-WORM-HOLE-CTR

      D      DISPLAY WS-QUAD-CTR "WS-ITEM-MAX-COUNTER-> "
      D              WS-ITEM-MAX-COUNTER

      D      DISPLAY WS-QUAD-CTR
      D              "WS-IS-ANOMALLY-MAX->"   WS-IS-ANOMALLY-MAX
      D              "WS-IS-ASTEROID-MAX->"   WS-IS-ASTEROID-MAX
      D              "WS-IS-BLACK-HOLE-MAX->" WS-IS-BLACK-HOLE-MAX
      D              "WS-IS-BORG-MAX->"       WS-IS-BORG-MAX
      D              "WS-IS-CARD-MAX->"       WS-IS-CARD-MAX
      D              "WS-IS-COMET-MAX->"      WS-IS-COMET-MAX
      D              "WS-IS-ENEMY-1-MAX->"    WS-IS-ENEMY-1-MAX
      D              "WS-IS-ENEMY-2-MAX->"    WS-IS-ENEMY-2-MAX
      D              "WS-IS-FERI-MAX->"       WS-IS-FERI-MAX
      D              "WS-IS-KLINGON-MAX->"    WS-IS-KLINGON-MAX
      D              "WS-IS-KOBIASHI-MAX->"   WS-IS-KOBIASHI-MAX
      D              "WS-IS-MOON-MAX->"       WS-IS-MOON-MAX
      D              "WS-IS-NEXUS-MAX->"      WS-IS-NEXUS-MAX
      D              "WS-IS-NOVA-MAX->"       WS-IS-NOVA-MAX
      D              "WS-IS-PLANET-MAX->"     WS-IS-PLANET-MAX
      D              "WS-IS-Q-MAX->"          WS-IS-Q-MAX
      D              "WS-IS-ROMULAN-MAX->"    WS-IS-ROMULAN-MAX
      D              "WS-IS-STAR-MAX->"       WS-IS-STAR-MAX
      D              "WS-IS-STAR-BASE-MAX->"  WS-IS-STAR-BASE-MAX
      D              "WS-IS-STAR-SHIP-MAX->"  WS-IS-STAR-SHIP-MAX
      D              "WS-IS-SUPER-NOVA-MAX->" WS-IS-SUPER-NOVA-MAX
      D              "WS-IS-UNKNOWN-MAX->"    WS-IS-UNKNOWN-MAX
      D              "WS-IS-VGER-MAX->"       WS-IS-VGER-MAX
      D              "WS-IS-VULCAN-MAX->"     WS-IS-VULCAN-MAX
      D              "WS-IS-WORM-HOLE-MAX->"  WS-IS-WORM-HOLE-MAX

           END-PERFORM.

       1040-END.
           EXIT.
      *
      * -------------------------------------------------------------- *
      *
       1050-CALCULATE-MAX-ITEMS SECTION.

           MOVE ZEROES TO WS-ITEM-COUNTER
                          WS-ITEM-MAX-COUNTER.

           IF WS-QUAD-ALPHA
              MOVE 1 TO WS-IS-KOBIASHI-MAX
           END-IF.

           COMPUTE WS-IS-ANOMALLY-MAX   = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM / 50 +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-ASTEROID-MAX   = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM / 10 +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-BLACK-HOLE-MAX = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM / 25 +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-BORG-MAX       = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-CARD-MAX       = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-COMET-MAX      = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM / 75 +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-ENEMY-1-MAX    = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-ENEMY-2-MAX    = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-FERI-MAX       = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-KLINGON-MAX    = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-MOON-MAX       = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM / 4 +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-NEXUS-MAX      = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM / 1500 +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-NOVA-MAX       = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM / 500 +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-PLANET-MAX     = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM * 2 +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-Q-MAX          = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-ROMULAN-MAX    = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-STAR-MAX       = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM / 2 +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-STAR-BASE-MAX  = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-STAR-SHIP-MAX  = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-SUPER-NOVA-MAX = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM /750 +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-UNKNOWN-MAX    = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM / 1000 +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-VGER-MAX       = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM / 100 +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-VULCAN-MAX     = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM +
                                          WS-DIFFICULTY-FACTOR.
           COMPUTE WS-IS-WORM-HOLE-MAX  = WS-DIFFICULTY-FACTOR-SQD *
                                          FUNCTION RANDOM / 10 +
                                          WS-DIFFICULTY-FACTOR.

       1050-END.
           EXIT.
      *
      * -------------------------------------------------------------- *
      *
       1060-POPULATE-SECTOR SECTION.

           MOVE ZEROES TO WS-SECTOR-ARRAY.

           MOVE WS-QUAD-SECTOR-ID
               (WS-QUAD-CTR WS-QUAD-X-CTR WS-QUAD-Y-CTR WS-QUAD-Z-CTR)
             TO WS-SECTOR-ID.

           MOVE WS-QUAD-SECTOR-CONTENTS
               (WS-QUAD-CTR WS-QUAD-X-CTR WS-QUAD-Y-CTR WS-QUAD-Z-CTR)
             TO WS-SECTOR-CONTENTS.

           PERFORM 1070-POPULATE-SECTOR-LOCATION
              THRU 1070-END.

           MOVE WS-SECTOR-ID
             TO WS-QUAD-SECTOR-ID
               (WS-QUAD-CTR WS-QUAD-X-CTR WS-QUAD-Y-CTR WS-QUAD-Z-CTR).

           MOVE WS-SECTOR-CONTENTS
             TO WS-QUAD-SECTOR-CONTENTS
               (WS-QUAD-CTR WS-QUAD-X-CTR WS-QUAD-Y-CTR WS-QUAD-Z-CTR).

      D     IF WS-ALPHA-QUAD
      D        DISPLAY 'WS-SECTOR-ID->' WS-SECTOR-ID ' '
      D                'WS-SECTOR-CONTENTS->' WS-SECTOR-CONTENTS
      D     END-IF.

      *D    IF WS-ALPHA-QUAD
      *D       DISPLAY WS-QUAD-SECTOR-ID
      *D         (WS-QUAD-CTR WS-QUAD-X-CTR WS-QUAD-Y-CTR WS-QUAD-Z-CTR)
      *D         " "
      *D               WS-QUAD-SECTOR-CONTENTS
      *D         (WS-QUAD-CTR WS-QUAD-X-CTR WS-QUAD-Y-CTR WS-QUAD-Z-CTR)
      *D    END-IF.

       1060-END.
           EXIT.
      *
      * -------------------------------------------------------------- *
      *
       1070-POPULATE-SECTOR-LOCATION SECTION.

           PERFORM VARYING WS-SECTOR-X-CTR FROM 1 BY 1
             UNTIL WS-SECTOR-X-CTR > WS-SECTOR-MAX

             PERFORM VARYING WS-SECTOR-Y-CTR FROM 1 BY 1
               UNTIL WS-SECTOR-Y-CTR > WS-SECTOR-MAX

               PERFORM VARYING WS-SECTOR-Z-CTR FROM 1 BY 1
                 UNTIL WS-SECTOR-Z-CTR > WS-SECTOR-MAX

                 SET WS-IS-EMPTY-SPACE TO TRUE

                 EVALUATE TRUE
                     WHEN WS-ALPHA-QUAD
                          PERFORM 1080-ALPHA-QUAD
                             THRU 1080-ALPHA-QUAD-END
                     WHEN WS-BETA-QUAD
                          PERFORM 1080-BETA-QUAD
                             THRU 1080-BETA-QUAD-END
                     WHEN WS-GAMMA-QUAD
                          PERFORM 1080-GAMMA-QUAD
                             THRU 1080-GAMMA-QUAD-END
                     WHEN WS-DELTA-QUAD
                          PERFORM 1080-DELTA-QUAD
                             THRU 1080-DELTA-QUAD-END
                 END-EVALUATE

               END-PERFORM

             END-PERFORM

           END-PERFORM.

       1070-END.
           EXIT.
      *
      * -------------------------------------------------------------- *
      *
       1080-ALPHA-QUAD SECTION.

           IF WS-SECTOR-QUAD-X = 5 AND WS-SECTOR-QUAD-Y = 5 AND
              WS-SECTOR-QUAD-Z = 5

              IF WS-SECTOR-Y-CTR = 5 AND WS-SECTOR-Z-CTR = 5 AND
                 WS-SECTOR-X-CTR NOT = 5
                 EVALUATE WS-SECTOR-X-CTR
                     WHEN 1
                      SET WS-IS-STAR TO TRUE
                      ADD 1 TO WS-IS-STAR-CTR
                     WHEN 2
                     WHEN 4
                     WHEN 9
                      SET WS-IS-PLANET TO TRUE
                      ADD 1 TO WS-IS-PLANET-CTR
                     WHEN 6
                      SET WS-IS-ASTEROID TO TRUE
                      ADD 1 TO WS-IS-ASTEROID-CTR
                     WHEN 10
                      SET WS-IS-COMET TO TRUE
                      ADD 1 TO WS-IS-COMET-CTR
                     WHEN OTHER
                      SET WS-IS-EMPTY-SPACE TO TRUE
                 END-EVALUATE
              END-IF

              IF WS-SECTOR-X-CTR = 5 AND WS-SECTOR-Z-CTR = 5 AND
                 WS-SECTOR-Y-CTR NOT = 5
                 EVALUATE WS-SECTOR-Y-CTR
                     WHEN 2
                     WHEN 9
                      SET WS-IS-PLANET TO TRUE
                      ADD 1 TO WS-IS-PLANET-CTR
                     WHEN 4
                      SET WS-IS-STAR-FLEET TO TRUE
                     WHEN 6
                      SET WS-IS-ASTEROID TO TRUE
                      ADD 1 TO WS-IS-ASTEROID-CTR
                     WHEN 7
                      SET WS-IS-COMET TO TRUE
                     WHEN OTHER
                      SET WS-IS-EMPTY-SPACE TO TRUE
                 END-EVALUATE
              END-IF

              IF WS-SECTOR-X-CTR = 5 AND WS-SECTOR-Y-CTR = 5 AND
                 WS-SECTOR-Z-CTR NOT = 5
                 EVALUATE WS-SECTOR-Z-CTR
                     WHEN 1
                     WHEN 7
                      SET WS-IS-PLANET TO TRUE
                      ADD 1 TO WS-IS-PLANET-CTR
                     WHEN 2
                          EVALUATE WS-RANK-CHOICES
                              WHEN 0
                               IF WS-IS-KOBIASHI-CTR <
                                  WS-IS-KOBIASHI-MAX
                                  SET WS-IS-KOBIASHI TO TRUE
                                  ADD 1 TO WS-IS-KOBIASHI-CTR
                               END-IF
                          END-EVALUATE
                     WHEN 3
                      SET WS-IS-STAR-BASE TO TRUE
                      ADD 1 TO WS-IS-STAR-BASE-CTR
                     WHEN 9
                      SET WS-IS-ASTEROID TO TRUE
                      ADD 1 TO WS-IS-ASTEROID-CTR
                     WHEN 10
                      SET WS-IS-COMET TO TRUE
                      ADD 1 TO WS-IS-COMET-CTR
                     WHEN OTHER
                      SET WS-IS-EMPTY-SPACE TO TRUE
                 END-EVALUATE
              END-IF

      D       IF NOT WS-IS-EMPTY-SPACE
      D          DISPLAY WS-SECTOR-X-CTR WS-SECTOR-Y-CTR WS-SECTOR-Z-CTR
      D                 'WS-SECTOR-ITEMS->' WS-SECTOR-ITEMS
      D       END-IF

              MOVE WS-SECTOR-ITEMS
                TO WS-SECTOR-LOCATION
                  (WS-SECTOR-X-CTR WS-SECTOR-Y-CTR WS-SECTOR-Z-CTR)
           END-IF.

           PERFORM 1090-POPULATE-SECTOR
              THRU 1090-END.

       1080-ALPHA-QUAD-END.
           EXIT.
      *
      * -------------------------------------------------------------- *
      *
       1080-BETA-QUAD SECTION.


           PERFORM 1090-POPULATE-SECTOR
              THRU 1090-END.

       1080-BETA-QUAD-END.
           EXIT.
      *
      * -------------------------------------------------------------- *
      *
       1080-GAMMA-QUAD SECTION.


           PERFORM 1090-POPULATE-SECTOR
              THRU 1090-END.

       1080-GAMMA-QUAD-END.
           EXIT.
      *
      * -------------------------------------------------------------- *
      *
       1080-DELTA-QUAD SECTION.


           PERFORM 1090-POPULATE-SECTOR
              THRU 1090-END.

       1080-DELTA-QUAD-END.
           EXIT.
      *
      * -------------------------------------------------------------- *
      *
       1090-POPULATE-SECTOR SECTION.

           IF WS-SECTOR-X-CTR = 1 AND
              WS-SECTOR-Y-CTR = 1 AND
              WS-SECTOR-Z-CTR = 1
              SET WS-IS-NEUTRAL-ZONE TO TRUE
           END-IF.

           IF WS-SECTOR-X-CTR = WS-SECTOR-MAX AND
              WS-SECTOR-Y-CTR = WS-SECTOR-MAX AND
              WS-SECTOR-Z-CTR = WS-SECTOR-MAX
              SET WS-IS-GALAXY-BARRIER TO TRUE
           END-IF.

           IF WS-IS-NEUTRAL-ZONE OR
              WS-IS-GALAXY-BARRIER
              GO TO 1090-NEXT
           END-IF.

           IF NOT WS-IS-EMPTY-SPACE
              GO TO 1090-END
           END-IF.

           COMPUTE WS-RANDOM-NUMBER = FUNCTION RANDOM *
                   WS-CONTENT-ID + 1.

           EVALUATE WS-RDM-NBR-INT
               WHEN  1
                     IF WS-IS-ANOMALLY-CTR <
                        WS-IS-ANOMALLY-MAX
                        SET WS-IS-ANOMALLY TO TRUE
                        ADD 1 TO WS-IS-ANOMALLY-CTR
                     END-IF
               WHEN  3
                     EVALUATE WS-SECTOR-QUAD
                         WHEN 1
                              IF WS-IS-KLINGON-CTR <
                                 WS-IS-KLINGON-MAX
                                 SET WS-IS-KLINGON TO TRUE
                                 ADD 1 TO WS-IS-KLINGON-CTR
                              END-IF
                         WHEN 2
                              IF WS-IS-CARD-CTR <
                                 WS-IS-CARD-MAX
                                 SET WS-IS-CARD TO TRUE
                                 ADD 1 TO WS-IS-CARD-CTR
                              END-IF
                         WHEN 3
                              IF WS-IS-ENEMY-1-CTR <
                                 WS-IS-ENEMY-1-MAX
                                 SET WS-IS-ENEMY-1 TO TRUE
                                 ADD 1 TO WS-IS-ENEMY-1-CTR
                              END-IF
                         WHEN 4
                              IF WS-IS-BORG-CTR <
                                 WS-IS-BORG-MAX
                                 SET WS-IS-BORG  TO TRUE
                                 ADD 1 TO WS-IS-BORG-CTR
                              END-IF
                     END-EVALUATE
               WHEN  5
                     IF WS-IS-ASTEROID-CTR <
                        WS-IS-ASTEROID-MAX
                        SET WS-IS-ASTEROID TO TRUE
                        ADD 1 TO WS-IS-ASTEROID-CTR
                     END-IF
               WHEN  9
                     IF WS-IS-NEXUS-CTR <
                        WS-IS-NEXUS-MAX
                        SET WS-IS-NEXUS TO TRUE
                        ADD 1 TO WS-IS-NEXUS-CTR
                     END-IF
               WHEN 11
                     IF WS-IS-VULCAN-CTR <
                        WS-IS-VULCAN-MAX
                        SET WS-IS-VULCAN TO TRUE
                        ADD 1 TO WS-IS-VULCAN-CTR
                     END-IF
               WHEN 13
                     IF WS-IS-NOVA-CTR <
                        WS-IS-NOVA-MAX
                        SET WS-IS-NOVA TO TRUE
                        ADD 1 TO WS-IS-NOVA-CTR
                     END-IF
               WHEN 17
                     IF WS-IS-STAR-SHIP-CTR <
                        WS-IS-STAR-SHIP-MAX
                        SET WS-IS-STAR-SHIP TO TRUE
                        ADD 1 TO WS-IS-STAR-SHIP-CTR
                     END-IF
               WHEN 20
                     IF WS-IS-Q-CTR < WS-IS-Q-MAX
                        SET WS-IS-Q TO TRUE
                        ADD 1 TO WS-IS-Q-CTR
                     END-IF
               WHEN 25
                     EVALUATE WS-SECTOR-QUAD
                         WHEN 1
                              IF WS-IS-BORG-CTR <
                                 WS-IS-BORG-MAX
                                 SET WS-IS-BORG TO TRUE
                                 ADD 1 TO WS-IS-BORG-CTR
                              END-IF
                         WHEN 2
                              IF WS-IS-CARD-CTR <
                                 WS-IS-CARD-MAX
                                 SET WS-IS-CARD TO TRUE
                                 ADD 1 TO WS-IS-CARD-CTR
                              END-IF
                         WHEN 3
                              IF WS-IS-ENEMY-2-CTR <
                                 WS-IS-ENEMY-2-MAX
                                 SET WS-IS-ENEMY-2 TO TRUE
                                 ADD 1 TO WS-IS-ENEMY-2-CTR
                              END-IF
                         WHEN 4
                              IF WS-IS-ENEMY-1-CTR <
                                 WS-IS-ENEMY-1-MAX
                                 SET WS-IS-ENEMY-1 TO TRUE
                                 ADD 1 TO WS-IS-ENEMY-1-CTR
                              END-IF
                     END-EVALUATE
               WHEN 33
                     EVALUATE WS-SECTOR-QUAD
                         WHEN 1
                              IF WS-IS-VGER-CTR < WS-IS-VGER-MAX
                                 SET WS-IS-VGER TO TRUE
                                 ADD 1 TO WS-IS-VGER-CTR
                              END-IF
                         WHEN 2
                              IF WS-IS-CARD-CTR <
                                 WS-IS-CARD-MAX
                                 SET WS-IS-CARD TO TRUE
                                 ADD 1 TO WS-IS-CARD-CTR
                              END-IF
                         WHEN 3
                              IF WS-IS-ENEMY-1-CTR <
                                 WS-IS-ENEMY-1-MAX
                                 SET WS-IS-ENEMY-1 TO TRUE
                                 ADD 1 TO WS-IS-ENEMY-1-CTR
                              END-IF
                         WHEN 4
                              IF WS-IS-BORG-CTR <
                                 WS-IS-BORG-MAX
                                 SET WS-IS-BORG TO TRUE
                                 ADD 1 TO WS-IS-BORG-CTR
                              END-IF
                     END-EVALUATE
               WHEN 36
                     IF WS-IS-STAR-CTR <
                        WS-IS-STAR-MAX
                        SET WS-IS-STAR TO TRUE
                        ADD 1 TO WS-IS-STAR-CTR
                     END-IF
               WHEN 38
                     IF WS-IS-PLANET-CTR <
                        WS-IS-PLANET-MAX
                        SET WS-IS-PLANET TO TRUE
                        ADD 1 TO WS-IS-PLANET-CTR
                     END-IF
               WHEN 40
                     IF WS-IS-STAR-BASE-CTR <
                        WS-IS-STAR-BASE-MAX
                        SET WS-IS-STAR-BASE TO TRUE
                        ADD 1 TO WS-IS-STAR-BASE-CTR
                     END-IF
               WHEN 42
                     IF WS-IS-ANOMALLY-CTR <
                        WS-IS-ANOMALLY-MAX
                        SET WS-IS-ANOMALLY TO TRUE
                        ADD 1 TO WS-IS-ANOMALLY-CTR
                     END-IF
               WHEN 44
                     IF WS-IS-SUPER-NOVA-CTR <
                        WS-IS-SUPER-NOVA-MAX
                        SET WS-IS-SUPER-NOVA TO TRUE
                        ADD 1 TO WS-IS-SUPER-NOVA-CTR
                     END-IF
               WHEN 46
                     IF WS-IS-WORM-HOLE-CTR <
                        WS-IS-WORM-HOLE-MAX
                        SET WS-IS-WORM-HOLE TO TRUE
                        ADD 1 TO WS-IS-WORM-HOLE-CTR
                     END-IF
               WHEN 48
                     IF WS-IS-UNKNOWN-CTR <
                        WS-IS-UNKNOWN-MAX
                        SET WS-IS-UNKNOWN TO TRUE
                        ADD 1 TO WS-IS-UNKNOWN-CTR
                     END-IF
               WHEN 49
                     IF WS-IS-FERI-CTR <
                        WS-IS-FERI-MAX
                        SET WS-IS-FERI TO TRUE
                        ADD 1 TO WS-IS-FERI-CTR
                     END-IF
               WHEN 50
                     IF WS-IS-MOON-CTR <
                        WS-IS-MOON-MAX
                        SET WS-IS-MOON TO TRUE
                        ADD 1 TO WS-IS-MOON-CTR
                     END-IF
               WHEN 54
                     EVALUATE WS-SECTOR-QUAD
                         WHEN 1
                              IF WS-IS-ROMULAN-CTR <
                                 WS-IS-ROMULAN-MAX
                                 SET WS-IS-ROMULAN TO TRUE
                                 ADD 1 TO WS-IS-ROMULAN-CTR
                              END-IF
                         WHEN 2
                              IF WS-IS-ENEMY-1-CTR <
                                 WS-IS-ENEMY-1-MAX
                                 SET WS-IS-ENEMY-1 TO TRUE
                                 ADD 1 TO WS-IS-ENEMY-1-CTR
                              END-IF
                         WHEN 3
                              IF WS-IS-ENEMY-2-CTR <
                                 WS-IS-ENEMY-2-MAX
                                 SET WS-IS-ENEMY-2 TO TRUE
                                 ADD 1 TO WS-IS-ENEMY-2-CTR
                              END-IF
                         WHEN 4
                              IF WS-IS-BORG-CTR <
                                 WS-IS-BORG-MAX
                                 SET WS-IS-BORG TO TRUE
                                 ADD 1 TO WS-IS-BORG-CTR
                              END-IF
                     END-EVALUATE
               WHEN 56
                     IF WS-IS-COMET-CTR <
                        WS-IS-COMET-MAX
                        SET WS-IS-COMET TO TRUE
                        ADD 1 TO WS-IS-COMET-CTR
                     END-IF
               WHEN 60
                     IF WS-IS-BLACK-HOLE-CTR <
                        WS-IS-BLACK-HOLE-MAX
                        SET WS-IS-BLACK-HOLE TO TRUE
                        ADD 1 TO WS-IS-BLACK-HOLE-CTR
                     END-IF

      *    88 WS-IS-ENTERPRISE       VALUE '@'.
           END-EVALUATE.

       1090-NEXT.

           IF NOT WS-IS-EMPTY-SPACE
              MOVE WS-SECTOR-ITEMS TO WS-SECTOR-LOCATION
                  (WS-SECTOR-X-CTR WS-SECTOR-Y-CTR WS-SECTOR-Z-CTR)
           END-IF.

       1090-END.
           EXIT.
      *
      * -------------------------------------------------------------- *
      *
       9990-STOP-PROGRAM SECTION.

           GOBACK.
           STOP RUN RETURNING 0.

       9990-END.
           EXIT.
      *
      * -------------------------------------------------------------- *
      *
        END PROGRAM STARTREK.
      *                                                                *
      * ************************************************************** *
      *                     END OF PROGRAM STARTREK                    *
      * ************************************************************** *
