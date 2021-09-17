      * ************************************************************** *
      *                    START OF PROGRAM STARTREK                   *
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
      *   DATE   INT                    REASON                         *
      * -------- --- ------------------------------------------------- *
      * 20200626 Q   PROGRAM WRITTEN.                                  *
      *                                                                *
      * ************************************************************** *
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.   STARTREK.
       AUTHOR.       STAR FLEET ADMIRAL Q.
       INSTALLATION. HOME PC DEVELOPER.
       DATE-WRITTEN. FRIDAY JUNE 26, 2020.
       DATE-COMPILED. FUNCTION(WHEN-COMPILED).
      *                                                                
      * ************************************************************** *
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. LINUX.
      * SET ENVIRONMENT VARIABLE COB_SET_DEBUG=Y, y OR 1 TO ACTIVATE
      *  DEBUGGING LINES DURING RUNTIME (THOSE WITH D IN COLUMN 7)
      *  PROVIDED THE PROGRAM WAS COMPILED WITH THE -FDEBUGGING-LINE
      *  AND THE -D OR -DEBUG SWITCH.
       SOURCE-COMPUTER. LINUX DEBUGGING MODE.
       OBJECT-COMPUTER. LINUX.
       SPECIAL-NAMES.
      * REPOSITORY.
      * FUNCTION ALL INTRINSIC.
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
          03 WS-RANDOM-NUMBER-1     PIC  9(02)V9(04).
          03 WS-RANDOM-NUMBER-1-RED REDEFINES
             WS-RANDOM-NUMBER-1.
             05 WS-RANDOM-1-INT     PIC  9(02).
             05 WS-RANDOM-1-DEC     PIC  9(04).
          03 WS-RANDOM-NUMBER-2     PIC  9(02)V9(04).
          03 WS-RANDOM-NUMBER-2-RED REDEFINES
             WS-RANDOM-NUMBER-2.
             05 WS-RANDOM-2-INT     PIC  9(02).
             05 WS-RANDOM-2-DEC     PIC  9(04).
          03 WS-RANDOM-NUMBER-3     PIC  9(02)V9(04).
          03 WS-RANDOM-NUMBER-3-RED REDEFINES
             WS-RANDOM-NUMBER-3.
             05 WS-RANDOM-3-INT     PIC  9(02).
             05 WS-RANDOM-3-DEC     PIC  9(04).
          03 WS-RANDOM-NUMBER-4     PIC  9(02)V9(04).
          03 WS-RANDOM-NUMBER-4-RED REDEFINES
             WS-RANDOM-NUMBER-4.
             05 WS-RANDOM-4-INT     PIC  9(02).
             05 WS-RANDOM-4-DEC     PIC  9(04).
          03 WS-RANDOM-NUMBER-5     PIC  9(02)V9(04).
          03 WS-RANDOM-NUMBER-5-RED REDEFINES
             WS-RANDOM-NUMBER-5.
             05 WS-RANDOM-5-INT     PIC  9(02).
             05 WS-RANDOM-5-DEC     PIC  9(04).
          03 WS-RANDOM-NUMBER-6     PIC  9(02)V9(04).
          03 WS-RANDOM-NUMBER-6-RED REDEFINES
             WS-RANDOM-NUMBER-6.
             05 WS-RANDOM-6-INT     PIC  9(02).
             05 WS-RANDOM-6-DEC     PIC  9(04).
       
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
          88 WS-RANK-CHOICE-0                   VALUE 0.
          88 WS-RANK-CHOICE-1                   VALUE 1.
          88 WS-RANK-CHOICE-2                   VALUE 2.
          88 WS-RANK-CHOICE-3                   VALUE 3.
          88 WS-RANK-CHOICE-4                   VALUE 4.
          88 WS-RANK-CHOICE-5                   VALUE 5.
          88 WS-RANK-CHOICE-6                   VALUE 6.
          88 WS-RANK-CHOICE-7                   VALUE 7.
          88 WS-RANK-CHOICE-8                   VALUE 8.
          88 WS-RANK-CHOICE-9                   VALUE 9.
          88 WS-RANK-CHOICE-10                  VALUE 10.
          88 WS-RANK-CHOICE-11                  VALUE 11.
          88 WS-RANK-CHOICE-12                  VALUE 12.
          88 WS-RANK-CHOICE-VALID               VALUES 0 THRU 12.      
       01 WS-RANK                   PIC  X(15)  VALUE SPACES.
          88 WS-RANK-TRAINEE                    VALUE 'Trainee        '.
          88 WS-RANK-ENSIGN                     VALUE 'Ensign         '.
          88 WS-RANK-LT-JR                      VALUE 'Lt. Jr Grade   '.
          88 WS-RANK-LT                         VALUE 'Lieutenant     '.
          88 WS-RANK-LT-CMDR                    VALUE 'Lt. Commander  '.
          88 WS-RANK-CMDR                       VALUE 'Commander      '.
          88 WS-RANK-CAPT                       VALUE 'Captain        '.
          88 WS-RANK-COMMODORE                  VALUE 'Commodore      '.
          88 WS-RANK-REAR-LOWER                 VALUE 'Rear Adm Lower '.
          88 WS-RANK-REAR-UPPER                 VALUE 'Rear Adm Upper '.
          88 WS-RANK-VICE                       VALUE 'Vice Admiral   '.
          88 WS-RANK-ADMIRAL                    VALUE 'Admiral        '.
          88 WS-RANK-FLEET                      VALUE 'Fleet Admiral  '.
       01 WS-RANK-NAME              PIC  X(45)  VALUE SPACES.
       01 WS-DIFFICULTY-FACTOR      PIC  9(03)  VALUE ZEROES.
       01 WS-SECTOR-ITEMS           PIC  X(01)  VALUE SPACES.
          88 WS-IS-ASTEROID                     VALUE 'a'.
          88 WS-IS-STAR-BASE                    VALUE '@'.
          88 WS-IS-BLACK-HOLE                   VALUE 'b'.
          88 WS-IS-COMET                        VALUE 'c'.
          88 WS-IS-ENEMY-1                      VALUE 'e'.
          88 WS-IS-ENEMY-2                      VALUE 'f'.
          88 WS-IS-MOON                         VALUE 'm'.
          88 WS-IS-NOVA                         VALUE 'n'.
          88 WS-IS-PLANET                       VALUE 'p'.
          88 WS-IS-STAR                         VALUE '*'.
          88 WS-IS-SUPER-NOVA                   VALUE 's'.
          88 WS-IS-WORM-HOLE                    VALUE 'w'.
       01 WS-IS-ENEMY               PIC  X(01)  VALUE SPACES.
          88 WS-IS-BORG                         VALUE 'B'.
          88 WS-IS-CARD                         VALUE 'C'.
          88 WS-IS-KLINGON                      VALUE 'K'.
          88 WS-IS-NEXUS                        VALUE 'N'.
          88 WS-IS-ROMULAN                      VALUE 'R'.
          88 WS-IS-VGER                         VALUE 'V'.
       01 WS-IS-ALLY                PIC  X(01)  VALUE SPACES.
          88 WS-IS-FERI                         VALUE 'f'.
          88 WS-IS-KOBIASHI                     VALUE '^'.
          88 WS-IS-VULCAN                       VALUE 'v'.
             
       01 WS-FLAG-SWITCHES.
          03 WS-EOF          BINARY PIC  9(01)  VALUE ZEROES.
             88 WS-EOF-FALSE                    VALUE 0.
             88 WS-EOF-TRUE                     VALUE 1.
             
       01 WS-GALAXY-ARRAY.
          03 WS-QUADRANT            OCCURS 4 TIMES.
             05 WS-QUAD-X-AXIS      OCCURS 30 TIMES.
                07 WS-QUAD-Y-AXIS   OCCURS 30 TIMES.
                   09 WS-QUAD-Z-AXIS
                                    OCCURS 30 TIMES.
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
          03 WS-QUAD-X-CTR          PIC  9(02)         VALUE ZEROES.
          03 WS-QUAD-Y-CTR          PIC  9(02)         VALUE ZEROES.
          03 WS-QUAD-Z-CTR          PIC  9(02)         VALUE ZEROES.
          03 WS-SECTOR-X-CTR        PIC  9(02)         VALUE ZEROES.
          03 WS-SECTOR-Y-CTR        PIC  9(02)         VALUE ZEROES.
          03 WS-SECTOR-Z-CTR        PIC  9(02)         VALUE ZEROES.
                                                
       01 WS-CONSTANTS.
          03 WS-QUAD-MAX            PIC  9(01)         VALUE 4.
          03 WS-SECTOR-MAX          PIC  9(02)         VALUE 10.
          03 WS-CONTENT-ID          PIC  9(02)         VALUE 30.
         
         
             
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
           DISPLAY ' 0 for Trainee'.
           DISPLAY ' 1 for Ensign'.
           DISPLAY ' 2 for Lieutenant, Junior Grade'.
           DISPLAY ' 3 for Lieutenant'.
           DISPLAY ' 4 for Lieutenant Commander'.
           DISPLAY ' 5 for Commander'.
           DISPLAY ' 6 for Captain'.
           DISPLAY ' 7 for Commodore'.
           DISPLAY ' 8 for Rear Admiral Lower Half'.
           DISPLAY ' 9 for Rear Admiral Upper Half'.
           DISPLAY '10 for Vice Admiral'.
           DISPLAY '11 for Admiral'.
           DISPLAY '12 for Fleet Admiral'.
           DISPLAY 'Note: the higher your rank the more difficult the '.
           DISPLAY 'missions become. ...'.
           ACCEPT WS-RANK-CHOICES FROM CONSOLE.

           IF NOT WS-RANK-CHOICE-VALID
              DISPLAY WS-BLANK-LINE
              DISPLAY WS-BLANK-LINE
              DISPLAY 'Invalid rank choice: ' WS-RANK
              MOVE 0 TO WS-RANK
              DISPLAY 'Please select a valid rank value (0 through 12)!'
              GO TO 1000-RANK-LOOP
           END-IF.

           PERFORM 1010-FORMAT-RANK-NAME
              THRU 1010-END.

           DISPLAY WS-BLANK-LINE.
           DISPLAY WS-BLANK-LINE.
           DISPLAY 'Welcome aboard ' WS-RANK-NAME.
           DISPLAY WS-BLANK-LINE.
           DISPLAY 'Congratulations - you have been appointed'.
           DISPLAY 'to command the U.S.S. Enterprise.'.
           DISPLAY WS-BLANK-LINE.
           DISPLAY 'Your mission is to perform the duties assigned'.
           DISPLAY 'as directed by those appointed above you. ...'.

           DISPLAY WS-BLANK-LINE.
           DISPLAY WS-BLANK-LINE.
           DISPLAY 'Press <ENTER> key to continue. ...'.
           ACCEPT WS-PAUSE FROM CONSOLE.
           
           PERFORM 1020-BUILD-GALAXY
              THRU 1020-END.

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
                SET WS-RANK-ENSIGN TO TRUE
               WHEN WS-RANK-CHOICE-2
                SET WS-RANK-LT-JR TO TRUE
               WHEN WS-RANK-CHOICE-3
                SET WS-RANK-LT TO TRUE
               WHEN WS-RANK-CHOICE-4
                SET WS-RANK-LT-CMDR TO TRUE
               WHEN WS-RANK-CHOICE-5
                SET WS-RANK-CMDR TO TRUE
               WHEN WS-RANK-CHOICE-6
                SET WS-RANK-CAPT TO TRUE
               WHEN WS-RANK-CHOICE-7
                SET WS-RANK-COMMODORE TO TRUE
               WHEN WS-RANK-CHOICE-8
                SET WS-RANK-REAR-LOWER TO TRUE
               WHEN WS-RANK-CHOICE-9
                SET WS-RANK-REAR-UPPER TO TRUE
               WHEN WS-RANK-CHOICE-10
                SET WS-RANK-VICE TO TRUE
               WHEN WS-RANK-CHOICE-11
                SET WS-RANK-ADMIRAL TO TRUE
               WHEN WS-RANK-CHOICE-12
                SET WS-RANK-FLEET TO TRUE
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

      *     DISPLAY WS-DIFFICULTY-FACTOR.

       1010-END.
           EXIT.
      *                                                                
      * -------------------------------------------------------------- *
      *
       1020-BUILD-GALAXY SECTION.
       
           MOVE ZEROES TO WS-GALAXY-ARRAY.
           
           PERFORM VARYING WS-QUAD-CTR FROM 1 BY 1
             UNTIL WS-QUAD-CTR > WS-QUAD-MAX
          
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
                                      
                   PERFORM 1030-POPULATE-SECTOR
                      THRU 1030-END
            
                 END-PERFORM         
            
               END-PERFORM         
      
             END-PERFORM         
             
           END-PERFORM.
       
       1020-END.
           EXIT.
      *                                                                
      * -------------------------------------------------------------- *
      *
       1030-POPULATE-SECTOR SECTION.
       
           MOVE ZEROES TO WS-SECTOR-ARRAY.                    

           MOVE WS-QUAD-SECTOR-ID 
               (WS-QUAD-CTR WS-QUAD-X-CTR WS-QUAD-Y-CTR WS-QUAD-Z-CTR)
             TO WS-SECTOR-ID.   

           MOVE WS-QUAD-SECTOR-CONTENTS 
               (WS-QUAD-CTR WS-QUAD-X-CTR WS-QUAD-Y-CTR WS-QUAD-Z-CTR)
             TO WS-SECTOR-CONTENTS.                  
      
      *     IF WS-ALPHA-QUAD     
      *        DISPLAY WS-SECTOR-ID " " WS-SECTOR-CONTENTS
      *     END-IF.
           
           PERFORM 1040-POPULATE-SECTOR-LOCATION
              THRU 1040-END.

           MOVE WS-SECTOR-CONTENTS
             TO WS-QUAD-SECTOR-CONTENTS 
               (WS-QUAD-CTR WS-QUAD-X-CTR WS-QUAD-Y-CTR WS-QUAD-Z-CTR).

           MOVE WS-SECTOR-ID
             TO WS-QUAD-SECTOR-ID 
               (WS-QUAD-CTR WS-QUAD-X-CTR WS-QUAD-Y-CTR WS-QUAD-Z-CTR).   

           IF WS-ALPHA-QUAD
              DISPLAY WS-QUAD-SECTOR-ID 
                (WS-QUAD-CTR WS-QUAD-X-CTR WS-QUAD-Y-CTR WS-QUAD-Z-CTR) 
                " "    
                      WS-QUAD-SECTOR-CONTENTS 
                (WS-QUAD-CTR WS-QUAD-X-CTR WS-QUAD-Y-CTR WS-QUAD-Z-CTR)
           END-IF.
                
       1030-END.
           EXIT.
      *                                                                
      * -------------------------------------------------------------- *
      *
       1040-POPULATE-SECTOR-LOCATION SECTION.

           PERFORM VARYING WS-SECTOR-X-CTR FROM 1 BY 1
             UNTIL WS-SECTOR-X-CTR > WS-SECTOR-MAX

             PERFORM VARYING WS-SECTOR-Y-CTR FROM 1 BY 1
               UNTIL WS-SECTOR-Y-CTR > WS-SECTOR-MAX

               PERFORM VARYING WS-SECTOR-Z-CTR FROM 1 BY 1
                 UNTIL WS-SECTOR-Z-CTR > WS-SECTOR-MAX
                
                 PERFORM 9040-GENERATE-RANDOM
                    THRU 9040-END
                
                 EVALUATE TRUE
                     WHEN WS-ALPHA-QUAD
                          PERFORM 1050-ALPHA-QUAD
                             THRU 1050-ALPHA-QUAD-END
                     WHEN WS-BETA-QUAD
                          PERFORM 1050-BETA-QUAD
                             THRU 1050-BETA-QUAD-END
                     WHEN WS-GAMMA-QUAD
                          PERFORM 1050-GAMMA-QUAD
                             THRU 1050-GAMMA-QUAD-END
                     WHEN WS-DELTA-QUAD
                          PERFORM 1050-DELTA-QUAD
                             THRU 1050-DELTA-QUAD-END
                 END-EVALUATE

               END-PERFORM         
      
             END-PERFORM         
             
           END-PERFORM.

       1040-END.
           EXIT.
      *                                                                
      * -------------------------------------------------------------- *
      *
       1050-ALPHA-QUAD SECTION.
 
           IF WS-SECTOR-QUAD-X = 5 AND WS-SECTOR-X-CTR = 5 AND
              WS-SECTOR-QUAD-Y = 5 AND WS-SECTOR-Y-CTR = 5 AND
              WS-SECTOR-QUAD-Z = 5 AND WS-SECTOR-Z-CTR = 5 
              SET WS-IS-STAR TO TRUE
              MOVE WS-SECTOR-ITEMS 
                TO WS-SECTOR-LOCATION 
                  (WS-SECTOR-X-CTR WS-SECTOR-Y-CTR WS-SECTOR-Z-CTR)
           END-IF.
       
       1050-ALPHA-QUAD-END.
           EXIT.
      *                                                                
      * -------------------------------------------------------------- *
      *
       1050-BETA-QUAD SECTION.
       
       1050-BETA-QUAD-END.
           EXIT.
      *                                                                
      * -------------------------------------------------------------- *
      *
       1050-GAMMA-QUAD SECTION.
       
       1050-GAMMA-QUAD-END.
           EXIT.
      *                                                                
      * -------------------------------------------------------------- *
      *
       1050-DELTA-QUAD SECTION.
       
       1050-DELTA-QUAD-END.
           EXIT.
      *                                                                
      * -------------------------------------------------------------- *
      *
      *                                                                
      * -------------------------------------------------------------- *
      *
       9040-GENERATE-RANDOM SECTION.
           
           COMPUTE WS-RANDOM-NUMBER-1 = 
                  (FUNCTION RANDOM * WS-CONTENT-ID) + 1.
                
           COMPUTE WS-RANDOM-NUMBER-2 = 
                  (FUNCTION RANDOM * WS-CONTENT-ID) + 1.

           COMPUTE WS-RANDOM-NUMBER-3 = 
                  (FUNCTION RANDOM * WS-CONTENT-ID) + 1.
                
           COMPUTE WS-RANDOM-NUMBER-4 = 
                  (FUNCTION RANDOM * WS-CONTENT-ID) + 1.
           
           COMPUTE WS-RANDOM-NUMBER-5 = 
                  (FUNCTION RANDOM * WS-CONTENT-ID) + 1.
                
           COMPUTE WS-RANDOM-NUMBER-6 = 
                  (FUNCTION RANDOM * WS-CONTENT-ID) + 1.
           
      *     DISPLAY WS-RANDOM-1-INT " " WS-RANDOM-2-INT " "
      *             WS-RANDOM-3-INT " " WS-RANDOM-4-INT " "
      *             WS-RANDOM-5-INT " " WS-RANDOM-6-INT " ".
       
       9040-END.
           EXIT.
      *                                                                
      * -------------------------------------------------------------- *
      *
       9990-STOP-PROGRAM SECTION.
        
           GOBACK.    
           STOP RUN.
        
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

