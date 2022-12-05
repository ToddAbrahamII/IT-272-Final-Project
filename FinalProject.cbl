      ******************************************************************
      * Author: Todd Abraham
      * Date: 12/5/2022
      * Purpose: This program updates a student master file with grade
      * point information that’s applied by the registration file.
      *The result is a new student master file that contains the
      * updated information.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FinalProject.
      ******************************************************************
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT STUMAST ASSIGN TO "stumast.dat"
                       ORGANIZATION IS LINE SEQUENTIAL.

                   SELECT CRSEREG ASSIGN TO "crsereg.dat"
                       ORGANIZATION IS LINE SEQUENTIAL.

                   SELECT NEWMAST ASSIGN TO "NEWMAST"
                       ORGANIZATION IS LINE SEQUENTIAL.

                   SELECT ERRRPT ASSIGN TO "ERRRPT"
                       ORGANIZATION IS LINE SEQUENTIAL.

      ******************************************************************
       DATA DIVISION.
      *--------------*
       FILE SECTION.
      **Copies Code from STUMAST
           FD STUMAST.
               COPY "stumast.cpy".
      **Copies Code from CRSEREG
           FD CRSEREG.
               COPY "crsereg.cpy".

           FD NEWMAST.
           01 WORK-AREA                    PIC X(115).

           FD ERRRPT.
           01 ERROR-AREA                   PIC X(100).
      *--------------------------*
       WORKING-STORAGE SECTION.

      *--------*
           01 END-OF-FILE0                 PIC XXX VALUE "NO".
           01 END-OF-FILE1                 PIC XXX VALUE "NO".

           01 CALC                         PIC 9999.

           01 ID-COUNT-CR                  PIC 999 VALUE 1.
           01 ID-COUNT-STU                 PIC 999 VALUE 1.
           01 WRITE-ID                     PIC 999 VALUE 1.
           01 SWITCH                       PIC X VALUE "N".
           01 PLACE                        PIC X.

      *--------*
           01  STUMAST-REC-TABLE.
            05 STUDENT OCCURS 160 TIMES.
               10 STU-ID-OUT               PIC 9(9).
               10 STU-STATUS-OUT           PIC X(01).
               10 STU-NAME-OUT             PIC X(25).
               10 STU-DOB-YEAR-OUT         PIC 9(04).
               10 STU-DOB-MONTH-OUT        PIC 9(02).
               10 STU-DOB-DAY-OUT          PIC 9(02).
               10 STU-ADDRESS-OUT          PIC X(25).
               10 STU-CITY-OUT             PIC X(11).
               10 STU-STATE-OUT            PIC X(02).
               10 STU-ZIP-CODE-OUT         PIC 9(05).
               10 STU-ZIP-EXT-OUT          PIC 9(04).
               10 STU-CLASS-OUT            PIC 9(01).
               10 STU-MAJOR-OUT            PIC X(04).
               10 STU-UNITS-COMP-OUT       PIC 9(03).
               10 STU-TTL-PTS-OUT          PIC 9(03).
               10 STU-UNITS-IP-OUT         PIC 9(03).
           05 STUDENT-SUB                  PIC 999 VALUE 1.

      *-------*
           01 CRSEREG-REC-TABLE.
            05 COURSE OCCURS 104 TIMES.
               10 CR-DEPT-CODE-OUT         PIC X(04).
               10 CR-CRSE-NUM-OUT          PIC 9(03).
               10 CR-SECTION-NUM-OUT       PIC 9(02).
               10 CR-COURSE-TITLE-OUT      PIC X(20).
               10 CR-START-YR-OUT          PIC 9(04).
               10 CR-START-MNTH-OUT        PIC 9(02).
               10 CR-CRSE-UNITS-OUT        PIC 9(01).
               10 CR-CRSE-DAYS-OUT         PIC 9(01).
               10 CR-TEACHER-NUM-OUT       PIC 9(03).
               10 CR-STU-ID-OUT            PIC 9(09).
               10 CR-STU-NAME-OUT          PIC X(25).
               10 CR-CLASS-OUT             PIC 9(01).
               10 CR-MAJOR-OUT             PIC X(04).
               10 CR-6-WK-GRADE-OUT        PIC X(01).
               10 CR-12-WKS-GRADE-OUT      PIC X(01).
               10 CR-FNL-EXM-GRD-OUT       PIC X(01).
               10 CR-SEM-GRADE-OUT         PIC X(01).
            05 COURSE-SUB                  PIC 999 VALUE 1.

      *------*
           01 NEWMAST-TOPLINE.
               05 FILLER               PIC X(36) VALUE SPACES.
               05 HDRCNTRY             PIC X(14) VALUE "STUDENT REPORT".

           01 FILLER-LINE.
               05 PIC X(115) VALUES ALL "*".

           01 DASHED-LINE.
               05 PIC X(115) VALUES ALL "-".

           01 NEWMAST-HDRS.
               05 STU-ID               PIC XX VALUE "ID".
               05 FILLER               PIC X(10) VALUE SPACES.
               05 STU-STATUS           PIC X(6) VALUE "STATUS".
               05 FILLER               PIC X(10) VALUE SPACES.
               05 STU-NAME             PIC X(5) VALUE "NAME".
               05 FILLER               PIC X(19) VALUE SPACES.
               05 STU-CLASS            PIC X(5) VALUE "CLASS".
               05 FILLER               PIC X(7) VALUE SPACES.
               05 STU-MAJOR            PIC X(5) VALUE "MAJOR".
               05 FILLER               PIC X(7) VALUE SPACES.
               05 STU-UNITS-COMP       PIC X(10) VALUE "UNITS COMP".
               05 FILLER               PIC X(5) VALUE SPACES.
               05 STU-TTL-PTS          PIC X(9) VALUE "TOTAL PTS".
               05 FILLER               PIC X(5) VALUE SPACES.
               05 STU-UNITS-IP         PIC X(8) VALUE "UNITS IP".

           01 ERRRPT-TOPLINE.
              05 FILLER                PIC X(37) VALUE SPACES.
              05 HDRERROR              PIC X(13) VALUE "ERROR REPORT".

           01 ERRRPT-HDRS.
               05 ERR-ID               PIC XX VALUE "ID".
               05 FILLER               PIC X(15) VALUE SPACES.
               05 ERR-NAME             PIC X(5) VALUE "NAME".
               05 FILLER               PIC X(20) VALUE SPACES.
               05 ERR-CLASS            PIC X(5) VALUE "CLASS".
               05 FILLER               PIC X(6) VALUE SPACES.
               05 ERR-MAJOR            PIC X(5) VALUE "MAJOR".

           01 NEWMAST-OUT.
               05 NEWMAST-ID           PIC 9(9).
               05 FILLER               PIC X(5) VALUE SPACES.
               05 NEWMAST-STATUS       PIC X.
               05 FILLER               PIC X(10) VALUE SPACES.
               05 NEWMAST-NAME         PIC X(25).
               05 FILLER               PIC X VALUE SPACES.
               05 CC-NEWMAST           PIC X(9).
               05 FILLER               PIC X(5).
               05 NEWMAST-MAJOR        PIC X(4).
               05 FILLER               PIC X(11).
               05 NEWMAST-UC           PIC 999.
               05 FILLER               PIC X(11).
               05 NEWMAST-TP           PIC 999.
               05 FILLER               PIC X(10).
               05 NEWMAST-UIP          PIC 999.


           01 ERRRPT-OUT.
               05 ERRR-ID              PIC 9(9).
               05 FILLER               PIC X(4).
               05 ERRR-NAME            PIC X(25).
               05 FILLER               PIC X.
      *         05 ERRR-CLASS           PIC X.
               05 CC-ERRR              PIC X(9).
               05 FILLER               PIC X(6).
               05 ERRR-MAJOR           PIC X(4).
               05 FILLER               PIC X.




      ******************************************************************
       PROCEDURE DIVISION.
      *--------------------*
       100-MAIN-PROCEDURE.
           PERFORM OPEN-FILES.

      *---Tables are populated---*
           PERFORM 200-READ-STUMAST
               UNTIL END-OF-FILE0 = "YES".

           PERFORM 300-READ-CRSEREG
               UNTIL END-OF-FILE1 = "YES".

           PERFORM 650-WRITE-NEWMAST-HEADERS.
           PERFORM 750-WRITE-ERRRPT-HDRS.

           PERFORM 600-COMPARE-STU-IDS
               UNTIL ID-COUNT-CR = 105.

           PERFORM 700-WRITE-NEWMAST
               UNTIL WRITE-ID = 161.

           CLOSE STUMAST
                 CRSEREG
                 NEWMAST
                 ERRRPT.
            STOP RUN.
      *--------------------*

      *--------------------*
       OPEN-FILES.
           OPEN INPUT  STUMAST
                       CRSEREG
                OUTPUT NEWMAST
                       ERRRPT.
       OPEN-FILES-END.
      *--------------------*

      *--------------------*
       200-READ-STUMAST.
      *Reads the STUMAST.dat file
           READ STUMAST
           AT END MOVE "YES" TO END-OF-FILE0
           NOT AT END PERFORM 400-PROCESS-STUMAST.
      *--------------------*


      *--------------------*
       300-READ-CRSEREG.
      *Reads the crsereg.dat file
           READ CRSEREG
           AT END MOVE "YES" TO END-OF-FILE1
           NOT AT END PERFORM 500-PROCESS-CRSEREG.
      *-------------------*

      *-------------------*
       400-PROCESS-STUMAST.
      *processes stumast.dat into the table
           MOVE SM-STUDENT-ID TO STU-ID-OUT(STUDENT-SUB).
           MOVE SM-STUDENT-STATUS TO STU-STATUS-OUT(STUDENT-SUB).
           MOVE SM-STUDENT-NAME TO STU-NAME-OUT(STUDENT-SUB).
           MOVE SM-DOB-YEAR TO STU-DOB-YEAR-OUT(STUDENT-SUB).
           MOVE SM-DOB-MONTH TO STU-DOB-MONTH-OUT(STUDENT-SUB).
           MOVE SM-DOB-DAY TO STU-DOB-DAY-OUT(STUDENT-SUB).
           MOVE SM-STUDENT-ADDRESS TO STU-ADDRESS-OUT(STUDENT-SUB).
           MOVE SM-STUDENT-CITY TO STU-CITY-OUT(STUDENT-SUB).
           MOVE SM-STUDENT-STATE TO STU-STATE-OUT(STUDENT-SUB).
           MOVE SM-STUDENT-ZIP-CODE TO STU-ZIP-CODE-OUT(STUDENT-SUB).
           MOVE SM-STUDENT-ZIP-CODE-EXT TO STU-ZIP-EXT-OUT(STUDENT-SUB).
           MOVE SM-CLASS-STANDING TO STU-CLASS-OUT(STUDENT-SUB).
           MOVE SM-MAJOR TO STU-MAJOR-OUT(STUDENT-SUB).
           MOVE SM-UNITS-COMPLETED TO STU-UNITS-COMP-OUT(STUDENT-SUB).
           MOVE SM-TOTAL-GRADE-POINTS TO STU-TTL-PTS-OUT(STUDENT-SUB).
           MOVE SM-UNITS-IN-PROGRESS TO STU-UNITS-IP-OUT(STUDENT-SUB).
           ADD 1 TO STUDENT-SUB.
      *------------------*

      *------------------*
       500-PROCESS-CRSEREG.
      *processes the crsereg.dat into a table
           MOVE CR-DEPARTMENT-CODE TO CR-DEPT-CODE-OUT(COURSE-SUB).
           MOVE CR-COURSE-NUMBER TO CR-CRSE-NUM-OUT(COURSE-SUB).
           MOVE CR-SECTION-NUMBER TO CR-SECTION-NUM-OUT(COURSE-SUB).
           MOVE CR-COURSE-TITLE TO CR-COURSE-TITLE-OUT(COURSE-SUB).
           MOVE CR-COURSE-START-YEAR TO CR-START-YR-OUT(COURSE-SUB).
           MOVE CR-COURSE-START-MONTH TO CR-START-MNTH-OUT(COURSE-SUB).
           MOVE CR-COURSE-UNITS TO CR-CRSE-UNITS-OUT(COURSE-SUB).
           MOVE CR-COURSE-DAYS TO CR-CRSE-DAYS-OUT(COURSE-SUB).
           MOVE CR-TEACHER-NUMBER TO CR-TEACHER-NUM-OUT(COURSE-SUB).
           MOVE CR-STUDENT-ID TO CR-STU-ID-OUT(COURSE-SUB).
           MOVE CR-STUDENT-NAME TO CR-STU-NAME-OUT(COURSE-SUB).
           MOVE CR-CLASS-STANDING TO CR-CLASS-OUT(COURSE-SUB).
           MOVE CR-MAJOR TO CR-MAJOR-OUT(COURSE-SUB).
           MOVE CR-6-WEEKS-GRADE TO CR-6-WK-GRADE-OUT(COURSE-SUB).
           MOVE CR-12-WEEKS-GRADE TO CR-12-WKS-GRADE-OUT(COURSE-SUB).
           MOVE CR-FINAL-EXAM-GRADE TO CR-FNL-EXM-GRD-OUT(COURSE-SUB).
           MOVE CR-SEMESTER-GRADE TO CR-SEM-GRADE-OUT(COURSE-SUB).
           ADD 1 TO COURSE-SUB.
      *------------------*

      *----------------*
       600-COMPARE-STU-IDS.
      *Loops Comparison loop until match is found or until table is fully compared
           PERFORM 625-ID-LOOPS
               UNTIL SWITCH = "Y".

           MOVE "N" TO SWITCH.

      *----------------*

      *----------------*
       625-ID-LOOPS.
      *loop to compare crsereg stuid to stumast stuid
           IF CR-STU-ID-OUT(ID-COUNT-CR) = STU-ID-OUT(ID-COUNT-STU)
               PERFORM 900-CALCULATIONS
      *        PERFORM 700-WRITE-NEWMAST
      *            Write here to observe changes in NEWMAST
               MOVE "Y" TO SWITCH
               ADD 1 TO ID-COUNT-CR
               MOVE 1 TO ID-COUNT-STU
           ELSE IF CR-STU-ID-OUT(ID-COUNT-CR) NOT =
               STU-ID-OUT(ID-COUNT-STU)
               ADD 1 TO ID-COUNT-STU
               MOVE "N" TO SWITCH
               PERFORM 610-LOOP-CHECK.
      *---------------*


      *----------------*
       610-LOOP-CHECK.
      *checks if loop has reached end of stumast table
           IF ID-COUNT-STU = 160
              PERFORM 800-WRITE-ERRRPT
              MOVE "Y" TO SWITCH
              MOVE 1 TO ID-COUNT-STU
              ADD 1 TO ID-COUNT-CR
           ELSE
               MOVE SPACE TO PLACE.
      *           keeps the loop going.

      *----------------*
       650-WRITE-NEWMAST-HEADERS.
      *writes headers for newmast
           MOVE NEWMAST-TOPLINE TO WORK-AREA.
           WRITE WORK-AREA.

           MOVE FILLER-LINE TO WORK-AREA.
           WRITE WORK-AREA AFTER ADVANCING 1 LINE.

           MOVE NEWMAST-HDRS TO WORK-AREA.
           WRITE WORK-AREA AFTER ADVANCING 1 LINE.

           MOVE DASHED-LINE TO WORK-AREA.
           WRITE WORK-AREA AFTER ADVANCING 1 LINE.
      *----------------*


      *----------------*
       700-WRITE-NEWMAST.
      *writes the newmast file
           MOVE STU-ID-OUT(WRITE-ID) TO NEWMAST-ID.
           MOVE STU-STATUS-OUT(WRITE-ID) TO NEWMAST-STATUS.
           MOVE STU-NAME-OUT(WRITE-ID) TO NEWMAST-NAME.
           MOVE STU-MAJOR-OUT(WRITE-ID) TO NEWMAST-MAJOR.
           MOVE STU-UNITS-COMP-OUT(WRITE-ID) TO NEWMAST-UC.
           MOVE STU-TTL-PTS-OUT(WRITE-ID) TO NEWMAST-TP.
           MOVE STU-UNITS-IP-OUT(WRITE-ID) TO NEWMAST-UIP.
           PERFORM 720-CLASS-CONVERTER-NM.
           MOVE NEWMAST-OUT TO WORK-AREA.
           WRITE WORK-AREA AFTER ADVANCING 1 LINE.
           ADD 1 TO WRITE-ID.



      *----------------*
      *converts letter grade to number for calculation for the ERRPT report
       710-CLASS-CONVERTER-ER.
           EVALUATE CR-CLASS-OUT(ID-COUNT-CR)
               WHEN '1'
                   MOVE "FRESHMAN" TO CC-ERRR
               WHEN '2'
                   MOVE "SOPHOMORE" TO CC-ERRR
               WHEN '3'
                   MOVE "JUNIOR" TO CC-ERRR
               WHEN '4'
                   MOVE "SENIOR" TO CC-ERRR
               END-EVALUATE.


       720-CLASS-CONVERTER-NM.
      *converts letter grade to number for calculation for the NEWMAST report
           EVALUATE STU-CLASS-OUT(WRITE-ID)
               WHEN '1'
                   MOVE "FRESHMAN" TO CC-NEWMAST
               WHEN '2'
                   MOVE "SOPHOMORE" TO CC-NEWMAST
               WHEN '3'
                   MOVE "JUNIOR" TO CC-NEWMAST
               WHEN '4'
                   MOVE "SENIOR" TO CC-NEWMAST
               END-EVALUATE.

      *----------------*
       750-WRITE-ERRRPT-HDRS.
      *writes the headers for the ERRRPT report
           MOVE ERRRPT-TOPLINE TO ERROR-AREA.
           WRITE ERROR-AREA AFTER ADVANCING 1 LINE.

           MOVE FILLER-LINE TO ERROR-AREA.
           WRITE ERROR-AREA AFTER ADVANCING 1 LINE.

           MOVE ERRRPT-HDRS TO ERROR-AREA.
           WRITE ERROR-AREA AFTER ADVANCING 1 LINE.

           MOVE DASHED-LINE TO ERROR-AREA.
           WRITE ERROR-AREA AFTER ADVANCING 1 LINE.
      *----------------*

      *----------------*
       800-WRITE-ERRRPT.
      *writes the ERRPT report
           MOVE CR-STU-ID-OUT(ID-COUNT-CR) TO ERRR-ID.
           MOVE CR-STU-NAME-OUT(ID-COUNT-CR) TO ERRR-NAME.
      *    MOVE CR-CLASS-OUT(ID-COUNT-CR) TO ERRR-CLASS.
           MOVE CR-MAJOR-OUT(ID-COUNT-CR) TO ERRR-MAJOR.
           PERFORM 710-CLASS-CONVERTER-ER.
           MOVE ERRRPT-OUT TO ERROR-AREA.
           WRITE ERROR-AREA AFTER ADVANCING 1 LINE.
      *----------------*


      *----------------*
       900-CALCULATIONS.
      *performs calculations for total grade points and adds to units completed

      *--------Adds # of course units to units completed*
           ADD CR-CRSE-UNITS-OUT(ID-COUNT-CR) TO
           STU-UNITS-COMP-OUT(ID-COUNT-STU).

           EVALUATE CR-SEM-GRADE-OUT(ID-COUNT-CR)
                   WHEN 'A'
                       COMPUTE CALC = 4
                   WHEN 'B'
                       COMPUTE CALC = 3
                   WHEN 'C'
                       COMPUTE CALC = 2
                   WHEN 'D'
                       COMPUTE CALC = 1
                   WHEN 'F'
                       COMPUTE CALC = 0
                  END-EVALUATE.

           COMPUTE CALC = CALC * CR-CRSE-UNITS-OUT(ID-COUNT-CR).
           ADD CALC TO STU-TTL-PTS-OUT(ID-COUNT-STU).
           MOVE 0 TO CALC.
      *----------------*


       END PROGRAM FinalProject.
