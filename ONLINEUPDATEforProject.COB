      *    *****************************************************************
      *     Author:
      *     Date:
      *     Purpose:
      *     Tectonics: cobc
      *    *****************************************************************
           IDENTIFICATION DIVISION.
           PROGRAM-ID. OnlineUpdate .
           ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT  STUDENT-FILE-IN
                   ASSIGN TO  ""
                       ORGANIZATION IS LINE SEQUENTIAL.
               SELECT  STUDENT-FILE-INDEXED
                   ASSIGN TO  ""
                       ORGANIZATION IS INDEXED
                       RECORD KEY IS IND-STUDENT-NUMBER
                       FILE STATUS IS STUDENT-FILE-STATUS.

          DATA DIVISION.
          FILE SECTION.
          FD STUDENT-FILE-IN.
          01  STUDENT-RECORD-IN.
               88 EOF-STUDENT-FILE-IN VALUE HIGH-VALUE.
           05 STUDENT-NUMBER  PIC 9(6).
           05 TUITION-OWED    PIC 9(5)V99.
           05 STUDENT-NAME    PIC X(40).
           05 PROGRAM-CODE    PIC X(5).
           05 COURSE-CODE-1   PIC X(7).
           05 AVERAGE-1       PIC 9(3).
           05 COURSE-CODE-2   PIC X(7).
           05 AVERAGE-2       PIC 9(3).
           05 COURSE-CODE-3   PIC X(7).
           05 AVERAGE-3       PIC 9(3).
           05 COURSE-CODE-4   PIC X(7).
           05 AVERAGE-4       PIC 9(3).
           05 COURSE-CODE-5   PIC X(7).
           05 AVERAGE-5       PIC 9(3).


          FD STUDENT-FILE-INDEXED.
          01  IND-STUDENT-RECORD.
               05 IND-STUDENT-NUMBER  PIC 9(6).
               05 IND-TUITION-OWED    PIC 9(5)V99.
               05 IND-STUDENT-NAME    PIC X(40).
               05 IND-PROGRAM-CODE    PIC X(5).
               05 IND-COURSE-CODE-1   PIC X(7).
               05 IND-AVERAGE-1       PIC 9(3).
               05 IND-COURSE-CODE-2   PIC X(7).
               05 IND-AVERAGE-2       PIC 9(3).
               05 IND-COURSE-CODE-3   PIC X(7).
               05 IND-AVERAGE-3       PIC 9(3).
               05 IND-COURSE-CODE-4   PIC X(7).
               05 IND-AVERAGE-4       PIC 9(3).
               05 IND-COURSE-CODE-5   PIC X(7).
               05 IND-AVERAGE-5       PIC 9(3).
           WORKING-STORAGE SECTION.

           01  GLOBALS.
               05  STUDENT-FILE-STATUS   PIC X(2).
               05  MORE-INPUT  PIC X(1).
               05  TRANSACTION-STUDENT-NUMBER    PIC 9(5).
               05  TRANSACTION-TUITION-OWED   PIC 9(5)V99.

           PROCEDURE DIVISION.

               100-ONLINE-UPDATE.
               PERFORM 200-INIT-UPDATE.
               PERFORM 200-COPY-RECORDS-TO-INDEXED.
               PERFORM 200-PROCESS-INPUT.
               PERFORM 200-FINALIZE-UPDATE.
               GOBACK.

               200-PROCESS-INPUT.
               MOVE ' ' TO MORE-INPUT.
               PERFORM UNTIL MORE-INPUT = 'n'
                 MOVE ' ' TO MORE-INPUT
                  PERFORM UNTIL MORE-INPUT = "n" OR MORE-INPUT = 'y'
                    DISPLAY 'Do you want to enter a transaction? (y/n)'
                       ACCEPT MORE-INPUT
                   END-PERFORM
                      IF MORE-INPUT = 'y'
                       PERFORM 300-ACCEPT-TRANSACTION
                       PERFORM 300-APPLY-TRANSACTION
                      END-IF
               END-PERFORM.

           200-FINALIZE-UPDATE.
               PERFORM 300-CLOSE-FILES.

           300-ACCEPT-TRANSACTION.
               DISPLAY 'Enter student number:'.
               ACCEPT TRANSACTION-STUDENT-NUMBER.
               DISPLAY 'Enter transaction amount:'
               ACCEPT TRANSACTION-TUITION-OWED.

           300-APPLY-TRANSACTION.
               MOVE TRANSACTION-STUDENT-NUMBER TO IND-STUDENT-NUMBER.
               START STUDENT-FILE-INDEXED
               KEY IS EQUAL TO IND-STUDENT-NUMBER
                   INVALID KEY
                       DISPLAY 'Unable to apply transaction.'
                               ' Wrong student number.'
                       EXIT PARAGRAPH
               END-START.

               READ STUDENT-FILE-INDEXED.

               REWRITE IND-STUDENT-RECORD.

           300-CLOSE-FILES.
               CLOSE STUDENT-FILE-IN.
               CLOSE STUDENT-FILE-INDEXED.

           200-COPY-RECORDS-TO-INDEXED.
               PERFORM 250-COPY-RECORD-LOOP
               UNTIL EOF-STUDENT-FILE-IN.

           250-COPY-RECORD-LOOP.
               READ STUDENT-FILE-IN
                   AT END SET EOF-STUDENT-FILE-IN TO TRUE.
               IF NOT EOF-STUDENT-FILE-IN
                   MOVE STUDENT-RECORD-IN TO IND-STUDENT-RECORD
                   WRITE IND-STUDENT-RECORD
               END-IF.

           200-INIT-UPDATE.
               OPEN INPUT STUDENT-FILE-IN.
               OPEN I-O STUDENT-FILE-INDEXED.

               END PROGRAM OnlineUpdate.
