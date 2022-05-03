      ******************************************************************
      * Author:EVGENII FILATOV
      * Date:02.03.2021
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT1.

        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT   STUDENT-FILE-OUT
               ASSIGN TO "C:\Users\Filat\COBOL_LABS\TEST-DATA.txt"
                    ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.

       FD STUDENT-FILE-OUT.
       01 STUDENT-RECORD-OUT.
           05  STUDENT-NUMBER-OUT PIC 9(6).
           05  TUITION-OWED-OUT   PIC 9(6).
           05  STUDENT-NAME-OUT   PIC X(40).

       WORKING-STORAGE SECTION.

        01 STUDENT-RECORD-IN.
           05 STUDENT-NUMBER PIC 9(6).
           05 TUITION-OWED   PIC 9(6).
           05 STUDENT-NAME   PIC X(40).
        01 EOF-FLAG PIC X(3).

       PROCEDURE DIVISION.

       PRODUCE-STUDENT-FILE-OUT.
           PERFORM  INITIATE-SR-FILE
                    UNTIL EOF-FLAG = "YES".
                    200-CREATE-RECORDS.
                    200-TERMINATE-SR-FILE.


           INITIATE-SR-FILE.
               OPEN OUTPUT STUDENT-FILE-OUT.
               DISPLAY  "ADD NEW FILE: 'YES' OR 'NO'".
               ACCEPT EOF-FLAG.

           200-CREATE-RECORDS.
               300-ENTER-SR-DATA.
               300-WRITE-SR-RECORD.

           200-TERMINATE-SR-FILE.
               CLOSE STUDENT-FILE-OUT.
               DISPLAY "STUDENT NUMBER", STUDENT-NUMBER-OUT.
               DISPLAY "TUITION OWED" , TUITION-OWED-OUT.
               DISPLAY "STUDENT NAME", STUDENT-NAME-OUT.


           300-ENTER-SR-DATA.
               INITIALIZE STUDENT-RECORD-IN.
               DISPLAY "PLEASE ENTER STUDENT NUMBER".
               ACCEPT STUDENT-NUMBER.
               DISPLAY "PLEASE ENTER AMOUNT".
               ACCEPT TUITION-OWED.
               DISPLAY "PLEASE ENTER STUDENT NAME".
               ACCEPT STUDENT-NAME.

           300-WRITE-SR-RECORD.

               MOVE STUDENT-NUMBER TO STUDENT-NUMBER-OUT.
               MOVE TUITION-OWED TO TUITION-OWED-OUT.
               MOVE STUDENT-NAME TO STUDENT-NAME-OUT.
               WRITE STUDENT-RECORD-OUT.


       MAIN-PROCEDURE.
           STOP RUN.
       END PROGRAM PROJECT1.
