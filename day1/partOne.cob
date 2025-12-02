       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARTONE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        SELECT INPUTFILE ASSIGN TO 'input.txt'
        ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUTFILE
           RECORDING MODE IS V.
       01 INPUTFILE-FILE.
               05 DIRECTION PIC A(1).
               05 MAGNITUDE PIC X(3).

       WORKING-STORAGE SECTION.
       01 WS-INPUTFILE.
               05 WS-DIRECTION PIC A(1).
               05 WS-MAGNITUDE PIC X(3).
       01 WS-EOF PIC A(1).
       01 WS-STATE PIC S9(10) VALUE +50.
       01 WS-LEFT PIC A(1) VALUE 'L'.
       01 WS-RIGHT PIC A(1) VALUE 'R'.
       01 WS-NUM-MAG PIC 9(03).
       01 WS-SCORE PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
           OPEN INPUT INPUTFILE.
           PERFORM UNTIL WS-EOF='Y'
                   READ INPUTFILE INTO WS-INPUTFILE
                           AT END 
                                   DISPLAY WS-SCORE
                                   MOVE 'Y' TO WS-EOF
                           NOT AT END 
                            MOVE FUNCTION NUMVAL (WS-MAGNITUDE)
                                   TO WS-NUM-MAG
                                IF WS-DIRECTION IS EQUAL TO "L"
                                   SUBTRACT WS-NUM-MAG FROM WS-STATE
                                ELSE
                                   ADD WS-NUM-MAG TO WS-STATE
                           END-IF
                           MOVE FUNCTION MOD (WS-STATE 100) TO WS-STATE
                           IF WS-STATE IS EQUAL TO 0
                                   ADD 1 TO WS-SCORE
                           END-IF
                   END-READ
           END-PERFORM.
           CLOSE INPUTFILE.
           STOP RUN.
