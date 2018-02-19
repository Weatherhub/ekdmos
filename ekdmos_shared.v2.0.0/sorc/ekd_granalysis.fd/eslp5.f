      SUBROUTINE ESLP5(KFILDO,DATA,DBB,BB,ER1,IFLAG)
C
C        JUNE     1993   GLAHN, CHAMBERS   TDL   HP9000
C        AUGUST   2000   GLAHN   MODIFIED FOR LAMP-2000
C        JUNE     2004   GLAHN   MODIFIED FOR MOS-2000
C                                SAME AS ELSP EXCEPT NAME
C
C        PURPOSE
C            IF SLP IS LT 970 MB (GT 1040 MB) AND DISAGREES WITH FIRST
C            GUESS BY GE 50 MB, 100 MB IS ADDED (SUBTRACTED) ON THE
C            THEORY REPORT WAS CODED INCORRECTLY.  CALLED FROM ESP
C            FOR SEA LEVEL PRESSURE ONLY.
C
C        DATA SET USE
C            KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C                DATA = DATA VALUE TO CHECK.  (INPUT)
C                       IF IT MEETS ER1 ERROR CRITERION AFTER
C                       MODIFICATION, IT IS RETURNED MODIFIED.
C                       (OUTPUT)
C                 DBB = DIFFERENCE BETWEEN DATA AND ANALYSIS.
C                       DBB IS CHANGED TO MATCH THE NEW VALUE OF
C                       DATA( ) IF DATA( ) IS CHANGED.  (INPUT/OUTPUT)
C                 ER1 = ERROR CRITERION.  (INPUT)
C                  BB = INTERPOLATED VALUE FROM ANALYSIS.  (INPUT)
C               IFLAG = 0 = DID NOT CHANGE VALUE.
C                       1 = VALUE CHANGED AND IT NOW MEETS ER1
C                           CRITERION.
C                       (OUTPUT)
C
C        NONSYSTEM SUBROUTINES CALLED
C            NONE.
C
      IFLAG=0
C
      IF(DATA.LT.970..AND.DBB.LT.-50.)THEN
         TEMP=DATA+100.
         DBBT=TEMP-BB
C
         IF(ABS(DBBT).LE.ER1)THEN
            DATA=TEMP
            IFLAG=1
            DBB=DBBT
         ENDIF
C         
      ELSEIF(DATA.GT.1040..AND.DBB.GT.+50.)THEN
         TEMP=DATA-100.
         DBBT=TEMP-BB
         IF(ABS(DBBT).LE.ER1)THEN
            DATA=TEMP
            IFLAG=1
            DBB=DBBT
         ENDIF
C
      ENDIF
C
      RETURN
      END
