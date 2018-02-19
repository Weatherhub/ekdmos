      SUBROUTINE CKRAEND(KFILDO,KFILX,CFILX,ISYSEND,IFILEND,
     1                   CONVERTX,IER)
C
C        MAR  2012   ENGLE  MDL   MOS-2000 CREATED FOR MIGRATION
C                                 TO INTEL/LINUX PLATFORM.
C
C        PURPOSE
C            DETERMINE THE ENDIAN OF MOS-2000 RANDOM ACCESS FILE.
C
C            THE SUBROUTINE MUST FIRST KNOW THE ENDIAN OF THE SYSTEM.
C            THIS IS DONE BY CALLING CKSYSEND. ONCE DETERMINED, THE
C            RANDOM ACCESS FILE IS OPENED WITH A RECORD LENGTH OF 6
C            (4-BYTE) WORDS (24-BYTES), ENOUGH TO READ THE MASTER
C            KEY RECORD.
C
C            THE SECOND WORD IN THE FILE HOLDS THE NUMBER OF WORDS
C            IN EACH MOS-2000 ID. THIS VALUE WILL BE 4 (SEE NOTE).
C            THE FILE ENDIAN IS THEN DETERMINED BY HOW THIS WORD
C            IS INTERPETED BY THE SYSTEM.
C
C                - IF THE VALUE IS 4, THE FILE ENDIAN IS THE SAME
C                  AS THE SYSTEM ENDIAN.
C                - IF THE VALUE IS 2^26, THE 4-BYTE WORD CONTAINING
C                  THE VALUE 4 WAS READ IN REVERSE BYTE ORDER, THUS
C                  TELLING US THE FILE ENDIAN IS OPPOSITE THE SYSTEM.
C                - IF THE VALUE IS SOMETHING ELSE, THERE IS A PROBLEM
C                  READING THE RANDOM ACCESS FILE. THIS SHOULD BE
C                  CONSIDERED A FATAL ERROR.
C
C            ONCE THE FILE ENDIAN IS DETERMINED, CONVERTX IS SET TO
C            EITHER 'BIG_ENDIAN' OR 'LITTLE_ENDIAN'. REMEMBER, THIS
C            IS THE ENDIAN OF THE FILE (I.E. WHAT THE COMPILER IS 
C            CONVERTING FROM WHEN READING).
C
C        NOTE: AT SOME POINT IN THE FUTURE, THIS VALUE MAY CHANGE AS
C              THE SOFTWARE EVOLVES. THIS CHECK WILL HAVE TO CHANGE
C              ACCORDINGLY.
C
C        DATA SET USE
C            KFILX     - UNIT NUMBER FOR INPUT RANDOM ACCESS FILE.  (INPUT)
C            KFILDO    - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT)
C              KFILX  = UNIT NUMBER FOR THE INPUT RANDOM ACCESS FILE. (INPUT)
C              CFILX  = FILE NAME OF THE INPUTER RANDOM ACCESS FILE.
C                       (CHARACTER*60) (INPUT)
C             ISYSEND = HOLD THE VALUE THAT REPRESENTS THE SYSTEM ENDIAN.
C                       THIS IS SET BY CALLING CKSYSEND. (OUTPUT)
C                        -1 = SYSTEM IS LITLE-ENDIAN.
C                         1 = SYSTEM IS BIG-ENDIAN.
C             IFILEND = HOLD THE VALUE THAT REPRESENTS THE ENDIAN OF THE
C                       INPUT RANDOM ACCESS FILE. (OUTPUT)
C            CONVERTX = HOLDS THE STRING THAT WILL TELL THE AN OPEN
C                       STATEMENT HOW TO CONVERT THE FILE.
C                       (CHARACTER*20) (OUTPUT)
C                 IER = STATUS RETURN FROM SUBROUTINE. (OUTPUT)
C                         0 = GOOD VALUE.
C            IPACK( ) = 6 WORDS (I=1,6) TO HOLD THE MASTER KEY RECORD. (INTERNAL)
C              NWDSID = NUMBER OF INTEGER WORDS IN A MOS-2000 ID. (INTERNAL)
C
C        NON SYSTEM SUBROUTINES CALLED
C           CKSYSEND
C
      IMPLICIT NONE
C
C        I/O VARIABLES
C
      INTEGER :: KFILDO,KFILX,ISYSEND,IFILEND,IER
      CHARACTER*20 :: CONVERTX
      CHARACTER*60 :: CFILX
C
C        INTERNAL VARIABLES
C
      INTEGER :: I
      INTEGER :: ITEMP(6),IOS,NWDSID
      CHARACTER*20 :: CENDIAN
C
      IFILEND=0
C
C        CALL CKSYSEND TO CHECK THE ENDIAN OF THE SYSTEM.
C
      CALL CKSYSEND(KFILDO,'NOPRINT',ISYSEND,IER)
C
C        OPEN RANDOM ACCESS FILE. FOR THE PURPOSES OF DETERMINING
C        THE ENDIAN, WE ONLY NEED TO KNOW THE THE FIRST 6 WORDS
C        OF THE FILE (24 BYTES IF WORD = 4 BYTES).
C
C        FILE= REMOVED FROM OPEN STATEMENT FOR OPERATIONAL
C        PURPOSES.
COPER
      OPEN(UNIT=KFILX,ACCESS='DIRECT',STATUS='OLD',
     1     FORM='UNFORMATTED',RECL=24,IOSTAT=IOS,ERR=990)
COPER
CDEV
C      OPEN(UNIT=KFILX,FILE=CFILX,ACCESS='DIRECT',STATUS='OLD',
C     1     FORM='UNFORMATTED',RECL=24,IOSTAT=IOS,ERR=990)
CDEV
C
      READ(KFILX,REC=1)(ITEMP(I),I=1,6)
C
C        THE SECOND WORD IN ITEMP ( ) CONTAINS THE NUMBER OF
C        WORDS THAT A MOS-2000 ID CONSUMES. STORE THIS VALUE
C        IN NWDSID.
C
      NWDSID=ITEMP(2)
C
C        THE FOLLOWING IF BLOCK WILL CHECK AGAINST THE VALUE OF
C        NWDSID.
C
      IF(NWDSID.EQ.4)THEN
C           VALUE IS 4. FILE ENDIAN IS SAME AS SYSTEM ENDIAN.
        IFILEND=ISYSEND
      ELSEIF(NWDSID.EQ.2**26)THEN
C           VALUE IS 2^26. FILE ENDIAN IS OPPOSITE THE SYSTEM ENDIAN.
        IFILEND=-ISYSEND
      ELSE
        GO TO 990
      ENDIF
C
C        KEYING OFF IFILEND, SET CONVERTX ACCORDINGLY.
C
      IF(IFILEND.EQ.-1)THEN
        CONVERTX='LITLE_ENDIAN'
        CENDIAN='LITTLE-ENDIAN'
      ELSEIF(IFILEND.EQ.1)THEN
        CONVERTX='BIG_ENDIAN'
        CENDIAN='BIG-ENDIAN'
      ELSE
        GO TO 990
      ENDIF
C
C        TEST IF THE RA FILE IS SAME ENDIAN AS SYSTEM.
C
      IF(IFILEND.EQ.ISYSEND) CONVERTX='NATIVE'
C
C        PRINT INFO TO KFILDO.
C
      WRITE(KFILDO,100)KFILX,TRIM(CFILX),TRIM(CENDIAN)
 100  FORMAT(/,' ENDIAN ANALYSIS OF RANDOM-ACCESS FILE',/,
     1       5X,I3,4X,A,4X,A)
      GO TO 992
C
C        ERROR HANDLING.
C
 990  WRITE(KFILDO,991)KFILX,IOS
 991  FORMAT(/,' ****COULD NOT DETERMINE ENDIAN OF FILE',
     1         ' ON UNIT NO. ',I3,'. PROGRAM WILL STOP.',/,
     2         '     IOSTAT = ',I4)
C
C        CLOSE THE FILE.
C
 992  CLOSE(KFILX)
C
 995  RETURN
      END SUBROUTINE CKRAEND  
        
