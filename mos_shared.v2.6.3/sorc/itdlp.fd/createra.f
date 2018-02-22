      SUBROUTINE CREATERA(KFILDO,L3264B,KFILRA,CRAOUT,MAXENT,NBYTES)
C
C        HISTORY
C
C        DECEMBER  2014   ENGLE       CREATED. ADAPTED FROM U350.F
C
C        PURPOSE
C
C            TO CREATE A NEW, EMPTY RANDOM ACCESS FILE.
C
C        VARIABLES (INPUT/OUTPUT)
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C              L3264B = BIT SIZE OF SCALAR INTEGERS.
C              KFILRA = UNIT NUMBER OF THE OUTPUT RANDOM ACCESS FILE.
C              CRAOUT = CHARACTER*256 THAT HOLDS THE OUTPUT TDLPACK RA FILENAME.
C              MAXENT = MAXIMUM NUMBER OF KEYS IN A KECORD.
C              NBYTES = SIZE OF A PHYSICAL RECORD.
C
C        VARIABLES (INTERNAL)
C                NIDS = NUMBER OF INTEGER WORDS IN EACH ID IN EACH RECORD. (PARAMETER)
C                  NW = SET TO SOME NUMBER SUCH THAT NW GE NBYTES*6. IS IS USED AS THE
C                       DIMENSION FOR KEYR( ) FOR WRITING A BLANK KEY RECORD. (PARAMETER)
C             KEYR(J) = AN ARRAY THAT HOLDS THE KEYS PORTION OF A KEY
C                       RECORD, ZEROED FOR WRITING.  A KEY RECORD IS 
C                       ALWAYS WRITTEN TO AVOID DIFFICULTIES LATER.
C            MASTR(J) = 6 WORDS (J=1,6) TO WRITE AS MASTER KEY RECORD 
C                       TO EACH FILE CREATED.  THE WORDS ARE: 
C                       1 = RESERVED.  SET TO ZERO.
C                       2 = NUMBER OF INTEGER WORDS IN ID FOR EACH
C                           RECORD.  INITIALIZED TO NIDS.  THIS IS
C                           4 UNLESS CHANGES ARE MADE TO THE SOFTWARE.
C                       3 = NUMBER OF WORDS IN A PHYSICAL RECORD =
C                           NBYTES/(L3264B/8).
C                       4 = NUMBER OF KEY RECORDS STORED IN THE FILE.
C                           INITIALLY = 1.
C                       5 = MAXIMUM NUMBER OF KEYS IN A KEY RECORD.
C                           INITIALIZED TO MAXENT READ FROM CONTROL FILE.
C                       6 = LOCATION OF WHERE THE FIRST PHYSICAL RECORD
C                           OF THE LAST LOGICAL KEY RECORD OF THE FILE
C                           IS LOCATED.
C           NOPREC(J) = 6 WORDS (J=1,6) USED BY THE FILE SYSTEM.  WORDS
C                       3, 5, AND 6 ARE WRITTEN AS PART OF THE KEY
C                       RECORD.  THE WORDS ARE:
C                       1 = LOCATION IN KEYREC( , , ) OF THE KEY RECORD.
C                           ZERO INITIALLY.
C                       2 = LOCATION OF THIS KEY RECORD IN THE FILE.
C                           SET TO 2.
C                       3 = NUMBER OF SLOTS FILLED IN THIS KEY.
C                           ZERO INITIALLY.
C                       4 = INDICATES WHETHER (1) OR NOT (0) THE KEY
C                           RECORD HAS BEEN MODIFIED AND NEEDS TO BE
C                           WRITTEN.  ZERO INITIALLY.
C                       5 = NUMBER OF PHYSICAL RECORDS IT TAKES TO HOLD
C                           THIS LOGICAL KEY RECORD.  THIS IS FILLED BY
C                           WRKEYM.
C                       6 = INITIALLY 99999999 TO INDICATE THIS IS THE
C                           LAST KEY RECORD IN THE FILE.
C
C        MOS-2000 SUBROUTINES CALLED
C           WRKEYM
C
C        ITDLP SUBROUTINE CALLED
C           ITDLP_STOP
C
      IMPLICIT NONE
C        INPUT/OUTPUT VARIABLES
      INTEGER, INTENT(IN) :: KFILDO,L3264B,KFILRA,MAXENT
      INTEGER, INTENT(INOUT) :: NBYTES
      CHARACTER(LEN=*), INTENT(IN) :: CRAOUT
C        INTERNAL VARIABLES
      INTEGER, PARAMETER :: NIDS=4
      INTEGER, PARAMETER :: NW=140000
      INTEGER :: J,IER,IOS,NSIZE
      INTEGER, DIMENSION(6) ::  MASTR,NOPREC
      INTEGER, DIMENSION(7*NW) :: KEYR
C        INITIALIZE
      KEYR(:)=0
      MASTR(:)=0
      NOPREC(:)=0
C
C        FILL ARRAY TO WRITE TO MASTER KEY RECORD. 
C 
      NBYTES=((NBYTES+7)/8)*8
      MASTR(1)=0
      MASTR(2)=NIDS 
      MASTR(3)=NBYTES/(L3264B/8)
      MASTR(4)=1      
      MASTR(5)=MAX(MAXENT,((NBYTES*8/L3264B)-3)/6)
      MASTR(6)=2
C
C        CHECK NBYTES AGAINST NW.
C
      IF(NBYTES*6.GT.NW)THEN
         NSIZE=7*NBYTES
         WRITE(6,105)NBYTES
 105     FORMAT(/' error: Trouble creating random access file. ',
     1           ' NBYTES = ',I0.1,' too small.'/)
         WRITE(KFILDO,110)NW,NSIZE
 110     FORMAT(/' ****SIZE OF ARRAY KEYR( ) =',I8,
     1           ' TOO SMALL.  INCREASE TO GE',I8/
     2           '     FILE NOT CREATED')
         CALL ITDLP_STOP
      ENDIF
C
C        FILL ARRAY NOPREC( ).
C
      NOPREC(1)=0
      NOPREC(2)=2
      NOPREC(3)=0
      NOPREC(4)=0
      NOPREC(5)=0
      NOPREC(6)=99999999
C
C        OPEN FILE AND WRITE MASTER KEY RECORD. 
C
      OPEN(UNIT=KFILRA,FILE=TRIM(CRAOUT),STATUS='NEW',
     1     CONVERT='BIG_ENDIAN',ACCESS='DIRECT',RECL=NBYTES,IOSTAT=IOS)
      IF(IOS.NE.0)THEN
         WRITE(6,115)IOS
 115     FORMAT(/' error: Trouble creating random access file. ',
     1           'IOSTAT = ',I0.1/)
         CALL ITDLP_STOP 
      ENDIF
C
C        WRITE MASTER KEY RECORD.
C
      WRITE(KFILRA,REC=1,IOSTAT=IOS)MASTR 
      IF(IOS.NE.0)THEN
         WRITE(6,120)IOS
 120     FORMAT(/' error: Trouble writing master key record. ',
     1           'IOSTAT = ',I0.1/)
         CALL ITDLP_STOP
      ENDIF
C
C        WRITE KEY RECORD WITH SUBROUTINE WRKEYM EVEN
C        THOUGH IT CONTAINS NO KEYS.
C
      CALL WRKEYM(KFILDO,KFILRA,NOPREC,KEYR,MASTR(5)*6+3,
     1              MASTR(3),'ITDLP',IER)
C
C        CLOSE FILE.
C
      CLOSE(UNIT=KFILRA,IOSTAT=IOS) 
      IF(IOS.NE.0)THEN
         WRITE(6,125)IOS
 125     FORMAT(/' error: Trouble closing new random access file. ',
     1           'IOSTAT = ',I0.1/)
         CALL ITDLP_STOP
      ENDIF
      WRITE(KFILDO,160)TRIM(CRAOUT),NBYTES,MASTR(5)
 160  FORMAT(/' FILE CREATED'/
     1        '    NAME                      = 'A/
     2        '    BYTES PER PHYSICAL RECORD = ',I8/
     3        '    ENTRIES PER KEY RECORD    = ',I8) 
C
      RETURN
      END SUBROUTINE CREATERA
