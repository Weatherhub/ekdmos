      PROGRAM REPACKMESO
C
C        JANUARY  2015   ENGLE   CREATED
C
C        PURPOSE
C           UTILITY PROGRAM TO REMOVE A STRING FROM MESONET STATION
C           IDS. THE MDL MADIS MESONET ARCHIVE PROCESS APPENDS A STRING
C           "_XX" TO THE BASE MESONET ID WHERE XX REPRESENTS A STATION
C           DUPLICATION NUMBER FROM 1 THROUGH 99.
C
C        DATA SET USE
C            KFILIN    - UNIT NUMBER OF THE INPUT TDLPACK VECTOR
C                        FILE.
C            KFILOUT   - UNIT NUMBER OF THE OUTPUT TDLPACK VECTOR FILE
C                        WITH A MODIFIED STATION CALL LETTER RECORD
C
C        VARIABLES   
C            CCALL(K) = STATION CALL LETTERS (K=1,NSTA). (CHARACTER*8)
C              CFILIN = FILENAME OF INPUT TDLPACK FILE. (CHARACTER*256)
C             CFILOUT = FILENAME OF OUTPUT TDLPACK FILE. (CHARACTER*256)
C              CPACK1 = CHARACTER REPRESENTATION OF IPACK1. (CHARACTER*4)
C                 IDX = POSITION OF "_" IN CCALL.
C                 IER = STATUS RETURN
C                 IOS = IOSTAT VALUE FROM I/O STATEMENTS.
C              IPACK1 = HOLDS IPACK(1).
C            IPACK(J) = WORK ARRAY (J=1,ND5).
C              KFILIN = UNIT NUMBER OF INPUT TDLPACK FILE.
C             KFILOUT = UNIT NUMBER OF OUTPUT TDLPACK FILE.
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  SET BY PARAMETER.
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).  
C                       CALCULATED BY PARAMETER, BASED ON L3464B.
C               NARGS = NUMBER OF COMMAND LINE ARGURMENTS PASSED IN
C                       (SHOULD BE 2).
C           NBYTES(L) = USED TO ACCESS DATA AS EITHER 2 WORDS FOR A 32-BIT
C                       MACHINE OR 1 WORD FOR A 64-BIT MACHINE (L=1,2).
C               NBYWD = EITHER 4 OR 8 DEPENDING ON THE SETTING OF L3264B.
C                 ND1 = THE MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                 ND5 = DIMENSION OF IPACK( ).
C                NSTA = NUMBER OF STATIONS.
C              NTOTBY = THE TOTAL NUMBER OF BYTES ON THE FILE.  IT IS
C                       INCREASED BY 32.
C              NTOTRC = THE TOTAL NUMBER OF RECORDS ON THE FILE.  IT IS
C                       INCREASED BY 1.
C              NWORDS = THE NUMBER OF WORDS IN IPACK( ).  ALSO TREATED
C                       AS ITS DIMENSION.
C
C        NONSYSTEM SUBROUINES USED
C            TRAIL, WRITEP
C
      IMPLICIT NONE
C  
      INTEGER, PARAMETER :: L3264B=32
      INTEGER, PARAMETER :: ND1=13000
      INTEGER, PARAMETER :: ND5=50000
C
      INTEGER, DIMENSION(2) :: NBYTES
      INTEGER, DIMENSION(ND5) :: IPACK
      CHARACTER(LEN=8), DIMENSION(ND1) :: CCALL
C
      INTEGER :: I,J,N
      INTEGER :: KFILIN,KFILOUT,IOS,IER,NARGS
      INTEGER :: NTRASH,IOCTET,L3264W,NBYWD
      INTEGER :: IPACK1,IDX,NSTA
      INTEGER :: NWORDS,NTOTBY,NTOTRC
      CHARACTER(LEN=4) :: CPACK1
      CHARACTER(LEN=256) :: CFILIN,CFILOUT
C
C        INITIALIZE VARIABLES
C
      KFILIN=10
      KFILOUT=20
      L3264W=64/L3264B
      NBYWD=L3264B/8
      NTOTBY=0
      NTOTRC=0
      IPACK(:)=0
      NBYTES(:)=0
C
      CALL W3TAGB('mdl_repackmeso',2015,21,21,'OST22')
C
C        CHECK COMMAND ARGUMENTS
C
      NARGS=COMMAND_ARGUMENT_COUNT()
      IF(NARGS.NE.2) THEN
         WRITE(6,100)
 100     FORMAT(/'Usage: mdl_repackmeso <input_TDLPACK> ',
     +           '<output_TDLPACK>'/)
         GO TO 9999
      ENDIF
C
C        GET FILE NAMES FROM COMMAND LINE ARGUMENTS
C
      CALL GETARG(1,CFILIN)
      CALL GETARG(2,CFILOUT)
C
C        OPEN INPUT AND OUTPUT FILES
C
      OPEN(UNIT=KFILIN,FILE=TRIM(CFILIN),FORM="UNFORMATTED",
     +     IOSTAT=IOS,ERR=900,CONVERT="BIG_ENDIAN",STATUS="OLD")
      OPEN(UNIT=KFILOUT,FILE=TRIM(CFILOUT),FORM="UNFORMATTED",
     +     IOSTAT=IOS,ERR=900,CONVERT="BIG_ENDIAN",STATUS="REPLACE")
C
C        ITERATE OVER ALL RECORDS ON FILE
C
      DO
C
C        READ FROM INPUT FILE
C
      IF(L3264B.EQ.32)THEN
         READ(KFILIN,IOSTAT=IOS,ERR=900,END=900)NTRASH,IOCTET,
     +   (IPACK(I),I=1,(IOCTET/NBYWD))
      ELSEIF(L3264B.EQ.64)THEN
         READ(KFILIN,IOSTAT=IOS,ERR=900,END=900)IOCTET,
     +   (IPACK(I),I=1,(IOCTET/NBYWD))
      ENDIF
      NWORDS=IOCTET/NBYWD
C
C        GET CHARACTER REPRESENTATION OF IPACK(1).
C
      IPACK1=IPACK(1)
      CPACK1=TRANSFER(IPACK1,CPACK1)
      IF((CPACK1.EQ.'TDLP'.OR.CPACK1.EQ.'PLDT'))THEN
C           RECORD IS TDLPACK. WRITE IPACK TO
C           OUTPUT FILE.
         CALL WRITEP(6,KFILOUT,IPACK,NWORDS,NTOTBY,NTOTRC,L3264B,IER)
      ELSE
C           RECORD CAN NOW EITHER BE STATION CALL LETTER
C           RECORD OR THE TRAILER RECORD.
         IF(IPACK1.EQ.0)THEN
C              TRAILER RECORD
            CALL TRAIL(6,KFILOUT,L3264B,L3264W,NTOTBY,NTOTRC,IER)
         ELSE
C              STATION CALL LETER RECORD. BACKSPACE AND REREAD
C              INTO CCALL.
            BACKSPACE(KFILIN)
            IF(L3264B.EQ.32)THEN
               READ(KFILIN,IOSTAT=IOS,ERR=900,END=900)NTRASH,
     +         IOCTET,(CCALL(I),I=1,(IOCTET/8))
            ELSEIF(L3264B.EQ.64)THEN
               READ(KFILIN,IOSTAT=IOS,ERR=900,END=900)IOCTET,
     +         (CCALL(I),I=1,(IOCTET/8))
            ENDIF
C              ITERATE OVER ALL STATION IDS, DETERMINE LOCATION
C              OF "_" AND WRITE CALL LETTER WITHOUT "_XX".
            NSTA=IOCTET/8
            DO N=1,NSTA
               IDX=INDEX(CCALL(N),'_')
               IF(IDX.GT.0)CCALL(N)=CCALL(N)(1:IDX-1)
            ENDDO
            NBYTES(L3264W)=NSTA*8
            WRITE(KFILOUT,IOSTAT=IOS,ERR=900)
     +      (NBYTES(J),J=1,L3264W),(CCALL(I),I=1,NSTA)
C              INCREMENT NUMBER OF RECORDS AND BYTES WRITTEN.
            NTOTRC=NTOTRC+1
            NTOTBY=NTOTBY+(NBYTES(L3264W)+8)
         ENDIF
C
      ENDIF

      END DO
C
 900  IF(IOS.GT.0.OR.IER.NE.0)THEN
         WRITE(6,9000)IOS
 9000    FORMAT(/'Error: IOSTAT = ',I3/,
     +           '          IER = ',I3/)
         GOTO 9990
      ENDIF  
C
C        CLEAN UP
C
      WRITE(6,9001)NSTA,NTOTRC,NTOTBY,(NTOTBY+(NTOTRC*8))
 9001 FORMAT(/' SUMMARY OF DATA WRITTEN'/
     +        ' -----------------------'/
     +        ' NUMBER OF STATIONS = ',I11/
     +        ' NUMBER OF RECORDS  = ',I11/
     +        ' NUMBER OF BYTES    = ',I11/
     +        ' TOTAL FILE SIZE    = ',I11/)
 9990 CLOSE(KFILIN)
      CLOSE(KFILOUT)
C
 9999 CALL W3TAGE('mdl_repackmeso')
      END PROGRAM REPACKMESO
