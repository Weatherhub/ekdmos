      SUBROUTINE RDCLARGS(KFILDO,KFILIN,NARGS,CARGS,IGRD,IREC,ISRA,ISSQ,
     1                    CFILP,IGIS,CFILGIS,IPRINT,IBIN,CFILBIN,ICHDD,
     2                    NEWDD,FSIZE,CNEWDDFILE,IVT,IVECT,NCOLS,ISECT,
     3                    IMID,ID,IPROBE,CPRBTYP,CCOORDS,IFTYPE,IENDIAN,
     4                    IUDATE,DATE,IDEBUG,IRECSIZE,ITDLPOUT,CTDLPOUT,
     5                    ISTDIN,ITXT,CFILTXT,MASTER,IRAOUT,CRAOUT,
     6                    IAPPEND,IRASIZE)
C
C        HISTORY
C
C        MARCH     2012   ENGLE       CREATED.
C
C        PURPOSE
C
C            TO READ AND PROCESS COMMAND LINE ARGUMENTS GIVEN BY
C            BY THE USER.
C
C        VARIABLES (INPUT/OUTPUT)
C
C              KFILDO = UNIT NUMBER OF DEFAULT PRINT FOR MOS-2000
C                       ROUTINES. HERE KFILDO IS A SCRATCH FILE.
C              KFILIN = UNIT NUMBER OF THE INPUT TDLPACK FILE.
C               NARGS = NUMBER OF COMMAND LINE ARGUMENTS GIVEN.
C            CARGS( ) = CHARACTER*256 VARIABLE ARRAY OF SIZE (I=1,64)
C                       THAT WILL HOLD THE COMMAND LINE ARGUMENTS.
C                IGRD = FLAG FOR WHETHER THE '-grid' COMMAND LINE OPTION IS GIVEN.
C                IREC = RECORD NUMBER GIVEN BY '-rec <number>' COMMAND LINE OPTION.
C                ISRA = FLAG FOR IF INPUT TDLPACK FILE IS RANDOM ACCESS.
C                ISSQ = FLAG FOR IF INPUT TDLPACK FILE IS SEQUENTIAL.
C               CFILP = CHARACTER*60 VARIABLE THAT CONTAINS THE NAME OF
C                       THE TDLPACK FILE (SEQ OR RA).
C                IGIS = FLAG FOR WHETHER THE '-gis' COMMAND LINE OPTION IS GIVEN.
C             CFILGIS = CHARACTER*60 VARIABLE THAT CONTAINS THE NAME OF
C                       THE ASCII GIS OUTPUT FILE.
C              IPRINT = FLAG FOR THE LEVEL VERBOSITY OF PRINTING TO SCREEN.
C                       -1 = DEFAULT VALUE (SIMPLE OUTPUT)
C                       +1 = MORE VERBOSE OUTPUT, GIVEN BY '-v' OPTION.
C                IBIN = FLAG FOR WHETHER THE '-bin' COMMAND LINE OPTION IS GIVEN.
C             CFILBIN = CHARACTER*60 VARIABLE THAT CONTAINS THE NAME OF
C                       THE BINARY DATA OUTPUT FILE.
C               ICHDD = FLAG FOR WHETHER THE COMMAND LINE OPTION '-change-dd' IS GIVEN
C               NEWDD = THE NEW DD VALUE GIVEN IN THE COMMAND LINE OPTION '-change-dd'.
C               FSIZE = INPUT TDLPACK FILE SIZE.
C          CNEWDDFILE = CHARACTER*256 VARIABLE THAT CONTAINS THE NAME OF
C            CTDLPOUT = CHARACTER*256 VARIABLE THAT CONTAINS THE FILENAME OF
C                       OUTPUT TDLPACKFILE USING THE '-tdlp' OPTION.
C              IUDATE = FLAG FOR WHETHER THE '-date' COMMAND LINE OPTION IS GIVEN.
C                 IVT = FLAG FOR WHETHER THE '-vt' COMMAND LINE OPTION IS GIVEN.
C               IVECT = FLAG FOR WHETHER THE '-invect' COMMAND LINE OPTION IS GIVEN.
C               NCOLS = NUMBER TO COLUMNS TO PRINT STATION CALL LETTERS AND DATE FOR
C                       OUTPUT USING '-invect' OPTION. THE DEFAULT NUMBER IS 5, BUT
C                       USER CAN GIVE VALUES 1-5.
C               ISECT = VARIABLE TO HOLD THE WHICH TDLPACK SECTIONS ARE PRINTED. THIS
C                       VALUE IS DETERMINED BY THE COMMAND LINE OPTIONS '-is0', '-is1',
C                       '-is2', AND '-is4'. THE NUMBERED BIT THAT REPRESENTS A TDLPACK IS
C                       SECTION WILL BE TURNED ON.
C                IMID = FLAG FOR WHETHER THE '-id' COMMAND LINE OPTION IS GIVEN.
C               ID(I) = MOS-2000 ID GIVEN WITH '-id', (I=1,4)
C              IPROBE = FLAG FOR WHETHER THE '-probe' COMMAND LINE OPTION IS GIVEN.
C             CPRBTYP = CHARACTER VARIABLE THAT HOLDS THE PROBE TYPE.
C                       'ij' = PROBE A GRID FOR GRIDPONT VALUES.
C                       'll' = PROBE A GRID FOR AN INTERPOLATED VALUE AT A GIVEN
C                              LATITUDE/LONGITUDE (INTERPOLATION IS BILINEAR).
C             CCOORDS = CHARACTER VARIABLE THAT HOLDS THE COORDINATES OF THE PROBE
C                       POINT. THIS STRING WILL EITHER CONTAIN GRIDPOINT I,J
C                       COORDINATES OR LAT/LON COORDS.
C              IFTYPE = FLAG FOR WHETHER THE '-ftype' COMMAND LINE OPTION IS GIVEN.
C             IENDIAN = HOLDS EITHER THE VALUE OF IRAEND OR IFILEND.
C              IUDATE = FLAG FOR WHETHER THE '-date' COMMAND LINE OPTION IS GIVEN.
C                DATE = DATE GIVEN WITH THE '-date' OPTION.
C              IDEBUG = FLAG FOR WHETHER THE '-debug' OPTION HAS BEEN INVOKED.
C            ITDLPOUT = FLAG FOR WHEHTER THE '-tdlp' OPTION HAS BEEN INVOKED.
C           MASTER(I) = INTEGER ARRAY (I=1,6) THAT HOLDS THE RANDOM ACCESS
C                       MASTER KEY RECORD.
C              IRAOUT = FLAG FOR IF THE '-tdlpra' COMMAND LINE OPTION HAS BEEN INVOKED.
C              CRAOUT = CHARACTER*256 THAT HOLDS THE OUTPUT TDLPACK RA FILENAME.
C             IAPPEND = FLAG FOR WHETHER THE '-append' COMMAND OPTION HAS BEEN INVOKED
C             IRASIZE = FLAG FOR IF THE '-rasize' COMMAND LINE OPTION HAS BEEN INVOKED
C                       1 = SMALL TEMPLATE (DEFAULT)
C                       2 = LARGE TEMPLATE
C
C        VARIABLES (INTERNAL)
C
C             ARGSKIP = LOGICAL VARIABLE THAT DETERMINES WHEN TRUE TO PERFORM SKIPPING OF
C                       COMMAND LINE OPTIONS.
C               CDATE = CHARACTER*10 THAT HOLDS THE DATE GIVEN WITH THE '-date' OPTION.
C              CID(I) = CHARACTER ARRAY THAT HOLDS THE CHARACTER VERSION OF THE MOS-2000
C                       ID THAT IS GIVEN FROM THE COMMAND LINE. (I=1,4)
C                 IOS = THE VALUE OF IOSTAT FROM READ/WRITE STATEMENTS.
C               NARGS = THE NUMBER OF COMMAND LINE ARGUMENTS GIVEN.
C            NARGSKIP = THE NUMBER OF COMMAND LINE ARGUMENTS TO SKIP. THIS IS
C                       USED TO SKIP OVER ARGUMENTS (SUB OPTIONS TO A GIVEN OPTION)
C                       THAT HAVE ALREADY BEEN PROCESSED.
C                NNUM = USED TO COUNT THE INDIVDUAL NUMBERS GIVEN AS PART OF A MOS-2000
C                       ID WHEN THE '-id' OPTION IS GIVEN.
C
C        SYSTEM SUBROUTINES CALLED
C           GETARG
C
      IMPLICIT NONE
C        INPUT/OUTPUT VARIABLES     
      INTEGER, INTENT(IN) :: KFILDO,KFILIN
      INTEGER :: IGIS,IGRD,IREC,ISRA,ISSQ,IPRINT,IBIN,ICHDD,NEWDD
      INTEGER :: FSIZE,IVT,IVECT,NCOLS,ISECT,IMID,ID(4),IPROBE,IFTYPE
      INTEGER :: IENDIAN,IUDATE,DATE,IDEBUG,IRECSIZE,ITDLPOUT,IRAOUT
      INTEGER :: ISTDIN,ITXT,IAPPEND,IRASIZE
      INTEGER, INTENT(INOUT), DIMENSION(6) :: MASTER
      CHARACTER(LEN=*), DIMENSION(64) :: CARGS
      CHARACTER(LEN=*) :: CFILP,CFILGIS,CFILBIN,CNEWDDFILE
      CHARACTER(LEN=*) :: CPRBTYP,CCOORDS,CTDLPOUT,CFILTXT
      CHARACTER(LEN=*) :: CRAOUT
C        INTERNAL VARIABLES
      INTEGER :: I,II,J,JJ
      INTEGER :: IER,NARGS,NARGSKIP,NNUM,IOS,IDX
      CHARACTER*7 :: STATUX
      CHARACTER*10 :: CID(4),CDATE
      CHARACTER*256 :: CFILDO,HOMEDIR,CTEMP
      LOGICAL :: ARGSKIP
C
C        INITIALIZE VARIABLES
C
      ARGSKIP=.FALSE.
      CFILP=' '
      CFILDO='itdlp.log'
      CID='          '
      CNEWDDFILE=' '
      CPRBTYP='  '
      IBIN=0
      ICHDD=0
      ID=0
      IENDIAN=0
      IFTYPE=0
      IGIS=0
      IGRD=0
      IMID=0
      ISRA=0
      ISSQ=0
      IPRINT=-1
      IREC=-1
      ITXT=0
      IVT=0
      IVECT=0
      NARGS=0
      NARGSKIP=0
      NCOLS=0
      NNUM=0
      STATUX='SCRATCH'
C
C        GET NUMBER OF COMMAND LINE ARGUMENTS. RETURN IF
C        NONE. 
C
      NARGS=COMMAND_ARGUMENT_COUNT()
      IF(NARGS.EQ.0) RETURN
C
C        ONCE A COUNT OF COMMAND LINE ARGUMENTS IS KNOWN, LOOP
C        THROUGH THE NUMBER OF ARGS AND STORE EACH ARGUMENT
C        INTO CARGS(I). BY DEFAULT, ARGUMENTS ARE PARSED BY
C        A BLANK CHARACTER.
C
      DO I=1,NARGS
        CALL GETARG(I,CARGS(I))
      END DO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
C
C        LOOP THROUGH ALL COMMAND LINE ARGUMENTS. SOME ARGUMENTS
C        REQUIRE VALUES IN THE NEXT ITEM OF CARGS(I), SO WHEN THIS 
C        OCCURS, ARGSKIP=.TRUE. AND WE CYCLE TO THE NEXT ITERATION
C        WHERE AT THE TOP OF LOOP, THE VALUE OF ARGSKIP IS CHECKED.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
C
      DO I=1,NARGS
C           RESET IOS
         IOS=0
C           CHECK THE VALUE OF ARGSKIP.
         IF(ARGSKIP)THEN
C              ARGSKIP IS TRUE, SO RESET IT TO FALSE, THEN CYCLE.
            NARGSKIP=NARGSKIP-1
            IF(NARGSKIP.EQ.0)ARGSKIP=.FALSE.
            CYCLE
         ENDIF
C
         IF(CARGS(I)(1:1).EQ.'-')THEN
C
C           CASE STATEMENT TO SET APPROPRIATE VARIABLES WHEN CERTAIN
C           COMMAND LINE ARGUMENTS ARE INVOKED.
C
         SELECT CASE (CARGS(I))
C
            CASE('-append')
C                 APPEND TDLPACK RECORDS TO EXISTING FILE
               IAPPEND=1
C
            CASE('-bin')
C                 WRITE OUT A BINARY DATA FILE
               IBIN=1
               CFILBIN=cargs(i+1)
               ARGSKIP=.TRUE.
               NARGSKIP=1
               CYCLE
C
            CASE('-change-dd')
C                 CHANGE THE DD OF IDS ON INPUT (SEQ FILE ONLY)
               ICHDD=1
               IPRINT=-1
               READ(CARGS(I+1),'(I)')NEWDD
               IF(NEWDD.LT.0.OR.NEWDD.GE.100)THEN
                  WRITE(6,100)
 100              FORMAT(/' error: Invalid new DD. Must be between ',
     1                    '0 and 99.'/)
                  CALL ITDLP_STOP
               ENDIF
               CNEWDDFILE=CARGS(I+2) 
               if(CNEWDDFILE.EQ.' '.OR.CNEWDDFILE(1:1).EQ.'-')THEN
                  WRITE(6,105)
 105              FORMAT(/' error: Invalid filename for new DD file'/)
                  CALL ITDLP_STOP
               ENDIF
               ARGSKIP=.TRUE.
               NARGSKIP=2
               CYCLE
C
            CASE('-date')
C                 INVENTORY BASED ON DATE (INITIALIZATION TIME ONLY)
C                 IN YYYYMMDDHH FORMAT
               IUDATE=1
               CDATE=cargs(i+1)
               ARGSKIP=.TRUE.
               NARGSKIP=1
               READ(CDATE,FMT='(I10.10)',IOSTAT=IOS)DATE
               IF(LEN_TRIM(CDATE).NE.10.OR.IOS.NE.0)THEN
                  WRITE(6,106)
 106              FORMAT(/' error: Invalid date format. Please use ',
     1                    'YYYYMMDDHH format'/)
                  CALL ITDLP_STOP
               ENDIF
               CYCLE
C
            CASE('-debug')
C                 KEEP KFILDO AFTER PROGRAM STOPS
               IDEBUG=1
               STATUX='REPLACE'
C
            CASE('-ftype')
C                 PRINT TDLPACK FILE TYPE INFORMATION
               IFTYPE=1
C
            CASE('-gis')
C                 WRITE AN ASCII GIS FILE
               IGIS=1
               CFILGIS=cargs(i+1)
               ARGSKIP=.TRUE.
               NARGSKIP=1
               CYCLE
C
            CASE('-help')
C                 SETTING NARGS=0 WILL TRIGGER THE CALL TO USAGE WHEN
C                 ITDLP RETURNS FROM HERE.
               NARGS=0
               RETURN
C
            CASE('-i')
C
               ISTDIN=1
C
            CASE('-id')
C                 INVENTORY BASED ON MOS-2000 ID
               IMID=1
               DO J=1,4
                  CID(J)=CARGS(I+J)
                  NNUM=LEN_TRIM(CID(J))
                  DO JJ=1,NNUM
                     IF(IACHAR(CID(J)(JJ:JJ)).LT.48.OR.
     1                  IACHAR(CID(J)(JJ:JJ)).GT.57)THEN
                        WRITE(6,110)
 110                    FORMAT(/' error: Invalid character detected ',
     1                          'for MOS-2000 Id'/)
                        CALL ITDLP_STOP
                     ENDIF
                  END DO
                  READ(CID(J),'(I)')ID(J)
               END DO
               ARGSKIP=.TRUE.
               NARGSKIP=4
               CYCLE
C
            CASE('-invect')
C                 PRINT STATION CALL LETTERS AND DATA VALUES
C                 IN NCOLS COLUMNS
               IVECT=1
               IF(LEN_TRIM(CARGS(I+1)).EQ.1)THEN
                  IF(CARGS(I+1)(1:1).EQ.'1'.OR.
     1               CARGS(I+1)(1:1).EQ.'2'.OR.
     1               CARGS(I+1)(1:1).EQ.'3'.OR.
     1               CARGS(I+1)(1:1).EQ.'4'.OR.
     1               CARGS(I+1)(1:1).EQ.'5')THEN
                     READ(CARGS(I+1),'(I)')NCOLS
                  ELSE
                     WRITE(6,115)
 115                 FORMAT(/' error: Invalid number of columns for ',
     1                       '-invect option'/)
                     CALL ITDLP_STOP
                  ENDIF
                  ARGSKIP=.TRUE.
                  NARGSKIP=1
                  CYCLE
               ELSEIF(LEN_TRIM(CARGS(I+1)).GT.1)THEN
                  NCOLS=5
               ENDIF
C
            CASE('-is0')
C                 PRINT IS0 SECTION INFORMATION
               ISECT=IBSET(ISECT,0)
C
            CASE('-is1')
C                 PRINT IS1 SECTION INFORMATION
               ISECT=IBSET(ISECT,1)
C
            CASE('-is2')
C                 PRINT IS2 SECTION INFORMATION
               ISECT=IBSET(ISECT,2)
C
            CASE('-is4')
C                 PRINT IS4 SECTION INFORMATION
               ISECT=IBSET(ISECT,4)
C
            CASE('-grid')
C                 PRINT GRID INFORMATION FOR SUPERIMAGEGEN
               IGRD=1
C
            CASE('-probe')
C                 PROBE A GRID
               IPROBE=1
               READ(CARGS(I+1),'(A2)')CPRBTYP
               IF(CPRBTYP(1:2).EQ.'ij'.OR.
     1            CPRBTYP(1:2).EQ.'ll')THEN
                  READ(CARGS(I+2),'(A)')CCOORDS
               ELSE
                  WRITE(6,116)
 116              FORMAT(/' error: Invalid probe type.'/)
                  CALL ITDLP_STOP
               ENDIF
               ARGSKIP=.TRUE.
               NARGSKIP=2
               CYCLE
C
            CASE('-rasize')
C                 SET THE "SIZE"" OF THE NEW RA FILE.
               IF(CARGS(I+1).EQ.'small')THEN
                  IRASIZE=1
               ELSEIF(CARGS(I+1).EQ.'large')THEN
                  IRASIZE=2
               ELSE
                  WRITE(6,117)
 117              FORMAT(/' error: Invalid size for new Random ',
     1                     'Access file.'/)
                  CALL ITDLP_STOP
               ENDIF
               ARGSKIP=.TRUE.
               NARGSKIP=1
               CYCLE
C
            CASE('-rec')
C                 INVENTORY BASED ON RECORD NUMBER
               IF(IREC.NE.-1)THEN
                  WRITE(6,120)
 120              FORMAT(/' error: Multiple -rec options given. Only',
     1                     ' 1 is allowed.'/)
                  CALL ITDLP_STOP
               ENDIF
               READ(CARGS(I+1),*)IREC
               ARGSKIP=.TRUE.
               NARGSKIP=1
               CYCLE
C
            CASE('-recsize')
C
               IRECSIZE=1
C
            CASE('-simple')
C                 SIMPLE OUTPUT (DATA VALUES ARE NOT UNPACKED)
               IPRINT=-1
C
            CASE('-tdlp')
C                 OUTPUT TDLPACK RECORDS TO A NEW FILE
               ITDLPOUT=1
               CTDLPOUT=CARGS(I+1) 
               if(CTDLPOUT.EQ.' '.OR.CTDLPOUT(1:1).EQ.'-')THEN
                  WRITE(6,121)
 121              FORMAT(/' error: Invalid filename for output ',
     1                    'TDLPACK file'/)
                  CALL ITDLP_STOP
               ENDIF
               ARGSKIP=.TRUE.
               NARGSKIP=1
               CYCLE
C
            CASE('-tdlpra')
C
               IRAOUT=1
               CRAOUT=CARGS(I+1)
               if(CRAOUT.EQ.' '.OR.CRAOUT(1:1).EQ.'-')THEN
                  WRITE(6,121)
                  CALL ITDLP_STOP
               ENDIF
               ARGSKIP=.TRUE.
               NARGSKIP=1
               CYCLE
C
            CASE('-text')
C                 WRITE OUT A BINARY DATA FILE
               ITXT=1
               CFILTXT=cargs(i+1)
               ARGSKIP=.TRUE.
               NARGSKIP=1
               CYCLE
C
            CASE('-v')
C                 VERBOSE OUTPUT (DATA ARE UNPACKED)
               IPRINT=+1
C
            CASE('-vt')
C                 PRINT THE VALID TIME
               IVT=+1
C
            CASE DEFAULT
C
               WRITE(6,125)TRIM(CARGS(I))
 125           FORMAT(/' error: Illegal option ',A/)
               CALL ITDLP_STOP
C
         END SELECT
C
C           IF THE LOOP COMES HERE, THEN THIS ARGUMENT IS THE
C           INPUT FILE NAME.
C
         ELSEIF(CARGS(I)(1:1).NE.'-')THEN
            CFILP=CARGS(I)
         ENDIF
         ARGSKIP=.FALSE.
C
      END DO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
C
C        OPEN KFILDO. BY DEFAULT, KFILDO IS OPENED WITH STATUS='SCRATCH'.
C        WHEN '-debug' OPTION IS INVOKED (IDEBUG=1), OPEN THE FILE AS 
C        STATUS='REPLACE'. 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
 1300 IF(IDEBUG.EQ.1)THEN
         OPEN(UNIT=KFILDO,FILE=CFILDO,FORM='FORMATTED',
     1        STATUS=STATUX,IOSTAT=IOS,ERR=1301)
      ELSE
         OPEN(UNIT=KFILDO,FORM='FORMATTED',
     1        STATUS=STATUX,IOSTAT=IOS,ERR=1301)
      ENDIF
C
 1301 IF(IOS.EQ.9)THEN
C           COME HERE IF IOS=9, MEANING KFILDO WAS OPENED IN AN
C           DIRECTORY IN WHICH THE USER DOES NOT HAVE WRITE PERMISSION.
C           OPEN KFILDO IN THEIR HOME DIRECTORY.
         CALL GETENV("HOME",HOMEDIR)
         CFILDO=TRIM(HOMEDIR)//'/itdlp.log' 
         GO TO 1300
      ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
C
C        CALL TDLP_OPEN TO DETERMINE WHAT TYPE OF TDLPACK
C        IS BEING WORKED ON (SEQUENTIAL OR RANDOM ACCESS)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
      IF(CFILP(1:1).EQ.' ')THEN
         WRITE(6,135)
 135     FORMAT(/' error: No input TDLPACK file specified.'/)
         CALL ITDLP_STOP
      ELSE
         CALL TDLP_OPEN(KFILDO,KFILIN,CFILP,ISRA,ISSQ,IENDIAN,
     1                  FSIZE,MASTER,IER)
      ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
C
C        THIS SECTION WILL CHECK FOR DEPENDECIES BETWEEN
C        COMMAND LINE OPTIONS.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
C
C        CHECK TO SEE IF '-rec' and '-id' WERE USED
C        SIMULTANEOUSLY. STOP IF SO.
C
      IF(IREC.NE.-1.AND.IMID.EQ.1)THEN
         WRITE(6,130)
 130     FORMAT(/' error: Cannot use -rec and -id together.'/)
         CALL ITDLP_STOP
      ENDIF
C
C        STOP WHEN '-gis' IS GIVEN WITHOUT '-rec' OPTION.
C
      IF(IGIS.EQ.1.AND.IREC.EQ.-1)THEN
         WRITE(6,145)
 145     FORMAT(/' error: must use -rec <rec_num> option when',
     1         ' specifying a gis output file.'/)
         CALL ITDLP_STOP
      ENDIF
C
C        STOP WHEN '-bin' IS GIVEN WITHOUT '-rec' OPTION.
C
      IF(IBIN.EQ.1.AND.IREC.EQ.-1)THEN
         WRITE(6,150)
 150     FORMAT(/' error: must use -rec <rec_num> option when',
     1         ' specifying a binary output file.'/)
        CALL ITDLP_STOP
      ENDIF
C
C        STOP WHEN '-text' IS GIVEN WITHOUT '-rec' OPTION.
C
      IF(ITXT.EQ.1.AND.IREC.EQ.-1)THEN
         WRITE(6,151)
 151     FORMAT(/' error: must use -rec <rec_num> option when',
     1         ' specifying a text output file.'/)
        CALL ITDLP_STOP
      ENDIF
C
C        STOP WHEN '-change-dd' IS GIVEN AND THE TDLPACK FILE
C        IS RANDOM ACCESS.
C
      IF(ICHDD.EQ.1.AND.(ISSQ.EQ.0.OR.ISRA.EQ.1))THEN
         WRITE(6,155)
 155     FORMAT(/' error: can only change DDs of TDLPACK sequential',
     1          ' files.'/)
         CALL ITDLP_STOP
      ENDIF
C
C       STOP WHEN '-date' IS GIVEN AND THE TDLPACK FILE IS RANDOM
C       ACCESS.
C
      IF(IUDATE.EQ.1.AND.(ISSQ.EQ.0.OR.ISRA.EQ.1))THEN
         WRITE(6,160)
 160     FORMAT(/' error: -date option used with random access file.'/)
         CALL ITDLP_STOP
      ENDIF
C
C        STOP WHEN '-tdlp' AND '-tdlpra' OPTIONS ARE GIVEN.
C
      IF(ITDLPOUT.EQ.1.AND.IRAOUT.EQ.1)THEN
         WRITE(6,165)
 165     FORMAT(/' error: Cannot use -tdlp and -tdlpra together.'/)
         CALL ITDLP_STOP
      ENDIF
C
      RETURN
      END SUBROUTINE RDCLARGS
