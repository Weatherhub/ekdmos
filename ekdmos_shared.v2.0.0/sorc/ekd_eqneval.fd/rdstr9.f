      SUBROUTINE RDSTR9(KFILDO,KFIL10,KFILIN,NAMIN,JFOPEN,NUMIN,
     1                  NDATE,LOOKAH,CCALLD,IPACK,IWORK,DATA,ND5,
     2                  IS0,IS1,IS2,IS4,ND7,
     3                  LSTORE,LITEMS,ND9,NBLOCK,CORE,ND10,
     4                  LASTL,LASTD,NSTORE,IP10,
     5                  CCALL,INDEXC,XDATA,ND1,NSTA,PXMISS,
     6                  IP12,IP23,L3264B,L3264W,ISTOP,IER)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM: RDSTR9
C   PRGMMR: GLAHN        ORG: W/OSD211    DATE: 99-01-01
C
C ABSTRACT: TO READ PACKED VECTOR DATA FROM ALL MODELS NEEDED FOR A
C           CYCLE/DAY AND TO STORE ALL FIELDS.  IT IS ASSUMED
C           NO DATA ARE NEEDED TO BE READ FOR A DATE PRIOR TO NDATE.
C           THE CALL LETTERS RECORDS ARE READ AND MATCHED
C           WITH THE CALL LETTERS IN CCALL( , ), AND THE LOCATIONS
C           STORED IN INDEXC( , ).  RDSTR9 INCORPORATES LOOKAHEAD
C           BY LOOKAH HOURS.
C
C PROGRAM HISTORY LOG:
C   99-09-21  GLAHN
C   00-05-16  CARROLL    ADDED NCEP DOCBLOCK.
C   00-05-17  MCE        REMOVED FILENAME FROM OPEN STATEMENT 
C                        NEAR STMNT #221
C   12-07-16  ENGLE      ADDED CONVERTX; ADDED CALL TO CKFILEND
C                        BEFORE OPENING A TDLPACK VECTOR FILE;
C                        MODIFIED OPEN STATEMENT TO INCLUDE
C                        CONVERT= SPECIFIER.
C
C USAGE:    CALL RDSTR9(KFILDO,KFIL10,KFILIN,NAMIN,JFOPEN,NUMIN,
C                       NDATE,LOOKAH,CCALLD,IPACK,IWORK,DATA,ND5,
C                       IS0,IS1,IS2,IS4,ND7,LSTORE,LITEMS,ND9,
C                       NBLOCK,CORE,ND10,LASTL,LASTD,NSTORE,IP10,
C                       CCALL,INDEXC,XDATA,ND1,NSTA,PXMISS,IP12,
C                       IP23,L3264B,L3264W,ISTOP,IER)
C   INPUT ARGUMENT LIST:
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              KFIL10 = UNIT NUMBER OF MDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C           KFILIN(J) = UNIT NUMBERS FOR INPUT DATA, ALL IN MDL GRIB
C                       FORMAT.  INPUT CAN INCLUDE INTERPOLATED MODEL
C                       DATA, PREDICTAND (OBSERVATIONS) DATA, VARIOUS
C                       CONSTANTS, OR MOS FORECASTS (FOR 2ND GENERATION
C                       MOS, POSSIBLY FOR LOCAL IMPLEMENTATION
C                       (J=1,NUMIN).  UNIT NUMBERS SHOULD BE THE SAME AND
C                       THE FILE NAMES IN SEQUENCE WHEN FILES ARE TO BE
C                       USED SEQUENTIALLY.  THE FIRST ONE (ONLY) WILL BE
C                       OPEN WHEN RDSTR9 IS ENTERED.  (INPUT)
C            NAMIN(J) = NAME OF THE INPUT FILES BEING PROCESSED
C                       (J=1,NUMIN).  (CHARACTER*60)  (INPUT)
C               NUMIN = THE NUMBER OF VALUES IN KFILIN( ), NAMIN( ), AND
C                       INDEXC( , ).  ALSO TREATED AS THEIR DIMENSIONS.
C                       (INPUT)
C               NDATE = DATE IN FORM YYYYMMDDHH.  (INPUT)
C              LOOKAH = NUMBER OF HOURS TO ADD TO NDATE FOR LOOKAHEAD.
C                       (INPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND DATA( ).
C                       MUST BE GE THE NUMBER OF STATIONS ON THE
C                       INPUT INTERPOLATED FILE(S).  (INPUT)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C         LSTORE(L,J) = THE ARRAY TO HOLD INFORMATION ABOUT THE DATA
C                       STORED (L=1,12) (J=1,LITEMS).  (INPUT-OUTPUT)
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK,
C                              THIS IS MINUS THE RECORD NUMBER WHERE
C                              THE DATA START.  NOTE THAT WHEN A FIELD
C                              CANNOT BE STORED IN CORE( ), IT IS PUT
C                              ON DISK.  IT MAY BE THAT A LATER FIELD
C                              WILL FIT, AND IT IS PUT IN CORE( ).
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN MDLPACK, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --NOT USED.
C                       L=11 --SET TO 7777.
C                       L=12 --NOT USED.
C                 ND9 = 2ND DIMENSION OF LSTORE( , ).  (INPUT)
C              NBLOCK = THE RECORD SIZE FOR THE FILE TO WRITE THE DATA
C                       WHEN CORE( ) IS FULL.  (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C               LASTL = THE LAST LOCATION IN CORE( ) USED.  INITIALIZED
C                       TO 0 ON FIRST ENTRY TO GSTORE.  (INPUT/OUTPUT)
C               LASTD = TOTAL NUMBER OF PHYSICAL RECORDS ON DISK.
C                       INITIALIZED TO 0 ON FIRST ENTRY TO GSTORE.
C                       (INPUT/OUTPUT)
C              NSTORE = RUNNING COUNT OF NUMBER OF TIMES DATA ARE
C                       STORED BY GSTORE.  (INPUT/OUTPUT)
C                IP10 = INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                       FIELDS READ FOR DAY 1 WILL BE PRINTED TO THE FILE
C                       WHOSE UNIT NUMBER IS IP10.  (INPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST.  (CHARACTER*8)
C                       (INPUT)
C            XDATA(J) = WORK ARRAY (J=1,ND1).  (INTERNAL)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       (INPUT)
C                NSTA = THE NUMBER OF STATIONS IN CCALL( , ).  (INPUT)
C              PXMISS = THE VALUE OF A SECONDARY MISSING VALUE TO INSERT
C                       WHEN THE SECONDARY MISSING VALUE IS 9997.
C                       THIS ALLOWS MAINTAINING A 9997, TREATING IT AS
C                       ZERO, AS 9999, OR AS SOME OTHER VALUE.  (INPUT)
C                IP12 = INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE INPUT FILES WILL BE PRINTED TO
C                       THE FILE WHOSE UNIT NUMBER IS IP12.  (INPUT)
C                IP23 = INDICATES WHETHER (>0) OR NOT (=0) STATEMENTS
C                       ABOUT EOF AND FILE OPENINGS AND CLOSINGS WILL
C                       BE OUTPUT FOR PRINTING ON UNIT IP(23).  (INPUT)
C              L3264B = THE NUMBER OF BITS IN THE WORD OF THE MACHINE
C                       BEING USED, 32 OR 64.  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).
C                       (INPUT)
C               ISTOP = INCREMENTED BY ONE EACH TIME AN ERROR IS
C                       ENCOUNTERED.  (INPUT-OUTPUT)
C
C   OUTPUT ARGUMENT LIST: 
C           JFOPEN(J) = FOR EACH FILE IN KFILIN(J), JFOPEN(J) IS 1 WHEN
C                       THE FILE IS OPEN, IS 0 WHEN IT HAS ALREADY BEEN
C                       USED AND IS 2 WHEN THE FILE HAS NOT BEEN OPENED
C                       (J=1,NUMIN).  (OUTPUT)
C         LSTORE(L,J) = THE ARRAY TO HOLD INFORMATION ABOUT THE DATA
C                       STORED (L=1,12) (J=1,LITEMS).  (INPUT-OUTPUT)
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK,
C                              THIS IS MINUS THE RECORD NUMBER WHERE
C                              THE DATA START.  NOTE THAT WHEN A FIELD
C                              CANNOT BE STORED IN CORE( ), IT IS PUT
C                              ON DISK.  IT MAY BE THAT A LATER FIELD
C                              WILL FIT, AND IT IS PUT IN CORE( ).
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN MDLPACK, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --NOT USED.
C                       L=11 --SET TO 7777.
C                       L=12 --NOT USED.
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , ) THAT
C                       HAVE BEEN USED FOR THIS DATE/TIME.  (OUTPUT)
C             CORE(J) = THE ARRAY TO STORE THE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL
C                       DATA ARE STORED ON DISK.  (OUTPUT)
C               LASTL = THE LAST LOCATION IN CORE( ) USED.  INITIALIZED
C                       TO 0 ON FIRST ENTRY TO GSTORE.  (INPUT/OUTPUT)
C               LASTD = TOTAL NUMBER OF PHYSICAL RECORDS ON DISK.
C                       INITIALIZED TO 0 ON FIRST ENTRY TO GSTORE.
C                       (INPUT/OUTPUT)
C              NSTORE = RUNNING COUNT OF NUMBER OF TIMES DATA ARE
C                       STORED BY GSTORE.  (INPUT/OUTPUT)
C         INDEXC(K,J) = LOCATIONS OF THE STATIONS CORRESPONDING TO
C                       CCALL(K, ) (K=1,NSTA) FOR EACH MODEL (J=1,NUMIN).
C                       IF THE STATION CAN'T BE FOUND IN THE DIRECTORY,
C                       INDEXC( ) IS SET TO 99999999.  NOTE THAT THIS
C                       MUST BE LARGER THAN THE MAXIMUM NUMBER OF
C                       STATIONS BEING DEALT WITH (GRIDPOINTS IN THE
C                       CASE OF GRIDPOINT DEVELOPMENT).  (OUTPUT)
C               ISTOP = INCREMENTED BY ONE EACH TIME AN ERROR IS
C                       ENCOUNTERED.  (INPUT-OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        31 = TROUBLE OPENING OR SWITCHING FILES.
C                        38 = NUMBER OF WORDS READ EXCEEDS ARRAY SIZE ND5.
C                        56 = NO DATA FOUND FOR DAY 1.
C                       120 = ONE OR MORE STATIONS CAN'T BE FOUND ON ONE OR
C                             INPUT FILES IN RDDIR/FINDST.
C                       138 = TOO MANY ERRORS READING PACKED RECORDS.
C                       140 = ERROR READING CALL LETTERS RECORD IN RDDIR.
C                       145 = SIZE OF CALL LETTERS RECORD EXCEEDS ARRAY SIZE
C                             CCALLD(ND5) IN RDDIR.  TRY TO RETURN
C                             WITH ERROR INDICATED.
C                       146 = END OF FILE FOUND IN RDDIR.
C                       SEE OTHER ROUTINES FOR OTHER VALUES.  (OUTPUT)
C
C        DATA SET USE
C        INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C             FORT.xx - INDICATE NAME & PURPOSE
C
C        OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C             FORT.xx - INDICATE NAME & PURPOSE
C
C        VARIABLES
C           CCALLD(K) = THE CALL LETTERS READ FROM THE INPUT FILES
C                       (K=1,ND5).  (CHARACTER*8)  (INTERNAL)
C            IPACK(J) = HOLDS THE MDL GRIB RECORD (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  FURNISHED TO UNPACK.
C                       (INTERNAL)
C             DATA(J) = WORK ARRAY (J=1,ND5).  FURNISHED TO UNPACK AND
C                       UNPACKED DATA RETURNED.  (INTERNAL)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C            XDATA(J) = WORK ARRAY (J=1,ND1).  (INTERNAL)
C      NBYTES(L3264W) = NUMBER OF BYTES IN RECORD, FOLLOWING THE INITIAL
C                       64 BITS CONTAINING NBYTES ITSELF.   (INTERNAL)
C              NWORDS = NUMBER OF WORDS IN IPACK( ).  NWORDS IS CALCULATED
C                       NWORDS=NBYTES*8/L3264B, WHERE NBYTES IS THE LENGTH
C                       IN BYTES READ FROM THE RECORD ITSELF.  (INTERNAL)
C                MSTA = NUMBER OF STATIONS IN THE DIRECTORY (FIRST) RECORD
C                       OF THE FILE BEING PROCESSED.  TAKEN FROM THE RECORD
C                       SIZE.  (INTERNAL)
C               LDATE = LOCAL COPY OF NDATE, UPDATED TO INDICATE
C                       HOW FAR TO READ A PARTICULAR FILE.  THIS IS
C                       DETERMINED BY LOOKAH.  (INTERNAL)
C              LSTOPC = AN INTERNAL COUNTER TO KEEP AN INFINITE READING
C                       LOOP FROM OCCURRING.  (INTERNAL)
C               LSTOP = THE VALUE TO COMPARE LSTOPC WITH TO STOP THE
C                       READING.  CURRENTLY SET AT 500; THIS ASSUMES
C                       500 READING ERRORS SHOULD NOT OCCUR IN A SINGLE
C                       RUN.  NOTE THAT THIS COUNT IS SEPARATE FROM
C                       ISTOP IN CASE ISTOP HAS TO BE LARGE WHEN DEALING
C                       WITH HOURLY DATA AND MISSING STATIONS.  (INTERNAL)
C               INTER = STARTS AT ZERO AND INCREMENTS ON EACH ENTRY.
C                       (INTERNAL)
C            CONVERTX = CHARACTER HOLDING THE KEYWORD USED TO OPEN
C                       RANDOM ACCESS FILE WITH THE CORRECT ENDIAN.
C                       (CHARACTER*20).
C
C        SUBPROGRAMS CALLED: 
C             UNIQUE: NONE.
C
C          LIBRARY: 
C            W3LIB   -
C           MDLLIB90 - UNPACK,GSTORE,UNPKBG,RDDIR,UPDAT,TIMPR,CKFILEND
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90 (ifort compiler)
C   MACHINE:  IBM SP
C
C$$$
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 NAMIN(NUMIN)
CINTEL
      CHARACTER*20 CONVERTX
CINTEL
C
      DIMENSION XDATA(ND1)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION KFILIN(NUMIN),JFOPEN(NUMIN)
      DIMENSION INDEXC(ND1,NUMIN)
      DIMENSION NBYTES(L3264W)
C        NBYTES IS AN AUTOMATIC ARRAY.
C
CINTEL
      CONVERTX='BIG_ENDIAN'
CINTEL
      DATA LSTOP/500/,
     1     LSTOPC/0/,
     2     INTER/0/
C
D     CALL TIMPR(KFILDO,KFILDO,'START RDSTR9        ')
C
      IER=0
      INTER=INTER+1
      LITEMS=0
      LSTOPC=0
C        LSTOPC COUNT STARTS FRESH WITH EACH ENTRY.
C
C        UPDATE LDATE, THE LOCAL COPY OF NDATE FOR LOOLAHEAD.
C
      CALL UPDAT(NDATE,LOOKAH,LDATE)
C
C        SINCE ALL INPUT DATA ARE IN MDLPACK, NUMIN SHOULD BE GT 0.
C
      IF(NUMIN.EQ.0)THEN
         IER=56
         GO TO 235
      ENDIF
C
C        START PROCESSING DAY 1 DATA.  FIRST READ THE CALL LETTERS
C        RECORD.  ALL DATA SOURCES ARE USED.
C
      DO 225 IN=1,NUMIN
      IF(JFOPEN(IN).NE.1)GO TO 225
      IF(INTER.GT.1)GO TO 200
C
C        THIS IS THE FIRST ENTRY, SO READ DIRECTORY.
C
 150  CALL RDDIR(KFILDO,KFILIN(IN),IP12,NAMIN(IN),NDATE,
     1           CCALL,INDEXC(1,IN),ND1,NSTA,CCALLD,ND5,MSTA,
     2           L3264B,L3264W,IER)
C
C
C***D     WRITE(KFILDO,151)IER,NSTA,MSTA,ND5,L3264B,L3264W
C***D151  FORMAT(' RDSTR9--IER,NSTA,MSTA,ND5,L3264B,L3264W'6I8)
      IF(IER.EQ.0)GO TO 200
C
      IF(IER.EQ.146)THEN
C           IER = 146 MEANS END OF FILE IN RDDIR.  SINCE
C           A TRAILER ALWAYS FOLLOWS DATA, THIS IS NOT
C           UNEXPECTED AND IS NOT COUNTED AS AN ERROR.
         GO TO 219
C
      ELSEIF(IER.EQ.120)THEN
C           IER = 120 IS NOT FATAL.  IT MEANS ONE OR MORE STATIONS
C           CAN'T BE FOUND ON INPUT FILE.
         ISTOP=ISTOP+1
         IF(MSTA.EQ.0)GO TO 2191
C           IT IS ASSUMED THAT IF THE NUMBER OF STATIONS FOUND
C           IN THE DIRECTORY IS ZERO, THE FILE IS UNUSABLE.
         GO TO 200
C
      ELSE
C           OTHER VALUES OF IER FROM RDDIR ARE FATAL FOR THIS DATA SET.
C           CLOSE IT AND MOVE ON.
         ISTOP=ISTOP+1
         GO TO 2191
C
      ENDIF
C 
C        READING CALL LETTERS COMPLETE AND INDEXC( , ) INITIALIZED
C        FOR MODEL IN.  NOW READ THE DATA.
C     
 200  READ(KFILIN(IN),IOSTAT=IOS,ERR=201,END=219)
     1               (NBYTES(J),J=1,L3264W),
     2               (IPACK(J),J=1,MIN(ND5,NBYTES(L3264W)*8/L3264B))
C
C*****************************
C****    TO CHECK MESSAGE, DECLARE IBYTE(2000) LOGICAL*1, AND REPLACE
C****    ABOVE READ WITH THE FOLLOWING READ AND WRITE.
C****      READ(KFILIN(IN),IOSTAT=IOS,ERR=201,END=219)
C****     1               (NBYTES(J),J=1,L3264W(L3264W)),
C****     2               (IBYTE(J),J=1,NBYTES)
C****D     WRITE(KFILDO,210)(IBYTE(J),J=1,NBYTES)
C****D210  FORMAT(/' PACKED MESSAGE'/
C****D    1      (16(1XO3.3)))
C****      STOP 5555
C*****************************
C
C        IPACK( ) CONTAINS THE PACKED RECORD.
C        THE RECORD CONSISTS OF AN INITIAL 8 BYTES CONTAINING THE NUMBER
C        OF BYTES FOLLOWING.  FOR A 32-BIT MACHINE, THIS IS TWO WORDS.
      IF(L3264W.EQ.2)THEN
C           FOR A 32-BIT MACHINE, IPACK(5) HOLDS THE DATE/TIME OF THE
C           RECORD.
         IDATE=IPACK(5)
      ELSE
C           FOR A 64-BIT MACHINE, THE LEFT HALF OF IPACK(3) HOLDS 
C           THE DATE/TIME OF THE RECORD.
         LOC=3
         IPOS=1
         CALL UNPKBG(KFILDO,IPACK,ND5,LOC,IPOS,IDATE,32,L3264B,IER,*900)
      ENDIF
C
      NWORDS=NBYTES(L3264W)*8/L3264B
C
      IF(NWORDS.GT.ND5)THEN
         WRITE(KFILDO,2000)NWORDS,ND5,NAMIN(IN)
 2000    FORMAT(/' ****THE NUMBER OF WORDS IN THE SOURCE FILE',
     1           ' IN RDSTR9 =',I7,' EXCEEDS THE',/,
     2           '     NUMBER AVAILABLE ND5 =',I7,' AT 2000.',
     2           ' FOR SOURCE FILE ',A60,/,
     3           '     STOP THE PRESSES!')
         IER=38
         ISTOP=ISTOP+1
         GO TO 235
      ENDIF
C
      GO TO 205
C
 201  WRITE(KFILDO,202)KFILIN(IN),NDATE,IOS,NAMIN(IN)
      IF(IP23.NE.0.AND.IP23.NE.KFILDO)
     1          WRITE(IP23,202)KFILIN(IN),NDATE,IOS,NAMIN(IN)
 202  FORMAT(/' ****ERROR READING PACKED RECORD ON UNIT NO.',I3,
     1        ' PROCESSING DATE',I11,' IN RDSTR9 AT 202.  IOSTAT =',I5/
     2        '  ON FILE = ',A60)
      ISTOP=ISTOP+1
      LSTOPC=LSTOPC+1
      IF(LSTOPC.LT.LSTOP)GO TO 200
C        THIS CHECK IS TO STOP AN INFINITE LOOP THAT MIGHT OCCUR.
      WRITE(KFILDO,203)LSTOP,NDATE
      IF(IP23.NE.0.AND.IP23.NE.KFILDO)WRITE(IP23,203)LSTOP,NDATE
 203  FORMAT('     A TOTAL OF',I5,' READING ERRORS HAVE OCCURRED.',
     1      '  RETURN FROM RDSTR9 AT 203 WHILE PROCESSING DATE',I11)
      IER=138
      GO TO 235
C
 205  IF(IDATE.EQ.9999)GO TO 150
C        IF THE ABOVE TEST IS MET, THIS IS A TRAILER RECORD, AND A
C        CALL LETTERS RECORD OR AN EOF SHOULD FOLLOW.
C
      IF(IDATE.LT.NDATE)THEN
         GO TO 200
C           THE ABOVE SPACES UP TO THE DAY WANTED.
      ELSEIF(IDATE.GT.LDATE)THEN
C           ALL DATA FOR NDATE + LOOKAH HAVE BEEN READ AND STORED.
         GO TO 225
      ENDIF
C
C        THIS IS A DATE/TIME TO SAVE FOR MODEL NAMIN(IN) FOR DAY 1.
C        IT MUST BE SAVED SO THAT IT CAN BE RETAINED IN A RANDOM 
C        ACCESS MANNER.  THE MDL MOS-2000 STORAGE SYSTEM IS
C        USED FOR THIS PURPOSE.  UNPACK DATA AS WELL AS ID'S.
C
      CALL UNPACK(KFILDO,IPACK,IWORK,DATA,ND5,IS0,IS1,IS2,IS4,ND7,
     1            MISSP,MISSS,2,L3264B,IER)
C
C***D     WRITE(KFILDO,210)(IS0(K),K=1,3)
C***D210  FORMAT(/' IS0',1XA4,2I10)
C***D     WRITE(KFILDO,211)(IS1(K),K=1,22+IS1(22))
C***D211  FORMAT(/' IS1'10I11/'    '10I11/'    '2I11,4X32R1)
C***D     WRITE(KFILDO,212)(IS2(K),K=1,12)
C***D212  FORMAT(/' IS2'10I11/('    '10I11))
C***D     WRITE(KFILDO,213)(IS4(K),K=1,4)
C***D213  FORMAT(/' IS4'10I11/('    '10I11))
C
      IF(IER.NE.0)THEN
C           IER NE 0 INDICATES PROBLEM WITH UNPACKING RECORD.
C           IT IS NOT TREATED AS FATAL, BUT THIS RECORD IS 
C           SKIPPED.  A DIAGNOSTIC WILL HAVE BEEN PRINTED IN
C           UNPACK.
         ISTOP=ISTOP+1
         GO TO 200
C
      ELSE
         ISTA=IS4(3)
         IF(ISTA.GT.ND5)THEN
            WRITE(KFILDO,215)ND5,ISTA,KFILIN(IN),NDATE,NAMIN(IN)
 215        FORMAT(/' ****ND5 =',I6,' TOO SMALL FOR DATA ARRAY',
     1              ' IN RDSTR9 AT 215.  INCREASE TO GE',I6,/,
     2              '     READING ON UNIT NO.',I3,
     3              ' PROCESSING DATE',I11,' ON FILE = ',A60)
            IER=38
C              SET XDATA( ) TO MISSING.  DATA( ) WILL NOT HAVE
C              BEEN OVERFLOWED, BUT WILL CONTAIN THE MISSING
C              INDICATOR.  THESE DATA WILL BE STORED BY GSTORE.
C
            DO 216 K=1,NSTA
            XDATA(K)=9999.
 216        CONTINUE
C
         ELSE
C
C              PUT DATA INTO XDATA( ).  NOTE THAT EXCEPT FOR THE
C              INITIAL RETRIEVAL INTO DATA( ), ONLY THE NSTA WORDS
C              OF DATA ARE DEALT WITH.
C      
            DO 217 K=1,NSTA
C
            IF(INDEXC(K,IN).EQ.99999999)THEN
               XDATA(K)=9999.
            ELSE
               XDATA(K)=DATA(INDEXC(K,IN))
               IF(XDATA(K).EQ.9997.)XDATA(K)=PXMISS
C                 THE ABOVE STATEMENT ALLOWS THE MISSING VALUE
C                 9997 TO BE TREATED AS SOME OTHER VALUE.  THIS
C                 WOULD USUALLY BE 0, BUT COULD BE, SAY, 9999.
            ENDIF
C
 217        CONTINUE
C
         ENDIF
C
      ENDIF  
C
      CALL GSTORE(KFILDO,KFIL10,IS1(9),IN,LSTORE,ND9,LITEMS,
     1            XDATA,NSTA,1,0,IDATE,
     2            CORE,ND10,LASTL,NBLOCK,LASTD,NSTORE,L3264B,IER)
C        IS1(9) IS THE FIRST OF 4 ID WORDS.  NOTE THAT THE VALUE OF
C        "IN" IS PROVIDED TO BE STORED IN LSTORE(10, ) TO INDICATE
C        FOR FUTURE ROUTINES THE SOURCE OF THE DATA.  HOWEVER,
C        THIS MAY BE OF LITTLE OR NO USE, AND WILL BE UPDATED
C        IN LSTORE(10, ) IF FILE IN IS CLOSED AND IN+1 OPENED.
C        NOTE THAT THE DATE STORED IS IDATE.
C
      IF(IER.NE.0)THEN
C           IER NE 0 IS TREATED AS FATAL IN RDSTR9 WITH RETURN TO
C           CALLING PROGRAM.
         ISTOP=ISTOP+1
         GO TO 235
      ENDIF
C
C        AT THIS POINT, THE VARIABLE READ HAS BEEN STORED AND
C        ITS IDS ARE IN IS1(9-12).
      GO TO 200
C
 219  IF(IP23.NE.0)WRITE(IP23,2190)KFILIN(IN),NDATE,NAMIN(IN)
 2190 FORMAT(/' END OF  FILE ON UNIT NO.',I3,
     1        ' PROCESSING DATE',I11,'    FILE = ',A60)
 2191 CLOSE(KFILIN(IN),IOSTAT=IOS,ERR=2195)
      IF(IP23.NE.0)WRITE(IP23,2192)KFILIN(IN),NDATE,NAMIN(IN)
 2192 FORMAT(' CLOSING FILE ON UNIT NO.',I3,
     1       ' PROCESSING DATE',I11,'    FILE = ',A60)
      JFOPEN(IN)=0
      GO TO 220
C
 2195 WRITE(KFILDO,2196)KFILIN(IN),NDATE,IOS,NAMIN(IN)
      IF(IP23.NE.0.AND.IP23.NE.KFILDO)
     1             WRITE(IP23,2196)KFILIN(IN),NDATE,IOS,NAMIN(IN)
 2196 FORMAT(/' ****ERROR CLOSING FILE NO.',I3,
     1        ' PROCESSING DATE',I11,' IN RDSTR9 AT 2196, IOSTAT =',I5/
     2        '  ON FILE = ',A60)
      ISTOP=ISTOP+1
      JFOPEN(IN)=0
C
 220  IF(IN.EQ.NUMIN)THEN
         WRITE(KFILDO,2200)KFILIN(IN),NDATE,NAMIN(IN)
         IF(IP23.NE.0.AND.IP23.NE.KFILDO)
     1         WRITE(IP23,2200)KFILIN(IN),NDATE,NAMIN(IN)
 2200    FORMAT(' END OF  DATA ON UNIT NO.',I3,
     1          ' PROCESSING DATE',I11,'    FILE = ',A60)
         JFOPEN(IN)=0
         GO TO 225
C
      ENDIF
C
      IF(KFILIN(IN).NE.KFILIN(IN+1))THEN
         WRITE(KFILDO,2200)KFILIN(IN),NDATE,NAMIN(IN)
         IF(IP23.NE.0.AND.IP23.NE.KFILDO)
     1        WRITE(IP23,2200)KFILIN(IN),NDATE,NAMIN(IN)
         GO TO 225
C
      ENDIF
C
      IF(JFOPEN(IN+1).EQ.2)THEN
C           JFOPEN(IN+1) SHOULD BE 2 WHEN
C           KFILIN(IN) = KFILIN(IN+1)
         JFOPEN(IN+1)=1
CINTEL
         CALL CKFILEND(KFILDO,KFILIN(IN+1),NAMIN(IN+1),ISYSEND,
     1                 IFILEND,CONVERTX,IER)
         OPEN(UNIT=KFILIN(IN+1),
     1        FORM='UNFORMATTED',STATUS='OLD',
     2        CONVERT=CONVERTX,IOSTAT=IOS,ERR=222)
C         OPEN(UNIT=KFILIN(IN+1),
C     1        FORM='UNFORMATTED',STATUS='OLD',
C     2        IOSTAT=IOS,ERR=222)
CINTEL
         IF(IP23.NE.0)WRITE(IP23,221)KFILIN(IN+1),NDATE,NAMIN(IN+1)
 221     FORMAT(' OPENING FILE ON UNIT NO.',I3,
     1          ' PROCESSING DATE',I11,'    FILE = ',A60)
C           ALLOW LOOP TO END.  IN WILL THEN BE INCREMENTED.
C
C           IT IS NECESSARY THAT LSTORE(10, ) REFLECT THE
C           NUMBER OF THE FILE THAT IS OPEN.
C
         DO 2215 J=1,LITEMS
         IF(LSTORE(10,J).EQ.IN)LSTORE(10,J)=IN+1
 2215    CONTINUE
C
         GO TO 225
C
 222     WRITE(KFILDO,223)KFILIN(IN+1),NDATE,IOS,NAMIN(IN+1)
         IF(IP23.NE.0.AND.IP23.NE.KFILDO)
     1          WRITE(IP23,223)KFILIN(IN+1),NDATE,IOS,NAMIN(IN+1)
 223     FORMAT(/' ****ERROR OPENING FILE ON UNIT NO.',I3,
     1       ' PROCESSING DATE',I11,' IN RDSTR9 AT 223, IOSTAT =',I5,/
     2       '  ON FILE = ',A60)
         IER=31
         ISTOP=ISTOP+1
      ELSE
         WRITE(KFILDO,224)KFILIN(IN+1),NDATE,NAMIN(IN+1)
         IF(IP23.NE.0.AND.IP23.NE.KFILDO)
     1           WRITE(IP23,224)KFILIN(IN+1),NDATE,NAMIN(IN+1)
 224     FORMAT(/' ****TROUBLE IN SWITCHING TO FILE ON UNIT NO.',I3,
     1           ' PROCESSING DATE',I11,' IN RDSTR9 AT 224.'/
     2           '  ON FILE = ',A60)
         ISTOP=ISTOP+1
         IER=31
      ENDIF
C
 225  CONTINUE
C
      IF(LITEMS.EQ.0)THEN
         WRITE(KFILDO,228)
 228     FORMAT(/' ****NO DATA FOUND FOR DAY 1.')
         IF(IP10.NE.0.AND.IP10.NE.KFILDO)WRITE(IP10,228)
         IER=56
         ISTOP=ISTOP+1
         GO TO 235
C
      ELSE
         IF(IP10.NE.0)
     1      WRITE(IP10,229)LITEMS,NDATE,
     2                     ((LSTORE(J,K),J=1,12),K=1,LITEMS)
 229     FORMAT(/' ',I4,' FIELDS READ AND STORED FOR DATE/TIME',I12,/,
     1          (' ',I10.9,2I10,I11,3I8,I12,3I8,I12))
      ENDIF
C
D     CALL TIMPR(KFILDO,KFILDO,'END RDSTR9          ')
C
 235  RETURN
C 
 900  WRITE(KFILDO,901)IER
 901  FORMAT(/' ****ERROR IN UNPKBG IN RDSTR9.  IER =',I4)
      ISTOP=ISTOP+1
      RETURN
      END
 
