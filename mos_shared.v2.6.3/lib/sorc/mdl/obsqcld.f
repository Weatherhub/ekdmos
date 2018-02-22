      SUBROUTINE OBSQCLD(KFILDO,KFIL10,ID,IDPARS,JD,NDATE, 
     1                   CCALL,STALAT,STALON,KSTOP,SDATA,ND1,NSTA,
     2                   IPACK,IWORK,FD1,FD2,FD3,FD4,FD5,ND2X3,
     3                   LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     4                   IS0,IS1,IS2,IS4,ND7,
     5                   ISTAV,L3264B,IER)
C
C        JULY     2005   WEISS   CHANGED THE SCP IDS FOR SCE AND SCW
C                                TO REFELECT THE NEW SCP IDS
C                                708350000 ---> 708370000  SCE
C                                708351000 ---> 708371000  SCW
C        DECEMBER 2005   YAN     ADOPTED MITCH WEISS'S CODE OBSTCLD
C                                (JULY 2005) AND CONVERTED IT TO COMPUTE
C                                OPAQUE SKY COVER PREDICTAND
C        JANUARY  2008   YAN     UPDATED IN-LINE DOCUMENTATION TO REMOVE
C                                HISTORY OF PARENT CODE OBSTCLD, AND
C                                EXPAND THE PURPOSE BLOCK
C        AUGUST   2010   SU      ADDED A TEST BLOCK FOR TROPICAL WESTERN PACIFIC STATIONS.
C
C        PURPOSE 
C           THIS SUBROUTINE COMPUTES OPAQUE SKY COVER FROM OBSERVATION
C           VECTOR DATA. OPAQUE CLOUD WILL BE ESTIMATED FROM BOTH
C           SURFACE OBSERVATIONS (METAR CLOUD COVERAGE) AND SATELLITE
C           CLOUD PRODUCT (SCP) GENERATED FROM GOES. THE OPACITY IS
C           ACCOUNTED FOR BY THE EFFECTIVE CLOUD AMOUNT (ECA) OF THE
C           SCP.  CLOUD CATEGORIES ARE MODIFIED BY THE ECA IN A CASCADING
C           DOWNSCALE SCHEME.  THE FINAL OPAQUE SKY COVER TAKES THE
C           GREATER VALUE OF THE LOW CLOUDS FROM METAR REPORTS AND THE
C           HIGH CLOUDS WHICH ARE COMPUTED IN THIS CODE.  IT IS NOTED
C           THAT ONLY THOSE STATIONS WHICH REQUIRE SCP DATA WILL BE
C           COMPLEMENTED (NON-MANUAL).  IF SCP VALUES ARE MISSING THEN
C           COMPLEMENTED SURFACE OPAQUE CLOUD WILL BE DECLARED MISSING.
C
C           THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C              708 313 - OPAQUE SKY COVER
C
C           RETURN WITH IER = 47 WHEN:
C              1)  SURFACE TYPE DATA NOT FOUND
C              2)  BOTH GOES EAST AND GOES WEST ARE MISSING
C              3)  ANY CLOUD LAYER IS MISSING
C 
C           THE FOLLOWING CODED VALUES AND THEIR ASSOCIATED CLOUD 
C           COVERAGE ARE LISTED AS FOLLOWS:
C
C           SKY COVERAGE    | CLOUD COVERAGE  | CODED VALUE |
C                           |                 |   (METAR)   |
C                           |                 |             |
C           CLEAR (ASOS)    |      = 0        |      0      |
C           CLEAR (MANUAL)  |      = 0        |    0 OR 1   |
C           FEW             |    > 0 - <= 2/8 |      2      |
C           SCATTERED       |  > 2/8 - <= 4/8 |      3      |
C           BROKEN          |  > 4/8 - < 8/8  |      6      |
C           OVERCAST        |      = 8/8      |      8      |
C           TOT OBSCURATION | ASSUME 8/8      |     10      |
C
C        DATA SET USE 
C
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE 
C                      (OUTPUT). 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                      (INPUT-OUTPUT). 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                       (INPUT). 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM 
C                       ACCESS (INPUT-OUTPUT).
C               ID(J) = THE VARIABLE ID (J=1,4) (INPUT).
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID( ) (J=1,15)
C                       (INPUT).
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 
C                            LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK 
C                            IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE 
C                       PORTIONS PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND THRESH.
C                       JD( ) IS USED TO IDENTIFY THE BASIC MODEL 
C                       FIELDS AS READ FROM THE ARCHIVE (INPUT).
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED
C                       (INPUT).
C            SDATA(K) = DATA TO RETURN (K=1,ND1).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                       WITH (INPUT).
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INTERNAL)
C           STALAT(K) = LATITUDE OF STATIONS (K=1,ND1) (INPUT).
C           STALON(K) = LONGITUDE OF STATIONS (K=1,ND1) (INPUT).
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND
C                       IN AN INPUT DIRECTORY (K=1,ND1).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST, EXCEPT POSSIBLY
C                       CCALLD( ) AND CCALLP( ).  EQUIVALENCED TO
C                       ICALL( , , ).  (CHARACTER*8)  (INPUT/OUTPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).
C            IWORK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).
C       FD1(J),FD2(J) = WORK ARRAYS (J=1,ND2X3).
C              FD3(J)   FD1 = STATION TYPE (EX. MANUAL VS ASOS).
C                       FD2 = THE CLOUD COVERAGE DERIVED FROM THE
C                             SATELLITE CLOUD PRODUCT (SCP).
C                       FD3 = ASOS CLOUD COVERAGE.
C               ND2X3 = DIMENSION OF IPACK( ), IWORK( ), FD1( ),
C                       FD2( ) AND FD3( ) (INPUT).
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS), (INPUT-OUTPUT).
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK, 
C                              THIS IS MINUS THE RECORD NUMBER WHERE 
C                              THE DATA START.
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDL GRIB, 1 FOR 
C                              NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN 
C                              RETRIEVED.
C                       L=10 --NUMBER OF THE SLAB IN DIR( , ,L) AND
C                              IN NGRIDC( ,L) DEFINING THE 
C                              CHARACTERISTICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE 
C                              SORTED LIST IN ID( ,N) (N=1,NPRED)
C                              FOR WHICH THIS VARIABLE IS NEEDED, WHEN 
C                              IT IS NEEDED ONLY ONCE FROM 
C                              LSTORE( , ).  WHEN IT IS NEEDED MORE
C                              THAN ONCE, THE VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING 
C                              MSTORE( , ). LATER USED AS A WAY OF 
C                              DETERMINING WHETHER TO KEEP THIS 
C                              VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ), (INPUT). 
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN (INPUT).  
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA 
C                       IDENTIFIED IN LSTORE( , ) (J=1,ND10). WHEN
C                       CORE( ) IS FULL DATA ARE STORED ON DISK
C                       (OUTPUT).
C                ND10 = DIMENSION OF CORE( ), (INPUT).
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE (INPUT).  
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING  COUNT FROM THE BEGINNING OF 
C                       OF THE PROGRAM. THIS COUNT IS MAINTAINED IN 
C                       CASE THE USER NEEDS IT (DIAGNOSTICS, ETC.). 
C                       NEEDS IT (DIAGNOSTICS, ETC.), (INTERNAL).  
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3)
C                       (INTERNAL).
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+)  
C                       (INTERNAL).
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12)
C                       (INTERNAL).
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4)
C                       (INTERNAL).
C                 ND7 = DIMENSION OF IS0, IS1, IS2, AND IS4. NOT ALL
C                       LOCATIONS ARE USED (INPUT).
C               ISTAV = 1 SINCE THE DATA RETURNED ARE STATION DATA
C                       (OUTPUT).
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64) (INPUT).
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        -1 = PROBLEM WITH PROCESSING DATES
C                        52 = NWORDS RETURNED FROM GFETCH NOT EQUAL
C                             TO NSTA
C                       103 = ROUTINE ENTERED WITH WRONG IDS
C                       SEE GFETCH FOR VALUES.  (INTERNAL-OUTPUT)
C
C        ADDITIONAL VARIABLES
C               ACALL = STATION CALL LETTER (CHARACTER*8) USED TO 
C                       CHECK FOR ALASKA STATIONS FOR SCP EXEMPTION
C                       (INTERNAL).
C            ASOS_MAN = LOGICAL VARIABLE USED TO DECLARE ASOS STATIONS
C                       WITH NO SCP AUGMENTATION (INTERNAL). 
C            ASOS_SCP = LOGICAL VARIABLE USED TO DECLARE ASOS STATIONS
C                       WITH SCP AUGMENTATION (INTERNAL).
C                   I = COUNTER (INTERNAL).
C        ID_CLDCOV(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD3( ) "CLOUD COVERAGE", WHERE J=1,4 (INTERNAL).
C         ID_OLDTC(J) = HOLDS THE 4 ID WORDS OF THE OLD TOTAL CLOUD
C                       COVERAGE RETRIEVED FOR FOR NDATES OCCURRING 
C                       BEFORE DEC. 1, 1996 (00Z), WHERE J=1,4 
C                       (INTERNAL). 
C          ID_SCPE(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD2( ) SCP CLOUD COVERAGE (GOES EAST), WHERE 
C                       J=1,4 (INTERNAL).
C          ID_SCPW(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD3( ) SCP CLOUD COVERAGE (GOES WEST), WHERE
C                       J=1,4 (INTERNAL).
C        ID_STATYP(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD1( ) "STATION TYPE", ITERATED OVER 6 POSSIBLE
C                       LAYERS, WHERE J-1,4 (INTERNAL).
C              IENTER = THE NUMBER OF TIMES THIS SUBROUTINE IS ENTERED
C                       DURING A U201 RUN (INTERNAL). 
C              ILEVEL = COUNTER FOR LEVELS 1 - 6 OF CLOUD COVERAGE
C                       (INTERNAL).
C            IOLDTCLD = INTEGER VALUE OF OLD SURFACE TOTAL CLOUD
C                       (INTERNAL).
C                   J = COUNTER (INTERNAL).
C                   K = COUNTER (INTERNAL). 
C            KSTOP(I) = FLAG VALUES TO DETERMINE IF PROCESSING FOR A
C                       GIVEN STATION HAS BEEN COMPLETED:
C                       = 0 PROCESSING INCOMPLETE
C                       = 1 PROCESSING COMPLETE
C               MISSP = PRIMARY MISSING VALUE INDICATOR. RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED (INTERNAL). 
C               MISSS = SECONDARY MISSING VALUE INDICATOR. RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED (INTERNAL).
C                   N = COUNTER (INTERNAL).
C             NEWPROC = LOGICAL USED TO CHOOSE TOTAL CLOUD PROCESSING  
C                       FOR 12/1/1996 (00Z) AND LATER (INTERNAL).
C               NPACK = 2 FOR TDL GRIB PACKED DATA: 1 FOR NOT PACKED.
C                       THIS IS STOREd IN LSTORE(7, ) (INTERNAL).
C               NSLAB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE FIRST FIELD.  THIS IS THE
C                       VALUE OF NSLAB RETURNED.  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED (OUTPUT).
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT
C                       THE RECORD HAS BEEN FETCHED. THIS IS STORED
C                       IN LSTORE(9, ) (INTERNAL).
C              NWORDS = NUMBER OF WORDS RETURNED IN FD1( ), FD2( ) OR 
C                       FD3( ) (INTERNAL).
C            OLDPROCA = LOGICAL USED TO CHOOSE TOTAL CLOUD PROCESSING
C                       PRIOR TO 09/01/1995 (00Z) (INTERNAL).
C            OLDPROCB = LOGICAL USED TO CHOOSE TOTAL CLOUD PROCESSING
C                       FROM 09/01/1995 (00Z) THROUGH 11/30/1996 (23Z)
C                       (INTERNAL). 
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH
C
      IMPLICIT NONE
C
      CHARACTER*8 CCALL(ND1,6),ACALL
C
      LOGICAL NEWPROC
      LOGICAL ASOS_MAN,ASOS_SCP
C
      INTEGER ID(4),IDPARS(15),JD(4)
      INTEGER KSTOP(ND1)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER IPACK(ND2X3),IWORK(ND2X3)
      INTEGER LSTORE(12,ND9)
      INTEGER ID_STATYP(4),ID_SCPE(4),ID_SCPW(4),ID_CLDCOV(4),
     *        ID_OLDTC(4),ID_ECAE(4),ID_ECAW(4),ID_CLDHGT(4)
      INTEGER KFILDO,KFIL10,ND9,LITEMS,ND10,NDATE,ND1,NSTA,
     1        ND2X3,NBLOCK,NFETCH,ISTAV,L3264B,IER,ND7,
     2        I,J,K,N,ILEVEL,NTIMES,NWORDS,IENTER,NPACK,NSLAB,
     3        MISSP,MISSS,IOLDTCLD,ICLOUD_TYPE,JCLOUD_TYPE,
     4        ISTAT_TYPE,ISCP_TYPE,ICLOUD_HGT,HC1LOWER
C
      REAL SDATA(ND1),STALAT(ND1),STALON(ND1)
      REAL CORE(ND10)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),FD5(ND2X3)
      REAL HC1(ND1),HC1FLAG(ND1),HC
C
      DATA IENTER/0/
      DATA NEWPROC/.FALSE./
C
      SAVE IENTER,NEWPROC
C
      IER=0
      ISTAV=1
C
C        STEP 1A. VERIFY THE PROCESSING OF OPAQUE CLOUD COVERAGE
C
      IF(JD(1).NE.708313000) THEN
        WRITE(KFILDO,150)(JD(J),J=1,4)
 150    FORMAT(/' ****OBSQCLD ENTERED FOR INCORRECT PREDICTOR',
     1          I9.9,2I10.9,I4.3)
        IER=103
        GO TO 800
      ENDIF
C
C        STEP 1B. INITIALIZATION.
C
      IENTER=IENTER+1
C        INCREMENT IENTER VARIABLE BY 1.
C
      DO 156 J=1,ND2X3
        FD1(J)=9999.
        FD2(J)=9999.
        FD3(J)=9999.
        FD4(J)=9999.
        FD5(J)=9999.
 156  CONTINUE
C
      DO 160 J=1,ND1
        SDATA(J)=9999.
	KSTOP(J)=0
 160  CONTINUE
C
C        STEP 1C. DETERMINE OLD OR NEW DATA PROCESSING BASED
C        ON THE CUTOFF DECEMBER 1, 1996 (00Z).
C
      IF(IENTER.EQ.1.AND.NDATE.GE.1996120100) NEWPROC=.TRUE.
C
C        STEP 1D. CONSTRUCT 4 WORD ID'S
C        CONSTRUCT THE STATION TYPE ARRAY
C
      ID_STATYP(1)=700002000
      ID_STATYP(2)=IDPARS(7)
      ID_STATYP(3)=IDPARS(9)*1000000+IDPARS(12)
      ID_STATYP(4)=0
C
C        CONSTRUCT THE SCP (GOES EAST) CLOUD COVERAGE ARRAY
C        (ID_SCPE) FOUR WORD ID
C
      ID_SCPE(1)=708370000
      ID_SCPE(2)=IDPARS(7)
      ID_SCPE(3)=IDPARS(9)*1000000+IDPARS(12)
      ID_SCPE(4)=0
C
C        CONSTRUCT THE SCP (GOES EAST) EFFECTIVE CLOUD AMOUNT
C        (ID_ECAE) FOUR WORD ID
C
      ID_ECAE(1)=708380000
      ID_ECAE(2)=IDPARS(7)
      ID_ECAE(3)=IDPARS(9)*1000000+IDPARS(12)
      ID_ECAE(4)=0
C
C        CONSTRUCT THE SCP (GOES WEST) CLOUD COVERAGE ARRAY
C        (ID_SCPW) FOUR WORD ID 
C
      ID_SCPW(1)=708371000
      ID_SCPW(2)=IDPARS(7)
      ID_SCPW(3)=IDPARS(9)*1000000+IDPARS(12)
      ID_SCPW(4)=0
C
C        CONSTRUCT THE SCP (GOES WEST) EFFECTIVE CLOUD AMOUNT
C        (ID_ECAW) FOUR WORD ID
C
      ID_ECAW(1)=708381000
      ID_ECAW(2)=IDPARS(7)
      ID_ECAW(3)=IDPARS(9)*1000000+IDPARS(12)
      ID_ECAW(4)=0
C
C        CONSTRUCT THE CLOUD HEIGHT ID
C
      ID_CLDHGT(1)=708321000
      ID_CLDHGT(2)=IDPARS(7)
      ID_CLDHGT(3)=IDPARS(9)*1000000+IDPARS(12)
      ID_CLDHGT(4)=0
C
C        CONSTRUCT THE CLOUD COVERAGE ARRAY FOR LEVEL #1
C
      ID_CLDCOV(1)=708320000
      ID_CLDCOV(2)=IDPARS(7)
      ID_CLDCOV(3)=IDPARS(9)*1000000+IDPARS(12)
      ID_CLDCOV(4)=0
C
C        STEP 2. FETCH STATION TYPE (FD1)
C
      IF(NEWPROC) THEN
        CALL GFETCH(KFILDO,KFIL10,ID_STATYP,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4              IER)
C
        IF(IER.NE.0)THEN
C
          IF(IENTER.EQ.1) THEN
             WRITE(KFILDO,202) (JD(J),J=1,4),IER
 202         FORMAT(/' ****ERROR IN OBSQCLD, FOR FIRST PROCESS DATE,',
     1               ' STATION TYPE IS MISSING.  POSSIBLE DATA GAP.',
     2              /'     ALL VALUES OF TOTAL',
     3               ' CLOUD ARE MISSING',
     4               ' FOR VARIABLE',I9.9,2I10.9,I4.3,' IER =',I5)
           ENDIF
C
           GO TO 800
C             STATION TYPE MUST BE PRESENT.  IF IT IS NOT THERE FOR DAY 1,
C             IT WILL NOT BE SAVED FOR FUTURE DAYS.  NO REASON TO PROCEED.
        ELSE
C
           IF(NWORDS.NE.NSTA) THEN
              WRITE(KFILDO,205)NWORDS,NSTA
 205          FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1                 ' NOT EQUAL TO NSTA =',I6,' FOR STATION TYPE',
     2                 ' IN OBSQCLD.  DATA SET TO MISSING.')
              IER=52
              GO TO 800
           ENDIF
C
        ENDIF
C
C        STEP 2A. MODIFY STATION TYPE ARRAY (FD1) FOR ASOS STATION
C        WITHOUT SCP AUGMENTATION TO THE VALUE OF 10. (ALASKA 
C        STATIONS ONLY).
C
 206    DO 212 K=1,NSTA
C
          DO 210 N=1,6
            ACALL=CCALL(K,N)
C
C              ALASKA TEST
C
            IF(N.EQ.1) THEN
C
              IF(ACALL(1:2).EQ.'PA') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PFYU    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
C************************************************************
C  Test for tropical western Pacific islands
C
              IF(ACALL.EQ.'NSTU    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PGRO    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PGSN    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PGUA    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PGUM    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PGWT    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PKMR    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PKWA    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PMDY    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PTKK    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PTKR    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PTSA    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PTTP    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PTYA    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PWAK    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
C************************************************************
C
            ENDIF
C
 210      CONTINUE
C
 212    CONTINUE
C
      ENDIF
C
C
C        STEP 3. FETCH SCP GOES EAST (FD2) AND GOES WEST (FD3)
C        CLOUD COVERAGE AND MERGE INTO FD2.
C
      IF(NEWPROC) THEN
C
C        STEP 3A. FETCH SCP (GOES EAST) CLOUD COVERAGE INTO FD2
C
        CALL GFETCH(KFILDO,KFIL10,ID_SCPE,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4              IER)
C
        IF(IER.NE.0)THEN
           IF(IENTER.EQ.1) THEN
              WRITE(KFILDO,230)(JD(J),J=1,4),IER
 230          FORMAT(/' ****ERROR IN OBSQCLD, FOR FIRST PROCESS DATE,',
     1                ' SCP (GOES EAST) LEVEL IS MISSING.',
     2                '  POSSIBLE DATA GAP.',
     3               /'     ALL VALUES OF TOTAL CLOUD ARE MISSING',
     4                ' FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER =',I4)
              GO TO 800
C               MASS STORAGE MUST BE SETUP PROPERLY ON FIRST
C               PROCESS DATE
           ELSE
             IF(IER.NE.47) GO TO 800
C               LET IT PROCEED SO THAT CLOUD OBS RECORDS CAN BE
C               ACCESSED TO MARK FOR FUTURE ACCESS.  GOES EAST COULD
C               BE MISSING AND GOES WEST PRESENT.
           ENDIF
        ELSE
          IF(NWORDS.NE.NSTA) THEN
            WRITE(KFILDO,235)NWORDS,NSTA
 235        FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1              ' NOT EQUAL TO NSTA =',I6,
     2             /'     FOR SCP CLOUD COVERAGE',
     2              ' (GOES EAST) IN OBSQCLD.  DATA SET TO MISSING.')
            IER=52
            GO TO 800
          ENDIF
        ENDIF
C
C                 FETCH EFFECTIVE CLOUD AMOUNT (GOES EAST) INTO FD4
C
        CALL GFETCH(KFILDO,KFIL10,ID_ECAE,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD4,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4              IER)
C
        IF(IER.NE.0)THEN
           IF(IENTER.EQ.1) THEN
              WRITE(KFILDO,231)(JD(J),J=1,4),IER
 231          FORMAT(/' ****ERROR IN OBSQCLD, FOR FIRST PROCESS DATE,',
     1                ' SCP (GOES EAST) LEVEL IS MISSING.',
     2                '  POSSIBLE DATA GAP.',
     3               /'     ALL VALUES OF TOTAL CLOUD ARE MISSING',
     4                ' FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER =',I4)
              GO TO 800
C               MASS STORAGE MUST BE SETUP PROPERLY ON FIRST 
C               PROCESS DATE
           ELSEIF(IENTER.GT.1)THEN
             IF(IER.NE.47) GO TO 800
C               LET IT PROCEED SO THAT CLOUD OBS RECORDS CAN BE
C               ACCESSED TO MARK FOR FUTURE ACCESS.  GOES EAST COULD
C               BE MISSING AND GOES WEST PRESENT.
           ENDIF
        ELSE
          IF(NWORDS.NE.NSTA) THEN
            WRITE(KFILDO,236)NWORDS,NSTA
 236        FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1              ' NOT EQUAL TO NSTA =',I6,
     2             /'     FOR SCP CLOUD COVERAGE',
     3              ' (GOES EAST) IN OBSQCLD.  DATA SET TO MISSING.')
            IER=52
            GO TO 800
          ENDIF
        ENDIF
C 
C         NOTE THAT CONTROL COMES HERE WHEN IER = 47 FROM GFETCH.  
C         THIS MEANS MISSING GOES DATA IN FD2( ), WHICH IS ACCOMMODATED.
C
C         STEP 3B. FETCH SCP (GOES WEST) CLOUD COVERAGE INTO FD3
C
        CALL GFETCH(KFILDO,KFIL10,ID_SCPW,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4              IER)
C
        IF(IER.NE.0)THEN
           IF(IENTER.EQ.1) THEN
              WRITE(KFILDO,245) (JD(J),J=1,4),IER
 245          FORMAT(/' ****ERROR IN OBSQCLD, FOR FIRST PROCESS DATE,',
     1                ' SCP (GOES WEST) LEVEL IS MISSING.',
     2                '  POSSIBLE DATA GAP.',
     3               /'     ALL VALUES OF TOTAL CLOUD ARE MISSING',
     4                ' FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER =',I4)
              go to 800
C               MASS STORAGE MUST BE SETUP PROPERLY ON FIRST
C               PROCESS DATE
           ELSEIF(IENTER.GT.1)THEN
             IF(IER.NE.47) GO TO 800
C               LET IT PROCEED SO THAT CLOUD OBS RECORDS CAN BE
C               ACCESSED TO MARK FOR FUTURE ACCESS.  GOES WEST COULD
C               BE MISSING AND GOES EAST PRESENT.
           ENDIF
        ELSE
          IF(NWORDS.NE.NSTA) THEN
            WRITE(KFILDO,250)NWORDS,NSTA
 250        FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1              ' NOT EQUAL TO NSTA =',I6,
     2             /'     FOR SCP CLOUD COVERAGE',
     2              ' (GOES WEST) IN OBSQCLD.  DATA SET TO MISSING.')
            IER=52
            GO TO 800
          ENDIF
        ENDIF
C
C                  FETCH EFFECTIVE CLOUD AMOUNT (GOES WEST) INTO FD5
C
        CALL GFETCH(KFILDO,KFIL10,ID_ECAW,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD5,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4              IER)
C
        IF(IER.NE.0)THEN
           IF(IENTER.EQ.1) THEN
              WRITE(KFILDO,246) (JD(J),J=1,4),IER
 246          FORMAT(/' ****ERROR IN OBSQCLD, FOR FIRST PROCESS DATE,',
     1                ' SCP (GOES WEST) LEVEL IS MISSING.',
     2                '  POSSIBLE DATA GAP.',
     3               /'     ALL VALUES OF TOTAL CLOUD ARE MISSING',
     4                ' FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER =',I4)
              go to 800
C               MASS STORAGE MUST BE SETUP PROPERLY ON FIRST 
C               PROCESS DATE
           ELSEIF(IENTER.GT.1)THEN
             IF(IER.NE.47) GO TO 800
C               LET IT PROCEED SO THAT CLOUD OBS RECORDS CAN BE
C               ACCESSED TO MARK FOR FUTURE ACCESS.  GOES WEST COULD
C               BE MISSING AND GOES EAST PRESENT.
           ENDIF
        ELSE
          IF(NWORDS.NE.NSTA) THEN
            WRITE(KFILDO,251)NWORDS,NSTA
 251        FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1              ' NOT EQUAL TO NSTA =',I6,
     2             /'     FOR SCP CLOUD COVERAGE',
     2              ' (GOES WEST) IN OBSQCLD.  DATA SET TO MISSING.')
            IER=52
            GO TO 800
          ENDIF
        ENDIF
C
C######################################################################
C
C         STEP 3C. DETERMINE SCP CLOUD COVERAGE FROM THE EAST (FD2)
C                  AND THE WEST (FD3). THE CRITERIA IS AS FOLLOWS:
C                  (FINAL TALLY PUT INTO FD2)
C                  1. WEST OF 105 WEST, GOES WEST IS SELECTED. IF
C                     GOES WEST IS MISSING, USE GOES EAST.
C                  2. EAST OF 105 WEST, GOES EAST IS SELECTED. IF
C                     GOES EAST IS MISSING, USE GOES WEST.
C
        DO 300 J=1,NSTA
          IF(STALON(J).GT.105.) THEN
            IF(NINT(FD3(J)).NE.9999) FD2(J)=FD3(J)
            IF(NINT(FD5(J)).NE.9999) FD4(J)=FD5(J)
          ELSEIF(STALON(J).LE.105.) THEN
            IF(NINT(FD2(J)).EQ.9999) FD2(J)=FD3(J)
            IF(NINT(FD4(J)).EQ.9999) FD4(J)=FD5(J)
          ENDIF
 300    CONTINUE 
C
      ENDIF
C
C        STEP 4. FETCH CLOUD COVERAGE FOR LEVELS 1 - 6 (USING FD3).
C                WORD #1 OF ID_CLDCOV CHANGE FOR LEVELS 2 - 6.
C                AND ESTIMATE TOTAL CLOUD (NEWPROC)
C
      IF(NEWPROC) THEN
C
        DO 400 ILEVEL=1,6
          IF(ILEVEL.GE.2) ID_CLDCOV(1)=ID_CLDCOV(1)+2000
C           FETCH ASOS CLOUD COVERAGE (METAR DATA)
          CALL GFETCH(KFILDO,KFIL10,ID_CLDCOV,7777,LSTORE,ND9,LITEMS,
     1                IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2                NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3                NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4                IER)
C
          IF(IER.NE.0)THEN
             IF(IENTER.EQ.1)THEN
               WRITE(KFILDO,320) ILEVEL,(JD(J),J=1,4),IER
 320           FORMAT(/' ****ERROR IN OBSQCLD, FOR FIRST PROCESS DATE,',
     1                 ' CLOUD AMOUNT FOR LEVEL ',I2,' IS MISSING',
     2                 ' POSSIBLE DATA GAP.',
     3                /'     ALL VALUES OF TOTAL CLOUD ARE MISSING',
     4                 ' FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER =',I4)
             ENDIF
             GO TO 800
C               IF A CLOUD LAYER IS MISSING, MIGHT AS WELL QUIT.
          ELSE
             IF(NWORDS.NE.NSTA) THEN
               WRITE(KFILDO,325)NWORDS,NSTA,ILEVEL
 325           FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1                 ' NOT EQUAL TO NSTA =',I6,
     2                 /'     FOR CLOUD COVERAGE',
     3                  ' (LEVEL ',I1,')',' IN OBSQCLD.  DATA SET',
     4                  ' TO MISSING.')
               IER=52
               GO TO 800
             ENDIF
          ENDIF
C
          IF(ILEVEL.GE.2) ID_CLDHGT(1)=ID_CLDHGT(1)+2000
C           FETCH CLOUD HEIGHT
          CALL GFETCH(KFILDO,KFIL10,ID_CLDHGT,7777,LSTORE,ND9,LITEMS,
     1                IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD5,ND2X3,
     2                NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3                NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4                IER)
C
          IF(IER.NE.0)THEN
             IF(IENTER.EQ.1)THEN
               WRITE(KFILDO,321) ILEVEL,(JD(J),J=1,4),IER
 321           FORMAT(/' ****ERROR IN OBSQCLD, FOR FIRST PROCESS DATE,',
     1                 ' CLOUD AMOUNT FOR LEVEL ',I2,' IS MISSING',
     2                 ' POSSIBLE DATA GAP.',
     3                /'     ALL VALUES OF TOTAL CLOUD ARE MISSING',
     4                 ' FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER =',I4)
             ENDIF
             GO TO 800
C               IF A CLOUD LAYER IS MISSING, MIGHT AS WELL QUIT.
          ELSE
             IF(NWORDS.NE.NSTA) THEN
               WRITE(KFILDO,326)NWORDS,NSTA,ILEVEL
 326           FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1                 ' NOT EQUAL TO NSTA =',I6,
     2                 /'     FOR CLOUD COVERAGE',
     3                  ' (LEVEL ',I1,')',' IN OBSQCLD.  DATA SET',
     4                  ' TO MISSING.')
               IER=52
               GO TO 800
             ENDIF
          ENDIF
C
C            STEP 4A. DETERMINE TOTAL CLOUD COVERAGE FOR EACH STATION
C                   BY DETERMINING THE HIGHEST (LARGEST) CLOUD
C                     COVERAGE FOR SIX POSSIBLE LAYERS.
C
C        *****  MAIN DO LOOP  ***** 
C
          DO 350 J=1,NSTA
	    ISTAT_TYPE=NINT(FD1(J))
	    ICLOUD_TYPE=NINT(FD3(J))
            ICLOUD_HGT=NINT(FD5(J))
C
            IF(KSTOP(J).EQ.0) THEN
	      ASOS_MAN=.FALSE.
	      ASOS_SCP=.FALSE.
C
C                1. MANUAL STATIONS (STATION TYPE = 1) OR 
C                   ASOS WITH NO SCP AUGMENTATION (STATION TYPE = 10).
C
              IF(((ISTAT_TYPE.EQ.1).OR.(ISTAT_TYPE.EQ.10)).AND.
     *          (NINT(FD3(J)).NE.9999)) ASOS_MAN=.TRUE.    
C
C                2. ASOS WITH SCP AUGMENTATION (STATION TYPE = 2 - 5)
C
              IF(((ISTAT_TYPE.GE.2).AND.(ISTAT_TYPE.LE.5)).AND.
     *          ((NINT(FD2(J)).NE.9999).AND.(NINT(FD3(J)).NE.9999)))
     *          ASOS_SCP=.TRUE.
C
C                STEP 4B. PROCESS LEVEL #1 
C
              IF(ILEVEL.EQ.1)THEN
                IF((ASOS_MAN).OR.(ASOS_SCP))THEN
                  IF(ICLOUD_HGT.LE.120.)THEN
                    IF((ICLOUD_TYPE.EQ.10).OR.(ICLOUD_TYPE.EQ.8)) THEN
                      SDATA(J)=FD3(J)  
                      KSTOP(J)=1
                    ENDIF
                    IF(ICLOUD_TYPE.EQ.9) THEN
	              SDATA(J)=0.
                    ELSE
                      IF((ICLOUD_TYPE.EQ.0).OR.(ICLOUD_TYPE.EQ.1)) THEN
                        SDATA(J)=0.
	                IF(ISTAT_TYPE.EQ.1.OR.ISTAT_TYPE.EQ.10)KSTOP(J)=1
                      ELSE
                        IF((ICLOUD_TYPE.GE.2).AND.
     +                     (ICLOUD_TYPE.LE.6))THEN
                          SDATA(J)=FD3(J)
                        ENDIF
                      ENDIF
                    ENDIF
                  ELSE
C                     METAR DATA SHOULD ALWAYS BE PUT INTO SDATA(J)
                    SDATA(J)=FD3(J)
                    IF((ICLOUD_TYPE.EQ.0).OR.(ICLOUD_TYPE.EQ.1))
     +                  SDATA(J)=0.
                    HC1(J)=FD3(J)
                    HC1FLAG(J)=1
                  ENDIF   
                ELSE
                  KSTOP(J)=1
                ENDIF
              ENDIF
C
C        STEP 4C. PROCESS LEVELS 2 - 6
C
	      IF(ILEVEL.GE.2.AND.ILEVEL.LE.6)THEN
	        IF((ASOS_MAN).OR.(ASOS_SCP)) THEN
C                   AS LONG AS THERE ARE METAR DATA, SDATA(J) SHOULD ALWAYS BE FILLED
C                   NO MATTER WHAT THE HEIGHT OF THE CLOUD IS
                  SDATA(J)=FD3(J)
                  IF(ICLOUD_HGT.LE.120.)THEN
	            IF(ICLOUD_TYPE.EQ.8) KSTOP(J)=1
                  ELSE
                    HC1(J)=FD3(J)
                    HC1FLAG(J)=1
                  ENDIF 
                ELSE
	          KSTOP(J)=1
                ENDIF
              ENDIF
            ENDIF
C
C        STEP 4D. ASOS WITH SCP AUGMENTATION
C
            IF(ILEVEL.EQ.6)THEN
              IF((ISTAT_TYPE.GE.2).AND.(ISTAT_TYPE.LE.5))THEN
                IF((NINT(SDATA(J)).NE.9999).AND.
     +            (NINT(FD2(J)).NE.9999))THEN
                  HC=FD2(J)
C                   SCP DATA HC NEED TO BE ADJUSTED
                  IF(HC.EQ.8..AND.FD4(J).GT.0..AND.FD4(J).LT.66.)HC=6.
                  IF(HC.EQ.6..AND.FD4(J).GT.0..AND.FD4(J).LT.33.)HC=3.
                  IF(HC.EQ.3..AND.FD4(J).GE.6..AND.FD4(J).LE.16.)HC=2.
                  IF((HC.EQ.2..OR.HC.EQ.3.)
     +              .AND.FD4(J).GT.0..AND.FD4(J).LT.6.)HC=1.
C                   COMPARE SCP DATA TO HIGH CLOUD METAR DATA, TAKE THE LARGER ONE
                  IF(HC1FLAG(J).EQ.1.AND.HC1(J).GT.HC)HC=HC1(J)
C                   COMPARE THE HIGH CLOUD DATA TO THE LOW CLOUD (METAR) DATA, TAKE THE LARGER ONE
                  IF(HC.GT.SDATA(J))SDATA(J)=HC
                ENDIF
              ENDIF
            ENDIF
C
 350      CONTINUE
C
C        ***** END OF MAIN DO LOOP *****
C
 400    CONTINUE
C
      ENDIF
C
      GO TO 850 
C
C        PREMATURE TERMINATION OF SUBROUTINE.
C
 800  DO 810 I=1,ND1
        SDATA(I)=9999.
 810  CONTINUE
C
 850  RETURN
      END
