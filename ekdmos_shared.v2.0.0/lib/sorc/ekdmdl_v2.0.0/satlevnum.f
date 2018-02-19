      SUBROUTINE SATLEVNUM(KFILDO,KFIL10,
     1                     IDPARS,JD,NDATE,
     2                     KFILRA,RACESS,NUMRA,
     3                     CCALL,ICALLD,CCALLD,NAME,STALAT,STALON,
     4                     ISDATA,SDATA,DIR,ND1,NSTA,
     5                     NGRIDC,NGRID,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     6                     LSTORE,ND9,LITEMS,CORE,ND10,LASTL,
     7                     NBLOCK,LASTD,NSTORE,NFETCH,
     8                     IS0,IS1,IS2,IS4,ND7,
     9                     IBNDS,FD3,FD4,ND2X3,
     A                     IP12,ISTAV,L3264B,L3264W,MISTOT,IER)
C
C
C        JULY     2003   WEISS   MDL    MOS-2000
C                                THIS SUBROUTINE IS BASED ON THE
C                                JUNE 2003 VERSION OF SUBROUTINE
C                                SATLEVRH
C        DECEMBER 2003   WEISS   MODIFIED COMMENT STATEMENT TO
C                                BE CONSISTENT WITH SATLEVRH.
C                                MODIFIED TERRAIN HEIGHT GRID CHECK.
C
C        PURPOSE:  THIS SUBROUTINE DETERMINS THE PERCENTAGE OF 
C                  ABOVE THE TERRAIN HEIGHT MODEL ISOBARIC LEVELS
C                  WHICH ARE GREATER THAN OR EQUAL TO THE DESIGNATED
C                  RH THRESHOLD VALUE (SATURATION LEVEL). THIS 
C                  PREDICTOR IS CONDUCIVE TO THE PREDICTION OF
C                  TOTAL SKY COVER.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C                 003 341 - MRF OR AVN ( 5 LEVEL), THRESHOLD 85 PERCENT
C                 003 342 - ETA        ( 9 LEVEL), THRESHOLD 85 PERCENT
C                 003 343 - AVN        (11 LEVEL), THRESHOLD 85 PERCENT
C                 003 351 - MRF OR AVN ( 5 LEVEL), THRESHOLD 90 PERCENT
C                 003 352 - ETA        ( 9 LEVEL), THRESHOLD 90 PERCENT
C                 003 353 - AVN        (11 LEVEL), THRESHOLD 90 PERCENT
C                 003 361 - MRF OR AVN ( 5 LEVEL), THRESHOLD 95 PERCENT
C                 003 362 - ETA        ( 9 LEVEL), THRESHOLD 95 PERCENT
C                 003 363 - AVN        (11 LEVEL), THRESHOLD 95 PERCENT
C            NOTE:  THIS ROUTINE DOES NOT WORK FOR THE NGM.
C
C        DATA SET USE
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT(PRINT) FILE.
C                       (OUTPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                       ACCESS.(INPUT-OUTPUT)
C              IP12   = INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE INPUT FILES WILL BE PRINTED TO
C                       THE FILE WHOSE UNIT NUMBER IS IP12.
C
C        VARIABLES
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID() (J=1,15).
C                       (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY
C                                      1 LAYER)
C                       J=7--LTLTLTLT (TOP OF LAYER)
C                       J=8--T (TRANSFORMATION)
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND 
C                                BACK IN TIME)
C                       J=10-OT (TIME APPLICATION)
C                       J=11-OH (TIME PERIOD IN HOURS)
C                       J=12-TAU (PROJECTION IN HOURS)
C                       J=13-I (INTERPOLATION TYPE)
C                       J=14-S (SMOOTHING INDICATOR)
C                       J=15-G (GRID INDICATOR)
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT
C                       THE PORTIONS PERTAINING TO PROCESSING
C                       ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       ID() IS USED TO HELP IDENTIFY THE BASIC MODEL
C                       FIELDS AS READ FROM THE ARCHIVE. (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C           KFILRA(J) = HOLDS THE UNIT NUMBER FOR THE RANDOM ACCESS
C                       FILES AVAILABLE. UNIT 44 IS USED TO
C                       ACCOMODATE GRIDDED DATA INPUT (INPUT).
C               NUMRA = THE NUMBER OF UNIT NUMBERS AND NAMES IN KFILRA( )
C                       AND RACESS( ).  (INPUT)
C           RACESS(J) = THE FILE NAME ASSOCIATED WITH KFILRA.
C                       (CHARACTER*60) (INPUT).
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST, EXCEPT POSSIBLY
C                       CCALLD( ) AND CCALLP( ).  (CHARACTER*8)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN INTEGER
C                       VARIABLE (L=1,L3264W) (K=1,NSTA).
C                       EQUIVALENCED TO CCALLD( ).  (INTERNAL)
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,NSTA).
C             NAME(K) = NAMES OF STATIONS (K=1,NSTA).  USED FOR PRINTOUT
C                       ONLY.  (CHARACTER*20)  (INPUT)
C           STALAT(K) = LATITUDE OF STATIONS (K=1,NSTA).  (INPUT)
C           STALON(K) = LONGITUDE OF STATIONS (K=1,NSTA).  (INPUT)
C           ISDATA(K) = WORK ARRAY (K=1,ND1).  (INPUT)
C            SDATA(K) = WORK ARRAY (K=1,ND1).  (INPUT)
C           DIR(K,J,) = THE IX (J=1) AND JY (J=2) POSITIONS ON THE
C                       GRID FOR STATION K (K=1,NSTA).  (INPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       DIMENSION OF SEVERAL VARIABLES.  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT WITH.
C                       (INPUT)
C         NGRIDC(L,M) = HOLDS THE GRID CHARACTERISTICS (L=1,6) FOR
C                       EACH GRID COMBINATION (M=1,ND11).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=
C                            POLAR STEREOGRAPHIC).
C                       L=2--GRID LENGTH IN METERS.
C                       L=3--LATITUDE AT WHICH THE GRID LENGTH IS
C                            CORRECT *1000.
C                       L=4--GRID ORIENTATION IN DEGREES * 1000.
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *1000.
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES
C                            *1000.
C               NGRID = THE NUMBER OF GRID COMBINATIONS IN NGRIDC( , ),
C                       MAXIMUM OF ND11.  (INPUT-OUTPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN
C                       BE DEALT WITH ON THIS RUN.  LAST DIMENSION
C                       OF NGRIDC(,). (INPUT)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID. (OUTPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5). (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5). (INTERNAL)
C             DATA(J) = THE SATLEVNUM PREDICTOR, OUTPUT VALUES
C                       (J=1,ND5). (OUTPUT)
C                 ND5 = DIMENSION OF IPACK(), IWORK() AND
C                       DATA() (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE
C                       DATA STORED (L=1,12) (J=1,LITEMS).
C                       (INPUT-OUTPUT)
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE() WHERE
C                              THE DATA START.  WHEN ON DISK,
C                              THIS IS MINUS THE RECORD NUMBER WHERE
C                              THE DATA START.
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDL GRIB, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --NUMBER OF THE SLAB IN DIR(, ,L) AND
C                              IN NGRIDC(,L) DEFINING THE
C                              CHARACTERISTICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID(,N) (N=1,NPRED) FOR WHICH
C                              THIS VARIABLE IS NEEDED, WHEN IT IS
C                              NEEDED ONLY ONCE FROM LSTORE(,).
C                              WHEN IT IS NEEDED MORE THAN ONCE, THE 
C                              VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING
C                              MOSTORE(,). LATER USED AS A WAY OF
C                              DETERMINING WHETHER TO KEEP THIS
C                              VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE(,). (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE(,)
C                       THAT HAVE BEEN USED IN THIS RUN. (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE(,) (J=1,ND10).
C                       WHEN CORE() IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C                ND10 = DIMENSION OF CORE(). (INPUT)
C               LASTL = THE LAST LOCATION IN CORE( ) USED FOR MOS-2000 INTERNAL
C                       STORAGE.  INITIALIZED TO 0 ON FIRST ENTRY TO GSTORE.
C                       ALSO INITIALIZED IN U201 IN CASE GSTORE IS NOT ENTERED.
C                       (INPUT-OUTPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE. (INPUT)
C               LASTD = TOTAL NUMBER OF PHYSICAL RECORDS ON DISK FOR MOS-2000
C                       INTERNAL STORAGE.  MUST BE CARRIED WHENEVER GSTORE
C                       IS TO BE CALLED.  (INPUT)
C              NSTORE = THE NUMBER OF TIMES GSTORE HAS BEEN ENTERED.  GSTORE
C                       KEEPS TRACK OF THIS AND RETURNS THE VALUE.  (OUTPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING COUNT FROM THE BEGINNING OF
C                       THE PROGRAM.  THIS COUNT IS MAINTAINED IN
C                       CASE THE USER NEEDS IT(DIAGNOSTICS, ETC.).
C                       (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).
C                       (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).
C                       (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       IS2(3) AND IS2(4) ARE USED BY THE CALLING
C                       PROGRAM AS THE GRID DIMENSIONS.
C                       (INTERNAL-OUTPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).
C                       (INTERNAL)
C                 ND7 = DIMENSION OF IS0(),IS1(),IS2(), AND IS4().
C                       NOT ALL LOCATIONS ARE USED. (INPUT)
C            IBNDS(J) = IBNDS IS A DIAGNOSTIC ARRAY USED TO
C                       DETERMINE THE METHOD USED TO ESTIMATE THE
C                       VALUE OF SATLEVNUM, J=1,ND2X3 
C                       (INPUT: INTERNAL, PASSED IN AS FD2).
C                       03=FIRST LEVEL SATURATION (WET ASSUMED)
C                       21=MULTI-LEVEL WET TO WET SATURATION AND
C                          MULTI-LEVEL WET TO WET EQUAL SATURATION
C                       01=MULTI-LEVEL DRY TO WET SATURATION
C                       11=MULTI-LEVEL DRY TO WET SPECIAL CASE
C                       02=MULTI-LEVEL DRY TO WET EQUAL SATURATION
C              FD3(J) = WORK ARRAY USED TO READ THE GRIDDED TERRAIN
C                       HEIGHT DATA, J=1,ND2X3 (INPUT).
C              FD4(J) = WORK ARRAY USED TO READ THE GRIDDED RH OF
C                       LOWEST 30 MB DATA, J=1,ND2X3 (INPUT).
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.
C                       ALL WORK ARRAYS ARE DIMENSIONED ND2X3. (INPUT)
C              IP12   = INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE INPUT FILES WILL BE PRINTED TO
C                       THE FILE WHOSE UNIT NUMBER IS IP12.
C               ISTAV = 0 SINCE THE DATA RETURNED ARE GRID DATA.
C                       (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64). (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS, EITHER 1 OR 2.  (INPUT)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C                 IER = STATUS RETURN
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C                   0 = GOOD RETURN
C                 100 = THE TWO GRIDS NEEDED ARE NOT THE SAME SIZE
C                 103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE
C                       SATLEVNUM.
C                 187 = THE USER WAS TRYING TO USE AND NDATE
C                       FOR WHICH DATA WAS NOT AVAILABLE.
C                       SEE GFETCH FOR OTHER VALUES.
C
C
C        ADDITIONAL VARIABLES: INTEGERS
C
C             ICCC_HT = CCC PORTION OF THE HEIGHT GRIDDED MODEL
C                       FIELDS, MD_HT(1) (INTERNAL).
C             ICCC_RH = CCC PORTION OF THE RH GRIDDED MODEL FIELDS,
C                       MD_RH(1) (INTERNAL).
C               IDIFF = A LOG FIX PARAMETER. THE ADJUSTED VALUE OF
C                       IRHDIFF. (IDIFF IS THE CORRECT VALUE OF 
C                       IRHDIFF (INTERNAL).
C           IFFF_RH30 = FFF PORTION OF THE RH30 (LOWEST 30 MB RH)
C                       GRIDDED MODEL FIELDS, MD_RH30(1) (INTERNAL).
C                ILEV = A COUNTER USED TO DETERMINE DRY AND WET 
C                       LAYERS. ILEV=K ASSUMES CONTINUOUS WET
C                       CONDITIONS (ABOVE AND BELOW TERRAIN HEIGHT)
C                       ILEV NE K INDICATES ATLEAST ONE DRY PRESSURE
C                       LEVEL BELOW ITHRESH (INTERNAL).
C              INTERV = A LOG FIX PARAMETER. THIS VALUE IS DEPENDENT
C                       ON ITHRESH. EQUALS 3 FOR 85%, =2 FOR 90% AND 
C                       =1 FOR 95% (INTERNAL).
C             IRHDIFF = A LOG FIX PARAMETER. THIS VALUE EQUALS THE 
C                       DIFFERENCE BETWEEN THE SATURATION LEVEL RH 
C                       AND ITHRESH (INTERNAL).
C         IS2_RH30(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       IS2(3) AND IS2(4) ARE USED BY THE 30 MB
C                       RH (INTERNAL).
C         IS2_TERR(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       IS2(3) AND IS2(4) ARE USED BY THE TERRAIN
C                       HEIGHT (INTERNAL).
C             ITHRESH = THE THRESHOLD SATURATION LEVEL, DEPENDENT
C                       ON THE FFF PORTION OF ID(1) (INPUT-INTERNAL)
C                  IT = COUNTER FOR NGRIDC(K,NSLABC1) TEST (INTERNAL).
C               KD(I) = THE 4-WORD ID OF THE TERRAIN HEIGHT GRIDDED
C                       DATA, I=1,4 (INTERNAL).
C           KDPARS(J) = ARRAYS TO HOLD ID OF VARIABLES TO BE FETCHED
C                       (J=1,15). (INTERNAL)
C            LEVEL(N) = ARRAY OF AVAILABLE LEVELS USED PER 
C                       RESPECTIVE MODEL, N=1,11, WHERE 11 IS THE
C                       MAXIMUM NUMBER OF LEVELS (INTERNAL).
C                  LX = IS2(3) X PORTION OF GRID (INTERNAL).
C                  LY = IS2(4) Y PORTION OF GRID (INTERNAL).
C            MD_HT(I) = THE 4-WORD ID OF THE HEIGHT GRIDDED MODEL
C                       DATA, I=1,4 (INTERNAL).
C            MD_RH(I) = THE 4-WORD ID OF THE RH GRIDDED MODEL DATA,
C                       I=1,4 (INTERNAL).
C          MD_RH30(I) = THE 4-WORD ID OF THE RH30 (LOWEST 30 MB RH)
C                       GRIDDED MODEL FIELDS, MD_RH30(1) (INTERNAL).
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 WHEN DATA ARE NOT PACKED. (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 WHEN DATA ARE NOT PACKED.  (INTERNAL)
C               NPACK = 2 FOR TDL GRIB PACKED DATA; 1 FOR NOT PACKED
C                       THIS IS RETURNED FROM GFETCH. (INTERNAL)
C              NSLAB1 = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,). COMPARED TO NSLAB FOR GRID
C                       CONSISTENCY OF MULTIPLE INPUTS (INTERNAL).
C              NSLAB2 = SAME PURPOSE AS NSLAB1 (INTERNAL).
C             NSLABC1 = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) FOR TERRAIN HEIGHT READ FROM
C                       CONST. NSLABC1 IS COMPARE TO NSLAB FOR 
C                       GRID CONSISTENCY WITH THE MODEL DATA (INTERNAL).
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE,
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS
C                       STORED IN LSTORE(9,). (INTERNAL)
C              NUMLEV = NUMBER OF LEVELS TO BE USED FOR A GIVEN
C                       MODEL (INTERNAL).
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA().  THIS
C                       IS RETURNED FROM GFETCH (INTERNAL)
C               I,J,K = LOOP CONTROL VARIABLE 
C
C
C        ADDITIONAL VARIABLES (REAL)
C
C               FD(N) = ARRAY OF SATURATION LEVEL ESTIMATES,
C                       N=1,11 LEVELS (INTERNAL).
C           FDHT(J,N) = ARRAY OF GRIDDED HEIGHT MODEL DATA TO A 
C                       MAXIMUM OF N=1,11 LEVELS, J=1,ND2X3 
C                       (INTERNAL).
C           FDRH(J,N) = ARRAY OF GRIDDED RH RH MODEL DATA TO A 
C                       MAXIMUM OF N=1,11 LEVELS, J=1,ND2X3 
C                       (INTERNAL).
C            F_INTERP = THE INITIAL SATURATION LEVEL, INTERPOLATED
C                       FOR DRY TO WET, AND THE SATURATION LEVEL
C                       ITSELF FOR WET TO WET (INTERNAL). 
C         SATL_INT(K) = THE INTERPOLATION USED FOR SATURATED 
C                       LEVELS (WET TO WET) BELOW AND ABOVE THE 
C                       TERRAIN HEIGHT LEVEL; THE LOG FIX.
C                       (INTERNAL).
C              RLEV(J)= THE TOTAL NUMBER OF QUALIFYING CLOUD LEVELS.
C                       A CLOUD LEVEL QUALIFIES IF IT'S ABOVE THE 
C                       TERRAIN HEIGHT, J=1,ND2X3 (INTERNAL). 
C            SATNUM(J)= THE NUMBER OF LEVELS MATCHING OR EXCEEDING THE 
C                       SATURATION THRESHOLD, J=1,ND2X3 (INTERNAL).
C               XMESH = REAL VALUE OF NGRIDC(2,K) FOR NSLAB TERRAIN
C                       HT COMPARISON (INTERNAL).
C             XMESHC1 = REAL VALUE OF NGRIDC(2,K) FOR NSLABC1 TERRAIN
C                       HT COMPARISON (INTERNAL).
C
C
C        LOGICAL VARIABLES 
C
C               INEXT = A LOGICAL USED TO CHECK WHEN A SATURATION
C                       LEVEL ESTIMATE HAS BEEN MADE (INTERNAL). 
C            RH30_ONE = A LOGICAL USED COMPARE THE FIRST INSTANCE
C                       OF A SATURATED LEVEL WITH THE VARIABLE 
C                       FD4 (RH30MB). IF THE SATURATION LEVEL HEIGHT
C                       IS LESS THAN 1000 FT (AFTER ALL INTERPOLATION)
C                       THEN, IF RH30MB IS LESS THAN ITHRESH, THIS
C                       INITIAL SATURATION LEVEL ESTIMATE IS 
C                       SKIPPED (INTERNAL). NOTE: A MAXIMUM OF ONE
C                       LEVEL CAN BE SKIPPED.
C         
C        NON-SYSTEM SUBROUTINES USED
C         GFETCH,PRSID1,CONST
C
      IMPLICIT NONE
C
C        CALLING ARGUMENTS:
C
      INTEGER KFILDO,KFIL10
      INTEGER IDPARS(15),JD(4),NDATE
      INTEGER KFILRA(NUMRA),NUMRA
      INTEGER ICALLD(L3264W,ND2X3)
      INTEGER ISDATA(ND1),ND1,NSTA
      INTEGER NGRIDC(6,ND11),NGRID,ND11,NSLAB,IPACK(ND5),
     1        IWORK(ND5),ND5
      INTEGER LSTORE(12,ND9),ND9,LITEMS,ND10,LASTL
      INTEGER NBLOCK,LASTD,NSTORE,NFETCH
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7),ND7
      INTEGER IBNDS(ND2X3),ND2X3,IP12
      INTEGER ISTAV,L3264B,L3264W,MISTOT,IER
C
      REAL STALAT(ND1),STALON(ND1)
      REAL SDATA(ND1),DIR(ND1,2,ND11)
      REAL DATA(ND5)
      REAL CORE(ND10)
      REAL FD3(ND2X3),FD4(ND2X3)
C
      CHARACTER*60 RACESS(NUMRA)
      CHARACTER*8 CCALL(ND1,6),CCALLD(ND2X3)
      CHARACTER*20 NAME(ND1)

C        ADDITIONAL VARIABLES:
C
      INTEGER ICCC_RH,ICCC_HT,IDIFF,IFFF_RH30
      INTEGER ILEV,INTERV,IRHDIFF,ITHRESH,KD(4),KDPARS(15)
      INTEGER LEVEL(11),LX,LY
      INTEGER MD_RH(4),MD_HT(4),MD_RH30(4)
      INTEGER IS2_RH30(ND7),IS2_TERR(ND7),MISSP,MISSS,NPACK
      INTEGER NSLAB1,NSLABC1,NSLAB2,NTIMES,NUMLEV,NWORDS
      INTEGER I,J,K
C
      REAL FDRH(ND2X3,11),FDHT(ND2X3,11),F_INTERP,SATL_INT(5)
      REAL XMESH,XMESHC1
      REAL SATNUM(ND2X3),RLEV(ND2X3),FD(11)
C
      LOGICAL INEXT,RH30_ONE
C
      DATA ICCC_RH/3/,ICCC_HT/1/,IFFF_RH30/7/
C
C
C        LOG    LOG    LOG  (8)   (6)   (4)   (2)  (1.5)
C
      DATA SATL_INT/0.903,0.778,0.602,0.301,0.176/
C
C*******************************************************
C
C        STEP 1A. INITIALIZE ALL THE VARIABLES INCLUDING
C                 THE WORK ARRAYS AND THE FINAL PRODUCT
C
      INEXT=.FALSE.
      IER  =0
      ISTAV=0
C
      DO 10 I=1,ND2X3
        IBNDS(I)=0
        DATA(I)=0.0
 10   CONTINUE
C
C       STEP 1B. MAKE SURE THE REQUESTED PREDICTOR IS THE 
C                 SATLEVNUM PREDICTOR
C
      IF(IDPARS(1).EQ.003)THEN
        SELECT CASE(IDPARS(2))
          CASE(341,351,361)
            GO TO 105
          CASE(342,352,362)
            GO TO 105
          CASE(343,353,363)
            GO TO 105
        END SELECT
      ENDIF
C
      IER=103
      WRITE(KFILDO,100)(JD(J),J=1,4),IER
 100  FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE',
     &  ' SATLEVNUM.  PREDICTOR ',I9.9,I10.9,I10.9,I4.3,' NOT ',
     &  'ACCOMMODATED IN SATLEVNUM.  IER = ',I3)
      GO TO 800
C
C        STEP 1C. SET THE VARIABLE ITHRESH (SATURATION THRESHOLD) TO 
C                 TO A VALUE DEPENDENT ON IDPARS(2)
C
 105  SELECT CASE(IDPARS(2))
C
C        MRF AND AVN 5 LEVEL
        CASE(341)
          ITHRESH=85
          INTERV=3
        CASE(351)
          ITHRESH=90
          INTERV=2
        CASE(361)
          ITHRESH=95
          INTERV=1
C
C        ETA 9 LEVEL
        CASE(342)
          ITHRESH=85
          INTERV=3
        CASE(352)
          ITHRESH=90
          INTERV=2
        CASE(362)
          ITHRESH=95
          INTERV=1
C
C        AVN 11 LEVEL
        CASE(343)
          ITHRESH=85
          INTERV=3
        CASE(353)
          ITHRESH=90
          INTERV=2 
        CASE(363)
          ITHRESH=95
          INTERV=1
      END SELECT
C
C
C        STEP 2. NOW CHECK TO SEE WHICH VERTICAL PROFILE IS REQUESTED
C                AND SET THE LEVEL AND COUNTING INDEX ACCORDINGLY
C                ALSO, CHECK THAT THE USER IS ASKING FOR AN 
C                APPROPRIATE NDATE FOR THAT MODEL
C
C        CHECK TO SEE WHICH VERTICAL PROFILE WAS REQUESTED
C
C        MRF (AVN PRIOR TO 10/1/98) VERTICAL PROFILE
      SELECT CASE(IDPARS(2))
        CASE(341,351,361)
          LEVEL(1)=1000
          LEVEL(2)=925
          LEVEL(3)=850
          LEVEL(4)=700
          LEVEL(5)=500
          NUMLEV=5
          KD(1)=409020000
          KD(2)=0
          KD(3)=0
          KD(4)=0
      END SELECT
C 
      IF(IDPARS(4).EQ.8)THEN
        IF(MOD(MOD(NDATE,100),3).NE.0)THEN
          WRITE(KFILDO,135)NDATE
 135      FORMAT(/' ****NDATE, ',I10,', FOR AVN SATLEVNUM MUST',
     &             ' BE A MULTIPLE OF 3.')
          IER=187
          GO TO 800
        ENDIF
      ENDIF
C
C	 ETA VERTICAL PROFILE
      SELECT CASE(IDPARS(2))
        CASE(342,352,362)
          LEVEL(1)=1000
          LEVEL(2)=950
          LEVEL(3)=900
          LEVEL(4)=850
          LEVEL(5)=800
          LEVEL(6)=750
          LEVEL(7)=700
          LEVEL(8)=600
          LEVEL(9)=500
          NUMLEV=9
          KD(1)=409030000
          KD(2)=0
          KD(3)=0
          KD(4)=0
      END SELECT
      IF(IDPARS(4).EQ.7)THEN
        IF(MOD(MOD(NDATE,100),6).NE.0)THEN
          WRITE(KFILDO,145)NDATE
 145      FORMAT(/' ****NDATE, ',I10,', FOR ETA SATLEVNUM MUST',
     &            ' BE 00Z,06Z,12Z,OR 18Z.')
          IER=187
          GO TO 800
        ENDIF
      ENDIF 
C
C        AVN VERTICAL PROFILE (AFTER 10/1/98)
      SELECT CASE(IDPARS(2))
        CASE(343,353,363)
          LEVEL(1)=1000
          LEVEL(2)=975
          LEVEL(3)=950
          LEVEL(4)=925
          LEVEL(5)=900
          LEVEL(6)=850
          LEVEL(7)=800
          LEVEL(8)=750
          LEVEL(9)=700
          LEVEL(10)=600
          LEVEL(11)=500
          NUMLEV=11
          KD(1)=409020000
          KD(2)=0
          KD(3)=0 
          KD(4)=0
      END SELECT
C
C        STEP 3A. READ THE TERRAIN DATA FROM A CONSTANT FILE
C         FD3 (TERNG). ARRAY KD IS MODEL DEPENDENT.
C
      CALL PRSID1(KFILDO,KD,KDPARS)

      DO 160 I=1,NUMRA
        IF(KFILRA(I).EQ.44)THEN
          CALL CONST1(KFILDO,KFIL10,IP12,
     1               KD,KDPARS,KD,NDATE,
     2               KFILRA(I),RACESS(I),1, 
     3               CCALL,ICALLD,CCALLD,NAME,STALAT,STALON,
     4               ISDATA,SDATA,DIR,ND1,NSTA,
     5               NGRIDC,NGRID,ND11,NSLAB, 
     6               IPACK,IWORK,FD3,ND2X3,
     7               LSTORE,ND9,LITEMS,CORE,ND10,LASTL,
     8               NBLOCK,LASTD,NSTORE,NFETCH,
     9               IS0,IS1,IS2,IS4,ND7,
     A               ISTAV,L3264B,L3264W,IER)
C
          IF(IER.NE.0)THEN
            WRITE(KFILDO,150)IER
 150        FORMAT(/' ****SATLEVNUM--TERRAIN HT READ ERROR, IER=',I3)
            GO TO 800
          ENDIF
C
          NSLABC1=NSLAB
          DO 152 J=1,ND7
            IS2_TERR(J)=IS2(J)
 152      CONTINUE
          GO TO 161
        ENDIF
 160  CONTINUE
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        STEP 3B. READ IN THE LOWEST 30 MB RH. (FD4)
C                 CREATE AN ID FOR THE RH AT THAT LEVEL
 161  MD_RH30(1) = ICCC_RH*1000000 + IFFF_RH30*1000 + IDPARS(4)
      MD_RH30(2) = 970
      MD_RH30(3) = IDPARS(9)*1000000 + IDPARS(12)
      MD_RH30(4) = 0
      CALL GFETCH(KFILDO,KFIL10,MD_RH30,7777,LSTORE,ND9,LITEMS,IS0,
     &            IS1,IS2,IS4,ND7,IPACK,IWORK,FD4,ND2X3,NWORDS,
     &            NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,NFETCH,
     &            NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0) GOTO 800
C
      NSLAB1 = NSLAB
      DO 162 I=1,ND7
        IS2_RH30(I)=IS2(I)
 162  CONTINUE
C
C
C        STEP 3C. SET UP THE VERTICAL PROFILE ARRAYS BASED ON 
C                 THE NUMBER OF LEVELS (NUMLEV) FOR THE GIVEN MODEL.
C
      DO 200 K=1,NUMLEV
C
C          STEP 3CA. READ IN THE RH GRIDDED MODEL FIELDS CREATE AN
C                    ID FOR THE RH AT THAT LEVEL (FDRH)
        MD_RH(1) = ICCC_RH * 1000000 + IDPARS(4)
        MD_RH(2) = LEVEL(K)
        MD_RH(3) = IDPARS(9)*1000000 + IDPARS(12)
        MD_RH(4) = 0
        CALL GFETCH(KFILDO,KFIL10,MD_RH,7777,LSTORE,ND9,LITEMS,IS0,
     &              IS1,IS2,IS4,ND7,IPACK,IWORK,FDRH(1,K),ND2X3,
     &              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,NFETCH,
     &              NSLAB,MISSP,MISSS,L3264B,1,IER)
        IF(MISSP.NE.0)MISTOT=MISTOT+1
        IF(IER.NE.0) GOTO 800
C
        LX = IS2(3)
	LY = IS2(4)
        NSLAB2 = NSLAB
C
C          STEP 3CB. READ IN THE HEIGHT GRIDDED MODEL FIELDS CREATE AN
C                    ID FOR THE HEIGHT AT THAT LEVEL (FDHT)
        MD_HT(1) = ICCC_HT * 1000000 + IDPARS(4)
        MD_HT(2) = LEVEL(K)
        MD_HT(3) = IDPARS(9)*1000000 + IDPARS(12)
        MD_HT(4) = 0
        CALL GFETCH(KFILDO,KFIL10,MD_HT,7777,LSTORE,ND9,LITEMS,IS0,
     &              IS1,IS2,IS4,ND7,IPACK,IWORK,FDHT(1,K),ND2X3,
     &              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,NFETCH,
     &              NSLAB,MISSP,MISSS,L3264B,1,IER)
        IF(MISSP.NE.0)MISTOT=MISTOT+1
        IF(IER.NE.0) GOTO 800
C
C          CHECK THE GRID DIMENSIONS HEIGHT AND RH LEVELS
C
        IF((NSLAB.NE.NSLAB2).OR.(IS2(3).NE.LX).OR.(IS2(4).NE.LY))THEN
          WRITE(KFILDO,165)NSLAB,NSLAB2
 165      FORMAT(/,' ****THE GRID CHARACTERISTICS OF THE MODEL',
     &           ' HEIGHTS AND RH LEVELS ARE DIFFERENT.',I3,2X,I3)
          IER=100
          GOTO 800
        ENDIF
C
C         
 200  CONTINUE
C
C        STEP 3D. CHECK THE GRID DIMENSIONS HEIGHTS AND 30 MB RH
C
      IF((NSLAB.NE.NSLAB1).OR.(IS2_RH30(3).NE.LX).OR.
     *                        (IS2_RH30(4).NE.LY))THEN
        WRITE(KFILDO,205)NSLAB,NSLAB1
 205    FORMAT(/,' ****THE GRID CHARACTERISTICS OF THE MODEL',
     &         ' HEIGHTS AND 30 MB RH ARE DIFFERENT.',I3,2X,I3)
        IER=100
        GOTO 800
      ENDIF
C
C        STEP 3E. CHECK THE GRID DIMENSIONS HEIGHTS AND TERRAIN
C                 HEIGHT
C
      IF(IS2_TERR(3).NE.LX.OR.IS2_TERR(4).NE.LY)THEN
         WRITE(KFILDO,210)IS2_TERR(3),IS2_TERR(4),LX,LY
 210     FORMAT(/' ****GRID SIZE OF TERRAIN = ',2I6,
     1           ' DOES NOT MATCH MODEL GRID SIZE = ',2I6)
         IER=100
         GO TO 800
      ENDIF
C
      IF(NSLAB.NE.NSLABC1)THEN
C
C           DOUBLE CHECK IF TERRAIN AND MODEL GRID ARE SO CLOSE
C           THAT THEY ARE CONSIDERED TO BE ESSENTIALLY IDENTICAL
C           THE TOLERANCE FOR THE CORNER POINTS IS .0005 DEGREES,
C           WHICH IS OF THE ORDER OF .05 KM.  THE TOLERANCE FOR
C           THE MESH LENGTH IS 5 MM.
C
         IF(NGRIDC(1,NSLAB).NE.NGRIDC(1,NSLABC1).OR.
     1      NGRIDC(3,NSLAB).NE.NGRIDC(3,NSLABC1).OR.
     2      NGRIDC(4,NSLAB).NE.NGRIDC(4,NSLABC1).OR.
     3      ABS(NGRIDC(2,NSLAB)-NGRIDC(2,NSLABC1)).GT.5.OR.
     4      ABS(NGRIDC(5,NSLAB)-NGRIDC(5,NSLABC1)).GT.5.OR.
     5      ABS(NGRIDC(6,NSLAB)-NGRIDC(6,NSLABC1)).GT.5)THEN
            WRITE(KFILDO,215)NSLAB,NSLABC1,
     1                      (NGRIDC(J,NSLAB),J=1,6),
     2                      (NGRIDC(J,NSLABC1),J=1,6)
 215        FORMAT(/' ****THE GRID CHARACTERISTICS OF THE MODEL',
     1               ' HEIGHTS AND THE TERRAIN HEIGHT ARE',
     2               ' DIFFERENT.  NSLAB VALUES =',2I4,
     3              /'  GRID CHARACTERISTICS ARE:',
     4              /(' ',6I10))
            IER=100
            GO TO 800
         ENDIF
C
      ENDIF
C
C 
C        STEP 4. FIND THE HEIGHT OF THE PARTICULAR SATURATION LEVEL
C                THRESHOLD OVER THE GRID
C
C
      DO 400 J=1,LX*LY
C
C          SOME INITIALIZATION
        SATNUM(J)=0.0
        RLEV(J)=0.0
        RH30_ONE=.FALSE.
        ILEV=0
C
        IF(NINT(FD3(J)).EQ.9999)THEN
          DATA(J)=9999.
          GO TO 400
        ENDIF
C
C
        DO 350 K=1,NUMLEV
C
C         INEXT IS TRUE FOR A SATURATION LEVEL ESTIMATE >= ITHRESH
          INEXT=.FALSE.
          FD(K)=0.0

          IF((NINT(FDRH(J,K)).EQ.9999).OR.
     *           (NINT(FDHT(J,K)).EQ.9999))THEN
            DATA(J)=9999.
            GO TO 400
          ENDIF
C
          IF(FDHT(J,K).GE.FD3(J)) RLEV(J)=RLEV(J)+1.0
C
C              STEP 4AA.THE TOP HEIGHT < 0 (BELOW GROUND) AND     
C                       THE TOP RH >= THRESH
          IF((NINT(FDRH(J,K)).GE.ITHRESH).AND.
     *      (NINT(FDHT(J,K)).LT.0))THEN
            ILEV=K
C
C              STEP 4A. THE TOP HEIGHT => 0 (ABOVE GROUND) AND
C                       THE TOP RH >= THRESH
          ELSEIF((NINT(FDRH(J,K)).GE.ITHRESH).AND.
     *      (NINT(FDHT(J,K)).GE.0))THEN
            ILEV=ILEV+1 
C
C
C                STEP 4B. FIRST LEVEL (ASSUMED TO BE THE TOP HEIGHT)
C                         THIS LEVEL MUST BE ABOVE THE TERRAIN. 
            IF(K.EQ.1)THEN
              IF(FDHT(J,K).GE.FD3(J))THEN
C
C                    1. ESTIMATE RH HEIGHT/TERRRAIN HEIGHT DIFF.    
                FD(K)=FDHT(J,K)-FD3(J)
                INEXT=.TRUE.
                IBNDS(J)=3
                SATNUM(J)=SATNUM(J)+1.0
C
C                    2. "LINEAR (LOG) FIX" LOWERS THE SATURATION LEVEL
C                       WHEN THE RH EXCEEDS THE RH THRESHOLD (RH AT THE
C                       SFC IS UNKNOWN).
C                       ASSUME WET ASSUME WET ASSUME WET LAYER AT SFC
                IRHDIFF=NINT(FDRH(J,K))-ITHRESH
                IF(IRHDIFF.NE.0)THEN
                  IDIFF=IRHDIFF/INTERV
                  IF(MOD(IRHDIFF,INTERV).NE.0) IDIFF=IDIFF+1
                  FD(K)=FD(K)*SATL_INT(IDIFF)
                ENDIF
              ENDIF
C
C
C                STEP 4C. MULTI-LEVEL CHECK
            ELSEIF(K.GT.1)THEN
              IF(NINT(FDRH(J,K)).GT.ITHRESH)THEN
C
C                    1. THE LOW HEIGHT IS => 0 AND LOW RH < THRESH (DRY)
C                       (DRY) 
C                       ALSO INCLUDED:THE LOW HEIGHT IS <  0 AND LOW RH 
C                       < THRESH. THEREFORE, ESTIMATE SATURATION LEVEL
C                       RIGHT KNOW. (INTERPOLATE) (ILEV NE K)
                IF(ILEV.NE.K)THEN
                  F_INTERP=FDHT(J,K-1)+(FDHT(J,K)-FDHT(J,K-1))*
     *            (ITHRESH-FDRH(J,K-1))/(FDRH(J,K)-FDRH(J,K-1))
C
C                    2. LOW RH AND HIGH RH ARE (WET) (ILEV=K)
C                       SATURATION LEVEL IS ASSUMED TO BE LOWER THAN
C                       PRESSURE LEVEL HEIGHT. IF THE PRESSURE LEVEL 
C                       HEIGHT IS ABOVE THE TERRAIN HEIGHT,
C                       THE "LOG FIX" VALUE IS GUARANTEED TO BE ABOVE
C                       THE TERRAIN HEIGHT (FRACTION OF THE 
C                       PRESSURE HEIGHT). 
                ELSEIF(ILEV.EQ.K)THEN
                  F_INTERP=FDHT(J,K)
                ENDIF
C
C                    3. THE SATURATION LEVEL (F_INTERP) MUST BE ABOVE
C                       THE TERRAIN HEIGHT (WET OR DRY).
C
                IF((F_INTERP.GE.FD3(J)).AND.(FDHT(J,K).GE.
     *            FD3(J)))THEN
C
C                      3A. LOW RH AND HIGH RH ARE WET (ILEV=K)
                  IF(ILEV.EQ.K)THEN
                    FD(K)=F_INTERP-FD3(J)
                    INEXT=.TRUE.
                    IBNDS(J)=21
                    SATNUM(J)=SATNUM(J)+1.0
C
C                        "LOG FIX" FOR WET (ILEV=K), LOWERS THE
C                         SATURATION LEVEL WHEN THE RH EXCEEDS THE 
C                         RH THRESHOLD.
                    IRHDIFF=NINT(FDRH(J,K))-ITHRESH
                    IDIFF=IRHDIFF/INTERV
                    IF(MOD(IRHDIFF,INTERV).NE.0) IDIFF=IDIFF+1
                    FD(K)=FD(K)*SATL_INT(IDIFF)
C
C                      3B. THE LOW HEIGHT IS => 0 AND LOW RH < THRESH 
C                          (DRY)   (DRY) ALSO INCLUDED:
C                          THE LOW HEIGHT IS <  0 AND LOW RH < THRESH
C                          (USE THE INTERPOLATED VALUE) (ILEV NE K)
C                          (ONLY THE HIGHER LEVEL RH IS >= ITHRESH)
                  ELSEIF(ILEV.NE.K)THEN
                    FD(K)=F_INTERP-FD3(J)
                    INEXT=.TRUE.
                    IBNDS(J)=1
                    SATNUM(J)=SATNUM(J)+1.0
                  ENDIF
C
C                    SPECIAL CASE:
C                      FIRST SATURATED LEVEL ABOVE TERRAIN, WHERE
C                      THE LOWER (BELOW TERRAIN) RH IS (DRY) (SEE 3B)
C                      PRODUCING A RARE F_INTERP BELOW FD3 (TERNG).
C                      THEREFORE ASSUME THE K.EQ.1 CONDITION. 
C                      ONLY POSSIBLE WHEN ILEV NE K.
                ELSEIF((F_INTERP.LT.FD3(J)).AND.(FDHT(J,K).GE.
     *            FD3(J)))THEN
                  IF(ILEV.NE.K)THEN
                    FD(K)=FDHT(J,K)-FD3(J)
                    INEXT=.TRUE.
                    IBNDS(J)=11
                    SATNUM(J)=SATNUM(J)+1.0
C
C                        APPLY THE "LOG FIX"
                    IRHDIFF=NINT(FDRH(J,K))-ITHRESH
                    IDIFF=IRHDIFF/INTERV
                    IF(MOD(IRHDIFF,INTERV).NE.0) IDIFF=IDIFF+1
                    FD(K)=FD(K)*SATL_INT(IDIFF)
                  ENDIF
C
C                    STEP 4C. STILL BELOW TERRAIN, THEREFORE MAKE 
C                             ILEV EQUAL (CONTINUED) TO K,
C                             WHICH TURNS OFF THE MULTI-LEVEL (DRY) 
C                             F_INTERP. DO NOT WANT TO INTERPOLATE TWO
C                             CONSECUTIVE SATURATED LEVELS.  CONTINUE. 
                ELSEIF((F_INTERP.LT.FD3(J)).AND.(FDHT(J,K).LT.
     *            FD3(J)))THEN
                  ILEV=K
                ENDIF
C
C
C                  STEP 4D. IF THE TOP RH = THRESH: NO MULTI-LEVEL 
C                           CHECK AND NO INTERPOLATION, SET
C                           SATURATION TO PRESSURE LEVEL REGARDLESS
C                           OF SATURATED LOW LAYERS(ILEV=K) OR 
C                           DRY UNDERNEATH (ILEV NE K).
              ELSEIF(NINT(FDRH(J,K)).EQ.ITHRESH)THEN
                IF(FDHT(J,K).GE.FD3(J))THEN
C
C                      SATURATION AT THE LOWEST LAYER
                  IF(ILEV.EQ.K)THEN
                    FD(K)=FDHT(J,K)-FD3(J)
                    INEXT=.TRUE.
                    IBNDS(J)=21
                    SATNUM(J)=SATNUM(J)+1.0
                  ELSE
                    FD(K)=FDHT(J,K)-FD3(J) 
	            INEXT=.TRUE.
                    IBNDS(J)=2
                    SATNUM(J)=SATNUM(J)+1.0
                  ENDIF
                ELSE
                  ILEV=K
                ENDIF
              ENDIF
            ENDIF
C
          ENDIF
C
C              STEP 5. CONVERSION OF SATLEVNUM HEIGHTS FROM
C                      METERS TO HUNDREDS OF FEET.
          IF(INEXT)THEN
            IF(NINT(FD(K)).LE.0)THEN
              FD(K)=0.0
            ELSE
C
C                  CONVERT METERS TO HUNDREDS OF FEET
              FD(K)=FD(K)/30.48
            ENDIF
            IF(NINT(FD(K)).GT.350) IBNDS(J)=IBNDS(J)+100
C
C                STEP 6. REDUCE THE AFFECT OF LOW LEVEL MOISTURE
C                        BY COMPARING THE ITHRESH RH TO THE LOWEST
C                        30 MB RH, BUT ONLY IF THE ITHRESH RH IS LESS
C                        THAN 1000' ABOVE THE SFC. IF THE LOWEST 30 MB
C                        RH IS LESS THAN ITHRESH, THAN WE PROCEED TO THE
C                        NEXT LEVEL. THIS TEST IS CONDUCTED ONLY ONCE
C                        PER GRID LOCATION TO AVOID OVER ELIMINATION.
            IF((.NOT.RH30_ONE).AND.(FD(K).LE.10.))THEN
C
C                  COMPARE RH30MB TO ITHRESH
              IF(NINT(FD4(J)).LT.ITHRESH)THEN
                INEXT=.FALSE.
                IBNDS(J)=0
                SATNUM(J)=SATNUM(J)-1.0
                ILEV=K
                RH30_ONE=.TRUE.
              ENDIF
            ENDIF
          ENDIF
C
 350    CONTINUE
 400  CONTINUE
C
C******************************************************
C******************************************************
C
      DO 600 J=1,LX*LY
        IF(NINT(RLEV(J)).EQ.0) DATA(J)=9999.
        IF(NINT(DATA(J)).NE.9999) THEN
          DATA(J)=SATNUM(J)/RLEV(J)
        ENDIF
 600  CONTINUE

C
      GO TO 900
C
C        IF THERE WAS A PROBLEM IN GFETCH IT WOULD COME HERE TO
C        SET DATA TO MISSING

 800   WRITE(KFILDO,805)NDATE
 805   FORMAT(/' ****SATLEVNUM ERROR: MISSING DATA FOR DATE ',I10)
C
C
 900  RETURN
      END
