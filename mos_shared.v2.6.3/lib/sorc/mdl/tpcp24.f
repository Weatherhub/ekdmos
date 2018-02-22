      SUBROUTINE TPCP24(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                  NGRIDC,ND11,NSLAB,IPACK,IWORK,FDTP,ND5,
     2                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                  IS0,IS1,IS2,IS4,ND7,
     4                  FD1,FD2,FD3,FD4,FD5,FD6,ND2X3,
     5                  ISTAV,L3264B,MISTOT,IER)
C
C        SEPTEMBER 1998   SFANOS   TDL   MOS-2000
C        OCTOBER   1998   SFANOS   MODIFIED FOR CONV  PRECIP
C        DECEMBER  1998   SFANOS   MODIFIED FOR NCONV PRECIP
C        AUGUST    2002   MALONEY  MODIFIED FOR GFS(MRF) @T192
C                                  TO USE 12H @192 AND 6H
C                                  @186 AND 180
C        AUGUST    2002   SFANOS   FIXED CALL TO GFETCH; ADDED ARRAY
C                                  INDEX TO FDTP CHECK; CHANGED
C                                  CALL TO NONCNVP; FIXED ERROR
C                                  CHECK IN 8*3-HR PRECIP PART -
C                                  SHOULD NOT GET TO THAT PART SINCE
C                                  NO CONSECUTIVE 3-HR PARTS; MODIFIED
C                                  FORMAT STATEMENTS TO CONFORM TO
C                                  FORTRAN 90 STANDARDS ON IBM
C        NOVEMBER  2002   WEISS    CHANGED ND5 TO ND2X3
C        MAY       2003   GLAHN    DIAGNOSTICS; WHITE SPACE;
C                                  ICCCFFF( ) IN DATA STATEMENT;
C                                  REARRANGED TYPE STATEMENTS;
C                                  REPLACED FD1 BY FDTP IN SOME
C                                  CALLS TO GFETCH AND NONCNVP AND 
C                                  ELIMINATED LOOPS SETTING FDTP( ) =
C                                  FD1( ); CHANGED DIMENSIONS OF
C                                  IPACK( ), IWORK( ), FDTP( ) TO ND5;
C                                  CHANGED COMPUTATIONAL LOOPS FROM
C                                  1,ND2X3 TO 1,NX*NY; ELIMINATED
C                                  ALL THREE AUTOMATIC ARRAYS; CHANGED
C                                  ND5 TO ND2X3 IN SEVERAL CALLS;
C                                  INSERTED SOME CHECKS ON IER
C        JUNE       2003   GLAHN   CHECKED GRID CHARACTERSITICS AFTER
C                                  CHECK ON IER IN TWO PLACES; ADDED
C                                  GRID CHARACTERISTS CHECK IN TWO 
C                                  PLACES; CHANGED MPT12 TO MPT6 IN
C                                  CALLS TO PRSID1 AND NONCNVP ABOVE
C                                  285
C        OCTOBER    2003   SMB     CORRECTED FORMAT STATEMENTS 170, 256,
C                                  AND 270 FOR THE IBM
C        NOVEMBER   2003   SMB     CHANGED FFF TO 226/256/286 (THIS
C                                  HAD BEEN DONE BY COSGROVE IN JUNE
C                                  2003 BUT THIS CHANGE WAS NOT IN
C                                  THIS VERSION OF TPCP24)
C        FEBRUARY   2004   SMB     REMOVED NON-CONVECTIVE CALCULATIONS;
C                                  ADDED TO NONCNVP.F
C
C        PURPOSE
C            TO COMPUTE THE 24-HR TOTAL OR CONVECTIVE PRECIP AMOUNT. 
C            THE FORMULA FOR THIS IS (DEPENDING ON THE MODEL):
C            MRF= 12-HR PRECIP AT PROJECTION T
C                 + PREVIOUS 12-HR PRECIP
C            ETA= 6-HR PRECIP AT PROJECTION T + 12-HR PRECIP
C                 AT PROJECTION T-6 + 12-HR PRECIP AT 
C                 PROJECTION T-18 - 6-HR PRECIP AT PROJECTION
C                 T-24. ANOTHER SCENARIO IS A 12-HR PRECIP
C                 AT PROJECTION T + PREVIOUS 12-HR PRECIP;
C                 ETA32 06/18Z WILL ADD (8) 3-HR PRECIPS
C            AVN= 4 6-HR PRECIP AT T,T-6,T-12,T-18 OR
C                 3-HR PRECIP AT PROJECTION T + 6-HR PRECIP
C                 AT PROJECTION T-3 + 6-HR PRECIP AT PROJECTION
C                 T-9 + 6-HR PRECIP AT PROJECTION T-15 + 6-HR
C                 PRECIP AT PROJECTION T-21 - 3-HR PRECIP AT
C                 PROJECTION T-24(FOR GREATER THAN 24-HRS)
C            GFS= AVN OR MRF METHOD, EXCEPT AT F192, WHERE THE 
C                 GFS TRANSITIONS FROM 6-HR PRECIP TO 12-HR PRECIP.
C                 HERE, USE THE 12-HR PRCIP AT 192 + THE 6-HR PRCIP
C                 AT 186 + THE 6-HR PRECIP AT 180.
C                   
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               003 226 - 24-HR TOTAL PRECIP AMT
C                         VARIABLE ON AN ISOBARIC SURFACE
C                         (FORMERLY 003 225)
C               003 256 - 24-HR CONVECTIVE PRECIP AMT
C                         VARIABLE ON AN ISOBARIC SURFACE
C                         (FORMERLY 003 255)
C
C        DATA SET USE
C            KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT(PRINT) FILE.
C                     (OUTPUT)
C            KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                     ACCESS.(INPUT-OUTPUT)
C
C        VARIABLES
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE(,) (J=1,ND10).
C                       WHEN CORE() IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C              FD1(J) = WORK ARRAY FOR EITHER THE 3-HR TOTAL,
C                       CONVECTIVE,OR NON-CONVECTIVE 
C                       PRECIPITATION(J=1,ND2X3).(INTERNAL)
C              FD2(J) = WORK ARRAY FOR EITHER THE 6-HR TOTAL, 
C                       CONVECTIVE,OR NON-CONVECTIVE 
C                       PRECIPITATION(J=1,ND2X3).(INTERNAL)
C              FD3(J) = WORK ARRAY FOR EITHER THE 12-HR TOTAL, 
C                       CONVECTIVE OR NON-CONVECTIVE 
C                       PRECIPITATION(J=1,ND2X3).(INTERNAL)
C      FD4(J), FD5(J), FD6(J) = WORK ARRAYS (J=1,ND2X3).  (INTERNAL)
C             FDTP(J) = ARRAY TO HOLD RETURNED DATA WHEN THE DATA ARE
C                       AT GRIDPOINTS. IN THIS CASE, EITHER THE 24-HR
C                       TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION(J=1,ND2X3). (OUTPUT)
C                   I = LOOP CONTROL VARIABLE
C           ICCCFFF() = CONTAINS IDPARS(1) AND IDPARS(2) ID FOR THE
C                       METEOROLOGICAL PARAMETERS BEING USED.
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID() (J=1,15).
C                       (INPUT)
C                       J=1--CCC      (CLASS OF VARIABLE),
C                       J=2--FFF      (SUBCLASS OF VARIABLE),
C                       J=3--B        (BINARY INDICATOR),
C                       J=4--DD       (DATA SOURCE, MODEL NUMBER),
C                       J=5--V        (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY
C                                      1 LAYER)
C                       J=7--LTLTLTLT (TOP OF LAYER)
C                       J=8--T        (TRANSFORMATION)
C                       J=9--RR       (RUN TIME OFFSET, ALWAYS +
C                                      AND BACK IN TIME)
C                       J=10-OT       (TIME APPLICATION)
C                       J=11-OH       (TIME PERIOD IN HOURS)
C                       J=12-TAU      (PROJECTION IN HOURS)
C                       J=13-I        (INTERPOLATION TYPE)
C                       J=14-S        (SMOOTHING INDICATOR)
C                       J=15-G        (GRID INDICATOR)
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                       100 = THE TWO GRIDS NEEDED ARE NOT THE SAME SIZE
C                       101 = GRID SIZE IS TOO BIG FOR AN ARRAY, WHOSE 
C                             DIMENSION IS ND5.
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE
C                             TPCP24.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND2X3). (INTERNAL)
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
C                 ISO = 1 FOR ISOBARIC, 2 FOR CONSTANT PRESSURE 
C                       SURFACE,3 FOR SIGMA SURFACE(INTERNAL)
C               ISTAV = 0 SINCE THE DATA RETURNED ARE GRID DATA.
C                       (OUTPUT)
C            IWORK(J) = WORK ARRAY (J=1,ND2X3). (INTERNAL)
C                   J = LOOP CONTROL VARIABLE
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
C                   K = LOOP CONTROL VARIABLE
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64). (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE(,)
C                       THAT HAVE BEEN USED IN THIS RUN. (INPUT)
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
C                              MSTORE(,). LATER USED AS A WAY OF
C                              DETERMINING WHETHER TO KEEP THIS
C                              VARIABLE.
C           MDPARS(J) = SEE IDPARS(J). MDPARS IS USED WHEN
C                       SUBROUTINES PRSID1 AND NONCNVP ARE NEEDED
C                       (J=1,15). (INPUT)
C             MDX,MDY = DIMENSIONS OF GRID RETURNED FOR EITHER THE
C                       6-HR TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 FROM CALLING GFETCH WHEN THERE IS NO
C                       PRIMARY MISSING VALUE. (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 FROM CALLING GFETCH WHEN THERE IS NO 
C                       SECONDARY MISSING VALUE.  (INTERNAL)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C             MLX,MLY = DIMENSIONS OF GRID RETURNED FOR EITHER THE 
C                       6-HR TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C             MLX,MLY = DIMENSIONS OF GRID RETURNED FOR EITHER THE
C                       6-HR TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C             MNX,MNY = DIMENSIONS OF GRID RETURNED FOR EITHER THE
C                       6-HR TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C             MPX,MPY = DIMENSIONS OF GRID RETURNED FOR EITHER THE 
C                       6-HR TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C             MRX,MRY = DIMENSIONS OF GRID RETURNED FOR EITHER THE 
C                       6-HR TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C             MSX,MSY = DIMENSIONS OF GRID RETURNED FOR EITHER THE
C                       3-HR TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C             MTP3(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD1() (J=1,4). (INTERNAL)
C             MTP6(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD2() (J=1,4). (INTERNAL)
C            MTP12(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD3() (J=1,4). (INTERNAL)
C             MUX,MUY = DIMENSIONS OF GRID RETURNED FOR EITHER THE
C                       6-HR TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C             MVX,MVY = DIMENSIONS OF GRID RETURNED FOR EITHER THE
C                       12-HR TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C             MWX,MWY = DIMENSIONS OF GRID RETURNED FOR EITHER THE
C                       6-HR TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C             MXX,MXY = DIMENSIONS OF GRID RETURNED FOR EITHER THE
C                       12-HR TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C             MYX,MYY = DIMENSIONS OF GRID RETURNED FOR EITHER THE
C                       12-HR TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C             MZX,MZY = DIMENSIONS OF GRID RETURNED FOR EITHER THE
C                       12-HR TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C             MAX,MAY = DIMENSIONS OF GRID RETURNED FOR EITHER THE
C                       12-HR TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C             MBX,MBY = DIMENSIONS OF GRID RETURNED FOR EITHER THE
C                       12-HR TOTAL,CONVECTIVE, OR NON-CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE. (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF
C                       THE GRID IS NOT KNOWN BEFORE VARIABLES 
C                       ARE FETCHED.  (INPUT)
C                 ND5 = DIMENSION OF IPACK(), IWORK() AND FDTP().
C                       (INPUT)
C                 ND7 = DIMENSION OF IS0(),IS1(),IS2(), AND IS4().
C                       NOT ALL LOCATIONS ARE USED. (INPUT)
C                 ND9 = THE SECOND DIMENSION OF LSTORE(,). (INPUT)
C                ND10 = DIMENSION OF CORE(). (INPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN
C                       BE DEALT WITH ON THIS RUN.  LAST DIMENSION 
C                       OF NGRIDC(,). (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING COUNT FROM THE BEGINNING OF
C                       THE PROGRAM.  THIS COUNT IS MAINTAINED IN
C                       CASE THE USER NEEDS IT(DIAGNOSTICS, ETC.).
C                       (OUTPUT)
C         NGRIDC(L,M) = HOLDS THE GRID CHARACTERISTICS (L=1,6) FOR
C                       EACH GRID COMBINATION (M=1,NGRID).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=
C                            POLAR STEREOGRAPHIC).
C                       L=2--GRID LENGTH IN METERS.
C                       L=3--LATITUDE AT WHICH THE GRID LENGTH IS
C                            CORRECT *1000.
C                       L=4--GRID ORIENTATION IN DEGREES * 1000.
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *1000.
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES
C                            *1000.
C               NPACK = 2 FOR TDL GRIB PACKED DATA; 1 FOR NOT PACKED
C                       THIS IS RETURNED FROM GFETCH. (INTERNAL)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID. (OUTPUT)
C              NSLABD = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 3-HR PRECIP. (INTERNAL)
C              NSLABL = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 6-HR PRECIP. (INTERNAL)
C              NSLABM = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 6-HR PRECIP. (INTERNAL)
C              NSLABN = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 12-HR PRECIP. (INTERNAL)
C              NSLABP = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 3-HR PRECIP. (INTERNAL)
C              NSLABR = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 6-HR PRECIP. (INTERNAL)
C              NSLABS = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 3-HR PRECIP. (INTERNAL)
C              NSLABT = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 3-HR PRECIP. (INTERNAL)
C              NSLABU = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 6-HR PRECIP. (INTERNAL)
C              NSLABV = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 12-HR PRECIP. (INTERNAL)
C              NSLABW = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 12-HR PRECIP. (INTERNAL)
C              NSLABX = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 12-HR PRECIP. (INTERNAL)
C              NSLABY = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 12-HR PRECIP. (INTERNAL)
C              NSLABZ = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 6-HR PRECIP. (INTERNAL)
C              NSLABA = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 6-HR PRECIP. (INTERNAL)
C              NSLABB = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 6-HR PRECIP. (INTERNAL)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE,
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS 
C                       STORED IN LSTORE(9,). (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA().  THIS 
C                       IS RETURNED FROM GFETCH (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C     NON SYSTEM SUBROUTINES USED
C        GFETCH,PRSID1,NONCNVP
C
      IMPLICIT NONE
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER MTP3(4),MTP6(4),MTP12(4),ICCCFFF(6),MDPARS(15)
      INTEGER I,IER,ISTAV,J,K,KFILDO,KFIL10,L3264B,LITEMS,
     1        MDX,MDY,MISSP,MISSS,MISTOT,
     2        MLX,MLY,MMX,MMY,MNX,MNY,MPX,MPY,MRX,MRY,MSX,MSY,
     3        MTX,MTY,MUX,MUY,MVX,MVY,MWX,MWY,MXX,MXY,
     4        MYX,MYY,MZX,MZY,MAX,MAY,MBX,MBY,
     5        NBLOCK,ND2X3,ND5,ND7,ND9,ND10,
     6        ND11,NDATE,NFETCH,NPACK,
     7        NSLAB,NSLABD,NSLABL,NSLABM,NSLABN,NSLABP,NSLABR,
     8        NSLABS,NSLABT,NSLABU,NSLABV,NSLABW,NSLABX,NSLABY,
     9        NSLABZ,NSLABA,NSLABB,NTIMES,NWORDS
C
      REAL FDTP(ND5)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),
     1     FD5(ND2X3),FD6(ND2X3)
      REAL CORE(ND10)
C
      DATA ICCCFFF/003205,
     1             003210,
     2             003220,
     3             003235,
     4             003240,
     5             003250/
C       ABOVE IDS ARE IN ORDER:
C        3-HR TOTAL
C        6-HR TOTAL
C       12-HR TOTAL  
C        3-HR CONVECTIVE
C        6-HR CONVECTIVE
C       12-HR CONVECTIVE
C
      IER=0
      ISTAV=0
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C
      IF(IDPARS(1).NE.003.OR.(IDPARS(2).NE.226.AND.
     1                        IDPARS(2).NE.256))THEN
         WRITE(KFILDO,101)(JD(J),J=1,4)
 101     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1           '24-HR TOTAL OR CONVECTIVE ',
     2           'PRECIP AMT.',
     3          /'     VARIABLE ',I9.9,2I10.9,I4.3,
     4           ' NOT COMPUTED IN TPCP24.')
         IER=103
         GOTO 800
      END IF
C
C        CHECK IF PROJECTION IS LESS THAN OR EQUAL TO 24 HOURS.
C
      IF(IDPARS(12).LT.24)THEN
        WRITE(KFILDO,115)IDPARS(12),(JD(J),J=1,4)
 115    FORMAT(/' ****PROJECTION ENTERED IN THE CONTROL FILE =',I5,
     1          ' IS LESS THAN 24.',
     2         /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3          ' NOT COMPUTED IN TPCP24.')
        IER=187
        GOTO 800
      END IF
C
C***********************************************************************
C
C        THIS SECTION IS TO ADD 3-HR AMOUNTS TO GET A 24-HR AMOUNT.
C
C***********************************************************************
C
C        READ THE FIRST 3-HR PRECIP AMT.
C
      MTP3(2)=IDPARS(7)
      MTP3(3)=IDPARS(9)*1000000+IDPARS(12)
      MTP3(4)=0
C
      IF(IDPARS(2).EQ.256)THEN
        MTP3(1)=ICCCFFF(4)*1000+IDPARS(4)
      ELSEIF(IDPARS(2).EQ.226)THEN
        MTP3(1)=ICCCFFF(1)*1000+IDPARS(4)
      END IF
C
C        IF THE PRECIP WANTED IS TOTAL OR CONVECTIVE, THEN 
C        READ FIRST 3-HR TOTAL OR CONVECTIVE PRECIP AMT;
C        IF A 3-HR IS READ, THEN TRY TO READ MORE 3-HR 
C        PROJECTIONS OR MORE 6-HR PROJECTIONS.
C
      CALL GFETCH(KFILDO,KFIL10,MTP3,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDTP,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
 110  IF(IER.NE.0)GOTO 200
C        IF THE 3-HR AMOUNT AT PROJECTION IDPARS(12) WASN'T
C        FOUND, TRY ANOTHER TACK AT 200.
C
      MDX=IS2(3)
      MDY=IS2(4)
      NSLABT=NSLAB
C
C        READ THE SECOND 3-HR PRECIP AMT, THE ONE AT
C        IDPARS(12)-3.
C
      MTP3(3)=MTP3(3)-3
C
      CALL GFETCH(KFILDO,KFIL10,MTP3,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
 130  IF(IER.NE.0)GOTO 145
C        IF THE 3-HR AMOUNT AT PROJECTION IDPARS(12)-3 WASN'T
C        FOUND, THE ELSE SECTION AT 145 WILL TRY ANOTHER TACK.
C
      MNX=IS2(3)
      MNY=IS2(4)
      NSLABP=NSLAB
C
C        COMPARE THE GRID CHARACTERISTICS.
C
      IF(MNX.NE.MDX.OR.MNY.NE.MDY)THEN
C          THE GRID CHARACTERISTICS ARE NOT THE SAME
        WRITE(KFILDO,135)(MTP3(J),J=1,4),(NGRIDC(J,NSLABT),J=1,6),
     1                    MDX,MDY,
     2                   (MTP3(J),J=1,4),(NGRIDC(J,NSLABP),J=1,6),
     3                    MNX,MNY
 135    FORMAT(/,' ****THE GRIDS NEEDED IN TPCP24 HAVE DIFFERENT',
     1           ' CHARACTERISTICS AT 135.  PREDICTOR NOT COMPUTED.',
     2           ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3           (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
      END IF
C
C        CHECK IF THE NSLABS ARE EQUAL.
C
      IF(NSLABP.NE.NSLABT)THEN
        WRITE(KFILDO,140)NSLABP,NSLABT,(JD(J),J=1,4)
 140    FORMAT(/' ****THE GRID CHARACTERISTICS OF THE FIRST TWO',
     1          '3-HR PRECIPITATION GRIDS ARE DIFFERENT.',I3,2X,I3,
     2         /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3          ' NOT COMPUTED IN TPCP24.')
        IER=100
        GOTO 800
      END IF
C
  145 IF(IER.EQ.0)THEN
C
        DO J=1,MNX*MNY
          FDTP(J)=FDTP(J)+FD1(J)
        END DO
C
C         START ACCUMULATING 3-HR PRECIP AMOUNTS  THE 6 HOURS AT
C         IDPARS(12) AND IDPARS(12)-3 HAVE BEEN ACCUMULATED.  NEED
C         TO DO THIS FOR 6 MORE 3-HOUR PERIODS.
C
        DO K=1,6
          MTP3(3)=MTP3(3)-3
C
          CALL GFETCH(KFILDO,KFIL10,MTP3,7777,LSTORE,ND9,LITEMS,
     1                IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2                NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3                NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
          IF(MISSP.NE.0)MISTOT=MISTOT+1
C
          IF(IER.NE.0)GOTO 800
C
          NSLABD=NSLAB
          MPX=IS2(3)
          MPY=IS2(4)
C
C            COMPARE THE GRID CHARACTERISTICS.
C   
          IF(MPX.NE.MNX.OR.MPY.NE.MNY)THEN
C              THE GRID CHARACTERISTICS ARE NOT THE SAME
            WRITE(KFILDO,150)(MTP3(J),J=1,4),(NGRIDC(J,NSLABP),J=1,6),
     1                        MNX,MNY,
     2                       (MTP3(J),J=1,4),(NGRIDC(J,NSLABD),J=1,6),
     3                        MPX,MPY
 150        FORMAT(/' ****THE GRIDS NEEDED IN TPCP24 HAVE DIFFERENT',
     1              ' CHARACTERISTICS AT 150.  PREDICTOR NOT COMPUTED.',
     2              ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3              (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
            IER=100
            GOTO 800
          END IF
C
C            CHECK IF THE NSLABS ARE EQUAL.
C
          IF(NSLABD.NE.NSLABP)THEN
            WRITE(KFILDO,155)NSLABD,NSLABP,(JD(J),J=1,4)
 155        FORMAT(/' ****THE GRID CHARACTERISTICS OF THE 3-HR',
     1              ' PRECIPITATION ARE DIFFERENT',I3,2X,I3,
     2             /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3              ' NOT COMPUTED IN TPCP24.')
            IER=100
            GOTO 800
          END IF
C
          DO J=1,MNX*MNY
            FDTP(J)=FDTP(J)+FD1(J)
            IF(FDTP(J).LT.0.)FDTP(J)=0.
          END DO
C
        END DO
C
C***********************************************************************
C
C          ALL 8 3-HR AMOUNTS FOUND.  GO TO END.
C
C***********************************************************************
C
        IF(IER.EQ.0)GOTO 900
      ELSE
C
C***********************************************************************
C
C          TRY TO SUM 6-HR AMOUNTS.  NOTE THAT ONE 3-HR AMOUNT, THAT
C          AT IDPARS(12) HAS ALREADY BEEN PUT INTO FDTP( ).
C
C***********************************************************************
C
C          READ THE 6-HR PRECIP AMOUNT AT IDPARS(12)-3.
C
        MTP6(2)=IDPARS(7)
        MTP6(3)=IDPARS(9)*1000000+IDPARS(12)-3
        MTP6(4)=0
C
        IF(IDPARS(2).EQ.256)THEN
          MTP6(1)=ICCCFFF(5)*1000+IDPARS(4)
        ELSEIF(IDPARS(2).EQ.226)THEN
          MTP6(1)=ICCCFFF(2)*1000+IDPARS(4)
        END IF
C
        CALL GFETCH(KFILDO,KFIL10,MTP6,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3              NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
        IF(MISSP.NE.0)MISTOT=MISTOT+1
 160    IF(IER.NE.0)GOTO 800
C
        NSLABL=NSLAB
        MLX=IS2(3)
        MLY=IS2(4)
C
C        COMPARE THE GRID CHARACTERISTICS.
C
      IF(MLX.NE.MDX.OR.MLY.NE.MDY)THEN
C          THE GRID CHARACTERISTICS ARE NOT THE SAME
        WRITE(KFILDO,161)(MTP3(J),J=1,4),(NGRIDC(J,NSLABT),J=1,6),
     1                    MDX,MDY,
     2                   (MTP3(J),J=1,4),(NGRIDC(J,NSLABL),J=1,6),
     3                    MLX,MLY
 161    FORMAT(/,' ****THE GRIDS NEEDED IN TPCP24 HAVE DIFFERENT',
     1           ' CHARACTERISTICS AT 161.  PREDICTOR NOT COMPUTED.',
     2           ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3           (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
C
      END IF
C
C        CHECK IF THE NSLABS ARE EQUAL.
C
      IF(NSLABL.NE.NSLABT)THEN
        WRITE(KFILDO,162)NSLABL,NSLABT,(JD(J),J=1,4)
 162    FORMAT(/' ****THE GRID CHARACTERISTICS OF THE FIRST ',
     1          '3-HR AND FIRST 6-HR PRECIPITATION GRIDS ARE',
     2          ' DIFFERENT.',I3,2X,I3,
     3         /'     VARIABLE ',I9.9,2I10.9,I4.3,
     4          ' NOT COMPUTED IN TPCP24.')
        IER=100
        GOTO 800
      END IF
C
        DO J=1,MLX*MLY
          FDTP(J)=FDTP(J)+FD2(J)
        END DO
C
        DO  178 I=1,3
          MTP6(3)=MTP6(3)-6
C
          CALL GFETCH(KFILDO,KFIL10,MTP6,7777,LSTORE,ND9,LITEMS,
     1                IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2                NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3                NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
          IF(MISSP.NE.0)MISTOT=MISTOT+1
 165      IF(IER.NE.0)GOTO 800
C
          NSLABR=NSLAB
          MRX=IS2(3)
          MRY=IS2(4)
C      
C            CHECK IF GRID CHARACTERISTICS ARE THE SAME.
C
          IF(MLX.NE.MRX.OR.MLY.NE.MRY)THEN
C              THE GRID CHARACTERISTICS ARE NOT HE SAME.
            WRITE(KFILDO,170)(MTP6(J),J=1,4),(NGRIDC(J,NSLABR),J=1,6),
     1                        MRX,MRY,
     2                       (MTP6(J),J=1,4),(NGRIDC(J,NSLABL),J=1,6),
     3                         MLX,MLY
 170        FORMAT(/' ****THE GRIDS NEEDED IN TPCP24 HAVE DIFFERENT',
     1              ' CHARACTERISTICS AT 170.  PREDICTOR NOT',
     2             /' COMPUTED.  VALUES FROM NGRIDC(,) AND MX,MY.',
     3              (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
            IER=100
            GOTO 800
          END IF
C
C            CHECK THE NSLABS ARE EQUAL.
C           
          IF(NSLABL.NE.NSLABR)THEN
            WRITE(KFILDO,175)NSLABL,NSLABR,(JD(J),J=1,4)
 175        FORMAT(/' ****THE GRID CHARACTERISTICS OF THE ',
     1               '6-HR PRECIP ARE DIFFERENT AT 175.',I3,2X,I3,
     2             /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3              ' NOT COMPUTED IN TPCP24.')
            IER=100
            GOTO 800
          END IF
C
          DO J=1,MLX*MLY
            FDTP(J)=FDTP(J)+FD2(J)
          END DO
C
 178    CONTINUE
C
C          READ LAST 3-HR PRECIP.  THIS IS FOR THE AVN MODEL, IN
C          WHICH A 3-HR PRECIP AMOUNT IS NEEDED AT T-24 TO SUBTRACT 
C          FROM THE PREVIOUS ACCUMULATED AMOUNT SO THAT THE TIME IN
C          HOURS EQUAL 24.  MTP3( ) IS STILL INTACT, EXCEPT FOR
C          PROJECTION IN MTP3(3).
C
        MTP3(3)=MTP6(3)-3
C
        CALL GFETCH(KFILDO,KFIL10,MTP3,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
        IF(MISSP.NE.0)MISTOT=MISTOT+1
 180    IF(IER.NE.0)GOTO 800
C
        NSLABS=NSLAB
        MSX=IS2(3)
        MSY=IS2(4)
C
C          CHECK IF THE GRID CHARACTERISTICS ARE THE SAME.
C     
        IF(MRX.NE.MSX.OR.MRY.NE.MSY)THEN
C          THE GRID CHARACTERISTICS ARE NOT THE SAME
          WRITE(KFILDO,185)(MTP6(J),J=1,4),(NGRIDC(J,NSLABR),J=1,6),
     1                      MRX,MRY,
     2                     (MTP3(J),J=1,4),(NGRIDC(J,NSLABS),J=1,6),
     3                      MSX,MSY
 185      FORMAT(/' ****THE GRIDS NEEDED IN TPCP24 HAVE DIFFERENT',
     1            ' CHARACTERISTICS AT 185.  PREDICTOR NOT COMPUTED.',
     2            ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3            (5X,I9.9,I10.9,I10.9,I4.3,6I10,4X,2I5))
          IER=100
          GOTO 800
        END IF
C
C          CHECK IF NSLABS ARE EQUAL
C
        IF(NSLABR.NE.NSLABS)THEN
          WRITE(KFILDO,190)NSLABR,NSLABS,(JD(J),J=1,4)
 190      FORMAT(/' ****THE GRID CHARACTERISTICS OF THE 3 AND 6-HR',
     1            ' PRECIPITATION ARE DIFFERENT AT 190.',I3,2X,I3,
     3           /'     VARIABLE ',I9.9,2I10.9,I4.3,
     4            ' NOT COMPUTED IN TPCP24.')
          IER=100
          GOTO 800
        END IF
C
        DO J=1,MLX*MLY
          FDTP(J)=FDTP(J)-FD1(J)
          IF(FDTP(J).LT.0.)FDTP(J)=0.
        END DO
C
      END IF
C
C***********************************************************************
C
C        24-HR AMOUNT HAS BEEN FOUND WITH A COMBINATION OF 6-HR
C        AMOUNTS AND FIRST AND LAST 3-HR AMOUNT.  GO TO END.
C
C***********************************************************************
      GOTO 900
C
C***********************************************************************
C
C        ACCUMULATION DEALING WITH 3-HR AMOUNTS FAILED (ACTUALLY, ONLY
C        THE 3-HR AMOUNT AT IDPARS(12) WAS TRIED AND FAILED.  TRY TO 
C        ACCUMULATE WITH FOUR 6-HR AMOUNTS.
C
C***********************************************************************
C
C        READ THE 6-HR AMOUNT AT IDPARS(12).
C
 200  MTP6(2)=IDPARS(7)
      MTP6(3)=IDPARS(9)*1000000+IDPARS(12)
      MTP6(4)=0
C
      IF(IDPARS(2).EQ.256)THEN
        MTP6(1)=ICCCFFF(5)*1000+IDPARS(4)
      ELSEIF(IDPARS(2).EQ.226)THEN
        MTP6(1)=ICCCFFF(2)*1000+IDPARS(4)
      END IF
C     
C        READ FIRST 6-HR TOTAL OR CONVECTIVE AMOUNT.
C
      CALL GFETCH(KFILDO,KFIL10,MTP6,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDTP,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
 205  IF(IER.NE.0)GOTO 300
C        THE FIRST 6-HR AMOUNT IS NOT AVAILABLE; TRY 12-HR
C        AMONTS AT 300.
C
      NSLABN=NSLAB
      MWX=IS2(3)
      MWY=IS2(4)
C
C        READ THE SECOND 6-HR PRECIP AMT.
C
      MTP6(3)=IDPARS(9)*1000000+IDPARS(12)-6
C
      CALL GFETCH(KFILDO,KFIL10,MTP6,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
 210  IF(IER.NE.0)GO TO 250
C        THE 2ND 6-HR AMOUNT IS NOT AVAILABLE.  TRY A 12-HR AMOUNT.
C
      MMX=IS2(3)
      MMY=IS2(4)
      NSLABM=NSLAB
C
C        COMPARE THE GRID CHARACTERISTICS.
C 
      IF(MWX.NE.MMX.OR.MWY.NE.MMY)THEN
C        THE GRID CHARACTERISTICS ARE THE SAME
        WRITE(KFILDO,215)(MTP6(J),J=1,4),(NGRIDC(J,NSLABN),J=1,6),
     1                    MWX,MWY,
     2                   (MTP6(J),J=1,4),(NGRIDC(J,NSLABM),J=1,6),
     3                    MMX,MMY
 215    FORMAT(/' ****THE GRIDS NEEDED IN TPCP24 HAVE DIFFERENT',
     1          ' CHARACTERISTICS AT 215.  PREDICTOR NOT COMPUTED.',
     2          ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3          (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
      END IF
C
C        CHECK IF THE NSLABS ARE EQUAL.
C
      IF(NSLABN.NE.NSLABM)THEN
        WRITE(KFILDO,220)NSLABN,NSLABM,(JD(J),J=1,4)
 220    FORMAT(/,' ****THE GRID CHARACTERISTICS OF THE ',
     1          '3-HR PRECIPITATION ARE DIFFERENT AT 20.',I3,2X,I3,
     2           /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3            ' NOT COMPUTED IN TPCP24.')
        IER=100
        GOTO 800
      END IF
C
      DO J=1,MWX*MWY
        FDTP(J)=FDTP(J)+FD2(J)
      END DO
C     
C       READ THE OTHER TWO 6-HR PRECIP AMOUNTS.

      DO 248 K=1,2
        MTP6(3)=MTP6(3)-6
C
        CALL GFETCH(KFILDO,KFIL10,MTP6,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
        IF(MISSP.NE.0)MISTOT=MISTOT+1
 230    IF(IER.NE.0)GOTO 800
C
        NSLABU=NSLAB
        MUX=IS2(3)
        MUY=IS2(4)
C
C          COMPARE GRID CHARACTERISTICS.
C
        IF(MWX.NE.MUX.OR.MWY.NE.MUY)THEN
C            THE GRID CHARACTERISTICS ARE NOT THE SAME.
          WRITE(KFILDO,235)(MTP6(J),J=1,4),(NGRIDC(J,NSLABN),J=1,6),
     1                      MWX,MWY,
     2                     (MTP6(J),J=1,4),(NGRIDC(J,NSLABU),J=1,6),
     3                      MUX,MUY
 235      FORMAT(/' ****THE GRIDS NEEDED IN TPCP24 HAVE DIFFERENT',
     1            ' CHARACTERISTICS AT 235.  PREDICTOR NOT COMPUTED.',
     2            ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3            (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
          IER=100
          GOTO 800
        END IF
C
C          CHECK IF THE NSLABS ARE EQUAL.
C
        IF(NSLABU.NE.NSLABN)THEN
          WRITE(KFILDO,240)NSLABU,NSLABN,(JD(J),J=1,4)
 240      FORMAT(/' ****THE GRID CHARACTERISTICS OF THE ',
     1            '6-HR PRECIPITATION ARE DIFFERENT AT 240.',I3,2X,I3,
     2           /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3            ' NOT COMPUTED IN TPCP24.')
          IER=100
          GOTO 800
        END IF
C
        DO I=1,MWX*MWY
          FDTP(I)=FDTP(I)+FD2(I)
          IF(FDTP(I).LT.0.)FDTP(I)=0.
        END DO
C
 248  CONTINUE
C
C***********************************************************************
C
C        24-HR AMOUNT HAS BEEN FOUND BY SUMMING FOUR 6-HR AMOUNT.
C        GO TO END.
C
C***********************************************************************
C
      GOTO 900
C
C***********************************************************************
C
C        THE FIRST 6-HR AMOUNT IS IN FDTP( ), BUT THE 2ND 6-HR
C        AMOUNT WAS NOT AVAILABLE.  TRY TO ADD TWO 12-HR AMOUNTS
C        AND THEN SUBTRACT THE 6-HR AMOUNT AT IDPARS(12)-24.
C
C***********************************************************************
C
C        READ THE 12-HR AMOUNT AT IDPARS(12)-6.
C
 250  MTP12(2)=IDPARS(7)
      MTP12(3)=IDPARS(9)*1000000+IDPARS(12)-6
      MTP12(4)=0
C
      IF(IDPARS(2).EQ.256)THEN
        MTP12(1)=ICCCFFF(6)*1000+IDPARS(4)
      ELSEIF(IDPARS(2).EQ.226)THEN
        MTP12(1)=ICCCFFF(3)*1000+IDPARS(4)
      END IF
C
      CALL GFETCH(KFILDO,KFIL10,MTP12,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
 255  NSLABW=NSLAB
      MXX=IS2(3)
      MXY=IS2(4)
C
C          COMPARE IF THE GRID CHARACTERISTICS ARE THE SAME.
C
      IF(MXX.NE.MWX.OR.MXY.NE.MWY)THEN
C          THE GRID CHARACTERISTICS ARE NOT THE SAME
        WRITE(KFILDO,256)(MTP12(J),J=1,4),(NGRIDC(J,NSLABW),J=1,6),
     1                    MXX,MXY,
     2                   (MTP12(J),J=1,4),(NGRIDC(J,NSLABN),J=1,6),
     3                    MWX,MWY
 256    FORMAT(/' ****THE GRIDS NEEDED IN TPCP24 HAVE DIFFERENT',
     1          ' CHARACTERISTICS AT 256.  PREDICTOR NOT COMPUTED.',
     2         /' VALUES FROM NGRIDC(,) AND MX,MY.',
     3          (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
C
      END IF
C
C        CHECK IF THE NSLABS ARE EQUAL.
C
      IF(NSLABW.NE.NSLABN)THEN
        WRITE(KFILDO,257)NSLABW,NSLABN,(JD(J),J=1,4)
 257    FORMAT(/' ****THE GRIDS OF THE FIRST 6-HR AND THE FIRST',
     1          '12-HR PRECIPITATION ARE NOT THE SAME AT 257.',
     2           I3,2X,I3,
     3         /'     VARIABLE ',I9.9,2I10.9,I4.3,
     4          ' NOT COMPUTED IN TPCP24.')
        IER=100
        GOTO 800
C
      END IF
C
      DO I=1,MXX*MXY
        FDTP(I)=FDTP(I)+FD3(I)
      END DO
C
      IF(IER.NE.0)GOTO 800
C
C       READ 12-HR PRECIP FOR IDPARS(12)-18.
C
      MTP12(3)=MTP12(3)-12
C
      CALL GFETCH(KFILDO,KFIL10,MTP12,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
 260  IF(IER.NE.0)GOTO 800
C
      NSLABY=NSLAB
      MYX=IS2(3)
      MYY=IS2(4)
C
C        COMPARE IF THE GRID CHARACTERISTICS ARE THE SAME.
C
      IF(MXX.NE.MYX.OR.MYY.NE.MXY)THEN
C          THE GRID CHARACTERISTICS ARE NOT THE SAME
        WRITE(KFILDO,270)(MTP12(J),J=1,4),(NGRIDC(J,NSLABW),J=1,6),
     1                    MXX,MXY,
     2                   (MTP12(J),J=1,4),(NGRIDC(J,NSLABY),J=1,6),
     3                    MYX,MYY
 270    FORMAT(/' ****THE GRIDS NEEDED IN TPCP24 HAVE DIFFERENT',
     1          ' CHARACTERISTICS AT 270.  PREDICTOR NOT COMPUTED.',
     2         /' VALUES FROM NGRIDC(,) AND MX,MY.',
     3          (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
      END IF
C
C        CHECK IF THE NSLABS ARE EQUAL.
C
      IF(NSLABW.NE.NSLABY)THEN
        WRITE(KFILDO,280)NSLABW,NSLABY,(JD(J),J=1,4)
 280    FORMAT(/' ****THE GRIDS OF THE 12-HR PRECIPITATION ',
     1          'ARE NOT THE SAME AT 280.',I3,2X,I3,
     2         /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3          ' NOT COMPUTED IN TPCP24.')
        IER=100
        GOTO 800
      END IF
C
      DO I=1,MXX*MXY
        FDTP(I)=FDTP(I)+FD3(I)
      END DO
C
C        READ 6-HR PRECIP FOR IDPARS(12)-24 AND SUBTRACT.  THIS IS
C        DONE FOR THE ETA MODEL BECAUSE THE TOTAL HOURS SUMMED
C        ACCOUNT FOR 30 HOURS AND A 24-HR AMOUNT IS NEEDED.  THUS, A
C        6-HR SUBTRACTION.
C
      MTP6(2)=IDPARS(7)
      MTP6(3)=IDPARS(9)*1000000+IDPARS(12)-24
      MTP6(4)=0
C
      IF(IDPARS(2).EQ.256)THEN
        MTP6(1)=ICCCFFF(5)*1000+IDPARS(4)
      ELSEIF(IDPARS(2).EQ.226)THEN
        MTP6(1)=ICCCFFF(2)*1000+IDPARS(4)
      END IF
C
      CALL GFETCH(KFILDO,KFIL10,MTP6,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
 285  IF(IER.NE.0)GOTO 800
C
      NSLABZ=NSLAB
      MZX=IS2(3)
      MZY=IS2(4)
C       
C        COMPARE GRID CHARACTERISTICS.
C
      IF(MZX.NE.MYX.OR.MZY.NE.MYY)THEN
C         THE GRID CHARACTERISTICS ARE NOT THE SAME.
        WRITE(KFILDO,290)(MTP12(J),J=1,4),(NGRIDC(J,NSLABY),J=1,6),
     1                  MYX,MYY,
     2                 (MTP12(J),J=1,4),(NGRIDC(J,NSLABZ),J=1,6),
     3                  MZX,MZY
 290    FORMAT(/' ****THE GRIDS NEEDED IN TPCP24 HAVE DIFFERENT',
     1          ' CHARACTERISTICS AT 290.  PREDICTOR NOT COMPUTED.',
     2          ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3          (5X,I9.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
      END IF
C
C        CHECK THE NSLABS ARE EQUAL.
C
      IF(NSLABY.NE.NSLABZ)THEN
        WRITE(KFILDO,295)NSLABY,NSLABZ,(JD(J),J=1,4)
 295    FORMAT(/' ****THE GRIDS OF THE 12-HR PRECIPITATION',
     1          ' ARE NOT EQUAL AT 295.',I3,2X,I3,
     2         /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3          ' NOT COMPUTED IN TPCP24.')
        IER=100
        GOTO 800
      END IF
C
      DO I=1,MYX*MYY
        FDTP(I)=FDTP(I)-FD2(I)
        IF(FDTP(I).LT.0.)FDTP(I)=0.
      END DO
C
C***********************************************************************
C
C        24-HR AMOUNT HAS BEEN FOUND BY SUMMING 3-HR AND 6-HR AMOUNTS.
C        GO TO END.
C
C***********************************************************************
C
      GOTO 900
C
C***********************************************************************
C
C        ACCUMULATION BY SUMMING 3-HR AND 6-HR AMOUNTS FAILED.
C        TRY TO ACCUMULATE WITH TWO 12-HR AMOUNTS.
C
C***********************************************************************
C
C        READ THE FIRST 12-HR PROJECTION.
C
 300  MTP12(2)=IDPARS(7)
      MTP12(3)=IDPARS(9)*1000000+IDPARS(12)
      MTP12(4)=0
C
      IF(IDPARS(2).EQ.256)THEN
        MTP12(1)=ICCCFFF(6)*1000+IDPARS(4)
      ELSEIF(IDPARS(2).EQ.226)THEN
        MTP12(1)=ICCCFFF(3)*1000+IDPARS(4)
      END IF
C
      CALL GFETCH(KFILDO,KFIL10,MTP12,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDTP,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1       
C
 310  IF(IER.NE.0)GOTO 800
C
C        READ THE NEXT 12-HR PROJECTION.  THIS PART IS
C        FOR THE MRF OR ETA.
C
      NSLABV=NSLAB
      MVX=IS2(3)
      MVY=IS2(4)
C
      MTP12(3)=MTP12(3)-12
C
      CALL GFETCH(KFILDO,KFIL10,MTP12,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
C        IF THERE IS AN ERROR FETCHING THE SECOND 12-HR 
C        PRECIP AMOUNT, TRY FETCHING THE TWO 6-HR
C        PRECIP AMOUNTS.
C
 315  IF(IER.NE.0)GOTO 350
C
      NSLABX=NSLAB
      MTX=IS2(3)
      MTY=IS2(4)
C
C        COMPARE IF THE GRID CHARACTERISTICS ARE THE SAME
C
      IF(MTX.NE.MVX.OR.MTY.NE.MVY)THEN
C        THE GRID CHARACTERISTICS ARE THE SAME
        WRITE(KFILDO,320)(MTP12(J),J=1,4),(NGRIDC(J,NSLABV),
     1                    J=1,6),MVX,MVY,
     2                    (MTP12(J),J=1,4),(NGRIDC(J,NSLABX),
     3                    J=1,6),MTX,MTY
 320    FORMAT(/,' ****THE GRIDS NEEDED IN TPCP24 HAVE ',
     1          ' DIFFERENT CHARACTERISTICS AT 320.  PREDICTOR NOT',
     2          ' COMPUTED.  VALUES FROM NGRIDC(,) AND MX,MY.'/
     3          (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
      END IF
C
C       CHECK THE NSLABS ARE EQUAL.
      IF(NSLABX.NE.NSLABV)THEN
        WRITE(KFILDO,325)NSLABX,NSLABV,(JD(J),J=1,4)
 325    FORMAT(/' ****THE GRIDS OF THE 12-HR PRECIPITATION',
     1          ' ARE DIFFERENT AT 325.',I3,2X,I3,
     2         /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3          ' NOT COMPUTED IN TPCP24.')
        IER=100
        GOTO 800
      END IF
C
      DO I=1,MTX*MTY
        FDTP(I)=FDTP(I)+FD3(I)
        IF(FDTP(I).LT.0.)FDTP(I)=0.
      END DO
C
C***********************************************************************
C
C        24-HR AMOUNT HAS BEEN FOUND BY SUMMING TWO 6-HR AMOUNTS.
C        GO TO END.
C
C***********************************************************************
C
      GOTO 900
C
C***********************************************************************
C
C        THIS IS THE GFS PIECE.  TO GET HERE, THE FIRST 12-HR PRECIP
C        MUST BE FOUND, BUT THE SECOND MUST BE MISSING.
C
C        THE 12-HR PRECIP AT PROJECTION IDPARS(12) HAS ALREADY BEEN
C        STORED IN FDTP( ).  WE NEED TO FETCH TWO 6-HR AMOUNTS.
C
C***********************************************************************
C
C        FETCH THE 6-HR PRECIP AT PROJECTION IDPARS(12)-12.
C
 350  MTP12(2)=IDPARS(7)
      MTP12(3)=IDPARS(9)*1000000+IDPARS(12)-12
      MTP12(4)=0
C
      IF(IDPARS(2).EQ.226)THEN
        MTP12(1)=ICCCFFF(2)*1000+IDPARS(4)
      ELSEIF(IDPARS(2).EQ.256)THEN
        MTP12(1)=ICCCFFF(5)*1000+IDPARS(4)
      END IF
C
      CALL GFETCH(KFILDO,KFIL10,MTP12,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
 355  IF(IER.NE.0)GO TO 800
D
      NSLABA=NSLAB
      MAX=IS2(3)
      MAY=IS2(4)
C
C        COMPARE GRID CHARACTERISTICS.
C
      IF(MVX.NE.MAX.OR.MVY.NE.MAY)THEN
        WRITE(KFILDO,360)(MTP12(J),J=1,4),(NGRIDC(J,NSLABV),J=1,6),
     1                    MVX,MVY,
     2                   (MTP12(J),J=1,4),(NGRIDC(J,NSLABA),J=1,6),
     3                    MAX,MAY
 360    FORMAT(/' ****THE GRIDS NEEDED IN TPCP24 HAVE DIFFERENT',
     1          ' CHARACTERISTICS AT 360.  PREDICTOR NOT COMPUTED.',
     2          ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3          (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
D
      END IF
C
C        CHECK IF THE NSLABS ARE EQUAL.
C
      IF(NSLABV.NE.NSLABA)THEN
         WRITE(KFILDO,365)NSLABV,NSLABA,(JD(J),J=1,4)
 365     FORMAT(/' ****THE GRIDS OF THE 12- AND 6-HR ',
     1           'PRECIPITATION ARE NOT THE SAME AT 365.',I3,2X,I3,
     2          /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3           ' NOT COMPUTED IN TPCP24.')
         IER=100
         GOTO 800
      END IF
C
      DO J=1,MVX*MVY
         FDTP(J)=FDTP(J)+FD3(J)
      END DO
C
C        FETCH THE 6-HR PRECIP AT T-18.
C
      MTP12(3)=IDPARS(9)*1000000+IDPARS(12)-18
C
      CALL GFETCH(KFILDO,KFIL10,MTP12,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
 370  IF(IER.NE.0)GOTO 800
D
      NSLABB=NSLAB
      MBX=IS2(3)
      MBY=IS2(4)
C
C        COMPARE IF GRID CHARACTERISTICS ARE THE SAME
C
      IF(MBX.NE.MAX.OR.MBY.NE.MAY)THEN
C        THE GRID CHARACTERISTICS ARE NOT THE SAME
         WRITE(KFILDO,375)(MTP12(J),J=1,4),(NGRIDC(J,NSLABA),J=1,6),
     1                     MAX,MAY,
     2                    (MTP12(J),J=1,4),(NGRIDC(J,NSLABB),J=1,6),
     3                     MBX,MBY
 375     FORMAT(/,' ****THE GRIDS NEEDED IN TPCP24 HAVE DIFFERENT',
     1           ' CHARACTERISTICS AT 375.  PREDICTOR NOT COMPUTED.',
     2           ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3           (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
         IER=100
         GOTO 800
D
      END IF
C
C        CHECK THE NSLABS ARE EQUAL.
C
      IF(NSLABB.NE.NSLABA)THEN
         WRITE(KFILDO,380)NSLABA,NSLABB,(JD(J),J=1,4)
 380     FORMAT(/' ****THE GRID CHARACTERISTICS OF THE ',
     1           '6-HR PRECIPITATION ARE DIFFERENT AT 380.',I3,2X,I3,
     2          /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3           ' NOT COMPUTED IN TPCP24.')
         IER=100
         GOTO 800
      END IF
C
      DO I=1,MBX*MBY
         FDTP(I)=FDTP(I)+FD3(I)
         IF(FDTP(I).LT.0.)FDTP(I)=0.
      END DO
C
C***********************************************************************
C
C        24-HR AMOUNT HAS BEEN FOUND BY SUMMING THE FIRST 12-HR AMOUNT
C        AND TWO 6-HR AMOUNTS.  GO TO END.
C
C***********************************************************************
C
      GOTO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C 
 800  DO 801 J=1,ND2X3
        FDTP(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END
