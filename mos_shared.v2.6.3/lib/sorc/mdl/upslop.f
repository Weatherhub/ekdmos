      SUBROUTINE UPSLOP(KFILDO,KFIL10,ID,IDPARS,JD,NDATE,
     1                  KFILRA,RACESS,NUMRA,
     2                  CCALL,NAME,SDATA,STALAT,STALON,DIR,ND1,NSTA,
     3                  NGRIDC,NGRID,ND11,NSLABT,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     5                  IS0,IS1,IS2,IS4,ND7,
     6                  FDU,FDV,FDUS,FDVS,XMAPFT,TM,TDIR,ND2X3,
     7                  ISTAV,L3264B,MISTOT,IER)
C
C        JULY      2002   GLAHN   TDL   MOS-2000
C        FEBRUARY  2003   WEISS   MODIFIED FORMAT STATEMENTS
C                                 TO CONFORM TO FORTRAN 90
C                                 STANDARDS ON THE IBM-SP
C        MAY       2003   GLAHN   REARRANGED DIMENSION STATEMENTS
C        JUNE      2003   GLAHN   CHANGED SETTING ND5 VALUES OF DATA( )
C                                 TO 9999 TO ND1 VALUES OF SDATA( )
C                                 FOR ERROR RETURN; COUPLE OF CHANGES
C                                 REGARDING ISTAV TO ALWAYS RETURN 1
C        JUNE      2003   GLAHN   ADDED NAME TO CALL
C        AUGUST    2003   GLAHN   ADDED CHECK FOR FFF/100 GE 3
C        SEPTEMBER 2003   GLAHN   ADDED CALL TO TIMGRD WHEN NECESSARY;
C                                 CHANGED ISDATA TO IWORK IN CALL TO
C                                 GFETCH FOR TM( ) AND TDIR( )
C        SEPTEMBER 2003   GLAHN   CHANGED ISTAV TO ISTAVD IN CALLS TO
C                                 TIMGRD; ADDED MISTOT TO CALL TO
C                                 TIMGRD; CHANGED IDS FOR STORING TM( )
C                                 AND TDIR( )
C        AUGUST    2004   GLAHN   INSERTED TIMPR AT ENTRY WITH /D;
C                                 REMOVED /D DIAGNOSTIC
C        SEPTEMBER 2005   GLAHN   MODIFIED TO SAVE NSLABT; CORRECTED
C                                 DIAGNOSTIC FORMAT 262  
C        JUNE      2015   ENGLE   UPDATED COMMENTS TO SHOW THAT X=4
C                                 REPRESENT NAM (AWIPS 151) NPS GRID.
C
C        PURPOSE 
C            TO COMPUTE THE UPSLOPE WIND FROM U AND V AT SOME LEVEL
C            AND A TERRAIN FIELD.  THE TERRAIN SLOPE MAGNITUDE AND
C            DIRECTION ARE RETRIEVED FROM INTERNAL STORAGE AT STATIONS,
C            IF THEY EXIST, AND ARE USED WITH THE U AND V WIND 
C            (INTERPOLATED TO STATIONS FROM THE U AND V GRIDS,
C            SMOOTHED IF DESIRED) TO COMPUTE THE UPSOPE WINDS.  IF
C            THE SLOPE MAGNITUDE AND DIRECTION ARE NOT IN INTERNAL
C            STORAGE, THEY ARE COMPUTED FROM THE TERRAIN GRID
C            OBTAINED FROM EXTERNAL RANDOM ACCESS STORAGE; THEY
C            ARE THEN STORED INTO INTERNAL STORAGE SO THEY WON'T
C            HAVE TO BE COMPUTED AGAIN.
C
C               CCCFFF = 005XYZ
C                  X = REPRESENTS THE MAP PROJECTION
C                      3 = LAMBERT
C                      4 = POLAR STEREOGRAPHIC (NAM AWIPS 151)
C                      5 = POLAR STEREOGRAPHIC
C                      7 = MERCATOR
C                  Y = REPRESENTS THE MESH LENGTH IN BEDIENTS
C                      0 = 1/4 BEDIENT
C                      1 = 1/8    "
C                      2 = 1/16   "
C                      3 = 1/32   "
C                      4 = 1/64   " ABOUT 5.9 KM AT 60 N
C                      5 = 1/128  "
C                      6 = 1/256  "
C                  Z = REPRESENTS TYPE OF WINDS AND THEIR SMOOTHING
C                      0 = ISOBARIC, NO SMOOTHING
C                      1 =    "    ,  5-PT  "
C                      2 =    "    ,  9-PT  "
C                      3 =    "    , 25 PT  "
C                      4 =    "    , 25 PT  " TWICE
C                      5 = CONSTANT HEIGHT, NO SMOOTHING
C                      6 =    "            , 5-PT   "
C                      7 =    "            , 9-PT  "
C                      8 =    "            , 25 PT  "
C                      9 =    "            , 25 PT  " TWICE
C            THE VALUES OF Y ARE APPROXIMATE, AND CAN APPLY TO ANY
C            ONE OF THE THREE MAP PROJECTIONS REPRESENTED BY X
C            ACCORDING TO THE MOS CONVENTIONS.
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                     (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C               ID(J) = THE VARIABLE WANTED (J=1,4).  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       PREDICTOR ID CORRESPONDING TO ID( ) (J=1,15).
C                       (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY
C                            1 LAYER),
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
C                       G = IDPARS(15), AND
C                       THRESH.
C                       JD( ) IS USED TO HELP IDENTIFY THE BASIC MODEL
C                       FIELDS AS READ FROM THE ARCHIVE.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C           KFILRA(J) = HOLDS THE UNIT NUMBERS FOR ACCESSING THE 
C                       MOS-2000 EXTERNAL RANDOM ACCESS FILES 
C                       (J=1,NUMRA).  (INPUT)
C           RACESS(J) = THE FILE NAMES CORRESPONDING TO KFILRA(J)
C                       (J=1,NUMRA).  (CHARACTER*60)  (INPUT)
C               NUMRA = THE NUMBER OF UNIT NUMBERS AND NAMES IN
C                       KFILRA( ) AND RACESS( ).  (INPUT)
C            CCALL(K) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRIDPOINT DEVELOPMENT (K=1,NSTA).
C                       USED FOR PRINTOUT ONLY.  (INPUT)
C             NAME(K) = NAMES OF STATIONS (K=1,NSTA).  USED FOR PRINTOUT
C                       ONLY.  (CHARACTER*20)  (INPUT)
C            SDATA(K) = TERRAIN SLOPE VALUES TO RETURN (K=1,NSTA).
C                       (OUTPUT)
C           STALAT(K) = LATITUDE OF STATIONS (K=1,NSTA).  (INPUT)
C           STALON(K) = LONGITUDE OF STATIONS (K=1,NSTA).  (INPUT)
C          DIR(K,J,M) = THE IX (J=1) AND JY (J=2) POSITIONS ON THE GRID
C                       FOR THE COMBINATION OF GRID CHARACTERISTICS M
C                       (M=1,NGRID) AND STATION K (K=1,NSTA) IN 
C                       NGRIDC( ,M).  (INPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       FIRST DIMENSION OF DIR( , , ).  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C         NGRIDC(L,M) = HOLDS THE GRID CHARACTERISTICS (L=1,6) FOR EACH
C                       GRID COMBINATION (M=1,NGRID).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=POLAR
C                            STEREOGRAPHIC). 
C                       L=2--GRID LENGTH IN MILLIMETERS,
C                       L=3--LATITUDE AT WHICH GRID LENGTH IS 
C                            CORRECT *10000,
C                       L=4--GRID ORIENTATION IN DEGREES *10000,
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *10000,
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES *10000.
C               NGRID = THE NUMBER OF GRID COMBINATIONS IN DIR( , , ),
C                       MAXIMUM OF ND11.  (INPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN BE
C                       DEALT WITH ON THIS RUN.  LAST DIMENSION OF
C                       NGRIDC( , ).  (INPUT)
C              NSLABT = SLAB IN NBRIDC( , ) WHERE TERRAIN GRID 
C                       CHARACTERISTICS ARE STORED.  (OUTPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(K) = THE TERRAIN FIELD READ FROM THE EXTERNAL RANDOM
C                       ACCESS FILE (J=1,ND5).  (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ),IWORK( ), AND DATA( ).
C                       (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS).  (INPUT-OUTPUT)
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK, 
C                              THIS IS MINUS THE RECORD NUMBER WHERE 
C                              THE DATA START.
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDL GRIB, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --NUMBER OF THE SLAB IN DIR( , ,L) AND
C                              IN NGRIDC( ,L) DEFINING THE
C                              CHARACTERISTICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,NPRED) FOR WHICH 
C                              THIS VARIABLE IS NEEDED, WHEN IT IS
C                              NEEDED ONLY ONCE FROM LSTORE( , ).
C                              WHEN IT IS NEEDED MORE THAN ONCE, THE 
C                              VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING 
C                              MSTORE( , ).  LATER USED AS A WAY OF 
C                              DETERMINING WHETHER TO KEEP THIS VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , ) THAT 
C                       HAVE BEEN USED IN THIS RUN.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL
C                       DATA ARE STORED ON DISK.  (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.  IT
C                       IS A RUNNING  COUNT FROM THE BEGINNING OF THE 
C                       PROGRAM.  THIS COUNT IS MAINTAINED IN CASE THE
C                       USER NEEDS IT (DIAGNOSTICS, ETC.).  (OUTPUT)
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
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C              FDU(J) = GRID OF U WINDS RETRIEVED FROM INTERNAL
C                       STORAGE (J=1,NX2X3).  (INTERNAL)
C              FDV(J) = GRID OF F WINDS RETRIEVED FROM INTERNAL
C                       STORAGE (J=1,ND2X3).  (INTERNAL)
C             FDUS)K) = U WINDS AT STATIONS (K=1,NSTA).  (INTERNAL)
C             FDVS)K) = V WINDS AT STATIONS (K=1,NSTA).  (INTERNAL)
C           XMAPFT(J) = WORK ARRAY (J=1,ND2X3).  MAP FACTOR FOR EACH
C                       STATION.  (INTERNAL)
C               TM(J) = WORK ARRAY (J=1,ND2X3).  SINE OF LATITUDE FOR
C                       EACH STATION.  NOT ACTUALLY USED.  THEN,
C                       TERRAIN MAGNITUDE FROM UPSLCM.  (INTERNAL)
C             TDIR(J) = WORK ARRAY (J=1,ND2X3).  DIRECTION OF
C                       TERRAIN SLOPE FROM UPSLCM.  (INTERNAL)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  FOR THIS
C                       ROUTINE, SOME COMPUTATIONS ARE BY STATION,
C                       AND NX2X3 MUST BE GE NSTA.  THIS IS ASSURED
C                       IN THE DRIVER.  (INPUT)
C               ISTAV = 1 SINCE THE DATA RETURNED ARE VECTOR DATA.
C                       (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64).  (INPUT)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       103 = IDPARS(1) DOES NOT INDICATE TERRAIN
C                             UPSLOP.  COMPUTATION NOT DONE.
C                       SEE GFETCH AND MAPLAT FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C              NSLABU = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) WHEN RETRIEVING U WINDS.  WHEN 
C                       IER NE 0, THIS VALUE SHOULD NOT BE USED.
C                       (INTERNAL) 
C              NSLABV = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) WHEN RETRIEVING V WINDS.  WHEN 
C                       IER NE 0, THIS VALUE SHOULD NOT BE USED.
C                       (INTERNAL) 
C              NSLABW = SLAB IN NBRIDC( , ) WHERE WIND GRID 
C                       CHARACTERISTICS ARE STORED.  (INTERNAL)
C                  NX = THE DIMENSION OF THE GRID IN THE IX DIRECTION.
C                       (INTERNAL).
C                  NY = THE DIMENSION OF THE GRID IN THE JY DIRECTION.
C                       (INTERNAL).
C               LD(J) = HOLDS THE 4 ID WORDS OF THE TERRAIN DATA
C                       RETRIEVED INTO DATA( ) AND LATER THE U-WINDS
C                       INTO FDU( ) (J=1,4).  (INTERNAL)
C               MD(J) = HOLDS THE 4 ID WORDS OF THE V-WINDS 
C                       RETRIEVED INTO FDV( ) (J=1,4).  (INTERNAL)
C              XMESHT = GRID LENGTH IN M AT THE LATITUDE XLAT FOR
C                       THE TERRAIN GRID.  (INTERNAL)
C                MAPP = THE MAP PROJECTION OF THE GRID ACCESSED.
C                       (INTERNAL)
C              ORIENT = THE VERTICAL LONGITUDE ON THE GRID IN DEGREES
C                       WEST.  DO NOT USE NEGATIVE.  (INTERNAL)
C                XLAT = THE LATITUDE IN DEGREES N AT WHICH XMESHL IS
C                       CORRECT.  DO NOT USE NEGATIVE.  (INTERNAL)
C              ALATLL = THE LATITUDE OF THE LOWER LEFT CORNER POINT OF
C                       THE GRID IN DEGREES NORTH.  (INTERNAL)
C              ALONLL = THE LONGITUDE OF THE LOWER LEFT CORNER POINT OF
C                       THE GRID IN DEGREES WEST.  DO NOT USE NEGATIVE.
C                       (INTERNAL)
C              IWSMTH = THE SMOOTHING PARAMETER FOR WINDS.  SINCE
C                       THE WINDS ACCESSED ARE ISOBARIC, VALUES 0-4
C                       ARE CONSIDERED.  (INTERNAL)
C              NRRDAT = DATE FAR INTO THE FUTURE TO FORCE SAVING OF
C                       STATION UPSLOPE VALUES.  (INTERNAL)
C               IFLAG = FLAG TO INDICATE WHETHER SLOPE COMPUTATIONS
C                       NEED BE MADE:
C                       0 = YES,
C                       1 = NO.
C                       (INTERNAL)
C              NSLABS = NSLABT WHEN CONSTANT GRIDS ARE SUCCESSFULLY
C                       READ.  ZERO OTHERWISE, SET BY DATA STATEMENT.
C                       (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH, CONSTG, GRCOMB, INTRP, INTRPA, INTRPB, UPSLCM,
C            GSTORE, MCMAPS, PSMAPS, LMMAPS,
C            SMTH5, SMTH9, SMTH25, SMTH2X, SMGH3X,
C
      CHARACTER*8 CCALL(ND1)
      CHARACTER*20 NAME(ND1)
      CHARACTER*60 RACESS(NUMRA)
C
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION SDATA(ND1),STALAT(ND1),STALON(ND1)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION FDU(ND2X3),FDV(ND2X3),FDUS(ND2X3),FDVS(ND2X3),
     1          XMAPFT(ND2X3),TM(ND2X3),TDIR(ND2X3)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION DIR(ND1,2,ND11),NGRIDC(6,ND11)
      DIMENSION CORE(ND10)
      DIMENSION KFILRA(NUMRA)
      DIMENSION LD(4),MD(4),LDPARS(15),MDPARS(15)
C
      DATA NRRDAT/2100010100/
      DATA NSLABS/0/
C
      SAVE NSLABS
C
D     CALL TIMPR(KFILDO,KFILDO,'START UPSLOP        ')
D     WRITE(KFILDO,100)(IDPARS(J),J=1,15)
D100  FORMAT(/' UPSLOP--IDPARS( )',4I12,11I5)
C
      IER=0
      ISTAV=1
      IFLAG=0
      NSLABT=NSLABS
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C        A MAP PROJECTION OF GE 3 IS NEEDED.
C
      IF(IDPARS(1).EQ.005.AND.IDPARS(2)/100.GE.3)GO TO 103
      WRITE(KFILDO,101)(JD(J),J=1,4)
 101  FORMAT(/' ****ID DOES NOT INDICATE USLOPE WIND.',/,
     1        '     PREDICTOR ',I9.9,1X,I9.9,1X,I9.9,I3,
     2        ' NOT COMPUTED IN UPSLOP.')
      IER=103
      GO TO 800
C
C        TRY TO FIND THE TERRAIN CONSTANTS TM( ) AND TDIR( ) IN
C        INTERNAL STORAGE.   CCC = 409, SAME AS TERRAIN HEIGHTS;
C        FFF = XY^ (THAT IS, IT DEPENDS ON THE MAP PROJECTION AND THE
C        GRID LENGTH, BUT NOT THE WIND SMOOTHING); THE TERRAIN
C        SMOOTHING IN IDPARS(14); AND "G" IN ID(4) REPRESENTS
C        TM( ) (=1) OR TDIR( ) (=2).
C
 103  MSDATE=0
C        DATE/TIME ON CONSTANT RECORD = 0.
      MSTIME=0
C        TIME ADJUSTMENT NOT TO BE MADE.
      NCOMBO=1
C        NCOMBO HAS NO MEANING FOR VECTOR DATA.
      NPACK=1
C        THE DATA ARE NOT PACKED.
      LD(1)=409*1000000+(IDPARS(2)/10)*10000
      LD(2)=0
      LD(3)=0
      LD(4)=IDPARS(14)*10+1
C        USE "G" = 1 IN LD(4) TO PREPRESENT TM( ).
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,TM,ND1,
     2            NWORDS,NPACK,MSDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,MSTIME,IER)
C        FOR UNPACKED DATA, WORK ARRAYS ARE NOT USED.  IF CONSTANT
C        DATA ARE AVAILABLE, THEY WILL BE EXACTLY AS NEEDED; NO
C        FURTHER PROCESSING IS TO BE DONE.  THE RETURNED VALUES
C        NTIMES, MISS, AND NWORDS ARE NOT NEEDED OR USED.  IT IS
C        ASSUMED NWORDS = NSTA, SINCE THIS PROGRAM STORED THE DATA.
C        THE DATE OF SUCH RECORDS IS 0.  NOTE THAT THE DIMENSION 
C        ND1 IS FURNISHED RATHER THAN ND5.
      IF(IER.NE.0)GO TO 130
C        IF TM( ) CANNOT BE FOUND, THEN COMPUTE TERRAIN FACTORS.
C
      LD(4)=IDPARS(14)*10+2
C        USE "G" = 2 IN LD(4) TO PREPRESENT TDIR( ).
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,TDIR,ND1,
     2            NWORDS,NPACK,MSDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,MSTIME,IER)
C
      IF(IER.EQ.0)THEN
D        WRITE(KFILDO,104)
D104     FORMAT(/' RETRIEVED TERRAIN SLOPE DATA IN UPSLOP.')
         IFLAG=1
         GO TO 200
C           IF TM( ) AND TDIR ARE FOUND, BYPASS COMPUTING TERRAIN 
C           FACTORS AND SET IFLAG FOR UPSLCM.
      ENDIF
C
C        GET THE TERRAIN IN DATA( ).  FIRST, FIND THE RANDOM
C        ACCESS UNIT, NO. 44.
C
 130  DO 140 J=1,NUMRA
         IF(KFILRA(J).NE.44)GO TO 140
         GO TO 145
 140  CONTINUE
C
C        FALL THROUGH HERE MEANS A RANDOM ACCESS UNIT 44 IS NOT
C        AVAILABLE.
C
      WRITE(KFILDO,143)(JD(J),J=1,4)
 143  FORMAT(/' ****RANDOM ACCESS UNIT 44 NOT AVAILABLE.',/,
     1        '     PREDICTOR ',I9.9,1X,I9.9,1X,I9.9,I3,
     2        ' NOT COMPUTED IN UPSLOP.')
      IER=181
      GO TO 800
C
 145  LD(1)=409*1000000+(IDPARS(2)/10)*10000
C        CCC, MAP PROJECTION, AND MESH LENGTH REPRESENTATION ARE
C        RETAINED.  TERRAIN ID HARDWIRED.
      LD(2)=0
      LD(3)=0
      LD(4)=0
      CALL CONSTG(KFILDO,KFILRA(J),RACESS(J),LD,
     1            IPACK,IWORK,DATA,ND5,
     2            IS0,IS1,IS2,IS4,ND7,
     3            ISTAV1,L3264B,IER) 
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,150)(ID(J),J=1,4)
 150     FORMAT('     TERRAIN HEIGHT UNAVAILABLE.',/,
     1          '     PREDICTOR ',I9.9,1X,I9.9,1X,I9.9,I3,
     2          ' NOT COMPUTED IN UPSLOP.')
C           CONSTANT GRID IS NOT AVAILABLE.  THIS DIAGNOSTIC
C           FOLLOWS ONE IN CONSTG.
         GO TO 800
      ENDIF
C
      IF(ISTAV1.NE.0)THEN
         WRITE(KFILDO,152)(ID(J),J=1,4)
 152     FORMAT(/' ****RECORD FROM RANDOM ACCESS FILE FOR VARIABLE',
     1             I9.9,1X,I9.9,1X,I9.9,1X,I10.3,/,
     2           '     SHOULD BE GRIDPOINT, BUT IS INDICATED AS',
     3           ' VECTOR.  NOT USED.')
         IER=180
         GO TO 800
      ENDIF
C
C        AT THIS POINT, DATA( ) CONTAINS HEIGHT IN METERS.
C
      CALL GRCOMB(KFILDO,0,IS2,ND7,NGRIDC,ND11,NGRID,NSLABT,
     1            CCALL,NAME,STALAT,STALON,DIR,ND1,NSTA,IER)
C        UPON RETURN FROM GRCOMB, NSLABT IS THE NUMBER OF THE GRID
C        COMBINATION IN NGRIDC OF THE TERRAIN GRID.  NOTE THAT "IP12"
C        IS USED AS ZERO HERE.
C
      IF(IER.NE.0)THEN
C           IER NE 0 IS TREATED AS FATAL WITH RETURN TO CALLING
C           PROGRAM.
         GO TO 800
C
      ENDIF
C
C        ESTABLISH TERRAIN GRID PARAMETERS FOR USE IN SUBROUTINE
C        UPSLCM.
C
      NXT=IS2(3)
      NYT=IS2(4)
      MAPT=NGRIDC(1,NSLABT)
      XMESHT=NGRIDC(2,NSLABT)/1000.
      XLAT=NGRIDC(3,NSLABT)/10000.
      ORIENT=NGRIDC(4,NSLABT)/10000.
      ALATLL=NGRIDC(5,NSLABT)/10000.
      ALONLL=NGRIDC(6,NSLABT)/10000.
C        THE ABOVE PERTAINS TO THE TERRAIN GRID.
C
C        SMOOTH TERRAIN IF DESIRED.
C
      IF(IDPARS(14).EQ.0)GO TO 162
C
D     WRITE(KFILDO,157)IDPARS(14)
D157  FORMAT(/' SMOOTHING TERRAIN--IDPARS(14)',I6)
C
      IF(IDPARS(14).EQ.1)THEN
C           IWORK( ) IS DIMENSIONED ND5 LIKE DATA( ).  IT IS
C           USED AS FLOATING POINT IN SMOOTHING ROUTINES, BUT
C           THAT IS OK.
         CALL SMTH5 (KFILDO,DATA,IWORK,NXT,NYT)
      ELSEIF(IDPARS(14).EQ.2)THEN
         CALL SMTH9 (KFILDO,DATA,IWORK,NXT,NYT)
      ELSEIF(IDPARS(14).EQ.3)THEN
         CALL SMTH25(KFILDO,DATA,IWORK,NXT,NYT)
      ELSEIF(IDPARS(14).EQ.4)THEN
         CALL SMTH2X(KFILDO,DATA,IWORK,NXT,NYT)
      ELSEIF(IDPARS(14).EQ.5)THEN
         CALL SMTH3X(KFILDO,DATA,IWORK,NXT,NYT)
      ELSE
         WRITE(KFILDO,160)IDPARS(14),(ID(J),J=1,4)
 160     FORMAT(/' ****INCORRECT SMOOTHING VALUE IN IDPARS(14) =',
     1            I3,' IN UPSLOP.'/
     2           '     PREDICTOR ',I9.9,1X,I9.9,1X,I9.9,I3,
     3           ' NOT COMPUTED IN UPSLOP.')
         IER=45
         GO TO 800
      ENDIF
C
C        CALCULATE MAP FACTOR FOR THE PROJECTION MAPT FOR EACH 
C        STATION FOR THE TERRAIN GRID.  TM( ) IS USED AS A 
C        WORK ARRAY TO HOLD THE SINE OF THE LATITUDE, BUT ONLY
C        IF TM( ) COULD NOT BE FILLED ABOVE WITH SLOPE
C        MAGNITUDE.
C
 162  IF(MAPT.EQ.5)THEN
         CALL PSMAPS(KFILDO,XMESHT,ORIENT,XLAT,ALATLL,ALONLL,
     1               DIR(1,1,NSLABT),DIR(1,2,NSLABT),TM,XMAPFT,NSTA,
     2               IER)
      ELSEIF(MAPT.EQ.3)THEN
         CALL LMMAPS(KFILDO,XMESHT,ORIENT,XLAT,ALATLL,ALONLL,
     1               DIR(1,1,NSLABT),DIR(1,2,NSLABT),TM,XMAPFT,NSTA,
     2               IER)
      ELSEIF(MAPT.EQ.7)THEN
         CALL MCMAPS(KFILDO,XMESHT,XLAT,ALATLL,ALONLL,
     1               DIR(1,1,NSLABT),DIR(1,2,NSLABT),TM,XMAPFT,NSTA,
     2               IER)
      ELSE
         WRITE(KFILDO,165)MAPT
 165     FORMAT(/' ****INCORRECT MAP PROJECTION =',I4,
     1           ' IN TSLCM.')
         IER=60
         GO TO 800
      ENDIF
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,166)IER
 166     FORMAT(/' ****ERROR IN UPSLOP, IER =',I4,
     1           ' CALCULATING MAP FACTOR.')
         GO TO 800
      ENDIF
C
C        SAVE NSLABT IN NSLABS FOR NEXT ENTRY.  IT IS USED FOR 
C        CHECKING ABOVE 253.
C
      NSLABS=NSLABT
C
C        GET THE U WINDS.  THE ID INCLUDES THE ADVECTIVE MODEL
C        DD AND THE RUN OFFSET TIME RR.
C
C
 200  IWSMTH=IDPARS(2)-(IDPARS(2)/10)*10
C        IWSMTH IS THE SMOOTHING PARAMETER FOR WINDS.  IT ALSO
C        DESIGNATES WHETHER WINDS ARE ISOBARIC (VALUES 0 - 4)
C        OR CONSTANT HEIGHT (VALUES 5 - 9).
      LD(1)=004000*1000+IDPARS(4)
      IF(IWSMTH.GE.5)LD(1)=LD(1)+20000
C        IT IS ASSUMED CC0 IS ISOBARIC AND CC2 IS CONSTANT HEIGHT.
      LD(2)=JD(2)
      LD(3)=IDPARS(9)*1000000+IDPARS(12)
      LD(4)=0
C
      IF(MOD(IDPARS(12),3).EQ.0)THEN
C           ONLY TAUS EVENLY DIVISIBLE BY 3 ARE AVAILABLE.
         CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1               IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDU,ND2X3,
     2               NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3               NBLOCK,NFETCH,NSLABU,MISSP,MISSS,L3264B,1,IER)
         IF(MISSP.NE.0)MISTOT=MISTOT+1
      ELSE
         CALL PRSID1(KFILDO,LD,LDPARS)
         CALL TIMGRD(KFILDO,KFIL10,LD,LDPARS,LD,NDATE,
     1               SDATA,ND1,NSTA,NSLABU,IPACK,IWORK,FDU,ND2X3,
     2               LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3               IS0,IS1,IS2,IS4,ND7,
     4               FDUS,FDVS,ND2X3,
     5               ISTAVD,L3264B,MISTOT,IER) 
      ENDIF
C
      IF(IER.NE.0)GO TO 800
      NX=IS2(3)
      NY=IS2(4)
C
C        GET THE V WINDS.  THE ID INCLUDES THE ADVECTIVE MODEL
C        DD AND THE RUN OFFSET TIME RR.
C
      MD(1)=004100*1000+IDPARS(4)
      IF(IWSMTH.GE.5)MD(1)=MD(1)+20000
C        IT IS ASSUMED CC0 IS ISOBARIC AND CC2 IS CONSTANT HEIGHT.
      MD(2)=JD(2)
      MD(3)=IDPARS(9)*1000000+IDPARS(12)
      MD(4)=0
      IF(MOD(IDPARS(12),3).EQ.0)THEN
C           ONLY TAUS EVENLY DIVISIBLE BY 3 ARE AVAILABLE.
         CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1               IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDV,ND2X3,
     2               NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3               NBLOCK,NFETCH,NSLABV,MISSP,MISSS,L3264B,1,IER)
         IF(MISSP.NE.0)MISTOT=MISTOT+1
      ELSE
         CALL PRSID1(KFILDO,MD,MDPARS)
         CALL TIMGRD(KFILDO,KFIL10,MD,MDPARS,MD,NDATE,
     1               SDATA,ND1,NSTA,NSLABV,IPACK,IWORK,FDV,ND2X3,
     2               LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3               IS0,IS1,IS2,IS4,ND7,
     4               FDUS,FDVS,ND2X3,
     5               ISTAVD,L3264B,MISTOT,IER) 
      ENDIF
C
      IF(IER.NE.0)GO TO 800
      IF(NSLABU.EQ.NSLABV.AND.NX.EQ.IS2(3).AND.NY.EQ.IS2(4))GO TO 230
C        THE GRID CHARACTERISTICS ARE NOT THE SAME.
      WRITE(KFILDO,220)(LD(J),J=1,4),(NGRIDC(J,NSLABU),J=1,6),NX,NY,
     1                 (MD(J),J=1,4),(NGRIDC(J,NSLABV),J=1,6),
     2                 IS2(3),IS2(4)
 220  FORMAT(/' ****THE TWO GRIDS NEEDED IN UPSLOP HAVE DIFFERENT',
     1        ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2        '  VALUES FROM NGRIDC( , ) AND NX, NY.',/,
     3        5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5)
      IER=100
      GO TO 800
C
 230  NSLABW=NSLABU
C
C        SMOOTH WINDS IF DESIRED.
C
      IF(IWSMTH.NE.0.AND.IWSMTH.NE.5)THEN
C
         IF(IWSMTH.EQ.1.OR.IWSMTH.EQ.6)THEN
            CALL SMTH5 (KFILDO,FDU,IWORK,IS2(3),IS2(4))
            CALL SMTH5 (KFILDO,FDV,IWORK,IS2(3),IS2(4))
         ELSEIF(IWSMTH.EQ.2.OR.IWSMTH.EQ.7)THEN
            CALL SMTH9 (KFILDO,FDU,IWORK,IS2(3),IS2(4))
            CALL SMTH9 (KFILDO,FDV,IWORK,IS2(3),IS2(4))
         ELSEIF(IWSMTH.EQ.3.OR.IWSMTH.EQ.8)THEN
            CALL SMTH25(KFILDO,FDU,IWORK,IS2(3),IS2(4))
            CALL SMTH25(KFILDO,FDV,IWORK,IS2(3),IS2(4))
         ELSEIF(IWSMTH.EQ.4.OR.IWSMTH.EQ.9)THEN
            CALL SMTH2X(KFILDO,FDU,IWORK,IS2(3),IS2(4))
            CALL SMTH2X(KFILDO,FDV,IWORK,IS2(3),IS2(4))
         ELSE
            WRITE(KFILDO,250)IWSMTH,(ID(J),J=1,4)
 250        FORMAT(/' ****INCORRECT SMOOTHING VALUE IN IDPARS(2) =',
     1               I3,' IN UPSLOP.',/,
     2              '     PREDICTOR ',I9.9,1X,I9.9,1X,I9.9,I3,
     3              ' NOT COMPUTED IN UPSLOP.')
C              THIS IS HERE FOR SAFETY.  ACTUALLY, ALL SINGLE-DIGIT
C              VALUES ARE ACCOMMODATED.
            IER=45
            GO TO 800
         ENDIF
C
      ENDIF
C        
C        CHECK CONSISTENCY OF GRIDS.  FOR THE UPSLOPE TO
C        BE CALCULATED WITHOUT TURNING EITHER THE WINDS
C        OR UPSLOPE COMPONENTS, THE ORIENTATION AND
C        MAP PROJECTION MUST BE THE SAME.
C
      IF(NGRIDC(1,NSLABT).EQ.NGRIDC(1,NSLABW).AND.
     1   NGRIDC(4,NSLABT).EQ.NGRIDC(4,NSLABW))THEN
         GO TO 255
      ELSE
         WRITE(KFILDO,253)NGRIDC(1,NSLABT),NGRIDC(4,NSLABT),
     1                    NGRIDC(1,NSLABW),NGRIDC(4,NSLABW),
     2                    (ID(J),J=1,4)
 253     FORMAT(/' ****MAP PROJECTION =',I4,' AND GIRD',
     1           ' ORIENTATION =',I4, 'OF THE TERRAIN GRID',
     2           ' DO NOT MATCH THE',/,
     3           '     MAP PROJECTION =',I4,' AND GRID',
     4           ' ORIENTATION =',I4,' OF THE WIND GRID'/
     5           '     PREDICTOR ',I9.9,1X,I9.9,1X,I9.9,I3,
     6           ' NOT COMPUTED IN UPSLOP.')
         IER=100
         GO TO 800
      ENDIF
C
C        INTERPOLATE WINDS TO STATIONS.
C
 255  IF(IDPARS(13).EQ.1)THEN
         CALL INTRPA(KFILDO,FDU,IS2(3),IS2(4),
     1               DIR(1,1,NSLABW),ND1,NSTA,FDUS)
         CALL INTRPA(KFILDO,FDV,IS2(3),IS2(4),
     1               DIR(1,1,NSLABW),ND1,NSTA,FDVS)
C           INTRPA IS BIQUADRATIC INTERPOLATION WHERE POSSIBLE,
C           BILINEAR OTHERWISE.
C
      ELSEIF(IDPARS(13).EQ.2)THEN
         CALL INTRPB(KFILDO,FDU,IS2(3),IS2(4),
     1               DIR(1,1,NSLABW),ND1,NSTA,FDUS)
         CALL INTRPB(KFILDO,FDV,IS2(3),IS2(4),
     1               DIR(1,1,NSLABW),ND1,NSTA,FDVS)
C           INTRPB IS BILINEAR.
C
      ELSEIF(IDPARS(13).EQ.3)THEN
         CALL INTRP(KFILDO,FDU,IS2(3),IS2(4),
     1              DIR(1,1,NSLABW),ND1,NSTA,FDUS)
         CALL INTRP(KFILDO,FDV,IS2(3),IS2(4),
     1              DIR(1,1,NSLABW),ND1,NSTA,FDVS)
C           INTRP IS INTERPOLATION FOR PRECIPITATION AMOUNT.  THE
C           PROCESS IS BILINEAR AFTER PREPARATION OF THE FIELD 
C           TO PUT THE ZERO LINE ABOUT HALFWAY BETWEEN POSITIVE 
C           AND ZERO GRIDPOINTS.
C
      ELSEIF(IDPARS(13).EQ.4)THEN
C***         CALL INTRPC(KFILDO,FDU,IS2(3),IS2(4),
C***     1               DIR(1,1,NSLABW),ND1,NSTA,FDUS)
C***         CALL INTRPC(KFILDO,FDV,IS2(3),IS2(4),
C***     1               DIR(1,1,NSLABW),ND1,NSTA,FDVS)
C           INTRPC FINDS VALUES CLOSEST TO A GRIDPOINT.
C***        THIS IS NOT IMPLEMENTED FOR THE WIND GRIDS.
         WRITE(KFILDO,260)(ID(J),J=1,4)
 260     FORMAT(/' ****INTERPOLATION PARAMETER = 4 IN UPSLOP FOR',
     1           ' WINDS.  THIS IS UNUSUAL AND IS PROBABLY AN ERROR.',/,
     2           '     PREDICTOR ',I9.9,1X,I9.9,1X,I9.9,I3,
     3           ' NOT COMPUTED IN UPSLOP.')
        GO TO 800
      ELSE
         WRITE(KFILDO,262)(ID(J),J=1,4)
 262     FORMAT(/' ****INTERPOLATION PARAMETER INCORRECT IN UPSLOP FOR',
     1           ' WINDS.  THIS IS AN ERROR.',/,
     2           '     PREDICTOR ',I9.9,1X,I9.9,1X,I9.9,I3,
     3           ' NOT COMPUTED IN UPSLOP.')
         GO TO 800
C           WHEN THE INTERPOLATION VALUE IS NOT LEGITIMATE, CKIDS WILL
C           HAVE PRINTED A DIAGNOSTIC.
      ENDIF
C      
C        AT THIS POINT, THE TERRAIN IS IN DATA( ) AND THE WIND
C        COMPONENTS ARE IN FDUS( ) AND FDVS( ), UNLESS THE
C        TERRAIN COMPONENTS WERE FOUND AS PREVIOUSLY COMPUTED,
C        IN WHICH CASE THE MAGNITUDE IS IN TM( ) AND THE 
C        DIRECTION IN DIR( ).  THESE OPTIONS ARE SIGNALED
C        TO UPSLCM BY IFLAG.  NOW CALCULATE SLOPE BY STATION.
C
      CALL UPSLCM(KFILDO,SDATA,DIR(1,1,NSLABT),FDUS,FDVS,TM,TDIR,
     1            XMAPFT,ND1,NSTA,XMESHT,DATA,NXT,NYT,NDATE,IFLAG)
C        UPSLCM DOES NOT PRODUCE AN ERROR.  IF CONTROL REACHES
C        HERE, VARIABLE HAS BEEN COMPUTED.
C
      IF(IFLAG.EQ.1)GO TO 900
C
C        WRITE TM( ) AND TDIR( ) TO INTERNAL STORAGE SO THEY 
C        WON'T HAVE TO BE COMPUTED AGAIN.
C
      NCOMBO=1
C        NCOMBO HAS NO MEANING FOR VECTOR DATA.
      NPACK=1
C        THE DATA ARE NOT PACKED.
      MSDATE=0
C        THE DATE TO STORE IS ZERO.
      LD(1)=409*1000000+(IDPARS(2)/10)*10000
      LD(2)=0
      LD(3)=0
      LD(4)=IDPARS(14)*10+1
C        USE "G" = 1 IN LD(4) TO PREPRESENT TM( ).
      CALL GSTORE(KFILDO,KFIL10,LD,NCOMBO,LSTORE,ND9,LITEMS,
     1            TM,NSTA,NPACK,NRRDAT,MSDATE,
     2            CORE,ND10,LASTL,NBLOCK,LASTD,
     3            NSTORE,L3264B,IER) 
      LD(4)=IDPARS(14)*10+2
C        USE "G" = 2 IN LD(4) TO PREPRESENT TDIR( ).
      CALL GSTORE(KFILDO,KFIL10,LD,NCOMBO,LSTORE,ND9,LITEMS,
     1            TDIR,NSTA,NPACK,NRRDAT,MSDATE,
     2            CORE,ND10,LASTL,NBLOCK,LASTD,
     3            NSTORE,L3264B,IER) 
      
      GO TO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C
 800  DO 801 J=1,ND1
      SDATA(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END      
