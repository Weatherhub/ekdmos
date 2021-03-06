      SUBROUTINE KINDEX(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                  NGRIDC,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     2                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                  IS0,IS1,IS2,IS4,ND7,
     4                  FDTK5,FDTK8,FDTK7,FDPT8,FDPT7,FD4,ND2X3,
     5                  ISTAV,L3264B,MISTOT,IER)
C
C
C        SEPTEMBER 1998   HUGHES  TDL   MOS-2000
C        OCTOBER   2002   WEISS   CHANGED ND5 TO ND2X3
C        APRIL     2003   GLAHN   MODIFIED LINES IN CALL;  SET
C                                 DIMENSIONS OF IPACK( ), IWORK( )
C                                 AND DATA( ) = ND5; SPELL CHECK;
C                                 CHANGED CALCULATION DO 600 LOOP
C                                 FROM ND2X3 TO MTX5*NTY5; REMOVED
C                                 DEFINING IDPARS(7)
C        MAY       2003   GLAHN   REARRANGED TYPE STATEMENTS
C
C        PURPOSE
C            TO COMPUTE THE K-INDEX WHICH IS BY DEFINITION ON AN
C            ISOBARIC SURFACE.  FIRST WORD OF ID IS 007200000
C            K-INDEX = (850T - 500T) + 850TD - (700T - 700TD)
C            THE K-INDEX IS USEFUL FOR PREDICTING NON-SEVERE
C            WARM SEASON CONVECTIVE ACTIVITY
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) IS ACCOMMODATED:
C
C               007 200 - K-INDEX
C            
C        DATA SET USE
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT(PRINT) FILE.
C                       (OUTPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                       ACCESS.(INPUT-OUTPUT)
C
C        VARIABLES
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE(,) (J=1,ND10).
C                       WHEN CORE() IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C             DATA(J) = K-INDEX CALCULATED FROM 850, 700 AND 500 MB
C                       TEMPERATURE AND 850 AND 700 MB DEWPOINT 
C                       TEMPERATURE (J=1,ND5).  (OUTPUT)
C            FDPT8(K) = DATA ARRAY TO HOLD THE DEW POINT TEMPERATURE
C                       AT 850 MB IN KELVIN (K=1,ND2X3). (INTERNAL)
C            FDPT7(K) = DATA ARRAY TO HOLD THE DEW POINT TEMPERATURE
C                       AT 700 MB IN KELVIN (K=1,ND2X3). (INTERNAL)
C            FDTK8(K) = WORK ARRAY TO HOLD THE AIR TEMPERATURE AT 
C                       850 MB IN KELVIN (K=1,ND2X3). (INTERNAL)
C            FDTK7(K) = WORK ARRAY TO HOLD THE AIR TEMPERATURE AT 
C                       700 MB IN KELVIN (K=1,ND2X3). (INTERNAL)
C            FDTK5(K) = WORK ARRAY TO HOLD THE AIR TEMPERATURE AT 
C                       500 MB IN KELVIN (K=1,ND2X3). (INTERNAL)
C              FD4(K) = WORK ARRAY USED BY SUBROUTINE DEWPT 
C                       (INTERNAL)
C                   I = LOOP CONTROL VARIABLE
C          ICCCFFF( ) = CONTAINS IDPARS(1) AND IDPARS(2) ID FOR THE
C                       METEOROLOGICAL PARAMETERS BEING USED.
C                       COLUMN 1 CONTAINS ID FOR ISOBARIC SURFACE
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
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                       100 = THE TWO GRIDS NEEDED ARE NOT THE SAME SIZE
C                       101 = GRID SIZE IS TOO BIG FOR ???(), WHOSE 
C                             DIMENSION IS ND5.
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE
C                             K-INDEX.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C                  IJ = LOOP CONTROL VARIABLE
C            IPACK(J) = WORK ARRAY (J=1,ND5). (INTERNAL)
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
C                 ISO = 1 FOR ISOBARIC, 2 FOR ISOHYETAL SURFACE,
C                       3 FOR SIGMA SURFACE(INTERNAL)
C               ISTAV = 0 SINCE THE DATA RETURNED ARE GRID DATA.
C                       (OUTPUT)
C            IWORK(J) = WORK ARRAY (J=1,ND5). (INTERNAL)
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
C            MDPARS() = PARSED ID USED IN SUBROUTINE PRSID1 FOR
C                       SUBROUTINE DEWPOINT
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 WHEN DATA ARE NOT PACKED. (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 WHEN DATA ARE NOT PACKED.  (INTERNAL)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE. (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF
C                       THE GRID IS NOT KNOWN BEFORE FDTK AND FDDP
C                       ARE FETCHED.  ALL WORK ARRAYS ARE DIMENSIONED
C                       ND2X3                              (INPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND
C                       DATA( ).  (INPUT)
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
C             NSLABD7 = THE NUMBER USED TO COMPARE TO NSLAB THAT
C                       IS RETURNED FROM SUBROUTINE DEWPOINT AFTER
C                       FETCHING THE 700MB DEWPOINT AND IS USED
C                       AS A CHECK
C             NSLABT5 = THE NUMBER USED TO COMPARE TO NSLAB THAT
C                       IS RETURNED FROM SUBROUTINE GFETCH AFTER
C                       FETCHING THE 500MB TEMP AND IS USED AS A CHECK
C             NSLABT7 = THE NUMBER USED TO COMPARE TO NSLAB THAT
C                       IS RETURNED FROM SUBROUTINE GFETCH AFTER
C                       FETCHING THE 700MB TEMP AND IS USED AS A CHECK
C             NSLABT8 = THE NUMBER USED TO COMPARE TO NSLAB THAT
C                       IS RETURNED FROM SUBROUTINE GFETCH AFTER
C                       FETCHING THE 700MB TEMP AND IS USED AS A CHECK
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE,
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS 
C                       STORED IN LSTORE(9,). (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA().  THIS 
C                       IS RETURNED FROM GFETCH (INTERNAL)
C        1         2         3         4         5         6         7 X
C
      IMPLICIT NONE
C     
      REAL,PARAMETER :: ABSZRO=-273.15
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER ICCCFFF(2),MD7(4),MD8(4),MT5(4),MT8(4),MT7(4),MDPARS(15)
      INTEGER IER,IJ,ISO,ISTAV,J,KFILDO,KFIL10,L3264B,LITEMS,
     1        MISSP,MISSS,MISTOT,MDX8,MDY8,MDX7,MDY7,MTX5,MTY5,
     2        MTX7,MTY7,MTX8,MTY8,NBLOCK,ND2X3,ND5,ND7,ND9,ND10,ND11,
     3        NDATE,NFETCH,NPACK,NSLAB,NSLABD7,NSLABT5,NSLABT7,NSLABT8,
     4        NTIMES,NWORDS
C
      REAL DATA(ND5)
      REAL FDTK5(ND2X3),FDTK7(ND2X3),FDTK8(ND2X3),FDPT7(ND2X3),
     1     FDPT8(ND2X3),FD4(ND2X3)
      REAL CORE(ND10)
C
      IER=0
      ISTAV=0
C
C        THE K-INDEX IS CALCULATED ONLY ON A CONSTANT PRESSURE
C        LEVEL, HENCE ISO=1.  SET THE ISO INDEX TO '1' SO THAT THE
C        METEOROLOGICAL VARIABLES WILL BE TAKEN ON ISOBARIC SURFACES.
C
      ISO=1
C        PREPARE PARSED ID TO REQUEST TEMPERATURE  
      ICCCFFF(1)=002000   
C        PREPARE PARSED ID TO REQUEST DEW POINT    
      ICCCFFF(2)=003100  
C
C        MAKE SURE THE REQUESTED PREDICTOR ID IS VALID FOR KINDEX
C
      IF(IDPARS(1).NE.007.OR.IDPARS(2).NE.200)THEN
        WRITE(KFILDO,101)(JD(J),J=1,4)
 101    FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE K-INDEX',
     1  ' PREDICTOR. ',I9.9,2I10.9,I4.3,' NOT COMPUTED IN KINDEX. ')
        IER=103
        GOTO 800
      END IF
C
C        CREATE ID FOR 850 MB DEW POINT TEMPERATURE
C
      MD8(1)=ICCCFFF(2)*1000+IDPARS(4)
      MD8(2)=850
      MD8(3)=IDPARS(9)*1000000+IDPARS(12)
      MD8(4)=0
C
C        CALL DEWPT TO RETURN THE 850 DEWPOINT TEMPERATURE
C
      CALL PRSID1(KFILDO,MD8,MDPARS) 
      CALL DEWPT(KFILDO,KFIL10,MDPARS,MD8,NDATE,NGRIDC,ND11,NSLAB,
     1           IPACK,IWORK,FDPT8,ND2X3,LSTORE,ND9,LITEMS,CORE,ND10,
     2           NBLOCK,NFETCH,IS0,IS1,IS2,IS4,ND7,FDTK8,FDTK7,
     3           FDTK5,FD4,ND2X3,ISTAV,
     4           L3264B,MISTOT,IER)
C
      IF(IER.NE.0)GOTO 800
C
      MDX8=IS2(3)
      MDY8=IS2(4)
C
C        CREATE ID FOR 700 MB DEW POINT TEMPERATURE
C
      MD7(1)=ICCCFFF(2)*1000+IDPARS(4)
      MD7(2)=700
      MD7(3)=IDPARS(9)*1000000+IDPARS(12)
      MD7(4)=0
C 
C        CALL DEWPT TO RETURN THE 700 DEWPOINT TEMPERATURE
C
      CALL PRSID1(KFILDO,MD7,MDPARS) 
      CALL DEWPT(KFILDO,KFIL10,MDPARS,MD7,NDATE,NGRIDC,ND11,NSLABD7,
     1           IPACK,IWORK,FDPT7,ND2X3,LSTORE,ND9,LITEMS,CORE,ND10,
     2           NBLOCK,NFETCH,IS0,IS1,IS2,IS4,ND7,FDTK8,FDTK7,
     3           FDTK5,FD4,ND2X3,ISTAV,
     4           L3264B,MISTOT,IER)
C
      IF(IER.NE.0)GOTO 800
C
      MDX7=IS2(3)
      MDY7=IS2(4)
C
      IF(NSLABD7.NE.NSLAB.OR.MDX7.NE.MDX8.OR.MDY7.NE.MDY8)THEN
        WRITE(KFILDO,200)NSLAB,NSLABD7
 200    FORMAT(/,' ****THE CHARACTERISTICS OF THE 700 MB ',
     1           'DEWPOINT GRID ARE DIFFERENT',I3,2X,I3)
        IER=100
        GOTO 800
      END IF  

C        CREATE ID FOR 850 TEMPERATURE ON THE APPROPRIATE SURFACE
C
      MT8(1)=ICCCFFF(1)*1000+IDPARS(4)
      MT8(2)=850
      MT8(3)=IDPARS(9)*1000000+IDPARS(12)
      MT8(4)=0
C
C        FETCH 850 MB TEMPERATURE
C
      CALL GFETCH(KFILDO,KFIL10,MT8,7777,LSTORE,ND9,LITEMS,
     1           IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDTK8,ND2X3,NWORDS,
     2           NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,NFETCH,NSLABT8,
     3           MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
      IF(IER.NE.0)GOTO 800
C      
      MTX8=IS2(3)
      MTY8=IS2(4)
C
      IF(NSLABT8.NE.NSLAB.OR.MTX8.NE.MDX8.OR.MTY8.NE.MDY8)THEN
        WRITE(KFILDO,300)NSLAB,NSLABT8
 300    FORMAT(/,' ****THE GRID CHARACTERISTICS OF THE TEMPERATURE',
     1           ' AT 850 MB ARE DIFFERENT.',I3,2X,I3)
        IER=100
        GOTO 800
      END IF

C        CREATE ID FOR 700 TEMPERATURE ON THE APPROPRIATE SURFACE
C
      MT7(1)=ICCCFFF(1)*1000+IDPARS(4)
      MT7(2)=700
      MT7(3)=IDPARS(9)*1000000+IDPARS(12)
      MT7(4)=0
C
C        FETCH 700 MB TEMPERATURE.
C
      CALL GFETCH(KFILDO,KFIL10,MT7,7777,LSTORE,ND9,LITEMS,IS0,
     1            IS1,IS2,IS4,ND7,IPACK,IWORK,FDTK7,ND2X3,NWORDS,
     2            NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,NFETCH,
     3            NSLABT7,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
      IF(IER.NE.0)GOTO 800
C
      MTX7=IS2(3)
      MTY7=IS2(4)
C
      IF(NSLABT7.NE.NSLAB.OR.MTX7.NE.MDX8.OR.MTY7.NE.MDY8)THEN
        WRITE(KFILDO,400)NSLAB,NSLABT7
 400    FORMAT(/,' ****THE GRID CHARACTERISTICS OF THE TEMPERATURE',
     1           ' AT 700 MB ARE DIFFERENT.',I3,2X,I3)
        IER=100
        GOTO 800
      END IF
C
C        CREATE ID FOR 500 MB TEMPERATURE
C
      MT5(1)=ICCCFFF(1)*1000+IDPARS(4)
      MT5(2)=500
      MT5(3)=IDPARS(9)*1000000+IDPARS(12)
      MT5(4)=0
C
C        NOW FETCH 500 MB TEMPERATURE
C
      CALL GFETCH(KFILDO,KFIL10,MT5,7777,LSTORE,ND9,LITEMS,IS0,
     1            IS1,IS2,IS4,ND7,IPACK,IWORK,FDTK5,ND2X3,NWORDS,
     2            NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,NFETCH,
     3            NSLABT5,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
      IF(IER.NE.0)GOTO 800
C
      MTX5=IS2(3)
      MTY5=IS2(4)
C
C        CHECK IF NSLAB EQUALS NSLABT5
C
      IF(NSLABT5.NE.NSLAB.OR.MTX5.NE.MDX8.OR.MTY5.NE.MDY8)THEN
        WRITE(KFILDO,500)NSLAB,NSLABT5
 500    FORMAT(/,' ****THE GRID CHARACTERISTICS OF THE TEMPERATURE',
     1           ' AT 500 MB ARE DIFFERENT.',I3,2X,I3)
        IER=100
        GOTO 800
      END IF
C
C        COMPUTATION OF K-INDEX 
C
      DO 600 IJ=1,MTX5*MTY5
        DATA(IJ)=((FDTK8(IJ)-FDTK5(IJ))+FDPT8(IJ)-
     1           (FDTK7(IJ)-FDPT7(IJ)))+ABSZRO
 600  CONTINUE
C
      GOTO 900
C 
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C
 800  DO 801 J=1,ND2X3
        DATA(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END
