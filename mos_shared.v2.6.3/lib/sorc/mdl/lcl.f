      SUBROUTINE LCL(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1           NGRIDC,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     2           LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3           IS0,IS1,IS2,IS4,ND7,
     4           FDTK,FD2,FD3,FD4,ND2X3,
     5           ISTAV,L3264B,MISTOT,IER)
C
C        SEPTEMBER 1998   HUGHES  TDL MOS-2000
C        OCTOBER   2002   WEISS   CHANGE ND5 TO ND2X3
C        APRIL     2003   GLAHN   MODIFIED LINES IN CALL;  SET
C                                 DIMENSIONS OF IPACK( ), IWORK( )
C                                 AND DATA( ) = ND5; SPELL CHECK;
C                                 ELIMINATED AUTOMATIC ARRAY FDPT( );
C                                 CHANGED ND2X3 TO MTX*MTY IN DO 300 
C                                 LOOP
C        MAY       2003   GLAHN   REARRANGED TYPE STATEMENTS; 
C                                 CHANGED DIMENSION OF ICCCFFF( )
C                                 FROM 5 TO 2 AND PUT ID'S IN DATA
C                                 STATEMENT
C
C        PURPOSE
C            TO COMPUTE THE TEMPERATURE AND THE PRESSURE OF THE
C            LIFTED CONDENSATION LEVEL BASED ON THE FORMULA
C            PRESENTED BY BOLTON (MWR 1980, VOLUME 108).
C            TLCL = [1/(1/DEWPT-56)+LN(TEMP/DEWPT)/800)]+56
C            PLCL = PRESS*(TLCL/TEMP))**(1/KAPPA), WHERE ALL
C            TEMPERATURES ARE EXPRESSED IN KELVIN AND THE INITIAL
C            PRESSURE IS GIVEN IN MB.  THIS IS ALSO THE SAME
C            FORMULA DESCRIBED IN THE GEMPAK DOCUMENTATION.
C            THE FIRST WORD OF THE PLCL ID IS 003160000.
C            THE FIRST WORD OF THE TLCL ID IS 003161000.
C            NOTE TO THE USER:  TLCL WILL BE RETURNED IN KELVIN
C            AND PLCL WILL BE RETURNED IN PASCALS (MB*100).
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C               003 160 - PRESSURE OF LIFTED CONDENSATION LEVEL IN
C                         MB*100 (PASCALS) STARTING AT LEVEL UUUU
C               003 161 - TEMPERATURE OF LIFTED CONDENSATION LEVEL IN K
C                         STARTING AT LEVEL UUUU
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
C             DATA(J) = DATA ARRAY WHICH RETURNS THE TEMPERATURE
C                       OF THE LIFTED CONDENSATION LEVEL WHEN THE
C                       FIRST WORD OF THE ID IS 003161000, OR THE
C                       PRESSURE OF THE LIFTED CONDENSATION LEVEL
C                       WHEN THE FIRST WORD OF THE ID IS 003160000
C                       (OUTPUT)
C              FD2(K) = WORK ARRAY TO HOLD THE ATMOSPHERIC PRESSURE
C                       IN PASCALS (K=1,ND2X3). (INTERNAL)
C             FDTK(K) = WORK ARRAY TO HOLD THE AIR TEMPERATURE IN 
C                       KELVIN (K=1,ND2X3). (INTERNAL)
C                   I = LOOP CONTROL VARIABLE
C           ICCCFFF() = CONTAINS IDPARS(1) AND IDPARS(2) ID FOR THE
C                       METEOROLOGICAL PARAMETERS BEING USED.
C                       COLUMN 1 CONTAINS ID FOR ISOBARIC SURFACE
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
C                       100 = GRID CHARACTERISTICS NOT THE SAME FOR 
C                             TWO REQUESTED FIELDS.
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE
C                             LCL.
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
C                       THE GRID IS NOT KNOWN BEFORE FDTK AND DATA
C                       ARE FETCHED.  (INPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ) AND DATA( ).
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
C              NSLABD = THE NUMBER USED TO COMPARE TO NSLAB THAT
C                       IS RETURNED FROM SUBROUTINE DEWPOINT AFTER
C                       FETCHING THE 850MB DEWPOINT AND IS USED
C                       AS A CHECK
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE,
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS 
C                       STORED IN LSTORE(9,). (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA().  THIS 
C                       IS RETURNED FROM GFETCH (INTERNAL)
C                PLCL = VARIABLE USED TO HOLD THE PRESSURE OF THE
C                       LIFTED CONDENSATION LEVEL BEFORE THE DATA
C                       ARRAY IS FILLED (INTERNAL).
C               PRESS = VARIABLE CONTAINING THE VALUE OF THE CONSTANT
C                       PRESSURE SURFACE REQUESTED BY THE USER.  THIS
C                       IS THE LOWEST LEVEL OF THE PARCEL.  IT IS THE
C                       SAME AS IDPARS(7).
C              RKAPPA = RD/CP, VALUE IS .28573 (or 2/7) WHERE RD IS
C                       DRY AIR GAS CONSTANT, CP IS SPECIFIC HEAT OF DRY AIR
C                       AT CONSTANT PRESSURE
C                TLCL = VARIABLE USED TO HOLD THE TEMPERATURE OF THE
C                       LIFTED CONDENSATION LEVEL BEFORE THE DATA
C                       ARRAY IS FILLED (INTERNAL).
      IMPLICIT NONE
C
      INTEGER IDPARS(15),JD(4)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER ICCCFFF(2),MD(4),MT(4),MDPARS(15)
      INTEGER IER,IJ,ISO,ISTAV,J,KFILDO,KFIL10,L3264B,
     1        LITEMS,MISSP,MISSS,MISTOT,MDX,MDY,MTX,MTY,
     2        NBLOCK,ND2X3,ND5,ND7,ND9,ND10,ND11,NDATE,
     3        NFETCH,NPACK,NSLAB,NSLABD,NTIMES,NWORDS,PRESS
C
      REAL DATA(ND5)
      REAL FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),FDTK(ND2X3)
      REAL CORE(ND10)
      REAL TLCL,PLCL
      REAL, PARAMETER :: RKAPPA=.28573
C
      DATA ICCCFFF/002000,
     1             003100/
C        THE ABOVE ID'S ARE IN ORDER;
C           TEMPERATURE
C           DEWPOINT
C
      IER  =0
      ISTAV=0
 
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
 
      IF((IDPARS(1).NE.003).OR.((IDPARS(2).NE.160).AND.
     1   (IDPARS(2).NE.161))) THEN
        WRITE(12,101)(JD(J),J=1,4)
 101    FORMAT(/,' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE LCL',
     1           ' PREDICTOR. ',I9.9,2I10.9,I4.3,
     2           ' NOT COMPUTED IN LCL. ')
        IER=103
        GOTO 800
      END IF
 
C        THE LCL IS CALCULATED ONLY ON A CONSTANT 
C        PRESSURE LEVEL, HENCE ISO=1.
C        SET THE ISO INDEX TO '1' SO THAT THE METEOROLOGICAL 
C        VARIABLES WILL BE TAKEN ON ISOBARIC SURFACES.
C
      ISO=1
      PRESS=IDPARS(7)
 
C        CREATE ID FOR DEW POINT TEMPERATURE
C
      MD(1)=ICCCFFF(2)*1000+IDPARS(4)
      MD(2)=IDPARS(7)
      MD(3)=IDPARS(9)*1000000+IDPARS(12)
      MD(4)=0
 
C        CALL DEWPT TO RETURN THE 850 DEWPOINT TEMPERATURE
C        IN DATA( ).
C
      CALL PRSID1(KFILDO,MD,MDPARS) 
      CALL DEWPT(KFILDO,KFIL10,MDPARS,MD,NDATE,NGRIDC,ND11,NSLABD,
     1           IPACK,IWORK,DATA,ND2X3,LSTORE,ND9,LITEMS,CORE,ND10,
     2           NBLOCK,NFETCH,IS0,IS1,IS2,IS4,ND7,FDTK,FD2,FD3,
     3           FD4,ND2X3,ISTAV,
     4           L3264B,MISTOT,IER)

      IF(IER.NE.0)GOTO 800
      MDX=IS2(3)
      MDY=IS2(4)

C        CREATE ID FOR TEMPERATURE AT REQUESTED SURFACE
C
      MT(1)=ICCCFFF(1)*1000+IDPARS(4)
      MT(2)=IDPARS(7)
      MT(3)=IDPARS(9)*1000000+IDPARS(12)
      MT(4)=0
 
C        FETCH TEMPERATURE
C
      CALL GFETCH(KFILDO,KFIL10,MT,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDTK,ND2X3,NWORDS,
     2            NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,NFETCH,NSLAB,
     3            MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
        IF(IER.NE.0)GOTO 800

      MTX=IS2(3)
      MTY=IS2(4)

      IF(NSLABD.NE.NSLAB.OR.MTX.NE.MDX.OR.MTY.NE.MDY)THEN
        WRITE(KFILDO,200)NSLAB,NSLABD
 200    FORMAT(/,' ****THE CHARACTERISTICS OF THE TEMPERATURE GRID',
     1           ' ARE DIFFERENT FROM THE DEWPOINT GRID',I3,2X,I3)
        IER=100
        GOTO 800
      END IF
 
C     ***************************************************************
      DO 300 IJ=1,MTX*MTY
C
C        COMPUTATION OF TEMPERATURE OF THE LIFTED CONDENSATION LEVEL 
C
        TLCL = (1./(1./(DATA(IJ)-56.)+LOG(FDTK(IJ)/DATA(IJ))/800.))
     1          +56.

C        COMPUTATION OF PRESSURE OF THE LIFTED CONDENSATION LEVEL 
C
        PLCL = PRESS*(TLCL/(FDTK(IJ)))**(1./RKAPPA)

C        CONVERT PRESSURE OF LCL FROM MB TO PASCALS (*100)
C
        IF (IDPARS(2).EQ.160) THEN
C            THIS IS PRESSURE.        
          DATA(IJ)=PLCL * 100.
        ELSE IF (IDPARS(2).EQ.161) THEN
C            THIS IS TEMPERATURE.        
          DATA(IJ)=TLCL
        ENDIF

 300   CONTINUE
C
       GOTO 900
 
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
  
 800  DO 801 J=1,ND2X3
        DATA(J)=9999.
 801  CONTINUE

 900  RETURN
      END
