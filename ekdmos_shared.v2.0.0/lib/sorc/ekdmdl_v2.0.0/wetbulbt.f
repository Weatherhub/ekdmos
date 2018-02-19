      SUBROUTINE WETBULBT(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                    NGRIDC,ND11,NSLAB,IPACK,IWORK,FDTW,ND5,
     2                    LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                    IS0,IS1,IS2,IS4,ND7,
     4                    FD1,FD2,FD3,FD4,FD5,ND2X3,
     5                    ISTAV,L3264B,MISTOT,IER)
C
C        OCTOBER 1998   YAN     TDL   MOS-2000
C        MAY     2000   DALLAVALLE   USED R. ALLEN'S VERSION OF WETBULBT
C                               WHICH CORRECTED AN ERROR IN THE
C                               ALGORITHM, NAMELY, USING AN ARRAY
C                               ELEMENT INSTEAD OF A SIMPLE 
C                               VARIABLE; ALSO CONVERTED LOWER-CASE
C                               LETTERS TO UPPER-CASE.  FINALLY,
C                               ELIMINATED SOME OF THE WRITE 
C                               STATEMENTS.  FINALLY, CONVERTED
C                               FORMAT STATEMENTS TO CONFORM TO
C                               FORTRAN 90 STANDARDS ON IBM-SP
C        OCTOBER 2002   WEISS   CHANGED ND5 TO ND2X3
C        MAY     2003   GLAHN   ADDED FD5( ) TO CALL; REARRANGED CALL
C                               AND TYPE STATEMENTS; REMOVED CHANGE
C                               TO IDPARS(7); PUT FD2( ) DIRECTLY IN
C                               CALL TO GFETCH AND REMOVED TRANSFER
C                               CODE;  SET DIMENSIONS OF IPACK( ),
C                               IWORK( ), FDTW( ) TO ND5; WHITE SPACE;
C                               ADDED TO PURPOSE; ELIMINATED NSLABL
C        NOVEMBER  2003 SFANOS  ADDED CONSTANT HEIGHT CHECK TO FETCH
C                               GROUND SURFACE PRESSURE(0-30MB) AS APPOX. FOR
C                               2 METER SURFACE WHEN SURFACE PRESSURE IS 
C                               NOT AVAILABLE.  USED FOR ETA-32 WORK. 
C                               TAKEN FROM MCALOON FIXES FOR DEWPT, MIXRAT,
C                               AND SPECHUM.  
C
C
C        PURPOSE 
C            TO COMPUTE GRIDDED WET BULBT TEMPERATURE USING PRESSURE
C            (HPA FOR NGM, PA FOR ALL OTHER MODELS), TEMPERATURE
C            (DEG K), AND DEW POINT TEMPERATURE (DEG K) ON AN ISOBARIC,
C            OR A CONSTANT HEIGHT, OR A SIGMA SURFACE.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               003 110 - WET BULB TEMPERATURE ON A PRESSURE SURFACE
C               003 111 - WET BULB TEMPERATURE AT A CONSTANT HEIGHT
C               003 116 - WET BULB TEMPERATURE ON A SIGMA SURFACE
C
C        DATA SET USE 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                     (INPUT-OUTPUT)
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                     (OUTPUT) 
C 
C        VARIABLES 
C              ABSZRO = THE ABSOLUT 0 POINT ON THE TEMPERATURE SCALE
C                       (-273.15 DEG K) (INTERNAL)
C              COEFFI = COMPUTATIONAL COEFFICIENT USED IN ITERATIVE
C                       COMPUTATION OF WET BULB TEMPERATURE (INTERNAL)
C             CORE(J) = ARRAY TO STORE OR RETRIEVE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL,
C                       DATA ARE STORED ON DISK.  UPON RETURN TO THE
C                       CALLING PROGRAM, THE ARRAY WILL BE IN THE SIZE
C                       OF LX*LY (OUTPUT).
C                 CPD = SPECIFIC HEAT CAPACITY OF DRY AIR AT CONSTANT
C                       PRESSURE (1004.6 J/KGK) (INTERNAL)
C              DIFFPV = THE DIFFERENCE BETWEEN PRESSURE AND VAPOR
C                       PRESSURE (HPA) (INTERNAL)
C              DIFFTW = THE DIFFERENCE BETWEEN THE OLD VALUE AND THE NEW
C                       VALUE (AFTER ONE ITERATION) OF THE WET BULB
C                       TEMPERATURE (INTERNAL)
C              EPSILN = THE RATIO OF DRY AIR GAS CONSTANT TO WATER VAPOR
C                       GAS CONSTANT (0.622) (INTERNAL)
C                   F = WET BULBT TEMPERATURE FUNCTION (INTERNAL)
C              FD1(J) = WORK ARRAY TO HOLD DEW POINT TEMPERATURE (DEG K)
C                       (J=1,ND2X3) (INTERNAL)
C              FD2(J) = WORK ARRAY TO HOLD TEMPERATURE (DEG K)
C                       (J=1,ND2X3) (INTERNAL)
C              FD3(J) = WORK ARRAY TO HOLD PRESSURE (PA) (J=1,ND2X3)
C                       (INTERNAL)
C              FD4(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL)
C              FD5(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL)
C               FDAPH = PRESSURE CONVERTED FROM PA TO HPA (INTERNAL)
C                FDMR = MIXING RATIO (INTERNAL)
C             FDTW(J) = DATA ARRAY TO HOLD WET BULB TEMPERATURE (DEG K)
C                       (ND5) (OUTPUT)
C              FPRIME = THE FIRST DELIVERTIVE OF WET BULB TEMPERATURE
C                       FUNCTION F (INTERNAL)
C                 I,J = LOOP COUNT (INTERNAL)
C       ICCCFFF ( , ) = CONTAINS IDPARS(1) AND IDPARS(2) ID FOR THE
C                       METEOROLOGICAL FIELDS BEING FETCHED OR COMPUTED.
C                       COLUMN 1 CONTAINS ID FOR ISOBARIC SURFACE,
C                       COLUMN 2 CONTAINS ID FOR CONSTANT HEIGHT
C                       SURFACE, AND COLUMN 3 CONTAINS ID FOR SIGMA
C                       SURFACE.  ROW 1 IS FOR DEW POINT IN DEG K, ROW
C                       2 IS FOR TEMPERATURE IN DEG K, AND ROW 3 IS FOR
C                       PRESSURE IN PA OR HPA DEPENDING ON THE DATA
C                       SOURCE. (INTERNAL)
C           IDPARS(J) = PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID(J) (J=1,15) DEFINED IN
C                       THE CALLING PROGRAM (INPUT)
C                       J=1  -- CCC (CLASS OF VARIABLE)
C                       J=2  -- FFF (SUBCLASS OF VARIABLE)
C                       J=3  -- B (BINARY INDICATOR)
C                       J=4  -- DD (DATA SOURCE, MODEL NUMBER)
C                       J=5  -- V (VERTICAL APPLICATION)
C                       J=6  -- LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1
C                               LAYER)
C                       J=7  -- LTLTLTLT (TOP OF LAYER)
C                       J=8  -- T (TRANSFORMATION)
C                       J=9  -- RR (RUN TIME OFFSET -- PREVIOUS CYCLE.
C                               IT IS ALWAYS A POSITIVE NUMBER AND
C                               COUNTED BACKWARDS IN TIME.)
C                       J=10 -- OT (TIME APPLIACTION)
C                       J=11 -- OH (TIME PERIOD IN HOURS)
C                       J=12 -- TAU (PROJECTION IN HOURS)
C                       J=13 -- I (INTERPOLATION TYPE)
C                       J=14 -- S (SMOOTHING INDICATOR)
C                       J=15 -- G (GRID INDICATOR)
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                        47 = DATA NOT FOUND
C                       100 = GRID CHARACTERISTICS ARE DIFFERENT FOR THE
C                             2 FIELDS.
C                       103 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED
C                             IN THIS SUBROUTINE
C                       SEE GFETCH FOR OTHER VALUES WHEN IER.NE.0 AND
C                       DATA ARE RETURNED AS MISSING (INTERNAL-OUTPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5) (INTERNAL)
C              IS0(J) = MOS-2000 GRIB SECTION 0 IDS (J=1,3) (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 IDS (J=1,22+) (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 IDS (J=1,12)
C                       IS2(3) AND IS2(4) ARE USED BY THE CALLING
C                       PROGRAM AS GRID DIMENSION (INTERNAL-OUTPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 IDS (J=1,4) (INTERNAL)
C                 ISO = VARIABLE TO INDICATE WHICH SURFACE TO COMPUTE
C                       WET BULB TEMPERATURE ON.  (1 FOR ISOBARIC, 2 FOR
C                       CONSTANT HEIGHT, 3 FOR SIGMA SURFACE) (INTERNAL)
C               ISTAV = 0 -- WHEN THE DATA RETURNED ARE GRID DATA OR
C                            DATA ARE NOT AVAILABLE FOR RETURN
C                       1 -- WHEN THE DATA RETURNED ARE STATION DATA
C                       (OUTPUT)
C              ITERAT = ITERATION LOOP COUNT (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5) (INTERNAL)
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).  THIS IS
C                       THE SAME AS ID(J) EXCEPT THAT THE PORTIONS
C                       PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3)
C                       T = IDPARS(8)
C                       I = IDPARS(13)
C                       S = IDPARS(14)
C                       G = IDPARS(15)
C                       JD( ) IS USED TO HELP IDENTIFY THE BASIC MODEL
C                       FIELDS AS READ FROM THE ARCHIVE (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                       (INPUT)
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS DEPENDING ON THE
C                       MACHINE BEING USED (EITHER 32 OR 64) (INPUT)
C               LD(J) = WORK ARRAY HOLDING THE 4 ID WORDS OF DEW POINT
C                       TEMPERATURE DATA  BEING RETRIEVED (J=1,4)
C                       (INTERNAL)
C           LDPARS(J) = PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO LD(J) (J=1,15).  SAME AS
C                       IDPARS(J), BUT SPECIFICALLY FOR DEW POINT
C                       TEMPERATURE (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS) (INPUT-OUTPUT)
C                       L=1,4-- THE 4 IDS FOR THE DATA
C                       L=5  -- LOCATION OF STORED DATA.  WHEN IN CORE,
C                               THIS IS THE LOCATION IN CORE( ) WHERE
C                               THE DATA START.  WHEN ON DISK, THIS IS
C                               MINUS THE RECORD NUMBER WHERE THE DATA
C                               START.
C                       L=6  -- THE NUMBER OF 4-BYTE WORDS STORED
C                       L=7  -- 2 FOR DATA PACKED IN TDL GRIB FORMAT,
C                               1 OTHERWISE
C                       L=8  -- DATE/TIME OF THE DATA IN FORMAT
C                               YYYYMMDDHH
C                       L=9  -- NUMBER OF TIMES DATA HAVE BEEN RETRIEVED
C                       L=10 -- NUMBER OF THE SLAB IN DIR( , ,L) AND IN
C                               NGRIDC( ,L) DEFINING THE CHARACTERISTICS
C                               OF THE GRID
C                       L=11 -- NUMBER OF PREDICTORS IN THE SORTED LIST
C                               IN IS( ,N) (N=1,NPRED) FOR WHICH THIS
C                               VARIABLE IS NEEDED ONLY ONCE FROM
C                               LSTORE( , ).  WHEN IT IS NEEDED MORE
C                               THAN ONCE, THE VALUE IS SET TO 7777.
C                       L=12 -- USED INITIALLY IN ESTABLISHING
C                               MSTORE( , ).  LATER USED TO DETERMINE
C                               WHETHER TO KEEP THIS VARIABLE
C               LX,LY = DIMENSIONS OF THE GRID RETURNED FROM CALLING
C                       GFETCH FOR DEW POINT TEMPERATURE (INTERNAL)
C               MD(J) = WORK ARRAY HOLDING THE 4 ID WORDS OF TEMPERATURE
C                       DATA BEING RETRIEVED (J=1,4) (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       ZERO FROM CALLING GFETCH WHEN THERE IS NO
C                       MISSING VALUE (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       ZERO FROM CALLING GFETCH WHEN THERE IS NO
C                       SECONDARY MISSING VALUE (INTERNAL)
C              MISTOT = TOTAL NUMBER OF MISSING ITEMS ENCOUNTED IN
C                       UNPACKING GRIDS (INPUT-OUTPUT)
C               MX,MY = DIMENSIONS OF THE GRID RETURNED FROM CALLING
C                       GFETCH FOR TEMPERATURE (INTERNAL)
C              NBLOCK = BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM DISK
C                       FILE (INPUT)
C               ND(J) = WORK ARRAY HOLDING THE 4 ID WORDS OF PRESSURE
C                       DATA BEING RETRIEVED (J=1,4) (INTERNAL)
C               ND2X3 = DIMENSION OF FD1( ), FD2( ), FD3( ), FD4( ),
C                       AND FD5( ).  (INPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND  FDTW( ).
C                       (INPUT)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED (INPUT)
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ) (INPUT)
C                ND10 = DIMENSION OF CORE( ) (INPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN BE
C                       DEALT WITH IN THIS RUN.  LAST DIMENSION OF
C                       NGRIDC( , ) (INPUT)
C               NDATE = DATE/TIME FOR WHICH THE PREDICTOR IS NEEDED
C                       (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.  IT IS
C                       A RUNNING COUNT FROM THE BEGINNING OF THE MAIN
C                       PROGRAM.  THIS COUNT IS MAINTAINED IN CASE THE
C                       USER NEEDS IT FOR DIAGNOSTICS, ETC. (OUTPUT)
C         NGRIDC(L,M) = HOLDING THE GRID CHARACTERISTICS (L=1,6) FOR
C                       EACH GRID COMBINATION (M=1,NGRID) (INPUT-OUTPUT)
C                       L=1 -- MAP PROJECTION NUMBER (3=LAMBERT, 5=POLAR
C                              STEREOGRAPHIC)
C                       L=2 -- GRID LENGTH IN METERS
C                       L=3 -- LATITUDE AT WHICH GRID LENGTH IS CORRECT
C                              *1000
C                       L=4 -- GRID ORIENTATION IN DEGREES *1000
C                       L=5 -- LATITUDE OF LL CORNER IN DEGREES *1000
C                       L=6 -- LONGITUDE OF LL CORNER IN DEGREES *1000
C               NPACK = 2 FOR TDL GRIB PACKED DATA; 1 FOR NOT PACKED.
C                       THIS IS RETURNED FROM CALLING GFETCH (INTERNAL)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND IN
C                       NGRIDC( , ) DEFINING THE CHARACTERISTICS OF THE
C                       GRID.  IT IS USED TO IDENTIFY THE DATA SOURCE,
C                       I.E., THE MODEL. (OUTPUT)
C              NSLABM = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH FOR
C                       TEMPERATURE (INTERNAL)
C              NSLABN = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH FOR
C                       PRESSURE (INTERNAL)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT
C                       THE RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ). (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN FD2( ) AND FD3( ).
C                       THIS IS RETURNED FROM CALLING GFETCH. (INTERNAL)
C               NX,NY = DIMENSIONS OF THE GRID RETURNED FROM CALLING
C                       GFETCH FOR PRESSURE (INTERNAL)
C                PSAT = SATURATION VAPOR PRESSURE AT TRIPLE POINT
C                       (6.1078MB) (INTERNAL)
C                  RD = DRY AIR GAS CONSTANT (287.04 J/KGK)
C                  RV = WATER VAPOR GAS CONSTANT (461.5 J/KGK)
C               SIGMA = SIGMA SURFACE VALUE TAKEN FROM IDPARS(7) DIVIDED
C                       BY 1000 IF IDPARS(2) INDICATES A SIGMA SURFACE
C                       (ISO=3) (INTERNAL)
C               TWNEW = THE NEW WET BULB TEMPERATURE VALUE OBTAINED
C                       AFTER ONE ITERATIVE OPERATION (INTERNAL)
C                 VPT = VAPOR PRESSURE AT THE CURRENT TEMPERATURE
C                       (INTERNAL)
C                VPTW = VAPOR PRESSURE AT THE CURRENT WET BULB
C                       TEMPERATURE (INTERNAL)
C 
C        NON-SYSTEM SUBROUTINES USED
C            DEWPT,GFETCH,PRSID1
C
      IMPLICIT NONE
C
      REAL,PARAMETER::ABSZRO=-273.15,CPD=1004.6,PSAT=6.1078,RD=287.04,
     +     RV=461.5,EPSILN=RD/RV
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER ICCCFFF(3,3),LD(4),LDPARS(15),MD(4),ND(4)
      INTEGER I,IER,ISO,ISTAV,ITERAT,J,KFIL10,KFILDO,L3264B,LITEMS,LX,
     +        LY,MISSP,MISSS,MISTOT,MX,MY,NBLOCK,ND2X3,ND5,ND7,ND9,ND10,
     +        ND11,NDATE,NFETCH,NPACK,NSLAB,NSLABM,NSLABN,NTIMES,
     +        NWORDS,NX,NY
C
      REAL FDTW(ND5)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),FD5(ND2X3)
      REAL CORE(ND10)
      REAL COEFFI,DIFFPV,DIFFTW,F,FDAPH,FDMR,FPRIME,SIGMA,TWNEW,VPT,
     +     VPTW
C
      DATA ((ICCCFFF(I,J),J=1,3),I=1,3) /003100,003101,003106,
     +                                   002000,002001,002006,
     +                                   999999,001100,001106/
C
      IER=0
      ISTAV=0
C
C        CHECK IF THIS CODE ACCOMMODATES WET BULB TEMPERATURE
 
      IF(IDPARS(1).NE.003.OR.(IDPARS(2).NE.110.AND.
     +   IDPARS(2).NE.111.AND.IDPARS(2).NE.116))THEN
         IER=103
         WRITE(KFILDO,100)(JD(J),J=1,4),IER
 100     FORMAT(/,' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE WET ',
     1            'BULB TEMPERATURE.',
     2          /,'     PREDICTOR ',I9.9,I10.9,I10.9,I4.3,
     3            ' NOT ACCOMMODATED IN WETBULBT.  IER =',I4)
	 GOTO 900
      ENDIF
C
C        DETERMINE IF WET BULB TEMPERATURET IS TO BE COMPUTED ON AN
C        ISOBARIC, OR A CONSTANT HEIGHT, OR A SIGMA SURFACE
 
      IF (IDPARS(2).EQ.110)THEN
C           THIS IS DATA FOR A CONSTANT PRESSURE SURFACE.
         ISO=1
      ELSE IF (IDPARS(2).EQ.111)THEN  
C           THIS IS DATA FOR A CONSTANT HEIGHT SURFACE.
         ISO=2
      ELSE 
C           THIS IS DATA FOR A SIGMA SURFACE.
         ISO=3
      END IF
C 
C        TO OBTAIN DEW POINT IN COMPUTING WET BULB TEMPERATURE
C 
C        CREATE ID FOR OBTAINING DEW POINT TEMPERATURE
C
      LD(1)=ICCCFFF(1,ISO)*1000+IDPARS(4)
      LD(2)=IDPARS(7)
      LD(3)=IDPARS(9)*1000000+IDPARS(12)
      LD(4)=0
C
C        CALL PRSID1 TO PARSE IDS LD( ) TO LDPARS( )
C
      CALL PRSID1(KFILDO,LD,LDPARS)
C
C        CALL DEWPT TO OBTAIN DEW POINT TEMPERATURE IN FD1( ).
C
      CALL DEWPT(KFILDO,KFIL10,LDPARS,LD,NDATE,
     1           NGRIDC,ND11,NSLAB,IPACK,IWORK,FD1,ND2X3,
     2           LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3           IS0,IS1,IS2,IS4,ND7,
     4           FD2,FD3,FD4,FD5,ND2X3,
     5           ISTAV,L3264B,MISTOT,IER)
C
      IF(IER.NE.0)THEN
C***        WRITE(KFILDO,200)IER
C***200     FORMAT(/' ****ATTEMPT TO OBTAIN DEW POINT TEMPERATURE FAILED ',
C***    +           'IN WETBULBT.  IER =',I4)
         GOTO 900
      ENDIF
C
      LX=IS2(3)
      LY=IS2(4)
C
C        TO OBTAIN TEMPERATURE IN COMPUTING WET BULB TEMPERATURE.
C 
C        CREATE ID FOR FETCHING TEMPERATURE.
C
      MD(1)=ICCCFFF(2,ISO)*1000+IDPARS(4)
      MD(2)=IDPARS(7)
      MD(3)=IDPARS(9)*1000000+IDPARS(12)
      MD(4)=0
C
C        CALL GFETCH TO FETCH TEMPERATURE FD2( ).
C
      CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLABM,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
      IF(IER.NE.0)THEN
C***        WRITE(KFILDO,300)IER
C***300     FORMAT(/,' ****TEMPERATURE FETCH FAILED IN WETBULBT. IER =',I4)
         GOTO 900
      ENDIF
C
      MX=IS2(3)
      MY=IS2(4)
C
C        CHECK GRID CHARACTERISTICS.

      IF(NSLAB.NE.NSLABM.OR.LX.NE.MX.OR.LY.NE.MY)THEN
C           THE GRID CHARACTERISTICS ARE NOT THE SAME.
         IER=100
         WRITE(KFILDO,400)(LD(J),J=1,4),(NGRIDC(J,NSLAB),J=1,6),LX,LY,
     +                    (MD(J),J=1,4),(NGRIDC(J,NSLABM),J=1,6),MX,MY,
     +                    IER
 400     FORMAT(/,' ****DIFFERENT GRID CHARACTERISTICS.',
     +            '  PREDICTOR NOT COMPUTED IN WETBULBT.',
     +            '  VALUES FROM NGRIDC( , ) AND X*Y ARE: ',
     +            2(/,2X,3I10.9,I4.3,4X,6I10,4X,I3,'*',I3),
     +            '  IER =',I4)
         GO TO 900
      ENDIF
C
C        TO OBTAIN PRESSURE IN COMPUTING WET BULB TEMPERATURE.
 
      IF (ISO.EQ.1)THEN
C           THIS IS ON AN ISOBARIC SURFACE.  SIMPLY USE THE PRESSURE
C           FROM IDPARS(7) TO FILL UP THE FD3( ) ARRAY.
C
         DO I=1,LX*LY 
            FD3(I)=IDPARS(7) 
         ENDDO
C
      ELSE
C           THIS IS ON A CONSTANT HEIGHT (ISO=2) OR A SIGMA SURFACE
C           (ISO=3) OR BOUNDARY LAYER(ISO=4).  
C           USE GFETCH TO OBTAIN THE PRESSURE FIELD
C
         IF((ISO.EQ.2.AND.IDPARS(7).EQ.2).OR.ISO.EQ.3.OR.ISO.EQ.4)THEN
C              FETCH THE SURFACE PRESSURE WHILE KEEP THE ORIGINAL
C              IDPARS(7) IN SIGMA.  WHEN ISO=2, GROUND SURFACE PRESSURE
C              IS USED AS AN APPROXIMATION FOR THE 2 METER SURFACE;
C              WHEN ISO=3, SIGMA IS USED TO ADJUST THE PRESSURE ON THE
C              SIGMA SURFACE.
            SIGMA=FLOAT(IDPARS(7))/1000.
C
C              CREATE ID FOR FETCHING PRESSURE
C
            ND(1)=ICCCFFF(3,ISO)*1000+IDPARS(4)
            ND(2)=0
            ND(3)=IDPARS(9)*1000000+IDPARS(12)
            ND(4)=0
C
C              CALL GFETCH TO FETCH PRESSURE FD3( ).
C
            CALL GFETCH(KFILDO,KFIL10,ND,7777,LSTORE,ND9,LITEMS,
     1                  IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2                  NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3                  NBLOCK,NFETCH,NSLABN,MISSP,MISSS,L3264B,1,IER)
            IF(MISSP.NE.0)MISTOT=MISTOT+1
C
C              WHEN THE SURFACE PRESSURE IS NOT AVAILABLE IT IS
C              APPROXIMATED BY FETCHING THE 0-30MB BOUNDARY LAYER
C              PRESSURE AND ADDING 15MB (1500PA) TO IT.
C
            IF(IER.NE.0.AND.ISO.EQ.2.AND.IDPARS(7).EQ.2)THEN
C
C              CREATE ID FOR FETCHING 0-30MB BL PRESSURE
C
              MD(1)=001107*1000+IDPARS(4)
              MD(2)=000000970
              MD(3)=IDPARS(9)*1000000+IDPARS(12)
              MD(4)=0
C
C              CALL GFETCH TO FETCH 0-30MB BL PRESSURE FD2
C
              CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1                    IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2                    NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3                    NBLOCK,NFETCH,NSLABN,MISSP,MISSS,L3264B,1,IER)
              IF(MISSP.NE.0)MISTOT=MISTOT+1
C
              IF(IER.NE.0)THEN  
                 WRITE(KFILDO,500)IER
 500             FORMAT(/' ****PRESSURE FETCH FAILED IN WETBULBT.',
     1                   '  IER =',I4)
                 GOTO 900
              ELSE
                 NX=IS2(3)
                 NY=IS2(4)
                 DO I=1,MX*MY
                    FD3(I)=FD3(I)+1500.
                 ENDDO
              ENDIF 
            ENDIF
C
            NX=IS2(3)
            NY=IS2(4)
         ELSE
C              ISO=2 AND NON-2M HEIGHT CASE IS NOT ACCOMMODATED IN THIS
C              SUBROUTINE
            IER=103
            WRITE(KFILDO,600)(JD(J),J=1,4),IER
 600        FORMAT(/' ****NON-2M CASE IS NOT ACCOMMODATED IN WETBULBT.',
     +             /'     IDS PROVIDED ARE ',3I10.9,I4.3,'   IER =',I4)
	    GOTO 900
         ENDIF
C           
C           ISO=3 INDICATES IT'S ON A SIGMA SURFACE, THE PRESSURE VALUES
C           JUST FETCHED HAVE TO BE ADJUSTED TO THE DESIGNATED SIGMA
C           SURFACE.
C
         IF (ISO.EQ.3)THEN
C
            DO I=1,NX*NY
C
               IF(FD3(I).NE.9999.)THEN
                  FD3(I)=FD3(I)*SIGMA 
               ENDIF
C
            END DO
C
         ENDIF
C
      ENDIF
C
C        CHECK GRID CHARACTERISTICS FOR ISO.NE.1 (WHEN ISO=1, GFETCH IS
C        NOT CALLED, THUS THERE IS NO GRID CHARACTERISTICS)

      IF(ISO.NE.1)THEN
C
         IF(NSLAB.NE.NSLABN.OR.LX.NE.NX.OR.LY.NE.NY)THEN
C              THE GRID CHARACTERISTICS ARE NOT THE SAME.
            IER=100
            WRITE(KFILDO,700)(LD(J),J=1,4),(NGRIDC(J,NSLAB),J=1,6),
     +                 LX,LY,(ND(J),J=1,4),(NGRIDC(J,NSLABN),J=1,6),
     +                 NX,NY,IER
 700        FORMAT(/,' ****DIFFERENT GRID CHARACTERISTICS.',
     +               '  PREDICTOR NOT COMPUTED IN WETBULBT.',
     +               '  VALUES FROM NGRIDC( , ) AND X*Y ARE: ',
     +               2(/,2X,3I10.9,I4.3,4X,6I10,4X,I3,'*',I3),
     +               '  IER =',I4)
            GO TO 900
         END IF
C
      END IF
C
C           COMPUTE DEW POINT FROM DEW POINT TEMPERATURE, TEMPERATURE,
C           AND PRESSURE BY USING THE NEWTON'S ITERATIVE METHOD (REF.
C           APPLIED NUMERICAL ANALYSIS, 3 EDITION, CHAP 1, BY C.F.GERALD
C           AND P.O.WHEATLEY, ADDISON-WESLEY PUBLISHING COMPANY, 1984.
C           THE CLAUSIUS-CLAPEYRON EQN. WAS USED TO DERIVE THE RELATION-
C           SHIP BETWEEN VAPOR PRESSURE AND TEMPERATURE.)
 
      DO 850 I=1,LX*LY
C
         IF(FD1(I).NE.9999..AND.FD2(I).NE.9999..AND.FD3(I).NE.9999.)THEN
C
C              CONVERT PRESSURE IN PASCAL TO HECTOPASCAL IF NECESSARY.
C
            IF(ISO.EQ.1.OR.IDPARS(4).EQ.6)THEN
C                 PRESSURE IS ALREADY IN HECTOPASCAL
               FDAPH=FD3(I)
            ELSE
C                 PRESSURE MUST BE CONVERTED FROM PA TO HPA.
               FDAPH=FD3(I)/100.
            END IF
C
            ITERAT=0
            VPT=PSAT*EXP(-6821.8792/FD1(I)-5.1353*LOG(FD1(I))+53.7841)
            FDMR=EPSILN*VPT/(FDAPH-VPT)
            COEFFI=(2501000.-2370.*(FD2(I)+ABSZRO))/(CPD+2724.35*FDMR)
            FDTW(I)=(FD1(I)+FD2(I))/2.
 800        ITERAT=ITERAT+1
            VPTW=PSAT*EXP(-6821.8792/FDTW(I)-5.1353*LOG(FDTW(I))
     +          +53.7841)
            DIFFPV=FDAPH-VPTW
            F=FDTW(I)-FD2(I)-COEFFI*(FDMR-0.622*VPTW/DIFFPV)
            FPRIME=1.+EPSILN*COEFFI*FDAPH*VPTW*(6821.8792/FDTW(I)
     +            -5.1353)/FDTW(I)/DIFFPV/DIFFPV
            TWNEW=FDTW(I)-F/FPRIME
	    DIFFTW=TWNEW-FDTW(I)
C              UPDATE WET BULB TEMPERATURE AFTER ONE ITERATION
	    FDTW(I)=TWNEW
C
C              SET THE TORLERANCE LEVEL TO ONE TENTH OF A DEGREE
C
            IF(ABS(DIFFTW).GE.0.1)THEN
C
               IF(ITERAT.LE.5)THEN
C                    DO ANOTHER ITERATION
                  GOTO 800
               ELSE
                  WRITE(KFILDO,830)I 
 830              FORMAT(/' ****NEWTON"S ITERATION DID NOT CONVERGE AT',
     +                    ' POINT (',I4,').   WET BULB TEMPERATURE ',
     +                    'SET TO MIDPOINT BETWEEN T AND TD.')         
                  FDTW(I)=(FD1(I)+FD2(I))/2.
                  GO TO 850
               ENDIF
C
            ENDIF
C
         ELSE
C              FILL IN THE WET BULB ARRAY WITH MISSING VALUES
            FDTW(I)=9999.
         END IF
C
 850  CONTINUE
      GO TO 9999
C
C        FILL IN THE DEW POINT ARRAY WITH MISSING VALUES

 900  DO J=1,ND2X3
         FDTW(J)=9999.
      ENDDO
C
 9999 RETURN
      END      
