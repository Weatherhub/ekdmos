      SUBROUTINE SPECHUM(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                   NGRIDC,ND11,NSLAB,IPACK,IWORK,FDSH,ND5,
     2                   LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                   IS0,IS1,IS2,IS4,ND7,
     4                   FD1,FD2,FD3,ND2X3,
     5                   ISTAV,L3264B,MISTOT,IER)
C
C        AUGUST    1997   FIEBRICH TDL  MOS-2000
C        SEPTEMBER 1998   YAN      PROGRAM RESTRUCTURED.
C                                  ADDED 2M AND SIGMA SURFACES.
C        DECEMBER  2000   RUDACK   MODIFIED CODE TO CONFORM WITH MDL
C                                  FORMAT SPECIFICATIONS
C        OCTOBER   2002   WEISS    CHANGED ND5 TO ND2X3
C        MAY       2003   GLAHN    REARRANBED LINES IN CALL;  CHANGED
C                                  DIMENSIONS OF IPACK( ), IWORK( ),
C                                  ADN FDSH( ) TO ND5; REMOVED CHANGE
C                                  TO IDPARS(7); REARRANGED TYPE 
C                                  STATEMENTS; IMPROVED DIAGNOSTICS
C        DECEMBER  2003   MCALOON  MADE CHANGE TO ESTIMATE SURFACE
C                                  PRESSURE FROM BL PRESSURE TO 
C                                  ESTIMATE 2M DEWPOINT
C
C        PURPOSE 
C            TO COMPUTE GRIDDED SPECIFIC HUMIDITY USING PRESSURE (HPA
C            FOR NGM, PA FOR ALL OTHER MODELS), TEMPERATURE (DEG K), AND
C            RELATIVE HUMIDITY (%) ON AN ISOBARIC, OR A CONSTANT HEIGHT,
C            OR A SIGMA SURFACE.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C                003 030 - SPECIFIC HUMIDITY ON PRESSURE SFC
C                003 031 - SPECIFIC HUMIDITY ON HEIGHT SFC
c                          WORKS ONLY FOR 2-M HEIGHT
C                003 036 - SPECIFIC HUMIDITY ON SIGMA SFC
C
C        DATA SET USE 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                     (INPUT-OUTPUT)
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                     (OUTPUT) 
C 
C        VARIABLES 
C              ABSZRO = THE ABSOLUTE 0 POINT ON THE TEMPERATURE SCALE
C                       (-273.15 DEG K) (INTERNAL)
C             CORE(J) = ARRAY TO STORE OR RETRIEVE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL,
C                       DATA ARE STORED ON DISK.  UPON RETURN TO THE
C                       CALLING PROGRAM, THE ARRAY WILL BE IN THE SIZE
C                       OF LX*LY (OUTPUT).  
C              EPSILN = THE RATIO OF DRY AIR GAS CONSTANT TO WATER VAPOR
C                       GAS CONSTANT (0.622) (INTERNAL)
C              FD1(J) = WORK ARRAY TO HOLD PRESSURE (PA) (J=1,ND2X3)
C                       (INTERNAL) 
C              FD2(J) = WORK ARRAY TO HOLD TEMPERATURE (DEG K)
C                       (J=1,ND2X3) (INTERNAL)
C              FD3(J) = WORK ARRAY TO HOLD RELATIVE HUMIDITY (%)
C                       (J=1,ND2X3) (INTERNAL)
C               FDAPH = PRESSURE CONVERTED FROM PA TO HPA (INTERNAL)
C                FDMR = MIXING RATIO (G/G) (INTERNAL)
C             FDSH(J) = DATA ARRAY TO HOLD SPECIFIC HUMIDITY (G/G)
C                       (ND5) (OUTPUT)
C               FDSMR = SATURATION MIXING RATIO (G/G) (INTERNAL)
C               FDSVP = SATURATION VAPOR PRESSURE (HPA) (INTERNAL)
C                FDTC = AIR TEMPERATURE CONVERTED FROM DEGREE KELVIN TO
C                       DEGREE CELSIUS (INTERNAL)
C                 I,J = LOOP COUNT (INTERNAL)
C       ICCCFFF ( , ) = CONTAINS IDPARS(1) AND IDPARS(2) ID FOR THE
C                       METEOROLOGICAL FIELDS BEING FETCHED OR COMPUTED.
C                       COLUMN 1 CONTAINS ID FOR ISOBARIC SURFACE,
C                       COLUMN 2 CONTAINS ID FOR CONSTANT HEIGHT SURFACE
C                       AND COLUMN 3 CONTAINS ID FOR SIGMA SURFACE.  ROW
C                       1 IS FOR PRESSURE IN PA OR HPA DEPENDING ON THE
C                       DATA SOURCE, ROW 2 IS FOR TEMPERATURE IN DEGREE
C                       K, AND ROW 3 IS FOR RELATIVE HUMIDITY IN %.
C                       (INTERNAL)
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
C                       SPECIFIC HUMIDITY ON.  (1 FOR ISOBARIC, 2 FOR
C                       CONSTANT HEIGHT, 3 FOR SIGMA SURFACE) (INTERNAL)
C               ISTAV = 0 -- WHEN THE DATA RETURNED ARE GRID DATA OR
C                            DATA ARE NOT AVAILABLE FOR RETURN
C                       1 -- WHEN THE DATA RETURNED ARE STATION DATA
C                       (OUTPUT)
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
C               LD(J) = WORK ARRAY HOLDING THE 4 ID WORDS OF PRESSURE
C                       DATA BEING RETRIEVED (J=1,4) (INTERNAL)
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
C                       GFETCH FOR PRESSURE (INTERNAL)
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
C                       GFETCH FOR FD2 (INTERNAL)
C              NBLOCK = BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM DISK
C                       FILE (INPUT)
C               ND(J) = WORK ARRAY HOLDING THE 4 ID WORDS OF RELATIVE
C                       HUMIDITY DATA BEING RETRIEVED INTO FD3( )
C                       (J=1,4) (INTERNAL)
C               ND2X3 = DIMENSION OF FD1( ), FD2( ), AND FD3( ) (INPUT)
C                 ND5 = FORMER DIMENSION OF IPACK( ), IWORK( ),
C                       AND FDSH( ) (INPUT)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED. (INPUT)
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
C               NPACK = 2 FOR TDL GRIB PACKED DATA; 1 OTHERWISE.  THIS
C                       IS RETURNED FROM CALLING GFETCH (INTERNAL)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND IN
C                       NGRIDC( , ) DEFINING THE CHARACTERISTICS OF THE
C                       GRID.  IT IS USED TO IDENTIFY THE DATA SOURCE,
C                       I.E., THE MODEL. (OUTPUT)
C              NSLABL = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH FOR
C                       PRESSURE (INTERNAL)
C              NSLABM = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH FOR
C                       TEMPERATURE (INTERNAL)
C              NSLABN = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH FOR
C                       RELATIVE HUMIDITY (INTERNAL)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT
C                       THE RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ) (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN FD1( ), FD2( ), AND
C                       FD3( ).  THIS IS RETURNED FROM CALLING GFETCH.
C                       (INTERNAL)
C               NX,NY = DIMENSION OF THE GRID RETURNED FROM CALLING
C                       GFETCH FOR RELATIVE HUMIDITY (INTERNAL)
C                PSAT = SATURATION VAPOR PRESSURE AT TRIPLE POINT
C                       (6.1078MB) (INTERNAL)
C                  RD = DRY AIR GAS CONSTANT (287.04 J/KGK)
C                  RV = WATER VAPOR GAS CONSTANT (461.5 J/KGK)
C               SIGMA = SIGMA SURFACE VALUE TAKEN FROM IDPARS(7) DIVIDED
C                       BY 1000 IF IDPARS(2) INDICATES A SIGMA SURFACE
C                       (ISO=3) (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NON-SYSTEM SUBROUTINES USED
C            GFETCH
C
      IMPLICIT NONE
C
      REAL,PARAMETER::ABSZRO=-273.15,PSAT=6.1078,RD=287.04,RV=461.5,
     1     EPSILN=RD/RV
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER ICCCFFF(3,3),LD(4),MD(4),ND(4)
      INTEGER I,IER,ISO,ISTAV,J,KFIL10,KFILDO,L3264B,LITEMS,LX,LY,MISSP,
     1        MISSS,MISTOT,MX,MY,NBLOCK,ND2X3,ND5,ND7,ND9,ND10,ND11,
     2        NDATE,NFETCH,NPACK,NSLAB,NSLABL,NSLABM,NSLABN,NTIMES,
     3        NWORDS,NX,NY
C
      REAL FDSH(ND5)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3)
      REAL CORE(ND10)
      REAL FDAPH,FDMR,FDSMR,FDSVP,FDTC,SIGMA
C
      DATA ((ICCCFFF(I,J),J=1,3),I=1,3) /999999,001100,001106,
     1                                   002000,002001,002006,
     2                                   003000,003001,003006/
C
      IER=0
      ISTAV=0
C
C        CHECK IF THIS CODE ACCOMMODATES SPECIFIC HUMIDITY
 
      IF(IDPARS(1).NE.003.OR.(IDPARS(2).NE.030.AND.
     1   IDPARS(2).NE.031.AND.IDPARS(2).NE.036))THEN
         IER=103
         WRITE(KFILDO,100)(JD(J),J=1,4),IER
 100     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1           'SPECIFIC  HUMIDITY.',
     2          /'     PREDICTOR ',I9.9,I10.9,I10.9,I4.3,
     3           'NOT ACCOMMODATED IN SPECHUM.  IER =',I4)
	 GOTO 900
      ENDIF
C
C        DETERMINE IF SPECIFIC HUMIDITY IS TO BE COMPUTED ON AN
C        ISOBARIC, A CONSTANT HEIGHT, OR A SIGMA SURFACE
 
      IF(IDPARS(2).EQ.030)THEN
C           THIS IS DATA FOR A CONSTANT PRESSURE SURFACE.
         ISO=1
      ELSE IF (IDPARS(2).EQ.031)THEN  
C           THIS IS DATA FOR A CONSTANT HEIGHT SURFACE.
         ISO=2
      ELSE 
C           THIS IS DATA FOR A SIGMA SURFACE.
         ISO=3
      END IF
C 
C        TO OBTAIN PRESSURE IN COMPUTING SPECIFIC HUMIDITY
 
      IF (ISO.EQ.1)THEN
C
C           THIS IS ON AN ISOBARIC SURFACE.  SIMPLY USE THE PRESSURE
C           FROM IDPARS(7) TO FILL UP THE FD1( ) ARRAY.  DON'T HAVE
C           A GRID SIZE YET, SO CAN'T USE NX*NY.
C
         DO I=1,ND2X3 
            FD1(I)=IDPARS(7) 
         ENDDO
      ELSE
C           THIS IS ON A CONSTANT HEIGHT (ISO=2) OR A SIGMA SURFACE
C           (ISO=3).  USE GFETCH TO OBTAIN THE PRESSURE FIELD
C
         IF((ISO.EQ.2.AND.IDPARS(7).EQ.2).OR.ISO.EQ.3)THEN
C
C              FETCH THE SURFACE PRESSURE.  WHEN ISO=2, GROUND SURFACE
C              PRESSURE IS USED AS AN APPROXIMATION FOR 2 METER HEIGHT
C              SURFACE; WHEN ISO=3, SIGMA IS USED TO ADJUST THE 
C              PRESSURE ON THE SIGMA SURFACE.
C
            SIGMA=FLOAT(IDPARS(7))/1000.
            IDPARS(7)=0
C     
C              CREATE ID FOR FETCHING PRESSURE
            LD(1)=ICCCFFF(1,ISO)*1000+IDPARS(4)
            LD(2)=IDPARS(7)
            LD(3)=IDPARS(9)*1000000+IDPARS(12)
            LD(4)=0
C
C              CALL GFETCH TO FETCH PRESSURE FD1
C
            CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1                  IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2                  NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3                  NBLOCK,NFETCH,NSLABL,MISSP,MISSS,L3264B,1,IER)
            IF(MISSP.NE.0)MISTOT=MISTOT+1
C              SET IDPARS(7) BACK TO ITS ORIGINAL VALUE SO THAT THE ID
C              IS NOT MESSED UP WHEN GFETCH IS CALLED THE NEXT TIME
            IDPARS(7)=INT(SIGMA*1000.)
C
            IF(IER.NE.0.AND.ISO.EQ.2.AND.IDPARS(7).EQ.2)THEN
C
C              WHEN THE SURFACE PRESSURE IS NOT AVAILABLE THE 0-30MB
C              BOUNDARY LAYER PRESSURE IS APPROXIMATED AS THE SURFACE
C              PRESSURE BY ADDING 15MB (1500PA) TO IT.
C     
C              CREATE ID FOR FETCHING 0-30MB BLPRESSURE
C
              LD(1)=ICCCFFF(1,ISO)*1000+IDPARS(4)
              LD(2)=IDPARS(7)
              LD(3)=IDPARS(9)*1000000+IDPARS(12)
              LD(4)=0
C
C              CALL GFETCH TO FETCH BL PRESSURE FD1
C
              CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1                    IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2                    NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3                    NBLOCK,NFETCH,NSLABL,MISSP,MISSS,L3264B,1,IER)
              IF(MISSP.NE.0)MISTOT=MISTOT+1
C
              IF(IER.NE.0)THEN
	         WRITE(KFILDO,150)(JD(J),J=1,4),IER
 150             FORMAT(/' ****PRESSURE FETCH FAILED IN SPECHUM.',
     1                 '  PREDICTOR ',I9.9,I10.9,I10.9,I4.3,
     2                 ' NOT COMPUTED.  IER =',I4)
	        GOTO 900
              ELSE
                LX=IS2(3)
                LY=IS2(4)
                DO I=1,LX*LY
                 FD1(I)=FD1(I)+1500.
                ENDDO
              ENDIF
            ENDIF
C
            LX=IS2(3)
            LY=IS2(4)
         ELSE
C              ISO=2 AND NON-2M HEIGHT CASE IS NOT ACCOMMODATED IN THIS
C              SUBROUTINE
            IER=103
            WRITE(KFILDO,200)(JD(J),J=1,4),IER
 200        FORMAT(/' ****NON-2M CASE IS NOT ACCOMMODATED IN SPECHUM.',
     1              '  PREDICTOR ',I9.9,I10.9,I10.9,I4.3,
     2              ' NOT COMPUTED.  IER =',I4)
	    GOTO 900
         ENDIF
C           
C           ISO=3 INDICATES IT'S ON A SIGMA SURFACE, THE PRESSURE VALUES
C           JUST FETCHED HAVE TO BE ADJUSTED TO THE DESIGNATED SIGMA
C           SURFACE.
 
         IF (ISO.EQ.3)THEN
C
            DO I=1,LX*LY
C
               IF(FD1(I).NE.9999.)THEN
                  FD1(I)=FD1(I)*SIGMA
               ENDIF
C
            END DO
C
         ENDIF
C
      ENDIF
C
C        TO OBTAIN TEMPERATURE IN COMPUTING SPECIFIC HUMIDITY
 
C        CREATE ID FOR FETCHING TEMPERATURE
C
      MD(1)=ICCCFFF(2,ISO)*1000+IDPARS(4)
      MD(2)=IDPARS(7)
      MD(3)=IDPARS(9)*1000000+IDPARS(12)
      MD(4)=0
C
C        CALL GFETCH TO FETCH TEMPERATURE FD2
C
      CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLABM,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)THEN
         WRITE(KFILDO,250)(JD(J),J=1,4),IER
 250     FORMAT(/' ****TEMPERATURE FETCH FAILED IN SPECHUM.',
     1           '  PREDICTOR ',I9.9,I10.9,I10.9,I4.3,
     2           ' NOT COMPUTED.  IER =',I4)
         GOTO 900
      ENDIF
C
      MX=IS2(3)
      MY=IS2(4)
C
C        CHECK GRID CHARACTERISTICS FOR ISO.NE.1. (WHEN ISO=1, GFETCH IS
C        NOT CALLED, THUS THERE ARE NO GRID CHARACTERISTICS.)

      IF(ISO.NE.1)THEN
C
         IF(NSLABL.NE.NSLABM.OR.LX.NE.MX.OR.LY.NE.MY)THEN
C              THE GRID CHARACTERISTICS ARE NOT THE SAME.
            IER=100
            WRITE(KFILDO,300)(LD(J),J=1,4),(NGRIDC(J,NSLABL),J=1,6),
     1                        LX,LY,
     2                       (MD(J),J=1,4),(NGRIDC(J,NSLABM),J=1,6),
     3                        MX,MY,IER
 300        FORMAT(/' ****DIFFERENT GRID CHARACTERISTICS.  PREDICTOR ',
     1              'NOT COMPUTED IN SPECHUM.  VALUES FROM NGRIDC( , )',
     2              ' AND X*Y ARE:',
     3              2(/,4X,3I10.9,I4.3,4X,6I10,4X,I3,'*',I3),
     4              '  IER =',I4)
            GOTO 900
         END IF
C
      END IF
C
C        TO OBTAIN RELATIVE HUMIDITY REQUIRED IN COMPUTING SPECIFIC
C        HUMIDITY

C        CREATE ID FOR FETCHING RELATIVE HUMIDITY
c
      ND(1)=ICCCFFF(3,ISO)*1000+IDPARS(4)
      ND(2)=IDPARS(7)
      ND(3)=IDPARS(9)*1000000+IDPARS(12)
      ND(4)=0
C
C        CALL GFETCH TO FETCH RELATIVE HUMIDITY FD3
C
      CALL GFETCH(KFILDO,KFIL10,ND,7777,LSTORE,ND9,LITEMS,IS0,IS1,IS2,
     1            IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,
     3            CORE,ND10,NBLOCK,NFETCH,NSLABN,MISSP,MISSS,L3264B,1,
     4            IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)THEN
         WRITE(KFILDO,350)(JD(J),J=1,4),IER
 350     FORMAT(/' ****RELATIVE HUMIDITY FETCH FAILED IN SPECHUM.',
     1           '  PREDICTOR ',I9.9,I10.9,I10.9,I4.3,
     2           ' NOT COMPUTED.  IER =',I4)
         GOTO 900
      ENDIF
C
      NX=IS2(3)
      NY=IS2(4)
C
C        MAKE SURE THE RELATIVE HUMIDITY VALUES ARE WITHIN THE RANGE
C        2 TO 100.
C
      DO I=1,NX*NY
	 IF(FD3(I).GT.100.)FD3(I)=100.
	 IF(FD3(I).LT.2.)FD3(I)=2.
      ENDDO
C
C        CHECK GRID CHARACTERISTICS

      IF(NSLABM.NE.NSLABN.OR.MX.NE.NX.OR.MY.NE.NY)THEN
C           THE GRID CHARACTERISTICS ARE NOT THE SAME
         IER=100
         WRITE(KFILDO,400)(MD(J),J=1,4),(NGRIDC(J,NSLABM),J=1,6),MX,MY,
     1                    (ND(J),J=1,4),(NGRIDC(J,NSLABN),J=1,6),NX,NY,
     2                    IER
 400     FORMAT(/' ****DIFFERENT GRID CHARACTERISTICS.  PREDICTOR NOT ',
     1           'COMPUTED IN SPECHUM.  VALUES FROM NGRIDC( , ) AND ',
     2           'X*Y ARE: ',2(/4X,3I10.9,I4.3,4X,6I10,4X,I3,'*',I3),
     3           '  IER =',I4)
         GOTO 900
      END IF
C
C        ASSIGN VALUE TO NSLAB TO BE PASSED BACK TO THE CALLING PROGRAM
C
      NSLAB=NSLABN
C
C        COMPUTE MIXING RATIO THEN SPECIFIC HUMIDITY

      DO 500 I=1,NX*NY
C
         IF(FD1(I).NE.9999..AND.FD2(I).NE.9999..AND.FD3(I).NE.9999.)THEN
C
C              USE TETEN-STACKPOLE APPROXIMATION (JAM, VOL 6, P. 465) TO
C              COMPUTE SATURATION VAPOR PRESSURE FROM TEMPERATURE 
C
            FDTC=FD2(I)+ABSZRO
            FDSVP=PSAT*EXP(17.269*FDTC/(237.3+FDTC))
C
C              COMPUTE SATURATION MIXING RATIO FROM THE SATURATION VAPOR
C              PRESSURE BY AN ALGEBRAIC EXPRESSION (BEYERS EQN 8-11).
C              CONVERT PRESSURE TO HECTOPASCAL IF NECESSARY
C
            IF(ISO.EQ.1.OR.IDPARS(4).EQ.6)THEN
C                 PRESSURE IS ALREADY IN HPA BECAUSE (1) THIS IS AN
C                 ISOBARIC SURFACE OR (2) THIS IS PRESSURE DATA FROM THE
C                 NGM MODEL
               FDAPH=FD1(I)
            ELSE
C                 PRESSURE MUST BE CONVERTED FROM PA TO HPA
               FDAPH=FD1(I)/100.
            END IF
C
            FDSMR=EPSILN*FDSVP/(FDAPH-FDSVP)
C              COMPUTE MIXING RATIO FROM THE RELATIVE HUMIDITY AND THE 
C              SATURATION MIXING RATIO (BEYERS EQN 8-13)
            FDMR=FDSMR*FD3(I)/100.
C              CONPUTE SPECIFIC HUMIDITY FROM THE MIXING RATIO
	    FDSH(I)=FDMR/(1.+FDMR)
         ELSE
C              FILL IN THE SPECIFIC HUMIDITY ARRAY WITH MISSING VALUES
            FDSH(I)=9999.
         END IF
C
 500  CONTINUE
C
      GOTO 999
C
C        FILL IN THE SPECIFIC HUMIDITY ARRAY WITH MISSING VALUES

 900  DO J=1,ND2X3
         FDSH(J)=9999.
      ENDDO
C
 999  RETURN
      END
