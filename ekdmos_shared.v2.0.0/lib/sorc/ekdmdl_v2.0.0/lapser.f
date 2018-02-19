      SUBROUTINE LAPSER(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                NGRIDC,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     2                LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                IS0,IS1,IS2,IS4,ND7,
     4                FD1,FD2,FD3,ND2X3,ISTAV,
     5                L3264B,MISTOT,IER)
C
C         APRIL 2004   SFANOS/CARROLL   TDL   MOS-2000  
C              
C        PURPOSE
C            TO COMPUTE THE LAPSE RATE IN THE LOWEST 30MB. 
C            WILL BE USED AS INPUT FOR
C            OTHER ROUTINES FOR GRIDDED MOS DEVELOPMENT.
C            FORMULA IS:
C            LASPE RATE = (AGRAV/(3000 PA * RD) * ((SURFACE PRESSURE
C                          - 3000) * (BL TEMP - 2M TEMP))/BL TEMP
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               002 770 - LAPSE RATE
C
C        DATA SET USE
C            KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT(PRINT) FILE.
C                     (OUTPUT)
C            KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                     ACCESS.(INPUT-OUTPUT)
C
C        VARIABLES
C               AGRAV = ACCELERATION OF GRAVITY IN M/S^2, WHICH
C                       IS EQUAL TO 9.0862.
C             AGRAVRD = CONSTANT THAT IS USED IN CALCULATING LAPSE
C                       RATE.  VALUE IS (AGRAV/(3000 PASCALS * RD))
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE(,) (J=1,ND10).
C                       WHEN CORE() IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C              FD1(J) = WORK ARRAY TO HOLD THE 2-M TEMPERATURE.
C                       (J=1,ND2X3). (INTERNAL)
C              FD2(J) = WORK ARRAY TO HOLD THE BL TEMPERATURE.
C                       (J=1,ND2X3). (INTERNAL)
C              FD3(J) = WORK ARRAY TO HOLD THE SURFACE PRESSURE.
C                       (J=1,ND2X3). (INTERNAL)
C             DATA(J) = DATA ARRAY TO HOLD RETURNED DATA AT
C                       GRIDPOINTS. IN THIS CASE, THE LAPSE
C                       RATE(J=1,ND5). (OUTPUT)
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
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE
C                             THE LAPSE RATE.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
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
C                       JD() IS USED TO HELP IDENTIFY THE BASIC MODEL
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
C             MBX,MBY = DIMENSIONS OF GRID RETURNED FOR
C                       SURFACE PRESSURE (INTERNAL)
C             MDTT(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD1() (J=1,4). (INTERNAL)
C             MDBL(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD2() (J=1,4). (INTERNAL)
C             MDSP(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD3() (J=1,4). (INTERNAL)
C             MDX,MDY = DIMENSIONS OF GRID RETURNED FOR 2-M
C                       TEMPERATURE (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 FROM CALLING GFETCH WHEN THERE IS NO
C                       PRIMARY MISSING VALUE. (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 FROM CALLING GFETCH WHEN THERE IS NO
C                       SECONDARY MISSING VALUE.  (INTERNAL)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C             MNX,MNY = DIMENSIONS OF GRID RETURNED FOR BOUNDARY LAYER
C                       TEMPERATURE (INTERNAL)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE. (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF
C                       THE GRID IS NOT KNOWN BEFORE FD1 AND FD2
C                       ARE FETCHED.  (INPUT)
C                 ND5 = DIMENSION OF IPACK( ), AND IWORK( ) AND
C                       FDRV( ) TO ND5.  (INPUT)
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
C              NSLABB = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR SURFACE PRESSURE. (INTERNAL)
C              NSLABD = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR BOUNDARY LAYER TEMPERATURE. (INTERNAL)
C              NSLABT = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 2-M TEMPERATURE. (INTERNAL)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE,
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS 
C                       STORED IN LSTORE(9,). (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA().  THIS 
C                       IS RETURNED FROM GFETCH (INTERNAL)
C                  RD = DRY AIR GAS CONSTANT WHICH EQUALS 287.04 
C                       J/(KG*K).
C
C     NON SYSTEM SUBROUTINES USED
C        GFETCH
C
      IMPLICIT NONE
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER ICCCFFF(3),MDTT(4),MDBL(4),MDSP(4)
      INTEGER I,IER,ISTAV,J,KFILDO,KFIL10,L3264B,
     1        LITEMS,MBX,MBY,MDX,MDY,MISSP,MISSS,
     2        MISTOT,MNX,MNY,NBLOCK,ND2X3,ND5,ND7,ND9,
     3        ND10,ND11,NDATE,NFETCH,NPACK,NSLAB,
     4        NSLABB,NSLABD,NSLABT,NTIMES,NWORDS
C
      REAL DATA(ND5)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3)
      REAL CORE(ND10)
      REAL AGRAV,RD,AGRAVRD
C
C        DECLARATIONS
C
      DATA ICCCFFF/002001,
     1             002007,
     2             001200/
      DATA RD/287.04/,AGRAV/9.8062/
C
      AGRAVRD=(AGRAV/(3000*RD))
      IER=0
      ISTAV=0
C     
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C
      IF(IDPARS(1).NE.002.OR.(IDPARS(2).NE.770))THEN
         WRITE(KFILDO,101)(JD(J),J=1,4)
 101     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1           'LAPSE RATE ',
     2          /'     PREDICTOR ',I9.9,I10.9,I10.9,I4.3,' NOT',
     3           ' ACCOMMODATED IN LAPSER.  IER =',I4)
         IER=103
         GOTO 800
      END IF
C
C        CREATE ID FOR 2-M TEMP
C
      MDTT(1)=ICCCFFF(1)*1000+IDPARS(4)
      MDTT(2)=2
      MDTT(3)=IDPARS(9)*1000000+IDPARS(12)
      MDTT(4)=0
C
C        FETCH THE 2-M TEMP USING GFETCH.
C        FD1 IS DIMENSIONED AS ND2X3 HERE.
C
      CALL GFETCH(KFILDO,KFIL10,MDTT,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3            NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GOTO 800
      MDX=IS2(3)
      MDY=IS2(4)
      NSLABT=NSLAB
C
C       CREATE ID FOR BL TEMP (30 MB)
C
      MDBL(1)=ICCCFFF(2)*1000+IDPARS(4)
      MDBL(2)=970
      MDBL(3)=IDPARS(9)*1000000+IDPARS(12)
      MDBL(4)=0
C
C        FETCH THE BOUNDARY LAYER TEMPERATURE USING GFETCH
C        FD2 IS DIMENSIONED AS ND2X3 HERE.
C
      CALL GFETCH(KFILDO,KFIL10,MDBL,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3            NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
C
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GOTO 800
      MNX=IS2(3)
      MNY=IS2(4)
      NSLABD=NSLAB
C
C        COMPARE THE GRID CHARACTERISTICS.
C
      IF(NSLABD.NE.NSLABT)THEN
        WRITE(KFILDO,140)NSLABD,NSLABT,(JD(J),J=1,4)
 140    FORMAT(/' ****THE GRID CHARACTERISTICS OF THE ',
     1          ' 2-M TEMP AND THE BOUNDARY LAYER ',
     2          ' TEMP.',I3,2X,I3,
     3         /'     VARIABLE ',I9.9,2I10.9,I4.3,
     4          ' NOT COMPUTED IN LAPSER.')
        IER=100
        GOTO 800
      END IF
C
      IF(MNX.NE.MDX.OR.MNY.NE.MDY)THEN
C        THE GRID CHARACTERISTICS ARE NOT THE SAME
        WRITE(KFILDO,145)(MDTT(J),J=1,4),(NGRIDC(J,NSLABT),J=1,6),
     1                    MDX,MDY,
     2                   (MDBL(J),J=1,4),(NGRIDC(J,NSLABD),J=1,6),
     3                    MNX,MNY
 145    FORMAT(/' ****THE GRIDS NEEDED IN LAPSER HAVE ',
     1          'DIFFERENT CHARACTERISTICS AT 145.  PREDICTOR ',
     2          'NOT COMPUTED. VALUES FROM NGRIDC(,) AND ',
     3          'MX,MY.',
     4        (/5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
      END IF
C
C        CREATE ID FOR SURFACE PRESSURE
C
      MDSP(1)=ICCCFFF(3)*1000+IDPARS(4)
      MDSP(2)=0
      MDSP(3)=IDPARS(9)*1000000+IDPARS(12)
      MDSP(4)=0
C
C        FETCH THE SURFACE PRESSURE USING GFETCH
C        FD3 IS DIMENSIONED AS ND2X3 HERE.  SURFACE PRESSURE
C        IS ASSUMED TO BE STATION PRESSURE HERE.
C
      CALL GFETCH(KFILDO,KFIL10,MDSP,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3            NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GOTO 800
      MBX=IS2(3)
      MBY=IS2(4)
      NSLABB=NSLAB
C
C        COMPARE THE GRID CHARACTERISTICS.
C
      IF(NSLABB.NE.NSLABD)THEN
        WRITE(KFILDO,150)NSLABD,NSLABB,(JD(J),J=1,4)
 150    FORMAT(/' ****THE GRID CHARACTERISTICS OF THE ',
     1          ' STATION PRESSURE AND THE BOUNDARY LAYER ',
     2          ' TEMP.',I3,2X,I3,
     3         /'     VARIABLE ',I9.9,2I10.9,I4.3,
     4          ' NOT COMPUTED IN LAPSER.')
        IER=100
        GOTO 800
      END IF
C
      IF(MNX.NE.MBX.OR.MNY.NE.MBY)THEN
C        THE GRID CHARACTERISTICS ARE NOT THE SAME
        WRITE(KFILDO,155)(MDSP(J),J=1,4),(NGRIDC(J,NSLABB),J=1,6),
     1                    MBX,MBY,
     2                   (MDBL(J),J=1,4),(NGRIDC(J,NSLABD),J=1,6),
     3                    MNX,MNY
 155    FORMAT(/' ****THE GRIDS NEEDED IN LAPSER HAVE ',
     1          'DIFFERENT CHARACTERISTICS AT 145.  PREDICTOR ',
     2          'NOT COMPUTED. VALUES FROM NGRIDC(,) AND ',
     3          'MX,MY.',
     4        (/5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
      END IF
C
C        BEGIN COMPUTATION OF THE LAPSE RATE
C        3000 PA IS USED HERE AS A APPROXIMATION SINCE WE ASSUME
C        THE TEMPERATURE IS VALID AT THE TOP OF THE BLAYER.
C
      DO I=1,MNX*MNY
        IF((NINT(FD1(I)).EQ.9999).OR.(NINT(FD2(I)).EQ.9999).OR.
     1     (NINT(FD3(I)).EQ.9999))THEN
          DATA(I)=9999.
        ELSE
          DATA(I)=AGRAVRD*(((FD3(I)-3000)*(FD2(I)-FD1(I)))/FD2(I))
C          WRITE(19,1165)DATA(I), AGRAVRD, FD3(I), FD2(I), FD1(I)
C 1165     FORMAT(2(F10.7,2X),F9.2,1X,2(F7.2,2X))
        END IF
      END DO
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
