      SUBROUTINE LINDEX(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                  NGRIDC,ND11,NSLAB,IPACK,IWORK,FDLI,ND5,
     2                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                  IS0,IS1,IS2,IS4,ND7,
     4                  FDTK,FDRH,FDTL,FDPL,FDTK5,ND2X3,
     5                  ISTAV,L3264B,MISTOT,IER)
C
C        JUNE      1999   HUGHES   TDL   MOS-2000
C        MAY       2000   HUGHES   MODIFIED FROM THE WORKSTATION
C                                  VERSION TO REPLACE INLINE 
C                                  FUNCTION STATEMENTS WITH EXTERNAL
C                                  FUNCTIONS LOCATED AT THE END OF
C                                  THE SUBROUTINE TO MAKE IT FORTRAN90
C                                  COMPLIANT ON THE IBM
C        DECEMBER  2000   RUDACK   MODIFIED CODE TO CONFORM WITH MDL 
C                                  FORMAT SPECIFICATIONS
C        DECEMBER  2002   WEISS    CHANGED ND5 TO ND2X3
C        MAY       2003   GLAHN    REARRANGED LINES IN CALL; CHANGED
C                                  DIMENSIONS OF IPACK( ), WORK( )
C                                  AND FDLI( ) TO ND5; PUT ICCCFFF( )
C                                  IN DATA STATEMENT; ELIMINATED FD3( ),
C                                  FD4( ), FDMR( ), DATA( ); ELIMINATED
C                                  INITIALIZATION OF FDLI( ); REARRANGED
C                                  CALLS TO LCL AND GFETCH; MADE INTEGER
C                                  VARIABLES FPRES AND PRESS IFPRES AND
C                                  IPRESS
C        1         2         3         4         5         6         7 X
C
C        PURPOSE
C            TO COMPUTE THE LIFTED INDEX.  THE USER SUPPLIES
C            THE STARTING PRESSURE LEVEL IN IDPARS(7).
C          
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               007 020 - LIFTED INDEX (FROM PRESSURE LEVEL IDPARS(7))
C            
C           DATA SET USE
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. 
C                       (OUTPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                       ACCESS. (INPUT-OUTPUT)
C
C        VARIABLES
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE(,) (J=1,ND10).
C                       WHEN CORE() IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C             FDLI(K) = VARIABLE USED TO HOLD THE LIFTED INDEX 
C                       (OUTPUT).
C             FDPL(K) = VARIABLE USED TO HOLD THE PRESSURE OF THE
C                       LIFTED CONDENSATION LEVEL.
C             FDRH(K) = WORK ARRAY TO HOLD THE RELATIVE HUMIDITY
C                       (K=1,ND2X3). (INTERNAL)
C             FDTK(K) = WORK ARRAY TO HOLD THE AIR TEMPERATURE IN 
C                       KELVIN (K=1,ND2X3). (INTERNAL)
C            FDTK5(K) = WORK ARRAY TO HOLD THE 500 MB TEMPERATURE IN 
C                       KELVIN (K=1,ND2X3). (INTERNAL)
C             FDTL(K) = WORK ARRAY TO HOLD THE TEMPERATURE OF THE
C                       LIFTED CONDENSATION LEVEL (INTERNAL)
C                   I = LOOP CONTROL VARIABLE
C           ICCCFFF() = CONTAINS IDPARS(1), IDPARS(2) AND IDPARS(3) ID 
C                       FOR THE TEMPERATURE, THE TEMPERATURE OF THE
C                       LIFTED CONDENSATION LEVEL, AND THE MIXING
C                       RATIO.
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
C                   K = LOOP CONTROL VARIABLE
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
C                              MOSTORE(,). LATER USED AS A WAY OF
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
C                       THE GRID IS NOT KNOWN BEFORE FDTK AND FDPT
C                       ARE FETCHED.  (INPUT)
C                 ND5 = FORMER DIMENSION OF IPACK(), AND IWORK(). 
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
C              IPRESS = VARIABLE CONTAINING THE VALUE OF THE CONSTANT
C                       PRESSURE SURFACE REQUESTED BY THE USER.  THIS
C                       IS THE LOWEST LEVEL OF THE PARCEL.  IT IS THE
C                       SAME AS IDPARS(7).
C              RKAPPA = RD/CP, VALUE IS .28573 (OR 2/7) WHERE RD IS
C                       DRY AIR GAS CONSTANT, CP IS SPECIFIC HEAT OF DRY AIR
C                       AT CONSTANT PRESSURE
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED 
C          GFETCH, PRSID1, LCL, 
C
C        FUNCTIONS INCLUDED WITH THIS SUBROUTINE
C             FTHETAE = FUNCTION TO CALCULATE THE EQUIVALENT POTENTIAL TEMPERATURE
C               FMIXR = FUNCTION TO CALCULATE THE MIXING RATIO
C                FSMR = FUNCTION TO CALCULATE THE SATURATION MIXING RATIO
C                FSVP = FUNCTION TO CALCULATE THE SATURATION VAPOR PRESSURE
C
      IMPLICIT NONE
C
      REAL,EXTERNAL :: FTHETAE,FMIXR,FSMR,FSVP
C
      REAL, PARAMETER :: RKAPPA=.28573,ABSZRO=-273.15,PSAT=6.1078
      REAL, PARAMETER :: CP=.24,RD=287.04,RV=461.5,EPSILN=RD/RV
C        CP DOESN'T SEEM TO BE USED.
C
      INTEGER IDPARS(15),JD(4)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER ICCCFFF(5),MT(4),ML(4),MP(4),MLPARS(15),MPPARS(15)
      INTEGER IER,K,ISTAV,J,I,KFILDO,KFIL10,L3264B,LITEMS,ITER,
     1        MISSP,MISSS,MISTOT,MLX,MLY,MTX,MTY,
     2        MPX,MPY,NSLABT5,MT5X,MT5Y,NSLABRH,MRHX,MRHY,
     3        NBLOCK,ND2X3,ND5,ND7,ND9,ND10,ND11,NDATE,NSLABP,
     4        NFETCH,NPACK,NSLAB,NSLABT,NTIMES,NWORDS,IPRESS,IFPRES
C
      REAL FDLI(ND5)
      REAL FDTL(ND2X3),FDTK(ND2X3),FDTK5(ND2X3),FDPL(ND2X3),
     1     FDRH(ND2X3)
      REAL CORE(ND10)
      REAL A,ANXT,EPS,DELTA,FDTC,TGUESSC,TGUESNXC,PPP,FRH
      REAL THETA,POTTMP,TGUESS,ASTORE,TGUESNXT,GUESSLI,FDTLC,FPLCL
C
      DATA ICCCFFF/002000,
     1             003161,
     2             003010,
     3             003160,
     4             003000/ 
C        THE ABOVE ID'S ARE IN ORDER:
C           TEMPERATURE ON AN ISOBARIC SURFACE
C           TEMPERATURE OF LCL (STARTING FROM PRESSURE IDPARS(7))
C           MIXING RATIO ON AN ISOBARIC SURFACE (NOT USED)
C           PRESSURE OF LCL (STARTING FROM PRESSURE IDPARS(7))
C           RELATIVE HUMIDITY ON AN ISOBARIC SURFACE
C
      IER=0
      ISTAV=0
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C
      IF((IDPARS(1).NE.007).OR.(IDPARS(2).NE.020)) THEN
        WRITE(KFILDO,200)(JD(J),J=1,4)
 200    FORMAT(/,' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE LIFTED',
     1           ' INDEX.  PREDICTOR ',I9.9,2I10.9,I4.3,
     2           ' NOT COMPUTED IN LINDEX.')
        IER=103
        GOTO 800
      END IF
C 
      IPRESS=IDPARS(7)
C 
C        CREATE ID FOR TEMPERATURE AT LIFTED CONDENSATION LEVEL
C
      ML(1)=ICCCFFF(2)*1000+IDPARS(4)
      ML(2)=IDPARS(7)
      ML(3)=IDPARS(9)*1000000+IDPARS(12)
      ML(4)=0
C 
C        GET TEMPERATURE OF THE LCL.
C
      CALL PRSID1(KFILDO,ML,MLPARS)
      CALL LCL(KFILDO,KFIL10,MLPARS,JD,NDATE,
     1         NGRIDC,ND11,NSLAB,IPACK,IWORK,FDTL,ND5,
     2         LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3         IS0,IS1,IS2,IS4,ND7,
     4         FDTK,FDTK5,FDRH,FDLI,ND2X3,
     5         ISTAV,L3264B,MISTOT,IER)
      IF(IER.NE.0)GOTO 800
C
      MLX=IS2(3)
      MLY=IS2(4)
C
C        CREATE ID FOR PRESSURE AT LIFTED CONDENSATION LEVEL
C
      MP(1)=ICCCFFF(4)*1000+IDPARS(4)
      MP(2)=IDPARS(7)
      MP(3)=IDPARS(9)*1000000+IDPARS(12)
      MP(4)=0
C 
C        GET PRESSURE OF THE LIFTED CONDENSATION LEVEL.
C
      CALL PRSID1(KFILDO,MP,MPPARS)
      CALL LCL(KFILDO,KFIL10,MPPARS,JD,NDATE,
     1         NGRIDC,ND11,NSLABP,IPACK,IWORK,FDPL,ND5,
     2         LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3         IS0,IS1,IS2,IS4,ND7,
     4         FDTK,FDTK5,FDRH,FDLI,ND2X3,
     5         ISTAV,L3264B,MISTOT,IER)
C
      IF(IER.NE.0) THEN
C***        WRITE(KFILDO,350)IER
C*** 350    FORMAT(/' ****LCL PRESSURE FAILED IN LINDEX.  IER =',I3)
      GOTO 800
      END IF
C
      MPX=IS2(3)
      MPY=IS2(4)
C
      IF(NSLABP.NE.NSLAB.OR.MPX.NE.MLX.OR.MPY.NE.MLY)THEN
        WRITE(KFILDO,400)NSLAB,NSLABP
 400    FORMAT(/' ****THE CHARACTERISTICS OF THE LCL PRESSURE GRID ARE',
     1          ' DIFFERENT FROM THE LCL TEMPERATURE GRID',I3,2X,I3,
     2          ' LINDEX CAN NOT BE COMPUTED.')
        IER=100
        GOTO 800
      END IF
C 
C        CREATE ID FOR TEMPERATURE AT REQUESTED SURFACE
C
      MT(1)=ICCCFFF(1)*1000+IDPARS(4)
      MT(2)=IDPARS(7)
      MT(3)=IDPARS(9)*1000000+IDPARS(12)
      MT(4)=0
C 
C        FETCH TEMPERATURE.
C
      CALL GFETCH(KFILDO,KFIL10,MT,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDTK,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLABT,MISSP,MISSS,L3264B,1,IER)
C
      IF(IER.NE.0) THEN
C***        WRITE(KFILDO,450)IER
C*** 450    FORMAT(/' ****TEMPERATURE FETCH FAILED IN LINDEX. IER =',I3)
        GOTO 800
      END IF
C
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
      IF(IER.NE.0)GOTO 800
C
      MTX=IS2(3)
      MTY=IS2(4)
C
      IF(NSLABT.NE.NSLAB.OR.MTX.NE.MLX.OR.MTY.NE.MLY)THEN
        WRITE(KFILDO,500)NSLAB,NSLABT
 500    FORMAT(/,' ****THE CHARACTERISTICS OF THE TEMPERATURE',
     1           ' GRID ARE DIFFERENT FROM THE LCL GRID',I3,2X,I3,
     2           ' LINDEX CAN NOT BE COMPUTED.')
        IER=100
        GOTO 800
      END IF
C
C        CREATE ID FOR TEMPERATURE AT 500 MB
C
      MT(1)=ICCCFFF(1)*1000+IDPARS(4)
      MT(2)=500
      MT(3)=IDPARS(9)*1000000+IDPARS(12)
      MT(4)=0
C 
C        FETCH 500 MB TEMPERATURE.
C
      CALL GFETCH(KFILDO,KFIL10,MT,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDTK5,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLABT5,MISSP,MISSS,L3264B,1,IER)
C
      IF(IER.NE.0) THEN
C***        WRITE(KFILDO,550)IER
C*** 550    FORMAT(/' ****500 MB TEMPERATURE FETCH FAILED IN LINDEX. IER ='
C***     1  ,I3)
      GOTO 800
      END IF
C
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
      IF(IER.NE.0)GOTO 800
C
      MT5X=IS2(3)
      MT5Y=IS2(4)
C
      IF(NSLABT5.NE.NSLAB.OR.MT5X.NE.MLX.OR.MT5Y.NE.MLY)THEN
        WRITE(KFILDO,600)NSLAB,NSLABT5
 600    FORMAT(/,' ****THE CHARACTERISTICS OF THE 500 MB TEMPERATURE',
     1           ' GRID ARE DIFFERENT FROM THE LCL GRID',I3,2X,I3,
     2           ' LINDEX CAN NOT BE COMPUTED.')
        IER=100
        GOTO 800
      END IF
C
C        CREATE ID FOR RELATIVE HUMIDITY 
C
      MT(1)=ICCCFFF(5)*1000+IDPARS(4)
      MT(2)=IDPARS(7)
      MT(3)=IDPARS(9)*1000000+IDPARS(12)
      MT(4)=0
C 
C        FETCH RELATIVE HUMIDITY.
C    
      CALL GFETCH(KFILDO,KFIL10,MT,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDRH,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLABRH,MISSP,MISSS,L3264B,1,IER)
C
      IF(IER.NE.0) THEN
C***        WRITE(KFILDO,650)IER
C*** 650    FORMAT(/' ****RELATIVE HUMIDITY FETCH FAILED IN LINDEX.',
C***     1           '  IER =',I3)
        GOTO 800
      END IF
C
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
      IF(IER.NE.0)GOTO 800
C
      MRHX=IS2(3)
      MRHY=IS2(4)
C
      IF(NSLABRH.NE.NSLAB.OR.MRHX.NE.MLX.OR.MRHY.NE.MLY)THEN
        WRITE(KFILDO,700)NSLAB,NSLABRH
 700    FORMAT(/,' ****THE CHARACTERISTICS OF THE RELATIVE HUMIDITY ',
     1           ' GRID ARE DIFFERENT FROM THE LCL GRID',I3,2X,I3,
     2           ' LINDEX CAN NOT BE COMPUTED.')
        IER=100
        GOTO 800
      END IF
C
C     ================================================================== 
C       BEGIN CALCULATIONS OF LIFTED INDEX
C     ================================================================== 
C
D     PRINT *,' ****BEGIN LIFTED INDEX CALCULATIONS ******'
C
      DO 750 I=1,MTX*MTY
      FDTC=FDTK(I)+ABSZRO
      PPP=1000./IPRESS
      FDTLC=FDTL(I)+ABSZRO
      FPLCL=FDPL(I)*.01
C
C        CALL FUNCTION FTHETAE
C
      THETA=FTHETAE(FDTK(I),IPRESS,FDTC,FDTL(I),FDRH(I))

      IF ((FPLCL).LT.499.9) THEN
C
C          LCL IS ABOVE THE 500MB LVL, ITERATIONS NOT NECESSARY
C          THE FOLLOWING EQUATION IS TAKEN FROM HOWCROFT W3LIB W3FA06
C
C          FDLI = FDTK5(I) - (UNPOT(POTEMP(FDTL,FDPL),500)), WHERE
C          UNPOT=((500./1000.)**RKAPPA)*POTTMP
C
        POTTMP= FDTL(I)*(1000./(FDPL(I)*.01))**RKAPPA
        FDLI(I)=FDTK5(I) - (((.5)**RKAPPA)*POTTMP) 
D       PRINT *,'LIFTED INDEX WHEN LCL ABOVE 500 = ',FDLI(I) 
C
      ELSE IF ((FDPL(I)*.01).GT.500.1) THEN
C
C          PROCEED WITH ITERATIONS
C
        TGUESS=FDTK5(I)
        TGUESSC=TGUESS+ABSZRO
        DELTA=10.
        EPS=0.5
        A=0.
        ITER=0
C
C          START ITERATIONS
C
        DO 850   
        ITER=ITER+1
C
        IF (ITER.GT.50) THEN
          FDLI(I)=9999.9
C
C            EXIT DO LOOP, GO TO NEXT GRID POINT
C
          EXIT
        END IF
C
        ASTORE=A
        TGUESSC=TGUESS+ABSZRO
C
C          CALCULATE THETAE AT 500 MILLIBARS FOR SATURATED AIR
C
        IFPRES=500
        FRH=100.
        A=FTHETAE(TGUESS,IFPRES,TGUESSC,TGUESS,FRH) - THETA
        GUESSLI=FDTK5(I)-TGUESS
C
        IF (ABS(A).LT.EPS) THEN
          FDLI(I)=FDTK5(I)-TGUESS
D         PRINT *,'LIFTED INDEX = ',FDLI(I) 
          EXIT
        ELSE
          DELTA=DELTA * 0.5
          IF (A*ASTORE.LT.0.0) DELTA=-DELTA
          TGUESNXT=TGUESS+DELTA
          TGUESNXC=TGUESNXT+ABSZRO
          ANXT=FTHETAE(TGUESNXT,IFPRES,TGUESNXC,TGUESNXT,FRH)-THETA      
C
          IF (ABS(ANXT).LT.EPS) THEN
             TGUESS=TGUESNXT
             FDLI(I)=FDTK5(I)-TGUESS
D            PRINT *,'LIFTED INDEX = ',FDLI(I) 
             EXIT
          ELSE 
             DELTA=A*DELTA/(A-ANXT)
             IF(ABS(DELTA).LT.0.01)  DELTA=SIGN(0.01,DELTA)
             TGUESS=TGUESS+DELTA
             IF (TGUESS.GT.(50.-ABSZRO)) TGUESS=50.-ABSZRO
          END IF
C
        END IF
C
 850    CONTINUE
C     
      ELSE
C
C          IF THE PRESSURE OF THE LIFTED CONDENSATION LEVEL IS
C          EQUAL TO 500 MB THEN THE LIFTED INDEX IS SIMPLY THE 
C          DIFFERENCE BETWEEN THE TEMPERATURE AT 500 MB AND THE
C          TEMPERATURE OF THE LIFTED CONDENSATION LEVEL.
C
        FDLI(I)= FDTK5(I) - FDTL(I) 
D       PRINT *,'LIFTED INDEX WHEN LCL EQUALS 500 = ',FDLI(I) 
      END IF
C
 750  CONTINUE
C
      GOTO 900
C
C     ==================================================================
C 
 800  DO 801 K=1,ND2X3
        FDLI(K)=9999.
 801  CONTINUE
C
 900  RETURN
      END
C 
C     ================================================================== 
C
      REAL FUNCTION FSVP(FTC)
C
C        FUNCTION TO CALCULATE THE SATURATION VAPOR PRESSURE
C        USING TETENS FORMULA WITH NATURAL BASE
C        VAPORPRESSURE = 6.11*EXP((17.2694*T)/(T+237.3))
C
      IMPLICIT NONE
      REAL, PARAMETER :: PSAT=6.1078
      REAL :: FTC
C
      FSVP=PSAT*EXP(17.269*FTC/(237.3+FTC))
C
      END FUNCTION FSVP
C
C     ================================================================== 
C
      REAL FUNCTION FSMR(FTC,IFPRES)
C
C         FUNCTION TO CALCULATE THE SATURATED MIXING RATIO
C
      IMPLICIT NONE
C
C        FUNCTIONS
C
      REAL,EXTERNAL :: FSVP
C
C        LOCAL VARIABLES
C
      REAL, PARAMETER :: CP=.24,RD=287.04,RV=461.5,EPSILN=RD/RV
C        CP DOESN'T SEEM TO BE USED.
      REAL :: FTC
      INTEGER :: IFPRES
C
      FSMR=(EPSILN*(FSVP(FTC)))/((IFPRES-(FSVP(FTC))))
C
      END FUNCTION FSMR
C
C     ================================================================== 
C
      REAL FUNCTION FMIXR(FTC,IFPRES,FRH)
C
C        FUNCTION TO CALCULATE THE MIXING RATIO
C
      IMPLICIT NONE
C
C        FUNCTIONS
C
      REAL,EXTERNAL :: FSMR, FSVP
C
C        LOCAL VARIABLES
C
      REAL ::  FTC,FRH
      INTEGER :: IFPRES
C
      FMIXR=FSMR(FTC,IFPRES)*FRH/100.
C
      END FUNCTION FMIXR
C
C     ================================================================== 
C
      REAL FUNCTION FTHETAE(FTK,IFPRES,FTC,FTL,FRH)
C
C        FUNCTION TO CALCULATE THE EQUIVALENT POTENTIAL TEMPERATURE
C        WHICH IS THE POTENTIAL TEMPERATURE OF A PARCEL OF AIR WHEN
C        THE MIXING RATIO HAS BEEN REDUCED TO ZERO AND THE LATENT HEAT
C        TURNED INTO SENSIBLE HEAT
C         VARIABLES
C           IFPRES = PRESSURE OF PARCEL
C              FRH = RELATIVE HUMIDITY
C              FTC = PARCEL TEMPERATURE IN DEGREES CELCIUS
C              FTK = PARCEL TEMPERATURE IN KELVIN 
C              FTL = TEMPERATURE OF PARCEL AT LIFTED CONDENSATION LEVEL
C         FUNCTIONS
C            FMIXR = FUNCTION TO CALCULATE MIXING RATION
C            FSMR  = FUNCTION TO CALCULATE SATURATION MIXING RATIO
C            FSVP  = FUNCTION TO CALCULATE SATURATION VAPOR PRESSURE
C
        IMPLICIT NONE
C
C          FUNCTIONS
C
        REAL,EXTERNAL ::  FMIXR,FSMR,FSVP
C
C          LOCAL VARIABLES
C
        REAL, PARAMETER :: RKAPPA=.28573
        INTEGER :: IFPRES
        REAL :: FTC,FRH,FTL,FTK
C
      FTHETAE=FTK*((1000./IFPRES)**(RKAPPA*
     1 (1.-(.28*FMIXR(FTC,IFPRES,FRH)))))*
     2 EXP((3.376/FTL-.00254)*
     3 ((1000.*FMIXR(FTC,IFPRES,FRH))*
     4 (1.+.81*FMIXR(FTC,IFPRES,FRH))))
C
      END FUNCTION FTHETAE
C
C     ================================================================== 
C     END SUBROUTINE LINDEX
