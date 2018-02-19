      SUBROUTINE THETAE(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                  NGRIDC,ND11,NSLAB,IPACK,IWORK,EPT,ND5,
     2                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                  IS0,IS1,IS2,IS4,ND7,
     4                  FDTK,FDMR,FDLC,FD4,ND2X3,
     5                  ISTAV,L3264B,MISTOT,IER)
C
C        SEPTEMBER 1998   HUGHES   TDL   MOS-2000
C        NOVEMBER  2002   WEISS    CHANGED ND5 TO ND2X3
C        MAY       2003   GLAHN    MODIFIED LINES IN CALL;  SET
C                                  DIMENSIONS OF IPACK( ), IWORK( ) AND 
C                                  EPT( ) = ND5; REARRANGED ARGUMENTS
C                                  TO SUBROUTINE CALLS; SUBSTITUTED
C                                  AUTOMATIC ARRAY FDLC( ) FOR FD3( );
C                                  COMMENTS; PUT ICCCFFF( ) INTO DATA
C                                  STATEMENT
C
C        PURPOSE
C            TO COMPUTE THE EQUIVALENT POTENTIAL TEMPERATURE.
C            THETAE IS THE FINAL TEMPERATURE WHICH A PARCEL
C            OF AIR ATTAINS WHEN IT IS LIFTED DRY ADIABATICALLY
C            TO ITS LIFTED CONDENSATION LEVEL, THEN PSEUDO-
C            ADIABATICALLY TO A GREAT HEIGHT (REMOVING MOISTURE),
C            THEN BROUGHT DOWN ADIABATICALLY TO 1000MB.
C            ORIGINALLY PRESENTED BY STACKPOLE (JAM 1967),
C            THE ALGORITHM USED IN THIS SUBROUTINE IS BASED ON
C            BOLTON (MWR 1980). THETAE WILL BE RETURNED IN KELVIN.
C            THIS CALCULATION IS VALID FOR A PARCEL INITIALLY
C            LOCATED ON AN ISOBARIC SURFACE.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               003 130 - EQUIVALENT POTENTIAL TEMPERATURE
C            
C           DATA SET USE
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                      (OUTPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                       ACCESS.(INPUT-OUTPUT)
C
C        VARIABLES
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE(,) (J=1,ND10).
C                       WHEN CORE() IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C              EPT(J) = DATA ARRAY WHICH RETURNS THE EQUIVALENT 
C                       POTENTIAL TEMPERATURE (THETAE) (J=1,ND5).
C                       (OUTPUT)
C             FDLC(K) = WORK ARRAY TO HOLD THE TEMPERATURE OF THE
C                       LIFTED CONDENSATION LEVEL (K=1,ND2x3).
C                       (INTERNAL).
C             FDMR(K) = WORK ARRAY TO HOLD THE MIXING RATIO
C                       (K=1,ND2X3). (INTERNAL)
C             FDPT(K) = WORK ARRAY TO HOLD THE DEW POINT TEMPERATURE
C                       IN KELVIN (K=1,ND2X3). (INTERNAL)
C             FDTK(K) = WORK ARRAY TO HOLD THE AIR TEMPERATURE IN 
C                       KELVIN (K=1,ND2X3). (INTERNAL)
C              FD4(K) = WORK ARRAY (K=1,ND2X3). (INTERNAL)
C                   I = LOOP CONTROL VARIABLE
C          ICCCFFF(J) = CONTAINS THE CCCFFF ID (SEE IDPARS) FOR THE
C                       TEMPERATURE (J=1), THE TEMPERATURE OF THE
C                       LIFTED CONDENSATION LEVEL (J=2), AND THE
C                       MIXING RATIO ON AN ISOBARIC SURFACE (J=3).
C                       (INTERNAL)
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
C                             THETAE.
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
C                              MOSTORE(,). LATER USED AS A WAY OF
C                              DETERMINING WHETHER TO KEEP THIS
C                              VARIABLE.
C            MLPARS() = PARSED ID USED IN SUBROUTINE PRSID1 FOR
C                       SUBROUTINE LCL. (INTERNAL)
C            MRPARS() = PARSED ID USED IN SUBROUTINE PRSID1 FOR
C                       SUBROUTINE MIXRAT. (INTERNAL)
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
C                       THE GRID IS NOT KNOWN BEFORE FDLC AND FDMR
C                       ARE FETCHED.  (INPUT)
C                 ND5 = DIMENSION OF IPACK(), IWORK(), AND EPT().
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
C              NSLABM = THE NUMBER USED TO COMPARE TO NSLAB THAT
C                       IS RETURNED FROM SUBROUTINE MIXRAT AFTER
C                       FETCHING THE MIXING RATIO AND IS USED
C                       AS A CHECK
C              NSLABT = THE NUMBER USED TO COMPARE TO NSLAB THAT
C                       IS RETURNED FROM SUBROUTINE GFETCH AFTER
C                       FETCHING THE TEMPERATURE AND IS USED
C                       AS A CHECK
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE,
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS 
C                       STORED IN LSTORE(9,). (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA().  THIS 
C                       IS RETURNED FROM GFETCH (INTERNAL)
C               PRESS = VARIABLE CONTAINING THE VALUE OF THE CONSTANT
C                       PRESSURE SURFACE REQUESTED BY THE USER.  THIS
C                       IS THE LOWEST LEVEL OF THE PARCEL.  IT IS THE
C                       SAME AS IDPARS(7).
C                 PPP = 1000/ PRESS, USED IN CALCULATION OF THE 
C                       POTENTIAL TEMPERATURE FOR MOIST AIR.
C              RKAPPA = RD/CP, VALUE IS .28573 (OR 2/7) WHERE RD IS
C                       DRY AIR GAS CONSTANT, CP IS SPECIFIC HEAT OF
C                       DRY AIR AT CONSTANT PRESSURE
C                FDLC = VARIABLE USED TO HOLD THE TEMPERATURE OF THE
C                       LIFTED CONDENSATION LEVEL BEFORE THE DATA
C                       ARRAY IS FILLED (INTERNAL).
C        1         2         3         4         5         6         7 X
C
      IMPLICIT NONE
C
      REAL, PARAMETER :: RKAPPA=.28573,ABSZRO=-273.15
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER ICCCFFF(3),MT(4),MR(4),ML(4),MRPARS(15),MLPARS(15)
      INTEGER IER,IJ,ISTAV,J,KFILDO,KFIL10,L3264B,LITEMS,
     1        MISSP,MISSS,MISTOT,MLX,MLY,MTX,MTY,MRX,MRY,
     2        NBLOCK,ND2X3,ND5,ND7,ND9,ND10,ND11,NDATE,NFETCH,
     3        NPACK,NSLAB,NSLABM,NSLABT,NTIMES,NWORDS,PPP,PRESS
C
      REAL EPT(ND5)
      REAL FDLC(ND2X3),FDMR(ND2X3),FDTK(ND2X3),FD4(ND2X3)
      REAL CORE(ND10)
      REAL E,THETA
C
      DATA ICCCFFF/002000,
     1             003161,
     2             003010/
C
      IER=0
      ISTAV=0
 
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C        CCCFFF = 003130.   
 
      IF((IDPARS(1).NE.003).OR.(IDPARS(2).NE.130)) THEN
        WRITE(KFILDO,200)(JD(J),J=1,4)
 200    FORMAT(/,' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1           ' THETAE PREDICTOR. ',I9.9,2I10.9,I4.3,
     2           ' NOT COMPUTED IN THETAE.')
        IER=103
        GOTO 800
      END IF
 
      PRESS=IDPARS(7)
 
C        CREATE ID FOR TEMPERATURE AT LIFTED CONDENSATION LEVEL.
C
      ML(1)=ICCCFFF(2)*1000+IDPARS(4)
      ML(2)=IDPARS(7)
      ML(3)=IDPARS(9)*1000000+IDPARS(12)
      ML(4)=0
 
C        GET FDLC( ), THE  TEMPERATURE AT LIFTED CONDENSATION LEVEL.
C
      CALL PRSID1(KFILDO,ML,MLPARS)
      CALL LCL(KFILDO,KFIL10,MLPARS,JD,NDATE,
     1         NGRIDC,ND11,NSLAB,IPACK,IWORK,FDLC,ND2X3,
     2         LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3         IS0,IS1,IS2,IS4,ND7,
     4         FDTK,FDMR,EPT,FD4,ND2X3,
     5         ISTAV,L3264B,MISTOT,IER)
      IF(IER.NE.0)GOTO 800

      MLX=IS2(3)
      MLY=IS2(4)

C        CREATE ID FOR MIXING RATIO.
C
      MR(1)=ICCCFFF(3)*1000+IDPARS(4)
      MR(2)=IDPARS(7)
      MR(3)=IDPARS(9)*1000000+IDPARS(12)
      MR(4)=0
 
C        GET FDMR( ), THE MIXING RATIO.
C
      CALL PRSID1(KFILDO,MR,MRPARS)
      CALL MIXRAT(KFILDO,KFIL10,MRPARS,JD,NDATE,
     1            NGRIDC,ND11,NSLABM,IPACK,IWORK,FDMR,ND2X3,
     2            LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3            IS0,IS1,IS2,IS4,ND7,
     4            FDTK,EPT,FD4,ND2X3,
     5            ISTAV,L3264B,MISTOT,IER)
      IF(IER.NE.0)GOTO 800

      MRX=IS2(3)
      MRY=IS2(4)

      IF(NSLABM.NE.NSLAB.OR.MRX.NE.MRX.OR.MRY.NE.MLY)THEN
        WRITE(KFILDO,400)NSLAB,NSLABM
 400    FORMAT(/' ****THE CHARACTERISTICS OF THE MIXING RATIO',
     1          ' GRID ARE DIFFERENT FROM THE LCL GRID AT 400',
     2          I3,2X,I3,'.',
     3         /'     THETAE CAN NOT BE COMPUTED IN THETAE.')
        IER=100
        GOTO 800
      END IF
 
C        CREATE ID FOR TEMPERATURE AT REQUESTED SURFACE.
C
      MT(1)=ICCCFFF(1)*1000+IDPARS(4)
      MT(2)=IDPARS(7)
      MT(3)=IDPARS(9)*1000000+IDPARS(12)
      MT(4)=0
 
C        FETCH FDTK( ), TEMPERATURE.
C
      CALL GFETCH(KFILDO,KFIL10,MT,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDTK,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLABT,MISSP,MISSS,L3264B,
     4            1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GOTO 800

      MTX=IS2(3)
      MTY=IS2(4)

      IF(NSLABT.NE.NSLAB.OR.MTX.NE.MLX.OR.MTY.NE.MLY)THEN
        WRITE(KFILDO,500)NSLAB,NSLABT
 500    FORMAT(/' ****THE CHARACTERISTICS OF THE TEMPERATURE',
     1          ' GRID ARE DIFFERENT FROM THE LCL GRID AT 500',
     2          I3,2X,I3,'.',
     3         /'     THETAE CAN NOT BE COMPUTED IN THETAE.')
        IER=100
        GOTO 800
      END IF
C 
C        COMPUTE THEATAE.
C
      PPP=1000/PRESS
C
      DO 600 IJ=1,(MLX*MLY)
C        COMPUTATION OF THE POTENTIAL TEMPERATURE IS NORMALLY BASED 
C        ON POISSON'S EQUATION WHERE THETA=T(1000/P)**KAPPA.
C        FOR THE COMPUTATION OF THE POTENTIAL TEMPERATURE FOR MOIST 
C        AIR SEE BOLTON (MWR 1980) OR GEMPAK PARAMETERS APPENDIX
C        THE MIXING RATIO RETURNED FROM MIXRAT (FDMR) HAS THE
C        UNIT KG/KG SO IT IS NOT NECESSARY TO MULTIPLY BY .001

        E = RKAPPA * (1. - (.28 * FDMR(IJ)))
        THETA = FDTK(IJ)*(PPP**E)

C        COMPUTATION OF THE EQUIVALENT POTENTIAL TEMPERATURE
C        THIS FORMULA IS FOUND IN BOLTON 1980, FORMULA (40)

        EPT(IJ)=THETA * EXP((3.376/FDLC(IJ) - .00254)*
     1  ((1000.*FDMR(IJ)) * (1. + .81 * FDMR(IJ))))

 600  CONTINUE
C
      GOTO 900
 
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
  
 800  DO 801 J=1,ND2X3
        EPT(J)=9999.
 801  CONTINUE

 900  RETURN
      END
