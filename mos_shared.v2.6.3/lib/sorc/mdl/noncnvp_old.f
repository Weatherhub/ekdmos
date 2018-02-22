      SUBROUTINE NONCNVP_OLD(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                  NGRIDC,ND11,NSLAB,IPACK,IWORK,
     2                  FDTP,ND5,LSTORE,ND9,LITEMS,CORE,ND10,
     3                  NBLOCK,NFETCH,IS0,IS1,IS2,IS4,ND7,
     4                  FD1,FD2,ND2X3,ISTAV,L3264B,MISTOT,IER)
C
C        AUG     1998   SFANOS         TDL MOS-2000
C        OCT     1998   SFANOS         CHANGED CODE SO THAT
C                                      3,6,AND 12 HOUR PROJECTIONS
C                                      WORK IN ONE CODE
C        JUN     2003   COSGROVE       RENAMED THIS OLD VERSION _OLD.
C                                      WE NEED TO KEEP THIS IN OPERATIONS
C                                      FOR NOW TO USE WITH TPCP24_OLD
C
C        PURPOSE
C            TO COMPUTE THE 3,6 OR 12 HR NON CONVECTIVE
C            PRECIP AMOUNT
C            THE FORMULA FOR THIS IS
C            DATA = (TOTAL PRECIP AMT FOR 3,6,OR 12 HR - 3,6 OR 12 
C                    HR CONVECTIVE AMT)
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C                   003 265 - 3  HR NON CONVECTIVE PRECIP AMT
C                   003 270 - 6  HR NON CONVECTIVE PRECIP AMT
C                   003 280 - 12 HR NON CONVECTIVE PRECIP AMT
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
C              FD1(J) = WORK ARRAY TO HOLD THE 3,6, OR 12 HOUR 
C                       CONVECTIVE PRECIPITATION. (J=1,ND2X3) (INTERNAL)
C              FD2(J) = WORK ARRAY TO HOLD THE 3,6, OR 12 HOUR
C                       TOTAL PRECIPITATION. (J=1,ND2X3) (INTERNAL)
C             FDTP(J) = ARRAY TO HOLD RETURNED DATA WHEN THE DATA 
C                       ARE AT GRIDPOINTS.  IN THIS CASE, THE 3,6,OR 12 
C                       HOUR NON-CONVECTIVE PRECIP. (J=1,ND5). (OUTPUT)
C                   I = LOOP CONTROL VARIABLE
C           ICCCFFF() = CONTAINS IDPARS(1) AND IDPARS(2) ID FOR THE
C                       3, 6, AND 12 HR TOTAL AND CONVECTIVE PRECIP
C                       AMOUNTS.
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
C                       101 = GRID SIZE IS TOO BIG FOR AN ARRAY(), WHOSE 
C                             DIMENSION IS ND5.
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE
C                             NON CONVECTIVE PRECIPITATION.
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
C                              MSTORE(,). LATER USED AS A WAY OF
C                              DETERMINING WHETHER TO KEEP THIS
C                              VARIABLE.
C              MCP(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD1() (J=1,4). (INTERNAL)
C             MCX,MCY = DIMENSIONS OF GRID RETURNED FOR 
C                       CONVECTIVE PRECIPITATION. (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 FROM CALLING GFETCH WHEN THERE IS NO
C                       PRIMARY MISSING VALUE. (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 FROM CALLING GFETCH WHEN THERE IS NO
C                       SECONDARY MISSING VALUE.  (INTERNAL)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C             MPX,MPY = DIMENSIONS OF THE GRID RETURNED FOR 
C                       TOTAL PRECIPITATION. (INTERNAL)
C              MTP(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD2() (J=1,4). (INTERNAL)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE. (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF
C                       THE GRID IS NOT KNOWN BEFORE FD1 AND FD2
C                       ARE FETCHED.  (INPUT)
C                 ND5 = DIMENSION OF IPACK(), AND IWORK(). (INPUT)
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
C              NSLABC = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  USED AFTER SUBROUTINE GFETCH
C                       TO CHECK IF NSLABC = NSLAB FOR CONVECTIVE
C                       PRECIPITATION OVER A 3,6,OR 12 HOUR 
C                       PERIOD. (OUTPUT)
C              NSLABP = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  USED AFTER SUBROUTINE GFETCH
C                       TO CHECK IF NSLABP =NSLAB FOR TOTAL 3,6, OR
C                       12 HOUR PRECIPITATION. (OUTPUT)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE,
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS 
C                       STORED IN LSTORE(9,). (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA().  THIS 
C                       IS RETURNED FROM GFETCH (INTERNAL)
C
C     NON SYSTEM SUBROUTINES USED
C        GFETCH
C
      IMPLICIT NONE
      INTEGER ICCCFFF(6)
      INTEGER IDPARS(15)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER IWORK(ND5),IPACK(ND5)
      INTEGER JD(4),MCP(4),MTP(4)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER IER,IJ,ISTAV,J,KFILDO,KFIL10,L3264B,LITEMS,
     1        MISSP,MISSS,MISTOT,MCX,MCY,MPX,MPY,
     2        NBLOCK,ND2X3,ND5,ND7,ND9,ND10,
     3        ND11,NDATE,NFETCH,NPACK,NSLAB,NSLABC,
     4        NSLABP,NTIMES,NWORDS
      REAL CORE(ND10)
      REAL FDTP(ND5)
      REAL FD1(ND2X3),FD2(ND2X3)
C
C        ICCFFF(1) AND (2) ARE 3 HR TOTAL PRECIP AND 3 HR CONVECTIVE
C        PRECIPITATION, RESPECTIVELY. (3) AND (4) ARE 6 HR TOTAL
C        PRECIP AND 6 HR CONVECTIVE PRECIPITATION.  (5) AND (6) ARE
C        TOTAL PRECIP OVER 12 HR AND 12 HR CONVECTIVE PRECIPITATION.
C
      ICCCFFF(1)=003205
      ICCCFFF(2)=003235
      ICCCFFF(3)=003210
      ICCCFFF(4)=003240
      ICCCFFF(5)=003220
      ICCCFFF(6)=003250
C
      IER  =0
      ISTAV=0
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C
      IF(IDPARS(1).NE.003.OR.(IDPARS(2).NE.265.AND.
     1   IDPARS(2).NE.270.AND.IDPARS(2).NE.280))THEN
         WRITE(KFILDO,101)(JD(J),J=1,4)
 101     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1           '3 OR 6 OR 12 HR NON CONV PRECIP AMT. ',I9.9,2I10.9,
     2           I4.3, 'NOT COMPUTED IN NONCNVP. ')
         IER=103
         GOTO 800
      END IF
C
C        CREATE ID FOR TOTAL PRECIP AMT
C        THE ID DEPENDS ON WHICH NONCNV TIME THE USER WANTS 
C
      IF(IDPARS(2).EQ.265)THEN
C        THREE HOUR TOTAL
        MTP(1)=ICCCFFF(1)*1000+IDPARS(4)
      ELSE IF(IDPARS(2).EQ.270)THEN
C        SIX HOUR TOTAL
        MTP(1)=ICCCFFF(3)*1000+IDPARS(4)
      ELSE 
C        TWELVE HOUR TOTAL
        MTP(1)=ICCCFFF(5)*1000+IDPARS(4)
      END IF
C
      MTP(2)=IDPARS(7)
      MTP(3)=IDPARS(9)*1000000+IDPARS(12)
      MTP(4)=0
C
C        FETCH THE TOTAL PRECIPITATION.  NOTE THAT FD2 IS
C        DIMENSIONED ND5.
C
      CALL GFETCH(KFILDO,KFIL10,MTP,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND5,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3            NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
C
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GOTO 800
      MPX=IS2(3)
      MPY=IS2(4)
      NSLABP=NSLAB
C
C        CREATE ID FOR CONVECTION OVER 3,6, OR 12 HRS
C
      IF(IDPARS(2).EQ.265)THEN
C        THREE HOUR CONVECTIVE
        MCP(1)=ICCCFFF(2)*1000+IDPARS(4)
      ELSE IF(IDPARS(2).EQ.270)THEN
C        SIX HOUR CONVECTIVE
        MCP(1)=ICCCFFF(4)*1000+IDPARS(4)
      ELSE 
C        TWELVE HOUR CONVECTIVE
        MCP(1)=ICCCFFF(6)*1000+IDPARS(4)
      END IF
C
      MCP(2)=IDPARS(7)
      MCP(3)=IDPARS(9)*1000000+IDPARS(12)
      MCP(4)=0
C
C        FETCH THE CONVECTIVE PRECIPITATION.  NOTE THAT FD1
C        IS DIMENSIONED ND5.
C
      CALL GFETCH(KFILDO,KFIL10,MCP,7777,LSTORE,ND9,
     1            LITEMS,IS0,IS1,IS2,IS4,ND7,IPACK,
     2            IWORK,FD1,ND5,NWORDS,NPACK,NDATE,
     3            NTIMES,CORE,ND10,NBLOCK,NFETCH,
     4            NSLAB,MISSP,MISSS,L3264B,1,IER)
C
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GOTO 800
      MCX=IS2(3)
      MCY=IS2(4)
      NSLABC=NSLAB
C
C        COMPARE IF GRID CHARACTERISTICS ARE THE SAME
C
      IF(MCX.NE.MPX.OR.MCY.NE.MPY.OR.NSLABC.NE.NSLABP)THEN
C        THE GRID CHARACTERISTICS ARE NOT THE SAME
        WRITE(KFILDO,145)(MCP(J),J=1,4),(NGRIDC(J,NSLABC),J=1,6),
     1                    MCX,MCY,
     2               (MTP(J),J=1,4),(NGRIDC(J,NSLABP),J=1,6),
     3                    MPX,MPY
 145    FORMAT(' ****THE GRIDS NEEDED IN NONCNV_PCP12 HAVE DIFFERENT'
     1         ,' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2         ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3         (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
      END IF
C
C        BEGIN COMPUTATION OF THE 3,6,OR 12 HOUR NON CONVECTIVE PRECIP
C
      DO 300 IJ=1,ND5
C
        FDTP(IJ)=(FD2(IJ)-FD1(IJ))
        IF(FDTP(IJ).LT.0.0)FDTP(IJ)=0.0
 300  CONTINUE
C
      GOTO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C 
 800  DO 801 J=1,ND5
        FDTP(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END
