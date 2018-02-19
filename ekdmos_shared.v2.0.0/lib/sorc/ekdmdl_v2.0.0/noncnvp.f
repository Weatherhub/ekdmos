      SUBROUTINE NONCNVP(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                  NGRIDC,ND11,NSLAB,IPACK,IWORK,FDTP,ND5,
     2                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                  IS0,IS1,IS2,IS4,ND7,
     4                  FD1,FD2,FD3,FD4,FD5,FD6,FD7,ND2X3,
     5                  ISTAV,L3264B,MISTOT,IER)
C
C        AUGUST   1998   SFANOS   TDL   MOS-2000
C        OCTOBER  1998   SFANOS   CHANGED CODE SO THAT 3, 6, AND 
C                                 12 HOUR PROJECTIONS WORK IN ONE CODE
C        MAY      1999   SFANOS   RESTRUCTURED CODE AFTER
C                                 TPCP3 WAS MODIFIED
C        AUGUST   2002   SFANOS   OLDER VERSION WAS IN LIBRARY
C        NOVEMBER 2002   WEISS    CHANGED ND5 TO ND2X3
C        MAY      2003   GLAHN    ADDED CHECKS ON IER; ELIMINATED
C                                 INITIALIZATION OF ARRAYS; ELIMINATED
C                                 A FEW REDUNDANT STATEMENTS; CHANGED
C                                 COMPUTATION LOOPS FROM 1,ND2X3 TO 
C                                 1,NTX*NTY
C        JUNE     2003   GLAHN    ADDED TESTS ON IER AFTER EACH CALL
C                                 TO TPCP3, TPCP6, AND TPCP12
C        OCTOBER  2003   SMB      CORRECTED FORMAT STATEMENT 110 FOR
C                                 THE IBM
C        FEBRUARY 2004   SMB      ADDED 24-HR CALCULATIONS (WERE 
C                                 ORIGINALLY IN TPCP24); REVERTED SOME
C                                 ND2X3'S IN CALLS TO VARIOUS TPCP 
C                                 CODES TO ND5 AS THIS IS WHAT THE TPCP
C                                 CODES ARE EXPECTING IN THEIR CALLS
C
C        PURPOSE
C            TO COMPUTE THE 3-, 6-, 12-, OR 24-HR NON CONVECTIVE 
C            PRECIP AMOUNT.  THE FORMULA FOR THIS IS
C            DATA = (TOTAL PRECIP AMT FOR 3- ,6-, 12-, OR 24-HR MINUS
C                    3-, 6-, 12-, OR 24-HR CONVECTIVE AMT)
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C                003 265 - 3 -HR NON CONVECTIVE PRECIP AMT
C                003 270 - 6 -HR NON CONVECTIVE PRECIP AMT
C                003 280 - 12-HR NON CONVECTIVE PRECIP AMT
C                003 286 - 24-HR NON CONVECTIVE PRECIP AMT
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
C                       TOTAL PRECIPITATION, OR 24 HOUR CONVECTIVE 
C                       PRECIPITATION (FDTP IS USED DIRECTLY FOR THE 
C                       24 HOUR TOTAL PRECIP). (J=1,ND2X3) (INTERNAL)
C              FD2(J) = WORK ARRAY TO HOLD THE 3,6, OR 12 HOUR
C                       CONVECTIVE PRECIPITATION. (J=1,ND2X3) (INTERNAL)
C              FD3(J) = TEMPORARY HOLDING ARRAY. (J=1,ND2X3) (INTERNAL)
C              FD4(J) = TEMPORARY HOLDING ARRAY. (J=1,ND2X3) (INTERNAL)
C              FD5(J) = TEMPORARY HOLDING ARRAY. (J=1,ND2X3) (INTERNAL)
C              FD6(J) = TEMPORARY HOLDING ARRAY. (J=1,ND2X3) (INTERNAL)
C              FD7(J) = TEMPORARY HOLDING ARRAY. (J=1,ND2X3) (INTERNAL)
C             FDTP(J) = ARRAY TO HOLD RETURNED DATA WHEN THE DATA 
C                       ARE AT GRIDPOINTS.  IN THIS CASE, THE 3,6,12,24
C                       HOUR NON-CONVECTIVE PRECIP. (J=1,ND2X3). 
C                       (OUTPUT)
C                   I = LOOP CONTROL VARIABLE
C           ICCCFFF() = CONTAINS IDPARS(1) AND IDPARS(2) ID FOR THE
C                       3-, 6-, AND 12-HR TOTAL AND CONVECTIVE PRECIP
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
C             MCP3(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD1() (J=1,4). (INTERNAL)
C             MCP6(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD1() (J=1,4). (INTERNAL)
C            MCP12(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD1() (J=1,4). (INTERNAL)
C            MCP24(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
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
C             MTP3(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD2() (J=1,4). (INTERNAL)
C             MTP6(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD2() (J=1,4). (INTERNAL)
C            MTP12(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD2() (J=1,4). (INTERNAL)
C            MTP24(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD2() (J=1,4). (INTERNAL)
C             MTX,MTY = DIMENSIONS OF THE GRID RETURNED FOR
C                       TOTAL PRECIPITATION. (INTERNAL)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE. (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF
C                       THE GRID IS NOT KNOWN BEFORE FD1 AND FD2
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
C               NSLAB = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID. (OUTPUT)
C              NSLABP = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  USED AFTER SUBROUTINE GFETCH
C                       TO CHECK IF NSLABC = NSLAB FOR CONVECTIVE
C                       PRECIPITATION OVER A 3,6,OR 12 HOUR 
C                       PERIOD. (OUTPUT)
C              NSLABT = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  USED AFTER SUBROUTINE GFETCH
C                       TO CHECK IF NSLABP =NSLAB FOR TOTAL 3,6, OR
C                       12 HOUR PRECIPITATION. (OUTPUT)
C        1         2         3         4         5         6         7 X
C
C        NON SYSTEM SUBROUTINES USED
C            GFETCH,TPCP3,TPCP6,TPCP12,TPCP24
C
      IMPLICIT NONE
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IWORK(ND5),IPACK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER MCP3(4),MCP6(4),MCP12(4),MCP24(4),MTP3(4),MTP6(4),
     1        MTP12(4),MTP24(4),MDPARS(15),ICCCFFF(8)
      INTEGER I,IER,ISTAV,J,KFILDO,KFIL10,L3264B,LITEMS,
     1        MCX,MCY,MISTOT,MTX,MTY,
     2        NBLOCK,ND2X3,ND5,ND7,ND9,ND10,
     3        ND11,NDATE,NFETCH,NSLAB,NSLABC,NSLABT
C
      REAL FDTP(ND5)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),
     1     FD5(ND2X3),FD6(ND2X3),FD7(ND2X3)
      REAL CORE(ND10)
C
      DATA ICCCFFF/003205,
     1             003235,
     2             003210,
     3             003240,
     4             003220,
     5             003250,
     6             003226,
     7             003256/
   
C       ABOVE IDS ARE IN ORDER:
C        3-HR TOTAL
C        3-HR CONVECTIVE
C        6-HR TOTAL
C        6-HR CONVECTIVE
C       12-HR TOTAL  
C       12-HR CONVECTIVE
C       24-HR TOTAL  
C       24-HR CONVECTIVE
C
      IER=0
      ISTAV=0
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C
      IF(IDPARS(1).NE.003.OR.(IDPARS(2).NE.265.AND.
     1   IDPARS(2).NE.270.AND.IDPARS(2).NE.280.AND.
     2   IDPARS(2).NE.286))THEN
         WRITE(KFILDO,101)(JD(J),J=1,4)
 101     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1           '3-, 6-, 12-, OR 24-HR NON CONV PRECIP AMT.',
     2          /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3           ' NOT COMPUTED IN NONCNVP.')
         IER=103
         GOTO 800
      END IF
C
C        CREATE ID FOR TOTAL PRECIP AMT.
C        THE ID DEPENDS ON WHICH NONCNV TIME THE USER WANTS.
C
      IF(IDPARS(2).EQ.265)THEN
C
C          GET THE 3-HR TOTAL.
C
        MTP3(1)=ICCCFFF(1)*1000+IDPARS(4)
        MTP3(2)=IDPARS(7)
        MTP3(3)=IDPARS(9)*1000000+IDPARS(12)
        MTP3(4)=0
C
        CALL PRSID1(KFILDO,MTP3,MDPARS)
        CALL TPCP3(KFILDO,KFIL10,MDPARS,MTP3,NDATE,
     1             NGRIDC,ND11,NSLAB,IPACK,IWORK,FD1,ND5,
     2             LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3             IS0,IS1,IS2,IS4,ND7,FD2,
     4             ND2X3,ISTAV,L3264B,MISTOT,IER)
        IF(IER.NE.0)GO TO 800
        NSLABT=NSLAB
        MTX=IS2(3)
        MTY=IS2(4)
C
C          GET THE 3-HR CONVECTIVE.
C
        MCP3(1)=ICCCFFF(2)*1000+IDPARS(4)
        MCP3(2)=IDPARS(7)
        MCP3(3)=IDPARS(9)*1000000+IDPARS(12)
        MCP3(4)=0
C
        CALL PRSID1(KFILDO,MCP3,MDPARS)
        CALL TPCP3(KFILDO,KFIL10,MDPARS,MCP3,NDATE,
     1             NGRIDC,ND11,NSLAB,IPACK,IWORK,FD2,ND5,
     2             LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3             IS0,IS1,IS2,IS4,ND7,FD3,
     4             ND2X3,ISTAV,L3264B,MISTOT,IER)
        IF(IER.NE.0)GO TO 800
        NSLABC=NSLAB
        MCX=IS2(3)
        MCY=IS2(4)
C
C          CHECK THE NSLABS.
C
        IF(NSLABT.NE.NSLABC)THEN
          WRITE(KFILDO,105)NSLABT,NSLABC,(JD(J),J=1,4)
 105      FORMAT(/' ****THE GRID CHARACTERISTICS OF THE 3-HR NON',
     1            ' CONVECTIVE PRECIPITATION ARE DIFFERENT.',I3,2X,I3,
     2           /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3            ' NOT COMPUTED IN NONCNVP.')
          IER=100
          GOTO 800
        END IF
C
C          COMPARE IF THE GRID CHARACTERISTICS ARE THE SAME.
C
        IF(MTX.NE.MCX.OR.MTY.NE.MCY)THEN
C           THE GRID CHARACTERISTICS ARE THE SAME
          WRITE(KFILDO,110)(MTP3(J),J=1,4),(NGRIDC(J,NSLABT),J=1,6),
     1                      MTX,MTY,
     2                     (MCP3(J),J=1,4),(NGRIDC(J,NSLABC),J=1,6),
     3                      MCX,MCY
 110      FORMAT(/,' ****THE GRIDS NEEDED IN NONCNVP HAVE DIFFERENT',
     1             ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2             ' VALUES FROM NGRIDC(,) AND MX,MY.',
     3             (/5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
          IER=100
          GOTO 800
        END IF
C
C          COMPUTE THE NON-CONVECTIVE AS TOTAL MINUS CONVECTIVE.
C
        DO I=1,MTX*MTY
          FDTP(I)=FD1(I)-FD2(I)
        END DO
C
        GOTO 900
C
      ELSEIF(IDPARS(2).EQ.270)THEN
C
C          GET THE 6-HR TOTAL.
C
        MTP6(1)=ICCCFFF(3)*1000+IDPARS(4)
        MTP6(2)=IDPARS(7)
        MTP6(3)=IDPARS(9)*1000000+IDPARS(12)
        MTP6(4)=0
        CALL PRSID1(KFILDO,MTP6,MDPARS)
        CALL TPCP6(KFILDO,KFIL10,MDPARS,MTP6,NDATE,
     1             NGRIDC,ND11,NSLAB,IPACK,IWORK,FD1,ND5,
     2             LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3             IS0,IS1,IS2,IS4,ND7,FD2,FD3,
     4             ND2X3,ISTAV,L3264B,MISTOT,IER)
        IF(IER.NE.0)GO TO 800
        NSLABT=NSLAB
        MTX=IS2(3)
        MTY=IS2(4)
C
C          GET THE 6-HR CONVECTIVE.
C
        MCP6(1)=ICCCFFF(4)*1000+IDPARS(4)
        MCP6(2)=IDPARS(7)
        MCP6(3)=IDPARS(9)*1000000+IDPARS(12)
        MCP6(4)=0
C
        CALL PRSID1(KFILDO,MCP6,MDPARS)
        CALL TPCP6(KFILDO,KFIL10,MDPARS,MCP6,NDATE,
     1             NGRIDC,ND11,NSLAB,IPACK,IWORK,FD2,ND5,
     2             LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3             IS0,IS1,IS2,IS4,ND7,FD3,FD4,
     4             ND2X3,ISTAV,L3264B,MISTOT,IER)
        IF(IER.NE.0)GO TO 800
        NSLABC=NSLAB
        MCX=IS2(3)
        MCY=IS2(4)
C
C          CHECK THE NSLABS.
C
        IF(NSLABT.NE.NSLABC)THEN
          WRITE(KFILDO,120)NSLABT,NSLABC,(JD(J),J=1,4)
 120      FORMAT(/' ****THE GRID CHARACTERISTICS OF THE 6-HR NON',
     1            ' CONVECTIVE PRECIPITATION ARE DIFFERENT.',I3,2X,I3,
     2           /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3            ' NOT COMPUTED IN NONCNVP.')
          IER=100
          GOTO 800
        END IF
C
C          COMPARE IF THE GRID CHARACTERISTICS ARE THE SAME.
C
        IF(MCX.NE.MTX.OR.MCY.NE.MTY)THEN
C            THE GRID CHARACTERISTICS ARE THE SAME
          WRITE(KFILDO,125)(MTP6(J),J=1,4),(NGRIDC(J,NSLABT),J=1,6),
     1                      MTX,MTY,
     2                     (MCP6(J),J=1,4),(NGRIDC(J,NSLABC),
     3                      J=1,6),MCX,MCY
 125      FORMAT(/,' ****THE GRIDS NEEDED IN NONCNVP HAVE DIFFERENT',
     1             ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2             ' VALUES FROM NGRIDC(,) AND MX,MY.',
     3             (/5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
          IER=100
          GOTO 800
        END IF
C
C          COMPUTE THE NON-CONVECTIVE AS TOTAL MINUS CONVECTIVE.
C
        DO I=1,MTX*MTY
          FDTP(I)=FD1(I)-FD2(I)
          IF (FDTP(I).LT.0.)FDTP(I)=0.
        END DO
C
        GOTO 900
C
      ELSEIF(IDPARS(2).EQ.280)THEN
C
C          GET THE 12-HR TOTAL.
C
        MTP12(1)=ICCCFFF(5)*1000+IDPARS(4)
        MTP12(2)=IDPARS(7)
        MTP12(3)=IDPARS(9)*1000000+IDPARS(12)
        MTP12(4)=0
        CALL PRSID1(KFILDO,MTP12,MDPARS)
        CALL TPCP12(KFILDO,KFIL10,MDPARS,MTP12,NDATE,
     1              NGRIDC,ND11,NSLAB,IPACK,IWORK,FD1,ND5,
     2              LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3              IS0,IS1,IS2,IS4,ND7,FD2,FD3,FD4,
     4              ND2X3,ISTAV,L3264B,MISTOT,IER)
        IF(IER.NE.0)GO TO 800
        NSLABT=NSLAB
        MTX=IS2(3)
        MTY=IS2(4)
C
C          GET THE 12-HR CONVECTIVE.
C
        MCP12(1)=ICCCFFF(6)*1000+IDPARS(4)
        MCP12(2)=IDPARS(7)
        MCP12(3)=IDPARS(9)*1000000+IDPARS(12)
        MCP12(4)=0
        CALL PRSID1(KFILDO,MCP12,MDPARS)
        CALL TPCP12(KFILDO,KFIL10,MDPARS,MCP12,NDATE,
     1              NGRIDC,ND11,NSLAB,IPACK,IWORK,FD2,ND5,
     2              LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3              IS0,IS1,IS2,IS4,ND7,FD3,FD4,FD5,
     4              ND2X3,ISTAV,L3264B,MISTOT,IER)
        IF(IER.NE.0)GO TO 800
        NSLABC=NSLAB
        MCX=IS2(3)
        MCY=IS2(4)
C
C          CHECK THE NSLABS.
C
        IF(NSLABT.NE.NSLABC)THEN
          WRITE(KFILDO,140)NSLABT,NSLABC,(JD(J),J=1,4)
 140      FORMAT(/' ****THE GRID CHARACTERISTICS OF THE 12-HR NON',
     1            ' CONVECTIVE PRECIPITATION ARE DIFFERENT.',I3,2X,I3,
     2           /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3            ' NOT COMPUTED IN NONCNVP.')
          IER=100
          GOTO 800
        END IF
C
C          COMPARE IF THE GRID CHARACTERISTICS ARE THE SAME
C
        IF(MTX.NE.MCX.OR.MTY.NE.MCY)THEN
C            THE GRID CHARACTERISTICS ARE THE SAME
          WRITE(KFILDO,145)(MTP12(J),J=1,4),(NGRIDC(J,NSLABT),J=1,6),
     1                      MTX,MTY,
     2                     (MTP12(J),J=1,4),(NGRIDC(J,NSLABC),
     3                      J=1,6),MCX,MCY
 145      FORMAT(/,' ****THE GRIDS NEEDED IN NONCNVP HAVE DIFFERENT',
     1             ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2             ' VALUES FROM NGRIDC(,) AND MX,MY.',
     3             (/5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
          IER=100
          GOTO 800
        END IF
C
C          COMPUTE THE NON-CONVECTIVE AS TOTAL MINUS CONVECTIVE.
C
        DO I=1,MTX*MTY
          FDTP(I)=FD1(I)-FD2(I)
          IF(FDTP(I).LT.0.)FDTP(I)=0.
        END DO
C
        GOTO 900
C
      ELSE
C
C          GET THE 24-HR TOTAL.  NOTE THAT FDTP() IS BEING
C          USED DIRECTLY HERE.  THIS IS BECAUSE 6 WORK ARRAYS
C          ARE NEEDED IN TPCP24, PLUS TWO TO HOLD TOTAL AND
C          CONVECTIVE PRECIP.  HOWEVER, ONLY SEVEN ARE AVAILABLE
C          FROM OPTION.
C
        MTP24(1)=ICCCFFF(7)*1000+IDPARS(4)
        MTP24(2)=IDPARS(7)
        MTP24(3)=IDPARS(9)*1000000+IDPARS(12)
        MTP24(4)=0
        CALL PRSID1(KFILDO,MTP24,MDPARS)
        CALL TPCP24(KFILDO,KFIL10,MDPARS,MTP24,NDATE,
     1              NGRIDC,ND11,NSLAB,IPACK,IWORK,FDTP,ND5,
     2              LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3              IS0,IS1,IS2,IS4,ND7,FD1,FD2,FD3,FD4,FD5,FD6,
     4              ND2X3,ISTAV,L3264B,MISTOT,IER)
        IF(IER.NE.0)GO TO 800
        NSLABT=NSLAB
        MTX=IS2(3)
        MTY=IS2(4)
C
C          GET THE 24-HR CONVECTIVE.
C
        MCP24(1)=ICCCFFF(8)*1000+IDPARS(4)
        MCP24(2)=IDPARS(7)
        MCP24(3)=IDPARS(9)*1000000+IDPARS(12)
        MCP24(4)=0
        CALL PRSID1(KFILDO,MCP24,MDPARS)
        CALL TPCP24(KFILDO,KFIL10,MDPARS,MCP24,NDATE,
     1              NGRIDC,ND11,NSLAB,IPACK,IWORK,FD1,ND5,
     2              LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3              IS0,IS1,IS2,IS4,ND7,FD2,FD3,FD4,FD5,FD6,FD7,
     4              ND2X3,ISTAV,L3264B,MISTOT,IER)
        IF(IER.NE.0)GO TO 800
        NSLABC=NSLAB
        MCX=IS2(3)
        MCY=IS2(4)
C
C          CHECK THE NSLABS.
C
        IF(NSLABT.NE.NSLABC)THEN
          WRITE(KFILDO,150)NSLABT,NSLABC,(JD(J),J=1,4)
 150      FORMAT(/' ****THE GRID CHARACTERISTICS OF THE 24-HR NON',
     1            ' CONVECTIVE PRECIPITATION ARE DIFFERENT.',I3,2X,I3,
     2           /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3            ' NOT COMPUTED IN NONCNVP.')
          IER=100
          GOTO 800
        END IF
C
C          COMPARE IF THE GRID CHARACTERISTICS ARE THE SAME
C
        IF(MTX.NE.MCX.OR.MTY.NE.MCY)THEN
C            THE GRID CHARACTERISTICS ARE THE SAME
          WRITE(KFILDO,155)(MTP24(J),J=1,4),(NGRIDC(J,NSLABT),J=1,6),
     1                      MTX,MTY,
     2                     (MTP24(J),J=1,4),(NGRIDC(J,NSLABC),
     3                      J=1,6),MCX,MCY
 155      FORMAT(/,' ****THE GRIDS NEEDED IN NONCNVP HAVE DIFFERENT',
     1             ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2             ' VALUES FROM NGRIDC(,) AND MX,MY.',
     3             (/5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
          IER=100
          GOTO 800
        END IF
C
C          COMPUTE THE NON-CONVECTIVE AS TOTAL MINUS CONVECTIVE.
C
        DO I=1,MTX*MTY
          FDTP(I)=FDTP(I)-FD1(I)
          IF(FDTP(I).LT.0.)FDTP(I)=0.
        END DO
C
        GOTO 900
C
      END IF
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C 
 800  DO 801 J=1,ND2X3
        FDTP(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END
