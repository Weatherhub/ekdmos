      SUBROUTINE TTOTALS(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                   NGRIDC,ND11,NSLAB,IPACK,IWORK,FDTT,ND5,
     2                   LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                   IS0,IS1,IS2,IS4,ND7,
     4                   FD1,FD2,FD3,FD4,FD5,ND2X3,
     5                   ISTAV,L3264B,MISTOT,IER)
C
C        JULY      1998   SFANOS  TDL   MOS-2000
C        SEPTEMBER 1998   SFANOS  CHANGED ALL WORK ARRAYS
C                                 EXCEPT FOR DATA TO ND2X3
C        DECEMBER  2002   WEISS   CHANGED ND5 TO ND2X3
C        MAY       2003   GLAHN   CHANGED DIMENSIONS OF FDTT( ),
C                                 IPACK( ), IWORK( ) TO ND5; REARRANGED
C                                 TYPE STATEMENTS; ADDED DIAGNOSTICS; 
C                                 PUT ICCCFFF( ) IN DATA STATEMENT;
C                                 MADE "2" REAL AND REMOVED PARENTHESES
C                                 IN CALCULATION  OF TT AT END
C                                 
C        PURPOSE
C            TO COMPUTE THE TOTAL TOTALS = 850T + 850TD - 2*500T
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C                007 210 - TOTAL TOTALS
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
C              FD1(K) = WORK ARRAY (K=1,ND2X3). (INTERNAL)
C              FD2(K) = WORK ARRAY (K=1,ND2X3). (INTERNAL)
C              FD3(K) = WORK ARRAY THAT HOLDS THE 500 MB
C                       TEMPERATURE (K=1,ND2X3). (INTERNAL)
C              FD4(K) = WORK ARRAY THAT HOLDS THE 850 MB
C                       TEMPERATURE (K=1,ND2X3). (INTERNAL)
C              FD5(K) = WORK ARRAY THAT HOLDS THE 850 MB
C                       DEWPOINT (K=1,ND2X3). (INTERNAL)
C             FDTT(K) = DATA ARRAY TO HOLD THE TOTAL TOTALS (ND5)
C                       (OUTPUT)
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
C                             TOTAL TOTALS INDEX.
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
C               MD8() = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD5() (J=1,4). (INTERNAL)
C            MDPARS() = PARSED ID USED IN SUBROUTINE PRSID1 FOR
C                       SUBROUTINE DEWPOINT
C           MDX8,MDY8 = DIMENSIONS OF THE GRID RETURNED FOR 850
C                       DEWPOINT (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 FROM CALLING GFETCH WHEN THERE IS NO
C                       MISSING VALUE. (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 FROM CALLING GFETCH WHEN THERE IS NO
C                       MISSING VALUE. (INTERNAL)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C               MT5() = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD3() (J=1,4). (INTERNAL)
C               MT8() = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD4() (J=1,4). (INTERNAL)
C           MTX5,MTY5 = DIMENSIONS OF THE GRID RETURNED FOR 500 MB
C                       TEMPERATURE (INTERNAL)
C           MTX8,MTY8 = DIMENSIONS OF THE GRID RETURNED FOR 800 MB 
C                       TEMPERATURE (INTERNAL)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE. (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF
C                       THE GRID IS NOT KNOWN BEFORE FDTK5, FDTK8, AND
C                       DPT ARE FETCHED.  (INPUT)
C                 ND5 = DIMENSION OF IPACK(), WORK() AND FDDD( ). 
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
C             NSLABT5 = THE NUMBER USED TO COMPARE TO NSLAB THAT
C                       IS RETURNED FROM SUBROUTINE GFETCH AFTER
C                       FETCHING THE 500MB TEMP AND IS USED AS A CHECK
C             NSLABT8 = THE NUMBER USED TO COMPARE TO NSLAB THAT
C                       IS RETURNED FROM SUBROUTINE GFETCH AFTER
C                       FETCHING THE 850MB TEMP AND IS USED AS A CHECK
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE,
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS 
C                       STORED IN LSTORE(9,). (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA().  THIS 
C                       IS RETURNED FROM GFETCH (INTERNAL)
C
C     NON SYSTEM SUBROUTINES USED
C        GFETCH,DEWPT,PRSID1
C
      IMPLICIT NONE
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER ICCCFFF(2),MD8(4),MT5(4),MT8(4),MDPARS(15)
      INTEGER IER,IJ,ISTAV,J,KFILDO,KFIL10,L3264B,LITEMS,
     1        MISSP,MISSS,MISTOT,MDX8,MDY8,MTX5,MTY5,MTX8,MTY8,
     2        NBLOCK,ND2X3,ND5,ND7,ND9,ND10,ND11,NDATE,
     3        NFETCH,NPACK,NSLAB,NSLABD,NSLABT5,
     4        NSLABT8,NTIMES,NWORDS
C
      REAL FDTT(ND5)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),FD5(ND2X3)
      REAL CORE(ND10)
C
      DATA ICCCFFF/002000,
     1             003100/       
C        ABOVE IDS ARE IN ORDER:
C          TEMPERATURE ON AN ISOBARIC SURFACE
C          DEWPOINT ON AN ISOBARIC SURFACE  
C
      IER=0
      ISTAV=0
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C
      IF(IDPARS(1).NE.007.OR.IDPARS(2).NE.210)THEN
        WRITE(12,101)(JD(J),J=1,4)
 101    FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE',
     1          ' TOTAL TOTALS PREDICTOR. ',I9.9,2I10.9,I4.3,
     2          ' NOT COMPUTED IN TTOTALS. ')
        IER=103
        GOTO 800
      END IF
C
C        CREATE ID FOR 850 MB DEW POINT TEMPERATURE.
C
      MD8(1)=ICCCFFF(2)*1000+IDPARS(4)
      MD8(2)=850
      MD8(3)=IDPARS(9)*1000000+IDPARS(12)
      MD8(4)=0
C
C        CALL DEWPT TO RETURN THE 850 DEWPOINT TEMPERATURE.
C        NOTE THAT FD5 IS DIMENSIONED AS ND2X3 IN CALL TO
C        SUBROUTINE DEWPT.
C
      CALL PRSID1(KFILDO,MD8,MDPARS)
      CALL DEWPT(KFILDO,KFIL10,MDPARS,MD8,NDATE,
     1           NGRIDC,ND11,NSLAB,IPACK,IWORK,FD5,ND2X3,
     2           LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3           IS0,IS1,IS2,IS4,ND7,
     4           FD1,FD2,FD3,FD4,ND2X3,
     5           ISTAV,L3264B,MISTOT,IER)
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,120)(JD(J),J=1,4)
 120     FORMAT(/'     VARIABLE ',I9.9,2I10.9,I4.3,
     1           ' NOT COMPUTED IN TTOTALS.')
C           THIS DIAGNOSTIC WILL FOLLOW ONEIN DEWPT.
         GOTO 800
      ENDIF
C
      NSLABD=NSLAB
      MDX8=IS2(3)
      MDY8=IS2(4)
C
C        CREATE ID FOR 850 TEMPERATURE.
C
      MT8(1)=ICCCFFF(1)*1000+IDPARS(4)
      MT8(2)=850
      MT8(3)=IDPARS(9)*1000000+IDPARS(12)
      MT8(4)=0
C
C        TO FETCH 850 MB TEMPERATURE.  NOTE THAT FD4 IS
C        DIMENSIONED AS ND2X3.
C
      CALL GFETCH(KFILDO,KFIL10,MT8,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD4,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,130)(JD(J),J=1,4)
 130     FORMAT(/' ****VARIABLE ',I9.9,2I10.9,I4.3,
     1           ' NOT COMPUTED IN TTOTALS.  850-MB TEMPERATURE',
     2           ' NOT FOUND BY GFETCH.')
         GOTO 800
      ENDIF
C
      MTX8=IS2(3)
      MTY8=IS2(4)
      NSLABT8=NSLAB
C
C        COMPARE THE GRID CHARACTERISTICS.
C
      IF(MTX8.NE.MDX8.OR.MTY8.NE.MDY8)THEN
C        THE GRID CHARACTERISTICS ARE NOT THE SAME
        WRITE(KFILDO,135)(MD8(J),J=1,4),(NGRIDC(J,NSLABD),J=1,6),
     1                    MDX8,MDY8,
     2                    (MT8(J),J=1,4),(NGRIDC(J,NSLABT8),J=1,6),
     3                    MTX8,MTY8
 135    FORMAT(/' ****THE GRIDS NEEDED IN TTOTALS HAVE DIFFERENT',
     1          ' CHARACTERISTICS AT 135.  PREDICTOR NOT COMPUTED.',
     2          ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3          (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
      END IF
C
C        CHECK IF NSLABT8 EQUALS NSLABT5.
C
      IF(NSLABT8.NE.NSLABD)THEN
        WRITE(KFILDO,140)NSLABT8,NSLABT5,(JD(J),J=1,4)
 140    FORMAT(/' ****THE GRID CHARACTERISTICS OF THE ',
     1          'TEMP AND DEWPT AT 850 MB ARE DIFFERENT.',I3,2X,I3,
     2         /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3          ' NOT COMPUTED IN TTOTALS.')
        IER=100
        GOTO 800
      END IF
C      
C        CREATE ID FOR 500 MB TEMPERATURE.
C
      MT5(1)=ICCCFFF(1)*1000+IDPARS(4)
      MT5(2)=500
      MT5(3)=IDPARS(9)*100000+IDPARS(12)
      MT5(4)=0
C
C        NOW FETCH 500 MB TEMPERATURE.  NOTE THAT FD3 IS
C        PASSED AS ND2X3.
C
      CALL GFETCH(KFILDO,KFIL10,MT5,7777,LSTORE,ND9,LITEMS,IS0,
     1            IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,142)(JD(J),J=1,4)
 142     FORMAT(/' ****VARIABLE ',I9.9,2I10.9,I4.3,
     1           ' NOT COMPUTED IN TTOTALS.  500-MB TEMPERATURE',
     2           ' NOT FOUND BY GFETCH.')
         GOTO 800
      ENDIF
C
C
      MTX5=IS2(3)
      MTY5=IS2(4)
      NSLABT5=NSLAB
C
C        COMPARE GRID CHARACTERISTICS.
C
      IF(MTX8.NE.MTX5.OR.MTY8.NE.MTY5)THEN
C         THE GRID CHARACTERISTICS ARE NOT THE SAME
        WRITE(KFILDO,145)(MT5(J),J=1,4),(NGRIDC(J,NSLABT5),J=1,6),
     1                    MTX5,MTY5,
     2                   (MT8(J),J=1,4),(NGRIDC(J,NSLABT8),J=1,6),
     3                    MTX8,MTY8
 145    FORMAT(/' ****THE GRIDS NEEDED IN TTOTALS HAVE DIFFERENT',
     1          ' CHARACTERISTICS AT 145.  PREDICTOR NOT COMPUTED.',
     2          ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3          (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
      END IF
C
C        CHECK IF NSLABT8 EQUALS NSLABT5.
C
      IF(NSLABT8.NE.NSLABT5)THEN
        WRITE(KFILDO,150)NSLABT8,NSLABT5,(JD(J),J=1,4)
 150    FORMAT(/' ****THE GRID CHARACTERISTICS OF THE ',
     1          'TEMP AT 500 AND 850 MB ARE DIFFERENT.',I3,2X,I3,
     2         /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3          ' NOT COMPUTED IN TTOTALS.')
        IER=100
        GOTO 800
      END IF
C
C        COMPUTATION OF TOTAL TOTALS.
C
      DO 300 IJ=1,MDX8*MDY8
C
C          CHECK IF ANY OF VARIABLES ARE MISSING.
C
        IF(FD4(IJ).EQ.9999.OR.FD5(IJ).EQ.9999.OR.FD3(IJ).EQ.
     1                9999.)THEN
          FDTT(IJ)=9999.
          GOTO 300
        END IF
C  
C         TOTAL TOTALS COMPUTATION.
C
        FDTT(IJ)=FD4(IJ)+FD5(IJ)-2.*FD3(IJ)
 300  CONTINUE
C
      GOTO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C 
 800  DO 801 IJ=1,ND2X3
        FDTT(IJ)=9999.
 801  CONTINUE

 900  RETURN
      END
