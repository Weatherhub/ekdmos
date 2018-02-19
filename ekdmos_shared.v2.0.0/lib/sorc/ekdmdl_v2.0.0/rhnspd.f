      SUBROUTINE RHNSPD(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                  NGRIDC,ND11,NSLAB,IPACK,IWORK,FFOG,ND5,
     2                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                  IS0,IS1,IS2,IS4,ND7,
     4                  FD1,FD2,ND2X3,
     5                  ISTAV,L3264B,MISTOT,IER)
C
C        JULY 2003      YAN        MDL       MOS-2000
C                       IN REFERENCE TO PAPER RADIATION FOG: UPS AIRLINES CONCEPTUAL
C                       MODEL AND FORECAST METHODS BY RANDY BAKER ET AL
C        APRIL    2004  RLC   CHANGED COMPUTATIONAL LOOPS TO USE MHX*MHY INSTEAD
C                             OF ND5.  ALSO CHANGED 800 LOOP TO USE ND2X3 NOT ND5.
C        MAY      2005  JPD/MSA  CHANGED CALLS TO GFETCH AND WSPEED TO PASS 
C                                PROPER DIMENSION OF FD1/FD2 AS ND2X3, NOT ND5.
C                                RESTRUCTURED CALL SEQUENCE FOR RHNSPD TO
C                                MATCH THAT IN OPTION/OPTN2.
C              
C        PURPOSE
C            TO COMPUTE THE UPS FOG POTENTIALS.  THIS IS USED IN THE REGRESSION
C            ANALYSIS OF VISIBILITY AND OBSTRUCTION TO VISION. THE PREDICTOR
C            VALUE IS DEFINED AS:
C
C            3 = MEAN RH IN PBL IS AT LEAST 90%, AND 10M WIND SPEED IS LESS THAN 3 M/S
C            2 = MEAN RH IN PBL IS AT LEAST 90%, AND 10M WIND SPEED IS AT LEAST 3 M/S
C            1 = MEAN RH IN PBL IS LESS THAN 90%, AND 10M WIND SPEED IS LESS THAN 3 M/S
C            0 = MEAN RH IN PBL IS LESS THAN 90%, AND 10M WIND SPEED IS AT LEAST 3 M/S
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C                        003067
C
C        DATA SET USE
C            KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT(PRINT) FILE.
C                     (OUTPUT)
C            KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                     ACCESS.(INPUT-OUTPUT)
C
C        VARIABLES
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
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
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
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
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN
C                       BE DEALT WITH ON THIS RUN.  LAST DIMENSION 
C                       OF NGRIDC(,). (INPUT)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID. (OUTPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5). (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5). (INTERNAL)
C             FFOG(J) = DATA ARRAY TO HOLD RETURNED DATA AT
C                       GRIDPOINTS. IN THIS CASE, THE PRODUCT
C                       OF THE TEMP(HIGHEST LEVEL) AND THE RH DIVIDED 
C                       BY THE PRODUCT OF THE TEMP(LOWEST LEVEL) AND 
C                       THE AVERAGE WIND SPEED BETWEEN THE UPPER AND 
C                       LOWER LAYER (J=1,ND5). (OUTPUT)
C                 ND5 = DIMENSION OF IPACK(), AND IWORK(). (INPUT)
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
C                 ND9 = THE SECOND DIMENSION OF LSTORE(,). (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE(,)
C                       THAT HAVE BEEN USED IN THIS RUN. (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE(,) (J=1,ND10).
C                       WHEN CORE() IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C                ND10 = DIMENSION OF CORE(). (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE. (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING COUNT FROM THE BEGINNING OF
C                       THE PROGRAM.  THIS COUNT IS MAINTAINED IN
C                       CASE THE USER NEEDS IT(DIAGNOSTICS, ETC.).
C                       (OUTPUT)
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
C                 ND7 = DIMENSION OF IS0(),IS1(),IS2(), AND IS4().
C                       NOT ALL LOCATIONS ARE USED. (INPUT)
C              FD1(J) = WORK ARRAY TO HOLD THE TEMPERATURE AT LOWEST LEVEL.
C                       (J=1,ND2X3). (INTERNAL)
C              FD2(J) = WORK ARRAY TO HOLD THE TEMPERATURE AT HIGHEST LEVEL.
C                       (J=1,ND2X3). (INTERNAL)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF
C                       THE GRID IS NOT KNOWN BEFORE FD1 AND FD2
C                       ARE FETCHED.  (INPUT)
C               ISTAV = 0 SINCE THE DATA RETURNED ARE GRID DATA.
C                       (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64). (INPUT)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                       100 = THE TWO GRIDS NEEDED ARE NOT THE SAME SIZE
C                       101 = GRID SIZE IS TOO BIG FOR FFOG(), WHOSE 
C                             DIMENSION IS ND5.
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE
C                             THE PRODUCT OF THE RELATIVE HUMIDITY AND
C                             THE VERTICAL VELOCITY.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C          ICCCFFF(J) = CONTAINS IDPARS(1) AND IDPARS(2) ID FOR THE
C                       METEOROLOGICAL PARAMETERS BEING USED (J=1,4). (INTERNAL)
C            IUUUU(J) = VARIOUS ATMOSPHERIC PRESSURE LEVELS FROM WHICH 
C                       DATA WILL BE COLLECTED (J=1,5). (INTERNAL) 
C           ITEMP1(J) = HOLDS THE PARSED 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD1() (J=1,4). (INTERNAL)
C           ITEMP2(J) = HOLDS THE PARSED 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD2() (J=1,4). (INTERNAL)
C              IRH(J) = HOLDS THE PARSED 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD3() (J=1,4). (INTERNAL)
C          IRHPARS(J) = HOLDS THE PARSED 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD3() (J=1,4). (INTERNAL)
C           LDPARS(J) = HOLDS THE PARSED 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD4() (J=1,4). (INTERNAL)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD4() (J=1,4). (INTERNAL)
C          LDPARS1(J) = HOLDS THE PARSED 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD5() (J=1,4). (INTERNAL)
C              LD1(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD5() (J=1,4). (INTERNAL)
C             MDX,MDY = DIMENSIONS OF GRID RETURNED FOR THE TEMPERATURE
C                       AT THE LOWEST LEVEL. (INTERNAL)
C             MNX,MNY = DIMENSIONS OF GRID RETURNED FOR THE TEMPERATURE
C                       AT THE HIGHEST LEVEL. (INTERNAL)
C             MHX,MHY = DIMENSIONS OF GRID RETURNED FOR THE RELATIVE
C                       HUMIDITY. (INTERNAL)
C             MSX,MSY = DIMENSIONS OF GRID RETURNED FOR THE WIND SPEED 
C                       AT THE LOWEST LEVEL. (INTERNAL)
C             MVX,MVY = DIMENSIONS OF GRID RETURNED FOR THE WIND SPEED
C                       AT THE HIGHEST LEVEL. (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 FROM CALLING GFETCH WHEN THERE IS NO
C                       PRIMARY MISSING VALUE. (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 FROM CALLING GFETCH WHEN THERE IS NO
C                       SECONDARY MISSING VALUE.  (INTERNAL)
C               NPACK = 2 FOR TDL GRIB PACKED DATA; 1 FOR NOT PACKED
C                       THIS IS RETURNED FROM GFETCH. (INTERNAL)
C              NSLABT = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR TEMPERATURE AT LOWEST LEVEL. (INTERNAL)
C              NSLABD = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR TEMPERATURE AT HIGHEST LEVEL. (INTERNAL)
C              NSLABH = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR THE RELATIVE HUMIDITY. (INTERNAL)
C              NSLABS = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR THE WIND SPEED AT THE LOWEST LEVEL. (INTERNAL)
C              NSLABV = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR THE WIND SPEED AT THE HIGHEST LEVEL. (INTERNAL)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE,
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS 
C                       STORED IN LSTORE(9,). (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA().  THIS 
C                       IS RETURNED FROM GFETCH (INTERNAL)
C                   I = LOOP CONTROL VARIABLE (INTERNAL)
C                   J = LOOP CONTROL VARIABLE (INTERNAL)
C
C
C        NON SYSTEM SUBROUTINES USED
C        GFETCH,PRSID1,WSPEED,MEANRH
C
      IMPLICIT NONE
      INTEGER ICCCFFF(8),IUUUU(3)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER IDPARS(15),LDPARS(15),LDPARS1(15),IRHPARS(15)
      INTEGER JD(4)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER I,IER,ISTAV,J,KFILDO,KFIL10,L3264B,
     1        LITEMS,MDX,MDY,MISSP,MISSS,MISTOT,MVX,MVY,NSLABV,
     2        MNX,MNY,NBLOCK,ND2X3,ND5,ND7,ND9,MHX,MHY,
     3        ND10,ND11,NDATE,NFETCH,NPACK,NSLAB,MSX,MSY,
     4        NSLABD,NSLABT,NSLABH,NSLABS,NTIMES,NWORDS
      INTEGER LD(4),LD1(4),ITEMP1(4),ITEMP2(4),IRH(4)
      REAL CORE(ND10)
      REAL FFOG(ND5),UWIND(ND2X3),VWIND(ND2X3)
      REAL FD1(ND2X3),FD2(ND2X3)
C
C        BOUNDARY LAYER RELATIVE HUMIDITY ID
      ICCCFFF(1)=003007
C        10M GRIDDED WIND SPEED ID
      ICCCFFF(2)=004211

      IUUUU(1)=970
      IUUUU(2)=10
C
      IER=0
      ISTAV=0
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C
      IF(IDPARS(1).NE.003.AND.IDPARS(2).NE.067) THEN
         WRITE(KFILDO,101)(JD(J),J=1,4)
 101     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1           'CORRECT ID FOR RHNSPD PREDICTOR',I9.9,2I10.9,
     2           I4.3, 'THEREFORE, NOT COMPUTED IN RHNSPD. ')
         IER=101
         GOTO 800
      END IF
C
C        CONSTRUC ID FOR THE MEAN PBL (30 MB) RH (00300700X 000000970 000000000)
C
      IRH(1)=ICCCFFF(1)*1000+IDPARS(4)
      IRH(2)=IUUUU(1)
      IRH(3)=IDPARS(9)*1000000+IDPARS(12)
      IRH(4)=0
C
C        FETCH MEAN PBL (30 MB) RH USING GFETCH
C        FD1 IS DIMENSIONED AS ND5 HERE.
C
      CALL GFETCH(KFILDO,KFIL10,IRH,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3            NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
C
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GOTO 800
      MHX=IS2(3)
      MHY=IS2(4)
      NSLABH=NSLAB
C
C        CREATE ID FOR 10M WIND SPEEDS
C
      LD(1)=ICCCFFF(2)*1000+IDPARS(4)
      LD(2)=IUUUU(2)
      LD(3)=IDPARS(9)*1000000+IDPARS(12)
      LD(4)=10
C
C        FETCH THE WIND SPEED AT 10M LEVEL
C        FD2 IS DIMENSIONED AS ND5 
C
C        CALL PRSID1 TO PARSE IDS LD( ) TO LDPARS( )
C
      CALL PRSID1(KFILDO,LD,LDPARS)
      CALL WSPEED(KFILDO,KFIL10,LDPARS,LD,NDATE,
     1            NGRIDC,ND11,NSLAB,IPACK,IWORK,FD2,ND2X3,
     2            LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3            IS0,IS1,IS2,IS4,ND7,
     4            UWIND,VWIND,ND2X3,
     5            ISTAV,L3264B,MISTOT,IER)
C
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GOTO 800
      MSX=IS2(3)
      MSY=IS2(4)
      NSLABS=NSLAB
C
      IF(NSLABS.NE.NSLABH)THEN
        WRITE(KFILDO,170)NSLABD,NSLABS
 170    FORMAT(/' ****THE GRID CHARACTERISTICS OF THE ',
     1          ' 925 (700) MB TEMPERATURE AND THE 1000 ',
     2          '(850) MB WIND SPEED ARE DIFFERENT.',4I3,2X,I3)
        IER=100
        GOTO 800
      END IF
C
      IF(MSX.NE.MHX.OR.MSY.NE.MHY)THEN
C
C           THE GRID CHARACTERISTICS ARE THE SAME
C
         WRITE(KFILDO,175)(ITEMP2(J),J=1,4),(NGRIDC(J,NSLABD),
     1                    J=1,6),MHX,MHY,
     2                    (LD(J),J=1,4),(NGRIDC(J,NSLABS),
     3                    J=1,6),MSX,MSY
  175    FORMAT(/' ****THE GRIDS NEEDED IN RHNSPD HAVE ',
     1           'DIFFERENT CHARACTERISTICS.  PREDICTOR ',
     2           'NOT COMPUTED. VALUES FROM NGRIDC(,) AND ',
     3           'MSX,MSY.'/
     4        (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
         IER=100
         GOTO 800
       END IF
C
C        BEGIN COMPUTATION OF THE PRODUCT OF THE RHNSPD PREDICTOR:
C    
      DO I=1,MHX*MHY
         IF((NINT(FD1(I)).NE.9999).AND.(NINT(FD2(I)).NE.9999)) THEN
            IF(FD1(I).GE.90.) THEN
               IF(FD2(I).LT.3.) THEN
                  FFOG(I)=3.
               ELSE
                  FFOG(I)=2.
               ENDIF
            ELSE
               IF(FD2(I).LT.3.) THEN
                  FFOG(I)=1.
               ELSE
                  FFOG(I)=0.
               ENDIF
            ENDIF
         ENDIF
      ENDDO 
C
      GOTO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C 
 800  DO 801 J=1,ND2X3
        FFOG(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END
