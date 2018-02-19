      SUBROUTINE TPCP6(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                 NGRIDC,ND11,NSLAB,IPACK,IWORK,FDTP,ND5,
     2                 LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                 IS0,IS1,IS2,IS4,ND7,
     4                 FD1,FD2,ND2X3,
     5                 ISTAV,L3264B,MISTOT,IER)
C
C        SEPTEMBER 1998   SFANOS   TDL   MOS-2000
C        OCTOBER   1998   SFANOS   MODIFIED FOR CONV PRECIP
C        DECEMBER  1998   SFANOS   ERROR CODES MODIFIED
C        MAY       2000   DALLAVALLE MODIFIED FORMAT STATEMENTS
C                                  TO CONFORM TO FORTRAN 90
C                                  STANDARDS ON THE IBM-SP
C        APRIL     2001   MALONEY  ADDED CALCULATION FOR 6-HR PCP
C                                  USING 12-HR PCP AND 6-HR PCP FROM
C                                  T-6 PROJECTION
C        OCTOBER   2002   WEISS    CHANGED ND5 TO ND2X3
C        MAY       2003   GLAHN    DIAGNOSTICS; WHITE SPACE;
C                                  ICCCFFF( ) IN DATA STATEMENT;
C                                  REARRANGED TYPE STATEMENTS;
C                                  REPLACED FD1 BY FDTP IN ONE CALL TO
C                                  GFETCH AND ELIMINATED LOOPS SETTING 
C                                  FDTP( ) = FD1( ); CHANGED DIMENSIONS
C                                  OF IPACK( ), IWORK( ), FDTP( ) TO
C                                  ND5; INTERCHANGED .OR. AND .AND. AT
C                                  101; CHANGED LOOPS FROM 1,ND2X3 TO
C                                  1,NX*NY EXCEPT THE LAST ONE FOR ERROR;
C                                  PUT DIAGNOSTICS AFTER GFETCH
C
C        PURPOSE
C            TO COMPUTE THE 6-HR TOTAL OR CONVECTIVE PRECIP 
C            AMOUNT. THE FORMULA FOR THIS IS(DEPENDING ON THE
C            MODEL):
C            AVN/ETA= 6 HOUR PRECIP AT PROJECTION T 
C            AVN= A 3-HR PRECIP AT PROJECTION T + 6-HR PROJECTION
C                 AT T-6 - 3-HR AT T-9(FOR GREATER THAN 6 HOURS)
C            ETA= 12 HOUR PRECIP AT PROJECTION T MINUS 6 HOUR PRECIP
C                 AT T-6 PROJECTION.  (THIS MAY WORK FOR OTHERS BUT HAS
C                 ONLY BEEN TESTED FOR ETA.)
C                   
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               003 210 - 6-HR TOT PRECIP AMT
C                         VARIABLE ON AN ISOBARIC SURFACE
C               003 240 - 6-HR CONVECTIVE PRECIP AMT
C                         VARIABLE ON AN ISOBARIC SURFACE
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
C              FD1(J) = WORK ARRAY FOR 3 HOUR TOTAL OR CONVECTIVE
C                       PRECIPITATION (J=1,ND2X3) (INTERNAL)
C              FD2(J) = WORK ARRAY FOR 6 HOUR TOTAL OR CONVECTIVE 
C                       PRECIPITATION (J=1,ND2X3) (INTERNAL)
C             FDTP(J) = ARRAY TO HOLD RETURNED DATA WHEN THE DATA ARE
C                       AT GRIDPOINTS. IN THIS CASE, 6 HOUR
C                       TOTAL PRECIPITATION(J=1,ND2X3). (OUTPUT)
C                   I = LOOP CONTROL VARIABLE
C           ICCCFFF() = CONTAINS IDPARS(1) AND IDPARS(2) ID FOR THE
C                       METEOROLOGICAL PARAMETERS BEING USED.
C                       COLUMN 1 CONTAINS ID FOR ISOBARIC SURFACE.
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
C                             TPCP6.
C                       187 = PROJECTION OF ID IS INCORRECT.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND2X3). (INTERNAL)
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
C            IWORK(J) = WORK ARRAY (J=1,ND2X3). (INTERNAL)
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
C             MDX,MDY = DIMENSIONS OF GRID RETURNED FOR 6 HOUR
C                       TOTAL OR CONVECTIVE PRECIPITATION 
C                       AMOUNT (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 FROM CALLING GFETCH WHEN THERE IS NO
C                       PRIMARY MISSING VALUE. (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 FROM CALLING GFETCH WHEN THERE IS NO
C                       SECONDARY MISSING VALUE. (INTERNAL)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C             MLX,MLY = DIMENSIONS OF GRID RETURNED FOR 6 HOUR
C                       TOTAL OR CONVECTIVE PRECIPITATION 
C                       AMOUNT (INTERNAL)
C             MNX,MNY = DIMENSIONS OF GRID RETURNED FOR 6 HOUR
C                       TOTAL OR CONVECTIVE
C                       PRECIPITATION AMOUNT (INTERNAL)
C             MSX,MSY = DIMENSIONS OF GRID RETURNED FOR 3 HOUR
C                       TOTAL OR CONVECTIVE PRECIPITATION 
C                       AMOUNT (INTERNAL)
C             MTX,MTY = DIMENSIONS OF GRID RETURNED FOR 12 HOUR
C                       TOTAL OR CONVECTIVE PRECIPITATION 
C                       AMOUNT (INTERNAL)
C             MUX,MUY = DIMENSIONS OF GRID RETURNED FOR 6 HOUR
C                       TOTAL OR CONVECTIVE PRECIPITATION 
C                       AMOUNT (INTERNAL)
C             MTP3(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD1() (J=1,4). (INTERNAL)
C             MTP6(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD2() (J=1,4). (INTERNAL)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE. (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF
C                       THE GRID IS NOT KNOWN BEFORE FDTK AND FDDP
C                       ARE FETCHED.  (INPUT)
C                 ND5 = FORMER DIMENSION OF IPACK(), AND 
C                       IWORK(). (INPUT)
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
C              NSLABL = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 6-HR PRECIP. (INTERNAL)
C              NSLABP = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 3-HR PRECIP. (INTERNAL)
C              NSLABS = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 3-HR PRECIP. (INTERNAL)
C              NSLABT = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 3-HR PRECIP. (INTERNAL)
C              NSLABU = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 12-HR PRECIP. (INTERNAL)
C              NSLABV = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR 6-HR PRECIP. (INTERNAL)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE,
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS 
C                       STORED IN LSTORE(9,). (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA().  THIS 
C                       IS RETURNED FROM GFETCH (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C     NON SYSTEM SUBROUTINES USED
C        GFETCH
C
      IMPLICIT NONE
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IPACK(ND5),IWORK(ND5) 
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER MTP3(4),MTP6(4),MTP12(4),ICCCFFF(6)
      INTEGER IER,ISTAV,J,KFILDO,KFIL10,L3264B,LITEMS,
     1        MDX,MDY,MISSP,MISSS,MISTOT,
     2        NBLOCK,ND2X3,ND5,ND7,ND9,ND10,
     3        ND11,NDATE,NFETCH,NPACK,
     4        NSLAB,NSLABL,NSLABT,
     5        NTIMES,NWORDS,NSLABU,NSLABV,MTX,MTY,MUX,MUY,
     6        MNX,MNY,NSLABP,NSLABS,MLX,MLY,MSX,MSY
C
      REAL CORE(ND10)
      REAL FDTP(ND5)
      REAL FD1(ND2X3),FD2(ND2X3)
C
      DATA ICCCFFF/003205,
     1             003210,
     2             003235,
     3             003240,
     4             003220,
     5             003250/
C       ABOVE IDS ARE IN ORDER:
C        3-HR TOTAL
C        6-HR TOTAL
C        3-HR CONVECTIVE
C        6-HR CONVECTIVE
C       12-HR TOTAL  
C       12-HR CONVECTIVE
C 
      IER=0 
      ISTAV=0 
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C
      IF(IDPARS(1).NE.003.OR.(IDPARS(2).NE.210.AND.IDPARS(2).NE.
     1             240))THEN
         WRITE(KFILDO,101)(JD(J),J=1,4)
 101     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1            '6 HOUR TOTAL OR CONVECTIVE PRECIP AMT.',
     2          /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3           ' NOT COMPUTED IN TPCP6.')
         IER=103
         GOTO 800
      END IF
C
C        CHECK IF PROJECTION IS LESS THAN 6 HOURS.
C
      IF(IDPARS(12).LT.6)THEN
         WRITE(KFILDO,105)IDPARS(12),(JD(J),J=1,4)
 105     FORMAT(/' ****PROJECTION ENTERED IN THE CONTROL FILE =',I5,
     1           ' IS LESS THAN 6.',
     2          /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3           ' NOT COMPUTED IN TPCP6.')
         IER=187
         GOTO 800
      END IF
C
C        CREATE ID FOR FIRST 3 HOUR PRECIP AMT AT PROJECTION IDPARS(12).
C
      IF(IDPARS(2).EQ.240)THEN
         MTP3(1)=ICCCFFF(3)*1000+IDPARS(4)
      ELSE
         MTP3(1)=ICCCFFF(1)*1000+IDPARS(4)
      END IF
C
      MTP3(2)=IDPARS(7)
      MTP3(3)=IDPARS(9)*1000000+IDPARS(12)
      MTP3(4)=0
C
C        READ FIRST 3-HR PRECIP AMT; IF A 3 IS READ, THEN THE
C        ALGORITHM TRIES TO READ MORE 3-HR PROJECTIONS OR
C        MORE 6-HR PROJECTIONS.
C
      CALL GFETCH(KFILDO,KFIL10,MTP3,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDTP,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3            NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
      IF(IER.NE.0)THEN
C           AT THIS POINT, THE 3-HR AMOUNT ENDING AT IDPARS(12)
C           COULD NOT BE FOUND.  TRY THE 6-HR AMOUNT DIRECTLY.
         GOTO 200
      END IF
C
      MDX=IS2(3)
      MDY=IS2(4)
      NSLABT=NSLAB
C
C        CREATE ID FOR SECOND 3-HR PRECIP AMT.  ID'S ARE THE SAME
C        EXCEPT PROJECTION IS 3 HOURS LESS.
C
      MTP3(3)=MTP3(3)-3
C
      CALL GFETCH(KFILDO,KFIL10,MTP3,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3            NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
      IF(IER.EQ.0)THEN
         MNX=IS2(3)
         MNY=IS2(4)
         NSLABP=NSLAB
C
C           COMPARE IF THE GRID CHARACTERISTICS ARE THE SAME.
C
         IF(NSLABP.NE.NSLABT)THEN
            WRITE(KFILDO,135)NSLABP,NSLABT,(JD(J),J=1,4)
 135        FORMAT(/' ****THE CHARACTERISTICS OF THE TWO 3-HR',
     1              ' PRECIPITATION GRIDS ARE DIFFERENT.',I3,2X,I3,
     3             /'     VARIABLE ',I9.9,2I10.9,I4.3,
     4              ' NOT COMPUTED IN TPCP6.')
            IER=100
            GOTO 800
         END IF
C
         IF(MNX.NE.MDX.OR.MNY.NE.MDY)THEN
C              THE GRID CHARACTERISTICS ARE NOT THE SAME.
            WRITE(KFILDO,140)(MTP3(J),J=1,4),(NGRIDC(J,NSLABT),J=1,6),
     1                        MDX,MDY,
     2                       (MTP3(J),J=1,4),(NGRIDC(J,NSLABP),J=1,6),
     3                        MNX,MNY
 140        FORMAT(/' ****THE GRIDS NEEDED IN TPCP6 HAVE DIFFERENT',
     1              ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2              ' VALUES FROM NGRIDC( , ) AND MX,MY.'/
     3              (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
            IER=100
            GOTO 800
         END IF
C
C           ADD THE SECOND 3-HR AMOUNT TO THE 1ST 3-HR AMOUNT.
C
         DO J=1,MDX*MDY
             FDTP(J)=FDTP(J)+FD1(J)
             IF(FDTP(J).LT.0.)FDTP(J)=0.
	 END DO
C
C           6-HR AMOUNTS HAVE BEEN FOUND BY ADDING TWO 3-HR AMOUNTS
C           AT IDPARS(12) AND IDPARS(12)-3.
C
         GOTO 900
      ELSE
C**********************************************************************
C
C           CONTROL COMES HERE WHEN THE 1ST 3-HR GRID HAS BEEN FOUND
C           BUT NOT A SECOND 3-HR GRID.  AN ATTEMPT IS MADE TO GET
C           AND SUM A 6-HR GRID 3 HOURS EARLIER WHICH NOW GIVES 
C           A 9-HOUR TOTAL, AND THEN GET AND SUBTRACT A 3-HR GRID 
C           TO GET THE NEEDED 6-HR GRID.
C 
C           CREATE 6-HR PRECIP ID.        
C
         IF(IDPARS(2).EQ.240)THEN
            MTP6(1)=ICCCFFF(4)*1000+IDPARS(4)
         ELSE
            MTP6(1)=ICCCFFF(2)*1000+IDPARS(4)
         END IF
C
         MTP6(2)=IDPARS(7)
         MTP6(3)=IDPARS(9)*1000000+IDPARS(12)-3
         MTP6(4)=0
         CALL GFETCH(KFILDO,KFIL10,MTP6,7777,LSTORE,ND9,LITEMS,
     1               IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2               NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3               NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
         IF(MISSP.NE.0)MISTOT=MISTOT+1
C
         IF(IER.NE.0)THEN
            WRITE(KFILDO,130)(JD(J),J=1,4)
 130        FORMAT(/' ****VARIABLE ',I9.9,2I10.9,I4.3,
     1              ' NOT COMPUTED IN TPCP6.  6-HR PRECIP',
     2              ' NOT FOUND BY GFETCH AT 130.')
            GOTO 800
         ENDIF
C
         NSLABL=NSLAB
         MLX=IS2(3)
         MLY=IS2(4)
C
C           CHECK THE GRID CHARACTERISTICS
C
         IF(MLX.NE.MDX.OR.MLY.NE.MDY)THEN
C              THE GRID CHARACTERISTICS ARE NOT THE SAME
            WRITE(KFILDO,150)(MTP6(J),J=1,4),(NGRIDC(J,NSLABL),J=1,6),
     1                        MLX,MLY,
     2                       (MTP3(J),J=1,4),(NGRIDC(J,NSLABT),J=1,6),
     3                        MDX,MDY
 150        FORMAT(/' ****THE GRIDS NEEDED IN TPCP6 HAVE DIFFERENT',
     1              ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2              ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3               (5X,I9.9,I10.9,I10.9,I4.3,6I10,4X,2I5))
            IER=100
            GOTO 800
         ENDIF
C
C           CHECK IF NSLABS EQUAL EACH OTHER
C
         IF(NSLABL.NE.NSLABT)THEN
            WRITE(KFILDO,155)NSLABL,NSLABT,(JD(J),J=1,4)
 155        FORMAT(/' ****THE GRID CHARACTERISTICS OF THE ',
     1               '3 AND 6-HR PRECIPITATION ARE DIFFERENT.',I3,2X,I3,
     2             /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3              ' NOT COMPUTED IN TPCP6.')
            IER=100
            GOTO 800
         END IF
C
C           ADD THE 3-HR GRID AT IDPARS(12) TO THE 6-HR GRID AT
C           IDPARS(12)-3.
C
         DO J=1,MDX*MDY
            FDTP(J)=FDTP(J)+FD2(J)
         END DO
C    
C           CREATE ID FOR FINAL 3 HOUR PRECIP GRID.
C
         IF(IDPARS(2).EQ.240)THEN
            MTP3(1)=ICCCFFF(3)*1000+IDPARS(4)
         ELSE
            MTP3(1)=ICCCFFF(1)*1000+IDPARS(4)
         END IF
C
         MTP3(2)=IDPARS(7)
         MTP3(3)=MTP6(3)-3
         MTP3(4)=0
C
         CALL GFETCH(KFILDO,KFIL10,MTP3,7777,LSTORE,ND9,LITEMS,
     1               IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2               NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3               NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
         IF(MISSP.NE.0)MISTOT=MISTOT+1
C
         IF(IER.NE.0)THEN
            WRITE(KFILDO,160)(JD(J),J=1,4)
 160        FORMAT(/' ****VARIABLE ',I9.9,2I10.9,I4.3,
     1              ' NOT COMPUTED IN TPCP6.  3-HR PRECIP',
     2              ' NOT FOUND BY GFETCH AT 160.')
            GOTO 800
         ENDIF
C
         NSLABS=NSLAB
         MSX=IS2(3)
         MSY=IS2(4)
C
C           CHECK THE GRID CHARACTERISTICS ARE THE SAME
C     
         IF(MLX.NE.MSX.OR.MSY.NE.MLY)THEN
C             THE GRID CHARACTERISTICS ARE NOT THE SAME
           WRITE(KFILDO,180)(MTP6(J),J=1,4),(NGRIDC(J,NSLABL),J=1,6),
     1                         MLX,MLY,
     2                        (MTP3(J),J=1,4),(NGRIDC(J,NSLABS),J=1,6),
     3                         MSX,MSY
 180       FORMAT(/' ****THE GRIDS NEEDED IN TPCP6 HAVE DIFFERENT',
     1             ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2             ' VALUES FROM NGRIDC( , ) AND MX,MY.',/,
     3             (5X,I9.9,I10.9,I10.9,I4.3,6I10,4X,2I5))
           IER=100
           GOTO 800
         END IF
C
C           CHECK IF NSLABS EQUAL EACH OTHER
C
         IF(NSLABL.NE.NSLABS)THEN
            WRITE(KFILDO,185)NSLABL,NSLABS,(JD(J),J=1,4)
 185        FORMAT(/' ****THE GRID CHARACTERISTICS OF THE ',
     1              '3 AND 6-HR PRECIPITATION ARE DIFFERENT.',I3,2X,I3,
     2             /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3              ' NOT COMPUTED IN TPCP6.')
            IER=100
            GOTO 800
         END IF
C
C           SUBTRACT THE 3-HR AMOUNT AT IDPARS(12)-6 FROM THE
C           ACCUMULATED 18-HR AMOUNT.
C
         DO J=1,MDX*MDY
            FDTP(J)=FDTP(J)-FD1(J)
            IF(FDTP(J).LT.0.)FDTP(J)=0.
         END DO
C
      END IF
C
C        6-HR AMOUNTS HAVE HAD BEEN FOUND BY SUBTRACTING THE THE 3-HR 
C        AMOUNTS AT IDPARS(12)-6 FROM THE SUM OF THE 3-HR AMOUNTS
C        AT IDPARS(12) PLUS THE 6-HR AMOUNTS AT IDPARS-3.
C
      GOTO 900
C**********************************************************************
C
C        CONTROL COMES HERE IF THE FIRST 3-HR AMOUNT COULD NOT BE FOUND.
C        TRY TO GET A 6-HR AMOUNT DIRECTLY.
C
C        CREATE 6-HR PRECIP ID AT PROJECTION IDPARS(12).
C
 200  IF(IDPARS(2).EQ.240)THEN
        MTP6(1)=ICCCFFF(4)*1000+IDPARS(4)
      ELSE
        MTP6(1)=ICCCFFF(2)*1000+IDPARS(4)
      END IF
C
      MTP6(2)=IDPARS(7)
      MTP6(3)=IDPARS(9)*1000000+IDPARS(12)
      MTP6(4)=0
C     
C        READ 6-HR AMOUNT.
C
      CALL GFETCH(KFILDO,KFIL10,MTP6,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDTP,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3            NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.EQ.0)GOTO 900
C
C        6-HR AMOUNT HAS BEEN FOUND DIRECTLY AT IDPARS(12).
C
C**********************************************************************
C
C        FINDING THE 6-HR AMOUNT GRID AND TWO 3-HR GRIDS HAS
C        FAILED.  NOW TRY READING 12-HR PRECIP FOR IDPRS(12)
C        AND THE 6-HR PRECIP FOR IDPARS(12)-6.  THE DIFFERENCE
C        WILL BE THE 6-HR PRECIPITATION AMOUNT.  (CHECKED FOR ETA
C        BUT NOT OTHER MODELS.)
C
C        CREATE 12-HR PRECIP ID AT PROJECTION IDPARS(12).
C
      IF(IDPARS(2).EQ.240) THEN
         MTP12(1)=ICCCFFF(6)*1000+IDPARS(4)
      ELSE
         MTP12(1)=ICCCFFF(5)*1000+IDPARS(4)
      END IF
C
      MTP12(2)=IDPARS(7)
      MTP12(3)=IDPARS(9)*1000000+IDPARS(12)
      MTP12(4)=0
C
C        READ 12-HR AMOUNT.
C
      CALL GFETCH(KFILDO,KFIL10,MTP12,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDTP,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3            NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,305)(JD(J),J=1,4)
 305     FORMAT(/' ****VARIABLE ',I9.9,2I10.9,I4.3,
     1           ' NOT COMPUTED IN TPCP6.  12-HR PRECIP',
     2           ' NOT FOUND BY GFETCH AT 305.')
         GOTO 800
      ENDIF
C
      NSLABU=NSLAB
      MTX=IS2(3)
      MTY=IS2(4)
C
C        CREATE 6-HR PRECIP ID AT PROJECTION IDPARS(12)-6.
C
      IF(IDPARS(2).EQ.240)THEN
         MTP6(1)=ICCCFFF(4)*1000+IDPARS(4)
      ELSE
         MTP6(1)=ICCCFFF(2)*1000+IDPARS(4)
      END IF
C
      MTP6(2)=IDPARS(7)
      MTP6(3)=IDPARS(9)*1000000+IDPARS(12)-6
      MTP6(4)=0
C
      CALL GFETCH(KFILDO,KFIL10,MTP6,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3            NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      MUX=IS2(3)
      MUY=IS2(4)
      NSLABV=NSLAB
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,310)(JD(J),J=1,4)
 310     FORMAT(/' ****VARIABLE ',I9.9,2I10.9,I4.3,
     1           ' NOT COMPUTED IN TPCP6.  12-HR PRECIP',
     2           ' NOT FOUND BY GFETCH AT 310.')
         GOTO 800
      ENDIF
C
C        CHECK THE GRID CHARACTERISTICS.
C
      IF(NSLABU.NE.NSLABV)THEN
         WRITE(KFILDO,330)NSLABU,NSLABV
 330     FORMAT(/,' ****THE GRID CHARACTERISTICS OF THE ',
     1            '12-HOUR AND 6-HOUR PRECIPITATION ARE DIFFERENT.',
     2             I3,2X,I3)
         IER=100
         GOTO 800
      END IF
C
      IF(MTX.NE.MUX.OR.MTY.NE.MUY)THEN
C         THE GRID CHARACTERISTICS ARE NOT THE SAME
          WRITE(KFILDO,340)(MTP12(J),J=1,4),(NGRIDC(J,NSLABU),J=1,6),
     1                      MDX,MDY,
     2                     (MTP6(J),J=1,4),(NGRIDC(J,NSLABV),J=1,6),
     3                      MNX,MNY
 340     FORMAT(/' ****THE GRIDS NEEDED IN TPCP6 HAVE DIFFERENT',
     1           ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2           ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3           (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
         IER=100
         GOTO 800
      END IF
C
C        SUBTRACT THE 6-HR AMOUNT AT IDPARS(12)-6 FROM THE 12-HR
C        AMOUNT AT IDPARS(12)
C
      DO 350 J=1,MTX*MTY
         FDTP(J)=FDTP(J)-FD2(J)
         IF(FDTP(J).LT.0.)FDTP(J)=0.
 350  CONTINUE
C
C       THE 6-HR AMOUNT AT IDPARS(12) HAS BEEN FOUOND BY FINDING 
C       THE 12-HR AMOUNT AT IDPARS(12) AND SUBTRACTING THE 6-HR
C       AMOUNT AT IDPARS(12)-6.
C
      GOTO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C 
 800  DO 801 J=1,ND2X3
         FDTP(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END
