      SUBROUTINE FRZLVL(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                  NGRIDC,ND11,NSLAB,IPACK,IWORK,FDFZ,ND5,
     2                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                  IS0,IS1,IS2,IS4,ND7,
     4                  FD1,FD2,FD3,FD4,ND2X3,
     5                  ISTAV,L3264B,MISTOT,IER)
C
C        OCTOBER  1998   SFANOS  TDL-MOS 2000
C        NOVEMBER 1998   SFANOS  RENAMED WORK ARRAYS,
C                                PUT IN MISSP IF TEST,
C                                AND CHANGED SOME COMMENTS
C        OCTOBER  1999   SFANOS  RESTRUCTURED PROGRAM TO
C                                FIT TEMPLATE BY JIM SU
C        JUNE     2000   ALLEN   ADDED COMMAS FOR IBM COMPILATION
C        JULY     2000   ALLEN   CHANGED NGM FFF ID FROM 040 TO 045
C        OCTOBER  2002   WEISS   CHANGED ND5 TO ND2X3
C        APRIL    2003   GLAHN   MODIFIED LINES IN CALL;  SET
C                                DIMENSIONS OF IPACK( ), IWORK( )
C                                AND FDFZ( ) = ND5; SPELL CHECK;
C                                INITIALIZED FDSZ( ); CHANGED
C                                CALCULATION DO 300 LOOP FROM 
C                                ND2X3 TO MSX*MSY; ELIMINATED
C                                DIMENSION OF ICCCFFF
C        MAY      2003   GLAHN   REARRANGED TYPE STATEMENTS
C        JUNE     2003   GLAHN   REMOVED ALTERING IDPARS(7);
C                                CHANGED LOOP DO 301 I=1,ILIMJ TO
C                                I=1,ILIMJ-1 AND REMOVED TEST ON
C                                300 MB TO GO TO 200; INSERTED TEST
C                                TO PRODUCE DIAGNOSTIC AND EXIT 
C                                WHEN FFF OF ID DOES NOT MATCH
C                                DD ACCORDING TO THE DEFINED IDS
C        OCTOBER  2003   SMB     MODIFIED FORMAT STATEMENT 155
C                                FOR THE IBM
C        FEBRUARY 2004   SMB     NEW IDS WERE ASSIGNED DUE TO THE
C                                4/03 INITIALIZATION CHANGE.  OLD
C                                IDS WERE 002042 - 002045.  REMOVED
C                                CHECK OF FFF AGAINST DD.
C
C        PURPOSE
C            TO COMPUTE THE FREEZING LEVEL PRESSURE (MB) AT GRID
C            POINTS USING TWO PRESSURE LEVELS AND TWO TEMPERATURES.
C            THE FOLLOWING EQUATION IS USED:
C            FDFZ=ZLVL(I,IDTMPL)*(EXP(FD4(J)*LOG((FD1(J)/ABSZRO))))
C                WHERE ZLVL(,)= PRESSURE IN MB AT STARTING LEVEL
C                FD1(J) IS THE TEMPERATURE AT THE LOWER LEVEL
C                FD4(J)= AGRAV/(RD*FD3(J))
C                WHERE AGRAV = 9.0862 M/S AND IS THE ACCELERATION
C                OF GRAVITY, RD= 287.04 J/(KG*K) AND IS THE DRY AIR
C                GAS CONSTANT, AND FD3 IS THE LAPSE RATE IN K/M
C                OF THE TWO LEVELS. (J=1,NX*NY)
C            IF THERE IS MORE THAN ONE FREEZING LEVEL, THE HIGHEST
C            ONE IS RETURNED.
C                
C           THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C                002 046 - THE FREEZING LEVEL HEIGHT RETURNED
C                          FOR THE MRF(OLD AVN).  LEVELS ARE
C                          1000,925,850,700,500,300 MB. (OLD FFF=042) 
C                002 047 - THE FREEZING LEVEL HEIGHT RETURNED
C                          FOR THE ETA.  LEVELS ARE 1000,950,900,
C                          850,800,750,700,600,500,400,300 MB. (OLD FFF=043) 
C                002 048 - THE FREEZING LEVEL HEIGHT RETURNED
C                          FOR THE AVN. LEVELS ARE 1000,975,950,925,
C                          900,850,800,750,700,600,500,300 MB. (OLD FFF=044) 
C                002 049 - THE FREEZING LEVEL HEIGHT RETURNED
C                          FOR THE NGM.  LEVELS ARE 100,950,900,850,
C                          800,750,700,500,300 MB. (OLD FFF=045)
C
C        DATA SET USE
C            KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT(PRINT) FILE.
C                     (OUTPUT)
C            KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                     ACCESS.(INPUT-OUTPUT)
C
C        VARIABLES
C              ABSZRO = THE TEMPERATURE OF ABSOLUTE ZERO,
C                       OR -273.15.
C               AGRAV = ACCELERATION OF GRAVITY IN M/S^2, WHICH
C                       IS EQUAL TO 9.0862.
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE(,) (J=1,ND10).
C                       WHEN CORE() IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C             FDFZ(J) = ARRAY TO HOLD RETURNED FREEZING LEVEL
C                       HEIGHT (J=1,ND5). (OUTPUT)
C              FD1(J) = WORK ARRAY TO HOLD THE LOWER LEVEL TEMPERATURE
C                       (J=1,ND2X3). (INTERNAL)
C              FD2(J) = WORK ARRAY TO HOLD THE UPPER LEVEL TEMPERATURE
C                       (J=1,ND2X3). (INTERNAL)
C              FD3(J) = THE LAPSE RATE BETWEEN THE TWO LEVELS. (J=1,
C                       ND2X3). (INTERNAL)
C              FD4(J) = AGRAV/(RD*FD5()).  FD4() IS AN 
C                       INTERMEDIATE TERM IN THE CALCULATION OF THE
C                       FREEZING LEVEL. (J=1,ND2X3) (INTERNAL)
C                   I = LOOP CONTROL VARIABLE
C             ICCCFFF = CONTAINS IDPARS(1) AND IDPARS(2) ID FOR THE
C                       METEOROLOGICAL PARAMETER BEING USED ON AN
C                       ISOBARIC SURFACE.  (INTERNAL)
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
C              IDTMPL = VARIABLE THAT ASSIGNS A LAYER TEMPLATE
C                       BASED ON THE VARIABLE AND MODEL. (INTERNAL)
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                       100 = THE TWO GRIDS NEEDED ARE NOT THE SAME SIZE
C                       101 = GRID SIZE IS TOO BIG FOR ???(), WHOSE 
C                             DIMENSION IS ND5.
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE
C                             FREEZING LEVEL.
C                       187 = 
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C             IFFF(J) = THE FFF'S FOR FOUR DIFFERENT MODELS IN ORDER 
C                       NGM, MRF, ETA, AND AVN (J=1,4).  THIS MUST MATCH
C                       IDPARS(2). (INTERNAL)
C              IDD(J) = THE DD'S FOR FOUR DIFFERENT MODELS IN ORDER
C                       NGM, MRF, ETA, AND AVN (J=1,4).  THIS MUST MATCH
C                       IDPARS(4). (INTERNAL)
C             ILIM(J) = NUMBER OF ISOBARIC LEVELS FOR FOUR DIFFERENT
C                       MODELS IN ORDER NGM, MRF, ETA, AND AVN (J=1,4).
C                       (INTERNAL)
C               ILIMJ = TEMPORARY VARIABLE THAT HAS THE NUMBER OF LEVELS 
C                       THAT ARE TO BE PROCESSED. (INTERNAL)
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
C             MDT1(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD1() AND FD2() (J=1,4). (INTERNAL)
C             MDX,MDY = DIMENSIONS OF GRID RETURNED FOR HEIGHT OF
C                       LOWER LEVEL. (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 FROM CALLING GFETCH WHEN THERE IS NO
C                       PRIMARY MISSING VALUE. (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 FROM CALLING GFETCH WHEN THERE IS NO
C                       SECONDARY MISSING VALUE.  (INTERNAL)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C             MNX,MNY = DIMENSIONS OF GRID RETURNED FOR TEMPERATURE
C                       OF LOWER LEVEL (INTERNAL)
C             MSX,MSY = DIMENSIONS OF GRID RETURNED FOR TEMPERATURE
C                       OF UPPER LEVEL (INTERNAL)
C             MTX,MTY = DIMENSIONS OF GRID RETURNED FOR HEIGHT OF
C                       UPPER LEVEL (INTERNAL)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE. (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF
C                       THE GRID IS NOT KNOWN BEFORE ARRAYS 
C                       ARE FETCHED.  (INPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND FDFZ( ). 
C                       (INPUT)
C                 ND7 = DIMENSION OF IS0( ),IS1( ),IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED. (INPUT)
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ). (INPUT)
C                ND10 = DIMENSION OF CORE(). (INPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN
C                       BE DEALT WITH ON THIS RUN.  LAST DIMENSION 
C                       OF NGRIDC( , ). (INPUT)
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
C              NSLABD = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR FD1(K).  (INTERNAL)
C              NSLABP = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR FD2(K).  (INTERNAL)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS 
C                       STORED IN LSTORE(9,). (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED FROM GFETCH.
C                       THIS WILL BE IS2(3)*IS2(4) AND WILL BE
C                       THE NUMBER OF VALUES RETURNED IN FRZL( )
C                       THROUGH IS2( ).  (INTERNAL)
C                  RD = DRY AIR GAS CONSTANT WHICH EQUALS 287.04 
C                       J/(KG*K).
C             ZLVL(,) = THE PARTICULAR LEVEL THAT IS BEING
C                       PROCESSED. THE PARTICULAR LEVEL DEPENDS
C                       UPON THE MODEL TYPE. (INTERNAL)
C        1         2         3         4         5         6         7 X
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
      INTEGER IFFF(4),ILIM(4),MDT1(4),IDD(4)
      INTEGER I,IDTMPL,IER,ILIMJ,ISTAV,J,KFILDO,KFIL10,L3264B,
     1        LITEMS,MISSP,MISSS,MISTOT,IDP7,
     2        MNX,MNY,MSX,MSY,NBLOCK,ND2X3,
     3        ND5,ND7,ND9,ND10,ND11,NDATE,NFETCH,
     4        NPACK,NSLAB,NSLABD,NSLABP,NTIMES,NWORDS,ICCCFFF

      REAL FDFZ(ND5)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3)  
      REAL CORE(ND10)
      REAL ZLVL(12,4)
      REAL ABSZRO,AGRAV,RD
C
      DATA ILIM/9,6,11,12/
      DATA (ZLVL(I,1),I=1,9)
     1           /1000.0,950.0,900.0,850.0,800.0,750.0,
     2            700.0,500.0,300.0/
     3     (ZLVL(I,2),I=1,6)  
     4           /1000.0,925.0,850.0,700.0,500.0,300.0/
     5     (ZLVL(I,3),I=1,11)
     6           /1000.0,950.0,900.0,850.0,800.0,750.0,
     7            700.0,600.0,500.0,400.0,300.0/
     8     (ZLVL(I,4),I=1,12)
     9           /1000.0,975.0,950.0,925.0,900.0,850.0,
     A            800.0,750.0,700.0,600.0,500.0,300.0/
      DATA IFFF/049,046,047,048/
      DATA IDD/6,9,7,8/
      DATA ABSZRO/-273.15/,RD/287.04/,AGRAV/9.8062/
C
      ICCCFFF=002000
C
      IER=0
      ISTAV=0
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C
      IF(IDPARS(1).NE.002.OR.(IDPARS(2).NE.049.AND.
     1   IDPARS(2).NE.046.AND.IDPARS(2).NE.047.AND.
     2   IDPARS(2).NE.048))THEN
         WRITE(KFILDO,101)(JD(J),J=1,4)
 101     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1           'THE FREEZING LEVEL. ',
     2           I9.9,2I10.9,I4.3, 'NOT COMPUTED IN FRZLVL. ')
         IER=103
         GOTO 800
      END IF
C
C        CHECK WHICH ID IT IS SO WE CAN USE THE PROPER NUMBER
C        OF ITERATIONS. THE LEVELS ARE SET IN THE CORRESPONDING
C        DO LOOPS. 2/2004 - COMMENTED OUT CHECK AGAINST IDPARS(4)
C
      DO 102 J=1,4
C
C         IF(IDPARS(2).EQ.IFFF(J).AND.IDPARS(4).EQ.IDD(J))THEN
         IF(IDPARS(2).EQ.IFFF(J))THEN
            IDTMPL=J
            GO TO 109
         ENDIF
C
 102  CONTINUE
C
        IER=103
        WRITE(KFILDO,105)(JD(J),J=1,4),IER
 105    FORMAT(/' ****IDPARS(2) AND IDPARS(4) DO NOT',
     1          ' MATCH FOR THE FREEZING LEVEL IN FRZLVL.',
     2         /'     VARIABLE ',I9.9,2I10.9,I4.3,' NOT ',
     3          'COMPUTED.  IER=',I3)
        GOTO 800
C
C        INITIALIZE FDFZ( ).
C
 109  DO 110 J=1,ND2X3
        FDFZ(J)=9999.
 110  CONTINUE
C
C        GET THE 1ST PRESSURE LEVEL
C
      ILIMJ=ILIM(IDTMPL)
c
      DO 301 I=1,ILIMJ-1
C
        IDP7=ZLVL(I,IDTMPL)
C  
C          CREATE ID FOR TEMPERATURE OF FIRST LEVEL
C
        MDT1(1)=ICCCFFF*1000+IDPARS(4)
        MDT1(2)=IDP7
        MDT1(3)=IDPARS(9)*1000000+IDPARS(12)
        MDT1(4)=0
C
C          FD1 IS A WORK ARRAY DIMENSIONED ND2X3.
C
        CALL GFETCH(KFILDO,KFIL10,MDT1,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3            NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
        IF(MISSP.NE.0)MISTOT=MISTOT+1
        IF(IER.NE.0)GOTO 800
        MNX=IS2(3)
        MNY=IS2(4)
        NSLABD=NSLAB
C
C          INCREASE ZBOT TO ALLOW FOR THE NEXT LEVEL 
C
        IDP7=ZLVL(I+1,IDTMPL)
        MDT1(2)=IDP7
C                
C          FD2 IS A WORK ARRAY DIMENSIONED AS ND2X3 BUT PASSED
C          FROM GFETCH AS A DATA ARRAY WITH DIMENSION ND2X3. CALL
C          GFETCH TO GET THE TEMPERATURE AT THE NEXT TEMPERATURE
C          LEVEL.
C
        CALL GFETCH(KFILDO,KFIL10,MDT1,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3              NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
        IF(MISSP.NE.0)MISTOT=MISTOT+1
        IF(IER.NE.0)GOTO 800
        NSLABP=NSLAB
        MSX=IS2(3)
        MSY=IS2(4)
C
C          COMPARE IF THE GRID CHARACTERISTICS ARE THE SAME
C
        IF(NSLABP.NE.NSLABD)THEN
          WRITE(KFILDO,150)NSLABP,NSLABD
 150      FORMAT(/' ****THE GRID CHARACTERISTICS OF THE ',
     1            ' TEMPERATURES AT THE TWO LEVELS ',
     2            ' ARE DIFFERENT IN FRZLVL AT 150.',I3,2X,I3)
          IER=100
          GOTO 800
        END IF
C
        IF(MSX.NE.MNX.OR.MSY.NE.MNY)THEN
C            THE GRID CHARACTERISTICS ARE NOT THE SAME.
          WRITE(KFILDO,155)(MDT1(J),J=1,4),(NGRIDC(J,NSLABD),J=1,6),
     1                      MNX,MNY,
     2                    (MDT1(J),J=1,4),(NGRIDC(J,NSLABP),J=1,6),
     3                      MSX,MSY
 155      FORMAT(/' ****THE GRIDS NEEDED IN FRZLVL HAVE DIFFERENT',
     1            ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2            ' VALUES FROM NGRIDC(,) AND MX,MY.',
     3            (/5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
          IER=100
          GOTO 800
        END IF
C
C          COMPUTE THE FREEZING LEVEL
C
        DO 300 J=1,MSX*MSY
C
          IF(FD1(J).GT.-(ABSZRO).AND.FD2(J).LT.-(ABSZRO))THEN
C
C              COMPUTATION OF THE LAPSE RATE
C
            FD3(J)=(FD2(J)-FD1(J))/(ZLVL(I,IDTMPL)-
     1             ZLVL(I+1,IDTMPL))    
C
C              CALCULATE THE TERM IN THE EQUATION THAT WILL BE
C              RAISED TO THE EXP FUNCTION.
C
            FD4(J)=(AGRAV/(RD*FD3(J)))
C
C              CALCULATION OF THE FREEZING LEVEL 
C
            FDFZ(J)=(ZLVL(I,IDTMPL))*
     1      EXP(FD4(J)*LOG((FD1(J)/(-1*(ABSZRO)))))
          END IF
C
C            CHECK FOR ANY FREEZING LEVELS THAT WERE NOT CALCULATED
C            BEFORE.  IF TEMPERATURE NEVER GETS ABOVE 273.15, THEN
C            SET FDFZ = 1000.  IF DATA NEVER IS BELOW 273.15, SET 
C
C         FDFZ = 300.
C
          IF(FDFZ(J).EQ.9999.AND.FD1(J).LT.-(ABSZRO))THEN
            FDFZ(J)=1000.
            GOTO 300
          ELSE IF(FDFZ(J).EQ.9999.AND.FD2(J).GT.-(ABSZRO))THEN
            FDFZ(J)=300.
            GOTO 300
          END IF
C
 300    CONTINUE
C
 301  CONTINUE
C
      GOTO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C 
 800  DO 801 J=1,ND2X3
        FDFZ(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END
