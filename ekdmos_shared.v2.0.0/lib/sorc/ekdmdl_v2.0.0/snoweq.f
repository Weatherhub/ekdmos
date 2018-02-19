      SUBROUTINE SNOWEQ(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                  NGRIDC,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     2                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                  IS0,IS1,IS2,IS4,ND7,FD1,FD2,FD3,FD4,FDT1,
     4                  FDT2,ND2X3,ISTAV,L3264B,MISTOT,IER)
C
C        FEBRUARY 2003   SFANOS   TDL   MOS-2000
C        JUNE     2003   GLAHN    REARRANGED TYPE STATEMENTS, PUT
C                                 ICCCFFF IN A DATA STATEMENT;
C                                 CHANGED IER =115 TO 187; ASSURED
C                                 MISTOT UPDATED AFTER EACH GFETCH AS
C                                 NECESSARY; REMOVED TEST CODE AND
C                                 INTEGER K TO STOP COMPILER FLAG OF
C                                 K UNUSUED
C
C        PURPOSE
C            TO COMPUTE THE 24 HR SNOWFALL AMOUNT BY
C            USING AN AVERAGE TEMPERATURE OVER THE PERIOD ALONG 
C            WITH THE WATER EQ. OVER THE SAME TIME.
C                   
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C                   003 640 - 24 HR SNOWFALL AMT (SHORT RANGE)
C                             VARIABLE ON AN ISOBARIC SURFACE
C                   003 645 - 24 HR SNOWFALL AMT (MEDIUM RANGE)
C                             VARIABLE ON AN ISOBARIC SURFACE
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
C              FD1(J) = WORK ARRAY  (J=1,ND2X3).
C                       (INTERNAL)
C              FD2(J) = WORK ARRAY TO GET THE PRECIP (J=1,ND2X3).
C                       (INTERNAL)
C              FD3(J) = AVERAGE OF TEMPERATURE BETWEEN CONSECUTIVE
C                       TIME.  IN THE SHORT RANGE, 3 HR; IN THE
C                       MEDIUM RANGE, 12 HR (J=1,ND2X3).
C                       (INTERNAL)
C              FD4(J) = WORK ARRAY TO HOLD THE SNOW EQ. FOR ONE
C                       PROJECTION AT A TIME (J=1,ND2X3). (INTERNAL)
C             FDT1(J) = TEMPERATURE FETCHED AT FIRST PROJECTION 
C                       (J=1,ND2X3).  (INTERNAL)
C             FDT2(J) = TEMPERATURE FETCHED AT SUBSEQUENT PROJECTION
C                       (J=1,ND2X3).  (INTERNAL)
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
C                       101 = GRID SIZE IS TOO BIG FOR ???(), WHOSE 
C                             DIMENSION IS ND5.
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE
C                             SNOWEQ.
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
C                 ISO = 1 FOR ISOBARIC, 2 FOR CONSTANT PRESSURE 
C                       SURFACE,3 FOR SIGMA SURFACE(INTERNAL)
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
C                   K = LOOP CONTROL VARIABLE
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
C                       SUBROUTINE TPCP3. (INTERNAL)
C             MDX,MDY = DIMENSIONS OF GRID RETURNED FOR TEMPERATURE
C                       COMPARISON. (INTERNAL)
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
C                       COMPARISON. (INTERNAL)
C              MP3(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD2() WHEN CALLING GFETCH FOR THE 
C                       PRECIP AMOUNT (J=1,4). (INTERNAL)
C             MP32(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD2() WHEN CALLING TPCP3 FOR THE 
C                       PRECIP AMOUNT (J=1,4). (INTERNAL)
C             MTP3(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FDT1() AND FDT2() (J=1,4). (INTERNAL)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE. (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF
C                       THE GRID IS NOT KNOWN BEFORE FDTK AND FDDP
C                       ARE FETCHED.  (INPUT)
C                 ND5 = DIMENSION OF IPACK(), AND 
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
C                       EACH GRID COMBINATION (M=1,NGRID). (INPUT)
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
C              NSLABP = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR TEMPERATURE COMPARISON. (INTERNAL)
C              NSLABT = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR TEMPERATURE COMPARISON. (INTERNAL)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE,
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS 
C                       STORED IN LSTORE(9,). (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA().  THIS 
C                       IS RETURNED FROM GFETCH (INTERNAL)
C             DATA(J) = THE SNOW EQ. PREDICTOR.
C                       (J=1,ND2X3). (OUTPUT)
C        1         2         3         4         5         6         7 X
C
C     NON SYSTEM SUBROUTINES USED
C        GFETCH, PRSID1, TPCP3, TPCP12
C
      IMPLICIT NONE
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER MTP3(4),MP3(4),MP32(4),MDPARS(15)
      INTEGER ICCCFFF(3)
      INTEGER IER,ISTAV,I,J,KFILDO,KFIL10,
     1        L3264B,LITEMS,MDX,MDY,
     2        MISSP,MISSS,MISTOT,MNX,MNY,
     3        NBLOCK,ND2X3,ND5,ND7,ND9,ND10,
     4        ND11,NDATE,NFETCH,NPACK,
     5        NSLAB,NSLABP,NSLABT,NTIMES,NWORDS
C
      REAL DATA(ND5)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),
     1     FDT1(ND2X3),FDT2(ND2X3)
      REAL CORE(ND10)
C
      DATA ICCCFFF/002001,
     1             003205,
     2             003220/
C
      IER=0
      ISTAV=0
C
C        INITIALIZE DATA( ).  USE ND2X3 BECAUSE SIZE OF GRID
C        IS NOT YET KNOWN.
C
      DO I=1,ND2X3
        DATA(I)=0.
      END DO
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C
      IF(IDPARS(1).NE.003.OR.(IDPARS(2).NE.640.AND.IDPARS(2).NE.
     1             645))THEN
         WRITE(KFILDO,101)(JD(J),J=1,4)
 101     FORMAT(/,' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1           '24-HOUR CALCULATED SNOW AMT.'/
     2            '     VARIABLE ',I9.9,2I10.9,I4.3,
     3            ' NOT COMPUTED IN SNOWEQ.')
         IER=103
         GOTO 800
      END IF
C
C        CHECK IF PROJECTION IS LESS THAN OR EQUAL TO 24 HOURS.
C
      IF(IDPARS(12).LT.24)THEN
        WRITE(KFILDO,115)IDPARS(12),(JD(J),J=1,4)
 115    FORMAT(/,' ****PROJECTION =',I5,' FOR VARIABLE ',
     1          I9.9,2I10.9,I4.3,' IS LESS THAN 24 IN SNOWEQ.')
        IER=187
        GOTO 800
      END IF
C
C        CREATE ID FOR 2-M TEMPERATURE AT PROJECTION IDPARS(12).
C
      IF(IDPARS(2).EQ.640)THEN
C
        DO I=1,8 
C
          IF(I.EQ.1)THEN
            MTP3(1)=ICCCFFF(1)*1000+IDPARS(4)
            MTP3(2)=2
            MTP3(3)=IDPARS(9)*1000000+IDPARS(12)
            MTP3(4)=0
C
C              READ 3 HR 2-M TEMP.
C
            CALL GFETCH(KFILDO,KFIL10,MTP3,7777,LSTORE,ND9,LITEMS,
     1                IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDT1,ND2X3,
     2                NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3                NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
C
            IF(MISSP.NE.0)MISTOT=MISTOT+1
            IF(IER.NE.0)GOTO 800
C
            MDX=IS2(3)
            MDY=IS2(4)
            NSLABT=NSLAB
C
C              CREATE ID FOR THE 2-M TEMPERATURE AT IDPARS(12)-3.
C
            MTP3(3)=IDPARS(9)*1000000+IDPARS(12)-3
C
            CALL GFETCH(KFILDO,KFIL10,MTP3,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDT2,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3            NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
            IF(MISSP.NE.0)MISTOT=MISTOT+1
            IF(IER.NE.0)GOTO 800
C
            MNX=IS2(3)
            MNY=IS2(4)
            NSLABP=NSLAB
C
C              COMPARE IF THE GRID CHARACTERISTICS ARE THE SAME
C
            IF(NSLABP.NE.NSLABT)THEN
              WRITE(KFILDO,135)NSLABP,NSLABT
 135          FORMAT(/,' ****THE GRID CHARACTERISTICS OF THE ',
     1          '1ST 3HR TEMP COMPARISON ARE DIFFERENT.',I3,2X,I3)
              IER=100
              GOTO 800
            END IF
C
            IF(MNX.NE.MDX.OR.MNY.NE.MDY)THEN
              WRITE(KFILDO,140)(MTP3(J),J=1,4),(NGRIDC(J,NSLABT),J=1,6),
     1                          MDX,MDY,
     2                         (MTP3(J),J=1,4),(NGRIDC(J,NSLABP),J=1,6),
     3                          MNX,MNY
 140          FORMAT(/,' ****THE GRIDS NEEDED IN SNOWEQ HAVE DIFFERENT',
     1                ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2                ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3                (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
              IER=100
              GOTO 800
            END IF
C
            GOTO 200
C
          ELSE IF(I.EQ.2.OR.I.EQ.4.OR.I.EQ.6.OR.I.EQ.8)THEN
            CALL GFETCH(KFILDO,KFIL10,MTP3,7777,LSTORE,ND9,LITEMS,
     1                IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDT1,ND2X3,
     2                NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3                NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
            IF(MISSP.NE.0)MISTOT=MISTOT+1
            IF(IER.NE.0)GOTO 800
C  
            MDX=IS2(3)
            MDY=IS2(4)
            NSLABT=NSLAB
C
C              COMPARE IF THE GRID CHARACTERISTICS ARE THE SAME.
C
            IF(NSLABP.NE.NSLABT)THEN
              WRITE(KFILDO,135)NSLABP,NSLABT
 145          FORMAT(/,' ****THE GRID CHARACTERISTICS OF THE ',
     1          'TEMP COMPARISON ARE DIFFERENT.',I3,2X,I3)
              IER=100
              GOTO 800
            END IF
C
            IF(MNX.NE.MDX.OR.MNY.NE.MDY)THEN
              WRITE(KFILDO,140)(MTP3(J),J=1,4),(NGRIDC(J,NSLABT),J=1,6),
     1                          MDX,MDY,
     2                         (MTP3(J),J=1,4),(NGRIDC(J,NSLABP),J=1,6),
     3                          MNX,MNY
 150          FORMAT(/,' ****THE GRIDS NEEDED IN SNOWEQ HAVE DIFFERENT',
     1                ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2                ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3                (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
              IER=100
              GOTO 800
            END IF
C
            GOTO 200
C
          ELSE IF(I.EQ.3.OR.I.EQ.5.OR.I.EQ.7)THEN
            CALL GFETCH(KFILDO,KFIL10,MTP3,7777,LSTORE,ND9,LITEMS,
     1                IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDT2,ND2X3,
     2                NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3                NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
            IF(MISSP.NE.0)MISTOT=MISTOT+1
            IF(IER.NE.0)GOTO 800
C
            MNX=IS2(3)
            MNY=IS2(4)
            NSLABP=NSLAB
C
C              COMPARE IF THE GRID CHARACTERISTICS ARE THE SAME.
C
            IF(NSLABP.NE.NSLABT)THEN
              WRITE(KFILDO,135)NSLABP,NSLABT
 155          FORMAT(/,' ****THE GRID CHARACTERISTICS OF THE ',
     1          'TEMP COMPARISON ARE DIFFERENT.',I3,2X,I3)
              IER=100
              GOTO 800
            END IF
C
            IF(MNX.NE.MDX.OR.MNY.NE.MDY)THEN
              WRITE(KFILDO,140)(MTP3(J),J=1,4),(NGRIDC(J,NSLABT),J=1,6),
     1                          MDX,MDY,
     2                         (MTP3(J),J=1,4),(NGRIDC(J,NSLABP),J=1,6),
     3                          MNX,MNY
 160          FORMAT(/,' ****THE GRIDS NEEDED IN SNOWEQ HAVE DIFFERENT',
     1                ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2                ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3                (5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
              IER=100
              GOTO 800
            END IF
C
            GOTO 200
C
          END IF
C
C            GET THE PRECIP AMT; DEPENDING ON THE HOUR CALL GFETCH
C            OR TPCP3
C
 200      IF(I.EQ.2.OR.I.EQ.4.OR.I.EQ.6.OR.I.EQ.8)THEN
            MP3(1)=ICCCFFF(2)*1000+IDPARS(4)
            MP3(2)=IDPARS(7)
            MP3(3)=MTP3(3)+3
            MP3(4)=0
            CALL GFETCH(KFILDO,KFIL10,MP3,7777,LSTORE,ND9,LITEMS,
     1                IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2                NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3                NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
            IF(MISSP.NE.0)MISTOT=MISTOT+1
            IF(IER.NE.0)GOTO 800
          ELSE
            MP32(1)=ICCCFFF(2)*1000+IDPARS(4)
            MP32(2)=IDPARS(7)
            MP32(3)=MTP3(3)+3
            MP32(4)=0
            CALL PRSID1(KFILDO,MP32,MDPARS)
            CALL TPCP3(KFILDO,KFIL10,MDPARS,MP32,NDATE,
     1                 NGRIDC,ND11,NSLAB,IPACK,IWORK,
     2                 FD2,ND5,LSTORE,ND9,LITEMS,CORE,ND10,
     3                 NBLOCK,NFETCH,IS0,IS1,IS2,IS4,ND7,FD1,
     4                 ND2X3,ISTAV,L3264B,MISTOT,
     5                 IER)
C
            IF(IER.NE.0)GOTO 800
          END IF
C
          DO 300 J=1,ND2X3
            IF(FDT1(J).EQ.9999.OR.FDT2(J).EQ.9999.OR.
     1         FD2(J).EQ.9999)THEN
               DATA(J)=9999.
              GOTO 300
            ELSE
              FD3(J)=(FDT1(J)+FDT2(J))/2
              IF(FD3(J).GE.271.AND.FD3(J).LE.274)THEN
                FD4(J)=(FD2(J)/25.4)*10
              ELSEIF(FD3(J).GE.267.AND.FD3(J).LT.271)THEN
                FD4(J)=(FD2(J)/25.4)*15
              ELSEIF(FD3(J).GE.264.AND.FD3(J).LT.267)THEN
                FD4(J)=(FD2(J)/25.4)*20
              ELSEIF(FD3(J).GE.261.AND.FD3(J).LT.264)THEN
                FD4(J)=(FD2(J)/25.4)*30
              ELSEIF(FD3(J).GE.255.AND.FD3(J).LT.261)THEN
                FD4(J)=(FD2(J)/25.4)*40
              ELSEIF(FD3(J).GE.244.AND.FD3(J).LT.255)THEN
                FD4(J)=(FD2(J)/25.4)*50
              ELSEIF(FD3(J).GE.233.AND.FD3(J).LT.244)THEN
                FD4(J)=(FD2(J)/25.4)*100
              ELSEIF(FD3(J).GT.274)THEN
                FD4(J)=0.
              END IF   
C
            END IF  
C
            DATA(J)=DATA(J)+FD4(J)                    
 300      CONTINUE
C
          MTP3(3)=MTP3(3)-3
        END DO
C
        GOTO 900
C
C         MRF PART
C
      ELSE IF(IDPARS(2).EQ.645)THEN
C
        DO I=1,2
C
          IF(I.EQ.1)THEN   
            MTP3(1)=ICCCFFF(1)*1000+IDPARS(4)
            MTP3(2)=2
            MTP3(3)=IDPARS(9)*1000000+IDPARS(12)
            MTP3(4)=0
C
C              READ 12 HR 2-M TEMP.
C
            CALL GFETCH(KFILDO,KFIL10,MTP3,7777,LSTORE,ND9,LITEMS,
     1                IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDT1,ND2X3,
     2                NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3                NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
            IF(MISSP.NE.0)MISTOT=MISTOT+1
            IF(IER.NE.0)GOTO 800
C
            MDX=IS2(3)
            MDY=IS2(4)
            NSLABT=NSLAB
C   
C              CREATE ID FOR T-12 HR 2-M TEMP.
C
            MTP3(3)=IDPARS(9)*1000000+IDPARS(12)-12
            CALL GFETCH(KFILDO,KFIL10,MTP3,7777,LSTORE,ND9,LITEMS,
     1                IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDT2,ND2X3,
     2                NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3                NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
            IF(MISSP.NE.0)MISTOT=MISTOT+1
            IF(IER.NE.0)GOTO 800
C
            MNX=IS2(3)
            MNY=IS2(4)
            NSLABP=NSLAB
C
C              COMPARE IF THE GRID CHARACTERISTICS ARE THE SAME.
C
            IF(NSLABP.NE.NSLABT)THEN
              WRITE(KFILDO,305)NSLABP,NSLABT
 305          FORMAT(/,' ****THE GRID CHARACTERISTICS OF ',
     1          'THE 12 HR TEMPERATURES ARE DIFFERENT.',I3,2X,I3)
              IER=100
              GOTO 800
            END IF
C
            IF(MNX.NE.MDX.OR.MNY.NE.MDY)THEN
C
C               THE GRID CHARACTERISTICS ARE THE SAME.
C
              WRITE(KFILDO,310)(MTP3(J),J=1,4),(NGRIDC(J,NSLABT),J=1,6),
     1                          MDX,MDY,
     2                         (MTP3(J),J=1,4),(NGRIDC(J,NSLABP),J=1,6),
     3                          MNX,MNY
 310          FORMAT(/,' ****THE GRIDS NEEDED IN SNOWEQ HAVE DIFFERENT',
     1                 ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2                 ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3              (5X,I9.9,I10.9,I4.3,4X,6I10,4X,2I5))
              IER=100
              GOTO 800
            END IF
C
            GOTO 350
C
          ELSE
            CALL GFETCH(KFILDO,KFIL10,MTP3,7777,LSTORE,ND9,LITEMS,
     1                IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDT1,ND2X3,
     2                NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,NBLOCK,
     3                NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
            IF(MISSP.NE.0)MISTOT=MISTOT+1
            IF(IER.NE.0)GOTO 800
C
            MDX=IS2(3)
            MDY=IS2(4)
            NSLABT=NSLAB
C
C              COMPARE IF THE GRID CHARACTERISTICS ARE THE SAME.
C
            IF(NSLABP.NE.NSLABT)THEN
              WRITE(KFILDO,315)NSLABP,NSLABT
 315          FORMAT(/,' ****THE GRID CHARACTERISTICS OF ',
     1          'THE 12 HR TEMPERATURES ARE DIFFERENT.',I3,2X,I3)
              IER=100
              GOTO 800
            END IF
C
            IF(MNX.NE.MDX.OR.MNY.NE.MDY)THEN
              WRITE(KFILDO,320)(MTP3(J),J=1,4),(NGRIDC(J,NSLABT),J=1,6),
     1                          MDX,MDY,
     2                         (MTP3(J),J=1,4),(NGRIDC(J,NSLABP),J=1,6),
     3                          MNX,MNY
 320          FORMAT(/,' ****THE GRIDS NEEDED IN SNOWEQ HAVE DIFFERENT',
     1                 ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2                 ' VALUES FROM NGRIDC(,) AND MX,MY.'/
     3              (5X,I9.9,I10.9,I4.3,4X,6I10,4X,2I5))
              IER=100
              GOTO 800
C
            END IF
C
            GOTO 350
C
          END IF
C
C            GET THE 12 HOUR PRECIP.
C
 350      MP3(1)=ICCCFFF(3)*1000+IDPARS(4)
          MP3(2)=IDPARS(7)
          MP3(3)=MTP3(3)+12
          MP3(4)=0
           CALL PRSID1(KFILDO,MP3,MDPARS)
           CALL TPCP12(KFILDO,KFIL10,MDPARS,MP3,NDATE,
     1                  NGRIDC,ND11,NSLAB,IPACK,IWORK,
     2                  FD2,ND5,LSTORE,ND9,LITEMS,CORE,ND10,
     3                  NBLOCK,NFETCH,IS0,IS1,IS2,IS4,ND7,FD1,
     4                  FD3,FD4,ND2X3,ISTAV,L3264B,MISTOT,
     5                  IER)
          IF(MISSP.NE.0)MISTOT=MISTOT+1
          IF(IER.NE.0)GOTO 800
C
          DO 380 J=1,ND2X3
            IF(FDT1(J).EQ.9999.OR.FDT2(J).EQ.9999.OR.
     1         FD2(J).EQ.9999)THEN
               DATA(J)=9999.
              GOTO 380
            ELSE
              FD3(J)=(FDT1(J)+FDT2(J))/2
              IF(FD3(J).GE.271.AND.FD3(J).LE.274)THEN
                FD4(J)=(FD2(J)/25.4)*10
              ELSEIF(FD3(J).GE.267.AND.FD3(J).LT.271)THEN
                FD4(J)=(FD2(J)/25.4)*15
              ELSEIF(FD3(J).GE.264.AND.FD3(J).LT.267)THEN
                FD4(J)=(FD2(J)/25.4)*20
              ELSEIF(FD3(J).GE.261.AND.FD3(J).LT.264)THEN
                FD4(J)=(FD2(J)/25.4)*30
              ELSEIF(FD3(J).GE.255.AND.FD3(J).LT.261)THEN
                FD4(J)=(FD2(J)/25.4)*40
              ELSEIF(FD3(J).GE.244.AND.FD3(J).LT.255)THEN
                FD4(J)=(FD2(J)/25.4)*50
              ELSEIF(FD3(J).GE.233.AND.FD3(J).LT.244)THEN
                FD4(J)=(FD2(J)/25.4)*100
              ELSEIF(FD3(J).GT.274)THEN
                FD4(J)=0.
              END IF   
            END IF  
C
            DATA(J)=DATA(J)+FD4(J)                    
 380      CONTINUE
C
        MTP3(3)=MTP3(3)-12
        END DO
C
      END IF      
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
