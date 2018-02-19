      SUBROUTINE WCHILL(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                  NGRIDC,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     2                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                  IS0,IS1,IS2,IS4,ND7,
     4                  FD1,FD2,FD3,ND2X3,
     5                  ISTAV,L3264B,MISTOT,IER)
C
C        OCTOBER   2008   JRW     MDL MOS-2000
C
C        PURPOSE 
C            TO COMPUTE WIND CHILL IN DEGREES F USING THE 10M MODEL
C            WIND AND 2 M MODEL TEMPERATURE. IF WIND SPEED IS NOT AT
C            LEAST 5 MPH AND THE TEMPERATURE IS NOT AT LEAST 
C            50 DEGREES F SET WIND CHILL TEMPERATURE TO SURFACE
C            TEMPERATURE IN DEGREES F.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               002 221 - MODEL SURFACE WIND CHILL 
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID( ) (J=1,15).
C                       (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE PORTIONS
C                       PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       JD( ) IS USED TO HELP IDENTIFY THE BASIC MODEL FIELDS
C                       AS READ FROM THE ARCHIVE.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.  (INPUT)
C         NGRIDC(L,M) = HOLDS THE GRID CHARACTERISTICS (L=1,6) FOR EACH GRID
C                       COMBINATION (M=1,NGRID).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=POLAR
C                            STEREOGRAPHIC). 
C                       L=2--GRID LENGTH IN METERS,
C                       L=3--LATITUDE AT WHICH GRID LENGTH IS CORRECT *1000,
C                       L=4--GRID ORIENTATION IN DEGREES *1000,
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *1000,
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES *1000.
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN BE
C                       DEALT WITH ON THIS RUN.  LAST DIMENSION OF
C                       NGRIDC( , ).  (INPUT)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND
C                       IN NGRIDC( , ) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  (OUTPUT) 
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(K) = COMPUTED WIND CHILL TEMPERATURE.
C                       (J=1,ND5).  SINCE THERE ARE NO SPATIAL
C                       RELATIONSHIPS INVOLVED, THE COMPUTATION CAN 
C                       BE IN A LINEAR ARRAY. UPON RETURN, THE ARRAY
C                       WILL BE NX X NY.  (OUTPUT)
C                 ND5 = DIMENSION OF IPACK( ),IWORK( ), AND SPEED( ).
C                       (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS).  (INPUT-OUTPUT)
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK, 
C                              THIS IS MINUS THE RECORD NUMBER WHERE 
C                              THE DATA START.
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDL GRIB, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --NUMBER OF THE SLAB IN DIR( , ,L) AND
C                              IN NGRIDC( ,L) DEFINING THE CHARACTERISTICS
C                              OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,NPRED) FOR WHICH THIS
C                              VARIABLE IS NEEDED, WHEN IT IS NEEDED ONLY
C                              ONCE FROM LSTORE( , ).  WHEN IT IS NEEDED
C                              MORE THAN ONCE, THE VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING MSTORE( , ).
C                              LATER USED AS A WAY OF DETERMINING WHETHER
C                              TO KEEP THIS VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , ) THAT 
C                       HAVE BEEN USED IN THIS RUN.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL
C                       DATA ARE STORED ON DISK.  (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING  COUNT FROM THE BEGINNING OF THE 
C                       PROGRAM.  THIS COUNT IS MAINTAINED IN CASE THE USER 
C                       NEEDS IT (DIAGNOSTICS, ETC.).  (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       IS2(3) AND IS2(4) ARE USED BY THE CALLING
C                       PROGRAM AS THE GRID DIMENSIONS.  (INTERNAL-OUTPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C              FD1(K) = WORK ARRAY HOLDING THE WIND SPEED.
C                       (K=1,ND2X3).  (INTERNAL)
C              FD2(K) = WORK ARRAY.(K=1,ND2X3).  (INTERNAL)
C              FD3(K) = WORK ARRAY.(K=1,ND2X3).  (INTERNAL)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF THE
C                       GRID IS NOT KNOWN BEFORE THE U AND V WINDS ARE
C                       FETCHED.  (INPUT)
C               ISTAV = 0 SINCE THE DATA RETURNED ARE GRID DATA.  (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING USED
C                       (EITHER 32 OR 64).  (INPUT)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       100 = THE TWO GRIDS NEEDED ARE NOT THE SAME SIZE.
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE 
C                             WIND SPEED.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C              NSLABU = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE U WIND.  THIS IS THE
C                       VALUE OF NSLAB RETURNED.  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED.  (INTERNAL) 
C              NSLABV = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE V WIND.  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED.  (INTERNAL) 
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT THE 
C                       RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ).  (INTERNAL)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FDU( ) (J=1,4).  (INTERNAL)
C               MD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FDV( ) (J=1,4).  (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED.  (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED.  (INTERNAL)
C         ITABLE(I,J) = CCCFFF OF THE SPEED (I=1) AND THE NEEDED
C                       U COMPONENT (I=2) AND V COMPONENT (I=3) FOR 
C                       CONSTANT PRESSURE (J=1) AND CONSTANT HEIGHT (J=2).
C                       (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH
C
      DIMENSION IDPARS(15),JD(4),MDPARS(15)
      DIMENSION FD1(ND2X3),FD2(ND2X3),FD3(ND2X3)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION NGRIDC(6,ND11)
      DIMENSION LD(4),MD(4)
      DIMENSION ICCCFFF(2)
C
      DATA ICCCFFF/002001,004211/
C
CD     CALL TIMPR(KFILDO,KFILDO,'START WCHILL        ')
C
      IER=0
      ISTAV=0
C
C        FIND THE LOCATION IN THE TABLE.
      IF((IDPARS(1).NE.002).OR.(IDPARS(2).NE.221)) THEN
        WRITE(KFILDO,200)(JD(J),J=1,4)
 200    FORMAT(/,' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1           ' WIND CHLL PREDICTOR. ',I9.9,2I10.9,I4.3,
     2           ' NOT COMPUTED IN WCHILL.')
        IER=103
        GOTO 800
      ENDIF
C
C        GET THE 2 METER TEMPERATURE IN DEGREES K.
C
      LD(1)=ICCCFFF(1)*1000+IDPARS(4)
      LD(2)=JD(2)
      LD(3)=IDPARS(9)*1000000+IDPARS(12)+IDPARS(11)
      LD(4)=0
C
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,DATA,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLABL,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GO TO 800
      LX=IS2(3)
      LY=IS2(4)
C
      NSLAB=NSLABL
C
C        CREATE THE MODEL 10M WIND SPEED ID.
C
      MD(1)=ICCCFFF(2)*1000+IDPARS(4)
      MD(2)=000000010
      MD(3)=IDPARS(9)*1000000+IDPARS(12)
      MD(4)=0
C
C        FETCH THE 10M MODEL WIND SPEED IN M/S.
C
      CALL PRSID1(KFILDO,MD,MDPARS)
      CALL WSPEED(KFILDO,KFIL10,MDPARS,MD,NDATE,
     1            NGRIDC,ND11,NSLABM,IPACK,IWORK,FD1,ND5,
     2            LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3            IS0,IS1,IS2,IS4,ND7,
     4            FD2,FD3,ND2X3,
     5            ISTAV,L3264B,MISTOT,IER)
C
       MX=IS2(3)
       MY=IS2(4)
C
      IF(IER.NE.0)GOTO 800
C
C        CHECK THE GRID CHARACTERISTICS.
C
      IF((NSLABL.NE.NSLABM).OR.(LX.NE.MX).OR.(LY.NE.MY))THEN
C           THE GRID CHARACTERISTICS ARE NOT THE SAME.
         IER=100
         WRITE(KFILDO,300)(LD(J),J=1,4),(NGRIDC(J,NSLABL),J=1,6),
     1                     LX,LY,
     2                    (MD(J),J=1,4),(NGRIDC(J,NSLABM),J=1,6),
     3                     MX,MY,IER
 300     FORMAT(/' ****DIFFERENT GRID CHARACTERISTICS.  PREDICTOR ',
     1           'NOT COMPUTED IN WCHILL.  VALUES FROM NGRIDC( , )',
     2           ' AND X*Y ARE:',
     3           2(/,4X,3I10.9,I4.3,4X,6I10,4X,I3,'*',I3),
     4           '  IER =',I4)
         GOTO 900
      ENDIF
C
      DO 400 J=1,ND2X3 
C
         IF(NINT(DATA(J)).NE.9999.AND.NINT(FD1(J)).NE.9999)THEN
C
C              CONVERT WIND SPEED FROM M/S TO MPH
C            FD1(J)=(3600/1609.344)*FD1(J)
            FD1(J)=2.23694*FD1(J)
C
C              CONVERT K TO F
            DATA(J)=(DATA(J)-273.15)*1.8+32.
C
            IF(DATA(J).LE.50.AND.FD1(J).GE.3)THEN
               TERM1=.6215*DATA(J)
               TERM2=FD1(J)**.16
               TERM3=(.4275*DATA(J))*TERM2
               DATA(J)=35.74+TERM1-(35.75*TERM2)+TERM3
            ELSE
               DATA(J)=DATA(J)
            ENDIF
         ELSE
            DATA(J)=9999.
         ENDIF
C
 400  CONTINUE
C
      GO TO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C
 800  DO 801 J=1,ND2X3
      DATA(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END      
