      SUBROUTINE TEMPCORR(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                    NGRIDC,ND11,NSLAB,IPACK,IWORK,CORR,ND5,
     2                    LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                    IS0,IS1,IS2,IS4,ND7,
     4                    FRR,FOO,FTT,FMM,ND2X3,
     5                    ISTAV,L3264B,MISTOT,IER)
C
C        JULY       2001   CARROLL    TDL   MOS-2000
C        DECEMBER   2001   CARROLL    FIXED AN ERROR IN THE FORMULA
C                                     FOR COMPUTING THE CORRECTED
C                                     TEMPERATURE.
C        MARCH      2002   CARROLL    ADDED CODE SO THAT IF THE
C                                     PREVIOUS DAYS MODEL RUN IS MISSING
C                                     THE ORIGINAL FIELD IS PRESERVED.
C        MARCH      2002   CARROLL    CLEANED UP COMMENTS.
C        DECEMBER   2002   WEISS      CHANGED ND5 TO ND2X3
C                                     NOTE: CODE WAS NOT TESTED WITH 
C                                     THESE CHANGES.
C
C        PURPOSE:  TO COMPUTE A BIAS CORRECTED FIELD FOR TEMPERATURE.
C                  THIS SUBROUTINE CALCULATES THE BIAS CORRECTED TEMPERATURE BY
C                  SUBTRACTING A FORECAST TEMPERATURE FIELD FROM A MODEL RUN RR
C                  HOURS AGO FROM THIS MODEL RUNS INITIAL TEMPERATURE FIELD.  THE
C                  BIAS IS THEN ADDED TO THE FORECAST TEMPERATURE FIELD SPECIFIED 
C                  IN THE MOS-2000 ID.
C
C                  ID FOR THE 24 HOUR BIAS CORRECTED TEMPERATURE:
C
C                        002200  =  THE ISOBARIC CORRECTED TEMPERATURE.
C                        002201  =  THE 2-METER CORRECTED TEMPERATURE.
C                        002207  -  THE BOUNDARY LAYER CORRECTED TEMPERATURE.
C
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
C            IPACK(J) = WORK ARRAY (J=1,ND2X3).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND2X3).  (INTERNAL)
C             CORR(J) = BIAS CORRECTED TEMPERATURE IN DEGREES KELVIN.
C                       SINCE THERE ARE NO SPATIAL RELATIONSHIPS INVOLVED,
C                       THE COMPUTATION CAN BE IN A LINEAR ARRAY.
C                       UPON RETURN, THE ARRAY WILL BE NX X NY.
C                       (OUTPUT)
C                 ND5 = FORMER DIMENSION OF IPACK( ),IWORK( ), AND CORR( ).
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
C                       L=12 --USED INITIALLY IN ESTABLISHING MOSTORE( , ).
C                              LATER USED AS A WAY OF DETERMINING WHETHER
C                              TO KEEP THIS VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , ) THAT 
C                       HAVE BEEN USED IN THIS RUN.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETIREVE THE DATA IDENTIFIED IN
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
C              FRR(K) = WORK ARRAY TO HOLD THE TEMPERATURE FROM RR RUNS
C                       AGO IN DEGREES KELVIN (K=1,ND2X3).  (INTERNAL)
C              FOO(K) = WORK ARRAY TO HOLD THE INITIAL MODEL TEMPERATURE
C                       IN DEGREES KELVIN (K=1,ND2X3).  (INTERNAL)
C              FTT(K) = WORK ARRAY TO HOLD THE FORECAST MODEL TEMPERATURE
C                       IN DEGREES KELVIN (K=1,ND2X3).  (INTERNAL)
C              FMM(K) = WORK ARRAY TO HOLD THE FORECAST MODEL TEMPERATURE
C                       IN DEGREES KELVIN (K=1,ND2X3).  THIS IS USED WHEN
C                       THE MODEL RUN FROM RR RUNS AGO IS MISSING ONLY.
C                       (INTERNAL)
C             BIAS(K) = INTERNAL WORK ARRAY TO HOLD THE CALCULATED BIAS.  THE
C                       BIAS EQUALS THE INITIAL MODEL FIELD MINUS THE MODEL 
C                       FIELD FROM RR RUNS AGO, OR FOO(K)-FRR(K).
C                       (K=1,ND2X3).  (INTERNAL)
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
C                       101 = GRID SIZE IS TOO BIG FOR CORR( ) WHOSE
C                             DIMENSION IS ND5.
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE 
C                             FOR BIAS CORRECTED TEMPERATURE.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C              NSLABA = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE TEMPERATURE FROM RR RUNS AGO.  
C                       THIS IS THE VALUE OF NSLAB RETURNED.  WHEN IER NE 0,
C                       THIS VALUE SHOULD NOT BE USED.  (INTERNAL) 
C              NSLABB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE INITIAL MODEL TEMPERATURE.  
C                       THIS IS THE VALUE OF NSLAB RETURNED.  WHEN IER NE 0,
C                       THIS VALUE SHOULD NOT BE USED.  (INTERNAL)
C              NSLABC = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE FORECAST MODEL TEMPERATURE.  
C                       THIS IS THE VALUE OF NSLAB RETURNED.  WHEN IER NE 0,
C                       THIS VALUE SHOULD NOT BE USED.  (INTERNAL)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT THE 
C                       RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ).  (INTERNAL)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FRR( ) (J=1,4).  (INTERNAL)
C               MD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FOO( ) (J=1,4).  (INTERNAL)
C               ND(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FTT( ) (J=1,4).  (INTERNAL)
C              LD2(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FMM( ) (J=1,4).  (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED.  (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED.  (INTERNAL)
C         ITABLE(I,J) = CCCFFF OF THE ISOBARIC TEMPERATURE (I=1), THE 2-M
C                       TEMPERATURE (I=2) AND THE BOUNDARY LAYER TEMPERATURE
C                       (I=3).
C                       (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH
C
      IMPLICIT NONE
      INTEGER NGRIDC(6,ND11)
      INTEGER IDPARS(15),JD(4)
      INTEGER IPACK(ND2X3),IWORK(ND2X3)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER LD(4),MD(4),ND(4),LD2(4)
      INTEGER ITABLE(2,3)
      INTEGER KFILDO,KFIL10,
     1        NPACK,NTIMES,NWORDS,NDATE,
     2        NSLAB,NSLABA,NSLABB,NSLABC,NBLOCK,NFETCH,
     3        ND5,ND7,ND9,ND10,ND11,
     4        J,JJ,L,NX,NY,IXJY,ND2X3,IER,
     5        MISSP,MISSS,MISTOT,LITEMS,ISTAV,L3264B
      REAL FRR(ND2X3),FOO(ND2X3),FTT(ND2X3),FMM(ND2X3)
      REAL CORE(ND10),BIAS(ND2X3),CORR(ND2X3)
C
      DATA ITABLE/002200, 002000,
     1            002201, 002001,
     2            002207, 002007/
C
      IER=0
      ISTAV=0
C
C        FIND THE LOCATION IN THE TABLE.
C
      DO 105 JJ=1,3
      IF(ITABLE(1,JJ).EQ.IDPARS(1)*1000+IDPARS(2)) GOTO 115
 105  CONTINUE 
C
      WRITE(KFILDO,110)(JD(L),L=1,4)
 110  FORMAT(' ****TEMPCORR ENTERED FOR PREDICTOR',
     1         2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2       ' NOT ACCOMMODATED.')
      IER=103
      GO TO 800
C
C	READ THE TEMPERATURE FROM THE MODEL RR RUNS AGO AND FIND
C	THE PROJECTION (RR) TO COMPARE WITH THE INITIAL STATE FOR
C	PERFORMING THE BIAS CORRECTION.  IN OTHERWORDS, WE ARE 
C	GETTING THE TEMPERATURE FROM THE MODEL RUN IN THE PAST
C	TO COMPARE WITH THE INITIAL TEMPERATURE FROM TODAYS
C	RUN.  TO DO THIS, IDPARS(12) (ttt) IS SET TO IDPARS(9)
C
C
 115  LD(1)=ITABLE(2,JJ)*1000+IDPARS(3)*100+IDPARS(4)
      LD(2)=JD(2)
      LD(3)=IDPARS(9)*1000000+IDPARS(9)
      LD(4)=0
      IF(IDPARS(10).NE.0) THEN
      WRITE(KFILDO,120) IDPARS(10)
 120  FORMAT('****IDPARS(10) DOES NOT EQUAL ZERO.  FOR TEMPCORR TO ',
     +       'RUN PROPERLY IDPARS(10) MUST EQUAL ZERO.  IDPARS(10) ',
     +       'EQUALS ',I2)
      STOP 120
      ENDIF
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FRR,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLABA,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      NX=IS2(3)
      NY=IS2(4)
C
C       IF TEMPERATURE FROM RR RUNS AGO IS UNAVAILABLE GO TO 700 AND
C       LEAVE TEMPERATURE FIELD UNCHANGED.
C
      IF(IER.EQ.47) GO TO 700
      IF(IER.NE.0) GO TO 800
C	IF IER DOES NOT EQUAL 0, A DIAGNOSTIC WILL BE PRINTED BY GFETCH.
C
C	NOW WE WANT TO READ TODAYS ANALYZED TEMPERATURE WITH A ttt=0.
C       WE ARE SETTING ttt=0.
C
      MD(1)=ITABLE(2,JJ)*1000+IDPARS(3)*100+IDPARS(4)
      MD(2)=JD(2)
      MD(3)=0
      MD(4)=0
      IF(IDPARS(10).NE.0) THEN
      WRITE(KFILDO,150) IDPARS(10)
 150  FORMAT('****IDPARS(10) DOES NOT EQUAL ZERO.  FOR TEMPCORR TO ',
     +       'RUN PROPERLY IDPARS(10) MUST EQUAL ZERO.  IDPARS(10) ',
     +       'EQUALS ',I2)
      STOP 150
      ENDIF
      CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FOO,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLABB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0) GO TO 800
C       IF IER DOES NOT EQUAL 0, A DIAGNOSTIC WILL BE PRINTED BY GFETCH.
C
      IF(NSLABA.EQ.NSLABB) GO TO 170
      WRITE(KFILDO,160)
 160  FORMAT(' ****THE 2 GRIDS NEEDED IN TEMPCORR HAVE DIFFERENT',
     1       ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.')
C
C	NOW FETCH TODAYS ttt TEMPERATURE FORECAST.
C
 170  ND(1)=ITABLE(2,JJ)*1000+IDPARS(3)*100+IDPARS(4) 
      ND(2)=JD(2)
      ND(3)=IDPARS(12)
      ND(4)=0
      IF(IDPARS(10).NE.0) THEN
      WRITE(KFILDO,250) IDPARS(10)
 250  FORMAT('****IDPARS(10) DOES NOT EQUAL ZERO.  FOR TEMPCORR TO ',
     +       'RUN PROPERLY IDPARS(10) MUST EQUAL ZERO.  IDPARS(10) ',
     +       'EQUALS ',I2)
      STOP 250
      ENDIF
      CALL GFETCH(KFILDO,KFIL10,ND,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FTT,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLABC,MISSP,MISSS,L3264B,1,IER)
      IF(IER.NE.0) GO TO 800
C
C       IF IER DOES NOT EQUAL 0, A DIAGNOSTIC WILL BE PRINTED BY GFETCH.
C
      IF(NSLABB.EQ.NSLABC) GO TO 270
      WRITE(KFILDO,260)
 260  FORMAT(' ****THE 2 GRIDS NEEDED IN TEMPCORR HAVE DIFFERENT',
     1       ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.')
C
C	COMPUTE THE BIAS AND APPLY IT TO THE DESIRED TEMPERATURE FIELD.
C
 270  DO 310 IXJY=1,NX*NY
      BIAS(IXJY)=(FOO(IXJY)-FRR(IXJY))
      CORR(IXJY)=(FTT(IXJY)+BIAS(IXJY))
 310  CONTINUE
      GOTO 900
C
C        IT TEMPERATURE FROM RR RUNS AGO IS MISSING, LEAVE TEMPERTURE
C        FIELD UNCHANGED.
C
 700  LD2(1)=ITABLE(2,JJ)*1000+IDPARS(3)*100+IDPARS(4)
      LD2(2)=JD(2)
      LD2(3)=IDPARS(12)
      LD2(4)=0
      IF(IDPARS(10).NE.0) THEN
      WRITE(KFILDO,710) IDPARS(10)
 710  FORMAT('****IDPARS(10) DOES NOT EQUAL ZERO.  FOR TEMPCORR TO ',
     +       'RUN PROPERLY IDPARS(10) MUST EQUAL ZERO.  IDPARS(10) ',
     +       'EQUALS ',I2)
      STOP 710
      ENDIF
      CALL GFETCH(KFILDO,KFIL10,LD2,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FMM,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLABC,MISSP,MISSS,L3264B,1,IER)
      IF(IER.NE.0) GO TO 800
C
      WRITE(KFILDO,720) NDATE
720   FORMAT(' ****TEMPERATURE FROM PREVIOUS MODEL RUN IS MISSING, ',
     1       'FOR DATE, ',I10,', TEMPERATURE FIELD IS UNCHANGED ',
     2       'BY TEMPCORR.')
C
      DO 730 IXJY=1,NX*NY
      CORR(IXJY)=FMM(IXJY)
 730  CONTINUE
      GOTO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C
 800  DO 801 J=1,ND2X3
      CORR(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END
