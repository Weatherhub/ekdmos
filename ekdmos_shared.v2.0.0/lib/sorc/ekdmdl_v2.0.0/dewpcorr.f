      SUBROUTINE DEWPCORR(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                    NGRIDC,ND11,NSLAB,IPACK,IWORK,CORR,ND5,
     2                    LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                    IS0,IS1,IS2,IS4,ND7,
     4                    FD1,FD2,FD3,FD4,FRR,FOO,FTT,ND2X3,
     5                    ISTAV,L3264B,MISTOT,IER)
C
C        FEBRUARY  2002   CARROLL   TDL   MOS-2000
C        MARCH     2002   CARROLL   ADDED CODE SO THAT IF THE
C                                   PREVIOUS DAYS MODEL RUN IS MISSING
C                                   THE ORIGINAL FIELD IS PRESERVED.
C        MARCH     2002   CARROLL   CLEANED UP COMMENTS.
C        DECEMBER  2002   WEISS     CHANGED ND5 TO ND2X3
C                                   NOTE: CODE WAS NOT TESTED WITH
C                                   THESE CHANGES.
C        APRIL     2003   GLAHN     SET DIMENSIONS OF IPACK( ),
C                                   IWORK( ), CORR( ) TO ND5; CHANGED
C                                   ND5 TO ND2X3 IN CALLS TO DEWPT
C        APRIL     2003   CARROLL   GOT RID OF AUTOMATIC ARRAYS, REMOVED
C                                   TESTS OF MISSP AFTER CALL TO DEWPT, 
C                                   AND FIXED UP CODE.
C                                   NOTE: CODE WAS TESTED SINCE THE
C                                   DECEMBER 2002 CHANGES.
C        MAY       2003   GLAHN     SLIGHT COSMETIC AND FORMAT MODS.
C        MAY       2003   GLAHN     CHANGED CORR( ) FROM INTEGER TO REAL;
C                                   CHANGED STOP TO IER=187 IN 3 PLACES
C                                   AND DIAGNOSTIC IMPROVED
C        JUNE      2003   GLAHN     MOVED DEFINTION OF NX,NY BELOW TEST
C                                   ON IER AFTER GFETCH
C                                     
C        PURPOSE
C            TO COMPUTE A BIAS CORRECTED FIELD FOR DEWPOINT TEMPERATURE.
C            THIS SUBROUTINE CALCULATES THE BIAS CORRECTED DEWPOINT BY
C            SUBTRACTING A FORECAST DEWPOINT FIELD FROM A MODEL RUN RR
C            HOURS AGO FROM THIS MODEL RUNS INITIAL DEWPOINT FIELD.  THE
C            BIAS IS THEN ADDED TO THE FORECAST DEWPOINT FIELD SPECIFIED 
C            IN THE MOS-2000 ID.
C
C            ID FOR THE 24 HOUR BIAS CORRECTED TEMPERATURE:
C
C                003 190 - THE ISOBARIC CORRECTED DEWPOINT.
C                003 191 - THE 2-METER CORRECTED DEWPOINT.
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
C             CORR(K) = BIAS CORRECTED DEWPOINT IN DEGREES KELIVN (K=1,ND5).
C                       SINCE THERE ARE NO SPATIAL RELATIONSHIPS INVOLVED,
C                       THE COMPUTATION CAN BE IN A LINEAR ARRAY.
C                       UPON RETURN, THE ARRAY WILL BE NX X NY.
C                       (OUTPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND CORR( ).
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
C   FD1( ),FD2( ),ETC = WORK ARRAYS (K=1,ND2X3).  (INTERNAL)
C              FRR(K) = WORK ARRAY TO HOLD THE DEWPOINT FIELD FROM YESTERDAYS
C                       MODEL RUN RR HOURS AGO IN DEGREES KELVIN.  (SO FAR
C                       ONLY 24 HOURS IS ACCOMMODATED.  (K=1,ND2X3).  (INTERNAL) 
C              FOO(K) = WORK ARRAY TO HOLD THE INITIAL DEWPOINT FIELD FROM
C                       TODAYS MODEL RUN IN DEGREES KELVIN.
C                       (K=1,ND2X3).  (INTERNAL)
C              FTT(K) = WORK ARRAY TO HOLD THE FORECAST DEWPOINT FIELD FROM
C                       TODAYS MODEL RUN IN DEGREES KELVIN.  
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
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE 
C                             DEWPOINT CORRECTION.
C                       187 = WHEN IDPARS(10) DOES NOT EQUAL ZERO
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C              NSLABA = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE DEWPOINT FROM THE MODEL RUN RR
C                       HOURS AGO.  THIS IS THE VALUE OF NSLAB RETURNED.
C                       WHEN IER NE 0, THIS VALUE SHOULD NOT BE USED.
C                       (INTERNAL) 
C              NSLABB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE INITIAL DEWPOINT FROM THE
C                       CURRENT MODEL RUN.  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED.  (INTERNAL) 
C              NSLABC = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE FORECAST DEWPOINT FROM THE
C                       CURRENT MODEL RUN.  THIS IS THE
C                       VALUE OF NSLAB RETURNED.  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED.  (INTERNAL)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT THE 
C                       RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ).  (INTERNAL)
C              MD1(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FRR( ) (J=1,4).  (INTERNAL)
C              MD2(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FOO( ) (J=1,4).  (INTERNAL)
C              MD3(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FTT( ) (J=1,4).  (INTERNAL)
C            MDPARS() = PARSED ID USED IN SUBROUTINE PRSID1 FOR
C                       SUBROUTINE DEWPOINT.  (INTERNAL)
C         ITABLE(I,J) = CCCFFF OF THE ISOBARIC DEWPOINT (I=1) AND THE
C                       2-METER DEWPOINT.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH,PRSID1,DEWPT
C
      IMPLICIT NONE
C
      INTEGER IDPARS(15),JD(4)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER MD1(4),MD2(4),MD3(4),MDPARS(15)
      INTEGER ITABLE(2,2)
      INTEGER IER,ISTAV,J,KFILDO,KFIL10,L3264B,
     1        JJ,L,NSLAB,NSLABA,NSLABB,NSLABC,MISTOT,
     2        IXJY,NX,NY,ND11,ND5,NDATE,ND9,NFETCH,
     3        ND7,ND2X3,LITEMS,ND10,NBLOCK
C
      REAL CORR(ND5)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),
     1     FRR(ND2X3),FOO(ND2X3),FTT(ND2X3)
      REAL CORE(ND10)
C
      DATA ITABLE/003100, 003190,
     1            003101, 003191/
C
      IER=0
      ISTAV=0
C
C        FIND THE LOCATION IN THE TABLE.
C
      DO 105 JJ=1,2
      IF(ITABLE(2,JJ).EQ.IDPARS(1)*1000+IDPARS(2)) GOTO 115
 105  CONTINUE 
C
      WRITE(KFILDO,110)(JD(L),L=1,4)
 110  FORMAT(/' ****DEWPCORR ENTERED FOR PREDICTOR',
     1         2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2        ' NOT ACCOMMODATED.')
      IER=103
      GO TO 800
C
C	READ THE DEWPOINT FROM THE MODEL RR RUNS AGO AND FIND
C	THE PROJECTION (RR) TO COMPARE WITH THE INITIAL STATE FOR
C	PERFORMING THE BIAS CORRECTION.  IN OTHER WORDS, WE ARE 
C	GETTING THE DEWPOINT FROM THE MODEL RUN IN THE PAST
C	TO COMPARE WITH THE INITIAL DEWPOINT FROM TODAYS
C	RUN.  TO DO THIS, IDPARS(12) (ttt) IS SET TO IDPARS(9)
C
 115  MD1(1)=ITABLE(1,JJ)*1000+IDPARS(3)*100+IDPARS(4)
      MD1(2)=IDPARS(7)
      MD1(3)=IDPARS(9)*1000000+IDPARS(9)
      MD1(4)=0
C
      IF(IDPARS(10).NE.0) THEN
         IER=187
         WRITE(KFILDO,120) IDPARS(10),(JD(L),L=1,4),IER
 120     FORMAT(/' ****IDPARS(10) DOES NOT EQUAL ZERO IN DEWPCORR.',
     1          '  IDPARS(10) EQUALS ',I2,
     2          /'     VARIABLE ',I9.9,2I10.9,I4.3,' NOT ',
     3           'COMPUTED.  IER=',I3)
         GO TO 800
      ENDIF
C
      CALL PRSID1(KFILDO,MD1,MDPARS)
      CALL DEWPT(KFILDO,KFIL10,MDPARS,MD1,NDATE,
     1           NGRIDC,ND11,NSLABA,IPACK,IWORK,FRR,ND2X3,
     2           LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3           IS0,IS1,IS2,IS4,ND7,
     4           FD1,FD2,FD3,FD4,ND2X3,
     5           ISTAV,L3264B,MISTOT,IER)
C
C         IF IER.EQ.47, PREVIOUS MODEL RUN IS MISSING AND ORIGINAL FORECAST
C         FIELD IS PRESERVED.
C
      IF(IER.EQ.47) GO TO 700
      IF(IER.NE.0) GO TO 800
C	IF IER DOES NOT EQUAL 0, A DIAGNOSTIC WILL BE PRINTED BY DEWPCORR.
C
      NX=IS2(3)
      NY=IS2(4)
C
C	NOW WE WANT TO READ TODAYS ANALYZED TEMPERATURE WITH A ttt=0.
C       WE ARE SETTING ttt=0.
C
      MD2(1)=ITABLE(1,JJ)*1000+IDPARS(3)*100+IDPARS(4)
      MD2(2)=IDPARS(7)
      MD2(3)=0
      MD2(4)=0
C
      IF(IDPARS(10).NE.0) THEN
         IER=187
         WRITE(KFILDO,150) IDPARS(10),(JD(L),L=1,4),IER
 150     FORMAT(/' ****IDPARS(10) DOES NOT EQUAL ZERO IN DEWPCORR.',
     1          '  IDPARS(10) EQUALS ',I2,
     2          /'     VARIABLE ',I9.9,2I10.9,I4.3,' NOT ',
     3           'COMPUTED.  IER=',I3)
         GO TO 800
      ENDIF
C
      CALL PRSID1(KFILDO,MD2,MDPARS)
      CALL DEWPT(KFILDO,KFIL10,MDPARS,MD2,NDATE,
     1           NGRIDC,ND11,NSLABB,IPACK,IWORK,FOO,ND2X3,
     2           LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3           IS0,IS1,IS2,IS4,ND7,
     4           FD1,FD2,FD3,FD4,ND2X3,
     5           ISTAV,L3264B,MISTOT,IER)
      IF(IER.NE.0) GO TO 800
C       IF IER DOES NOT EQUAL 0, A DIAGNOSTIC WILL BE PRINTED BY DEWPT.
C
      IF(NSLABA.EQ.NSLABB) GO TO 170
      WRITE(KFILDO,160)
 160  FORMAT(/' ****THE 2 GRIDS NEEDED IN DEWPCORR HAVE DIFFERENT',
     1        ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.')
 170  NSLAB=NSLABB
C
C	NOW FETCH TODAYS ttt TEMPERATURE FORECAST.
C
      MD3(1)=ITABLE(1,JJ)*1000+IDPARS(3)*100+IDPARS(4) 
      MD3(2)=IDPARS(7)
      MD3(3)=IDPARS(12)
      MD3(4)=0
C
      IF(IDPARS(10).NE.0) THEN
         IER=187
         WRITE(KFILDO,250) IDPARS(10),(JD(L),L=1,4),IER
 250     FORMAT(/' ****IDPARS(10) DOES NOT EQUAL ZERO IN DEWPCORR.',
     1          '  IDPARS(10) EQUALS ',I2,
     2          /'     VARIABLE ',I9.9,2I10.9,I4.3,' NOT ',
     3           'COMPUTED.  IER=',I3)
         GO TO 800
      ENDIF
C
      CALL PRSID1(KFILDO,MD3,MDPARS)
      CALL DEWPT(KFILDO,KFIL10,MDPARS,MD3,NDATE,
     1           NGRIDC,ND11,NSLABC,IPACK,IWORK,FTT,ND2X3,
     2           LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3           IS0,IS1,IS2,IS4,ND7,
     4           FD1,FD2,FD3,FD4,ND2X3,
     5           ISTAV,L3264B,MISTOT,IER)
      IF(IER.NE.0) GO TO 800
C
      IF(NSLABB.EQ.NSLABC) GO TO 270
      WRITE(KFILDO,260)
 260  FORMAT(/' ****THE 2 GRIDS NEEDED IN DEWPCORR HAVE DIFFERENT',
     1        ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.')
C
C       IF IER DOES NOT EQUAL 0, A DIAGNOSTIC WILL BE PRINTED BY GFETCH.
C
C	COMPUTE THE BIAS AND APPLY IT TO THE DESIRED TEMPERATURE FIELD.
C
 270  CONTINUE
C
      DO 310 IXJY=1,NX*NY
         CORR(IXJY)=(FTT(IXJY)+(FOO(IXJY)-FRR(IXJY)))
 310  CONTINUE
C
      GOTO 900
C
C       FETCH TODAYS ttt TEMPERATURE FORECAST.  THIS IS SET TO CORR
C       IN A CASE WHERE YESTERDAYS MODEL DATA WAS NOT AVAILABLE.
C
 700  CALL PRSID1(KFILDO,MD3,MDPARS)
      CALL DEWPT(KFILDO,KFIL10,MDPARS,MD3,NDATE,
     1           NGRIDC,ND11,NSLAB,IPACK,IWORK,CORR,ND5,
     2           LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3           IS0,IS1,IS2,IS4,ND7,
     4           FD1,FD2,FD3,FD4,ND2X3,
     5           ISTAV,L3264B,MISTOT,IER)
      IF(IER.NE.0) GO TO 800
C
C       IF IER DOES NOT EQUAL 0, A DIAGNOSTIC WILL BE PRINTED BY GFETCH.
C
C       IF PREVIOUS MODEL RUN IS MISSING, PRESERVE FORECAST DEWPOINT
C       FIELD FROM TODAYS RUN.
C
      WRITE(KFILDO,720) NDATE
720   FORMAT(/,' ****DEWPOINT FROM PREVIOUS MODEL RUN IS MISSING, ',
     1         'FOR DATE, ',I10,', DEWPOINT FIELD IS UNCHANGED ',
     2         'BY DEWPCORR.')
C
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
