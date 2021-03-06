      SUBROUTINE EOWND(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                 NGRIDC,ND11,NSLAB,IPACK,IWORK,WIND,ND5,
     2                 LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                 IS0,IS1,IS2,IS4,ND7,
     4                 FDU,FDV,ND2X3,
     5                 ISTAV,L3264B,MISTOT,IER)
C
C        NOVEMBER  1994   GLAHN   TDL   MOS-2000
C        AUGUST    1996   GLAHN   ADDED MISSS AND MISTOT
C        AUGUST    1996   GLAHN   ADDED LAT/LONG, GRIDLENGTH ACCURACY 
C        OCTOBER   1996   GLAHN   WIND IDS FROM 004011 AND 004111
C                                 TO 004101 AND 004110
C        JUNE      1997   GLAHN   SLIGHT CHANGE TO COMMENTS, NOT CODE
C        OCTOBER   1998   SFANOS  MODIFIED FOR WIND IDS 004020 AND 
C                                 004120
C        JANUARY   1999   GLAHN   REMOVED TEST ON GRID SIZE AFTER 200
C        JANUARY   2000   GLAHN   ADDED IDS ACCOMMODATED IN PURPOSE
C        MARCH     2001   GLAHN   ADDED COMMAS IN FORMAT; REMOVED
C                                 SOME INOPERATIVE CODE
C        MARCH     2002   RUDACK  MODIFIED CODE TO INCLUDE U AND V
C                                 COMPONENTS IN KTS AT GRIDPOINTS.
C        MAY       2002   GLAHN   CHANGED ND5 TO ND2X3 IN CALL TO GFETCH
C        AUGUST    2002   GLAHN   ADDED LAMBERT AND MERCATOR CAPABILITY
C        OCTOBER   2002   WEISS   CHANGED ND5 TO ND2X3 FOR IPACK, IWORK,
C                                 AND WIND
C        APRIL     2003   GLAHN   SET DIMENSIONS OF IPACK( ), IWORK( )
C                                 AND WIND( ) TO ND5; REMOVED TEST ON
C                                 GRID SIZE AFTER 200; REMOVED STATEMENT
C                                 NUMBER 210; REARRANGED DIMENSION
C                                 STATEMENTS
C        JUNE      2003   GLAHN   ADDED FFF = 066 AND 166 (SIGMA
C                                 SURFACE) TO CALL TO EOUWND AND EOVWND,
C                                 RESPECTIVELY; ADDED CONVERSION TO KT
C                                 FOR FFF = 061 AND 066; REVISED HOW
C                                 ID'S FOR CALL TO GFETCH ARE FORMED --
C                                 CHANGES TO IDS AND CODE OVER YEARS
C                                 MAY RENDER THIS NOT ROBUST
C
C        PURPOSE
C            TO COMPUTE EARTH-ORIENTED U- OR V-WIND IN M/SEC OR
C            GRID U- OR V-WIND IN KNOTS FROM GRID-ORIENTED
C            U- AND V-WINDS.  THE SINGLE LEVEL IS CONTAINED IN JD(2).
C            RUN TIME OFFSET RR, TAU, AND OH ARE ACCOMMODATED.
C            SINCE THE ACTUAL GRID DIMENSIONS NX AND NY ARE NOT KNOWN
C            UNTIL THE DATA ARE FETCHED, THE GRID COMPUTATIONS ARE MADE
C            IN SUBROUTINES EOUWND AND EOVWND.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               004 010 - EARTH-ORIENTED U-WIND ON PRESSURE SFC (M/SEC)
C               004 110 - EARTH-ORIENTED V-WIND ON PRESSURE SFC (M/SEC)
C               004 011 - EARTH-ORIENTED U-WIND ON HEIGHT SFC (M/SEC)
C               004 111 - EARTH-ORIENTED V-WIND ON HEIGHT SFC (M/SEC)
C               004 066 - EARTH-ORIENTED U-WIND ON SIGMA SFC (M/SEC)
C               004 166 - EARTH-ORIENTED V-WIND ON SIGMA SFC (M/SEC)
C               004 061 - EARTH-ORIENTED U-WIND ON HEIGHT SFC (KTS)
C               004 161 - EARTH-ORIENTED V-WIND ON HEIGHT SFC (KTS)
C
C            IT APPEARS THAT THE CONVERSION OF GRID ORIENTED M/S WINDS 
C            TO EARTH ORIENTED KTS
C               FFF = 061 FROM 020 AND
C               FFF = 161 FROM 120 
C            IN EOWND FOR U201 MAY BE THE SAME AS THE CONVERSION OF
C            EARTH ORIENTED M/S WINDS TO EARTH ORIENTED KTS IN MPSKTS
C            FOR OPTX
C               FFF = 031 FROM 011 AND
C               FFF = 131 FROM 111.
C            THAT IS, U201 COULD COMPUTE EARTH ORIENTED M/S WINDS FROM 
C            GRID ORIENTED GRIDS AND THESE COULD BE CONVERTED IN OPTX 
C            TO KTS WITH A DIFFERENT ID (FFF = 031 AND 131) THAN THE
C            DIRECT CONVERSION IN U201 FROM M/S WINDS FROM GRID
C            ORIENTED GRIDS TO EARTH ORIENTED WINDS IN KTS
C            (FFF = 061 AND 161).  NOTE THAT THE FFF = 030 AND 131 ARE
C            NOT IN THE MOS-2000 NOTEBOOK WHICH PROBABLY MEANS THESE
C            ARE IN USE ONLY IN LAMP, AND THE FFF = 061 AND 161 ARE
C            IN USE IN MOS.
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                     (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       PREDICTOR ID CORRESPONDING TO ID( ) (J=1,15).
C                       (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 
C                            1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK 
C                            IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE
C                       PORTIONS PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       JD( ) IS USED TO HELP IDENTIFY THE BASIC MODEL
C                       FIELDS AS READ FROM THE ARCHIVE.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C         NGRIDC(L,M) = HOLDS THE GRID CHARACTERISTICS (L=1,6) FOR EACH
C                       GRID COMBINATION (M=1,NGRID).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=POLAR
C                            STEREOGRAPHIC). 
C                       L=2--GRID LENGTH IN MILLIMETERS,
C                       L=3--LATITUDE AT WHICH GRID LENGTH IS CORRECT
C                            *10000,
C                       L=4--GRID ORIENTATION IN DEGREES *10000,
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *10000,
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES *10000.
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN BE
C                       DEALT WITH ON THIS RUN.  LAST DIMENSION OF
C                       NGRIDC( , ).  (INPUT)
C               NSLAB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE U-WINDS, WHEN THE WINDS
C                       HAVE BEEN CORRECTLY RETURNED.  WHEN IER NE 0,
C                       THIS VALUE SHOULD NOT BE USED.  (OUTPUT) 
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             WIND(K) = EARTH-ORIENTED WIND CALCULATED FROM U AND V
C                       WINDS.  ALTHOUGH THIS IS A LINEAR ARRAY, IT IS
C                       TREATED AS A 2-DIMENSIONAL ARRAY IN EOUWND AND 
C                       EOVWND, AND THE WIND IS RETURNED AS A GRID 
C                       NX BY NY (K=1,ND5).  (OUTPUT)
C                 ND5 = DIMENSION OF IPACK( ),IWORK( ), AND  WIND( ).
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
C                              IN NGRIDC( ,L) DEFINING THE
C                              CHARACTERISTICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,NPRED) FOR WHICH
C                              THIS VARIABLE IS NEEDED, WHEN IT IS 
C                              NEEDED ONLY ONCE FROM LSTORE( , ).  WHEN
C                              IT IS NEEDED MORE THAN ONCE, THE VALUE IS
C                              SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING 
C                              MSTORE( , ).  LATER USED AS A WAY OF
C                              DETERMINING WHETHER TO KEEP THIS
C                              VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE( , ) (J=1,ND10).  WHEN 
C                       CORE( ) IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING  COUNT FROM THE BEGINNING OF THE 
C                       PROGRAM.  THIS COUNT IS MAINTAINED IN CASE THE 
C                       USER NEEDS IT (DIAGNOSTICS, ETC.).  (OUTPUT)
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
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C              FDU(K) = WORK ARRAY TO HOLD THE U WINDS IN M/SEC
C                       (K=1,ND2X3).  (INTERNAL)
C              FDV(K) = WORK ARRAY TO HOLD THE V WINDS IN M/SEC
C                       (K=1,ND2X3).  (INTERNAL)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF
C                       THE GRID IS NOT KNOWN BEFORE THE U AND V WINDS
C                       ARE FETCHED.  (INPUT)
C               ISTAV = 0 SINCE THE DATA RETURNED ARE GRID DATA.
C                       (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        60 = MAP PROJECTION NUMBER NOT EXPECTED.
C                       100 = THE TWO GRIDS NEEDED DO NOT HAVE THE
C                             SAME CHARACTERISTICS.
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE 
C                             EARTH-ORIENTED WINDS.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C              NSLABU = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE U WIND.  THIS IS THE
C                       VALUE OF NSLAB RETURNED.  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED.  (INTERNAL) 
C              NSLABV = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE V WIND.  WHEN IER NE 0,
C                       THIS VALUE SHOULD NOT BE USED.  (INTERNAL) 
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT
C                       THE RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ).  (INTERNAL)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FDU( ) (J=1,4).  (INTERNAL)
C               MD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FDV( ) (J=1,4).  (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       ZERO  WHEN DATA ARE NOT PACKED.  (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       ZERO WHEN DATA ARE NOT PACKED.  (INTERNAL)
C                MAPP = MAP PROJECTION
C                       3 = LAMBERT,
C                       5 = POLAR STEREOGRAPHIC
C                       7 = MERCATOR
C                       (INTERNAL)
C              XMESHL = MESH LENGTH IN KM AT LATITUDE XLAT.  (INTERNAL)
C              ORIENT = VERTICAL LONGITUDE IN DEGREES WEST.  (INTERNAL)
C                XLAT = LATITUDE AT WHICH MESH LENGTH IS TRUE.  ALSO
C                       THE LATITUDE OF TANGENCY FOR LAMBERT PROJECTION.
C                       (INTERNAL)
C              XLATLL = LATITUDE OF LOWER LEFT CORNER OF GRID IN 
C                       NORTH.  (INTERNAL)
C              XLONLL = LONGITUDE OF LOWER LEFT CORNER OF GRID IN
C                       DEGREES WEST.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH, PSLLIJ, LMLLIJ, EOUWND, EOVWND
C
      DIMENSION IDPARS(15),JD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),WIND(ND5)
      DIMENSION FDU(ND2X3),FDV(ND2X3)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION NGRIDC(6,ND11)
      DIMENSION LD(4),MD(4)
C
      IER=0
      ISTAV=0
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C
      IF(IDPARS(1).EQ.004.AND.(IDPARS(2).EQ.010.OR.
     1                         IDPARS(2).EQ.110.OR.
     2                         IDPARS(2).EQ.011.OR.
     3                         IDPARS(2).EQ.111.OR.
     4                         IDPARS(2).EQ.066.OR.
     5                         IDPARS(2).EQ.166.OR.
     6                         IDPARS(2).EQ.061.OR.
     7                         IDPARS(2).EQ.161))GO TO 105
      WRITE(KFILDO,101)(JD(J),J=1,4)
 101  FORMAT(/,'****IDPARS(1) AND IDPARS(2) DO NOT INDICATE',
     1        ' EARTH-ORIENTED WIND',/,
     2        '     PREDICTOR ',I9.9,1X,I9.9,1X,I9.9,I3,
     3        ' NOT COMPUTED IN EOWND.')
      IER=103
      GO TO 800
C
C        FORM THE IDS FOR THE WINDS TO FETCH.
C
 105  IF(IDPARS(2).EQ.011.OR.IDPARS(2).EQ.061.OR.
     1   IDPARS(2).EQ.111.OR.IDPARS(2).EQ.161)THEN
C           THIS IS FOR CONSTANT HEIGHT LEVEL.
         LD(1)=004020*1000+IDPARS(4)
         MD(1)=004120*1000+IDPARS(4)
C
      ELSEIF(IDPARS(2).EQ.010.OR.IDPARS(2).EQ.110)THEN
C           THIS IS FOR CONSTANT PRESSURE LEVEL.
         LD(1)=004000*1000+IDPARS(4)
         MD(1)=004100*1000+IDPARS(4)
C
      ELSEIF(IDPARS(2).EQ.066.OR.IDPARS(2).EQ.166)THEN
C           THIS IS FOR CONSTANT SIGMA LEVEL.  THERE MAY BE
C           NO DATA AVAILABLE.
         LD(1)=004016*1000+IDPARS(4)
         MD(1)=004116*1000+IDPARS(4)
C
      ELSE
C           THIS SHOULD NOT OCCUR BECAUSE OF TEST AT 101.
         WRITE(KFILDO,101)
         GO TO 800
      ENDIF
C
      LD(2)=IDPARS(7)
      MD(2)=IDPARS(7)
C        WORD 2 ACCOMMODATES THE SINGLE LEVEL.
C
      IF(IDPARS(10).LE.4)THEN
         LD(3)=IDPARS(9)*1000000+IDPARS(12)
         MD(3)=IDPARS(9)*1000000+IDPARS(12)
      ELSE
         LD(3)=IDPARS(12)
         MD(3)=IDPARS(12)
C           WORD 3 ACCOMMODATES RR AND TAU UNLESS THE TIME OPERATOR
C           IDPARS(10) IS GT 4, IN WHICH CASE IT IS ONLY TAU.
C           (THIS WAS IN ORIGINAL CODE FOR SUBROUTINE TIMEP; NOT
C           RECHECKED AS OF THIS DATE 6/22/03.)
      ENDIF
C
      LD(4)=0
      MD(4)=0
C
C        GET THE U WINDS.  THE ID INCLUDES THE RUN OFFSET TIME.
C
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDU,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLABU,MISSP,MISSS,L3264B,1,IER)

      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GO TO 800
C        IF IER.NE 0, A DIAGNOSTIC WILL BE PRINTED BY GFETCH.
      NX=IS2(3)
      NY=IS2(4)
C
C        GET THE V WINDS.  THE ID INCLUDES THE RUN OFFSET TIME.
C
      CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDV,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLABV,MISSP,MISSS,L3264B,1,IER)

      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GO TO 800
      IF(NSLABU.EQ.NSLABV.AND.NX.EQ.IS2(3).AND.NY.EQ.IS2(4))GO TO 200
C
C        THE GRID CHARACTERISTICS ARE NOT THE SAME.
C
      WRITE(KFILDO,150)(LD(J),J=1,4),(NGRIDC(J,NSLABU),J=1,6),NX,NY,
     1                 (MD(J),J=1,4),(NGRIDC(J,NSLABV),J=1,6),
     2                 IS2(3),IS2(4)
 150  FORMAT(' ****THE TWO GRIDS NEEDED IN EOWND HAVE DIFFERENT',
     1       ' CHARACTERISTICS.  PREDICTOR NOT COMPUTED.',
     2       '  VALUES FROM NGRIDC( , ) AND NX, NY.',
     3       (/5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
      IER=100
      GO TO 800
C
 200  NSLAB=NSLABU
      MAPP=NGRIDC(1,NSLAB)
      XMESHL=NGRIDC(2,NSLAB)/1000. 
      XLAT=NGRIDC(3,NSLAB)/10000.
      ORIENT=NGRIDC(4,NSLAB)/10000.
      XLATLL=NGRIDC(5,NSLAB)/10000.
      XLONLL=NGRIDC(6,NSLAB)/10000.
C
C        GET THE POLE POSITION XNP, YNP.
C 
      IF(MAPP.EQ.3)THEN
         CALL LMLLIJ(KFILDO,90.,0.,XMESHL,ORIENT,XLAT,
     1               XLATLL,XLONLL,XNP,YNP)
      ELSEIF(MAPP.EQ.5)THEN
         CALL PSLLIJ(KFILDO,90.,0.,XMESHL,ORIENT,XLAT,
     1               XLATLL,XLONLL,XNP,YNP)
      ELSEIF(MAPP.NE.7)THEN
         WRITE(KFILDO,212)MAPP,(JD(J),J=1,4)
 212     FORMAT(/,' ****MAP PROJECTION NUMBER =',I3,' NOT EXPECTED.',
     1           '     PREDICTOR ',I9.9,1X,I9.9,1X,I9.9,I3,
     2           ' NOT COMPUTED IN EOWND.')
         IER=60
         GO TO 800
      ENDIF
C
C        COMPUTE THE EARTH-ORIENTED WIND.  NOTE THAT THE SAME 
C        ALGORITHM WORKS FOR POLAR STEREOGRAPHIC AND LAMBERT, 
C        PROVIDED THE POLE POSITION IS FOUND FOR THE PROJECTION
C        MAPP.
C
      IF(IDPARS(2).EQ.010.OR.IDPARS(2).EQ.011.OR.
     1   IDPARS(2).EQ.061.OR.IDPARS(2).EQ.066)THEN
         CALL EOUWND(KFILDO,WIND,FDU,FDV,NX,NY,XNP,YNP,MAPP)
      ELSEIF(IDPARS(2).EQ.110.OR.IDPARS(2).EQ.111.OR.
     1       IDPARS(2).EQ.161.OR.IDPARS(2).EQ.166)THEN
         CALL EOVWND(KFILDO,WIND,FDU,FDV,NX,NY,XNP,YNP,MAPP)
      ELSE
         GO TO 800
C           THE ID'S WERE ALREADY TESTED.  THIS GO TO 
C           SHOULD NOT OCCUR, BUT IS INCLUDED FOR SAFETY.
      ENDIF
C
C        CONVERT TO KTS IF NECESSARY.  CONVERSION IS 1.9425 KTS
C        PER M/S AS PER MOS-2000 NOTEBOOK, CHAPTER 3.
C
      IF(IDPARS(2).EQ.061.OR.IDPARS(2).EQ.161)THEN
C
         DO 250 J=1,NX*NY
            WIND(J)=WIND(J)*1.9425
 250     CONTINUE
C
      ENDIF
C
      GO TO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C
 800  DO 801 J=1,ND2X3
         WIND(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END      
