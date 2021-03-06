      SUBROUTINE VORTH(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                 NGRIDC,ND11,NSLAB,IPACK,IWORK,VORT,ND5,
     2                 LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                 IS0,IS1,IS2,IS4,ND7,
     4                 FDSIN,FDM,FDH,FDSINS,FDMS,ND2X3,
     5                 ISTAV,L3264B,MISTOT,IER)
C
C        JANUARY  1995   GLAHN   TDL   MOS-2000 
C        AUGUST   1996   GLAHN   ADDED MISSS, MISTOT, ACCURACY TO XMESHL
C        JUNE     1997   GLAHN   SLIGHT CHANGE TO COMMENTS, NOT CODE
C                                COMPILE OPTION COMMENTED OUT
C        OCTOBER  1998   GLAHN   REMOVED TEST AFTER CALL TO GFETCH
C        DECEMBER 2002   WEISS   CHANGED ND5 TO ND2X3 
C        MAY      2003   GLAHN   CHANGED ND2X3 TO ND5 IN IPACK( ),
C                                IWORK( ), DATA( ); REARRANGED DIMENSION
C                                STATEMENTS
C
C        PURPOSE 
C            TO COMPUTE RELATIVE GEOSTROPHIC VORTICITY FROM HEIGHTS
C            IN THE NORTHERN HEMISPHERE.  UNITS ARE /SEC * 100000.
C            THE LEVEL IS CONTAINED IN JD(2).  RUN TIME
C            OFFSET RR, TAU, AND OH ARE ACCOMMODATED.  SINCE
C            THE ACTUAL GRID DIMENSIONS NX AND NY ARE NOT KNOWN UNTIL
C            THE DATA ARE FETCHED, THE SPATIAL GRID COMPUTATIONS ARE
C            MADE IN SUBROUTINES MAPLAT AND VORTH1.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               006 020 - GEOSTROPHIC VORTICITY FROM HEIGHTS
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
C                       L=2--GRID LENGTH IN MILLIMETERS,
C                       L=3--LATITUDE AT WHICH GRID LENGTH IS CORRECT *10000,
C                       L=4--GRID ORIENTATION IN DEGREES *10000,
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *10000,
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES *10000.
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN BE
C                       DEALT WITH ON THIS RUN.  LAST DIMENSION OF
C                       NGRIDC( , ).  (INPUT)
C               NSLAB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE HEIGHTS, WHEN THE HEIGHTS
C                       HAVE BEEN CORRECTLY RETURNED.  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED.  (OUTPUT) 
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             VORT(K) = RELATIVE VORTICITY * 100000 IN UNITS /SEC
C                       CALCULATED FROM HEIGHTS (J=1,ND5).
C                       ALTHOUGH THIS IS A LINEAR ARRAY, IT IS TREATED
C                       AS A 2-DIMENSIONAL ARRAY IN MAPLAT AND VORTH1, AND
C                       THE VORTICITY IS RETURNED AS A GRID NX BY NY.  
C                       (OUTPUT)
C                 ND5 = DIMENSION OF IPACK( ),IWORK( ), AND VORT( ).
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
C            FDSIN(K) = WORK ARRAY TO HOLD THE SIN OF THE LATITUDE
C                       (K=1,ND2X3).  (INTERNAL)
C              FDM(K) = WORK ARRAY TO HOLD THE MAP FACTOR
C                       (K=1,ND2X3).  (INTERNAL)
C              FDH(K) = WORK ARRAY TO HOLD THE HEIGHTS IN M 
C                       (K=1,ND2X3).  (INTERNAL)
C       FDSINS(IX,JY) = USED TO SAVE THE SIN OF THE LATITUDE IN SUBROUTINE
C                       MAPLAT (IX=1,NX) (JY=1,NY).  THE USER MUST NOT
C                       USE THIS ARRAY EXCEPT IN CALLING MAPLAT.
C                       (INPUT/OUTPUT)
C         FDMS(IX,JY) = USED TO SAVE THE MAP FACTOR IN SUBROUTINE
C                       MAPLAT (IX=1,NX) (JY=1,NY).  THE USER MUST NOT
C                       USE THIS ARRAY EXCEPT IN CALLING MAPLAT.
C                       (INPUT/OUTPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF THE
C                       GRID IS NOT KNOWN BEFORE THE HEIGHTS ARE
C                       FETCHED.  (INPUT)
C               ISTAV = 0 SINCE THE DATA RETURNED ARE GRID DATA.  (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING USED
C                       (EITHER 32 OR 64).  (INPUT)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        60 = MAP PROJECTION NUMBER NOT EXPECTED.
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE 
C                             VORTICITY FROM HEIGHTS.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C                  NX = THE DIMENSION OF THE GRID IN THE IX DIRECTION.
C                       (INTERNAL).
C                  NY = THE DIMENSION OF THE GRID IN THE JY DIRECTION.
C                       (INTERNAL).
C               NSLAB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE HEIGHTS.  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED.  (INTERNAL) 
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT THE 
C                       RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ).  (INTERNAL)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FDU( ) (J=1,4).  (INTERNAL)
C              XMESHL = GRID LENGTH IN M AT THE LATITUDE XLAT.  (INTERNAL)
C               TITLE = BLANK TITLE FOR OPTIONAL MAPS.  (CHARACTER*74)
C                       (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED.  (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH, MAPLAT, VORTH1
C
D     CHARACTER*74 TITLE/' '/
C
      DIMENSION IDPARS(15),JD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),VORT(ND5)
      DIMENSION FDSIN(ND2X3),FDM(ND2X3),FDH(ND2X3),
     1          FDSINS(ND2X3),FDMS(ND2X3)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION NGRIDC(6,ND11)
      DIMENSION LD(4)
C
C***D     CALL TIMPR(KFILDO,KFILDO,'START VORTH         ')
C
      IER=0
      ISTAV=0
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C
      IF(IDPARS(1).EQ.006.AND.IDPARS(2).EQ.020)GO TO 105
      WRITE(KFILDO,101)(JD(J),J=1,4)
 101  FORMAT(/'****IDPARS(1) AND IDPARS(2) DO NOT INDICATE VORTICITY.'/
     2        '     PREDICTOR ',I9.9,1X,I9.9,1X,I9.9,I3,
     3        ' NOT COMPUTED IN VORTH.')
      IER=103
      GO TO 800
C
C        GET THE HEIGHTS.  THE ID INCLUDES THE RUN OFFSET TIME.
C
 105  LD(1)=001000*1000+IDPARS(4)
      LD(2)=JD(2)
      LD(3)=IDPARS(9)*1000000+IDPARS(12)+IDPARS(11)
      IF(IDPARS(10).GT.4)LD(3)=IDPARS(12)
C        THE USE OF IDPARS(10) AND IDPARS(11) IN THE ABOVE 2 
C        STATEMENTS IS NECESSARY WHEN COMPUTING VORTICITY
C        FOR SUBROUTINE TIMEP.  ONLY WHEN TIMEP IS USED WILL
C        IDPARS(10) AND IDPARS(11) NOT EQUAL ZERO.  THIS
C        ACCOMMODATES RUN OFFSET RR, TAU, AND OH.
      LD(4)=0
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDH,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GO TO 800
      NX=IS2(3)
      NY=IS2(4)
C
C       CALCULATE SINE OF LATITUDE AND MAP FACTOR.
C
      CALL MAPLAT(KFILDO,JD,NGRIDC(1,NSLAB),
     1            FDSIN,FDM,FDSINS,FDMS,NX,NY,IER)
      IF(IER.NE.0)GO TO 800
C        IF IER NE O, A DIAGNOSTIC WILL HAVE BEEN PRINTED IN
C        MAPLAT.
C***D     CALL PRTGR(KFILDO,FDSIN,NX,NY,.1,0.,
C***D    1           100.,0.,0,TITLE,IER)                
C***D     CALL PRTGR(KFILDO,FDM,NX,NY,.1,0.,
C***D    1           100.,0.,0,TITLE,IER)                
C***D     CALL PRTGR(KFILDO,FDH,NX,NY,60.,0.,
C***D    1           1.,0.,0,TITLE,IER)                
C
C        COMPUTE THE VORTICITY.  BEWARE OF THE EQUATOR AND
C        SOUTHERN HEMISPHERE.  VORTH1 RETURNS UNITS /SEC * 100000.
C
      XMESHL=NGRIDC(2,NSLAB)/1000.
      CALL VORTH1(KFILDO,XMESHL,VORT,FDSIN,FDM,FDH,NX,NY)
      GO TO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C
 800  DO 801 J=1,ND2X3
      VORT(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END      
