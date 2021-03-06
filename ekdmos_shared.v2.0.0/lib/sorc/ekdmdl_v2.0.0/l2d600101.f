      SUBROUTINE L2D600101(KFILDO,KFIL10,NFIRST,
     1                    ID,IDPARS,THRESH,JD,NDATE,
     2                    KFILRA,RACESS,NUMRA,
     3                    ICALL,CCALL,ICALLD,CCALLD,NAME,
     4                    NELEV,STALAT,STALON,ITIMEZ,
     5                    ISDATA,SDATA,SDATA1,L1DATA,DIR,ND1,NSTA,
     6                    NGRIDC,NGRID,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     7                    LSTORE,ND9,LITEMS,CORE,ND10,LASTL,
     8                    NBLOCK,LASTD,NSTORE,NFETCH,
     9                    IS0,IS1,IS2,IS4,ND7,
     A                    FD1,FD2,FD3,FD4,FD5,FD6,FD7,
     B                    FDVERT,FDTIME,FDSINS,FDMS,ND2X3,IP12,IP16,
     C                    ISTAV,L3264B,L3264W,MISTOT,IER)
C
C        JUNE      1995   GLAHN   TDL   MOS-2000
C        AUGUST    1996   GLAHN   ADDED MISTOT
C        SEPTEMBER 1996   GLAHN   ADDED 2-DIGIT CYCLE TIME, NDEF
C        OCTOBER   1996   GLAHN   CHANGED ID FROM 400101 TO 600101
C        NOVEMBER  1996   GLAHN   ADDED KFILRA, RACESS, NUMRA, ICALLD
C        NOVEMBER  1996   GLAHN   CHANGED DEFINITION OF SEASONS
C        JANUARY   1998   GLAHN   ADDED SUBSTITUTE STATIONS IN CCALL( , )
C        MAY       1998   GLAHN   ADDED ITIMEZ( ) 
C        NOVEMBER  1998   GLAHN   CHANGED DIMENSIONS OF CCALLD( )
C                                 AND ICALLD( , ) FROM ND1 TO ND5
C        AUGUST    1999   GLAHN   ADDED IP12 TO CALL, CALL TO L2D1
C        JUNE      2003   GLAHN   ADDED NAME AND NGRID TO CALL AND TO
C                                 CALLS TO L2D1
C 
C        PURPOSE 
C            TO TRANSFORM A PAIR OF PREDICTORS INTO CCCFFF = 600101
C            THROUGH A SET OF THRESHOLDS.  THE FILE NAME HOLDING THE
C            THRESHOLDS, ETC. AND THE INDEX VARIABLE NAME STORED IN
C            THE MOS-2000 STORAGE SYSTEM HAS THE ELEMENTS CCC, FFF,
C            SEASON NUMBER, PROJECTION NUMBER, AND RUN NUMBER.
C            CCC IS IN IDPARS(1) AND FFF IS IN IDPARS(2).  SINCE
C            TWO VARIABLES ARE INVOLVED, THE INFORMATION IN ID(2),
C            ID(3), AND ID(4) IS NOT SUFFICIENT TO DEFINE THEM.  HOWEVER,
C            BY FULLY DEFINING THE VARIABLES TO LINEARIZE IN MD1( )
C            AND MD2( ), CORRESPONDING TO ID( ), MDPRS1( ) AND 
C            MDPRS2( ), CORRESPONDING TO IDPARS( ), AND LD1( ) 
C            AND LD2( ), CORRESPONDING TO JD( ), IN THIS 
C            ROUTINE MAKES IT POSSIBLE FOR THE CALLED ROUTINES
C            TO BE GENERIC FOR ALL 2-D LINEARIZATIONS.
C
C            THE SEASON IS COMPUTED AS:
C              17 = SUMMER, 0401-0930,
C              18 = WINTER.
C            THE PROJECTION NUMBER IS COMPUTED AS:
C               0 = PROJECTIONS OF 0-5, 24-29, ETC.,
C               1 = PROJECTIONS OF 6-11, 30-35, ETC.,
C               2 = PROJECTIONS OF 12-17, 36-41, ETC.,
C               3 = PROJECTIONS OF 18-23, 42-47, ETC.
C            THE RUN NUMBER IS COMPUTED AS:
C               0 = HOURS 00-11,
C               2 = HOURS 12-23.
C            THE CYCLE IS MOD(IDATE,100)
C
C        DATA SET USE 
C            KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C              NFIRST = 1 FOR THE 1ST DATE.
C               ID(J) = THE PREDICTOR ID (J=1,4).  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID( ) (J=1,15).  (INPUT)
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
C              THRESH = THE BINARY THRESHOLD ASSOCIATED WITH IDPARS( ).
C                       (INPUT)
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE PORTIONS
C                       PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       JD(J) (J=1,3), TOGETHER WITH IDPARS(4) AND THE CCCFFF
C                       FOR THE FIELD TO LINEARIZE THAT IS SPECIFIC TO THE
C                       CCCFFF USED TO IDENTIFY THE LINEARIZED PREDICTOR, 
C                       DEFINE THE VARIABLE TO LINEARIZE.  THIS IS, THEN, THE
C                       VARIABLE TO READ FROM THE ARCHIVE.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.  (INPUT)
C           KFILRA(J) = HOLDS THE UNIT NUMBERS FOR ACCESSING THE MOS-2000
C                       EXTERNAL RANDOM ACCESS FILES (J=1,NUMRA).  (INPUT)
C           RACESS(J) = THE FILE NAMES CORRESPONDING TO KFILRA(J) (J=1,NUMRA).
C                       (CHARACTER*60)  (INPUT)
C               NUMRA = THE NUMBER OF UNIT NUMBERS AND NAMES IN KFILRA( )
C                       AND RACESS( ).  (INPUT)
C        ICALL(L,K,J) = 8 STATION CALL LETTERS AS CHARACTERS IN AN INTEGER
C                       VARIABLE (L=1,L3264W) (K=1,NSTA) (J=1,6).
C                       EQUIVALENCED TO CCALL( , )  (INPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST, EXCEPT POSSIBLY 
C                       CCALLD( ).  EQUIVALENCED TO ICALL( , , ).
C                       (CHARACTER*8)  (INPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN INTEGER
C                       VARIABLE (L=1,L3264W) (K=1,ND5).
C                       EQUIVALENCED TO CCALLD( ).  (INTERNAL)
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).  THIS LIST IS USED 
C                       IN L2D1 TO READ THE REGION LISTS.  (INTERNAL)
C             NAME(K) = NAMES OF STATIONS (K=1,NSTA).  USED FOR PRINTOUT
C                       ONLY.  (CHARACTER*20)  (INPUT)
C            NELEV(K) = ELEVATION OF STATIONS (K=1,NSTA).  (INTPUT)
C           STALAT(K) = LATITUDE OF STATIONS (K=1,NSTA).  (INPUT)
C           STALON(K) = LONGITUDE OF STATIONS (K=1,NSTA).  (INPUT)
C           ITIMEZ(K) = TIME ZONE INDICATOR.  THE NUMBER OF HOURS
C                       THE STATION IS DIFFERENT FROM UTC (K=1,NSTA).
C                       (INPUT)
C           ISDATA(K) = WORK ARRAY (K=1,ND1).  (INTERNAL)
C            SDATA(K) = LINEARIZED VARIABLE TO RETURN (K=1,NSTA).  (OUTPUT)
C           SDATA1(K) = WORK ARRAY RESERVED FOR USE IN L2D2 (K=1,NSTA).
C                       (INTERNAL)
C           L1DATA(K) = THE ARRAY RESERVED FOR USE BY LINEARIZATION
C                       ROUTINES (K=1,NSTA). 
C          DIR(K,J,M) = THE IX (J=1) AND JY (J=2) POSITIONS ON THE GRID
C                       FOR THE COMBINATION OF GRID CHARACTERISTICS M
C                       (M=1,NGRID) AND STATION K (K=1,NSTA) IN NGRIDC( ,M).
C                       (INPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       DIMENSION OF SEVERAL VARIABLES.  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT WITH.
C                       (INPUT)
C         NGRIDC(L,M) = HOLDS THE GRID CHARACTERISTICS (L=1,6) FOR EACH GRID
C                       COMBINATION (M=1,NGRID).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=POLAR
C                            STEREOGRAPHIC). 
C                       L=2--GRID LENGTH IN MILLIMETERS,
C                       L=3--LATITUDE AT WHICH GRID LENGTH IS CORRECT *10000,
C                       L=4--GRID ORIENTATION IN DEGREES *10000,
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *10000,
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES *10000.
C               NGRID = THE NUMBER OF GRID COMBINATIONS IN DIR( , , ),
C                       MAXIMUM OF ND11.  (INPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN BE
C                       DEALT WITH ON THIS RUN.  LAST DIMENSION OF
C                       NGRIDC( , ) AND DIR( , , ).  (INPUT)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND
C                       IN NGRIDC( , ) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  SEE LSTORE(10, ).  FOR THE
C                       COMPUTATION ROUTINES RETURNING A GRID, THIS
C                       VALUE MUST BE OUTPUT BY GFETCH.  (OUTPUT) 
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND DATA( ).
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
C                       HAVE BEEN USED IN THIS RUN.
C             CORE(J) = THE ARRAY TO STORE OR RETIREVE THE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL
C                       DATA ARE STORED ON DISK.  (OUTPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C               LASTL = THE LAST LOCATION IN CORE( ) USED FOR MOS-2000 INTERNAL
C                       STORAGE.  INITIALIZED TO 0 ON FIRST ENTRY TO GSTORE.
C                       ALSO INITIALIZED IN U201 IN CASE GSTORE IS NOT ENTERED.
C                       (INPUT-OUTPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C               LASTD = TOTAL NUMBER OF PHYSICAL RECORDS ON DISK FOR MOS-2000
C                       INTERNAL STORAGE.  (INPUT)
C              NSTORE = THE NUMBER OF TIMES GSTORE HAS BEEN ENTERED.  GSTORE
C                       KEEPS TRACK OF THIS AND RETURNS THE VALUE.  (OUTPUT)
C              NFETCH = THE NUMBER OF TIMES GFETCH HAS BEEN ENTERED.  GFETCH
C                       KEEPS TRACK OF THIS AND RETURNS THE VALUE.  (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C  FD1(J),FD2(J), ETC = WORK ARRAYS (J=1,ND2X3).  THESE MAY BE USED IN
C                       ROUTINES AS 2-DIMENSIONAL ARRAYS, WHERE THE
C                       TOTAL ARRAY SIZE IS ND2*ND3=ND2X3 AS DECLARED IN
C                       THE CALLING PROGRAM.  (INTERNAL)
C           FDVERT(J) = TEMPORARY STORAGE RESERVED FOR SUBROUTINE VERTP
C                       (J=1,ND2X3).  (INTERNAL)
C           FDTIME(J) = TEMPORARY STORAGE RESERVED FOR SUBROUTINE TEMEP
C                       (J=1,ND2X3).  (INTERNAL)
C       FDSINS(IX,JY) = USED TO SAVE THE SIN OF THE LATITUDE IN SUBROUTINE
C                       PSMAPF (IX=1,NX) (JY=1,NY).  THE USER MUST NOT
C                       USE THIS ARRAY EXCEPT IN CALLING PSMAPF.
C                       (INPUT/OUTPUT)
C         FDMS(IX,JY) = USED TO SAVE THE MAP FACTOR IN SUBROUTINE
C                       PSMAPF (IX=1,NX) (JY=1,NY).  THE USER MUST NOT
C                       USE THIS ARRAY EXCEPT IN CALLING PSMAPF.
C                       (INPUT/OUTPUT)
C               ND2X3 = DIMENSION OF FD1( ), FD2( ), ETC.  (INPUT)
C                IP12 = INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE INPUT VECTOR FILES WILL BE WRITTEN 
C                       TO UNIT IP12.  (INPUT)
C                IP16 = INDICATES WHETHER (>0) OR NOT (=0) CERTAIN DIAGNOSTICS
C                       WILL BE WRITTEN TO UNIT IP16 FOR LINEARIZATION
C                       ROUTINES (E.G., STATIONS IN THRESHOLD LISTS THAT
C                       ARE NOT BEING DEALT WITH IN THIS RUN).
C               ISTAV = 1 WHEN THE DATA RETURNED ARE STATION DATA.  
C                       0 WHEN THE DATA ARE NOT AVAILABLE FOR RETURN.
C                       (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING USED
C                       (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS, EITHER 1 OR 2.  (INPUT)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C                 IER = STATUS RETURN.
C                        0 = GOOD RETURN.
C                       SEE CALLED ROUTINES FOR OTHER VALUES.
C                       (OUTPUT)
C                MMDD = MONTH AND DAY OF NDATE.  (INTERNAL)
C              ICCFF1 = CCCFFF OF THE FIRST OF THE VARIABLE PAIR TO 
C                       LINEARIZE.  IT IS DEFINED IN THIS ROUTINE AS
C                       GEOPOTENTIAL HEIGHT.  (INTERNAL)
C              ICCFF2 = CCCFFF OF THE SECOND OF THE VARIABLE PAIR TO 
C                       LINEARIZE.  IT IS DEFINED IN THIS ROUTINE AS
C                       GEOSTROPHIC VORTICITY.  (INTERNAL)
C                 IXX = THE SEASON NUMBER.  (INTERNAL)
C                 IYY = THE PROJECTION NUMBER.  (INTERNAL)
C                 ICC = THE CYCLE TIME.  (INTERNAL)
C              MD1(J) = THE 4-WORD ID OF THE FIRST OF THE VARIABLE PAIR 
C                       TO TRANSFORM (J=1,4).  CORRESPONDS TO ID( ).
C                       (INTERNAL)
C           MDPRS1(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE ID
C                       OF THE FIRST OF THE PREDICTOR PAIR TO TRANSFORM
C                       CORRESPONDING TO MD1( ) (J=1,15).  MDPRS1( )
C                       BEARS THE SAME RELATIONSHIP TO MD1( ) THAT IDPARS( )
C                       DOES TO ID( ).  (INTERNAL)
C              LD1(J) = THE 4-WORD ID OF THE FIRST OF THE VARIABLE PAIR
C                       TO TRANSFORM, SANS PROCESSING INDICATORS (J=1,4).
C                       CORRESPONDS TO JD( ).  (INTERNAL)
C              MD2(J) = THE 4-WORD ID OF THE SECOND OF THE VARIABLE PAIR 
C                       TO TRANSFORM (J=1,4).  CORRESPONDS TO ID( ).
C                       (INTERNAL)
C           MDPRS2(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE ID
C                       OF THE SECOND OF THE PREDICTOR PAIR TO TRANSFORM
C                       CORRESPONDING TO MD2( ) (J=1,15).  MDPRS2( )
C                       BEARS THE SAME RELATIONSHIP TO MD2( ) THAT IDPARS( )
C                       DOES TO ID( ).  (INTERNAL)
C              LD2(J) = THE 4-WORD ID OF THE FIRST OF THE VARIABLE PAIR
C                       TO TRANSFORM, SANS PROCESSING INDICATORS (J=1,4).
C                       CORRESPONDS TO JD( ).  (INTERNAL)
C       THRES1(M,N,J) = THE THRESHOLD VALUES FOR CATEGORY M (M=1,NTHRS1),
C                       REGION N (N=1,NREG), AND THRESHOLD COMBINATION J
C                       (J=1,L2DCMB) FOR THE FIRST OF THE VARIABLE PAIR.
C                       ARRAY MUST BE ESTABLISHED BY THE USER, AND SAVED.
C                       (INTERNAL)
C       THRES2(M,N,J) = THE THRESHOLD VALUES FOR CATEGORY M (M=1,NTHRS2),
C                       REGION N (N=1,NREG), AND THRESHOLD COMBINATION J
C                       (J=1,L2DCMB) FOR THE SECOND OF THE VARIABLE PAIR.
C                       ARRAY MUST BE ESTABLISHED BY THE USER, AND SAVED.
C                       (INTERNAL)
C       PROB(L,M,N,J) = THE PROBABILITY VALUES FOR CATEGORY L OF THE FIRST
C                       OF THE VARIABLE PAIR (L=1,NTHRS1), CATEGORY M OF
C                       THE SECOND OF THE VARIABLE PAIR (M=1,NTHRS2),
C                       REGION N (N=1,NREG), AND THRESHOLD COMBINATION J
C                       (J=1,L2DCMB).  ARRAY MUST BE ESTABLISHED BY THE USER,
C                       AND SAVED.  (INTERNAL)
C              NTHRS1 = THE MAXIMUM NUMBER OF THRESHOLDS THAT CAN BE 
C                       USED FOR THE FIRST OF THE VARIABLE PAIR FOR
C                       THE TRANSFORMATION DEFINED BY THIS
C                       ROUTINE.  SET BY PARAMETER.  (INTERNAL)
C              NTHRS2 = THE MAXIMUM NUMBER OF THRESHOLDS THAT CAN BE 
C                       USED FOR THE SECOND OF THE VARIABLE PAIR FOR
C                       THE TRANSFORMATION DEFINED BY THIS
C                       ROUTINE.  SET BY PARAMETER.  (INTERNAL)
C                NREG = THE MAXIMUM NUMBER OF REGIONS THAT CAN BE
C                       USED FOR THE TRANSFORMATION DEFINED BY THIS
C                       ROUTINE.  SET BY PARAMETER.  (INTERNAL)
C              L2DCMB = THE NUMBER OF LINEARIZATION THRESHOLDS AND
C                       PROBABILITY COMBINATIONS THAT CAN BE ACCOMMODATED
C                       FOR THIS PARTICULAR CCCFFF.  THE COMBINATIONS
C                       CAN VARY WITH SEASON NUMBER (IXX), PROJECTION
C                       NUMBER (IYY), CYCLE (ICC), AND DATA SOURCE (DD).
C                       (INTERNAL)
C           IDEF(J,L) = THE DEFINITION OF THE THRESHOLD/PROBABILITY
C                       COMBINATIONS FOR THIS CCCFFF (J=1,4) (L=1,L2DCMB).
C                       THE DEFINITION OF THE 4 VALUES OF J:
C                       J = 1--DATA SOURCE DD,
C                       J = 2--SEASON NUMBER IXX,
C                       J = 3--PROJECTION NUMBER IYY, AND
C                       J = 4--CYCLE TIME ICC.
C                       NOTE THAT IT IS NOT NECESSARY TO CARRY CCCFFF,
C                       BECAUSE THIS PARTICULAR ROUTINE IS UNIQUELY
C                       IDENTIFIED WITH CCCFFF.  THESE 4 VALUES, ALONG
C                       WITH THE CCCFFF, ARE IN THE THRESHOLD FILE NAME
C                       AND ALSO FOR SAFETY WITHIN THE FIRST RECORD OF
C                       THE FILE.  NOTE THAT THIS ARRAY IS SPECIFIC
C                       TO THIS ROUTINE, BUT THE ARRAY IS FILLED BY L2D1.
C                       (INTERNAL)
C                NDEF = THE NUMBER OF THE THRESHOLD/PROBABILITY COMBINATION
C                       TO BE USED NEXT.  WHEN THIS NUMBER OF COMBINATIONS
C                       IS ABOUT TO BE STORED, IT IS CHECKED IN L2D1 FOR 
C                       ITS MAX VALUE OF L21DCMB.  (INTERNAL)
C              NTHRG1 = NTHRS1*NREG*L2DCMB.  SET BY PARAMETER SO THAT IT
C                       CAN BE USED IN THE DATA STATEMENTS TO INITIALIZE
C                       AND SAVE THRES1( , ).  (INTERNAL)
C              NTHRG2 = NTHRS2*NREG*L2DCMB.  SET BY PARAMETER SO THAT IT
C                       CAN BE USED IN THE DATA STATEMENTS TO INITIALIZE
C                       AND SAVE THRES2( , ).  (INTERNAL)
C              NTHRGP = NTHRS1*NTHRS2*NREG*L2DCMB*.  SET BY PARAMETER SO THAT IT
C                       CAN BE USED IN THE DATA STATEMENTS TO INITIALIZE
C                       AND SAVE THRES1( , ) AND THRES2( , ).  (INTERNAL)
C           TEMPT1(J) = TEMPORARY ARRAY USED IN L2D1 (J=1,NTHRS1).
C                       (INTERNAL)
C           TEMPT2(J) = TEMPORARY ARRAY USED IN L2D1 (J=1,NTHRS2).
C                       (INTERNAL)
C          TEMPP(J,L) = TEMPORARY ARRAY USED IN L2D1 (J=1,NTHRS1)
C                       (L=1,NTHRS2).  (INTERNAL)
C                         
C        NON SYSTEM SUBROUTINES CALLED 
C            L2D1, BASICP
C
      PARAMETER (NREG=4)
      PARAMETER (NTHRS1=10)
      PARAMETER (NTHRS2=9)
      PARAMETER (L2DCMB=4)
      PARAMETER (NTHRG1=NTHRS1*NREG*L2DCMB)
      PARAMETER (NTHRG2=NTHRS2*NREG*L2DCMB)
      PARAMETER (NTHRGP=NTHRS1*NTHRS2*NREG*L2DCMB)
      PARAMETER (L2DCM4=L2DCMB*4)
C
      CHARACTER*8 CCALL(ND1,6),
     1            CCALLD(ND5)
      CHARACTER*20 NAME(ND1)
      CHARACTER*60 RACESS(NUMRA)
C
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION ICALL(L3264W,ND1,6),
     1          NELEV(ND1),STALAT(ND1),STALON(ND1),ITIMEZ(ND1),
     2          ISDATA(ND1),SDATA(ND1),SDATA1(ND1),L1DATA(ND1)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5),ICALLD(L3264W,ND5)
      DIMENSION FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),
     1          FD5(ND2X3),FD6(ND2X3),FD7(ND2X3),
     2          FDVERT(ND2X3),FDTIME(ND2X3),FDSINS(ND2X3),FDMS(ND2X3)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION DIR(ND1,2,ND11),NGRIDC(6,ND11)
      DIMENSION KFILRA(NUMRA)
      DIMENSION MD1(4),MDPRS1(15),LD1(4),MD2(4),MDPRS2(15),LD2(4)
      DIMENSION THRES1(NTHRS1,NREG,L2DCMB),
     1          THRES2(NTHRS2,NREG,L2DCMB),
     2          PROB(NTHRS1,NTHRS2,NREG,L2DCMB)
      DIMENSION TEMPT1(NTHRS1),
     1          TEMPT2(NTHRS2),
     2          TEMPP(NTHRS1,NTHRS2)
      DIMENSION IDEF(4,L2DCMB)
C
      DATA THRES1/NTHRG1*0./
      DATA THRES2/NTHRG2*0./
      DATA PROB  /NTHRGP*0./
      DATA IDEF  /L2DCM4*0/
      DATA NDEF  /1/
C
C     CALL TIMPR(KFILDO,KFILDO,'START L2D600101     ')
C
      IER=0
C
C        COMPUTE THE SEASON NUMBER XX IN IXX, REPRESENTING WINTER = 6
C        AND SUMMER = 5.  FIRST UPDATE NDATE WITH THE RUN TIME OFFSET.
C        NOTE THAT THE OFFSET IS USED HERE, ALTHOUGH IT MIGHT NOT BE
C        IN DEFINING THE VARIABLES BELOW.
C
      MDATE=NDATE
      IF(IDPARS(9).NE.0)CALL UPDAT(MDATE,-IDPARS(9),MDATE)
      MMDD=(MDATE-(MDATE/1000000)*1000000)/100
      IXX=17
      IF(MMDD.LT.0401.OR.MMDD.GT.0930)IXX=18
C
C        COMPUTE THE PROJECTION NUMBER YY IN IYY, EACH REPRESENTING
C        6 PROJECTION TIMES OUT OF EACH 24-HOUR PERIOD.
C
      IYY=MOD(IDPARS(12),24)/6
C
C        COMPUTE THE CYCLE TIME NUMBER CC IN ICC.
C
      ICC=MOD(NDATE,100)
C
C        DEFINE THE MDPRS1( ) OF THE FIRST OF THE VARIABLE PAIR
C        TO LINEARIZE.  ONCE MDPRS1( ) IS DEFINED, ICCFF1, MD1( ),
C        AND LD1( ) CAN BE COMPUTED.
C
      MDPRS1(1)=001
      MDPRS1(2)=000
C        GEOPOTENTIAL HEIGHT.
      MDPRS1(3)=0
      MDPRS1(4)=IDPARS(4)
C        THE ABOVE ASSUMES THE MODEL IS THE SAME AS IN IDPARS(4).
      MDPRS1(5)=0
      MDPRS1(6)=0
C        THIS IS A SINGLE LEVEL.
      MDPRS1(7)=850
C        THE LEVEL IS 850 MB.
      MDPRS1(8)=0
C        TRANSFORMATION DOES NOT APPLY TO VARIABLE.
      MDPRS1(9)=IDPARS(9)
C        RUN TIME OFFSET THE SAME AS IN IDPARS(9).
      MDPRS1(10)=IDPARS(10)
C        TIME APPLICATION IS THE SAME AS IN IDPARS(10).
      MDPRS1(11)=IDPARS(11)
C        TIME PERIOD IS THE SAME AS IN IDPARS(11).
      MDPRS1(12)=IDPARS(12)
C        PROJECTION IS THE SAME AS IN IDPARS(12).
      MDPRS1(13)=IDPARS(13)
C        INTERPOLATION TYPE IS THE SAME AS IN IDPARS(13).
      MDPRS1(14)=IDPARS(14)
C        SMOOTHING IS THE SAME AS IN IDPARS(14).
      MDPRS1(15)=IDPARS(15)
C        GRID INDICATOR IS THE SAME AS IN IDPARS(15)
C
C        FIRST PREDICTOR IS NOW DEFINED.
C
      ICCFF1=MDPRS1(1)*1000+MDPRS1(2)
C        ICCFF1 IS THE CCCFFF OF THE FIRST VARIABLE. 
      MD1(1)=ICCFF1*1000+MDPRS1(3)*100+MDPRS1(4)
      MD1(2)=MDPRS1(5)*100000000+MDPRS1(6)*10000+MDPRS1(7)
      MD1(3)=MDPRS1(8)*100000000+MDPRS1(9)*1000000+
     1       MDPRS1(10)*100000+MDPRS1(11)*1000+MDPRS1(12)
      MD1(4)=MDPRS1(13)*100+MDPRS1(14)*10+MDPRS1(15)
      CALL BASICP(KFILDO,MDPRS1,LD1)
C
C        DEFINE THE MDPRS2( ) OF THE SECOND OF THE VARIABLE PAIR
C        TO LINEARIZE.  ONCE MDPRS2( ) IS DEFINED, ICCFF2, MD2( ),
C        AND LD2( ) CAN BE COMPUTED.
C
      MDPRS2(1)=006
      MDPRS2(2)=020
C        GEOSTROPHIC VORTICITY.
      MDPRS2(3)=0
      MDPRS2(4)=IDPARS(4)
C        THE ABOVE ASSUMES THE MODEL IS THE SAME AS IN IDPARS(4).
      MDPRS2(5)=0
      MDPRS2(6)=0
C        THIS IS A SINGLE LEVEL.
      MDPRS2(7)=1000
C        THE LEVEL IS 1000 MB.
      MDPRS2(8)=0
C        TRANSFORMATION DOES NOT APPLY TO VARIABLE.
      MDPRS2(9)=IDPARS(9)
C        RUN TIME OFFSET IS THE SAME AS IN IDPARS(9).
      MDPRS2(10)=IDPARS(10)
C        TIME APPLICATION IS THE SAME AS IN IDPARS(10).
      MDPRS2(11)=IDPARS(11)
C        TIME PERIOD IS THE SAME AS IN IDPARS(11).
      MDPRS2(12)=IDPARS(12)
C        PROJECTION IS THE SAME AS IN IDPARS(12).
      MDPRS2(13)=IDPARS(13)
C        INTERPOLATION TYPE IS THE SAME AS IN IDPARS(13).
      MDPRS2(14)=IDPARS(14)
C        SMOOTHING IS THE SAME AS IN IDPARS(14).
      MDPRS2(15)=IDPARS(15)
C        GRID INDICATOR IS THE SAME AS IN IDPARS(15)
C
C        SECOND PREDICTOR IS NOW DEFINED.
C
      ICCFF2=MDPRS2(1)*1000+MDPRS2(2)
C        ICCFF2 IS THE CCCFFF OF THE SECOND VARIABLE. 
      MD2(1)=ICCFF2*1000+MDPRS2(3)*100+MDPRS2(4)
      MD2(2)=MDPRS2(5)*100000000+MDPRS2(6)*10000+MDPRS2(7)
      MD2(3)=MDPRS2(8)*100000000+MDPRS2(9)*1000000+
     1       MDPRS2(10)*100000+MDPRS2(11)*1000+MDPRS2(12)
      MD2(4)=MDPRS2(13)*100+MDPRS2(14)*10+MDPRS2(15)
      CALL BASICP(KFILDO,MDPRS2,LD2)
C
C        NOW DO THE TRANSFORMATION IN L2D1.
C
      CALL L2D1(KFILDO,KFIL10,NFIRST,
     1          ID,IDPARS,THRESH,JD,NDATE,
     2          KFILRA,RACESS,NUMRA,
     3          ICALL,CCALL,ICALLD,CCALLD,NAME,
     4          NELEV,STALAT,STALON,
     5          ITIMEZ,ISDATA,SDATA,SDATA1,L1DATA,DIR,ND1,NSTA,
     6          NGRIDC,NGRID,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     7          LSTORE,ND9,LITEMS,CORE,ND10,LASTL,
     8          NBLOCK,LASTD,NSTORE,NFETCH,
     9          IS0,IS1,IS2,IS4,ND7,
     A          FD1,FD2,FD3,FD4,FD5,FD6,FD7,
     B          FDVERT,FDTIME,FDSINS,FDMS,ND2X3,IP12,IP16,
     C          ICCFF1,ICCFF2,IXX,IYY,ICC,
     D          MD1,MDPRS1,LD1,MD2,MDPRS2,LD2,
     E          TEMPT1,TEMPT2,TEMPP,
     F          THRES1,THRES2,PROB,IDEF,NDEF,
     G          NTHRS1,NTHRS2,NREG,L2DCMB,
     H          ISTAV,L3264B,L3264W,MISTOT,IER)
C
      RETURN
      END      
