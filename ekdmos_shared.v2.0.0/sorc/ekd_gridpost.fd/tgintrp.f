      SUBROUTINE TGINTRP(KFILDO,KFIL10,NFIRST,
     1                   ID,IDPARS,THRESH,JD,NDATE,
     2                   KFILRA,RACESS,NUMRA,
     3                   ICALL,CCALL,ICALLD,CCALLD,NAME,
     4                   NELEV,STALAT,STALON,
     5                   ITIMEZ,ISDATA,SDATA,SDATA1,DIR,ND1,NSTA,
     6                   NGRIDC,NGRID,ND11,NSLAB,IPACK,IWORK,ND5,
     7                   LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     8                   IS0,IS1,IS2,IS4,ND7,
     9                   FD1,FD2,FD3,FD4,FD5,FD6,FD7,DATACDF,NCDF,
     A                   FDVERT,FDTIME,FDSINS,FDMS,ND2X3,
     B                   ISTAV,L3264B,L3264W,MISTOT,IER)
C
C        DECEMBER    2006   RUDACK                MDL MOS-2000. 
C        SEPTEMBER   2007   WIEDENFELD/WAGNER     ADDED SMOOTHING CAPABILITY.
C        DECEMBER    2007   RUDACK                CHANGED THE ASSIGNMENT VALUE
C                                                 OF LD(4) FROM "JD(4)" TO
C                                                 "(ID(4)/1000)*1000".  THIS
C                                                 MODIFICATION NOW ALLOWS
C                                                 "TGINTRP" TO ACCOMMODATE
C                                                 VARIABLES THAT CONTAIN A 
C                                                 THRESHOLD VALUE. 
C        FEBRUARY    2009   RUDACK                MODIFIED CODE TO ACCOMMODATE
C                                                 A TEMPORAL INTERPOLATION USING
C                                                 FORECASTS THAT ARE 12 HOURS 
C                                                 APART.  THIS WAS NECESSARY
C                                                 FOR ARCHIVED FORECASTS BEYOND
C                                                 180 HOURS.  ADDED CAPABILITY
C                                                 TO GENERATE GRID BINARY VALUES.
C                                                 RESTRUCTURED GRID 
C                                                 CHARACTERISTIC DIAGNOSTIC 
C                                                 CHECKS IN CODE.  REMOVED "ITABLE"
C                                                 TO NOW ACCOMODATE ANY 
C                                                 PRECIPITATION FIELD.  CORRECTED
C                                                 SPELLING MISTAKES AND CHANGED 
C                                                 REPRESENTATION OF "LD(3)" PER
C                                                 SUGGESTION OF DR. GLAHN.
C
C        PURPOSE
C            TO TEMPORALLY INTERPOLATE GRIDDED DATA VALUES TO 
C            "OFF TIME" PROJECTIONS USING QUADRATIC INTERPOLATION 
C            ONCE THE GRIDDED DATA VALUES HAVE BEEN INTERPOLATED 
C            TO STATIONS.  NOTE THAT WHEN PERFORMING THE TEMPORAL 
C            INTERPOLATION FOR A DISCONTINUOUS VARIABLE SUCH AS
C            PRECIPITATION AMOUNT, A STRAIGHT TEMPORAL LINEAR 
C            INTERPOLATION IS USED.  THE RETURNED DATA VALUES 
C            FROM "TGINTRP" ARE VECTOR.  TGINTRP CURRENTLY 
C            ACCOMMODATES MODEL DATA THAT IS AVAILABLE EVERY 
C            6 OR 12 HOURS.  NOTE THAT FOR PROJECTIONS PRIOR
C            TO 180 HOURS, A 6 OR 12 HOUR INCREMENT MAY BE USED
C            (IDPARS(13)=8 OR 9).  HOWEVER, IT IS RECOMMENDED THAT 
C            ONLY THE 6 HOUR INCREMENT BE USED.  SIX HOUR TIME 
C            INCREMENTS WILL YIELD MORE ACCURATE RESULTS IN THIS
C            SITUATION.
C
C            THE ALGORITHM IS AS FOLLOWS: AN "ON TIME" GRIDDED DATA
C            RECORD IS RETRIEVED OR COMPUTED.  IF THE USER DESIRES,
C            THE GRIDDED DATA MAY THEN BE TURNED INTO GRID BINARIES. 
C            THE GRIDDED (GRID BINARY) DATA MAY THEN BE SMOOTHED.
C            THE (SMOOTHED-GRID BINARY) DATA IS THEN SPATIALLY 
C            INTERPOLATED (USUALLY USING A BILINEAR INTERPOLATION 
C            UNLESS A DISCONTINUOUS VARIABLE IS INDICATED THROUGH 
C            THE ID) TO STATIONS.  THIS PROCESS IS REPEATED FOR THE 
C            THREE REMAINING "ON TIME" PROJECTIONS.  ONCE ALL FOUR DATA 
C            RECORDS HAVE BEEN RETRIEVED AND SPATIALLY INTERPOLATED 
C            TO STATIONS, THE FOUR "ON TIME" STATION DATA VALUES ARE 
C            THEN TEMPORALLY INTERPOLATED USING A QUADRATIC 
C            INTERPOLATION.  IF A QUADRATIC INTERPOLATION CANNOT 
C            BE PERFORMED, A STRAIGHT TEMPORAL LINEAR INTERPOLATION 
C            IS USED.  THIS LATTER SITUATION OCCURS WHEN THE DESIRED 
C            "OFF TIME" PROJECTION LIES WITHIN 6 (OR 12 HOURS - USING 
C            IDPARS(13)=8) HOURS OF THE FIRST OR LAST AVAILABLE MODEL 
C            PROJECTION OR A DISCONTINUOUS VARIABLE IS TO BE TEMPORALLY 
C            INTERPOLATED. 
C            
C            THE FOLLOWING CCC, FFF, AND ISG ARE ACCOMMODATED:
C
C               00X XXX 8XX - MODEL FORECASTS AND IDPARS(13) EQ 8.  DATA 
C                             RETURNED ARE VECTOR. (INTERPOLATION IS 
C                             PERFORMED USING FORECASTS 12 HOURS APART)
C               00X XXX 9XX - MODEL FORECASTS AND IDPARS(13) EQ 9.  DATA
C                             RETURNED ARE VECTOR.  (INTERPOLATION IS
C                             PERFORMED USING FORECASTS 6 HOURS APART)
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
C              NFIRST = 1 FOR THE 1ST DATE. (INPUT)
C               ID(J) = THE VARIABLE WANTED (J=1,4).  (INPUT)
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
C                       JD( ) IS USED TO HELP IDENTIFY THE BASIC MODEL FIELDS
C                       AS READ FROM THE ARCHIVE.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.  (INPUT)
C           KFILRA(J) = HOLDS THE UNIT NUMBERS FOR ACCESSING THE MOS-2000
C                       EXTERNAL RANDOM ACCESS FILES (J=1,NUMRA).  (INPUT)
C           RACESS(J) = THE FILE NAMES CORRESPONDING TO KFILRA(J) (J=1,NUMRA).
C                       (CHARACTER*60)  (INPUT)
C               NUMRA = THE NUMBER OF UNIT NUMBERS AND NAMES IN KFILRA( )
C                       AND RACESS( ).  (INPUT)
C        ICALL(L,K,J) = 8 STATION CALL LETTERS AS CHARACTERS IN AN INTEGER
C                       VARIABLE (L=1,L3264W) (K=1,ND5) (J=1,6).
C                       EQUIVALENCED TO CCALL( , )  (INPUT/OUTPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST, EXCEPT POSSIBLY
C                       CCALLD( ).  EQUIVALENCED TO ICALL( , ).
C                       (CHARACTER*8)  (INPUT/OUTPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN INTEGER
C                       VARIABLE (L=1,L3264W) (K=1,ND5).  (INPUT)
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).  THIS LIST IS USED
C                       IN L1D1 TO READ THE REGION LISTS.  (CHARACTER*8)
C                       (INPUT)
C             NAME(K) = NAMES OF STATIONS (K=1,NSTA).  USED FOR PRINTOUT
C                       ONLY.  (CHARACTER*20)  (INPUT)
C            NELEV(K) = ELEVATION OF STATIONS (K=1,NSTA).  (INPUT)
C           STALAT(K) = LATITUDE OF STATIONS (K=1,NSTA).  (INPUT)
C           STALON(K) = LONGITUDE OF STATIONS (K=1,NSTA).  (INPUT)
C           ITIMEZ(K) = TIME ZONE INDICATOR.  THE NUMBER OF HOURS
C                       THE STATION IS DIFFERENT FROM UTC (K=1,NSTA).
C                       (INPUT)
C           ISDATA(K) = WORK ARRAY (K=1,ND1).  (INPUT)
C            SDATA(K) = TEMPORALLY INTERPOLATED VALUES ARE RETURNED 
C                       IN SDATA( ) THESE VALUES ARE VECTOR (K=1,NSTA). 
C                       (OUTPUT)
C           SDATA1(K) = WORK ARRAY RESERVED FOR USE IN L2D2 (K=1,NSTA).
C                       (INPUT)
C          DIR(K,J,M) = THE IX (J=1) AND JY (J=2) POSITIONS ON THE GRID
C                       FOR THE COMBINATION OF GRID CHARACTERISTICS M
C                       (M=1,NGRID) AND STATION K (K=1,NSTA) IN NGRIDC( ,M).
C                       (INPUT)
C                 ND1 = DIMENSION OF SDATA( ).  (INPUT)
C                NSTA = NUMBER OF STATIONS.  (INPUT)
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
C                 ND5 = DIMENSION OF IPACK( ) AND IWORK( ). FIRST
C                       DIMENSION OF GDATA( , ).  (INPUT)
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
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INTERNAL)
C                       IS2(3) AND IS2(4) ARE USED BY THE CALLING
C                       PROGRAM AS THE GRID DIMENSIONS.  (INTERNAL-OUTPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C    FD1(K),..,FD7(K) = WORK ARRAYS (K=1,ND2X3).  (INTERNAL)
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
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  (INPUT)
C               ISTAV = 0 FOR GRID DATA;
C                       1 FOR VECTOR DATA.  (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING USED
C                       (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS, EITHER 1 OR 2.  (INPUT)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT/OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        52 = NWORDS RETURNED FROM GFETCH NOT EQUAL
C                             TO NSTA
C                       103 = IDPARS(1) AND IDPARS(12) DO NOT INDICATE 
C                             FORECASTS FOR WHICH INTERPOLATION IS 
C                             NEEDED.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C
C        INTERNAL VARIABLES:
C
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT THE 
C                       RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ).  (INTERNAL)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED FROM 
C                       THE FIRST CALL TO GFETCH OR OPTN2 (J=1,4). 
C                       (INTERNAL)
C               MD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED FROM
C                       THE SECOND CALL TO GFETCH (IF NECESSARY) (J=1,4).
C                       (INTERNAL)
C           LDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO LD( ) (J=1,15).
C                       (INTERNAL)
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
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED.  (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED.  (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN GDATA( , ).
C                       (INTERNAL).
C          GDATA(K,J) = WORK ARRAY THAT CONTAINS THE FOUR "ON TIME" MODEL 
C                       FIELDS (WHICH ARE AT EVENLY SPACED PROJECTIONS) AT
C                       GRIDPOINTS.  (K=1,ND5) (J=1,4).  (AUTOMATIC ARRAY)
C                       (INTERNAL)
C          TDATA(K,J) = WORK ARRAY THAT CONTAINS THE FOUR (OR TWO FOR 
C                       DISCONTINUOUS VARIABLES) "ON TIME" MODEL FIELDS 
C                       (WHICH ARE AT EVENLY SPACED PROJECTIONS) AT
C                       STATIONS.  (K=1,ND1) (J=1,4).  (AUTOMATIC ARRAY)
C                       (INTERNAL) 
C                  DT = THE TIME DIFFERENTIAL FACTOR BETWEEN THE "ON TIME"
C                       PROJECTION JUST PRECEDING THE DESIRED "OFF TIME" 
C                       PROJECTION.  (INTERNAL)
C             INCTIME = THE TIME INCREMENT BETWEEN SUCCESSIVE "ON TIME"
C                       MODEL PROJECTIONS (INCTIME=12 FOR IDPARS(13)=8;
C                       INCTIME=6 FOR IDPARS(13)=9).  (INTERNAL)
C               JTERP = FLAG INDICATING WHICH TYPE OF SPATIAL INTERPOLATION 
C                       IS TO BE PERFORMED ON THE DATA.  FOR MOST VARIABLES, 
C                       A BILINEAR INTERPOLATION IS PERFORMED (JTERP=2), 
C                       HOWEVER, VARIABLES ASSOCIATED WITH ACCUMULATED 
C                       PRECIPITATION UTILIZES A DIFFERENT INTERPOLATION TYPE
C                       (JTERP=3).  (INTERNAL)
C             BIQUADR = LOGICAL VARIABLE TO INDICATE THAT A BIQUADRATIC TIME 
C                       INTERPOLATION IS TO BE PERFORMED.  (INTERNAL)
C              BILINE = LOGICAL VARIABLE TO INDICATE THAT A STRAIGHT LINEAR
C                       TIME INTERPOLATION IS TO BE PERFORMED.  (INTERNAL) 
C            JMISS(J) = FLAG INDICATING IF A RECORD IS AVAILABLE (JMISS( )=0))
C                       OR NOT (JMISS( )=1) FOR PROCESSING (J=1,4).  (INTERNAL)
C             IERPREV = RETURNED IER OF THE PREVIOUS PASS.  (INTERNAL)
C
C        NONSYSTEM SUBROUTINES CALLED
C            GFETCH, OPTN2, PRSID1, INTRP, INTRPB, SMTH5,
C            SMTH9, SMTH25, SMTH2X, SMTH3X, GRIDB
C
      LOGICAL BILINE,BIQUADR
C
      CHARACTER*8 CCALL(ND1,6),
     1            CCALLD(ND5)
      CHARACTER*20 NAME(ND1)
      CHARACTER*60 RACESS(NUMRA)
C
      DIMENSION ICALL(L3264W,ND1,6),
     1          NELEV(ND1),STALAT(ND1),STALON(ND1),ITIMEZ(ND1),
     2          ISDATA(ND1),SDATA(ND1),SDATA1(ND1),TDATA(ND1,4)
C        TDATA( , ) IS AN AUTOMATIC ARRAY.
      DIMENSION DIR(ND1,2,ND11),NGRIDC(6,ND11)
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),ICALLD(L3264W,ND5)
      DIMENSION FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),
     1          FD5(ND2X3),FD6(ND2X3),FD7(ND2X3),
     2          FDVERT(ND2X3),FDTIME(ND2X3),FDSINS(ND2X3),FDMS(ND2X3)
      DIMENSION GDATA(ND5,4)
C        GDATA( , ) IS AN AUTOMATIC ARRAY.
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9),DATACDF(ND5,NCDF)
      DIMENSION CORE(ND10)
      DIMENSION KFILRA(NUMRA)
      DIMENSION LD(4),LDPARS(15),MD(4),JMISS(4)
C
      IER=0
      ISTAV=0
C
      BILINE=.FALSE.
      BIQUADR=.FALSE.
C
C        CHECK IF THE VARIABLE ID REQUESTED IN THE CALL IS
C        FOR A QUADRATIC TEMPORAL INTERPOLATION FROM GRIDS
C        TO STATIONS.
C
      IF(((IDPARS(1)/10).NE.0).AND.(IDPARS(13).NE.8).AND.
     1                             (IDPARS(13).NE.9)) THEN
         IER=103
         WRITE(KFILDO,40)(JD(J),J=1,4),IER
 40      FORMAT(/,' ****IDPARS(13) DOES NOT INDICATE THAT A QUADRATIC',
     1            ' TEMPORAL INTERPOLATION ',
     2            'FROM GRIDS IS TO BE PERFORMED.',
     3          /,'     PREDICTOR ',I9.9,2I10.9,I4.3,
     4            ' NOT ACCOMMODATED IN SUBROUTINE ',
     5            'TGINTRP.  IER =',I3)
         GO TO 900
      ENDIF
C
C        INITIALIZE THE ARRAY THAT HOLDS THE SPATIALLY INTERPOLATED
C        STATION VALUES TO "9999".
C
      DO 60 K=1,ND1
         DO 50 J=1,4
            TDATA(K,J)=9999.
 50      CONTINUE
 60   CONTINUE
C
C        INITIALIZE THE ARRAY THAT IS USED TO DETERMINE IF A BILINEAR OR
C        QUADRATIC TEMPORAL INTERPOLATION IS TO BE PERFORMED.
C
      DO 70 J=1,4
         JMISS(J)=0
 70   CONTINUE
C
C        SET THE APPROPRIATE VALUE TO "INCTIME" FOR PROCESSING.
C
      IF(IDPARS(13).EQ.8) THEN 
         INCTIME=12
      ELSE
         INCTIME=6
      ENDIF
C
C        DETERMINE WHAT TYPE OF SPATIAL INTERPOLATION IS TO BE DONE.
C        CURRENTLY, ALL DATA IS INTERPOLATED IN A BILINEAR FASHION
C        EXCEPT FOR PRECIPITATION.  IN THE FUTURE, IF OTHER VARIABLES
C        ARE TO BE PROCESSED THAT REQUIRE ANOTHER TYPE OF INTERPOLATION,
C        IT CAN BE ADDED TO THE CODE AT THAT TIME.
C
      IF((IDPARS(1).EQ.3).AND.((IDPARS(2)/100).EQ.2)) THEN
         JTERP=3
      ELSE
         JTERP=2
      ENDIF
C
C        GUARD FOR THE CASE WHERE THE USER DESIRES AN ACCUMULATED 
C        FIELD (E.G., PRECIPITATION AMOUNT) AND THE PROJECTION
C        IS LESS THAN 6 (ACCUMULATED AMOUNTS BEGIN AT THE SIX
C        HOUR PROJECTION.  IN THIS CASE, EVEN A STRAIGHT LINEAR 
C        TEMPORAL INTERPOLATION CANNOT BE PERFORMED.
C
      IF((JTERP.EQ.3).AND.(IDPARS(12).LT.INCTIME)) THEN
         WRITE(KFILDO,130)
 130     FORMAT(/,' ****THE USER DESIRES A TEMPORAL INTERPOLATION',
     1            ' FOR AN ACCUMULATED QUANTITY DURING A PERIOD FOR',
     2            ' WHICH AN INTERPOLATION',/,5X,'CANNOT BE',
     3            ' PERFORMED.  NOT ENOUGH DATA TO PERFORM', 
     4            ' INTERPOLATION.  EXITING SUBROUTINE TGINTRP.')
         GOTO 800
      ENDIF
C
C        IF THE VARIABLE TO BE PROCESSED IS CONTINUOUS THE ASSUMPTION
C        IS THAT A QUADRATIC TEMPORAL INTERPOLATION IS TO BE PERFORMED.
C        THIS MEANS "TGINTRP" REQUIRES 4 PROJECTIONS OF DATA.  HOWEVER,
C        IF THE VARIABLE TO BE PROCESSED IS DISCONTINUOUS, A STRAIGHT
C        TEMPORAL LINEAR INTERPOLATION IS PERFORMED.  IN THIS CASE,
C        ONLY 2 PROJECTIONS OF DATA ARE NEEDED.  FIND THE TEMPORAL 
C        END POINTS THAT ARE NEEDED FOR THE QUADRATIC OR STRAIGHT 
C        LINEAR TEMPORAL INTERPOLATION.
C
      IADD=INCTIME+MOD(IDPARS(12),INCTIME)
C
      IF(JTERP.EQ.2) THEN
         IBEG=IDPARS(12)-IADD
         IEND=IBEG+(INCTIME*3)
      ELSE
         IBEG=IDPARS(12)-MOD(IDPARS(12),INCTIME)
         IEND=IBEG+INCTIME
      ENDIF
C
C        PROVIDE A DIAGNOSTIC TO THE USER WHEN HE/SHE IS ATTEMPTING
C        TO USE FORECASTS 12 HOURS APART WHEN ATTEMPTING TO PERFORM 
C        AN INTERPOLATION TO AN "OFF HOUR" PRIOR TO 180 HOURS.  SINCE
C        6-H FORECASTS ARE AVAILABLE PRIOR TO THE 180-H PROJECTION,
C        AN INCREMENT OF 6 HOURS SHOULD BE USED.  
C
      IF((IDPARS(12).LE.180).AND.(IDPARS(13).EQ.8)) THEN
         WRITE(KFILDO,150)(ID(JJ),JJ=1,4)
 150     FORMAT(/,' ****THE USER IS ATTEMPTING TO PERFORM',
     1            ' A TEMPORAL INTERPOLATION USING FORECASTS',
     2            ' 12 HOURS APART FOR',/,5X,'VARIABLE',2X,I9.9,1X,
     3            I9.9,1X,I9.9,1X,I10.3,'.  FOR THE BEST RESULTS,',
     4            ' USE FORECASTS THAT',/,5X,'ARE SEPARATED BY 6',
     5            ' HOURS.  SET IDPARS(13) TO EQUAL "9" IN ORDER TO',
     6            ' OBTAIN AN INTERPOLATED',/,5X,'FORECAST USING',
     7            ' FORECASTS THAT ARE SEPARATED BY 6 HOURS.',
     8            '  CONTINUE PROCESSING.') 
      ENDIF
C
C        CONSTRUCT THE ID FOR THE FOUR FORECAST PROJECTIONS NEEDED FOR THE
C        QUADRATIC INTERPOLATION.  GRIDDED DATA WILL BE RETURNED IN
C        GDATA( , ).  NOTE THAT LD(4) IS SET TO JD(4).  IF LD(4) WAS SET
C        TO ID(4), THE VARIABLE WOULD NOT BE FOUND.
C
      J=1
      IERPREV=0
C
      DO 300 NTAU=IBEG,IEND,INCTIME
C 
         LD(1)=ID(1)
         LD(2)=ID(2)
         LD(3)=(ID(3)/1000)*1000+NTAU
         LD(4)=(ID(4)/1000)*1000
C
C           RETRIEVE THE GRIDDED DATA.
C 
         CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1               IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,GDATA(1,J),ND5,
     2               NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3               NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
C
C           CHECK THE ERROR VALUE.
C
         IF(IER.EQ.0) THEN
C
C              SET THE "NX" AND "NY" VALUES SO THAT THE GRIDS CAN BE
C              CHECKED WITH THE SUBSEQUENT PROJECTION.
C
            NX=IS2(3)
            NY=IS2(4)
C
C              PERFORM GRID CHARACTERISTIC DIAGNOSTIC CHECK.  PERFORM 
C              THIS CHECK ONLY IF DATA HAS BEEN RETRIEVED OVER THE
C              LAST TWO PASSES.
C
            IF((NTAU.GT.IBEG).AND.(IERPREV.EQ.0)) THEN
               IF((NX.NE.IS2(3)).OR.(NY.NE.IS2(4)))THEN
                  WRITE(KFILDO,235)(JD(L),L=1,4)
 235              FORMAT(/' ****THE TWO GRIDS RETURNED BY GFETCH IN',
     1                    ' TGINTRP HAVE DIFFERENT CHARACTERISTICS.'/,
     2                    '     VARIABLE',2X,I9.9,1X,I9.9,1X,I9.9,1X,
     3                    I10.3,' NOT ACCOMMODATED.')
                  IER=100
                  GO TO 800
               ENDIF
            ENDIF
C
         ELSE
C
C              PLACE THE VARIABLE ID INTO ITS BASIC FORM.  PERHAPS
C              THE USER WOULD LIKE TO CALCULATE A GRID BINARY FOR A
C              NON-CALCULATED FIELD AND THEREFORE THE CURRENT INPUT 
C              ID IS NOT IN ITS BASIC FORM.
C
C              RETRIEVE THE GRIDDED DATA.
C
            MD(1)=JD(1)
            MD(2)=JD(2)
            MD(3)=NTAU
            MD(4)=JD(4)
C
            CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1                  IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,GDATA(1,J),
     2                  ND5,NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3                  NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
C
C              CHECK THE ERROR VALUE.
C
            IF(IER.EQ.0) THEN
C
C                 SET THE "NX" AND "NY" VALUES SO THAT THE GRIDS CAN BE
C                 CHECKED WITH THE SUBSEQUENT PROJECTION.
C
               NX=IS2(3)
               NY=IS2(4)
C               NSTA=NX*NY
C
C                  PERFORM GRID CHARACTERISTIC DIAGNOSTIC CHECK.  PERFORM
C                  THIS CHECK ONLY IF DATA HAS BEEN RETRIEVED OVER THE
C                  LAST TWO PASSES.
C
               IF((NTAU.GT.IBEG).AND.(IERPREV.EQ.0)) THEN
                  IF((NX.NE.IS2(3)).OR.(NY.NE.IS2(4)))THEN
                     WRITE(KFILDO,245)(JD(L),L=1,4)
 245                 FORMAT(/' ****THE TWO GRIDS RETURNED BY GFETCH',
     1                       'IN TGINTRP HAVE DIFFERENT',
     2                       ' CHARACTERISTICS.'/,'     VARIABLE',
     3                       2X,I9.9,1X,I9.9,1X,I9.9,1X,
     4                       I10.3,' NOT ACCOMMODATED.')
                     IER=100
                     GO TO 800
                  ENDIF
               ENDIF
C
C                 THE RECORD IS AVAILABLE AND A SPATIAL INTERPOLATION
C                 CAN BE PERFORMED ON THE COMPUTED VARIABLE.  CONTINUE
C                 PROCESSING THE VARIABLE.
C
               JMISS(J)=0
               GOTO 290
C
            ENDIF 
C
C              IF THE VARIABLE IS NOT AVAILABLE DIRECTLY, TRY TO
C              COMPUTE IT THROUGH OPTN2.
C
            CALL PRSID1(KFILDO,LD,LDPARS)
C
            CALL OPTION(KFILDO,KFIL10,NFIRST,
     1                  LD,LDPARS,THRESH,JD,NDATE,
     2                  KFILRA,RACESS,NUMRA,ICALL,CCALL,ICALLD,
     3                  CCALLD,NAME,NELEV,STALAT,STALON,
     4                  ITIMEZ,ISDATA,SDATA,SDATA1,L1DATA,DIR,ND1,NSTA,
     5                  NGRIDC,NGRID,ND11,NSLAB,IPACK,IWORK,GDATA(1,J),
     +                  ND5,
     6                  LSTORE,ND9,LITEMS,CORE,ND10,LASTL,
     7                  NBLOCK,LASTD,NSTORE,NFETCH,
     8                  IS0,IS1,IS2,IS4,ND7,
     9                  FD1,FD2,FD3,FD4,FD5,FD6,FD7,DATACDF,NCDF,
     A                  FDVERT,FDTIME,FDSINS,FDMS,ND2X3,IP12,IP16,
     B                  ISTAV,L3264B,L3264W,MISTOT,IER)
C
C            CALL OPTN2(KFILDO,KFIL10,NFIRST,
C     1                 LD,LDPARS,THRESH,JD,NDATE,
C     2                 KFILRA,RACESS,NUMRA,
C     3                 ICALL,CCALL,ICALLD,CCALLD,NAME,
C     4                 NELEV,STALAT,STALON,
C     5                 ITIMEZ,ISDATA,SDATA,SDATA1,DIR,ND1,NSTA,
C     6                 NGRIDC,NGRID,ND11,NSLAB,IPACK,IWORK,
C     7                 GDATA(1,J),ND5,LSTORE,ND9,LITEMS,CORE,
C     8                 ND10,NBLOCK,NFETCH,
C     9                 IS0,IS1,IS2,IS4,ND7,
C     A                 FD1,FD2,FD3,FD4,FD5,FD6,FD7,
C     B                 FDVERT,FDTIME,FDSINS,FDMS,ND2X3,
C     C                 ISTAV,L3264B,L3264W,MISTOT,IER)
C
C               IF THE DATA CANNOT BE FOUND OR COMPUTED FOR J=2 OR J=3,
C               NO TEMPORAL INTERPOLATION CAN BE PERFORMED.  SET THE
C               DATA VALUES TO MISSING AT 800 AND EXIT THE PROGRAM.  
C               IF THE RETURNED DATA VALUES ARE VECTOR, EXIT THE 
C               PROGRAM AS WELL.
C
            IF((((J.EQ.2).OR.(J.EQ.3)).AND.(IER.NE.0))
     1                   .OR.(ISTAV.GT.0)) THEN
               WRITE(KFILDO,250)
 250           FORMAT(/,'   ****DATA COULD NOT BE FOUND IN TGINTRP', 
     1            ' TO PERFORM EITHER A QUADRATIC OR BILINEAR',
     2            ' INTERPOLATION.  EXITING TGINTRP.')
               GOTO 800
            ENDIF
C
C              CHECK THE "IER" VALUE RETURNED FROM OPTN2.  IF IER=0,
C              SET JMISS( )=0, OTHERWISE JMISS( )=1.  ALTHOUGH THE FIRST
C              OR LAST RECORD MAY NOT BE AVAILABLE, PROCESSING CONTINUES 
C              BECAUSE A BILINEAR INTERPOLATION MAY STILL NEED TO BE
C              PERFORMED.  
C
            IF(IER.EQ.0) THEN
C
C                 THE RECORD IS AVAILABLE AND A SPATIAL INTERPOLATION
C                 CAN BE PERFORMED ON THE COMPUTED VARIABLE. 
C
               JMISS(J)=0
C
C                 SET THE "NX" AND "NY" VALUES SO THAT THE GRIDS CAN BE
C                 CHECKED WITH THE SUBSEQUENT PROJECTION.
C
               NX=IS2(3)
               NY=IS2(4)
C
C                 PERFORM GRID CHARACTERISTIC DIAGNOSTIC CHECK.  PERFORM
C                 THIS CHECK ONLY IF DATA HAS BEEN RETRIEVED OVER THE
C                 LAST TWO PASSES.
C
               IF((NTAU.GT.IBEG).AND.(IERPREV.EQ.0)) THEN
                  IF((NX.NE.IS2(3)).OR.(NY.NE.IS2(4)))THEN
                     WRITE(KFILDO,285)(JD(L),L=1,4)
 285                 FORMAT(/' ****THE TWO GRIDS RETURNED BY OPNT2 IN',
     1                       ' TGINTRP HAVE DIFFERENT CHARACTERISTICS.',
     2                       /,'     VARIABLE',2X,I9.9,1X,I9.9,1X,I9.9,1X,
     3                       I10.3,' NOT ACCOMMODATED.')
                     IER=100
                     GO TO 800
                  ENDIF
               ENDIF
C
            ELSE
C
C                 THE RECORD IS NOT AVAILABLE AND A SPATIAL INTERPOLATION
C                 CANNOT BE PERFORMED ON THE COMPUTED VARIABLE.  INCREMENT
C                 TO THE NEXT "ON TIME" PROJECTION AND PROCESS THE NEXT
C                 PROJECTION.
C
               JMISS(J)=1
               J=J+1
               IERPREV=IER
               GOTO 300
C
            ENDIF
C         
         ENDIF
C
C           GENERATE A GRID BINARY IF THE USER DESIRES.
C
 290     IF(IDPARS(3).EQ.5)THEN
C
            CALL GRIDB(KFILDO,LD,IDPARS(3),THRESH,
     1                 GDATA(1,J),IWORK,NX,NY,IER)
C
C              IF AN ERROR HAS OCCURRED, EXIT THE PROGRAM.
C
            IF(IER.NE.0) THEN
               WRITE(KFILDO,292)(ID(L),L=1,4),IER
 292           FORMAT(/' ****THE GRID BINARY VALUE FOR VARIABLE',
     1                 2X,I9.9,1X,I9.9,1X,I9.9,1X,
     2                 I10.3,' COULD NOT BE COMPUTED.  IER = ',
     3                 I3,'.  EXITING TGINTRP.')
               GO TO 900
            ENDIF
C
         ENDIF
C
C           SMOOTH FIELD IF DESIRED.
C
C           IWORK( ) IS USED AS FLOATING POINT IN
C           SMOOTHING ROUTINES.  THAT IS OK.  IWORK( )
C           AND GDATA( , ) ARE BOTH DIMENSIONED ND5.
C
         IF(IDPARS(14).EQ.1)THEN
            CALL SMTH5(KFILDO,GDATA(1,J),IWORK,NX,NY)
         ELSEIF(IDPARS(14).EQ.2)THEN
            CALL SMTH9 (KFILDO,GDATA(1,J),IWORK,NX,NY)
         ELSEIF(IDPARS(14).EQ.3)THEN
            CALL SMTH25(KFILDO,GDATA(1,J),IWORK,NX,NY)
         ELSEIF(IDPARS(14).EQ.4)THEN
            CALL SMTH2X(KFILDO,GDATA(1,J),IWORK,NX,NY)
         ELSEIF(IDPARS(14).EQ.5)THEN
            CALL SMTH3X(KFILDO,GDATA(1,J),IWORK,NX,NY)
         ENDIF
C
C           SPATIALLY INTERPOLATE THE GRIDDED FORECAST VALUES TO THE
C           STATIONS FOR THE CURRENT PROJECTION.  CURRENTLY, ALL DATA 
C           IS SPATIALLY INTERPOLATED IN A BILINEAR FASHION EXCEPT FOR 
C           PRECIPITATION.  IF THE RECORD IS NOT AVAILABLE, NO SPATIAL
C           INTERPOLATION IS PERFORMED. 
C
         IF(JTERP.EQ.2)THEN
C
C              "INTRPB" IS A BILINEAR SPATIAL INTERPOLATION.
C
C            CALL INTRPB(KFILDO,GDATA(1,J),NX,NY,
C     1                  DIR(1,1,NSLAB),ND1,NSTA,TDATA(1,J))
            DO 293 MMM=1,NX,NY
               TDATA(MMM,J)=GDATA(MMM,J)
 293        CONTINUE
C
         ELSEIF(JTERP.EQ.3)THEN
C
C              "INTRP" IS INTERPOLATION FOR PRECIPITATION AMOUNT. 
C              THE PROCESS IS BILINEAR AFTER PREPARATION OF THE FIELD
C              TO PUT THE ZERO LINE ABOUT HALFWAY BETWEEN POSITIVE
C              AND ZERO GRIDPOINTS.
C
            CALL INTRP(KFILDO,GDATA(1,J),FD1,NX,NY,
     1                 DIR(1,1,NSLAB),ND1,NSTA,TDATA(1,J))
C
         ENDIF
C
C           INCREMENT "J" TO ACCOMMODATE THE NEXT PROJECTION.
C
         J=J+1
         IERPREV=0
C
 300  CONTINUE
C
C        DETERMINE WHETHER A TEMPORAL QUADRATIC OR BILINEAR 
C        INTERPOLATION IS TO BE PERFORMED FOR ALL STATIONS.
C
      IF(((JMISS(2)).EQ.0).AND.(JMISS(3)).EQ.0) THEN
         IF((JTERP.EQ.2).AND.(JMISS(1).EQ.0).AND.(JMISS(4).EQ.0)) THEN
            BIQUADR=.TRUE.
         ELSEIF((JTERP.EQ.3).OR.(JMISS(1).EQ.1).OR.(JMISS(4).EQ.1)) THEN
            BILINE=.TRUE.
         ENDIF
      ELSE
         WRITE(KFILDO,310) 
 310     FORMAT(/,'   ****RECORDS COULD NOT BE FOUND IN TGINTRP TO',
     1            ' PERFORM EITHER A QUADRATIC OR BILINEAR',
     2            ' INTERPOLATION.  EXITING TGINTRP.')
         GOTO 800
      ENDIF 
C
C        DETERMINE THE TEMPORAL DIFFERENCE BETWEEN THE INTERPOLATED TIME
C        AND THE SUBSEQUENT "ON TIME" MODEL FIELD.
C
      DT=FLOAT(MOD(IDPARS(12),INCTIME))/FLOAT(INCTIME)
C
      IF(BIQUADR) THEN
C
C           PERFORM A QUADRATIC TEMPORAL INTERPOLATION FOR EACH STATION.
C
C         DO 450 K=1,NSTA
         DO 450 K=1,NX*NY
            FCT=(DT**2-DT)/4.
            SDATA(K)=GDATA(K,2)+(GDATA(K,3)-GDATA(K,2))*DT+
     1               (GDATA(K,1)+GDATA(K,4)-GDATA(K,2)-GDATA(K,3))*FCT
C            SDATA(K)=TDATA(K,2)+(TDATA(K,3)-TDATA(K,2))*DT+
C     1               (TDATA(K,1)+TDATA(K,4)-TDATA(K,2)-TDATA(K,3))*FCT
 450     CONTINUE
C
      ELSEIF(BILINE) THEN
C
C           PERFORM A STRAIGHT LINEAR TEMPORAL INTERPOLATION FOR EACH STATION.
C         
         IF(JTERP.EQ.2) THEN
C
C              IF THE VARIABLE IS CONTINUOUS, ALL FOUR FIELDS ARE RETRIEVED. 
C              HOWEVER, SINCE THE DESIRED PROJECTION STRADDLES THE THE FIRST
C              OR LAST SIX HOURS OF AVAILABLE MODEL DATA, ONLY TDATA( ,2) AND
C              TDATA( ,3) ARE USED IN THE TEMPORAL LINEAR CALCULATION.
C
            DO 475 K=1,NSTA
               SDATA(K)=(TDATA(K,3)-TDATA(K,2))*DT+TDATA(K,2)
 475        CONTINUE
C
         ELSE
C
C              IF THE VARIABLE IS DISCONTINUOUS, ONLY TWO FIELDS ARE 
C              RETRIEVED.  CONSEQUENTLY, TDATA( ,1) AND TDATA( ,2) ARE
C              THE ONLY ARRAY INDICES USED IN THE TEMPORAL LINEAR CALCULATION.
C
            DO 485 K=1,NSTA
               SDATA(K)=(TDATA(K,2)-TDATA(K,1))*DT+TDATA(K,1)
 485        CONTINUE
C
         ENDIF
C
      ENDIF
C
C        SINCE QUADRATIC INTERPOLATED GRID BINARIES CAN BE SLIGHTLY 
C        ABOVE THE VALUE OF ONE AND SLIGHTLY BELOW THE VALUE OF ZERO,
C        ENSURE THAT ALL INTERPOLATED QUADRATIC GRID BINARY VALUES 
C        ARE CONSTRAINED BETWEEN THE VALUES OF ZERO AND ONE.
C
      IF(IDPARS(3).EQ.5) THEN
         DO 490 K=1,NSTA
            IF(NINT(SDATA(K)).NE.9999) THEN
               IF(SDATA(K).GT.1.) THEN
                  SDATA(K)=1.
               ELSEIF(SDATA(K).LT.0.) THEN
                  SDATA(K)=0.
               ENDIF
            ENDIF
 490    CONTINUE
      ENDIF
C
C        IF THE EXECUTION OF THE CODE HAS REACHED THIS POINT, SET THE 
C        "IER" RETURN VALUE TO ZERO.  THE "IER" VALUE COULD HAVE BEEN 
C        CHANGED IF A LINEAR INTERPOLATION WAS PERFORMED.  THIS WOULD
C        HAVE NECESSITATED THAT ONE OF THE VALUES WAS MISSING (WHICH
C        IS IN FACT NOT A TRUE "IER" IN THIS CONTEXT.  "ISTAV" IS SET
C        TO A VALUE OF ONE BECAUSE THE RETURNED DATA ARE VECTOR. 
C  
      IER=0
      ISTAV=1 
C
      GO TO 900
C
C        IF A TEMPORAL QUADRATIC OR STRAIGHT LINEAR INTERPOLATION
C        COULD NOT BE PERFORMED, SET THE RETURNED DATA VALUES TO MISSING.
C
 800  DO 850 K=1,ND1
         SDATA(K)=9999.
 850  CONTINUE 
C
 900  RETURN
      END
