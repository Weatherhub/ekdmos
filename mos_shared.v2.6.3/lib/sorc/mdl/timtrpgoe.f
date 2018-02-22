      SUBROUTINE TIMTRPGOE(KFILDO,KFIL10,NFIRST,
     1                  ID,IDPARS,THRESH,JD,NDATE,
     2                  KFILRA,RACESS,NUMRA,
     3                  ICALL,CCALL,ICALLD,CCALLD,NAME,
     4                  NELEV,STALAT,STALON,
     5                  ITIMEZ,ISDATA,SDATA,SDATA1,DIR,ND1,NSTA,
     6                  NGRIDC,NGRID,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     7                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     8                  IS0,IS1,IS2,IS4,ND7,
     9                  FD1,FD2,FD3,FD4,FD5,FD6,FD7,
     A                  FDVERT,FDTIME,FDSINS,FDMS,ND2X3,
     B                  ISTAV,L3264B,L3264W,MISTOT,IER)
C
C        AUGUST    2003   GLAHN   TDL   MOS-2000
C        SEPTEMBER 2003   GLAHN   MADE INTEGER 3 REAL 3 IN COMPUTING F
C        SEPTEMBER 2003   GLAHN   CHANGED ND2X3 TO ND1 IN DO 801 LOOP;
C                                 SET LD( ) AND MD( ) = ID( ) INSTEAD
C                                 OF 0 AND JD( )
C        OCTOBER   2003   GLAHN   REVISED CALL; ADDED CALL OPTN2 IF
C                                 NEEDED; ADDED GRID CAPABILITY
C        OCTOBER   2003   GLAHN   REVISED TO DEFINE LD( ) WITH ID( )
C                                 RATHER THAN JD( ) IN VECTOR LOOP
C        OCTOBER   2003   GLAHN   ALLOWED FOR MISSING VECTOR VALUES
C                                 9999 AND 9997 IN DO 320 LOOP
C        NOVEMBER  2003   GLAHN   ACCOMMODATED 9997 ON BOTH ENDS OF
C                                 THE TIME SPAN TO GIVE 9997 BETWEEN
C        DECEMBER  2003   GLAHN   MODIFIED TO USE CLOSEST PROJECTION
C                                 WHEN INTERPOLATION IS NOT APPROPRIATE
C                                 FOR VECTOR DATA; INSERTED 2ND CALL
C                                 TO GFETCH TO ACCOUNT FOR JD( ) NE
C                                 ID( )
C        AUGUST    2004   GLAHN   INSERTED ELSE LOOP DO 200; SPELLING;
C                                 CHECKED ISTAV1 VICE ISTAV AFTER CALL
C                                 TO OPTN2 IN VECTOR SECTION
C        AUGUST    2004   GLAHN   FILLED LD(4), LDPARS(13), LDPARS(14)
C                                 ABOVE CALL TO OPTN2 IN GRID SECTION;
C                                 MODS IN GRIDPOINT SECTION TO 
C                                 ACCOMMODATE RETURN OF VECTOR DATA
C                                 FROM OPTN2
C        AUGUST    2004   GLAHN   ADDED AUTOMATIC VARIABLE YDATA( );
C                                 DATA( ) RESERVED FOR CALL TO OPTN2
C        SEPTEMBER 2004   RUDACK  RESTRUCTURED LOGIC OF CODE.
C        SEPTEMBER 2004   RUDACK  INSERTED CODE TO ACCOMMODATE THE
C                                 TEMPORAL INTERPOLATION OF THE MOS
C                                 6-HR QPF PROBABILITY VALUES.
C        JANUARY   2005   RUDACK  MODIFIED CODE TO TREAT A SECONDARY 
C                                 MISSING VALUE OF '9997' AS A ZERO.
C        APRIL     2008   SMB     MODIFIED TIMTRP FOR EXTENDED-RANGE FIRST
C                                 GUESSES TO INTERPOLATE BETWEEN 12-HR
C                                 FIELDS.  RENAMED TIMTRPGOE.   REMOVED
C                                 HANDLING OF STATISTICAL FORECASTS.
C        NOVEMBER  2010   ENGLE   MODIFIED TO HANDLE TIME INTERPOLATION
C                                 BETWEEN DIFFERENT GRIDS, ONLY IF THEY 
C                                 ARE DIFFERENT BEDIENTS OF A PARENT
C                                 GRID. ADDED CALL TO DENSRM TO HANDLE
C                                 SPATIAL INTERP OF COARSE GRID TO FINE
C                                 GRID (ASSUMING COARSE GRID IS AT A
C                                 LATER FORECAST PROJECTION AND FINE
C                                 FINE GRID).
C
C        PURPOSE 
C            TO INTERPOLATE FROM 12-HR FORECASTS TO INTERMEDIATE
C            HOURS.  FOR INSTANCE, MODEL 12-HR FIELDS CAN BE
C            INTERPOLATED TO 3-HR VALUES FOR FIRST GUESSES FOR U155.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               00X XXX - MODEL FORECASTS, ONLY WHEN TAU NOT EVENLY 
C                         DIVISIBLE BY 12 AND DD NE 5.  DATA RETURNED
C                         CAN BE GRIDPOINT OR VECTOR.  (THE VECTOR
C                         OPTION IS NECESSARY HERE BECAUSE UPSLOP
C                         COMPUTES VECTOR DATA INTERNALLY, AND 
C                         INTERPOLATION MUST BE ON THOSE DATA.  
C            NOTE:  OTHER VECTOR OR GRIDPOINT DATA CAN BE ACCOMMODATED
C            WITH REVISED CRITERIA FOR CALLING IN OPTION AND IN 
C            CHECK FOR VARIABLES ACCOMMODATED ABOVE 110.
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C 
C        VARIABLES 
C              NOTE:  MANY OF THESE VARIABLES ARE IN THE CALL ONLY TO
C                     PASS TO OPTN2.  ONLY THE ONES OTHERWISE USED IN THIS
C                     ROUTINE ARE EXPLAINED HERE.
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C             NFIRST = 1 FOR THE 1ST DATE. (INPUT)
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
C            SDATA(J) = INTERPOLATED VALUES RETURNED WHEN DATA ARE VECTOR
C                       (ISTAV=1).  (OUTPUT)
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
C             DATA(K) = INTERPOLATED VALUES RETURNED WHEN DATA ARE GRIDPOINT
C                       (ISTAV=0).  INTERNALLY, MUST BE USED FOR CALL
C                       TO OPTN2 BECAUSE DIMENSION IS ND5.  (OUTPUT)
C                 ND5 = DIMENSION OF IPACK( ),IWORK( ), AND DATA( ).
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
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT THE 
C                       RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ).  (INTERNAL)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED FROM 
C                       GFETCH OR OPTN2 (J=1,4).  (INTERNAL)
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
C              NWORDS = NUMBER OF WORDS RETURNED IN XDATA( ).
C                       (INTERNAL).
C            XDATA(K) = WORK ARRAY (K=1,ND2X3).  (AUTOMATIC)
C            YDATA(K) = WORK ARRAY (K=1,ND2X3).  (AUTOMATIC)
C               NXCHK = NUMBER OF GRIDPOINTS IN X-DIRECTION OF A COARSE
C                       GRID TO COMPARE AGAINST A FINE GRID.  (INTERNAL)
C               NYCHK = NUMBER OF GRIDPOINTS IN Y-DIRECTION OF A COARSE
C                       GRID TO COMPARE AGAINST A FINE GRID.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH, OPTN2, PRSID1, DENSRM
C
      CHARACTER*8 CCALL(ND1,6),
     1            CCALLD(ND5)
      CHARACTER*20 NAME(ND1)
      CHARACTER*60 RACESS(NUMRA)
C
      DIMENSION ICALL(L3264W,ND1,6),
     1          NELEV(ND1),STALAT(ND1),STALON(ND1),ITIMEZ(ND1),
     2          ISDATA(ND1),SDATA(ND1),SDATA1(ND1)
      DIMENSION DIR(ND1,2,ND11),NGRIDC(6,ND11)
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5),ICALLD(L3264W,ND5)
      DIMENSION FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),
     1          FD5(ND2X3),FD6(ND2X3),FD7(ND2X3),
     2          FDVERT(ND2X3),FDTIME(ND2X3),FDSINS(ND2X3),FDMS(ND2X3)
      DIMENSION XDATA(ND2X3),YDATA(ND2X3)
C        XDATA( ) AND YDATA( ) ARE AUTOMATIC ARRAYS.
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION KFILRA(NUMRA)
      DIMENSION LD(4),LDPARS(15)
C
D     CALL TIMPR(KFILDO,KFILDO,'START TIMTRPGOE        ')
D     WRITE(KFILDO,100)(IDPARS(J),J=1,15)
D100  FORMAT(/,' TIMTRPGOE AT 100--IDPARS( )',4I12,11I5)
C
      IER=0
      IDENS=0
C
C        ************************************************************
C
C        THIS SECTION FOR GRIDPOINT DATA.
C
C        ************************************************************
C
      IF(IDPARS(1)/10.EQ.0.AND.MOD(IDPARS(12),12).NE.0.AND.
     1       IDPARS(4).NE.5)THEN
C
C           IT IS ASSUMED MODEL FORECASTS ARE AT 12-HR INCREMENTS
C           (STARTING AT 0).  THE VARIABLE IS ACCOMMODATED ONLY
C           FOR CCC = 00X, MOD(IDPARS(12),12) NE 0, AND DD NE 5.
C           DD = 5 IS NOT ACCOMMODATED BECAUSE LAMP FORECASTS ARE
C           ALREADY AT HOURLY INTERVALS.
C           WHILE THIS IS CURRENTLY TAILORED FOR FORECASTS EVERY
C           12 HOURS, OTHER INTERVALS CAN BE ACCOMMODATED IN THE LOOP
C           BELOW, PROVIDED IBEG AND IEND ARE COMPUTED CORRECTLY.
C
         ISTAV=0
C           THE DATA RETURNED ARE GRIDDED IN DATA( ).
         IADD=MOD(IDPARS(12),12)
C           IADD IS THE NUMBER OF HOURS THE FORECAST WANTED IS AFTER
C           THE FIRST VALUE TO BE USED IN INTERPOLATION.
         IBEG=IDPARS(12)-IADD
C           IBEG IS THE FIRST FORECAST TO USE IN INTERPOLATION.
         IEND=IBEG+12
C           IEND IS THE SECOND FORECAST TO USE IN INTERPOLATION.
C
C           GET THE TWO FORECASTS TO USE IN INTERPOLATION.  THE ID 
C           INCLUDES THE RUN OFFSET TIME.  THE LOOP BELOW WILL EXECUTE
C           TWICE.
C
         DO 700 NTAU=IBEG,IEND,IEND-IBEG
C
         LD(1)=JD(1)
         LD(2)=JD(2)
         LD(3)=IDPARS(9)*1000000+NTAU
         LD(4)=JD(4)
C
         CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1               IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,XDATA,ND2X3,
     2               NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3               NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
C
         ISTAV=0
C           IF DATA ARE RETURNED BY GFETCH, THEY ARE GRIDPOINT.
C           THIS IS TESTED BELOW.
C
         IF(IER.NE.0)THEN
C
C              IF THE VARIABLE IS NOT AVAILABLE DIRECTLY, TRY TO
C              COMPUTE IT THROUGH OPTN2.
C
            LD(4)=ID(4)
            CALL PRSID1(KFILDO,LD,LDPARS)
C              LD( ) IS PARSED INTO LDPARS( ).
            LDPARS(13)=IDPARS(13)
            LDPARS(14)=IDPARS(14)
C
C              PRSID1 ASSUMES ISG = 0; NEED TO RETAIN THEM.
C              NOTE THAT  DATA( ) MUST BE USED IN CALL TO OPTN2 BECAUSE IT
C              HAS DIMENSION ND5.
C
            CALL OPTN2(KFILDO,KFIL10,NFIRST,
     1                 LD,LDPARS,THRESH,JD,NDATE,
     2                 KFILRA,RACESS,NUMRA,
     3                 ICALL,CCALL,ICALLD,CCALLD,NAME,
     4                 NELEV,STALAT,STALON,
     5                 ITIMEZ,ISDATA,SDATA,SDATA1,DIR,ND1,NSTA,
     6                 NGRIDC,NGRID,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     7                 LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     8                 IS0,IS1,IS2,IS4,ND7,
     9                 FD1,FD2,FD3,FD4,FD5,FD6,FD7,
     A                 FDVERT,FDTIME,FDSINS,FDMS,ND2X3,
     B                 ISTAV,L3264B,L3264W,MISTOT,IER)
C
            IF((IER.NE.0).OR.(ISTAV.GT.0)) GOTO 800
C              THE VARIABLE COULD NOT BE COMPUTED OR THE RETURNED 
C              DATA VALUES ARE VECTOR.
C
C              WHEN GRIDDED DATA ARE RETURNED FROM OPTN2, THEY COME
C              BACK IN DATA( ) RATHER THAN XDATA( ).  TRANSFER
C              TO TEMPORARY ARRAY YDATA( ).  
C
            IF(NTAU.EQ.IBEG)THEN
C
               NX=IS2(3)
               NY=IS2(4)
               NSLAB1=NSLAB
C
               DO 350 J=1,NX*NY
                 YDATA(J)=DATA(J)
 350           CONTINUE
C 
            ELSE
C            
               IF(NX.NE.IS2(3).OR.NY.NE.IS2(4).OR.NSLAB1.NE.NSLAB)THEN
C
                  NXCHK=((IS2(3)-1)*2)+1
                  NYCHK=((IS2(4)-1)*2)+1
C
                  IF(NX.EQ.NXCHK.AND.NY.EQ.NYCHK.AND.
     1               NGRIDC(5,NSLAB1).EQ.NGRIDC(5,NSLAB).AND.
     2               NGRIDC(6,NSLAB1).EQ.NGRIDC(6,NSLAB))THEN
C
C                 CHECK TO SEE IF THE DIFFERENT GRID ARE JUST A CHANGE
C                 IN RESOLUTION, BUT REALLY ARE THE SAME GRID.
C
C                 THE LOGIC HERE IS ASSUMING THAT THE COARSE GRID WILL
C                 ALWAYS BE IN A LATER FORECAST PROJECTION THAN THE FINE
C                 GRID.
C
                    CALL DENSRM(KFILDO,DATA,IS2(3),IS2(4),NXCHK*NYCHK)
                    IDENS=1
C
C                   DENSRM WILL CREATE A GRID OF LINEARLY INTERPOLATED
C                   VALUES FROM THE COARSE GRID TO THE FINE GRID.
C
                  ELSE
C
                    WRITE(KFILDO,355)(JD(L),L=1,4)
 355                FORMAT(/' ****THE TWO GRIDS RETURNED BY OPTN2 IN', 
     1                    ' TIMTRPGOE HAVE DIFFERENT CHARACTERISTICS.'/,
     2                    ' VARIABLE',2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     3                    ' NOT ACCOMMODATED.')
                    IER=100
                    GO TO 800
C
                  ENDIF
C
               ENDIF
C               
C                 PLACE THE VALUES FROM THE LAST PROJECTION INTO XDATA( ).
C                 THIS IS NECESSARY BECAUSE THE INTERPOLATION EQUATION 
C                 CONTAINING THE FIRST AND LAST PROJECTIONS VALUES ARE A 
C                 FUNCTION OF YDATA( ) AND XDATA( ), RESPECTIVELY.
C
               DO 360 J=1,NX*NY
                  XDATA(J)=DATA(J)
 360           CONTINUE
C
            ENDIF
C
         ELSE
C
C              IF GFETCH IS SUCCESSFUL, NX BY NY VALUES ARE RETURNED. 
C
C              DATA RETURNED ARE GRIDDED.  ON FIRST TIME THROUGH THE
C              LOOP, SAVE GRID CHARACTERISTICS FROM GFETCH ON THE SECOND 
C              TIME, CHECK THE CHARACTERISTICS OF THE TWO GRIDS.
C
            IF(NTAU.EQ.IBEG)THEN
               NX=IS2(3)
               NY=IS2(4)
               NSLAB1=NSLAB
C
C                 FOR THE FIRST FIELD, TRANSFER IT TO YDATA( ).
C
               DO 675 J=1,NX*NY
                  YDATA(J)=XDATA(J)
 675           CONTINUE
C
            ELSEIF(NX.NE.IS2(3).OR.NY.NE.IS2(4).OR.NSLAB1.NE.NSLAB)THEN
C
                  NXCHK=((IS2(3)-1)*2)+1
                  NYCHK=((IS2(4)-1)*2)+1
C
                  IF(NX.EQ.NXCHK.AND.NY.EQ.NYCHK.AND.
     1               NGRIDC(5,NSLAB1).EQ.NGRIDC(5,NSLAB).AND.
     2               NGRIDC(6,NSLAB1).EQ.NGRIDC(6,NSLAB))THEN
C
C                 CHECK TO SEE IF THE DIFFERENT GRID ARE JUST A CHANGE
C                 IN RESOLUTION, BUT REALLY ARE THE SAME GRID.
C
C                 THE LOGIC HERE IS ASSUMING THAT THE COARSE GRID WILL
C                 ALWAYS BE IN A LATER FORECAST PROJECTION THAN THE FINE
C                 GRID.
C
                    CALL DENSRM(KFILDO,XDATA,IS2(3),IS2(4),ND2X3)
                    IDENS=1
C
C                   DENSRM WILL CREATE A GRID OF LINEARLY INTERPOLATED
C                   VALUES FROM THE COARSE GRID TO THE FINE GRID.
C
                  ELSE
C
                    WRITE(KFILDO,685)(JD(L),L=1,4)
 685                FORMAT(/' ****THE TWO GRIDS RETURNED BY OPTN2 IN',
     1                    ' TIMTRPGOE HAVE DIFFERENT CHARACTERISTICS.'/,
     2                    ' VARIABLE',2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     3                    ' NOT ACCOMMODATED.')
                    IER=100
                    GO TO 800
C
                  ENDIF
C
            ENDIF
C
         ENDIF
C
 700     CONTINUE
C
C           COMPUTE THE TIME INTERPOLATED VALUE.  PUT THE DATA
C           INTO DATA( ) FOR GRIDDED.  IT IS ASSUMED GRIDPOINT
C           DATA VALUES ARE NEVER MISSING.
C
         F=FLOAT(IADD)/(IEND-IBEG)
C
         
         DO 730 J=1,NX*NY
            DATA(J)=(XDATA(J)-YDATA(J))*F+YDATA(J)
 730     CONTINUE
C
      ELSE
         ISTAV=0
         WRITE(KFILDO,799)(JD(L),L=1,4)
 799     FORMAT(/,' ****ERROR IN TIMTRPGOE WITH VARIABLE',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2          /,' NOT HANDLED PROPERLY.  SET TO MISSING.')
         GO TO 800
C
      ENDIF
C
D     CALL TIMPR(KFILDO,KFILDO,'END   TIMTRPGOE GOOD   ')
C
C        INSERT COMMENTS HERE ABOUT SETTING NSLAB=1
C
      IF(IDENS.EQ.1) NSLAB=1
C
      GO TO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C        PUT THE DATA INTO SDATA( ) FOR VECTOR AND DATA( ) FOR
C        GRIDDED.
C
 800  IF(ISTAV.EQ.1)THEN
C
         DO 801 J=1,ND1
            SDATA(J)=9999.
 801     CONTINUE
C
      ELSE
C
         DO 802 J=1,ND2X3
            DATA(J)=9999.
 802     CONTINUE
C
      ENDIF
C
D     CALL TIMPR(KFILDO,KFILDO,'END   TIMTRPGOE ERROR  ')
 900  RETURN
      END      
