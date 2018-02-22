      SUBROUTINE U660(KFILDI,KFILDO,
     1                CCALL,ISDATA,SDATA,NGP,XDATA,MWORK,NAME,ND1,
     2                ID,IDPARS,TRESHL,TRESHU,JD,INDEX,JP,
     3                ITAU,IWDTH,WDTH,IPREC,PREC,CFMT,NWHERE,
     4                ISCALD,PRINT,HEAD,IPLAIN,PLAIN,L3264B,L3264W,ND4,
     5                AA,ND4X1,
     6                ICALLRA,CCALLRA,
     7                ICALLD,CCALLD,IPACK,DATA,IWORK,ND5,
     8                KFILIN,MODNUM,NAMIN,JFOPEN,MDATE,MAXTAU,
     9                MSDATE,INDEXC,ND6,
     A                IS0,IS1,IS2,IS4,ND7,
     B                IDATE,NWORK,ND8,
     C                LSTORE,MSTORE,ND9,
     D                CORE,ND10,NBLOCK)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***
C
C SUBPROGRAM: U660 
C   PRGMMR: GLAHN           ORG: W/OST22     DATE: 1998-01-01
C
C ABSTRACT: SUBPROGRAM U660 IS USED TO COLLATE INFORMATION FROM
C   ONE OR MORE MOS-2000 VECTOR SOURCES.  COLLATED DATA
C   CAN BE PRINTED OR WRITTEN TO A MOS-2000 VECTOR
C   OUTPUT.  THE PRINTING FORMAT IS GOVERNED BY USER
C   INPUT; THE WRITING PRECISION IS THE SAME AS INPUT.
C   THE VARIABLE ITAU( ) IS USED FOR THE LOOKAHEAD
C   FEATURE, THE SAME AS IN U850.  THE CAPABILITY OF
C   GROUPS OF STATIONS IS RETAINED FROM U600 EVEN THOUGH
C   IT IS EXPECTED THE NUMBER OF GROUPS USED IN U660
C   WILL BE 1.
C   THIS VERSION OF "U660" WAS MODIFIED SUBSTANTIALLY 
C   FROM THE ORIGINAL VERSION.  THE DOCBLOCK REFLECTS
C   A LOT OF EXTRA STUFF WHICH ISN'T NECESSARY.
C
C PROGRAM HISTORY LOG:
C   98-01-01  GLAHN   
C   01-03-02  MALONEY    ADDED NCEP DOCBLOCK, REMOVED
C                        UNNECESSARY CODE FOR MULTIPLE DAYS
C   01-04-03  MALONEY    CLEANED UP CALL TO VRBL66
C   05-02-28  DALLAVALLE ADDED LOGIC TO ALLOW FOR WRITING
C                        A RANDOM ACCESS OUTPUT FILE
C                        TO UNIT 49.  NOTE THAT THE MOS-2000
C                        SOFTWARE WILL ONLY ALLOW WRITING
C                        TO THIS FILE.  NOTE, TOO, THAT
C                        THE INPUT RANDOM ACCESS FILES
C                        WILL LIKELY BE ON UNITS 46 AND
C                        48.  THE NEED TO WRITE A RANDOM ACCESS
C                        FILE AND TO USE SUBROUTINE PACKRA
C                        CAUSED THE ADDITION OF TWO ARRAYS
C                        TO THE CALL TO U660, I.E., ARRAY
C                        ICALLRA AND CCALLRA.  THESE
C                        ARRAYS MUST BE EQUIVALENCED SO THAT
C                        PACKRA WORKS PROPERLY.
C                       
C USAGE:  CALL U660(KFILDI,KFILDO,CCALL,ISDATA,SDATA,NGP,
C                   XDATA,MWORK,NAME,ND1,ID,IDPARS,TRESHL,
C                   TRESHU,JD,INDEX,JP,ITAU,IWDTH,WDTH,IPREC,
C                   PREC,CFMT,NWHERE,ISCALD,PRINT,HEAD,IPLAIN,
C                   PLAIN,L3264B,L3264W,ND4,AA,ND4X1,
C                   ICALLRA,CCALLRA,ICALLD,
C                   CCALLD,IPACK,DATA,IWORK,ND5,KFILIN,MODNUM,
C                   NAMIN,JFOPEN,MDATE,MAXTAU,MSDATE,INDEXC,ND6,
C                   IS0,IS1,IS2,IS4,ND7,IDATE,NWORK,ND8,LSTORE,
C                   MSTORE,ND9,CORE,ND10,NBLOCK)
C   INPUT ARGUMENT LIST: 
C              KFILDI = UNIT NUMBER TO READ INPUT FILE 'U660.CN'.
C                       SET BY DATA STATEMENT TO 5 IN DRU660.  
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  THIS IS SET
C                       BY DATA STATEMENT TO 12 IN DRU660.  LATER, IN IPOPEN,
C                       IF IP(1) NE 0, KFILDO IS SET = IP(1).  THIS ALLOWS
C                       CHANGING THE "DEFAULT" PRINT FILE ON THE FLY.
C                       OTHERWISE, ON SOME SYSTEMS, THE OUTPUT FILE MIGHT
C                       HAVE THE SAME NAME AND BE OVERWRITTEN.  WHEN THE
C                       OUTPUT FILE IS NOT THE ORIGINAL DEFAULT, THE NAME
C                       IS GENERATED AND CAN BE DIFFERENT FOR EACH RUN.
C                       THIS ALLOWS SAVING EACH OUTPUT AND NOT HAVING IT
C                       OVERWRITTEN.
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST.  (CHARACTER*8)
C           ISDATA(K) = WORK ARRAY (K=1,NSTA).
C            SDATA(K) = WORK ARRAY (K=1,NSTA).  USED AS STALAT( ) IN INT660.
C              NGP(J) = THE NUMBER OF STATIONS IN EACH GROUP (J=1,KGP).
C            XDATA(K) = THE ARRAY USED BY SUBROUTINE VRBL61 AND VRBL62 FOR
C                       VECTOR VALUES (K=1,NSTA).  USED AS STALON( )
C                       IN INT660.
C            MWORK(J) = WORK ARRAY (J=1,ND1)
C             NAME(K) = NAMES OF STATIONS (K=1,NSTA)  (CHARACTER*20)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       STORAGE SPACE IS HIGHLY DEPENDENT ON ND1.
C             ID(J,N) = THE INTEGER VARIABLE ID'S (J=1,4) (N=1,NVRBL).
C         IDPARS(J,N) = THE PARSED, INDIVIDUAL COMPONENTS OF THE VARIABLE
C                       ID'S CORRESPONDING TO ID( ,N) (J=1,15), (N=1,NVRBL).
C                       (OUTPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                          0 = NOT BINARY,
C                          1 = CUMULATIVE FROM ABOVE, VALUES GE LOWER THRESHOLD
C                              TRESHL = 1,
C                          2 = CUMULATIVE FROM BELOW, VALUES LT UPPER THRESHOLD
C                              TRESHU = 1.
C                          3 = DISCRETE BINARY.  VALUES GE LOWER THRESHOLD AND
C                              LT UPPER THRESHOLD = 1.
C                          5 = GRID BINARY.  VALUES GE LOWER THRESHOLD
C                          ONLY THE VALUE OF 0, 1, OR 5 SHOULD BE USED FOR
C                          PREDICTORS;
C                          0, 1, 2, OR 3 CAN BE USED FOR PREDICTANDS.
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
C           TRESHL(N) = THE LOWER BINARY THRESHOLD CORRESPONDING TO IDPARS( ,N)
C                       (N=1,ND4).
C           TRESHU(N) = THE UPPER BINARY THRESHOLD CORRESPONDING TO IDPARS( ,N)
C                       (N=1,ND4).
C             JD(J,N) = THE BASIC INTEGER VARIABLE ID'S (J=1,4) (N=1,ND4).
C                       THIS IS THE SAME AS ID(J,N), EXCEPT THAT THE FOLLOWING
C                       PORTIONS ARE OMITTED:
C                       B = IDPARS(3, ),
C                       G = IDPARS(15, ), AND
C                       TRESHL( ).
C                       JD( , ) IS USED TO IDENTIFY WHICH CALCULATIONS
C                       CAN BE MADE DIRECTLY IN U660, WHICH IS ONLY FORMING
C                       BINARIES.  THE "G" VARIABLE HAS NO MEANING IN U660,
C                       IT BEING ONLY FOR POSSIBLE USE IN U201.
C            INDEX(N) = WORK ARRAY IN VRBL62 (N=1,ND4).
C             JP(J,N) = CONTROLS THE OUTPUT BY VARIABLE (N=1,ND4).
C                       J=1--INDICATES WHETHER (>0) OR NOT (=0) VARIABLE N
C                            WILL BE WRITTEN TO THE BINARY OUTPUT;
C                       J=2--INDICATES WHETHER (>0) OR NOT (=0) VARIABLE N
C                            WILL BE WRITTEN TO UNIT IP(16) WITH THE FORMAT 
C                            PROVIDED WITH THE VARIABLE; AND 
C                       J=3--INDICATES WHETHER (>0) OR NOT (=0) VARIABLE N
C                            WILL BE WRITTEN TO UNIT IP(15) NOT UNDER
C                            CONTROL OF THE FORMAT PROVIDED BUT TO THE
C                            RESOLUTION PACKED, PROVIDED JP(1,N).NE.0.
C             ITAU(N) = THE NUMBER OF HOURS TO ADD TO NDATE TO GET 
C                       THE VARIABLE N (N=1,ND4).  THIS IS THE 
C                       "LOOKAHEAD" FEATURE.
C            IWDTH(N) = WIDTH OF FIELD FOR PRINTING (N=1,ND4).
C             WDTH(N) = WIDTH OF FIELD FOR PRINTING (N=1,ND4).  COMPUTED
C                       AND USED ONLY IN PRU660.  (CHARACTER*2)
C            IPREC(N) = PRECISION FOR PRINTING (N=1,ND4).  THIS IS THE
C                       NUMBER OF PLACES AFTER THE DECIMAL POINT. 
C             PREC(N) = CHARACTER REPRESENTATION OF IPREC(J) (N=1,ND4).
C                       COMPUTED AND USED ONLY IN PRU660.  (CHARACTER*1)
C             CFMT(N) = FORMAT FOR PRINTING, EITHER I OR F (N=1,ND4).
C           NWHERE(N) = INDICATES WHERE THE VARIABLE IS TO COME FROM (N=1,ND4)
C                       0 = UNDETERMINED
C                       1 = FROM INPUT FILE
C                       2 = BINARY FROM BASIC VARIABLE
C                       3 = FROM OPTX.
C                       4 = CAN'T BE COMPUTED.
C           ISCALD(N) = THE DECIMAL SCALING CONSTANT TO USE WHEN PACKING THE 
C                       COLLATED DATA (N=1,ND4).  ISCALD COMES FROM THE
C                       VARIABLE CONSTANT FILE, MODIFIED TO BE 2 FOR GRID
C                       BINARIES, AND 0 FOR BINARIES.  ZERO WHEN NOT FOUND
C                       IN THE FILE.  NO BINARY SCALING IS PROVIDED FOR.
C            PRINT(N) = FURNISHED TO PRU660 FOR PRINTING.
C           HEAD(J,N) = HEADING FOR COLUMNS WHEN PRINTING DATA (J=1,30)
C                       (N=1,ND4).  (CHARACTER*1)
C       IPLAIN(L,J,N) = 32 CHARACTERS (L=1,L3264W) (J=1,4) OF PLAIN
C                       LANGUAGE DESCRIPTION OF VARIABLES (N=1,ND4).
C                       NOTE THAT THIS REQUIRES TWO 32-BIT WORDS TO HOLD
C                       THE DESCRIPTION BUT ONLY ONE 64-BIT WORD.
C                       EQUIVALENCED TO PLAIN( ).
C            PLAIN(N) = THE PLAIN LANGUAGE DESCRIPTION OF THE VARIABLES
C                       (N=1,ND4).  EQUIVALENCED TO IPLAIN( , , ).
C                       (CHARACTER*32)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING USED
C                       (EITHER 32 OR 64).  SET BY PARAMETER IN DRU660.
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).  
C                       CALCULATED BY PARAMETER, BASED ON L3464B.
C                 ND4 = MAXIMUM NUMBER OF VARIABLES THAT CAN BE DEALT WITH 
C                       IN ONE RUN. 
C               AA(J) = THE MATRIX OF DATA (J=1,ND4X1).
C                       ND4X1 = ND4*ND1, BUT THE ARRAY IS DEALT WITH 
C                       IN SUBROUTINES AS AA(NVRBL,NSTA) SO THAT THE
C                       PORTION ACTUALLY USED IS ALL TOGETHER AND CAN 
C                       BE DEALT WITH IN SOME INSTANCES AS A LINEAR BLOCK.  
C               ND4X1 = ND4*ND1 = SIZE OF AA( ).
C        ICALLRA(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN INTEGER
C                       VARIABLE (L=1,L3264W) (K=1,ND5).
C                       NOTE THAT THIS REQUIRES TWO 32-BIT WORDS TO HOLD
C                       THE 8 BYTE CHARACTER VARIABLE, BUT ONLY ONE
C                       64-BIT WORD.  EQUIVALENCED TO CCALLRA IN THE
C                       MAIN PROGRAM.  THIS ARRAY IS ONLY USED FOR THE
C                       PURPOSE OF CHECKING THE STATION DIRECTORY IN
C                       THE OUTPUT RANDOM ACCESS FILE.
C          CCALLRA(K) = 8 STATION CALL LETTERS (K=1,ND5).  THIS LIST IS 
C                       USED IN PACKRA FOR THE PURPOSE OF CHECKING
C                       THE STATION DICTIONARY IN THE OUTPUT
C                       RANDOM ACCESS FILE. 
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN INTEGER
C                       VARIABLE (L=1,L3264W) (K=1,ND5).
C                       NOTE THAT THIS REQUIRES TWO 32-BIT WORDS TO HOLD
C                       THE DESCRIPTION BUT ONLY ONE 64-BIT WORD.
C                       NEEDED IN CONST6 FOR ARGUMENT TO RDTDLM.
C                       EQUIVALENCED TO CCALLD( ).
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).  THIS LIST IS 
C                       USED IN RDSTAD TO RETAIN THE ORIGINAL LIST IN
C                       CCALL( , ).
C            IPACK(J) = PACKED DATA READ FROM THE INPUT FILE(S)
C                       (J=1,ND5).
C             DATA(J) = WORK ARRAY (J=1,ND5).
C            IWORK(J) = WORK ARRAY (J=1,ND5).  USED AS IWBAN( ) IN
C                       INT660.
C                 ND5 = THE MAXIMUM NUMBER OF STATION CALL LETTERS ON
C                       THE INPUT FILES.  DIMENSION OF IWORK( ),
C                       DATA( ), AND IPACK( ) AND SECOND DIMENSION OF
C                       ICALLD( , ).
C                       MUST BE GE THE LARGEST RECORD ON THE INPUT 
C                       VECTOR FILE(S).  MUST ALSO BE GE ND1.
C                       IT IS A SEPARATE DIMENSION FROM ND1, SO THAT 
C                       ONLY THOSE ARRAYS REQUIRING INPUT FROM INPUT
C                       FILES ARE THIS LARGE.
C           KFILIN(J) = UNIT NUMBERS FOR INPUT DATA, ALL IN MDL MOS-2000
C                       FORMAT (J=1,ND6).
C           MODNUM(J) = THE "MODEL" NUMBER CORRESPONDING TO KFILIN(J),
C                       AND NAMIN(J) (J=1,ND6).  NOT USED IN U660.
C                       MODNUM( ) IS CARRIED AS A DIMENSIONED VARIABLE
C                       TO BE COMPATIBLE WITH OTHER PROGRAMS, SUCH AS
C                       U201.
C            NAMIN(J) = HOLDS DATA SET NAMES FOR THE UNIT NUMBERS
C                       IN KFILIN(J) (J=1,ND6).  (CHARACTER*60)
C           JFOPEN(J) = FOR EACH FILE IN KFILIN(J), JFOPEN(J) IS SET
C                       TO 1 FOR J=1, MEANING THE FILE IS OPEN AND IS
C                       SET TO 2 FOR J GT 1 (IF ANY) MEANING THE FILE
C                       IS AVAILABLE, BUT NOT OPEN (J=1,NUMIN).  LATER,
C                       WHEN THE DATA ON A FILE ARE EXHAUSTED, JFOPEN( )
C                       IS SET TO 0, THE FILE CLOSED, AND THE NEXT ONE
C                       IN KFILIN( ), WITH JFOPEN( ) = 2, OPENED.
C            MDATE(J) = THE LAST DATE READ BY RDSTRX FOR DAY 1 FOR EACH
C                       FILE (J=1,ND6).  THIS IS USED TO KEEP A 
C                       DIAGNOSTIC FROM PRINTING IN RDVECT.
C           MAXTAU(J) = THE MAXIMUM TAU OF ANY PREDICTAND IN FILE J
C                       (J=1,ND6).
C           MSDATE(J) = KEEPS TRACK OF WHETHER ANY DATA ARE AVAILABLE
C                       FOR A PARTICULAR DATE ON AN INPUT FILE 
C                       (J=1,NUMIN).  USED FOR DIAGNOSTIC PRINT.
C         INDEXC(K,J) = LOCATIONS OF THE NSTA STATIONS (K=1,NSTA)
C                       IN THE LIST CCALL(K, ) IN REFERENCE TO THE 
C                       STATION CALL LETTERS RECORD ON THE INPUT
C                       DATA SET NUMBER J (J=1,NUMIN).
C                 ND6 = MAXIMUM NUMBER OF INPUT DATA SETS (MODELS) THAT 
C                       CAN BE DEALT WITH.
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,4).
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,21+).
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  MAXIMUM SIZE IS FOR
C                       IS1( ) = 22 PLUS 32 CHARACTERS (ONE CHARACTER PER
C                       WORD) OF PLAIN TEXT = 54.
C            IDATE(J) = INITIAL DATE LIST (J=1,NDATES) WHICH MAY CONTAIN
C                       NEGATIVE VALUES INDICATING A DATE SPAN.
C                       THIS IS MODIFIED IN DATPRO TO CONTAIN THE COMPLETE
C                       DATE LIST WITH THE DATES IN THE SPANS FILLED IN
C                       (J=1,NDATES), WHERE NDATES HAS BEEN INCREASED
C                       IF NECESSARY.  DATES ARE INPUT AS YYMMDDHH AND
C                       MODIFIED TO YYYYMMDDHH.  ZEROS IN THE INPUT ARE
C                       ELIMINATED.  TERMINATOR IS 99999999.  MAXIMUM
C                       NUMBER OF DATES IS ND8.
C            NWORK(J) = WORK ARRAY (J=1,ND8).
C                 ND8 = MAXIMUM NUMBER OF DATES THAT CAN BE DEALT WITH.
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED IN THE MOS-2000 STORAGE SYSTEM
C                       (L=1,12) (J=1,LITEMS).
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK, 
C                              THIS IS MINUS THE RECORD NUMBER WHERE 
C                              THE DATA START.
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN MDL GRIB, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --NOT USED.
C                       L=11 --THE NUMBER OF THE FIRST PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,VRBL) FOR WHICH THIS
C                              VARIABLE IS NEEDED, WHEN IT DOES NOT NEED
C                              TO BE STORED AFTER DAY 1.  WHEN THE VARIABLE
C                              MUST BE STORED (TO BE ACCESSED THROUGH OPTION)
C                              FOR ALL DAYS, ID(11,N) IS 7777 + THE NUMBER
C                              OF THE FIRST PREDICTOR IN THE SORTED LIST
C                              FOR WHICH THIS VARIABLE IS NEEDED.
C                       L=12 --USED INITIALLY IN ESTABLISHING MSTORE( , ).
C                              LATER USED AS A WAY OF DETERMINING WHETHER
C                              TO KEEP THIS VARIABLE.
C         MSTORE(L,J) = THE ARRAY HOLDING THE VARIABLES NEEDED AS INPUT, AFTER
C                       DAY 1, AND ASSOCIATED INFORMATION (L=1,8) (J=1,MITEMS).
C                       THIS ARRAY IS USED BY LMSTR6.
C                       J=1,4--THE 4 ID'S FOR THE DATA.
C                       J=5  --THE VALUE TAKEN FROM LSTORE(11, ) WHICH
C                              INDICATES WHETHER OR NOT TO STORE THE
C                              VARIABLE AND THE FIRST PREDICTOR TO USE IT FOR.
C                       J=6  --THE NUMBER OF HOURS AHEAD THE VARIABLE
C                              NEEDS TO BE SAVED.  THIS IS BASED ON THE
C                              INPUT FILE AND HOW FAR AHEAD IT NEEDS TO
C                              BE READ FOR THE LOOKAHEAD FEATURE.  FOR
C                              INSTANCE, IF A PREDICTOR IS ON THE SAME FILE
C                              AS A PREDICTAND THAT HAS A TAU OF 60 HOURS,
C                              THEN THERE WOULD BE AN ENTRY IN MSTORE( , )
C                              FOR EACH PROJECTION AHEAD IT NEEDS TO BE 
C                              SAVED AS THE FILE IS READ.  HOWEVER, IF
C                              THE PREDICTAND IS ON A SEPARATE FILE,
C                              THERE WOULD BE ONLY ONE ENTRY FOR THE 
C                              PREDICTOR, AND MSTORE(6, ) WOULD BE 0.
C                              MSTORE( , ) IS NOT CHANGED AFTER EXIT.
C                       J=7  --TAKEN FROM LSTORE(10, ).  THE NUMBER IN THE
C                              LIST OF UNIT NUMBERS FROM WHICH THE DATA CAME.
C                              STORED AS "IN" IN RDSTRX.  A ZERO MEANS THE
C                              VARIABLE WAS NOT FOUND ON DAY 1.
C                       J=8  --CALCULATED FROM THE MAXIMUM TAU MAXTAU(IN),
C                              WHERE "IN" IS THE INPUT FILE NUMBER IN 
C                              MSTORE(7, ), AND THE VALUE IN MSTORE(6, )
C                              FOR EACH NEW CYCLE.  THIS KEEPS CALLS TO 
C                              UPDAT TO A MINIMUM.
C                 ND9 = MAXIMUM NUMBER OF FIELDS STORED IN LSTORE( , )
C                       AND MSTORE( , ).  SECOND DIMENSION OF LSTORE( , ) 
C                       AND MSTORE( , ).
C             CORE(J) = SPACE ALLOCATED FOR SAVING PACKED DATA
C                       (J=1,ND10).  WHEN THIS SPACE IS EXHAUSTED,
C                       SCRATCH DISK WILL BE USED.
C                ND10 = THE MEMORY IN WORDS ALLOCATED TO THE SAVING OF 
C                       PACKED DATA IN CORE( ).  WHEN THIS
C                       SPACE IS EXHAUSTED, SCRATCH DISK WILL BE USED.
C              NBLOCK = BLOCK SIZE IN WORDS OF INTERNAL MOS-2000 DISK STORAGE.
C                       SINCE MUCH, IF NOT ALL, INTERNAL STORAGE WILL BE OF
C                       PACKED DATA, THE NUMBER OF BYTES WILL BE THE SAME FOR
C                       EITHER A 32- OR 64-BIT MACHINE.  THEREFORE, THE BLOCK
C                       SIZE IS SET BY PARAMETER TO VARY WITH L3264B.  IN THE
C                       PARAMETER STATEMENT, THE 6400 IS ARBITRARY, AND CAN BE
C                       CHANGED.  PERFORMANCE SHOULD NOT BE HIGHLY DEPENDENT
C                       ON THIS.  HOWEVER, IF TOO LARGE, SPACE WILL BE WASTED,
C                       AND IF TOO SMALL MANY RECORDS WILL BE NECESSARY TO
C                       HOLD EACH RECORD.  THE 6400 ACCOMMODATES 800 BYTES
C                       ON EITHER A 32- OR 64-BIT MACHINE.  SET BY PARAMETER
C                       IN DRU660.
C
C   OUTPUT ARGUMENT LIST: NONE.  ALL INPUT TO OTHER SUBROUTINES.
C
C        DATA SET USE
C        INPUT FILES:
C      FORT.KFILDI    - UNIT NUMBER OF INPUT FILE.  SET BY DATA
C                       STATEMENT IN DRU660.  (INPUT)
C      FORT.KFIL10    - UNIT NUMBER OF MDL MOS-2000 INTERNAL FILE
C                       SYSTEM ACCESS.  SET BY DATA STATEMENT.
C                       (INPUT-OUTPUT)
C      FORT.KFILIN(J) - UNIT NUMBERS FOR DATA INPUT FILES.  (INPUT)
C      FORT.KFILD(J)  - UNIT NUMBERS FOR WHERE THE STATION LIST (J=1)
C                       AND THE STATION DIRECTORY (J=2) RESIDES.
C                       (INPUT)
C      FORT.KFILDT    - UNIT NUMBER FOR READING THE DATE LIST.
C                       (INPUT)
C      FORT.KFILP     - UNIT NUMBER FOR READING THE VARIABLE LIST.
C                       (INPUT)
C      FORT.KFILCP    - UNIT NUMBER FOR VARIABLE CONSTANT FILE.
C                       (INPUT)
C      FORT.KFILRA(J) - UNIT NUMBERS FOR EXTERNAL RANDOM ACCESS FILES
C                       (J=1,5).  (INPUT/OUTPUT)
C
C        OUTPUT FILES: 
C      FORT.KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  SET BY
C                       DATA STATEMENT IN DRU660.  (OUTPUT)
C      FORT.KFIL10    - UNIT NUMBER OF MDL MOS-2000 INTERNAL FILE
C                       SYSTEM ACCESS.  SET BY DATA STATEMENT.
C                       (INPUT-OUTPUT)
C      FORT.KFILIO    - UNIT NUMBER OF SEQUENTIAL OUTPUT MDLPACK FILE.
C                       (OUTPUT)
C      FORT.IP(J)     - UNIT NUMBERS FOR OPTIONAL OUTPUT (J=1,25)
C                       (SEE IP( ) UNDER "VARIABLES" BELOW.)  (OUTPUT)
C
C        VARIABLES
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       STORAGE SPACE IS HIGHLY DEPENDENT ON ND1.
C                 ND4 = MAXIMUM NUMBER OF VARIABLES THAT CAN BE DEALT WITH 
C                       IN ONE RUN. 
C                 ND5 = THE MAXIMUM NUMBER OF STATION CALL LETTERS ON
C                       THE INPUT FILES.  DIMENSION OF IWORK( ),
C                       DATA( ), AND IPACK( ) AND SECOND DIMENSION OF
C                       ICALLD( , ).
C                       MUST BE GE THE LARGEST RECORD ON THE INPUT 
C                       VECTOR FILE(S).  MUST ALSO BE GE ND1.
C                       IT IS A SEPARATE DIMENSION FROM ND1, SO THAT 
C                       ONLY THOSE ARRAYS REQUIRING INPUT FROM INPUT
C                       FILES ARE THIS LARGE.
C                 ND6 = MAXIMUM NUMBER OF INPUT DATA SETS (MODELS) THAT 
C                       CAN BE DEALT WITH.
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  MAXIMUM SIZE IS FOR
C                       IS1( ) = 22 PLUS 32 CHARACTERS (ONE CHARACTER PER
C                       WORD) OF PLAIN TEXT = 54.
C                 ND8 = MAXIMUM NUMBER OF DATES THAT CAN BE DEALT WITH.
C                 ND9 = MAXIMUM NUMBER OF FIELDS STORED IN LSTORE( , )
C                       AND MSTORE( , ).  SECOND DIMENSION OF LSTORE( , ) 
C                       AND MSTORE( , ).
C                ND10 = THE MEMORY IN WORDS ALLOCATED TO THE SAVING OF 
C                       PACKED DATA IN CORE( ).  WHEN THIS
C                       SPACE IS EXHAUSTED, SCRATCH DISK WILL BE USED.
C              KFILDI = UNIT NUMBER TO READ INPUT FILE 'U660.CN'.
C                       SET BY DATA STATEMENT TO 5 IN DRU660.  
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  THIS IS SET
C                       BY DATA STATEMENT TO 12 IN DRU660.  LATER, IN IPOPEN,
C                       IF IP(1) NE 0, KFILDO IS SET = IP(1).  THIS ALLOWS
C                       CHANGING THE "DEFAULT" PRINT FILE ON THE FLY.
C                       OTHERWISE, ON SOME SYSTEMS, THE OUTPUT FILE MIGHT
C                       HAVE THE SAME NAME AND BE OVERWRITTEN.  WHEN THE
C                       OUTPUT FILE IS NOT THE ORIGINAL DEFAULT, THE NAME
C                       IS GENERATED AND CAN BE DIFFERENT FOR EACH RUN.
C                       THIS ALLOWS SAVING EACH OUTPUT AND NOT HAVING IT
C                       OVERWRITTEN.
C              KFIL10 = UNIT NUMBER OF MDL MOS-2000 FILE SYSTEM ACCESS.
C                       SET BY DATA STATEMENT.
C              KFILIO = UNIT NUMBER OF OUTPUT MDLPACK FILE.
C            KFILRAIO = UNIT NUMBER OF OUTPUT RANDOM ACCESS FILE
C                       CONTAINING TDLPACK VECTOR DATA.
C              IPINIT = 4 CHARACTERS USED TO HELP IDENTIFY OUTPUT ASSOCIATED
C                       WITH THE IP( ) NUMBERS.  (CHARACTER*4)
C               IP(J) = EACH VALUE (J=1,25) INDICATES WHETHER (>1)
C                       OR NOT (=0) CERTAIN INFORMATION WILL BE WRITTEN.
C                       WHEN IP( ) > 0, THE VALUE INDICATES THE UNIT
C                       NUMBER FOR OUTPUT.  THESE VALUES SHOULD NOT BE THE
C                       SAME AS ANY KFILX VALUES EXCEPT POSSIBLY
C                       KFILDO, WHICH IS THE DEFAULT OUTPUT FILE.  THIS IS
C                       ASCII OUTPUT, GENERALLY FOR DIAGNOSTIC PURPOSES.
C                       THE FILE NAMES WILL BE 4 CHARACTERS 'U660',
C                       THEN 4 CHARACTERS FROM IPINIT, THEN 2 CHARACTERS
C                       FROM IP(J) (E.G., 'U660HRG130').  THE ARRAY IS
C                       INITIALIZED TO ZERO IN CASE LESS THAN THE EXPECTED
C                       NUMBER OF VALUES ARE READ IN.  EACH OUTPUT ASCII
C                       FILE WILL BE TIME STAMPED.  NOTE THAT THE TIME
C                       ON EACH FILE SHOULD BE VERY NEARLY THE SAME, BUT
C                       COULD VARY BY A FRACTION OF A SECOND.  IT IS 
C                       INTENDED THAT ALL ERRORS BE INDICATED ON THE
C                       DEFAULT, SOMETIMES IN ADDITION TO BEING INDICATED
C                       ON A FILE WITH A SPECIFIC IP( ) NUMBER, SO THAT
C                       THE USER WILL NOT MISS AN ERROR.
C                       (1) = ALL ERRORS AND OTHER INFORMATION NOT
C                           SPECIFICALLY IDENTIFIED WITH OTHER IP( )
C                           NUMBERS.  WHEN IP(1) IS READ AS NONZERO,
C                           KFILDO, THE DEFAULT OUTPUT FILE UNIT NUMBER,
C                           WILL BE SET TO IP(1).  WHEN IP(1) IS READ
C                           AS ZERO, KFILDO WILL BE USED UNCHANGED.
C                       (2) = THE INPUT DATES IN IDATE( ).  WHEN THERE
C                           ARE ERRORS, PRINT WILL BE TO UNIT KFILDO AS 
C                           WELL AS TO UNIT IP(2).
C                       (3) = THE OUTPUT DATES IN IDATE( ).  WHEN THERE
C                           ARE ERRORS, OUTPUT WILL BE TO UNIT KFILDO AS 
C                           WELL AS TO UNIT IP(3).
C                       (4) = THE INPUT STATION LIST (CALL LETTERS ONLY).
C                           IF THERE ARE INPUT ERRORS, THE STATION LIST
C                           WILL BE WRITTEN TO THE DEFAULT OUTPUT FILE UNIT
C                           KFILDO AS WELL AS TO UNIT IP(4).
C                       (5) = THE STATIONS AND STATION DIRECTORY INFORMATION
C                           IN THE ORDER TO BE DEALT WITH IN U660.  THE
C                           STATIONS WILL BE IN ALPHABETICAL ORDER WITHIN
C                           EACH GROUP PROVIDED THE DIRECTORY IS.  IF THERE
C                           ARE INPUT ERRORS, THE STATION LIST WILL BE 
C                           WRITTEN TO THE DEFAULT OUTPUT FILE UNIT KFILDO
C                           AS WELL AS TO UNIT IP(5). 
C                       (6) = THE VARIABLE IDS AS THEY ARE BEING READ IN.
C                           THIS IS GOOD FOR CHECKOUT; FOR ROUTINE
C                           OPERATION, IP(7), IP(8), AND/OR IP(9),
C                           MAY BE BETTER.  
C                       (7) = THE VARIABLE LIST IN SUMMARY FORM.
C                           IF THERE ARE ERRORS, THE VARIABLE LIST WILL 
C                           BE WRITTEN TO THE DEFAULT OUTPUT FILE 
C                           UNIT KFILDO AS WELL AS TO UNIT IP(7).
C                           THIS LIST INCLUDES THE PARSED ID'S IN IDPARS( , ).
C                       (8) = THE VARIABLE LIST IN SUMMARY FORM.
C                           THIS LIST INCLUDES THE PARSED ID'S IN 
C                           IDPARS( , ).
C                       (9) = THE VARIABLE LIST IN SUMMARY FORM.
C                           THIS DIFFERS FROM (8) IN THAT (9) DOES NOT 
C                           INCLUDE THE PARSED ID'S IN IDPARS( , ),
C                           BUT RATHER INCLUDES THE INFORMATION TAKEN
C                           FROM THE PREDICTOR CONSTANT FILE.
C                       (10) = INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                           FIELDS READ FOR DAY 1 WILL BE PRINTED TO THE FILE
C                           WHOSE UNIT NUMBER IS IP(10).
C                       (11) = THE VARIABLE ID'S OF THE ARCHIVED FIELDS
C                           ACTUALLY NEEDED, IN ORDER AS THEY APPEAR ON
C                           THE ARCHIVE TAPES.
C                       (12) = INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                           STATIONS ON THE INPUT FILES WILL BE PRINTED TO 
C                           THE FILE WHOSE UNIT NUMBER IS IP(12).
C                       (14) = INDICATES WHETHER (>0) OR NOT (=0) A DIAGNOSTIC
C                           WILL BE PROVIDED ON UNIT IP(14) WHEN THERE ARE
C                           NO DATA FOR A DESIRED DATE/TIME ON A PARTICULAR
C                           INPUT FILE.  THIS MIGHT HAPPEN FOR EACH DATE/TIME
C                           AND A LOT OF OUTPUT BE CREATED.  (WITH THE
C                           SWITCHING OF DATA SETS, THIS IS NOT OF MUCH USE,
C                           AND MAY BE MISLEADING.  IT IS ACTIVATED ONLY 
C                           ON THE /D COMPILE IN RDVECT.) 
C                       (15) = INDICATES WHETHER (>0) OR NOT (=0) THE DATA
C                           WILL BE WRITTEN TO UNIT IP(15) WHEN JP(3, ) >0.
C                           THIS PRINT IS LIKE THAT PROVIDED BY U201, AND
C                           IS SEPARATE FROM THE OPTIONAL LISTING PROVIDED
C                           UNDER CONTROL OF JP(2, ) WITH THE FORMAT 
C                           PROVIDED.
C                       (16) = INDICATES WHETHER (>0) OR NOT (=0) THE DATA
C                           WILL BE WRITTEN TO UNIT IP(16) WHEN JP(2, ) >0.
C                           THIS PRINT IS UNDER CONTROL OF THE FORMAT
C                           PROVIDED WITH EACH VARIABLE.
C                       (23) = INDICATES WHETHER (>0) OR NOT (=0) STATEMENTS
C                           ABOUT EOF AND FILE OPENINGS AND CLOSINGS WILL
C                           BE OUTPUT FOR PRINTING ON UNIT IP(23).
C                       (24) = INDICATES WHETHER (>0) OR NOT (=0) A LISTING
C                           OF WHERE THE VARIABLES ARE TO BE FOUND IS
C                           CREATED FOR PRINTING ON UNIT IP(24).  THESE
C                           ARE THE VALUES OF NWHERE.
C               KSKIP = WHEN NONZERO, INDICATES THAT THE OUTPUT FILE
C                       IS TO BE MOVED FORWARD UNTIL ALL DATA FOR
C                       DATE KSKIP HAVE BEEN SKIPPED.  KSKIP IS INPUT
C                       AS YYMMDDHH OR YYYYMMDDHH AND THEN USED AS
C                       YYYYMMDDHH.
C               NSKIP = THE NUMBER OF ERRORS THAT WILL BE TOLERATED ON DAY 1
C                       WITHOUT HALTING.
C               JSTOP = THE NUMBER OF ERRORS THAT WILL BE TOLERATED ON THE
C                       TOTAL RUN BEFORE PROGRAM STOPS.
C              PXMISS = THE VALUE OF A SECONDARY MISSING VALUE TO INSERT
C                       WHEN THE SECONDARY MISSING VALUE IS 9997.
C                       THIS ALLOWS MAINTAINING A 9997, TREATING IT AS 
C                       ZERO, AS 9999, OR AS SOME OTHER VALUE.
C                       PXMISS APPLIES TO ALL VARIABLES.
C              NPRINT = THE NUMBER OF CYCLES OF DATA TO PRINT UNDER
C                       JP(2, ) CONTROL.
C            IDATE(J) = INITIAL DATE LIST (J=1,NDATES) WHICH MAY CONTAIN
C                       NEGATIVE VALUES INDICATING A DATE SPAN.
C                       THIS IS MODIFIED IN DATPRO TO CONTAIN THE COMPLETE
C                       DATE LIST WITH THE DATES IN THE SPANS FILLED IN
C                       (J=1,NDATES), WHERE NDATES HAS BEEN INCREASED
C                       IF NECESSARY.  DATES ARE INPUT AS YYMMDDHH AND
C                       MODIFIED TO YYYYMMDDHH.  ZEROS IN THE INPUT ARE
C                       ELIMINATED.  TERMINATOR IS 99999999.  MAXIMUM
C                       NUMBER OF DATES IS ND8.
C              NDATES = THE NUMBER OF DATES IN IDATE( ).
C           KFILIN(J) = UNIT NUMBERS FOR INPUT DATA, ALL IN MDL MOS-2000
C                       FORMAT (J=1,ND6).
C           MODNUM(J) = THE "MODEL" NUMBER CORRESPONDING TO KFILIN(J),
C                       AND NAMIN(J) (J=1,ND6).  NOT USED IN U660.
C                       MODNUM( ) IS CARRIED AS A DIMENSIONED VARIABLE
C                       TO BE COMPATIBLE WITH OTHER PROGRAMS, SUCH AS
C                       U201.
C            NAMIN(J) = HOLDS DATA SET NAMES FOR THE UNIT NUMBERS
C                       IN KFILIN(J) (J=1,ND6).  (CHARACTER*60)
C           JFOPEN(J) = FOR EACH FILE IN KFILIN(J), JFOPEN(J) IS SET
C                       TO 1 FOR J=1, MEANING THE FILE IS OPEN AND IS
C                       SET TO 2 FOR J GT 1 (IF ANY) MEANING THE FILE
C                       IS AVAILABLE, BUT NOT OPEN (J=1,NUMIN).  LATER,
C                       WHEN THE DATA ON A FILE ARE EXHAUSTED, JFOPEN( )
C                       IS SET TO 0, THE FILE CLOSED, AND THE NEXT ONE
C                       IN KFILIN( ), WITH JFOPEN( ) = 2, OPENED.
C            MDATE(J) = THE LAST DATE READ BY RDSTRX FOR DAY 1 FOR EACH
C                       FILE (J=1,ND6).  THIS IS USED TO KEEP A 
C                       DIAGNOSTIC FROM PRINTING IN RDVECT.
C           MAXTAU(J) = THE MAXIMUM TAU OF ANY PREDICTAND IN FILE J
C                       (J=1,ND6).
C           MSDATE(J) = KEEPS TRACK OF WHETHER ANY DATA ARE AVAILABLE
C                       FOR A PARTICULAR DATE ON AN INPUT FILE 
C                       (J=1,NUMIN).  USED FOR DIAGNOSTIC PRINT.
C               NUMIN = THE NUMBER OF VALUES IN KFILIN( ), NAMES IN
C                       NAMIN( ), AND OTHER VARIABLES WITH DIMENSION ND6.
C                       MAXIMUM OF ND6.
C           KFILRA(J) = UNIT NUMBERS FOR READING CONSTANT DATA (J=1,5).
C           RACESS(J) = FILE NAMES FOR CONSTANT DATA READ ON UNIT
C                       NOS. KFILRA(J) (J=1,5).  (CHARACTER*60)
C               NUMRA = NUMBER OF VALUES IN KFILRA( ) AND RACESS( ).
C              OUTNAM = NAME OF DATA SET FOR PACKED VECTOR OUTPUT TO
C                       BE WRITTEN TO UNIT KFILIO.  (CHARACTER*60)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST.  (CHARACTER*8)
C           ISDATA(K) = WORK ARRAY (K=1,NSTA).
C                NSTA = THE NUMBER OF STATIONS BEING DEALT WITH.  THE
C                       NUMBER OF VALUES IN CCALL( , ), ETC.  MAXIMUM
C                       OF ND1.
C                 KGP = THE NUMBER OF GROUPS OF STATIONS TO BE PROCESSED.
C                       MAXIMUM OF ND1.
C              NGP(J) = THE NUMBER OF STATIONS IN EACH GROUP (J=1,KGP).
C             NAME(K) = NAMES OF STATIONS (K=1,NSTA)  (CHARACTER*20)
C            MWORK(J) = WORK ARRAY (J=1,ND1)
C             ID(J,N) = THE INTEGER VARIABLE ID'S (J=1,4) (N=1,NVRBL).
C         IDPARS(J,N) = THE PARSED, INDIVIDUAL COMPONENTS OF THE VARIABLE
C                       ID'S CORRESPONDING TO ID( ,N) (J=1,15), (N=1,NVRBL).
C                       (OUTPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                          0 = NOT BINARY,
C                          1 = CUMULATIVE FROM ABOVE, VALUES GE LOWER THRESHOLD
C                              TRESHL = 1,
C                          2 = CUMULATIVE FROM BELOW, VALUES LT UPPER THRESHOLD
C                              TRESHU = 1.
C                          3 = DISCRETE BINARY.  VALUES GE LOWER THRESHOLD AND
C                              LT UPPER THRESHOLD = 1.
C                          5 = GRID BINARY.  VALUES GE LOWER THRESHOLD
C                          ONLY THE VALUE OF 0, 1, OR 5 SHOULD BE USED FOR
C                          PREDICTORS;
C                          0, 1, 2, OR 3 CAN BE USED FOR PREDICTANDS.
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
C           TRESHL(N) = THE LOWER BINARY THRESHOLD CORRESPONDING TO IDPARS( ,N)
C                       (N=1,ND4).
C           TRESHU(N) = THE UPPER BINARY THRESHOLD CORRESPONDING TO IDPARS( ,N)
C                       (N=1,ND4).
C             JD(J,N) = THE BASIC INTEGER VARIABLE ID'S (J=1,4) (N=1,ND4).
C                       THIS IS THE SAME AS ID(J,N), EXCEPT THAT THE FOLLOWING
C                       PORTIONS ARE OMITTED:
C                       B = IDPARS(3, ),
C                       G = IDPARS(15, ), AND
C                       TRESHL( ).
C                       JD( , ) IS USED TO IDENTIFY WHICH CALCULATIONS
C                       CAN BE MADE DIRECTLY IN U660, WHICH IS ONLY FORMING
C                       BINARIES.  THE "G" VARIABLE HAS NO MEANING IN U660,
C                       IT BEING ONLY FOR POSSIBLE USE IN U201.
C             JP(J,N) = CONTROLS THE OUTPUT BY VARIABLE (N=1,ND4).
C                       J=1--INDICATES WHETHER (>0) OR NOT (=0) VARIABLE N
C                            WILL BE WRITTEN TO THE BINARY OUTPUT;
C                       J=2--INDICATES WHETHER (>0) OR NOT (=0) VARIABLE N
C                            WILL BE WRITTEN TO UNIT IP(16) WITH THE FORMAT 
C                            PROVIDED WITH THE VARIABLE; AND 
C                       J=3--INDICATES WHETHER (>0) OR NOT (=0) VARIABLE N
C                            WILL BE WRITTEN TO UNIT IP(15) NOT UNDER
C                            CONTROL OF THE FORMAT PROVIDED BUT TO THE
C                            RESOLUTION PACKED, PROVIDED JP(1,N).NE.0.
C             ITAU(N) = THE NUMBER OF HOURS TO ADD TO NDATE TO GET 
C                       THE VARIABLE N (N=1,ND4).  THIS IS THE 
C                       "LOOKAHEAD" FEATURE.
C               LNGTH = LINE LENGTH FOR PRINTING TO IP(16).  (OUTPUT)
C              ICHARS = NUMBER OF CHARACTERS FOR CALL LETTERS IN 
C                       PRINTING, MIN OF 4, MAX OF 8.  INT660 ASSURES
C                       THIS RANGE.
C            IWDTH(N) = WIDTH OF FIELD FOR PRINTING (N=1,ND4).
C             WDTH(N) = WIDTH OF FIELD FOR PRINTING (N=1,ND4).  COMPUTED
C                       AND USED ONLY IN PRU660.  (CHARACTER*2)
C            IPREC(N) = PRECISION FOR PRINTING (N=1,ND4).  THIS IS THE
C                       NUMBER OF PLACES AFTER THE DECIMAL POINT. 
C             PREC(N) = CHARACTER REPRESENTATION OF IPREC(J) (N=1,ND4).
C                       COMPUTED AND USED ONLY IN PRU660.  (CHARACTER*1)
C             CFMT(N) = FORMAT FOR PRINTING, EITHER I OR F (N=1,ND4).
C       IPLAIN(L,J,N) = 32 CHARACTERS (L=1,L3264W) (J=1,4) OF PLAIN
C                       LANGUAGE DESCRIPTION OF VARIABLES (N=1,ND4).
C                       NOTE THAT THIS REQUIRES TWO 32-BIT WORDS TO HOLD
C                       THE DESCRIPTION BUT ONLY ONE 64-BIT WORD.
C                       EQUIVALENCED TO PLAIN( ).
C            PLAIN(N) = THE PLAIN LANGUAGE DESCRIPTION OF THE VARIABLES
C                       (N=1,ND4).  EQUIVALENCED TO IPLAIN( , , ).
C                       (CHARACTER*32)
C            INDEX(N) = WORK ARRAY IN VRBL62 (N=1,ND4).
C           NWHERE(N) = INDICATES WHERE THE VARIABLE IS TO COME FROM (N=1,ND4)
C                       0 = UNDETERMINED
C                       1 = FROM INPUT FILE
C                       2 = BINARY FROM BASIC VARIABLE
C                       3 = FROM OPTX.
C                       4 = CAN'T BE COMPUTED.
C           ISCALD(N) = THE DECIMAL SCALING CONSTANT TO USE WHEN PACKING THE 
C                       COLLATED DATA (N=1,ND4).  ISCALD COMES FROM THE
C                       VARIABLE CONSTANT FILE, MODIFIED TO BE 2 FOR GRID
C                       BINARIES, AND 0 FOR BINARIES.  ZERO WHEN NOT FOUND
C                       IN THE FILE.  NO BINARY SCALING IS PROVIDED FOR.
C            PRINT(N) = FURNISHED TO PRU660 FOR PRINTING.
C           HEAD(J,N) = HEADING FOR COLUMNS WHEN PRINTING DATA (J=1,30)
C                       (N=1,ND4).  (CHARACTER*1)
C               NVRBL = THE NUMBER OF VARIABLES.
C               AA(J) = THE MATRIX OF DATA (J=1,ND4X1).
C                       ND4X1 = ND4*ND1, BUT THE ARRAY IS DEALT WITH 
C                       IN SUBROUTINES AS AA(NVRBL,NSTA) SO THAT THE
C                       PORTION ACTUALLY USED IS ALL TOGETHER AND CAN 
C                       BE DEALT WITH IN SOME INSTANCES AS A LINEAR BLOCK.  
C               ND4X1 = ND4*ND1 = SIZE OF AA( ).
C            XDATA(K) = THE ARRAY USED BY SUBROUTINE VRBL61 AND VRBL62 FOR
C                       VECTOR VALUES (K=1,NSTA).  USED AS STALON( )
C                       IN INT660.
C            SDATA(K) = WORK ARRAY (K=1,NSTA).  USED AS STALAT( ) IN INT660.
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN INTEGER
C                       VARIABLE (L=1,L3264W) (K=1,ND5).
C                       NOTE THAT THIS REQUIRES TWO 32-BIT WORDS TO HOLD
C                       THE DESCRIPTION BUT ONLY ONE 64-BIT WORD.
C                       NEEDED IN CONST6 FOR ARGUMENT TO RDTDLM.
C                       EQUIVALENCED TO CCALLD( ).
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).  THIS LIST IS 
C                       USED IN RDSTAD TO RETAIN THE ORIGINAL LIST IN
C                       CCALL( , ).
C            IPACK(J) = PACKED DATA READ FROM THE INPUT FILE(S)
C                       (J=1,ND5).
C            IWORK(J) = WORK ARRAY (J=1,ND5).  USED AS IWBAN( ) IN
C                       INT660.
C             DATA(J) = WORK ARRAY (J=1,ND5).
C         INDEXC(K,J) = LOCATIONS OF THE NSTA STATIONS (K=1,NSTA)
C                       IN THE LIST CCALL(K, ) IN REFERENCE TO THE 
C                       STATION CALL LETTERS RECORD ON THE INPUT
C                       DATA SET NUMBER J (J=1,NUMIN).
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,4).
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,21+).
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).
C            NWORK(J) = WORK ARRAY (J=1,ND8).
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED IN THE MOS-2000 STORAGE SYSTEM
C                       (L=1,12) (J=1,LITEMS).
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK, 
C                              THIS IS MINUS THE RECORD NUMBER WHERE 
C                              THE DATA START.
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN MDL GRIB, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --NOT USED.
C                       L=11 --THE NUMBER OF THE FIRST PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,VRBL) FOR WHICH THIS
C                              VARIABLE IS NEEDED, WHEN IT DOES NOT NEED
C                              TO BE STORED AFTER DAY 1.  WHEN THE VARIABLE
C                              MUST BE STORED (TO BE ACCESSED THROUGH OPTION)
C                              FOR ALL DAYS, ID(11,N) IS 7777 + THE NUMBER
C                              OF THE FIRST PREDICTOR IN THE SORTED LIST
C                              FOR WHICH THIS VARIABLE IS NEEDED.
C                       L=12 --USED INITIALLY IN ESTABLISHING MSTORE( , ).
C                              LATER USED AS A WAY OF DETERMINING WHETHER
C                              TO KEEP THIS VARIABLE.
C         MSTORE(L,J) = THE ARRAY HOLDING THE VARIABLES NEEDED AS INPUT, AFTER
C                       DAY 1, AND ASSOCIATED INFORMATION (L=1,8) (J=1,MITEMS).
C                       THIS ARRAY IS USED BY LMSTR6.
C                       J=1,4--THE 4 ID'S FOR THE DATA.
C                       J=5  --THE VALUE TAKEN FROM LSTORE(11, ) WHICH
C                              INDICATES WHETHER OR NOT TO STORE THE
C                              VARIABLE AND THE FIRST PREDICTOR TO USE IT FOR.
C                       J=6  --THE NUMBER OF HOURS AHEAD THE VARIABLE
C                              NEEDS TO BE SAVED.  THIS IS BASED ON THE
C                              INPUT FILE AND HOW FAR AHEAD IT NEEDS TO
C                              BE READ FOR THE LOOKAHEAD FEATURE.  FOR
C                              INSTANCE, IF A PREDICTOR IS ON THE SAME FILE
C                              AS A PREDICTAND THAT HAS A TAU OF 60 HOURS,
C                              THEN THERE WOULD BE AN ENTRY IN MSTORE( , )
C                              FOR EACH PROJECTION AHEAD IT NEEDS TO BE 
C                              SAVED AS THE FILE IS READ.  HOWEVER, IF
C                              THE PREDICTAND IS ON A SEPARATE FILE,
C                              THERE WOULD BE ONLY ONE ENTRY FOR THE 
C                              PREDICTOR, AND MSTORE(6, ) WOULD BE 0.
C                              MSTORE( , ) IS NOT CHANGED AFTER EXIT.
C                       J=7  --TAKEN FROM LSTORE(10, ).  THE NUMBER IN THE
C                              LIST OF UNIT NUMBERS FROM WHICH THE DATA CAME.
C                              STORED AS "IN" IN RDSTRX.  A ZERO MEANS THE
C                              VARIABLE WAS NOT FOUND ON DAY 1.
C                       J=8  --CALCULATED FROM THE MAXIMUM TAU MAXTAU(IN),
C                              WHERE "IN" IS THE INPUT FILE NUMBER IN 
C                              MSTORE(7, ), AND THE VALUE IN MSTORE(6, )
C                              FOR EACH NEW CYCLE.  THIS KEEPS CALLS TO 
C                              UPDAT TO A MINIMUM.
C              MITEMS = THE NUMBER OF ITEMS IN MSTORE( , ).
C             CORE(J) = SPACE ALLOCATED FOR SAVING PACKED DATA
C                       (J=1,ND10).  WHEN THIS SPACE IS EXHAUSTED,
C                       SCRATCH DISK WILL BE USED.
C              NBLOCK = BLOCK SIZE IN WORDS OF INTERNAL MOS-2000 DISK STORAGE.
C                       SINCE MUCH, IF NOT ALL, INTERNAL STORAGE WILL BE OF
C                       PACKED DATA, THE NUMBER OF BYTES WILL BE THE SAME FOR
C                       EITHER A 32- OR 64-BIT MACHINE.  THEREFORE, THE BLOCK
C                       SIZE IS SET BY PARAMETER TO VARY WITH L3264B.  IN THE
C                       PARAMETER STATEMENT, THE 6400 IS ARBITRARY, AND CAN BE
C                       CHANGED.  PERFORMANCE SHOULD NOT BE HIGHLY DEPENDENT
C                       ON THIS.  HOWEVER, IF TOO LARGE, SPACE WILL BE WASTED,
C                       AND IF TOO SMALL MANY RECORDS WILL BE NECESSARY TO
C                       HOLD EACH RECORD.  THE 6400 ACCOMMODATES 800 BYTES
C                       ON EITHER A 32- OR 64-BIT MACHINE.  SET BY PARAMETER
C                       IN DRU660.
C            ISTOP(J) = FOR J=1, ISTOP IS INCREMENTED BY 1 EACH TIME
C                       AN ERROR OCCURS THAT MAY BE FATAL.
C                       FOR J=2, ISTOP IS INCREMENTED BY 1 WHENEVER AN
C                       INPUT DATA RECORD IS NOT FOUND.
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING USED
C                       (EITHER 32 OR 64).  SET BY PARAMETER IN DRU660.
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).  
C                       CALCULATED BY PARAMETER, BASED ON L3464B.
C               MINPK = MINIMUM GROUP SIZE WHEN PACKING THE INTERPOLATED
C                       VALUES.  SET IN DATA STATEMENT TO 14, THE AGREED
C                       ON VALUE FOR MOS-2000.
C               LASTL = THE LAST LOCATION IN CORE( ) USED.  THIS MAY BE
C                       MODIFIED, ALONG WITH ITEMS, IF COMPACTION IS
C                       DONE BY GCPAC.  INITIALIZED TO ZERO ON FIRST 
C                       ENTRY TO GSTORE.  ALSO SET TO ZERO IN U660 IN
C                       CASE GSTORE IS NOT ENTERED.
C               LASTD = TOTAL NUMBER OF PHYSICAL RECORDS ON DISK.  INITIALIZED
C                       TO ZERO ON FIRST ENTRY TO GSTORE.  ALSO SET TO
C                       ZERO IN U660 IN CASE GSTORE IS NOT ENTERED.
C              NSTORE = THE NUMBER OF TIMES GSTORE HAS BEEN ENTERED.
C              NFETCH = THE NUMBER OF TIMES GFETCH HAS BEEN ENTERED.
C              NWORDS = NUMBER OF WORDS IN IPACK( ) RETUNED FROM PACKV.
C              NTOTBY = THE TOTAL NUMBER OF BYTES ON THE FILE ASSOCIATED
C                       WITH UNIT NO. KFILIO (THE OUTPUT FILE).
C                       IT IS INITIALIZED BY SKIPWR AND UPDATED AS
C                       DATA ARE WRITTEN.  (THIS DOES NOT ACCOUNT FOR
C                       ANY BYTES WRITTEN BY THE SYSTEM THAT ARE NOT
C                       PART OF THE FORTRAN WRITES.  THIS IS PROBABLY
C                       8 BYTES PER RECORD.)
C              NTOTRC = THE TOTAL NUMBER OF RECORDS IN THE FILE.  IT IS
C                       INITIALIZED BY SKIPWR AND UPDATED AS DATA ARE
C                       WRITTEN.  
C 
C        SUBPROGRAMS CALLED:
C             UNIQUE:   - INT660, VRBL66, PACKRA_OPER
C          LIBRARY:
C             MDLLIB90  - RDSTRX, PRU660, TRDATA, SETMIS, PACKV, TRAIL
C                W3LIB  - W3TAGE
C
C        EXIT STATES:
C          COND =    0  - SUCCESSFUL RUN
C                  130  - FATAL ERROR READING FILE
C                  241  - TOO MANY ERRORS ( >= NSKIP)
C
C REMARKS: ONLY WILL WORK FOR ONE DATE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90 (xlf90 compiler)
C   MACHINE:  IBM SP
C
C$$$
C
      CHARACTER*1 CFMT(ND4),HEAD(30,ND4),PREC(ND4)
      CHARACTER*2 WDTH(ND4)
      CHARACTER*4 IPINIT
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5),CCALLRA(ND5)
      CHARACTER*20 NAME(ND1)
      CHARACTER*32 PLAIN(ND4)
      CHARACTER*60 NAMIN(ND6),OUTNAM,RACESS(5)
C
      DIMENSION ISDATA(ND1),SDATA(ND1),NGP(ND1),XDATA(ND1),
     2          MWORK(ND1)
      DIMENSION ID(4,ND4),IDPARS(15,ND4),TRESHL(ND4),TRESHU(ND4),
     1          JD(4,ND4),INDEX(ND4),JP(3,ND4),ITAU(ND4),
     2          IWDTH(ND4),IPREC(ND4),NWHERE(ND4),ISCALD(ND4),
     3          PRINT(ND4)
      DIMENSION IPLAIN(L3264W,4,ND4)
      DIMENSION AA(ND4X1)
      DIMENSION IPACK(ND5),ICALLD(L3264W,ND5),ICALLRA(L3264W,ND5),
     1          IWORK(ND5),DATA(ND5)
      DIMENSION KFILIN(ND6),MODNUM(ND6),MSDATE(ND6),
     1          JFOPEN(ND6),MDATE(ND6),MAXTAU(ND6)
      DIMENSION INDEXC(ND1,ND6)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION IDATE(ND8),NWORK(ND8)
      DIMENSION LSTORE(12,ND9),MSTORE(8,ND9)
      DIMENSION CORE(ND10)
      DIMENSION IP(25),ISTOP(2),KFILRA(5)
C
      DATA ISTOP/0,0/
      DATA KFIL10/99/
      DATA IP/25*0/
      DATA LASTL/0/,
     1     LASTD/0/
      DATA NFETCH/0/,
     1     NSTORE/0/
      DATA MINPK/14/
      DATA NTOTBY/0/,JTOTBY/0/,
     1     NTOTRC/0/,JTOTRC/0/
C
C        READ CONTROL INFORMATION.
C
      CALL INT660(KFILDI,KFILDO,KFILIO,IP,
     1            CCALL,MWORK,IWORK,SDATA,XDATA,
     2            ISDATA,IPACK,NGP,KGP,NAME,NSTA,ND1,CCALLD,ND5,
     3            ID,IDPARS,TRESHL,TRESHU,JD,JP,
     4            ITAU,ICHARS,IWDTH,IPREC,CFMT,ISCALD,HEAD,
     5            PLAIN,L3264B,L3264W,ND4,
     6            KFILIN,MODNUM,NAMIN,JFOPEN,NUMIN,ND6,
     7            KFILRA,RACESS,NUMRA,OUTNAM,KFILRAIO,
     8            IDATE,NDATES,NWORK,ND8,
     9            KSKIP,NSKIP,JSTOP,PXMISS,NPRINT,
     A            NVRBL,LNGTH,
     B            NTOTBY,NTOTRC,IPINIT,ISTOP(1),IER)
C
C        NOTE THAT IER IS NOT CHECKED AFTER EXITING SUBROUTINE
C        INT660.  IF A SERIOUS PROBLEM HAS OCCURRED, THE STOP
C        IS LOCATED IN INT660.
C
C        NOTE THAT MWORK( ), IWORK( ), SDATA( ), AND XDATA( ),
C        ARE THE SAME AS NELEV( ), IWBAN( ), STALAT( ), AND
C        STALON( ), RESPECTIVELY, IN INT660.
C        NOTE THAT FOR IWORK, ND5 MUST BE MAINTAINED AS GE ND1.
C
C        ALL CONTROL INFORMATION HAS BEEN READ.  NOW READ THE
C        DATA FROM ALL NUMIN SOURCES AND FILL THE MATRIX AA( , ).
C        THIS INCLUDES BOTH PREDICTORS AND PREDICTANDS.
C
C        SET UP INFORMATION CONCERNING LOOPS AND SAVING DATA.
C
      ND=1               
C
C        SET UP SOME VALUES FOR LOADING IS1( ) WHEN PACKING.
C
      NDATE=IDATE(ND)
      NYR=NDATE/1000000
      NMO=NDATE/10000-NYR*100
      NDA=NDATE/100-NYR*10000-NMO*100
      NHR=NDATE-NYR*1000000-NMO*10000-NDA*100
C
C        FILL MATRIX AA( , ) WITH DATA FOR DAY 1.
C
      CALL VRBL66(KFILDO,KFILRA,NUMRA,
     1            ID,IDPARS,JD,NWHERE,NVRBL,
     1            IDATE(ND),CCALL,XDATA,ND1,
     2            AA,NSTA,DATA,ND5,
     3            IS0,IS1,IS2,IS4,ND7,
     4            IP(12),ISTOP,IER)
C
C        NOTE THAT NO CHECK OF IER IS DONE AFTER
C        VRBL66 IS CALLED.  IF DATA HAVE NOT BEEN FOUND,
C        MESSAGES HAVE BEEN PUT TO FILE KFILDO, AND THE
C        AA ARRAY HAS BEEN FILLED WITH MISSING DATA.
C        RAMERGE WILL CONTINUE UNTIL AN ATTEMPT IS MADE
C        TO WRITE THE OUTPUT FILES.
C
C        ISTOP( ) HAS BEEN INCREMENTED AS NECESSARY.
C
C        PRINT THE DATA AS NECESSARY.  ONLY THOSE VARIABLES
C        FOR WHICH JP(2, ) NE 0 WILL BE PRINTED.
C
      IF(NPRINT.EQ.0)GO TO 140
      IF(IP(16).EQ.0)GO TO 140
C
      CALL PRU660(KFILDO,IP(16),CCALL,NSTA,NGP,KGP,JP,IDATE(ND),
     1            LNGTH,ICHARS,IWDTH,WDTH,IPREC,PREC,CFMT,HEAD,
     2            AA,PRINT,NVRBL,ISTOP(1),IER)
C
C        NO CHECK OF IER IS MADE AFTER CALLING PRU660.  IT IS
C        NOT RATIONAL TO CALL PRU660 IN OPERATIONS.  ONE
C        MIGHT CALL PRU660 IN CHECKOUT.
C
C        ISTOP(1) IS NOT ACTUALLY USED IN PRU660.
C   
C        NOW WRITE THE DATA TO THE SPECIFIED OUTPUT FILES. FIRST
C        THERE IS A CHECK FOR A SEQUENTIAL WRITE.  THEN THERE
C        IS A CHECK FOR THE RANDOM ACCESS OUTPUT FILE.
C
 140  IF(KFILIO.NE.0)THEN
C
C        FOR EACH VARIABLE N FOR WHICH JP(1,N) NE 0,
C        SET XMISSP AND XMISSS AND PACK THE DATA.  THE ORDER OF
C        DATA IN AA( , ) IS NOT OPTIMAL, BUT FOLLOWS
C        THE PATTERN OF U600.  THE INEFFICIENCY IS TRIVIAL.
C        THE DATA ARE PUT INTO XDATA( ) FROM AA( ), BECAUSE
C        OF THE INDEXING USED.
C
           DO 150 N=1,NVRBL
           IF(JP(1,N).EQ.0)GO TO 150
C
           CALL TRDATA(KFILDO,AA,XDATA,N,NVRBL,NSTA)
C
           CALL SETMIS(KFILDO,XDATA,NSTA,XMISSP,XMISSS)
C
C        PACK AND WRITE THE DATA.
C
           CALL PACKV(KFILDO,KFILIO,ID(1,N),IDPARS(1,N),JP(1,N),
     1                ISCALD(N),0,IPLAIN(1,1,N),PLAIN(N),
     2                NDATE,NYR,NMO,NDA,NHR,CCALL,
     3                ISDATA,XDATA,ND1,NSTA,IPACK,ND5,MINPK,
     4                IS0,IS1,IS2,IS4,ND7,XMISSP,XMISSS,
     5                IP(15),NWORDS,NTOTBY,NTOTRC,
     6                L3264B,L3264W,ISTOP(1),IER)
           IF(IER.NE.0)THEN
              WRITE(KFILDO,145)(ID(I,N),I=1,4),IER
 145          FORMAT(/,' ****UNABLE TO WRITE RECORD WITH ID = ',
     1               I9.9,1X,I9.9,1X,I9.9,1X,I10.3,' ON OUTPUT ',
     2               'SEQUENTIAL FILE -- IER = ',I3,
     3               /,'      PROGRAM CONTINUES TO RUN')
           ENDIF
C
 150       CONTINUE
C
C        WRITE TRAILER RECORD AND EOF.  IF THERE 
C        IS AN ERROR, TRAIL WILL PRODUCE A DIAGNOSTIC.
C
      CALL TRAIL(KFILDO,KFILIO,L3264B,L3264W,NTOTBY,NTOTRC,IER)
      ENDFILE KFILIO
      WRITE(KFILDO,151)NTOTBY,NTOTRC,OUTNAM
 151  FORMAT(/,' A TOTAL OF ',I11,' BYTES IN ',I7,' RECORDS NOW',
     1       ' EXIST ON FILE ',A60)
      ENDIF
C
C        NOW CHECK IF THERE IS TO BE A WRITE TO THE
C        RANDOM ACCESS OUTPUT FILE
C
       IF(KFILRAIO.NE.0)THEN
       CALL PACKRA_OPER(KFILDO,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JP,ISCALD,IPLAIN,
     2                  PLAIN,NVRBL,ND4,
     3                  NDATE,NYR,NMO,NDA,NHR,
     4                  ICALLD,CCALL,AA,ISDATA,XDATA,ND1,
     5                  NSTA,ICALLRA,CCALLRA,IPACK,ND5,MINPK,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  JTOTBY,JTOTRC,
     8                  L3264B,L3264W,ISTOP(1),IER)
      WRITE(KFILDO,152)JTOTBY,JTOTRC
 152  FORMAT(/,' A TOTAL OF ',I11,' BYTES IN ',I7,' RECORDS NOW',
     1        ' EXIST ON THE RANDOM ACCESS OUTPUT FILE')
C        CHECK FOR NON-ZERO CONDITION CODES--IT IS LIKELY THAT
C        A PROBLEM IN WRITING THE RANDOM ACCESS FILE WILL HAVE
C        CAUSED A STOP IN SUBROUTINE PACKRA.
         IF(IER.NE.0)THEN 
C           WRITE(KFILDO,155)IER
 155        FORMAT(/,'    NON-ZERO RETURN CODE FROM PACKRA ',
     1             'IER = ',I3,' COULD BE A PROBLEM ')
         ENDIF
      ENDIF
C
 215  IF(ISTOP(1).NE.0)WRITE(KFILDO,216)ISTOP(1)
 216  FORMAT(/,' AT LEAST ISTOP(1) =',I6,' ERRORS OCCURRED ON DAY 1.')
      IF(ISTOP(2).NE.0.AND.ISTOP(1).EQ.0)WRITE(KFILDO,2165)ISTOP(2)
 2165 FORMAT(/,' AT LEAST ISTOP(2) =',I6,' VARIABLES MISSING ON DAY 1.')
      IF(ISTOP(2).NE.0.AND.ISTOP(1).NE.0)WRITE(KFILDO,2166)ISTOP(2)
 2166 FORMAT(' AT LEAST ISTOP(2) =',I6,' VARIABLES MISSING ON DAY 1.')
      IF(ISTOP(1).EQ.0.AND.ISTOP(2).EQ.0)WRITE(KFILDO,217)
 217  FORMAT(/,' NO ERRORS OCCURRED AND ALL NEEDED DATA WERE FOUND',
     1        ' FOR DAY 1.')
C
 240  IF(ND.EQ.1)LSTOP=ISTOP(1)
C        THE NUMBER OF ERRORS, ISTOP(1), ON DAY 1 IS SAVED IN LSTOP.  AFTER
C        DAY THREE, IF LSTOP IS GT NSKIP, U660 HALTS.
      IF(LSTOP.LE.NSKIP)GO TO 800
C
      WRITE(KFILDO,241)LSTOP,ISTOP(1)
 241  FORMAT(/,' NUMBER OF ERRORS ON DAY 1 =',I3,' EXCEEDS NSKIP.',
     1        '  STOP AT END OF DAY 1, ISTOP(1) TOTAL ERRORS =',I3,
     2        '.  STOP IN U660 AT 241.')
      CALL W3TAGE('U660')
      STOP 241
C
C        WRITE TRAILER RECORD AND EOF UNLESS KFILIO = 0.  IF THERE 
C        IS AN ERROR, TRAIL WILL PRODUCE A DIAGNOSTIC.
C
C        CLOSE UP SHOP.
C
 800  IF(ISTOP(1).NE.0)WRITE(KFILDO,808)ISTOP(1)
 808  FORMAT(/,' AT LEAST ISTOP(1) =',I6,
     1        ' ERRORS HAVE OCCURRED ON THIS RUN.')
      IF(ISTOP(2).NE.0.AND.ISTOP(1).EQ.0)WRITE(KFILDO,809)ISTOP(2)
 809  FORMAT(/,' AT LEAST ISTOP(2) =',I6,
     1        ' DATA RECORDS NOT FOUND ON THIS RUN.')
      IF(ISTOP(2).NE.0.AND.ISTOP(1).NE.0)WRITE(KFILDO,8090)ISTOP(2)
 8090 FORMAT(' AT LEAST ISTOP(2) =',I6,
     1       ' DATA RECORDS NOT FOUND ON THIS RUN.')
      IF(ISTOP(1).EQ.0.AND.ISTOP(2).EQ.0)WRITE(KFILDO,810)
 810  FORMAT(/,' NO ERRORS HAVE BEEN DETECTED ON THIS RUN.')
      WRITE(KFILDO,811)
 811  FORMAT(' ')
      RETURN
      END
