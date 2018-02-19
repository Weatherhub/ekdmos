      SUBROUTINE U365(KFILDI,KFILDO,KFILIN,ND2X3,
     1                ND4,ND5,ND6,ND7,ND8,ND11,
     2                ID,IDPARS,THRESH,JD,JP,ISCALD,
     3                SMULT,SADD,ORIGIN,CINT,UNITS,
     4                NGRIDC,PLAIN,IPLAIN,L3264B,L3264W,
     5                NAMIN,IS0,IS1,IS2,IS4,MDATE,NWORK)
C
C$$$   SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: U365
C   PRGMMR: RUDACK         ORG: W/OST22          DATE: 2002-07-01
C
C ABSTRACT: PROGRAM U365 IS USED TO INTERPOLATE DATA FROM ONE
C           GRID TO ANOTHER.  THREE DIFFERENT TYPES OF MAP 
C           PROJECTIONS ARE SUPPORTED BY U365. (1) NORTH POLAR 
C           STEROGRAPHIC (2) LAMBERT CONFORMAL (3) MERCATOR.  THESE
C           MAP PROJECTIONS ARE SUPPORTED AS EITHER INPUT OR OUTPUT.
C           THE USER MAY CHOOSE ONE OF FOUR POSSIBLE INTERPOLATION 
C           SCHEMES: (1) LINEAR INTERPOLATION (2) BILINEAR 
C           INTERPOLATION (3) PRECIPITAION AMOUNT INTERPOLATION
C           (4) NEAREST NEIGHBOR INTERPOLATION.
C            
C           THE INTERPOLATED GRIDPOINT VARIABLES ARE WRITTEN TO 
C           FILE KFILRA*.  THIS PROGRAM SHOULD RUN ON EITHER 
C           THE HP UNIX PLATFORM WHICH USES 32-BIT INTEGERS OR 
C           THE CRAY UNIX PLATFORM WHICH USES 64-BIT INTEGERS.  
C           THE ONLY DIFFERENCE IS THAT THE DRIVER IS COMPILED 
C           WITH THE PARAMETER STATEMENT:
C              PARAMETER (L3264B=32) FOR THE 32-BIT MACHINE AND
C              PARAMETER (L3264B=64) FOR THE 64-BIT MACHINE. 
C
C PROGRAM HISTORY LOG:
C   02-07-01  RUDACK
C   05-11-03  MALONEY ADDED NCEP DOCBLOCK, CALLS TO W3TAGE.
C   05-11-08  MALONEY REMOVED KFILGO, REPLACED WITH KFILRA.
C   05-11-09  MALONEY ADDED CALL TO CLFILM TO CLOSE RA FILE.
C
C USAGE:  CALLED BY GRD2GRD
C
C        DATA SET USE:
C         INPUT FILES:
C          FORT.KFILDI - UNIT NUMBER OF INPUT FILE.  (INPUT)
C           FORT.KFILP - THE UNIT NUMBER FOR WHERE THE VARIABLE LIST 
C                        IS TO BE FOUND.  (INPUT)
C          FORT.KFILCP - UNIT NUMBER FOR VARIABLE CONSTANT FILE. 
C                        (INPUT)
C       FORT.KFILIN(J) - UNIT NUMBERS FOR INPUT DATA, ALL IN TDLPACK
C                        FORMAT. (J=1,NUMIN).  (INPUT)
C          FORT.KFILDT - UNIT NUMBER WHERE THE DATE LIST IS LOCATED.
C                        (INPUT)
C        OUTPUT FILES:
C          FORT.KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C           FORT.IP(J) - UNIT NUMBERS FOR OPTIONAL OUTPUT (SEE IP( )
C                        UNDER "VARIABLES" BELOW.)  (J=1,25)  (OUTPUT)
C          FORT.KFILRA - UNIT NUMBER OF GRIDPOINT RANDOM ACCESS OUTPUT FILE.
C                        (OUTPUT)
C
C        VARIABLES
C              KFILDI = UNIT NUMBER TO READ INPUT FILE 'U365.CN'.
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  INITIALLY,
C                       THIS IS SET BY DATA STATEMENT.  LATER, IN 
C                       IPOPEN, IF IP(1) NE 0, KFILDO IS SET = IP(1).
C                       THIS ALLOWS CHANGING THE "DEFAULT" PRINT FILE ON 
C                       THE FLY.  OTHERWISE, ON SOME SYSTEMS, THE OUTPUT
C                       FILE MIGHT HAVE THE SAME NAME AND BE OVERWRITTEN.
C                       WHEN THE OUTPUT FILE IS NOT THE ORIGINAL DEFAULT,
C                       THE NAME IS GENERATED AND CAN BE DIFFERENT FOR
C                       EACH RUN.
C           KFILIN(J) = UNIT NUMBERS FOR INPUT DATA, ALL IN TDLPACK 
C                       FORMAT.  INPUT CAN INCLUDE GRIDPOINT (FILES)
C                       DATA (J=1,NUMIN).
C               ND2X3 = THE DIMENSION OF SEVERAL ARRAYS (ND2*ND3).  SET 
C                       BY PARAMETER.
C                 ND4 = THE MAXIMUM NUMBER OF VARIABLES IN A RUN OF U365.
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND DATA( ).
C                       THESE ARE GENERAL PURPOSE ARRAYS, SOMETIMES USED
C                       FOR GRIDS.  TO AVIOD ERRORS IN CERTAIN ROUTINES,
C                       AND TO AVOID CONFUSION, ND5 SHOULD BE SET EQUAL TO
C                       ND2X3.  ALSO, BECAUSE IPACK( ) AND IWORK( ) ARE 
C                       USED AS WORK ARRAYS IN RDSNAM, ND5 SHOULD NOT BE
C                       LT ND12.
C                 ND6 = MAXIMUM NUMBER OF SEQUENTIAL FILES THAT CAN BE DEALT 
C                       WITH.  
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       SHOULD BE GE 54.
C                 ND8 = MAXIMUM NUMBER OF DATES THAT CAN BE DEALT WITH.
C                ND11 = MAXIMUM NUMBER OF INPUT GRID COMBINATIONS IN A RUN OF 
C                       U365.
C             ID(J,N) = THE INTEGER VARIABLE ID'S (J=1,4) (N=1,ND4).
C         IDPARS(J,N) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       VARIABLE ID'S CORRESPONDING TO ID( ,N)
C                       (J=1,15), (N=1,ND4).
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
C                       J=13--I (INTERPOLATION TYPE), (NOT USED)
C                       J=14--S (SMOOTHING INDICATOR), AND (NOT USED)
C                       J=15--G (GRID INDICATOR).  (NOT USED)
C           THRESH(N) = THE BINARY THRESHOLD ASSOCIATED WITH 
C                       IDPARS( ,N), N=1,ND4).
C             JD(J,N) = THE BASIC INTEGER VARIABLE ID'S (J=1,4) 
C                       (N=1,ND4).
C                       THIS IS THE SAME AS ID(J,N), EXCEPT THAT THE
C                       PORTIONS PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3, ),
C                       T = IDPARS(8,),
C                       I = IDPARS(13, ),
C                       S = IDPARS(14, ),
C                       G = IDPARS(15, ), AND
C                       THRESH( ).
C                       JD( , ) IS USED TO IDENTIFY THE BASIC MODEL
C                       FIELDS AS READ FROM THE ARCHIVE.
C             JP(J,N) = JP(1,N) INDICATES WHETHER (>0) OR NOT (=0)
C                       VARIABLE N WILL BE OUTPUT FOR VIEWING.
C                       JP(2,N) INDICATES THE TYPE OF INTERPOLATION
C                       TO BE PERFORMED ON THE INPUT GRID IN OBTAINING
C                       THE OUTPUT GRID.  (N=1,ND4).
C           ISCALD(N) = THE DECIMAL SCALING CONSTANT TO USE WHEN PACKING
C                       THE GRIDDED DATA (N=1,ND4).  NO BINARY 
C                       SCALING IS PROVIDED FOR.
C            SMULT(N) = THE MULTIPLICATIVE FACTOR WHEN CONTOURING OR
C                       GRIDPRINTING THE DATA (N=1,ND4).
C             SADD(N) = THE ADDITIVE FACTOR WHEN CONTOURING OR
C                       GRIDPRINTING THE DATA (N=1,ND4).
C           ORIGIN(N) = THE CONTOUR ORIGIN, APPLIES TO THE UNITS IN
C                       UNITS(N) (N=1,ND4).
C             CINT(N) = THE CONTOUR INTERVAL, APPLIES TO THE UNITS IN
C                       UNITS(N) (N=1,ND4).
C            UNITS(N) = THE UNITS OF THE DATA THAT APPLY AFTER
C                       MULTIPLYING BY SMULT(N) AND ADDING SADD(N) 
C                       (N=1,ND4).  (CHARACTER*12)
C         NGRIDC(L,M) = HOLDS THE GRID CHARACTERISTICS (L=1,6) FOR THE 
C                       INPUT GRID BEING PROCESSED (M=1,ND11).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=POLAR
C                            STEREOGRAPHIC AND 7=MERCATOR). 
C                       L=2--GRID LENGTH IN MILLIMETERS,
C                       L=3--LATITUDE AT WHICH GRID LENGTH IS
C                            CORRECT *10000,
C                       L=4--GRID ORIENTATION IN DEGREES *10000,
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *10000,
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES *10000.
C            PLAIN(N) = THE PLAIN LANGUAGE DESCRIPTION OF THE VARIABLES
C                       (N=1,ND4).  EQUIVALENCED TO IPLAIN( , , ).
C                       (CHARACTER*32)
C       IPLAIN(L,J,N) = 32 CHARACTERS (L=1,L3264W) (J=1,4) OF PLAIN
C                       LANGUAGE DESCRIPTION OF VARIABLES (N=1,ND4).
C                       NOTE THAT THIS REQUIRES TWO 32-BIT WORDS TO HOLD
C                       THE DESCRIPTION BUT ONLY ONE 64-BIT WORD.
C                       EQUIVALENCED TO PLAIN( ).
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  SET BY PARAMETER.
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).
C            NAMIN(J) = HOLDS DATA SET NAMES FOR THE UNIT NUMBERS IN 
C                       KFILIN(J) (J=1,NUMIN).  (CHARACTER*60)  
C                       CALCULATED BY PARAMETER, BASED ON L3464B.
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,4).
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,21+).
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).
C            MDATE(J) = INITIAL DATE LIST (J=1,NDATES) WHICH MAY CONTAIN
C                       NEGATIVE VALUES INDICATING A DATE SPAN.
C                       THIS IS MODIFIED IN DATPRO TO CONTAIN THE
C                       COMPLETE DATE LIST WITH THE DATES IN THE SPANS
C                       FILLED IN (J=1,NDATES), WHERE NDATES HAS BEEN
C                       INCREASED IF NECESSARY.  DATES ARE INPUT AS
C                       YYMMDDHH AND MODIFIED TO YYYYMMDDHH.  ZEROS IN 
C                       THE INPUT ARE ELIMINATED.  TERMINATOR IS 
C                       99999999.  MAXIMUM NUMBER OF DATES IS ND8.
C            NWORK(J) = A WORK ARRAY (J=1,ND8).
C               NUMIN = THE NUMBER OF VALUES IN KFILIN( ) AND NAMIN( ).
C               KFILP = THE UNIT NUMBER FOR WHERE THE VARIABLE LIST IS
C                       TO BE FOUND.
C              PRENAM = FILE NAME THAT CORRESPONDS TO THE UNIT NUMBER
C                       IN KFILP.  (CHARACTER*60)
C              KFILCP = UNIT NUMBER FOR VARIABLE CONSTANT FILE.  THIS
C                       CONTAINS DEFAULT VALUES FOR CERTAIN CONSTANTS
C                       FOR BASIC NMC VARIABLES AND OTHER VARIABLES
C                       SANS THRESHOLDS, ETC.  THESE INCLUDE PACKING
C                       CONSTANTS, GRIDPOINT CONSTANTS, AND NAMES.
C              CONNAM = HOLDS DATA SET NAME FOR THE UNIT NUMBER IN
C                       KFILCP.  (CHARACTER*60)
C              KFILDT = THE UNIT NUMBER FOR WHERE THE DATE LIST IS
C                       LOCATED.
C              DATNAM = HOLDS DATA SET NAME FOR THE UNIT NUMBER IN
C                       KFILDT.  (CHARACTER*60) 
C               IP(J) = EACH VALUE (J=1,25) INDICATES WHETHER (>1)
C                       OR NOT (=0) CERTAIN INFORMATION WILL BE WRITTEN.
C                       WHEN IP( ) > 0, THE VALUE INDICATES THE UNIT
C                       NUMBER FOR OUTPUT.  THESE VALUES SHOULD NOT BE
C                       THE SAME AS ANY KFILX VALUES EXCEPT POSSIBLY
C                       KFILDO, WHICH IS THE DEFAULT OUTPUT FILE.  THIS 
C                       IS ASCII OUTPUT, GENERALLY FOR DIAGNOSTIC 
C                       PURPOSES.  THE FILE NAMES WILL BE 4 CHARACTERS
C                       'U365', THEN 4 CHARACTERS FROM IPINIT, THEN 
C                       2 CHARACTERS FROM IP(J) (E.G., 'U365HRG130').
C                       THE ARRAY IS INITIALIZED TO ZERO IN CASE LESS
C                       THAN THE EXPECTED NUMBER OF VALUES ARE READ IN.
C                       (1) = ALL ERRORS AND OTHER INFORMATION NOT
C                           SPECIFICALLY IDENTIFIED WITH OTHER IP( )
C                           NUMBERS.  WHEN IP(1) IS READ AS NONZERO,
C                           KFILDO, THE DEFAULT OUTPUT FILE UNIT NUMBER,
C                           WILL BE SET TO IP(1).  WHEN IP(1) IS READ
C                           AS ZERO, KFILDO WILL BE USED UNCHANGED.
C                       (2) = THE INPUT DATES IN MDATE( ).  WHEN THERE
C                           ARE ERRORS, PRINT WILL BE TO UNIT KFILDO AS 
C                           WELL AS TO UNIT IP(2).
C                       (3) = THE OUTPUT DATES IN MDATE( ).  WHEN THERE
C                           ARE ERRORS, OUTPUT WILL BE TO UNIT KFILDO AS 
C                           WELL AS TO UNIT IP(3).
C                       (6) = THE VARIABLES AS THEY ARE BEING READ IN.
C                           THIS IS GOOD FOR CHECKOUT; FOR ROUTINE
C                           OPERATION, IP(7), IP(8), AND/OR IP(9),
C                           MAY BE BETTER.  
C                       (7) = THE VARIABLE LIST IN SUMMARY FORM.
C                           IF THERE ARE ERRORS, THE VARIABLE LIST
C                           WILL BE WRITTEN TO THE DEFAULT OUTPUT FILE 
C                           UNIT KFILDO AS WELL AS TO UNIT IP(7).
C                           THIS LIST INCLUDES THE PARSED ID'S IN 
C                           IDPARS( , ).
C                       (8) = THE VARIABLE LIST IN SUMMARY FORM AFTER 
C                           REORDERING.  THIS LIST INCLUDES THE PARSED 
C                           ID'S IN IDPARS( , ).
C                       (9) = THE VARIABLE LIST IN SUMMARY FORM AFTER
C                           REORDERING.  THIS DIFFERS FROM (8) IN THAT
C                           (9) DOES NOT INCLUDE THE PARSED ID'S IN 
C                           IDPARS( , ), BUT RATHER INCLUDES THE 
C                           INFORMATION TAKEN FROM THE VARIABLE
C                           CONSTANT FILE ON UNIT KFILCP.
C                       (13) = GRIDPOINT FIELDS.  WHEN THE VARIABLE
C                           LIST INDICATES GRIDPOINT VALUES ARE TO BE
C                           WRITTEN FOR VIEWING (JP(1, )>0), THEY WILL
C                           BE WRITTEN TO UNIT IP(13). 
C                       (16) = INDICATES WHETHER (>0) OR NOT (=0) PRINT
C                            WILL BE WRITEN THAT INFORMS THE USER
C                            WHICH IDS HAVE BEEN WRITTEN TO THE 
C                            OUTPUT FILE.  
C             IUSE(J) = EACH VALUE J PERTAINS TO IP(J).  WHEN AN IP(J)
C                       VALUE IS USED BY THE PROGRAM, IPRINT(J) = 1;
C                       OTHERWISE, IPRINT(J) = 0.  USED BY IPRINT TO
C                       PRINT IP( ) VALUES.
C              KFILRA = UNIT NUMBER OF GRIDPOINT RANDOM ACCESS OUTPUT FILE.
C               RUNID = INFORMATION INPUT TO IDENTIFY THE OUTPUT.
C                       (CHARACTER*72)
C               JSTOP = THE NUMBER OF ERRORS THAT WILL BE TOLERATED ON
C                       THE TOTAL RUN BEFORE PROGRAM STOPS.
C              INCCYL = INCREMENT IN HOURS BETWEEN DATE/TIMES THAT
C                       ARE PUT INTO MDATE( ) BY SUBROUTINE DATPRO.
C                 NEW = 1 WHEN NEW 8-LETTER CALL LETTERS ARE TO BE USED;
C                       0 WHEN OLD 3-LETTER CALL LETTERS ARE TO BE USED.
C              NTOTBG = THE TOTAL NUMBER OF BYTES ON THE FILE 
C                       ASSOCIATED WITH UNIT NO. KFILRA (THE GRIDPOINT
C                       FILE).  IT IS UPDATED WHEN THE DATA IN
C                       IPACK( ) ARE WRITTEN.
C              NTOTRG = THE TOTAL NUMBER OF RECORDS IN THE GRIDPOINT
C                       FILE.  IT IS UPDATED AS NEEDED IN WRITEP.  
C              NDATES = NUMBER OF VALUES IN MDATE( ).  MODIFIED AS 
C                       NECESSARY IN DATPRO.
C              IPINIT = 4 CHARACTERS, USUALLY A USER'S INITIALS PLUS
C                       A RUN NUMBER, TO APPEND TO 'U365' TO 
C                       IDENTIFY A PARTICULAR SEGMENT OF OUTPUT 
C                       INDICATED BY A SUFFIX IP(J).  THE RUN NUMBER
C                       ALLOWS MULTIPLE RUNS OF U365 AND WRITING OF
C                       UNIQUELY NAMED FILES, PROVIDED THE USER USES
C                       A DIFFERENT RUN NUMBER FOR EACH RUN.
C              OUTNAM = NAME OF DATA SET FOR VECTOR OUTPUT.
C                       (CHARACTER*60)
C              NVRBLS = THE NUMBER OF VARIABLES ACTUALLY NEEDED AND
C                       IDENTIFIED IN ID( , ), ETC.
C             IDUM(J) = SCRATCH ARRAY (J=1,2).
C                 IER = STATUS RETURN.
C                       0 = GOOD RETURN.  SEE CALLED ROUTINES FOR OTHER
C                       VALUES.
C                       OTHER VALUES RETURNED FROM SUBROUTINES.
C               STATE = VARIABLE SET TO STATEMENT NUMBER TO INDICATE
C                       WHERE AN ERROR OCCURRED.  (CHARACTER*4)
C               BLANK = 8 BLANKS.  (CHARACTER*8)  (INTERNAL)
C               MINPK = MINIMUM GROUP SIZE WHEN PACKING THE GRIDDED
C                       VALUES.  SET IN DATA STATEMENT.
C            ITEMP(J) = WORK ARRAY (J=1,14).
C            ISTOP(J) = FOR J=1, ISTOP( ) IS INCREMENTED BY 1 EACH TIME
C                       FOR J=2, ISTOP( ) IS INCREMENTED BY 1 WHENEVER
C                       AN INPUT DATA RECORD IS NOT FOUND.
C               MPROJ = MAP PROJECTION OF THE OUTPUT GRID.  
C                  NX = X-EXTENT OF THE OUTPUT GRID.  
C                  NY = Y-EXTENT OF THE OUTPUT GRID.  
C              XLATLL = LATITUDE OF LOWER LEFT CORNER POINT OF THE
C                       OUTPUT GRID.  
C              YLONLL = WEST LONGITUDE OF LOWER LEFT CORNER POINT
C                       OF THE OUTPUT GRID.  DO NOT USE NEGATIVE.  
C               XMESH = MESH LENGTH OF OUTPUT GRID IN KM AT XLAT DEGREES 
C                       NORTH LATITUDE.  
C              ORIENT = ORIENTATION OF OUTPUT GRID IN DEGREES WEST 
C                       LONGITUDE.  DO NOT USE NEGATIVE.  
C                XLAT = LATITUDE OF OUTPUT GRID IN DEGREES AT WHICH 
C                       XMESH APPLIES. ALSO THE LATITUDE WHERE THE 
C                       PROJECTION CUTS THE EARTH.  DO NOT USE NEGATIVE.
C 
C        SUBPROGRAMS CALLED:  IPOPEN, IPRINT, TIMPR, DATPRO, RDSNAM, RDI, 
C                             RDSTAD, RDPRED, RDINTRP, TRAIL, IERX, W3TAGE,
C                             CLFILM
C          UNIQUE: RDINTRP
C          LIBRARY:
C           MOSLIB - IPOPEN, IPRINT, TIMPR, DATPRO, RDSNAM, 
C                    RDI, RDSTAD, RDPRED, TRAIL, IERX, CLFILM
C            W3LIB - W3TAGE
C
C        EXIT STATES:
C          COND =    0 - SUCCESSFUL RUN
C                  134 - ERROR IN DATE
C                 1282 - ERROR IN CN FILE
C                 1435 - NO OUTPUT FILE SPECIFIED
C                 1452 - INCORRECT MAP PROJECTION
C                 1462 - INCONSISTENCY WITH KFILIN( )
C                 1465 - INCONSISTENCY WITH KFILDI
C                 1466 - INCONSISTENCY WITH KFILP
C                 1467 - INCONSISTENCY WITH KFILCP
C                 1468 - INCONSISTENCY WITH IP( )
C                 1475 - INVALID INTERPOLATION TYPE
C                 9999 - ERROR WITH EITHER AN OPEN OR WRITE STATEMENT
C                   SEE OTHER ROUTINES FOR OTHER VALUES.
C
C REMARKS:  NONE
C
C ATTRIBUTES:
C   LANGUAGE:  FORTRAN 90 (xlf90 compiler)
C   MACHINE:  IBM SP
C
C$$$
C

C     NONSYSTEM SUBROUTINES USED 
C
      CHARACTER*4 STATE,IPINIT
      CHARACTER*8 BLANK
      CHARACTER*12 UNITS(ND4)
      CHARACTER*32 PLAIN(ND4)
      CHARACTER*60 NAMIN(ND6)
      CHARACTER*60 RACESS,PRENAM,CONNAM,DATNAM
      CHARACTER*72 RUNID/' '/
C
      DIMENSION ID(4,ND4),IDPARS(15,ND4),THRESH(ND4),JD(4,ND4),
     1          JP(3,ND4),ISCALD(ND4),SMULT(ND4),SADD(ND4),
     2          ORIGIN(ND4),CINT(ND4)
      DIMENSION IPLAIN(L3264W,4,ND4)
      DIMENSION KFILIN(ND6)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION MDATE(ND8),NWORK(ND8)
      DIMENSION NGRIDC(6,ND11)
      DIMENSION ITEMP(14),IP(25),IUSE(25),IDUM(2),ISTOP(2)
C
      DATA ISTOP/0,0/
      DATA MINPK/21/
      DATA BLANK/' '/
      DATA IP/25*0/
      DATA JSTOP,INCCYL/2*0/
      DATA KFILRA/0/,
     1     NTOTBG/0/,
     2     NTOTRG/0/
      DATA IUSE/1,1,1,0,0,1,1,1,1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0/
C
C        READ AND PROCESS THE PRINT UNIT NUMBERS.  THE INPUT UNIT
C        KFILDI HAS BEEN OPENED IN THE DRIVER OR BY SOME OTHER
C        MECHANISM SUCH AS A SCRIPT. 
C     
C        NOTE THAT IF KFILDO NE IP(1) (READ BELOW), THE OUTPUT FROM
C        TIMPR (IN THE DRIVER) WILL BE ON UNIT KFILDO, BUT ALL OTHER 
C        "DEFAULT" PRINT ON UNIT IP(1), UNLESS THERE IS AN ERROR ON 
C        THE OPEN STATEMENT BELOW OR THE FOLLOWING READ.
C
      STATE='108 ' 
      READ(KFILDI,108,IOSTAT=IOS,ERR=900,END=109)IPINIT,(IP(J),J=1,25) 
 108  FORMAT(A4,25I3)
C        LESS THAN 25 IP( ) VALUES WILL NOT BE INDICATED AS AN ERROR.
C        SOME IP( ) VALUES ARE NOT USED; SEE IUSE( ).
      CALL IPOPEN(KFILDO,'U365',IPINIT,IP,IER)
C        WHEN IP(1) NE 0, KFILDO HAS BEEN SET TO IP(1).
C        A FILE WILL BE OPENED FOR EVERY DIFFERENT VALUE IN IP( ).
C        THE FILE NAMES WILL BE 4 CHARACTERS 'U365' THEN 4 CHARACTERS
C        FROM IPINIT, THEN 2 CHARACTERS FROM IP(J).  IPINIT MIGHT BE
C        'HRG1' INDICATING THE PERSONS INITIALS PLUS A SEQUENCE NUMBER.
      IF(IER.NE.0)ISTOP(1)=ISTOP(1)+1
 109  WRITE(KFILDO,110)IPINIT
 110  FORMAT(/,' IPINIT = ',A4)
      CALL IPRINT(KFILDO,IP,IUSE)
C
C        TIME STAMP ALL ASCII OUTPUT OTHER THAN KFILDO.
C        THIS IS NOT DONE IN IPOPEN BECAUSE SOME PROGRAMS
C        MIGHT NOT WANT SOME FILE TO BE TIME STAMPED.
C
      DO 113 J=1,25
      IF(IP(J).EQ.0.OR.IP(J).EQ.KFILDO)GO TO 113
      IF(J.EQ.1)GO TO 112
C
      DO 111 I=1,J-1
      IF(IP(J).EQ.IP(I))GO TO 113
 111  CONTINUE
C
 112  CALL TIMPR(IP(J),IP(J),'START U365          ')
 113  CONTINUE
C
C        READ AND PRINT THE RUN IDENTIFICATION.
C
      STATE='115 '
      READ(KFILDI,115,IOSTAT=IOS,ERR=900,END=116)RUNID 
C        LESS THAN 72 CHARACTERS IS NOT CONSIDERED AN ERROR.
 115  FORMAT(A72)  
 116  WRITE(KFILDO,117)RUNID
 117  FORMAT(/,' ',A72)
C
C        PRINT TO MAKE SURE USER KNOWS WHAT MACHINE IS BEING USED.
C 
      WRITE(KFILDO,119)L3264B
 119  FORMAT(/,' RUNNING ON A',I3,'-BIT MACHINE.')
C
C        READ AND PRINT CONTROL INFORMATION.
C
      READ(KFILDI,120,IOSTAT=IOS,ERR=900,END=1281)
     1     JSTOP,INCCYL,MPROJ,NX,NY,
     2     XLATLL,YLONLL,XMESH,ORIENT,XLAT
 120  FORMAT(5(I10/),2(F10.4/),F10.5/,F10.4/,F10.4)
      GOTO 1283
C
C        INCOMPLETE CONTROL INFORMATION SHOULD BE CONSIDERED AN ERROR.
C        HOWEVER, A SHORT RECORD DOES NOT CAUSE AN "END" CONDITION.
C
1281  WRITE(KFILDO,1282)
1282  FORMAT(/,' ****CONTROL INFORMATION NOT COMPLETE.',/,
     1        '     STOP IN U365 AT 1282.')
      STOP 1282
C
 1283  WRITE(KFILDO,1284) JSTOP,INCCYL,MPROJ,NX,NY,XLATLL,YLONLL,
     1                    XMESH,ORIENT,XLAT,MINPK,L3264B
 1284  FORMAT(/,' JSTOP ',I10,'   NUMBER OF ERRORS THAT WILL BE',
     X                      ' TOLERATED ON TOTAL RUN BEFORE STOPPING',/,
     1        ' INCCYL',I10,'   INCREMENT IN HOURS BETWEEN DATE/TIMES'/
     2        ' MPROJ ',I10,'   MAP PROJECTION NUMBER OF OUTPUT ',
     X                      'GRID',/,
     3        ' NX    ',I10,'   NX = SIZE OF OUTPUT GRID IN X',
     X                      ' DIRECTION IN MESH UNITS',/,
     4        ' NY    ',I10,'   NY = SIZE OF OUTPUT GRID IN Y',
     X                      ' DIRECTION IN MESH UNITS',/,
     5        ' XLATLL  ',F8.4,'   NORTH LATITUDE OF LOWER LEFT',
     X                      ' CORNER OF OUTPUT GRID',/,
     6        ' YLONLL  ',F8.4,'   WEST LONGITUDE OF LOWER LEFT',
     X                      ' CORNER OF OUTPUT GRID',/,
     7        ' XMESH ',F10.5,'   ACTUAL GRIDLENGTH OF OUTPUT GRID IN',
     X                            ' KM',/,
     8        ' ORIENT',F10.4,'   GRID ORIENTATION OF OUTPUT GRID'/
     9        ' XLAT  ',F10.4,'   LATITUDE AT WHICH XMESH APPLIES ON',
     X                            ' OUTPUT GRID',/,
     A        ' MINPK ',I10,'   MINIMUM GROUP SIZE WHEN PACKING',/,
     B        ' L3264B',I10,'   INTEGER WORD SIZE OF MACHINE')
C 
C        READ AND PROCESS UNIT NUMBER AND FILE NAME FOR READING
C        DATE LIST.  FILE WILL BE OPENED AS 'OLD', UNLESS THE FILE
C        IS THE DEFAULT INPUT FILE.
C
      CALL RDSNAM(KFILDI,KFILDO,KFILDT,DATNAM,IDUM,IDUM,1,
     1            N,'OLD','FORMATTED',IP,IER)
      IF(IER.NE.0)ISTOP(1)=ISTOP(1)+1
      WRITE(KFILDO,130)KFILDT
 130  FORMAT(/,' NCEP DATE FILE UNIT NUMBER.',/,' ',I4)
C
C        READ AND PRINT THE DATE TO BE PROCESSED
C
      CALL GET_NCEPDATE(KFILDT,IYR,IMO,IDA,IHR,NDATE,IER)
      IF(IER.NE.0)THEN
         WRITE(KFILDO,134)
 134     FORMAT(/' ****ERROR: CAN NOT READ NCEP DATE FILE - ',
     1           'CATASTROPHIC ERROR IN U365. STOP AT 134.')
         CALL W3TAGE('U365')
         STOP 134
      ENDIF
      NDATES = 1
      MDATE(1) = NDATE
      WRITE(KFILDO,135)NDATES,(MDATE(J),J=1,NDATES)
 135  FORMAT(/,' ',I4,' INPUT DATE AS READ',/,(1X,10I12))
C
C        READ AND PROCESS UNIT NUMBERS AND FILE NAMES FOR ALL TDLPACK 
C        INPUT.  FILES WILL BE OPENED IN 'RDINTRP'.
C
      CALL RDSNAM(KFILDI,KFILDO,KFILIN,NAMIN,ITEMP,ITEMP,ND6,
     1            NUMIN,'NOT','UNFORMATTED',IP,IER)
C
      IF(IER.NE.0)ISTOP(1)=ISTOP(1)+1
C
      IF(NUMIN.EQ.0)THEN
         WRITE(KFILDO,140)NUMIN
 140     FORMAT(/,' ',I2,' MODEL INPUT DATA SETS.')
      ELSE
         WRITE(KFILDO,141)NUMIN,(KFILIN(M),NAMIN(M),M=1,NUMIN)
 141     FORMAT(/,' ',I2,' MODEL INPUT DATA SETS, UNITS, MODEL',
     1            ' NUMBERS AND NAMES.',/,(' ',I4,2X,A60))
      ENDIF
C
C        READ AND PROCESS UNIT NUMBER AND FILE NAME FOR GRIDDED 
C        OUTPUT.  THE FILE WILL BE NOT BE OPENED.
C
      CALL RDSNAM(KFILDI,KFILDO,KFILRA,RACESS,IDUM,IDUM,1,
     1            IOUT,'NOT','NOTOPENED',IP,IER)
C         IOUT IS THE NUMBER OF VALUES READ AND IS NOT USED.
      IF(IER.NE.0)ISTOP(1)=ISTOP(1)+1
C
      IF(KFILRA.EQ.0)THEN
         WRITE(KFILDO,1435)
 1435    FORMAT(/,' ****NO OUTPUT DATA SET PROVIDED;',
     1           ' PACKED GRIDDED OUTPUT WILL NOT BE WRITTEN.',
     2          /,'     THIS IS A CATASTROPHIC ERROR!')
         CALL W3TAGE('MDL_GRD2GRD')
         STOP 1435
      ELSE
         WRITE(KFILDO,144)KFILRA,RACESS
 144     FORMAT(/,' OUTPUT DATA SET, UNIT AND NAME.',/,
     1         (' ',I4,2X,A60))
      ENDIF
C
C        READ AND PROCESS UNIT NUMBER AND FILE NAME FOR READING 
C        VARIABLE LIST.
C
      CALL RDSNAM(KFILDI,KFILDO,KFILP,PRENAM,IDUM,IDUM,1,
     1            N,'OLD','FORMATTED',IP,IER)
      IF(IER.NE.0)ISTOP(1)=ISTOP(1)+1
      WRITE(KFILDO,145)KFILP,PRENAM
 145  FORMAT(/,' VARIABLE LIST DATA SET, UNIT AND NAME.',/,
     1       (' ',I4,2X,A60))
C
C        READ AND PROCESS UNIT NUMBER FOR THE VARIABLE CONSTANTS DIRECTORY.
C
      CALL RDSNAM(KFILDI,KFILDO,KFILCP,CONNAM,IDUM,IDUM,1,
     1            N,'OLD','FORMATTED',IP,IER)
      IF(IER.NE.0)ISTOP(1)=ISTOP(1)+1
      WRITE(KFILDO,1451)KFILCP,CONNAM
 1451 FORMAT(/,' VARIABLE CONSTANT DIRECTORY, UNIT AND NAME.',/,
     1       (' ',I4,2X,A60))
C
C        ENSURE THAT A CORRECT MAP PROJECTION HAS BEEN ENTERED.
C
      IF((MPROJ.NE.3).AND.(MPROJ.NE.5).AND.(MPROJ.NE.7)) THEN
         WRITE(KFILDO,1452) MPROJ
 1452    FORMAT(/,' ****OUTPUT MAP PROJECTION OF',I2,' IS NOT',
     1            ' SUPPORTED IN U365.  VALUES OF 3,5 AND 7 ARE',
     2            ' SUPPORTED.',/,'STOP 1452 IN U365.')
         STOP 1452
      ENDIF
C
C        CHECK POSSIBLE INCONSISTENCY OF INPUT UNIT NUMBERS WITH
C        OTHERS USED BY THE PROGRAM.  THIS SHOULD PROTECT THE LARGE
C        DATA SETS IN NAMIN( ) FROM BEING OVERWRITTEN.
C
      DO 1464 J=1,NUMIN
         IF(KFILIN(J).EQ.KFILDT.OR.
     1      KFILIN(J).EQ.KFILRA.OR.
     2      KFILIN(J).EQ.KFILP.OR.
     3      KFILIN(J).EQ.KFILCP)THEN
            WRITE(KFILDO,1462)
 1462       FORMAT(/,' ****INCONSISTENCY IN INPUT UNIT NUMBERS',
     1               ' IN KFILIN( ) WITH EITHER KFILDT,',
     2               ' KFILRA, KFILP, OR KFILCP.',/,
     3               '     STOP IN U365 AT 1462')
            STOP 1462
         ENDIF
 1464 CONTINUE
C
      IF((KFILDI.EQ.KFILDO).OR.(KFILDI.EQ.KFILRA))THEN
         WRITE(KFILDO,1465)
 1465    FORMAT(/,' ****INCONSISTENCY IN INPUT UNIT NUMBER IN KFILDI',
     1           ' WITH EITHER KFILDO OR KFILRA.',/,
     2           '     STOP IN U365 AT 1465')
         STOP 1465
      ENDIF
C
      IF(KFILP.NE.0.AND.
     1  (KFILP.EQ.KFILDO.OR.
     2   KFILP.EQ.KFILRA))THEN
         WRITE(KFILDO,1466)
 1466    FORMAT(/,' ****INCONSISTENCY IN INPUT UNIT NUMBER IN KFILP',
     1           ' WITH EITHER KFILDO OR KFILRA.',/,
     2           '     STOP IN U365 AT 1466')
         STOP 1466
      ENDIF
C
      IF(KFILCP.NE.0.AND.
     1  (KFILCP.EQ.KFILDO.OR.KFILCP.EQ.KFILRA))THEN
         WRITE(KFILDO,1467)
 1467    FORMAT(/,' ****INCONSISTENCY IN INPUT UNIT NUMBER IN KFILCP',
     1           ' WITH EITHER KFILDO OR KFILRA.',/,
     2           '     STOP IN U365 AT 1467')
         STOP 1467
      ENDIF
C
      DO 1469 J=1,25
         IF(IP(J).NE.0.AND.
     1     (IP(J).EQ.KFILDI.OR.
     2      IP(J).EQ.KFILP.OR.
     3      IP(J).EQ.KFILCP.OR.
     4      IP(J).EQ.KFILDT.OR.
     5      IP(J).EQ.KFILRA))THEN
            WRITE(KFILDO,1468)
 1468       FORMAT(/,' ****INCONSISTENCY IN INPUT UNIT NUMBER IN IP( )',
     1              ' WITH EITHER KFILDI, KFILP, KFILCP, ',
     2              ' KFILDT OR KFILRA.',/,
     3              '     STOP IN U365 AT 1468')
            STOP 1468
         ENDIF
 1469 CONTINUE
C
C        READ VARIABLE LIST FOR WHICH GRIDDED VALUES ARE TO BE OUTPUT.
C
      CALL RDPRED(KFILDO,IP(6),IP(7),IP(8),IP(9),KFILP,KFILCP,
     1            ID,IDPARS,THRESH,JD,JP,ISCALD,SMULT,SADD,
     2            ORIGIN,CINT,PLAIN,UNITS,ND4,NVRBLS,ISTOP(1),IER)
C
C        FILE KFILP IS CLOSED WHEN IT IS NOT THE SAME AS
C        THE DEFAULT INPUT FILE.
      IF(KFILP.NE.KFILDI)CLOSE(UNIT=KFILP)
C
C        ENSURE THAT A CORRECT INTERPOLATION HAS BEEN ENTERED FOR 
C        ALL VARIABLES.
C
      DO 1479 N=1,NVRBLS
         IF((JP(2,N).NE.1).AND.(JP(2,N).NE.2).AND.
     1      (JP(2,N).NE.3).AND.(JP(2,N).NE.4)) THEN
            WRITE(KFILDO,1475) JP(2,N),(ID(J,N),J=1,4)
 1475       FORMAT(/,' ****AN INCORRECT INTERPOLATION TYPE OF',I2,
     1               ' HAS BEEN ENTERED FOR VARIABLE ',3(I9.9,1X),
     2               I10.10,'.',/,4X,' VALUES OF 1,2 3 AND 4 ARE ',
     3               'SUPPORTED.  STOP 1475 IN U365.')
            STOP 1475
         ENDIF
 1479 CONTINUE
C
C        PERFORM THE INTERPOLATION TO THE OUTPUT GRID AND PACK
C        THE DATA INTO FILE KFILRA.
C
      CALL RDINTRP(KFILDO,KFILIN,KFILRA,RACESS,ND2X3,ND4,ND5,
     1             ND6,ND7,ND8,ND11,JP,JSTOP,NAMIN,NUMIN,
     2             MDATE,NDATES,MINPK,ISCALD,ID,IDPARS,
     3             NVRBLS,IS0,IS1,IS2,IS4,IPLAIN,PLAIN,
     4             NGRIDC,MPROJ,NX,NY,XLATLL,YLONLL,
     5             XMESH,ORIENT,XLAT,IP(13),IP(16),
     6             L3264B,L3264W,NTOTBG,NTOTRG,ISTOP,IER)
C
      IF(KFILRA.NE.0)THEN
         WRITE(KFILDO,3095)NTOTBG,NTOTRG,RACESS
 3095    FORMAT(/,' A TOTAL OF ',I11,' BYTES IN ',I7,' RECORDS NOW',
     1           ' EXIST ON FILE ',A60)
      ENDIF
C
C        CLOSE THE GRIDDED RANDOM ACCESS FILE.
C
      IF(KFILRA.NE.0)THEN
         CALL CLFILM(KFILDO,KFILRA,IER)
      ENDIF
C
      IF(ISTOP(1).NE.0)WRITE(KFILDO,311)ISTOP(1)
 311  FORMAT(/,' AT LEAST ISTOP(1) =',I6,
     1        ' ERRORS HAVE OCCURRED ON THIS RUN.')
      IF(ISTOP(2).NE.0.AND.ISTOP(1).EQ.0)WRITE(KFILDO,312)ISTOP(2)
 312  FORMAT(/,' AT LEAST ISTOP(2) =',I6,
     1        ' DATA RECORDS NOT FOUND ON THIS RUN.')
      IF(ISTOP(2).NE.0.AND.ISTOP(1).NE.0)WRITE(KFILDO,313)ISTOP(2)
 313  FORMAT(' AT LEAST ISTOP(2) =',I6,
     1       ' DATA RECORDS NOT FOUND ON THIS RUN.')
      IF(ISTOP(1).EQ.0.AND.ISTOP(2).EQ.0)WRITE(KFILDO,314)
 314  FORMAT(/,' NO ERRORS HAVE BEEN DETECTED ON THIS RUN.')
      WRITE(KFILDO,315)
 315  FORMAT(' ')
      RETURN
C 
C        ERROR STOP BELOW IS FOR ERRORS OF CONTROL INFORMATION INPUT.
C
 900  CALL IERX(KFILDO,KFILDO,IOS,'U365  ',STATE)
      STOP 9999
      END
