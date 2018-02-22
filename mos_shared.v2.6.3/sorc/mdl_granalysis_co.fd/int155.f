      SUBROUTINE INT155(KFILDI,KFILDO,KFILIO,KFILVO,KFILOG,KFILCP,
     1                  KFILOV,KFILQC,KFILAN,KFIL10,KFILLP,KFILNI,IP,
     2                  CCALL,ELEV,IWBAN,STALAT,STALON,ISDATA,IPACK,
     3                  NAME,IQUAL,LNDSEA,NSTA,ND1,
     4                  CCALLD,ND5,NAREA,
     5                  ID,IDPARS,THRESH,JD,JP,NCEPNO,MODNO,NPRED,
     6                  ISCALD,IWRITS,IWRITA,ICOMPT,SMULT,SADD,ORIGIN,
     7                  CINT,ANLTAB,INLTAB,PLAIN,UNITS,L3264B,ND4,
     8                  KFILIN,MODNUM,NAMIN,JFOPEN,NUMIN,ND6,
     9                  KFILRA,RACESS,NUMRA,GOTNAM,OUTDIS,
     A                  OUTVEC,OUTQCV,ANLNAM,VOTNAM,
     B                  IDATE,NDATES,NWORK,ND8,INCCYL,
     C                  KSKIP,NSKIP,JSTOP,PXMISS,NPROJ,ORIENT,XLAT,
     D                  ALATL,ALONL,NXL,NYL,
     E                  MESHB,BMESH,MESHL,XMESHL,MESHD,DMESH,
     F                  MESHE,EMESH,IPRTEL,IOPTB,MINVEC,MINMOD,
     G                  ISTA,ISMPL,ISTOP,IER)
C$$$   SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: INT155
C   PGMMR: WAGNER             ORG: W/OST22            DATE: 09-09-17
C
C ABSTRACT:INT155 PERFORMS MUCH OF THE INITIALIZATION FOR U155.
C
C PROGRAM HISTORY LOG:
C
C        JUNE      2004   GLAHN   TDL   MOS-2000
C                                 ADAPTED FROM INT150
C        AUGUST    2004   GLAHN   ADDED IQUAL(ND1,2) TO DIMENSION
C                                 AND TO CALL
C        OCTOBER   2004   GLAHN   OMITTED 1/B FROM COMMENTS, DIAGNOSTIC
C        OCTOBER   2004   GLAHN   MODIFIED PARSING INTO IQUAL( , )
C        OCTOBER   2004   GLAHN   CHANGED NELEV( ) TO ELEV( )
C        OCTOBER   2004   GLAHN   MODIFIED FOR LAT/LON VICE POLE 
C        OCTOBER   2004   GLAHN   ADDED LNDSEA( )
C        OCTOBER   2004   GLAHN   INSERTED LAMBERT AND MERCATOR
C                                 CAPABILITY
C        NOVEMBER  2004   GLAHN   EXCHANGED ACTUAL FOR MSHXMS; ADDED
C                                 902 AND ISTOP=ISTOP+1
C        NOVEMBER  2004   GLAHN   INTERCHANGED 2 ARGUMENTS IN CALL TO
C                                 ACTUAL
C        JANUARY   2005   GLAHN   ELIMINATED REFERENCE TO IP(14)
C        JANUARY   2005   GLAHN   ELIMINATED REFERENCE TO IP(8)
C        MAY       2005   GLAHN   ADDED KFILLP
C        MAY       2005   GLAHN   ADDED IP14 FOR COMPUTED LAPSE RATES
C        AUGUST    2005   GLAHN   ADDED WRITING TO IP(20) AT 1286
C        AUGUST    2005   GLAHN   INCREASED SIZE OF ANLTAB TO 14
C        AUGUST    2005   GLAHN   MODIFIED DIAGNOSTICS AROUND 1445
C        OCTOBER   2005   GLAHN   ADDED ISTA AND ISMPL; SAMPLING OF
C                                 FIRST GUESS FOR ANALYSIS POINTS
C        OCTOBER   2005   GLAHN   CORRECTED TEXT FOR IPRTEL; IP(8)
C                                 DEFINED
C        JANUARY   2006   GLAHN   ADDED IP(24)
C        MARCH     2006   GLAHN   ADDED READING KFILSL AND STALST
C        APRIL     2006   GLAHN   INCREASED ANLTAB CHARACTER*14 TO *17
C        MAY       2006   GLAHN   CHANGED OPENING OF FILES KFILQC AND
C                                 KFILOV FROM "NEW" TO "OLD"
C        MAY       2006   GLAHN   OPENING OF FILES KFILQC AND KFILOV
C                                 DEPEND ON KSKIP
C        JULY      2006   GLAHN   ADDED IPIN=25 ABOVE CALL U155CK
C        SEPTEMBER 2006   GLAHN   MODIFIED IF TEST AT 158
C        MARCH     2007   GLAHN   CORRECTED IQUAL(,2) IN DO 151
C        MAY       2007   GLAHN   INCREASED IQUAL( ,2) TO IQUAL( ,5)
C        JULY      2007   GLAHN   ADDED READING VARIABLE RADIUS
C        OCTOBER   2007   GLAHN   REPLACED RDVRAD WITH RDVRHL
C        DECEMBER  2007   GLAHN   ADDED IP(25) CAPABILITY
C        DECEMBER  2007   GLAHN   ADDED READING NEIGHBORS FILE UNIT NO.
C        DECEMBER  2007   GLAHN   ADDED DIAGNOSTIC 1585; MOD TO 159
C        FEBRUARY  2008   GLAHN   REMOVED KFILSL FROM CALL
C        FEBRUARY  2008   GLAHN   ADDED NAREA TO CALL AND INPUT
C        FEBRUARY  2008   GLAHN   CHANGED USE OF KFILVO; ADDED IWRITA
C        APRIL     2008   GLAHN   ADDED ICOMPT( )
C        MAY       2008   GLAHN   PULLED READING VARIABLE RADIUS FILE
C                                 AND VARIABLES VRAD( ), ELEVLO( ),
C                                 ELEVHI( ), KFILSL, STALST
C        JULY      2008   GLAHN   MODIFIED DIAGNOSTIC FORMAT 1517
C        APRIL     2009   GLAHN   MODIFIED DIAGNOSTIC FORMAT 1445
C        SEPTEMBER 2009   GAW     MODIFIED FOR OPERATIONAL VERSION.
C                                 CHANGED TO USE GET_NCEPDATE.  ANY
C                                 MODIFIED LINES ARE LABELED AS COPS.
C                                 MODIFIED TO ALLOW IT TO READ IN MORE
C                                 THAN ONE 405 CN FILE.
C        DECEMBER  2012   ENGLE   INITIALIZED KFILP=0
C
C USAGE: CALLED BY U155
C        INPUT FILES:
C            KFILDI    - UNIT NUMBER OF INPUT FILE.  (INPUT)  
C            KFILD(J)  - UNIT NUMBERS FOR WHERE THE STATION LIST (J=1)
C                        AND THE STATION DIRECTORY (J=2) RESIDES.
C                        (INPUT)
C            KFILDT    - UNIT NUMBER FOR READING THE DATE LIST.
C                        (INPUT)
C            KFILP     - UNIT NUMBER FOR READING THE VARIABLE LIST.
c                        (INPUT)
C            KFILCP    - UNIT NUMBER FOR VARIABLE CONSTANT FILE.
C                        (INPUT)
C            KFILNI    - UNIT NUMBER FOR READING NEIGHBORS LIST.  (INPUT)
C
C        OUTPUT FILES:
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C            IP(J)     - UNIT NUMBERS FOR OPTIONAL OUTPUT (J=1,25)  
C                        (SEE IP( ) UNDER "VARIABLES" BELOW.)  (OUTPUT)
C
C        VARIABLES
C              KFILDI = UNIT NUMBER TO READ INPUT FILE 'U155.CN'.
C                       (INPUT)
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  INITIALLY,
C                       THIS IS INPUT EQUAL TO 12.  LATER, IN IPOPEN,
C                       IF IP(1) NE 0, KFILDO IS SET = IP(1).  THIS 
C                       ALLOWS CHANGING THE "DEFAULT" PRINT FILE ON 
C                       THE FLY.  OTHERWISE, ON SOME SYSTEMS, THE 
C                       OUTPUT FILE MIGHT HAVE THE SAME NAME AND BE 
C                       OVERWRITTEN.  WHEN THE OUTPUT FILE IS NOT THE 
C                       ORIGINAL DEFAULT, THE NAME IS GENERATED AND 
C                       CAN BE DIFFERENT FOR EACH RUN.  THIS ALLOWS
C                       SAVING EACH OUTPUT AND NOT HAVING IT
C                       OVERWRITTEN.  (INPUT-OUTPUT)
C              KFILIO = UNIT NUMBER OF GRIDDED OUTPUT TDLPACK FILE.
C                       ZERO MEANS OUTPUT WILL NOT BE WRITTEN.
C                       (OUTPUT)
C              KFILVO = UNIT NUMBER OF OUTPUT ASCII FILE WITH
C                       LATITUDES, LONGITUDES, AND DATA FOR GMOS_PLOT.
C                       ZERO MEANS OUTPUT WILL NOT BE WRITTEN.
C                       (OUTPUT)
C              KFILOG = UNIT NUMBER FOR DISPOSABLE TDLPACK GRIDPOINT
C                       OUTPUT.  THIS IS FOR DIFFERENT PASSES OF THE
C                       ANALYSES AND THEIR SMOOTHINGS.  (OUTPUT)
C              KFILOV = UNIT NUMBER OF OUTPUT VECTOR FILE CONTAINING
C                       TOSSED OR QUESTIONABLE OBS AS MISSING.  (OUTPUT)
C              KFILQC = UNIT NUMBER OF OUTPUT VECTOR FILE CONTAINING
C                       QUALITY CONTROLLED OBS AFTER THE FINAL ANALYSIS
C                       PASS.  TOSSED OBS ARE SET TO MISSING.  (OUTPUT)
C           KFILAN(N) = UNIT NUMBER FOR READING INDIVIDUAL ANALYSIS
C                       CONTROL FILES (N=1,ND4).  DIMENSIONING ADDED
C                       FOR IBM.  (OUTPUT)
C              KFIL10 = UNIT NUMBER FOR INTERNAL RANDOM ACCESS FILE.
C                       (OUTPUT)
C              KFILLP = UNIT NUMBER FOR READING STATION PAIRS.
C                       ZERO MEANS PAIRS WILL NOT BE READ.  (OUTPUT)
C              KFILNI = UNIT NUMBER FOR READING STATION NEIGHBORS.
C                       WHEN = 0, THERE IS NO NEIGHBORS LIST.  (OUTPUT)
C               IP(J) = EACH VALUE (J=1,25) INDICATES WHETHER (>1)
C                       OR NOT (=0) CERTAIN INFORMATION WILL BE WRITTEN.
C                       WHEN IP( ) > 0, THE VALUE INDICATES THE UNIT
C                       NUMBER FOR OUTPUT.  THESE VALUES SHOULD NOT BE 
C                       THE SAME AS ANY KFILX VALUES EXCEPT POSSIBLY
C                       KFILDO, WHICH IS THE DEFAULT OUTPUT FILE.  THIS
C                       IS ASCII OUTPUT, GENERALLY FOR DIAGNOSTIC
C                       PURPOSES.  THE FILE NAMES WILL BE 4 CHARACTERS
C                       'U155', THEN 4 CHARACTERS FROM IPINIT, THEN 
C                       2 CHARACTERS FROM IP(J) (E.G., 'U155HRG130').
C                       THE ARRAY IS INITIALIZED TO ZERO IN CASE LESS
C                       THAN THE EXPECTED NUMBER OF VALUES ARE READ IN.
C                       EACH OUTPUT ASCII FILE WILL BE TIME STAMPED.
C                       NOTE THAT THE TIME ON EACH FILE SHOULD BE VERY
C                       NEARLY THE SAME, BUT COULD VARY BY A FRACTION
C                       OF A SECOND.  IT IS INTENDED THAT ALL ERRORS
C                       BE INDICATED ON THE DEFAULT, SOMETIMES IN 
C                       ADDITION TO BEING INDICATED ON A FILE WITH A 
C                       SPECIFIC IP( ) NUMBER, SO THAT THE USER WILL 
C                       NOT MISS AN ERROR.  NOTE THAT SUBROUTINE
C                       IPRINT SETS IP(J) = 0 WHEN IUSE(J) = 0.  IF
C                       IP(J) WAS READ AS NON ZERO, A FILE WITH
C                       UNIT NUMBER IP(J) WILL HAVE BEEN OPENED, BUT
C                       WILL NOT BE TIME STAMPED.
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
C                       (4) = THE INPUT STATION LIST (CALL LETTERS 
C                           ONLY) WHEN THE STATION LIST IS NOT FROM
C                           THE DIRECTORY (I.E., KFILD(1) NE KFILD(2)).
C                           HOWEVER, IF THERE ARE INPUT ERRORS, THE 
C                           STATION LIST WILL ALWAYS BE WRITTEN TO THE 
C                           DEFAULT OUTPUT FILE UNIT KFILDO AS WELL AS
C                           TO UNIT IP(4).
C                       (5) = THE STATIONS AND STATION DIRECTORY 
C                           INFORMATION IN THE ORDER TO BE DEALT WITH 
C                           IN U155.  THE STATIONS WILL BE IN 
C                           ALPHABETICAL ORDER WHEN NALPH = 1, PROVIDED
C                           THE DIRECTORY IS; WHEN NALPH NE 1, THE ORDER
C                           IS AS READ.  IF THERE ARE INPUT ERRORS, THE
C                           STATION LIST WILL BE WRITTEN TO THE DEFAULT
C                           OUTPUT FILE UNIT KFILDO AS WELL AS TO UNIT
C                           IP(5). 
C                       (6) = THE VARIABLES AS THEY ARE BEING READ IN.
C                           THIS IS GOOD FOR CHECKOUT; FOR ROUTINE
C                           OPERATION, IP(7), AND/OR IP(9),
C                           MAY BE BETTER.  
C                       (7) = THE VARIABLE LIST IN SUMMARY FORM.
C                           IF THERE ARE ERRORS, THE VARIABLE LIST WILL 
C                           BE WRITTEN TO THE DEFAULT OUTPUT FILE 
C                           UNIT KFILDO AS WELL AS TO UNIT IP(7).
C                           THIS LIST INCLUDES THE PARSED ID'S IN 
C                           IDPARS( , ).
C                       (8) = THE STATIONS AND THEIR PAIRS AS READ ARE
C                           WRITTEN TO IP8 IF NOT ZERO.  (OUTPUT)
C                       (9) = THE VARIABLE LIST IN SUMMARY FORM .  THIS 
C                           DIFFERS FROM (8) IN THAT (9) DOES NOT 
C                           INCLUDE THE PARSED ID'S IN IDPARS( , ),
C                           BUT RATHER INCLUDES THE INFORMATION TAKEN
C                           FROM THE PREDICTOR CONSTANT FILE ON UNIT 
C                           KFILCP.
C                       (10) = INDICATES WHETHER (>1) OR NOT (=0) THE 
C                           LIST OF FIELDS READ FOR DAY 1 WILL BE 
C                           PRINTED TO THE FILE WHOSE UNIT NUMBER IS 
C                           IP(10).
C                       (11) = INDICATES WHETHER (>0) OR NOT (=0)
C                           THE VARIABLE ID'S OF THE ARCHIVED FIELDS
C                           ACTUALLY NEEDED, IN ORDER AS THEY APPEAR ON
C                           THE THE FIRST DAY OF THE ARCHIVE FILES
C                           WILL BE PRINTED.  THIS IS THE CONTENTS OF
C                           MSTORE( , ).
C                       (12) = INDICATES WHETHER (>1) OR NOT (=0) THE 
C                           LIST OF STATIONS ON THE INPUT FILES WILL BE 
C                           PRINTED TO THE FILE WHOSE UNIT NUMBER IS 
C                           IP(12).  SINCE HOURLY DATA WILL PROBABLY
C                           BE READ AND THE STATION LIST CHANGES
C                           HOURLY, THIS CAN BE VOLUMINOUS OUTPUT.
C                           THE PRINT OCCURS IN SUBROUTINE FINDST.
C                           FINDST ALSO PRINTS A LIST OF STATIONS
C                           NOT FOUND ON THE INPUT FILE (EACH HOUR
C                           READ) UNLESS COMPILED WITH /D OPTION.
C                       (13) = INDICATES WHETHER (>0) OR NOT (=0)
C                           THE CONTENTS OF LSTORE( , ) WILL BE 
C                           WRITTEN TO UNIT IP(13) AFTER COMPRESSION
C                           AFTER EACH DAY NUMBER (CYCLE) LE LSTPRT,
C                           WHICH IS SET IN DATA STATEMENT.
C                       (14) = INDICATES WHETHER (>0) OR NOT (=0)
C                           THE COMPUTED LAPSE RATES WILL BE PROVIDED
C                           ON IP(14).
C                       (15) = INDICATES WHETHER (>0) OR NOT (=0) A
C                           LIST OF THE X AND Y POSITIONS OF THE STATIONS
C                           FOR THE BASIC GRID WILL BE PROVIDED ON
C                           IP(15).
C                       (16) = INDICATES WHETHER (>0) OR NOT (=0) 
C                           A STATEMENT WILL BE OUTPUT TO IP(16)
C                           WHEN A SEQUENTIAL FILE IS WRITTEN THROUGH
C                           PAWOTG.
C                       (17) = INDICATES WHETHER (>0) OR NOT (=0) A
C                           LISTING OF STATIONS, THEIR X/Y POSITIONS,
C                           THEIR DATA VALUES, AND LTAGS WILL BE WRITTEN
C                           AT THE END OF SUBROUTINE ESP TO IP(17).
C                       (18) = INDICATES WHETHER (>0) OR NOT (=0) THE
C                           AVERAGE DEGREE OF FIT BETWEEN THE DATA 
C                           AND THE ANALYSIS WILL BE WRITTEN TO IP(18).
C                           IN ADDITION, WITH THE /D COMPILER OPTION, A
C                           LISTING OF STATIONS, THEIR X/Y POSITIONS,
C                           DATA VALUES, LTAGS, ANALYSIS (INTERPOLATED)
C                           VALUES, AND DIFFERENCES BETWEEN THE DATA
C                           AND THE ANALYSIS VALUES WILL BE WRITTEN
C                           IN SUBROUTINE ESP TO IP(18).
C                       (19) = SAME AS (18) EXCEPT IT APPLIES TO THE
C                           SMOOTHED ANALYSIS.  IF THE ANALYSIS IS NOT
C                           SMOOTHED, IP19 IS NOT WRITTEN TO.
C                       (20) = INDICATES WHETHER (>0) OR NOT (=0) A
C                           LISTING OF STATIONS, THEIR X/Y POSITIONS,
C                           DATA VALUES, LTAGS, ANALYSIS (INTERPOLATED)
C                           VALUES, AND DIFFERENCES BETWEEN THE DATA
C                           AND THE ANALYSIS VALUES WILL BE WRITTEN
C                           IN SUBROUTINE BCD TO IP(20) FOR ONLY
C                           THE SUBSETTED AREA FOR GRIDPRINTING.
C                           IF IOPT( ) IS NOT USED, IP(20) IS NOT
C                           ACTIVATED.
C                       (21) = INDICATES WHETHER (>0) OR NOT (=0) THE
C                           AVERAGE DEGREE OF FIT BETWEEN THE DATA AND 
C                           THE ANALYSIS WILL BE WRITTEN TO UNIT IP(21)
C                           FOR THE UNSMOOTHED AND, IF SMOOTHED, THE 
C                           SMOOTHED ANALYSIS.  THIS PRODUCES ONLY
C                           ONE LINE PER PASS FOR EACH ANALYSIS BEING
C                           DONE (E.G., U400A, U400B, ETC.)
C                       (22) = UNIT NUMBER OF GRIDPRINTED MAPS, IF
C                           OTHER THAN KFILDO.  OPTIONAL PRINTING
C                           IS INDICATED IN ROUTINES.
C                       (23) = INDICATES WHETHER (>0) OR NOT (=0)
C                           STATEMENTS ABOUT EOF AND FILE OPENINGS 
C                           AND CLOSINGS WILL BE OUTPUT FOR PRINTING
C                           ON UNIT IP(23).
C                       (24) = UNIT NUMBER FOR WRITING FIT TO WITHHELD
C                           STATIONS, IF ANY, AND NON-WITHHELD STATIONS OVER
C                           WHOLE ANALYSIS AREA WHEN NWITH NE 0.  LIST OF
C                           WITHHELD STATIONS IS ALSO PROVIDED.
C                       (25) = UNIT NUMBER FOR WRITING PROBLEMS WITH
C                           BOGUS STATIONS.
C                       (OUTPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST.  (CHARACTER*8)
C                       (OUTPUT)
C             ELEV(K) = ELEVATIONS OF STATIONS IN METERS (K=1,NSTA).
C                       THESE ARE READ FROM THE STATION DICTIONARY
C                       BY RDSTQN OR RDSTQA IN FT, BUT ARE CONVERTED
C                       TO METERS BY THOSE READERS.  (OUTPUT)
C            IWBAN(K) = WBAN NUMBERS OF STATIONS (K=1,NSTA).
C           STALAT(K) = LATITUDE OF STATIONS (K=1,NSTA).  (OUTPUT)
C           STALON(K) = LONGITUDE OF STATIONS (K=1,NSTA).  (OUTPUT)
C           ISDATA(K) = WORK ARRAY.  USED IN RDSTAL TO KEEP TRACK OF
C                       THE STATIONS FOUND IN THE DIRECTORY (K=1,NSTA).
C                       (INTERNAL)
C            IPACK(K) = WORK ARRAY.  USED IN RDSTAL AND RDSTAD 
C                       (K=1,NSTA).  (INTERNAL)
C             NAME(K) = NAMES OF STATIONS (K=1,NSTA).  (CHARACTER*20)
C                       (OUTPUT)
C          IQUAL(K,I) = THE QUALITY VALUES FROM THE STATION DICTIONARY
C                       FOR FIVE POSSIBLE DATA TYPES (K=1,ND1) (I=1,5).
C                       (OUTPUT)
C           LNDSEA(K) = LAND/SEA INFLUENCE FLAG FOR EACH STATION
C                       (K=1,ND1).
C                       0 = WILL BE USED FOR ONLY OCEAN WATER (=0)
C                           GRIDPOINTS.
C                       3 = WILL BE USED FOR ONLY INLAND WATER (=3)
C                           GRIDPOINTS.
C                       6 = WILL BE USED FOR BOTH INLAND WATER (=3)
C                           AND LAND (=9) GRIDPOINTS.
C                       9 = WILL BE USED FOR ONLY LAND (=9) GRIDPOINTS.
C                       (OUTPUT)
C                NSTA = THE NUMBER OF STATIONS BEING DEALT WITH.  THE
C                       NUMBER OF VALUES IN CCALL( , ), ETC.  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                       WITH.  (INPUT)
C           CCALLD(K) = WORK ARRAY.  8 STATION CALL LETTERS (K=1,ND5).
C                       THIS IS USED IN RDSTAL AND RDSTAD.
C                       (CHARACTER*8)  (INTERNAL)
C                 ND5 = DIMENSION OF CCALLD( ).  (INPUT)
C               NAREA = THE AREA OVER WHICH THE ANALYSIS IS MADE:
C                       1 = CONUS,
C                       2 = ALASKA,
C                       3 = HAWAII,
C             ID(J,N) = THE INTEGER VARIABLE ID'S (J=1,4) (N=1,NPRED).
C                       (OUTPUT)
C         IDPARS(J,N) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       VARIABLE ID'S CORRESPONDING TO ID( ,N) 
C                       (J=1,15), (N=1,NPRED).  (OUTPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                          0 = NOT BINARY,
C                          1 = CUMULATIVE FROM ABOVE, VALUES GE LOWER 
C                              THRESHOLD TRESHL = 1,
C                          2 = CUMULATIVE FROM BELOW, VALUES LT UPPER 
C                              THRESHOLD TRESHU = 1.
C                          3 = DISCRETE BINARY.  VALUES GE LOWER 
C                              THRESHOLD AND LT UPPER THRESHOLD = 1.
C                          5 = GRID BINARY.  VALUES GE LOWER THRESHOLD
C                          ONLY THE VALUE OF 0, 1, OR 5 SHOULD BE USED 
C                          FOR PREDICTORS;
C                          0, 1, 2, OR 3 CAN BE USED FOR PREDICTANDS.
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY
C                            1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK 
C                               IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C                             G WILL BE 0 ON INPUT.  U155 USES OTHER
C                             VALUES INTERNALLY (ONLY) TO INDICATE
C                             GRID LENGTH.
C                       (OUTPUT)
C           THRESH(N) = THE UPPER BINARY THRESHOLD CORRESPONDING TO 
C                       IDPARS( ,N) (N=1,ND4).  (OUTPUT)
C             JD(J,N) = THE BASIC INTEGER VARIABLE ID'S (J=1,4)
C                       (N=1,ND4).  THIS IS THE SAME AS ID(J,N),
C                       EXCEPT THAT THE PORTIONS PERTAINING TO 
C                       PROCESSING ARE OMITTED:
C                       B = IDPARS(3, ),
C                       T = IDPARS(8,),
C                       I = IDPARS(13, ),
C                       S = IDPARS(14, ),
C                       G = IDPARS(15, ), AND
C                       THRESH( ).
C                       JD( , ) IS USED TO IDENTIFY THE BASIC MODEL
C                       FIELDS AS READ FROM THE ARCHIVE.  DERIVED FROM
C                       IDPARS( ) IN SUBROUTINE BASICP.  (OUTPUT)
C             JP(J,N) = INDICATES WHETHER A PARTICULAR VARIABLE N MAY
C                       HAVE GRIDPRINTS (J=1), INTERMEDIATE TDLPACK
C                       OUTPUT (J=2), OR PRINT OF VECTOR RECORDS IN
C                       PACKV (J=3) (N=1,ND4).  PACKV IS FOR THE 
C                       DATA SHOWING TOSSED DATA AS MISSING AND 
C                       QUESTIONABLE DATA AS MISSING.  THIS IS
C                       AN OVERRIDE FEATURE FOR THE PARAMETERS FOR 
C                       GRIDPRINTING AND TDLPACKING IN EACH VARIABLE'S 
C                       CONTROL FILE.  (OUTPUT)
C              NCEPNO = NCEP MODEL NUMBER FOR RUN.  (OUTPUT)
C               MODNO = DD FOR WRITING OUTPUT.  (OUTPUT)
C               NPRED = THE NUMBER OF ENTRIES IN ID( , ), ETC.  WHILE
C                       THIS NAME, USED IN U201, IS NOT VERY DESCRIPTIVE
C                       FOR U155, IT IS USED TO BE CONSISTENT WITH
C                       OTHER SOFTWARE.  (OUTPUT)
C           ISCALD(N) = THE DECIMAL SCALING CONSTANT TO USE WHEN PACKING
C                       THE DATA (N=1,ND4).  ISCALD COMES FROM 
C                       THE VARIABLE CONSTANT FILE, MODIFIED TO BE 2 FOR 
C                       GRID BINARIES, AND 0 FOR POINT BINARIES.  ZERO
C                       WHEN NOT FOUND IN THE FILE.  NO BINARY SCALING
C                       IS PROVIDED FOR.  (OUTPUT)
C           IWRITS(N) = 1 WHEN ANALYSIS FOR VARIABLE N IS TO BE WRITTEN
C                       TO INTERNAL STORAGE; 0 OTHERWISE (N=1,ND4).
C                       (OUTPUT)
C           IWRITA(N) = 1 WHEN ASCII DATA FOR VARIABLE N IS TO BE WRITTEN
C                       TO FILE UNIT NUMBER KFIOVO; 0 OTHERWISE (N=1,ND4).
C                       (OUTPUT)
C           ICOMPT(N) = SIGNALS WHETHER THE VARIABLE IS TO BE ANALYZED
C                       OR COMPUTED (N=1,ND4).
C                       0 WHEN THE VARIABLE IS TO BE ANALYZED; THE USUAL
C                         CASE.
C                       1 WHEN THE VARIABLE IS NOT TO BE ANALYZED BUT TO
C                         BE COMPUTED FROM OTHER ALREADY ANALYZED 
C                         VARIABLES.
C                       (OUTPUT)
C            SMULT(N) = THE MULTIPLICATIVE FACTOR WHEN CONTOURING OR
C                       GRIDPRINTING THE DATA (N=1,ND4).
C                       TAKEN FROM THE VARIABLE CONSTANT FILE.  (OUTPUT)
C             SADD(N) = THE ADDITIVE FACTOR WHEN CONTOURING OR
C                       GRIDPRINTING THE DATA (N=1,ND4).
C                       TAKEN FROM THE VARIABLE CONSTANT FILE.  (OUTPUT)
C           ORIGIN(N) = THE CONTOUR ORIGIN, APPLIES TO THE UNITS IN
C                       UNITS(N) (N=1,ND4).
C                       TAKEN FROM THE VARIABLE CONSTANT FILE.  (OUTPUT)
C             CINT(N) = THE CONTOUR INTERVAL, APPLIES TO THE UNITS IN
C                       UNITS(N) (N=1,ND4).
C                       TAKEN FROM THE VARIABLE CONSTANT FILE.  (OUTPUT)
C           ANLTAB(N) = THE CONTROL FILE NAME FOR THE VARIABLE 
C                       (N=1,NPRED).  (CHARACTER*17) (OUTPUT)
C           INLTAB(N) = UNIT NUMBER FOR CONTROL FILE ANLTAB(N) REQUIRED
C                       BY THE IBM.  (OUTPUT)
C            PLAIN(N) = THE PLAIN LANGUAGE DESCRIPTION OF THE VARIABLES
C                       (N=1,ND4).  (CHARACTER*32)  (OUTPUT)
C                       TAKEN FROM THE VARIABLE CONSTANT FILE.  (OUTPUT)
C            UNITS(N) = THE UNITS OF THE DATA THAT APPLY AFTER
C                       MULTIPLYING BY SMULT(N) AND ADDING SADD(N) 
C                       (N=1,ND4).  TAKEN FROM THE VARIABLE CONSTANT
C                       FILE.  (CHARACTER*12)  (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64).  (INPUT)
C                 ND4 = MAXIMUM NUMBER OF VARIABLES THAT CAN BE DEALT 
C                       WITH IN ONE RUN.  SECOND DIMENSION OF ID( , ),
C                       IDFORC( , ) JD( , ), JP( , ), AND IDPARS( , )
C                       AND DIMENSION OF THRESH( ), PLAIN( ) UNITS( ),
C                       ORIGIN( ), CINT( ), SMULT( ), AND SADD( ). 
C                       (INPUT)
C           KFILIN(J) = UNIT NUMBERS FOR INPUT DATA, ALL IN TDL MOS-2000
C                       TDLPACK FORMAT (J=1,ND6).
C                       UNIT NUMBERS GE 80 ARE RESERVED FOR GRID DATA;
C                       UNIT NUMBERS LT 80 ARE RESERVED FOR VECTOR DATA.
C                       (OUTPUT)
C           MODNUM(J) = THE "MODEL" NUMBER CORRESPONDING TO KFILIN(J),
C                       AND NAMIN(J) (J=1,ND6).  FOR VECTOR DATA,
C                       MODDUM( ) = 0.  (OUTPUT)
C            NAMIN(J) = HOLDS DATA SET NAMES FOR THE UNIT NUMBERS
C                       IN KFILIN(J) (J=1,ND6).  (CHARACTER*60) 
C                       (OUTPUT)
C           JFOPEN(J) = FOR EACH FILE IN MODNUM(J), JFOPEN(J) IS SET
C                       TO 1 WHEN THE FILE IS OPEN AND IS SET TO 2
C                       WHEN THE FILE IS NOT OPEN BUT IS AVAILABLE
C                       (J=1,NUMIN).  (OUTPUT)
C               NUMIN = THE NUMBER OF VALUES IN KFILIN( ) AND JFOPEN( )
C                       AND NAMES IN NAMIN( ).  MAXIMUM OF ND6.
C                       (OUTPUT)
C                 ND6 = MAXIMUM NUMBER OF INPUT DATA SETS (MODELS) THAT 
C                       CAN BE DEALT WITH.  (INPUT)
C           KFILRA(J) = UNIT NUMBERS FOR READING CONSTANT DATA (J=1,6).
C                       THE ACCESS ROUTINES ALLOW 6 RANDOM ACCESS
C                       FILES.  HOWEVER, IT UNLIKELY U155 WILL NEED
C                       MORE THAN 1 OR 2.  (OUTPUT)
C           RACESS(J) = FILE NAMES FOR CONSTANT DATA READ ON UNIT
C                       NOS. KFILRA(J) (J=1,6).  (CHARACTER*60) 
C                       (OUTPUT)
C               NUMRA = NUMBER OF VALUES IN KFILRA( ) AND RACESS( ).
C                       (OUTPUT)
C              GOTNAM = NAME OF DATA SET FOR OUTPUT GRIDS CORRESPONDING
C                       TO UNIT NO. KFILIO.  (CHARACTER*60)  (OUTPUT)
C              VOTNAM = NAME OF DATA SET FOR OUTPUT DATA IN FORMAT
C                       CORRESPONDING TO UNIT NO. KFILVO.
C                       (CHARACTER*60)  (OUTPUT)
C              OUTDIS = NAME OF DATA SET FOR DISPOSABLE GRIDS IN TDLPACK
C                       FORMAT CORRESPONDING TO UNIT NUMBER KFILOG.
C                       (CHARACTER*60)  (OUTPUT)
C              OUTVEC = NAME OF DATA SET FOR VECTOR DATA IN TDLPACK
C                       FORMAT CORRESPONDING OT UNIT NUMBER KFILOV.
C                       (CHARACTER*60)  (OUTPUT)
C              OUTQCV = NAME OF DATA SET FOR QUALITY CONTROLLED DATA
C                       IN TDLPACK FORMAT CORRESPONDING TO UNIT 
C                       NO. KFILQC.  (CHARACTER*60)  (OUTPUT)
C           ANLNAM(N) = NAME OF DATA SET FOR READING THE INDIVIDUAL
C                       ANALYSIS .CN FILES CORRESPONDING TO UNIT 
C                       NO. KFILAN(N) (N=1,ND4).  DIMENSIONING NEEDED
C                       FOR IBM.  (CHARACTER*60)  (OUTPUT)
C              STAPRS = NAME OF DATA SET FOR READING STATION PAIRS.
C                       (CHARACTER*60)  (OUTPUT)
C            IDATE(J) = INITIAL DATE LIST (J=1,NDATES) WHICH MAY CONTAIN
C                       NEGATIVE VALUES INDICATING A DATE SPAN.  THIS
C                       IS MODIFIED IN DATPRO TO CONTAIN THE COMPLETE
C                       DATE LIST WITH THE DATES IN THE SPANS FILLED IN
C                       (J=1,NDATES), WHERE NDATES HAS BEEN INCREASED
C                       IF NECESSARY.  DATES ARE INPUT AS YYMMDDHH AND
C                       MODIFIED TO YYYYMMDDHH.  ZEROS IN THE INPUT ARE
C                       ELIMINATED.  TERMINATOR IS 99999999.  MAXIMUM
C                       NUMBER OF DATES IS ND8.  (OUTPUT)
C              NDATES = THE NUMBER OF DATES IN IDATE( ).  (OUTPUT)
C            NWORK(J) = WORK ARRAY (J=1,ND8).  (INTERNAL)
C                 ND8 = MAXIMUM NUMBER OF DATES THAT CAN BE DEALT WITH.
C                       DIMENSION OF IDATE( ) AND NWORK( ).  (INPUT)
C              INCCYL = THE NUMBER OF HOURS BETWEEN DATES WHEN DATE 
C                       SPANNING IS USED.  (INTERNAL/OUTPUT)
C               KSKIP = WHEN NONZERO, INDICATES THAT THE OUTPUT FILE
C                       ON UNIT NO. KFILIO IS TO BE MOVED FORWARD 
C                       UNTIL ALL DATA FOR DATE KSKIP HAVE BEEN SKIPPED.
C                       KSKIP IS INPUT AS YYMMDDHH OR YYYYMMDDHH AND
C                       THEN USED AS YYYYMMDDHH.  (OUTPUT)
C               NSKIP = THE NUMBER OF ERRORS THAT WILL BE TOLERATED ON 
C                       DAY 1 WITHOUT HALTING.  (OUTPUT)
C               JSTOP = THE NUMBER OF ERRORS THAT WILL BE TOLERATED ON 
C                       THE TOTAL RUN BEFORE PROGRAM STOPS.  (OUTPUT)
C              PXMISS = THE VALUE OF A SECONDARY MISSING VALUE TO INSERT
C                       WHEN THE SECONDARY MISSING VALUE IS 9997.
C                       THIS ALLOWS MAINTAINING A 9997, TREATING IT AS 
C                       ZERO, AS 9999, OR AS SOME OTHER VALUE.  (OUTPUT)
C               NRPOJ = MAP PROJECTION.  (OUTPUT)
C              ORIENT = ORIENTATION OF GRID IN WEST LONGITUDE.  (INPUT)
C                XLAT = NORTH LATITUDE AT WHICH GRIDLENGTH IS SPECIFIED.
C                       (INPUT)
C               ALATL = NORTH LATITUDE OF LOWER LEFT CORNER POINT
C                       OF A GRID OF THE SIZE  NXL, NYL.  TRUNCATED
C                       TO TEN THOUSANDTHS OF DEGREES.  NOTE THAT THE
C                       MOS-2000 ARCHIVE IS ONLY TO THOUSANDTHS OF
C                       DEGREES.  (OUTPUT)
C               ALONL = WEST LONGITUDE OF LOWER LEFT CORNER POINT
C                       OF A  GRID OF THE SIZE  NXL, NYL.  TRUNCATED
C                       TO TEN THOUSANDTHS OF DEGREES.  NOTE THAT THE
C                       MOS-2000 ARCHIVE IS ONLY TO THOUSANDTHS OF
C                       DEGREES.  (OUTPUT)
C                 NXL = THE SIZE OF THE ANALYSIS GRID FOR THIS RUN
C                       IN THE X DIRECTION IN MESHB UNITS.  (OUTPUT)
C                 NYL = THE SIZE OF THE ANALYSIS GRID FOR THIS RUN
C                       IN THE Y DIRECTION IN MESHB UNITS.  (OUTPUT)
C               MESHB = THE NOMINAL MESH LENGTH IN KM OF THE ANALYSIS
C                       GRID SPECIFIED BY NXL, NYL AT LATITUDE XLAT.
C                       FOR INSTANCE, NOMINAL 80 CORRESPONDS
C                       TO 95.25 KM FOR POLAR STEREOGRAPHIC.  FOR
C                       ALL ROUTINES TO WORK, THIS VALUE MUST BE 
C                       1, 3, 5, 10, 20, 40, 80, 160, OR 320.
C                       THE LOWER NUMBERS ARE INTEGERS APPROXIMATING
C                       EVEN FRACTIONS OF BEDIENTS.  (OUTPUT)
C               BMESH = ACTUAL MESH LENGTH IN KM CORRESPONDING TO
C                       MESHB.  THE FINAL NXL BY NYL ANALYSIS OUTPUT
C                       WILL BE AT MESH LENGTH BMESH.  (OUTPUT)
C               MESHL = NOMINAL MESH LENGTH IN KM OF QUALITY CONTROL
C                       (SUBSETTED) GRID FOR CONTINUOUS VARIABLES.
C                       (OUTPUT) 
C              XMESHL = ACTUAL MESH LENGTH IN KM CORRESPONDING TO
C                       MESHL.  (OUTPUT)
C               MESHD = NOMINAL MESH LENGTH IN KM OF QUALITY CONTROL
C                       (SUBSETTED) GRID FOR DISCONTINUOUS VARIABLES. 
C                       (OUTPUT) 
C               DMESH = ACTUAL MESH LENGTH IN KM CORRESPONDING TO
C                       MESHD.  (OUTPUT)
C               MESHE = THE NOMINAL MESH LENGTH OF THE TERRAIN GRID IN
C                       KM.  IT IS MANDATORY THE GRID AVAILABLE IS OF
C                       THIS MESH SIZE AND COVER THE SAME AREA SPECIFIED
C                       BY NXL BY NYL, EVEN IF MESHE IS NOT EQUAL
C                       TO MESHB.  (OUTPUT)
C               EMESH = ACTUAL MESH LENGTH CORRESPONDING TO MESHE.
C                       (OUTPUT)
C              IPRTEL = 1 TO GRIDPRINT OR WRITE TO FILE NO. KFILOG
C                       THE TERRAIN AND THE LAND/SEA MASK GRIDS.
C                       0 OTHERWISE.  (OUTPUT)
C            IOPTB(J) = SUBSETTING VALUES USED IN GRIDPRINTING AND
C                       TDLPACKING IN RELATION TO MESHB (J=1,8).
C                       INITIALIZED FROM NXGMIN, ETC.  (OUTPUT)
C                       1 - 0 OPTION TABLE NOT USED/1 USED. 
C                       2 - SUB ARRAY MIN IX VALUE. 
C                       3 - SUB ARRAY MAX IX VALUE. 
C                       4 - SUB ARRAY MIN JY VALUE. 
C                       5 - SUB ARRAY MAX JY VALUE. 
C                       6 - 1 IF ALL INTERPOLATION IS TO BE BILINEAR. 
C                           OTHERWISE, INTERPOLATION IS TO BE 
C                           BIQUADRATIC WHERE POSSIBLE. 
C                       7 - NOT USED. 
C                       8 - PAGE WIDTH IN GRID POINTS. 
C              MINVEC = THE MINIMUM NUMBER OF HOURS OF DATA TO SAVE
C                       FOR VECTOR DATA.  (OUTPUT)
C              MINMOD = THE MINIMUM NUMBER OF HOURS OF DATA TO SAVE
C                       FOR GRIDPOINT DATA.  THIS IS MODIFIED AS
C                       NECESSARY BASED ON ASSUMPTION OF A MODEL
C                       RUN NO OFTENER THAN 6 HOURS.  (OUTPUT)
C                ISTA = 1 TO READ STATION DIRECTORY.  0 OTHERWISE.
C                       THE POINTS TO ANALYZE CAN COME FROM THE
C                       DIRECTORY (ISTA=1), FROM RANDOMLY SAMPLED
C                       POINTS (ISTA=0), OR BOTH (ISTA=1).  (OUTPUT)
C               ISMPL = MAXIMUM NUMBER OF POINTS TO SAMPLE FROM THE
C                       FIRST GUESS FIELD.  0 OTHERWISE.  THE POINTS
C                       TO ANALYZE CAN COME FROM THE DIRECTORY ONLY
C                       (ISMPL=0), FROM SAMPLED POINTS (ISMPL GT 0),
C                       OR BOTH (ISMPL GT 0).  NSTA FROM THE DIRECTORY
C                       + ISMPL MUST NOT EXCEED ND1 FOR ALL POINTS
C                       TO BE USED.  THE RAMDOM POINTS ARE DETERMINED
C                       IN U155, BUT THE SAMPLING IS DONE IN U405A.
C                       (OUTPUT)
C               ISTOP = 0 MEANS THE PROGRAM IS RUNNING OK UP TO THIS
C                       POINT.  WHENEVER AN ERROR OCCURS THAT SHOULD
C                       HALT THE PROGRAM AFTER INPUT DIAGNOSTICS ARE
C                       PRINTED, ISTOP IS SET = ISTOP+1.  (INPUT-OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       777 = FATAL ERROR.
C                       OTHER VALUES CAN COME FROM CALLED SUBROUTINES.
C                       (OUTPUT)
C               RUNID = INFORMATION INPUT TO IDENTIFY THE OUTPUT ON
C                       KFILDO.  (CHARACTER*72)  (INTERNAL)
C              IPINIT = 4 CHARACTERS, USUALLY A USER'S INITIALS PLUS
C                       A RUN NUMBER, TO APPEND TO 'U155' TO IDENTIFY 
C                       A PARTICULAR SEGMENT OF OUTPUT INDICATED BY A 
C                       SUFFIX IP(J).  THE RUN NUMBER ALLOWS MULTIPLE
C                       RUNS OF U155 AND WRITING OF UNIQUELY NAMED
C                       FILES, PROVIDED THE USER USES A DIFFERENT RUN 
C                       NUMBER FOR EACH RUN.  (CHARACTER*4)  (OUTPUT)
C              KFILDT = UNIT NUMBER FOR READING THE DATE LIST. 
C                       (INTERNAL)
C              DATNAM = FILE NAME FOR READING DATE LIST.  CORRESPONDS
C                       TO KFILDT.  (CHARACTER*60) (INTERNAL) 
C              KFILCP = UNIT NUMBER FOR VARIABLE CONSTANT FILE.  THIS
C                       CONTAINS DEFAULT VALUES FOR CERTAIN CONSTANTS 
C                       FOR BASIC NMC VARIABLES AND OTHER VARIABLES
C                       SANS THRESHOLDS, ETC.  THESE INCLUDE PACKING 
C                       CONSTANTS, GRIDPOINT CONSTANTS, AND NAMES. 
C                       (OUTPUT)
C              CONNAM = HOLDS DATA SET NAME FOR THE VARIABLE CONSTANT
C                       FILE.  CORRESPONDS TO KFILCP.  (CHARACTER*60) 
C                       (INTERNAL)
C            KFILD(J) = THE UNIT NUMBERS FOR WHERE THE STATION LIST
C                       (J=1) AND THE STATION DIRECTORY (J=2) RESIDES.  
C                       CORRESPONDS TO DIRNAM(J).  WHEN KFILD(1) =
C                       KFILDI, THE DEFAULT INPUT IS INDICATED,
C                       DIRNAM(1) IS NOT USED, AND THE FILE IS NOT
C                       OPENED.  KFILD(1) CAN EQUAL KFILD(2), IN WHICH
C                       CASE THE STATION LIST IS TAKEN FROM THE 
C                       DIRECTORY (I.E., A SEPARATE STATION LIST IS 
C                       NOT PROVIDED).  (INTERNAL)
C           DIRNAM(J) = HOLDS NAME OF DATA SET CONTAINING THE STATION
C                       CALL LETTERS (J=1) AND STATION DIRECTORY (J=2).
C                       CORRESPONDS TO UNIT NO. KFILD(J).  IT IS 
C                       EXPECTED THAT THE STATIONS IN THE DIRECTORY
C                       BE ORDERED ALPHABETICALLY BY CALL LETTERS.
C                       (CHARACTER*60)  (INTERNAL)
C               KFILP = UNIT NUMBER FOR READING THE VARIABLE LIST.
C                       THESE ARE THE VARIABLES FOR WHICH ANALYSES
C                       ARE TO BE MADE.  (OUTPUT)
C              PRENAM = HOLDS DATA SET NAME FOR THE UNIT NUMBER
C                       FOR THE VARIABLE LIST.  CORRESPONDS TO
C                       KFILP.  (CHARACTER*60)  (INTERNAL)
C            ITEMP(J) = SCRATCH ARRAY (J=1,7).  (INTERNAL)
C               STATE = VARIABLE SET TO STATEMENT NUMBER TO INDICATE
C                       WHERE AN ERROR OCCURRED.  (CHARACTER*4) 
C                       (INTERNAL)
C                 NEW = 1 WHEN NEW 4-LETTER CALL LETTERS ARE TO BE 
C                         USED;
C                       0 WHEN OLD 3-LETTER CALL LETTERS ARE TO BE
C                         USED.
C                       (INTERNAL)
C               NALPH = 1 WHEN THE CALL STATIONS USED ARE TO BE
C                         ALPHABETIZED, OR MORE EXACTLY, PUT 
C                         IN THE ORDER THEY EXIST IN THE STATION 
C                         DIRECTORY.
C                       0 WHEN THE ORDER READ IN IS TO BE PRESERVED.
C                       (INTERNAL)
C             IUSE(J) = EACH VALUE J PERTAINS TO IP(J).  WHEN AN IP(J)
C                       VALUE IS USED BY THE PROGRAM, IUSE(J) = 1;
C                       OTHERWISE, IUSE(J) = 0.  USED BY IPRINT TO
C                       PRINT IP( ) VALUES.  NOTE THAT SUBROUTINE
C                       IPRINT SETS IP(J) = 0 WHEN IUSE(J) = 0.  IF
C                       IP(J) WAS READ AS NON ZERO, A FILE WITH
C                       UNIT NUMBER IP(J) WILL HAVE BEEN OPENED, BUT
C                       WILL NOT BE TIME STAMPED.  (INTERNAL)
C              NXGMIN = THE MINIMUM NX VALUE FOR GRIDPRINTING.
C                       IOPT( ) IN PRTGR ALLOWS SUBSETTING OF THE
C                       ANALYSIS AREA FOR GRIDPRINTING.  THIS APPLIES
C                       TO THE GRID BEING DEALT WITH, WHICH WAS
C                       SPECIFIED IN TERMS OF MESHB.
C                       THE GRIDPRINTED MAP WILL ALWAYS BE OVER THIS
C                       SUBSETTED AREA AT THE MESH LENGTH MESHL, NO
C                       MATTER WHAT THE MESH LENGTH IS OF THE MAP
C                       GRIDPRINTED.  THIS MAY MEAN INTERPOLATING OR
C                       THINNING BEFORE GRIDPRINTING.  (INTERNAL)
C              NXGMAX = THE MAXIMUM NX VALUE FOR GRIDPRINTING.
C                       SEE NXGMIN( ).  (INTERNAL)
C              NYGMIN = THE MINIMUM NY VALUE FOR GRIDPRINTING.
C                       SEE NXGMIN( ).  (INTERNAL)
C              NYGMAX = THE MAXIMUM NY VALUE FOR GRIDPRINTING.
C                       SEE NXGMIN( ).  (INTERNAL)
C              KFILKY = WHEN 0, KFILOG WILL BE SET TO 0, INDICATING
C                       NO SUBSET WRITING.  DO THIS WHEN SUBSET AREA
C                       IS INCONSISTENT.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        SUBPROGRAMS CALLED:
C            IPRINT, ACTUAL, CHKSIZ, RDV155, GET_NCEPDATE, W3TAGE
C          UNIQUE: - NONE
C          LIBRARY:
C           MDLLIB - IPOPEN, IERX, DATPRO, RDI, RDSNAM, RDSTAL, RDSTAD,
C             TIMPR, IPRINT, ACTUAL, CHKSIZ, RDV155
C           W3LIB - W3TAGE
C
C        EXIT STATES:
C          COND =    0 - SUCCESSFUL RUN
C                  134 - ERROR IN NCEPDATE
C
C REMARKS:  NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90 (xlf90 compiler)
C   MACHINE: IBM SP
C
C$$$
C
      CHARACTER*4 STATE,IPINIT
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*17 ANLTAB(ND4)
      CHARACTER*12 UNITS(ND4)
      CHARACTER*20 NAME(ND1)
      CHARACTER*32 PLAIN(ND4)
      CHARACTER*60 ANLNAM(ND4)
      CHARACTER*60 NAMIN(ND6),RACESS(6)
      CHARACTER*60 DIRNAM(2),PRENAM,CONNAM,DATNAM,GOTNAM,OUTDIS,
     1             OUTVEC,OUTQCV,VOTNAM,STAPRS,STANEI
      CHARACTER*72 RUNID/' '/
C
      DIMENSION ELEV(ND1),IWBAN(ND1),STALAT(ND1),STALON(ND1),
     1          ISDATA(ND1),IQUAL(ND1,5),LNDSEA(ND1)
      DIMENSION ID(4,ND4),IDPARS(15,ND4),THRESH(ND4),JD(4,ND4),
     1          JP(3,ND4),ISCALD(ND4),IWRITS(ND4),IWRITA(ND4),
     2          ICOMPT(ND4),INLTAB(ND4),KFILAN(ND4),
     3          SMULT(ND4),SADD(ND4),ORIGIN(ND4),CINT(ND4)
      DIMENSION IPACK(ND5)
      DIMENSION KFILIN(ND6),MODNUM(ND6),JFOPEN(ND6)
      DIMENSION IDATE(ND8),NWORK(ND8)
      DIMENSION ITEMP(7),IP(25),IUSE(25),KFILD(2),KFILRA(6),IOPTB(8)
C
      DATA IUSE/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/
C
C        INITIALIZE ARRAYS HOLDING FILE UNIT NUMBERS.
C
      KFILP=0
      DO 98 J=1,ND6
         KFILIN(J)=0
 98   CONTINUE
C
      DO 99 J=1,6
         KFILRA(J)=0
 99   CONTINUE
C
      DO 100 J=1,2
         KFILD(J)=0
 100  CONTINUE
C
C        THE KFILDO OUTPUT HAS BEEN TIME STAMPED IN THE DRIVER.
C        NOTE THAT THIS IS ON THE DEFAULT OUTPUT FILE KFILDO.
C        IF IP(1) NE 0, KFILDO IS SET TO IP(1) AND IS TIME STAMPED
C        BELOW.
C     
      STATE='105 '
COPS      OPEN(UNIT=KFILDI,FILE='U155.CN',STATUS='OLD',IOSTAT=IOS,ERR=900)
C
C        READ AND PROCESS THE PRINT UNIT NUMBERS.  FIRST,
C        INITIALIZE IP( ) IN CASE NOT ALL 25 VALUES ARE READ.
C   
      DO 105 J=1,25
      IP(J)=0
 105  CONTINUE  
C
      STATE='108 ' 
      READ(KFILDI,108,IOSTAT=IOS,ERR=900,END=109)IPINIT,(IP(J),J=1,25)
 108  FORMAT(A4,25I3)
C        LESS THAN 25 IP( ) VALUES WILL NOT BE INDICATED AS AN ERROR.
C        SOME IP( ) VALUES ARE NOT USED; SEE IUSE( ).

      CALL IPOPEN(KFILDO,'U155',IPINIT,IP,IER)
C        WHEN IP(1) NE 0, KFILDO HAS BEEN SET TO IP(1).
C        A FILE WILL BE OPENED FOR EVERY DIFFERENT VALUE IN IP( ).
C        THE FILE NAMES WILL BE 4 CHARACTERS 'U155' THEN 4 CHARACTERS
C        FROM IPINIT, THEN 2 CHARACTERS FROM IP(J).  IPINIT MIGHT BE
C        'HRG1' INDICATING THE PERSONS INITIALS PLUS A SEQUENCE NUMBER.
      IF(IER.NE.0)ISTOP=ISTOP+1
 109  WRITE(KFILDO,110)IPINIT
 110  FORMAT(/' IPINIT = ',A4)
C
C        PRINT THE IP VALUES.  WHEN IUSE(J) = 0, THE CORRESPONDING
C        IP(J) VALUE IS CONSIDERED TO NOT BE USED, AND IP(J) IS
C        SET TO 0.  IF IP(J) WAS READ AS NON ZERO, THE FILE WITH
C        THAT UNIT NUMBER HAS BEEN OPENED IN IPOPEN, BUT WILL
C        NOT BE TIME STAMPED BELOW, BECAUSE IP(J) IS NOW ZERO.
C
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
 112  CALL TIMPR(IP(J),IP(J),'START U155          ')
 113  CONTINUE
C
C        READ AND PRINT THE RUN IDENTIFICATION.
C
      STATE='115 '
      READ(KFILDI,115,IOSTAT=IOS,ERR=900,END=116)RUNID 
 115  FORMAT(A72)
C        LESS THAN 72 CHARACTERS WILL NOT BE CONSIDERED AN ERROR.  
 116  WRITE(KFILDO,117)RUNID
 117  FORMAT(/' ',A72)
C
C        PRINT TO MAKE SURE USER KNOWS WHAT MACHINE IS BEING USED.
C 
      WRITE(KFILDO,119)L3264B
 119  FORMAT(/' RUNNING ON A',I3,'-BIT MACHINE.')
C
C        READ AND PRINT CONTROL INFORMATION.
C
      STATE='125 ' 
      READ(KFILDI,125,IOSTAT=IOS,ERR=900,END=1250)
     1     KSKIP,NSKIP,JSTOP,INCCYL,NEW,NALPH,PXMISS,NAREA,
     2     NPROJ,ORIENT,XLAT,MESHB,MESHE,
     3     NXL,NYL,ALATL,ALONL,MESHL,MESHD,
     4     NXGMIN,NXGMAX,NYGMIN,NYGMAX,
     5     NCEPNO,MODNO,MINVEC,MINMOD,IPRTEL,ISTA,ISMPL
 125  FORMAT(6(I10/),F10.0/I10/I10/2(F10.0/),4(I10/),2(F10.0/),
     1      12(I10/),I10)
      GO TO 1255
C        INCOMPLETE CONTROL INFORMATION SHOULD BE CONSIDERED AN ERROR.
C        HOWEVER, A SHORT RECORD DOES NOT CAUSE AN "END" CONDITION.
1250  WRITE(KFILDO,1251)
1251  FORMAT(/' ****CONTROL INFORMATION NOT COMPLETE.')
      ISTOP=ISTOP+1
C
C        ACCEPT KSKIP AS YY OR YYYY FOR YEAR.  IF IT IS ZERO, NO
C        SKIPPING IS DONE.  KSKIP REFERS TO THE OUTPUT SEQUENTIAL
C        FILE.
C
 1255 IF(KSKIP.EQ.0)GO TO 1257
      IF(KSKIP/1000000.GT.1900)GO TO 1257
      IF(KSKIP/1000000.GT.60)KSKIP=KSKIP+1900000000
      IF(KSKIP/1000000.LE.60)KSKIP=KSKIP+2000000000
C
C        CHECK MAP PROJECTION NPROJ IS ONE OF THREE ACCOMMODATED.
C
      IF(NPROJ.EQ.3.OR.NPROJ.EQ.5.OR.NPROJ.EQ.7)THEN
         GO TO 1257
      ELSE
         WRITE(KFILDO,1256)NPROJ
 1256    FORMAT(/' ****MAP PROJECTION NUMBER NPROJ =',i4,
     1           ' NOT 3, 5, OR 7.  FATAL ERROR IN INT155.')
         ISTOP=ISTOP+1
         IER=777
         GO TO 160
      ENDIF
C
C        GET TRUE MESH LENGTH CORRESPONDING TO MESHL, MESHD,
C        MESLB, AND MESHE.
C
 1257 CALL ACTUAL(KFILDO,MESHL,XMESHL,TRASH,NPROJ,IER)
C
      IF(IER.NE.0)THEN
C           IF ACTUAL GIVES IER NE 0, IT WILL HAVE FURNISHED A
C           DIAGNOSTIC.  THIS IS TREATED AS A FATAL ERROR.
         WRITE(KFILDO,1258)MESHL,IER
 1258    FORMAT('     FATAL ERROR IN INT155 NEAR 1258.  ',
     1          'NOMINAL MESH LENGTH =',I4,' IER =',I4)
         GO TO 902
      ENDIF
C
      CALL ACTUAL(KFILDO,MESHD,DMESH,TRASH,NPROJ,IER)
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,1258)MESHD,IER
         GO TO 902
      ENDIF
C
      CALL ACTUAL(KFILDO,MESHB,BMESH,TRASH,NPROJ,IER)
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,1258)MESHB,IER
         GO TO 902
      ENDIF
C
      CALL ACTUAL(KFILDO,MESHE,EMESH,TRASH,NPROJ,IER)
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,1258)MESHE,IER
         GO TO 902
      ENDIF
C
C        CALCULATE UPPER RIGHT GRIDPOINT OF ANALYSIS GRID.  MESH 
C        LENGTH MUST BE IN M.
C
      IF(NPROJ.EQ.3)THEN
         CALL LMIJLL(KFILDO,FLOAT(NXL),FLOAT(NYL),XMESHL*1000.,ORIENT,
     1               XLAT,ALATL,ALONL,URLAT,URLON,IER)
      ELSEIF(NPROJ.EQ.5)THEN
         CALL PSIJLL(KFILDO,FLOAT(NXL),FLOAT(NYL),XMESHL*1000.,ORIENT,
     1               XLAT,ALATL,ALONL,URLAT,URLON)
      ELSE
         CALL MCIJLL(KFILDO,FLOAT(NXL),FLOAT(NYL),XMESHL*1000.,XLAT,
     1               ALATL,ALONL,URLAT,URLON)
      ENDIF
C
      ALATL=NINT(ALATL*10000.)/10000.
      ALONL=NINT(ALONL*10000.)/10000.
      URLAT=NINT(URLAT*10000.)/10000.
      URLON=NINT(URLON*10000.)/10000.
C        RESOLUTION OF LAT/LON IS ONLY TO TEN THOUSANDTHS OF
C        DEGREES.  THE TDLPACK ARCHIVES (E.G., AVN ARCHIVE) IS TO
C        THOUSANDTHS OF DEGREES, SO ACCOMMODATION WILL HAVE TO
C        BE MADE WHEN CHECKING.  TRUNCATING TO DEGREES*10000 MAKES
C        A DIFFERENCE OF ONLY ABOUT 11 METERS.
C
      WRITE(KFILDO,128)KSKIP,NSKIP,JSTOP,INCCYL,NEW,NALPH,
     1      PXMISS,L3264B,NAREA,NPROJ,ORIENT,XLAT,MESHB,MESHE,
     2      ALATL,ALONL,URLAT,URLON,NXL,NYL,MESHL,XMESHL,
     3      MESHD,DMESH,
     4      NCEPNO,MODNO,MINVEC,MINMOD,IPRTEL,ISTA,ISMPL
 128  FORMAT(/' KSKIP ',I10,'   SKIP PAST THIS DATE ON OUTPUT FILE'/  
     2        ' NSKIP ',I10,'   NUMBER OF ERRORS THAT WILL BE',
     X                      ' TOLERATED ON DAY 1 BEFORE STOPPING'/
     3        ' JSTOP ',I10,'   NUMBER OF ERRORS THAT WILL BE',
     X                      ' TOLERATED ON TOTAL RUN BEFORE STOPPING'/
     4        ' INCCYL',I10,'   INCREMENT IN HOURS BETWEEN DATE/TIMES'/
     5        ' NEW   ',I10,'   NEW ICAO CALL LETTERS, 1 = YES,',
     X                      ' 0 = NO'/
     6        ' NALPH ',I10,'   ALPHABETIZE CALL LETTERS ACCORDING',
     X                      ' TO DIRECTORY, 1 = YES, 0 = NO'/
     7        ' PXMISS',F10.4,'   SECONDARY MISSING VALUE TO INSERT',
     X                      ' FOR 9997'/
     8        ' L3264B',I10,'   INTEGER WORD SIZE OF MACHINE'/
     9        ' NAREA ',I10,'   AREA OF ANALYSIS'/
     A        ' NPROJ ',I10,'   MAP PROJECTION'/
     B        ' ORIENT',F10.4,'   MAP ORIENTATION'/
     C        ' XLAT  ',F10.4,'   LATITUDE OF MESH LENGTH'/
     D        ' MESHB ',I10,'   MESHB = MESH LENGTH OF BASIC GRID',
     X                      ' SPECIFIED BY NXL, NYL'/
     E        ' MESHE ',I10,'   MESHE = MESH LENGTH OF ELEVATION GRID',
     X                      ' COVERING THE NXL BY NYL AREA'/
     F        ' ALATL ',F10.4,'   ALATL = NORTH LATITUDE OF LOWER LEFT',
     X                      ' CORNER OF ANALYSIS GRID'/
     G        ' ALONL ',F10.4,'   ALONL = WEST LONGITUDE OF LOWER LEFT',
     X                      ' CORNER OF ANALYSIS GRID'/
     H        ' URLAT ',F10.4,'   URLAT = NORTH LATITUDE OF UPPER',
     X                      ' RIGHT CORNER OF ANALYSIS GRID'/
     I        ' URLON ',F10.4,'   URLON = WEST LONGITUDE OF UPPER',
     X                      ' RIGHT CORNER OF ANALYSIS GRID'/
     J        ' NXL   ',I10,'   NXL = SIZE OF ANALYSIS GRID IN X',
     X                      ' DIRECTION IN MESHB UNITS'/
     K        ' NYL   ',I10,'   NYL = SIZE OF ANALYSIS GRID IN Y',
     X                      ' DIRECTION IN MESHB UNITS'/
     L        ' MESHL ',I10,'   NOMINAL GRIDLENGTH OF QUALITY CONTROL',
     X                      ' GRID FOR CONTINUOUS VARIABLES'/
     M        ' XMESHL',F10.5,'   ACTUAL GRIDLENGTH CORRESPONDING TO',
     X                        ' MESHL'/
     N        ' MESHD ',I10,'   NOMINAL GRIDLENGTH OF QUALITY CONTROL',
     X                       ' GRID FOR DISCONTINUOUS VARIABLES'/
     O        ' DMESH ',F10.5,'   ACTUAL GRIDLENGTH CORRESPONDING TO',
     X                        ' MESHD'/
     P        ' NCEPNO',I10,'   NCEP MODEL NUMBER'/
     Q        ' MODNO ',I10,'   DD IN CCCFFFBDD FOR OUTPUT'/
     R        ' MINVEC',I10,'   MINIMUM NUMBER OF HOURS TO SAVE',
     X                       ' VECTOR DATA'/
     S        ' MINMOD',I10,'   MINIMUM NUMBER OF HOURS TO SAVE',
     X                       ' MODEL DATA'/
     T        ' IPRTEL',I10,'   IPRTEL = 1 TO OUTPUT TERRAIN AND',
     X                        ' LAND/SEA GRIDS PER IP22 AND KFILOG;',
     X                        ' 0 OTHERWISE'/
     U        ' ISTA  ',I10,'   ISTA = 1 TO READ DIRECTORY;',
     X                        '  0 OTHERWISE'/
     V        ' ISMPL ',I10,'   ISMPL = MAXIMUM NUMBER OF POINTS TO',
     X                        ' SAMPLE FROM THE FIRST GUESS;',
     X                        ' 0 OTHERWISE')
C
C        SET IOPTB( ).
C
      IF(NXGMIN.GT.0.AND.NXGMIN.LE.NXL.AND.
     1   NYGMIN.GT.0.AND.NYGMIN.LE.NYL.AND.
     2   NXGMAX.GT.0.AND.NXGMAX.LE.NXL.AND.
     3   NYGMAX.GT.0.AND.NYGMAX.LE.NYL)THEN
C           IF THESE ARE INCONSISTENT OR IMPLY NO SUBSETTING
C           (E.G., MXGMIN = 0), THEN IOPTB( ) DEFAULTS TO ZERO,
C           AS SET IN DATA STATEMENT.  IOPTB(6) DEFINES
C           LINEAR INTERPOLATION BETWEEN GRIDPOINTS FOR
C           DETERMINING THE ZEBRA STRIPES.  THE DEFAULT
C           VALUES OF IOPTB( ) ARE SET TO ZERO IN U155.
         IOPTB(1)=1
         IOPTB(2)=NXGMIN
         IOPTB(3)=NXGMAX
         IOPTB(4)=NYGMIN
         IOPTB(5)=NYGMAX
         IOPTB(6)=1
         KFILKY=1
      ELSE
C
         IF(NXGMIN.NE.0)THEN
            WRITE(KFILDO,1285)NXGMIN,NXGMAX,NYGMIN,NYGMAX
 1285       FORMAT(/' ****SUBSETTING VALUES INCONSISTENT',
     1              ' WITH BASIC GRID.',
     2              '  NXGMIN, NXGMAX, NYGMIN, NYGMAX ARE',4I7/
     3              '     DISPOSABLE GRIDS WILL NOT BE WRITTEN,',
     4              ' AND STATISTICS WILL NOT BE CALCULATED',
     5              ' OVER A SUBSETTED AREA.'/
     6              '     ANY GRIDPRINTS WILL BE OVER THE WHOLE',
     7              ' GRID AND AT THE MESHL MESH LENGTH.')
            ISTOP=ISTOP+1
            KFILKY=0
C              KFILOG WILL BE SET TO ZERO, ELSE THERE WILL
C              LIKELY BE TROUBLE IN SUBROUTINE CUT.
         ELSE
            WRITE(KFILDO,1286)
 1286       FORMAT(/' NO SUBSETTING OF GRIDS WILL BE DONE.') 
            IF(IP(20).NE.0)WRITE(IP(20),1286)
         ENDIF
C
      ENDIF
C
C         CHECK VALUES OF ISTA AND ISMPL.
C     
      IF(ISTA.EQ.0..AND.ISMPL.EQ.0)THEN
         WRITE(KFILDO,1290)
 1290    FORMAT(/,' ****BOTH ISTA AND ISMPL ARE ZERO.  THERE WILL ',
     1            'BE NO DATA TO ANALYZE.  FATAL ERROR.')
         ISTOP=ISTOP+1
         IER=777
         GO TO 160
      ENDIF
C 
C        PRINT IOPTB( ) VALUES IF OTHER THAN ZERO.
C
      IF(IOPTB(1).EQ.0.AND.KFILKY.NE.0)THEN
C           WHEN THE AREA DEFINITION IS INCONSISTENT, THE PRINT BELOW
C           HAS ALREADY BEEN DONE.
         WRITE(KFILDO,1295)
 1295    FORMAT(/' ANY GRIDPRINTS WILL BE OVER THE WHOLE GRID',
     1           ' AND AT THE MESHL MESH LENGTH.')
      ELSEIF(IOPTB(1).NE.0)THEN
C     
C           COMPUTE THE LL AND UR LAT/LON OF THE GRID TO BE
C           PRINTED OR TDLPACKED.  MESH LENGTH MUST BE IN M.
C
         IF(NPROJ.EQ.3)THEN
            CALL LMIJLL(KFILDO,FLOAT(NXGMIN),FLOAT(NYGMIN),XMESHL*1000.,
     1                  ORIENT,XLAT,ALATL,ALONL,XLLAT,XLLON,IER)
            CALL LMIJLL(KFILDO,FLOAT(NXGMAX),FLOAT(NYGMAX),XMESHL*1000.,
     1                  ORIENT,XLAT,ALATL,ALONL,URLAT,URLON,IER)
         ELSEIF(NPROJ.EQ.5)THEN
            CALL PSIJLL(KFILDO,FLOAT(NXGMIN),FLOAT(NYGMIN),XMESHL*1000.,
     1                  ORIENT,XLAT,ALATL,ALONL,XLLAT,XLLON)
            CALL PSIJLL(KFILDO,FLOAT(NXGMAX),FLOAT(NYGMAX),XMESHL*1000.,
     1                  ORIENT,XLAT,ALATL,ALONL,URLAT,URLON)
         ELSE
            CALL MCIJLL(KFILDO,FLOAT(NXGMIN),FLOAT(NYGMIN),XMESHL*1000.,
     1                  XLAT,ALATL,ALONL,XLLAT,XLLON)
            CALL MCIJLL(KFILDO,FLOAT(NXGMAX),FLOAT(NYGMAX),XMESHL*1000.,
     1                  XLAT,ALATL,ALONL,URLAT,URLON)
         ENDIF
C
         XLLAT=NINT(XLLAT*10000.)/10000.
         XLLON=NINT(XLLON*10000.)/10000.
         URLAT=NINT(URLAT*10000.)/10000.
         URLON=NINT(URLON*10000.)/10000.
C           RESOLUTION OF LAT/LON IS ONLY TO TEN THOUSANDTHS OF
C           DEGREES.  THE TDLPACK ARCHIVES (E.G., AVN ARCHIVE) IS TO
C           THOUSANDTHS OF DEGREES, SO ACCOMMODATION WILL HAVE TO
C           BE MADE WHEN CHECKING.  TRUNCATING TO DEGREES*10000 MAKES
C           A DIFFERENCE OF ONLY ABOUT 11 METERS.
C
         WRITE(KFILDO,1296)(IOPTB(J),J=2,5)
 1296    FORMAT(/' ANY GRIDPRINTS AND/OR TDLPACKS FOR QUALITY',
     1           ' CONTROL WILL BE OVER THE AREA DEFINED'/
     2           ' BY THE GRIDPOINTS AND LAT/LON BELOW IN TERMS OF',
     3           ' MESHB.'/
     4           ' ALL GRIDPRINTS AND TDLPACKS FOR     CONTINUOUS',
     5           ' VARIABLES WILL BE AT THE MESHL MESH LENGTH.'/
     6           ' ALL GRIDPRINTS AND TDLPACKS FOR NON-CONTINUOUS',
     7           ' VARIABLES WILL BE AT THE MESHD MESH LENGTH.'/
     8           '    NXGMIN',I10,'   MINIMUM NX VALUE'/
     9           '    NXGMAX',I10,'   MAXIMUM NX VALUE'/
     A           '    NYGMIN',I10,'   MINIMUM NY VALUE'/
     B           '    NYGMAX',I10,'   MAXIMUM NY VALUE')
         WRITE(KFILDO,1297)XLLAT,XLLON,URLAT,URLON
 1297    FORMAT('    XLLAT ',F10.4,'   XLLAT = NORTH LATITUDE OF LOWER',
     X                             ' LEFT CORNER OF DISPOSABLE GRID'/
     1          '    ALONL ',F10.4,'   ALONL = WEST LONGITUDE OF LOWER',
     X                             ' LEFT CORNER OF DISPOSABLE GRID'/
     2          '    URLAT ',F10.4,'   URLAT = NORTH LATITUDE OF UPPER',
     X                              ' RIGHT CORNER OF DISPOSABLE GRID'/
     3          '    URLON ',F10.4,'   URLON = WEST LONGITUDE OF UPPER',
     X                              ' RIGHT CORNER OF DISPOSABLE GRID')
      ENDIF
C
C        GUARANTEE THAT NXL-1 AND NYL-1 ARE EVENLY DIVISIBLE BY 4.
C
      IF(((NXL-1)/4)*4.NE.NXL-1.OR.((NYL-1)/4)*4.NE.NYL-1)THEN
C
         WRITE(KFILDO,1298)NXL,NYL
 1298    FORMAT(/' ****NXL OR NYL =',2I4,' MINUS 1 NOT EVENLY',
     1           ' DIVISIBLE BY 4',/,
     2           '     BECAUSE OF THINNING OF GRIDS, NXL-1 AND NYL-1',
     3           ' MUST BE EVENLY DIVISIBLE BY 2**N, WHERE N IS THE',
     4           ' RATIO OF MESHB',/,
     5           '     TO THE LOWEST MESH USED IN THE PROCESS.',
     6           '  THIS IS NOT AN ABSOLUTE, BUT CHECK IS IN FOR',
     7           ' SAFETY.',/,
     8           '     THIS IS COUNTED AS A FATAL ERROR.')
         ISTOP=ISTOP+1
         IER=777
         GO TO 160
      ENDIF
C
C        READ AND PROCESS UNIT NUMBER AND FILE NAME FOR READING
C        DATE LIST.  FILE WILL BE OPENED AS 'OLD', UNLESS THE FILE
C        IS THE DEFAULT INPUT FILE.
C
 1299 CALL RDSNAM(KFILDI,KFILDO,KFILDT,DATNAM,IDUM,IDUM,1,N,'OLD',
     1            'FORMATTED',IP,IER)
      IF(IER.NE.0)ISTOP=ISTOP+1
COPS      WRITE(KFILDO,130)KFILDT,DATNAM
      WRITE(KFILDO,130)KFILDT
 130  FORMAT(/' NCEP DATE FILES UNIT NUMBER..',/,' ',I4)
COPS 130  FORMAT(/' DATE INPUT DATA SET, UNIT AND NAME.'/
COPS     1      (' ',I4,2X,A60))
C        READ AND PRINT UNDER CONTROL OF IP(2) AND IP(3) THE
C        DATES TO BE PROCESSED, MAX OF ND8. 
C
      CALL GET_NCEPDATE(KFILDT,IYR,IMO,IDA,IHR,NDATE,IER)
COPS      CALL RDI(KFILDO,IP(3),KFILDT,IDATE,ND8,ITEMP,7,'(7I10)',NDATES,
COPS     1         99999999,IER)
COPSC        ITEMP( ) IS AN ARRAY AT LEAST 7 IN SIZE.
COPS      IF(KFILDT.NE.KFILDI)CLOSE(UNIT=KFILDT)
COPSC        KFILDT IS CLOSED WHEN IT IS NOT THE SAME AS THE DEFAULT
COPSC        INPUT FILE.
COPS      CALL DATPRO(KFILDO,IDATE,NWORK,ND8,INCCYL,NDATES,IP(2),IP(3),IER)
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,134)
COPS 134     FORMAT(/' ****ERROR IN DATE LIST.  FATAL ERROR IN INT155',
COPS     1           ' AT 134.')
COPS         ISTOP=ISTOP+1
COPS         IER=777
COPS         GO TO 160
COPS       ENDIF
 134     FORMAT(/' ****ERROR: CAN NOT READ NCEP DATE FILE - ',
     1           'CATASTROPHIC ERROR IN 155. STOP AT 134.')
         CALL W3TAGE('INT155')
         STOP 134
      ENDIF
      NDATES = 1
      IDATE(1) = NDATE
      WRITE(KFILDO,1351)NDATES,(IDATE(J),J=1,NDATES)
 1351 FORMAT(/,' ',I4,' INPUT DATE AS READ',/,(1X,10I12))
C
C        MAKE SURE DATA WON'T BE WRITTEN WITH A DATE EQUAL TO OR LESS
C        THAN THE DATE SKIPPED.  THIS IS OK WHEN KSKIP = 0.
      IF(KSKIP.GE.IDATE(1))THEN
         WRITE(KFILDO,135)KSKIP,IDATE(1)
 135     FORMAT(/' ****DATE TO BE SKIPPED ',I11,' IS NOT BEFORE THE',
     1           ' FIRST DATE TO BE WRITTEN ',I11,
     2           '.  FATAL ERROR IN INT155 AT 135.')
         ISTOP=ISTOP+1
         IER=777
         GO TO 160
      ENDIF
C
C        MODIFY MINMOD TO MAKE SURE MODEL DATA ARE SAVED,
C        EVEN THOUGH NO "BACKUP" IS REQUIRED.  IF THE DATES
C        ARE AT THE STANDARD 00, 06, 12, AND 18 TIMES, THEN
C        WITH NO BACKUP, MODEL DATA NEED NOT BE SAVED.  IF
C        DATA WERE AVAILABLE ONLY EVERY 12 HOURS, THIS MIGHT
C        HAVE TO BE MODIFIED.
C
      IF(MINMOD.LT.4.AND.
     1       ((MOD(MOD(IDATE(1),100),06).NE.0).OR.
     2       (MOD(INCCYL,06).NE.0)))THEN
         MINMOD=4
C           AN ADDITIONAL 2 HOURS IS ADDED LATER IN OTHER ROUTINES
C           FOR POSSIBLE INTERPOLATION.
         WRITE(KFILDO,137)
 137     FORMAT(/' MINMOD BEING MODIFIED TO MAKE SURE MODEL',
     1           ' DATA ARE AVAILABLE EVEN THOUGH NO "BACKUP" IS',
     2           ' REQUIRED.')
      ENDIF
C
C        READ AND PROCESS UNIT NUMBERS AND FILE NAMES FOR ALL TDLPACK 
C        INPUT.  FILES WILL BE OPENED AS 'OLD'.
C
      CALL RDSNAM(KFILDI,KFILDO,KFILIN,NAMIN,MODNUM,JFOPEN,ND6,NUMIN,
     1            'OLD','UNFORMATTED',IP,IER)
C        ONLY THE FIRST FILE IS OPENED.
      IF(IER.NE.0)ISTOP=ISTOP+1
C
      IF(NUMIN.EQ.0)THEN
         WRITE(KFILDO,141)NUMIN
 141     FORMAT(/' ',I2,' MODEL INPUT DATA SETS.')
      ELSE
         WRITE(KFILDO,142)NUMIN,(KFILIN(J),MODNUM(J),NAMIN(J),J=1,NUMIN)
 142     FORMAT(/' ',I2,' MODEL INPUT DATA SETS, UNITS, MODEL NUMBERS,',
     1           ' AND NAMES.'/(' ',I4,I3,2X,A60))
      ENDIF
C
C        READ AND PROCESS THE UNIT NUMBER AND FILE NAME FOR THE 
C        MOS-2000 EXTERNAL RANDOM ACCESS FILE.  FILE WILL NOT BE OPENED.
C
      CALL RDSNAM(KFILDI,KFILDO,KFILRA,RACESS,ITEMP,ITEMP,6,NUMRA,'NOT',
     1            'NOTOPENED',IP,IER)
C        ITEMP( ) IS AN ARRAY AT LEAST 5 IN SIZE.
      IF(IER.NE.0)ISTOP=ISTOP+1
C
      IF(NUMRA.NE.0)THEN
         WRITE(KFILDO,143)NUMRA,(KFILRA(J),RACESS(J),J=1,NUMRA)
 143     FORMAT(/' ',I2,' MOS-2000 EXTERNAL RANDOM ACCESS DATA SETS,',
     1           ' UNITS, AND NAMES.'/(' ',I4,2X,A60))
      ELSE
         WRITE(KFILDO,1430)NUMRA
 1430    FORMAT(/' ',I2,' MOS-2000 EXTERNAL RANDOM ACCESS DATA SET.')
C           THE ABOVE PRINT IS FOR THE EMPTY SET.
      ENDIF
C
C        READ AND PROCESS UNIT NUMBER AND FILE NAME FOR GRIDDED 
C        OUTPUT.  FILE WILL BE OPENED AS 'NEW'.  
C
      CALL RDSNAM(KFILDI,KFILDO,KFILIO,GOTNAM,IDUM,IDUM,1,
     1            IOUT,'NEW','UNFORMATTED',IP,IER)
C         IOUT IS THE NUMBER OF VALUES READ AND IS NOT USED.
      IF(IER.NE.0)ISTOP=ISTOP+1
C
      IF(KFILIO.EQ.0)THEN
         WRITE(KFILDO,1432)
 1432    FORMAT(/' NO GRIDDED OUTPUT DATA SET PROVIDED;',
     1           ' PACKED GRIDDED OUTPUT WILL NOT BE WRITTEN.')
         GOTNAM=' '
      ELSE
         WRITE(KFILDO,1433)KFILIO,GOTNAM
 1433    FORMAT(/' OUTPUT GRIDDED DATA SET, UNIT AND NAME.'/
     1         (' ',I4,2X,A60))
      ENDIF   
C
C        READ AND PROCESS UNIT NUMBER AND FILE NAME FOR ASCII DATA
C        OUTPUT FOR GMOS_PLOT PLOTTING.  FILE WILL BE OPENED 
C        AS 'NEW'.  
C
      CALL RDSNAM(KFILDI,KFILDO,KFILVO,VOTNAM,IDUM,IDUM,1,
     1            IOUT,'NEW','FORMATTED',IP,IER)
C         IOUT IS THE NUMBER OF VALUES READ AND IS NOT USED.
      IF(IER.NE.0)ISTOP=ISTOP+1
C
      IF(KFILVO.EQ.0)THEN
         WRITE(KFILDO,1435)
 1435    FORMAT(/' NO ASCII OUTPUT DATA SET PROVIDED;',
     1           ' DATA FOR GMOS_PLOT FOR PLOTTING',
     2           ' WILL NOT BE WRITTEN.')
         VOTNAM=' '
      ELSE
         WRITE(KFILDO,144)KFILVO,VOTNAM
 144     FORMAT(/' ASCII OUTPUT DATA SET FOR PLOTTING, UNIT AND NAME.'/
     1         (' ',I4,2X,A60))
      ENDIF
C
C        READ AND PROCESS UNIT NUMBER AND FILE NAME FOR DISPOSABLE
C        GRIDDED OUTPUT.  FILE WILL BE OPENED AS 'NEW'.
C
      CALL RDSNAM(KFILDI,KFILDO,KFILOG,OUTDIS,IDUM,IDUM,1,
     1            IOUT,'NEW','UNFORMATTED',IP,IER)
C         IOUT IS THE NUMBER OF VALUES READ AND IS NOT USED.
      IF(IER.NE.0)ISTOP=ISTOP+1
      IF(KFILKY.EQ.0)KFILOG=0
C
      IF(KFILOG.EQ.0)THEN
         WRITE(KFILDO,1445)
 1445    FORMAT(/' NO DISPOSABLE OUTPUT DATA SET PROVIDED,',
     1           ' SUBSETTING VALUES ARE INCONSISTENT, OR',
     2           ' NXGMIN = 0 SIGNIFYING NONE TO BE WRITTEN.')
         OUTDIS=' '
      ELSE
         WRITE(KFILDO,145)KFILOG,OUTDIS
 145     FORMAT(/' DISPOSABLE GRIDDED OUTPUT DATA SET, UNIT AND NAME.'/
     1         (' ',I4,2X,A60))
      ENDIF
C
C        READ AND PROCESS UNIT NUMBER AND FILE NAME FOR DISPOSABLE
C        VECTOR OUTPUT.  FILE WILL BE OPENED AS "NEW"
C        WHEN KSKIP = 0.  OTHERWISE, FILE WILL BE OPENED AS "'OLD".
C        IF IT DOES NOT EXIST, IT WILL BE OPENED AS "NEW.,".
C
      IF(KSKIP.EQ.0)THEN
         CALL RDSNAM(KFILDI,KFILDO,KFILOV,OUTVEC,IDUM,IDUM,1,
     1               IOUT,'NEW','UNFORMATTED',IP,IER)
C            IOUT IS THE NUMBER OF VALUES READ AND IS NOT USED.
      ELSE
         CALL RDSNAM(KFILDI,KFILDO,KFILOV,OUTVEC,IDUM,IDUM,1,
     1               IOUT,'OLD','UNFORMATTED',IP,IER)
C            IOUT IS THE NUMBER OF VALUES READ AND IS NOT USED.
      ENDIF
C
      IF(IER.NE.0)ISTOP=ISTOP+1
C
      IF(KFILOV.EQ.0)THEN
         WRITE(KFILDO,146)
 146     FORMAT(/' NO VECTOR OUTPUT DATA SET PROVIDED;',
     1           ' VECTOR OUTPUT WILL NOT BE WRITTEN.')
         OUTVEC=' '
      ELSE
         WRITE(KFILDO,1465)KFILOV,OUTVEC
 1465    FORMAT(/' VECTOR OUTPUT DATA SET, UNIT AND NAME.'/
     1         (' ',I4,2X,A60))
      ENDIF
C
C        READ AND PROCESS UNIT NUMBER AND FILE NAME FOR QUALITY
C        CONTROLLED OBS OUTPUT.  FILE WILL BE OPENED AS "NEW"
C        WHEN KSKIP = 0.  OTHERWISE, FILE WILL BE OPENED AS "OLD".
C        IF IT DOES NOT EXIST, IT WILL BE OPENED AS "NEW".
C
      IF(KSKIP.EQ.0)THEN
         CALL RDSNAM(KFILDI,KFILDO,KFILQC,OUTQCV,IDUM,IDUM,1,
     1               IOUT,'NEW','UNFORMATTED',IP,IER)
C            IOUT IS THE NUMBER OF VALUES READ AND IS NOT USED.
      ELSE
         CALL RDSNAM(KFILDI,KFILDO,KFILQC,OUTQCV,IDUM,IDUM,1,
     1               IOUT,'OLD','UNFORMATTED',IP,IER)
C            IOUT IS THE NUMBER OF VALUES READ AND IS NOT USED.
      ENDIF
C
      IF(IER.NE.0)ISTOP=ISTOP+1
C
      IF(KFILQC.EQ.0)THEN
         WRITE(KFILDO,147)
 147     FORMAT(/' NO QUALITY CONTROLLED OBS OUTPUT DATA SET',
     1           ' PROVIDED; VECTOR OUTPUT WILL NOT BE WRITTEN.')
         OUTQCV=' '
      ELSE
         WRITE(KFILDO,1475)KFILQC,OUTQCV
 1475    FORMAT(/' QUALITY CONTROLLED OBS OUTPUT DATA SET,',
     1           ' UNIT AND NAME.'/(' ',I4,2X,A60))
      ENDIF
C
C        READ AND PROCESS UNIT NUMBERS AND FILE NAMES FOR STATION LIST
C        (CALL LETTERS) AND STATION DIRECTORY WHICH HOLDS CALL LETTERS,
C        LATITUDE, LONGITUDE, WBAN NUMBER, ELEVATION, AND NAME FOR EACH 
C        POSSIBLE STATION.  THIS CAN BE A MASTER DIRECTORY, OR BE A
C        DIRECTORY SUPPLIED BY A USER.
C
      CALL RDSNAM(KFILDI,KFILDO,KFILD,DIRNAM,ITEMP,ITEMP,2,N,'OLD',
     1            'FORMATTED',IP,IER)
C        ITEMP( ) IS AN ARRAY AT LEAST 2 IN SIZE.
C
      WRITE(KFILDO,150)(KFILD(J),DIRNAM(J),J=1,2)
 150  FORMAT(/' STATION LIST AND DIRECTORY DATA SETS, UNITS AND NAMES.'/
     1       (' ',I4,2X,A60))
C
C        READ STATION LIST AND OTHER STATION INFORMATION.  THE STATION
C        LIST CAN COME FROM THE DEFAULT INPUT FILE KFILDI, OR BE ON A 
C        SEPARATE FILE AS DETERMINED BY KFILD(1).  THE STATION LIST 
C        CAN BE USED AS READ, OR ORDERED ACCORDING TO THE STATION
C        DIRECTORY, WHICH IS ALPHABETICAL BY ICAO CALL LETTERS.
C
      NSTA=0
CCCD     CALL TIMPR(KFILDO,KFILDO,'INT155 BEFORE RDSTN  ')
C
      IF(ISTA.GT.0)THEN
C           WHEN ISTA NE 0, THE DIRECTORY IS READ AND STATION DATA 
C           WILL BE USED IN THE ANALYSIS.  OTHERWISE, ONLY POINTS
C           SAMPLED FROM THE FIRST GUESS WILL BE USED.
C
         IF(NALPH.EQ.0)THEN
            CALL RDSTQN(KFILDO,IP(4),IP(5),KFILD,NEW,CCALL,
     1                 NAME,IQUAL,ELEV,IWBAN,STALAT,STALON,ISDATA,IPACK,
     2                 ND1,NSTA,IER)
         ELSE
            CALL RDSTQA(KFILDO,IP(4),IP(5),KFILD,NEW,CCALL,CCALLD,
     1                 NAME,IQUAL,ELEV,IWBAN,STALAT,STALON,ISDATA,IPACK,
     2                 ND1,NSTA,IER)
C           CCALLD( ) IS TREATED HERE AS IF IT HAD THE SAME DIMENSIONS
C           AS CCALL( , ).  THIS IS OK, BECAUSE ND5 IS GE ND1.
C           ISDATA( ) AND IPACK( ) ARE WORK ARRAYS IN RDSTAL AND RDSTAD.
         ENDIF
C
CCCD     CALL TIMPR(KFILDO,KFILDO,'INT155 AFTER  RDSTN  ')
C
         IF(IER.NE.0)ISTOP=ISTOP+1
C           THE DIRECTORY FILE IS READ ONLY ONCE.
C
C           PARSE 6-DIGIT VALUES IN IQUAL( ,1) INTO FIVE SINGLE DIGIT
C           VALUES AND INTO LNDSEA( ).  THE RIGHTMOST DIGIT IN
C           IQUAL( ,1) GOES INTO LNDSEA( ), THE NEXT ONE TO THE LEFT
C           GOES INTO IQUAL(N,1) AND THE NEXT ONE TO THE LEFT GOES INTO
C           IQUAL(N,2), ETC.  ONLY 5 FLAGS CAN BE ACCOMMODATED.
C
         DO 151 K=1,NSTA
         IQ=IQUAL(K,1)
         LNDSEA(K)=IQ-(IQ/10)*10
C
         DO 1505 L=2,6
         IQUAL(K,L-1)=(IQ-(IQ/10**L)*10**L)/10**(L-1)
 1505    CONTINUE
C
CCCD        WRITE(KFILDO,1506)IQ,LNDSEA(K),(IQUAL(K,L),L=1,5)
CCCD1506    FORMAT(/' IN INT155--IQ,LANDSEA(K),(IQUAL(K,L),L=1,5)',7I7)
C
 151     CONTINUE
C
      ENDIF
C
      IF(KFILD(1).NE.KFILDI)CLOSE(UNIT=KFILD(1))
      CLOSE(UNIT=KFILD(2))
C        THE FILES ARE CLOSED WHEN THEY ARE NOT THE SAME AS
C        THE DEFAULT INPUT FILE.  THE DIRECTORY IS NEVER THE DEFAULT.
C      
C        READ AND PROCESS UNIT NUMBER AND FILE NAME FOR READING STATION
C        PAIRS FILE.
C
      CALL RDSNAM(KFILDI,KFILDO,KFILLP,STAPRS,IDUM,IDUM,1,N,'OLD',
     1            'UNFORMATTED',IP,IER)
      IF(IER.NE.0)ISTOP=ISTOP+1
C
      IF(KFILLP.LE.0)THEN
         WRITE(KFILDO,1515)
 1515    FORMAT(/,' UNIT NUMBER LE ZERO READ FOR READING STATION PAIRS',
     1            ' FILE.  NO LAPSE RATES WILL BE CALCULATED.')
      ELSE
         WRITE(KFILDO,1516)KFILLP,STAPRS
 1516    FORMAT(/' STATION PAIRS DATA SET, UNIT AND NAME.'/
     1          (' ',I4,2X,A60))
      ENDIF 
C      
C        READ AND PROCESS UNIT NUMBER AND FILE NAME FOR READING STATION
C        NEIGHBORS.
C
      CALL RDSNAM(KFILDI,KFILDO,KFILNI,STANEI,IDUM,IDUM,1,N,'OLD',
     1            'UNFORMATTED',IP,IER)
      IF(IER.NE.0)ISTOP=ISTOP+1
C
      IF(KFILNI.LE.0)THEN
         WRITE(KFILDO,1517)
 1517    FORMAT(/,' NO STATION NEIGHBORS FILE PROVIDED.')
      ELSE
         WRITE(KFILDO,1518)KFILNI,STANEI
 1518    FORMAT(/,' STATION NEIGHBORS DATA SET, UNIT AND NAME.'/
     1           (' ',I4,2X,A60))
      ENDIF  
C
C        READ AND PROCESS UNIT NUMBER AND FILE NAME FOR READING VARIABLE
C        LIST.
C
      CALL RDSNAM(KFILDI,KFILDO,KFILP,PRENAM,IDUM,IDUM,1,N,'OLD',
     1            'FORMATTED',IP,IER)
      IF(IER.NE.0)ISTOP=ISTOP+1
      WRITE(KFILDO,152)KFILP,PRENAM
 152  FORMAT(/' VARIABLE LIST DATA SET, UNIT AND NAME.'/
     1       (' ',I4,2X,A60))
C
C        READ AND PROCESS UNIT NUMBER AND FILE NAME FOR READING INDIVIDUAL
C        ANALYSIS CONTOL FILES.
C        NOTE: FOR OPERATIONAL VERSION, CHANGED ARGUEMENT TO ALLOW
C              MULTIPLE 405 CN FILES
C
COPS      CALL RDSNAM(KFILDI,KFILDO,KFILAN,ANLNAM,IDUM,IDUM,1,N,'OLD',
      CALL RDSNAM(KFILDI,KFILDO,KFILAN,ANLNAM,IDUM,IDUM,ND4,N,'OLD',
     1            'FORMATTED',IP,IER)
      IF(IER.NE.0)ISTOP=ISTOP+1
C
      IF(KFILAN(1).LE.0)THEN
C           DEVELOPMENTAL VERSION ON HP USES ONLY KFILAN(1).  IBM
C           NEEDS MORE.
         WRITE(KFILDO,153)
 153     FORMAT(/' UNIT NUMBER LE ZERO READ FOR READING INDIVIDUAL',
     1          ' .CN ANALYSIS CONTROL FILES.  THIS IS A FATAL ERROR.',
     2          '  FATAL ERROR IN INT155 AT 153.')
         ISTOP=ISTOP+1
         IER=777
         GO TO 160
      ELSE
COPS         WRITE(KFILDO,154)KFILAN(1),ANLNAM(1)
COPS 154     FORMAT(/' ANALYSIS CONTROL FILE DATA SET, UNIT AND NAME.'/
COPS     1          (' ',I4,2X,A60))
         WRITE(KFILDO,154)(KFILAN(J),ANLNAM(J),J=1,N)
 154     FORMAT(/' ANALYSIS CONTROL FILE DATA SETS, UNIT AND NAME.',
     1          /,(' ',I4,2X,A60))
      ENDIF  
C
C        READ AND PROCESS UNIT NUMBER FOR THE VARIABLE CONSTANTS 
C        DIRECTORY.
C
      CALL RDSNAM(KFILDI,KFILDO,KFILCP,CONNAM,IDUM,IDUM,1,N,'OLD',
     1            'FORMATTED',IP,IER)
      IF(IER.NE.0)ISTOP=ISTOP+1
      IF(KFILCP.NE.0)WRITE(KFILDO,157)KFILCP,CONNAM
 157  FORMAT(/' VARIABLE CONSTANT DIRECTORY, UNIT AND NAME.'/
     1       (' ',I4,2X,A60))
C         
C        READ GRIDPOINT VARIABLE LIST FOR WHICH OUTPUT IS WANTED.
C        NOTE THAT THERE IS NO VECTOR OUTPUT.
C
      CALL RDV155(KFILDO,IP(6),IP(7),IP(9),KFILP,KFILCP,ID,IDPARS,
     1            THRESH,JD,JP,ANLTAB,INLTAB,ISCALD,IWRITS,IWRITA,
     2            ICOMPT,SMULT,SADD,ORIGIN,CINT,PLAIN,UNITS,ND4,
     3            NPRED,ISTOP,IER)
C        NPRED IS THE NUMBER OF ANALYSES TO DO AND IS THE NUMBER
C        OF IDS READ INTO ID( , ).
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,159)
 159     FORMAT(/'    FATAL ERROR IN RDV155',
     1           ' IN INT155 AT 159.')
         ISTOP=ISTOP+1
         IER=777
         GO TO 160
      ENDIF
C
C        CHECK POSSIBLE INCONSISTENCY OF UNIT NUMBERS.  NOTE THAT 
C        RDSNAM HAS ALREADY CHECKED IP( ) NUMBERS WITH ANY UNIT
C        NUMBERS IT READS.  HOWEVER, THEY ARE RECHECKED IN U155CK.
C
      IPIN=25
      CALL U155CK(KFILDO,KFILIO,KFILDI,KFILIN,NUMIN,
     1            KFILD,KFILRA,NUMRA,IP,IPIN,KFIL10,
     2            KFILVO,KFILOG,KFILAN(1))
C
 160  RETURN
C 
C        ERROR STOP BELOW IS FOR ERRORS OF CONTROL INFORMATION INPUT.
C
 900  CALL IERX(KFILDO,KFILDO,IOS,'INT155',STATE)
      WRITE(KFILDO,901)
 901  FORMAT(/' ****FATAL SYSTEM ROUTINE ERROR IN INT155.')
 902  ISTOP=ISTOP+1
      IER=777
      GO TO 160
      END
