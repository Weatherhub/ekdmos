      SUBROUTINE FCST95(KFILDO,KFIL10,IP12,KFILFC,KFILX,CFILX,
     1                  KFILRA,RACESS,NUMRA,
     2                  NDATE,ICALL,CCALL,ISDATA,XDATA,ND1,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NSTORE,NFETCH,NESNUM,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  KGP,NGP,LGP,MTRMS,MTANDS,ICAT,
     8                  ID,IDPARS,TRESHL,JD,ITAU,IUSED,
     9                  CONST,AVG,ESS,CORR,COEF,X,VECTOR,ND2,ND3,
     A                  IDTAND,IDTPAR,LOCSTA,ND13,
     B                  FCST,INITF,
     C                  IPLAIN,PLAIN,ISCALD,MINPK,NREPLA,NCHECK,
     D                  IDTNSD,IDPRSD,PLNSD,IPLNSD,ISCLSD,
     E                  IP16,IP17,IP18,IP21,NTOTBY,NTOTRC,
     F                  L3264B,L3264W,ISTOP,IER)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM: FCST95
C   PRGMMR: GLAHN/WIEDENFELD        ORG: W/OSD211    DATE: 07-10-25
C
C ABSTRACT: TO MAKE FORECASTS FOR ALL PREDICTANDS FOR ONE DATE/TIME
C           IN U905.  PREDICTORS ARE IDENTIFIED IN ID( , , ).
C           AVAILABLE VARIABLES ARE INDICATED IN LSTORE( , ).
C           VARIABLES MAY BE OBTAINED FROM CORE( ) OR FROM DISK 
C           IN THE INTERNAL MOS-2000 STORAGE SYSTEM AS INDICATED 
C           IN LSTORE( , ).  DATA NEEDED MUST BE ON PACKED SEQUENTIAL
C           FILES OR CAN BE OBTAINED THROUGH OPTX, WHICH CAN ACCESS
C           FIVE MOS-2000 EXTERNAL RANDOM ACCESS FILES.  BINARIES CAN BE
C           AVAILABLE ON INPUT OR COMPUTED WITHIN U905.  IT IS ASSUMED 
C           THE PRIMARY MISSING VALUE = 9999.  IT IS ALSO ASSUMED 
C           THE SECONDARY MISSING VALUE = 9997.  FORECASTS ARE WRITTEN
C           TO A MOS-2000 PACKED SEQUENTIAL FILE AND/OR A MOS-2000
C           RANDOM ACCESS FILE.
C
C PROGRAM HISTORY LOG:
C   07-10-25  WIEDENFELD ADOPTED FROM FCST75 AND FCST95
C   08-10-10  WIEDENFELD MODIFIED TO ONLY PACK SD'S WHEN
C                        BINARY INDICATOR IS 0. NO NEED TO PACK
C                        SECOND ORDER ERROR VARIANCE.
C   08-11-10  WIEDENFELD ADDED CODE TO ENSURE THAT THE ERROR ESTIMATES
C                        ARE MISSING IF THE FORECASTS ARE MISSING.
C   10-11-18  VEENHUIS   ADDED NESNUM, ENSEMBLE SYSTEM NUMBER, TO INPUT
C                        CALL LIST. ADDED ARRAYS LD(4) AND LDPARS(15). 
C                        MODFIED CALLS TO PACKV TO INSERT NESNUM VALUE 
C                        INTO THE 'G' OF THE FOURTH WORD OUTPUT ID.
C                        MODIFED CALLS TO WRTDLR TO PASS LD(1) RATHER
C                        THAN IS1(9).
C
C USAGE:    CALL FCST95(KFILDO,KFIL10,IP12,KFILFC,KFILX,CFILX,KFILRA,RACESS,
C                       NUMRA,NDATE,ICALL,CCALL,ISDATA,XDATA,ND1,NSTA,
C                       ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,LSTORE,ND9,LITEMS,
C                       CORE,ND10,NBLOCK,NSTORE,NFETCH,NESNUM,IS0,IS1,IS2,IS4,ND7,
C                       KGP,NGP,LGP,MTRMS,MTANDS,ICAT,ID,IDPARS,TRESHL,JD,
C                       ITAU,IUSED,CONST,AVG,ESS,CORR,COEF,X,VECTOR,ND2,ND3,
C                       IDTAND,IDTPAR,LOCSTA,ND13,FCST,INITF,IPLAIN,PLAIN,ISCALD,MINPK,
C                       NREPLA,NCHECK,IDTNSD,IDPRSD,PLNSD,IPLNSD,ISCLSD,
C                       IP16,IP17,IP18,NTOTBY,NTOTRC,L3264B,
C                       ISTOP,IER)
C   INPUT ARGUMENT LIST:
C            KFIL10 - UNIT NUMBER OF MDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT)
C            KFILFC - UNIT NUMBER FOR WRITING THE SEQUENTIAL
C                     FORECAST OUTPUT.  WHEN KFILFC = 0, PACKED
C                     FORECASTS WILL NOT BE WRITTEN.  (INPUT)
C             KFILX - UNIT NUMBER FOR WRITING MOS-2000 EXTERNAL
C                     RANDOM ACCESS FILE.  (INPUT)
C             CFILX - FILE NAME FOR MOS-2000 EXTERNAL RANDOM ACCESS
C                     FILE FOR WRITING FORECASTS.  (CHARACTER*60)
C                     (INPUT)
C         KFILRA(J) - UNIT NUMBERS FOR READING MOS-2000 EXTERNAL
C                     RANDOM ACCESS FILES (J=1,NUMRA).  (INPUT)
C         RACESS(J) - FILE NAMES FOR MOS-2000 EXTERNAL RANDOM ACCESS
C                     FILES HOLDING CONSTANT DATA READ ON UNIT NOS.
C                     KFILRA(J) IN OPTX (J=1,NUMRA).  (CHARACTER*60)
C                     (INPUT)
C             NUMRA - NUMBER OF VALUES IN KFILRA( ) AND RACESS( ).
C                     (INPUT)
C             NDATE - THE DATE/TIME FOR WHICH FORECASTS ARE TO BE
C                     MADE ON THIS CALL TO FCST95.  (INPUT)
C        ICALL(L,K) - 8-CHARACTER STATION CALL LETTERS AS CHARACTERS
C                     IN AN INTEGER VARIABLE (L=1,L3264W).
C                     EQUIVALENCED TO CCALL( ).
C          CCALL(K) - 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                     LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                     OUTPUT FOR (K=1,NSTA).  ALL STATION
C                     DATA ARE KEYED TO THIS LIST.  THIS LIST IS USED
C                     FOR PRINTING ONLY.  EQUIVALENCED TO ICALL( , ).
C                     (CHARACTER*8) (INPUT)
C               ND1 - MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                     WITH.  USED AS DIMENSION OF SEVERAL VARIABLES.
C                     (INPUT)
C              NSTA - THE NUMBER OF STATIONS IN CCALL( ).  (INPUT)
C       ICALLD(L,K) - 8-CHARACTER STATION CALL LETTERS AS CHARACTERS
C                     IN AN INTEGER VARIABLE (L=1,L3264W) (K=1,ND5).
C                     NOTE THAT THIS REQUIRES TWO 32-BIT WORDS TO HOLD
C                     THE DESCRIPTION BUT ONLY ONE 64-BIT WORD.
C                     NEEDED IN CONST6 FOR ARGUMENT TO RDTDLM.
C                     EQUIVALENCED TO CCALLD( ).  (INPUT)
C         CCALLD(K) - 8-CHARACTER STATION CALL LETTERS (K=1,ND5).
C                     THIS LIST IS USED IN OPTX.
C                     EQUIVALENCED TO ICALLD( ).  (INPUT)
C               ND5 - DIMENSION OF IPACK( , ), IWORK( ), AND DATA( ).
C                     IN THE DRIVER, IT IS ASSURED THAT ND5 GE ND1.
C                     IT MUST BE LARGE ENOUGH TO ACCOMMODATE THE
C                     DATA READ FROM EXTERNAL SOURCES.  (INPUT)
C       LSTORE(L,J) - THE ARRAY HOLDING INFORMATION ABOUT THE DATA
C                     STORED IN THE MOS-2000 INTERNAL STORAGE SYSTEM
C                     (L=1,12) (J=1,LITEMS).  (INPUT-OUTPUT)
C                     L=1,4--THE 4 ID'S FOR THE DATA.
C                     L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                            THIS IS THE LOCATION IN CORE( ) WHERE
C                            THE DATA START.  WHEN ON DISK,
C                            THIS IS MINUS THE RECORD NUMBER WHERE
C                            THE DATA START.
C                     L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                     L=7  --2 FOR DATA PACKED IN MDL GRIB, 1 FOR NOT.
C                     L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                            YYYYMMDDHH.
C                     L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                     L=10 --NOT USED.
C                     L=11 --SET TO 7777.
C                     L=12 --NOT USED.
C               ND9 - THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C            LITEMS - THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                     THAT HAVE BEEN USED FOR THIS DATE/TIME.  (INPUT)
C           CORE(J) - THE ARRAY TO STORE OR RETIREVE THE DATA
C                     IDENTIFIED IN LSTORE( , ) (J=1,ND10).  WHEN
C                     CORE( ) IS FULL DATA ARE STORED ON DISK.
C                     (INPUT)
C              ND10 - DIMENSION OF CORE( ).  (INPUT)
C            NBLOCK - THE BLOCK SIZE IN WORDS OF THE INTERNAL
C                     MOS-2000 RANDOM DISK FILE.  (INPUT)
C               ND7 - DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                     NOT ALL LOCATIONS ARE USED.  (INPUT)
C               KGP - THE NUMBER OF GROUPS FOR THE SET OF EQUATIONS
C                     BEING EVALUATED IN THIS CALL.  (INPUT)
C            NGP(L) - THE NUMBER OF EQUATIONS IN EACH GROUP L
C                     (L=1,KGP).  (INPUT)
C            LGP(L) - FOR EACH EQUATION (L=1,KGP), THE LOCATION
C                     IN LOCSTA ( ) OF WHERE THE FIRST STATION
C                     IN THE GROUP IS.  (INPUT)
C          MTRMS(L) - THE NUMBER OF TERMS IN EACH GROUP L (L=1,KGP).
C                     (INPUT)
C            MTANDS - THE NUMBER OF PREDICTANDS FOR THIS EQUATION
C                     SET.  (INPUT)
C          ICAT(NN) - THE POSTPROCESSING INDICATOR FOR THE EQUATION
C                     GROUP BEING PROCESSED FOR PREDICTAND NN
C                     (NN=1,MTANDS).  FOR EXAMPLE, THE
C                     VALUE 1 WOULD REFER TO SUBROUTINE CAT1, WHICH
C                     IS USED FOR INFLATION.  POSTPROCESSING
C                     IS VERY LIMITED IN U905, AND MAY ONLY
C                     APPLY TO INFLATION.  (INPUT)
C         ID(J,L,M) - THE 4-WORD ID (J=1,7) FOR EACH PREDICTOR
C                     (M=1,MTRMS(L)) IN EACH EQUATION (L=1,KGP) OF THE
C                     GROUP BEING PROCESSED.  VALUES OF J=5-7 NOT USED;
C                     DIMENSION IS 7 RATHER THAN 4 TO ACCOMMODATE
C                     RDEQN, WHICH IS ALSO USED FOR U700.  (INPUT)
C     IDPARS(J,L,M) - THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                     VARIABLE ID'S CORRESPONDING TO ID( ,L,M) (J=1,15),
C                     (L=1,KGP) (M=1,MTRMS(L)).
C                     J=1--CCC (CLASS OF VARIABLE),
C                     J=2--FFF (SUBCLASS OF VARIABLE),
C                     J=3--B (BINARY INDICATOR),
C                        0 = NOT BINARY,
C                        1 = CUMULATIVE FROM ABOVE, VALUES GE LOWER
C                            THRESHOLD TRESHL = 1,
C                        2 = CUMULATIVE FROM BELOW, VALUES LT UPPER
C                            THRESHOLD TRESHU = 1.
C                        3 = DISCRETE BINARY.  VALUES GE LOWER
C                           THRESHOLD AND LT UPPER THRESHOLD = 1.
C                        5 = GRID BINARY.  VALUES GE LOWER THRESHOLD
C                        ONLY THE VALUE OF 0, 1, OR 5 SHOULD BE USED
C                        FOR PREDICTORS;
C                        0, 1, 2, OR 3 CAN BE USED FOR PREDICTANDS.
C                     J=4--DD (DATA SOURCE, MODEL NUMBER),
C                     J=5--V (VERTICAL APPLICATION),
C                     J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY
C                          1 LAYER),
C                     J=7--LTLTLTLT (TOP OF LAYER),
C                     J=8--T (TRANSFORMATION),
C                     J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK
C                          IN TIME),
C                     J=10--OT (TIME APPLICATION),
C                     J=11--OH (TIME PERIOD IN HOURS),
C                     J=12--TAU (PROJECTION IN HOURS),
C                     J=13--I (INTERPOLATION TYPE),
C                     J=14--S (SMOOTHING INDICATOR), AND
C                     J=15--G (GRID INDICATOR).
C            NESNUM - THE ENSEMBLE SYSTEM NUMBER USED TO SET THE 'G'
C                     VALUE PRIOR TO PACKING. (INPUT)
C       TRESHL(L,M) - THE LOWER BINARY THRESHOLD CORRESPONDING TO
C                     IDPARS( ,L,M) (M=1,MTRMS(L)), (L=1,KGP).
C                     FOR U905, THE UPPER THRESHOLD IS ALWAYS LARGE.
C                     THAT IS, THE PREDICTORS CARRY WITH THEM ONLY
C                     ONE THRESHOLD, THE LOWER ONE.
C         JD(J,L,M) - THE BASIC INTEGER VARIABLE ID'S (J=1,4)
C                     (M=1,MTRMS(L)), (L=1,KGP).  THIS IS THE SAME
C                     AS ID(J,L,M), EXCEPT THAT THE FOLLOWING PORTIONS
C                     ARE OMITTED:
C                     B = IDPARS(3, , ),
C                     G = IDPARS(15, , ), AND
C                     TRESHL( , ).
C                     THE "G" VARIABLE HAS NO MEANING IN U905,
C                     IT BEING ONLY FOR POSSIBLE USE IN U201.
C         ITAU(L,M) - THE NUMBER OF HOURS TO ADD TO NDATE TO GET
C                     THE VARIABLE ID( ,L,M) (M=1,MTRMS(L)), (L=1,KGP).
C                     THIS IS THE "LOOKAHEAD" FEATURE.  (INPUT)
C       CONST(L,NN) - THE EQUATION CONSTANTS FOR GROUP L (L=1,KGP),
C                     PREDICTAND NN (NN=1,MTANDS).  (INPUT)
C         AVG(L,NN) - THE PREDICTAND MEANS FOR GROUP L (L=1,KGP),
C                     PREDICTAND NN (NN=1,MTANDS).  (INPUT)
C         ESS(L,NN) - THE STANDARD ERROR ESTIMATE FOR GROUP L
C                     (L=1,KGP( )), PREDICTAND NN (NN=1,MTANDS(I))
C                     (INPUT)
C        CORR(L,NN) - THE MULTIPLE CORRELATIONS FOR GROUP L
C                     (L=1,KGP), PREDICTAND NN (NN=1,MTANDS).
C                     (INPUT)
C      COEF(L,M,NN) - THE COEFFICIENTS FOR GROUP L (L=1,KGP),
C                     TERM M (M=1,MTRMS(L), PREDICTAND NN
C                     (NN=1,MTANDS).  (INPUT)
C          X(M,M,L) - THE PREDICTOR INVERS X-PRODUCT PREDICTOR MATRIX
C                     (INCLUDING THE CONSTANT) TO USE IN CALCULATING
C                     EXPECTED ERROR (M=1,MTRMS(L,I)+1) (L=1,KGP( ))
C                     (INPUT) (REAL*8)
C     VECTOR(K,L,M) - VECTOR OF DATA (K=1,NSTA) (L=1,KGP( ))
C                     (M=1,MTRMS(L,I)) (INTERNAL
C               ND2 - MAXIMUM NUMBER OF TERMS IN ANY EQUATION.
C                     THIRD DIMENSION OF ID( , , ) AND
C                     SECOND DIMENSION OF COEF( , , ).  (INPUT)
C               ND3 - MAXIMUM NUMBER OF PREDICTANDS IN ANY EQUATION.
C                     USED AS DIMENSION OF SEVERAL VARIABLES.
C                     (INPUT)
C      IDTAND(J,NN) - THE PREDICTAND ID'S (J=1,4) FOR PREDICTAND NN
C                     (NN=1,MTANDS).  (INPUT)
C      IDTPAR(J,NN) - THE PARSED PREDICTAND ID'S (J=1,15), FOR
C                     PREDICTAND NN (NN=1,MTANDS).  (INPUT)
C         LOCSTA(K) - THE LOCATION IN FCST( ,NN) (NN=1,MTANDS)
C                     OF WHERE TO PUT THE FORECAST, WHERE K IS IN
C                     ORDER OF THE EQUATIONS AS READ.  (INPUT)
C              ND13 - MAXIMUM NUMBER OF DIFFERENT EQUATIONS.
C                     THIS WOULD = ND1 FOR SINGLE STATION EQUATIONS,
C                     BUT MIGHT BE ON THE ORDER OF 30 FOR REGIONAL
C                     EQUATIONS.  DIMENSION OF SEVERAL VARIABLES.
C                     (INPUT)
C             INITF - 0 WHEN EVERY STATION IN THE LIST IN CCALL( )
C                       HAS AN EQUATION AND INITIALIZATION OF
C                       FCST( , ) DOES NOT HAVE TO BE DONE.
C                   - 1 OTHERWISE.
C                     (INPUT)
C     PLAIN(L,J,NN) - 32 CHARACTERS (L=1,L3264W) (J=1,4) OF PLAIN
C                     LANGUAGE DESCRIPTION OF THE PREDICTAND
C                     NN (NN=1,MTANDS).  NOTE THAT THIS REQUIRES
C                     TWO 32-BIT WORDS TO HOLD THE DESCRIPTION
C                     BUT ONLY ONE 64-BIT WORD.  EQUIVALENCED
C                     TO PLAIN( , ).  (INPUT)
C         PLAIN(NN) - THE PLAIN LANGUAGE DESCRIPTION OF THE
C                     PREDICTAND NN (NN=1,MTANDS).  EQUIVALENCED TO
C                     IPLAIN( , , ).  (CHARACTER*32)  (INPUT)
C        ISCALD(NN) - THE DECIMAL SCALING CONSTANT TO USE WHEN
C                     PACKING THE FORECASTS FOR PREDICTAND NN
C                     (NN=1,MTANDS).  ISCALD( , ) COMES FROM THE
C                     VARIABLE CONSTANT FILE,.
C                     ZERO WHEN NOT FOUND IN THE FILE.  (INPUT)
C             MINPK - MINIMUM GROUP SIZE WHEN PACKING THE
C                     INTERPOLATED VALUES.  (INPUT)
C            NREPLA - RECORD REPLACEMENT FLAG FOR WRITING RANDOM
C                     ACCESS FILE.
C                     0 = NOT REPLACING RECORD.
C                     1 = REPLACING, ERROR IF RECORD NOT FOUND TO
C                         REPLACE.
C                     2 = REPLACING, WRITE NEW RECORD IF RECORD NOT
C                         FOUND TO REPLACE.
C                    (INPUT)
C            NCHECK - IDENTIFICATION CHECKING FLAG FOR WRITING
C                     RANDOM ACCESS FILE.
C                     0 = DON'T CHECK FOR DUPLICATES.
C                     1 = CHECK FOR DUPLICATES, ERROR IF FOUND.
C                     (INPUT)
C    IPLNSD(L,J,NN) - 32 CHARACTERS (L=1,L3264W) (J=1,4) OF PLAIN
C                     LANGUAGE DESCRIPTION OF THE PREDICTAND
C                     NN (NN=1,MTANDS(I)), AND EQUATION
C                     NOTE THAT THIS REQUIRES
C                     TWO 32-BIT WORDS TO HOLD THE DESCRIPTION
C                     BUT ONLY ONE 64-BIT WORD.  EQUIVALENCED
C                     TO PLNSD( ).
C         PLNSD(NN) - THE PLAIN LANGUAGE DESCRIPTION OF THE
C                     PREDICTAND SD'S (ERROR ESTIMATES) NN
C                     (NN=1,MTANDS(I)), AND EQUATION
C                     EQUIVALENCED TO IPLNSD( , , ).  (CHARACTER*32)
C      IDTNSD(J,NN) - THE PREDICTAND SD ID'S (J=1,4), FOR
C                     PREDICTAND NN (NN=1,MTANDS(I)), AND EQUATION
C      IDTPAR(J,NN) - THE PARSED PREDICTAND SD ID'S (J=1,15), FOR
C                     PREDICTAND NN (NN=1,MTANDS(I)), AND EQUATION
C        ISCLSD(NN) - THE DECIMAL SCALING CONSTANT TO USE WHEN
C                     PACKING THE COLLATED DATA FOR PREDICTAND SD
C                     NN (NN=1,MTANDS(I)). ISCALD( , ) COMES FROM THE
C                     VARIABLE CONSTANT FILE, MODIFIED TO BE 2
C                     FOR GRID BINARIES, AND 0 FOR BINARIES. 
C                     ZERO WHEN NOT FOUND IN THE FILE.
C              IP16 - INDICATES WHETHER (>0) OR NOT (=0) INPUT DATA
C                     WILL BE PRINTED ON UNIT IP16.  (INPUT)
C              IP17 - INDICATES WHETHER (>0) OR NOT (=0) FORECASTS
C                     WILL BE PRINTED ON UNIT IP17.  (INPUT)
C              IP18 - INDICATES WHETHER (>0) OR NOT (=0) FORECASTS
C                     WILL BE PRINTED ON UNIT IP18 TO THE ACCURACY
C                     PACKED.  (INPUT)
C            NTOTBY - THE NUMBER OF BYTES PROCESSED ON THE OUTPUT
C                     FILE KFILFC.  (INPUT/OUTPUT)
C            NTOTRC - THE NUMBER OF RECORDS PROCESSED ON THE
C                     OUTPUT FILE KFILFC.  (INPUT/OUTPUT)
C            L3264B - INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                     USED (EITHER 32 OR 64).  (INPUT)
C            L3264W - NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).
C                     (INPUT)
C          ISTOP(J) - FOR J=1, ISTOP( ) IS INCREMENTED BY 1 EACH TIME
C                     AN ERROR OCCURRS THAT MAY BE FATAL.
C                     FOR J=2, ISTOP( ) IS INCREMENTED BY 1 WHENEVER AN
C                     INPUT DATA RECORD IS NOT FOUND.  (INPUT-OUTPUT)
C               IER - STATUS RETURN.
C                       0 = GOOD RETURN.
C                      39 = NWORDS FROM GFETCH DO NOT EQUAL NSTA.
C   OUTPUT ARGUMENT LIST: 
C            KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C            KFIL10 - UNIT NUMBER OF MDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT)
C       LSTORE(L,J) - THE ARRAY HOLDING INFORMATION ABOUT THE DATA
C                     STORED IN THE MOS-2000 INTERNAL STORAGE SYSTEM
C                     (L=1,12) (J=1,LITEMS).  (INPUT-OUTPUT)
C                     L=1,4--THE 4 ID'S FOR THE DATA.
C                     L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                            THIS IS THE LOCATION IN CORE( ) WHERE
C                            THE DATA START.  WHEN ON DISK,
C                            THIS IS MINUS THE RECORD NUMBER WHERE
C                            THE DATA START.
C                     L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                     L=7  --2 FOR DATA PACKED IN MDL GRIB, 1 FOR NOT.
C                     L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                            YYYYMMDDHH.
C                     L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                     L=10 --NOT USED.
C                     L=11 --SET TO 7777.
C                     L=12 --NOT USED.
C            NSTORE - COUNTS THE NUMBER OF TIMES GSTORE HAS BEEN
C                     ENTERED.  (OUTPUT)
C            NFETCH - COUNTS THE NUMBER OF TIMES GFETCH HAS BEEN
C                     ENTERED.  (OUTPUT)
C        FCST(K,NN) - THE FORECASTS FOR STATION K (K=1,KSTA),
C                     PREDICTAND NN (NN=1,MTANDS).  (OUTPUT)
C            NTOTBY - THE NUMBER OF BYTES PROCESSED ON THE OUTPUT
C                     FILE KFILFC.  (INPUT/OUTPUT)
C            NTOTRC - THE NUMBER OF RECORDS PROCESSED ON THE
C                     OUTPUT FILE KFILFC.  (INPUT/OUTPUT)
C          ISTOP(J) - FOR J=1, ISTOP( ) IS INCREMENTED BY 1 EACH TIME
C                     AN ERROR OCCURRS THAT MAY BE FATAL.
C                     FOR J=2, ISTOP( ) IS INCREMENTED BY 1 WHENEVER AN
C                     INPUT DATA RECORD IS NOT FOUND.  (INPUT-OUTPUT)
C               IER - STATUS RETURN.
C                       0 = GOOD RETURN.
C                      39 = NWORDS FROM GFETCH DO NOT EQUAL NSTA.
C                     SEE OTHER ROUTINES FOR OTHER VALUES.
C
C        DATA SET USE
C        INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C             FORT.xx - INDICATE NAME & PURPOSE
C
C        OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C             FORT.xx - INDICATE NAME & PURPOSE
C
C        VARIABLES
C           ISDATA(K) - SCRATCH ARRAY (K=1,ND1).  (INTERNAL)
C            XDATA(K) - THE DATA FOR THE NSTA STATIONS OF THE VARIABLE
C                       BEING PROCESSED (K=1,NSTA).  (INTERNAL)
C            IPACK(J) - WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) - WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(J) - WORK ARRAY (J=1,ND5).  (INTERNAL)
C              IS0(J) - MOS-2000 GRIB SECTION 0 ID'S (J=1,3).
C                       (INTERNAL)
C              IS1(J) - MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).
C                       (INTERNAL)
C              IS2(J) - MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       (INTERNAL)
C              IS4(J) - MOS-2000 GRIB SECTION 4 ID'S (J=1,4).
C                       (INTERNAL)
C          IUSED(L,M) - WORK ARRAY ASSOCIATED WITH EACH PREDICTOR
C                       (M=1,MTRMS(L)) IN EACH EQUATION (L=1,KGP).
C                       INITIALLY SET TO ZERO; SET TO 1 WHEN THE
C                       TERM HAS BEEN EVALUATED.  (INTERNAL)
C               ITIME - 0 TO FURNISH TO GFETCH TO INDICATE NO TIME
C                       OFFSET IS TO BE MADE BECAUSE OF RR.  (INTERNAL)
C              NWORDS - NUMBER OF WORDS IN XDATA( ) RETURNED FROM GFETCH.
C                       (INTERNAL)
C              LWORDS - NUMBER OF WORDS IN IPACK( ) RETURNED FROM PACKV.
C                       (INTERNAL)
C              NTIMES - THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT
C                       THE DATA HAVE BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ) AND RETURNED BY GFETCH.  (INTERNAL)
C               LD(J) - USED TO HOLD AND MANIPULATE THE 4-WORD 
C                       PREDICTAND ID PRIOR TO PACKING.(J=1,4)(INTERNAL)
C            LDPARS(J)- USED TO HOLD AND MANIPULATED THE PARED PREDICTAND
C                       ID PRIOR TO PACKING.(J=1,15) (INTERNAL)
C              NSOURC - THE "MODEL NUMBER" OR SOURCE OF DATA TAKEN FROM
C                       LSTORE(10, ) BY GFETCH.  (INTERNAL)
C               ISTAB - 1 WHEN THE VARIABLE RETRIEVED FROM OPTX IS BINARY;
C                       0 OTHERWISE.
C                       NOT ACTUALLY USED.  (INTERNAL)
C              ISCALE - BINARY SCALING FOR PACKING = 0.  (INTERNAL)
C               JP(J) - CONTROLS PRINTING OF FORECASTS (J=3).
C                       1 = PRINT, 0 = DON'T PRINT.  (J=1,2) NOT USED.
C                       NEEDED FOR PACKV.  INITIALIZED FROM IP18.
C                       (INTERNAL)
C                   N - DUMMY VARIABLE FOR CALL TO GFETCH.  (INTERNAL)
C               MDATE - NDATE UPDATED WITH ITAU( , ).  (INTERNAL)
C                NCAT - 0 FOR CALL TO OPTX.  MEANINGLESS FOR U905.
C
C        SUBPROGRAMS CALLED:
C             UNIQUE: - EST905
C
C          LIBRARY:
C            W3LIB    -
C            ENSLIB   - BINARY,CAT1,CAT2,DOY,GFETCH,OPTX,PACKV,TIMPR,UPDAT,
C                       WRTDLR
C
C        EXIT STATES:
C          COND =    0 - SUCCESSFUL RUN
C                   39 - NWORDS FROM GFETCH DO NOT EQUAL NSTA.
C
C REMARKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90 (xlf90 compiler)
C   MACHINE:  IBM SP
C
C$$$
C   
      CHARACTER*8 CCALL(ND1),
     1            CCALLD(ND5)
      CHARACTER*32 PLAIN(ND3),
     1             PLNSD(ND3)
      CHARACTER*60 RACESS(NUMRA),CFILX
C
      DIMENSION ICALL(L3264W,ND1),ISDATA(ND1),XDATA(ND1)
      DIMENSION IPACK(ND5),ICALLD(L3264W,ND5),
     1          IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION NGP(ND13),LGP(ND13),MTRMS(ND13)
      DIMENSION ID(7,ND13,ND2),IDPARS(15,ND13,ND2),TRESHL(ND13,ND2),
     1          JD(4,ND13,ND2),ITAU(ND13,ND2),IUSED(ND13,ND2)
      DIMENSION CONST(ND13,ND3),
     1          AVG(ND13,ND3),
     2          ESS(ND13,ND3),
     3          CORR(ND13,ND3)
      DIMENSION COEF(ND13,ND2,ND3)
      DIMENSION VECTOR(ND1,ND13,ND2)
      DIMENSION ISCALD(ND3),ICAT(ND3)
      DIMENSION ISCLSD(ND3)
      DIMENSION IDTAND(4,ND3),IDTPAR(15,ND3),
     1          IDTNSD(4,ND3),IDPRSD(15,ND3)
      DIMENSION IPLAIN(L3264W,4,ND3),
     1          IPLNSD(L3264W,4,ND3)
      DIMENSION LOCSTA(ND1)
      DIMENSION FCST(ND1,ND3)
      DIMENSION FINERR(ND1,ND3)
C        FINERR( , ) IS AN AUTOMATIC ARRAY
      DIMENSION KFILRA(NUMRA)
      DIMENSION JP(3),ISTOP(2)
      DIMENSION LD(4), LDPARS(15)
C
      DATA ITIME/0/
      DATA ISCALE/0/
      DATA JP/3*0/
      DATA NCAT/0/
C
D     CALL TIMPR(KFILDO,KFILDO,'START FCST95        ')
C
      IER=0
      IF(IP18.NE.0)JP(3)=1
C        JP(3) IS USED IN PACKV.
      CALL DOY(NDATE,NYR,NMO,NDA,NHR,MDOY) 
C
C        INITIALIZE THE FORECAST ARRAY TO 9999 WHEN ONE OR MORE
C        STATIONS HAS NO EQUATION.
C
      IF(INITF.EQ.0)GO TO 101
C
      DO 1001 NN=1,MTANDS
      DO 1000 K=1,NSTA
      FCST(K,NN)=9999.
      FINERR(K,NN)=9999.
 1000 CONTINUE
 1001 CONTINUE
C
C        INITIALIZE THE FORECAST ARRAY TO THE EQUATION CONSTANTS.
C        NOTE THAT NGP( ) CAN BE ZERO, BUT UNLESS THE "ONE TRIP"
C        COMPILER DIRECTIVE IS ON, THE LOOP WILL CORRECTLY NOT
C        EXECUTE.
C 
 101  DO 104 L=1,KGP
C
      DO 103 KK=1,NGP(L)
      K=LOCSTA(LGP(L)+KK-1)
C
      DO 102 NN=1,MTANDS
      FCST(K,NN)=CONST(L,NN)
D     WRITE(KFILDO,1010)L,KK,NN,K,CONST(L,NN)
D1010 FORMAT(' IN FCST95 AT 101,L,KK,NN,K,CONST(L,NN)',
D    1       4I6,F10.4)
 102  CONTINUE
C
 103  CONTINUE
C
 104  CONTINUE
C
C        INITIALIZE IUSED( , )
C
      DO 110 L=1,KGP
C
      DO 109 M=1,MTRMS(L)
      IUSED(L,M)=0
 109  CONTINUE
C
 110  CONTINUE
C
C        FIND ALL VARIABLES AND MAKE FORECASTS.
C  
      DO 400 L=1,KGP
C
      DO 399 M=1,MTRMS(L)
      LL=L
C       LL MAY BE CHANGED DURING THE LOOP.
      MM=M
C       MM MAY BE CHANGED DURING THE LOOP.
D     WRITE(KFILDO,115)L,M,IUSED(L,M)
D115  FORMAT(' FCST95 AT 105--L,M,IUSED(L,M)'3I4)
C
      IF(IUSED(L,M).EQ.1)GO TO 399
C
C        ADJUST NDATE FOR ITAU( , ).
C
      IF(ITAU(L,M).EQ.0)THEN
         MDATE=NDATE
      ELSE
         CALL UPDAT(NDATE,ITAU(L,M),MDATE)
      ENDIF
C
C        LOOK FIRST FOR FULL ID.  NORMALLY, FOR U905, MOST OR
C        ALL COMPUTATIONS WILL HAVE BEEN MADE.
C
      CALL GFETCH(KFILDO,KFIL10,ID(1,L,M),7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,XDATA,ND5,
     2            NWORDS,NPACK,MDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSOURC,MISSP,MISSS,L3264B,ITIME,
     4            IER)
C
C        AN ERROR IN GFETCH WILL GENERATE A DIAGNOSTIC.  MOST VALUES
C        OF IER SHOULD BE NEAR FATAL.  ISTOP(1) IS INCREMENTED, EXCEPT
C        WHEN IER = 47, WHICH JUST MEANS DATA COULD NOT BE FOUND.
C        ONLY POINT BINARY TRANSFORMATIONS CAN BE MADE DIRECTLY
C        (NOT USING OPTX) FROM U905.  IF THE VARIABLE IS NOT PRESENT,
C        THE BASIC VARIABLE IS LOOKED FOR.
C
      IF(IER.EQ.0)THEN
C
         IF(NWORDS.NE.NSTA)THEN
            WRITE(KFILDO,131)NWORDS,NSTA
            IER=39
            ISTOP(1)=ISTOP(1)+1
C
            DO 123 K=1,NSTA
            XDATA(K)=9999.
 123        CONTINUE
C
            GO TO 350
C
         ENDIF
C
         GO TO 350
C           WHEN FULL ID IS FOUND, IF VARIABLE IS BINARY, IT IS
C           ALREADY BINARY, SO DON'T GO THERE.
C
      ELSEIF(IER.EQ.47)THEN
C           JUST MISSING DATA, A NOT UNEXPECTED EVENT.  GO TO OPTX,
C           DO NOT COUNT THIS AS AN ERROR.
         GO TO 128
C
      ELSE
         ISTOP(1)=ISTOP(1)+1
C           FATAL ERROR FOR THIS VARIABLE.  IT WAS FOUND BY GFETCH, BUT
C           COULD NOT BE RETURNED.  ALL VALUES WILL BE MISSING.
C
         GO TO 350
C
      ENDIF
C
C        TRY TO FIND BASIC VARIABLE IN LSTORE AND RETURN IT
C        IN XDATA( ), UNLESS ID( , , ) AND JD( , , ) ARE THE
C        SAME.
C
 128  IF(JD(1,L,M).EQ.ID(1,L,M).AND.
     1   JD(2,L,M).EQ.ID(2,L,M).AND.
     2   JD(3,L,M).EQ.ID(3,L,M).AND.
     3   JD(4,L,M).EQ.ID(4,L,M))GO TO 134
C
      CALL GFETCH(KFILDO,KFIL10,JD(1,L,M),7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,XDATA,ND5,
     2            NWORDS,NPACK,MDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSOURC,MISSP,MISSS,L3264B,ITIME,
     4            IER)
C        NOTE THAT JD( ), NOT ID( ) IS USED IN THE ABOVE CALL.
C
      IF(IER.EQ.0)THEN
C
         IF(NWORDS.NE.NSTA)THEN
            WRITE(KFILDO,131)NWORDS,NSTA
 131        FORMAT(/' ****ERROR IN FCST95 RETURNING FROM GFETCH.',
     1              '  IER = 0, BUT NWORDS =',I5,' NOT EQUAL TO',
     2              ' NSTA =',I5,'.')
            IER=39
            ISTOP(1)=ISTOP(1)+1
C
            DO 133 K=1,NSTA
            XDATA(K)=9999.
 133        CONTINUE
C
            GO TO 350
C
         ENDIF
C
         GO TO 206
C           IF THE BASIC VARIABLE IS THE ONE FOUND, GIVE BINARY
C           A CHANCE.
C
      ELSEIF(IER.EQ.47)THEN
C           JUST MISSING DATA, A NOT UNEXPECTED EVENT.  GO TO OPTX,
C           DO NOT COUNT THIS AS AN ERROR.
         GO TO 134
C
      ELSE
         ISTOP(1)=ISTOP(1)+1
C           FATAL ERROR FOR THIS VARIABLE.  IT WAS FOUND BY GFETCH, BUT
C           COULD NOT BE RETURNED.  ALL VALUES WILL BE MISSING.
C
         GO TO 350
C
      ENDIF
C
C        MUST COMPUTE THIS VARIABLE.
C        NOTE THAT 9997 AS WELL AS 9999 MUST BE HANDLED.
C        THE DATA WILL BE RETURNED READY TO USE, EXCEPT
C        FOR POSSIBLE BINARY FORMULATION, IN XDATA( ).
C
 134  CALL OPTX(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1          ID(1,L,M),IDPARS(1,L,M),TRESHL(L,M),JD(1,L,M),ITAU(L,M),
     2          NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NCAT,NSTA,
     3          ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4          LSTORE,ND9,LITEMS,CORE,ND10,
     5          LASTL,LASTD,NBLOCK,NSTORE,NFETCH,
     6          IS0,IS1,IS2,IS4,ND7,
     7          L3264B,L3264W,ISTAB,IER)
C        IT IS ASSUMED THAT ANY VARIABLE COMPUTED IN OPTX IS NOT
C        BINARY.
C
      IF(IER.NE.0)THEN
C
         IF(IER.EQ.120)THEN
            ISTOP(1)=ISTOP(1)+1
C              IER = 120 FROM FINDST IN CONST IN OPTX MEANS ONE OR MORE
C              STATIONS NOT FOUND IN THE DIRECTORY.
         ELSE
            ISTOP(2)=ISTOP(2)+1
C              AN ERROR IN OPTX WILL GENERATE A DIAGNOSTIC AND DATA IN
C              XDATA( ) HAVE BEEN SET TO 9999.  ISTOP(2) IS INCREMENTED,
C              EVEN WHEN IER = 47, WHICH JUST MEANS DATA COULD NOT 
C              BE FOUND.  (IT IS POSSIBLE THAT ERRORS OTHER THAN JUST
C              MISSING DATA OCCURRED.)
            GO TO 350
         ENDIF
      ENDIF
C
 206  IF(IDPARS(3,L,M).NE.1)GO TO 207
C        IT IS NOT A BINARY.
C
      CALL BINARY(KFILDO,ID(1,L,M),IDPARS(3,L,M),TRESHL(L,M),
     1                XDATA,NSTA,IER)
      IF(IER.NE.0)ISTOP(1)=ISTOP(1)+1
C        IF IER NE 0, BINARY WILL HAVE WRITTEN A DIAGNOSTIC
C        AND SET XDATA( ) = 9999.
 207  CONTINUE
C
 350  IF(IP16.NE.0)THEN
         WRITE(IP16,351)(ID(J,L,M),J=1,4),NDATE
 351     FORMAT(/,' DATA FOR VARIABLE  ',3(1X,I9.9),1X,I10.3,
     1            ' FOR DATE',I12)
         WRITE(IP16,352)(CCALL(K),XDATA(K),K=1,NSTA)
 352     FORMAT((' ',A8,1X,F12.6))
      ENDIF
C
C
 353  DO 360 KK=1,NGP(LL)
      K=LOCSTA(LGP(LL)+KK-1)
C
      IF(XDATA(K).NE.9999.)THEN
C
         DO 355 NN=1,MTANDS
         IF(FCST(K,NN).EQ.9999.)GO TO 355
D        WRITE(KFILDO,354)LL,MM,KK,NGP(LL),LGP(LL),K,NN,
D    1                    FCST(K,NN),COEF(LL,MM,NN),XDATA(K)
D354     FORMAT(' FCST95 AT 354--LL,MM,KK,NGP(LL),LGP(LL),K,NN,',
D    1          'FCST(K,NN),COEF(LL,MM,NN),XDATA(K)'/
D    2          '            ',7I6,3F8.2)
         FCST(K,NN)=FCST(K,NN)+COEF(LL,MM,NN)*XDATA(K)
 355     CONTINUE      
C
      ELSE
C
         DO 357 NN=1,MTANDS
         FCST(K,NN)=9999.
 357     CONTINUE
C
      ENDIF
C
C        SAVE THE PREDICTOR VECTOR FOR USE WITH THE MATRIX
C        X( , , ) LATER TO COMPUT ERROR ESTIMATES.  THE
C        ORDER IS THE SAME AS THE COEFFICIENTS IN COEF( , , ).
C
      VECTOR(K,LL,MM)=XDATA(K)
C
 360  CONTINUE 
C
      IUSED(LL,MM)=1
C
C        THE FOLLOWING LOOPS DETERMINE WHETHER OR NOT THIS
C        VARIABLE IS NEEDED FOR SOME OTHER EQUATION.  IF SO,
C        THE LL,MM COMBINATION IS USED IN THE LOOP ABOVE.  SINCE 
C        THE NUMBER OF EQUATIONS WILL USUALLY BE MUCH LESS
C        THAN THE NUMBER OF VARIABLES IN LSTORE( ), THE SEARCH
C        TIME SHOULD BE LESS THAN IF GFETCH WERE ENTERED FOR
C        EVERY TERM IN EVERY EQUATION.
C
      DO 370 LLL=LL,KGP
C
      DO 365 MMM=MM,MTRMS(LLL)
D     WRITE(KFILDO,361)LL,MM,LLL,MMM
D361  FORMAT(' FCST95 AT 361--LL,MM,LLL,MMM'4I4)
      IF(IUSED(LLL,MMM).EQ.1)GO TO 365
C        THE ABOVE TEST IS FOR SAFETY AND TO SKIP THE
C        LL = L, MM = M CASE.
C
C        THIS ID( ,LL,MM) HAS NOT BEEN USED.
C
D     WRITE(KFILDO,362)(ID(J,L,M),J=1,4),ITAU(L,M),
D    1                 (ID(J,LLL,MMM),J=1,4),ITAU(LLL,MMM)
D362  FORMAT(' FCST95 AT 362'2(I15,2I11,2I4))
C
      IF(ID(1,LLL,MMM).NE.ID(1,L,M).OR.
     1   ID(2,LLL,MMM).NE.ID(2,L,M).OR.
     2   ID(3,LLL,MMM).NE.ID(3,L,M).OR.
     3   ID(4,LLL,MMM).NE.ID(4,L,M).OR.
     4   ITAU(LLL,MMM).NE.ITAU(L,M))GO TO 365
C        THIS IS THE SAME PREDICTOR.
      LL=LLL
      MM=MMM
      GO TO 353
C
 365  CONTINUE
C
 370  CONTINUE
C
 399  CONTINUE
C
 400  CONTINUE
C
C        PRINT FORECASTS IF DESIRED.
C
      IF(IP17.EQ.0)GO TO 435
      WRITE(IP17,420)NDATE
 420  FORMAT(/' FORECASTS FOR STATIONS FOR',I12)
C
      DO 429 K=1,NSTA
      WRITE(IP17,427)CCALL(K),(FCST(K,NN),NN=1,MTANDS)
 427  FORMAT(' ',A8,1X,12F10.4/(10X,12F10.4))
 429  CONTINUE
C
C        POSTPROCESS THE FORECASTS.  INFLATION NEEDS TO BE
C        DONE HERE BECAUSE CORRELATIONS AND AVERAGE ARE
C        AVAILABLE HERE AND ARE NOT CARRIED FORWARD.  OTHER
C        POST PROCESSING COULD BE DONE, BUT MOST IS RESERVED
C        FOR POST PROCESSING ROUTINES THAT MAY NEED DATA AND
C        FORECASTS FROM MORE THAN ONE SET OF EQUATIONS.  IF
C        DONE HERE, THE ORDER OF EQUATION EVALUATION MIGHT
C        BE AN ISSUE.  CAT1/CAT2 ARE ENTERED FOR EACH CATEGORY.
C 
 435  DO 437 NN=1,MTANDS
C
      IF(ICAT(NN).EQ.1)THEN
         CALL CAT1(KFILDO,FCST(1,NN),AVG(1,NN),CORR(1,NN),
     1             LOCSTA,ND1,ND3,ND13,NSTA,
     2             KGP,NGP,LGP,MTANDS,
     3             ICAT(NN),IER)
         IF(IER.NE.0)ISTOP(1)=ISTOP(1)+1
      ELSEIF(ICAT(NN).EQ.2)THEN
         CALL CAT2(KFILDO,FCST(1,NN),AVG(1,NN),CORR(1,NN),
     1             LOCSTA,ND1,ND3,ND13,NSTA,
     2             KGP,NGP,LGP,MTANDS,
     3             ICAT(NN),IER)
         IF(IER.NE.0)ISTOP(1)=ISTOP(1)+1
C        IF IER NE 0, CAT1 OR CAT2 WILL HAVE WRITTEN A DIAGNOSTIC.
C        XDATA( ) IS NOT MODIFIED.
      ELSEIF(ICAT(1).NE.0.AND.ICAT(1).NE.1.AND.ICAT(1).NE.2)THEN 
         WRITE(KFILDO,436)ICAT(1)
 436     FORMAT(/,' ****ICAT(1) =',I4,' NOT RECOGNIZABLE IN FCST95.',
     1           '  CONTINUING.')
         ISTOP(1)=ISTOP(1)+1
      ENDIF
C
 437  CONTINUE
C
C        COMPUTE THE ERRORS OF ESTIMATE.
      CALL EST905(KFILDO,IP21,NDATE,CCALL,NSTA,
     1            KGP,NGP,LGP,MTRMS,MTANDS,
     2            ID,CONST,ESS,CORR,COEF,X,VECTOR,FINERR,
     3            IDTAND,LOCSTA,ND1,ND2,ND3,ND13,
     4            PLAIN,IER)
C
C        WRITE IF DESIRED.
C
      IF(KFILFC.EQ.0.AND.KFILX.EQ.0)GO TO 500
C    
      DO 440 NN=1,MTANDS
C
C        SET XMISSS = 0 OR 9997, RESPECTIVELY, DEPENDING ON
C        WHETHER OR NOT A 9997 OCCURS IN THE DATA TO BE 
C        PACKED.  SET XMISSP = 0 OR 9999, RESPECTIVELY, DEPENDING ON
C        WHETHER A 9997 OR 9999 OCCURS IN THE DATA.
C
      CALL SETMIS(KFILDO,FCST(1,NN),NSTA,XMISSP,XMISSS)
C
C        PACK THE DATA AND WRITE TO SEQUENTAL FILE WHEN DESIRED.
C        THE PACKED DATA ARE RETURNED IN IPACK( ) AND CAN BE
C        WRITTEN TO THE RANDOM ACCESS FILE.  PACKV WRITES
C        THE PACKED DATA ONLY WHEN KFILX NE 0.
C
C
C        SET THE 'G' OF THE OUTPUT ID USING THE VALUE IN
C        NESNUM AND THEN PACK THE DATA.
C
      LD(:)=0
      LDPARS(1:15)=0
      LD(1:4)=IDTAND(1:4,NN)
      LDPARS(1:15)=IDTPAR(1:15,NN)
C      
      LD(4)=(LD(4)/10)*10+NESNUM
      LDPARS(15)=NESNUM
C
      NBYT=NTOTBY
      CALL PACKV(KFILDO,KFILFC,LD(1),LDPARS(1),
     1           JP,ISCALD(NN),ISCALE,
     2           IPLAIN(1,1,NN),PLAIN(NN),NDATE,NYR,NMO,NDA,NHR,
     3           CCALL,ISDATA,FCST(1,NN),ND1,NSTA,IPACK,ND5,MINPK,
     4           IS0,IS1,IS2,IS4,ND7,XMISSP,XMISSS,
     5           IP18,LWORDS,NTOTBY,NTOTRC,
     6           L3264B,L3264W,ISTOP(1),IER)
C        AN ERROR IN PACKV WILL PRINT A DIAGNOSTIC AND INCREMENT
C        ISTOP(1).
C
      IF(KFILX.EQ.0)GO TO 430
C        VEENHUIS: CHANGED IS1(9) TO LD(1)
C
      CALL WRTDLR(KFILDO,KFILX,CFILX,LD(1),ICALL,CCALL,ND1,NSTA,
     1            ICALLD,CCALLD,ND5,IPACK,LWORDS,
     2            NREPLA,NCHECK,L3264B,L3264W,IER) 
      IF(IER.NE.0)ISTOP(1)=ISTOP(1)+1
C
C        PACK AND WRITE THE ERRORS OF ESTIMATE.
C
 430  IF(IDPRSD(3,NN).EQ.0)THEN
      CALL SETMIS(KFILDO,FINERR(1,NN),NSTA,XMISSP,XMISSS)
C
C        ENSURE THAT THE ERROR ESTIMATE IS SET TO MISSING IF
C        THE FORECAST IS MISSING
C
      DO 433 K=1,NSTA
         IF(NINT(FCST(K,NN)).EQ.9999)FINERR(K,NN)=9999.
 433  CONTINUE
C
C        SET THE 'G' OF THE OUTPUT ID USING THE VALUE IN
C        NESNUM AND THEN PACK THE DATA.
C
      LD(:)=0
      LDPARS(1:15)=0
      LD(1:4)=IDTNSD(1:4,NN)
      LDPARS(1:15)=IDPRSD(1:15,NN)
C      
      LD(4)=(LD(4)/10)*10+NESNUM
      LDPARS(15)=NESNUM
C
      CALL PACKV(KFILDO,KFILFC,LD(1),LDPARS(1),
     1           JP,ISCLSD(NN),ISCALE,
     2           IPLNSD(1,1,NN),PLNSD(NN),NDATE,NYR,NMO,NDA,NHR,
     3           CCALL,ISDATA,FINERR(1,NN),ND1,NSTA,IPACK,ND5,MINPK,
     4           IS0,IS1,IS2,IS4,ND7,XMISSP,XMISSS,
     5           IP18,LWORDS,NTOTBY,NTOTRC,
     6           L3264B,L3264W,ISTOP(1),IER)
C
      IF(KFILX.EQ.0)GO TO 440
C        VEENHUIS: CHANGED IS1(9) TO LD(1)
C
      CALL WRTDLR(KFILDO,KFILX,CFILX,LD(1),ICALL,CCALL,ND1,NSTA,
     1            ICALLD,CCALLD,ND5,IPACK,LWORDS,
     2            NREPLA,NCHECK,L3264B,L3264W,IER) 
      IF(IER.EQ.0)GO TO 440
      ISTOP(1)=ISTOP(1)+1
C        AN ERROR IN WRTDLR WILL PRINT A DIAGNOSTIC.
      ENDIF
C
 440  CONTINUE
C
 500  RETURN
      END
