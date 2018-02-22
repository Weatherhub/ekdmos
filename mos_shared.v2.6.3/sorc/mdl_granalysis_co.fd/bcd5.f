      SUBROUTINE BCD5(KFILDO,KFILOG,KFILOV,KFILQC,
     1                IP14,IP16,IP17,IP18,IP19,IP20,IP21,IP22,I405ADG,
     2                ID,IDPARS,JP,IVRBL,MODNO,ISCALD,JFIRST,
     3                NDATE,JDATE,DATA,CCALL,NAME,XP,YP,XPL,YPL,
     4                XPE,YPE,
     5                TOSS,QUEST,LTAG,LNDSEA,ELEV,QUALST,XLAPSE,
     6                VRAD,NSTA,ND1,ISETP,ILS,WTWTL,WTLTW,HGTTHA,HGTTHB,
     7                P,FD2,CORR,COUNT,NCOUNT,ND2X3,NX,NY,
     8                IPACK,IWORK,ND5,MINPK,
     9                TELEV,SEALND,NXE,NYE,MESHE,EMESH,
     A                IS0,IS1,IS2,IS4,ND7,
     B                MD,IPLAIN,PLAIN,
     C                NAREA,ALATL,ALONL,NPROJ,ORIENT,
     D                MESH,MESHB,MESHL,XLAT,
     E                MSHPAS,ER1,
     F                NTYPE,B,CSTSM,R,
     G                RSTAR,LNDWAT,ITRPLQ,
     H                IALGOR,ELCORR,IBKPN,BK,
     I                ELCORU,RWATO,RWATI,IVRAD,
     J                TLOD,SETLOD,THID,SETHID,CONSTD,NSCALD,EX1D,EX2D,
     K                IALOC,ADIST,AELEV,ND13,N4P,NSHLN,
     L                NSMTYP,U,V,WNDWT,WNDGRD,WNDTHR,WNDTRN,
     M                NPRT,JPRT,NTDL,JTDL,NPASS,MGUESS,
     N                CINT,ORIGIN,SMULT,SADD,TITLE,IOPT,POSTDS,
     O                JTOTBY,JTOTRC,MTOTBY,MTOTRC,ITOTBY,ITOTRC,NOTOSS,
     P                L3264B,L3264W,ISTOP,IER)
C
C        JUNE      1993   GLAHN, CHAMBERS   TDL   HP9000
C        AUGUST    2000   GLAHN   MODIFIED FOR LAMP-2000
C        JUNE      2004   GLAHN   MODIFIED SLIGHTLY FOR MOS-2000
C        SEPTEMBER 2004   GLAHN   MODIFIED LD( ) AND ITAUH FOR WRITING
C        SEPTEMBER 2004   GLAHN   ADDED IQUAL( ) TO CALL, ETC.
C        OCTOBER   2004   GLAHN   SHORTENED LINE TO 72 CHARACTERS
C                                 WRITING FORMAT 2787 TO KFILDO;
C                                 ADDED QUALWT( ) TO CALL; EXTRA PRINT
C                                 TO IP18 WHEN PACKED DATA ARE LISTED
C        OCTOBER   2004   GLAHN   ADDED NELEV( ) TO CALL AND TO CORBC5
C        OCTOBER   2004   GLAHN   CHANGED NELEV( ) TO ELEV( ); REMOVED
C                                 EMESH IN CALL TO CORBC5
C        OCTOBER   2004   GLAHN   MODIFIED FOR LAT/LON VICE POLE 
C        OCTOBER   2004   GLAHN   ADDED LNDSEA( ), LNDWAT( , ), ISETP,
C                                 ILS, CALL TO SETPNT
C        OCTOBER   2004   GLAHN   CHANGED CALL TO PAWOTG TO PAWGTS 
C        OCTOBER   2004   GLAHN   INSERTED LAMBERT AND MERCATOR
C                                 CAPABILITY 
C        NOVEMBER  2004   GLAHN   EXCHANGED ACTUAL FOR MSHXMS; SOME
C                                 STATEMENT NUMBERS CHANGED
C        NOVEMBER  2004   GLAHN   EXCHANGED 2 ARGUMENTS IN CALL TO
C                                 ACTUAL
C        DECEMBER  2004   GLAHN   ADDED ELCORR( ), XLAPSE( , , ),
C                                 AA( , , , ), IBASE, IDIMTB, IDIM,
C                                 IBASE( ), ND1, ND13, ND14, ND15 TO
C                                 CALL; REMOVED IDIM
C        DECEMBER  2004   GLAHN   DIMENSIONED ELCORR( )
C        DECEMBER  2004   GLAHN   ELIMINATED CALL TO CUTIT IN TWO PLACES
C                                 IF INPUT AND OUTPUT GRIDS ARE THE SAME
C        DECEMBER  2004   GLAHN   POSITION TO CALL SETPNT CHANGED AND
C                                 280 CHANGED TO 2799 IN SEVERAL PLACES
C        DECEMBER  2004   GLAHN   CHANGED QUALWT(3) TO QUALWT(4)
C        JANUARY   2005   GLAHN   CHANGED KM TO M IN CALLS TO PSIJLL, 
C                                 LMIJLL, AND MCIJLL
C        JANUARY   2005   GLAHN   SUBSTITUTED ITRPSL FOR ITRP TO GET
C                                 ANALYSIS VALUE; ADDED LNDSEA,
C                                 SEALND( ), NXE, NYE, MESHE TO CALL
C                                 TO ESP5
C        JANUARY   2005   GLAHN   ADDED COMMENT AFTER CALL TO ITRPSL
C        FEBRUARY  2005   GLAHN   ADDED ISETP TO CALL TO SETPNT
C        FEBRUARY  2005   GLAHN   ADDED JFIRST TO CALL AND TO CALL TO
C                                 ESP5; CHANGED SOME VALUES OF
C                                 FRACT( ) FROM .51 TO .6
C        FEBRUARY  2005   GLAHN   UPDATED IVRBL RELATED COMMENTS
C        MARCH     2005   GLAHN   CHANGED LOCATION OF STATEMENT 400
C        MARCH     2005   GLAHN   ADDED NOSTM, TRSTL( ), TRSTU( ),
C                                 IDST( ), IDPARST( ) TO CALL
C        MAY       2005   GLAHN   ELIMINATED STRATIFICATION FEATURE;
C                                 ADDED LAPSE CALCULATION ON THE FLY
C        JULY      2005   GLAHN   ADDED XLAPSE AND ELEV TO CALL TO ESP5
C        JULY      2005   GLAHN   ADDED NAME( ) TO CALL; ADDED IP21, 
C                                 NAME( ), AND NPASS TO CALL TO ESP5
C        JULY      2005   GLAHN   ADDED NOTOSS TO CALL AND CALL TO ESP5
C        JULY      2005   GLAHN   ADDED CALLS TO SMOTHG
C        AUGUST    2005   GLAHN   ADDED IF TESTS TO DEAL WITH IOPT(1)=0;
C                                 ADDED COMMENTS ABOUT SUBSET AREA
C        AUGUST    2005   GLAHN   CHANGED FORMATS 260 AND 262 TO FX.3
C        AUGUST    2005   GLAHN   CHANGED ARGUMENT MD TO MD(4)
C        AUGUST    2005   GLAHN   MODIFIED USE OF LNDSEA( )
C        SEPTEMBER 2005   GLAHN   ADDED POSTPROCESSING; POSTDS TO CALL
C        SEPTEMBER 2005   GLAHN   CHANGED NSCALE FOR DISSNO FROM 0 TO 1
C                                 AND TRUNC FROM 0 TO .1
C        SEPTEMBER 2005   GLAHN   ADDED TRUNC TO CALL
C        SEPTEMBER 2005   GLAHN   CORRECTED ERROR IN DISPOSABLE OUTPUT
C                                 WHEN MESH AND GRID SIZE NE BASE VALUE
C        SEPTEMBER 2005   GLAHN   ADDED NSMTYP = 7 CAPABILITY; ADDED
C                                 NSMTYP TO CALL TO SMOTHG
C        SEPTEMBER 2005   GLAHN   ADDED IUSEIW (NOT IMPLEMENTED)
C        OCTOBER   2005   GLAHN   ADDED IBKPN AND BK( )
C        NOVEMBER  2005   GLAHN   MOVED SOME STATEMENTS 3 SPACES LEFT
C        NOVEMBER  2005   GLAHN   ADDED LIMITX
C        JANUARY   2006   GLAHN   REMOVED LIMITX
C        JANUARY   2006   GLAHN   ADDED ELCORU( , )
C        JANUARY   2006   GLAHN   ADDED IALGOR( , )
C        MARCH     2006   GLAHN   ADJUSTED CALL FORMAT; CORRECTED FORMAT
C                                 1865 FROM KFILOV TO KFILQC
C        MARCH     2006   GLAHN   ADDED IP14 TO CALL AND IP14 AND
C                                 ISTOP( ) TO CALL TO ITRPSL; ADDED
C                                 CSTSM, N4P
C        APRIL     2006   GLAHN   CHANGED FORMAT 186
C        APRIL     2006   GLAHN   REVISED COMPUTATIONAL LOOPS INVOLVING
C                                 IP19, 1P20, IP21, IOPT(1)
C        MAY       2006   GLAHN   CHANGED 218461 TO 208462--VECTOR QC
C                                 CHANGED 218461 TO 228462--GRIDDED FG
C                                 CHANGED 208461 TO 208462--VECTOR TOSS
C        JUNE      2006   GLAHN   ADDED IBKPN = 99 CAPABILITY
C        JUNE      2006   GLAHN   CHANGED CALLS TO SZGRID TO SZGRDM
C        JULY      2006   GLAHN   ADDED DISPOP; COMMENTS
C        DECEMBER  2006   GLAHN   ADDED MGUESS TO CALL & CALL TO CORBC5
C        JANUARY   2007   GLAHN   ADDED NSHLN( ).
C        JANUARY   2007   GLAHN   ADDED POSTPROCESSING VOR WIND SP
C        FEBRUARY  2007   GLAHN   CHANGED COMMENT TO QUALWT( )
C        FEBRUARY  2007   GLAHN   ADDED IBKPN TO CALL TO ESP5
C        MARCH     2007   GLAHN   ADDED COUNT( , ) TO CALL TO SMOTHG
C        MARCH     2007   GLAHN   PULLED THE DO 105 LOOP INTO U405
C        MARCH     2007   GLAHN   REMOVED IQUAL( ) AND QUALWT( )
C        MARCH     2007   GLAHN   CORRECTED CALL TO SMTH9
C        JUNE      2007   GLAHN   ADDED RWATO( ) AND RWATI( )
C        JUNE      2007   GLAHN   REARRANGED CALL TO CORBC5
C        JUNE      2007   GLAHN   ELIMINATED TRUNC( ); ADDED 
C                                 TLOD,SETLOD,THID,SETHID,EX1D,EX2D;
C                                 CALL TO POST
C        AUGUST    2007   GLAHN   ADDED VRAD(ND1), IVRAD
C        SEPTEMBER 2007   GLAHN   REPLACED NEWXY WITH NEWXY1
C        OCTOBER   2007   GLAHN   INCREASED DIMENSION VRAD(ND1,6)
C        OCTOBER   2007   GLAHN   ADDED ND1 TO CALL
C        NOVEMBER  2007   GLAHN   INCREASED POSTDS FROM 1 TO POSTDS(3)
C                                 ALONG WITH POSTPROCESSING PARAMETERS
C        NOVEMBER  2007   GLAHN   ADDED ORIENT TO CALL TO CORBC5
C        DECEMBER  2007   GLAHN   ADDED IP(25) AND ISTOP(6) CAPABILITY
C        FEBRUARY  2007   GLAHN   ADDED NAREA
C        MARCH     2008   GLAHN   SUBSTITUTED ITRPSX FOR ITRPSL
C        MARCH     2008   GLAHN   ADDED DISCIG; CHANGED NVAL TO
C                                 NXD*NYD IN CALL TO POST
C        MARCH     2008   GLAHN   ADDED IBKPN, ELCORR(LP), ELCORU(LP)
C                                 TO CALL TO ITRPSX
C        MAY       2008   GLAHN   ADDED LNDWAT = 2,3 CAPABILITY;
C                                 ELIMINATED IUSEIW
C        OCTOBER   2008   GLAHN   ADDED WTWTL, WTLTW
C        NOVEMBER  2008   GLAHN   CHANGED CORR( ) TO NCOUNT( ) IN
C                                 3 CALLS TO PAWGTS; COMMENTS; SPELLING
C        JANUARY   2009   GLAHN   ADDED CALL TO VISMI
C
C        PURPOSE
C            TO DO A BERGTHORSSEN-CRESSMAN-DOOS SUCCESSIVE
C            APPROXIMATION ANALYSIS ON A SCALAR FIELD.  ADAPTED FROM
C            IBM 360/195 M400.  THE VARIABLES HANDLED ARE SPECIFIED
C            IN A TABLE IN THE CALLING PROGRAM, U405A.   SURFACE WINDS
C            CAN BE USED IN SEA LEVEL PRESSURE ANALYSIS (NOT YET
C            IMPLEMENTED).  OUTPUT IS IN MB FOR PRESSURE AND DEG
C            FAHRENHEIT FOR TEMPERATURE.  THE NX BY NY ANALYSIS IS
C            RETURNED IN P( ).
C
C            "DISPOSABLE" OR "SUBSETTED" GRIDS, DIMENSIONS IN IOPT( ),
C            CAN BE CREATED FOR (1) GRIDPRINTING AND WRITING TO UNIT
C            NO. IP22 AND/OR (2) TDLPACKING AND WRITING TO UNIT 
C            NO. KFILOG.  THESE GRIDS CAN BE USED FOR CHECKOUT AND
C            QUALITY CONTROL.  THE CAPABILITY PERTAINS TO EACH PASS
C            AND TO BOTH UNSMOOTHED AND SMOOTHED GRIDS.  WHEN
C            IOPT(1) = 0, THERE IS NO SUBSET AREA AND SOME COMPUTATIONS
C            ARE BYPASSED.
C
C            BCD5 AND THE CALLED ESP5 DEPEND ON IVRBL BEING 1 FOR
C            SEA LEVEL PRESSURE AND 4 FOR SATURATION.  OTHER NUMBERS
C            ARE FLEXIBLY USED. 
C
C            THE OUTPUT GRIDS FROM BCD5 ARE AT THE CURRENT MESH LENGTH
C            MESH.  WHEN A GRID HAS BEEN CLIPPED TO A LARGER MESH 
C            LENGTH IN FSTGS5 THAN MESH, THE AREA COVERED WITH
C            NON-MISSING DATA MAY BE SLIGHTLY GRATER THAN THE DESIRED
C            AREA AT MESH LENGTH MESH.              
C
C            FATAL ERRORS, IER:
C               NONE.
C
C        DATA SET USE
C            KFILDO   - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C            KFILOG   - UNIT NUMBER FOR DISPOSABLE TDLPACK GRIDPOINT
C                       OUTPUT.  (OUTPUT)
C            KFILOV   - UNIT NUMBER OF OUTPUT VECTOR FILE CONTAINING
C                       ALL OBS EXCEPT THOSE TOSSED OR QUESTIONABLE
C                       OBS AS MISSING.  (OUTPUT)
C            KFILQC   - UNIT NUMBER OF OUTPUT VECTOR FILE CONTAINING
C                       QUALITY CONTROLLED OBS AFTER THE FINAL ANALYSIS
C                       PASS.  (OUTPUT)
C            IP14     - UNIT NUMBER FOR LISTING COMPUTED LAPSE
C                       RATES AND PROBLEMS WITH LAPSE RATES.  (OUTPUT)
C            IP16     - UNIT NUMBER FOR INDICATING WHEN A RECORD IS
C                       WRITTEN TO THE SEQUENTIAL FILE.  (OUTPUT)
C            IP17     - UNIT NUMBER FOR LISTING OF STATIONS, THEIR
C                       X/Y POSITIONS,THEIR DATA VALUES, AND LTAGS.
C                       (OUTPUT)
C            IP18     - UNIT NUMBER FOR LISTING OF STATIONS,
C                       THEIR X/Y POSITIONS, DATA VALUES, LTAGS,
C                       UNSMOOTHED ANALYSIS (INTERPOLATED) VALUES, AND 
C                       DIFFERENCES BETWEEN THE DATA AND THE UNSMOOTHED
C                       ANALYSIS VALUES FOR THE WHOLE ANALYSIS AREA. ALSO 
C                       USED IN PACKV FOR LISTING OF DATA WRITTEN. 
C                       (OUTPUT)
C            IP19     - UNIT NUMBER FOR LISTING OF STATIONS, THEIR 
C                       X/Y POSITIONS, DATA VALUES, LTAGS,
C                       SMOOTHED ANALYSIS (INTERPOLATED) VALUES, AND 
C                       DIFFERENCES BETWEEN THE DATA AND THE ANALYSIS
C                       VALUES FOR THE WHOLE ANALYSIS AREA FOR EACH
C                       PASS ON WHICH SMOOTHING IS DONE.  (OUTPUT)
C            IP20     - UNIT NUMBER FOR LISTING OF STATIONS, THEIR
C                       X/Y POSITIONS, DATA VALUES, LTAGS, UNSMOOTHED
C                       ANALYSIS (INTERPOLATED) VALUES, AND DIFFERENCES 
C                       BETWEEN THE DATA AND THE UNSMOOTHED ANALYSIS
C                       VALUES FOR THE ANALYSIS AREA.  ALSO, WHEN
C                       THERE IS A SUBSETTED AREA, THE MEAN ABSOLUTE
C                       DIFFERENCE BETWEEN THE UNSMOOTHED ANALYSIS
C                       AND THE DATA.  (OUTPUT)
C            IP21     - UNIT NUMBER FOR LISTING THE AVERAGE DEGREE
C                       OF FIT BETWEEN THE UNSMOOTHED AND SMOOTHED,
C                       IF SMOOTHED, ANALYSIS AND THE DATA OVER THE
C                       WHOLE AREA.  ALSO USED IN ESP5 TO LIST
C                       STATIONS TOSSED ON PASS 5.  (OUTPUT)
C            IP22     - UNIT NUMBER FOR GRIDPRINTING.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C              KFILOG = UNIT NUMBER FOR DISPOSABLE TDLPACK GRIDPOINT
C                       OUTPUT.  THIS IS FOR DIFFERENT PASSES OF THE
C                       ANALYSES AND THEIR SMOOTHINGS.  (INPUT)
C              KFILOV = UNIT NUMBER OF OUTPUT VECTOR FILE CONTAINING
C                       ALL OBS EXCEPT THOSE TOSSED OR QUESTIONABLE
C                       OBS AS MISSING.  (INPUT)
C              KFILQC = UNIT NUMBER OF OUTPUT VECTOR FILE CONTAINING
C                       QUALITY CONTROLLED OBS AFTER THE FINAL ANALYSIS
C                       PASS.  (OUTPUT)
C                IP14 = UNIT NUMBER FOR LISTING COMPUTED LAPSE
C                       RATES AND PROBLEMS WITH LAPSE RATES.  (INPUT)
C                IP16 = INDICATES WHETHER (>0) OR NOT (=0) 
C                       A STATEMENT WILL BE OUTPUT TO IP(16)
C                       WHEN A SEQUENTIAL FILE IS WRITTEN THROUGH
C                       PAWGTS.  (INPUT))
C                IP17 = UNIT NUMBER FOR LISTING OF STATIONS, THEIR
C                       X/Y POSITIONS,THEIR DATA VALUES, AND LTAGS.
C                       (INPUT)
C                IP18 = UNIT NUMBER FOR LISTING OF STATIONS, THEIR 
C                       X/Y POSITIONS, DATA VALUES, LTAGS,
C                       UNSMOOTHED ANALYSIS (INTERPOLATED) VALUES, AND 
C                       DIFFERENCES BETWEEN THE DATA AND THE ANALYSIS
C                       VALUES FOR THE WHOLE ANALYSIS AREA.  ALSO USED
C                       IN PACKV FOR LISTING OF DATA WRITTEN TO THE
C                       RESOLUTION PACKED.  ALSO, SEE IP20.  (INPUT)
C                IP19 = UNIT NUMBER FOR LISTING OF STATIONS, THEIR 
C                       X/Y POSITIONS, DATA VALUES, LTAGS,
C                       SMOOTHED ANALYSIS (INTERPOLATED) VALUES, AND 
C                       DIFFERENCES BETWEEN THE DATA AND THE ANALYSIS
C                       VALUES FOR THE WHOLE ANALYSIS AREA FOR EACH
C                       PASS ON WHICH SMOOTHING IS DONE.  (INPUT)
C                IP20 = UNIT NUMBER FOR LISTING OF STATIONS, THEIR
C                       X/Y POSITIONS, DATA VALUES, LTAGS, UNSMOOTHED
C                       ANALYSIS (INTERPOLATED) VALUES, AND DIFFERENCES 
C                       BETWEEN THE DATA AND THE UNSMOOTHED ANALYSIS
C                       VALUES FOR THE ANALYSIS AREA.  ALSO, WHEN
C                       THERE IS A SUBSETTED AREA, THE MEAN ABSOLUTE
C                       DIFFERENCE BETWEEN THE UNSMOOTHED ANALYSIS
C                       AND THE DATA.
C                       (INPUT)
C                IP21 = UNIT NUMBER FOR LISTING THE AVERAGE DEGREE
C                       OF FIT BETWEEN THE UNSMOOTHED AND SMOOTHED,
C                       IF SMOOTHED, ANALYSIS AND THE DATA FOR THE
C                       WHOLE ANALYSIS AREA.  ALSO USED IN ESP5 TO 
C                       LIST STATIONS TOSSED ON PASS 5.  (INPUT)
C                IP22 = UNIT NUMBER FOR WRITING GRIDPRINTED UNSMOOTHED
C                       AND SMOOTHED ANALYSES OVER SUBSETTED AREA.
C                       (INPUT)
C             I405ADG = 1 = DIAGNOSTIC PRINT TO KFILDO;
C                       0 OTHERWISE.  (INTERNAL)
C               ID(J) = 4 WORD ID OF VARIABLE BEING ANALYZED.
C                       COMES FROM ITABLE( ,2, ) IN U405A.  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       PREDICTOR ID'S CORRESPONDING TO ID( ,N)
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
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C                       (INPUT)
C             JP(J,N) = INDICATES WHETHER A PARTICULAR VARIABLE N MAY
C                       HAVE GRIDPRINTS (J=1), INTERMEDIATE TDLPACK
C                       OUTPUT (J=2), OR PRINT OF VECTOR RECORDS IN
C                       PACKV (J=3) (N=1,ND4).  PACKV IS FOR THE 
C                       DATA SHOWING T0SSED DATA AS MISSING AND 
C                       QUESTIONABLE DATA AS MISSING.  THIS IS
C                       AN OVERRIDE FEATURE FOR THE PARAMETERS FOR 
C                       GRIDPRINTING AND TDLPACKING IN EACH VARIABLE'S 
C                       CONTROL FILE.  (INPUT)
C               IVRBL = 1 = VARIABLE IS SLP.
C                       2 = FLEXIBLE.
C                       3 = FLEXIBLE.
C                       4 = SATURATION DEFICIT.
C                       OTHERS - FLEXIBLE.
C                       SEE ITABLE( , , ) IN U405A FOR OTHER VALUES.
C                       NOTE:  BCD5 AND CALLED ESP5 DEPEND ON IVRBL
C                       BEING 1 AND 4 FOR SEA LEVEL PRESSURE AND
C                       SATURATION DEFICIT, RESPECTIVELY.  (INPUT)
C               MODNO = OUTPUT MODEL NUMBER.  (INPUT)
C              ISCALD = DECIMAL SCALING FOR TDLPACKING.  (INPUT)
C              JFIRST = USED IN ESP5 VIA BCD5 TO CONTROL PRINTING.
C                       THIS IS SPECIFIC TO THE VARIABLE BEING
C                       ANALYZED.  (INPUT/OUTPUT)
C               NDATE = DATE/TIME, YYYYMMDDHH.  THIS IS THE ANALYSIS
C                       RUN TIME, INCLUDING HH.  (INPUT)
C            JDATE(J) = NDATE PARSED INTO ITS 4 COMPONENTS:
C                       J=1 IS YYYY
C                       J=2 IS MM
C                       J=3 IS DD
C                       J=4 IS HH
C                       (INPUT)
C             DATA(J) = DATA TO ANALYZE (J=1,NSTA).  (INPUT)
C            CCALL(J) = STATION CALL LETTERS (J=1,NSTA).  (CHARACTER*8)
C                       (INPUT)
C             NAME(K) = NAMES OF STATIONS (K=1,NSTA).  (CHARACTER*20)
C                       (INPUT)
C               XP(K) = THE X POSITION FOR STATION K (K=1,NSTA) ON 
C                       THE ANALYSIS GRID AREA AT THE CURRENT GRID MESH 
C                       LENGTH MESH.  (INPUT)
C               YP(K) = THE Y POSITION FOR STATION K (K=1,NSTA) ON 
C                       THE ANALYSIS GRID AREA AT THE CURRENT GRID MESH 
C                       LENGTH MESH.  (INPUT)
C              XPL(K) = THE X POSITION FOR STATION K (K=1,NSTA) ON
C                       THE ANALYSIS GRID AT THE MESH LENGTH MESHB.
C                       (INPUT)
C              YPL(K) = THE Y POSITION FOR STATION K (K=1,NSTA) ON
C                       THE ANALYSIS GRID AT THE MESH LENGTH MESHB.
C                       (INPUT)
C              XPE(K) = THE X POSITION FOR STATION K (K=1,NSTA) ON 
C                       THE ELEVATION GRID AT THE GRID MESH LENGTH
C                       MESHE.  (INPUT)
C              YPE(K) = THE Y POSITION FOR STATION K (K=1,NSTA) ON 
C                       THE ELEVATION GRID AT THE GRID MESH LENGTH
C                       MESHE.  (INPUT)
C             TOSS(K) = CONTAINS TOSSED OBS (K=1,NSTA).  ALL OTHER
C                       VALUES ARE 9999.  (INTERNAL)
C            QUEST(K) = CONTAINS QUESTIONABLE OBS (K=1,NSTA).  ALL
C                       OTHER VALUES ARE 9999.  QUESTIONABLE IS DEFINED
C                       AS NOT MEETING X PERCENT OF THE ERROR THRESHOLD,
C                       WHERE X IS HARDWIRED BY PASS.  (INTERNAL)
C             LTAG(J) = DENOTES USE OF DATA CORRESPONDING TO CCALL(J).
C                       +2 = NOT USED FOR ANY PURPOSE.
C                       +1 = PERMANENTLY DISCARDED FOR THE VARIABLE
C                            BEING ANALYZED.  INCLUDES DATA FAR
C                            OUTSIDE THE GRID, AS DEFINED BY RMAX
C                        0 = USE ON CURRENT PASS THROUGH DATA.
C                       -1 = DO NOT USE ON THIS PASS.
C                       -3 = ACCEPT THIS STATION ON EVERY PASS.  THIS
C                            FEATURE MAY OR MAY NOT BE IMPLEMENTED IN
C                            THE CALLING PROGRAM.  (INPUT/OUTPUT)
C           LNDSEA(K) = LAND/SEA INFLUENCE FLAG FOR EACH STATION
C                       (K=1,ND1).
C                       0 = WILL BE USED FOR ONLY OCEAN WATER (=0)
C                           GRIDPOINTS.
C                       3 = WILL BE USED FOR ONLY INLAND WATER (=3)
C                           GRIDPOINTS.
C                       6 = WILL BE USED FOR BOTH INLAND WATER (=3)
C                           AND LAND (=9) GRIDPOINTS.
C                       9 = WILL BE USED FOR ONLY LAND (=9) GRIDPOINTS.
C                       (INPUT)
C             ELEV(K) = ELEVATION OF STATIONS IN METERS (K=1,NSTA).
C                       (INPUT)
C           QUALST(K) = THE QUALITY WEIGHTS TO APPLY FOR THIS VARIABLE
C                       (K=1,KSTA).  (INTERNAL)
C           VRAD(K,L) = RADII OF INFLUENCE USED AS OVERRIDE TO U405.CN 
C                       CONTROL FILE (K=1,NSTA) (L=1,6).  NOTE THAT
C                       THIS APPLIES TO THE TOTAL RUN; ITS USE IS 
C                       CONTROLLED BY IVRAD BY ELEMENT.  (INPUT)
C                NSTA = NUMBER OF POINTS FOR WHICH DATA ARE AVAILABLE.
C                       THIS CAN BE BOTH FROM STATION DATA AND FROM
C                       SAMPLING THE FIRST GUESS.  (INPUT)
C               ISETP = FLAG TO INDICATE WHETHER AFTER THE LASS PASS
C                       A GRIDPOINT WILL BE SET TO THE CLOSEST
C                       STATION (=2), TO A VALUE IN THE DIRECTION
C                       OF THE STATION VALUE BUT NOT CROSS AN INTEGER
C                       BOUNDARY (=1), OR NOT (=0).  (THIS COULD BE
C                       PARTICULARIZED TO QUALITY OF DATA.)  (INPUT)
C                 ILS = FLAG FOR HOW THE SEA/LAND CORRECTIONS ARE
C                       GOING TO BE MADE:
C                       1 = LAND/WATER TREATED SEPARATELY;
C                       0 = OTHERWISE.  THIS OPERATES IN TANDOM WITH
C                       LNDWAT( , ), BUT ILS CAN OVERRIDE FOR EASY CHANGE.
C                       (INPUT)
C               WTWTL = WEIGHTING FACTOR TO USE FOR OCEAN OR INLAND WATER
C                       POINTS OVER LAND WHEN ILS = 1.  OCEAN POINTS
C                       NEVER AFFECT INLAND WATER.  (INPUT)
C               WTLTW = WEIGHTING FACTOR TO USE FOR LAND POINTS OVER
C                       OCEAN AND INLAND WATER WHEN ILS = 1.  (INPUT)
C              HGTTHA = ELEVATION DIFFERENCE IN M BETWEEN A STATION
C                       AND A GRIDPOINT WHICH MUST NOT BE EXCEEDED
C                       FOR THE STATION TO INFLUENCE THE GRIDPOINT.
C                       (INPUT)
C              HGTTHB = ELEVATION DIFFERENCE IN M BETWEEN A STATION
C                       AND ANY (INTERPOLATED) POINT ON THE ELEVATION
C                       GRID BETWEEN THE STATION AND THE GRIDPOINT
C                       WHICH MUST NOT BE EXCEEDED FOR THE STATION 
C                       TO INFLUENCE THE GRIDPOINT.  (INPUT)
C                P(J) = FIELD HOLDING FIRST GUESS AND ANALYSIS
C                       (J=1,NX*NY).  (INPUT/OUTPUT)
C              FD2(J) = WORK ARRAY FOR GRIDPRINTING (J=1,ND2X3).
C                       (INTERNAL)
C             CORR(J) = CORRECTION FOR GRIDPOINT IX,JY (J=1,NX*NY).
C                       (INTERNAL)
C            COUNT(J) = SUM OF WEIGHTS FOR GRIDPOINT IX,JY WHEN TYPE 3
C                       CORRECTION BEING MADE (J=1,NX*NY).
C                       (INTERNAL)
C           NCOUNT(J) = COUNT OF STATIONS CORRECTING GRIDPOINT IX,JY
C                       (J=1,NX*NY) IN CORBC5.  ALSO USED AS SCRATCH 
C                       IN PACKV, SOMTHN, SOMTHC, AND PAWGTS.
C                       (INTERNAL)
C               ND2X3 = SIZE OF FD2( ), P( ), CORR( ), COUNT( ), AND
C                       NCOUNT( ).  (INPUT)
C                  NX = NUMBER OF GRIDPOINTS IN THE XI (LEFT TO RIGHT)
C                       DIRECTION AT CURRENT MESH LENGTH MESH.
C                       (INPUT/OUTPUT)
C                  NY = NUMBER OF GRIDPOINTS IN THE YJ (BOTTOM TO TOP)
C                       DIRECTION AT CURRENT MESH LENGTH MESH.
C                       (INPUT/OUTPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND DATA( ).
C                       (INPUT)
C               MINPK = MINIMUM GROUP SIZE WHEN PACKING THE DATA.
C                       (INPUT)
C            TELEV(J) = THE TERRAIN ELEVATION FROM THE MOS-2000 EXTERNAL
C                       RANDOM ACCESS FILE (J=1,NXE*NYE).  (INPUT)
C           SEALND(J) = THE LAND/SEA MASK (J=1,NXE*NYE).
C                       0 = OCEAN WATER GRIDPOINTS;
C                       3 = INLAND WATER GRIDPOINTS.
C                       9 = LAND GRIDPOINTS.
C                       (INPUT)
C                 NXE = X-EXTENT OF TELEV( , ), LNDSEA( , ), AND
C                       SEALND( , ) AT MESH LENGTH MESHE.  (INPUT)
C                 NYE = Y-EXTENT OF TELEV( , ), LNDSEA( , ), AND
C                       SEALND( , ) AT MESH LENGTH MESHE.  (INPUT)
C               MESHE = THE NOMINAL MESH LENGTH OF THE TERRAIN GRID.
C                       IT IS MANDATORY THE GRID AVAILABLE IS OF THIS
C                       MESH SIZE AND COVER THE SAME AREA SPECIFIED
C                       BY NXL BY NYL, EVEN IF MESHE IS NOT EQUAL
C                       TO MESHB.  (INPUT)
C               EMESH = ACTUAL MESH LENGTH CORRESPONDING TO MESHE.
C                       (INPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,4).
C                       (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,21+).
C                       (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).
C                       (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND
C                       IS4( ).  (INPUT)
C               MD(J) = THE 4-WORD ID OF THE VECTOR DATA BEING
C                       ANALYZED (ITABLE(1,2,IVRBL) IN CALLING
C                       PROGRAM).  (INPUT)
C         IPLAIN(L,J) = 32 CHARACTERS (L=1,L3264W) (J=1,4) OF PLAIN
C                       LANGUAGE DESCRIPTION OF THE VARIABLE.
C                       NOTE THAT THIS REQUIRES TWO 32-BIT WORDS TO HOLD
C                       THE DESCRIPTION BUT ONLY ONE 64-BIT WORD.
C                       EQUIVALENCED TO PLAIN( ) IN DRU150.
C               PLAIN = THE PLAIN LANGUAGE DESCRIPTION OF THE VARIABLE
C                       IN ID( ).  EQUIVALENCED TO IPLAIN( , ) IN 
C                       DRU150.  (CHARACTER*32)
C               NAREA = THE AREA OVER WHICH THE ANALYSIS IS MADE:
C                       1 = CONUS,
C                       2 = ALASKA,
C                       3 = HAWAII,
C                       4 = PUERTO.
C               ALATL = NORTH LATITUDE OF LOWER LEFT CORNER POINT
C                       OF A GRID OF THE SIZE  NXL, NYL.  TRUNCATED
C                       TO TEN THOUSANDTHS OF DEGREES.  NOTE THAT THE
C                       MOS-2000 ARCHIVE IS ONLY TO THOUSANDTHS OF
C                       DEGREES.  (INPUT)
C               ALONL = WEST LONGITUDE OF LOWER LEFT CORNER POINT
C                       OF A  GRID OF THE SIZE  NXL, NYL.  TRUNCATED
C                       TO TEN THOUSANDTHS OF DEGREES.  NOTE THAT THE
C                       MOS-2000 ARCHIVE IS ONLY TO THOUSANDTHS OF
C                       DEGREES.  (INPUT)
C               NPROJ = NUMBER OF MAP PROJECTION TO WHICH THIS GRID
C                       APPLIES.
C                       3 = LAMBERT.
C                       5 = POLAR STEREOGRAPHIC.
C                       7 = MERCATOR.
C                       (INPUT)
C              ORIENT = ORIENTATION OF GRID IN WEST LONGITUDE.  (INPUT)
C                MESH = THE NOMINAL MESH LENGTH OF THE CURRENT GRID.
C                       (INPUT/OUTPUT)
C               MESHB = THE NOMINAL MESH LENGTH OF THE ANALYSIS GRID.
C                       1/4 BEDIENT AT 60 N IS 95.25 KM WHICH IS ABOUT
C                       80 KM OVER THE U.S.  MESH = 80 CORRESPONDS TO 
C                       95.25 STORED WITH THE GRIDS.  NXL, NYL, ETC.
C                       ARE IN RELATION TO THIS.
C               MESHL = NOMINAL MESH LENGTH OF QUALITY CONTROL
C                       (SUBSETTED) GRID FOR CONTINUOUS VARIABLES.
C                       (INPUT)
C                XLAT = NORTH LATITUDE AT WHICH GRIDLENGTH IS SPECIFIED
C                       IN DEGREES.  (INPUT)
C           MSHPAS(J) = THE NOMINAL MESH LENGTH FOR EACH PASS
c                       (J=1,NPASS) FOR THE GUESS OPTION BEING USED.
C                       (INPUT)
C              ER1(J) = ERROR CRITERIA FOR PASS (J=1,NPASS).
C                       IF OBSERVATION IS DIFFERENT FROM CURRENT
C                       ANALYSIS BY MORE THAN ER1( ), IT IS
C                       PROBABLY NOT USED ON THIS PASS.  HOWEVER,
C                       BEFORE A DATUM IS DISCARDED, A BUDDY CHECK
C                       IS MADE.  ALSO IF ER1(J) = 0, IT MEANS CHECK
C                       IS NOT PERFORMED ON THIS PASS.  (INPUT)
C            NTYPE(J) = TYPE OF CORRECTION FOR PASS J (J=1,NPASS).
C                         0 MEANS SKIP THIS PASS
C                         1 MEANS W = 1
C                         2 MEANS W = (R**2 - D**2)/(R**2 + D**2)
C                         3 MEANS SAME AS 2 EXCEPT SUM OF WEIGHTS IN
C                           DENOMINATOR.  (INPUT)
C                B(J) = SMOOTHING PARAMETER FOR PASS J (J=1,NPASS).
C                       B( ) = 0 MEANS NO SMOOTHING.  (INPUT)
C               CSTSM = THE SMOOTHING PARAMETER IF ANY POINT HAS WATER 
C                       BUT NOT ALL ARE WATER.  USE INSTEAD OF BQ.
C                       (INPUT)
C                R(J) = RADIUS OF INFLUENCE FOR PASS J IN GRID UNITS
C                       (J=1,NPASS).  NOTE THAT THE ACTUAL DISTANCE
C                       DEPENDS ON MESH.  (INPUT)
C            RSTAR(J) = MULTIPLICATIVE FACTOR TO USE WITH R(J) IN
C                       DETERMINING HOW FAR OUTSIDE GRID TO USE DATA.
C                       FOR PASS J, PROGRAM WILL USE DATA R(J)*RSTAR(J)
C                       GRID UNITS OUTSIDE GRID.  (INPUT)
C           LNDWAT(J) = FLAG FOR EACH PASS (J=1,NPASS) TO DETERMINE 
C                       HOW THE SEA/LAND CORRECTIONS WILL BE MADE.
C                       0 = DON'T USE THE DIFFERENCES FEATURE; ALL DATA
C                           WILL BE USED FOR ALL POINTS.
C                       1 = USE THE DIFFERENCE FEATURE.  SEE LNDSEA( ).
C                       2 = OCEAN POINTS WILL NOT BE CHANGED.
C                       3 = NEITHER OCEAN NOR INLAND WATER POINTS WILL
C                           BE CHANGED.
C                       (INPUT)
C           ITRPLQ(J) = TYPE OF INTERPOLATION TO GO FROM ONE MESH
C                       LENGTH TO ONE OF HALF THAT FOR EACH PASS J
C                       (J=1,NPASS).
C                       1 = BILINEAR
C                       2 = BIQUADRATIC
C                       (INPUT)
C           IALGOR(J) = TYPE OF CORRECTION ALGORITHM TO APPLY FOR EACH
C                       PASS (J=1,NPASS).
C                       1 = NORMAL TERRAIN
C                       2 = DISTANCE WEIGHTED TERRAIN
C                       (INPUT)
C           ELCORR(J) = FRACTION OF THE ELEVATION CORRECTION TO 
C                       APPLY FOR EACH PASS (J=1,NPASS).  (INPUT)
C               IBKPN = FLAG TO INDICATE WHETHER TO APPLY BK( , ) TO
C                       POSITIVE OR NEGATIVE LAPSE RATES:
C                        0 = DON'T OPERATE BK( , ) (ALL LAPSES USED),
C                       +1 = APPLY TO POSITIVE LAPSES (POSITIVE IS ODD
C                            FOR TEMPERATURE),
C                       +2 = SAME AS 1, BUT DON'T APPLY DOWNWARD,
C                       -1 = APPLY TO NEGATIVE LAPSES (NEGATIVE IS ODD
C                            FOR SNOW),
C                       -2 = SAME AS -1, BUT DON'T APPLY DOWNWARD, AND
C                       99 = DON'T COMPUTE OR USE LAPSE RATES.
C                       (LAPSE RATES WILL ALSO NOT BE USED WHEN KFILLP
C                       IS NOT PROVIDED AND WHEN ELCORR( ) FOR ALL
C                       PASSES = 0.  KFILLP IS THE FILE FOR READING
C                       PAIRS TO COMPUTE LAPSE.)
C                       (INPUT)
C               BK(J) = THE MAXIMUM RADII IN GRIDPOINTS FOR WHICH
C                       THE LAPSE RATES INDICATED BY IBKPN ARE USED
C                       FOR EACH PASS J (J=1,NPASS).  (INTERNAL)
C           ELCORU(J) = FRACTION OF THE ELEVATION CORRECTION TO 
C                       APPLY FOR EACH PASS (J=1,NPASS) FOR THE
C                       "UNUSUAL" LAPSE RATE (THE ONE WITH THE SIGN
C                       SPECIFIED IN IBKPN.  (INPUT)
C            RWATO(J) = FACTOR BY WHICH TO INCREASE THE RADIUS FOR
C                       OCEAN WATER POINTS (J=1,NPASS).  (INPUT)
C            RWATI(J) = FACTOR BY WHICH TO INCREASE THE RADIUS FOR
C                       INLAND WATER POINTS (J=1,NPASS).  (INPUT)
C               IVRAD = CONTOLS HOW VRAD( ) AND THE RADII R( , )
C                       ARE USED.  (INPUT)
C                       0 = USE R( , ) NORMALLY.
C                       1 = USE VRAD( , ) OVERRIDE.
C             TLOD(J) = LOW THRESHOLD FOR DISPOSABLE GRIDS (J=1,3).
C                       WHEN A LAST PASS GRIDPOINT IS
C                       LT TLOD, IT IS SET TO SETLOD.  (INPUT)
C           SETLOD(J) = SEE TLOD (J=1,3).  (INPUT)
C             THID(J) = HIGH THRESHOLD FOR DISPOSABLE GRIDS (J=1,3).
C                       WHEN A LAST PASS  GRIDPOINT IS
C                       GT THID, IT IS SET TO SETHID.  (INPUT)
C           SETHID(J) = SEE THID (J=1,3).  (INPUT)
C           CONSTD(J) = ADDITIVE CONSTANT TO FURNISH TO THRESHOLDING
C                       AND SCALING SUBROUTINE FOR DISPOSABLE GRIDS
C                       (J=1,3).  (INPUT)
C           NSCALD(J) = SCALING CONSTANT TO FURNISH TO THRESHOLDING
C                       AND SCALING SUBROUTINE FOR DISPOSABLE GRIDS
C                       (J=1,3).  (INPUT)
C             EX1D(J) = EXTRA PARAMETER FOR DISPOSABLE GRIDS NOT YET
C                       USED FOR THRESHOLDING (J=1,3).  (INPUT)
C             EX2D(J) = EXTRA PARAMETER FOR DISPOSABLE GRIDS NOT YET
C                       USED FOR THRESHOLDING (J=1,3).  (INPUT)
C            IALOC(J) = LOCATIONS IN CCALL( , ) OF THE PAIRED STATIONS
C                       (J=1,ND13), NOALOC(K) VALUES FOR EACH STATION K
C                       (K=1,LSTA).  (INPUT)
C            ADIST(J) = DISTANCES OF BASE STATION OF THE PAIRED STATIONS
C                       (J=1,ND13), NOALOC(K) VALUES FOR EACH STATION K.
C                       (INPUT)
C            AELEV(J) = ELEVATION DIFFERENCES OF BASE STATION OF THE
C                       PAIRED STATIONS (J=1,ND13), NOALOC(K) VALUES
C                       FOR EACH STATION K.  (INPUT)
C                ND13 = MAXIMUM TOTAL PAIRS OF STATIONS.  DIMENSION OF
C                       IALOC( ), ADIST( ), AND AELEV( ).  (INPUT)
C                 N4P = 4 INDICATES THE SURROUNDING 4 POINTS WILL BE
C                         CHECKED WHEN TRYING TO FIND A GRIDPOINT OF
C                         THE SAME TYPE AS THE DATUM AND INTERPOLATION
C                         CAN'T BE DONE.  CURRENTLY, THIS IS ALWAYS
C                         DONE (DOES NOT REQUIRE N4P=4).
C                       12 SAME AS ABOVE, EXCEPT 12 ADDITIONAL POINTS
C                         WILL BE CHECKED WHEN NONE OF THE 4 POINTS
C                         ARE OF THE CORRECT TYPE.
C                       N4P IS OPERATIVE ONLY WHEN THE DATUM AND
C                       THE SURROUNDING 4 POINTS ARE OF MIXED TYPE.
C                       (INPUT)
C            NSHLN(J) = DETERMINES SMOOTHING AT HIGH AND LOW ELEVATIONS.
C                       A 1 INDICATES:
C                         J=1--HIGH ELEVATION, HIGH VALUE SMOOTHED.
C                         J=2--HIGH ELEVATION, LOW VALUE SMOOTHED.
C                         J=3--HIGH ELEVATION, NOT HIGH OR LOW VALUE 
C                                              SMOOTHED.
C                         J=4--LOW ELEVATION, HIGH VALUE SMOOTHED.
C                         J=5--LOW ELEVATION, LOW VALUE SMOOTHED.
C                         J=6--LOW ELEVATION, NOT HIGH OR LOW VALUE
C                                             SMOOTHED.
C                       A 0 INDICATES NO SMOOTHING FOR THE VALUES OF J.
C                       (INPUT)
C              NSMTYP = TYPE OF SMOOTHING:
C                       1 = NORMAL, 5-PT
C                       2 = SAME EXCEPT NO CHANGE IS MADE UNLESS ONE
C                           OF THE POINTS TO CONTRIBUTE TO THE NEW
C                           VALUE HAS BEEN CHANGED.
C                       3 = 9-POINT SMOOTHING USED ON LAST PASS ONLY,
C                           ANY OTHER PASS DEFAULTS TO NSMTYP = 2.
C                       4 = USES SUBROUTINE SMOTHC FOR PASSES GE 4 
C                           FOR SLP ONLY.  FOR PASSES LT 4 OR NOT SLP,
C                           DEFAULTS TO NSMTYP = 2.
C                       5 = SPECIAL TERRAIN-FOLLOWING SMOOTHING.
C                       6 = TWO PASSES OF 5 ABOVE.
C                       7 = THREE PASSES OF 5 ABOVE.
C                       (INPUT)
C                U(K) = THE FACTOR TO USE IN CONVERTING U-WINDS
C                       TO CHANGE IN MB PER MESH LENGTH (K=1,NSTA)
C                       BUT ONLY WHEN ANALYZING SLP.  U(K) HAS 
C                       BEEN SET TO 9999 FOR OBS TOSSED BY U405B AS
C                       WELL AS WIND SPEEDS LT WNDTHR.  U( ) IS 
C                       DIMENSIONED ND2X3 IN THE DRIVER DRU155, AND
C                       ND2X3 IS GUARANTEED TO BE GE ND1.  (INPUT)
C                V(K) = SAME AS U(K) EXCEPT FOR V-WINDS.  (INPUT)
C            WNDWT(J) = WEIGHT TO APPLY TO WIND OBS CORRECTIONS
C                       RELATIVE TO PRESSURE CORRECTIONS (J=1,6).
C                       THIS WILL BE ZERO FOR ALL EXCEPT PRESSURE
C                       ANALYSIS.  (INPUT)
C              WNDGRD = PARAMETER FOR CONVERTING WIND SPEED TO SLP
C                       GRADIENTS.  (INPUT)
C              WNDTHR = THRESHOLD TO USE FOR WIND SPEED FOR WIND TO BE 
C                       USED IN ANALYSIS.  FOR PRINTING ONLY.  (INPUT)
C              WNDTRN = DEGREES TO TURN SURFACE WIND BEFORE APPLYING
C                       GEOSTROPHIC CORRECTION.  FOR PRINTING ONLY.
C                       (INPUT)
C             NPRT(J) = 1 FOR PRINTING OF ANALYSIS AFTER PASS J
C                       (J=1,6).
C                       0 FOR NO PRINTING.  (INPUT)
C             JPRT(J) = SAME AS NPRT(J) EXCEPT FOR SMOOTHED ANALYSIS.
C                       (INPUT)
C             NTDL(J) = 1 FOR TDLPACKING AND WRITING SUBSETTED 
C                       UNSMOOTHED ANALYSIS AFTER PASS J (J=1,NPASS).
C                       ZERO FOR NO WRITING.  (INPUT)
C             JTDL(J) = SAME AS ABOVE EXCEPT FOR SMOOTHED ANALYSIS.
C                       (INPUT)
C               NPASS = NUMBER OF PASSES TO PERFORM FOR VARIABLE. 
C                       (INPUT)
C              MGUESS = THE TYPE OF FIRST GUESS ACTUALLY USED.
C                       1 = CONSTANT.
C                       2 = PRIMARY GRID (E.G., A MOS FORECAST).
C                       3 = ALTERNATE GRID.
C                       4 = AVERAGE OF OBSERVATIONS.
C                       (INPUT)
C             CINT(J) = THE CONTOUR INTERVAL WHEN GRIDPRINTING, APPLIES 
C                       TO THE UNITS IN UNITS(N) (J=1,NPASS).
C                       (INTERNAL)
C           ORIGIN(J) = THE CONTOUR ORIGIN WHEN GRIDPRINTING, APPLIES
C                       TO THE UNITS IN UNITS(N) (J=1,NPASS). 
C                       (INTERNAL)
C            SMULT(J) = THE MULTIPLICATIVE FACTOR WHEN CONTOURING OR
C                       GRIDPRINTING THE DATA (J=1,NPASS).
C                       NOTE SMULT( ), SADD( ), ORIGIN( ), CINT( ),
C                       AND UNITS( ), ALTHOUGH NAMED THE SAME AND
C                       PLAYING THE SAME ROLE IN GRIDPRINTING, ARE
C                       NOT THE SAME VARIABLES AS IN THE CALLING
C                       PROGRAM; THEY ARE FILLED HERE AND PERTAIN
C                       TO EACH PASS.  (INTERNAL)
C             SADD(J) = THE ADDITIVE FACTOR WHEN CONTOURING OR
C                       GRIDPRINTING THE DATA (J=1,NPASS).  (INTERNAL)
C               TITLE = 40-CHARACTER TITLE FOR VARIABLE.  (CHARACTER*40)
C                       (INPUT)
C             IOPT(J) = SUBSETTING VALUES USED IN GRIDPRINTING (J=1,8).
C                       WHEN IOPT(1) = 0, SUBSETTING IS NOT DONE,
C                       STATISTICS ARE NOT CALCULATED, AND IP20 IS NOT
C                       USED.  IOPT( ) IS IN RELATION TO THE SUBSETTED
C                       AREA MESH LENGTH MESHL.
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
C                       (INPUT)
C           POSTDS(J) = HOLDS NAME OF DISPOSABLE POSTPROCESSING ROUTINE
C                       (J=1,3).  (CHARACTER*6)  (INPUT)
C              JTOTBY = THE TOTAL NUMBER OF BYTES ON THE FILE ASSOCIATED
C                       WITH UNIT NO. KFILOG.  (INPUT/OUTPUT)
C              JTOTRC = THE TOTAL NUMBER OF RECORDS IN THE FILE ON UNIT
C                       NUMBER KFILOG.  (INPUT/OUTPUT)
C              MTOTBY = THE TOTAL NUMBER OF BYTES ON THE FILE ASSOCIATED
C                       WITH UNIT NO. KFILOV.  (INPUT/OUTPUT)
C              MTOTRC = THE TOTAL NUMBER OF RECORDS IN THE FILE ON UNIT
C                       NUMBER KFILOV.  (INPUT/OUTPUT)
C              ITOTBY = THE TOTAL NUMBER OF BYTES ON THE FILE ASSOCIATED
C                       WITH UNIT NO. KFILQC.
C              ITOTRC = THE TOTAL NUMBER OF RECORDS IN THE FILE ON UNIT
C                       NUMBER KFILQC.
C              NOTOSS = RUNNING OF COUNT OF STATIONS TOSSED ON LAST
C                       PASS.  (INPUT/OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT).
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).
C                       (INPUT)
C            ISTOP(J) = ISTOP(1)--IS INCREMENTED BY 1 EACH TIME AN ERROR 
C                                 OCCURS.
C                       ISTOP(2)--IS INCREMENTED WHEN THERE ARE
C                                 FEW DATA (200) FOR AN ANALYSIS.
C                       ISTOP(3)--IS INCREMENTED WHEN A DATA RECORD 
C                                 COULD NOT BE FOUND.
C                       ISTOP(4)--IS INCREMENTED WHEN A LAPSE RATE COULD
C                                 NOT BE COMPUTED OR HAS TOO FEW CASES
C                                 TO BE USED.
C                       ISTOP(5)--IS INCREMENTED WHEN NO NON-MISSING
C                                 GRIDPOINT AROUND THE DATA POINT IS
C                                 OF THE SAME TYPE.
C                       ISTOP(6)--IS INCREMENTED WHEN THERE IS A PROBLEM
C                                 WITH MAKING BOGUS STATIONS.
C                       (INPUT/OUTPUT)
C                 IER = ERROR RETURN.
C                         0 = GOOD RETURN.
C                       777 = FATAL ERROR.
C                       (OUTPUT)
C                  LP = PASS NUMBER.  (INTERNAL)
C                  BB = INTERPOLATED VALUE FROM SUBROUTINE INTR.
C                       (INTERNAL)
C               ITAUH = PROJECTION OF FIELD IN HOURS.  (INTERNAL)
C               ITAUM = PROJECTION OF FIELD IN MINUTES.  (INTERNAL)
C           IOPTGR(J) = SUBSETTING VALUES USED IN DETERMINING WHETHER
C                       A POINT SHOULD BE INCLUDED IN SUMMING THE
C                       DIFFERENCES FOR THE SUBSETTED AREA (J=1,8).
C                       THESE ARE CALCULATED IN REFERENCE TO THE 
C                       CURRENT GRID MESH BEING USED.  (INTERNAL)
C                 NXD = THE X EXTENT OF THE DISPOSABLE GRID.  (INTERNAL)
C                 NYD = THE Y EXTENT OF THE DISPOSABLE GRID.  (INTERNAL)
C               ALATD = LL LATITUDE OF THE DISPOSABLE GRID.  TRUNCATED
C                       TO THOUSANDS TO AGREE WITH ARCHIVE WHEN THE
C                       GRIDS ARE THE SAME.  THIS IS NECESSARY FOR 
C                       U203 FOR GEMPAK.  (INTERNAL)
C               ALOND = LL LONGITUDE OF THE DISPOSABLE GRID.  SEE ALATD.
C                       (INTERNAL)
C            FRACT(J) = FRACTION QUEST( ) CRITERIA ARE OF ER1(J) (J=1,6).
C                       J = PASS.  (INTERNAL)
C                ER1Q = ERROR CRITERIA FOR THE CURRENT PASS FOR QUEST( ).
C                       ER1Q(LP)=ER1(LP)*FRACT(LP), WHERE LP IS THE 
C                       PASS NUMBER.  (INTERNAL)
C              ISCALE = BINARY SCALING FACTOR, SET TO ZERO.  (INTERNAL)
C              XMISSP = PRIMARY MISSING VALUE FOR PACKING, SET = 9999.
C                       OR 0 DEPENDING ON USE.  (INTERNAL)
C              XMISSS = SECONDARY MISSING VALUE FOR PACKING, SET = 0.
C                       (INTERNAL)
C              IUSEIW = THIS HAS BEEN ELIMINATED.  PREVIOUS USE WAS:
C                       0 WHEN INLAND WATER/LAND POINT (LNDSEA(K) = 6) IS 
C                       NOT TO BE USED FOR INAND WATER (SEALND( , )=3).
C                       1 OTHERWISE.  THIS WAS FOR FOR THE WESTERN
C                       TILE, THE ONLY INLAND BODY IS THE GREAT SALT 
C                       LAKE.  WHEN THE ANALYSIS IS EXTENDED TO THE 
C                       CONUS, THIS WILL SOMEHOW HAVE TO DISTINGUISHES
C                       WHICH INLAND BODIES.
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED
C            TIMPR, ESP5, ITRP, PRTGR, SMOTH, SMOTHN, SMOTHC, SMTH9,
C            SZGRDM, NEWXY1, CORBC5, TRNSFR, CUTIT, ACTUAL, ITRPSX,
C            PSIJLL, LMIJLL, MCIJLL, PAWGTS, PRSID1, PACKV, SETPNT,
C            POST
C
      CHARACTER*6 POSTDS(3)
      CHARACTER*8 CCALL(NSTA)
      CHARACTER*8 SMTH/'SMOOTHD '/
      CHARACTER*8 BLANK/'        '/
      CHARACTER*20 NAME(NSTA)
      CHARACTER*32 PLAIN
      CHARACTER*40 TITLE
C
      DIMENSION DATA(NSTA),XP(NSTA),YP(NSTA),XPL(NSTA),YPL(NSTA),
     1          XPE(NSTA),YPE(NSTA),LTAG(NSTA),TOSS(NSTA),QUEST(NSTA),
     2          U(NSTA),V(NSTA),LNDSEA(NSTA),ELEV(NSTA),
     3          QUALST(NSTA),XLAPSE(NSTA),VRAD(ND1,6)
      DIMENSION P(ND2X3),CORR(ND2X3),COUNT(ND2X3),NCOUNT(ND2X3),
     1          FD2(ND2X3)
      DIMENSION IPACK(ND5),IWORK(ND5)
      DIMENSION IPLAIN(L3264W,4)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION IALOC(ND13),ADIST(ND13),AELEV(ND13)
      DIMENSION TELEV(NXE*NYE),SEALND(NXE*NYE)
      DIMENSION SMULT(6),SADD(6),ORIGIN(6),CINT(6),
     1          ER1(6),R(6),B(6),NTYPE(6),RSTAR(6),LNDWAT(6),
     2          NPRT(6),JPRT(6),NTDL(6),JTDL(6),WNDWT(6),
     3          MSHPAS(6),ITRPLQ(6),FRACT(6),IALGOR(6),ELCORR(6),
     4          BK(6),ELCORU(6),RWATO(6),RWATI(6)
      DIMENSION ID(4),IDPARS(15),LD(4),LDPARS(15),MD(4),NSHLN(6),
     1          IOPT(8),IOPTGR(8),JDATE(4),JP(3),ISTOP(6)
      DIMENSION TLOD(3),SETLOD(3),THID(3),SETHID(3),CONSTD(3),NSCALD(3),
     1          EX1D(3),EX2D(3)
C
      DATA FRACT/.6, .6, .6, .6, .6, .6/
      DATA ISCALE/0/
C
      IER=0
CD     CALL TIMPR(KFILDO,KFILDO,'START BCD5          ')
C
C        INITIALIZE THE PORTION OF IOPTGR( ) THAT DOES NOT CHANGE.
C        WHEN THERE IS NO SUBSET AREA, IOPT(1) = IOPTGR(1) = 0.
C
      IOPTGR(1)=IOPT(1)
      IOPTGR(6)=IOPT(6)
      IOPTGR(7)=IOPT(7)
      IOPTGR(8)=IOPT(8)
C 
C        DO NPASS PASSES FOR THE ANALYSIS.
C
      DO 280 LP=1,NPASS
CD     WRITE(KFILDO,1400)LP,ALATL,ALONL,MESHL,MESH,NX,NY
CD1400 FORMAT(/' AT 1400 IN BCD5--LP,ALATL,ALONL,MESHL,',
CD    1        'MESH,NX,NY,',I4,2F10.5,6I6)
C
      WRITE(KFILDO,150)LP,TITLE,ER1(LP)
 150  FORMAT(/' STARTING PASS',I3,' FOR ',A16,
     1        '   ERROR CRITERION =',F8.2,15X,
     2        '&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&')
C
      IF(NTYPE(LP).EQ.0)THEN
C           EVEN THOUGH NPASS CAN BE AS HIGH AS 6, A PARTICULAR
C           PASS IS SKIPPED IF NTYPE(LP) = 0.
         WRITE(KFILDO,170)LP
 170     FORMAT(/' ****THIS PASS =',I3,' BEING SKIPPED BECAUSE',
     1           ' NTYPE(LP) = 0.  MAY BE AN ERROR.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 280
C
      ELSEIF(MSHPAS(LP).EQ.0)THEN
C           A MESH LENGTH OF 0 CANNOT BE ACCOMMODATED, SO SKIP
C           THIS PASS.
         WRITE(KFILDO,172)LP
 172     FORMAT(/' ****THIS PASS =',I3,' BEING SKIPPED BECAUSE',
     1           ' MSHPAS(LP) = 0.  PROBABLY AN ERROR.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 280
      ENDIF
C
C        AT THE BEGINNING OF EACH PASS, THERE IS A GRID IN P( )
C        WITH DIMENSIONS NX, NY AND AT A MESH LENGTH OF MESH.
C        MSHPAS(LP) CONTAINS THE MESH LENGTH TO USE FOR PASS LP.
C        IF MESH NE MSHPAS(LP), THE GRID MUST BE "RESIZED" AND THE
C        X AND Y POSITIONS OF THE STATIONS IN RELATION TO IT MUST BE
C        CHANGED.  NOTE, AGAIN, THAT NX, NY, MESH,
C        XP( ) AND YP( ) FOLLOW THE GRID.  THAT IS, WHATEVER
C        IS IN P( ) WILL HAVE THOSE CHARACTERISTICS.  ON PASS 1
C        THE GRID SHOULD BE OF THE CORRECT SIZE.
C
      IF(MESH.NE.MSHPAS(LP))THEN
C           XP( ) AND YP( ) ARE IN RELATION TO MESH; MODIFY
C           THEM IF MESH IS TO CHANGE.
C
C           MSHPAS( ) IS READ FROM THE .CN CONTROL FILE.  VERIFY
C           MSHPAS( ) IS A LEGITIMATE VALUE IN RANGE 1 TO 320.
C  
         IF(MESH.GT.320.OR.MESH.LT.1)THEN
            WRITE(KFILDO,173)MSHPAS(LP)
 173        FORMAT(/' ****MSHPAS(L) =',I6,' NOT IN RANGE 1 TO 320.',
     1              '  STOP IN BCD5 AT 173.')
            STOP 173
         ENDIF
C
         CALL NEWXY1(KFILDO,MESH,XP,YP,MSHPAS(LP),XP,YP,NPROJ,NSTA)
         RATIO=FLOAT(MESH)/MSHPAS(LP)
C
CD        WRITE(KFILDO,1735)NX,NY
CD1735    FORMAT(/' CALLING SZGRDM AT 1735--NX,NY',2I6)
C
         CALL SZGRDM(KFILDO,P,NX,NY,
     1               MESH,MSHPAS(LP),ITRPLQ(LP),ND2X3)
C           NOTE THAT SZGRDM SETS MESH TO EQUAL MSHPAS(LP) AND
C           MODIFIES NX AND NY ACCORDINGLY.
C
C           ADJUST U( ) AND V( ) FOR CURRENT MESH WHEN SLP
C           BEING ANALYZED (IVRBL = 1).
C
         IF(IVRBL.EQ.1)THEN
            FAC=1./RATIO
C
            DO 174 K=1,NSTA
C
            IF(U(K).NE.9999.)THEN
               U(K)=U(K)*FAC
               V(K)=V(K)*FAC
            ENDIF
C         
 174        CONTINUE
C         
         ENDIF
C
      ENDIF
C   
C        MESH HAS BEEN SET FOR THIS PASS AND WILL REMAIN CONSTANT.
C        LIKEWISE, IOPTGR( ) CAN BE CALCULATED AND WILL REMAIN
C        UNCHANGED FOR THIS PASS.  SOME ELEMENTS IF IOPTGR( ) 
C        HAVE BEEN SET ABOVE WHICH DO NOT CHANGE AT ALL.
C        WHEN IOPT(1) = 0, THERE IS NO SUBSET AREA.
C     
      IF(IOPT(1).NE.0)THEN 
         RATIO=FLOAT(MESHL)/MESH
         IOPTGR(2)=NINT((IOPT(2)-1)*RATIO)+1
         IOPTGR(3)=NINT((IOPT(3)-1)*RATIO)+1
         IOPTGR(4)=NINT((IOPT(4)-1)*RATIO)+1
         IOPTGR(5)=NINT((IOPT(5)-1)*RATIO)+1
      ENDIF
C
C        CALL ESP IF ERROR CHECKING IS DESIRED.  LTAG( ) IS SET.
C        INITIALIZE ER1Q, DEPENDING ON PASS.  THIS IS HARDWIRED
C        FOR NOW.
C
      IF(ER1(LP).EQ.0)GO TO 190
      ER1Q=ER1(LP)*FRACT(LP)
C
CD     WRITE(KFILDO,1742)LP,IVRBL,ER1(LP),ER1Q
CD1742 FORMAT(/' IN BCD5 AT 1742--LP,IVRBL,ER1(LP),ER1Q',2I4,2F8.3)
C
      CALL ESP5(KFILDO,IP14,IP17,IP21,JDATE,IVRBL,CCALL,NAME,DATA,
     1          XP,YP,TOSS,QUEST,LTAG,U,V,LNDSEA,XLAPSE,ELEV,NSTA,
     2          P,NX,NY,LP,NPASS,ER1(LP),ER1Q,MESH,MESHB,TITLE,JFIRST,
     3          SEALND,NXE,NYE,MESHE,R(LP)*RSTAR(LP),NOTOSS,N4P,
     4          IBKPN,ISTOP,I405ADG)
C
C        PACK AND WRITE THE VECTOR DATA IN TOSS( ) CONTAINING THE
C        OBSERVATIONS WITH ALL THOSE EXCEPT TOSSED AS 9999.
C        THE ID'S WRITTEN ARE THE SAME AS THE DATA BEING ANALYZED
C        EXCEPT THE 3RD WORD HAS THE PASS NUMBER.
C
      XMISSP=9999.0
      XMISSS=0.
C
      IF(KFILOV.EQ.0)GO TO 180
C        DON'T WRITE WHEN KFILOV = 0
      LD(1)=(MD(1)/1000000)*1000000+IDPARS(2)*1000+IDPARS(3)*100
     1                             +IDPARS(4)
C        MD(1) IS FROM ITABLE( ,2, ) AND REPRESENTS THE VARIABLE
C        BEING ANALYZED.  HOWEVER, THE UNITS MAY HAVE CHANGED
C        (E.G., SNOWFALL), SO RETAIN THE CCC, BUT USE FFFBDD FROM
C        IDPARS( ) REPRESENTING THE OUTPUT VARIABLE.
      LD(2)=LP*10000
C        LD(2) HOLDS THE PASS NUMBER.
      LD(3)=MD(3)+IDPARS(12)
      LD(4)=MD(4)
      CALL PRSID1(KFILDO,LD,LDPARS)
C        PRSID1 FILLS LDPARS( ) ACCORDING TO LD( ).
C
      IF(IP18.NE.0.AND.JP(3).NE.0)THEN
         WRITE(IP18,1745)LP
 1745    FORMAT(/' TOSSED OBSERVATIONS WRITTEN TO FILE KFILOV',
     1           ' FOR PASS NO.',I3,
     2           '.   ALL OTHERS ARE WRITTEN AS MISSING.')
      ENDIF
C
      CALL PACKV(KFILDO,KFILOV,LD,LDPARS,
     1           JP,ISCALD,ISCALE,
     2           IPLAIN,PLAIN,NDATE,JDATE(1),JDATE(2),JDATE(3),JDATE(4),
     3           CCALL,NCOUNT,TOSS,NSTA,NSTA,IPACK,ND5,MINPK,
     4           IS0,IS1,IS2,IS4,ND7,XMISSP,XMISSS,
     5           IP18,NWORDS,MTOTBY,MTOTRC,
     6           L3264B,L3264W,ISTOP,IER)
C        NCOUNT( ) IS WORK ARRAY FOR PACKV.  IP18 IS USED TO LIST VALUES.
C        ISCALD IS USED FOR GRIDPOINT DATA; IT IS ASSUMED IT IS OK
C        FOR THE VECTOR DATA.  THE SPECIFIC IDENTIFYING WORD IS IN
C        ID(2) OF THE FORM X0000, WHERE X IS THE PASS NUMBER.
C        IF THERE WAS AN ERROR, IT IS COUNTED BY ISTOP, BUT IS NOT
C        CONSIDERED FATAL.
C
      IF(IP16.NE.0)THEN
         WRITE(IP16,175)(LD(J),J=1,4),
     1                  ((IPLAIN(I,J),I=1,L3264W),J=1,4),NDATE
 175     FORMAT(/' WRITING DATA TO UNIT KFILOV',3I10.9,I10.3,3X,8A4,
     1           ' FOR DATE',I12)
      ENDIF
C
C        PACK AND WRITE THE VECTOR DATA IN QUEST( ) CONTAINING THE
C        OBSERVATIONS WITH ALL THOSE EXCEPT QUESTIONABLE AS 9999.
C        THE ID'S WRITTEN ARE THE SAME AS THE DATA BEING ANALYZED
C        EXCEPT THE 3RD WORD HAS THE PASS NUMBER PRECEDED BY "1".
C
      LD(1)=(MD(1)/1000000)*1000000+IDPARS(2)*1000+IDPARS(3)*100
     1                             +IDPARS(4)
C        MD(1) IS FROM ITABLE( ,2, ) AND REPRESENTS THE VARIABLE
C        BEING ANALYZED.  HOWEVER, THE UNITS MAY HAVE CHANGED
C        (E.G., SNOWFALL), SO RETAIN THE CCC, BUT USE FFFBDD FROM
C        IDPARS( ) REPRESENTING THE OUTPUT VARIABLE.
      LD(2)=LP*10000+100000
C        LD(2) HOLDS THE PASS NUMBER, PRECEDED BY A ONE.
      LD(3)=MD(3)+IDPARS(12)
      LD(4)=MD(4)
      CALL PRSID1(KFILDO,LD,LDPARS)
C        PRSID1 FILLS LDPARS( ) ACCORDING TO LD( ).
C
      IF(IP18.NE.0.AND.JP(3).NE.0)THEN
         WRITE(IP18,1755)LP
 1755    FORMAT(/' QUESTIONABLE OBSERVATIONS WRITTEN TO FILE KFILOV',
     1           ' FOR PASS NO.',I3,
     2           '.   ALL OTHERS ARE WRITTEN AS MISSING.')
      ENDIF
C
      CALL PACKV(KFILDO,KFILOV,LD,LDPARS,
     1           JP,ISCALD,ISCALE,
     2           IPLAIN,PLAIN,NDATE,JDATE(1),JDATE(2),JDATE(3),JDATE(4),
     3           CCALL,NCOUNT,QUEST,NSTA,NSTA,IPACK,ND5,MINPK,
     4           IS0,IS1,IS2,IS4,ND7,XMISSP,XMISSS,
     5           IP18,NWORDS,MTOTBY,MTOTRC,
     6           L3264B,L3264W,ISTOP,IER)
C        NCOUNT( ) IS WORK ARRAY FOR PACKV.  IP18 IS USED TO LIST VALUES.
C        ISCALD IS USED FOR GRIDPOINT DATA; IT IS ASSUMED IT IS OK
C        FOR THE VECTOR DATA.  THE SPECIFIC IDENTIFYING WORD IS IN
C        ID(2) OF THE FORM 1X0000, WHERE X IS THE PASS NUMBER.
C        IF THERE WAS AN ERROR, IT IS COUNTED BY ISTOP(1), BUT IS NOT
C        CONSIDERED FATAL.
C
      IF(IP16.NE.0)THEN
         WRITE(IP16,175)(LD(J),J=1,4),
     1                  ((IPLAIN(I,J),I=1,L3264W),J=1,4),NDATE
      ENDIF
C
 180  IF(LP.LT.NPASS)GO TO 190
C        WRITE ONLY ON THE LAST PASS.
      IF(KFILQC.EQ.0)GO TO 190
C        DON'T WRITE WHEN KFILQC = 0
C
C        IF ERROR CHECKING WAS DONE, COMBINE DATA( ) AND TOSS( ) TO
C        PRODUCE A QUALITY CONTROLLED ARRAY IN TOSS.  THIS ARRAY
C        WILL BE THE SAME AS DATA( ) EXCEPT THE TOSSED OBS IN
C        TOSS( ) WILL BE SET TO MISSING.  THIS ARRAY IS THEN WRITTEN
C        TO THE QC FILE.  IF ERROR CHECKING IS NOT DONE, WHICH WOULD
C        BE VERY UNUSUAL IN U405A, DATA ARE NOT WRITTEN, BECAUSE
C        THEY WOULD NOT BE QUALITY CONTROLLED SO THAT THE ID WOULD
C        APPLY.  NOTE THAT THE QC DATA WILL BE WRITTEN ONLY WHEN
C        ERROR CHECKING WAS DONE ON THE LAST PASS.
C
      DO 185 K=1,NSTA
C
      IF(DATA(K).NE.9999.)THEN
C
         IF(TOSS(K).EQ.9999.)THEN
            TOSS(K)=DATA(K)
         ELSE
            TOSS(K)=9999.
         ENDIF
C
      ENDIF
C
 185  CONTINUE      
C         
      LD(1)=(MD(1)/1000000)*1000000+IDPARS(2)*1000+IDPARS(3)*100
     1                             +IDPARS(4)
C        MD(1) IS FROM ITABLE( ,2, ) AND REPRESENTS THE VARIABLE
C        BEING ANALYZED.  HOWEVER, THE UNITS MAY HAVE CHANGED
C        (E.G., SNOWFALL), SO RETAIN THE CCC, BUT USE FFFBDD FROM
C        IDPARS( ) REPRESENTING THE OUTPUT VARIABLE.
C        NOTE THAT FOR THE MERGED LAMP/MOS OBS WITH CCCFFF = 202900,
C        THE QC DATA WILL BE WRITTEN CCCFFF = 202020 BECAUSE 020 IS
C        THE FFF OF THE VARIABLE BEING ANALYZED.  THAT MAKES THE
C        ID THE SAME AS THE MOS DATA.  BUT THE DD WILL REPRESENT
C        LAMP.
      LD(2)=MD(2)
      LD(3)=MD(3)+IDPARS(12)
      LD(4)=MD(4)
      CALL PRSID1(KFILDO,LD,LDPARS)
C        PRSID1 FILLS LDPARS( ) ACCORDING TO LD( ).
C
      IF(I405ADG.NE.0)THEN
         WRITE(KFILDO,186)(LD(J),J=1,4),PLAIN,NDATE
 186     FORMAT(/' WRITING QC DATA RECORD FOR ',3(1X,I9.9),
     1           1X,I10.3,'  ',A32,' FOR DATE',I12)
      ENDIF
C
      IF(IP18.NE.0.AND.JP(3).NE.0)THEN
         WRITE(IP18,1865)LP
 1865    FORMAT(/' QUALITY CONTROLLED OBSERVATIONS WRITTEN TO FILE',
     1           ' KFILQC FOR FINAL PASS NO.',I3,
     2           '.  TOSSED VALUES ARE WRITTEN AS MISSING.')
      ENDIF
C  
      CALL PACKV(KFILDO,KFILQC,LD,LDPARS,
     1           JP,ISCALD,ISCALE,
     2           IPLAIN,PLAIN,NDATE,JDATE(1),JDATE(2),JDATE(3),JDATE(4),
     3           CCALL,NCOUNT,TOSS,NSTA,NSTA,IPACK,ND5,MINPK,
     4           IS0,IS1,IS2,IS4,ND7,XMISSP,XMISSS,
     5           IP18,NWORDS,ITOTBY,ITOTRC,
     6           L3264B,L3264W,ISTOP,IER)
C        NCOUNT( ) IS WORK ARRAY FOR PACKV.  IP18 IS USED TO LIST
C        VALUES.  ISCALD IS USED FOR GRIDPOINT DATA; IT IS ASSUMED IT IS
C        OK FOR THE VECTOR DATA.  IF THERE IS AN ERROR, IT IS COUNTED
C        BY ISTOP(1), BUT IS NOT CONSIDERED FATAL.
C
      IF(IP16.NE.0)THEN
         WRITE(IP16,188)(LD(J),J=1,4),
     1                  ((IPLAIN(I,J),I=1,L3264W),J=1,4),NDATE
 188     FORMAT(/' WRITING DATA TO UNIT KFILQC',3I10.9,I10.3,3X,8A4,
     2           ' FOR DATE',I12)
      ENDIF
C      
C        MAKE THE CORRECTIONS IN A SUBROUTINE SO THAT DOUBLY
C        DIMENSIONED VARIABLES CAN BE USED.  NX AND NY ARE
C        CHANGED IN BCD5, AND ARE FURNISHED TO CORBC5.
C
CD     CALL TIMPR(KFILDO,KFILDO,'START CORBC5        ')
 190  CALL CORBC5(KFILDO,IP14,IP20,CCALL,DATA,XP,YP,XPL,YPL,LTAG,
     1            QUALST,XLAPSE,VRAD(1,LP),LNDSEA,ELEV,NSTA,
     2            ILS,LNDWAT(LP),WTWTL,WTLTW,
     3            IALGOR(LP),ELCORR(LP),IBKPN,BK(LP),ELCORU(LP),
     4            RWATO(LP),RWATI(LP),IVRAD,IALOC,ADIST,AELEV,ND13,
     5            P,CORR,COUNT,NCOUNT,FD2,NX,NY,MESH,MESHL,
     6            U,V,WNDWT(LP),WNDGRD,
     7            TELEV,SEALND,NXE,NYE,XPE,YPE,MESHE,ORIENT,
     8            HGTTHA,HGTTHB,NAREA,
     9            LP,NTYPE(LP),R(LP),IOPT,JDATE,TITLE,N4P,MGUESS,
     A            ISTOP,IER)
CD     CALL TIMPR(KFILDO,KFILDO,'END   CORBC5        ')
C        THERE IS NO NON-ZERO IER RETURN FROM CORBC5.
C
C        THE FOLLOWING DIAGNOSTIC PRINT PRODUCED ON IP18 INDICATES
C        FOR EACH DATUM THE DEGREE OF FIT OF THE UNSMOOTHED ANALYSIS
C        TO THE DATA OF THE PASS LP FOR THE WHOLE ANALYSIS AREA.
C        DIFFERENCES BETWEEN DATA VALUES AND INTERPOLATED VALUES
C        ARE NOT USED FOR THIS PURPOSE FOR POINTS OUTSIDE THE
C        NX BY NY GRID.
C
      IF(IP18.NE.0.OR.IP21.NE.0.OR.IOPT(1).NE.0)THEN
C           ANY TIME THERE IS A SUBSET AREA, THE DEGREE OF FIT IS
C           COMPUTED.
C
         IF(IP18.NE.0)THEN
            WRITE(IP18,258)(JDATE(J),J=1,4),LP,TITLE(1:16),MESH
 258        FORMAT(/' FOR DATE',I6,3I3.2,'  VALUES AT END OF BCD5 PASS',
     1           ' NO.',I3,' (UNSMOOTHED) FOR ',A16,
     2           ' FOR MESH LENGTH =',I4/
     3           ' ONLY STATIONS WITH LTAG = 0 OR -1 ARE PRINTED;',
     4           ' THOSE PERMANENTLY DISCARDED ARE NOT PRINTED.',
     5           '  BB IS THE ANALYSIS VALUE.'/
     6           ' STATIONS WITH NO DIFFERENCES ARE OUTSIDE THE GRID'/
     7           '  NO.  STATION      XPOS    YPOS      DATA',
     8           '        BB     DIF   LTAG')
         ENDIF
C
         SUM=0.
         NSUM=0
C           SUM AND NSUM ARE FOR SUMMING THE DIFFERENCES BETWEEN THE
C           INTERPOLATED VALUES AND THE OBSERVATIONS FOR ALL
C           OBSERVATIONS WITHIN THE GRID.
         SUMGR=0.
         NSUMGR=0
C           SUMGR AND NSUMGR ARE FOR SUMMING THE DIFFERENCES BETWEEN
C           THE INTERPOLATED VALUES AND THE OBSERVATIONS FOR ALL
C           OBSERVATIONS WITHIN THE SUBSETTED AREA ONLY.
         XNX=NX
         YNY=NY
C 
         DO 263 K=1,NSTA
         IF(LTAG(K).GE.1)GO TO 263
C
C           FIND INTERPOLATED VALUE OR NEAREST NEIGHBOR VALUE IN
C           ITRPSL ACCORDING TO THE LAND/WATER TYPE LNDSEA(K).
C
C    GAW: COMMENTED OUT CALL TO ITRPSX, CHANGED TO ITRPSL, TO TEST
C    DIFFERENCES BETWEEN CONUS U155 VERSIONS.  WILL CHANGE BACK.
C         CALL ITRPSX(KFILDO,IP14,P,NX,NY,
C     1               CCALL(K),DATA(K),XLAPSE(K),ELEV(K),XP(K),YP(K),
C     2               LNDSEA(K),SEALND,TELEV,NXE,NYE,
C     3               IBKPN,ELCORR(LP),ELCORU(LP),
C     4               MESH,MESHE,N4P,BB,ISTOP,IER)
C
      CALL ITRPSL(KFILDO,IP14,P,NX,NY,CCALL(K),XP(K),YP(K),
     1            LNDSEA(K),SEALND,NXE,NYE,
     2            MESH,MESHE,N4P,BB,ISTOP,IER)
C
C           VALUE INTERPOLATED FROM CURRENT ANALYSIS OR FIRST
C           GUESS TO LOCATION OF STATION IS NOW IN BB.  THIS CAN BE
C           MISSING BECAUSE AN INTERPOLATED VALUE FOR A LAND (WATER)
C           STATION IS ONLY TAKEN FROM LAND (WATER) STATIONS, AND IT IS
C           POSSIBLE NONE EXIST.  ALSO, THE FIRST GUESS ANALYSIS AREA
C           MAY NOT FILL GRID. IN THIS CASE, IER NE 0.
         IF(IER.NE.0)GO TO 261
C           IER NE 0 IS NOT COUNTED AS AN ERROR HERE.
         IF(XP(K).LT.1..OR.XP(K).GT.XNX)GO TO 261
         IF(YP(K).LT.1..OR.YP(K).GT.YNY)GO TO 261
         DIF=BB-DATA(K)
         IF(LTAG(K).EQ.-1)GO TO 259
         SUM=SUM+ABS(DIF)
         NSUM=NSUM+1
C
C           WHEN IOPT(1) = 0, THERE IS NO SUBSET AREA, SO DO NO
C           CALCULATIONS FOR IT.
C            
         IF(IOPT(1).NE.0)THEN
C
            IF(XP(K).LE.IOPTGR(3).AND.
     1         XP(K).GE.IOPTGR(2).AND.
     2         YP(K).LE.IOPTGR(5).AND.
     3         YP(K).GE.IOPTGR(4))THEN
               NSUMGR=NSUMGR+1
               SUMGR=SUMGR+ABS(DIF)
            ENDIF
C
         ENDIF
C
C***         IF(IP20.NE.0.AND.IOPT(1).NE.0)THEN
C***D              WRITE(KFILDO,2588)K,CCALL(K),XP(K),YP(K),
C***D    1                           IOPTGR(3),IOPTGR(2),
C***D    2                           IOPTGR(5),IOPTGR(4),NSUMGR,SUMGR
C***D2588          FORMAT(' IN BCD5 AT 2588--K,CCALL(K),XP(K),YP(K),',
C***D    1                'IOPTGR(3,2,5,4),',
C***D    2                'NSUMGR,SUMGR',I5,1X,A6,2F7.2,4F6.0,I5,F8.2)
C
C***         ENDIF
C
 259     IF(IP18.NE.0)THEN
            WRITE(IP18,260)K,CCALL(K),XP(K),YP(K),DATA(K),BB,DIF,LTAG(K)
 260        FORMAT(' ',I5,3X,A8,F8.2,F8.2,F10.3,F10.3,F8.3,I6)
            GO TO 263
         ENDIF
C
 261     IF(IP18.NE.0)THEN
            WRITE(IP18,262)K,CCALL(K),XP(K),YP(K),DATA(K),BB,LTAG(K)
 262        FORMAT(' ',I5,3X,A8,F8.2,F8.2,F10.3,F10.3,8X,I6)
         ENDIF
C
 263     CONTINUE
C
         AVG=9999.
         IF(NSUM.NE.0)AVG=SUM/NSUM
C
         IF(IP21.NE.0)THEN
            WRITE(IP21,264)(JDATE(J),J=1,4),NSUM,LP,TITLE(1:16),AVG
 264        FORMAT(/' FOR DATE',I6,3I3.2,'  MEAN ABS DIFF OF',I5,
     1              ' VALUES USED WITHIN THE GRID',
     2              ' ON PASS NO.',I2,' FOR ',A16,' =',F7.3,
     3              ' (UNSMOOTHED)')
         ENDIF
C
C           COMPUTE AND PRINT THE AVERAGE DIFFERENCE BETWEEN THE DATA
C           AND THE CURRENT ANALYSIS OVER THE SUBSETTED AREA.
C
         IF(IOPT(1).NE.0)THEN
C              THERE IS A SUBSET AREA.
C
            IF(NSUMGR.EQ.0)THEN
               AVGGR=9999.
            ELSE
               AVGGR=SUMGR/NSUMGR
            ENDIF
C*******************
            IF(I405ADG.NE.0)THEN
               WRITE(KFILDO,2644)
            ENDIF
C
            IF(IP20.NE.KFILDO.AND.IP20.NE.0)THEN        
               WRITE(IP20,2644)
 2644          FORMAT(' ')
            ENDIF
C
            IF(IP20.NE.0)THEN
               WRITE(IP20,2645)
     1                 (JDATE(J),J=1,4),NSUMGR,LP,TITLE(1:16),AVGGR
 2645          FORMAT(' FOR DATE',I6,3I3.2,'  MEAN ABS DIFF OF',I5,
     1                ' VALUES USED IN SUBSET AREA ',
     2                ' ON PASS NO.',I2,' FOR ',A16,' =',F7.3,
     3                ' (UNSMOOTHED)')
            ENDIF
C
            IF(I405ADG.NE.0)THEN
C
               IF(IP20.NE.KFILDO)THEN
                  WRITE(KFILDO,2645)
     1                   (JDATE(J),J=1,4),NSUMGR,LP,TITLE(1:16),AVGGR
               ENDIF
C
            ENDIF
C
         ELSE
C              THERE IS NOT A SUBSET AREA.
            AVGGR=AVG
            NSUMGR=NSUM
C              AVGGR AND NSUMGR ARE PRINTED AT THE BOTTOM OF THE
C              GRIDPRINTED MAP.  IF THERE IS NO SUBSETTED AREA, THE
C              WHOLE AREA MAY BE PRINTED.  THEREFORE, SET AVGGR TO
C              AVG AND NSUMGR TO NSUM.
         ENDIF
C
      ENDIF
C
C        PREPARE UNSMOOTHED MAPS FOR GRIDPRINTING AND OR
C        WRITING IN TDLPACK, IF DESIRED.
C
      IF(NPRT(LP).NE.0.AND.IP22.NE.0.AND.JP(1).NE.0)GO TO 2646
      IF(NTDL(LP).NE.0.AND.KFILOG.NE.0.AND.JP(2).NE.0)GO TO 2646
      GO TO 271
C        TO PROCEED BELOW, EITHER NPRT(LP) AND JP(1)
C        INDICATE GRIDPRINTING OR NTDL(LP) AND JP(2) INDICATE
C        PACKING AND THERE IS A NONZERO UNIT NUMBER TO WRITE TO.
C
C        WHEN DESIRED, SET EACH STATION VALUE TO THE NEAREST GRIDPOINT,
C        PROVIDED A CLOSER STATION TO THE GRIDPOINT DOES NOT EXIST.
C        THIS IS DONE HERE IF IT IS (1) THE LAST PASS, (2) SMOOTHING
C        IS NOT TO BE DONE, AND (3) ISETP NE 0.
C
 2646 IF(LP.EQ.NPASS.AND.B(LP).EQ.0..AND.ISETP.NE.0)THEN
         CALL SETPNT(KFILDO,DATA,LTAG,XP,YP,NSTA,P,FD2,NX,NY,ISETP,
     1               IER)
C           THERE IS CURRENTLY NO NON ZERO IER RETURN.
      ENDIF
C
      NXG=NX
      NYG=NY
      MESHG=MESH
C        NXG, ETC. ARE NECESSARY BECAUSE SZGRDM CHANGE THEM, AND
C        NX, ETC. MUST BE RETAINED.
      CALL TRNSFR(P,FD2,NX*NY)
CD     WRITE(KFILDO,2647)NX,NY
CD2647 FORMAT(/' CALLING SZGRDM AT 2647--NX,NY',2I6)
      CALL SZGRDM(KFILDO,FD2,NXG,NYG,
     1            MESHG,MESHL,ITRPLQ(LP),ND2X3)
C        SZGIRD PUTS THE GRID IN FD2( ) AT SUBSET MESH LENGTH MESHL.
C        IOPT( ) IS IN RELATION TO THAT MESH LENGTH.
C
C        GRIDPRINT UNSMOOTHED FIELD IF DESIRED.
C
      IF(NPRT(LP).EQ.0.OR.IP22.EQ.0.OR.JP(1).EQ.0)GO TO 2654
      TITLE(17:24)=BLANK(1:8)
      CALL PRTGR(IP22,FD2,NXG,NYG,
     1           CINT(LP),ORIGIN(LP),SMULT(LP),SADD(LP),IOPT,TITLE,IER)
      IF(IER.NE.0)ISTOP(1)=ISTOP(1)+1
C        AN ERROR IS COUNTED BY ISTOP(1), BUT IS NOT FATAL.
C
C        AT THE BOTTOM OF THE MAP, BELOW THE TITLE, PRINT
C        THE PASS NUMBER LP, NTYPE( ), R( ), B( ), AND ER1( ).
C        IT IS EXPECTED THAT 4 PASSES WILL USUALLY BE USED
C        AND ALL THOSE VALUES ARE PRINTED.  HOWEVER, IF NPASS
C        IS GT 4, IT IS ACCOMMODATED.  THE NUMBER OF VALUES WITHIN
C        THE GRIDPRINTED AREA AND THE AVERAGE ABSOLUTE DIFFERENCE
C        ARE ALSO PRINTED.
C
      IF(NPASS.LE.4)THEN
         WRITE(IP22,2648)LP,(NTYPE(J),J=1,4),(R(J),J=1,4),
     1                   (MSHPAS(J),J=1,4),NSMTYP,(B(J),J=1,4),
     2                   (ER1(J),J=1,4),
     3                   (ITRPLQ(J),J=1,4),NSUMGR,AVGGR
 2648    FORMAT('       PASS NO.',I2,'    NTYPE',4I4,
     1          '       R',F7.1,3F5.1,
     2          '     MSHPAS',4I5,5X,'NSMTYP',I5/
     3          19X,'  B',F8.1,3F4.1,'       ER1',4F5.1,
     4          '     ITRPLQ',4I5,
     5          '     VALUES',I5,'   FIT',F6.2)
C           REMOVED THE "CTRL-L" AND "/" IN THE FORMAT 2648.
C
         IF(IVRBL.EQ.1)THEN
            WRITE(IP22,2649)(WNDWT(J),J=1,4),WNDTHR,WNDTRN,WNDGRD
 2649       FORMAT(21X,'WNDWT',4F4.1,'       WNDTHR',F8.1,
     1             14X,'WNDTRN',F8.2,17X,'WNDGRD',F6.2)
         ENDIF
C
      ELSEIF(NPASS.EQ.5)THEN
         WRITE(IP22,265)LP,(NTYPE(J),J=1,5),(R(J),J=1,5),
     1                   (MSHPAS(J),J=1,5),NSMTYP,(B(J),J=1,5),
     2                   (ER1(J),J=1,5),
     3                   (ITRPLQ(J),J=1,5),NSUMGR
 265     FORMAT('       PASS NO.',I2,'    NTYPE',5I4,
     1          '     R  ',5F5.1,
     2          '     MSHPAS',5I5,'     NSMTYP',I5/
     3          19X,'  B    ',5F4.1,'     ER1',5F5.1,
     4          '     ITRPLQ',5I5,'     VALUES',I5)
C
         IF(IVRBL.EQ.1)THEN
            WRITE(IP22,2650)(WNDWT(J),J=1,5),WNDTHR,WNDTRN,WNDGRD,AVGGR
 2650       FORMAT(21X,'WNDWT',5F4.1,'     WNDTHR',F8.1,
     1             19X,'WNDTRN',F8.2,'     WNDGRD',F6.2,'     FIT',F8.2)
         ENDIF
C
         WRITE(IP22,2651)CINT(LP),ORIGIN(LP),SMULT(LP),SADD(LP)
 2651    FORMAT(21X,'CINT',F8.2,16X,'  ORIGIN',F8.2,
     1          19X,'SMULT ',F8.2,17X,'     SADD',F7.2)
      ELSE
C
         WRITE(IP22,2652)LP,(NTYPE(J),J=1,6),(R(J),J=1,6),
     1                   (MSHPAS(J),J=1,6),(B(J),J=1,6),(ER1(J),J=1,6),
     2                   (ITRPLQ(J),J=1,6),NSUMGR,AVGGR
 2652    FORMAT('       PASS NO.',I2,'    NTYPE',6I4,'      R',6F5.1,
     1          '     MSHPAS',6I5/
     2          10X,'  B',6F4.1,'    ER1',6F5.1,'     ITRPLQ',I5,5I2,
     3          '     VALUES',I5,'   FIT',F6.2)
      ENDIF
C
      IF(NPASS.NE.5)THEN
         WRITE(IP22,2653)CINT(LP),ORIGIN(LP),SMULT(LP),SADD(LP)
      ENDIF
C
 2653 FORMAT(21X,'CINT',F8.2,16X,'ORIGIN',F8.2,
     1       14X,'SMULT ',F8.2,17X,'SADD',F8.2)
C
C        TDLPACK AND WRITE UNSMOOTHED FIELD IF DESIRED FOR THE
C        SUBSET AREA.  WHEN IOPT(1) = 0, THERE IS NO SUBSET AREA.
C
 2654 IF(NTDL(LP).EQ.0.OR.KFILOG.EQ.0.OR.JP(2).EQ.0.
     1                                OR.IOPT(1).EQ.0)GO TO 271
      LD(1)=ID(1)
      LD(2)=LP*10000+IDPARS(7)
C        THE LLLL IN ID(2) IS USED FOR THE PASS NUMBER.
C        IDPARS(7) MAINTAINS THE LEVEL.
      LD(3)=ID(3)
      LD(4)=(ID(4)/1000)*1000
C        THIS IS THE UNSMOOTHED ANALYSIS; S IN ID(4) = 0.  
      ITAUH=IDPARS(12)
      ITAUM=0
      NSEQ=0
      NCHAR=32
C        32 CHARACTERS OF PLAIN LANGUAGE ARE PACKED.
      XMISSP=0.
      XMISSS=0.
C        THESE ARE ANALYSES AND NO MISSING VALUES ARE PROVIDED FOR.
C        IF THERE EVER ARE, JUST SET XMISSP=9999., OR WHATEVER THE
C        MISSING VALUE IS.
C
C        THE GRID IN FD2( ) IS ALWAYS AT MESH LENGTH MESHL AS A
C        RESULT OF SZGRDM, WHICH IS WHAT IS WANTED FOR THE
C        DISPOSABLE GRID.  NOW CUT THE GRID TO THE DISPOSABLE
C        AREA.  IOPT( ) IS IN RELATION TO MESHL, WHICH IS THE 
C        MESH LENGTH OF FD2( ).
C
      NXD=IOPT(3)-IOPT(2)+1
      NYD=IOPT(5)-IOPT(4)+1
      NXOFF=IOPT(2)-1
      NYOFF=IOPT(4)-1
C        NXOFF AND NYOFF ARE THE DIFFERENCES IN THE (1,1) POINT
C        OF THE ANALYSIS AND SUBSETTED GRIDS AT THE CURRENT MESH
C        LENGTH MESH.  THERE IS NO NEED TO CALL CUTIT IF THE INPUT
C        AND OUTPUT GRIDS ARE THE SAME.  NXG AND NYG ARE THE
C        DIMENSIONS OF THE GRID IN FD2( ) AT MESH LENGTH MESHL.
C
      IF(NXOFF.NE.0.OR.NYOFF.NE.0.OR.NXG.NE.NXD.OR.NYG.NE.NYD)THEN
         CALL CUTIT(KFILDO,FD2,NXG,NYG,NXOFF,NYOFF,
     1              FD2,NXD,NYD,IER)
      ENDIF
C
      IF(IER.NE.0)THEN
         ISTOP(1)=ISTOP(1)+1
         WRITE(KFILDO,2655)(LD(J),J=1,4),PLAIN,NDATE
 2655    FORMAT('     NOT WRITING UNSMOOTHED ANALYSIS ',3(1X,I9.9),
     1           1X,I10.3,'  ',A32,' FOR DATE',I12,/,
     2          '     TO UNIT NO. KFILOG.')
         GO TO 271
      ENDIF
C
C        FIND THE ACTUAL MESH LENGTH XMESHL FROM THE NOMINAL
C        MESH LENGTH MESHL.
C
      CALL ACTUAL(KFILDO,MESHL,XMESHL,TRASH,NPROJ,IER)
C        XMESHL IS THE ACTUAL MESH LENGTH IN KM.         
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,2656)IER
 2656    FORMAT(/'     FATAL ERROR IN ACTUAL FROM BCD5,',
     1           ' IER =',I4)
         ISTOP(1)=ISTOP(1)+1
         IER=777
         GO TO 400
      ENDIF
C
C        FIND THE LAT/LONG ALATD/ALOND OF THE LOWER LEFT CORNER
C        OF THE DISPOSABLE GRID.  THIS IS GRIDPOINT IOPT(2), IOPT(4)
C        ON THE FD2 GRID AT MESH LENGTH MESHL.  THE MESH LENGTH MUST
C        BE IN METERS FOR THE CALLED ROUTINES.
C
      IF(NPROJ.EQ.3)THEN
         CALL LMIJLL(KFILDO,FLOAT(IOPT(2)),FLOAT(IOPT(4)),
     1               XMESHL*1000.,ORIENT,XLAT,
     2               ALATL,ALONL,ALATD,ALOND,IER)
C
            IF(IER.NE.0)THEN
               WRITE(KFILDO,2657)IER
 2657          FORMAT(/'     FATAL ERROR IN LMIJLL FROM BCD5 AT 2750,',
     1                 ' IER =',I4)
               ISTOP(1)=ISTOP(1)+1
               IER=777
               GO TO 400
            ENDIF
C
      ELSEIF(NPROJ.EQ.5)THEN
         CALL PSIJLL(KFILDO,FLOAT(IOPT(2)),FLOAT(IOPT(4)),
     1               XMESHL*1000.,ORIENT,XLAT,
     2               ALATL,ALONL,ALATD,ALOND)
      ELSEIF(NPROJ.EQ.7)THEN
         CALL MCIJLL(KFILDO,FLOAT(IOPT(2)),FLOAT(IOPT(4)),
     1               XMESHL*1000.,XLAT,
     2               ALATL,ALONL,ALATD,ALOND)
      ELSE
         WRITE(KFILDO,2658)NPROJ
 2658    FORMAT(/' ****MAP PROJECTION NUMBER NPROJ =',I3,
     1           ' NOT 3, 5, OR 7.  FATAL ERROR IN BCD5 AT 2658.')
         ISTOP(1)=ISTOP(1)+1
         IER=777
         GO TO 400
      ENDIF
C
CD     WRITE(KFILDO,2659)IOPT(2),IOPT(3),IOPT(4),IOPT(5),NXOFF,NYOFF,
CD    1                  NXG,NYG,NXD,NYD,MESH,MESHL,
CD    2                  ALATL,ALONL,ALATD,ALOND
CD2659 FORMAT(/,' AT 2659--IOPT(2),IOPT(3),IOPT(4),IOPT(5),NXOFF,NYOFF,',
CD    1                 'NXG,NYG,NXD,NYD,MESH,MESHL,',
CD    2                 'ALATL,ALONL,ALATD,ALOND',/,12I7,4F9.4)
C
C        ALATD AND ALOND ARE THE LL LAT/LON GRID POSITION.
C        TRUNCATE TO THREE DECIMAL PLACES TO ASSURE
C        COMPATIBILITY WITH AVN ARCHIVE AND OTHER PROGRAMS.
C
      ALATD=NINT(ALATD*1000)/1000.
      ALOND=NINT(ALOND*1000)/1000.
C
C        PAWGTS PACKS AND WRITES THE UNSMOOTHED DATA TO A 
C        GRIDPOINT FILE.  BUT FIRST POSTPROCESS IF DESIRED.
C        THE OUTPUT GRIDS FROM BCD5 ARE AT THE CURRENT MESH LENGTH
C        MESH.  WHEN A GRID HAS BEEN CLIPPED TO A LARGER MESH 
C        LENGTH IN FSTGS5 THAN MESH, THE AREA COVERED WITH
C        NON-MISSING DATA MAY BE SLIGHTLY GRATER THAN THE DESIRED
C        AREA AT MESH LENGTH MESH.              
C
      CALL TRNSFR(FD2,COUNT,NXD*NYD)
C        DATA IN FD2( ) ARE TRANSFERRED TO COUNT( ) SO THIS
C        DISPOSABLE OUTPUT POSTPROCESSING DOES NOT MODIFY THE
C        DATA.
C
C        POSTPROCESS IF NEEDED.  THREE ROUTINES ARE AVAILABLE.
C
      DO 2705 NN=1,3
C
      IF(POSTDS(NN).NE.'      ')THEN
C
         IF(POSTDS(NN).EQ.'POST  ')THEN           
            CALL POST(KFILDO,COUNT,NXD*NYD,
     1                TLOD(NN),SETLOD(NN),THID(NN),SETHID(NN),
     2                CONSTD(NN),NSCALD(NN),EX1D(NN),EX2D(NN),IER)
C              IF A POSTPROCESSING ROUTINE OTHER THAN POST IS
C              NEEDED, PUT CHECK AND CALL HERE.
C
         ELSEIF(POSTDS(NN).EQ.'CIGFT ')THEN           
            CALL CIGFT(KFILDO,COUNT,NXD*NYD,
     1                 TLOD(NN),SETLOD(NN),THID(NN),SETHID(NN),
     2                 CONSTD(NN),NSCALD(NN),EX1D(NN),EX2D(NN),IER)
C              THIS IS TO CHANGE LAMP CEILING HEIGHT IN CATEGORIES
C              TO HUNDREDS OF FT.
C
         ELSEIF(POSTDS(NN).EQ.'VISMI ')THEN           
            CALL VISMI(KFILDO,COUNT,NXD*NYD,
     1                 TLOD(NN),SETLOD(NN),THID(NN),SETHID(NN),
     2                 CONSTD(NN),NSCALD(NN),EX1D(NN),EX2D(NN),IER)
C              THIS IS TO CHANGE LAMP VISIBILITY IN CATEGORIES
C              TO MILES.
         ELSE
            WRITE(KFILDO,270)POSTDS(NN)
 270        FORMAT(/' ****POSTPROCESSING ROUTINE SPECIFIED',
     1              ' IN U405A.CN FILE = ',A6,' FOR DISPOSABLE',
     2              ' GRIDS NOT AVAILABLE IN BCD5.  PROCEEDING.')
            ISTOP(1)=ISTOP(1)+1
         ENDIF
C
      ENDIF
C
 2705 CONTINUE
C
      CALL PAWGTS(KFILDO,KFILOG,'KFILOG',IP16,NDATE,
     1            LD,ITAUH,ITAUM,MODNO,NSEQ,ISCALD,
     2            NPROJ,ALATD,ALOND,ORIENT,MESHL,XLAT,NXD,NYD,
     3            COUNT,NCOUNT,IWORK,IPACK,ND5,MINPK,
     4            IS0,IS1,IS2,IS4,ND7,
     5            IPLAIN,PLAIN,NCHAR,
     6            XMISSP,XMISSS,LX,IOCTET,
     7            JTOTBY,JTOTRC,L3264B,L3264W,IER)
C
      IF(IER.NE.0)THEN
         ISTOP(1)=ISTOP(1)+1
C           AN ERROR IN PAWGTS IS NOT CONSIDERED FATAL.
      ENDIF
C
C        THIS IS THE END OF THE DISPOSABLE AND GRIDPRINT OUTPUT
C        FOR UNSMOOTHED GRIDS.
C
C        SMOOTH FIELD IF DESIRED.
C
 271  IF(B(LP).EQ.0.)GO TO 2799
C
      IF(NSMTYP.EQ.1)THEN
         CALL SMOTH(P,CORR,NX,NY,B(LP))
C
      ELSEIF(NSMTYP.EQ.2)THEN
         CALL SMOTHN(P,CORR,NX,NY,B(LP),NCOUNT)
C
      ELSEIF(NSMTYP.EQ.3)THEN
C
         IF(LP.EQ.NPASS)THEN
            CALL SMTH9(KFILDO,P,CORR,NX,NY)
         ELSE
            CALL SMOTHN(P,CORR,NX,NY,B(LP),NCOUNT)
         ENDIF
C
      ELSEIF(NSMTYP.EQ.4)THEN
C
         IF(LP.GE.4.AND.IVRBL.EQ.1)THEN
            CALL SMOTHC(KFILDO,P,CORR,FD2,NX,NY,B(LP),NCOUNT,MESH)
C
C              PAWGTS PACKS AND WRITES THE SMOOTHING ARRAY FD2 
C              TO A GRIDPOINT FILE.
C
CD           IF(KFILOG.NE.0.AND.NTDL(LP).NE.0.AND.JP(2).NE.0)THEN
C                 NXD ETC. HAVE TO BE COMPUTED ABOVE FOR THIS TO WORK.
CD              LD(1)=099000005
CD              LD(2)=LP*10000
C                 THE LLLL IN ID(2) IS USED FOR THE PASS NUMBER.
CD              LD(3)=0
CD              LD(4)=0
CD              ITAUH=0
CD              ITAUM=0
CD              NSEQ=0
CD              NCHAR=32
C                 32 CHARACTERS OF PLAIN LANGUAGE ARE PACKED.
CD              XMISSP=9999.
CD              XMISSS=0.
CD              IZERO=0
C
C                 ASSUME FOR THIS TESTING THAT THE LOOP ABOVE TO SET
C                 ALATD, ETC. HAS BEEN EXECUTED.  THIS GRID IS NOT
C                 CUT AND SHOULD BE USED WHEN THE DISPOSABLE GRID IS
C                 OF THE FULL SIZE.              
C
CD              CALL PAWGTS(KFILDO,KFILOG,'KFILOG',IP16,NDATE,
CD    1                     LD,ITAUH,ITAUM,MODNO,NSEQ,IZERO,
CD    2                     NPROJ,ALATD,ALOND,ORIENT,MESHL,XLAT,NXD,NYD,
CD    3                     FD2,NCOUNT,IWORK,IPACK,ND5,MINPK,
CD    4                     IS0,IS1,IS2,IS4,ND7,
CD    5                     IPLAIN,PLAIN,NCHAR,
CD    6                     XMISSP,XMISSS,LX,IOCTET,
CD    7                     JTOTBY,JTOTRC,L3264B,L3264W,IER)
C
CD              IF(IER.NE.0)THEN
CD                 ISTOP(1)=ISTOP(1)+1
C                    AN ERROR IN PAWGTS IS NOT CONSIDERED FATAL.
CD              ENDIF
C
CD           ENDIF
C
         ELSE
            CALL SMOTHN(P,CORR,NX,NY,B(LP),NCOUNT)
         ENDIF
C
      ELSEIF(NSMTYP.GE.5.AND.NSMTYP.LE.7)THEN
         RMESH=FLOAT(MESH)/FLOAT(MESHE)
C           RMESH IS THE RATIO OF THE MESH LENGTH OF THE ANALYSIS GRID
C           TO THE TERRAIN GRID.
         CALL SMOTHG(KFILDO,P,CORR,COUNT,NX,NY,B(LP),CSTSM,
     1               TELEV,SEALND,NXE,NYE,
     2               RMESH,NSMTYP,NSHLN,IER)
C           THIS IS A SPECIALIZED SMOOTHER FOR GRIDDED MOS.
      ELSE
C
         WRITE(KFILDO,2754)NSMTYP
 2754    FORMAT(/' ****NSMTYP =',I4,' NOT A CORRECT VALUE OF',
     1           ' 1, 2, 3, ,4, 5, 6, OR 7 (ZERO DEFAULTS TO 1).'/
     2           '     NO SMOOTHING WILL BE DONE FOR THIS VARIABLE.')
         ISTOP(1)=ISTOP(1)+1 
         GO TO 2799
      ENDIF   
C
C        THE FOLLOWING DIAGNOSTIC PRINT PRODUCED ON IP19 INDICATES
C        FOR EACH DATUM THE DEGREE OF FIT OF THE SMOOTHED ANALYSIS
C        TO THE DATA OF THE PASS LP.  DIFFERENCES BETWEEN DATA VALUES
C        AND INTERPOLATED VALUES ARE NOT USED FOR THIS PURPOSE FOR 
C        POINTS OUTSIDE THE NX BY NY GRID.
C
      IF(IP19.NE.0.OR.IP21.NE.0.OR.IOPT(1).NE.0)THEN
C           ANY TIME THERE IS A SUBSET AREA, THE DEGREE OF FIT IS
C           COMPUTED.
C
         IF(IP19.NE.0)THEN
            WRITE(IP19,2755)(JDATE(J),J=1,4),LP,TITLE(1:16),MESH
 2755       FORMAT(/' FOR DATE',I6,3I3.2,'  VALUES AT END OF BCD5 PASS',
     1           ' NO.',I3,' (SMOOTHED) FOR ',A16,
     2           ' FOR MESH LENGTH =',I4/
     3           ' ONLY STATIONS WITH LTAG = 0 OR -1 ARE PRINTED;',
     4           ' THOSE PERMANENTLY DISCARDED ARE NOT PRINTED.',
     5           '  BB IS THE ANALYSIS VALUE.'/
     6           ' STATIONS WITH NO DIFFERENCES ARE OUTSIDE THE GRID'/
     7           '  NO.  STATION      XPOS    YPOS      DATA',
     8           '        BB     DIF   LTAG')
         ENDIF
C
         SUM=0.
         NSUM=0
C           SUM AND NSUM ARE FOR SUMMING THE DIFFERENCES BETWEEN THE
C           INTERPOLATED VALUES AND THE OBSERVATIONS FOR ALL
C           OBSERVATIONS WITHIN THE GRID.
         SUMGR=0.
         NSUMGR=0
C              SUMGR AND NSUMGR ARE FOR SUMMING THE DIFFERENCES BETWEEN
C              THE INTERPOLATED VALUES AND THE OBSERVATIONS FOR ALL
C              OBSERVATIONS WITHIN THE SUBSETTED AREA ONLY.
         XNX=NX
         YNY=NY
C 
         DO 278 K=1,NSTA
         IF(LTAG(K).GE.1)GO TO 278
C
C           FIND INTERPOLATED VALUE OR NEAREST NEIGHBOR VALUE IN
C           ITRPSL ACCORDING TO THE LAND/WATER TYPE LNDSEA(K).
C
C    GAW: COMMENTED OUT CALL TO ITRPSX AND REPLACED WITH ITRPSL, TO
C    TEST DIFFERENCES BETWEEN CONUS U155 VERSIONS.  WILL CHANGE BACK.
C         CALL ITRPSX(KFILDO,IP14,P,NX,NY,
C     1               CCALL(K),DATA(K),XLAPSE(K),ELEV(K),XP(K),YP(K),
C     2               LNDSEA(K),SEALND,TELEV,NXE,NYE,
C     3               IBKPN,ELCORR(LP),ELCORU(LP),
C     4               MESH,MESHE,N4P,BB,ISTOP,IER)
C
      CALL ITRPSL(KFILDO,IP14,P,NX,NY,CCALL(K),XP(K),YP(K),
     1            LNDSEA(K),SEALND,NXE,NYE,
     2            MESH,MESHE,N4P,BB,ISTOP,IER)
C
C           VALUE INTERPOLATED FROM CURRENT ANALYSIS OR FIRST
C           GUESS TO LOCATION OF STATION IS NOW IN BB.  THIS CAN BE
C           MISSING BECAUSE AN INTERPOLATED VALUE FOR A LAND (WATER)
C           STATION IS ONLY TAKEN FROM LAND (WATER) STATIONS, AND IT IS
C           POSSIBLE NONE EXIST.  ALSO, THE FIRST GUESS ANALYSIS AREA
C           MAY NOT FILL GRID.  IN THIS CASE, IER NE 0.
C
         IF(IER.NE.0)THEN
            IF(IER.EQ.195)ISTOP(1)=ISTOP(1)+1
C              A **** IER = 195 ERROR IN ITRPSL MEANS AN ERROR IN THE
C              STATION TABLE CONCERNING LAND/WATER POINT DESIGNATIONS.
C              A #### IER = 196 ERROR MEANS GRID POINTS OF NECESSARY
C              TYPE ARE NOT AVAILABLE, AND IS NOT COUNTED AS AN ERROR.
            GO TO 277
         ENDIF
C
         IF(XP(K).LT.1..OR.XP(K).GT.XNX)GO TO 277
         IF(YP(K).LT.1..OR.YP(K).GT.YNY)GO TO 277
         DIF=BB-DATA(K)
         IF(LTAG(K).EQ.-1)GO TO 276
         SUM=SUM+ABS(DIF)
         NSUM=NSUM+1
C
C           WHEN IOPT(1) = 0, THERE IS NO SUBSET AREA, SO DO NO
C           CALCULATIONS FOR IT.
C
         IF(IOPT(1).NE.0)THEN
C
            IF(XP(K).LE.IOPTGR(3).AND.
     1         XP(K).GE.IOPTGR(2).AND.
     2         YP(K).LE.IOPTGR(5).AND.
     3         YP(K).GE.IOPTGR(4))THEN
               NSUMGR=NSUMGR+1
               SUMGR=SUMGR+ABS(DIF)
            ENDIF
C
         ENDIF
C
C***D              WRITE(KFILDO,2758)K,CCALL(K),XP(K),YP(K),
C***D    1                           IOPTGR(3),IOPTGR(2),
C***D    2                           IOPTGR(5),IOPTGR(4),NSUMGR,SUMGR
C***D2758          FORMAT(' IN BCD5 AT 2758--K,CCALL(K),XP(K),YP(K),',
C***D    1                'IOPTGR(3,2,5,4),',
C***D    2                'NSUMGR,SUMGR',I5,1X,A6,2F7.2,4F6.0,I5,F8.2)
C
 276     IF(IP19.NE.0)THEN
            WRITE(IP19,260)K,CCALL(K),XP(K),YP(K),DATA(K),BB,DIF,LTAG(K)
            GO TO 278
         ENDIF
C
 277     IF(IP19.NE.0)THEN
            WRITE(IP19,262)K,CCALL(K),XP(K),YP(K),DATA(K),BB,LTAG(K)
         ENDIF
C
 278     CONTINUE
C
         AVG=9999.
         IF(NSUM.NE.0)AVG=SUM/NSUM
C         
         IF(IP21.NE.0)THEN
            WRITE(IP21,2785)(JDATE(J),J=1,4),NSUM,LP,TITLE(1:16),AVG
 2785       FORMAT(' FOR DATE',I6,3I3.2,'  MEAN ABS DIFF OF',I5,
     1             ' VALUES USED WITHIN THE GRID',
     2             ' ON PASS NO.',I2,' FOR ',A16,' =',F7.3,
     3             ' (SMOOTHED)')
         ENDIF
C
C           COMPUTE AND PRINT THE AVERAGE DIFFERENCE BETWEEN THE DATA
C           AND THE CURRENT SMOOTHED ANALYSIS OVER THE SUBSETTED AREA.
C
         IF(IOPT(1).NE.0)THEN
C
            IF(NSUMGR.EQ.0)THEN
               AVGGR=9999.
            ELSE
               AVGGR=SUMGR/NSUMGR
            ENDIF
C
            IF(IP20.NE.0)THEN
               WRITE(IP20,2787)
     1                (JDATE(J),J=1,4),NSUMGR,LP,TITLE(1:16),AVGGR
 2787          FORMAT(' FOR DATE',I6,3I3.2,'  MEAN ABS DIFF OF',I5,
     1                ' VALUES USED IN SUBSET AREA ',
     2                ' ON PASS NO.',I2,' FOR ',A16,' =',F7.3,
     3                ' (SMOOTHED)')
            ENDIF
C
            IF(I405ADG.NE.0)THEN
C
               IF(IP20.NE.KFILDO)THEN
                  WRITE(KFILDO,2787)
     1                  (JDATE(J),J=1,4),NSUMGR,LP,TITLE(1:16),AVGGR
               ENDIF
C
            ENDIF
C
         ELSE
C              THERE IS NOT A SUBSET AREA.
            AVGGR=AVG
            NSUMGR=NSUM
C              AVGGR AND NSUMGR ARE PRINTED AT THE BOTTOM OF THE
C              GRIDPRINTED MAP.  IF THERE IS NO SUBSETTED AREA, THE
C              WHOLE AREA MAY BE PRINTED.  THEREFORE, SET AVGGR TO
C              AVG AND NSUMGR TO NSUM.
         ENDIF
C
      ENDIF
C
C        PREPARE SMOOTHED MAPS FOR GRIDPRINTING AND OR
C        WRITING IN TDLPACK, IF DESIRED.
C
      IF(JPRT(LP).NE.0.AND.IP22.NE.0.AND.JP(1).NE.0)GO TO 2790
      IF(JTDL(LP).NE.0.AND.KFILOG.NE.0.AND.JP(2).NE.0)GO TO 2790
      GO TO 2799
C
C        TO PROCEED BELOW, EITHER JPRT(LP) AND JP(1)
C        INDICATE GRIDPRINTING OR JTDL(LP) AND JP(2) INDICATE
C        PACKING AND THERE IS A NONZERO UNIT NUMBER TO WRITE TO.
C
 2790 NXG=NX
      NYG=NY
      MESHG=MESH
C        NXG, ETC. ARE NECESSARY BECAUSE SZGRDM CHANGE THEM, AND
C        NX, ETC. MUST BE RETAINED.
      CALL TRNSFR(P,FD2,NX*NY)
C 
CD     WRITE(KFILDO,2791)NX,NY
CD2791 FORMAT(/' CALLING SZGRDM AT 2791--NX,NY',2I6)
C
      CALL SZGRDM(KFILDO,FD2,NXG,NYG,
     1            MESHG,MESHL,ITRPLQ(LP),ND2X3)
C        SZGIRD PUTS THE GRID IN FD2( ) AT SUBSET MESH LENGTH MESHL.
C        IOPT( ) IS IN RELATION TO THAT MESH LENGTH.
C
C        GRIDPRINT SMOOTHED FIELD IF DESIRED.
C
      IF(JPRT(LP).EQ.0.OR.IP22.EQ.0.OR.JP(1).EQ.0)GO TO 2793
      TITLE(17:24)=SMTH(1:8)
      CALL PRTGR(IP22,FD2,NXG,NYG,
     1           CINT(LP),ORIGIN(LP),SMULT(LP),SADD(LP),IOPT,TITLE,IER)
      IF(IER.NE.0)ISTOP(1)=ISTOP(1)+1
C        A NON-ZERO IER IS NOT CONSIDERED FATAL.
C
C        AT THE BOTTOM OF THE MAP, BELOW THE TITLE, PRINT
C        THE PASS NUMBER LP, NTYPE( ), R( ), B( ), AND ER1( ).
C        IT IS EXPECTED THAT 4 PASSES WILL USUALLY BE USED
C        AND ALL THOSE VALUES ARE PRINTED.  HOWEVER, IF NPASS
C        IS GT 4, IT IS ACCOMMODATED.  THE NUMBER OF VALUES WITHIN
C        THE GRIDPRINTED AREA AND THE AVERAGE ABSOLUTE DIFFERENCE
C        ARE ALSO PRINTED.
C
      IF(NPASS.LE.4)THEN
         WRITE(IP22,2648)LP,(NTYPE(J),J=1,4),(R(J),J=1,4),
     1                   (MSHPAS(J),J=1,4),NSMTYP,(B(J),J=1,4),
     2                   (ER1(J),J=1,4),
     3                   (ITRPLQ(J),J=1,4),NSUMGR,AVGGR
C
         IF(IVRBL.EQ.1)THEN
            WRITE(IP22,2649)(WNDWT(J),J=1,4),WNDTHR,WNDTRN,WNDGRD
         ENDIF
C
      ELSEIF(NPASS.EQ.5)THEN
         WRITE(IP22,265)LP,(NTYPE(J),J=1,5),(R(J),J=1,5),
     1                   (MSHPAS(J),J=1,5),NSMTYP,(B(J),J=1,5),
     2                   (ER1(J),J=1,5),
     3                   (ITRPLQ(J),J=1,5),NSUMGR
C
         IF(IVRBL.EQ.1)THEN
            WRITE(IP22,2650)(WNDWT(J),J=1,5),WNDTHR,WNDTRN,WNDGRD,AVGGR
         ENDIF
C
         WRITE(IP22,2651)CINT(LP),ORIGIN(LP),SMULT(LP),SADD(LP)
      ELSE
         WRITE(IP22,2652)LP,(NTYPE(J),J=1,6),(R(J),J=1,6),
     1                   (MSHPAS(J),J=1,6),(B(J),J=1,6),(ER1(J),J=1,6),
     2                   (ITRPLQ(J),J=1,6),NSUMGR,AVGGR
      ENDIF
C
      IF(NPASS.NE.5)THEN
         WRITE(IP22,2653)CINT(LP),ORIGIN(LP),SMULT(LP),SADD(LP)
      ENDIF
C
C        TDLPACK AND WRITE SMOOTHED FIELD IF DESIRED FOR THE
C        SUBSET AREA.  WHEN IOPT(1) = 0, THERE IS NO SUBSET AREA.
C
 2793 IF(JTDL(LP).EQ.0.OR.KFILOG.EQ.0.OR.JP(2).EQ.0.
     1                                OR.IOPT(1).EQ.0)GO TO 2799
      LD(1)=ID(1)
      LD(2)=LP*10000+IDPARS(7)
C        THE LLLL IN ID(2) IS USED FOR THE PASS NUMBER.
C        IDPARS(7) MAINTAINS THE LEVEL.
      LD(3)=ID(3)
      LD(4)=(ID(4)/1000)*1000+010
C        THIS IS THE SMOOTHED ANALYSIS; S IN ID(4) = 010.  
      ITAUH=IDPARS(12)
      ITAUM=0
      NSEQ=0
      NCHAR=32
C        32 CHARACTERS OF PLAIN LANGUAGE ARE PACKED.
      XMISSP=0
      XMISSS=0
C        THESE ARE ANALYSES AND NO MISSING VALUES ARE PROVIDED FOR.
C        IF THERE EVER ARE, JUST SET XMISSP=9999, OR WHATEVER THE
C        MISSING VALUE IS.
C
C        THE GRID IN FD2( ) IS ALWAYS AT MESH LENGTH MESHL AS A
C        RESULT OF SZGRDM, WHICH IS WHAT IS WANTED FOR THE
C        DISPOSABLE GRID.  NOW CUT THE GRID TO THE DISPOSABLE
C        AREA.  IOPT( ) IS IN RELATION TO MESHL, WHICH IS THE 
C        MESH LENGTH OF FD2( ).
C
      NXD=IOPT(3)-IOPT(2)+1
      NYD=IOPT(5)-IOPT(4)+1
      NXOFF=IOPTGR(2)-1
      NYOFF=IOPTGR(4)-1
C        NXOFF AND NYOFF ARE THE DIFFERENCES IN THE (1,1) POINT
C        OF THE ANALYSIS AND SUBSETTED GRIDS AT THE CURRENT MESH
C        LENGTH MESH.  THERE IS NO NEED TO CALL CUTIT IF THE INPUT
C        AND OUTPUT GRIDS ARE THE SAME.  NXG AND NYG ARE THE
C        DIMENSIONS OF THE GRID IN FD2( ) AT MESH LENGTH MESHL.
C
      IF(NXOFF.NE.0.OR.NYOFF.NE.0.OR.NXG.NE.NXD.OR.NYG.NE.NYD)THEN
         CALL CUTIT(KFILDO,FD2,NXG,NYG,NXOFF,NYOFF,
     1              FD2,NXD,NYD,IER)
      ENDIF
C
      IF(IER.NE.0)THEN
         ISTOP(1)=ISTOP(1)+1
         WRITE(KFILDO,2795)(LD(J),J=1,4),PLAIN,NDATE
 2795    FORMAT('     NOT WRITING SMOOTHED ANALYSIS ',3(1X,I9.9),
     1           1X,I10.3,'  ',A32,' FOR DATE',I12,/,
     2          '     TO UNIT NO. KFILOG.')
         GO TO 2799
      ENDIF
C
      CALL ACTUAL(KFILDO,MESHL,XMESHL,TRASH,NPROJ,IER)
C        XMESHL IS THE ACTUAL MESH LENGTH IN KM. 
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,2656)IER
         ISTOP(1)=ISTOP(1)+1
         IER=777
         GO TO 400
      ENDIF
C
      IF(NPROJ.EQ.3)THEN
         CALL LMIJLL(KFILDO,FLOAT(IOPT(2)),FLOAT(IOPT(4)),
     1               XMESHL*1000.,ORIENT,XLAT,
     2               ALATL,ALONL,ALATD,ALOND,IER)
C
         IF(IER.NE.0)THEN
            WRITE(KFILDO,2796)IER
 2796       FORMAT(/'     FATAL ERROR IN LMIJLL FROM BCD5 AT 2796,',
     1              ' IER =',I4)
            ISTOP(1)=ISTOP(1)+1
            IER=777
            GO TO 400
         ENDIF
C
      ELSEIF(NPROJ.EQ.5)THEN
         CALL PSIJLL(KFILDO,FLOAT(IOPT(2)),FLOAT(IOPT(4)),
     1               XMESHL*1000.,ORIENT,XLAT,
     2               ALATL,ALONL,ALATD,ALOND)
      ELSEIF(NPROJ.EQ.7)THEN
         CALL MCIJLL(KFILDO,FLOAT(IOPT(2)),FLOAT(IOPT(4)),
     1               XMESHL*1000.,XLAT,
     2               ALATL,ALONL,ALATD,ALOND)
      ELSE
         WRITE(KFILDO,2797)NPROJ
 2797    FORMAT(/' ****MAP PROJECTION NUMBER NPROJ =',I3,
     1           ' NOT 3, 5, OR 7.  FATAL ERROR IN BCD5 AT 2797.')
         ISTOP(1)=ISTOP(1)+1
         IER=777
         GO TO 400
      ENDIF
C
C        ALATD AND ALOND ARE THE LL LAT/LON GRID POSITION.
C        TRUNCATE TO THREE DECIMAL PLACES TO ASSURE
C        COMPATIBILITY WITH AVN ARCHIVE AND OTHER PROGRAMS.
         ALATD=NINT(ALATD*1000)/1000.
         ALOND=NINT(ALOND*1000)/1000.
C
C           PAWGTS PACKS AND WRITES THE SMOOTHED DATA TO A
C           GRIDPOINT FILE.  BUT FIRST POSTPROCESS IF DESIRED.
C           THE OUTPUT GRIDS FROM BCD5 ARE AT THE CURRENT MESH LENGTH
C           MESH.  WHEN A GRID HAS BEEN CLIPPED TO A LARGER MESH 
C           LENGTH IN FSTGS5 THAN MESH, THE AREA COVERED WITH
C           NON-MISSING DATA MAY BE SLIGHTLY GRATER THAN THE DESIRED
C           AREA AT MESH LENGTH MESH.              
C
      CALL TRNSFR(FD2,COUNT,NXD*NYD)
C        DATA IN FD2( ) ARE TRANSFERRED TO COUNT( ) SO THIS
C        ARCHIVE OUTPUT POSTPROCESSING DOES NOT MODIFY THE
C        DATA.
C
C        POSTPROCESS IF NEEDED.  THREE ROUTINES ARE AVAILABLE.
C
      DO 2798 NN=1,3
C
      IF(POSTDS(NN).NE.'      ')THEN
C
         IF(POSTDS(NN).EQ.'POST  ')THEN           
            CALL POST(KFILDO,COUNT,NXD*NYD,
     1                TLOD(NN),SETLOD(NN),THID(NN),SETHID(NN),
     2                CONSTD(NN),NSCALD(NN),EX1D(NN),EX2D(NN),IER)
C              IF A POSTPROCESSING ROUTINE OTHER THAN POST IS
C              NEEDED, PUT CHECK AND CALL HERE.
C
         ELSEIF(POSTDS(NN).EQ.'CIGFT ')THEN           
            CALL CIGFT(KFILDO,COUNT,NXD*NYD,
     1                 TLOD(NN),SETLOD(NN),THID(NN),SETHID(NN),
     2                 CONSTD(NN),NSCALD(NN),EX1D(NN),EX2D(NN),IER)
C              THIS IS TO CHANGE LAMP CEILING HEIGHT IN CATEGORIES
C              TO HUNDREDS OF FT.
         ELSE
            WRITE(KFILDO,270)POSTDS(NN)
            ISTOP(1)=ISTOP(1)+1
         ENDIF
C
      ENDIF
C
 2798 CONTINUE
C
      CALL PAWGTS(KFILDO,KFILOG,'KFILOG',IP16,NDATE,
     1            LD,ITAUH,ITAUM,MODNO,NSEQ,ISCALD,
     2            NPROJ,ALATD,ALOND,ORIENT,MESHL,XLAT,NXD,NYD,
     3            COUNT,NCOUNT,IWORK,IPACK,ND5,MINPK,
     4            IS0,IS1,IS2,IS4,ND7,
     5            IPLAIN,PLAIN,NCHAR,
     6            XMISSP,XMISSS,LX,IOCTET,
     7            JTOTBY,JTOTRC,L3264B,L3264W,IER)
C
C        THIS IS THE END OF THE DISPOSABLE AND GRIDPRINT OUTPUT
C        FOR SMOOTHED GRIDS.
C
C        WHEN DESIRED, SET EACH STATION VALUE TO THE NEAREST GRIDPOINT,
C        PROVIDED A CLOSER STATION TO THE GRIDPOINT DOES NOT EXIST.
C        THIS IS DONE HERE IF IT IS (1) THE LAST PASS, (2) SMOOTHING
C        IS TO BE DONE, AND (3) ISETP NE 0.
C
 2799 IF(LP.EQ.NPASS.AND.B(LP).NE.0.AND.ISETP.NE.0)THEN
         CALL SETPNT(KFILDO,DATA,LTAG,XP,YP,NSTA,P,FD2,NX,NY,ISETP,
     1               IER)
C           THERE IS CURRENTLY NO NON ZERO IER RETURN.
      ENDIF
C
 280  CONTINUE
C     
 400  IER=0
C        ANALYSIS DONE AND PRINTED.
C        BCD5 DOES NOT RETURN A NON-ZERO ERROR.
C
CD     CALL TIMPR(KFILDO,KFILDO,'END BCD5            ')
      RETURN
      END
