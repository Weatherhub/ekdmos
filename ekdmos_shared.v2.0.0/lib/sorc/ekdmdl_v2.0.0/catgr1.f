      SUBROUTINE CATGR1(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JD,ITAU,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,IER)
C 
C        FEBRUARY 2000   GLAHN   TDL   MOS-2000
C        MARCH    2000   DALLAVALLE    MODIFIED FORMAT STATEMENTS 
C                                      TO CONFORM TO FORTRAN 90
C                                      STANDARDS ON IBM SP
C        MAY      2000   DALLAVALLE    FIXED AN ERROR WHERE THE
C                                      VARIABLE SUMBRB WAS USED
C                                      INSTEAD OF SUMPRB
C        JUNE     2000   RUDACK        MODIFIED SUBROUTINE TO
C                                      CALCULATE CATEGORICAL
C                                      FORECASTS FOR CLOUD COVER,
C                                      CEILING HEIGHT, AND QPF.
C        SEPT.    2000   DALLAVALLE    MODIFIED SUBROUTINE TO 
C                                      CALCULATE CATEGORICAL 
C                                      FORECASTS FOR VISIBILITY,
C                                      OBSTRUCTION TO VISION, POPC,
C                                      AVER. CLOUD COVER, AND PRECIP. 
C                                      TYPE DURING A 12-H PERIOD.
C        FEB.     2001   ALLEN         MODIFIED SUBROUTINE TO CALCULATE
C                                      CATEGORICAL FORECASTS FOR AVN QPF
C        FEB.     2001   ALLEN         MODIFIED SUBROUTINE TO ADJUST 
C                                      FOR REVISED CATEGORICAL IDS
C        MAY      2001   ANTOLIK       CORRECTED AVN QPF ENTRIES IN ITABLE
C        JUN      2001   MCE           CORRECTED 203331 (WAS 203321) TO LINK
C                                      TO 203330 FOR AVN
C        JAN      2002   COSGROVE      ADDED ETA IDS FOR QPF, PTYPE, AND CLOUDS
C        APR      2003   SHEETS        ADDED SECONDARY IDS FOR POPC
C        JUN      2003   COSGROVE      ADDED IDS FOR SNOWFALL AMOUNT
C        JULY     2003   COSGROVE      CHANGED IDS TO GET RID OF OLD MRF
C                                      MODEL NUMBER 9S FOR GFS TRANSITION.
C                                      REMOVED ENTRY IN ITABLE FOR 203421
C                                      BECAUSE IT IS NOT USED ANYMORE.
C        NOV      2003   COSGROVE      ADDED NEW CIGCLD IDS.  DID NOT TAKE OUT
C                                      OLD ONES YET, ALTHOUGH THEY WERE TAKEN 
C                                      OUT OF CALL IN OPTX.
C        NOV      2003   COSGROVE      HAD TO PUT BACK THE 9'S FOR MRF QPF, PTYPX,
C                                      AND CLDX FOR THE ENSEMBLES FOR THE TIME BEING.
C        JAN      2004   COSGROVE      ADDED IN NEW CIGCLD IDS, AND SNOWFALL FOR THE ETA
C        APRIL    2004   CMM           ADDED IDS FOR NEW GFS VISIBILITY AND ETA VIS AND
C                                      VISOBVIS. 
C        MARCH    2005   DR/JW         ADDED VIS AND OBVIS IDS TO MAKE LAMP CATEGORICAL 
C                                      FORECASTS.  CHANGED NDIM TO 77.
C        JUNE     2005   JRW           ADDED CONDITIONAL VIS IDS.  CHANGED NDIM TO 79.
C                                      ADDED POPO IDS. CHANGED NDIM TO 81.
C                                      ADDED GUST IDS. CHANGED NDIM TO 83.
C        JULY     2005   JRW           ADDED LAMP CEILING HGT IDS.
C                                      ADDED LAMP SKY COVER IDS. CHANGED NIM TO 87
C        AUGUST   2005   GRAMS         ADDED IDS FOR ETA POPC. 
C        AUGUST   2005   JRW           MERGED LMP VERSION AND MOS VERSION.  
C                                      CHANGED NDIM TO 86
C        OCTOBER  2005   JRW           ADDED LMP PTYPE IDS.  CHANGED NDIM TO 88
C        OCTOBER  2005   CHARBA        ADDED IDS FOR THE LAMP TSTM CATE-
C                                      GORICAL FORECASTS TO THOSE AC-
C                                      COMMODATED BY CATGR1. MODIFIED NDIM TO 90
C        OCTOBER  2005   CHARBA        MODIFIED TO ACCOMMODATE THREE-
C                                      SEASON MODEL FOR TSTMS.  THE
C                                      THREE SEASONS ARE:
C                                            16 MAR - 30 JUN;
C                                            01 JUL - 15 OCT;
C                                            16 OCT - 15 MAR.
C                                      MODIFIED L2L2TB DIMENION TO 5.
C        NOVEMBER 2005   WEISS         ADDED CONDITIONAL CEILING HT
C                                      CHANGED NDIM TO 92
C        NOVEMBER 2005   JRW           MODIFIED ITABLE.  DATA STATEMENT FOR ITABLE
C                                      BECAME TOO LARGE.  MADE IMPLICIT LOOP FOR ITABLE.
C                                      NDIMM=DIMENSION FOR MOS IDS.
C                                      NDIML=DIMENSION FOR LMP IDS.
C        NOVEMBER 2005   JRW           ADDED LMP PCHAR IDS TO TABLEL MODIFIED NDIML
C                                      TO NDIML=22
C        FEBRUARY 2006   SDS           MODIFIED IDS FOR PCHAR.
C        FEBRUARY 2006   JRW           MODIFIED TO ACCOMDATE TWO SEASONAL
C                                      LMP PTYPE.  MODIFIED L2L2TB TO DIMENSION 6.
C                                      MODIFIED IF CHECK NEAR FORMAT 109 TO CHECK
C                                      AGAINST 6 INSTEAD OF 5.
C        MARCH    2006   RUDACK        ADDED CHECK FOR CATEGORICAL WIND GUST
C                                      FORECAST(YES-NO)
C        MAY      2006   CHARBA        CHANGED IDS SUCH THAT LAMP THUN-
C                                      DERSTORM PROBABILITY THRESHOLDS
C                                      (AND THUS CATEGORICAL FORECASTS)
C                                      ARE BASED ON PROBABILITIES THAT
C                                      HAVE BEEN SMOOTHED ALONG REGIONAL
C                                      BOUNDARIES.
C                                      
C
C        PURPOSE 
C            TO COMPUTE THE BEST CATEGORY FROM A SET OF PROBABILITY
C            FORECASTS USING THRESHOLDS.  CURRENTLY DEALS WITH:
C              --CUMULATIVE PROBABILITIES FROM ABOVE, THRESHOLDS
C                FROM ABOVE
C              --CUMULATIVE PROBABILITIES FROM BELOW, THRESHOLDS
C                FROM BELOW
C              --DISCRETE PROBABILITIES, THRESHOLDS FROM ABOVE
C              --DISCRETE PROBABILITIES, THRESHOLDS FROM BELOW
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                     (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C            IP12   - INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                     STATIONS ON THE INPUT FILES WILL BE PRINTED TO 
C                     THE FILE WHOSE UNIT NUMBER IS IP12.  (OUTPUT)
C         KFILRA(J) - THE UNIT NUMBERS FOR WHICH RANDOM ACCESS FILES
C                     ARE AVAILABLE (J=1,NUMRA).  (INPUT)
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C                IP12 = INDICATES WHETHER (>0) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE EXTERNAL RANDOM ACCESS FILES
C                       WILL BE LISTED TO UNIT IP12.  (INPUT)
C           KFILRA(J) = THE UNIT NUMBERS FOR WHICH RANDOM ACCESS FILES
C                       ARE AVAILABLE (J=1,NUMRA).  (INPUT)
C           RACESS(J) = THE FILE NAMES ASSOCIATED WITH KFILRA(J) (J=1,NUMRA).
C                       (CHARACTER*60)  (INPUT)
C               NUMRA = THE NUMBER OF VALUES IN KFILRA( ) AND RACESS( ).
C                       (INPUT)
C               ID(J) = THE VARIABLE ID (J=1,4) FOR WHICH THE BEST
C                       CATEGORY IS DESIRED.  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       VARIABLE ID CORRESPONDING TO ID( ) (J=1,15).
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
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK IN 
C                            TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               JD(J) = THE BASIC INTEGER VARIABLE ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE
C                       PORTIONS PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       JD( ) IS USED TO IDENTIFY THE BASIC MODEL FIELDS
C                       AS READ FROM THE ARCHIVE.  (INPUT)
C                ITAU = THE NUMBER OF HOURS AHEAD TO FIND A VARIABLE.
C                       THIS HAS ALREADY BEEN CONSIDERED IN MDATE, BUT
C                       IS NEEDED FOR CALL TO RETVEC.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH FORECAST CATEGORY IS
C                       NEEDED.  (INPUT)
C               MDATE = NDATE UPDATED WITH ITAU( ).  (INPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST, EXCEPT POSSIBLY 
C                       CCALLD( ).  EQUIVALENCED TO ICALL( , , ). 
C                       (CHARACTER*8)  (INPUT)
C           ISDATA(K) = WORK ARRAY (K=1,ND1).  (INTERNAL)
C            XDATA(K) = THE BEST CATEGORY.  (OUTPUT) 
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF XDATA( ) YDATA( ) AND JDATA( ).
C                       (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN INTEGER
C                       VARIABLE (L=1,L3264W) (K=1,ND5).  THIS ARRAY IS USED 
C                       TO READ THE STATION DIRECTORY FROM A MOS-2000
C                       EXTERNAL FILE.  EQUIVALENCED TO CCALLD( ). 
C                       (CHARACTER*8)  (INTERNAL)
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).  EQUIVALENCED
C                       TO ICALLD( , ).  (INTERNAL)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(K) = WORK ARRAY (J=1,ND5).  (INTERNAL)
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
C                              IN NGRIDC( ,L) DEFINING THE 
C                              CHARACTERISTICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,NPRED) FOR WHICH 
C                              THIS VARIABLE IS NEEDED, WHEN IT IS 
C                              NEEDED ONLY ONCE FROM LSTORE( , ). 
C                              WHEN IT IS NEEDED MORE THAN ONCE, THE 
C                              VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING 
C                              MSTORE( , ).  LATER USED AS A WAY OF
C                              DETERMINING WHETHER TO KEEP THIS 
C                              VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT ARE IN USE.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA 
C                       IDENTIFIED IN LSTORE( , ) (J=1,ND10).  WHEN 
C                       CORE( ) IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NFETCH = THE NUMBER OF TIMES GFETCH HAS BEEN ENTERED.
C                       GFETCH KEEPS TRACK OF THIS AND RETURNS THE
C                       VALUE.  (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3). 
C                       (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+). 
C                       (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12). 
C                       (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4). 
C                       (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).  
C                       CALCULATED BY PARAMETER, BASED ON L3464B.
C                       (INPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       102 = ID NOT ACCOMMODATED OR VALUE IN 
C                             ITABLE( , ) IS INCORRECT.
C                       SEE RETVEC FOR OTHER VALUES.  (OUTPUT)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       YDATA( ) (J=1,4).  (INTERNAL)
C           LDPARS(J) = PARSED VALUES CORRESPONDING TO LD( ) (J=1,15)
C                       (INTERNAL)
C               MD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       XDATA( ) (J=1,4).  (INTERNAL)
C           MDPARS(J) = PARSED VALUES CORRESPONDING TO MD( ) (J=1,15)
C                       (INTERNAL)
C            YDATA(K) = WORK ARRAY (K=1,ND1) WHICH HOLDS THRESHOLDS.
C                       (AUTOMATIC)
C            JDATA(K) = BEST CATEGORY WHILE COMPUTING (K=1,ND1).  THEN
C                       TRANSFERRED TO XDATA( ) FOR RETURN.  (AUTOMATIC)
C           SUMPRB(K) = SUM OF PROBABILITIES OF CATEGORIES (K=1,ND1).
C                       (AUTOMATIC)
C         ITABLE(I,J) = I=1--VALUE OF CCCFFF ACCOMMODATED;
C                       I=2--ID(1) FOR THRESHOLDS;
C                       I=3--ID(1) FOR PROBABILITY FORECASTS;
C                       I=4--NUMBER OF THRESHOLDS AND THE NUMBER OF
C                            PROBABILITY FORECASTS NEEDED;
C                       I=5--HOW THE CATEGORICAL FORECASTS ARE TO BE 
C                            DETERMINED
C                            1 = FORECASTS ARE CUMULATIVE FROM ABOVE AND
C                                THRESHOLDS ARE DETERMINED CUMULATIVE
C                                FROM ABOVE
C                            2 = FORECASTS ARE CUMULATIVE FROM BELOW AND
C                                THRESHOLDS ARE DETERMINED CUMULATIVE
C                                FROM BELOW
C                            3 = FORECASTS ARE DISCRETE AND THRESHOLDS
C                                ARE DETERMINED CUMULATIVE FROM ABOVE
C                            4 = FORECASTS ARE DISCRETE AND THRESHOLDS
C                                ARE DETERMINED CUMULATIVE FROM BELOW
C                       I=6--KEY REPRESENTING THE TIME SLICING:
C                            1 = 1 SEASON, ALL YEAR, VALUE = 19
C                            2 = TWO SEASONS, APR-SEPT, OCT-MAR,
C                                VALUES = 17, 18
C                            3 = FOUR SEASONS, MARCH-MAY, JUNE-AUG, ETC.
C                                VALUES = 13, 14, 15, 16
C                            4 = EACH MONTH, VALUES = 1-12
C                            5 = LMP TSTM 3 SEASON CYCLE, MARCH 16-JUNE 30
C                                JULY 1-OCT 15, OCT 16-MARCH 15
C                                VALUES = 22,20,21 
C                            6 = LMP PTYPE 2 SEASON CYCLE, SEPT-MAY, JUNE-AUG.
C                                VALUES = 23,14
C
C                       I=7,6+ITABLE(2,J)--THE THRESHOLDS FOR THE 
C                            PROBABILITY FORECASTS (THE FIRST 6 DIGITS OF
C                            ID(4).  NOTE THAT THESE ARE IN THE EXACT
C                            FORM IN ID(4) AND ARE ENTERED IN THE ORDER 
C                            USED IN THE COMPUTATION, NOT NECESSARILY 
C                            LOW TO HIGH.
C                       (J=1,NDIM).  (INTERNAL)
C                       WITH 7 THRESHOLDS PROVIDED FOR, 8 CATEGORIES ARE
C                       POSSIBLE.  THIS CAN EASILY BE INCREASED.
C                NCAT = THE NUMBER OF THRESHOLDS AND NUMBER OF 
C                       PROBABILITY FORECASTS NEEDED FOR COMPUTATION.
C                       SET = ITABLE(2, ).
C         L2L2TB(N,J) = VALUES FOR L2L2 IN ID(2) FOR THE THRESHOLDS
C                       CORRESPONDING TO MONTH (N=1,12) AND TIME
C                       SLICE FOR COMPUTING THRESHOLDS (J=1,4).
C                       (J = ITABLE(6, ) ABOVE.)
C
C        NONSYSTEM SUBROUTINES CALLED 
C            RETVEC, PRSID1
C
      PARAMETER (NDIMM=75)
      PARAMETER (NDIML=22)
      PARAMETER (NDIM=NDIMM+NDIML)
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION XDATA(ND1)
      DIMENSION YDATA(ND1),JDATA(ND1),SUMPRB(ND1)
C        YDATA( ), JDATA( ), AND SUMPRB( ) ARE AUTOMATIC ARRAYS.
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10),ICALLD(L3264W,ND5)
      DIMENSION LD(4),LDPARS(15),MD(4),MDPARS(15),KFILRA(5)
      DIMENSION ITABLE(13,NDIM),L2L2TB(12,6)
      DIMENSION ITABLEM(13,NDIMM),ITABLEL(13,NDIML)
C 
      DATA ((ITABLE(I,J),I=1,13),J=1,NDIMM)/
     A            208546008,828545308,208545308,2,4,1,
     1            0350001,0450001,0,0,0,0,0,
     B            208546018,828545318,208545318,2,4,1,
     1            0350001,0450001,0,0,0,0,0,
     C            208546028,828545328,208545328,2,4,1,
     1            0350001,0450001,0,0,0,0,0,
     D            208341008,828340308,208340308,3,4,2,
     1            0150001,0400001,0700001,0,0,0,0,
     E            208351008,828350308,208350308,4,4,2,
     1            0150001,0250001,0400001,0700001,0,0,0,
     F            208341018,828340318,208340318,3,4,2,
     1            0150001,0400001,0700001,0,0,0,0,
     G            208351018,828350318,208350318,4,4,2,
     1            0150001,0250001,0400001,0700001,0,0,0,
     H            208341028,828340328,208340328,3,4,2,
     1            0150001,0400001,0700001,0,0,0,0,
     I            208351028,828350328,208350328,4,4,2,
     1            0150001,0250001,0400001,0700001,0,0,0,
     J            208041018,828040318,208040318,6,4,2,
     1            0150001,0450001,0950001,0305002,0655002,0120503,0,
     K            208051018,828050318,208050318,7,4,2,
     1            0150001,0450001,0950001,0195002,0305002,0655002,
     2            0120503,
     L            208041008,828040308,208040308,6,4,2,
     1            0150001,0450001,0950001,0305002,0655002,0120503,0,
     M            208051008,828050308,208050308,7,4,2,
     1            0150001,0450001,0950001,0195002,0305002,0655002,
     2            0120503,
     N            208041028,828040328,208040328,6,4,2,
     1            0150001,0450001,0950001,0305002,0655002,0120503,0,
     O            208051028,828050328,208050328,7,4,2,
     1            0150001,0450001,0950001,0195002,0305002,0655002,
     2            0120503,
     P            208121008,808120208,208120208,6,2,2,
     1            0300000,0550000,0950000,0295001,0505001,
     2            0605001,0,
     Q            208121018,808120218,208120218,6,2,2,
     1            0300000,0550000,0950000,0295001,0505001,
     2            0605001,0,
     R            208121028,808120228,208120228,6,2,2,
     1            0300000,0550000,0950000,0295001,0505001,
     2            0605001,0,
     S            208131008,808130208,208130208,6,2,2,
     1            0495000,0950000,0195001,0295001,0505001,
     2            0605001,0,
     T            208131018,808130218,208130218,6,2,2,
     1            0495000,0950000,0195001,0295001,0505001,
     2            0605001,0,
     U            208131028,808130228,208130228,6,2,2,
     1            0495000,0950000,0195001,0295001,0505001,
     2            0605001,0,
     V            208291008,818290308,208290308,4,3,2,
     1            0999905,0450001,0350001,0250001,0,0,0,
     W            208291018,818290318,208290318,4,3,2,
     1            0999905,0450001,0350001,0250001,0,0,0,
     X            208291028,818290328,208290328,4,3,2,
     1            0999905,0450001,0350001,0250001,0,0,0,
     Y            203321008,803320108,203320108,6,1,2,199501,995000,
     1            495000,245000,950051,950052,0,
     Z            208646008,828645308,208645308,2,4,2,
     1            0150001,0250001,0,0,0,0,0,
     A            208646018,828645318,208645318,2,4,2,
     1            0150001,0250001,0,0,0,0,0,
     B            208646028,828645328,208645328,2,4,2,
     1            0150001,0250001,0,0,0,0,0,
     C            208391008,828390308,208390308,2,4,2,
     1            0312500,0687500,0,0,0,0,0,
     D            208391018,828390318,208390318,2,4,2,
     1            0312500,0687500,0,0,0,0,0,
     E            208391028,828390328,208390328,2,4,2,
     1            0312500,0687500,0,0,0,0,0,
     F            208391009,828390309,208390309,2,4,2,
     1            0312500,0687500,0,0,0,0,0,
     G            208391019,828390319,208390319,2,4,2,
     1            0312500,0687500,0,0,0,0,0,
     H            208391029,828390329,208390329,2,4,2,
     1            0312500,0687500,0,0,0,0,0,
     I            203321009,803320109,203320109,6,1,2,199501,995000,
     1            495000,245000,950051,950052,0,
     J            203421009,803420109,203420109,7,1,2,299501,199501,
     1            995000,495000,245000,950051,950052,
     K            208556009,828555309,208555309,3,4,1,
     1            0150001,0250001,0350001,0,0,0,0,
     L            208556019,828555319,208555319,3,4,1,
     1            0150001,0250001,0350001,0,0,0,0,
     M            208556029,828555329,208555329,3,4,1,
     1            0150001,0250001,0350001,0,0,0,0,
     N            208556008,828555308,208555308,3,4,1,
     1            0150001,0250001,0350001,0,0,0,0,
     O            208556018,828555318,208555318,3,4,1,
     1            0150001,0250001,0350001,0,0,0,0,
     P            208556028,828555328,208555328,3,4,1,
     1            0150001,0250001,0350001,0,0,0,0,
     Q            203221008,803220108,203220108,5,1,2,
     1            995000,495000,245000,950051,950052,0,0,
     R            203331008,803330108,203330108,6,1,2,
     1            199501,995000,495000,245000,950051,950052,0,
     S            203431008,803430108,203430108,6,1,2,
     1            199501,995000,495000,245000,950051,950052,0,
     T            208546007,828545307,208545307,2,4,1,
     1            0350001,0450001,0,0,0,0,0,
     U            208546017,828545317,208545317,2,4,1,
     1            0350001,0450001,0,0,0,0,0,
     V            208546027,828545327,208545327,2,4,1,
     1            0350001,0450001,0,0,0,0,0,
     W            208341007,828340307,208340307,3,4,2,
     1            0150001,0400001,0700001,0,0,0,0,
     X            208351007,828350307,208350307,4,4,2,
     1            0150001,0250001,0400001,0700001,0,0,0,
     Y            208341017,828340317,208340317,3,4,2,
     1            0150001,0400001,0700001,0,0,0,0,
     Z            208351017,828350317,208350317,4,4,2,
     1            0150001,0250001,0400001,0700001,0,0,0,
     A            208341027,828340327,208340327,3,4,2,
     1            0150001,0400001,0700001,0,0,0,0,
     B            208351027,828350327,208350327,4,4,2,
     1            0150001,0250001,0400001,0700001,0,0,0,
     C            208041017,828040317,208040317,6,4,2,
     1            0150001,0450001,0950001,0305002,0655002,0120503,0,
     D            208051017,828050317,208050317,7,4,2,
     1            0150001,0450001,0950001,0195002,0305002,0655002,
     2            0120503,
     E            208041007,828040307,208040307,6,4,2,
     1            0150001,0450001,0950001,0305002,0655002,0120503,0,
     F            208051007,828050307,208050307,7,4,2,
     1            0150001,0450001,0950001,0195002,0305002,0655002,
     2            0120503,
     G            208041027,828040327,208040327,6,4,2,
     1            0150001,0450001,0950001,0305002,0655002,0120503,0,
     H            208051027,828050327,208050327,7,4,2,
     1            0150001,0450001,0950001,0195002,0305002,0655002,
     2            0120503,
     I            203221007,803220107,203220107,5,1,2,
     1            995000,495000,245000,950051,950052,0,0,
     J            203331007,803330107,203330107,6,1,2,
     1            199501,995000,495000,245000,950051,950052,0,
     K            203431007,803430107,203430107,6,1,2,
     1            199501,995000,495000,245000,950051,950052,0,
     L            208461008,808460108,208460108,5,1,1,
     1            795001,595001,395001,195001,500051,0,0,
     M            208461007,808460107,208460107,5,1,1,
     1            795001,595001,395001,195001,500051,0,0,
     N            208131007,808130207,208130207,6,2,2,
     1            0495000,0950000,0195001,0295001,0505001,
     2            0605001,0,
     O            208131017,808130217,208130217,6,2,2,
     1            0495000,0950000,0195001,0295001,0505001,
     2            0605001,0,
     P            208131027,808130227,208130227,6,2,2,
     1            0495000,0950000,0195001,0295001,0505001,
     2            0605001,0,
     Q            208291007,818290307,208290307,4,3,2,
     1            0999905,0450001,0350001,0250001,0,0,0,
     R            208291017,818290317,208290317,4,3,2,
     1            0999905,0450001,0350001,0250001,0,0,0,
     S            208291027,818290327,208290327,4,3,2,
     1            0999905,0450001,0350001,0250001,0,0,0,
     T            208646007,828645307,208645307,2,4,2,
     1            0150001,0250001,0,0,0,0,0,
     U            204366008,804365108,204365108,1,1,2,139502,0,
     1            0,0,0,0,0,
     V            204366018,804365118,204365118,1,1,2,139502,0,
     1            0,0,0,0,0,
     X            204366028,804365128,204365128,1,1,2,139502,0,
     1            0,0,0,0,0/
C
      DATA ((ITABLE(I,J),I=1,13),J=NDIMM+1,NDIM)/
     A            208546015,828545315,208545315,2,4,6,
     1            0350001,0450001,0,0,0,0,0,
     B            208546025,828545325,208545325,2,4,6,
     1            0350001,0450001,0,0,0,0,0,
     C            208071015,808070215,208070215,7,2,2,
     1            0150001,0450001,0950001,0195002,0305002,0655002,
     2            0120503,
     D            208071025,808070225,208070225,7,2,2,
     1            0150001,0450001,0950001,0195002,0305002,0655002,
     2            0120503,
     E            208056015,808055215,208055215,7,2,2,
     1            0150001,0450001,0950001,0195002,0305002,0655002,
     2            0120503,
     F            208056025,808055225,208055225,7,2,2,
     1            0150001,0450001,0950001,0195002,0305002,0655002,
     2            0120503,
     G            208131015,808130215,208130215,6,2,2,
     1            0495000,0950000,0195001,0295001,0505001,
     2            0605001,0,
     H            208131025,808130225,208130225,6,2,2,
     1            0495000,0950000,0195001,0295001,0505001,
     2            0605001,0,
     I            208156015,808155215,208155215,6,2,2,
     1            0495000,0950000,0195001,0295001,0505001,
     2            0605001,0,
     J            208156025,808155225,208155225,6,2,2,
     1            0495000,0950000,0195001,0295001,0505001,
     2            0605001,0,
     K            208291015,818290315,208290315,4,3,2,
     1            0999905,0450001,0350001,0250001,0,0,0,
     L            208291025,818290325,208290325,4,3,2,
     1            0999905,0450001,0350001,0250001,0,0,0,
     M            208351015,828350315,208350315,4,4,2,
     1            0150001,0250001,0400001,0700001,0,0,0,
     N            208351025,828350325,208350325,4,4,2,
     1            0150001,0250001,0400001,0700001,0,0,0,
     O            204366015,804365115,204365115,1,1,2,
     1            0139502,0,0,0,0,0,0,
     P            204366025,804365125,204365125,1,1,2,
     1            0139502,0,0,0,0,0,0,
     Q            208621015,808620115,208620115,1,1,2,
     1            0500000,0,0,0,0,0,0,
     R            208621025,808620125,208620125,1,1,2,
     1            0500000,0,0,0,0,0,0,
     S            207501015,807505115,207505115,1,1,5,
     1            500000,000000,000000,000000,000000,0,0,
     T            207501025,807505125,207505125,1,1,5,
     1            500000,000000,000000,000000,000000,0,0,
     U            208666015,828665315,208665315,2,4,2,
     1            0150001,0250001,0,0,0,0,0,
     V            208666025,828665325,208665325,2,4,2,
     1            0150001,0250001,0,0,0,0,0/


      DATA L2L2TB/19,19,19,19,19,19,19,19,19,19,19,19,
     1            18,18,18,17,17,17,17,17,17,18,18,18,
     2            16,16,13,13,13,14,14,14,15,15,15,16,
     3             1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,
     4            22,22,22,20,20,20,21,21,21,21,22,22,
     5            23,23,23,23,23,14,14,14,23,23,23,23/
C
      IER=0
C
      DO 100 JJ=1,NDIM
      IF(ITABLE(1,JJ).EQ.ID(1))GO TO 1075
 100  CONTINUE

      WRITE(KFILDO,107)(ID(L),L=1,4)
 107  FORMAT(' ****CATGR1 ENTERED FOR VARIABLE',
     1        2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2        ' NOT ACCOMMODATED.')
      IER=102
      GO TO 400
C   
 1075 IF(ITABLE(5,JJ).GT.4)THEN
         WRITE(KFILDO,108)ITABLE(5,JJ),(ID(J),J=1,4)
 108     FORMAT(/,' ****ITABLE(5, ) =',I4,' OUT OF RANGE IN ',
     1           'CATGR1.  CANNOT COMPUTE BEST CATEGORY FOR',
     2            2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         IER=102
         GO TO 400
C
      ELSEIF(ITABLE(6,JJ).GT.6)THEN
         WRITE(KFILDO,109)ITABLE(6,JJ),(ID(J),J=1,4)
 109     FORMAT(/,' ****ITABLE(6, ) =',I4,' OUT OF RANGE IN ',
     1           'CATGR1.  CANNOT COMPUTE BEST CATEGORY FOR',
     2            2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         IER=102
         GO TO 400
C
      ENDIF         
C       
C        ZERO THE JDATA( ) ARRAY WHICH WILL HOLD THE
C        CATEGORY DURING THE COMPUTATION.  ALSO, ZERO
C        AN ARRAY TO HOLD THE SUM OF THE PROBABILITIES
C        WHEN DEALING WITH DISCRETE PROBABILITIES.
C
      DO 111 K=1,NSTA
      JDATA(K)=0
      SUMPRB(K)=0.
 111  CONTINUE
C
      NCAT=ITABLE(4,JJ)
      NCYCLE=NDATE-(NDATE/100)*100
C        NCYCLE IS L1L1 IN ID(2)
      MONTH=NDATE/10000-(NDATE/1000000)*100
C        MONTH DETERMINES L2L2 IN ID(2) FOR THRESHOLDS.
      L2L2=L2L2TB(MONTH,ITABLE(6,JJ))
C
C        L2L2 IS THE L2L2 IN ID(2) FOR THRESHOLDS.  ADJUST L2L2 FOR
C        SPLIT MONTHS IN THREE-SEASON MODEL FOR TSTMS.
C
      NDAY=MOD(NDATE,10000)/100
      IF(MONTH.EQ.03.AND.L2L2.EQ.22.AND.NDAY.GE.16) THEN
         L2L2=20
      ELSEIF(MONTH.EQ.10.AND.L2L2.EQ.21.AND.NDAY.GE.16) THEN
         L2L2=22
      ENDIF
C
      LD(1)=ITABLE(2,JJ)
      LD(2)=NCYCLE*1000000+L2L2*10000
      LD(3)=IDPARS(12)
C
      MD(1)=ITABLE(3,JJ)
      MD(2)=0
      MD(3)=LD(3)
C
C        START LOOP FOR NCAT THRESHOLDS AND FORECASTS.
C        USE THE THRESHOLDS AND FORECASTS IN THE ORDER
C        SPECIFIED IN ITABLE( ,JJ).
C
      DO 200 N=1,NCAT
C
C        COMPUTE ID(4) FOR THE THRESHOLDS.  THE FIRST 3 IDS DO NOT
C        CHANGE FROM CATEGORY TO CATEGORY.
C        
      LD(4)=ITABLE(N+6,JJ)*1000+IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
C 
C        FETCH THE THRESHOLDS IN YDATA( ).
C
      CALL PRSID1(KFILDO,LD,LDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            LD,LDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,YDATA,ND1,NSTA,
     3            ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4            LSTORE,ND9,LITEMS,CORE,ND10,
     5            NBLOCK,NFETCH,
     6            IS0,IS1,IS2,IS4,ND7,
     7            L3264B,L3264W,IER)
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,127)(LD(J),J=1,4),(ID(J),J=1,4)
 127     FORMAT(' ****THRESHOLDS',2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     1          ' NOT RETRIEVED BY RETVEC IN CATGR1.',
     2          '  CANNOT COMPUTE BEST CATEGORY FOR',/,
     3          '               ',2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 400
C
      ENDIF
C
C        COMPUTE ID(4) FOR THE PROBABILITIES.  THE FIRST 3 IDS DO NOT
C        CHANGE FROM CATEGORY TO CATEGORY.
C
      MD(4)=LD(4)     
C 
C        FETCH THE PROBABILITIES IN XDATA( ).
C
      CALL PRSID1(KFILDO,MD,MDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            MD,MDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NSTA,
     3            ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4            LSTORE,ND9,LITEMS,CORE,ND10,
     5            NBLOCK,NFETCH,
     6            IS0,IS1,IS2,IS4,ND7,
     7            L3264B,L3264W,IER)
C 
C        JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C        IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,135)(MD(J),J=1,4),(ID(J),J=1,4)
 135     FORMAT(' ****PROBABILITIES',2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     1          ' NOT RETRIEVED BY RETVEC IN CATGR1.',
     2          '  CANNOT COMPUTE BEST CATEGORY FOR',/,
     3          '                  ',2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 400
C
      ENDIF
C
      DO 160 K=1,NSTA
      IF(JDATA(K).NE.0)GO TO 160
C        ONCE JDATA( ) HAS BEEN SET NE 0, IT IS NOT CHANGED AGAIN.
      IF(NINT(XDATA(K)).EQ.9997)XDATA(K)=0.
C        A FORECAST OF 9997 IS TREATED AS 0.
C
      IF(NINT(XDATA(K)).EQ.9999)THEN
C           IF ANY FORECAST IS TRULY MISSING, THE BEST CATEGORY
C           IS SET MISSING.
         JDATA(K)=9999
      ELSEIF(NINT(YDATA(K)).EQ.9999)THEN
C           IF ANY THRESHOLD IS MISSING, THE BEST CATEGORY
C           IS SET MISSING.
         JDATA(K)=9999
      ELSE
C
         IF(ITABLE(5,JJ).LE.2)THEN
C
C              THIS SECTION FOR CUMULATIVE PROBABILITIES.
C
            IF(XDATA(K).GE.YDATA(K))THEN
C 
               IF(ITABLE(5,JJ).EQ.1)THEN
                  JDATA(K)=NCAT+2-N
               ELSE
                  JDATA(K)=N
               ENDIF
C
            ENDIF
C
         ELSE
C
C              THIS SECTION FOR DISCRETE PROBABILITIES.
C
            SUMPRB(K)=SUMPRB(K)+XDATA(K)
C
            IF(SUMPRB(K).GE.YDATA(K))THEN
C
               IF(ITABLE(5,JJ).EQ.3)THEN
                  JDATA(K)=NCAT+2-N
               ELSE
                  JDATA(K)=N
               ENDIF
C
            ENDIF
C
         ENDIF
C
      ENDIF
C
 160  CONTINUE
C
 200  CONTINUE
C
C        SET XDATA( ) TO THE CATEGORY IN JDATA( ).
C
      DO 210 K=1,NSTA
C
      IF(JDATA(K).EQ.0)THEN
      
         IF(ITABLE(5,JJ).EQ.1.OR.
     1      ITABLE(5,JJ).EQ.3)THEN
            XDATA(K)=1
         ELSE
            XDATA(K)=NCAT+1
         ENDIF
C
      ELSE
         XDATA(K)=JDATA(K)
      ENDIF
C
 210  CONTINUE
C     
      GO TO 450
C
C        THIS PREDICTOR CANNOT BE COMPUTED.  SET THE ARRAY TO MISSING.
C
 400  DO 410 K=1,NSTA
         XDATA(K)=9999.
 410  CONTINUE 
C
 450  RETURN
      END     


