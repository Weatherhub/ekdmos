      SUBROUTINE WXPCHR(KFILDO,KFIL10,IP12,KFILRA,RACESS,
     1                  NUMRA,ID,IDPARS,JD,NDATE,
     2                  CCALL,NAME,STALAT,STALON,
     3                  ISDATA,SDATA,DIR,ND1,NSTA,
     4                  ND2X3,ICALLD,CCALLD,IPACK,IWORK,
     5                  ND5,NGRIDC,NGRID,ND11,
     6                  NSLAB,LSTORE,ND9,LITEMS,CORE,ND10,
     7                  NBLOCK,NFETCH,LASTL,LASTD,NSTORE,
     8                  IS0,IS1,IS2,IS4,ND7,
     9                  L3264B,L3264W,ISTAV,
     A                  JSTA,NPCAT,NXPCH,
     B                  RRMIS,MISSEL,IER)
C
C        MARCH     2012   HUNTEMANN   MDL   MOS-2000
C        MARCH     2013   HUNTEMANN   MDL   ADDED IMPLICIT NONE,
C                                           ADDED JSTA TO CALL.
C
C        PURPOSE
C           SUBROUTINE WXPCHR ASSIGNS A PRECIPITATION
C           CHARACTER (STRATIFORM, CONVECTIVE, OR DRIZZLE) BASED
C           ON MOS GRIDDED GUIDANCE.  AS OF MARCH 2012, THESE MOS
C           PROBABILITIES DO NOT EXIST AND THIS ROUTINE IS JUST
C           A PLACEHOLDER.
C
C           THIS ROUTINE IS FOR GRIDPOINT DATA.  GRIDPOINT DATA ARE 
C           RETURNED IN NXPCH( ).  CALLED BY GENWX.
C
C        DATA SET USE
C           KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C           KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                    (INPUT-OUTPUT) 
C           IP12   - INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                    STATIONS ON THE INPUT FILES WILL BE PRINTED TO 
C                    THE FILE WHOSE UNIT NUMBER IS IP12.  (OUTPUT)
C         
C        VARIABLES
C           MOS2K SYSTEM VARIABLES
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) 
C                       FILE. (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT-OUTPUT) 
C              IP12   = INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE INPUT FILES WILL BE PRINTED TO 
C                       THE FILE WHOSE UNIT NUMBER IS IP12.
C           KFILRA(J) = THE UNIT NUMBERS FOR WHICH RANDOM ACCESS FILES
C                       ARE AVAILABLE (J=1,NUMRA).  (INPUT)
C           RACESS(J) = THE FILE NAMES ASSOCIATED WITH KFILRA(J)
C                       (J=1,NUMRA).(CHARACTER*60)  (INPUT)
C               NUMRA = THE NUMBER OF VALUES IN KFILRA( ) AND RACESS( ).
C                       (INPUT)
C               ID(J) = THE PREDICTOR ID (J=1,4).  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTORID CORRESPONDING TO ID( ) 
C                       (J=1,15).  (INPUT)
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
C             JD(J,N) = THE BASIC INTEGER VARIABLE ID (J=1,4) (N=1,NPRED).
C                       THIS IS THE SAME AS ID(J,N), EXCEPT THAT THE PORTIONS
C                       PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3, ),
C                       T = IDPARS(8, ),
C                       I = IDPARS(13, ),
C                       S = IDPARS(14, ),
C                       G = IDPARS(15, ), AND
C                       THRESH( ).
C                       JD( , ) IS USED TO FOR INPUT TO CONSTG, BECAUSE
C                       INTERPOLATION INTO THE GRID MAY BE REQUIRED,
C                       BUT IS NOT PART OF THE BASIC ID.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.  (INPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST, EXCEPT POSSIBLY 
C                       CCALLD( ).  EQUIVALENCED TO ICALL( , , ). 
C                       (CHARACTER*8)  (INPUT)
C             NAME(K) = NAMES OF STATIONS (K=1,NSTA).  USED FOR PRINTOUT
C                       ONLY.  (CHARACTER*20)  (INPUT)
C           STALAT(K) = LATITUDE OF STATIONS (K=1,NSTA).  (INPUT/OUTPUT)
C           STALON(K) = LONGITUDE OF STATIONS (K=1,NSTA).  (INPUT/OUTPUT)
C           ISDATA(K) = WORK ARRAY (K=1,ND1).  (INTERNAL)
C            SDATA(K) = DATA RETURNED WHEN DATA ARE VECTOR (K=1,NSTA).
C                       (OUTPUT)
C          DIR(K,J,M) = THE IX (J=1) AND JY (J=2) POSITIONS ON THE GRID
C                       FOR THE COMBINATION OF GRID CHARACTERISTICS M
C                       (M=1,NGRID) AND STATION K (K=1,NSTA) IN NGRIDC( ,M).
C                       (INPUT/OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       DIMENSION OF SEVERAL VARIABLES.  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT WITH.
C                       (INPUT)
C               ND2X3 = DIMENSION OF GRIDDED VARIABLES. (INPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN INTEGER
C                       VARIABLE (L=1,L3264W) (K=1,ND5).  THIS ARRAY IS USED 
C                       TO READ THE STATION DIRECTORY FROM A MOS-2000
C                       EXTERNAL FILE.  EQUIVALENCED TO CCALLD( ). 
C                       (CHARACTER*8)  (INTERNAL)
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).  THIS ARRAY IS USED 
C                       IN CONST TO READ THE STATION DIRECTORY.  EQUIVALENCED 
C                       TO ICALLD( , ).  (CHARACTER*8)  (INTERNAL)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), DATA( ) AND
C                       CCALLD( ); SECOND DIMENSION OF ICALLD( , ).
C                       THESE ARE GENERAL PURPOSE ARRAYS, SOMETIMES USED
C                       FOR GRIDS.  (INPUT)
C         NGRIDC(L,M) = HOLDS THE GRID CHARACTERISTICS (L=1,6) FOR EACH GRID
C                       COMBINATION (M=1,NGRID).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=POLAR
C                            STEREOGRAPHIC). 
C                       L=2--GRID LENGTH IN MILLIMETERS,
C                       L=3--LATITUDE AT WHICH GRID LENGTH IS CORRECT *10000,
C                       L=4--GRID ORIENTATION IN DEGREES *10000,
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *10000,
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES *10000.
C                       (INPUT/OUTPUT)
C               NGRID = THE NUMBER OF GRID COMBINATIONS IN DIR( , , ),
C                       MAXIMUM OF ND11.  (INPUT/OUTPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN BE
C                       DEALT WITH ON THIS RUN.  LAST DIMENSION OF
C                       NGRIDC( , ) AND DIR( , , ).  (INPUT)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND
C                       IN GRIDC( , ) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  SEE LSTORE(10, ).  FOR THE
C                       COMPUTATION ROUTINES RETURNING A GRID, THIS
C                       VALUE MUST BE OUTPUT BY GFETCH.
C                       NSLAB = 0 FOR VECTOR DATA (AND FOR NO DATA
C                       RETURNED AND NWORDS = 0), AS STORED BY GSTORE;
C                       OTHERWISE, GRIDDED DATA ARE INDICATED.  (OUTPUT) 
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
C                       HAVE BEEN USED IN THIS RUN.
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL
C                       DATA ARE STORED ON DISK.  (OUTPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NFETCH = THE NUMBER OF TIMES GFETCH HAS BEEN ENTERED.  GFETCH
C                       KEEPS TRACK OF THIS AND RETURNS THE VALUE.  (OUTPUT)
C               LASTL = THE LAST LOCATION IN CORE( ) USED FOR MOS-2000 INTERNAL
C                       STORAGE.  INITIALIZED TO 0 ON FIRST ENTRY TO GSTORE.
C                       ALSO INITIALIZED IN U720 IN CASE GSTORE IS NOT ENTERED.
C                       MUST BE CARRIED WHENEVER GSTORE IS TO BE CALLED.
C                       (INPUT/OUTPUT)
C               LASTD = TOTAL NUMBER OF PHYSICAL RECORDS ON DISK FOR MOS-2000
C                       INTERNAL STORAGE.  MUST BE CARRIED WHENEVER GSTORE
C                       IS TO BE CALLED.  (INPUT)
C              NSTORE = THE NUMBER OF TIMES GSTORE HAS BEEN ENTERED.  GSTORE
C                       KEEPS TRACK OF THIS AND RETURNS THE VALUE.  (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS, EITHER 1 OR 2.
C                       (INPUT)
C               ISTAV = 1 WHEN THE DATA RETURNED ARE VECTOR (STATION)
C                         DATA.  
C                       0 WHEN THE DATA RETURNED ARE GRID DATA.
C                         THIS ESSENTIALLY TELLS U720 (PRED25/PRED26)
C                         HOW TO TREAT THE DATA, EVEN IF MISSING (9999)
C                        (OUTPUT)
C
C           WEATHER GRID INPUT/OUTPUT VARIABLES
C             JSTA(K) = ARRAY CONTAINING THE GRIDPOINT LOCATIONS OF
C                       THE STATIONS IN THE STATION LIST. 
C                       (K=1,NSTA) (INPUT)
C            NPCAT(J) = PRECIPITATION CATEGORY GRIDPOINT FORECAST FOR
C                       FIRST SUBKEY. IF NPCAT IS NON-MISSING AND
C                       NON-ZERO AT A GRIDPOINT, THEN THAT GRIDPOINT IS
C                       "WET" AND SHOULD HAVE A CHARACTER ASSIGNED TO
C                       IT. (J=1,ND2X3) (INPUT)
C            NXPCH(J) = PRECIPITATION CHARACTER CATEGORY GRIDPOINT
C                       FORECASTS. VALUES:
C                       MISSEL - MISSING
C                            1 - STRATIFORM (DEFAULT)
C                            2 - CONVECTIVE
C                            3 - DRIZZLE (NOT IMPLEMENTED
C                       (J=1,ND2X3) (OUTPUT)
C               RRMIS = MISSING DATA FLAG. (INPUT)
C              MISSEL = INTEGER VALUE DENOTING AN ELEMENT NOT FETCHED.
C                       (INPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        47 = DATA NOT RETURNED FROM GTVEGR.
C                       103 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED IN
C                             THIS ROUTINE.
C                       187 = PROJECTION NOT ACCOMMODATED IN THIS ROUTINE.
C                       SEE CALLED ROUTINES FOR OTHER VALUES.
C                       (INTERNAL-OUTPUT)
C
C            INTERNAL VARIABLES
C           ITABLE(I) = CCCFFF OF WX FORECASTS. (INTERNAL)
C
C        NONSYSTEM SUBROUTINES USED
C            NONE
C
C***********************************************************************
C
      IMPLICIT NONE
C
C        DECLARE MOS2K SYSTEM VARIABLES:
C 
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*20 NAME(ND1)
      CHARACTER*60 RACESS(5)
C
      INTEGER KFILDO,KFIL10,IP12,KFILRA,
     1        NUMRA,ID(4),IDPARS(15),JD(4),NDATE,
     2        ISDATA(ND1),ND1,NSTA,
     3        ND2X3,ICALLD(L3264W,ND5),IPACK(ND5),IWORK(ND5),
     4        ND5,NGRIDC(6,ND11),NGRID,ND11,
     5        NSLAB,LSTORE(12,ND9),ND9,LITEMS,ND10,
     6        NBLOCK,NFETCH,LASTL,LASTD,NSTORE,
     7        IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7),ND7,
     8        L3264B,L3264W,ISTAV,IER
C
      REAL    STALAT(ND1),STALON(ND1),SDATA(ND1),CORE(ND10),
     1        DIR(ND1,2,ND11)
C
C        DECLARE WEATHER GRID VARIABLES (INPUT/OUTPUT):
C
      INTEGER MISSEL,NPCAT(ND2X3),NXPCH(ND2X3),JSTA(NSTA)
C
      REAL    RRMIS
C
C        DECLARE WEATHER GRID VARIABLES (INTERNAL):
C
      INTEGER LDPARS(15),NSIZE,ISTOP,J,L
C
      INTEGER ITABLE(1) / 228500 /
C                           WX
C
C        INITIALIZE SOME CONSTANTS.
C
      IER=0
C
C***********************************************************************
C
C***D WRITE(KFILDO,100)(ID(J),J=1,4)
 100  FORMAT(' *********** IN WXPCHR *************'/' ',4I10)
C
C        VERIFY THE PROCESSING INDICATOR, IDPARS(1) AND IDPARS(2).
C
      IF(ITABLE(1).EQ.IDPARS(1)*1000+IDPARS(2).AND.
     1  (IDPARS(7).EQ.0.OR.IDPARS(7).EQ.1.OR.
     2   IDPARS(7).EQ.2.OR.IDPARS(7).EQ.3.OR.
     3   IDPARS(7).EQ.4.OR.IDPARS(7).EQ.5.OR.
     4   IDPARS(7).EQ.6.OR.IDPARS(7).EQ.7))GO TO 108
C
      WRITE(KFILDO,102)(ID(L),L=1,4)
  102 FORMAT(/,' ****WXPCHR ENTERED FOR VARIABLE',
     1        2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2        ' NOT ACCOMMODATED.')
      IER=103
      GO TO 340
C  
  108 CONTINUE
C
C        LOOP THROUGH ALL GRIDPOINTS
C
      DO 200 J=1,ND2X3
C
C        ...CHECK FOR VALID GRIDPOINT:
         IF(NPCAT(J).EQ.RRMIS)THEN
            NXPCH(J)=MISSEL
C
C        ...CHECK FOR DRY GRIDPOINT:
         ELSEIF(NPCAT(J).EQ.0)THEN
            NXPCH(J)=0
         ELSE
C
C        NOTE THAT THUNDER/SEVERE SUBROUTINES AFTER SUBROUTINE WXPCHR 
C        MAY CHANGE THIS VALUE TO 2 (CONVECTIVE). DRIZZLE IS NOT 
C        IMPLEMENTED IN THIS DEVELOPMENT.
            NXPCH(J)=1
C
C        END OF VALID/WET GRIDPOINT TEST
         ENDIF
C
 200  CONTINUE
      GO TO 350
C
C        MUST SET RETURNABLE ARRAY TO MISSING FOR SAFETY
C        WHEN DATA CANNOT BE RETURNED.
C
 340  DO 341 J=1,ND2X3
         NXPCH(J)=MISSEL
 341  CONTINUE
C
 350  RETURN
      END
