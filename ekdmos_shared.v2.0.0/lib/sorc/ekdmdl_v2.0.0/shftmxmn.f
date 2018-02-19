      SUBROUTINE SHFTMXMN(KFILDO,KFIL10,
     1                    ID,IDPARS,JD,NDATE,
     2                    KFILRA,RACESS,NUMRA,CCALL,ICALLD,
     3                    CCALLD,NAME,STALAT,STALON,
     4                    ISDATA,SDATA,DIR,ND1,NSTA,
     5                    NGRIDC,NGRID,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     6                    LSTORE,ND9,LITEMS,CORE,ND10,LASTL,
     7                    NBLOCK,LASTD,NSTORE,NFETCH,
     8                    IS0,IS1,IS2,IS4,ND7,
     9                    ND2X3,IP12,
     A                    ISTAV,L3264B,L3264W,IER)
C
C
C        MARCH    2005   CARROLL  MDL MOS-2000. 
C        HISTORY:   
C        05-03       CARROLL	THIS SUBROUTINE IS NEW TO U202.
C        03-19-07    COSGROVE   ADDED SNOWFALL SHIFT.
C
C        PURPOSE
C            TO SHIFT THE TAU'S OF THE DAYTIME MAX, NIGHTTIME MIN
C            OR SNOWFALL TO 00Z (MAX) OR 12Z (MIN).
C
C            THIS CODE WILL ACCOMODATE TDLPACKED SEQUENTIAL AND
C            RANDOM ACCESS FILES. 
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C                222 129 - TIME SHIFTED GRIDDED DAYTIME MAX.
C                222 229 - TIME SHIFTED GRIDDED NIGHTTIME MIN.
C                228 479 - TIME SHIFTED GRIDDED SNOWFALL AMOUNT.
C
C
C        DATA SET USE
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT(PRINT) FILE.
C                       (OUTPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                       ACCESS.(INPUT-OUTPUT)
C
C        VARIABLES
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST, EXCEPT POSSIBLY
C                       CCALLD( ).  EQUIVALENCED TO ICALL( , ).
C                       (CHARACTER*8)  (INPUT/OUTPUT)
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).  THIS LIST IS USED
C                       IN L1D1 TO READ THE REGION LISTS.  (CHARACTER*8)
C                       (INTERNAL)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE(,) (J=1,ND10).
C                       WHEN CORE() IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C             DATA( ) = THE GRIDDED DAYTIME MAX AND NIGHTTIME MIN FIELDS IN
C                       DEGREES KELVIN (OUTPUT)
C          DIR(K,J,M) = THE IX (J=1) AND JY (J=2) POSITIONS ON THE GRID
C                       FOR THE COMBINATION OF GRID CHARACTERISTICS M
C                       (M=1,NGRID) AND STATION K (K=1,NSTA) IN NGRIDC( ,M).
C                       (INPUT)
C                   I = LOOP CONTROL VARIABLE.  (INTERNAL)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN INTEGER
C                       VARIABLE (L=1,L3264W) (K=1,ND5).
C                       EQUIVALENCED TO CCALLD( ).  (INTERNAL)
C               ID(J) = THE PREDICTOR ID (J=1,4).  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID() (J=1,15).
C                       (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY
C                                      1 LAYER)
C                       J=7--LTLTLTLT (TOP OF LAYER)
C                       J=8--T (TRANSFORMATION)
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND 
C                                BACK IN TIME)
C                       J=10-OT (TIME APPLICATION)
C                       J=11-OH (TIME PERIOD IN HOURS)
C                       J=12-TAU (PROJECTION IN HOURS)
C                       J=13-I (INTERPOLATION TYPE)
C                       J=14-S (SMOOTHING INDICATOR)
C                       J=15-G (GRID INDICATOR)
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                        47 = DATA MISSING AFTER CALL TO GFETCH
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C                IP12 = INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE INPUT VECTOR FILES WILL BE WRITTEN
C                       TO UNIT IP12.  (INPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5). (INTERNAL)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).
C                       (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).
C                       (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       IS2(3) AND IS2(4) ARE USED BY THE CALLING
C                       PROGRAM AS THE GRID DIMENSIONS.
C                       (INTERNAL-OUTPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).
C                       (INTERNAL)
C               ISTAV = 0 SINCE THE DATA RETURNED ARE GRID DATA.
C                       (OUTPUT)
C           ISDATA(K) = WORK ARRAY (K=1,ND1).  (INTERNAL)
C         ITABLE(I,J) = 1ST VALUE IS THE CCCFFF OF THE VARIABLE RETURNED. 
C                       2ND VALUE IS THE CCCFFF OF THE VARIABLE CONVERTED.
C                       (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5). (INTERNAL)
C                   J = LOOP CONTROL VARIABLE. (INTERNAL)
C                  JJ = LOOP CONTROL VARIABLE. (INTERNAL)
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT
C                       THE PORTIONS PERTAINING TO PROCESSING
C                       ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       ID() IS USED TO HELP IDENTIFY THE BASIC MODEL
C                       FIELDS AS READ FROM THE ARCHIVE.  (INPUT)
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT)
C           KFILRA(J) = HOLDS THE UNIT NUMBERS FOR ACCESSING THE MOS-2000
C                       EXTERNAL RANDOM ACCESS FILES (J=1,NUMRA).  (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C                   L = LOOP CONTROL VARIABLE. (INTERNAL)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS, EITHER 1 OR 2.  (INPUT)
C               LASTD = TOTAL NUMBER OF PHYSICAL RECORDS ON DISK FOR MOS-2000
C                       INTERNAL STORAGE.  MUST BE CARRIED WHENEVER GSTORE
C                       IS TO BE CALLED.  (INPUT)
C               LASTL = THE LAST LOCATION IN CORE( ) USED FOR MOS-2000 INTERNAL
C                       STORAGE.  INITIALIZED TO 0 ON FIRST ENTRY TO GSTORE.
C                       ALSO INITIALIZED IN U201 IN CASE GSTORE IS NOT ENTERED.
C                       MUST BE CARRIED WHENEVER GSTORE IS TO BE CALLED.
C                       (INPUT/OUTPUT)
C               LD(J) = THE PREDICTOR ID (J=1,4).  (INTERNAL)
C           LDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID( ) (J=1,15).  (INTERNAL)
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
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE(,)
C                       THAT HAVE BEEN USED IN THIS RUN.  (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE
C                       DATA STORED (L=1,12) (J=1,LITEMS).
C                       (INPUT-OUTPUT)
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE() WHERE
C                              THE DATA START.  WHEN ON DISK,
C                              THIS IS MINUS THE RECORD NUMBER WHERE
C                              THE DATA START.
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDL GRIB, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --NUMBER OF THE SLAB IN DIR(, ,L) AND
C                              IN NGRIDC(,L) DEFINING THE
C                              CHARACTERISTICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID(,N) (N=1,NPRED) FOR WHICH
C                              THIS VARIABLE IS NEEDED, WHEN IT IS
C                              NEEDED ONLY ONCE FROM LSTORE(,).
C                              WHEN IT IS NEEDED MORE THAN ONCE, THE 
C                              VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING
C                              MSTORE(,). LATER USED AS A WAY OF
C                              DETERMINING WHETHER TO KEEP THIS
C                              VARIABLE.
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 WHEN DATA ARE NOT PACKED.  (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 WHEN DATA ARE NOT PACKED.  (INTERNAL)
C             NAME(K) = NAMES OF STATIONS (K=1,NSTA).  USED FOR PRINTOUT
C                       ONLY.  (CHARACTER*20)  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  
C                       ALL WORK ARRAYS ARE DIMENSIONED ND2X3. (INPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       FIRST DIMENSION OF DIR( , , ).  (INPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND DATA( ). 
C                       (INPUT)
C                 ND7 = DIMENSION OF IS0(),IS1(),IS2(), AND IS4().
C                       NOT ALL LOCATIONS ARE USED. (INPUT)
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C                ND10 = DIMENSION OF CORE().  (INPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN
C                       BE DEALT WITH ON THIS RUN.  LAST DIMENSION 
C                       OF NGRIDC(,).  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C                NDIM = SECOND DIMENSION OF ITABLE( , ).  SET BY
C                       PARAMETER.  (INTERNAL)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING COUNT FROM THE BEGINNING OF
C                       THE PROGRAM.  THIS COUNT IS MAINTAINED IN
C                       CASE THE USER NEEDS IT(DIAGNOSTICS, ETC.).
C                       (OUTPUT)
C               NGRID = THE NUMBER OF GRID COMBINATIONS IN DIR( , , ),
C                       MAXIMUM OF ND11.  (INPUT)
C         NGRIDC(L,M) = HOLDS THE GRID CHARACTERISTICS (L=1,6) FOR
C                       EACH GRID COMBINATION.  (M=1,NGRID).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=
C                            POLAR STEREOGRAPHIC).
C                       L=2--GRID LENGTH IN METERS.
C                       L=3--LATITUDE AT WHICH THE GRID LENGTH IS
C                            CORRECT *1000.
C                       L=4--GRID ORIENTATION IN DEGREES * 1000.
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *1000.
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES
C                            *1000.
C               NPACK = 2 FOR TDL GRIB PACKED DATA; 1 FOR NOT PACKED
C                       THIS IS RETURNED FROM GFETCH.  (INTERNAL)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND
C                       IN NGRIDC( , ) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  (OUTPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT WITH.
C                       (INPUT)
C              NSTORE = THE NUMBER OF TIMES GSTORE HAS BEEN ENTERED.  GSTORE
C                       KEEPS TRACK OF THIS AND RETURNS THE VALUE.  (OUTPUT)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE,
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS 
C                       STORED IN LSTORE(9,).  (INTERNAL)
C               NUMRA = THE NUMBER OF UNIT NUMBERS AND NAMES IN KFILRA( )
C                       AND RACESS( ).  (INPUT)
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA().  THIS 
C                       IS RETURNED FROM GFETCH.  (INTERNAL)
C           RACESS(J) = THE FILE NAMES CORRESPONDING TO KFILRA(J) (J=1,NUMRA).
C                       (CHARACTER*60)  (INPUT)
C            SDATA(K) = INTERPOLATED DATA TO RETURN, WHEN STATION DATA ARE
C                       BEING GENERATED (K=1,NSTA).  (OUTPUT)
C           STALAT(K) = LATITUDE OF STATIONS (K=1,NSTA).  (INPUT)
C           STALON(K) = LONGITUDE OF STATIONS (K=1,NSTA).  (INPUT)
C        1         2         3         4         5         6         7 X
C
C     NON-SYSTEM SUBROUTINES USED
C         GFETCH,PRSID1,CONST1
C
      IMPLICIT NONE
C
      INTEGER NDIM
      PARAMETER (NDIM=3)
C
      CHARACTER*60 RACESS(NUMRA)
      CHARACTER*8 CCALL(ND1,6),CCALLD(ND5)
      CHARACTER*20 NAME(ND1)
C
      INTEGER ID(4),IDPARS(15),JD(4),LD(4),LDPARS(15)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER STALAT(ND1),STALON(ND1),
     1        ISDATA(ND1),SDATA(ND1)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER KFILRA(NUMRA),ICALLD(L3264W,ND5)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER IER,ISTAV,I,J,JJ,L,
     1        KFILDO,KFIL10,NDATE,L3264B,LITEMS,
     2        MISSP,MISSS,LASTL,LASTD,NX,NY,NCYC,
     3        NBLOCK,ND2X3,ND1,NSTA,ND5,ND7,ND9,ND10,ND11,
     4        NSTORE,NFETCH,NGRID,NPACK,NSLAB,
     5        NTIMES,NWORDS,NUMRA,IP12,L3264W
      INTEGER ITABLE(2,NDIM)
C
      REAL DATA(ND5)
      REAL FD1(ND2X3)
      REAL CORE(ND10)
      REAL DIR(ND1,2,ND11)
C
C        DATA TABLE
C
      DATA ITABLE/222129, 222124,
     2            222229, 222224,
     3            228479, 228474/
C
C
      IER=0
      ISTAV=0
C
C        MAKE SURE THE REQUESTED VARIABLE IS A DAYTIME MAX OR NIGHTTIME
C        MIN OR SNOWFALL GRIDDED FIELD IN DEGREES K OR METERS.
C
C        FIND CCCFFF OF ID(1) IN ITABLE(1, ).
C
      DO 105 JJ=1,NDIM
         IF(ITABLE(1,JJ).EQ.IDPARS(1)*1000+IDPARS(2)) GOTO 120
 105  CONTINUE
C
      WRITE(KFILDO,110)(JD(L),L=1,4)
 110  FORMAT(/,' ****SHIFTMXMN ENTERED FOR VARIABLE',
     1         2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2       ' NOT ACCOMMODATED.')
      IER=102
      GO TO 800
C
 120  NCYC=NDATE-(NDATE/100)*100
C
C        MAKE SURE THE VALID TIME OF THE MODEL RUN IS 00, 06, 12, OR 18Z.
C
      IF(NCYC.NE.00.AND.NCYC.NE.06.AND.NCYC.NE.12.AND.NCYC.NE.18) THEN
         WRITE(KFILDO,125) NCYC
 125     FORMAT(/,' ****SHFTMXMN ENTERED WITH A MODEL CYCLE OF ',I3,'.',
     1          ' THIS IS A NON MODEL RUN TIME AND IT IS PRUDENT',
     2          ' TO EXIT.',/,'     STOP IN SHFTMXMN AT 125.')
         IER=102
         GO TO 800
      ENDIF
C
      IF(MOD(NCYC+IDPARS(12),12).EQ.0) THEN
C
C        FILL LD( ) AND LDPARS( ) AND GET THE MAX/MIN TEMPERATURE IN KELVIN.
C        ADD 6 TO JD(3) TO ADJUST FETCH THE FIELD PRE SHIFT.
C
         LD(1)=ITABLE(2,JJ)*1000+IDPARS(4)
         LD(2)=JD(2)
         LD(3)=JD(3)+6
         LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
         CALL PRSID1(KFILDO,LD,LDPARS)
C
C        CALL TO GFETCH TO GET SEQUENTIAL DATA IF NEEDED.
C
         CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1               IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,DATA,ND2X3,
     2               NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3               NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,
     4               1,IER)
C
C        IF DATA ARE MISSING (IER=47) THEN CALL CONST1 TO LOOK FOR
C        DATA IN THE RANDOM ACCESS FILE.
C
         IF(IER.EQ.47) THEN
            GOTO 200
C
C        IF IER=0, NO ERROR IS OUT OF GFETCH; FOUND DATA CAN BAIL
C
         ELSEIF(IER.EQ.0) THEN
            GOTO 900
         ELSE
            GOTO 800
         ENDIF
C
C       CALL TO CONST1 FOR RANDOM ACCESS FILE.
C
 200     CALL CONST1(KFILDO,KFIL10,IP12,LD,LDPARS,LD,NDATE,
     1               KFILRA,RACESS,NUMRA,CCALL,ICALLD,CCALLD,
     2               NAME,STALAT,STALON,ISDATA,SDATA,DIR,
     3               ND1,NSTA,NGRIDC,NGRID,ND11,NSLAB,
     4               IPACK,IWORK,DATA,ND5,LSTORE,ND9,LITEMS,CORE,
     5               ND10,LASTL,NBLOCK,LASTD,NSTORE,NFETCH,
     6               IS0,IS1,IS2,IS4,ND7,
     7               ISTAV,L3264B,L3264W,IER)
C
C        IF THERE IS AN ERROR, GRACEFULLY EXIT.
C
         IF(IER.NE.0) GOTO 800
C
C       IF NO ERROR, MAX OR MIN GUIDANCE HAS BEEN FOUND AND IS STORED
C       IN THE DATA ARRAY.  CAN BAIL.
C
         GO TO 900
       ELSE
          WRITE(KFILDO,700) JD(3)
 700      FORMAT(/,' ****SHFTMXMN ENTERED WITH AN ID TAU OF ',I3,
     1           ' WHICH IS NOT DIVISIBLE BY 12 AS A SHIFTED',
     2           ' MAX OR MIN SHOULD BE.')
          IER=102
        ENDIF
C
C         IF THERE WAS A PROBLEM IN GFETCH OR CONST1 IT WOULD COME
C         HERE TO SET THE DATA TO MISSING.
C
 800   DO I=1,ND2X3
         DATA(I)=9999.
       ENDDO
C
 900  RETURN
      END
