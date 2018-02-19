      SUBROUTINE OBSTCLD(KFILDO,KFIL10,ID,IDPARS,JD,NDATE,
     1                   CCALL,STALAT,STALON,KSTOP,SDATA,ND1,NSTA,
     2                   IPACK,IWORK,FD1,FD2,FD3,ND2X3,
     3                   LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     4                   IS0,IS1,IS2,IS4,ND7,
     5                   ISTAV,L3264B,IER)
C
C        FEBRUARY 1999   WEISS   TDL   MOS-2000
C        AUGUST   2001   WEISS   TEMPORARY CHANGES TO OBSTCLD.F
C                                FIRST, STATIONS KALB, KTLH, KJAX,
C                                KBDL, AND KFWA HAVE BEEN TAKEN OFF
C                                THE FAA MANUAL COMPLEMENTING LIST.
C                                SECOND, THE ALASKA EXCEPTION FOR
C                                SCP COMPLEMENTING HAS BEEN CHANGED
C                                TO "ONLY" CHECK PRIMARY STATION
C                                LINK. ALSO, STATION PFYU IS INCLUDED.
C        OCTOBER  2001   WEISS   IF STATEMENTS CONTAINING REAL VALUES
C                                ARE CHANGED TO INTEGER VALUES.
C        OCTOBER  2001   WEISS   THE ALASKA EXCEPTION FOR SCP
C                                COMPLEMENTING (TO ONLY CHECK THE
C                                PRIMARY STATION LINK) HAS BEEN MADE
C                                PERMANENT. THE FAA EXCEPTION TO SCP
C                                COMPLEMENTING (CONUS STATIONS ONLY)
C                                HAS BEEN TAKEN OUT. MANUAL
C                                COMPLEMENTING FOR THESE STATIONS WAS
C                                SCHEDULED TO BE DISCONTINUED
C                                STARTING IN JULY 2001.
C        MARCH    2002   WEISS   THE THIRD WORD DEFINITION FOR VARIOUS
C                                INPUT VARIABLES HAS BEEN CORRECTED
C                                SO THAT IF RR OR TAU ARE USED IN THE
C                                FUTURE, THE THIRD WORD DEFINITION
C                                WILL BE CORRECT.
C        OCTOBER  2002   WEISS   CHANGED ND5 TO ND2X3
C        MARCH    2003   WEISS   DURING SCP COMPLEMENTING, IF THE 
C                                ASOS/MANUAL OBSERVATION IS FEW(2)
C                                AND THE SCP OBSERVATION IS SCT(3)
C                                THE ADJUSTED OBSERVATION REMAINS 
C                                FEW.
C        APRIL    2003   GLAHN   WHITE SPACE; REORDERED CALL
C        MAY      2003   GLAHN   MODIFIED DIAGNOSTICS, REORDERED TYPE
C                                STATEMENTS; REMOVED SETTING ARRAYS
C                                TO 9999 AFTER BAD RETURN FROM GFETCH;
C                                KSTOP( ) PUT IN CALL; CHANGED NWORDS
C                                TO NSTA IN LOOPS
C        JUNE     2003   GLAHN   RESTRUCTURED TESTS ON IER AND IENTER
C                                TO PROCEED FOR DAY 1 EVEN THOUGH GOES
C                                IS MISSING; RETURN WITH IER = 47 WHEN
C                                BOTH GOES EAST AND WEST ARE MISSING
C        OCTOBER  2003   SMB     CORRECTED FORMAT STATEMENT NUMBER 202
C                                FOR THE IBM
C        DECEMBER 2003   WEISS   INSERTED GO TO 800 FOLLOWING SCP 
C                                READ FOR FIRST PROCESS DATE IF IER
C                                NE TO ZERO. TOOK OUT STATEMENTS TO NOT
C                                MAKE ANY CALCULATIONS IF SCP EAST AND
C                                WEST ARE BOTH MISSING. MANY STATIONS 
C                                STILL DO NOT RELY ON SCP. 
C                                INITIALIZATION OF WORK ARRAYS HAS
C                                BEEN ADDED. 
C
C        PURPOSE 
C            THIS SUBROUTINE WILL COMPUTE THE AMOUNT OF TOTAL CLOUD
C            COVERAGE FROM OBSERVATION VECTOR DATA. TOTAL CLOUD WILL
C            BE ESTIMATED FROM BOTH SURFACE OBSERVATIONS AND 
C            SATELLITE CLOUD PRODUCT (SCP) GENERATED FROM GOES.
C            FOR DATES PRECEDING 1995090100, NO SCP DATA ARE 
C            AVAILABLE. TOTAL CLOUD WILL BE EQUIVALENT TO THE VALUES 
C            OF OLD SURFACE TOTAL CLOUD (708310) READ FROM ARCHIVE
C            EXCEPT, THE CODED VALUES (PRE-METAR) WILL BE CONVERTED 
C            TO METAR CODED VALUES (SEE TABLE BELOW). FOR THE
C            PERIOD 1995090100 - 1996083123, OLD SURFACE TOTAL
C            CLOUD WILL BE CONVERTED TO METAR CODED VALUES AND THEN
C            ALL STATIONS WILL BE COMPLEMENTED WITH AVAILABLE SCP 
C            VALUES, BUT ONLY IF SCP DATA ARE NOT MISSING (SEE *). 
C            FOR THE PERIOD 1996120100 AND LATER, TOTAL CLOUD
C            WILL ESTIMATED FROM METAR CLOUD COVERAGE AND SCP VALUES.
C            STATION TYPE WILL DETERMINE WHETHER SCP COMPLEMENTING
C            IS NECESSARY (SEE **). ALSO NOTE THAT ALASKA STATIONS 
C            WILL BE EXEMPT SCP COMPLEMENTING.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C               708 312 - TOTAL CLOUD COVERAGE        
C
C           *  STATION TYPE INFORMATION IS NOT AVAILABLE FOR THE 
C              PERIOD 1995090100 - 1996083123, THEREFORE, SCP 
C              COMPLEMENTING IS CONDUCTED FOR STATIONS WITH
C              AVAILABLE SCP DATA. DUE TO LIMITED SCP SAMPLING 
C              DURING THIS PERIOD, SCP COMPLEMENTING WILL ONLY BE 
C              DONE IF SCP VALUES ARE NON-MISSING (COMPLEMENTED
C              SURFACE TOTAL CLOUD WILL NOT BE DECLARED MISSING 
C              IF SCP VALUES ARE MISSING).
C
C           ** STATION TYPE IS AVAILABLE, THEREFORE ONLY STATIONS
C              REQUIRING SCP DATA WILL BE COMPLEMENTED. COMPLEMENTED
C              SURFACE TOTAL CLOUD WILL BE DECLARED MISSING IF SCP
C              VALUES ARE MISSING.
C
C           ***RETURN WITH IER = 47 WHEN:
C              1)  SURFACE TYPE DATA NOT FOUND WHEN NDATE =
C                  12/01/96 00Z AND LATER
C              2)  BOTH GOES EAST AND GOES WEST ARE MISSING WHEN
C                  NDATE = 9/01/95 00Z AND LATER
C              3)  ANY CLOUD LAYER MISSING FOR NDATE =
C                  12/01/96 00Z AND LATER
C              4)  TOTAL CLOUD MISSING FOR NDATE PRIOR TO 
C                  12/01/96 00Z
C 
C           THE FOLLOWING CODED VALUES AND THEIR ASSOCIATED CLOUD 
C           COVERAGE ARE LISTED AS FOLLOWS:
C
C        SKY COVERAGE    | CLOUD COVERAGE  | CODED VALUE | OLD CODED
C                        |                 |   (METAR)   | VALUES
C                        |                 |             | (PRE-METAR)
C        CLEAR (ASOS)    |      = 0        |      0      |   0
C        CLEAR (MANUAL)  |      = 0        |  0 FROM 1   | 0 OR 1 
C        FEW             |    > 0 - <= 2/8 |      2      |
C        SCATTERED       | >= 3/8 - <= 4/8 |      3      |   2,5
C        BROKEN          | >= 5/8 - < 8/8  |      6      |   3,6
C        OVERCAST        |      = 8/8      |      8      |   4,7
C        TOT OBSCURATION | ASSUME 8/8      |     10      |   8
C
C        FOR PARTIAL OBSCURATION, CLOUD COVERAGE WILL BE DETERMINED 
C        FROM CLOUDS OBSERVED ABOVE THE PARTIALLY OBSCURED LAYER.
C
C
C        DATA SET USE 
C
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE 
C                      (OUTPUT). 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                      (INPUT-OUTPUT). 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                       (INPUT). 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM 
C                       ACCESS (INPUT-OUTPUT).
C               ID(J) = THE VARIABLE ID (J=1,4) (INPUT).
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID( ) (J=1,15)
C                       (INPUT).
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 
C                            LAYER),
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
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE 
C                       PORTIONS PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND THRESH.
C                       JD( ) IS USED TO IDENTIFY THE BASIC MODEL 
C                       FIELDS AS READ FROM THE ARCHIVE (INPUT).
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED
C                       (INPUT).
C            SDATA(K) = DATA TO RETURN (K=1,ND1).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                       WITH (INPUT).
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INTERNAL)
C           STALAT(K) = LATITUDE OF STATIONS (K=1,ND1) (INPUT).
C           STALON(K) = LONGITUDE OF STATIONS (K=1,ND1) (INPUT).
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND
C                       IN AN INPUT DIRECTORY (K=1,ND1).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST, EXCEPT POSSIBLY
C                       CCALLD( ) AND CCALLP( ).  EQUIVALENCED TO
C                       ICALL( , , ).  (CHARACTER*8)  (INPUT/OUTPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).
C            IWORK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).
C       FD1(J),FD2(J) = WORK ARRAYS (J=1,ND2X3).
C              FD3(J)   FD1 = STATION TYPE (EX. MANUAL VS ASOS).
C                       FD2 = THE CLOUD COVERAGE DERIVED FROM THE
C                             SATELLITE CLOUD PRODUCT (SCP).
C                       FD3 = ASOS CLOUD COVERAGE.
C               ND2X3 = DIMENSION OF IPACK( ), IWORK( ), FD1( ),
C                       FD2( ) AND FD3( ) (INPUT).
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS), (INPUT-OUTPUT).
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK, 
C                              THIS IS MINUS THE RECORD NUMBER WHERE 
C                              THE DATA START.
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDL GRIB, 1 FOR 
C                              NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN 
C                              RETRIEVED.
C                       L=10 --NUMBER OF THE SLAB IN DIR( , ,L) AND
C                              IN NGRIDC( ,L) DEFINING THE 
C                              CHARACTERISTICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE 
C                              SORTED LIST IN ID( ,N) (N=1,NPRED)
C                              FOR WHICH THIS VARIABLE IS NEEDED, WHEN 
C                              IT IS NEEDED ONLY ONCE FROM 
C                              LSTORE( , ).  WHEN IT IS NEEDED MORE
C                              THAN ONCE, THE VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING 
C                              MSTORE( , ). LATER USED AS A WAY OF 
C                              DETERMINING WHETHER TO KEEP THIS 
C                              VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ), (INPUT). 
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN (INPUT).  
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA 
C                       IDENTIFIED IN LSTORE( , ) (J=1,ND10). WHEN
C                       CORE( ) IS FULL DATA ARE STORED ON DISK
C                       (OUTPUT).
C                ND10 = DIMENSION OF CORE( ), (INPUT).
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE (INPUT).  
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING  COUNT FROM THE BEGINNING OF 
C                       OF THE PROGRAM. THIS COUNT IS MAINTAINED IN 
C                       CASE THE USER NEEDS IT (DIAGNOSTICS, ETC.). 
C                       NEEDS IT (DIAGNOSTICS, ETC.), (INTERNAL).  
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3)
C                       (INTERNAL).
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+)  
C                       (INTERNAL).
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12)
C                       (INTERNAL).
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4)
C                       (INTERNAL).
C                 ND7 = DIMENSION OF IS0, IS1, IS2, AND IS4. NOT ALL
C                       LOCATIONS ARE USED (INPUT).
C               ISTAV = 1 SINCE THE DATA RETURNED ARE STATION DATA
C                       (OUTPUT).
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64) (INPUT).
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        -1 = PROBLEM WITH PROCESSING DATES
C                        52 = NWORDS RETURNED FROM GFETCH NOT EQUAL
C                             TO NSTA
C                       103 = ROUTINE ENTERED WITH WRONG IDS
C                       SEE GFETCH FOR VALUES.  (INTERNAL-OUTPUT)
C
C        ADDITIONAL VARIABLES
C               ACALL = STATION CALL LETTER (CHARACTER*8) USED TO 
C                       CHECK FOR ALASKA STATIONS FOR SCP EXEMPTION
C                       (INTERNAL).
C            ASOS_MAN = LOGICAL VARIABLE USED TO DECLARE ASOS STATIONS
C                       WITH NO SCP AUGMENTATION (INTERNAL). 
C            ASOS_SCP = LOGICAL VARIABLE USED TO DECLARE ASOS STATIONS
C                       WITH SCP AUGMENTATION (INTERNAL).
C                   I = COUNTER (INTERNAL).
C        ID_CLDCOV(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD3( ) "CLOUD COVERAGE", WHERE J=1,4 (INTERNAL).
C         ID_OLDTC(J) = HOLDS THE 4 ID WORDS OF THE OLD TOTAL CLOUD
C                       COVERAGE RETRIEVED FOR FOR NDATES OCCURRING 
C                       BEFORE DEC. 1, 1996 (00Z), WHERE J=1,4 
C                       (INTERNAL). 
C          ID_SCPE(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD2( ) SCP CLOUD COVERAGE (GOES EAST), WHERE 
C                       J=1,4 (INTERNAL).
C          ID_SCPW(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD3( ) SCP CLOUD COVERAGE (GOES WEST), WHERE
C                       J=1,4 (INTERNAL).
C        ID_STATYP(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD1( ) "STATION TYPE", ITERATED OVER 6 POSSIBLE
C                       LAYERS, WHERE J-1,4 (INTERNAL).
C              IENTER = THE NUMBER OF TIMES THIS SUBROUTINE IS ENTERED
C                       DURING A U201 RUN (INTERNAL). 
C              ILEVEL = COUNTER FOR LEVELS 1 - 6 OF CLOUD COVERAGE
C                       (INTERNAL).
C            IOLDTCLD = INTEGER VALUE OF OLD SURFACE TOTAL CLOUD
C                       (INTERNAL).
C                   J = COUNTER (INTERNAL).
C                   K = COUNTER (INTERNAL). 
C            KSTOP(I) = FLAG VALUES TO DETERMINE IF PROCESSING FOR A
C                       GIVEN STATION HAS BEEN COMPLETED:
C                       = 0 PROCESSING INCOMPLETE
C                       = 1 PROCESSING COMPLETE
C               MISSP = PRIMARY MISSING VALUE INDICATOR. RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED (INTERNAL). 
C               MISSS = SECONDARY MISSING VALUE INDICATOR. RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED (INTERNAL).
C                   N = COUNTER (INTERNAL).
C             NEWPROC = LOGICAL USED TO CHOOSE TOTAL CLOUD PROCESSING  
C                       FOR 12/1/1996 (00Z) AND LATER (INTERNAL).
C               NPACK = 2 FOR TDL GRIB PACKED DATA: 1 FOR NOT PACKED.
C                       THIS IS STORED IN LSTORE(7, ) (INTERNAL).
C               NSLAB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE FIRST FIELD.  THIS IS THE
C                       VALUE OF NSLAB RETURNED.  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED (OUTPUT).
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT
C                       THE RECORD HAS BEEN FETCHED. THIS IS STORED
C                       IN LSTORE(9, ) (INTERNAL).
C              NWORDS = NUMBER OF WORDS RETURNED IN FD1( ), FD2( ) OR 
C                       FD3( ) (INTERNAL).
C            OLDPROCA = LOGICAL USED TO CHOOSE TOTAL CLOUD PROCESSING
C                       PRIOR TO 09/01/1995 (00Z) (INTERNAL).
C            OLDPROCB = LOGICAL USED TO CHOOSE TOTAL CLOUD PROCESSING
C                       FROM 09/01/1995 (00Z) THROUGH 11/30/1996 (23Z)
C                       (INTERNAL). 
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH
C
      IMPLICIT NONE
C
      CHARACTER*8 CCALL(ND1,6),ACALL
C
      LOGICAL OLDPROCA,OLDPROCB,NEWPROC
      LOGICAL ASOS_MAN,ASOS_SCP
C
      INTEGER ID(4),IDPARS(15),JD(4)
      INTEGER KSTOP(ND1)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER IPACK(ND2X3),IWORK(ND2X3)
      INTEGER LSTORE(12,ND9)
      INTEGER ID_STATYP(4),ID_SCPE(4),ID_SCPW(4),ID_CLDCOV(4),
     *        ID_OLDTC(4)
      INTEGER KFILDO,KFIL10,ND9,LITEMS,ND10,NDATE,ND1,NSTA,
     1        ND2X3,NBLOCK,NFETCH,ISTAV,L3264B,IER,ND7,
     2        I,J,K,N,ILEVEL,NTIMES,NWORDS,IENTER,NPACK,NSLAB,
     3        MISSP,MISSS,IOLDTCLD,ICLOUD_TYPE,JCLOUD_TYPE,
     4        ISTAT_TYPE,ISCP_TYPE
C
      REAL SDATA(ND1),STALAT(ND1),STALON(ND1)
      REAL CORE(ND10)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3)
C
      DATA IENTER/0/
      DATA OLDPROCA/.FALSE./,
     1     OLDPROCB/.FALSE./,
     2     NEWPROC/.FALSE./
C
      SAVE IENTER,OLDPROCA,OLDPROCB,NEWPROC
C
      IER=0
      ISTAV=1
C
C        STEP 1A. VERIFY THE PROCESSING OF TOTAL CLOUD COVERAGE
      IF(JD(1).NE.708312000) THEN
        WRITE(KFILDO,150)(JD(J),J=1,4)
 150    FORMAT(/' ****OBSTCLD ENTERED FOR INCORRECT PREDICTOR',
     1          I9.9,2I10.9,I4.3)
        IER=103
        GO TO 800
      ENDIF
C
C        STEP 1B. INITIALIZATION.
C
      IENTER=IENTER+1
C        INCREMENT IENTER VARIABLE BY 1.
C
      DO 156 J=1,ND2X3
        FD2(J)=9999.
        FD3(J)=9999.
 156  CONTINUE
C
      DO 160 J=1,ND1
        SDATA(J)=9999.
	KSTOP(J)=0
 160  CONTINUE
C
C        STEP 1C. DETERMINE OLD OR NEW DATA PROCESSING BASED
C        ON THE CUTOFF DECEMBER 1, 1996 (00Z).
C
      IF(IENTER.EQ.1) THEN
        IF(NDATE.LT.1995090100) OLDPROCA=.TRUE.
        IF((NDATE.GE.1995090100).AND.(NDATE.LT.1996120100))
     *     OLDPROCB=.TRUE.
        IF(NDATE.GE.1996120100) NEWPROC=.TRUE.
      ENDIF
C
      IF(OLDPROCA.AND.(NDATE.GE.1995090100)) THEN
        WRITE(KFILDO,170) NDATE
 170    FORMAT(/,' ****ERROR IN OBSTCLD, ATTEMPTING OLDPROCA',
     1          ' PROCESSING ',I12,' > 1995083123',
     2         /,'     NO PHYSICAL RECORD IS WRITTEN FOR THIS',
     3          ' DATE.')
        IER=-1
        GO TO 800
      ELSEIF((OLDPROCB).AND.((NDATE.LT.1995090100).OR.
     *       (NDATE.GE.1996120100)))THEN
        WRITE(KFILDO,175) NDATE
 175    FORMAT(/' ****ERROR IN OBSTCLD, ATTEMPTING OLDPROCB',
     1          ' PROCESSING DATE ',I12,' < 1995090100 OR > 1996113023',
     2         /'     NO PHYSICAL RECORD IS WRITTEN FOR THIS DATE.')
        IER=-1
        GO TO 800
      ENDIF
C
C        (IF(NEWPROC.AND.(NDATE.LT.1996120100)) THEN)
C        IS NOT NECESSARY SINCE DATPRO WITHIN U201 WILL FLAG
C        DATES LESS THAN THE PRECEDING DATE.
C 
C        STEP 1D. CONSTRUCT 4 WORD ID'S
C        CONSTRUCT THE STATION TYPE ARRAY
C
      ID_STATYP(1)=700002000
      ID_STATYP(2)=IDPARS(7)
      ID_STATYP(3)=IDPARS(9)*1000000+IDPARS(12)
      ID_STATYP(4)=0
C
C        CONSTRUCT THE SCP (GOES EAST) CLOUD COVERAGE ARRAY
C        (ID_SCPE) FOUR WORD ID
C
      ID_SCPE(1)=708350000
      ID_SCPE(2)=IDPARS(7)
      ID_SCPE(3)=IDPARS(9)*1000000+IDPARS(12)
      ID_SCPE(4)=0
C
C        CONSTRUCT THE SCP (GOES WEST) CLOUD COVERAGE ARRAY
C        (ID_SCPW) FOUR WORD ID 
C
      ID_SCPW(1)=708351000
      ID_SCPW(2)=IDPARS(7)
      ID_SCPW(3)=IDPARS(9)*1000000+IDPARS(12)
      ID_SCPW(4)=0
C
C        CONSTRUCT THE CLOUD COVERAGE ARRAY FOR LEVEL #1
C
      ID_CLDCOV(1)=708320000
      ID_CLDCOV(2)=IDPARS(7)
      ID_CLDCOV(3)=IDPARS(9)*1000000+IDPARS(12)
      ID_CLDCOV(4)=0
C
C        CONSTRUCT THE "OLD" TOTAL CLOUD ARRAY
C
      ID_OLDTC(1)=708310000 
      ID_OLDTC(2)=IDPARS(7)
      ID_OLDTC(3)=IDPARS(9)*1000000+IDPARS(12)
      ID_OLDTC(4)=0
C
C        STEP 2. FETCH STATION TYPE (FD1)
C
      IF(NEWPROC) THEN
        CALL GFETCH(KFILDO,KFIL10,ID_STATYP,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4              IER)
C
        IF(IER.NE.0)THEN
C
          IF(IENTER.EQ.1) THEN
             WRITE(KFILDO,202) (JD(J),J=1,4),IER
 202         FORMAT(/' ****ERROR IN OBSTCLD, FOR FIRST PROCESS DATE,',
     1               ' STATION TYPE IS MISSING.  POSSIBLE DATA GAP.',
     2              /'     ALL VALUES OF TOTAL',
     3               ' CLOUD ARE MISSING',
     4               ' FOR VARIABLE',I9.9,2I10.9,I4.3,' IER =',I5)
           ENDIF
C
           GO TO 800
C             STATION TYPE MUST BE PRESENT.  IF IT IS NOT THERE FOR DAY 1,
C             IT WILL NOT BE SAVED FOR FUTURE DAYS.  NO REASON TO PROCEED.
        ELSE
C
           IF(NWORDS.NE.NSTA) THEN
              WRITE(KFILDO,205)NWORDS,NSTA
 205          FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1                 ' NOT EQUAL TO NSTA =',I6,' FOR STATION TYPE',
     2                 ' IN OBSTCLD.  DATA SET TO MISSING.')
              IER=52
              GO TO 800
           ENDIF
C
        ENDIF
C
C        STEP 2A. MODIFY STATION TYPE ARRAY (FD1) FOR ASOS STATION
C        WITHOUT SCP AUGMENTATION TO THE VALUE OF 10. (ALASKA 
C        STATIONS ONLY).
C
 206    DO 212 K=1,NSTA
C
          DO 210 N=1,6
            ACALL=CCALL(K,N)
C
C              ALASKA TEST
C
            IF(N.EQ.1) THEN
C
              IF(ACALL(1:2).EQ.'PA') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
              IF(ACALL.EQ.'PFYU    ') THEN
                FD1(K)=10.
                GO TO 212
              ENDIF
C
            ENDIF
C
CCC            OLD ALASKA TEST
CCC         IF(ACALL(1:2).EQ.'PA') THEN
CCC           FD1(K)=10.
CCC           GO TO 212
CCC         ENDIF
C
 210      CONTINUE
C
 212    CONTINUE
C
      ENDIF
C
C
C        STEP 3. FETCH SCP GOES EAST (FD2) AND GOES WEST (FD3)
C        CLOUD COVERAGE AND MERGE INTO FD2.
C
      IF((NEWPROC).OR.(OLDPROCB)) THEN
C
C        STEP 3A. FETCH SCP (GOES EAST) CLOUD COVERAGE INTO FD2
C
        CALL GFETCH(KFILDO,KFIL10,ID_SCPE,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4              IER)
C
        IF(IER.NE.0)THEN
C
           IF(IENTER.EQ.1) THEN
              WRITE(KFILDO,230)(JD(J),J=1,4),IER
 230          FORMAT(/' ****ERROR IN OBSTCLD, FOR FIRST PROCESS DATE,',
     1                ' SCP (GOES EAST) LEVEL IS MISSING.',
     2                '  POSSIBLE DATA GAP.',
     3               /'     ALL VALUES OF TOTAL CLOUD ARE MISSING',
     4                ' FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER =',I4)
              GO TO 800
C
C               MASS STORAGE MUST BE SETUP PROPERLY ON FIRST 
C               PROCESS DATE
           ELSEIF(IENTER.GT.1)THEN
             IF(IER.NE.47) GO TO 800
C          
C               LET IT PROCEED SO THAT CLOUD OBS RECORDS CAN BE
C               ACCESSED TO MARK FOR FUTURE ACCESS.  GOES EAST COULD
C               BE MISSING AND GOES WEST PRESENT.
           ENDIF
C
        ELSE
C
          IF(NWORDS.NE.NSTA) THEN
            WRITE(KFILDO,235)NWORDS,NSTA
 235        FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1              ' NOT EQUAL TO NSTA =',I6,
     2             /'     FOR SCP CLOUD COVERAGE',
     2              ' (GOES EAST) IN OBSTCLD.  DATA SET TO MISSING.')
            IER=52
            GO TO 800
C
          ENDIF
C
        ENDIF
C 
C         NOTE THAT CONTROL COMES HERE WHEN IER = 47 FROM GFETCH.  
C         THIS MEANS MISSING GOES DATA IN FD2( ), WHICH IS ACCOMMODATED.
C
C         STEP 3B. FETCH SCP (GOES WEST) CLOUD COVERAGE INTO FD3
C
        CALL GFETCH(KFILDO,KFIL10,ID_SCPW,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4              IER)
C
        IF(IER.NE.0)THEN
C        
           IF(IENTER.EQ.1) THEN
              WRITE(KFILDO,245) (JD(J),J=1,4),IER
 245          FORMAT(/' ****ERROR IN OBSTCLD, FOR FIRST PROCESS DATE,',
     1                ' SCP (GOES WEST) LEVEL IS MISSING.',
     2                '  POSSIBLE DATA GAP.',
     3               /'     ALL VALUES OF TOTAL CLOUD ARE MISSING',
     4                ' FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER =',I4)

              go to 800
C               MASS STORAGE MUST BE SETUP PROPERLY ON FIRST 
C               PROCESS DATE
           ELSEIF(IENTER.GT.1)THEN
             IF(IER.NE.47) GO TO 800
C          
C               LET IT PROCEED SO THAT CLOUD OBS RECORDS CAN BE
C               ACCESSED TO MARK FOR FUTURE ACCESS.  GOES WEST COULD
C               BE MISSING AND GOES EAST PRESENT.
           ENDIF
C
        ELSE
C
          IF(NWORDS.NE.NSTA) THEN
            WRITE(KFILDO,250)NWORDS,NSTA
 250        FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1              ' NOT EQUAL TO NSTA =',I6,
     2             /'     FOR SCP CLOUD COVERAGE',
     2              ' (GOES WEST) IN OBSTCLD.  DATA SET TO MISSING.')
            IER=52
            GO TO 800
          ENDIF
C
        ENDIF
C
C######################################################################
C
C         STEP 3C. DETERMINE SCP CLOUD COVERAGE FROM THE EAST (FD2)
C                  AND THE WEST (FD3). THE CRITERIA IS AS FOLLOWS:
C                  (FINAL TALLY PUT INTO FD2)
C                  1. WEST OF 105 WEST, GOES WEST IS SELECTED. IF
C                     GOES WEST IS MISSING, USE GOES EAST.
C                  2. EAST OF 105 WEST, GOES EAST IS SELECTED. IF
C                     GOES EAST IS MISSING, USE GOES WEST.
C
        DO 300 J=1,NSTA
C
D         WRITE(KFILDO,298)J,STALON(J),FD2(J),FD3(J)
D298      FORMAT(' AT 298--J,STALON(J),FD2(J),FD3(J)',I4,3F10.4)
          IF(STALON(J).GT.105.) THEN
            IF(NINT(FD3(J)).NE.9999) FD2(J)=FD3(J)
          ELSEIF(STALON(J).LE.105.) THEN
            IF(NINT(FD2(J)).EQ.9999) FD2(J)=FD3(J)
          ENDIF
D         WRITE(KFILDO,299)J,STALON(J),FD2(J),FD3(J)
D299      FORMAT(' AT 299--J,STALON(J),FD2(J),FD3(J)',I4,3F10.4)
C
C        SPECIAL CHECK: MISSING HOURS CONTAIN ONE STATION, USUALLY 
C        EITHER ABE OR ABQ WITH A VALUE OF -9.0.
C
C          IF(FD2(J).EQ.-9.) FD2(J)=9999.
 300    CONTINUE 
C
      ENDIF
C
C        STEP 4. FETCH CLOUD COVERAGE FOR LEVELS 1 - 6 (USING FD3).
C                WORD #1 OF ID_CLDCOV CHANGE FOR LEVELS 2 - 6.
C                AND ESTIMATE TOTAL CLOUD (NEWPROC)
C
      IF(NEWPROC) THEN
C
        DO 400 ILEVEL=1,6
          IF(ILEVEL.GE.2) ID_CLDCOV(1)=ID_CLDCOV(1)+2000
C           FETCH CLOUD COVERAGE
          CALL GFETCH(KFILDO,KFIL10,ID_CLDCOV,7777,LSTORE,ND9,LITEMS,
     1                IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2                NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3                NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4                IER)
C
          IF(IER.NE.0)THEN
C
             IF(IENTER.EQ.1)THEN
               WRITE(KFILDO,320) ILEVEL,(JD(J),J=1,4),IER
 320           FORMAT(/' ****ERROR IN OBSTCLD, FOR FIRST PROCESS DATE,',
     1                 ' CLOUD AMOUNT FOR LEVEL ',I2,' IS MISSING',
     2                 ' POSSIBLE DATA GAP.',
     3                /'     ALL VALUES OF TOTAL CLOUD ARE MISSING',
     4                 ' FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER =',I4)
             ENDIF
C
             GO TO 800
C               IF A CLOUD LAYER IS MISSING, MIGHT AS WELL QUIT.
          ELSE
C
             IF(NWORDS.NE.NSTA) THEN
               WRITE(KFILDO,325)NWORDS,NSTA,ILEVEL
 325           FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1                 ' NOT EQUAL TO NSTA =',I6,
     2                 /'     FOR CLOUD COVERAGE',
     3                  ' (LEVEL ',I1,')',' IN OBSTCLD.  DATA SET',
     4                  ' TO MISSING.')
               IER=52
               GO TO 800
             ENDIF
C
          ENDIF
C
C            STEP 4A. DETERMINE TOTAL CLOUD COVERAGE FOR EACH STATION
C                   BY DETERMINING THE HIGHEST (LARGEST) CLOUD
C                     COVERAGE FOR SIX POSSIBLE LAYERS.
C
C        *****  MAIN DO LOOP  ***** 
C
          DO 350 J=1,NSTA
	    ISTAT_TYPE=NINT(FD1(J))
	    ICLOUD_TYPE=NINT(FD3(J))
C
            IF(KSTOP(J).EQ.0) THEN
	      ASOS_MAN=.FALSE.
	      ASOS_SCP=.FALSE.
C
C                1. MANUAL STATIONS (STATION TYPE = 1) OR 
C                   ASOS WITH NO SCP AUGMENTATION (STATION TYPE = 10).
C
              IF(((ISTAT_TYPE.EQ.1).OR.(ISTAT_TYPE.EQ.10)).AND.
     *          (NINT(FD3(J)).NE.9999)) ASOS_MAN=.TRUE.    
C
C                2. ASOS WITH SCP AUGMENTATION (STATION TYPE = 2 - 5)
C
              IF(((ISTAT_TYPE.GE.2).AND.(ISTAT_TYPE.LE.5)).AND.
     *          ((NINT(FD2(J)).NE.9999).AND.(NINT(FD3(J)).NE.9999)))
     *          ASOS_SCP=.TRUE.
C
C                STEP 4B. PROCESS LEVEL #1 
C
D          WRITE(KFILDO,326)J,ISTAT_TYPE,ICLOUD_TYPE,ASOS_SCP,ASOS_MAN,
D    1                      FD2(J),FD3(J),(FD1(N),N=1,4)
D326       FORMAT(' AT 326--J,ISTAT_TYPE,ICLOUD_TYPE,ASOS_SCP,ASOS_MAN',
D    1            'FD2(J),FD3(J),(FD1(N),N=1,4)',
D    2                5I6,6F10.4)
              IF(ILEVEL.EQ.1)THEN
C
	        IF((ASOS_MAN).OR.(ASOS_SCP)) THEN
C                   TOTAL OBSCURATION OR OVERCAST
C
                  IF((ICLOUD_TYPE.EQ.10).OR.(ICLOUD_TYPE.EQ.8)) THEN
                    SDATA(J)=FD3(J)
                    KSTOP(J)=1
C                      PARTIAL OBSCURATION: SDATA SET TO 0 TEMPORARILY
                  ELSEIF(ICLOUD_TYPE.EQ.9) THEN
	            SDATA(J)=0.
C                      CLEAR
                  ELSEIF((ICLOUD_TYPE.EQ.0).OR.(ICLOUD_TYPE.EQ.1)) THEN
                    SDATA(J)=0.
C                      IF CLEAR AND MANUAL (POSSIBLE CLOUD LAYERS ABOVE
C                      ASOS CLEAR)
	            IF(ISTAT_TYPE.EQ.1) KSTOP(J)=1
C                      FEW, SCATTERED OR BROKEN
                  ELSEIF((ICLOUD_TYPE.GE.2).OR.(ICLOUD_TYPE.LE.6)) THEN
                     SDATA(J)=FD3(J)
                  ENDIF
C
                ELSE
	          KSTOP(J)=1
                ENDIF
C
              ENDIF
C
C        STEP 4C. PROCESS LEVELS 2 - 6
C
	      IF(ILEVEL.GE.2)THEN
	        JCLOUD_TYPE=NINT(SDATA(J))
C
	        IF((ASOS_MAN).OR.(ASOS_SCP)) THEN
C
C                   FEW, SCATTERED, BROKEN OR OVERCAST
C
                  IF(ICLOUD_TYPE.GE.JCLOUD_TYPE) THEN
C
	            SDATA(J)=FD3(J)
		    IF(ICLOUD_TYPE.EQ.8) KSTOP(J)=1
                  ENDIF
C
                ELSE
	          KSTOP(J)=1
                ENDIF
C
              ENDIF
C
            ENDIF
C
C        STEP 4D. ASOS WITH SCP AUGMENTATION
C
	    IF(ILEVEL.EQ.6) THEN
D           WRITE(KFILDO,348)ILEVEL,J,KSTOP(J),ISTAT_TYPE,
D    1                       FD2(J),FD3(J),SDATA(J)
D348        FORMAT(' 348--ILEVEL,J,KSTOP(J),ISTAT_TYPE,',
D    1             'FD2(J),FD3(J),SDATA(J)',4I4,3F10.4)
C
	      IF((ISTAT_TYPE.GE.2).AND.(ISTAT_TYPE.LE.5))THEN
C
                IF((NINT(SDATA(J)).NE.9999).AND.
     *            (NINT(FD2(J)).NE.9999)) THEN
C
                  ISCP_TYPE=NINT(FD2(J))
                  JCLOUD_TYPE=NINT(SDATA(J))
C
                  IF(ISCP_TYPE.GT.JCLOUD_TYPE)THEN
                    SDATA(J)=FD2(J)
                    IF((ISCP_TYPE.EQ.3).AND.
     *                     (JCLOUD_TYPE.EQ.2))SDATA(J)=JCLOUD_TYPE
                  ENDIF
C
                ENDIF
C
              ENDIF
C
            ENDIF
C
D           WRITE(KFILDO,349)ILEVEL,J,KSTOP(J),FD2(J),FD3(J),
D    1                       SDATA(J)
D349        FORMAT(' 349--ILEVEL,J,KSTOP(J),FD2(J),FD3(J),SDATA(J)',
D    1             3I4,3F10.4)
 350      CONTINUE
C
C        ***** END OF MAIN DO LOOP *****
C
 400    CONTINUE
C
C        STEP 5. IF NDATE BEFORE 1996120100 THEN READ IN 
C        SFC TOTAL CLOUD (708310000) TO ESTIMATE TOTAL CLOUD FOR
C        OLDPROCA AND OLDPROCB PROCESSING.
C
      ELSEIF((OLDPROCA).OR.(OLDPROCB)) THEN
        CALL GFETCH(KFILDO,KFIL10,ID_OLDTC,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4              IER)
C
        IF(IER.NE.0)THEN
C
          IF(IENTER.EQ.1) THEN
            WRITE(KFILDO,500) NDATE,(JD(J),J=1,4),IER
 500        FORMAT(/,' ****ERROR IN OBSTCLD, FOR FIRST PROCESS DATE,',
     1             ' SFC TOTAL CLOUD IS MISSING FOR DATE=',I12,
     3             /,'     POSSIBLE DATA GAP.  ALL VALUES OF TOTAL',
     3               ' CLOUD ARE MISSING,',
     4               ' FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER =',I4)
          ENDIF
C
          GO TO 800
C           IF A CLOUD LAYER IS MISSING, MIGHT AS WELL QUIT.
        ELSE
C
          IF(NWORDS.NE.NSTA) THEN
            WRITE(KFILDO,510)NWORDS,NSTA
 510        FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1             ' NOT EQUAL TO NSTA =',I6,' FOR SFC TOTAL CLOUD',
     2             ' IN OBSTCLD.  DATA SET TO MISSING.')
            IER=52
            GO TO 800
          ENDIF
C
        ENDIF
C
C         STEP 5A. FOR OLDPROCA SIMPLY COPY ARRAY FD3 INTO ARRAY 
C         SDATA (SFC TOTAL CLOUD  =  TOTAL CLOUD)
C         (1995083123 AND EARLIER)
C
        IF(OLDPROCA) THEN
C
          DO 550 J=1,NSTA
            IOLDTCLD=NINT(FD3(J))
C
            IF((IOLDTCLD.EQ.0).OR.(IOLDTCLD.EQ.1)) THEN
              SDATA(J)=0.
            ELSEIF((IOLDTCLD.EQ.2).OR.(IOLDTCLD.EQ.5)) THEN
              SDATA(J)=3.
            ELSEIF((IOLDTCLD.EQ.3).OR.(IOLDTCLD.EQ.6)) THEN
              SDATA(J)=6.
            ELSEIF((IOLDTCLD.EQ.4).OR.(IOLDTCLD.EQ.7)) THEN
              SDATA(J)=8.
            ELSEIF(IOLDTCLD.EQ.8) THEN
              SDATA(J)=10.
            ELSE
              SDATA(J)=9999.
            ENDIF
C
 550      CONTINUE
C
        ENDIF
C
C         STEP 5B. FOR OLDPROCB COMPLEMENT SFC TOTAL CLOUD WITH 
C         SCP CLOUD COVERAGE AND COPY INTO SDATA
C         (1995090100 - 1996113023)
C         FD3 = SFC TOTAL CLOUD    FD2 = SCP CLOUD
C
	IF(OLDPROCB) THEN
C
          DO 560 J=1,NSTA
            IOLDTCLD=NINT(FD3(J))
            IF((IOLDTCLD.EQ.0).OR.(IOLDTCLD.EQ.1)) THEN
              FD3(J)=0.
            ELSEIF((IOLDTCLD.EQ.2).OR.(IOLDTCLD.EQ.5)) THEN
              FD3(J)=3.
            ELSEIF((IOLDTCLD.EQ.3).OR.(IOLDTCLD.EQ.6)) THEN
              FD3(J)=6.
            ELSEIF((IOLDTCLD.EQ.4).OR.(IOLDTCLD.EQ.7)) THEN
              FD3(J)=8.
            ELSEIF(IOLDTCLD.EQ.8) THEN
              FD3(J)=10.
            ELSE
              FD3(J)=9999.
            ENDIF
C
 560      CONTINUE
C
          DO 570 J=1,NSTA
C
            IF(NINT(FD3(J)).NE.9999) THEN
C
              IF(NINT(FD2(J)).NE.9999) THEN
                ISCP_TYPE=NINT(FD2(J))
                JCLOUD_TYPE=NINT(FD3(J))
C
                IF(ISCP_TYPE.GT.JCLOUD_TYPE) THEN
                  SDATA(J)=FD2(J)
                ELSE
                  SDATA(J)=FD3(J)
                ENDIF
C
              ELSEIF(NINT(FD2(J)).EQ.9999) THEN
                SDATA(J)=FD3(J)
              ENDIF
C
            ENDIF
C
 570      CONTINUE
        ENDIF
C
      ENDIF
C
      GO TO 850 
C
C        PREMATURE TERMINATION OF SUBROUTINE.
C
 800  DO 810 I=1,ND1
        SDATA(I)=9999.
 810  CONTINUE
C
C     WRITE(KFILDO,815) IER,(JD(J),J=1,4)
C815  FORMAT(/' ****ERROR IN OBSTCLD, IER =',I5,' FOR VARIABLE',4I12)
C
 850  RETURN
      END
