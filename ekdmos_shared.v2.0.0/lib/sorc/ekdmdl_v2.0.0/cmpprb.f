      SUBROUTINE CMPPRB(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JD,ITAU,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NCAT,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,ISTAB,IER)
C
C        JUNE      2000   RUDACK   TDL   MOS-2000
C        SEPTEMBER 2000   DALLAVALLE   COMMENTED OUT WRITE
C                                      STATEMENT REFERRING
C                                      TO FORMAT STATEMENT NO. 195.
C        FEBRUARY  2001   ALLEN        REWORKED SUBROUTINE:  CHANGED  
C                                      CONSISTENCY CHECK IDS; TOOK
C                                      OUT ABILITIY TO CHECK 24-H VALUES
C                                      AGAINST 6-H ONES;MADE IT SO
C                                      SUBROUTINE WOULD FIRST TRY TO
C                                      CHECK 24-H VALUES AGAINST 
C                                      CONSISTENCY CHECK'D 12-H 
C                                      (FFF=330/375), AND IF THEY DON'T 
C                                      EXIST, CHECK AGAINST MONOTONIC 
C                                      12-H'S(FFF=320/365) 
C        NOVEMBER  2002   HUGHES       UPDATED TO INCLUDE COMMENTS ABOUT
C                                      THE REMOVAL OF OBSOLETE IDS
C        SEPTEMBER 2005   COSGROVE     ADDED CONSISTENCY CHECK OF 6-HR
C                                      THUNDERSTORMS NOW THAT WE HAVE 3-HR
C                                      FORECASTS FOR THE 40KM.
C        JULY      2010   VEENHUIS     MADE CHANGES TO ITABLE TO ACCOMODATE
C                                      EKDMOS PQPF POST PROCESSSING. INCREASED
C                                      NDIM TO 9.
C
C        PURPOSE
C           SUBROUTINE CMPPRB ENSURES THAT THE 6/12/24 HOUR 
C           PROBABILITY FORECASTS ARE GREATER THAN OR EQUAL 
C           TO THE 3/6/12 HOUR PROBABILITY FORECASTS 
C           COVERING THE SAME 6/12/24 HOUR PERIOD. IN SOME CASES,
C           AN ATTEMPT IS FIRST MADE TO USE THE CONSISTENCY CHECKED
C           FORECASTS FROM THE COMPONENT PERIODS.  IF THESE
C           ARE NOT AVAILABLE (I.E. MEDIUM RANGE), THEN THE
C           'BACKUP' OF THE MONOTONIC IS USED. 
C
C            IDPARS(1) & IDPARS(2) OF THE PROBABILITIES
C            ARE MAPPED    INTO    FROM    USING     OR
C                          207230  207220  207120   ----
C                          207330  207320  207230  207220
C                          207430  207420  207330  207320
C                          207275  207265  207165   ----
C                          207375  207365  207275  207265
C                          207475  207465  207375  207365
C                          203330  203320  203220   ----
C                          203430  203420  203330  203320
C                          PROBLEM: 
C            IF THE DATUM = 9999 OR 9997, IT IS LEFT UNCHANGED.
C  
C            PRIOR TO MARCH 2001 IDPARS(1) AND IDPARS(2) WERE
C            WERE MAPPED    INTO    FROM       
C                          207322  207320     
C                          207422  207420    
C                          207423  207420   
C                          207367  207365  
C                          207467  207465 
C                          207468  207465
C                          203322  203320    
C                          203422  203420   
C                          203423  203420  
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
C             ID(J,L) = THE PREDICTOR IDS (J=1,4) (L=1,NCAT).  (INPUT)
C         IDPARS(J,L) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID( ) (J=1,15)
C                       (L=1,NCAT).  (INPUT)
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
C             JD(J,L) = THE BASIC INTEGER PREDICTOR IDS (J=1,4) (L=1,NCAT).
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
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
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
C            XDATA(K,J) = RETURNED CONSISTENT 12 OR 24 HOUR 
C                       PROBABILITY FORECAST (K=1,NSTA) (J=1,NCAT).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF XDATA( ).  (INPUT)
C                NCAT = NUMBER OF FORECAST CATEGORIES.  (INPUT)
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
C                              MOSTORE( , ).  LATER USED AS A WAY OF
C                              DETERMINING WHETHER TO KEEP THIS 
C                              VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN.  (INPUT)
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
C                       102 = ID NOT ACCOMMODATED.
C                       SEE RETVEC FOR OTHER VALUES.  (OUTPUT)
C         ITABLE(I,J) = I=1--VALUES OF CCCFFFB ACCOMMODATED 
C                       I=2--ID(1) FOR 12(24)H MONOTONIC FORECAST 
C                            TO BE CONSISTENCY CHECKED
C                       I=3--ID(1) FOR TWO 6(12)H FORECASTS USED TO 
C                            CHECK THE 12(24)H FORECAST SPECIFIED IN I=2.
C                       I=4--ID(1) FOR MONOTONIC 12H FORECASTS USED TO
C                            CHECK THE 24H FORECASTS.  THESE FORECASTS
C                            ARE ONLY USED WHEN THE FORECASTS SPECIFIED
C                            IN I=3 ARE NOT AVAILABLE.  THIS VALUE WILL
C                            BE SET TO MISSING FOR CASES WHERE THE
C                            ONLY CHOICE IS TO USE THE MONOTONIC FORECAST. 
C                       (J=1,NDIM).  (INTERNAL)
C               ISTAB = 1 SINCE THE DATA RETURNED ARE BINARY.  (OUTPUT)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       XDATA( ) (J=1,4).  (INTERNAL)
C           LDPARS(J) = PARSED VALUES CORRESPONDING TO LD( ) (J=1,15)
C                       (INTERNAL)
C                NDIM = SECOND DIMENSION OF ITABLE( , ).  SET BY
C                       PARAMETER.  (INTERNAL)
C               ISWAP = NUMBER OF TIMES PROBABILITY VALUES ARE SWAPPED. 
C                       (INTERNAL)
C             IFINISH = NUMBER OF PROBABILITIES WHICH ARE FETCHED BY RETVEC.
C                       (INTERNAL)
C               ITIME = INCREMENTAL TIME BY WHICH THE PROBABILITIES ARE 
C                       FETCHED. (INTERNAL)
C          YDATA(K,J) = AUTOMATIC WORK ARRAY (K=1,ND1) (J=1,NCAT).  (INTERNAL)
C
C        NONSYSTEM SUBROUTINES CALLED 
C            PRSID1, RETVEC
C
      PARAMETER(NDIM=9)
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION XDATA(ND1,NCAT),YDATA(ND1,NCAT)
C        YDATA( ) IS AN AUTOMATIC ARRAY.
      DIMENSION ISDATA(ND1),ID(4,NCAT),IDPARS(15,NCAT),JD(4,NCAT)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10),ICALLD(L3264W,ND5)
      DIMENSION LD(4),LDPARS(15),KFILRA(5)
      DIMENSION ITABLE(4,NDIM)
C               
C         DATA STATEMENT
C
      DATA ISWAP/0/
      DATA LDPARS/15*0/
C         FOR ITABLE, THE COLUMNS ARE:
C                  INTO    FROM  USING  'BACKUP'                 
      DATA ITABLE/2072301,207220,207120,999999,
     1            2073301,207320,207230,207220,
     2            2074301,207420,207330,207320,
     3            2072751,207265,207165,999999,
     4            2073751,207365,207275,207265,
     5            2074751,207465,207375,207365,
     6            2033301,203320,203220,999999,
     7            2034301,203420,203330,203320,
     8            2033351,203323,203223,999999/
C
      SAVE ISWAP
C                                                  
      IER=0
      ISTAB=1
C     
C        VERIFY THE PROCESSING INDICATOR, IDPARS(1).
C
      DO 100 JJ=1,NDIM
      IF(ITABLE(1,JJ).EQ.(IDPARS(1,1)*10000+IDPARS(2,1)*10+IDPARS(3,1))) 
     1 GOTO 108
 100  CONTINUE
C
      WRITE(KFILDO,107)(JD(L,1),L=1,4)
 107  FORMAT(/,' ****CMPPRB ENTERED FOR VARIABLE',
     1         2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2       ' NOT ACCOMMODATED.')
      IER=102
      GO TO 300
C
C        VERIFY THAT NCAT GT 0.
C
 108  IF(NCAT.LE.0)THEN
         WRITE(KFILDO,110)NCAT
 110     FORMAT(/,' ****NCAT =',I4,' NOT CORRECT IN CMPRB.')
         IER=102
         GO TO 300
      ENDIF
C
C        SET XDATA( ) AND YDATA( ) TO MISSING PRIOR TO ITS EVALUATION. 
C 
      DO 115 K=1,ND1
         DO 114 J=1,NCAT
            XDATA(K,J)=9999.
            YDATA(K,J)=9999.
 114     CONTINUE
 115  CONTINUE
C     
C        GET THE FOUR WORD ID FOR MONOTONIC PROBABILITY.
C
 120  LD(1)=ITABLE(2,JJ)*1000+IDPARS(3,1)*100+IDPARS(4,1) 
      LD(2)=JD(2,1)
      LD(3)=JD(3,1)
C
C        GET THE NCAT PROBABILITIES.
C         
      DO 140 J=1,NCAT
C
      LD(4)=ID(4,J)         
C            
C        FETCHING THE MONOTONIC PROBABILITY THAT WILL BE
C        CHECKED FOR CONSISTENCY.  NOTE THAT THIS IS STORED
C        IN XDATA.  THE WAY THIS ROUTINE IS CURRENTLY WRITTEN
C        IF YOU DON"T FIND THE COMPONENT PROBABILITIES IN THE
C        NEXT STEP, IT'LL ASSIGN THE MONOTONIC PROBABILITIES TO
C        THE CONSISTENCY-CHECKED ID.  WE LEFT IT THIS WAY TO
C        EXPLOIT THIS FACT FOR THE QPF PROBABILITIES IN THE 2005 
C        NEW MEX BUFR. 
C
      CALL PRSID1(KFILDO,LD,LDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            LD,LDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,XDATA(1,J),ND1,NSTA,
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
         WRITE(KFILDO,135)(LD(L),L=1,4)
 135     FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN CMPPRB',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         IER=0
         GO TO 300
      ENDIF 
C
 140  CONTINUE
C
C        DEFINE THE BEGININNG TIME OF THE 3/6/12 HOUR
C        PROBABILITIES WHICH ARE TO BE FETCHED AS WELL 
C        AS THE INCREMENT TIME BY WHICH THEY ARE TO BE
C        FETCHED.    
C
      ITIME=0
      IF((IDPARS(2,1).EQ.230).OR.(IDPARS(2,1).EQ.275))THEN
         LD(3)=LD(3)-3
         ITIME=3
      ELSEIF((IDPARS(2,1).EQ.330).OR.(IDPARS(2,1).EQ.375))THEN
         LD(3)=LD(3)-6
         ITIME=6
      ELSEIF((IDPARS(2,1).EQ.430).OR.(IDPARS(2,1).EQ.475))THEN
         LD(3)=LD(3)-12
         ITIME=12
      ENDIF
C
C        LOOP THROUGH THE FOLLOWING SECTION OF CODE TWICE, ONCE
C        FOR THE INITIAL 3/6/12H FORECAST, AND THEN AGAIN FOR
C        THE SECOND 3/6/12H FORECAST.  IF IT IS EVER NECESSARY TO
C        USE MORE THAN 2 FORECASTS TO CONSISTENCY CHECK A FORECAST
C        IFINISH CAN BE SET TO VALUES OTHER THAN 2
C
      IFINISH=2
      DO 250 N=1,IFINISH
C
C        GET THE FOUR WORD ID FOR THE PROBABILITY
C        AT THE BEGINNING OF THE PERIOD. 
C
      LD(1)=ITABLE(3,JJ)*1000+IDPARS(3,1)*100+IDPARS(4,1) 
C
      DO 170 J=1,NCAT
C
      LD(4)=ID(4,J)           
C                  
C        FETCHING THE COMPONENT PROBABILITIES.
C
      CALL PRSID1(KFILDO,LD,LDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            LD,LDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,YDATA(1,J),ND1,NSTA,
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
C
C        IF YOU ARE CONSISTENCY CHECKING A FORECAST, AND THE
C        FORECAST IN ITABLE(3,JJ) WAS MISSING, TRY FOR THE 
C        'BACKUP' FORECAST LISTED IN ITABLE(4,JJ).  NOTE: ONCE 
C        YOU HAVE MADE THIS SWITCH TO THE MONOTONIC FORECAST FOR THE
C        FIRST CATEGORY OF AN ID, ALL THE OTHER CATEGORIES WILL
C        JUST AUTOMATICALLY CHECK FOR THE 'BACKUP' FORECAST
C
        IF(ITABLE(4,JJ).NE.999999)THEN
         WRITE(KFILDO,161)NCAT-1
 161     FORMAT('CONSISTENCY CHECK NOT AVAILABLE;  CHECKING FOR ',
     &           'MONOTONIC FORECAST.  '/'IF FOUND, THE ',I2, 
     &           ' OTHER QPF PROBABILITIES FOR THIS CCCFFF AND ',
     &           'PROJ. WILL ALSO'/'USE MONOTONIC FORECASTS.')
         LD(1)=ITABLE(4,JJ)*1000+IDPARS(3,1)*100+IDPARS(4,1) 
C
C        TRY TO FETCH THE 'BACKUP' PROBABILITY.
C
          CALL PRSID1(KFILDO,LD,LDPARS)
          CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                LD,LDPARS,JD,ITAU,
     2                NDATE,MDATE,CCALL,ISDATA,YDATA(1,J),ND1,NSTA,
     3                ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                LSTORE,ND9,LITEMS,CORE,ND10,
     5                NBLOCK,NFETCH,
     6                IS0,IS1,IS2,IS4,ND7,
     7                L3264B,L3264W,IER)
          IF(IER.NE.0)THEN
            WRITE(KFILDO,162)(LD(L),L=1,4)
 162        FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN CMPPRB',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
            WRITE(KFILDO,1622)ITIME,(ID(L,J),L=1,4)
 1622       FORMAT(/,' ****NO',I3,' HR FORECASTS WERE FOUND.  ID', 
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,/,5X,
     2          'WILL BE FILLED WITH MONOTONIC FORECASTS.')
            IER=0
          ENDIF
        ELSE
         WRITE(KFILDO,163)(LD(L),L=1,4)
 163     FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN CMPPRB',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
            WRITE(KFILDO,1622)ITIME,(ID(L,J),L=1,4)
         IER=0
        ENDIF
      ENDIF
C
 170  CONTINUE
C          
      DO 200 K=1,NSTA
      DO 199 J=1,NCAT 
C
C           IF THE PROBABILITY COVERING THE ENTIRE PERIOD AND A 
C           PROBABILITY WITHIN THE PERIOD ARE BOTH AVAILABLE ENSURE THAT 
C           THE FORMER PROBABILITY IS AT LEAST EQUAL TO THE LATTER 
C           PROBABILITY.  IF NOT, SET THE LATTER PROBABILITY VALUE 
C           EQUAL TO THE FORMER PROBABILITY VALUE.             
C       
         IF((NINT(XDATA(K,J)).NE.9999.AND.NINT(XDATA(K,J)).NE.9997).AND.
     1      (NINT(YDATA(K,J)).NE.9999.AND.NINT(YDATA(K,J)).NE.9997))THEN
C
         IF(XDATA(K,J).LT.YDATA(K,J)) THEN
C
C           THE FOLLOWING WRITE STATEMENT WAS COMMENTED OUT PRIOR
C           TO THE SEPTEMBER 2000 IMPLEMENTATION.
C
C        WRITE(KFILDO,195)CCALL(K,1),XDATA(K,J),YDATA(K,J),LD(3),ISWAP+1
C195           FORMAT('STATION= ',A,2X,'12 HR PROB= ',F5.3,2X,
C    1                '6 HR PROB', ' = ',F5.3,4X,'6 HR PROJ =',I2,2X,
C    2                ' # OF TIMES SWAPPED = ',I5)
               XDATA(K,J)=YDATA(K,J)
               ISWAP=ISWAP+1
         ENDIF
C
         ENDIF
C
 199  CONTINUE
 200  CONTINUE
C
      LD(3)=LD(3)+ITIME

 250  CONTINUE
C
      GO TO 350              
C
C        THIS PREDICTOR CANNOT BE COMPUTED.  SET THE ARRAY TO MISSING.

 300  DO 310 K=1,NSTA
         DO 309 J=1,MAX(1,NCAT)
            XDATA(K,J)=9999.
 309     CONTINUE
 310  CONTINUE
C        
 350  RETURN
      END
