      SUBROUTINE AVGPRB(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JD,ITAU,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NCAT,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,ISTAB,IER)
C 
C        MAY       2008   WIEDENFELD   MDL   MOS-2000
C        OCTOBER   2008   WIEDENFELD   MDL   INPUT ID TO BE CONSISTENT WITH
C                                            DEVELOPMENTAL PROCESS.
C                                            CLEANED UP COMMENTS.
C
C        PURPOSE:
C            TO FIND THE MEAN OF EACH CATEGORY OF ENSEMBLE MOS 
C            FORECAST.  CURRENTLY SETUP TO RUN FOR 21 MEMBERS.
C            DO 130 WILL NEED TO CHANGE IF MORE MEMBERS ARE ADDED.
C            ONCE THE MEAN IS FOUND AVGPRB WILL ALSO INSURE MONOTONIC 
C            BEHAVIOR FOR A SET OF CUMULATIVE PROBABILITIES 
C            CUMULATIVE FROM ABOVE AND GOING FROM THE COMMON EVENT 
C            TO THE RARE EVENT.  WRITTEN ESPECIALLY FOR PRECIP AMOUNT.
C            AT THIS TIME, AVGPRB ALSO PERFORMS TRUNCATION OF PROBABILITY 
C            FORECASTS LIKE POP, TSTM, AND POPO 
C
C            IDPARS(1) & IDPARS(2) & ID(2) 003000000 OF PROBABILITIES  ARE MAPPED
C                          INTO    FROM
C                          203220  203220
C                          
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
C                       CCALLD( ).  (CHARACTER*8)  (INPUT)
C           ISDATA(K) = WORK ARRAY (K=1,ND1).  (INTERNAL)
C          XDATA(K,L) = COMPUTED VARIABLE IS RETURNED IN XDATA( , )
C                       (K=1,NSTA) (L=1,NCAT).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF ISDATA( ) AND FIRST DIMENSION 
C                       OF XDATA( , ) .  (INPUT)
C                NCAT = NUMBER OF FORECAST CATEGORIES.  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN 
C                       INTEGER VARIABLE (L=1,L3264W) (K=1,ND5).
C                       THIS ARRAY IS USED TO READ THE STATION 
C                       DIRECTORY FROM A MOS-2000 EXTERNAL FILE.
C                       EQUIVALENCED TO CCALLD( ).  (CHARACTER*8) 
C                       (INTERNAL)
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5). 
C                       EQUIVALENCED TO ICALLD( , ).  (INTERNAL)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(J) = WORK ARRAY (J=1,ND5).  (NOT ACTUALLY USED.)
C                       (INTERNAL)
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
C               ISTAB = 1 SINCE THE DATA RETURNED ARE BINARY.  (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).  
C                       CALCULATED BY PARAMETER, BASED ON L3464B.
C                       (INPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       102 = ID NOT ACCOMMODATED OR NCAT LE 0.
C                       SEE GFETCH FOR OTHER VALUES.  (OUTPUT)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       XDATA( ) (J=1,4).  (INTERNAL)
C           LDPARS(J) = PARSED VALUES CORRESPONDING TO LD( ) (J=1,15)
C                       (INTERNAL)
C           ITABLE(I) = ID'S OF CCCFFF FOR OUTPUT (I=1,NDIM).  (INTERNAL)
C           JTABLE(I) = ID'S OF PRECIPITATION PROBABILITIES (I=1,NDIM).  
C                       (INTERNAL)
C                NDIM = NUMBER OF POSSIBLE ID'S FOR PROCESS IN ITABLE( ).  
C                       SET BY PARAMETER.  (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            RETVEC, PRSID1
C
      PARAMETER(NDIM=1)
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION XDATA(ND1,NCAT),ISDATA(ND1),SUMPRB(ND1)
      DIMENSION ID(4,NCAT),IDPARS(15,NCAT),JD(4,NCAT)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10),ICALLD(L3264W,ND5)
      DIMENSION LD(4),LDPARS(15),KFILRA(5)
      DIMENSION ITABLE(NDIM),JTABLE(NDIM)
C     
      DATA ITABLE/2032201/
      DATA JTABLE/203220/
C
      IER=0
      ISTAB=1
C      
C        FIND THE ID(1) IN ITABLE(1).
C
      DO 105 JJ=1,NDIM      
      IF(ITABLE(JJ).EQ.(IDPARS(1,1)*10000+IDPARS(2,1)*10+IDPARS(3,1)))
     1 GO TO 108
 105  CONTINUE
C     
      WRITE(KFILDO,107)(JD(L,1),L=1,4)
 107  FORMAT(/,' ****AVGPRB ENTERED FOR VARIABLE',
     1        2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2        ' NOT ACCOMMODATED.')
      IER=102
      GO TO 300
C
C        VERIFY THAT NCAT GT 0.
C
 108  IF(NCAT.LE.0)THEN
         WRITE(KFILDO,110)NCAT
 110     FORMAT(/,' ****NCAT =',I4,' NOT CORRECT IN AVGPRB.')
         IER=102
         GO TO 300
      ENDIF
C
C        GET THE NCAT PROBABILITIES 
C    
      DO 150 J=1,NCAT
         MEMCNT=0
         NER=1
         DO 120 N=1,ND1
            SUMPRB(N)=0.
 120     CONTINUE
C
C        GET THE MEMBERS OF EACH CATEGORY
C    
         DO 130 IDD=40,60
         LD(1)=JTABLE(JJ)*1000+IDPARS(3,1)*100+IDD
         LD(2)=JD(2,1)-3000000
         LD(3)=JD(3,1)
         LD(4)=ID(4,J)
C
         CALL PRSID1(KFILDO,LD,LDPARS)
         CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1               LD,LDPARS,JD,ITAU,
     2               NDATE,MDATE,CCALL,ISDATA,XDATA(1,J),ND1,NSTA,
     3               ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4               LSTORE,ND9,LITEMS,CORE,ND10,
     5               NBLOCK,NFETCH,
     6               IS0,IS1,IS2,IS4,ND7,
     7               L3264B,L3264W,IER)
         IF(IER.NE.0)THEN
            GOTO 135
         ELSE
            NER=0
            MEMCNT=MEMCNT+1
            DO 128 K=1,NSTA
               IF(NINT(XDATA(K,J)).NE.9997.AND.
     1            NINT(XDATA(K,J)).NE.9999.AND.
     2            NINT(SUMPRB(K)).NE.9997.AND.
     3            NINT(SUMPRB(K)).NE.9999)THEN
                  SUMPRB(K)=XDATA(K,J)+SUMPRB(K)
               ELSE
                  SUMPRB(K)=XDATA(K,J)
               ENDIF
 128        CONTINUE
         ENDIF
 130     CONTINUE
C
C           JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C           IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C
 135     IF(IER.NE.0.AND.NER.NE.0)THEN
            WRITE(KFILDO,137)(LD(L),L=1,4)
 137        FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN AVGPRB',
     1               2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
            GO TO 300
         ELSE
            IER=0
         ENDIF
C
C           AVERAGE ALL MEMBERS FOR CURRENT CATEGORY.
C 
         DO 140 K=1,NSTA
            IF(NINT(SUMPRB(K)).NE.9997.AND.
     1         NINT(SUMPRB(K)).NE.9999)THEN
               XDATA(K,J)=SUMPRB(K)/FLOAT(MEMCNT)
            ELSE
               XDATA(K,J)=SUMPRB(K)
            ENDIF
 140     CONTINUE
C         
C           RETURN TO NEW CATEGORY LEVEL.
C
 150     CONTINUE
C 
C        ENSURING THAT THE PROBABILITIES ARE GREATER THAN OR EQUAL
C        TO ZERO AND LESS THAN OR EQUAL TO ONE.  IN ADDITION, THE QPF 
C        PROBABILITIES ARE CHECKED TO ENSURE MONOTONIC BEHAVIOR.  
C        NOTE THAT THIS ROUTINE WILL WORK WITH NCAT = 1; IN THAT
C        CASE, THE PROBABILITIES ARE ADJUSTED TO LIE IN THE
C        RANGE 0 TO 1, AND THE CHECKING ACROSS CATEGORIES
C        IS NOT NEEDED OR POSSIBLE.      
C
C        CHECKING FOR CUMULATIVE FROM BELOW FORECASTS CURRENTLY
C        NOT NECCESSARY, BUT WAS LEFT IN FOR CONSISTENSY  WITH MONPRB.
C
      IF(ITABLE(JJ).EQ.2081202.OR.
     1   ITABLE(JJ).EQ.2081302.OR.
     2   ITABLE(JJ).EQ.2080702.OR. 
     3   ITABLE(JJ).EQ.2081552) GOTO 195
C
      DO 190 J=1,NCAT              
         DO 189 K=1,NSTA   
C
C              A FORECAST OF 9997 IS TREATED AS 0.
C
            IF(NINT(XDATA(K,J)).EQ.9997) XDATA(K,J)=0.
C
            IF(NINT(XDATA(K,J)).EQ.9999) GO TO 189
C
C              MAKE SURE THE PROB BETWEEN 0 AND 1.
C
            IF(XDATA(K,J).LT.0.) THEN 
               XDATA(K,J)=0.
            ELSEIF(XDATA(K,J).GT.1.) THEN 
               XDATA(K,J)=1.
            ENDIF               
C
 189     CONTINUE
 190  CONTINUE
C
C        WHEN THERE IS ONLY ONE CATEGORY SKIP THE MONOTONIC CHECK.        
C
      IF((NCAT-1).EQ.0) GOTO 348   
C
C        MONOTONIC CHECK.
C 
      DO 193 J=1,NCAT-1
         DO 192 K=1,NSTA
C
            IF(NINT(XDATA(K,J+1)).NE.9999) THEN
               IF(XDATA(K,J+1).GT.XDATA(K,J)) THEN
                  XDATA(K,J+1)=XDATA(K,J)
               ENDIF
            ENDIF
C
 192     CONTINUE
 193  CONTINUE
C
      GOTO 348 
C
 195  DO 200 J=1,NCAT              
         DO 199 K=1,NSTA   
C
C              A FORECAST OF 9997 IS TREATED AS 0.
C
            IF(NINT(XDATA(K,J)).EQ.9997) XDATA(K,J)=0.
C
            IF(NINT(XDATA(K,J)).EQ.9999) GO TO 199
C
C              MAKE SURE THE PROB BETWEEN 0 AND 1.
C
            IF(XDATA(K,J).LT.0.) THEN 
               XDATA(K,J)=0.
            ELSEIF(XDATA(K,J).GT.1.) THEN 
               XDATA(K,J)=1.
            ENDIF               
C
 199     CONTINUE
 200  CONTINUE
C
C        MONOTONIC CHECK.
C
      DO 205 J=NCAT,2,-1              
         DO 206 K=1,NSTA
C 
            IF(NINT(XDATA(K,J-1)).NE.9999) THEN
               IF(XDATA(K,J-1).GT.XDATA(K,J)) THEN
                  XDATA(K,J-1)=XDATA(K,J)
               ENDIF
            ENDIF
C
 206     CONTINUE
 205  CONTINUE 
C
      GO TO 348   
C     
C        THIS PREDICTOR CANNOT BE COMPUTED.  SET THE ARRAY TO MISSING.
C        THE MAX FUNCTION GUARDS AGAINST NCAT LT 0 AND ALWAYS SETS
C        ONE COLUMN TO MISSING.  THIS IS RELATIVELY UNIMPORTANT
C        BECAUSE THE CALLING ROUTINES WILL DO THE SAME THING.
C
 300  DO 310 J=1,NCAT
         DO 309 K=1,NSTA
            XDATA(K,J)=9999.
 309     CONTINUE
 310  CONTINUE
C
 348  RETURN
      END

     
                  
