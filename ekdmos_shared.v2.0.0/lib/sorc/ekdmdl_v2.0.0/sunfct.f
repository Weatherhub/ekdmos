      SUBROUTINE SUNFCT(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JD,ITAU,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,IER)
C
C        JULY      1999   RUDACK   TDL   MOS-2000
C        AUGUST    1999   GLAHN    INSERTED CALL TO PRSID1, ETC.
C        SEPTEMBER 1999   GLAHN    NAME CHANGE FROM CLDPRB
C        APRIL     2000   DALLAVALLE   MODIFIED FORMAT STATEMENTS TO
C                                  CONFORM TO FORTRAN 90 STANDARDS
C                                  ON THE IBM SP
C        APRIL     2000   GLAHN    IMPROVED COMMENTS; CHECKED SPELLING;
C                                  INSERTED /, FOR **** COMMENTS
C
C        PURPOSE: 
C            TO COMPUTE THE FRACTIONAL AMOUNT OF SUNSHINE FROM
C            THE MOS FORECAST PROBABILITIES OF CLEAR, SCATTERED,
C            BROKEN, AND OVERCAST SKY CONDITIONS.
C            THE PERCENT SUNSHINE    CCCFFF = 209300
C            THE CLOUD PROBABILITIES CCCFFF = 208340
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
C               ID(J) = THE PREDICTOR ID (J=1,4).  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID( ) (J=1,15).
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
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
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
C                       IS NEEDED FOR CALL TO GFETCH.  (INPUT)
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
C            XDATA(K) = THREE PURPOSE ARRAY. FIRST, THE LATITUDE FOR EACH
C                       STATION IS STORED IN XDATA.  SECOND, THE SUNLIGHT 
C                       HOURS FOR EACH STATION IS STORED IN XDATA.  THIRD, 
C                       THE COMPUTED DAYLIGHT HOURS IN PERCENT IS RETURNED 
C                       IN XDATA(K) (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF XDATA( ), ISDATA( ), AND
C                       SLONG( ) AND FIRST DIMENSION OF YDATA( , , ).
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
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       XDATA( ) (J=1,4).  (INTERNAL)
C        YDATA(K,L,J) = YDATA( , , ) STORES THE CLOUD PROBABILITIES FOR 
C                       EACH CATEGORY (L=1,3), FOR EACH PROJECTION 
C                       (J=1,5), AND FOR EACH STATION (K=1,NSTA).
C                       (AUTOMATIC)
C           ITABLE(I) = CONTAINS THE CCCFFF OF THE NORMALIZED CLOUD 
C                       PROBABILITIES (I=2) AND OF THE PERCENT OF
C                       SUNSHINE (I=1).  THE NUMBER OF CLOUD CATEGORIES
C                       (CLR,SCT & BKN) IS IN ITABLE(I=3) WHILE THE 
C                       CORRESPONDING THRESHOLD VALUES ARE IN (I=4,7).
C                       (INTERNAL)
C           MTABLE(I) = ARRAY CONTAINING THE LATITUDE ID (I=1,4)
C                       AND THE LONGITUDE ID (I=5,8).  (INTERNAL)
C            SLONG(K) = ARRAY WHICH HOLDS THE LONGITUDE FOR EACH STATION
C                       (K=1,NSTA).  (AUTOMATIC ARRAY)  (INTERNAL)
C           FACTOR(L) = REGRESSION FACTOR FOR EQUATIONS (L=1,3).  (INTERNAL)
C           PERIOD(M) = STORES THE ASTRONOMICAL AMOUNT OF SUNSHINE 
C                       (IN HOURS) FOR EACH 6 HOUR PERIOD (M=1,4).
C                       (INTERNAL)
C           PROB(M,L) = STORES THE WEIGHTS FOR THE PROBABILITIES
C                       FOR EACH OF THE FOUR PERIODS OF THE DAY
C                       (M=1,4) AND FOR EACH CLOUD COVER CATEGORY 
C                       (L=1,3).  (INTERNAL)
C
C        NONSYSTEM SUBROUTINES CALLED 
C           RETVEC, SUNHRS, PRSID1
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION XDATA(ND1),ISDATA(ND1)
      DIMENSION ID(4),IDPARS(15),JD(4),LD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10),ICALLD(L3264W,ND5)
      DIMENSION ITABLE(6),MTABLE(8),LDPARS(15),FACTOR(3),
     1          PERIOD(4),PROB(4,3),KFILRA(5)
      DIMENSION YDATA(ND1,5,3),SLONG(ND1)
C        YDATA( , , ) AND SLONG( ) ARE AUTOMATIC ARRAYS.
C
C        DATA TABLE
C         
      DATA LDPARS/15*0/
      DATA ITABLE/209300,208340,3,
     1            150001000,400001000,700001000/
      DATA MTABLE/400006000,000000000,000000000,0000000000,
     1            400007000,000000000,000000000,0000000000/
      DATA FACTOR/1.09863,.95443,.52990/
      DATA CONSTANT/-.02800/
      DATA PERIOD/4*0./
C
      IER=0
      JCYCLE=MOD(MDATE,100)
C
C        FIND CCCFFF OF ID(1) IN ITABLE(1) AND CHECK FOR LEGITIMATE 
C        PROJECTION.
C
      IF(ITABLE(1).NE.(IDPARS(1)*1000+IDPARS(2))) GOTO 105 
C
      IF((JCYCLE.EQ.0).AND.(IDPARS(12).EQ.24.OR.
     1                      IDPARS(12).EQ.48.OR.
     2                      IDPARS(12).EQ.72)) GOTO 110
C
      IF((JCYCLE.EQ.6).AND.(IDPARS(12).EQ.42.OR.
     1                      IDPARS(12).EQ.66)) GOTO 110
C
      IF((JCYCLE.EQ.12).AND.(IDPARS(12).EQ.36.OR.
     1                       IDPARS(12).EQ.60)) GOTO 110
C
      IF((JCYCLE.EQ.18).AND.(IDPARS(12).EQ.30.OR.
     1                       IDPARS(12).EQ.54)) GOTO 110
C
C        THE ID DOES NOT MATCH IDPARS(1) AND IDPARS(2).
C
 105  WRITE(KFILDO,107)(ID(L),L=1,4)
 107  FORMAT(/,' ****SUNFCT ENTERED FOR VARIABLE',
     1       2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2       ' NOT ACCOMMODATED.')
      IER=102
      GOTO 300
C         
 110  KCAT=ITABLE(3)
C    
C        FILL LD( ) AND LDPARS( ) AND READ MOS NORMALIZED CLOUD 
C        PROBABILITIES FOR 5 PROJECTIONS AND 3 CATEGORIES.  
C     
      LD(1)=ITABLE(2)*1000+300+IDPARS(4)
      LD(2)=JD(2)
C          
      MTAU=18
C   
      DO 150 L=1,5
C
      LD(3)=JD(3)-MTAU
C
      DO 149 J=1,KCAT
C              
      LD(4)=ITABLE(J+3)
C  
C        FILL LDPARS( ) TO CORRESPOND TO LD( ).
C        
      CALL PRSID1(KFILDO,LD,LDPARS)
C
C        FETCH THE CLOUD PROBABILITIES FOR EACH 6-HOUR PERIOD
C        BEGINNING AT IDPARS(12)-18.
C
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            LD,LDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,YDATA(1,L,J),ND1,NSTA,
     3            ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4            LSTORE,ND9,LITEMS,CORE,ND10,
     5            NBLOCK,NFETCH,
     6            IS0,IS1,IS2,IS4,ND7,
     7            L3264B,L3264W,IER)
C
C        JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C        IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C
C        CHECKING IF IDPARS(12)+6 IS AVAILABLE.  IF NOT, LET YDATA 
C        VALUES AT IDPARS(12) EQUAL YDATA VALUES IDPARS(12)+6.         
C               
      IF((L.EQ.5).AND.(IER.NE.0))THEN
C
        DO 121 K=1,NSTA
           YDATA(K,5,J)=YDATA(K,4,J)
 121    CONTINUE
C        
      ENDIF
C  
      IF((IER.NE.0).AND.(L.NE.5))THEN
         WRITE(KFILDO,125)(LD(M),M=1,4)
 125     FORMAT(/,' ****CLOUD PROBABILITY NOT RETRIEVED BY RETVEC IN', 
     1          ' SUNFCT',2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 300
      ENDIF
C     
 149  CONTINUE
C
C        SUBTRACT 6 HOURS FROM THE PROJECTION MTAU.
C
      MTAU=MTAU-6
C       
 150  CONTINUE
C 
C        FILL LD( ) AND LDPARS( ) AND CALL SUBROUTINE
C        RETVEC TO FETCH AND RETURN IN XDATA( )
C        THE LATITUDE FOR EACH STATION.
C
      LD(1)=MTABLE(1)
      LD(2)=MTABLE(2)
      LD(3)=MTABLE(3)
      LD(4)=MTABLE(4)
C
      CALL PRSID1(KFILDO,LD,LDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            LD,LDPARS,JD,ITAU,
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
C        ENSURING THAT THE LATITUDES WERE RETRIEVED BY 'RETVEC'.
C     
      IF(IER.NE.0)THEN
         WRITE(KFILDO,152)(LD(J),J=1,4)
 152     FORMAT(/,' ****LATITUDE NOT RETRIEVED BY RETVEC', 
     1          ' IN SUNFCT',2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 300
      ENDIF
C
C        FILL LD( ) AND LDPARS( ) AND CALL SUBROUTINE
C        RETVEC TO FETCH AND RETURN IN SLONG( )
C        THE LONGITUDE FOR EACH STATION.
C
      LD(1)=MTABLE(5)
      LD(2)=MTABLE(6)
      LD(3)=MTABLE(7)
      LD(4)=MTABLE(8)
C
      CALL PRSID1(KFILDO,LD,LDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            LD,LDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,SLONG,ND1,NSTA,
     3            ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4            LSTORE,ND9,LITEMS,CORE,ND10,
     5            NBLOCK,NFETCH,
     6            IS0,IS1,IS2,IS4,ND7,
     7            L3264B,L3264W,IER)
C
C        JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C        IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C 
C        ENSURING THAT THE LONGITUDES WERE RETRIEVED BY 'RETVEC'.
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,155)(LD(J),J=1,4)
 155     FORMAT(/,' ****LONGITUDE NOT RETRIEVED BY RETVEC IN SUNFCT',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 300
      ENDIF
C
C        CALL SUBROUTINE 'SUNHRS' TO CALCULATE THE HOURS OF SUNLIGHT
C        WHICH IS RETURNED IN XDATA AND THE EXTRATERRESTRIAL RADIATION
C        (ETR) WHICH IS RETURNED IN DATA.
C 
      CALL SUNHRS(KFILDO,MDATE,IDPARS,XDATA,
     1            DATA,ND1,ND5,NSTA)     
C               
      DO 280 K=1,NSTA
C
C        FOR EACH STATION, CHECK IF THERE ARE ANY MISSING CLOUD
C        PROBABILITIES, LONGITUDE VALUES OR HOURS OF SUNSHINE.  
C        IF MISSING, SET FRACTION FORECAST=9999 AND DON'T MAKE 
C        A FORECAST FOR THAT STATION.  IF A CLOUD PROBABILITY
C        IS 9997, SET IT TO 0.
C
         DO 165 J=1,5
C
            IF(NINT(YDATA(K,J,1)).EQ.9999)GO TO 270
            IF(NINT(YDATA(K,J,1)).EQ.9997)YDATA(K,J,1)=0.
            IF(NINT(YDATA(K,J,2)).EQ.9997)YDATA(K,J,2)=0.
            IF(NINT(YDATA(K,J,3)).EQ.9997)YDATA(K,J,3)=0.
C            
 165     CONTINUE
C        
         IF(NINT(SLONG(K)).EQ.9999.OR.
     1      NINT(XDATA(K)).EQ.9999)GO TO 270
C 
C           DETERMINE SUNRISE AND SUNSET.
C           SUNRISE IS BEFORE 12 GMT.  THEREFORE, CALCULATE THE AVERAGE 
C           WEIGHTED CLOUD PROBABILITIES FOR EACH CATEGORY BETWEEN 
C           SUNRISE AND 12 GMT AND 12 GMT AND 18 GMT THIS WAY.
C
         SUNRIZ=12.+(SLONG(K)/15.)-(.5*XDATA(K))
         SUNSET=12.+(SLONG(K)/15.)+(.5*XDATA(K))
C
         IF(SUNRIZ.LT.12.)THEN
C
            PROB(1,1)=((1.-(SUNRIZ/12.))*YDATA(K,1,1))+((SUNRIZ/12.)* 
     1                     YDATA(K,2,1))     
            PROB(1,2)=((1.-(SUNRIZ/12.))*YDATA(K,1,2))+((SUNRIZ/12.)* 
     1                     YDATA(K,2,2)) 
            PROB(1,3)=((1.-(SUNRIZ/12.))*YDATA(K,1,3))+((SUNRIZ/12.)* 
     1                     YDATA(K,2,3))
C
            PROB(2,1)=(YDATA(K,2,1)+YDATA(K,3,1))/2.
            PROB(2,2)=(YDATA(K,2,2)+YDATA(K,3,2))/2.
            PROB(2,3)=(YDATA(K,2,3)+YDATA(K,3,3))/2.
C
C              DETERMINING THE AMOUNT OF EXTRATERRESTRIAL SUNSHINE FOR 
C              THE TWO PERIODS.
C
            PERIOD(1)=12.-SUNRIZ
            PERIOD(2)=6.
C
C              SUNRISE IS AFTER 12 GMT.  THEREFORE, CALCULATE THE AVERAGE 
C              WEIGHTED CLOUD PROBABILITIES FOR EACH CATEGORY BETWEEN 
C              SUNRISE AND 12 GMT AND 12 GMT AND 18 GMT THIS WAY.
C                   
         ELSEIF(SUNRIZ.GE.12.)THEN
C
            PROB(1,1)=9999.
            PROB(1,2)=9999.
            PROB(1,3)=9999.              
C
            PROB(2,1)=((1.5-(SUNRIZ/12.))*YDATA(K,2,1))+
     1                (((SUNRIZ/12.)-.5)*YDATA(K,3,1))     
            PROB(2,2)=((1.5-(SUNRIZ/12.))*YDATA(K,2,2))+
     1                (((SUNRIZ/12.)-.5)*YDATA(K,3,2))     
            PROB(2,3)=((1.5-(SUNRIZ/12.))*YDATA(K,2,3))+
     1                (((SUNRIZ/12.)-.5)*YDATA(K,3,3))
C  
C              DETERMINING THE AMOUNT OF EXTRATERRESTRIAL SUNSHINE FOR 
C              TWO PERIODS.
C
            PERIOD(1)=0.
            PERIOD(2)=18.-SUNRIZ
C
         ENDIF
C
C           SUNSET IS BEFORE 00 GMT.  THEREFORE, CALCULATE THE AVERAGE 
C           WEIGHTED CLOUD PROBABILITIES FOR EACH CATEGORY BETWEEN 
C           18 - 00 GMT AND 00 - 24 GMT THIS WAY.
C               
         IF(SUNSET.LE.24.)THEN
C
            PROB(3,1)=(((SUNSET/12.)-1.5)*YDATA(K,4,1))+ 
     1                ((2.5-(SUNSET/12.))*YDATA(K,3,1)) 
            PROB(3,2)=(((SUNSET/12.)-1.5)*YDATA(K,4,2))+ 
     1                ((2.5-(SUNSET/12.))*YDATA(K,3,2)) 
            PROB(3,3)=(((SUNSET/12.)-1.5)*YDATA(K,4,3))+ 
     1                ((2.5-(SUNSET/12.))*YDATA(K,3,3)) 
C            
            PROB(4,1)=9999.
            PROB(4,2)=9999.
            PROB(4,3)=9999.
C
C              DETERMINING THE AMOUNT OF EXTRATERRESTRIAL SUNSHINE FOR 
C              TWO PERIODS.
C
            PERIOD(3)=SUNSET-18.
            PERIOD(4)=0.
C
C           SUNSET IS AFTER 00 GMT.  THEREFORE, CALCULATE THE AVERAGE 
C           WEIGHTED CLOUD PROBABILITIES FOR EACH CATEGORY BETWEEN 
C           18 - 00 GMT AND 00 - 24 GMT THIS WAY.
C
         ELSEIF(SUNSET.GT.24.)THEN
C 
            PROB(3,1)=(YDATA(K,3,1)+YDATA(K,4,1))/2.
            PROB(3,2)=(YDATA(K,3,2)+YDATA(K,4,2))/2.
            PROB(3,3)=(YDATA(K,3,3)+YDATA(K,4,3))/2.
C 
            PROB(4,1)=((3.-(SUNSET/12.))*YDATA(K,4,1))+
     1                (((SUNSET/12.)-2.)*YDATA(K,5,1))             
            PROB(4,2)=((3.-(SUNSET/12.))*YDATA(K,4,2))+
     1                (((SUNSET/12.)-2.)*YDATA(K,5,2))             
            PROB(4,3)=((3.-(SUNSET/12.))*YDATA(K,4,3))+
     1                (((SUNSET/12.)-2.)*YDATA(K,5,3)) 
C
C              DETERMINING THE AMOUNT OF EXTRATERRESTRIAL SUNSHINE FOR 
C              TWO PERIODS.
C
            PERIOD(3)=6.
            PERIOD(4)=SUNSET-24.
C
         ENDIF
C
C           CALCULATE THE FORECAST HOURS OF SUNSHINE BASED UPON CLOUD
C           PROBABILITIES AND LENGTH OF DAYLIGHT FOR EACH OF THE PERIODS.
C           THE WEIGHTING FACTORS USED HERE ARE BASED ON REGRESSION.
C
         HRSSUN=0.
C
         DO 170 M=1,4
C
            DO 169 J=1,3
C            
               HRSSUN=HRSSUN+PROB(M,J)*PERIOD(M)*FACTOR(J)
C               
 169        CONTINUE
C
 170     CONTINUE
C
C           DETERMINE THE PERCENT OF SUNSHINE BY DIVIDING THE FORECAST
C           AMOUNT BY THE TOTAL POSSIBLE AMOUNT OF SUNSHINE.  THE VALUES
C           ARE RETURNED IN XDATA. THE CONSTANT IN THE EQUATION IS THE 
C           REGRESSION CONSTANT.
C
         IF(XDATA(K).NE.0.) THEN
            XDATA(K)=(HRSSUN/XDATA(K))+CONSTANT
C
C              MAKE SURE THAT THE FORECAST FRACTIONAL AMOUNT IS GE ZERO 
C              AND LE 1.
C
            IF(XDATA(K).LT.0.) XDATA(K)=0.
            IF(XDATA(K).GT.1.) XDATA(K)=1.
         ENDIF
C
         GO TO 280
C
 270     XDATA(K)=9999.
C
 280  CONTINUE
C
      GO TO 350
C
C        THIS PREDICTOR CANNOT BE COMPUTED.  SET THE ARRAY TO MISSING.
C
 300  DO 310 K=1,NSTA
         XDATA(K)=9999.
 310  CONTINUE 
C
 350  RETURN
      END     
                  
