      SUBROUTINE MAXTEST(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                   ID,IDPARS,JD,ITAU,
     2                   NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NSTA,
     3                   ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                   LSTORE,ND9,LITEMS,CORE,ND10,
     5                   NBLOCK,NFETCH,
     6                   IS0,IS1,IS2,IS4,ND7,
     7                   L3264B,L3264W,IER)
C 
C        JUNE      1999   RUDACK   TDL   MOS-2000
C        AUGUST    1999   RUDACK   CHANGED CALL AND CALL TO RETVEC
C                                  FROM GFETCH
C        SEPTEMBER 1999   GLAHN    ADDED YDATA( )
C        APRIL     2000   RUDACK   CHANGED IDS FOR OPERATIONAL USE
C        APRIL     2000   GLAHN    IMPROVED COMMENTS; CHECKED SPELLING;
C                                  ADDED DIMENSION FOR ISDATA( );
C                                  INSURED /, IN **** COMMENTS 
C        APRIL     2000   RUDACK   ADDED DATA LDPARS/15*0/
C        MAY       2000   RUDACK   ADDED ERROR CHECK 'IER' TO INSURE
C                                  PROPER PROGRAM FLOW 
C        OCTOBER   2001   COSGROVE MODIFIED CODE TO WORK FOR AVN 6/18Z
C                                  MAX/MIN TEMPERATURES
C        JANUARY   2001   COSGROVE MODIFIED CODE TO WORK FOR ETA 0/12Z
C                                  TEMPERATURES.  FIXED IF TEST THAT
C                                  CHECKS FOR MEDIUM RANGE AND SKIPS
C                                  PROGJECTIONS NOT / BY 6. 
C        JULY      2003   COSGROVE MODIFIED CODE TO REMOVE ALL USES OF
C                                  MODEL NUMBER 9 FOR THE GFS TRANSITION.
C                                  IN THE IF TEST NEAR DO 170 I CHANGED 
C                                  IT FROM CHECKING ON THE MODEL NUMBER
C                                  TO CHECKING ON THE PROJECTION.
C        NOVEMBER  2003   COSGROVE THE NEW GFS TEMP EQNS HAVE MORE PROJECTIONS
C                                  SO THE IF TEST AT 165 WAS CHANGED.  ALSO
C                                  CHANGED WHERE IT SWITCHES FROM LOOKING
C                                  EVERY 3 HOURS TO EVERY 6.
C        MARCH     2003   COSGROVE IN THE NOV CHANGE, THE ETA PIECE OF THE IF
C                                  TEST AT 165 WAS DROPPED.  PUT IT BACK IN.
C        JANUARY   2005   COSGROVE MODIFIED AGAIN TO HAVE 12Z GO OUT TO 192 
C                                  EVERY 3 HOURS.  NOTE THAT THIS ISN'T SET 
C                                  UP TO HANDLE THE OLD CRIPPLED ENSEMBLES,
C                                  BUT THEY JUST CHUG ALONG WITH LOTS OF ERROR
C                                  MESSAGES.  THE CHECK STILL GETS DONE WITH 
C                                  THE LITTLE PROJECTIONS THE ENSEMBLES HAVE.
C
C        PURPOSE
C           THIS SUBROUTINE ENSURES THAT THE MAXIMUM AND MINIMUM 
C           TEMPERATURES ARE EQUAL TO OR GREATER THAN THE INDIVIDUAL
C           3-HOUR FORECASTS FOR EACH FORECAST PERIOD.  IDPARS(1) &
C           IDPARS(2) OF TEMPERATURE ARE MAPPED
C                         INTO      FROM 
C                         202120    202100 AND 202020
C                         202220    202200 AND 202020
C
C           IF THE DATUM = 9999, IT IS LEFT UNCHANGED.
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
C            XDATA(K) = MAX OR MIN TEMPERATURE, CONSISTENT WITH 3-H
C                       VALUES (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF XDATA( ), ISDATA( ) AND
C                       YDATA( ).  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN
C                       INTEGER VARIABLE (L=1,L3264W) (K=1,ND5). 
C                       THIS ARRAY IS USED TO READ THE STATION
C                       DIRECTORY FROM A MOS-2000 EXTERNAL FILE.
C                       EQUIVALENCED TO CCALLD( ).  (CHARACTER*8)
C                       (INTERNAL)
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
C              NFETCH = THE NUMBER OF TIMES RETVEC HAS BEEN ENTERED.
C                       RETVEC KEEPS TRACK OF THIS AND RETURNS THE
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
C           LDPARS(J) = PARSED VALUES CORRESPONDING TO LD( ) (J=1,15)
C                       (INTERNAL)
C            YDATA(K) = WORK ARRAY WHICH HOLD 3-H TEMPERATURES (K=1,ND1).  
C                       (AUTOMATIC)
C         ITABLE(I,J) = VALUES OF CCCFFF ACCOMMODATED (I=1) AND
C                       THE ASSOCIATED VARIABLE NEEDED (I=2)
C                       (J=1,NDIM).  (INTERNAL)
C                NDIM = SECOND DIMENSION OF ITABLE( , ).  SET BY
C                       PARAMETER.  (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            PRSID1,RETVEC
C
      PARAMETER(NDIM=2)
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION ISDATA(ND1),XDATA(ND1),YDATA(ND1)
C        YDATA( ) IS AN AUTOMATIC ARRAY.
      DIMENSION ID(4),IDPARS(15),JD(4),LD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9),ICALLD(L3264W,ND5)
      DIMENSION CORE(ND10),LDPARS(15),KFILRA(5)
      DIMENSION ITABLE(3,NDIM)
C               
      DATA LDPARS/15*0/  
C     DATA ITABLE/202120,202100,202020,
C    1            202220,202200,202020/  
      DATA ITABLE/202120,202001,202020,
     1            202220,202011,202020/  
      IER=0
      NCYCLE=MOD(NDATE,100)
C     
C        VERIFY THE PROCESSING INDICATOR, IDPARS(1).
C
      DO 100 JJ=1,NDIM                                                 
      IF(ITABLE(1,JJ).EQ.(IDPARS(1)*1000+IDPARS(2))) GO TO 120
 100  CONTINUE
C
      WRITE(KFILDO,107)(JD(L),L=1,4)
 107  FORMAT(/,' ****MAXTEST ENTERED FOR VARIABLE',
     1        2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2        ' NOT ACCOMMODATED.')
      IER=102
      GOTO 300
C     
C        GET THE FOUR WORD ID FOR MAXIMUM OR MINIMUM TEMPERATURE
C        FOR THE PERIOD.
C      
 120  LD(1)=ITABLE(2,JJ)*1000+IDPARS(4)
      LD(2)=JD(2)
      LD(3)=JD(3) 
      LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)     
C            
C        FETCH THE MAXIMUM OR MINIMUM TEMPERATURE FOR EACH STATION.
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
      IF(IER.NE.0)THEN
         WRITE(KFILDO,125)(LD(J),J=1,4)
  125    FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN MAXTEST',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GOTO 300
      ENDIF
C              
C        GET THE FOUR WORD ID FOR 3-HOUR TEMPERATURES FOR THE
C        CORRESPONDING PERIOD.
C
      IF(IDPARS(2).EQ.120)THEN 
        L=1
      ELSE
        L=2
      ENDIF
C
      MTAU=0
C
      DO 170 J=1,5
C
      LD(1)=ITABLE(3,L)*1000+IDPARS(4)
      LD(2)=JD(2)
C
      LD(3)=(JD(3)-15)+MTAU
      LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)     
C
C        CHECK IF THE COMPARISON OF MAX/MIN TEMPERATURES TO HOURLY
C        TEMPERATURES ARE FOR THE SHORT RANGE OR MEDIUM RANGE. IF THE
C        CHECK IS FOR THE MEDIUM RANGE (PROJECTIONS BEYOND 84 HOURS)
C        AND THE PROJECTION IS NOT DIVISIBLE BY 6, THE PROJECTION TIME 
C        NEEDS TO BE INCREMENTED BY THREE HOURS.  IN THIS CASE, GOTO 
C        170 FOR THE INCREMENTATION.  11/2003 - CHANGED TO CHECK GT
C        198, CAUSE THE GFS NOW GOES OUT THAT FAR EVERY 3 HOURS.
C
      IF(IDPARS(12).GT.198)THEN
         IF(MOD(LD(3),6).NE.0) GOTO 168
      ENDIF
C            
C         FETCH THE 3-H OR 6-H TEMPERATURES FOR EACH STATION.
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
C        JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C        IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,127)(LD(M),M=1,4)
  127    FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN MAXTEST',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
C
C           IF ALL OR SOME OF THE THREE HOUR FORECAST TEMPERATURES ARE
C           MISSING, RESET IER EQUAL TO ZERO AND CONTINUE ITERATING.
C
         IER=0
         GOTO 165
      ENDIF
C     
      IF(IDPARS(2).EQ.120)THEN
C      
C           CHECKING MAXIMUM TEMPERATURE AGAINST EACH 3-H TEMPERATURE
C           FORECAST FOR EACH STATION.
C
         DO 150 K=1,NSTA
C
            IF(NINT(XDATA(K)).EQ.9999.OR.
     2         NINT(YDATA(K)).EQ.9999) GOTO 150
C
            IF(YDATA(K).GT.XDATA(K))THEN
               XDATA(K)=YDATA(K)      
            ENDIF
C
 150     CONTINUE
C 
      ELSE
C
C           CHECKING MINIMUM TEMPERATURE AGAINST EACH 3-H TEMPERATURE
C           FORECAST FOR EACH STATION.
C
         DO 160 K=1,NSTA
            IF(NINT(XDATA(K)).EQ.9999.OR.
     1         NINT(YDATA(K)).EQ.9999) GOTO 160
C
            IF(YDATA(K).LT.XDATA(K))THEN
               XDATA(K)=YDATA(K)      
            ENDIF 
C             
 160     CONTINUE
C
      ENDIF
C
C        FOR MOST MAX/MINS, THERE ARE 5 SPOT TEMPERATURES AVAILABLE
C        FOR THE PERIOD THE MAX/MIN COVERS.  FOR THE LAST MAX OR MIN 
C        PROJECTION FOR EACH CYCLE, THERE ARE ONLY 4 SPOT TEMPS.
C        THEREFORE, YOU MUST JUMP OUT OF THIS J=1,5 LOOP IN THESE
C        CASES, WHICH ARE COVERED BY THE FOLLOWING IF TESTS
C
C        AS OF 1/2005, THE 00/12 GFS HAVE 5 FOR EVERYTHING BUT 198.
C        THE 06/18Z GFS HAVE 5 FOR EVERYTHING.  THE ETA HAS 5 FOR
C        EVERYTHING BUT 90.  THE ENSEMBLES ARE JUST SO OLD THAT
C        WE LET THEM JUST RUN.  LOTS OF ERROR MESSAGES, BUT THE
C        CHECK IS STILL DOING WHAT IT CAN WITH THE DATA IT'S GOT.
C
  165  IF((IDPARS(4).EQ.8).OR.(IDPARS(4).EQ.18).OR.
     &    (IDPARS(4).EQ.28))THEN
        IF(J.EQ.4)THEN
         IF((NCYCLE.EQ.0).AND.(IDPARS(12).EQ.198))GOTO 350
         IF((NCYCLE.EQ.12).AND.(IDPARS(12).EQ.198))GOTO 350
        ENDIF
       ELSEIF((IDPARS(4).EQ.7).OR.(IDPARS(4).EQ.17).OR.
     &       (IDPARS(4).EQ.27))THEN
        IF(J.EQ.4)THEN
         IF(IDPARS(12).EQ.90)GOTO 350
        ENDIF
       ENDIF
C
 168  MTAU=MTAU+3 
C               
 170  CONTINUE
C 
      GOTO 350
C     
C        THIS VARIABLE CANNOT BE COMPUTED.  SET XDATA( ) TO MISSING.
C        THIS IS FOR SAFETY; XDATA( ) SHOULD ALREADY BE SET TO MISSING.
C        
 300  DO 310 K=1,NSTA
         XDATA(K)=9999.
 310  CONTINUE
C                                                        
 350  RETURN
      END

     
                  
