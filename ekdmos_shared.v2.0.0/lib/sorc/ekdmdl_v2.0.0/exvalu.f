      SUBROUTINE EXVALU(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JD,ITAU,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,IER)
C 
C        JULY      1999   RUDACK   TDL   MOS-2000
C        AUGUST    1999   RUDACK   CHANGED CALL TO GFETCH TO RETVEC
C        SEPTEMBER 1999   GLAHN    ADDED YDATA( )
C        NOVEMBER  2002   HUGHES   ADDED COMMAS TO FORMAT STATEMENTS
C                         REMOVED A WRITE STATEMENT USED FOR DEBUGGING
C                         AND DELETED EXTRA BLANK LINES BETWEEN CODE
C
C                         THERE HAVE BEEN MANY CHANGES, POSSIBLY BY
C                         RUDACK, (DIFFERENT IDS, NEW TABLES), SO
C                         THAT IT NO LONGER PROCESSES THE SAME 
C                         VARIABLES AS THE VERSION THAT WAS ON THE IBM.
C                         THE ORIGINAL VERSION HAS BEEN KEPT ON THE 
C                         IBM IN MOSLIB AS EXVALU.F.OLD UNTIL
C                         SOMEONE WISER THAN ME DECIDES IT IS NO
C                         LONGER NEEDED.
C
C        PURPOSE:
C            CALCULATES THE EXPECTED AMOUNT OF PRECIPITATION FOR THE 6-HR,
C            12-HR AND 24-HR PROJECTIONS.  NOTE THAT, IN INSTANCES WHERE 
C            NECESSARY,  THE CATEGORICAL PRECIPITATION VALUES HAVE ALREADY 
C            BEEN MESSAGED TO ENSURE MONOTONIC BEHAVIOR.  
C
C            IDPARS(1) & IDPARS(2) OF PROBABILITIES  ARE MAPPED
C                          INTO    FROM
C                          203240  203220
C                          203340  203320
C                          203440  203420
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
C            XDATA(K) = EXPECTED VALUE (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF XDATA( ).  (INPUT)
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
C             CORE(J) = THE ARRAY TO STORE OR RETIREVE THE DATA 
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
C           LDPARS(J) = PARSED VALUES CORRESPONDING TO LD( ) (J=1,15)
C                       (INTERNAL)
C          YDATA(K,J) = WORK ARRAY WHICH STORES THE PROBABILITIES (K=1,ND1)  
C                       (J=1,5 FOR 6-HR PROJ.) (J=1,6 FOR 12 OR 24 HR PROJ.)
C                       (AUTOMATIC)
C           ITABLE(I) = VALUES OF CCCFFF ACCOMMODATED (I=1) AND FOR 6-HR 
C                       FORECAST.  (INTERNAL)
C           JTABLE(I) = VALUES OF CCCFFF ACCOMMODATED (I=1) AND FOR 24-HR 
C                       FORECAST.  (INTERNAL)
C           KTABLE(I) = VALUES OF CCCFFF ACCOMMODATED (I=1) AND FOR 12-HR 
C                       FORECAST.  (INTERNAL)
C             STABLE  = MID-POINT VALUES FOR 6-HR PROBABILITY CATEGORIES.
C                       (INTERNAL)
C             TTABLE  = MID-POINT VALUES FOR 12-HR PROBABILITY CATEGORIES.
C                       (INTERNAL)
C             ZTABLE  = MID-POINT VALUES FOR 24-HR PROBABILITY CATEGORIES.
C                       (INTERNAL)
C                KCAT = NUMBER OF PROBABILITY CATEGORIES. SET IN JTABLE(1)
C                       AND KTABLE(1).                        
C                NDIM = DIMENSION OF ITABLE( ).  SET BY
C                       PARAMETER.  (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            RETVEC, PRSID1
C
      PARAMETER(NDIM=3)
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION XDATA(ND1),ISDATA(ND1),YDATA(ND1,6)
C        YDATA( ) IS AN AUTOMATIC ARRAY.
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10),ICALLD(L3264W,ND5)
      DIMENSION LD(4),LDPARS(15),KFILRA(5)
      DIMENSION ITABLE(3),JTABLE(7),KTABLE(9)
      DIMENSION STABLE(5),TTABLE(6),ZTABLE(6)
C     
      DATA ITABLE/203240,203340,203440/
      DATA JTABLE/5,203220,950052000,950051000,245000000,
     1                     495000000,995000000/
      DATA KTABLE/6,203320,203420,950052000,950051000,245000000,
     1                            495000000,995000000,199501000/
      DATA STABLE/0.050,0.170,0.370,0.745,1.2/
      DATA TTABLE/0.050,0.170,0.370,0.745,1.495,2.1/
      DATA ZTABLE/0.050,0.170,0.370,0.745,1.495,3.0/
C
      IER=0
C      
C        FIND THE ID(1) IN ITABLE(1).
C
      DO 105 JJ=1,NDIM      
         IF(ITABLE(JJ).EQ.(IDPARS(1)*1000+IDPARS(2))) GO TO 108
 105  CONTINUE
C     
      WRITE(KFILDO,107)(JD(L),L=1,4)
 107  FORMAT(' ****EXVALU ENTERED FOR VARIABLE',
     1        2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2        ' NOT ACCOMMODATED.')
      IER=102
      GO TO 300
C
 108  IF(IDPARS(12).EQ.6) GOTO 110
      IF(IDPARS(12).EQ.12) GOTO 152
      IF(IDPARS(12).EQ.24) GOTO 168 
C
 110  KCAT=JTABLE(1)
      LD(1)=JTABLE(2)*1000+100+IDPARS(4)
      LD(2)=JD(2)
      LD(3)=JD(3)
C          
C        GET THE KCAT PROBABILITIES FOR THE 6-HR PROJECTION.
C    
      DO 150 J=1,KCAT
         LD(4)=JTABLE(J+2)
C
         CALL PRSID1(KFILDO,LD,LDPARS)
         CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1               LD,LDPARS,JD,ITAU,
     2               NDATE,MDATE,CCALL,ISDATA,YDATA(1,J),ND1,NSTA,
     3               ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4               LSTORE,ND9,LITEMS,CORE,ND10,
     5               NBLOCK,NFETCH,
     6               IS0,IS1,IS2,IS4,ND7,
     7               L3264B,L3264W,IER)
C
C           JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C           IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C
         IF(IER.NE.0)THEN
            WRITE(KFILDO,125)(LD(L),L=1,4)
 125        FORMAT(' ****VARIABLE NOT RETRIEVED BY RETVEC IN EXVALU'
     1              ,2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
            GO TO 300
         ENDIF
C      
 150  CONTINUE
      GOTO 177 
C
C        GET THE KCAT PROBABILITIES FOR THE 12-HR PROJECTION.
C     
 152  KCAT=KTABLE(1)
      LD(1)=KTABLE(2)*1000+100+IDPARS(4)
      LD(2)=JD(2)
      LD(3)=JD(3)
C
      DO 165 J=1,KCAT
         LD(4)=KTABLE(J+3)
C
         CALL PRSID1(KFILDO,LD,LDPARS)
         CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1               LD,LDPARS,JD,ITAU,
     2               NDATE,MDATE,CCALL,ISDATA,YDATA(1,J),ND1,NSTA,
     3               ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4               LSTORE,ND9,LITEMS,CORE,ND10,
     5               NBLOCK,NFETCH,
     6               IS0,IS1,IS2,IS4,ND7,
     7               L3264B,L3264W,IER)
C
C          JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C          IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C
         IF(IER.NE.0)THEN
            WRITE(KFILDO,155)(LD(L),L=1,4)
 155        FORMAT(' ****VARIABLE NOT RETRIEVED BY RETVEC IN EXVALU'
     1             ,2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
            GO TO 300
         ENDIF
C      
 165  CONTINUE
      GOTO 177
C
C        GET THE KCAT PROBABILITIES FOR THE 24-HR PROJECTION.
C
 168  KCAT=KTABLE(1)
      LD(1)=KTABLE(3)*1000+100+IDPARS(4)
      LD(2)=JD(2)
      LD(3)=JD(3)
C                  
      DO 175 J=1,KCAT
         LD(4)=KTABLE(J+3)
C
         CALL PRSID1(KFILDO,LD,LDPARS)
         CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1               LD,LDPARS,JD,ITAU,
     2               NDATE,MDATE,CCALL,ISDATA,YDATA(1,J),ND1,NSTA,
     3               ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4               LSTORE,ND9,LITEMS,CORE,ND10,
     5               NBLOCK,NFETCH,
     6               IS0,IS1,IS2,IS4,ND7,
     7               L3264B,L3264W,IER)
C
C           JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C           IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C
         IF(IER.NE.0)THEN
            WRITE(KFILDO,172)(LD(L),L=1,4)
 172        FORMAT(' ****VARIABLE NOT RETRIEVED BY RETVEC IN EXVALU'
     1             ,2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
            GO TO 300
         ENDIF
C      
 175  CONTINUE 
C 
C        IDENTIFYING XMID FOR THE GIVEN PROJECTION AND DETERMINING THE 
C        EXPECTED AMOUNT OF PRECIPITATIO FOR EACH STATION FOR THE 6-HR, 
C        12-HR OR 24-HR PROJECTION.      
C 
 177  DO 178 K=1,NSTA
         XDATA(K)=0.0
 178  CONTINUE
C
      DO 180 K=1,NSTA         
         SUM=0.0
         DO 179 J=1,KCAT   
C
C              IF ANY PROBABILITY IS MISSING FOR A STATION, THEN ALL 
C              SHOULD BE, AND THE RETURNED EXPECTED VALUE MUST BE MISSING.
C
            IF(NINT(YDATA(K,J)).EQ.9997) YDATA(K,J)=0.0
C
            IF((NINT(YDATA(K,J)).EQ.9999.).OR.
     1         (NINT(XDATA(K)).EQ.9999))THEN
               XDATA(K)=9999.
              GOTO 180
            ENDIF           
C
            IF(IDPARS(12).EQ.6) THEN
               XMID=STABLE(J)
            ELSEIF(IDPARS(12).EQ.12) THEN
               XMID=TTABLE(J)
            ELSEIF(IDPARS(12).EQ.24) THEN
               XMID=ZTABLE(J)
            ENDIF
C
            IF(J.LT.KCAT) THEN
               PROB=(YDATA(K,J)-YDATA(K,J+1))
            ELSEIF(J.EQ.KCAT) THEN
               PROB=YDATA(K,J)
            ENDIF
C               
            SUM=SUM+PROB*XMID
C
 179     CONTINUE
C
         XDATA(K)=SUM
C
 180  CONTINUE 
      GO TO 348   
C     
C        THIS PREDICTOR CANNOT BE COMPUTED.  SET THE ARRAY TO MISSING.
C        THE MAX FUNCTION GUARDS AGAINST NCAT LT 0 AND ALWAYS SETS
C        ONE COLUMN TO MISSING.  THIS IS REALTIVELY UNIMPORTANT
C        BECAUSE THE CALLING ROUTINES WILL DO THE SAME THING.
C     
 300  DO 310 K=1,NSTA
         XDATA(K)=9999.
 310  CONTINUE
C
 348  RETURN
      END

     
                  
