      SUBROUTINE TRUNCP(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JD,ITAU,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NCAT,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,ISTAB,IER)
C 
C        JUNE      1999   RUDACK   TDL   MOS-2000
C        AUGUST    1999   RUDACK   CHANGED CALL AND CALL TO RETVEC FROM 
C                                  GFETCH
C        SEPTEMBER 1999   GLAHN    ADDED ISTAB TO CALL
C        APRIL     2000   DALLAVALLE   MODIFIED FORMAT STATEMENTS TO 
C                                   CONFORM TO FORTRAN 90 STANDARDS ON 
C                                   THE IBM SP
C        APRIL     2000    GLAHN    CHECKED SPELLING; IMPROVED COMMENTS;
C                                   CHANGED ORDER OF LOOP INDICES, ADDED
C                                   DIMENSION FOR CCALL( , ), CALLD( ),
C                                   AND RACESS( )
C
C        PURPOSE
C           TRUNCATES PROBABILITY FORECASTS FOR SEVERAL CATEGORIES.
C           PROBABILITY FORECASTS FOR EACH CATEGORY ARE SET TO 0 IF
C           THEY ARE NEGATIVE, OR SET TO 1 IF THEY ARE GREATER THAN 1.
C           FORECASTS BETWEEN 0 AND 1 ARE UNCHANGED.
C           IDPARS(1) & IDPARS(2) OF PROBABILITIES  ARE MAPPED
C                          INTO    FROM
C                          208330  208310
C                          203230  203210
C                          203330  203310
C                          203430  203410
C           IF THE DATUM = 9999 OR 9997, IT IS LEFT UNCHANGED.
C
C           NOTE THAT ALTHOUGH ID( , ), IDPARS( , ), AND JD( , ) ARE
C           CARRIED THROUGH OPTX AS ID( ), IDPARS( ), AND JD( ), THE
C           SEQUENCE OF NCAT VALUES IN ID( , ), IDPARS( , ), AND JD( , )
C           WILL BE OK.
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
C                              M`STORE( , ).  LATER USED AS A WAY OF
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
C         ITABLE(I,J) = VALUES OF CCCFFF ACCOMMODATED (I=1) AND
C                       THE ASSOCIATED VARIABLE NEEDED (I=2)
C                       (J=1,NDIM).  (INTERNAL)
C                NDIM = SECOND DIMENSION OF ITABLE( , ).  SET BY
C                       PARAMETER.  (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            RETVEC, PRSID1
C
      PARAMETER (NDIM=4)
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION ISDATA(ND1),XDATA(ND1,NCAT)
      DIMENSION ID(4,NCAT),IDPARS(15,NCAT),JD(4,NCAT)
      DIMENSION ICALLD(L3264W,ND5),IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION ITABLE(2,NDIM)
      DIMENSION KFILRA(5),LD(4),LDPARS(15)
C     
      DATA ITABLE/208330, 208310,
     1            203230, 203210,
     2            203330, 203310,
     3            203430, 203410/  
C
      IER=0
      ISTAB=1
C     
C        FIND THE ID(1,1) IN ITABLE(1, ).
C
      DO 105 JJ=1,NDIM      
         IF(ITABLE(1,JJ).EQ.(IDPARS(1,1)*1000+IDPARS(2,1))) GO TO 108
 105  CONTINUE
C     
      WRITE(KFILDO,107)((JD(L,K),L=1,4),K=1,NCAT)
 107  FORMAT(/,' ****TRUNCP ENTERED FOR VARIABLE',
     1        2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2        ' NOT ACCOMMODATED.')
      IER=102
      GO TO 300
C     
C        VERIFY THAT NCAT GT 0.
C
 108  IF(NCAT.LE.0)THEN
         WRITE(KFILDO,110)NCAT
 110     FORMAT(/,' ****NCAT =',I4,' NOT CORRECT IN TRUNCP.')
         IER=102
         GO TO 300
      ENDIF
C
C        GET THE FOUR WORD ID.
C     
      LD(1)=ITABLE(2,JJ)*1000+IDPARS(3,1)*100+IDPARS(4,1)
      LD(2)=JD(2,1)
      LD(3)=JD(3,1)
C          
C        GET THE NCAT PROBABILITIES.
C         
      DO 140 J=1,NCAT
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
C
C        JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C        IN CALL TO CONST, BUT IS NOT USED THERE EITHER.
C     
         IF(IER.NE.0)THEN
            WRITE(KFILDO,125)(LD(L),L=1,4)
 125        FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN TRUNCP',
     1              2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
            GO TO 300
         ENDIF
C     
 140  CONTINUE
C     
C        TRUNCATING EACH CATEGORY FOR EACH STATION. DO FOR EACH STATION
C
      DO 150 J=1,NCAT
C
         DO 149 K=1,NSTA
            IF((NINT(XDATA(K,J)).EQ.9999)) GO TO 149
            IF((NINT(XDATA(K,J)).EQ.9997)) GO TO 149
C
            IF(XDATA(K,J).LT.0.0) THEN
               XDATA(K,J)=0.0
            ELSEIF(XDATA(K,J).GT.1.0) THEN
               XDATA(K,J)=1.0
            ENDIF
C
 149     CONTINUE
C
 150  CONTINUE    
C     
      GO TO 348   
C     
C        THIS PREDICTOR CANNOT BE COMPUTED.  SET THE ARRAY TO MISSING.
C        THE MAX FUNCTION GUARDS AGAINST NCAT LT 0 AND ALWAYS SETS
C        ONE COLUMN TO MISSING.  THIS IS RELATIVELY UNIMPORTANT
C        BECAUSE THE CALLING ROUTINES WILL DO THE SAME THING.
C     
 300  DO 310 K=1,NSTA
C
         DO 309 J=1,MAX(1,NCAT)
            XDATA(K,J)=9999.
 309     CONTINUE
C
 310  CONTINUE
C     
 348  CONTINUE
C
      RETURN
      END

     
                  
