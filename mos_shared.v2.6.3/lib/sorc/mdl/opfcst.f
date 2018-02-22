       SUBROUTINE OPFCST(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JD,ITAU,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NCAT,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,ISTAB,IER)
C 
C        JULY      1999   RUDACK   TDL   MOS-2000
C        AUGUST    1999   RUDACK   CHANGED CALL TO GFETCH TO RETVEC
C        SEPTEMBER 1999   GLAHN    CHANGED NAME FROM MDLCHK; ADDED
C                                  ISTAB TO CALL; ADDED FULL ID(4)
C                                  IN CALLS TO RETVEC; ADDED YDATA( );
C                                  INSURED /, IN **** COMMENTS 
C        APRIL     2000   RUDACK   ADDED DATA LDPARS/15*0/ 
C        MAY       2000   GLAHN    MODIFIED PRINT FORMATS 137 AND 159
C        MAY       2000   GLAHN    CHECKING FOR CCC = 2XX
C                                  AND DD = 07, 08, OR 09
C        MAY       2000   RUDACK   ADDED ERROR CHECK 'IERP' TO INSURE
C                                  PROPER PROGRAM FLOW
C        JUNE      2000   RUDACK   ADDED LOOP FOR EACH CALL TO 
C                                  RETVEC TO ALLOW THE RETRIEVAL OF NCAT
C                                  CATEGORICAL FORECASTS 
C                                  ADDED CHECK TO INSURE THAT NCAT IS
C                                  GREATER THAN ZERO 
C                                  MODIFIED MANY SUBROUTINE CALLS TO
C                                  PROCESS CATEGORICAL PROBABILITY
C                                  FORECASTS
C        MARCH     2004 RUDACK     CORRECTED CODE SO THAT IF A PRIMARY
C                                  FORECAST HAS A VALUE OF '9999.' OR
C                                  '9997.' AND THE SECONDARY FORECAST 
C                                  HAS A VALUE OF '9997', THE OPERATIONAL
C                                  FORECAST VALUE BECOMES '9997.'
C        JANUARY   2005 WIEDENFELD ADDED DD OF 05 TO IF STATEMENT TO INCLUDE LAMP
C                                  SYSTEM.
C
C        PURPOSE 
C            TO AUGMENT PRIMARY FORECASTS WITH BACKUP FORECASTS
C            WHEN OPERATIONAL FORECASTS ARE NOT PRESENT.
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
C          XDATA(K,L) = COMPUTED VARIABLE IS RETURNED IN XDATA( , )
C                       (K=1,NSTA) (L=1,NCAT).  (OUTPUT)
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
C                NCAT = NUMBER OF FORECAST CATEGORIES.  (INPUT)
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
C               ISTAB = SET TO 1 WHEN RETURNED VARIABLE IS BINARY;
C                       0 OTHERWISE.  (OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       SEE RETVEC FOR OTHER VALUES.  (OUTPUT)
C                IERP = HOLDS THE IER VALUE RETURNED FROM THE FIRST CALL
C                       TO RETVEC.  (INTERNAL) 
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA FOR PRIMARY
C                       THEN BACKUP FORECASTS.  (INTERNAL)
C           LDPARS(J) = PARSED VALUES CORRESPONDING TO LD( ) (J=1,15)
C                       (INTERNAL)
C          YDATA(K,L) = WORK ARRAY WHICH STORES BACKUP FORECASTS (K=1,ND5).  
C                       (L=1,NCAT) (AUTOMATIC)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            RETVEC, PRSID1
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION XDATA(ND1,NCAT),ISDATA(ND1),YDATA(ND1,NCAT)
C        YDATA( ) IS AN AUTOMATIC ARRAY.
      DIMENSION ID(4,NCAT),IDPARS(15,NCAT),JD(4,NCAT)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10),ICALLD(L3264W,ND5)
      DIMENSION LD(4),LDPARS(15),KFILRA(5)
C
      DATA LDPARS/15*0/  
C
      IER=0
      IERP=0
C
C        VERIFY THAT NCAT GT 0.
C
      IF(NCAT.LE.0)THEN
         WRITE(KFILDO,80)NCAT
 80      FORMAT(/,' ****NCAT =',I4,' NOT CORRECT IN OPFCST.')
         IER=102
         GO TO 300
      ENDIF
C
C        NOW VERIFY THAT OPFCST HAS BEEN ENTERED WITH A FORECAST ID
C
      IF(IDPARS(1,1)/100.NE.2)THEN
C        THE ABOVE TEST RETURNS WHEN THIS CCC IS NOT A FORECAST.
C        IER = 99 CONFORMS WITH OPTX USE.
	IER=99
	GO TO 350  
C
C        VERIFY THAT OPFCST HAS BEEN ENTERED WITH A FORECAST ID
C        REPRESENTING THE LAMP, ETA, AVN, OR MRF MODELS 
C
      ELSEIF(IDPARS(4,1).NE.1.AND.
     1       IDPARS(4,1).NE.5.AND.
     2       IDPARS(4,1).NE.7.AND.
     3       IDPARS(4,1).NE.8.AND.
     4       IDPARS(4,1).NE.9)THEN
C        THE ABOVE TEST RETURNS WHEN THE DD IS NOT 7 (ETA), 8 (AVN),
C        9 (MRF), 5 (LAMP), OR 1 (ECM).  
C        IER = 99 CONFORMS WITH OPTX USE.
	IER=99
	GO TO 350
      ENDIF
C
      IF(IDPARS(3,1).EQ.0)THEN
          ISTAB=0
      ELSE
          ISTAB=1
      ENDIF
C
C        DETERMINE WHETHER OPERATIONAL FORECASTS OF CCCFFFBDD 
C        ARE PRESENT.  IF SO, KEEP THEM.
C        
      LD(1)=ID(1,1)
      LD(2)=ID(2,1)
      LD(3)=ID(3,1)
C
      DO 85 N=1,NCAT
C
      LD(4)=ID(4,N)
C      
      CALL PRSID1(KFILDO,LD,LDPARS)      
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            LD,LDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,XDATA(1,N),ND1,NSTA,
     3            ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4            LSTORE,ND9,LITEMS,CORE,ND10,
     5            NBLOCK,NFETCH,
     6            IS0,IS1,IS2,IS4,ND7,
     7            L3264B,L3264W,IER)
C
 85   CONTINUE
C
C        JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C        IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C
      IF(IER.EQ.0)THEN
C           IT SEEMS THIS WOULD BE AN UNUSUAL CASE WHEN OPFCST
C           IS CALLED AND OPERATIONAL FORECASTS ARE ALREADY
C           PRESENT.
         WRITE(KFILDO,95)(ID(J,1),J=1,4)
 95      FORMAT(/,' ****OPERATIONAL FORECASTS FOR  ',
     1             I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2            ' ALREADY AVAILABLE IN OPFCST.',/,
     3            '     NO REPLACEMENT WITH BACKUPS DONE.')
         GO TO 350
      ENDIF
C
C        DROP THROUGH HERE MEANS OPEATIONAL FORECASTS ARE NOT PRESENT
C        AND WILL BE DEVELOPED FROM PRIMARIES AND BACKUPS.
C
C        FETCH CCCFFFBDD+10 (PRIMARY VALUES). 
C       
      LD(1)=ID(1,1)+10
      LD(2)=ID(2,1)
      LD(3)=ID(3,1)
C
      DO 100 N=1,NCAT
C
      LD(4)=ID(4,N)
C      
      CALL PRSID1(KFILDO,LD,LDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            LD,LDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,XDATA(1,N),ND1,NSTA,
     3            ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4            LSTORE,ND9,LITEMS,CORE,ND10,
     5            NBLOCK,NFETCH,
     6            IS0,IS1,IS2,IS4,ND7,
     7            L3264B,L3264W,IER)
C
C        JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C        IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C
 100  CONTINUE
C     
C        SET IER EQUAL TO IERP FOR ERROR CHECK BEFORE EXITING
C        THE PROGRAM.
C
      IERP=IER
      IF(IER.NE.0)THEN
C           IT SEEMS THIS WOULD BE AN UNUSUAL CASE WHEN BOTH
C           OPERATIONAL AND PRIMARY FORECASTS ARE NOT PRESENT.
         WRITE(KFILDO,137)(LD(J),J=1,4),MDATE
  137    FORMAT(' ****CANNOT OBTAIN PRI FCST',I9.9,1X,I9.9,1X,I9.9,
     1          1X,I10.3,' IN OPFCST FOR DATE',I11,'.')         
      ENDIF
C
C       FETCH CCCFFFBDD+20 (BACKUP VALUES) INTO YDATA( ).
C
      LD(1)=LD(1)+10
C
      DO 140 N=1,NCAT
C
      LD(4)=ID(4,N)
C
      CALL PRSID1(KFILDO,LD,LDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            LD,LDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,YDATA(1,N),ND1,NSTA,
     3            ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4            LSTORE,ND9,LITEMS,CORE,ND10,
     5            NBLOCK,NFETCH,
     6            IS0,IS1,IS2,IS4,ND7,
     7            L3264B,L3264W,IER)
C
 140  CONTINUE
C
C        JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C        IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C
      IF(IER.NE.0)THEN
C           IT SEEMS THIS WOULD BE AN UNUSUAL CASE WHEN BOTH
C           OPFCST IS CALLED AND BACKUP FORECASTS ARE NOT PRESENT.
         WRITE(KFILDO,159)(LD(J),J=1,4),MDATE
  159    FORMAT(' ****CANNOT OBTAIN BCK FCST ',I9.9,1X,I9.9,1X, 
     1          I9.9,1X,I10.3,' IN OPFCST FOR DATE',I11,'.')
      ENDIF
C     
C        DETERMINING WHETHER THE PRIMARY VALUES ARE MISSING. IF THEY ARE 
C        MISSING, REPLACE THE PRIMARY MISSING VALUES WITH THE BACK-UP VALUES.
C
      DO 180 K=1,NSTA
      DO 179 N=1,NCAT
C       
      IF((NINT(XDATA(K,N)).EQ.9999).OR.(NINT(XDATA(K,N)).EQ.9997))THEN
         IF(NINT(YDATA(K,N)).NE.9999) XDATA(K,N)=YDATA(K,N)
      ENDIF
C
 179  CONTINUE
 180  CONTINUE
C
C       IF 'OPFCST' USES THE PRIMARY FORECAST VALUES INSTEAD OF THE
C       SECONDARY FORECAST VALUES, RESET THE RETURNED IER VALUE FROM 
C       RETVEC BACK TO ZERO.
C      
      IF((IERP.EQ.0.).AND.(IER.NE.0)) IER=0
      GOTO 350
C
 300  DO 310 K=1,NSTA
         DO 309 J=1,MAX(1,NCAT)
            XDATA(K,J)=9999.
 309  CONTINUE
 310  CONTINUE
C
 350  RETURN
      END     
                  
