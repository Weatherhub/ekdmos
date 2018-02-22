      SUBROUTINE DPCAMT(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JD,ITAU,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NCAT,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,ISTAB,IER)
C 
C        JANUARY   2004  GLAHN   TDL   MOS-2000
C        FEBRUARY  2004  GLAHN   REVISED FOR 8 CATEGORIES, OMITTING
C                                THE OBSCURED BIN
C
C        PURPOSE
C            TO COMPUTE NORMALIZED PROBABILITIES OF 8 DISCRETE CATEGORIES
C            OF CLOUD AMOUNTS IN ORDER:
C               P(UNKNOWN=U), P(CLR=C), P(PARTIAL OBSCURATION=P),
C               P(FEW=F), P(SCT=S), P(BKN=B), P(OVC=O), AND
C               P(TOTAL OBSCURATION=X)
C            FROM PROBABILITIES OF 8 CATEGORIES IN ORDER:
C               P(U), P(C+P+F+S+B+O+X), P(P+F+S+B+O+X) ,P(F+S+B+O+X),
C               P(S+B+O+X), P(B+O+X), P(O+X), P(X)
C
C            A DIAGNOSTIC IS PRINTED AND AN "ERROR" IER = 139 RETURNED
C            IF SUM OF NON-NORMALIZED PROBABILITIES IS OUTSIDE THE
C            RANGE 0.90 AND 1.35.
C
C            THE FOLLOWING IDPARS(1), IDPARS(2), IDPARS(3) AND WXXXXYY
C            OF ID(4) ARE ACCOMMODATED:
C
C               208 372 3 -.5000E+00 - PROBABILITY OF CLOUD AMOUNT P(U)
C               208 372 3  .5000E+00 - PROBABILITY OF CLOUD AMOUNT P(C)
C               208 372 3  .1500E+01 - PROBABILITY OF CLOUD AMOUNT P(P)
C               208 372 3  .2500E+01 - PROBABILITY OF CLOUD AMOUNT P(F)
C               208 372 3  .4500E+01 - PROBABILITY OF CLOUD AMOUNT P(S)
C               208 372 3  .7500E+01 - PROBABILITY OF CLOUD AMOUNT P(B)
C               208 372 3  .8500E+01 - PROBABILITY OF CLOUD AMOUNT P(O)
C               208 372 3  .9999E+04 - PROBABILITY OF CLOUD AMOUNT P(X)
C
C            THE FOLLOWING IDPARS(1), IDPARS(2), IDPARS(3), AND WXXXXYY
C            OF ID(4) ARE NEEDED:
C
C               208 370 2 -.5000E+00 - PROBABILITY OF CLOUD AMOUNT P(U)
C               208 370 1 -.5000E+00 - PROBABILITY OF CLOUD AMOUNT P(CPFSBOX)
C               208 370 1  .5000E+00 - PROBABILITY OF CLOUD AMOUNT P(PFSBOX)
C               208 370 1  .1500E+01 - PROBABILITY OF CLOUD AMOUNT P(FSBOX)
C               208 370 1  .2500E+01 - PROBABILITY OF CLOUD AMOUNT P(SBOX)
C               208 370 1  .4500E+01 - PROBABILITY OF CLOUD AMOUNT P(BOX)
C               208 370 1  .7500E+01 - PROBABILITY OF CLOUD AMOUNT P(OX)
C               208 370 1  .8500E+01 - PROBABILITY OF CLOUD AMOUNT P(X)
C
C        DATA SET USE 
C            KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                     (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT) 
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
C                       IS NEEDED FOR CALL TO GFETCH.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C               MDATE = NDATE UPDATED WITH ITAU.  (INPUT)
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
C                       WITH.  DIMENSION OF XDATA( ), ISDATA( ) AND
C                       YDATA( ).  (INPUT)
C                NCAT = NUMBER OF FORECAST CATEGORIES.  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN 
C                       INTEGER VARIABLE (L=1,L3264W) (K=1,ND5). 
C                       THIS ARRAY IS USED TO READ THE STATION 
C                       DIRECTORY FROM A MOS-2000 EXTERNAL FILE.
C                       EQUIVALENCED TO CCALLD( ).  (CHARACTER*8) 
C                       (INTERNAL)
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).  EQUIVALENCED
C                       TO ICALLD( , ).  (CHARACTER*8)   (INTERNAL)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
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
C                       102 = ID NOT ACCOMMODATED OR NCAT LE 0.
C                       SEE GFETCH FOR OTHER VALUES.  (OUTPUT)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       XDATA( ) (J=1,4).  (INTERNAL)
C           LDPARS(J) = CORRESPONDS TO LD( ) (J=1,15)  (INTERNAL)
C         ITABLE(M,J) = VALUES OF CCCFFFB (M=1) AND WXXXXYY ACCOMMODATED
C                       (M=2) (J=1,8).  ACTUALLY, ONLY THE FIRST J=1
C                       ENTRY IS USED FOR THE 8-CATEGORY BINS AND J=2
C                       FOR THE 2-CATEGORY OBSCURATION BIN.  (INTERNAL)
C         JTABLE(M,J) = VALUES OF CCCFFFB (M=1) AND WXXXXYY NEEDED
C                       (M=2) (J=1,8) FOR THE SIX CATEGORY BINS.
C                       (INTERNAL)
C         KTABLE(M,J) = VALUES OF CCCFFFB (M=1) AND WXXXXYY NEEDED
C                       (M=2) (J=1,2) FOR THE TWO CATEGORY OBSCURATION
C                       BIN.  ACTUALLY, THERE ARE 3 BINS INCLUDING THE
C                       CLEAR, BUT IT WAS NOT REGRESSED AND IS JUST
C                       P(C) = 1. - [P(PO) + P(FO)].  (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            RETVEC, PRSID1
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
      DIMENSION ITABLE(2,8),JTABLE(2,8)
      DIMENSION KFILRA(5),LD(4),LDPARS(15)
C   
      DATA ITABLE/208 372 3, 1500000000,
     1            208 372 3,  500000000,
     2            208 372 3,  150001000,
     3            208 372 3,  250001000,
     4            208 372 3,  450001000,
     5            208 372 3,  750001000,
     6            208 372 3,  850001000,
     7            208 372 3,  999995000/
C
      DATA JTABLE/208 370 2, 1500000000,
     1            208 370 1, 1500000000,
     2            208 370 1,  500000000,
     3            208 370 1,  150001000,
     4            208 370 1,  250001000,
     5            208 370 1,  450001000,
     6            208 370 1,  750001000,   
     7            208 370 1,  850001000/   
C
      IER=0
      ISTAB=1
C     
C        FIND CCCFFFB OF ID(1,1) IN ITABLE(1, ).  THIS ROUTINE IS
C        UNIQUE TO DISCRETE CLOUD BIN AMOUNTS.  ACTUALLY, ONLY
C        ITABLE(1,1) AND ITABLE (2,1) ARE USED FOR THE BINS WITH
C        8 PROBABILITY FORECASTS, BECAUSE ALL 8 CATEGORIES ARE
C        DEALT WITH AT ONCE, AND THE ENTRY HAS TO BE FOR THE FIRST.
C        HOWEVER, ALL JTABLE( , ) ENTRIES ARE NECESSARY.
C  
      DO 103 JJ=1,2 
      IF(ID(1,1)/100.EQ.ITABLE(1,JJ).AND.
     1   ID(4,1).EQ.ITABLE(2,JJ)+
     2     IDPARS(13,1)*100+IDPARS(14,1)*10+IDPARS(15,1))GO TO 108
D     IDCK1=ID(1,1)/100
D     IDCK2=ITABLE(2,JJ)+
D    1      IDPARS(13,1)*100+IDPARS(14,1)*10+IDPARS(15,1)
D     WRITE(KFILDO,102)IDCK1,IDCK2
D102  FORMAT(/' DPCAMT--IDCK,IDCK2--'2I15)
 103  CONTINUE
C     
      WRITE(KFILDO,107)(ID(JJ,1),JJ=1,4)
 107  FORMAT(/,' ****DPCAMT ENTERED FOR VARIABLE',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2         ' NOT ACCOMMODATED.')
      IER=102
      GO TO 270
C
C        VERIFY THAT NCAT EQ 8.
C
 108  IF(NCAT.NE.8)THEN
         WRITE(KFILDO,110)NCAT,(ID(JJ,1),JJ=1,4)
 110     FORMAT(/' ****NCAT =',I4,' NOT = 8 FOR VARIABLE',
     1           4I12,' IN DPCAMT.')
         IER=102
         GO TO 270
      ENDIF
C   
C        FILL LD( ) AND LDPARS( ) AND CALL SUBROUTINE
C        RETVEC TO FETCH AND RETURN IN XDATA( , ) FOR
C        THE NCAT PROBABILITIES.
C     
      DO 140 J=1,NCAT
C          
C        GET THE NCAT PROBABILITIES.
C         
      LD(1)=JTABLE(1,J)*100+IDPARS(4,1)
      LD(2)=ID(2,1)
      LD(3)=ID(3,1)
      LD(4)=JTABLE(2,J)+IDPARS(13,1)*100+IDPARS(14,1)*10
C        ID( ,J) AND IDPARS( ,J) (J=1,NCAT) ARE ALL EQUAL.
      CALL PRSID1(KFILDO,LD,LDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            LD,LDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,XDATA(1,J),ND1,NSTA,
     3            ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4            LSTORE,ND9,LITEMS,CORE,ND10,
     5            NBLOCK,NFETCH,
     6            IS0,IS1,IS2,IS4,ND7,
     7            L3264B,L3264W,IER)
C        JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C        IN CALL TO CONST, BUT IS NOT USED THERE EITHER.
      IF(IER.NE.0)GO TO 270
C
 140  CONTINUE
C     
C        XDATA( ,1) HOLDS P(U)
C        XDATA( ,2) HOLDS P(C+P+F+S+B+O+X)
C        XDATA( ,3) HOLDS P(P+F+S+B+O+X)
C        XDATA( ,4) HOLDS P(F+S+B+O+X)
C        XDATA( ,5) HOLDS P(S+B+O+X)
C        XDATA( ,6) HOLDS P(B+O+X)
C        XDATA( ,7) HOLDS P(O+X)
C        XDATA( ,8) HOLDS P(X)
C
D     WRITE(KFILDO,142)((XDATA(K,J),J=1,NCAT),K=1,NSTA)
D142  FORMAT(/' IN DPCAMT AT 142--(XDATA(K,J))',/(8F10.3))
C
C        CALCULATE PROBABILITIES OF DISCRETE CATEGORIES.
C        NOTE THAT THIS IS SPECIFIC TO 8 CLOUD CATEGORIES AS
C        DEFINED ABOVE.  ANY ORIGINAL VALUE OUTSIDE THE 
C        RANGE 0 TO 1 IS SET BACK TO 0 OR 1, RESPECTIVELY.
C
      DO 160 K=1,NSTA
C
         IF(NINT(XDATA(K,1)).EQ.9999)GO TO 160
C           IF ONE CATEGORY IS MISSING, ALL WILL BE; THESE DO
C           NOT HAVE TO BE RESET TO 9999.
C
         SUMPRB=0. 
C
         DO 145 J=1,NCAT
            IF(NINT(XDATA(K,J)).EQ.9997)THEN
               XDATA(K,J)=0.
            ELSEIF(XDATA(K,J).LT.0.)THEN
               XDATA(K,J)=0.
            ELSEIF(XDATA(K,J).GT.1.)THEN
               XDATA(K,J)=1.
            ENDIF
C
 145     CONTINUE
C
C           XDATA(K,1) IS THE NON NORMALIZED P(U).
C           IT WILL BE IN THE 0 TO 1 RANGE.
C
         SUMPRB=XDATA(K,1)
C
         DO 150 M=2,NCAT-1
         XDATA(K,M)=XDATA(K,M)-XDATA(K,M+1)
         IF(XDATA(K,M).LT.0.)XDATA(K,M)=0.
C           XDATA(K,M) IS NOW NON NORMALIZED PROBABILITY OF CATEGORY M.
C           IT WILL BE IN THE 0 TO 1 RANGE.
         SUMPRB=SUMPRB+XDATA(K,M)
 150     CONTINUE
C
C           XDATA(K,8) IS THE NON NORMALIZED P(X)
         SUMPRB=SUMPRB+XDATA(K,8)
C
D        WRITE(KFILDO,152)SUMPRB
D152     FORMAT(' SUMPRB',F12.3)
C           DIVIDE EACH PROBABILITY FORECAST BY THE SUM.  IF SUMPRB = 0.,
C           THEN EACH PROBABILITY = 0 AND DIVISION IS NOT NECESSARY.
C
         IF(SUMPRB.NE.0.)THEN
C
            DO 154 M=1,NCAT
            XDATA(K,M)=XDATA(K,M)/SUMPRB
 154        CONTINUE
C
         ELSE
            WRITE(KFILDO,155)CCALL(K,1)
 155        FORMAT(/' ****THE SUM OF CLOUD BIN PROBABILITIES IS',
     1              ' EQ ZERO FOR STATION ',A8,
     2              ' IN DPCAMT.  IER SET = 191.')
            IER=191
         ENDIF   
C
         IF(SUMPRB.LT..90)THEN
            WRITE(KFILDO,158)SUMPRB,CCALL(K,1),(ID(J,1),J=1,4),NDATE,
     1                       (XDATA(K,J),J=1,8)
 158        FORMAT(/' ****THE SUM OF CLOUD BIN PROBABILITIES = ',F7.2,
     1              ' IS  LT .90 IN DPCAMT FOR STATION ',A8,
     2              ' IN DPCAMT BEFORE NORMALIZATION'/
     3              '     FOR VARIABLE',4I12,' FOR DATE',I11,
     4              '.  IER SET = 191.'/
     5              '     NORMALIZED VALUES ARE:',8F7.3)
            IER=191
         ELSEIF(SUMPRB.GT.1.35)THEN
            WRITE(KFILDO,159)SUMPRB,CCALL(K,1),(ID(J,1),J=1,4),NDATE,
     1                       (XDATA(K,J),J=1,8)
 159        FORMAT(/' ****THE SUM OF CLOUD BIN PROBABILITIES = ',F7.2,
     1              ' IS  GT 1.35 IN DPCAMT FOR STATION ',A8,
     2              ' IN DPCAMT BEFORE NORMALIZATION'/
     3              '     FOR VARIABLE',4I12,' FOR DATE',I11,
     4              '.  IER SET = 191.'/
     5              '     NORMALIZED VALUES ARE:',8F7.3)
            IER=191
         ENDIF
C
 160  CONTINUE
C
C        AT THIS POINT, THE PROBABILITIES OF THE 8 (M=1,8) CATEGORIES
C        U, C, P, F, S, B, O, AND X ARE IN XDATA(K,M), RESPECTIVELY,
C        FOR EACH STATION K = 1,NSTA.
C   
      GO TO 350  
C     
C        THIS VARIABLE CANNOT BE COMPUTED.  SET THE ARRAY TO MISSING.
C        THE MAX FUNCTION GUARDS AGAINST NCAT LT 0 AND ALWAYS SETS
C        ONE COLUMN TO MISSING.  THIS IS RELATIVELY UNIMPORTANT
C        BECAUSE THE CALLING ROUTINES WILL DO THE SAME THING.
C     
 270  DO 280 K=1,NSTA
C
         DO 275 J=1,MAX(1,NCAT)
            XDATA(K,J)=9999.
 275     CONTINUE
C
 280  CONTINUE
 
 350  RETURN
      END
