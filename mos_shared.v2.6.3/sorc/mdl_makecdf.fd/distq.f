      SUBROUTINE DISTQ(KFILDO,KFIL10,KFILAO,IP12,IP15,
     1                 KFILRA,RACESS,NUMRA,
     2                 ID,IDPARS,JD,TRESHL,TRESHU,ITAU,NVRBL,
     3                 NDATE,CCALL,ISDATA,SDATA,ND1,NSTA,
     4                 XDATA,SDDATA,ND2,KER,ISD,SD,DS,NN,M,
     5                 ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     6                 LSTORE,ND9,LITEMS,CORE,ND10,
     7                 NBLOCK,NFETCH,
     8                 IS0,IS1,IS2,IS4,ND7,
     9                 XAXIS,PDF,CDF,ND11,NPCDF,CDFTH,XCDF,NCDFTH,
     A                 L3264B,L3264W,ISTOP,IER)
C
C        MAY       2008   WIEDENFELD   CREATED. ADAPTED FROM DISTF.  THIS SUBROUTINE
C                                      COMPUTES THE CDF OF QPF, BASED OFF
C                                      CUMULATIVE FROM ABOVE PROBABILITES OF
C                                      X MEMBERS. 
C        JUNE      2008   WIEDENFELD   FIXED CODE SO THAT CUMULATIVE FROM ABOVE TO 
C                                      CUMULATIVE FROM BELOW PRB'S ARE CORRECT.
C
C        PURPOSE
C            DISTQ IS CALLED FROM OPTY AND COMPUTES THE CDF OF QPF, BASED OFF
C            CUMULATIVE FROM ABOVE PROBABILITES OF X MEMBERS.  IT ALSO INSURES
C            THAT ALL RAW PROBABILITES ARE MONOTONICALLY DECREASING AND BETWEEN
C            0 AND 1.
C   
C        DATA SET USE 
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C            KFIL10    - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                        ACCESS.  (INPUT-OUTPUT) 
C            KFILAO    - UNIT NUMBER OF ASCII OUTPUT FILE.  ZERO MEANS
C                        OUTPUT WILL NOT BE WRITTEN.  (OUTPUT)
C            IP12      - LIST OF STATIONS ON THE INPUT FILES.  (OUTPUT)
C            IP15      - LIST OF DATA IN DIST.  (OUTPUT)
C            KFILRA(J) - UNIT NUMBERS FOR EXTERNAL RANDOM ACCESS FILES
C                        (J=1,5).  (INPUT)
C 
C        VARIABLES 
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C              KFILAO = UNIT NUMBER OF ASCII OUTPUT FILE.
C                       ZERO MEANS OUTPUT WILL NOT BE WRITTEN.  (INPUT)
C                IP12 = INDICATES WHETHER (>0) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE EXTERNAL RANDOM ACCESS FILES
C                       WILL BE LISTED TO UNIT IP12.  (INPUT)
C                IP15 = LIST OF DATA IN DIST.  (INPUT)
C           KFILRA(J) = THE UNIT NUMBERS FOR THE MOS-2000 EXTERNAL
C                       RANDOM ACCESS FILES (J=1,NUMRA)
C           RACESS(J) = THE FILE NAME FOR THE MOS-2000 EXTERNAL RANDOM
C                       ACCESS FILE (J=1,NUMRA).  (CHARACTER*60)
C               NUMRA = THE NUMBER OF VALUES IN KFILRA( ) AND RACESS( ).
C                       (INPUT)
C             ID(J,N) = THE VARIABLE ID (J=1,4) (N=1,NVRBL).  (INPUT)
C         IDPARS(J,N) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       VARIABLE ID CORRESPONDING TO ID( ) (J=1,15)
C                       (N=1,NVRBL).
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY
C                            1 LAYER),
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
C                       (INPUT)
C             JD(J,N) = THE BASIC INTEGER VARIABLE ID (J=1,4)
C                       (N=1,NVRBL).  THIS IS THE SAME AS ID(J), EXCEPT
C                       THAT THE FOLLOWING PORTIONS ARE OMITTED:
C                       B = IDPARS(3),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       (INPUT)
C           TRESHL(N) = THE LOWER BINARY THRESHOLD ASSOCIATED WITH 
C                       IDPARS( ,N) (N=1,NVRBL).  (INPUT)
C           TRESHU(N) = THE UPPER BINARY THRESHOLD ASSOCIATED WITH
C                       IDPARS( ,N) (N=1,NVRBL).  (INPUT)
C             ITAU(N) = THE NUMBER OF HOURS AHEAD TO FIND A VARIABLE
C                       (N=1,NVRBL).  THIS DOES NOT APPLY TO ALL
C                       SUBROUTINES.  NO PRESENT USE; SHOULD BE ZERO.
C                       (INPUT)
C               NVRBL = THE NUMBER OF VARIABLES IN ID( , ), ETC.
C                       (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH VARIABLE IS NEEDED.
C                       (INPUT)
C            CCALL(K) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (K=1,NSTA).  ALL STATION DATA ARE
C                       KEYED TO THIS LIST.  (CHARACTER*8)  (INPUT)
C           ISDATA(K) = WORK ARRAY (K=1,ND1). (INTERNAL)
C            SDATA(K) = WORK ARRAY (K=1,ND1).  (INTERNAL)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                       WITH.  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C          XDATA(K,L) = THE ARRAY USED FOR VECTOR VALUES (K=1,ND1) 
C                       (L=1,ND2).  THE COLUMNS HOLD THE VALUES FOR
C                       THE M MEMBERS (SEE M BELOW).
C                       (INTERNAL/OUTPUT)
C         SDDATA(K,L) = USED FOR THE STANDARD ERRORS (K=1,ND1)
C                       (L=1,ND2).  THE COLUMNS HOLD THE SD'S FOR THE
C                       M MEMBERS (SEE M BELOW).  (INTERNAL/OUTPUT)
C                 ND2 = MAXIMUM NUMBER OF ENSEMBLE MEMBERS.  (INPUT)
C              KER(N) = DESIGNATES THE KERNAL TO BE USED FOR VARIABLE N
C                       (N=1,ND4).
C                       1 = NORMAL (GAUSIAN).
C                       (INPUT)
C              ISD(N) = DESIGNATES WHETHER THE KERNAL WIDTH FOR
C                       VARIABLE N (N=1,ND4) IS TO BE TAKEN FROM SD( )
C                       IN THE VARIABLE RECORD OR FROM A PACKED INPUT
C                       RECORD.
C                       0 = COMES FROM PACKED RECORD;
C                       2 = WHEN THERE ARE MULTIPLE ENSEMBLES OR
C                           ONLY ONE ENSEMBLE AND SD( ) > 9, 
C                           CALL KERNELW; OTHERWISE, CALL KERNEL.
C                       3 = CALL SUBROUTINE SCALE3 TO SCALE THE
C                           DISPERSION OF THE KERNEL DENSITY OUTPUT
C                           TO APPROXIMATELY WHAT IT WOULD HAVE BEEN
C                           WITH A SINGLE RUN.
C                       (INPUT)
C               SD(N) = A FACTOR TO USE IN THE SPREAD ADJUSTMENT FOR
C                       MULTIPLE ENSEMBLES FOR THIS VARIABLE N
C                       (N=1,ND4).  (INPUT)
C               DS(N) = SCALING FACTOR FOR THE STANDARD DEVIATION FOR
C                       THIS VARIABLE (N=1,ND4).  (INPUT)      
C                  NN = ON INPUT, THE FIRST VARIABLE IN THE ID LIST NOT
C                       ALREADY USED.  THIS IS THE VARIABLE TO PROCESS.
C                       ON OUTPUT, THE FIRST VARIABLE IN THE ID LIST NOT
C                       USED.  WHEN ALL VARIABLES HAVE BEEN PROCESSED,
C                       NN IS RETURNED = 9999.  (INPUT/OUTPUT)
C                   M = THE NUMBER OF MEMBERS AVERAGED IN DISTF.
C                       (OUTPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN 
C                       INTEGER VARIABLE (L=1,L3264W) (K=1,ND1).
C                       NOTE THAT THIS REQUIRES TWO 32-BIT WORDS TO HOLD
C                       THE DESCRIPTION BUT ONLY ONE 64-BIT WORD.
C                       NEEDED IN CONST FOR ARGUMENT TO RDTDLM.
C                       EQUIVALENCED TO CCALLD( ).
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).  EQUIVALENCED
C                       TO ICALLD( , ).  (INTERNAL)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ), WORK( ), DATA( ), AND
C                       CALLD( ), AND SECOND DIMENSION OF ICALLD( , ).
C                       (INPUT)
C         LSTORE(L,J) = THE ARRAY TO HOLD INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS).
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK, 
C                              THIS IS MINUS THE RECORD NUMBER WHERE 
C                              THE DATA START.  NOTE THAT WHEN A FIELD 
C                              CANNOT BE STORED IN CORE( ), IT IS PUT
C                              ON DISK.  IT MAY BE THAT A LATER FIELD 
C                              WILL FIT, AND IT IS PUT IN CORE( ).
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDLPACK, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --THE NUMBER IN THE LIST OF INPUT SOURCES
C                              THIS VARIABLE CAME FROM.
C                       L=11 --FOR U715, THIS WILL BE 7777, INDICATING
C                              THE VARIABLE IS ALWAYS STORED IN THE
C                              INTERNAL STORAGE FACILITY.
C                       L=12 --MINUS THE NUMBER OF HOURS THIS VARIABLE
C                              MUST BE KEPT.  LATER SET TO A DATE WHEN
C                              THIS VARIABLE CAN BE DISCARDED.
C                       (INPUT)
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       FILLED.  (INPUT)
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
C            XAXIS(J) = THE DATA VALUES ALONG THE X-AXIS, EACH
C                       VALUE CORRESPONDING TO A VALUE IN PDF(J) AND
C                       CDF(J) (J=1,NPCDF).  (OUTPUT) 
C              PDF(J) = THE PDF WRITTEN TO UNIT KFILAO (J=1,NPCDF).
C                       (OUTPUT)
C              CDF(J) = THE CDF WRITTEN TO UNIT KFILAO (J=1,NPCDF).
C                       (OUTPUT) 
C                ND11 = THE MAXIMUM NUMBER OF VALUES IN XAXIS( ),
C                       PDF( ), AND CDF( ).  (INPUT)
C               NPCDF = THE NUMBER OF VALUES IN PDF( ), CDF( ), AND
C                       XAXIS( ).  (OUTPUT)
C            CDFTH(J) = THE THRESHOLDS, OR PROBABILITY LEVELS, FOR
C                       OUTPUTTING THE CDF VALUES (J=1,NCDFTH).
C                       (INPUT)
C           XCDF(K,J) = THE VALUES FOR STATION K (K=1,NSTA) OF THE CDF
C                       FOR EACH OF THE LEVELS IN CDFTH(J) (J=1,NCDFTH)
C                       (OUTPUT)
C              NCDFTH = NUMBER OF VALUES IN CDFTH( ) AND XCDF( ).
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).  
C                       CALCULATED BY PARAMETER, BASED ON L3464B.
C                       (INPUT)
C            ISTOP(J) = FOR J=1, ISTOP IS INCREMENTED BY 1 EACH TIME
C                       AN ERROR OCCURS THAT MAY BE FATAL.
C                       FOR J=2, ISTOP IS INCREMENTED BY 1 WHENEVER AN
C                       INPUT DATA RECORD IS NOT FOUND.
C                       FOR J=3, ISTOP IS INCREMENTED BY 1 WHENEVER AN
C                       AN UNUSUAL CIRCUMSTANCE OCCURS WHICH IS NOT FATAL.
C                       (INPUT/OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       777 = SD = 0 FOUND IN KERNEL.
C                       OTHER VALUES CAN COME FROM CALLED SUBROUTINES.
C                       (OUTPUT)
C               MDATE = NDATE UPDATED WITH ITAU( ).  NO REASON FOR
C                       ITAU( ) TO BE OTHER THAN ZERO.  NEEDED FOR 
C                       RETVEC.  (INTERNAL)
C            RDATA(J) = HOLDS THE SINGLE VALUE FORECASTS TO FURNISH
C                       TO KERNEL (J=1,ND1).  (AUTOMATIC)  (INTERNAL)
C                  MM = THE NUMBER OF VALUES IN RDATA( ) AND SDATA( ).
C                       THIS IS THE NUMBER OF ENSEMBLES BEING 
C                       PROCESSED.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES USED 
C            TIMPR, PRSID1, RETVEC, KERNEL
      PARAMETER (NCAT=13)
C
      CHARACTER*8 CCALL(ND1)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION ISDATA(ND1),SDATA(ND1)
      DIMENSION XMNDTA(ND1,NCAT),XCOUNT(ND1,NCAT)
      DIMENSION XDATA(ND1,ND2),SDDATA(ND1,ND2)
      DIMENSION ID(4,NVRBL),IDPARS(15,NVRBL),
     1          TRESHL(NVRBL),TRESHU(NVRBL),JD(4,NVRBL),ITAU(NVRBL),
     2          KER(NVRBL),ISD(NVRBL),SD(NVRBL),DS(NVRBL)
      DIMENSION ICALLD(L3264W,ND5),IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION CDFTH(NCDFTH),XCDF(ND1,NCDFTH)
      DIMENSION XAXIS(NCAT),PDF(ND11),CDF(ND11)
      DIMENSION ITABLE(NCAT),XVALS(NCAT)
      DIMENSION KFILRA(5),LD(4),LDPARS(15),ISTOP(3)
C
      DATA JFIRST/0/
      DATA ITABLE/950052, 495051, 950051, 149500, 195000, 
     1            245000, 295000, 395000, 495000, 749500, 
     2            995000, 149501, 195001/
      DATA XVALS/0.010, 0.050, 0.100, 0.150, 
     1           0.200, 0.250, 0.300, 0.400, 
     2           0.500, 0.750, 1.000, 1.500, 2.000/
      SAVE JFIRST,NDATES
C        NDATE IS SAVED IN NDATES ON THE FIRST ENTRY SO THAT
C        DIAGNOSTIC 212 WON'T PRINT AFTER THE FIRST DAY.
C
      IF(JFIRST.EQ.0)THEN
         NDATES=NDATE
         JFIRST=1
      ENDIF
      DO 91 K=1,NSTA
         DO 90 L=1,NCAT
            XMNDTA(K,L)=0.
            XCOUNT(K,L)=0.
   90    CONTINUE
   91 CONTINUE
C
C        GET SINGLE VALUE ESTIMATES AND THE STANDARD DEVIATION FOR
C        EACH ENSEMBLE MEMBER (DENOTED BY DD).  THIS CAN BE A SINGLE
C        RUN WITH OR WITHOUT AN ACCOMPANYING STANDARD DEVIATION
C        PACKED RECORD.
C
D     CALL TIMPR(KFILDO,KFILDO,'START DISTF         ')
C
      IER=0
      IFIRST=0
      NNSAVE=9999
C        INITIALIZE NSAVE IN CASE ALL DATA RETRIEVES ARE SUCCESSFUL.
      M=0
C        M IS THE COLUMN IN XDATA( , ) TO RETRIEVE THE DATA.
C
      DO 200 N=NN,NVRBL
C        NVRBL IS THE NUMBER OF VALUES IN THE ID( , ) LIST.
C        NN IS THE LOCATION OF THE FIRST ONE NOT USED.
      MDATE=NDATE+ITAU(N)
C        I KNOW OF NO CURRENT USE FOR TAU NE 0, BUT IS CARRIED ALONG.
C***      WRITE(KFILDO,100)IER,IFIRST,NNSAVE,M,N,NN,NVRBL,ITAU(N),MDATE
C*** 100  FORMAT(/' AT 100--IER,IFIRST,NNSAVE,M,N,NN,NVRBL,ITAU(N),MDATE',
C***     1        8I6,I11)
C***      WRITE(KFILDO,101)(IDPARS(J,NN),J=1,15),(IDPARS(J,N),J=1,15)
C*** 101  FORMAT(/' IDPARS(J,NN),J=1,15),(IDPARS(J,N),J=1,15)',/,
C***     1        (15I7))
C
C        GET THE SINGLE VALUE FORECAST AND ITS ASSOCIATED
C        STANDARD DEVIATION FOR IDS THAT ARE THE SAME
C        EXCEPT FOR THE DD.
C
C    
      IF(ID(1,NN)/100.EQ.ID(1,N)/100.AND.
     1   ID(2,NN).EQ.ID(2,N).AND.
     2   IDPARS(8,NN).EQ.IDPARS(8,N).AND.
     3   IDPARS(10,NN).EQ.IDPARS(10,N).AND.
     4   IDPARS(11,NN).EQ.IDPARS(11,N).AND.
     5   IDPARS(12,NN)-IDPARS(9,NN).EQ.IDPARS(12,N)-IDPARS(9,N).AND.
     6   ID(4,NN).EQ.ID(4,N))THEN
C
C           THE ID OF VARIABLE N AGREES WITH THAT OF THE BASE VARIABLE
C           NN, EXCEPT DD, R, AND TAU.  THE TEST ASSURES THAT THE DATES
C           AND PROJECTIONS ARE SUCH THAT THE FORECASTS VERIFY AT THE
C           SAME TIME.  NORMALLY, IDPARS(9,NN) WILL BE ZERO.
C
C           RETRIEVE THE SINGLE VALUE FORECASTS FOR EACH CUMULATIVE FROM
C           ABOVE CATEGORY.
C
         LD(1)=ID(1,N)+100
         LD(2)=ID(2,N)
         LD(3)=ID(3,N)
         IERCNT=0
         DO 130 L=1,NCAT-1
            LD(4)=ITABLE(L)*1000
            CALL PRSID1(KFILDO,LD,LDPARS)
            ITIME=IDPARS(9,N)
C           ITIME IS USED IN GFETCH.  IT IS CALLED ITAU IN RETVEC.         
            
            CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  LD,LDPARS,JD(1,N),ITIME,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA(1,L),ND1,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,IER)
C           JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C           IN CALL TO CONST, BUT IS NOT USED THERE EITHER.
C
            IF(IER.NE.0)THEN
            WRITE(KFILDO,125)(LD(J),J=1,4),NDATE
  125       FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN DISTF',
     1             2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,' FOR DATE',I12)
C
            IERCNT=IERCNT+1
            ENDIF
  130    CONTINUE
C
C        IF THERE IS EVEN ONE ERROR WHILE FETCHING CUMULATIVE FROM
C        ABOVE CATEGORIES FOR A SINGLE MEMBER THEN SET XCDF TO MISSING
C        AND MOVE ON TO NEXT VARIABLE. ELSE COUNT MEMBERS AND SUM UP
C        PROBABILITY
C
         IF(IERCNT.NE.0)THEN
            WRITE(KFILDO,135)(ID(J,N),J=1,4),NDATE
  135       FORMAT(/,' ****VARIABLE NOT COMPUTED IN DISTQC',
     1             2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,' FOR DATE',I12)
C
            DO 137 K=1,NSTA
            DO 136 L=1,NCDFTH
               XCDF(K,L)=9999.
  136       CONTINUE
  137       CONTINUE
            GOTO 200
         ELSE
            M=M+1
            DO 139 K=1,NSTA
            DO 138 L=1,NCAT-1
               IF(NINT(XDATA(K,L)).EQ.9997)XDATA(K,L)=0.
               IF(NINT(XDATA(K,L)).NE.9999)THEN
                  XMNDTA(K,L)=XMNDTA(K,L)+XDATA(K,L)
                  XCOUNT(K,L)=XCOUNT(K,L)+1
               ENDIF
  138       CONTINUE
  139       CONTINUE
         ENDIF
C
      ELSE
C
         IF(IFIRST.EQ.0)THEN
            NNSAVE=N
C               NNSAVE IS NOW THE FIRST VARIABLE IN THE ID( , ) LIST
C               NOT USED.  LATER, TRANSFER NNSAVE TO NN TO BE USED
C               ON NEXT ENTRY.
            IFIRST=1
         ENDIF
C
      ENDIF
C
 200  CONTINUE
C 
      IF(IP15.NE.0)THEN
C
         DO 206 K=1,NSTA
         WRITE(IP15,204)CCALL(K),NDATE,(XDATA(K,L),L=2,NCAT)
 204     FORMAT(/' SINGLE VALUE ENSEMBLE FORECASTS FOR STATION ',A8,
     1           '  FOR DATE ',I12,/,('     ',15F8.3))
 206     CONTINUE
C
      ENDIF
C
C     MAIN LOOP FOR STATION COMPUTATIONS.
C
      DO 250 K=1,NSTA
C
C     WRITE(KFILDO,208)K,CCALL(K)
C208  FORMAT(/' STARTING STATION NO.',I5,2X,A8)
C
C     NOW LOOP THROUGH EACH CATEGORY AND COMPUTE ENSEMBLE MEAN
C     MEANS ARE MULTIPLIED BY 1000 TO KEEP PRECISION.  THE FIRST 
C     CATEGORY IS NOT COMPUTED SINCE AT THIS POINT IT IS ZERO.
C     ONCE MEAN IS COMPUTED INSURE THAT ALL CATEGORIES ARE BETWEEN 0 AND 1.
C
      DO 207 L=1,NCAT-1
         IF(XCOUNT(K,L).GT.0)THEN
            XMNDTA(K,L)=NINT(XMNDTA(K,L)*1000/XCOUNT(K,L))
            XMNDTA(K,L)=XMNDTA(K,L)/1000.
            IF(XMNDTA(K,L).LT.0.)THEN
               XMNDTA(K,L)=0.
            ELSEIF(XMNDTA(K,L).GT.1.)THEN
               XMNDTA(K,L)=1.
            ENDIF
         ELSE
            XMNDTA(K,L)=9999.
         ENDIF
 207  CONTINUE
C
C     LOOP THROUGH EACH CATEGORY TO INSURE PROBABILITIES ARE MONOTONIC.
C
      DO 2071 L=1,NCAT-2
         IF(NINT(XMNDTA(K,L+1)).NE.9999)THEN
            IF(XMNDTA(K,L+1).GT.XMNDTA(K,L))XMNDTA(K,L+1)=XMNDTA(K,L)
         ENDIF
 2071 CONTINUE
C
C     SET XAXIS OF CDF USING XVALS
C
      DO 209 L=1,NCAT
         XAXIS(L)=XVALS(L)
 209  CONTINUE
C
C     CREATE CUMULATIVE FROM BELOW PROBABILITES BY SUBTRACTING
C     1 FROM THE CUMULATIVE FROM ABOVE B
C
      DO 210 L=1,NCAT-1
         IF(NINT(XMNDTA(K,L)).NE.9999)THEN
            CDF(L)=1.-XMNDTA(K,L) 
         ELSE
C           WRITE(KFILDO,212)CCALL(K),(ID(J,NN),J=1,4),NDATE
            DO 2101 I=1,NCDFTH
               XCDF(K,I)=9999.
 2101       CONTINUE
            GOTO 250
         ENDIF
 210  CONTINUE
C
C     SET THE FINAL CDF CAT TO 1 SINCE THE CDF MUST END AT 1.
C
      CDF(NCAT)=1.
C
C     CHECK TO SEE HOW MANY CDF VALUES ARE NEED TO GET UNITY
C     NPCDF WILL BE THE NUMBER OF POINTS ON THE CDF THAT ARE LESS
C     THEN OR EQUAL TO 1. WITHOUT REPEATING 1.
C
      IEND=0
      DO 211 L=1,NCAT
         IF(CDF(L).EQ.1.0.AND.IEND.NE.1)THEN
             NPCDF=L
             IEND=1
         ENDIF
 211  CONTINUE
C
C     INSURE THAT NO POINTS ARE DUPICLATED ON THE CDF.
C
      DO 2111 L=1,NPCDF
         RVAL=CDF(L)
         DO 2110 I=L,NPCDF
        
         IF(I.NE.L)THEN
            IF(RVAL.EQ.CDF(I))THEN
               IF(CDF(L).NE.0.)THEN
               CDF(L)=CDF(L)-.00001
               ENDIF
            ENDIF
         ENDIF
 2110    CONTINUE
 2111 CONTINUE
C
      IF(IERCNT.NE.0)THEN
C
C        IF(NDATE.EQ.NDATES)THEN
C              THIS DIAGNOSTIC WILL PRINT ON ONLY THE FIRST DAY.
C           WRITE(KFILDO,212)CCALL(K),(ID(J,NN),J=1,4),NDATE
C212        FORMAT(/,' NO FORECASTS FOR STATION ',A8,
C    1              ' FOR VARIABLE',
C    2                2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
C    3               ' FOR DATE',I12,'.  (PRINT ON DAY 1.)')
C
C              WHEN THERE ARE NO GOOD NON-MISSING FORECASTS
C              FOR STATION K, SET THE XCDF(K,L) VALUES MISSING
C              FOR ALL THRESHOLDS.
C        ENDIF
C
         DO 213 L=1,NCDFTH
         XCDF(K,L)=9999.
 213     CONTINUE
C
         GO TO 250
C
C     ELSEIF(MM.NE.M)THEN
C        WRITE(KFILDO,214)MM,CCALL(K),(ID(J,NN),J=1,4),NDATE
C214     FORMAT(/,' ONLY',I4,' FORECASTS FOR STATION ',A8,
C    1            ' FOR VARIABLE',
C    2             2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
C    3            ' FOR DATE',I12)
D        WRITE(KFILDO,2143)(XDATA(K,L),L=1,M)
D2143    FORMAT(/' FORECASTS',11(F10.3))
D        WRITE(KFILDO,2144)(SDDATA(K,L),L=1,M)
D2144    FORMAT( ' STD DEVS ',11(F10.3))
      ENDIF
C
C
      KERN=1
C        KERN = 1 SIGNIFIES KERNEL WILL BE CALLED.  IF 
C        CHANGED TO 2, KERNELW WILL BE CALLED.
      FAC=1.
C        FAC IS INITIALIZED TO 1 IN CASE SCALE3 IS NOT CALLED.
C
C           IER = 777 WILL BE RETURNED WHEN DS = 0, OR WHEN THE STANDARD
C           DEVIATION IN SDATA( ) IN TENTHS OF UNITS FURNISHED ROUNDS
C           TO 0.  A DIAGNOSTIC WILL HAVE BEEN PRINTED IN KERNAL; THE
C           ONE BELOW WILL EXPLAIN WHICH VARIABLE AND STATION.
C
      IF(IER.EQ.777)THEN
         WRITE(KFILDO,216)(ID(J,NN),J=1,4),CCALL(K)
 216     FORMAT('     VARIABLE ',4I11,' STATION ',A8)
      ELSEIF(IER.EQ.778)THEN
         WRITE(KFILDO,216)(ID(J,NN),J=1,4),CCALL(K)
C
         DO 2165 L=1,NCDFTH
         XCDF(K,L)=9999.
 2165    CONTINUE
C
         GO TO 250
      ENDIF  
C
      IF(KFILAO.NE.0)THEN
C
C           WRITE THE ASCII DATA.  FORM THE ID'S.
C
         LD(1)=ID(1,NN)
C
         IF(M.GT.1)THEN
            LD(1)=(LD(1)/100)*100+76
C              WHEN MORE THAN ONE ENSEMBLE IS INVOLVED, USD DD = 76.
C              OTHERWISE, USE THE DD OF THE MEMBER.
         ENDIF
C
         LD(2)=ID(2,NN)
C           THIS PUTS THE PROBABILITY THRESHOLD INTO LLLL OF WORD 2.
C           IT ASSUMES UUUU = 0, BUT RETAINS THE FIRST DIGIT, V.
         LD(3)=ID(3,NN)
         LD(4)=ID(4,NN)
C
         WRITE(KFILAO,217)CCALL(K),(LD(J),J=1,4),NDATE,NPCDF
217      FORMAT(/,' ',A8,I12,2X,3I11.9,I11.3,I6)
C
D        WRITE(KFILAO,218)(XAXIS(J),J=1,NPCDF)
D218     FORMAT(' ',15F8.3)
D        WRITE(KFILAO,218)(CDF(J),J=1,NPCDF)
C
         WRITE(KFILAO,219)(XAXIS(J),CDF(J),J=1,NPCDF)
 219     FORMAT(' ',2F12.6)
      ENDIF
C
C        FIND THE DATA VALUE IN XAXIS( ) CORRESPONDING TO THE
C        THRESHOLDS IN CDFTH( ) APPLIED TO CDF( ) AND PUT
C        THEM IN XCDF( , ).
C
      NSTART=2
C
      DO 230 L=1,NCDFTH
C
      IF(NPCDF.GE.NSTART)THEN
         DO 220 LL=NSTART,NPCDF
C
            IF(CDF(LL).GT.CDFTH(L))THEN
               DIF=CDF(LL)-CDF(LL-1)
C           
            IF(DIF.LE.0.)THEN
               XCDF(K,L)=XAXIS(LL)
              WRITE(KFILDO,2195)CCALL(K),NDATE
 2195          FORMAT(/' ****CDF( ) AT TWO POINTS ALONG THE',
     1                 ' AXIS ARE EQUAL FOR STATION ',A8,
     2                 '.  PROBABLY AN ERROR.  CONTINUING.',I10)
               ISTOP(3)=ISTOP(3)+1
            ELSE
               F=(CDFTH(L)-CDF(LL-1))/DIF
               XCDF(K,L)=XAXIS(LL-1)+(XAXIS(LL)-XAXIS(LL-1))*F
               IF(XCDF(K,L).LT.0)XCDF(K,L)=0.
C                 PACKING WILL DO ROUNDING.
            ENDIF
C
               NSTART=LL
               GO TO 230
            ENDIF
C
 220     CONTINUE
      ELSE
         XCDF(K,L)=0.
      ENDIF
C
 230  CONTINUE
C
      IF(IP15.NE.0)THEN
         WRITE(IP15,232)NCDFTH,(CDFTH(L),XCDF(K,L),L=1,NCDFTH)
 232     FORMAT(/,' VALUES CORRESPONDING TO ',I4,' THRESHOLDS.',/,
     1           (5(3X,2F8.3)))  
      ENDIF       
C      
 250  CONTINUE
C
      NN=NNSAVE
C*** 300  WRITE(KFILDO,301)N,NN,M
C*** 301  FORMAT(/' AT 301 IN DISTF--N,NN,M',3i12)
  300 RETURN
      END   
