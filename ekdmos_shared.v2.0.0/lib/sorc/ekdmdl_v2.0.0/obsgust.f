      SUBROUTINE OBSGUST(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                   SDATA,ND1,NSTA,IPACK,IWORK,FD1,FD2,ND2X3,
     2                   LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                   IS0,IS1,IS2,IS4,ND7,ISTAV,L3264B,IER)
C
C        FEBRUARY    2005   WIEDENFELD    MDL   MOS-2000
C        FEBRUARY    2006   WIEDENFELD    MDL   CLEANED UP CODE.  ADDED
C                                               NINT IN ALL IF CHECKS 
C                                               INVOLVING COMPARISONS
C                                               OF REAL AND INTEGER.
C                                               REMOVED ID FROM CALL.
C        MARCH       2006   RUDACK        MDL   ADDED PREDICTAND OF
C                                               CONTINUOUS WIND GUST
C
C        PURPOSE 
C            THIS SUBROUTINE WILL CREATE THREE DIFFERENT WIND GUST 
C            VARIABLES.
C              1. DIFFERENCE BETWEEN WIND SPEED AND WIND GUST.
C              2. MODIFIED WIND GUST. (SETS MISSING GUSTS TO 0)
C              3. DIFFERENCE BETWEEN WIND GUST AND WIND SPEED. 
C                 (SETS MISSING GUST TO 0 WIND SPD IF OBS IS NON-
C                  MISSING - PREDICTAND)
C              4. SETS THE WIND GUST VALUES BELOW 14 KTS TO MISSING,
C                 OTHERWISE THE WIND GUST VALUE REMAINS UNCHANGED.
C            
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               704 212 - DIFFERENCE BETWEEN WIND GUST AND WIND SPEED.
C               704 213 - MODIFIED WIND GUST (SETS MISSING GUSTS TO 0 
C                         WHEN SPD OB IS AVAILABLE).
C               704 214 - DIFFERENCE BETWEEN WIND GUST AND WIND SPEED.
C                         (SETS MISSING GUST TO 0 WHEN SPD OB IS
C                          AVAILABLE)
C               704 215 - CONTINUOUS WIND GUST (WIND GUSTS LESS THAN
C                         14 KTS ARE SET TO MISSING)
C
C        DATA SET USE 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE 
C                       (OUTPUT). 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                       (INPUT-OUTPUT). 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                       (INPUT). 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM 
C                       ACCESS (INPUT-OUTPUT).
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
C            SDATA(K) = DATA TO RETURN (K=1,ND1) (OUTPUT).
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                       WITH.  DIMENSION OF SDATA (INPUT).
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING
C                       DEALT WITH (INPUT).
C            IPACK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).
C            IWORK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).
C       FD1(J),FD2(J) = WORK ARRAYS (J=1,ND2X3). FD1 CONTAINS
C                       WIND GUST AND FD2 CONTAINS WIND SPEED
C                       (INTERNAL).
C               ND2X3 = FORMER DIMENSION OF IPACK(),IWORK(),FD1(),
C                       AND FD2(). (INPUT)
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
C                              MOSTORE( , ). LATER USED AS A WAY OF 
C                              DETERMINING WHETHER TO KEEP THIS 
C                              VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ), (INPUT). 
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN (INPUT).  
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA 
C                       IDENTIFIED IN LSTORE( , ) (J=1,ND10). WHEN
C                       CORE( ) IS FULL, DATA ARE STORED ON DISK
C                       (OUTPUT).
C                ND10 = DIMENSION OF CORE( ), (INPUT).
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE (INPUT).  
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING  COUNT FROM THE BEGINNING OF 
C                       THE PROGRAM.  THIS COUNT IS MAINTAINED IN 
C                       CASE THE USER NEEDS IT (DIAGNOSTICS, ETC.),
C                       (INTERNAL).  
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
C                        52 = NWORDS DOES NOT EQUAL NSTA.
C                       103 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED
C                             IN THIS SUBROUTINE.
C                       SEE GFETCH FOR VALUES. (INTERNAL-OUTPUT)
C
C         ADDITIONAL VARIABLES
C              IENTER = NUMBER OF TIMES SUBROUTINE IS ENTERED
C                       (INTERNAL).
C                   J = INTEGER COUNTER (INTERNAL). 
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD1( ) "WIND GUST" (J=1,4) (INTERNAL).
C               MD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD2( ) "WIND SPEED" (J=1,4) 
C                       (INTERNAL).
C               MISSP = PRIMARY MISSING VALUE INDICATOR. RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED (INTERNAL). 
C               MISSS = SECONDARY MISSING VALUE INDICATOR. RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED (INTERNAL). 
C               NPACK = 2 FOR TDL GRIB PACKED DATA: 1 FOR NOT PACKED.
C                       THIS IS STORED IN LSTORE(7, ) (INTERNAL).
C               NSLAB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE FIRST FIELD.  THIS IS THE
C                       VALUE OF NSLAB RETURNED.  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED (INTERNAL).  
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT
C                       THE RECORD HAS BEEN FETCHED. THIS IS STORED 
C                       IN LSTORE(9, ) (INTERNAL).
C              NWORDS = NUMBER OF WORDS RETURNED IN FD1 OR FD2( ), 
C                       (INTERNAL).
C
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH
C
      IMPLICIT NONE 
C
      INTEGER IDPARS(15),JD(4)
      INTEGER IPACK(ND2X3),IWORK(ND2X3)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER LD(4),MD(4)
      INTEGER KFILDO,KFIL10,NDATE,ND1,NSTA,ND7,ISTAV,L3264B,IER,
     1        ND9,LITEMS,ND10,NBLOCK,NFETCH,IENTER,NTIMES,NWORDS,
     2        NPACK,NSLAB,J,MISSP,MISSS,ND2X3
C
      REAL SDATA(ND1)
      REAL FD1(ND2X3),FD2(ND2X3)
      REAL CORE(ND10)
C
      DATA IENTER/0/
C
      SAVE IENTER
C
C        STEP 1A. VERIFY THE PROCESSING OF WIND GUST MODIFICATIONS.
C
      IF((JD(1).EQ.704212000).OR.(JD(1).EQ.704213000).OR.
     1   (JD(1).EQ.704214000).OR.(JD(1).EQ.704215000))THEN
        GOTO 151
      ELSE
        WRITE(KFILDO,150)(JD(J),J=1,4)
 150    FORMAT(/,' ****OBSGUST ENTERED FOR INCORRECT PREDICTOR ',
     1          I9.9,2I10.9,I4.3)
	IER=103
	GO TO 800
      ENDIF
C
C        STEP 1B. INITIALIZATION
C
 151  IER=0
      ISTAV=1
      IENTER=IENTER+1
C        CONSTRUCT THE ARRAY LD FOR WIND GUST          
C
      LD(1)=704211000
      LD(2)=IDPARS(7)
      LD(3)=IDPARS(9)*1000000+IDPARS(12)
      LD(4)=0
C
C        CONSTRUCT THE ARRAY MD FOR WIND SPEED              
C
      MD(1)=704210000
      MD(2)=IDPARS(7)
      MD(3)=IDPARS(9)*1000000+IDPARS(12)
      MD(4)=0
C      
C        STEP 2A. FETCH THE HOURLY WIND GUST      
C
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4            IER)
C
      IF((IER.NE.0).AND.(IENTER.EQ.1)) THEN
        WRITE(KFILDO,205) (JD(J),J=1,4),IER
 205    FORMAT(/,' ****ERROR IN OBSGUST, FOR FIRST PROCESS DATE,',
     1         ' HOURLY WIND GUST IS MISSING', 
     2         /,'     POSSIBLE DATA GAP.  ALL VALUES',
     3         ' ARE MISSING,',
     4         ' FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER =',I4)
        GO TO 800
      ELSEIF((IER.NE.0).AND.(IENTER.GT.1)) THEN
	GO TO 800
      ENDIF
C
      IF(NWORDS.NE.NSTA) THEN
        WRITE(KFILDO,210)NWORDS,NSTA
 210    FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1         ' NOT EQUAL TO NSTA =',I6,' FOR WIND GUST',
     2         ' IN OBSGUST. DATA SET TO MISSING.')
        IER=52
        GO TO 800
      ENDIF
C
C        STEP 2B. FETCH THE WIND SPEED              
C
      CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4            IER)
C
      IF((IER.NE.0).AND.(IENTER.EQ.1)) THEN
        WRITE(KFILDO,212) (JD(J),J=1,4),IER
 212    FORMAT(/,' ****ERROR IN OBSGUST, FOR FIRST PROCESS DATE,',
     1         ' WIND SPEED IS MISSING', 
     2         /,'     POSSIBLE DATA GAP.  ALL VALUES',
     3         ' ARE MISSING',
     4         ' FOR VARIABLE ',I9.9,2I10.9,I4.3,'.   IER =',I4)
        GO TO 800
      ELSEIF((IER.NE.0).AND.(IENTER.GT.1)) THEN
        GO TO 800
      ENDIF
C
      IF(NWORDS.NE.NSTA) THEN
        WRITE(KFILDO,215)NWORDS,NSTA
 215    FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1          ' NOT EQUAL TO NSTA =',I6,' FOR WIND SPEED',
     2          ' IN OBSGUST. DATA SET TO MISSING.') 
        IER=52 
        GO TO 800 
      ENDIF
C
C        STEP 3.  WIND GUST DIFF ID. SET ALL MISSING GUST VALUES TO 9999.
C                 SET ALL MISSING SPEED VALUES TO MISSING.
C                 OTHERWISE SET TO DIFFERENCE BETWEEN SPEED AND GUST.
C
      IF(JD(1).EQ.704212000)THEN
         DO 220 J=1,NSTA
       	    IF(NINT(FD1(J)).EQ.9999.OR.NINT(FD2(J)).EQ.9999)THEN
               SDATA(J)=9999.
            ELSE
               SDATA(J)=FD1(J)-FD2(J)
            ENDIF
 220     CONTINUE
      ENDIF
C
C        STEP 4.  MOD WIND GUST ID.  SET ALL MISSING GUST VALUES TO 0.  
C                 SET ALL MISSING SPEED VALUES TO MISSING.  OTHERWISE
C                 SET TO GUST.
C
      IF(JD(1).EQ.704213000)THEN
         DO 250 J=1,NSTA
       	    IF(NINT(FD1(J)).EQ.9999.AND.NINT(FD2(J)).NE.9999)THEN
               SDATA(J)=0.
            ELSEIF(NINT(FD2(J)).EQ.9999)THEN
               SDATA(J)=9999.
            ELSE
               SDATA(J)=FD1(J)
            ENDIF
 250     CONTINUE
      ENDIF
C
C        STEP 5.  WIND GUST DIFF W/O MISSINGS.  SET ALL MISSING WIND 
C                 GUST VALUES TO 0. SET ALL NON-MISSING VALUES TO THE 
C                 DIFFERENCE IN WIND GUST - WIND SPD.
C
      IF(JD(1).EQ.704214000)THEN
         DO 280 J=1,NSTA
       	    IF(NINT(FD1(J)).NE.9999.AND.NINT(FD2(J)).NE.9999)THEN                       
               SDATA(J)=FD1(J)-FD2(J)
            ELSEIF(NINT(FD2(J)).EQ.9999)THEN
               SDATA(J)=9999.
            ELSE
               SDATA(J)=0.0
            ENDIF
 280     CONTINUE
      ENDIF
C
C        STEP 6.   ASSIGN THE WIND GUST TO MISSING IF A GUST WAS NOT
C                  REPORTED OR IT IS LESS THAN 14 KTS.  OBSERVED
C                  WIND GUSTS GREATER THAN OR EQUAL TO 14 KTS ARE SET
C                  TO THE CONTINUOUS WIND GUST.
C
      IF(JD(1).EQ.704215000)THEN
         DO 350 J=1,NSTA
            IF(NINT(FD2(J)).NE.9999) THEN
               IF((FD1(J).LE.13.95).OR.
     1            (NINT(FD1(J)).EQ.9999)) THEN
                  SDATA(J)=9999.
               ELSEIF((FD1(J).GT.13.95).AND.
     1                (NINT(FD1(J)).NE.9999)) THEN
                  SDATA(J)=FD1(J)
               ELSE
                  SDATA(J)=9999.
               ENDIF
            ELSE
               SDATA(J)=9999.
            ENDIF
 350     CONTINUE
      ENDIF
C
      GO TO 850 
C
 800  DO 810 J=1,ND1
        SDATA(J)=9999.
 810  CONTINUE
C
D     WRITE(KFILDO,815) IER,(JD(J),J=1,4)
D815  FORMAT(/,' ****ERROR IN OBSGUST, IER =',I5,' FOR VARIABLE ',4I12)
C
 850  RETURN
      END     
