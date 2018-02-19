      SUBROUTINE SUMPRB(KFILDO,KFIL10,IDPARS,ID,NDATE,
     1                  SDATA,ND1,NSTA,IPACK,IWORK,FD1,ND2X3,
     2                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                  IS0,IS1,IS2,IS4,ND7,
     4                  ISTAV,L3264B,IER)
C
C        JUNE 2005   RUDACK, WEISS   MDL   MOS-2000
C        MAY  2006   WEISS           CODE WALK THROUGH MODIFICATIONS
C                                    MOSTLY COSMETIC
C              
C        PURPOSE
C           TO CALCULATE MOS CUMULATIVE CEILING HEIGHT PROBABILITIES,
C           DERIVED BY SUMMING EXCLUSIVE CEILING HEIGHT CATEGORIES.
C
C           THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C              208 043 - CUMULATIVE CEILING HEIGHT PROBABILITIES
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE 
C                     (OUTPUT). 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                     (INPUT-OUTPUT). 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                       (INPUT). 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM 
C                       ACCESS (INPUT).
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID(J) (J=1,15)
C                       DEFINED IN THE CALLING PROGRAM (INPUT).
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 
C                            LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET -- PREVIOUS CYCLE.  
C                            ALWAYS + AND COUNTED BACKWARDS IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               ID(J) = THE INTEGER PREDICTOR ID (J=1,4).
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED
C                       (INPUT).
C            SDATA(K) = SUMMATION OF DISCRETE PROBABILITIES (K=1,NSTA)
C                       (OUTPUT) 
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  (INPUT).
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT WITH
C                       (INPUT).
C            IPACK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).  
C            IWORK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).  
C              FD1(J) = WORK ARRAY (J=1,ND2X3). FD1 CONTAINS EACH FORECAST 
C                       PROBABILITY. (INTERNAL)
C               ND2X3 = FORMER DIMENSION OF IPACK( ),IWORK( ) AND 
C                       FD1( ). (INPUT)
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
C                ND10 = DIMENSION OF CORE( ) (INPUT).
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE (INPUT).  
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING  COUNT FROM THE BEGINNING OF 
C                       THE PROGRAM.  THIS COUNT IS MAINTAINED IN 
C                       CASE THE USER NEEDS IT (DIAGNOSTICS, ETC.). 
C                       (OUTPUT).  
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
C               ISTAV = 1 WHEN THE DATA RETURNED ARE STATION DATA;
C                       EQUALS 0 WHEN THE DATA RETURNED ARE GRID DATA OR
C                       DATA ARE NOT AVAILABLE FOR RETURN. 
C                       (OUTPUT).
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64) (INPUT).
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        52 = NWORDS DOES NOT EQUAL NSTA.
C                       102 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED
C                             IN THIS SUBROUTINE.
C                       SEE GFETCH FOR OTHER VALUES WHEN IER.NE.0 AND
C                       DATA ARE RETURNED AS MISSING (INTERNAL-OUTPUT)
C
C      ADDITIONAL VARIABLES 
C              IENTER = NUMBER OF TIMES THIS SUBROUTINE IS ENTERED.
C                       (INTERNAL).
C           ITABLE(K) = ARRAY USED TO CONSTRUCT THE DISCRETE
C                       CEILING HEIGHT PROBABILITIES TO GFETCH.
C                       (K=1,7).
C            JFLAG(K) = FLAG (=1) INDICATING THAT A STATION HAS AT LEAST
C                       ONE MISSING PROBABILITY.  A STATION WITH AT LEAST
C                       ONE MISSING PROBABILITY WILL BE RETURNED WITH A
C                       SUMMATION VALUE OF MISSING (9999.).  JFLAG( ) IS
C                       AN AUTOMATIC ARRAY.
C                JSUM = THE NUMBER OF DISCRETE PROBABILITES TO
C                       SUM OVER.
C           JTABLE(K) = ARRAY OF BREAK POINTS OF THE CEILING HEIGHT 
C                       DISCRETE PROBABILITIES, (K=1,7).
C               LD(J) = THE ID OF THE DISCRETE CEILING HEIGHT 
C                       PROBABILITIES, J=1,4 (INTERNAL). 
C              NWORDS = NUMBER OF WORDS RETURNED IN FD1( ). (INTERNAL)
C
C        NONSYSTEM SUBROUTINES USED
C            GFETCH
C
      PARAMETER (NDIM=7)
C
      DIMENSION LD(4),ID(4),IDPARS(15)
      DIMENSION IPACK(ND2X3),IWORK(ND2X3)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9),SDATA(ND1)
      DIMENSION CORE(ND10) 
      DIMENSION FD1(ND2X3),JFLAG(ND1)
      DIMENSION JTABLE(NDIM),ITABLE(8,NDIM)
C
      DATA JTABLE/0150001,0450001,0950001,0195002,
     1            0305002,0655002,0120503/
C
      DATA ITABLE/0150001,0,0,0,0,0,0,1,
     1            0150001,0450001,0,0,0,0,0,2,
     2            0150001,0450001,0950001,0,0,0,0,3,
     3            0150001,0450001,0950001,0195002,0,0,0,4,
     4            0150001,0450001,0950001,0195002,0305002,
     5            0,0,5,
     6            0150001,0450001,0950001,0195002,0305002,
     7            0655002,0,6,
     8            0150001,0450001,0950001,0195002,0305002,
     9            0655002,0120503,7/
C
      SAVE IENTER
C
      IER=0
      ISTAV=1
C
C       INITIALIZE THE PROBABILITY SUMMATION ARRAY.
C  
      DO 50 K=1,ND1
         SDATA(K)=0.
         JFLAG(K)=0
 50   CONTINUE
C     
      DO 100 JJ=1,NDIM
         IF((ID(1)/1000.EQ.208043).AND.
     1      (JTABLE(JJ).EQ.ID(4)/1000)) THEN
            JSUM=ITABLE(8,JJ)
            GOTO 110
         ENDIF
 100  CONTINUE
C    
      WRITE(KFILDO,107)(ID(L),L=1,4)
 107  FORMAT(' ****SUMPRB ENTERED FOR VARIABLE',
     1        2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2        ' NOT ACCOMMODATED.')
      IER=102
      GO TO 800
C  
 110  LD(1)=2080503*100+IDPARS(4)
      LD(2)=0
      LD(3)=ID(3)
C
      DO 150 J=1,JSUM
         LD(4)=ITABLE(J,JSUM)*1000
C
         CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1               IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2               NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3               NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
C
C           CHECK TO SEE IF IER NE 0, AND IF THIS IS THE FIRST
C           PROCESS DATE.  PRINT ERROR MESSAGE IF NEEDED.
C
         IF((IER.NE.0).AND.(IENTER.EQ.1)) THEN
            WRITE(KFILDO,140)NDATE
 140        FORMAT(/,' ****ERROR FROM GFETCH OCCURRED ON',
     1              ' 1ST PROCESS DATE - ',I12,
     2              ' SUMPRB CANNOT RUN. PLEASE READ WRITE UP')
            GOTO 800
         ENDIF
C
         IF(IER.NE.0) GO TO 800
C
C           CHECK WHETHER NWORDS RETURNED FROM GFETCH EQ NSTA.
C
         IF(NWORDS.NE.NSTA) THEN
            WRITE(KFILDO,142)NWORDS,NSTA,(LD(L),L=1,4)
 142        FORMAT(/' ****NWORDS =',I7,' RETURNED FROM GFETCH',
     1              ' NOT EQUAL TO NSTA =',I7,' IN SUMPRB.'/
     2              '     VARIABLE',2X,I9.9,1X,I9.9,1X,I9.9,I11.3,
     3              ' NOT ACCOMMODATED.')
            IER=52
            GO TO 800
         ENDIF
C
         DO 145 K=1,NSTA
C
C              ALL FORECASTS OF 9997 ARE TREATED AS 0.
            IF(NINT(FD1(K)).EQ.9997) FD1(K)=0.
C
C              SUM THE DISCRETE PROBABILITIES.  FLAG A STATION
C              WITH A MISSING PROBABILITY.
            IF(NINT(FD1(K)).EQ.9999) JFLAG(K)=1
            SDATA(K)=SDATA(K)+FD1(K)
 145     CONTINUE         
C  
 150  CONTINUE
C
C          IF A STATION HAS AT LEAST ONE MISSING PROBABILITY,
C          SET THE SUMMATION TO MISSING.
C
      DO 200 K=1,NSTA
        IF(JFLAG(K).EQ.1) SDATA(K)=9999.
 200  CONTINUE
C
      GOTO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C 
 800  DO 801 K=1,ND1
        SDATA(K)=9999.
 801  CONTINUE
C
 900  RETURN
      END
