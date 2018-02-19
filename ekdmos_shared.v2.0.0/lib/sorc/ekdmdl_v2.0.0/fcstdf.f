      SUBROUTINE FCSTDF(KFILDO,KFIL10,
     1                  ID,IDPARS,JD,ITAU,MDATE,XDATA,ND1,NSTA,
     2                  IPACK,IWORK,DATA,ND5,
     3                  LSTORE,ND9,LITEMS,CORE,ND10,
     4                  NBLOCK,NFETCH,
     4                  IS0,IS1,IS2,IS4,ND7,
     5                  ISTAV,L3264B,IER)
C 
C        MARCH 2000   GLAHN   TDL   MOS-2000
C        MAY   2000   GLAHN   COMMAS ADDED TO FORMAT STATEMENTS
C        MAY   2000   GLAHN   DIM2 CHANGED TO NDIM
C        MAY   2001   DAGOSTARO   MODIFIED TO HANDLE AVN MOS
C        NOVEMBER 2002 HUGHES  COMMAS ADDED TO FORMAT STATEMENTS
C
C        PURPOSE 
C            TO COMPUTE THE ABSOLUTE OR ARITHMETIC DIFFERENCE BETWEEN
C            A LOCAL FORECAST AND A MOS FORECAST.  THE PARTICULAR 
C            MOS FORECAST IS DETERMINED FROM DD:
C               DD = 06 - NGM
C               DD = 07 - ETA
C               DD = 08 - AVN
C               DD = 09 - MRF
C               DD = 80 - AEV (THE MOS FORECASTS FROM THE AEV ARCHIVE)
C            NOTE THAT OTHER MOS FORECASTS CAN BE ACCOMMODATED (E.G., A
C            LAMP FORECAST); AS WELL AS FORECASTS FROM BACKUP EQUATIONS
C            (E.G., DD = 18, 28, ETC. FROM THE AVN)
C            THE LOCAL FORECASTS HAVE A DD = 81.
C
C            FFF = 8XX DENOTES ALGEBRAIC DIFFERENCE;
C            FFF = 9XX DENOTES ABSOLUTE DIFFERENCE.
C
C            IDPARS (1, 2, AND 4) DENOTE THE ALGEBRAIC DIFFERENCE BETWEEN 
C 
C               202 801 ^ DD = 202 001 0 081 - 202 001 0 DD (DAYTIME MAX TEMP)
C               202 811 ^ DD = 202 011 0 081 - 202 011 0 DD (NIGHTTIME MIN TEMP)
C               203 805 ^ DD = 203 505 0 081 - 203 505 0 DD (12-H POP)
C               204 810 ^ DD = 204 210 0 081 - 204 211 0 DD (WIND SPEED)
C                                       NOTE THAT THE MOS WIND SPEED IS INFLATED.
C
C            IDPARS (1, 2, AND 4) DENOTE THE ABSOLUTE DIFFERENCE BETWEEN 
C 
C               202 901 ^ DD = 202 001 0 081 - 202 001 0 DD (DAYTIME MAX TEMP)
C               202 911 ^ DD = 202 011 0 081 - 202 011 0 DD (NIGHTTIME MIN TEMP)
C               203 905 ^ DD = 203 505 0 081 - 203 505 0 DD (12-H POP)
C               (EXCEPT IF DD=08, SUBTRAHEND IS 203 330 1 08)
C               204 900 ^ DD = 204 200 0 081 - 204 200 0 DD (WIND DIRECTION)
C               204 910 ^ DD = 204 210 0 081 - 204 211 0 DD (WIND SPEED)
C                                       NOTE THAT THE MOS WIND SPEED IS INFLATED.
C
C            POSSIBLE MISSING VALUES OF 9999. ARE TREATED AS 
C            MISSING = 9999.  VECTOR DATA FROM THE MOS-2000 INTERNAL
C            STORAGE SYSTEM WILL BE UNPACKED, AND WILL BE IN THE ORDER
C            NEEDED.  FCSTDF ASSUMES THERE WILL BE NO SECONDARY
C            MISSING VALUES OF 9997; THESE SHOULD HAVE ALREADY BEEN
C            TREATED.
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                     (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
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
C               MDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C            XDATA(K) = COMPUTED VARIABLE IS RETURNED IN XDATA( )
C                       (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF XDATA( ).  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
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
C               ISTAV = 1 SINCE THE DATA RETURNED ARE VECTOR DATA. 
C                       (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        52 = NWORDS FROM GFETCH NE NSTA.
C                       102 = ID NOT ACCOMMODATED.
C                       SEE GFETCH FOR OTHER VALUES.  (OUTPUT)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT
C                       THE RECORD HAS BEEN FETCHED.  THIS IS STORED
C                       IN LSTORE(9, ).  (INTERNAL)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       XDATA( ) FOR THE SPEED IN M/S (J=1,4).  (INTERNAL)
C              NSOURC = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ).  NOT ACTUALLY USED.  (INTERNAL) 
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA( ) BY GFETCH.
C                       (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  (INTERNAL)
C         ITABLE(I,J) = WHEN IDPARS(4) INDICATES AN AEV VARIABLE, THESE
C                       ARE THE VALUES OF IDPARS(1) (I=1), IDPARS(2)(I=2) 
C                       ACCOMMODATED, THE ASSOCIATED VARIABLE LOCAL
C                       AEV FORECAST NEEDED (I=3), AND THE MOS FORECAST
C                       NEEDED SANS DD (I=4), (J=1,NDIM).
C                       WHEN FFF IN ITABLE(2, ) = 8XX, THE ALGEBRAIC
C                       DIFFERENCE IS USED;
C                       WHEN FFF IN ITABLE(2, ) = 9XX, THE ABSOLUTE 
C                       DIFFERENCE IS USED.  (INTERNAL)
C         JTBL06(I,J) = WHEN IDPARS(4) INDICATES AN NGM MOS VARIABLE, THESE
C                       ARE THE VALUES OF IDPARS(1) (I=1), IDPARS(2) (I=2) 
C                       ACCOMMODATED, THE ASSOCIATED VARIABLE LOCAL
C                       AEV FORECAST NEEDED (I=3), AND THE NON-AEV MOS
C                       FORECAST NEEDED SANS DD (I=4), (J=1,NDIM).
C                       WHEN FFF IN ITABLE(2, ) = 8XX, THE ALGEBRAIC
C                       DIFFERENCE IS USED;
C                       WHEN FFF IN ITABLE(2, ) = 9XX, THE ABSOLUTE 
C                       DIFFERENCE IS USED.  (INTERNAL)
C         JTBL08(I,J) = WHEN IDPARS(4) INDICATES AN AVN MOS VARIABLE, THESE
C                       ARE THE VALUES OF IDPARS(1) (I=1), IDPARS(2) (I=2) 
C                       ACCOMMODATED, THE ASSOCIATED VARIABLE LOCAL
C                       AEV FORECAST NEEDED (I=3), AND THE NON-AEV MOS
C                       FORECAST NEEDED SANS DD (I=4), (J=1,NDIM).
C                       WHEN FFF IN ITABLE(2, ) = 8XX, THE ALGEBRAIC
C                       DIFFERENCE IS USED;
C                       WHEN FFF IN ITABLE(2, ) = 9XX, THE ABSOLUTE 
C                       DIFFERENCE IS USED.  (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH
C
      PARAMETER (NDIM=9)
      PARAMETER (JDIM=9)
      DIMENSION XDATA(ND1)
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION LD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION ITABLE(4,NDIM)
      DIMENSION JTBL06(5,JDIM)
      DIMENSION JTBL08(5,JDIM)
C
      DATA ITABLE/202, 801, 202001081, 202001000,
     1            202, 811, 202011081, 202011000,
     2            203, 805, 203505081, 203505000,
     3            204, 810, 204210081, 204211000,
     4            202, 901, 202001081, 202001000,
     5            202, 911, 202011081, 202011000,
     6            203, 905, 203505081, 203505000,
     7            204, 900, 204200081, 204200000,
     8            204, 910, 204210081, 204211000/
CXXXX note that the ids in jtbl06 were just copied from jtbl08
cxxxx and may not be correct for variables other than pop.
cxxxx only pop was done correctly just to get quick results.
      DATA JTBL06/202, 801, 202001081, 202001006, 0,
     1            202, 811, 202011081, 202011006, 0,
     2            203, 805, 203505081, 203334006, 0,
     3            204, 810, 204210081, 204325006, 0,
     4            202, 901, 202001081, 202001006, 0,
     5            202, 911, 202011081, 202011006, 0,
     6            203, 905, 203505081, 203334006, 0,
     7            204, 900, 204200081, 204225006, 0,
     8            204, 910, 204210081, 204325006, 0/
      DATA JTBL08/202, 801, 202001081, 202120008, 0,
     1            202, 811, 202011081, 202220008, 0,
     2            203, 805, 203505081, 203330108, 950052000,
     3            204, 810, 204210081, 204325008, 0,
     4            202, 901, 202001081, 202120008, 0,
     5            202, 911, 202011081, 202220008, 0,
     6            203, 905, 203505081, 203330108, 950052000,
     7            204, 900, 204200081, 204225008, 0,
     8            204, 910, 204210081, 204325008, 0/
C
      IER=0
      ISTAV=1
      ITIME=-ITAU
C
C        GET THE IDS FOR THE FIELD TO RETRIEVE.
C
      IF(IDPARS(4).GE.80.AND.IDPARS(4).LE.82) THEN
         DO 110 JJ=1,NDIM
         IF(ITABLE(1,JJ).EQ.IDPARS(1).AND.
     1      ITABLE(2,JJ).EQ.IDPARS(2)) THEN
            LD(1)=ITABLE(3,JJ)
            LD(2)=0
            LD(3)=JD(3)
            LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
            GO TO 120
         ENDIF
 110     CONTINUE
      ELSEIF(IDPARS(4).EQ.06) THEN
         DO 111 JJ=1,NDIM
         IF(JTBL06(1,JJ).EQ.IDPARS(1).AND.
     1      JTBL06(2,JJ).EQ.IDPARS(2)) THEN
            LD(1)=JTBL06(3,JJ)
            LD(2)=0
            LD(3)=JD(3)
            LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
            GO TO 120
         ENDIF
 111     CONTINUE
      ELSEIF(IDPARS(4).EQ.08) THEN
         DO 112 JJ=1,NDIM
         IF(JTBL08(1,JJ).EQ.IDPARS(1).AND.
     1      JTBL08(2,JJ).EQ.IDPARS(2)) THEN
            LD(1)=JTBL08(3,JJ)
            LD(2)=JD(2)
            LD(3)=JD(3)
C        NEED TO ADJUST THE MAX/MIN FORECAST PROJECTION BY 6 HOURS TO
C        MATCH THE AEV PROJECTION
            IF(MOD(IDPARS(2),100).EQ.1.OR.MOD(IDPARS(2),100).EQ.11) 
     1      LD(3)=JD(3)-6
            LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
            GO TO 120
         ENDIF
 112     CONTINUE
      ENDIF
C
      WRITE(KFILDO,117)(JD(L),L=1,4)
 117  FORMAT(' ****FCSTDF ENTERED FOR FIRST VARIABLE',
     1         2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2       ' NOT ACCOMMODATED.')
      IER=102
      GO TO 300
C
C        FETCH THE LOCAL AEV FORECAST.
C
c120  write(kfildo,fmt='('' before 1st call to gfetch, id='',
c    14I10)') (ld(ldd),ldd=1,4)
c     WRITE(KFILDO,121)(JD(L),L=1,4)
c121  FORMAT(' stripped down version of original id=',
c    1         2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
 120  CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,XDATA,ND1,
     2            NWORDS,NPACK,MDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSOURC,MISSP,MISSS,L3264B,ITIME,IER)
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,125)(LD(J),J=1,4)
 125     FORMAT(' ****1ST VARIABLE NOT RETRIEVED BY GFETCH IN FCSTDF',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 300
      ENDIF
C
      IF(NWORDS.NE.NSTA)THEN
         WRITE(KFILDO,131)NWORDS,NSTA
 131     FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1           ' NOT EQUAL TO NSTA =',I6,
     2           ' IN FCSTDF.  DATA SET TO MISSING.')
         IER=52
         GO TO 300
C
      ENDIF
C
C        FETCH THE MOS FORECAST.
C 
      IF(IDPARS(4).GE.80.AND.IDPARS(4).LE.82) THEN
         LD(1)=ITABLE(4,JJ)+IDPARS(4)
         LD(2)=JD(2)
         LD(3)=JD(3)
         LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
      ELSEIF(IDPARS(4).EQ.06) THEN
         LD(1)=JTBL06(4,JJ)
         LD(2)=JTBL06(5,JJ)*10000
         LD(3)=JD(3)
         LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
      ELSEIF(IDPARS(4).EQ.08) THEN
         LD(1)=JTBL08(4,JJ)
         LD(3)=JD(3)
         LD(4)=JTBL08(5,JJ)
      ENDIF
C
c     write(kfildo,fmt='('' before 2nd call to gfetch, id='',
c    14I10)') (ld(ldd),ldd=1,4)
c     WRITE(KFILDO,121)(JD(L),L=1,4)
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,DATA,ND1,
     2            NWORDS,NPACK,MDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSOURC,MISSP,MISSS,L3264B,ITIME,IER)
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,135)(LD(J),J=1,4)
 135     FORMAT(' ****2ND VARIABLE NOT RETRIEVED BY GFETCH IN FCSTDF',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 300
      ENDIF
C
      IF(NWORDS.NE.NSTA)THEN
         WRITE(KFILDO,141)NWORDS,NSTA
 141     FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1           ' NOT EQUAL TO NSTA =',I6,
     2           ' IN FCSTDF.  DATA SET TO MISSING.')
         IER=52
         GO TO 300
C
      ENDIF
C
      IF(IDPARS(2)/100.EQ.8)THEN
C
         DO 200 J=1,NSTA
            IF(XDATA(J).NE.9999..AND.DATA(J).NE.9999.)THEN
               XDATA(J)=(XDATA(J)-DATA(J))
            ELSE
               XDATA(J)=9999.
            ENDIF
C
 200     CONTINUE
C
      ELSEIF(IDPARS(2)/100.EQ.9)THEN
 
         DO 210 J=1,NSTA
C
            IF(XDATA(J).NE.9999..AND.DATA(J).NE.9999.)THEN
               XDATA(J)=ABS(XDATA(J)-DATA(J))
C
               IF(ITABLE(1,JJ).EQ.204.AND.ITABLE(2,JJ).EQ.900)THEN
C                    THIS TEST FOR WIND DIRECTION.
                  IF(XDATA(J).GT.180.)XDATA(J)=ABS(XDATA(J)-360.)
               ENDIF
C
            ELSE
               XDATA(J)=9999.
            ENDIF
C
 210     CONTINUE
C
      ELSE
         GO TO 300
C 
      ENDIF
C
      GO TO 350   
C
C        THIS PREDICTOR CANNOT BE COMPUTED.  SET THE ARRAY TO MISSING.
C
 300  DO 310 J=1,NSTA
      XDATA(J)=9999.
 310  CONTINUE 
C
 350  RETURN
      END     
