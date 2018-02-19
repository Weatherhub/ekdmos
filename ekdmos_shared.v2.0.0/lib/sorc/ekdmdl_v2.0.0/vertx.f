      SUBROUTINE VERTX(KFILDO,KFIL10,
     1                 ID,IDPARS,JD,ITAU,MDATE,XDATA,ND1,NSTA,
     2                 IPACK,IWORK,DATA,ND5,
     3                 LSTORE,ND9,LITEMS,CORE,ND10,
     4                 NBLOCK,NFETCH,
     4                 IS0,IS1,IS2,IS4,ND7,
     5                 ISTAV,L3264B,IER)
C 
C        SEPTEMBER 1996   GLAHN   TDL   MOS-2000
C        JANUARY   1997   GLAHN   NAME CHANGE FROM VERT6 
C        JANUARY   1997   GLAHN   ELIMINATED NELEV( ), STALAT( ),
C                                 AND STALON( )
C        SEPTEMBER 1997   GLAHN   ELIMINATED USE OF INDEXC( , )
C        JANUARY   1998   GLAHN   ELIMINATED TEST FOR 9997; ARRAYS
C                                 USED DIRECTLY IN CALL TO GFETCH
C        JUNE      1998   GLAHN   PURPOSE CONCERNING 9997 CHANGED
C        OCTOBER   1998   GLAHN   ISTAV ADDED TO CALL; LASTL, LASTD,
C                                 NSTORE REMOVED; PLUS OTHER CHANGES
C        MARCH     1999   GLAHN   LD( ) TO MD( ) IN DIAGNOSTIC
C        APRIL     1999   GLAHN   ADDED ITAU TO CALL;
C                                 CHANGED NDATE TO MDATE
C        APRIL     2000   DALLAVALLE   MODIFIED FORMAT STATEMENTS TO
C                                      CONFORM TO FORTRAN 90 
C                                      STANDARDS ON THE IBM SP
C
C        PURPOSE 
C            TO COMPUTE DIFFERENCE, SUM, OR MEAN OF TWO VARIABLES OF THE
C            SAME VARIABLE DEFINITION (JD(1) THE SAME), THE SAME TAU,
C            AND THE SAME RUN OFFSET RR AT THE TWO LEVELS GIVEN BY
C            JD(2).  THE OPERATION IS CONTROLLED BY IDPARS(5):
C
C            1 = DIFFERENCE IDPARS(7) - IDPARS(6)
C            2 = SUM        IDPARS(7) + IDPARS(6)
C            3 = MEAN      (IDPARS(7) + IDPARS(6))/2.
C
C            POSSIBLE MISSING VALUES OF 9999. ARE TREATED AS 
C            MISSING = 9999.  VECTOR DATA FROM THE MOS-2000 INTERNAL
C            STORAGE SYSTEM WILL BE UNPACKED, WILL BE IN THE ORDER
C            MEEDED.  VERTX ASSUMES THERE WILL BE NO SECONDARY
C            MISSING VALUES OF 9997.
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
C                       NOTE THAT THIS IS NDATE UPDATED WITH ITAU.
C                       (INPUT)
C            XDATA(K) = COMPUTED VARIABLE IS RETURNED IN XDATA( )
C                       (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF XDATA( ).  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
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
C               ISTAV = 0 SINCE THE DATA RETURNED ARE GRID DATA. 
C                       (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        52 = NWORDS FROM GFETCH NE NSTA.
C                       102 = VERTICAL PROCESSING INDICATOR IDPARS(5)
C                             NOT ACCOMMODATED.
C                       SEE GFETCH FOR OTHER VALUES.  (OUTPUT)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT
C                       THE RECORD HAS BEEN FETCHED.  THIS IS STORED
C                       IN LSTORE(9, ).  (INTERNAL)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       XDATA( ) FOR THE TOP LEVEL (J=1,4).  (INTERNAL)
C               MD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       DATA( ) FOR THE BOTTOM LEVEL (J=1,4).  (INTERNAL)
C              NSOURC = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ).  NOT ACTUALLY USED.  (INTERNAL) 
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA( ) BY GFETCH.
C                       (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS 
C                       ZERO BY GFETCH WHEN DATA ARE NOT PACKED. 
C                       (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED 
C                       AS ZERO BY GFETCH WHEN DATA ARE NOT PACKED. 
C                       (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH
C
      DIMENSION XDATA(ND1)
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION LD(4),MD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
C
      IER=0
      ISTAV=1
      ITIME=-ITAU
C
C        VERIFY THE PROCESSING INDICATOR, IDPARS(5).
C
      IF(IDPARS(5).LT.1.OR.IDPARS(5).GT.3)THEN
         WRITE(KFILDO,219)(JD(L),L=1,4),IDPARS(5)
         IER=102
         GO TO 800
C
      ENDIF
C
C        GET THE FIRST VARIABLE.
C
      LD(1)=JD(1)
      LD(2)=IDPARS(7)
      LD(3)=JD(3)
      LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
C        LD( ) IS IN BASIC PREDICTOR FORMAT.
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,XDATA,ND1,
     2            NWORDS,NPACK,MDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSOURC,MISSP,MISSS,L3264B,ITIME,IER)
C
      IF(IER.NE.0)THEN
D        WRITE(KFILDO,130)(LD(J),J=1,4)
D 130    FORMAT(' ****VARIABLE NOT RETRIEVED BY GFETCH IN VERTX',
D    1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 800
      ENDIF
C
      IF(NWORDS.NE.NSTA)THEN
         WRITE(KFILDO,131)NWORDS,NSTA
 131     FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1            ' NOT EQUAL TO NSTA =',I6,
     2            ' IN VERTX.  DATA SET TO MISSING.')
         IER=52
         GO TO 800
C
      ENDIF
C
C        GET THE SECOND VARIABLE.
C
      MD(1)=JD(1)
      MD(2)=IDPARS(6)
      MD(3)=JD(3)
      MD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
C        MD( ) IS IN BASIC PREDICTOR FORMAT.
      CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,DATA,ND5,
     2            NWORDS,NPACK,MDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSOURC,MISSP,MISSS,L3264B,ITIME,IER)
C
      IF(IER.NE.0)THEN
D        WRITE(KFILDO,130)(MD(J),J=1,4)
         GO TO 800
      ENDIF
C
C
      IF(NWORDS.NE.NSTA)THEN
         WRITE(KFILDO,131)NWORDS,NSTA
         IER=52
         GO TO 800
C
      ENDIF
C
      IF(IDPARS(5).EQ.1)THEN
C
          DO 211 J=1,NSTA
          IF(XDATA(J).NE.9999..AND.DATA(J).NE.9999.)THEN
             XDATA(J)=XDATA(J)-DATA(J)
          ELSE
             XDATA(J)=9999.
          ENDIF
C
 211      CONTINUE
C
      ELSEIF(IDPARS(5).EQ.2)THEN
C
          DO 212 J=1,NSTA
          IF(XDATA(J).NE.9999..AND.DATA(J).NE.9999.)THEN
             XDATA(J)=XDATA(J)+DATA(J)
          ELSE
             XDATA(J)=9999.
          ENDIF
C
 212      CONTINUE
C
      ELSEIF(IDPARS(5).EQ.3)THEN
C
          DO 213 J=1,NSTA
          IF(XDATA(J).NE.9999..AND.DATA(J).NE.9999.)THEN
             XDATA(J)=(XDATA(J)+DATA(J))/2.
          ELSE
             XDATA(J)=9999.
          ENDIF
C
 213      CONTINUE
C
      ELSE 
         WRITE(KFILDO,219)(JD(L),L=1,4),IDPARS(5)
 219     FORMAT(' ****VERTX ENTERED FOR PREDICTOR',
     1            2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2          ' WITH PROCESSING IDPARS(5) = ',I3,
     3          ' NOT ACCOMMODATED.')
         IER=102
         GO TO 800
C
      ENDIF
C
      GO TO 850
C
C        THIS PREDICTOR CANNOT BE COMPUTED.  SET THE ARRAY TO MISSING.
C
 800  DO 801 J=1,NSTA
      XDATA(J)=9999.
 801  CONTINUE 
C
 850  RETURN
      END     
