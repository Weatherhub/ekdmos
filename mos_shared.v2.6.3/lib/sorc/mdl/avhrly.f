      SUBROUTINE AVHRLY(KFILDO,KFIL10,
     1                  ID,IDPARS,JD,NDATE,XDATA,ND1,NSTA,
     2                  IPACK,IWORK,DATA,ND5,
     3                  LSTORE,ND9,LITEMS,CORE,ND10,
     4                  NBLOCK,NFETCH,
     5                  IS0,IS1,IS2,IS4,ND7,
     6                  ISTAV,L3264B,IER)
C
C        JULY    1997   GLAHN   TDL   MOS-2000 
C        OCTOBER 1998   GLAHN   REVISIONS TO MATCH FORMAT OF
C                               OTHER VECTOR ROUTINES
C        APRIL   2003   GLAHN   PUT COMMAS IN FORMATS; SPELL CHECK
C
C***********
C        THIS IS BASICALLY A TEST ROUTINE, BUT WITH A CHANGE IN
C        CCCFFF MAY BE USEFUL.  FOR PURPOSES OF THIS TEST, THE
C        ID OF VARIABLE BEING AVERAGED IS IDENTICAL TO THAT
C        IN JD( ), EXCEPT THE LAST DIGIT IN FFF IS ELIMINATED.
C        THAT MEANS, ANY VARIABLE WITH A DEFINED ID CAN HAVE
C        A FFX, WHERE X REPLACES 0, AND THE AVERAGE FOUND.
C************
C
C        PURPOSE 
C            TO COMPUTE THE MEAN OF HOURLY VALUES OF A VECTOR
C            THE AVERAGE STARTING IDPARS(9) = RR HOURS BEFORE
C            NDATE AND CONTINUING FOR IDPARS(11) = OH HOURS FORWARD.
C            THE AVERAGE WILL BE COMPUTED FROM OH+1 VALUES.  OH+1 MUST
C            NOT EXCEED NDATE.  IT IS ASSUMED ANY SECONDARY MISSING
C            VALUES HAVE BEEN REMOVED.
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. 
C                     (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C               ID(J) = THE VARIABLE ID (J=1,4).  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID( ) (J=1,15).
C                       (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE PORTIONS
C                       PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       JD( ) IS USED TO IDENTIFY THE BASIC MODEL FIELDS
C                       AS READ FROM THE ARCHIVE.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.  (INPUT)
C            XDATA(K) = DATA TO RETURN (K=1,NSTA) (OUTPUT).  
C                 ND1 = MAXIMUM NUMBER OF STATIONS OR LOCATIONS THAT
C                       CAN BE DEALT WITH.  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT WITH.
C                       (INPUT)
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
C                              IN NGRIDC( ,L) DEFINING THE CHARACTERISTICS
C                              OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,NPRED) FOR WHICH THIS
C                              VARIABLE IS NEEDED, WHEN IT IS NEEDED ONLY
C                              ONCE FROM LSTORE( , ).  WHEN IT IS NEEDED
C                              MORE THAN ONCE, THE VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING MSTORE( , ).
C                              LATER USED AS A WAY OF DETERMINING WHETHER
C                              TO KEEP THIS VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , ) THAT 
C                       HAVE BEEN USED IN THIS RUN.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL
C                       DATA ARE STORED ON DISK.  (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING  COUNT FROM THE BEGINNING OF THE 
C                       PROGRAM.  THIS COUNT IS MAINTAINED IN CASE THE USER 
C                       NEEDS IT (DIAGNOSTICS, ETC.).  (INTERNAL)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C               ISTAV = 1 SINCE THE DATA RETURNED ARE STATION DATA.  (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING USED
C                       (EITHER 32 OR 64).  (INPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       SEE GFETCH FOR VALUES.  (INTERNAL-OUTPUT)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT THE 
C                       RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ).  (INTERNAL)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       DATA( ) (J=1,4).  (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA( ).  (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH
C
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION XDATA(ND1) 
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION LD(4)
C
      IER=0
      ISTAV=1
C
C        COMPUTE THE SUM OVER OH+1 VECTORS.
C
      LD(1)=IDPARS(1)*1000000+
     1     (IDPARS(2)/10)*10000+
     2      IDPARS(3)*100+
     3      IDPARS(4)
      LD(2)=JD(2)
      LD(4)=JD(4)
C
      DO 250 L=0,IDPARS(11)
      LD(3)=(IDPARS(9)-L)*1000000+IDPARS(12)
C        ONLY PART OF ID(3) USED IS TAU AND RR.
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,DATA,ND5,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4            IER)
      IF(IER.NE.0)GO TO 800
C
      IF(NWORDS.NE.NSTA)THEN
         WRITE(KFILDO,131)NWORDS,NSTA
 131     FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1           ' NOT EQUAL TO NSTA =',I6,
     2           ' IN AVHRLY.  DATA SET TO MISSING.')
         IER=52
         GO TO 800
C
      ENDIF
C
      IF(L.EQ.0)THEN
C
         DO 220 J=1,NSTA
         XDATA(J)=DATA(J)
 220     CONTINUE
C
      ELSE
C
C           PERFORM THE COMPUTATIONS.  SINCE XDATA( )
C           WILL MORE LIKELY BE MISSING THAN DATA( ),
C           DO THAT CHECK FIRST.
C
         DO 230 J=1,NSTA
C
         IF(XDATA(J).EQ.9999.)THEN
            GO TO 230
         ELSEIF(DATA(J).EQ.9999.)THEN
            XDATA(J)=9999.
         ELSE
            XDATA(J)=XDATA(J)+DATA(J)
         ENDIF
C
 230     CONTINUE
C
      ENDIF
C
 250  CONTINUE
C
C        COMPUTE MEAN.
C
      PD=IDPARS(11)+1
C
      DO 260 J=1,NSTA
      IF(XDATA(J).NE.9999.)XDATA(J)=XDATA(J)/PD
 260  CONTINUE
C
      GO TO 850
C
C        THIS PREDICTOR CANNOT BE COMPUTED.  SET THE ARRAY TO MISSING.
C
 800  DO 801 J=1,ND1
      XDATA(J)=9999.
 801  CONTINUE 
C
D     WRITE(KFILDO,802)IER,(JD(J),J=1,4)
D802  FORMAT(/' ****ERROR IN AVHRLY, IER =',I5,' FOR VARIABLE',4I12)
 850  RETURN
      END     
                  
