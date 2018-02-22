      SUBROUTINE PCT2DC(KFILDO,KFIL10,
     1                  ID,IDPARS,JD,ITAU,MDATE,XDATA,ND1,NSTA,
     2                  IPACK,IWORK,ND5,
     3                  LSTORE,ND9,LITEMS,CORE,ND10,
     4                  NBLOCK,NFETCH,
     5                  IS0,IS1,IS2,IS4,ND7,
     6                  ISTAV,L3264B,IER)
C 
C        OCTOBER  2002   DAGOSTARO   TDL   MOS-2000
C        FEBRUARY 2003   GLAHN   0 REPLACED BY -ITAU IN CALL TO GFETCH
C
C        PURPOSE 
C            TO CONVERT PROBABILITY FORECASTS FROM PERCENT (0-100)
C            TO DECIMAL (0-1.00).
C            POSSIBLE MISSING VALUES OF 9999. OR 9997. ARE TREATED AS 
C            MISSING = 9999.  VECTOR DATA FROM THE MOS-2000 INTERNAL
C            STORAGE SYSTEM WILL BE UNPACKED, AND WILL BE IN THE ORDER
C            NEEDED.
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
C                 ND5 = DIMENSION OF IPACK( ) AND IWORK( ).  (INPUT)
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
C               ISTAV = 1 FOR STATION DATA (OUTPUT)
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
C              NSOURC = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ).  NOT ACTUALLY USED.  (INTERNAL) 
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA?( ) BY GFETCH.
C                       (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  (INTERNAL)
C         ITABLE(I,J) = VALUES OF IDPARS(1) AND (2) (J=NDIM) ACCOMMODATED AND
C                       PIECES NEEDED TO BUILD ASSOCIATED VARIABLE ID
C                       (I=1,3).  (INTERNAL)
C               LD(J) = THE PREDICTOR ID TO FETCH (J=1,4).  (INTERNAL)
C               NDIM = NUMBER OF VARIABLE IDS THIS SUBROUTINE CAN
C                       HANDLE, DIMENSION OF ARRAY ITABLE (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH
C
      PARAMETER (NDIM=1)
      DIMENSION XDATA(ND1)
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION IPACK(ND5),IWORK(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION ITABLE(3,NDIM),LD(4)
      DATA ITABLE/203, 520, 524/

C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH
C
C
      IER=0
      ISTAV=1
C
C        GET THE IDS FOR THE FIELD TO RETRIEVE.
C
      DO 200 JJ=1,NDIM
      IF(ITABLE(1,JJ).EQ.IDPARS(1).AND.
     2   ITABLE(2,JJ).EQ.IDPARS(2)) THEN
C
        LD(1)=ITABLE(1,JJ)*1000000+ITABLE(3,JJ)*1000+IDPARS(4)
        LD(2)=ID(2)
        LD(3)=ID(3)
        LD(4)=ID(4)
C        LD( ) IS IN BASIC PREDICTOR FORMAT.
C
        CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,XDATA,ND1,
     2              NWORDS,NPACK,MDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSOURC,MISSP,MISSS,L3264B,-ITAU,IER)
C
        IF(IER.EQ.0)THEN
          IF(NWORDS.NE.NSTA)THEN
            WRITE(KFILDO,FMT='(/,'' ****NWORDS ='',I6,'' RETURNED '',
     1           ''FROM GFETCH NOT EQUAL TO NSTA ='',I6,'' IN '',
     2           ''PCT2DC.  DATA SET TO MISSING.'')') NWORDS,NSTA
            IER=52
            GO TO 300
          ENDIF
C 
D*********WRITE(KFILDO,FMT='('' ****PCT2DC RETRIEVED VARIABLE'',2X,
D****1          I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
D****2          '' FOR DIVISION BY 100.'')') (LD(L),L=1,4)
C
          DO 100 J=1,NSTA
C
          IF(NINT(XDATA(J)).NE.9997.AND.NINT(XDATA(J)).NE.9999) THEN
            XDATA(J)=XDATA(J)/100.
          ENDIF
C
 100      CONTINUE
          GO TO 350   

C        THE VARIABLE WAS NOT SUCCESSFULLY RETRIEVED
	ELSE
          WRITE(KFILDO,FMT='('' ****VARIABLE NOT RETRIEVED BY '',
     1      ''GFETCH IN PCT2DC'',2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2      '' FOR'',I12)') (LD(L),L=1,4),MDATE
          GO TO 300
        ENDIF
      ENDIF
 200  CONTINUE
C 
      WRITE(KFILDO,FMT='('' ****PCT2DC ENTERED FOR VARIABLE'',2X,
     1      I9.9,1X,I9.9,1X,I9.9,1X,I10.3,'' NOT ACCOMMODATED.'')')
     2      (JD(L),L=1,4)
      IER=102
C
C        THIS PREDICTOR CANNOT BE COMPUTED.  SET THE ARRAY TO MISSING.
C
 300  DO 310 J=1,NSTA
      XDATA(J)=9999.
 310  CONTINUE 
C
 350  RETURN
      END     
