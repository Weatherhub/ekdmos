      SUBROUTINE GTVECT1(KFILDO,KFIL10,IN,IPRINT,
     1                   ID,IDPARS,JD,ITAU,NWHERE,NVRBL,N,
     2                   NDATE,XDATA,ND1,NSTA,
     3                   IPACK,IWORK,DATA,ND5,
     4                   LSTORE,ND9,LITEMS,CORE,ND10,
     5                   NBLOCK,NFETCH,
     6                   IS0,IS1,IS2,IS4,ND7,
     7                   L3264B,L3264W,ISTOP,ISTAB,IER)
C
C        JULY      2002   GLAHN   MOS-2000
C                                 ADAPTED FROM GTVECT
C
C        PURPOSE
C           TO OBTAIN FOR VRBL69 AND SIMILAR ROUTINES VARIABLES
C           THAT MUST BE RETRIEVED FROM THE MOS-2000 INTERNAL
C           STORAGE SYSTEM.  ONE VARIABLE IS RETURNED PER CALL.
C
C           THE DIFFERENCE BETWEEN GTVECT1 (WHICH IS USED FOR U662)
C           AND GTVECT IS THAT GTVECT1 HAS NO COMPUTATIONAL 
C           CAPABILITY (IT DOES NOT CALL OPTX) AND DOES NOT SAVE DATA
C           IN SDATA WITH IDS IN LD( ).  IT CALLS GFETCH1 RATHER
C           THAN GFETCH.
C   
C        DATA SET USE
C            KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C            IP12   - INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                     STATIONS ON THE INPUT FILES WILL BE PRINTED TO 
C                     THE FILE WHOSE UNIT NUMBER IS IP12.
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C                  IN = THE INPUT SOURCE SEQUENCE NUMBER.  (INPUT)
C              IPRINT = USED TO KEEP A DIAGNOSTIC FROM BEING PRINTED
C                       (IPRINT = 0) FOR EACH MISSING VARIABLE
C                       WHEN THE ENTIRE DAY IS MISSING.  (INPUT)
C             ID(J,N) = THE INTEGER PREDICTOR ID'S (J=1,4) (N=1,NVRBL).
C                       (INPUT)
C         IDPARS(J,N) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID( ) (J=1,15) (N=1,NVRBL).
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
C             JD(J,N) = THE BASIC INTEGER PREDICTOR ID (J=1,4) (N=1,NVRBL).
C                       THIS IS THE SAME AS ID(J,N), EXCEPT THAT THE PORTIONS
C                       PERTAINING TO PROCESSING THAT CAN BE DONE
C                       IN VECTOR PROGRAMS ARE OMITTED:
C                       B = IDPARS(3, ),
C                       G = IDPARS(15, ), AND
C                       THRESH( ).
C                       G HAS NO MEANING IN VECTOR PROGRAMS.
C                       JD( , ) IS USED TO IDENTIFY THE BASIC MODEL FIELDS
C                       AS READ FROM THE ARCHIVE.  (INPUT)
C             ITAU(N) = THE NUMBER OF HOURS TO ADD TO NDATE TO GET 
C                       THE VARIABLE N (N=1,NVRBL).
C                       THIS IS THE "LOOKAHEAD" FEATURE.  (INPUT)
C           NWHERE(N) = INDICATES WHERE THE VARIABLE IS TO COME FROM (N=1,ND4)
C                       0 = UNDETERMINED
C                       1 = FROM INPUT FILE.  THIS MAY ALREADY BE IN THE 
C                           MOS-2000 STORAGE SYSTEM BECAUSE OF THE LOOKAHEAD
C                           FEATURE.
C                       2 = BINARY FROM BASIC VARIABLE IN VRBL61.
C                       3 = FROM OPTX.
C                       4 = A DUPLICATE (PREDICTORS ONLY).  NOT VALID
C                           IN GTVECT1.
C                       (INPUT)
C               NVRBL = THE NUMBER OF VARIABLES NEEDED AND IDENTIFIED IN 
C                       ID( , ), ETC.  ALSO TREATED AS THE DIMENSION OF THE
C                       VARIABLES ID( , ), ETC.  (INPUT)
C                   N = THE NUMBER OF THE VARIABLE IN ID( ,N), ETC.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTORS ARE TO BE
C                       FURNISHED ON THIS CALL TO GTVECT1.  (INPUT)
C            XDATA(K) = THE DATA FOR THE NSTA STATIONS BEING PROCESSED
C                       (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       MUST BE LE ND5.  (INPUT)
C                NCAT = A CATEGORY NUMBER USED BY U710 AND U910 FOR
C                       INPUT TO OPTX.  (INPUT)
C                NSTA = THE NUMBER OF STATIONS IN CCALL( ).  (INPUT)
C            IPACK(J) = HOLDS THE TDL GRIB RECORD (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  FURNISHED TO UNPACK, BUT
C                       NOT ACTUALLY USED BECAUSE ONLY THE ID'S ARE
C                       UNPACKED.  (INTERNAL)
C             DATA(J) = WORK ARRAY (J=1,ND5).  FURNISHED TO UNPACK, BUT
C                       NOT ACTUALLY USED BECAUSE ONLY THE ID'S ARE
C                       UNPACKED.  (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND DATA( ).
C                       (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED IN THE MOS-2000 INTERNAL STORAGE SYSTEM
C                       (L=1,11) (J=1,LITEMS).  (INPUT)
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
C                       L=10 --NOT USED.
C                       L=11 --THE NUMBER OF THE FIRST PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,NVRBL) FOR WHICH THIS
C                              VARIABLE IS NEEDED, WHEN IT DOES NOT NEED
C                              TO BE STORED AFTER DAY 1.  WHEN THE VARIABLE
C                              MUST BE STORED (TO BE ACCESSED THROUGH OPTION)
C                              FOR ALL DAYS, ID(11,N) IS 7777 + THE NUMBER
C                              OF THE FIRST PREDICTOR IN THE SORTED LIST
C                              FOR WHICH THIS VARIABLE IS NEEDED.
C                       L=12 --USED INITIALLY IN ESTABLISHING MSTORE( , ).
C                              LATER USED AS A WAY OF DETERMINING WHETHER
C                              TO KEEP THIS VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ) AND MSTORE( , ).
C                       (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , ) THAT 
C                       HAVE BEEN USED IN THIS RUN.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL
C                       DATA ARE STORED ON DISK.  (INPUT-OUTPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NFETCH = THE NUMBER OF TIMES GFETCH HAS BEEN ENTERED.  GFETCH
C                       KEEPS TRACK OF THIS AND RETURNS THE VALUE.  (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING USED
C                       (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).  (INPUT) 
C               ISTOP = INCREMENTED BY ONE EACH TIME AN ERROR IS ENCOUNTERED.
C                       (INPUT-OUTPUT)
C               ISTAB = USUALLY  ZERO, BUT SET TO ONE WHEN CERTAIN
C                       SUBROUTINES ARE CALLED FROM OPTX.  (OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       139 = MISSING DATA FOR THIS VARIABLE.
C                       (OUTPUT)
C               MDATE = NDATE UPDATED WITH ITAU( ).  (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES USED 
C            TIMPR, UPDAT, GFETCH
C
      DIMENSION XDATA(ND1)
      DIMENSION ID(4,NVRBL),IDPARS(15,NVRBL),JD(4,NVRBL),
     1          ITAU(NVRBL),NWHERE(NVRBL)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
C
C***D     CALL TIMPR(KFILDO,KFILDO,'START GTVECT1       ')
      IER=0
      ISTAB=0
C        ISTAB = 0 MEANS VARIABLE IS NOT BINARY.
C
C****D     WRITE(KFILDO,405)N,(JD(J,N),J=1,4),(ID(J,N),J=1,4),NWHERE(N)
C****D405  FORMAT(' IN GTVECT1, LOOKING FOR VRBL NO. ',
C****D    1       I3,3(1XI9.9),1XI10.3,3(1XI9.9),1XI10.3,'  NWHERE ='I4)
C
C        SET MDATE AND ITIME FOR THIS VARIABLE.
C
      IF(ITAU(N).EQ.0)THEN
         MDATE=NDATE
         ITIME=0
      ELSE
         CALL UPDAT(NDATE,ITAU(N),MDATE)
         ITIME=-ITAU(N)
      ENDIF
C
C***D     WRITE(KFILDO,409)(LD(J),J=1,7)
C***D409  FORMAT(' IN GTVECT1, VRBL IN LD IS           ',
C***D    1        3(1X,I9.9),3I4,I10)
      IF(NWHERE(N).EQ.1)GO TO 423
      IF(NWHERE(N).EQ.4)GO TO 411
C        NWHERE(N) = 4 MAY NOT BE USED IN U660.
C        THE TEST IS LEFT HERE FOR SAFETY.
      WRITE(KFILDO,410)NWHERE(N),N,NDATE
 410  FORMAT(/,' ****NWHERE( ) VARIABLE =',I3,
     1         ' INCORRECT FOR VARIABLE NO. ',I4,
     2         ' IN GTVECT1 AT 410.  DATE =',I11)
 411  ISTOP=ISTOP+1
C
      DO 413 K=1,NSTA
      XDATA(K)=9999.
 413  CONTINUE
C
      IER=139
      GO TO 530
C
 423  CALL GFETCH1(KFILDO,KFIL10,ID(1,N),N,LSTORE,ND9,LITEMS,
     1             IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,XDATA,ND1,
     2             NWORDS,NPACK,MDATE,NTIMES,CORE,ND10,
     3             NBLOCK,NFETCH,IN,MISSP,MISSS,L3264B,ITIME,
     4             IER)
      IF(IER.EQ.0)GO TO 540
C        AT THIS POINT, THE VARIABLE EXISTS IN XDATA( ).  THE FULL 
C        IDENTIFICATION OF THE DATA IS IN IS1( ), IS2( ), AND IS4( ).
C
      IER=139
C        ISTOP IS NOT INCREMENTED, BECAUSE EACH VARIABLE MAY NOT
C        BE ON EACH INPUT.
C
 530  IF(IPRINT.EQ.0)GO TO 540
      WRITE(KFILDO,531)(ID(J,N),J=1,4),MDATE
C        IF ANY OF THE INPUT FILES HAVE NO DATA FOR THIS DATE,
C        THIS DIAGNOSTIC IS NOT PRINTED.
 531     FORMAT(' ****CANNOT OBTAIN VARIABLE       ',
     1           I9.9,1X,I9.9,1X,I9.9,1X,I10.3,' FOR DATE ',I11,'.')
 540  RETURN
      END
