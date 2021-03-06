      SUBROUTINE ENSMEAN(KFILDO,KFIL10,NFIRST,
     1                  ID,IDPARS,THRESH,JD,NDATE,
     2                  KFILRA,RACESS,NUMRA,ICALL,CCALL,ICALLD,
     3                  CCALLD,NAME,NELEV,STALAT,STALON,
     4                  ITIMEZ,ISDATA,SDATA,SDATA1,L1DATA,DIR,ND1,NSTA,
     5                  NGRIDC,NGRID,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     6                  LSTORE,ND9,LITEMS,CORE,ND10,LASTL,
     7                  NBLOCK,LASTD,NSTORE,NFETCH,
     8                  IS0,IS1,IS2,IS4,ND7,
     9                  FD1,FD2,FD3,FD4,FD5,FD6,FD7,
     A                  FDVERT,FDTIME,FDSINS,FDMS,ND2X3,IP12,IP16,
     B                  ISTAV,L3264B,L3264W,MISTOT,IER)
C
C        FEB      2008  WIEDENFELD  MDL  MOS 2000                      
C
C        PURPOSE 
C            TO COMPUTE THE MEAN OF THE GFS ENSEMBLE MEMEBERS.
C
C            ALL IDPARS(4) = 76 ARE ACCOMMODATED
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. 
C		      (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. 
C		        (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PRE-
C	 		DICTOR ID CORRESPONDING TO ID( ) (J=1,15).
C                       (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 
C			     LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK IN 
C			     TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE POR-
C			TIONS PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       JD( ) IS USED TO HELP IDENTIFY THE BASIC MODEL 
C			FIELDS
C                       AS READ FROM THE ARCHIVE.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.  
C			(INPUT)
C               NSLAB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE HEIGHTS, WHEN THE HEIGHTS
C                       HAVE BEEN CORRECTLY RETURNED.  WHEN IER NE 0, 
C			THIS VALUE SHOULD NOT BE USED.  (OUTPUT) 
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C           THETAE(K) = THETAE CALCULATED AT THE SURFACE USING TERRAIN 
C			ELEVATION, TEMPERATURE, DEW POINT AND, MEAN SEA
C		        LEVEL PRESSURE (K=1,ND5).  (OUTPUT)
C		  ND5 = DIMENSION OF IPACK( ),IWORK( ), AND THETAE( ).
C			(INPUT)
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
C                              IN NGRIDC( ,L) DEFINING THE CHARACTERIS-
C			       TICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,NPRED) FOR WHICH 
C			       THIS VARIABLE IS NEEDED, WHEN IT IS NEED-
C			       ED ONLY ONCE FROM LSTORE( , ).  WHEN IT 
C			       IS NEEDED MORE THAN ONCE, THE VALUE IS 
C			       SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING 
C			       MOSTORE( , ).
C                              LATER USED AS A WAY OF DETERMINING WHE-
C			       THER TO KEEP THIS VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , ) 
C			THAT HAVE BEEN USED IN THIS RUN.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETIREVE THE DATA IDENTI-
C			FIED IN LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS
C		        FULL DATA ARE STORED ON DISK.  (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING  COUNT FROM THE BEGINNING OF THE
C                       PROGRAM.  THIS COUNT IS MAINTAINED IN CASE THE 
C			USER NEEDS IT (DIAGNOSTICS, ETC.).  (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  
C			(INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  
C			(INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       IS2(3) AND IS2(4) ARE USED BY THE CALLING
C                       PROGRAM AS THE GRID DIMENSIONS.  (INTERNAL-
C		        OUTPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  
C			(INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C               NMEM  = THE TOTAL NUMBER OF ENSEMBLE MEMBERS TO BE USED
C             MODNUM  = THE CONTROL ENSEMBLE MEMBERS DD.
C             RSUM(K) = SUMS THE ENSEMBLE MEMBERS
C                       (K=1,ND2X3).  (AUTOMATIC)
C             RCNT(K) = COUNTS THE NUMBER OF MEMBERS TO COMPUTE AVERAGE.
C                       (K=1,ND2X3).  (AUTOMATIC)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF THE
C                       GRID IS NOT KNOWN BEFORE THE HEIGHTS ARE
C                       FETCHED.  (INPUT)
C               ISTAV = SET TO 0 TO INDICATE A GRID FIELD IS BEING RE-
C                       TURNED. (OUTPUT).
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C			USED (EITHER 32 OR 64).  (INPUT)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       103 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED.
C                       SEE GFETCH AND CONSTG FOR OTHER VALUES. 
C                       (INTERNAL-OUTPUT)
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT 
C			THE RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ).  (INTERNAL)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED FROM
C			INTERNAL AND EXTERNAL STORAGE (J=1,4).  
C			(INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS 
C			ZERO WHEN DATA ARE NOT PACKED.  (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C		        ZERO WHEN DATA ARE NOT PACKED.  (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH, CONSTG, SMTH9V
C
      PARAMETER (MODNUM=40)
      PARAMETER (NMEM=21)
C
      CHARACTER*8 CCALL(ND1,6),
     1            CCALLD(ND5)
      CHARACTER*20 NAME(ND1)
      CHARACTER*60 RACESS(NUMRA)
C
      DIMENSION ICALL(L3264W,ND1,6),
     1          NELEV(ND1),STALAT(ND1),STALON(ND1),ITIMEZ(ND1),
     2          ISDATA(ND1),SDATA(ND1),SDATA1(ND1),L1DATA(ND1)
      DIMENSION DIR(ND1,2,ND11),NGRIDC(6,ND11)
      DIMENSION ID(4),IDPARS(15),JD(4),LD(4),MDPARS(15),MD(4)
      DIMENSION FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),
     1          FD5(ND2X3),FD6(ND2X3),FD7(ND2X3),
     2          FDVERT(ND2X3),FDTIME(ND2X3),FDSINS(ND2X3),FDMS(ND2X3)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5),ICALLD(L3264W,ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION KFILRA(NUMRA)
      DIMENSION RSUM(ND2X3),RCNT(ND2X3)
C 	 
C	 CHECK WHETHER THIS ROUTINE APPLIES TO ID(1).
      IF(IDPARS(4).EQ.76)GOTO 52
      
        WRITE(KFILDO,50)(JD(J),J=1,4)
 50     FORMAT(/,' ****EMEAN ENTERED FOR INCORRECT PREDICTOR ',
     1          I9.9,2I10.9,I4.3)
        IER=103
        GO TO 800
C
 52   IER=0
      IERCNT=0
C
D     CALL TIMPR(KFILDO,KFILDO,'START ENSMEAN       ')
      IER=0
      ISTAV=0
C
      DO 100 J=1,ND2X3
         DATA(J)=9999.
         RSUM(J)=0.
         RCNT(J)=0.
 100  CONTINUE
C  
      DO 150 I=1,NMEM
         DD=MODNUM+(I-1)
C
C          NOTICE THE USE OF JD(4).  FIRST GFETCH TRIES TO FIND
C          ID WITHOUT ANY INTERPOLATION OR SMOOTHING SET.
C
         LD(1)=IDPARS(1)*1000000+IDPARS(2)*1000+DD
         LD(2)=ID(2)
         LD(3)=ID(3)
         LD(4)=JD(4)
C
         CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1               IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,DATA,ND2X3,
     2               NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3               NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
         IF(IER.NE.0)THEN
C
C             IF LD CANNOT BE FOUND MUST USED MD AND CALL OPTION AGAIN.
C             NOW MD( ) IS THE SAME AS LD( ) WITH EXCEPTION OF USING
C             ID(4) WHICH USES THE ISG IN THE FORTH WORD.  THIS IS HOW
C             OPTION IS EXPECTING THE MD/ID PLACE HOLDER.
C
            MD(1)=LD(1)
            MD(2)=LD(2)
            MD(3)=LD(3)
            MD(4)=ID(4)
C
            CALL PRSID1(KFILDO,MD,MDPARS)
            MDPARS(13)=IDPARS(13)
            MDPARS(14)=IDPARS(14)
            MDPARS(15)=IDPARS(15)
C
            CALL OPTION(KFILDO,KFIL10,NFIRST,
     1                  MD,MDPARS,THRESH,LD,NDATE,
     2                  KFILRA,RACESS,NUMRA,ICALL,CCALL,ICALLD,
     3                  CCALLD,NAME,NELEV,STALAT,STALON,
     4                  ITIMEZ,ISDATA,SDATA,SDATA1,L1DATA,DIR,ND1,NSTA,
     5                  NGRIDC,NGRID,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     6                  LSTORE,ND9,LITEMS,CORE,ND10,LASTL,
     7                  NBLOCK,LASTD,NSTORE,NFETCH,
     8                  IS0,IS1,IS2,IS4,ND7,
     9                  FD1,FD2,FD3,FD4,FD5,FD6,FD7,
     A                  FDVERT,FDTIME,FDSINS,FDMS,ND2X3,IP12,IP16,
     B                  ISTAV,L3264B,L3264W,MISTOT,IER)
            IF(IER.NE.0) THEN
               IERCNT=IERCNT+1
               IF(IERCNT.EQ.NMEM)THEN
                  WRITE(KFILDO,140) (JD(J),J=1,4),IER
 140              FORMAT(/,' ****ERROR IN ENSMEAN, FOR FIRST DATE,',
     1                     ' COULD NOT FIND ANY ESEMBLE MEMBERS',/,
     2                     '     POSSIBLE DATA GAP.  ALL VALUES',
     3                     ' ARE MISSING,',
     4                     ' FOR VARIABLE ',I9.9,2I10.9,I4.3,
     5                     '.  IER =',I4)
                  GO TO 800
               ELSE
                  IER=0
                  NSLAB=1
                  GO TO 150
               ENDIF
            ENDIF
         ENDIF
C
C        COUNT UP MEMBERS FOUND AND COMPUT THE SUM OF THE 
C        PREDICTOR
C 
         DO 145 J=1,ND2X3
            IF(NINT(DATA(J)).NE.9999)THEN
               RSUM(J)=DATA(J)+RSUM(J)
               RCNT(J)=RCNT(J)+1. 
            ENDIF
 145     CONTINUE
C
 150  CONTINUE
C
C     NOW COMPUTE THE AVERAGE OF EACH PREDICTOR USING 
C     THE RSUM AND RCNT VARIABLES.
C
      DO 175 J=1,ND2X3
         IF(NINT(RCNT(J)).GT.0.AND.
     1      NINT(RCNT(J)).NE.9999)THEN
            DATA(J)=RSUM(J)/RCNT(J)
         ELSE
            DATA(J)=9999.
         ENDIF
 175  CONTINUE
C
C     SET ISTAV TO INDICATE A GRID FIELD IS BEING RETURNED.
C
      ISTAV=0
      GO TO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C
 800  DO 801 J=1,ND5
      DATA(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END      
