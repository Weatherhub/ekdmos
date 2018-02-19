      SUBROUTINE GRMEAN(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                     NGRIDC,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     2                     LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                     KFILRA,RACESS,IS0,IS1,IS2,IS4,ND7,
     4                     FD1,FD2,FD3,FD4,FD5,FD6,FD7,NUMRA,ND1,
     A                     FDVERT,FDTIME,FDSINS,FDMS,NSTA,
     B                     ND2X3,DATACDF,NCDF,NELEV,DIR,
     5                     STALAT,STALON,L3264W,
     6                     ISTAV,L3264B,MISTOT,IER)
C
C
C        SEPT     2010   WAGNER    MDL   MOS-2000
C
C        PURPOSE
C            THIS SUBROUTINE COMPUTED THE AVERAGE OF THE ENSEMBLE    
C            MEMBERS OF A SINGLE MODEL FOR EKDMOS.                   
C
C            THE FOLLOWING IDPARS(4) ARE ACCOMMODATED:
C
C                62      -  GEFS                
C                63      -  CMCE                     
C                61      -  NAEFS                   
C
C        HISTORY:   
C        09-10       WAGNER  	THIS SUBROUTINE IS NEW TO U202.
C        12-11       WAGNER     ADDED NMEM CHECK FOR MEANS. 
C
C        DATA SET USE
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT(PRINT) FILE.
C                       (OUTPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                       ACCESS.(INPUT-OUTPUT)
C
C        VARIABLES
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE(,) (J=1,ND10).
C                       WHEN CORE() IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C             DATA( ) = THE 12-H MX/MN TEMPERATURE COMPUTED FROM THE TWO 6-H 
C                       VALUES.  (OUTPUT)
C                   I = LOOP CONTROL VARIABLE.  (INTERNAL)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID() (J=1,15).
C                       (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY
C                                      1 LAYER)
C                       J=7--LTLTLTLT (TOP OF LAYER)
C                       J=8--T (TRANSFORMATION)
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND 
C                                BACK IN TIME)
C                       J=10-OT (TIME APPLICATION)
C                       J=11-OH (TIME PERIOD IN HOURS)
C                       J=12-TAU (PROJECTION IN HOURS)
C                       J=13-I (INTERPOLATION TYPE)
C                       J=14-S (SMOOTHING INDICATOR)
C                       J=15-G (GRID INDICATOR)
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                       100 = THE TWO GRIDS NEEDED ARE NOT THE SAME SIZE
C                       101 = GRID SIZE IS TOO BIG FOR ???(), WHOSE 
C                             DIMENSION IS ND5.
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE
C                             MX OR MX TEMP.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5). (INTERNAL)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).
C                       (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).
C                       (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       IS2(3) AND IS2(4) ARE USED BY THE CALLING
C                       PROGRAM AS THE GRID DIMENSIONS.
C                       (INTERNAL-OUTPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).
C                       (INTERNAL)
C               ISTAV = 0 SINCE THE DATA RETURNED ARE GRID DATA.
C                       (OUTPUT)
C            IWORK(J) = WORK ARRAY (J=1,ND5). (INTERNAL)
C                   J = LOOP CONTROL VARIABLE
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT
C                       THE PORTIONS PERTAINING TO PROCESSING
C                       ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       ID() IS USED TO HELP IDENTIFY THE BASIC MODEL
C                       FIELDS AS READ FROM THE ARCHIVE.  (INPUT)
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE(,)
C                       THAT HAVE BEEN USED IN THIS RUN.  (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE
C                       DATA STORED (L=1,12) (J=1,LITEMS).
C                       (INPUT-OUTPUT)
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE() WHERE
C                              THE DATA START.  WHEN ON DISK,
C                              THIS IS MINUS THE RECORD NUMBER WHERE
C                              THE DATA START.
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDL GRIB, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --NUMBER OF THE SLAB IN DIR(, ,L) AND
C                              IN NGRIDC(,L) DEFINING THE
C                              CHARACTERISTICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID(,N) (N=1,NPRED) FOR WHICH
C                              THIS VARIABLE IS NEEDED, WHEN IT IS
C                              NEEDED ONLY ONCE FROM LSTORE(,).
C                              WHEN IT IS NEEDED MORE THAN ONCE, THE 
C                              VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING
C                              MSTORE(,). LATER USED AS A WAY OF
C                              DETERMINING WHETHER TO KEEP THIS
C                              VARIABLE.
C            MDPARS() = PARSED ID USED IN SUBROUTINE PRSID1 FOR
C                       SUBROUTINE WETBULBT.  (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 WHEN DATA ARE NOT PACKED.  (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 WHEN DATA ARE NOT PACKED.  (INTERNAL)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  
C                       ALL WORK ARRAYS ARE DIMENSIONED ND2X3. (INPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND DATA( ). 
C                       (INPUT)
C                 ND7 = DIMENSION OF IS0(),IS1(),IS2(), AND IS4().
C                       NOT ALL LOCATIONS ARE USED. (INPUT)
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C                ND10 = DIMENSION OF CORE().  (INPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN
C                       BE DEALT WITH ON THIS RUN.  LAST DIMENSION 
C                       OF NGRIDC(,).  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING COUNT FROM THE BEGINNING OF
C                       THE PROGRAM.  THIS COUNT IS MAINTAINED IN
C                       CASE THE USER NEEDS IT(DIAGNOSTICS, ETC.).
C                       (OUTPUT)
C         NGRIDC(L,M) = HOLDS THE GRID CHARACTERISTICS (L=1,6) FOR
C                       EACH GRID COMBINATION.  (M=1,NGRID).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=
C                            POLAR STEREOGRAPHIC).
C                       L=2--GRID LENGTH IN METERS.
C                       L=3--LATITUDE AT WHICH THE GRID LENGTH IS
C                            CORRECT *1000.
C                       L=4--GRID ORIENTATION IN DEGREES * 1000.
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *1000.
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES
C                            *1000.
C               NPACK = 2 FOR TDL GRIB PACKED DATA; 1 FOR NOT PACKED
C                       THIS IS RETURNED FROM GFETCH.  (INTERNAL)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND
C                       IN NGRIDC( , ) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  (OUTPUT)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE,
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS 
C                       STORED IN LSTORE(9,).  (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA().  THIS 
C                       IS RETURNED FROM GFETCH.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C     NON-SYSTEM SUBROUTINES USED
C         GFETCH,PRSID1
C
C     
      PARAMETER (NMEM=21)
C
      CHARACTER*60 RACESS(NUMRA)
      CHARACTER*8 CCALL(ND1,6),
     1            CCALLD(ND5)
      CHARACTER*20 NAME(ND1)
C
      DIMENSION IDPARS(15),JD(4),MDPARS(15)
      DIMENSION IPACK(ND5),IWORK(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9),DATACDF(ND5,NCDF)
      DIMENSION DIR(ND1,2,ND11),NGRIDC(6,ND11)
      DIMENSION MD(4),MD1(4),KFILRA(NUMRA)
C      DIMENSION NSLAB1,IER,ISTAV,
C     1        KFILDO,KFIL10,L3264B,LITEMS,
C     2        MISSP,MISSS,MISTOT,IMXMN,
C     3        NBLOCK,ND2X3,ND5,ND7,ND9,ND10,ND11,
C     4        NDATE,NFETCH,NPACK,NSLAB,
C     5        NTIMES,NWORDS
      DIMENSION ICALL(L3264W,ND1,6),ICALLD(L3264W,ND5),
     1          NELEV(ND1),STALAT(ND1),STALON(ND1),ITIMEZ(ND1),
     2          ISDATA(ND1),SDATA(ND1),SDATA1(ND1),L1DATA(ND1)
C
      DIMENSION DATA(ND5)
      DIMENSION FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),
     1          FD5(ND2X3),FD6(ND2X3),FD7(ND2X3),
     2          FDVERT(ND2X3),FDTIME(ND2X3),FDSINS(ND2X3),FDMS(ND2X3)
      DIMENSION CORE(ND10)
      DIMENSION DATAMEM(ND5,NMEM)
      DIMENSION RSUM(ND2X3),RCNT(ND2X3)
C
      IER  =0
      ISTAV=0
      IERCNT=0
C
C        MAKE SURE THE REQUESTED PREDICTOR IS AN ENSEMBLE MEAN         
C
       IF((IDPARS(4).NE.62).AND.(IDPARS(4).NE.63).AND.
     &    (IDPARS(4).NE.61))THEN
	 IER=103
	 WRITE(KFILDO,100)(JD(J),J=1,4),IER
 100     FORMAT(/' ****IDPARS(4) DOES NOT INDICATE',
     1           ' AN ENSEMBLE MEAN.',
     2          /'     PREDICTOR ',I9.9,I10.9,I10.9,I4.3,
     3           ' NOT ACCOMMODATED IN GRMEAN.  IER = ',I3)
	 GO TO 800
      ENDIF
C
C        INITIALIZE OUR ARRAYS.                                        
C
      DO 101 J=1,ND2X3
         DATA(J)=9999.
         SDATA(J)=9999.
         RSUM(J)=0.
         RCNT(J)=0.
         DO 1001 I=1,NMEM
            DATAMEM(J,I)=9999.
 1001    CONTINUE
         DO 1002 K=1,NCDF
            DATACDF(J,K)=9999.
 1002    CONTINUE
 101  CONTINUE
C
      DO 150 I=1,NMEM
         IF((IDPARS(4).EQ.62).OR.(IDPARS(4).EQ.63))THEN
            MODNUM=40
         ELSE
            MODNUM=62
         ENDIF
         DD=MODNUM+(I-1)
         IF(DD.GE.64) GO TO 150
C
C          NOTICE THE USE OF JD(4).  FIRST GFETCH TRIES TO FIND
C          ID WITHOUT ANY INTERPOLATION OR SMOOTHING SET.
C
         MD(1)=IDPARS(1)*1000000+IDPARS(2)*1000+DD
C         MD(2)=IDPARS(7)
         MD(2)=IDPARS(6) * 10000 + IDPARS(7)
         MD(3)=IDPARS(9) * 1000000 + IDPARS(12)
         MD(4)=JD(4)
C
         CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,DATA,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,
     4            1,IER)
         IF(MISSP.NE.0)MISTOT=MISTOT+1
         IF((IER.NE.0).AND.(IDPARS(4).NE.61))THEN
C
C             IF MD CANNOT BE FOUND, CALL OPTION AGAIN.
C             NOW MD( ) USES IDPARS(13-15) INT HE FOURTH WORD.  THIS IS 
C             HOW OPTION IS EXPECTING THE MD/ID PLACE HOLDER.
C
            MD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
C
            CALL PRSID1(KFILDO,MD,MDPARS)
            MDPARS(13)=IDPARS(13)
            MDPARS(14)=IDPARS(14)
            MDPARS(15)=IDPARS(15)
C
            CALL OPTION(KFILDO,KFIL10,NFIRST,
     1                  MD,MDPARS,THRESH,JD,NDATE,
     2                  KFILRA,RACESS,NUMRA,ICALL,CCALL,ICALLD,
     3                  CCALLD,NAME,NELEV,STALAT,STALON,
     4                  ITIMEZ,ISDATA,SDATA,SDATA1,L1DATA,DIR,ND1,NSTA,
     5                  NGRIDC,NGRID,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     6                  LSTORE,ND9,LITEMS,CORE,ND10,LASTL,
     7                  NBLOCK,LASTD,NSTORE,NFETCH,
     8                  IS0,IS1,IS2,IS4,ND7,
     9                  FD1,FD2,FD3,FD4,FD5,FD6,FD7,DATACDF,NCDF,
     A                  FDVERT,FDTIME,FDSINS,FDMS,ND2X3,IP12,IP16,
     B                  ISTAV,L3264B,L3264W,MISTOT,IER)
C           NOTE:  May have to remove DATACDF and NCDF from the call to option

            IF(IER.NE.0) THEN
               IERCNT=IERCNT+1
               IF(IERCNT.EQ.NMEM)THEN
                  WRITE(KFILDO,140) (JD(J),J=1,4),IER
 140              FORMAT(/,' ****ERROR IN GRMEAN, FOR FIRST DATE,',
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
         ELSEIF((IER.NE.0).AND.(IDPARS(4).EQ.61))THEN
            IF(IERCNT.EQ.NMEM)THEN
               WRITE(KFILDO,141) (JD(J),J=1,4),IER
 141           FORMAT(/,' ****ERROR IN GRMEAN, FOR FIRST DATE,',
     1               ' COULD NOT FIND ANY ESEMBLE MEANS',/,
     2               '     POSSIBLE DATA GAP.  ALL VALUES',
     3               ' ARE MISSING,',
     4               ' FOR VARIABLE ',I9.9,2I10.9,I4.3,
     5               '.  IER =',I4)
               GO TO 800
            ELSE
               IER=0
               NSLAB=1
               GO TO 150
            ENDIF
         ENDIF
         IF(IER.NE.0)GOTO 800
C
C       RECORD THE GRID DIMENSIONS TO USE FOR VERIFICATION IN LATER
C       CALLS TO GFETCH
C
         LX = IS2(3)
         LY = IS2(4)
         NSLAB1 = NSLAB
C
C        COUNT UP MEMBERS FOUND AND COMPUTE THE SUM OF THE 
C        PREDICTOR
C 
         DO 145 J=1,LX*LY
            IF(NINT(DATA(J)).NE.9999)THEN
               RSUM(J)=DATA(J)+RSUM(J)
               RCNT(J)=RCNT(J)+1.
               DATAMEM(J,I)=DATA(J)
            ENDIF
 145     CONTINUE
C
C
 150   CONTINUE
C
C     NOW COMPUTE THE AVERAGE OF EACH PREDICTOR USING 
C     THE RSUM AND RCNT VARIABLES.
C
      DO 175 J=1,LX*LY
         IF(NINT(RCNT(J)).GT.0.AND.
     1      NINT(RCNT(J)).NE.9999)THEN
            DATA(J)=RSUM(J)/RCNT(J)
         ELSE
            DATA(J)=9999.
         ENDIF
 175  CONTINUE
C
      IF(((IDPARS(2).EQ.301).OR.(IDPARS(2).EQ.351).OR.
     1   (IDPARS(2).EQ.361)).AND.((IDPARS(4).EQ.62).OR.
     2   (IDPARS(4).EQ.63)))THEN
         CALL MODCDF(KFILDO,KFIL10,DATAMEM,DATACDF,NMEM,RCNT,
     1               ND2X3,NCDF,ND5)
C,IDPARS(1),IDPARS(2),IDPARS(3),
C     1               IDPARS(4),IDPARS(5),IDPARS(6),IDPARS(7),
C     1               IDPARS(8),IDPARS(9),IDPARS(10),IDPARS(11),
C     1               IDPARS(12),IDPARS(13),IDPARS(14),IDPARS(15),
C     1               LD(1),LD(2),LD(3),LD(4))
      ENDIF
C
C     SET ISTAV TO INDICATE A GRID FIELD IS BEING RETURNED.
C
      ISTAV=0
      GO TO 900
C
C         IF THERE WAS A PROBLEM IN GFETCH IT WOULD COME HERE TO
C         SET DATA TO MISSING
C
 800   DO I=1,ND2X3
         DATA(I)=9999.
       ENDDO
C
 900  RETURN
      END

