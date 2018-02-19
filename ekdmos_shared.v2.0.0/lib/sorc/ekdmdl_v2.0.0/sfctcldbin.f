      SUBROUTINE SFCTCLDBIN(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                      KSTOP,SDATA,ND1,NSTA,IPACK,IWORK,FD1,ND2X3,
     2                      LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                      IS0,IS1,IS2,IS4,ND7,
     4                      ISTAV,L3264B,IER)
C
C
C        JUNE     2005   WEISS  MDL   MOS-2000
C                               NEW ROUTINE BASED ON SUBROUTINE 
C                               SFCTCLD.F
C        FEBRUARY 2006   WEISS  CHANGES MADE IN ACCORDANCE WITH THE
C                               JANUARY 2006 CODE REVIEW. TOOK OUT 
C                               CALLING ARGUMENT "ID" AND CHANGED THE
C                               IDPARS ID CHECK. ALL OTHER CHANGES 
C                               WERE COSMETIC.
C
C        PURPOSE 
C            THIS SUBROUTINE WILL COMPUTE THE AMOUNT OF TOTAL CLOUD
C            COVERAGE FROM OBSERVATION VECTOR DATA USING ONLY SURFACE 
C            OBSERVATIONS, AND THEN WILL CONVERT THE CLOUD AMOUNT TO
C            A DISCRETE BINARY VARIABLE (OCCURRED=1 AND 
C            0 = NO OCCURRENCE). THIS ROUNTINE WILL CONVERT METAR
C            SURFACE TOTAL SKY COVER ESTIMATES FOR EITHER THE
C            PERIOD STARTING ON 1996120100 00Z AND LATER (NEWPROC),
C            OR FOR DATES PRECEDING 1996120100 (OLDPROC).
C            FOR DATES 1996120100 00Z AND LATER, SURFACE TOTAL CLOUD
C            WILL BE ESTIMATED FROM THE SURFACE OBSERVATIONS OF
C            CLOUD AMOUNT FROM DIFFERENT LEVELS.
C
C            THE GENERATED PARAMETER WILL BE REFERRED TO AS THE
C            SURFACE TOTAL CLOUD COVERAGE DISCRETE BINARY WITH A
C            FIRST WORD VALUE FOR IDPARS(1) AND IDPARS(2) OF
C               708 341 - CLR
C               708 342 - FEW
C               708 343 - SCT
C               708 344 - BKN
C               708 345 - OVC
C
C           THE FOLLOWING CODED VALUES AND THEIR ASSOCIATED CLOUD 
C           COVERAGE ARE LISTED AS FOLLOWS:
C
C        SKY COVERAGE    | CLOUD COVERAGE  | CODED VALUE | OLD CODED
C                        |                 |   (METAR)   | VALUES
C                        |                 |             | (PRE-METAR)
C        CLEAR (ASOS)    |        = 0      |      0      |   0
C        CLEAR (MANUAL)  |        = 0      |  0 FROM 1   | 0 OR 1
C        FEW             |    > 0 - <= 2/8 |      2      |
C        SCATTERED       | >= 3/8 - <= 4/8 |      3      |   2,5
C        BROKEN          | >= 5/8 - < 8/8  |      6      |   3,6
C        OVERCAST        |        = 8/8    |      8      |   4,7
C        TOT OBSCURATION | ASSUME 8/8      |     10      |   8
C
C        FOR PARTIAL OBSCURATION, CLOUD COVERAGE WILL BE DETERMINED 
C        FROM CLOUDS OBSERVED ABOVE THE PARTIALLY OBSCURED LAYER.
C
C        DATA SET USE 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE 
C                      (OUTPUT). 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                      (INPUT-OUTPUT). 
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
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C            SDATA(K) = DATA TO RETURN (K=1,ND1) (OUTPUT).
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                       WITH (INPUT).
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH (INTERNAL).
C            IPACK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).
C            IWORK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).
C              FD1(J) = WORK ARRAYS (J=1,ND2X3).
C                       FD1 = ASOS CLOUD COVERAGE.
C               ND2X3 = DIMENSION OF IPACK( ), IWORK( ) AND FD1( ).
C                       SINCE THERE IS ONLY ONE DIMENSION COMING IN,
C                       IT NEEDS TO BE ND2X3.  (INPUT)
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
C                              MSTORE( , ). LATER USED AS A WAY OF 
C                              DETERMINING WHETHER TO KEEP THIS 
C                              VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ), (INPUT). 
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN (INPUT).  
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA 
C                       IDENTIFIED IN LSTORE( , ) (J=1,ND10). WHEN
C                       CORE( ) IS FULL DATA ARE STORED ON DISK
C                       (OUTPUT).
C                ND10 = DIMENSION OF CORE( ), (INPUT).
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE (INPUT).  
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING  COUNT FROM THE BEGINNING OF 
C                       OF THE PROGRAM. THIS COUNT IS MAINTAINED IN 
C                       CASE THE USER NEEDS IT (DIAGNOSTICS, ETC.). 
C                       NEEDS IT (DIAGNOSTICS, ETC.), (INTERNAL).  
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
C                      -1 = WRONG PROCEDURE (OLD VS NEW) FOR A GIVEN DATE
C                       0 = GOOD RETURN.
C                      52 = GFETCH ERROR FOR CLOUD COVERAGE
C                     103 = INPUT ID IS WRONG FOR THIS SUBROUTINE
C                       SEE GFETCH FOR VALUES.  (INTERNAL-OUTPUT)
C
C        ADDITIONAL VARIABLES
C            ASOS_MAN = LOGICAL VARIABLE USED TO DECLARE ASOS STATIONS
C                       WITH MANUAL AUGMENTATION (INTERNAL). 
C                   I = COUNTER (INTERNAL).
C        ID_CLDCOV(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD1( ) "CLOUD COVERAGE", WHERE J=1,4 (INTERNAL).
C         ID_OLDTC(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD1( ) "OLD SURFACE TOTAL CLOUD", WHERE J=1,4 
C                       (INTERNAL).
C           ITABLE(J) = TABLE CONTAINING THE FIVE POSSIBLE MOS-2000 IDS
C                       ACCEPTED BY SUBROUTINE "SFCTCLDBIN" (J=1,5).
C                       (INTERNAL)
C              ILEVEL = COUNTER FOR LEVELS 1 - 6 OF CLOUD COVERAGE
C                       (INTERNAL).
C            IOLDTCLD = INTEGER VALUE OF OLD SURFACE TOTAL CLOUD
C                       (INTERNAL).
C                   J = COUNTER (INTERNAL).
C            KSTOP(I) = FLAG VALUES TO DETERMINE IF PROCESSING FOR A
C                       GIVEN STATION HAS BEEN COMPLETED:
C                       = 0 PROCESSING INCOMPLETE
C                       = 1 PROCESSING COMPLETE
C                       (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR. RETURNED AS
C                       ZERO (GFETCH) WHEN DATA ARE NOT PACKED 
C                       (INTERNAL). 
C               MISSS = SECONDARY MISSING VALUE INDICATOR. RETURNED AS
C                       ZERO (GFETCH) WHEN DATA ARE NOT PACKED 
C                       (INTERNAL).
C             NEWPROC = LOGICAL USED TO CHOOSE SURFACE TOTAL CLOUD  
C                       FOR DATES 12/1/1996 (00Z) AND LATER (INTERNAL).
C               NPACK = 2 FOR TDL GRIB PACKED DATA: 1 FOR NOT PACKED.
C                       THIS IS STORED IN LSTORE(7, ) (INTERNAL).
C               NSLAB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE FIRST FIELD.  THIS IS THE
C                       VALUE OF NSLAB RETURNED.  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED (OUTPUT).
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT
C                       THE RECORD HAS BEEN FETCHED. THIS IS STORED
C                       IN LSTORE(9, ) (INTERNAL).
C              NWORDS = NUMBER OF WORDS RETURNED IN FD1( ) (INTERNAL).
C             OLDPROC = LOGICAL USED TO CHOOSE SURFACE TOTAL CLOUD FOR
C                       DATES PRECEDING 12/1/96 00Z (INTERNAL).
C
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH
C
      IMPLICIT NONE
C
      LOGICAL OLDPROC,NEWPROC
      LOGICAL ASOS_MAN
C
      INTEGER IDPARS(15),JD(4)
      INTEGER KSTOP(ND1)
      INTEGER IPACK(ND2X3),IWORK(ND2X3)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER ID_OLDTC(4),ID_CLDCOV(4)
      INTEGER KFILDO,KFIL10,NDATE,ND1,NSTA,ND2X3,NBLOCK,NFETCH,
     1        ND10,ISTAV,L3264B,IER,ILEVEL,NTIMES,NWORDS,IENTER,
     2        NPACK,NSLAB,MISSP,MISSS,ND7,
     3        ICLOUD_TYPE,JCLOUD_TYPE,IOLDTCLD,ND9,LITEMS,I,J,
     4        ITABLE(5),JOBVWX
C
      REAL SDATA(ND1)
      REAL FD1(ND2X3)
      REAL CORE(ND10)
C
      DATA IENTER/0/
      DATA OLDPROC/.FALSE./,NEWPROC/.FALSE./ 
      SAVE IENTER,OLDPROC,NEWPROC
      DATA ITABLE/708341,708342,708343,708344,708345/
C
D     CALL TIMPR(KFILDO,KFILDO,'START SFCTCLDBIN    ')
C
      IER=0
      ISTAV=1
C
C        STEP 1A. VERIFY THE PROCESSING OF BINARY SURFACE CLOUD 
C                 COVERAGE
C
C
      IF(IDPARS(1).NE.708.OR.((IDPARS(2).NE.341).AND.
     1                         (IDPARS(2).NE.342).AND.
     2                         (IDPARS(2).NE.343).AND.
     3                         (IDPARS(2).NE.344).AND.
     4                         (IDPARS(2).NE.345))) THEN
        IER=103
        WRITE(KFILDO,150)(JD(J),J=1,4),IER
 150    FORMAT(/,' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1           'BINARY SFC TOTAL SKY.  PREDICTOR ',
     2           I9.9,2I10.9,
     3           I4.3,' NOT ACCOMMODATED IN SUBROUTINE ',
     4           'SFCTCLDBIN.  IER=',I3)
        GO TO 800
      ENDIF
C
C        STEP 1B. INITIALIZATION.
C
      IENTER=IENTER+1
C
      DO 160 J=1,ND1
        SDATA(J)=9999.
	KSTOP(J)=0
 160  CONTINUE
C
C        STEP 1C. DETERMINE IF PROCESSING IS TO TAKE PLACE
C                 BASED ON THE CUTOFF DECEMBER 1, 1996 (00Z).
C
      IF(IENTER.EQ.1) THEN
        IF(NDATE.LT.1996120100) OLDPROC=.TRUE.
        IF(NDATE.GE.1996120100) NEWPROC=.TRUE.
      ELSEIF(IENTER.GT.1) THEN
C
C          (IF(NEWPROC.AND.(NDATE.LT.1996120100)) THEN)
C          IS NOT NECESSARY SINCE DATPRO WITHIN U201 WILL FLAG
C          DATES LESS THAN THE PRECEDING DATE.
C
        IF((OLDPROC).AND.(NDATE.GE.1996120100)) THEN
          WRITE(KFILDO,170) NDATE
 170      FORMAT(/,' ****ERROR IN SFCTCLDBIN ATTEMPTING OLDPROC',
     1             ' PROCESSING WHEN DATE =',I12,' > 1996120100',/,
     2             '     NO PHYSICAL RECORD IS WRITTEN FOR THIS',
     3             ' PROCESS DATE.  ALL VALUES OF',
     3             ' SURFACE TOTAL CLOUD ARE MISSING.')
          IER=-1
          GO TO 800
        ENDIF
C
      ENDIF
C
C        STEP 1D. CONSTRUCT 4 WORD ID'S.
C                 CONSTRUCT THE CLOUD COVERAGE ARRAY FOR LEVEL #1.
c
      ID_CLDCOV(1)=708320000
      ID_CLDCOV(2)=IDPARS(7)
      ID_CLDCOV(3)=IDPARS(9)*1000000+IDPARS(12)
      ID_CLDCOV(4)=0
C
      ID_OLDTC(1)=708310000
      ID_OLDTC(2)=IDPARS(7)
      ID_OLDTC(3)=IDPARS(9)*1000000+IDPARS(12)
      ID_OLDTC(4)=0
C
C
C*********** NEWPROC *****
C*********** NEWPROC *****
C
C
      IF(NEWPROC) THEN

C        STEP 2. FETCH CLOUD COVERAGE FOR LEVELS 1 - 6 (USING FD1).
C                WORD #1 OF ID_CLDCOV CHANGE FOR LEVELS 2 - 6.
C                AND ESTIMATE TOTAL CLOUD (NEWPROC)
C
        DO 400 ILEVEL=1,6
          IF(ILEVEL.GE.2) ID_CLDCOV(1)=ID_CLDCOV(1)+2000
C
C            FETCH CLOUD COVERAGE.
C
          CALL GFETCH(KFILDO,KFIL10,ID_CLDCOV,7777,LSTORE,ND9,LITEMS,
     1                IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2                NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3                NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
C
          IF((IER.NE.0).AND.(IENTER.EQ.1)) THEN
            WRITE(KFILDO,320) ILEVEL,(JD(J),J=1,4),IER
 320        FORMAT(/' ****ERROR FROM GFETCH IN SFCTCLDBIN AT 320 FOR',
     1              ' FIRST PROCESS DATE, CLOUD AMOUNT FOR LEVEL ',I2,
     2             /'     POSSIBLE DATA GAP.   VALUES OF VARIABLE ',
     3               I9.9,2I10.9,I4.3,' SET TO MISSING.  IER = ',I4)

            GO TO 800
          ELSEIF((IER.NE.0).AND.(IENTER.GT.1)) THEN
            GO TO 800
          ENDIF
C
          IF(NWORDS.NE.NSTA) THEN
            WRITE(KFILDO,325)NWORDS,NSTA,ILEVEL,(JD(J),J=1,4)
 325        FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1              ' NOT EQUAL TO NSTA =',I6,' AT 325 FOR CLOUD',
     2              ' COVERAGE (LEVEL ',I1,') IN SFCTCLDBIN.',
     3             /'     VALUES OF VARIABLE ',I9.9,2I10.9,I4.3,
     4              ' SET TO MISSING.')
            IER=52
            GO TO 800
          ENDIF
C
C            STEP 2A. DETERMINE TOTAL CLOUD COVERAGE FOR EACH STATION
C                     BY DETERMINING THE HIGHEST (LARGEST) CLOUD
C                     COVERAGE FOR SIX POSSIBLE LAYERS.
C
C        *****  MAIN DO LOOP  ***** 
C
          DO 350 J=1,NSTA
	    ICLOUD_TYPE=NINT(FD1(J))
C
            IF(KSTOP(J).EQ.0) THEN
	      ASOS_MAN=.FALSE.
              IF(NINT(FD1(J)).NE.9999) ASOS_MAN=.TRUE.
C
C               STEP 2B. PROCESS LEVEL #1.
C
              IF(ILEVEL.EQ.1)THEN
C
                IF(ASOS_MAN) THEN
C
C                      TOTAL OBSCURATION OR OVERCAST
C
                  IF((ICLOUD_TYPE.EQ.10).OR.(ICLOUD_TYPE.EQ.8)) THEN
                    SDATA(J)=FD1(J)
                    KSTOP(J)=1
C
C                      PARTIAL OBSCURATION: SDATA SET TO 0 TEMPORARILY
C
                  ELSEIF(ICLOUD_TYPE.EQ.9) THEN
	            SDATA(J)=0.
C
C                      CLEAR
C
                  ELSEIF((ICLOUD_TYPE.EQ.0).OR.(ICLOUD_TYPE.EQ.1)) THEN
                    SDATA(J)=0.
		    KSTOP(J)=1
C
C                      FEW, SCATTERED OR BROKEN
C
                  ELSEIF((ICLOUD_TYPE.GE.2).OR.(ICLOUD_TYPE.LE.6)) THEN
                    SDATA(J)=FD1(J)
                  ENDIF
C
                ELSE
	          KSTOP(J)=1
                ENDIF
C
              ENDIF
C
C                STEP 2C. PROCESS LEVELS 2 - 6
C
	      IF(ILEVEL.GE.2)THEN
	        JCLOUD_TYPE=NINT(SDATA(J))
C
	        IF(ASOS_MAN) THEN
C
C                    FEW, SCATTERED, BROKEN OR OVERCAST
C
                  IF(ICLOUD_TYPE.GE.JCLOUD_TYPE) THEN
	            SDATA(J)=FD1(J)
		    IF(ICLOUD_TYPE.EQ.8) KSTOP(J)=1
                  ENDIF
C
                ELSE
	          KSTOP(J)=1
                ENDIF
C
              ENDIF
C
            ENDIF
C
 350      CONTINUE
C
C        ***** END OF MAIN DO LOOP *****
C
 400    CONTINUE
C
C
C*********** OLDPROC *****
C*********** OLDPROC *****
C
      ELSEIF(OLDPROC) THEN
C
C          STEP 3. READ SURFACE TOTAL CLOUD (708310) IN THE
C          OLD CODED VALUE (FD1) AND CONVERT TO THE NEW CODED VALUES
C          (708311)
C
C          FETCH OLD SURFACE TOTAL CLOUD COVERAGE
C
        CALL GFETCH(KFILDO,KFIL10,ID_OLDTC,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4              IER)
C
        IF((IER.NE.0).AND.(IENTER.EQ.1)) THEN
          WRITE(KFILDO,450)(JD(J),J=1,4),IER
 450      FORMAT(/' ****ERROR FROM GFETCH IN SFCTCLDBIN,FIRST PROCESS',
     1            ' DATE.  OLD SURFACE TOTAL CLOUD IS MISSING AT 450.',
     2           /'     POSSIBLE DATA GAP.  VALUES OF VARIABLE ',
     3            I9.9,2I10.9,I4.3,' SET TO MISSING.  IER = ',I4)
          GO TO 800
        ELSEIF((IER.NE.0).AND.(IENTER.GT.1)) THEN
          GO TO 800
        ENDIF
C
        IF(NWORDS.NE.NSTA) THEN
          WRITE(KFILDO,460)NWORDS,NSTA,(JD(J),J=1,4)
 460      FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1            ' NOT EQUAL TO NSTA =',I6,' AT 460 FOR OLD SURFACE',
     2            ' TOTAL CLOUD IN SFCTCLDBIN.',
     3           /'     VALUES OF VARIABLE ',I9.9,2I10.9,I4.3,
     4            ' SET TO MISSING.')
          IER=52
          GO TO 800
        ENDIF
C
C          STEP 3A. CONVERT OLD SURFACE TOTAL CLOUD CODED VALUES TO
C                   METAR CODED VALUES
C
        DO 500 J=1,NSTA
          IOLDTCLD=NINT(FD1(J))
C
          IF((IOLDTCLD.EQ.0).OR.(IOLDTCLD.EQ.1)) THEN
            SDATA(J)=0.
          ELSEIF((IOLDTCLD.EQ.2).OR.(IOLDTCLD.EQ.5)) THEN
            SDATA(J)=3.
          ELSEIF((IOLDTCLD.EQ.3).OR.(IOLDTCLD.EQ.6)) THEN
            SDATA(J)=6.
          ELSEIF((IOLDTCLD.EQ.4).OR.(IOLDTCLD.EQ.7)) THEN
            SDATA(J)=8.
          ELSEIF(IOLDTCLD.EQ.8) THEN
            SDATA(J)=10.
          ELSE
            SDATA(J)=9999.
          ENDIF
C
 500    CONTINUE
C
      ENDIF
C
C        STEP 4. CALCULATE THE DISCRETE BINARY 
C                SURFACE TOTAL SKY COVER VALUE

      JOBVWX=JD(1)/1000
C
      IF(JOBVWX.EQ.ITABLE(1)) THEN
C
C             ASSIGN BINARY TO CLEAR.
C
         DO 505 J=1,NSTA
            IF(NINT(SDATA(J)).NE.9999) THEN
               IF(NINT(SDATA(J)).LT.2) THEN
                  SDATA(J)=1.
               ELSE
                  SDATA(J)=0.
               ENDIF
            ENDIF
 505     CONTINUE
C
      ELSEIF(JOBVWX.EQ.ITABLE(2)) THEN
C
C             ASSIGN BINARY TO FEW.
C
         DO 510 J=1,NSTA
            IF(NINT(SDATA(J)).NE.9999) THEN
               IF(NINT(SDATA(J)).EQ.2) THEN
                  SDATA(J)=1.
               ELSE
                  SDATA(J)=0.
               ENDIF
            ENDIF
 510     CONTINUE
C
      ELSEIF(JOBVWX.EQ.ITABLE(3)) THEN
C
C             ASSIGN BINARY TO SCT.
C
         DO 515 J=1,NSTA
            IF(NINT(SDATA(J)).NE.9999) THEN
               IF(NINT(SDATA(J)).EQ.3) THEN
                  SDATA(J)=1.
               ELSE
                  SDATA(J)=0.
               ENDIF
            ENDIF
 515     CONTINUE
C
      ELSEIF(JOBVWX.EQ.ITABLE(4)) THEN
C
C             ASSIGN BINARY TO BKN.
C
         DO 520 J=1,NSTA
            IF(NINT(SDATA(J)).NE.9999) THEN
               IF(NINT(SDATA(J)).EQ.6) THEN
                  SDATA(J)=1.
               ELSE
                  SDATA(J)=0.
               ENDIF
            ENDIF
 520       CONTINUE
C
      ELSEIF(JOBVWX.EQ.ITABLE(5)) THEN
C
C             ASSIGN BINARY TO OVC.
C
         DO 525 J=1,NSTA
            IF(NINT(SDATA(J)).NE.9999) THEN
               IF(NINT(SDATA(J)).GE.8) THEN
                  SDATA(J)=1.
               ELSE
                  SDATA(J)=0.
               ENDIF
            ENDIF
 525     CONTINUE
C
      ENDIF
C
D      CALL TIMPR(KFILDO,KFILDO,'END   SFCTCLDBIN    ')
C
      GO TO 850 
C
C        PREMATURE TERMINATION OF SUBROUTINE.
C
 800  DO 810 I=1,ND1
        SDATA(I)=9999.
 810  CONTINUE
C
      WRITE(KFILDO,815) IER,(JD(J),J=1,4)
 815  FORMAT(/' ****ERROR IN SFCTCLDBIN, IER =',I5,
     1        ' FOR VARIABLE',4I12)
C
 850  RETURN
      END
