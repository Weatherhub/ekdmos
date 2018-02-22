      SUBROUTINE OBSCIGHT(KFILDO,KFIL10,
     1                    ID,IDPARS,JD,NDATE,SDATA,ND1,NSTA,
     2                    IPACK,IWORK,FD1,FD2,ND2X3,
     3                    LSTORE,ND9,LITEMS,CORE,ND10,
     4                    NBLOCK,NFETCH,
     5                    IS0,IS1,IS2,IS4,ND7,
     6                    ISTAV,L3264B,IER)
C
C        DECEMBER 1998   WEISS   TDL   MOS-2000
C        NOVEMBER 2000   RUDACK  MODIFIED CODE TO COMPLY WITH
C                                MDL FORMAT SPECIFICATIONS
C        MARCH    2001   GLAHN   MODIFIED SOME FORMATS TO BETTER
C                                CONFORM WITH MOS-2000 DIAGNOSTICS
C        OCTOBER  2001   WEISS   IF STATEMENTS CONTAINING REAL VALUES
C                                ARE CHANGED TO INTEGER VALUES.
C        MAY      2002   GLAHN   CHANGED ND5 TO ND2X3
C        APRIL    2003   GLAHN   ADDED WHITE SPACE; SPELL CHECK;
C                                REARRANGED TYPED VARIABLES;
C                                CHANGED LOOP TO NSTA FROM NWORDS;
C                                ADDED KSTOP TO CALL
C        OCTOBER  2003   SMB     CORRECTED FORMAT STATEMENTS 205
C                                AND 212 FOR THE IBM 
C        JANUARY  2005   JPD     REMOVED KSTOP FROM CALL
C
C        PURPOSE 
C            THIS SUBROUTINE WILL COMPUTE THE CEILING HEIGHT
C            FROM OBSERVATION VECTOR DATA. THE CEILING HEIGHT 
C            WILL BE ESTIMATED FROM THE CLOUD HEIGHT OF THE LOWEST 
C            LAYER OF REPORTED BROKEN OR OVERCAST CLOUD COVER (6/8 
C            COVERAGE OR GREATER) OR, IF THE SKY IS TOTALLY OBSCURED.
C            CLOUD COVERAGE LESS THAN 6/8 OR A PARTIALLY OBSCURED
C            SKY WILL NOT BE CONSIDERED A CEILING.
C
C            NOTE: THIS SUBROUTINE WORKS ONLY FOR METAR DATA COVERING 
C                  THE PERIOD STARTING DEC. 1996.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               708 000 - CEILING HEIGHT - HUNDREDS OF FEET
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
C               ID(J) = THE VARIABLE ID (J=1,4) (INPUT). 
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
C                       WITH (INPUT).
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH (INTERNAL).
C            IPACK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).
C            IWORK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).
C       FD1(J),FD2(J) = WORK ARRAYS (J=1,ND2X3).
C                       FD1 CONTAINS CLOUD AMOUNT AND
C                       FD2 CONTAINS CLOUD HEIGHT (INTERNAL).
C               ND2X3 = DIMENSION OF IPACK( ), IWORK( ), FD1( ) AND 
C                       FD2( ) (INPUT).
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
C                       SEE GFETCH FOR VALUES.  (INTERNAL-OUTPUT)
C
C        ADDITIONAL VARIABLES
C             ICLDAMT = INTEGER VALUE OF CLOUD AMOUNT (INTEGER).
C              IENTER = NUMBER OF TIMES SUBROUTINE IS ENTERED
C                       (INTERNAL).
C              ILEVEL = COUNTER FOR LEVELS 1 - 6 OF CLOUD HEIGHT AND 
C                       AMOUNT (INTERNAL).
C                   J = INTEGER COUNTER (INTERNAL).
C            KSTOP(K) = FLAG VALUES TO DETERMINE IF PROCESSING FOR A
C                       GIVEN STATION HAS BEEN COMPLETED (K=1,ND1):
C                       = 0 PROCESSING INCOMPLETE
C                       = 1 PROCESSING COMPLETE
C                       (INTERNAL).
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO 
C                       FD1( ) "CLOUD AMOUNT", WHERE J=1,4 (INTERNAL).  
C               MD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO 
C                       FD2( ) "CLOUD HEIGHT", WHERE J=1,4 (INTERNAL). 
C               MISSP = PRIMARY MISSING VALUE INDICATOR. RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED (INTERNAL). 
C               MISSS = SECONDARY MISSING VALUE INDICATOR. RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED (INTERNAL). 
C               NPACK = 2 FOR TDL GRIB PACKED DATA: 1 FOR NOT PACKED.
C                       THIS IS STORED IN LSTORE(7, ) (INTERNAL). 
C               NSLAB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE FIRST FIELD.  THIS IS THE
C                       VALUE OF NSLAB RETURNED.  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED (OUTPUT).  
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT
C                       THE RECORD HAS BEEN FETCHED. THIS IS STORED
C                       IN LSTORE(9, ) (INTERNAL).
C              NWORDS = NUMBER OF WORDS RETURNED IN FD1( ) OR FD2( ),
C                       (INTERNAL).
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH
C
      IMPLICIT NONE
C
      INTEGER ID(4),IDPARS(15),JD(4)
      INTEGER KSTOP(ND1)
      INTEGER IPACK(ND2X3),IWORK(ND2X3)
      INTEGER LSTORE(12,ND9)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LD(4),MD(4)
      INTEGER KFILDO,KFIL10,ND2X3,ND1,ND7,ND9,LITEMS,ND10,
     1        ISTAV,L3264B,IER,IENTER,NBLOCK,NFETCH,
     2        J,NSLAB,ILEVEL,NTIMES,NWORDS,NPACK,NSTA,NDATE,
     3        ICLDAMT,MISSP,MISSS 
C 
      REAL SDATA(ND1)
      REAL FD1(ND2X3),FD2(ND2X3)
      REAL CORE(ND10)
C
      DATA IENTER/0/
      SAVE IENTER
C
      IER=0
      ISTAV=1
C
C        STEP 1A. VERIFY THE PROCESSING OF CEILING HEIGHT.
C
      IF(JD(1).NE.708000000) THEN
        WRITE(KFILDO,150)(JD(J),J=1,4)
 150    FORMAT(/' ****OBSCIGHT ENTERED FOR INCORRECT ID.',
     1          '  PREDICTOR ',I9.9,2I10.9,I4.3,' NOT COMPUTED.')
        IER=103
        GO TO 800
      ENDIF
C
C        STEP 1B. INITIALIZATION.
C
      IENTER=IENTER+1
C
      DO 200 J=1,ND1
        SDATA(J)=9999.
	KSTOP(J)=0
 200  CONTINUE
C
C        CONSTRUCT THE CLOUD AMOUNT ARRAY (LD) FOR LEVEL #1.
C
      LD(1)=708320000
      LD(2)=IDPARS(7)
      LD(3)=IDPARS(9)*1000000+IDPARS(12)
      LD(4)=0
C
C        CONSTRUCT THE CLOUD HEIGHT ARRAY (MD) FOR LEVEL #1.
C
      MD(1)=708321000
      MD(2)=IDPARS(7)
      MD(3)=IDPARS(9)*1000000+IDPARS(12)
      MD(4)=0
C
C        STEP 2. FETCH CLOUD AMOUNT AND HEIGHT FOR LEVELS
C                1 - 6. WORD #1 OF LD AND MD WILL CHANGE FOR
C                LEVELS 2 - 6. 
C
      DO 300 ILEVEL=1,6
C
        IF(ILEVEL.GE.2)THEN
          LD(1)=LD(1)+2000
          MD(1)=MD(1)+2000
        ENDIF
C
C         FETCH CLOUD AMOUNT.
C
        CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4              IER)
C
        IF((IER.NE.0).AND.(IENTER.EQ.1)) THEN
          WRITE(KFILDO,205) ILEVEL,(JD(J),J=1,4),IER
 205      FORMAT(/' ****ERROR IN OBSCIGHT, FOR FIRST PROCESS DATE,',
     1            ' CLOUD AMOUNT FOR LEVEL ',I2,' IS MISSING',
     2           /'     POSSIBLE DATA GAP.  ALL VALUES OF CEILING',
     3            ' HEIGHT ARE MISSING',
     4            ' FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER =',I3)
          GO TO 800
C
        ELSEIF((IER.NE.0).AND.(IENTER.GT.1)) THEN
          GO TO 800
        ENDIF
C
        IF(NWORDS.NE.NSTA) THEN
          WRITE(KFILDO,210)NWORDS,NSTA
 210      FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1           ' NOT EQUAL TO NSTA =',I6,' FOR CLOUD AMOUNT',
     2           ' IN OBSCIGHT.  DATA SET TO MISSING.')
          IER=52
          GO TO 800
        ENDIF
C
C         FETCH CLOUD HEIGHT.
C
        CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4              IER)
C
        IF((IER.NE.0).AND.(IENTER.EQ.1)) THEN
          WRITE(KFILDO,212) ILEVEL,(JD(J),J=1,4),IER
 212      FORMAT(/,' ****ERROR IN OBSCIGHT, FOR FIRST PROCESS DATE,',
     1            ' CLOUD HEIGHT FOR LEVEL ',I2,' IS MISSING',
     2           /'     POSSIBLE DATA GAP.  ALL VALUES OF CEILING',
     3            ' HEIGHT ARE MISSING',
     4            ' FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER =',I3)
          GO TO 800
        ELSEIF((IER.NE.0).AND.(IENTER.GT.1)) THEN
          GO TO 800
        ENDIF
C
        IF(NWORDS.NE.NSTA) THEN
          WRITE(KFILDO,215)NWORDS,NSTA
 215      FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1             ' NOT EQUAL TO NSTA =',I6,' FOR CLOUD HEIGHT',
     2             ' IN OBSCIGHT.  DATA SET TO MISSING.')
          IER=52
          GO TO 800
        ENDIF
C
C        STEP 3. DETERMINE IF A CEILING EXISTS FOR LEVEL #1 FOR ALL 
C                STATIONS. 
C
	DO 250 J=1,NSTA
C
C            STEP 3A. PROCESS LEVEL #1 
C
          IF(ILEVEL.EQ.1)THEN
C
            IF((NINT(FD1(J)).NE.9999).AND.(NINT(FD2(J)).NE.9999))THEN
C                BKN, OVC, TOTAL OBS, BUT NOT PARTIAL OBS
	      ICLDAMT=NINT(FD1(J))
C
              IF((ICLDAMT.GE.6).AND.(ICLDAMT.NE.9))THEN
                SDATA(J)=FD2(J)
                KSTOP(J)=1
C                  CLR
C                  888 = INSUFFICIENT CLOUD COVER FOR CEILING HT ESTIMATE
              ELSEIF((ICLDAMT.EQ.0).OR.(ICLDAMT.EQ.1))THEN
	        SDATA(J)=888.
	        KSTOP(J)=1
C                  FEW, SCT OR PARTIAL OBS 
              ELSEIF(((ICLDAMT.GT.1).AND.(ICLDAMT.LT.6)).OR.
     *              (ICLDAMT.EQ.9))THEN
		SDATA(J)=888.
              ENDIF
C
            ELSE
	      SDATA(J)=9999.  
	      KSTOP(J)=1
            ENDIF
C
          ENDIF
C
	  IF(KSTOP(J).EQ.1)GO TO 250
C
C            STEP 3B. PROCESS LEVELS #2 - 6.
C
	  IF(ILEVEL.GE.2)THEN
C
            IF((NINT(FD1(J)).NE.9999).AND.(NINT(FD2(J)).NE.9999))THEN
C                BKN OR OVC
	      ICLDAMT=NINT(FD1(J))
C
	      IF(ICLDAMT.GE.6)THEN
                SDATA(J)=FD2(J)
                KSTOP(J)=1
C                  FEW OR SCT
C                  888 = INSUFFICIENT CLOUD COVER FOR CEILING HT ESTIMATE
	      ELSEIF((ICLDAMT.GT.1).AND.(ICLDAMT.LT.6))THEN
		SDATA(J)=888.
              ENDIF
C
            ELSE
	      KSTOP(J)=1
	    ENDIF
C
          ENDIF
C
 250    CONTINUE
C
 300  CONTINUE
C
      GO TO 850
C
 800  DO 810 J=1,ND1
        SDATA(J)=9999.
 810  CONTINUE
C
D     WRITE(KFILDO,815) IER,(JD(J),J=1,4)
D815  FORMAT(/' ****ERROR IN OBSCIGHT, IER =',I5,' FOR VARIABLE',4I12)
C
 850  RETURN
      END     
