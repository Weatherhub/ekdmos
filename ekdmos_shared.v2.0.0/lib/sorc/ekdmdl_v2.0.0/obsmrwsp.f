      SUBROUTINE OBSMRWSP(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                    CCALL,SDATA,ND1,NSTA,IPACK,IWORK,FD1,ND2X3,
     2                    LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                    IS0,IS1,IS2,IS4,ND7,
     4                    ISTAV,L3264B,IER)
C
C        JANUARY  2002   ERICKSON  TDL   MOS-2000
C                                  STARTED WITH OBSMRCLD 
C        AUGUST   2002   ERICKSON  REMOVED EXTRA PRINT STATEMENTS
C                                  ADDED INITIALIZATION OF NREPORTS
C        DECEMBER 2002   WEISS     CHANGED ND5 TO ND2X3
C        APRIL    2003   GLAHN     MODIFIED LINES IN CALL;  COMMENTS;
C                                  REMOVED INITIALIZATION OF SDATA( );
C                                  MODIFIED LD(3) IN CALL TO GFETCH
C                                  TO INITIALIZE STORAGE; REMOVED TABS;
C                                  CHANGED FFF TO IFFF AND CHKHR TO 
C                                  ICHKHR; REMOVED DIAG, MM, NDATE1,
C                                  FINAL( ), FD2( ), FD3( ), AND FD4( );
C                                  WHITE SPACE
C
C        PURPOSE 
C            TO CALCULATE THE OBSERVED MAXIMUM WIND SPEED DURING
C            A 12 HOUR TIME PERIOD.  THIS SUBROUTINE CALLS GFETCH
C            TO GET OBSERVED HOURLY VALUES OF WIND (704210).
C            EACH OF THE 13 OBSERVATIONS ARE CHECKED, AND THE 
C            GREATEST SPEED IS PRESERVED.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C                704 420 - MAXIMUM WIND SPEED OVER A 12-HR PERIOD
C
C            NOTE:  SINCE DATA OVER THE PAST 12 HOURS ARE NEEDED,
C            IT WILL PROBABLY BE NECESSARY TO INCLUDE
C            799000000 000000000 012000000 TO ACTIVATE THE LOOKBACK
C            FEATURE.
C           
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
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND
C                       IN AN INPUT DIRECTORY (K=1,ND1).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST, EXCEPT POSSIBLY
C                       CCALLD( ) AND CCALLP( ).  EQUIVALENCED TO
C                       ICALL( , , ).  NOT ACTUALLY USED.
C                       (CHARACTER*8)  (INPUT)
C            SDATA(K) = DATA TO RETURN (K=1,NSTA). (OUTPUT) 
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  (INPUT).
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT 
C                       WITH.  (INPUT).
C            IPACK(J) = WORK ARRAY (J=1,ND2X3) USED BY OBSMRWSP 
C                       (INTERNAL).
C            IWORK(J) = WORK ARRAY (J=1,ND2X3) USED BY OBSMRWSP 
C                       (INTERNAL).  
C              FD1(J) = WORK ARRAY (J=1,ND2X3) USED BY OBSMRWSP 
C                       (INTERNAL).
C                 ND5 = FORMER DIMENSION OF IPACK( ),IWORK( ),FDPCPN( ), 
C                       AND FDFLAG( ).(INPUT).
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
C                ND10 = DIMENSION OF CORE( ), (INPUT).
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
C               ISTAV = 1 WHEN THE DATA RETURNED ARE STATION DATA
C                       0 THE DATA RETURNED ARE GRID DATA OR
C                       DATA ARE NOT AVAILABLE FOR RETURN. 
C                       (OUTPUT).
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64) (INPUT).
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C			 -1 = STARTING DATE NOT 00 OR 12 UTC.
C                        47 = DATA NOT FOUND IN GFETCH BY OBSMRWSP.
C                        52 = NWORDS DOES NOT EQUAL NSTA.
C                       103 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED
C                             IN THIS SUBROUTINE.
C                       SEE GFETCH FOR OTHER VALUES WHEN IER.NE.0 AND
C                       DATA ARE RETURNED AS MISSING (INTERNAL-OUTPUT)
C
C      ADDITIONAL VARIABLES
C              ICHKHR = VARIABLE THAT HOLDS THE HOUR OF THE FIRST DATE
C                       READ IN BY U201. (INTERNAL)
C                IFFF = FFF PORTION OF THE FIRST WORD ID FOR THE
C                       HOURLY OBSERVED WIND SPEED. (INTERNAL)
C               I,J,K = LOOP COUNTERS. (INTERNAL)
C              IBADHR = THE NUMBER OF TIMES A NON 00/12 HR IS USED IN DATE.
C              IENTER = THE NUMBER OF TIMES THIS SUBROUTINE IS ENTERED.
C                       (INTERNAL)
C         JIDPARS(15) = HOLDS THE IDS FOR THE CALL TO OBSMRWSP. (INTERNAL).
C               LD(J) = HOLDS THE 4 WORD ID TO BE PASSED INTO OBSMRWSP
C			TO GET OBSERVED WIND SPEED.  J=1,4 (INTERNAL).
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED. 
C                       (INTERNAL, RETURNED FROM GFETCH)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED.  
C                       (INTERNAL, RETURNED FROM GFETCH)
C               NPACK = 2 FOR TDL GRIB PACKED DATA; 1 FOR NOT PACKED.
C                       THIS IS STORED IN LSTORE(7, ). 
C                       (INTERNAL, RETURNED FROM GFETCH)
C               NSLAB = FOR U201, THE NUMBER OF THE SLAB IN DIR( , , ) AND
C                       IN NGRIDC( , ) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  FOR OTHER ROUTINES, THIS NUMBER
C                       MAY MEAN SOMETHING ELSE.  FOR INSTANCE, IN U600
C                       IT IS THE "MODEL NUMBER" OR SOURCE OF THE DATA.
C                       SEE LSTORE(10, ). (INTERNAL, RETURNED FROM GFETCH)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT THE
C                       DATA HAVE BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ).  (INTERNAL, RETURNED FROM GFETCH)
C              NWORDS = NUMBER OF WORDS RETURNED IN FD1( ).  THIS IS
C                       STORED IN LSTORE(6, ) (ONLY) WHEN THE DATA
C                       ARE STORED UNPACKED.  MUST BE LE NDX.
C                       FOR UNPACKED DATA, NWORDS COMES FROM LSTORE(6, );
C                       FOR PACKED DATA, NWORDS COMES FROM IS4(3).  
C                       (INTERNAL, RETURNED FROM GFETCH)
C
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH,UPDAT
C
      IMPLICIT NONE
C
      CHARACTER*8 CCALL(ND1,6)
C
      INTEGER JD(4),IDPARS(15),LD(4)
      INTEGER NREPORTS(ND1)
C        NREPORTS( ) IS AN AUTOMATIC ARRAY.
      INTEGER IPACK(ND2X3),IWORK(ND2X3)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER IER,ISTAV,KFIL10,KFILDO,IENTER,
     +        L3264B,LITEMS,NBLOCK,ND1,ND2X3,ND7,
     +        ND9,ND10,NDATE,NFETCH,NSTA,
     +	      I,J,K,ICHKHR,IFFF,NWORDS,IBADHR,
     +        NPACK,NTIMES,NSLAB,MISSP,MISSS
C
      REAL SDATA(ND1)
      REAL FD1(ND2X3)
      REAL CORE(ND10)
C
      DATA IENTER/0/
      DATA IBADHR/0/
      DATA IFFF/210/
C	IFFF IS FOR THE OBSERVED WIND SPEED (HOURLY).
C
      IER=0
      ISTAV=1
C
C        INITIALIZE WIND COUNTER VARIABLE.
C
      DO 100 J=1,ND1
         NREPORTS(J)=0
         SDATA(J)=-9999.
 100  CONTINUE
C
C        CHECK IF IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED
C        IN THIS SUBROUTINE.
C
      IF(IDPARS(1).NE.704.OR.IDPARS(2).NE.420) THEN
         IER=103
         WRITE(KFILDO,110)(JD(J),J=1,4),IER
 110     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE',
     +           ' 12 HOUR MAXIMUM WIND SPEED',
     +          /'     PREDICTAND ',I9.9,I10.9,I10.9,I4.3,' NOT',
     +           ' ACCOMMODATED IN OBSMRWSP.  IER=',I3)
         GO TO 910
      ENDIF
C
C	INCREMENT IENTER.
C
      IENTER=IENTER+1
C
C        CHECK FOR 00Z OR 12Z DATES.  IF THE FIRST DATE IS NOT 00Z
C        OR 12Z, THEN IER = -1 AND CALUCLATION IS NOT DONE.
C
      IF (IENTER.EQ.1) THEN
         ICHKHR=(MOD(MOD(NDATE,100),12))
         IF(ICHKHR.NE.0.AND.ICHKHR.NE.12) THEN
            IER = -1
            WRITE(KFILDO,120) NDATE
 120        FORMAT(/' ****ERROR IN OBSMRWSP AT 120.  BEGINNING DATE ',
     1              'MUST BE 00Z OR 12Z AND THE START DATE IS ',I12/
     2              '     ALL VALUES OF OBSMRWSP SET TO MISSING.')
            GOTO 910
         ENDIF
      ENDIF
C
C        IF NDATE DOES NOT END AT 00Z, OR 12Z, OBSMRWSP WILL NOT BE 
C        CALCULATED, AND THE SUBROUTINE WILL BE EXITED WITH IER= -1.
C        THE FIRST TIME A NON-SYNOPTIC TIME IS ENCOUNTERED,
C        A MESSAGE WILL BE PRINTED TO REMIND THE USER OF THIS
C        LIMITATION.
C
      IF(MOD(MOD(NDATE,100),12).NE.0) THEN
C
         IF(IBADHR.EQ.0) THEN
            WRITE(KFILDO,125) NDATE
 125        FORMAT(/,' ****REMINDER IN OBSMRWSP; ONE OR MORE',
     1               ' NON-SYNOPTIC PROCESS DATES HAVE BEEN DETECTED.',
     2               ' NDATE =',I12,/
     3               '     NO PHYSICAL RECORD',
     4               ' IS WRITTEN FOR THIS PROCESS DATE.')
	    IBADHR=IBADHR+1
            IER=-1
            GO TO 900
         ELSEIF(IBADHR.GT.0) THEN
            IER=-1
            GO TO 900
         ENDIF
C
      ENDIF
C
      DO 600 K=1,13
C
C        SET UP THE LD ARRAY TO HOLD THE OBS WIND SPEED ID 
C        THEN CALL GFETCH TO GET HOURLY WIND SPEED OBS AND STORE
C        THEM IN DATA.
C
       LD(1)=IDPARS(1)*1000000+(IFFF*1000)+IDPARS(4)
       LD(2)=IDPARS(7)
       LD(3)=(IDPARS(9)+K-1)*1000000+IDPARS(12)
       LD(4)=0
       CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     +             IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     +             NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     +             NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     +             IER)
C
       IF(IER.NE.0.AND.IENTER.EQ.1) THEN
         WRITE(KFILDO,150)NDATE,IER
 150     FORMAT(/' **** ERROR FROM OBSMRWSP OCCURRED ON',
     1           ' FIRST PROCESS DATE =',I12,' IER =',I5,'.',
     2           ' OBSMRWSP CAN NOT RUN.  PLEASE READ WRITE UP.',
     3          /'     YOU MAY NEED A DUMMY CCC = 799 RECORD',
     4           ' WITH AN RR = 12.')
         GO TO 900
       ENDIF
C
       IF(IER.EQ.47) THEN
         WRITE(KFILDO,175) NDATE,IER
 175     FORMAT(' ****DATA NOT FOUND BY GFETCH IN OBSMRWSP ',
     +          'FOR ',I10,'.  IER=',I3,'.')
         CYCLE
       ENDIF
C
       IF(NWORDS.NE.NSTA) THEN
          IER=52
          WRITE(KFILDO,200) NWORDS,NSTA,IER
 200      FORMAT(/,' ****NWORDS=',I6,'  RETURNED FROM GFETCH',
     +             ' NOT EQUAL TO NSTA =',I6,
     +             ' IN OBSMRWSP.  DATA SET TO MISSING,',
     +             ' IER =',I3)
          GO TO 900
       ENDIF
C
C	  KEEP TRACK OF HOW MANY REPORTS THERE ARE FOR EACH
C	  STATION.  CHECK TO SEE IF REPORTED SPEED IS .GT.
C         SPEED OF RECORD.
C
       DO 400 J=1,NSTA
C
	  IF(NINT(FD1(J)).NE.9999) THEN
             NREPORTS(J)=NREPORTS(J)+1
C
             IF(FD1(J).GT.SDATA(J)) THEN
	        SDATA(J)=FD1(J)
             ENDIF
C
	  ENDIF
C
 400  CONTINUE
C
 600  CONTINUE 
C
      DO 800 J=1,NSTA
C
C	 WE MUST HAVE AT LEAST 7 OBSERVATIONS TO HAVE A CASE.
C
       IF(NREPORTS(J).LT.6.4) THEN
         SDATA(J)=9999.
       ENDIF
C
C        REPLACE -9999. WITH REGULAR MISSING
C   
       IF(SDATA(J).LT.-0.2) THEN
         SDATA(J)=9999.
       ENDIF       
C
 800  CONTINUE
C
      GO TO 910
C
 900  DO I=1,ND1
         SDATA(I)=9999.
      END DO
C
 910  RETURN
      END 
