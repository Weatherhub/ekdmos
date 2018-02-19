      SUBROUTINE OBSNMINT(KFILDO,KFIL10,ID,IDPARS,JD,NDATE,
     1                    ITIMEZ,SDATA,ND1,NSTA,
     2                    CCALL,IPACK,IWORK,FD1,FD2,ND2X3,
     3                    LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     4                    IS0,IS1,IS2,IS4,ND7,
     5                    ISTAV,L3264B,IER)
C
C        JANUARY   1999   WEISS   TDL   MOS-2000 
C        MAY       2003   GLAHN   CHANGED ND5 TO ND2X3
C        MAY       2003   GLAHN   CHANGES TO CALL TO GFETCH AND
C                                 RESULTING ELIMINATION OF CODE; WHITE
C                                 SPACE; CHANGED NWORDS TO NSTA IN CALL
C                                 TO MAXMINB; ELSEIF TO ELSE BELOW 165;
C                                 DIAGNOSTICS
C        SEPTEMBER 2003   GLAHN   DIAGNOSTIC ON FIRST TIME THE HOUR
C                                 IS NOT 18; CHANGED ND2X3 TO ND1 IN
C                                 ALL CALLS TO GFETCH
C        OCTOBER   2003   SMB     CORRECTED FORMAT STATEMENT 340 FOR 
C                                 THE IBM
C         DECEMBER 2003   WEISS   REINSERTED INITIALIZE STATEMENT
C                                 FOR ARRAY HRLY
C
C        PURPOSE 
C            THIS SUBROUTINE WILL COMPUTE THE NIGHTTIME MINIMUM 
C            TEMPERATURE (NIGHTMIN) FROM OBSERVATION VECTOR DATA.
C            INPUT DATA AVAILABLE PRIOR TO DECEMBER 1, 1996 INCLUDES 
C            HOURLY OBS, AND BOTH 12 AND 24 HOUR MINIMUM TEMPERATURE.
C            FOR DECEMBER 1, 1996 AND LATER, HOURLY OBS AND 6 HOUR 
C            MINIMUM TEMPERATURE ARE THE INPUTS. DUE TO THESE INPUT
C            DATA DIFFERENCES, TWO ALGORITHMS ARE USED TO ESTIMATE
C            NIGHTMIN FOR EACH PERIOD OF TIME RESPECTIVELY. 
C            CALCULATIONS ARE DONE FOR IDPARS(7) = 0.
C
C            THE MAXIMUM TEMPERATURE WILL BE CALCULATED ONLY FOR 
C            18Z.  NO RECORD IS WRITTEN FOR ANY OTHER TIMES.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               702 011 - NIGHTTIME MINIMUM TEMPERATURE 
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
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C              ITIMEZ = TIME ZONE INDICATOR. THE NUMBER OF HOURS THE
C                       STATION IS DIFFERENT FROM UTC WHERE K=1,ND1,
C                       (INPUT).
C            SDATA(K) = DATA TO RETURN (K=1,ND1) (OUTPUT).
C                 ND1 = MINIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                       WITH (INPUT).
C                NSTA = THE NUMBER OF STATIONS OR LOCATIONS BEING
C                       DEALT WITH (INPUT).
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND
C                       IN AN INPUT DIRECTORY (K=1,ND1).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST, EXCEPT POSSIBLY
C                       CCALLD( ) AND CCALLP( ).  EQUIVALENCED TO
C                       ICALL( , , ).  (CHARACTER*8)  (INPUT/OUTPUT).
C                       NOTE: WHEN CALLED IN MAXMIN OR MAXMINB, ONLY
C                             THE FIRST COLUMN IS TRANSFERRED. THIS
C                             IS BY DEFAULT. 
C            IPACK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).
C            IWORK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).
C       FD1(M),FD2(M) = WORK ARRAYS (M=1,ND2X3). FD1 CONTAINS THE HOURLY
C                       TEMPERATURE AND FD2 CONTAINS EITHER 6, 12 OR 24
C                       MINIMUM TEMPERATURE (INPUT).
C                       NOTE: IF HOUR OF FIRST PROCESS DATE IS NOT
C                             EQUAL TO 18 THEN FD1 IS USED FOR DUMMY
C                             READS WHICH ARE NECESSARY TO INITIALIZE
C                             U201'S MASS STORAGE SYSTEM.
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
C                              MOSTORE( , ). LATER USED AS A WAY OF 
C                              DETERMINING WHETHER TO KEEP THIS 
C                              VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ), (INPUT). 
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN (INPUT).  
C             CORE(J) = THE ARRAY TO STORE OR RETIREVE THE DATA 
C                       IDENTIFIED IN LSTORE( , ) (J=1,ND10). WHEN
C                       CORE( ) IS FULL DATA ARE STORED ON DISK
C                       (OUTPUT).
C                ND10 = DIMENSION OF CORE( ), (INPUT).
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE (INPUT).  
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING  COUNT FROM THE BEGINNING OF 
C                       THE PROGRAM.  THIS COUNT IS MAINTAINED IN 
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
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN.
C			 -1 = STARTING DATE NOT 00, 06, 12, OR 18 UTC.
C                        52 = NWORDS DOES NOT EQUAL NSTA.
C                       103 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED
C                             IN THIS SUBROUTINE.
C                       SEE GFETCH FOR OTHER VALUES.  (OUTPUT)
C
C        ADDITIONAL VARIABLES
C
C           HRLY(N,K) = HOLDS THE HOURLY OBS OF EACH STATION USED FOR
C                       MIN EXTIMATES, WHERE N=1,ND1 AND K=MAXHRL
C                       (INTERNAL).
C                   I = INTEGER COUNTER (INTERNAL).
C              IBADHR = COUNTER OF NON-SYNPOTIC PROCESS DATES, DEFINED
C                       AS DATES NOT ENDING AS 00Z, 06Z 12Z, OR 18Z.
C                       (INTERNAL).
C          ID_HRLY(J) = THE 4 WORD ID OF HOURLY TEMPERATURE, WHERE
C                       J=1,4 (INTERNAL).
C        ID_06MNHR(J) = THE 4 WORD ID OF 6 HOUR MINIMUM TEMPERATURE,
C                       WHERE J=1,4 (INTERNAL).
C        ID_12MNHR(J) = THE 4 WORD ID OF 12 HOUR MINIMUM TEMPERATURE,
C                       WHERE J=1,4 (INTERNAL).
C        ID_24MNHR(J) = THE 4 WORD ID OF 24 HOUR MINIMUM TEMPERATURE,
C                       WHERE J=1,4 (INTERNAL).
C            IDUMYRR  = OFFSET USED TO INSERT ENOUGH DATA TO PROPERLY
C                       INITIALIZE U201'S DATA MASS STORAGE (INTERNAL).
C                 IHR = THE HOUR EXTRACTED FROM THE FULL CALENDER 
C                       DATE (INTERNAL). 
C              IENTER = NUMBER OF TIMES SUBROUTINE IS ENTERED
C                       (INTERNAL).
C               NOOUT = 0 UNTIL A DATE WITH HOUR NE 6 IS ENCOUNTERED,
C                       THEN SET TO 1.  (INTERNAL)
C                 IRR = OFFSET HOUR TO ACCESS DATA STORED IN U201'S
C                       DATA MASS STORAGE (INTERNAL).
C               J, JJ = INTEGER COUNTERS (INTERNAL). 
C              MAXHRL = MINIMUM NUMBER OF STORED HOURLY TEMPERATURES
C                       FROM ACROSS ALL POTENTIAL TIME ZONES (=25)
C                       (INTERNAL).
C               MISSP = PRIMARY MISSING INDICATOR. RETURNED AS ZERO
C                       WHEN DATA ARE PACKED; RETURNED BY GFETCH
C                       (INTERNAL).
C               MISSS = SECONDARY MISSING INDICATOR. RETURNED AS ZERO
C                       WHEN DATA ARE PACKED; RETURNED BY GFETCH
C                       (INTERNAL).
C                MXMN = INPUT PARAMETER FOR EITHER SUBROUTINE MAXMIN
C                       OR MAXMINB, EQUAL TO 1 FOR NIGHTMIN PROCESSING
C                       (INTERNAL). 
C              NDATE1 = CURRENT PROCESSING DATE (INTERNAL).
C       NDATE_FOUR(K) = FOUR DATES USED TO HELP ESTABLISH THE PROPER
C                       READS TO INITIALIZE U201'S MASS STORAGE, 
C                       WHERE K=1,4; RETURNED FROM DATEFOUR (INTERNAL).
C          NEWPROCESS = LOGICAL USED TO CHOOSE MIN TEMPERATURE 
C                       PROCESSING FOR 12/1/1996 AND LATER (INTERNAL).
C                   N = INTEGER COUNTER (INTERNAL). 
C               NPACK = 2 FOR TDL GRIB PACKED DATA; 1 FOR NOT PACKED.
C                       THIS IS STORED IN LSTORE(7, ); RETURNED BY
C                       GFETCH (INTERNAL). 
C                NPOS = FLAG FOR NEWPROCESS DUMY READS (INTERNAL).
C               NSLAB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE FIRST FIELD.  THIS IS THE
C                       VALUE OF NSLAB RETURNED.  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED (OUTPUT).  
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT
C                       THE RECORD HAS BEEN FETCHED. THIS IS STORED 
C                       IN LSTORE(9, ); RETURNED BY GFETCH (INTERNAL).
C              NWORDS = NUMBER OF WORDS(STATIONS); RETURNED IN
C                       FD1 OR FD2( ), BY GFETCH (INTERNAL).
C          OLDPROCESS = LOGICAL USED TO CHOOSE MIN TEMPERATURE
C                       PROCESSING PRIOR TO 12/1/1996 (INTERNAL).
C      RMIN_06HR(N,J) = 6 HOUR MINIMUM TEMPERATURES WHERE N=1,ND1
C                       AND J=1,4 (INTERNAL).
C        RMIN_12HR(N) = 12 HOUR MIN TEMPERATURE FOR EACH STATION
C                       WHERE N=1,ND1 (INTERNAL).
C        RMIN_24HR(N) = 24 HOUR MIN TEMPERATURE FOR EACH STATION
C                       WHERE N=1,ND1 (INTERNAL).
C********
C        INTERNAL SUBROUTINES
C            DATEFOUR = GENERATES FOUR DATES FOR MASS STORAGE READ 
C                       PURPOSES.
C              MINMAX = SUBROUTINE TO GENERATE NIGHTTIME MIN VALUES
C                       FROM 12/1/1996 AND LATER.
C             MINMAXB = SUBROUTINE TO GENERATE NIGHTTIME MIN VALUES
C                       PRIOR TO 12/1/1996.
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH, DATEFOUR, MAXMIN, MAXMINB
C
      IMPLICIT NONE
C 
      CHARACTER*8 CCALL(ND1,6)
C
      LOGICAL PROCESS,OLDPROCESS,NEWPROCESS
C
      INTEGER, PARAMETER :: MAXHRL=25
C
      INTEGER ID(4),IDPARS(15),JD(4)
      INTEGER ITIMEZ(ND1)
      INTEGER IPACK(ND2X3),IWORK(ND2X3)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER ID_HRLY(4),ID_06MNHR(4),ID_12MNHR(4),ID_24MNHR(4),
     1        NDATE_FOUR(4)
      INTEGER KFILDO,KFIL10,NDATE,ND1,NSTA,ND9,LITEMS,ND10,
     1        NBLOCK,NFETCH,ND7,ISTAV,L3264B,IER,NPACK,
     2        NTIMES,NWORDS,MISSP,MISSS,NDATE1,IHR,
     3        IRR,IDUMYRR,IENTER,IBADHR,NPOS,NSLAB,
     4        J,I,JJ,N,MXMN,ND2X3,NOOUT
C
      REAL SDATA(ND1)
      REAL HRLY(ND1,MAXHRL),RMIN_06HR(ND1,4),
     *     RMIN_24HR(ND1),RMIN_12HR(ND1)
C        HRLY( , ), RMIN_06HR( , ), RMIN_24HR( ), AND RMIN12HR( )
C        ARE AUTOMATIC ARRAYS.
      REAL FD1(ND2X3),FD2(ND2X3)
      REAL CORE(ND10)
      
      DATA OLDPROCESS/.FALSE./,
     1     NEWPROCESS/.FALSE./
      DATA IENTER/0/,
     1     IBADHR/0/,
     2     NOOUT/0/
      SAVE IENTER,IBADHR,OLDPROCESS,NEWPROCESS,NOOUT
C
      IER=0
      ISTAV=1
C
C        STEP 1A. VERIFY THE PROCESSING OF NIGHTTIME MINIMUM 
C                 TEMPERATURE
C
      IF(JD(1).NE.702011000) THEN
        WRITE(KFILDO,150)(JD(J),J=1,4)
 150    FORMAT(/' ****OBSNMINT WAS ENTERED WITH THE INCORRECT',
     1          ' PREDICTOR ID ',I9.9,I10.9,I10.9,I4.3)
        IER=103
        GO TO 800
      ENDIF
C
C        INFORM THE USER IS A TIME OTHER THAN 18 IS ENCOUNTERED.
C
      IF(MOD(NDATE,100).NE.18.AND.NOOUT.EQ.0)THEN
         WRITE(KFILDO,151)
 151     FORMAT(/' A TIME OTHER THAN 18 IS BEING PROCESSED IN',
     1           ' SUBROUTINE OBSNMINT.  THERE WILL BE NO OUTPUT',
     2           ' FOR ANY TIME OTHER THAN 18.')
         NOOUT=1
      ENDIF
C
C        STEP 1B. INITIALIZE DATA INPUT PARAMETERS
C
      IENTER=IENTER+1
C        INCREMENT IENTER VARIABLE BY 1.
C
      DO 155 J=1,ND1
        RMIN_12HR(J)=9999.
        RMIN_24HR(J)=9999.
C
        DO 152 JJ=1,4
          RMIN_06HR(J,JJ)=9999.
 152    CONTINUE
C
        DO 154 JJ=1,MAXHRL
          HRLY(J,JJ)=9999.
 154    CONTINUE
C
 155  CONTINUE
C
C        STEP 1C. IF NDATE DOES NOT END AT 00Z, 06Z, 12Z, OR 18Z,
C        AND IT IS THE FIRST PROCESS DATE.  OBSNMINT WILL NOT WORK,
C        AND THE SUBROUTINE WILL BE EXITED WITH IER= -1.
C
      IF(MOD(MOD(NDATE,100),6).NE.0.AND.IENTER.EQ.1) THEN
        WRITE(KFILDO,160)NDATE
 160    FORMAT(/' ****ERROR IN OBSNMINT, FIRST PROCESS DATE IS',
     1          ' ON A NON-SYNOPTIC TIME.  NDATE =',I12,
     2         /'     OBSNMINT CANNOT RUN, PLEASE READ WRITE UP.',
     3          '  ALL VALUES OF NIGHTTIME MINIMUM TEMPERATURE',
     4          ' ARE MISSING.'
     5         /'     NO RECORD IS WRITTEN FOR THIS PROCESS DATE.')
        IER=-1
        GO TO 800
      ENDIF
C
C        IF NDATE DOES NOT END AT 00Z, 06Z, 12Z, OR 18Z,
C        OBSNMINT WILL NOT (CANNOT) BE CALCULATED,
C        AND THE SUBROUTINE WILL BE EXITED WITH IER= -1.
C        THE FIRST TIME A NON-SYNOPTIC TIME IS ENCOUNTERED,
C        A MESSAGE WILL BE PRINTED TO REMIND THE USER OF THIS
C        LIMITATION.
C
      IF(MOD(MOD(NDATE,100),6).NE.0) THEN
C
        IF(IBADHR.EQ.0) THEN
          WRITE(KFILDO,165) NDATE
 165      FORMAT(/' ****IN OBSNMINT, ONE OR MORE NON-SYNOPTIC DATES',
     1            ' HAVE BEEN DETECTED.  NDATE =',I12,
     2           /'     NO RECORD IS WRITTEN FOR THIS DATE')
          IBADHR=IBADHR+1
          IER=-1
          GO TO 800
        ELSE
          IER=-1
          GO TO 800
        ENDIF
C
      ENDIF
C
C        STEP 1D. DETERMINE OLD OR NEW DATA PROCESSING
C        BASED ON THE CUTOFF DATE NOVEMBER 30, 1996 (18Z)
C
      IF(IENTER.EQ.1)THEN
C         GENERATE FOUR DATES FOR DUMY READS (IF NEEDED)
        CALL DATEFOUR(NDATE,NDATE_FOUR,24)
        IF(NDATE.LE.1996113018) OLDPROCESS=.TRUE.
        IF(NDATE.GT.1996113018) NEWPROCESS=.TRUE.
      ENDIF
C
C        NOTE: SPECIAL PROCESSING FOR START DATE
C              19961201 IS NOT NECESSARY FOR NIGHTTIME MINIMUM
C              TEMPERATURE AS IT IS FOR DAYTIME MAXIMUM
C              TEMPERATURE
C
      PROCESS=.FALSE.
      IHR=MOD(NDATE,100)
C
      IF(IHR.EQ.18)THEN
        PROCESS=.TRUE.
C        OLDPROCESS DATE ERROR MESSAGE:
c
        IF(OLDPROCESS.AND.(NDATE.GT.1996113018)) THEN
C
          WRITE(KFILDO,170) NDATE
 170      FORMAT(/' ****ATTEMPTING OLDDATA PROCESS IN',
     1            ' OBSNMINT, NDATE = ',I12,' IS > 1996113018',
     2           /'     NO RECORD IS WRITTEN FOR THIS DATE')
          IER=-1
          GO TO 800
        ENDIF
C
      ENDIF
C
C        NEWPROCESS DATE ERROR MESSAGE IS NOT NECESSARY SINCE
C        DATPRO WITHIN U201 WILL FLAG DATES LESS THAN THE
C        PRECEEDING DATE.
C        (IF(NEWPROCESS.AND.(NDATE.LE.1996113018)) THEN)
C
C
C        STEP 1E. CONSTRUCT FOUR WORD ID'S  
C        CONSTRUCT THE ARRAY FOR HOURLY AIR TEMPERATURE
C
      ID_HRLY(1)=702000000
      ID_HRLY(2)=IDPARS(7)
      ID_HRLY(3)=IDPARS(9)*1000000+IDPARS(12)
      ID_HRLY(4)=0
C
      IF(NEWPROCESS) THEN
C        CONSTRUCT THE ARRAY OF 6 HOUR MINIMUM TEMPERATURE
	ID_06MNHR(1)=702110000
        ID_06MNHR(2)=IDPARS(7)
        ID_06MNHR(3)=IDPARS(9)*1000000+IDPARS(12)
        ID_06MNHR(4)=0
      ELSEIF(OLDPROCESS)THEN
C        CONSTRUCT THE ARRAYS OF 12 AND 24 HOUR MINIMUM TEMPERATURE
        ID_12MNHR(1)=702014000
        ID_12MNHR(2)=IDPARS(7)
        ID_12MNHR(3)=IDPARS(9)*1000000+IDPARS(12)
        ID_12MNHR(4)=0
        ID_24MNHR(1)=702015000
        ID_24MNHR(2)=IDPARS(7)
        ID_24MNHR(3)=IDPARS(9)*1000000+IDPARS(12)
        ID_24MNHR(4)=0
      ENDIF
C
C        ****  MAIN IF BLOCK FOR NIGHTMIN ESTIMATES  ****
C        ****  IF HOUR = 18Z  ****
C
      IF(PROCESS)THEN
C
        DO 500 I=1,MAXHRL
          IRR=MAXHRL-I
          CALL UPDAT(NDATE,-IRR,NDATE1)
          IHR=MOD(NDATE1,100)
C
C           STEP 2A. FILL IN THE ARRAY HRLY
C                    REGARDLESS OF OLDPROCESS OR NEWPROCESS.
C           1. FETCH THE HOURLY SURFACE TEMPERATURE (ID_HRLY) 
C
          CALL GFETCH(KFILDO,KFIL10,ID_HRLY,7777,LSTORE,ND9,LITEMS,
     1                IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,HRLY(1,I),ND1,
     2                NWORDS,NPACK,NDATE1,NTIMES,CORE,ND10,
     3                NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,0,
     4                IER)
C
          IF((IENTER.EQ.1).AND.(IER.NE.0)) THEN
            WRITE(KFILDO,195) NDATE1,(JD(J),J=1,4),IER
 195        FORMAT(/' ****ERROR IN OBSNMINT, FOR FIRST DATE,',
     1              ' HOURLY TEMPERATURE IS MISSING FOR DATE =',I12,
     2             /'     POSSIBLE DATA GAP.  ALL VALUES OF NIGHTTIME',
     3              ' MINIMUM TEMPERATURE ARE MISSING',
     4             /'     FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER =',I3,
     5             /'     YOU MAY NEED A DUMMY CCC = 799 RECORD',
     6              ' WITH AN RR = 24 AND DD = 00.')

            GO TO 800
          ELSEIF((IENTER.GT.1).AND.((IER.NE.0).AND.(IER.NE.47))) THEN
            GO TO 800
          ENDIF
C
          IF(IER.EQ.0) THEN
C
            IF(NWORDS.NE.NSTA) THEN
              WRITE(KFILDO,200)NWORDS,NSTA
 200          FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1               ' NOT EQUAL TO NSTA =',I6,' FOR HOURLY',
     2               ' TEMPERATURE IN OBSNMINT.  DATA SET TO MISSING.')
              IER=52
              GO TO 800
            ENDIF
C
          ENDIF
C
C           STEP 2B. FILL IN THE ARRAYS RMIN_12HR AND RMIN_24HR
C                    IF OLDPROCESS
C           2. FETCH THE 12HR AND 24HR  MIN TEMPERATURES
C              (ID_12MNHR AND ID24_MNHR) IF OLDPROCESS
C
          IF(OLDPROCESS) THEN
C
C              12HR (NORMAL PROCESSING AND SPECIAL TEST).
C
            IF(IHR.EQ.12) THEN 
              CALL GFETCH(KFILDO,KFIL10,ID_12MNHR,7777,LSTORE,ND9,
     1                    LITEMS,IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,
     2                    RMIN_12HR,ND1,
     2                    NWORDS,NPACK,NDATE1,NTIMES,CORE,ND10,
     3                    NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,0,
     4                    IER)
C
              IF((IENTER.EQ.1).AND.(IER.NE.0)) THEN
                WRITE(KFILDO,290) NDATE1,(JD(J),J=1,4),IER
 290            FORMAT(/' ****ERROR IN OBSNMINT, FOR FIRST DATE,',
     1                  ' 12-HOUR TEMPERATURE IS MISSING FOR DATE=',I12,
     2                 /'     POSSIBLE DATA GAP.  ALL VALUES OF',
     3                  ' NIGHTTIME MIN TEMPERATURE ARE MISSING',
     4             /'     FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER =',I3,
     5             /'     YOU MAY NEED A DUMMY CCC = 799 RECORD',
     6              ' WITH AN RR = 24 AND DD = 00.')

                GO TO 800
              ELSEIF((IENTER.GT.1).AND.((IER.NE.0).AND.(IER.NE.47)))THEN
                GO TO 800
              ENDIF
C
              IF(IER.EQ.0) THEN
                IF(NWORDS.NE.NSTA) THEN
                  WRITE(KFILDO,300)NWORDS,NSTA
 300              FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1                     ' NOT EQUAL TO NSTA =',I6,' FOR 12 HOUR',
     2                     ' TEMPERATURE IN OBSNMINT.  DATA SET TO',
     3                     ' MISSING.')
                  IER=52
                  GO TO 800
                ENDIF
C
              ENDIF
C
C              24HR (NORMAL PROCESSING AND SPECIAL TEST)
C
            ELSEIF((IHR.EQ.18).AND.(I.GT.1)) THEN
              CALL GFETCH(KFILDO,KFIL10,ID_24MNHR,7777,LSTORE,ND9,
     1           LITEMS,IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,RMIN_24HR,ND1,
     2           NWORDS,NPACK,NDATE1,NTIMES,CORE,ND10,
     3           NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,0,
     4           IER)
C
              IF((IENTER.EQ.1).AND.(IER.NE.0)) THEN
                WRITE(KFILDO,340) NDATE1,(JD(J),J=1,4),IER
 340            FORMAT(/' ****ERROR IN OBSNMINT, FOR FIRST DATE.  24',
     1                  ' HOUR TEMPERATURE IS MISSING FOR DATE =',I12,
     2                 /'     POSSIBLE DATA GAP.  ALL VALUES OF',
     3                  ' NIGHTTIME MIN TEMPERATURE ARE MISSING',
     4                 /'     FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER ='
     6                 ,' WITH AN RR = 24 AND DD = 00.')
                GO TO 800
              ELSEIF((IENTER.GT.1).AND.((IER.NE.0).AND.(IER.NE.47)))THEN
                GO TO 800
              ENDIF
C
              IF(IER.EQ.0) THEN
c
                IF(NWORDS.NE.NSTA) THEN
                  WRITE(KFILDO,350)NWORDS,NSTA
 350              FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1                     ' NOT EQUAL TO NSTA =',I6,' FOR 24 HOUR',
     2                     ' TEMPERATURE IN OBSNMINT.  DATA SET TO',
     3                     ' MISSING.')
                  IER=52
                  GO TO 800
                ENDIF
C
              ENDIF
C
            ENDIF
C
C             STEP 2C. FILL IN THE ARRAYS RMIN_06HR
C                      IF NEWPROCESS
C
          ELSEIF(NEWPROCESS) THEN
            NPOS=0
C
C              06HR (NORMAL PROCESSING) 
c
            IF(IHR.EQ.0)  NPOS=1
            IF(IHR.EQ.6)  NPOS=2
            IF(IHR.EQ.12) NPOS=3
            IF((IHR.EQ.18).AND.(I.GT.1)) NPOS=4
C
            IF(NPOS.GE.1) THEN
              CALL GFETCH(KFILDO,KFIL10,ID_06MNHR,7777,LSTORE,ND9,
     1                 LITEMS,IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,
     2                 RMIN_06HR(1,NPOS),ND1,
     2                 NWORDS,NPACK,NDATE1,NTIMES,CORE,ND10,
     3                 NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,0,
     4                 IER)
C
              IF((IENTER.EQ.1).AND.(IER.NE.0)) THEN
                WRITE(KFILDO,400) NDATE1,(JD(J),J=1,4),IER
 400            FORMAT(/' ****ERROR IN OBSNMINT, FOR FIRST DATE.  6',
     1                  '-HOUR TEMPERATURE IS MISSING FOR DATE =',I12,
     2                 /'     POSSIBLE DATA GAP.  ALL VALUES OF',
     3                  ' NIGHTTIME MIN TEMPERATURE ARE MISSING',
     4                 /'     FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER ='
     5              ,I3/'     YOU MAY NEED A DUMMY CCC = 799 RECORD',
     6                  ' WITH AN RR = 24 AND DD = 00.')
                GO TO 800
              ELSEIF((IENTER.GT.1).AND.((IER.NE.0).AND.(IER.NE.47)))THEN
                GO TO 800
              ENDIF
C
              IF(IER.EQ.0) THEN
C
                IF(NWORDS.NE.NSTA) THEN
                  WRITE(KFILDO,410)NWORDS,NSTA
 410              FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1                     ' NOT EQUAL TO NSTA =',I6,' FOR 6 HOUR',
     2                     ' TEMPERATURE IN OBSNMINT.  DATA SET TO',
     3                     ' MISSING.')
                  IER=52
                  GO TO 800
                ENDIF
C
              ENDIF
C
            ENDIF
C
          ENDIF
C
 500    CONTINUE
C
C         STEP 3. CALCULATE THE NIGHTTIME MINIMUM TEMPERATURE
C                 BY CALLING EITHER SUBROUTINE MAXMIN OR MAXMINB.
        MXMN=1
C
        IF(NEWPROCESS) THEN
          CALL MAXMIN(KFILDO,HRLY,RMIN_06HR,ITIMEZ,CCALL,SDATA,
     *                ND1,4,MAXHRL,MXMN,0,NSTA)
        ELSEIF(OLDPROCESS) THEN
          CALL MAXMINB(KFILDO,HRLY,RMIN_24HR,RMIN_12HR,ITIMEZ,CCALL,
     *                 SDATA,ND1,NDATE,MAXHRL,MXMN,NSTA)
        ENDIF
C
C        STEP 4. IF THE HOUR IS NOT 18Z AND THE SUBROUTINE IS 
C                BEING CALLED FOR THE FIRST PROCESS DATE
C                (REMEMBER, THE HOUR MUST BE 00Z, 06Z, 12Z, 
C                OR 18Z), "DUMY" READS OF INPUT PARAMETERS
C                ARE CONDUCTED TO INITIALIZE U201'S MASS
C                STORAGE FOR THE PROPER STORAGE OF DATA.
C                NOTE: A LOOKBACK OF ATLEAST 24 HOURS MUST BE
C                      IMPLEMENTED.
C
C         STEP 4A. FIRST PROCESS DATE ONLY
C
      ELSE
C
        IF(IENTER.EQ.1)THEN
C           1. HOURLY READ (25 TIMES) ON ONLY THE FIRST ENTRY.
C             THIS IS NECESSARY TO SET THE LOOKBACK FEATURE.
C
          DO 600 N=1,MAXHRL
            IDUMYRR=MAXHRL-N
            CALL UPDAT(NDATE,-IDUMYRR,NDATE1)
            CALL GFETCH(KFILDO,KFIL10,ID_HRLY,7777,LSTORE,ND9,LITEMS,
     1                  IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND1,
     2                  NWORDS,NPACK,NDATE1,NTIMES,CORE,ND10,
     3                  NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,0,
     4                  IER)
C
            IF(IER.NE.0) THEN
              WRITE(KFILDO,590) NDATE1,(JD(J),J=1,4),IER
 590          FORMAT(/' ****ERROR IN OBSNMINT ON READ',
     1                ' OF HOURLY TEMPERATURES FOR FIRST DATE =',I12,
     2               /'     POSSIBLE DATA GAP.  ALL VALUES OF',
     3                ' NIGHTTIME MIN TEMPERATURE ARE MISSING',
     4               /'     FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER ='
     5            ,I3/'     YOU MAY NEED A DUMMY CCC = 799 RECORD',
     6                ' WITH AN RR = 24 AND DD = 00.')
              GO TO 800
            ENDIF
C
            IF(IER.EQ.0) THEN
C
	      IF(NWORDS.NE.NSTA) THEN
                WRITE(KFILDO,595)NWORDS,NSTA
 595            FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1                   ' NOT EQUAL TO NSTA =',I6,' FOR DUMY HOURLY',
     2                   ' TEMPERATURE READ IN OBSNMINT. DATA SET',
     3                   ' TO MISSING.')
                IER=52
                GO TO 800
              ENDIF
C
            ENDIF
C
 600      CONTINUE
C
C           2. 24 HOUR (18Z)
C
          IF(OLDPROCESS)THEN
C
            DO 610 N=1,4
              IF(MOD(NDATE_FOUR(N),100).EQ.18) NDATE1=NDATE_FOUR(N)
 610        CONTINUE
C
            CALL GFETCH(KFILDO,KFIL10,ID_24MNHR,7777,LSTORE,ND9,LITEMS,
     1                  IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND1,
     2                  NWORDS,NPACK,NDATE1,NTIMES,CORE,ND10,
     3                  NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,0,
     4                  IER)
C
            IF(IER.NE.0) THEN
              WRITE(KFILDO,620) NDATE1,(JD(J),J=1,4),IER
 620          FORMAT(/' ****ERROR IN OBSNMINT ON READ',
     1                ' OF 24-HOUR TEMPERATURES FOR FIRST DATE =',I12,
     2               /'     POSSIBLE DATA GAP.  ALL VALUES OF',
     3                ' NIGHTTIME MIN TEMPERATURE ARE MISSING',
     4               /'     FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER ='
     5            ,I3/'     YOU MAY NEED A DUMMY CCC = 799 RECORD',
     6                ' WITH AN RR = 24 AND DD = 00.')
              GO TO 800
            ENDIF
C
            IF(IER.EQ.0) THEN
C
              IF(NWORDS.NE.NSTA) THEN
                WRITE(KFILDO,621)NWORDS,NSTA
 621            FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1                   ' NOT EQUAL TO NSTA =',I6,' FOR DUMY 24 HOUR',
     2                   ' TEMPERATURE IN OBSNMINT.  DATA SET TO',
     3                   ' MISSING.')
                IER=52
                GO TO 800
              ENDIF
C
            ENDIF
C
C             3. 12 HOUR (12Z)
C
            DO 625 N=1,4
              IF(MOD(NDATE_FOUR(N),100).EQ.12) NDATE1=NDATE_FOUR(N)
 625        CONTINUE
C
            CALL GFETCH(KFILDO,KFIL10,ID_12MNHR,7777,LSTORE,ND9,LITEMS,
     1                  IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND1,
     2                  NWORDS,NPACK,NDATE1,NTIMES,CORE,ND10,
     3                  NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,0,
     4                  IER)
C
            IF(IER.NE.0) THEN
              WRITE(KFILDO,635) NDATE1,(JD(J),J=1,4),IER
 635          FORMAT(/' ****ERROR IN OBSNMINT ON READ',
     1                ' OF 12-HOUR TEMPERATURES FOR FIRST DATE =',I12,
     2               /'     POSSIBLE DATA GAP.  ALL VALUES OF',
     3                ' NIGHTTIME MIN TEMPERATURE ARE MISSING',
     4               /'     FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER ='
     5            ,I3/'     YOU MAY NEED A DUMMY CCC = 799 RECORD',
     6                ' WITH AN RR = 24 AND DD = 00.')
              GO TO 800
            ENDIF
C
            IF(IER.EQ.0) THEN 
C
              IF(NWORDS.NE.NSTA) THEN
                WRITE(KFILDO,640)NWORDS,NSTA
 640            FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1                   ' NOT EQUAL TO NSTA =',I6,' FOR DUMY 12 HOUR',
     2                   ' TEMPERATURE IN OBSNMINT.  DATA SET TO',
     3                   ' MISSING.')
                IER=52
                GO TO 800
              ENDIF
C
            ENDIF
C
C            4. 06 HOUR
C
          ELSEIF(NEWPROCESS)THEN
C
            DO 670 N=1,4
              NDATE1=NDATE_FOUR(N)
              CALL GFETCH(KFILDO,KFIL10,ID_06MNHR,7777,LSTORE,ND9,
     1                 LITEMS,IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND1,
     2                 NWORDS,NPACK,NDATE1,NTIMES,CORE,ND10,
     3                 NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,0,
     4                 IER)
C
              IF(IER.NE.0) THEN
                WRITE(KFILDO,660) NDATE1,(JD(J),J=1,4),IER
 660            FORMAT(/' ****ERROR IN OBSNMINT ON READ',
     1                  ' OF 6-HOUR TEMPERATURES FOR FIRST DATE =',I12,
     2                 /'     POSSIBLE DATA GAP.  ALL VALUES OF',
     3                  ' NIGHTTIME MIN TEMPERATURE ARE MISSING',
     4                 /'     FOR VARIABLE ',I9.9,2I10.9,I4.3,'.  IER ='
     5              ,I3/'     YOU MAY NEED A DUMMY CCC = 799 RECORD',
     6                  ' WITH AN RR = 24 AND DD = 00.')
                GO TO 800
              ENDIF
C
              IF(IER.EQ.0) THEN
C
                IF(NWORDS.NE.NSTA) THEN
                  WRITE(KFILDO,665)NWORDS,NSTA
 665              FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1                     ' NOT EQUAL TO NSTA =',I6,' FOR DUMY 6 HOUR',
     2                     ' AIR TEMPERATURE IN OBSNMINT.  DATA SET TO',
     3                     ' MISSING.')
                  IER=52
                  GO TO 800
                ENDIF
C
              ENDIF
C
 670        CONTINUE
C
          ENDIF
C
        ENDIF
        IER=-1
      ENDIF
C
      GO TO 850
C
 800  DO 810 J=1,ND1
        SDATA(J)=9999.
 810  CONTINUE
C
D     WRITE(KFILDO,815)IER,(JD(J),J=1,4)
D815  FORMAT(/,' ****ERROR ON OBSNMINT, IER=',I5,' FOR VARIABLE',
D    *       4I12)
 850  RETURN
      END
