      SUBROUTINE OBSMRCLD(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                    CCALL,STALAT,STALON,ISDATA,SDATA,ND1,NSTA,
     2                    IPACK,IWORK,FD1,FD2,FD3,OBS,WGTSUM,ND2X3,
     3                    LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     4                    IS0,IS1,IS2,IS4,ND7,
     5                    ISTAV,L3264B,IER)
C
C        JUNE      2000   CARROLL  TDL   MOS-2000
C        JULY      2000   CARROLL  SEVERAL MODIFICATIONS BASED ON TESTING.
C                                  ADDED THE USE OF WEIGHTS, BREAKPOINTS,
C                                  AND COEFFICIENTS.
C        SEPTEMBER 2000   CARROLL  MODIFIED TO MAKE SDATA(J) A NUMBER
C                                  BETWEEN 0 AND 1 CORRESPONDING TO THE
C                                  PERCENT OF CLOUD COVER DURING THE 12 
C                                  HOUR PERIOD INSTEAD OF A CATEGORY.
C        DECEMBER  2002   WEISS    CHANGED ND5 TO ND2X3
C        APRIL     2003   GLAHN    REMOVED COMMENT FOR ND5; SPELL CHECK;
C                                  REMOVED TABS; INDENTION
C        MAY       2003   CARROLL  FIXED ERROR ADDING STALAT AND STALON IN
C                                  CALL; CLEANED UP UNNECESSARY ERROR CHECKS
C        MAY       2003   GLAHN    REARRANGED LINES IN CALL; REARRANGED
C                                  ARGUMENTS IN CALL TO OBSTCLD;
C                                  COMMENTS; RESTRUCTURED CODE IN MAIN
C                                  LOOPS, CHANGED FFF TO IFFF, CHKHR TO 
C                                  ICHKHR, ELIMINATED DIAG; COMMENTS;
C                                  REARRANGED TYPE STATEMENTS
C        JUNE      2003   GLAHN    ADDED ISDATA TO CALL FOR OBSTCLD;
C                                  ADDED IWRITE; MODIFIED TO CHECK FOR
C                                  00 AND 12Z FOR EACH ENTRY, NOT JUST
C                                  THE FIRST; ELIMINATED NCLR( ), NFEW( ),
C                                  NSCT( ), NBKN( ), NOVC( ), NMSG( ),
C                                  NMIX( )
C        NOVEMBER  2004   SMB      RESTRUCTURED CODE TO FUNCTION AS DESCRIBED
C                                  BEFORE THE JUNE 2003.  CORRECTED SPELLING
C                                  ERRORS
C        JANUARY   2005   ANTOLIK  CHANGED DIMENSIONS OF OBS, WGTSUM TO ND2X3.
C
C        PURPOSE 
C	     TO CALCULATE THE OBSERVED PREVAILING CLOUD AMOUNT COVERING
C            12 HOUR TIME PERIOD.  THIS SUBROUTINE CALLS OBSTCLD
C            TO GET OBSERVED HOURLY VALUES OF TOTAL CLOUD:
C               0 = CLEAR
C               2 = FEW
C               3 = SCATTERED
C               6 = BROKEN
C               8 = OVERCAST
C              10 = TOTALLY OBSCURED
C            EACH OF THE 13 OBSERVATIONS ARE MULTIPLIED BY A WEIGHT
C            BETWEEN 0 AND 1 CORRESPONDING TO THE CLOUD COVER AMOUNT.
C            THE FOLLOWING WEIGHTS ARE ASSIGNED:
C               CLR = 0.00
C               FEW = 0.15
C               SCT = 0.38
C               BKN = 0.69
C               OVC = 1.00
C               OBS = 1.00
C	     TO CALCULATE THE OBSERVED PREVAILING CLOUD AMOUNT COVERING
C            IF THE OBSERVATION IS THE FIRST OR LAST OF THE 12 HOUR
C            PERIOD, IT RECEIVES HALF THE WEIGHT, SINCE THIS
C            OBSERVATION OVERLAPS 2 CONSECUTIVE TIME PERIODS.  THE
C            WEIGHTS ARE SUMMED UP AND DIVIDED BY A MAXIMUM OF 12 
C            (11 REGULAR OBS AND 2 "HALF" OBS) TO OBTAIN THE AVERAGE 
C            PERCENT CLOUD COVER FOR THE 12 HOUR PERIOD.  THE OUTPUT 
C            CONTAINED IN SDATA(J) IS A NUMBER BETWEEN 0.0 (TOTALLY CLEAR)
C            AND 1.0 (TOTALLY CLOUDY).  BREAKPOINTS CAN BE APPLIED TO
C            THIS OUTPUT IN U600 TO DIVIDE THE PREDICTAND INTO 
C            APPROPRIATE CLOUD CATEGORIES.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C      
C               708 315 - MRF PREVAILING CLOUD 
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
C                       ICALL( , , ).  (CHARACTER*8)  (INPUT/OUTPUT)
C           STALAT(K) = LATITUDE OF STATIONS (K=1,ND1) (INPUT).
C           STALON(K) = LONGITUDE OF STATIONS (K=1,ND1) (INPUT).
C            ISDATA(K) = WORK ARRAY (K=1,NSTA).  (INTERNAL) 
C            SDATA(K) = DATA TO RETURN (K=1,NSTA).  (OUTPUT) 
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  (INPUT).
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT 
C                       WITH.  (INPUT).
C            IPACK(J) = WORK ARRAY (J=1,ND2X3) USED BY OBSTCLD 
C                       (INTERNAL).  
C            IWORK(J) = WORK ARRAY (J=1,ND2X3) USED BY OBSTCLD 
C                       (INTERNAL).  
C       FD1(J),FD2(J) = WORK ARRAYS(J=1,ND2X3) USED BY OBSTCLD 
C              FD3(J)   (INTERNAL).
C                       FD1 = STATION TYPE (EX. MANUAL VS ASOS).
C                       FD2 = THE CLOUD COVERAGE DERIVED FROM THE
C                             SATELLITE CLOUD PRODUCT (SCP).
C                       FD3 = ASOS CLOUD COVERAGE. 
C			WORK ARRAY USED IN OBSTCLD (INTERNAL).
C              OBS(K) = COUNTER TO DETERMINE THE NUMBER OF TOTAL
C                       OBSERVATIONS IN THE 12 HOUR PERIOD (K=1,NSTA).
C                       (INTERNAL)
C           WGTSUM(K) = COUNTER TO SUM THE CLOUD AMOUNT IN THE
C                       12 HOUR PERIOD (K=1,NSTA).  (INTERNAL)
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
C			 -1 = DATE NOT 00 OR 12 UTC.
C                         0 = GOOD RETURN.
C                        47 = DATA NOT FOUND IN GFETCH BY OBSTCLD.
C                       103 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED
C                             IN THIS SUBROUTINE.
C                       SEE GFETCH FOR OTHER VALUES WHEN IER.NE.0 AND
C                       DATA ARE RETURNED AS MISSING (INTERNAL-OUTPUT)
C
C      ADDITIONAL VARIABLES
C              ICHKHR = VARIABLE THAT HOLDS THE HOUR OF THE FIRST DATE
C                       READ IN BY U201. (INTERNAL)
C            COEFF(J) = COEFFICIENT FOR EACH CLOUD OBSERVATION (J=1,2)
C                       THE FIRST AND LAST OBSERVATION IN THE 12 HOUR
C                       PERIOD HAVE HALF THE WEIGHT OF THE OTHERS,
C			SINCE THEY OCCUR IN TWO TIME PERIODS. 
C			(INTERNAL).
C                IFFF = FFF PORTION OF THE FIRST WORD ID FOR THE
C                       COMPLEMENTED TOTAL CLOUD AMOUNT. (INTERNAL)
C               I,J,K = LOOP COUNTERS. (INTERNAL)
C                ICLD = INTEGER VARIABLE USED TO SELECT CLOUD AMOUNT
C                       (INTERNAL)
C              IENTER = THE NUMBER OF TIMES THIS SUBROUTINE IS ENTERED.
C                       (INTERNAL)
C              JD1(J) = HOLDS JD FOR THE CALL TO OBSTCLD (J=1,4).
C                       (INTERNAL)
C          JIDPARS(J) = HOLDS THE IDS FOR THE CALL TO OBSTCLD (J=1,15).
C                       (INTERNAL).
C               LD(J) = HOLDS THE 4 WORD ID TO BE PASSED INTO OBSTCLD
C			TO GET COMPLEMENTED TOTAL CLOUD (J=1,4).
C                       (INTERNAL).
C              NDATE1 = DATE RETURNED FROM UPDAT.  THIS ALLOWS THE CODE
C                       TO GET PRIOR/LATER OBSERVATIONS.  (INTERNAL)
C           WEIGHT(J) = WEIGHTS ASSIGNED TO EACH CLOUD CATEGORY (J=1,5).
C                       REPRESENTS FRACTIONAL COVERAGE FOR THE CLOUD
C                       CATEGORIES RETURNED BY OBSTCLD.  (INTERNAL)
C              IWRITE = 0 UNTIL THE DIAGNOSTIC AT STATEMENT 110 IS 
C                       WRITTEN ONCE, THEN INCREMENTED.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            OBSTCLD,UPDAT
C
      IMPLICIT NONE
C
      CHARACTER*8 CCALL(ND1,6)
C
      INTEGER IDPARS(15),JD(4)
      INTEGER ISDATA(ND1)
      INTEGER IPACK(ND2X3),IWORK(ND2X3)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER LD(4),JD1(4),JIDPARS(15)
      INTEGER IER,ISTAV,KFIL10,KFILDO,IENTER,
     +        L3264B,LITEMS,NBLOCK,ND1,ND2X3,ND7,
     +        ND9,ND10,NDATE,NDATE1,NFETCH,NSTA,
     +	      I,J,K,ICHKHR,IFFF,ICLD,L,M,IWRITE
C
      REAL SDATA(ND1),STALAT(ND1),STALON(ND1)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),OBS(ND2X3),WGTSUM(ND2X3)
      REAL CORE(ND10)
      REAL WEIGHT(5),COEFF(2)
      REAL NCLR(ND1),NFEW(ND1),NSCT(ND1),NBKN(ND1),NOVC(ND1),NMSG(ND1)
C
      DATA IENTER/0/
      DATA IFFF/312/
C	IFFF IS FOR THE COMPLIMENTED TOTAL CLOUD AMOUNT (HOURLY).
      DATA WEIGHT/0.0,0.15,0.38,0.69,1.0/
C        THE 5 WEIGHTS REPRESENT THE FRACTIONAL COVERAGE FOR THE CLOUD
C        VALUES 0, 2, 3, 6, AND 8 OR 10, RESPECTIVELY, AS RETURNED
C        FROM OBSTCLD.
      DATA COEFF/0.5,1.0/
      DATA IWRITE/0/
C
      IER=0
      ISTAV=1
C
C        INITIALIZE CLOUD CATEGORY COUNTER VARIABLES AND
C        PREDICTAND VALUE IN SDATA.
C
      DO 50 J=1,ND1
         NCLR(J)=0.
         NFEW(J)=0.
         NSCT(J)=0.
         NBKN(J)=0.
         NOVC(J)=0.
         NMSG(J)=0.
         OBS(J)=0.
         WGTSUM(J)=0.
  50  CONTINUE
C
C        CHECK IF IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED
C        IN THIS SUBROUTINE.
C
      IF(IDPARS(1).NE.708.OR.IDPARS(2).NE.315) THEN
         IER=103
         WRITE(KFILDO,100)(JD(J),J=1,4),IER
 100     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE',
     +           ' 12 HOUR PREVAILING TOTAL CLOUD.',
     +          /'     VARIABLE ',I9.9,I10.9,I10.9,I4.3,' NOT',
     +           ' ACCOMMODATED IN OBSMRCLD.  IER=',I3)
         GO TO 910
      ENDIF
C
C	INCREMENT IENTER.
C
      IENTER=IENTER+1
C
C       CHECK FOR 00Z OR 12Z DATES.  IF THE DATE IS NOT 00Z OR
C       12Z THEN IER = -1.
C
      ICHKHR=(MOD(MOD(NDATE,100),12))
C
      IF(ICHKHR.NE.0.AND.ICHKHR.NE.12) THEN
C
         IF(IWRITE.EQ.0)THEN
            WRITE(KFILDO,110) NDATE,(JD(J),J=1,4)
 110        FORMAT(/' ****ERROR IN OBSMRCLD AT 110.  DATE ',
     1              'MUST BE 00Z OR 12Z AND THE DATE IS ',I12,'.',
     2             /'     ALL VALUES OF VARIABLE ',
     3              I9.9,I10.9,I10.9,I4.3,' SET TO MISSING.',
     4             /'     THIS DIAGNOSTIC WILL NOT REPEAT, BUT RECORDS',
     5              ' FOR OTHER THAN 00 OR 12Z WILL NOT BE WRITTEN.')
            IWRITE=1
         ENDIF
C
         IER=-1
         GOTO 910
      ENDIF
C
      DO 600 K=1,13
C
C	    CALL UPDAT TO THE MODIFY THE DATE.  EACH TIME THROUGH
C           THE LOOP THE DATE (NDATE1) IS INCREMENTED BACKWARDS
C           BY ONE HOUR OVER THE 12 HOUR PERIOD (13 HOURS READ).
C
C	    PROBABLY WILL NEED SPECIAL ID 799000000 000000000 012000000
C           IN U201.CN FILE IN ORDER TO ACTIVATE LOOKBACK FEATURE
C           PROPERLY.  HOWEVER, NOTE THAT THE HOURLY RETRIEVALS ARE
C           NOT EXERCISED AS THEY ARE IN OBSDMAXT, SO ACTIVATION
C           STILL MAY NOT WORK.
C
      CALL UPDAT(NDATE,-(K-1),NDATE1)
C
C           SET UP THE LD, JIDPARS, AND JD1 ARRAY TO HOLD THE ID 
C	    FOR THE OBSERVED TOTAL CLOUD AMOUNT THAT WILL BE FETCHED
C           AND PROCESSED.
C
      LD(1)=IDPARS(1)*1000000+(IFFF*1000)+IDPARS(4)
      LD(2)=IDPARS(7)
      LD(3)=IDPARS(9)*1000000+IDPARS(12)
      LD(4)=0
C
      DO 130 I = 1,15
         JIDPARS(I)=IDPARS(I)
 130  CONTINUE
C
      DO 140 I = 1,4
         JD1(I)=JD(I)
 140  CONTINUE
C
      JIDPARS(2)=IFFF
      JD1(1)=IDPARS(1)*1000000+(IFFF*1000)+IDPARS(4)
C
C          CALL OBSTCLD TO GET HOURLY TOTAL CLOUD IN SDATA( ).
C
      CALL OBSTCLD(KFILDO,KFIL10,LD,JIDPARS,JD1,NDATE1,
     +             CCALL,STALAT,STALON,ISDATA,SDATA,ND1,NSTA,
     +             IPACK,IWORK,FD1,FD2,FD3,ND2X3,
     +             LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     +             IS0,IS1,IS2,IS4,ND7,
     +              ISTAV,L3264B,IER)
C
      IF(IER.NE.0.AND.IENTER.EQ.1) THEN
         WRITE(KFILDO,150)NDATE1,IER,(JD(J),J=1,4)
 150     FORMAT(/' ****ERROR FROM OBSTCLD OCCURRED ON',
     1           ' FIRST PROCESS DATE, ',I12,' IER =',I5,'.',
     2          /'     ALL VALUES OF VARIABLE ',
     3           I9.9,I10.9,I10.9,I4.3,' SET TO MISSING.',
     4          /'     YOU MAY NEED A DUMMY CCC = 799 RECORD',
     5           ' WITH AN RR = 12.')
C              NOTE THAT THIS DIAGNOSTIC WILL USUALLY FOLLOW ONE
C              IN OBSTCLD.
            GO TO 900
         ENDIF
C
C           IF HOURLY OBSERVATION IS MISSING THEN CYCLE.
C
         IF(IER.EQ.47) THEN
            WRITE(KFILDO,175) NDATE1,IER
 175        FORMAT(' ****DATA NOT FOUND BY GFETCH IN OBSTCLD ',
     +             'FOR DATE ',I10,'.  IER=',I3,'.')
            CYCLE
         ENDIF
C
         IF(IER.NE.0) GO TO 900
C
C	     SUM UP THE HOURLY WEIGHTS REPRESENTING THE FRACTIONAL
C            COVERAGE AND THE NUMBER OF OBS, WEIGHTING THE FIRST
C            AND LAST ONLY HALF A VALUE.
C
         DO 400 J=1,NSTA
            ICLD = NINT(SDATA(J))
C
            SELECT CASE(ICLD)
C
C	          CLEAR SKIES
C
               CASE(0)
                  NCLR(J)=NCLR(J)+1
                  IF(K.EQ.1.OR.K.EQ.13) THEN
                     WGTSUM(J)=WGTSUM(J)+(COEFF(1)*WEIGHT(1))
                     OBS(J)=OBS(J)+0.5
                  ELSE
                     WGTSUM(J)=WGTSUM(J)+(COEFF(2)*WEIGHT(1))
                     OBS(J)=OBS(J)+1
                  ENDIF
C
C	          FEW CLOUDS
C
               CASE(2)
                  NFEW(J)=NFEW(J)+1
                  IF(K.EQ.1.OR.K.EQ.13) THEN
                     WGTSUM(J)=WGTSUM(J)+(COEFF(1)*WEIGHT(2))
                     OBS(J)=OBS(J)+0.5
                  ELSE
                     WGTSUM(J)=WGTSUM(J)+(COEFF(2)*WEIGHT(2))
                     OBS(J)=OBS(J)+1
                  ENDIF
C
C	          SCATTERED CLOUDS
C
               CASE(3)
                  NSCT(J)=NSCT(J)+1
                  IF(K.EQ.1.OR.K.EQ.13) THEN
                     WGTSUM(J)=WGTSUM(J)+(COEFF(1)*WEIGHT(3))
                     OBS(J)=OBS(J)+0.5
                  ELSE
                     WGTSUM(J)=WGTSUM(J)+(COEFF(2)*WEIGHT(3))
                     OBS(J)=OBS(J)+1
                  ENDIF
C
C	          BROKEN
C
               CASE(6)
                  NBKN(J)=NBKN(J)+1
                  IF(K.EQ.1.OR.K.EQ.13) THEN
                     WGTSUM(J)=WGTSUM(J)+(COEFF(1)*WEIGHT(4))
                     OBS(J)=OBS(J)+0.5
                  ELSE
                     WGTSUM(J)=WGTSUM(J)+(COEFF(2)*WEIGHT(4))
                     OBS(J)=OBS(J)+1
                  ENDIF
C
C	          OVERCAST
C
               CASE(8,10)
                  NOVC(J)=NOVC(J)+1
                  IF(K.EQ.1.OR.K.EQ.13) THEN
                     WGTSUM(J)=WGTSUM(J)+(COEFF(1)*WEIGHT(5))
                     OBS(J)=OBS(J)+0.5
                  ELSE
                     WGTSUM(J)=WGTSUM(J)+(COEFF(2)*WEIGHT(5))
                     OBS(J)=OBS(J)+1
                  ENDIF
C
               CASE(9999)
                  NMSG(J)=NMSG(J)+1
C
               CASE DEFAULT
                  WRITE(KFILDO,350) ICLD
 350              FORMAT(/' ****ERROR IN OBSMRCLD AT 350.  ICLD=',I4,
     +                    '.  SHOULD EQUAL 0, 2, 3, 6, 8, 10, OR 9999.')
C
            END SELECT
C
 400       CONTINUE
C
 600  CONTINUE 
C
C	 DIVIDE THE SUM OF THE WEIGHTED OBSERVATIONS BY THE NUMBER OF
C	 OBSERVATIONS TO OBTAIN THE PERCENT OF CLOUD COVER AVERAGED OVER
C	 THE 12 HOUR PERIOD.
C
      DO 800 J=1,NSTA
C
C	   WE MUST HAVE AT LEAST 7 OBSERVATIONS TO HAVE A CASE.
C
	IF(OBS(J).LT.6.4) THEN
           SDATA(J)=9999.
           GO to 800
        ELSE
           WGTSUM(J)=(WGTSUM(J)/OBS(J))
        ENDIF
           SDATA(J)=WGTSUM(J)
C
 800  CONTINUE
      GO TO 910
C
 900  DO I=1,ND1
         SDATA(I)=9999.
      END DO
C
 910  RETURN
      END 
