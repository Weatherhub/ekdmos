      SUBROUTINE MAXMINB(KFILDO,HRLY,RMXMN_24HR,RMXMN_12HR,ITIMEZ,
     *                   CCALL1,RESFLD,ND1,NDATE,MAXHRL,MXMN,NSTA)
C
C        SEPTEMBER 1998   WEISS   TDL   MOS-2000
C                                 BASED ON CODE BY 1990 CODE BY
C                                 DAVE MILLER         
C        JANUARY   1999   WEISS   REVISED COMMENTS IN THE 24 HOUR
C                                 MAX/MIN COMPARISON SECTION (STEP 4).
C        MAY       2003   GLAHN   CHANGED NWORDS TO NSTA; WHITE SPACE;
C                                 SPELL CHECK; CHANGED 25 TO INUMZN
C                                 IN SEVERAL PLACES; REORDERED TYPE
C                                 TYPE STATEMENTS; COMMNETED AUTOMATIC
C                                 VARIABLES
C
C        PURPOSE
C            THIS SUBROUTINE COMPUTES DAYTIME/NIGHTTIME MAX/MIN
C            TEMPERATURES FOR STATION (VECTOR) DATA FOR DATES 
C            PRECEDING DECEMBER 1, 1996. INPUT INCLUDE THE 12 AND
C            24 HOUR MINIMUM OR MAXIMUM TEMPERATURE AND HOURLY
C            OBSERVATION FILES.
C
C        SEE BELOW FOR TDL STANDARDS
C
C        DATA SET USE
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                     (OUTPUT).
C
C        VARIABLES
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                       (INPUT).
C           HRLY(N,K) = HOLDS THE HOURLY OBS OF EACH STATION USED
C                       FOR MIN ESTIMATES, WHERE N=1,ND1 AND
C                       K=1,MAXHRL (INPUT).
C       RMXMN_24HR(N) = 24 HOUR MAX OR MIN TEMPERATURE FOR EACH STATION
C                       WHERE N=1,ND1 (INPUT).
C       RMXMN_12HR(N) = 12 HOUR MAX OR MIN TEMPERATURE FOR EACH STATION
C                       WHERE N=1,ND1 (INPUT).
C           ITIMEZ(N) = TIME ZONE INDICATOR BASED ON THE DIFFERENCE
C                       IN HOURS FROM UTC, WHERE N=1,ND1 (INPUT).
C                       NOTE: TIME ZONES ARE RE-REFERENCED TO
C                       EASTERN STANDARD TIME.
C           CCALL1(N) = STATION CALL LETTERS FROM CCALL(N,1), WHERE
C                       N=1,ND1.  (NOT ACTUALLY USED.)  (INPUT)
C           RESFLD(N) = THE RESULTANT FIELD (MAX OR MIN) FOR EACH
C                       STATION, WHERE N=1,ND1 (OUTPUT).
C                 ND1 = MAXIMUM NUMBER OF STATIONS (INPUT).
C               NDATE = PROCESSING DATE IN TDL FORMAT (EX. 1996120512)
C                       (INPUT). 
C              MAXHRL = MAXIMUM NUMBER OF STORED HOURLY TEMPERATURES
C                       FROM ACROSS ALL POTENTIAL TIME ZONES (=25) 
C                       (INPUT).
C                MXMN = INDICATOR OF WHETHER YOU ARE TO CALCULATE A
C                       DAYTIME MAX (=2) OR A NIGHTTIME MIN (=1) 
C                       (INPUT).
C                NSTA = NUMBER OF STATIONS THAT THE MAX OR MIN WILL BE
C                       CALCULATED FOR (INPUT).
C
C
C        ADDITIONAL VARIABLES
C                CMAX = LOGICAL PARAMETER FOR DAYTIME MAXIMUM
C                       PROCESSING (INTERNAL).
C                CMIN = LOGICAL PARAMETER FOR NIGHTTIME MINIMUM
C                       PROCESSING (INTERNAL).
C           ICSKIP(N) = OUTSIDE WINDOW MISSING VALUE FLAG FOR 24 HOUR 
C                       CHECKING. IF EQUAL TO 1, THEN PROCEED TO 12 
C                       HOUR CHECKING, WHERE N=1,ND1 (INTERNAL). 
C                 IMO = MONTH COMPONENT OF NDATE (INTERNAL).
C             IMSG(N) = THE NUMBER OF MISSING HOURLY VALUES INSIDE THE 
C                       "ESTIMATED WINDOW" (STEP 3) PER STATION, WHERE
C                       N=1,ND1 (INTERNAL).
C              INUMZN = NUMBER OF HOURS (=25) USED FOR RMXHRLY AND
C                       RMNHRLY ARRAYS (INTERNAL).
C              IPRIOD = REPRESENTS THE LENGTH OF A TIME PERIOD WITHIN 
C                       THE MAX MIN WINDOW (INTERNAL).
C           IWNCHK(N) = CHECK ARRAY WHICH INDICATES WHETHER THE
C                       MAX/MIN OCCURRED INSIDE OR OUTSIDE THE
C                       DAYTIME/NIGHTTIME WINDOW PER STATION, FOR 24
C                       HOUR CHECKING, WHERE N=1,ND1 (INTERNAL).
C           ISTART(K) = THE STARTING POSITION (HOUR) IN THE HRLY ARRAY
C                       FOR THE FIVE PERIODS USED IN CALCULATING THE
C                       MAX/MIN "ESTIMATED WINDOW" TEMPERATURE. THE 
C                       POSITIONS ARE VALID FOR EST, K=1,5 (INTERNAL).
C           ITMCOR(N) = TIME ZONE CORRECTION FOR EACH STATION. ADDED
C                       TO THE GMT HOUR SO THE PROGRAM CAN ARRIVE   
C                       AT THE CORRECT DAYTIME/NIGHTTIME HOURS, 
C                       WHERE N=1,ND1 (INTERNAL).         
C             JMSG(N) = THE NUMBER OF MISSING HOURLY VALUES OUTSIDE THE
C                       "ESTIMATE WINDOW" (STEP 4) PER STATION, WHERE 
C                       N=1,ND1 (INTERNAL).
C                KEND = ENDING HOURLY POSITION WITHIN HRLY ARRAY OF 
C                       "ESTIMATED WINDOW" PERIOD FOR EITHER MAX OR
C                       MIN (=l9/20 FOR MIN/MAX FOR 25 HOURS) 
C                       BASED ON EST (INTERNAL).
C          KFLAG(K,N) = CONTAINS HOW MANY TEMPERATURES WERE MISSING
C                       PER TIME PERIOD PER STATION, WHERE K=1,5 AND
C                       N=1,ND1 (INTERNAL).
C              KSTART = STARTING HOURLY POSITION WITHIN HRLY ARRAY OF 
C                       "ESTIMATED WINDOW" PERIOD FOR EITHER MAX OR
C                       MIN (=7 FOR 25 HOURS) BASED ON EST (INTERNAL).
C               KTIME = THE PERIOD BEING PROCESSED, 1 TO 5 (INTERNAL).
C                LEND = "ESTIMATED WINDOW" END POSITION FOR EACH  
C                       STATION BASED ON TIME ZONE (INTERNAL).
C              LSTART = "ESTIMATED WINDOW" START POSITION FOR EACH
C                       STATION BASED ON TIME ZONE (INTERNAL).
C                  NS = COUNTER OF STATIONS FOR "MAIN" DO LOOP 
C                       (INTERNAL).
C             RJTPDIF = TEMPERATURE DIFFERENCE BETWEEN THE REPORTED
C                       24 HOUR MAX/MIN AND THE HOURLY OBSERVED
C                       TEMPERATURE (INTERNAL).
C           RMNABC(J) = HOLDS THE MIN TEMPERATURES FOR SEGMENTS 
C                       A,B AND C RESPECTIVELY. SEGMENT A IS OUTSIDE 
C                       THE WINDOW AND WITHIN THE 12 HOUR TEMPERATURE 
C                       FOR EASTERN - ALEUTIAN TIME ZONES AND 
C                       WITHIN WINDOW BUT OUTSIDE 12 HOUR TEMPERATURE
C                       FOR ATLANTIC TIME ZONE. SEGMENT B ALWAYS
C                       OVERLAPS THE WIDOW AND 12 HOUR TEMPERATURE. 
C                       SEGMENT C IS WITHIN THE WINDOW  BUT OUTSIDE 
C                       12 HOUR TEMPERATURE FOR EASTERN - ALEUTIAN 
C                       TIME ZONES. FOR THE ATLANTIC TIME ZONE 
C                       SEGMENT C IS ZERO FOR MIN AND OUTSIDE WINDOW 
C                       BUT WITHIN 12 HOUR TEMPERATURE FOR MAX, 
C                       FOR A GIVEN STATION, WHERE J=1,3 (INTERNAL).
C          RMNHRLY(K) = HOLDS THE HOURLY TEMPERATURES TO GENERATE
C                       MIN TEMPERATURE SEGMENTS A,B, AND C,
C                       WHERE K=1,INUMZN (INPUT).
C      RTMP24_DIFF(N) = TEMPERATURE DIFFERENCE ARRAY USED TO KEEP       
C                       TRACK OF THE DIFFERENCE BETWEEN THE REPORTED    
C                       24 HOUR MAX/MIN AND THE OBSERVED           
C                       TEMPERATURES, WHERE N=1,ND1 (INTERNAL).
C           RMXABC(J) = HOLDS THE MAX TEMPERATURES FOR SEGMENTS
C                       A,B AND C RESPECTIVELY. SEE RMNABC FOR MORE 
C                       DETAILS, J=1,3 (INTERNAL). 
C          RMXHRLY(K) = HOLDS THE HOURLY TEMPERATURES TO GENERATE
C                       MAX TEMPERATURE SEGMENTS A,B, AND C,
C                       WHERE K=1,INUMZN (INPUT).
C           RMXMN2(N) = THE "ESTIMATED WINDOW" MAX/MIN TEMPERATURE AS
C                       DETERMINED FROM THE MAX/MIN WINDOW, WHERE
C                       N=1,ND1 (INTERNAL).
C       RMXMN_5P(N,J) = HOLDS VALUES OF TEMPERATURE FOR EACH OF THE
C                       5 TIME PERIODS FOR EACH STATION, WHERE N=1,ND1
C                       AND J=1,5 (INTERNAL).
C               ZMISS = THE FILL VALUE FOR THE RMXMN2 ARRAY: 9999 FOR
C                       MIN AND -9999 FOR MAX ESTIMATES (INTERNAL).
C                 ZZZ = MISSING VALUE OF 9999. (INTERNAL)
C
C        INTERNAL SUBROUTINES
C
C               CKABC = CHECKS THE NUMBER OF MISSING HOURLY OBSERVATIONS 
C                       FOR SEGMENTS A,B, AND C. 
C
C             CKPRIOD = DETERMINES THE MAX/MIN TEMPERATURE
C                       CORRESPONDING TO EACH PERIOD IN THE
C                       MAX/MIN WINDOW FOR EACH STATION.
C
      IMPLICIT NONE
C
      LOGICAL CMAX,CMIN
C
      CHARACTER*8 CCALL1(ND1) 
C
      INTEGER, PARAMETER :: INUMZN=25
C
      INTEGER ITIMEZ(ND1)
      INTEGER ITMCOR(ND1),KFLAG(5,ND1),IMSG(ND1),JMSG(ND1),
     *        IWNCHK(ND1),ICSKIP(ND1)
C        ITMCOR( ), KFLAG( , ), IMSG( ), JMSG( ), 
C        IWNCHK( ), AND ICSKIP( ) ARE AUTOMATIC VARIABLES.
      INTEGER ISTART(5)
      INTEGER KFILDO,ND1,NDATE,MXMN,MAXHRL,NSTA,KSTART,LSTART,LEND,KEND,
     1        IMO,MTIME,KTIME,NS,J,II,IPRIOD
C
      REAL RMXMN_12HR(ND1),RMXMN_24HR(ND1),HRLY(ND1,MAXHRL),
     *     RESFLD(ND1)
      REAL RMXMN_5P(ND1,5),RMXMN2(ND1),RTMP24_DIFF(ND1),
     1     RMXHRLY(INUMZN),RMNHRLY(INUMZN)
C        RMXMN_5P( ), RMXMN2( ), RTMP24_DIFF( ),
C        RMXHRLY( ), AND RMNHRLY( ) ARE AUTOMATIC ARRAYS.
      REAL RMXABC(3),RMNABC(3),ZZZ,ZMISS,RJTPDIF
C
      DATA ISTART/7,10,13,16,19/
C
C******************************************************
C        STEP 1. INITIALIZATION AND DECLARATIONS
C 
      CMAX=.FALSE.
      CMIN=.FALSE.
      IF(MXMN.EQ.1)CMIN=.TRUE.
      IF(MXMN.EQ.2)CMAX=.TRUE.
      ZMISS=9999.
      ZZZ=9999.
      IMO=MOD(NDATE,1000000)/10000
C
      DO 400 NS=1,ND1
C
        DO 395 KTIME=1,5
C
          IF(KTIME.EQ.1)THEN
            IMSG(NS)=0
            JMSG(NS)=0
            ICSKIP(NS)=0
            RTMP24_DIFF(NS)=50.
            IWNCHK(NS)=0
	    ITMCOR(NS)=0
	    IF(CMAX) RMXMN2(NS)=-ZMISS
            IF(CMIN) RMXMN2(NS)=ZMISS
          ENDIF
C
          KFLAG(KTIME,NS)=0
          IF(CMAX) RMXMN_5P(NS,KTIME)=-ZMISS
          IF(CMIN) RMXMN_5P(NS,KTIME)=ZMISS
 395    CONTINUE
C
 400  CONTINUE
C
C        END OF STEP 1.
C**********************
C
C        *** MAIN DO LOOP ***
C        *** FOR EACH STATION...........
C
      DO 600 NS=1,NSTA
C
C        STEP 2. CHANGE TIME ZONE REFERENCE POINT
C                 FROM GMT TO EST.
C                 EX. ITIMEZ FOR JFK IS -5, BECOMES 0
C                     ITIMEZ FOR STL IS -6, BECOMES 1 
C                     ITIMEZ FOR DEN IS -7, BECOMES 2
C                     ITIMEZ FOR LAX IS -8, BECOMES 3
C        ALASKA       ITIMEZ FOR ANC IS -9, BECOMES 4
C        ALEUTIANS    ITIMEZ        IS -10, BECOMES 5
C
C        ATLANTIC     ITIMEZ FOR YFC IS -4, BECOMES -1 
C
        ITMCOR(NS)=ABS(ITIMEZ(NS)+5)
        IF(ITIMEZ(NS).EQ.-4)ITMCOR(NS)=-1
C
        IF((ITIMEZ(NS).GE.-3).OR.(ITIMEZ(NS).LE.-11)) THEN
          WRITE(KFILDO,100) NS,ITIMEZ(NS)
 100      FORMAT(/' ****WARNING:  TIME ZONE FOR STATION # ',I5, 
     *            ' OF INPUT LIST IS OUT OF RANGE, TIME ZONE =',I4)
          GO TO 600
        ENDIF
C
C        END OF STEP 2.
C********************** 
C                                                                       
C        BEGIN MAX/MIN CALCULATIONS                                        
C                                                                       
C        STEP 3. GENERATE A VALUE OF THE "ESTIMATED WINDOW" MAX/MIN
C                FOR THE PERIOD 7PM - 8AM LST (MIN) AND 7AM - 7PM 
C                LST (MAX). THE "ESTIMATED WINDOW" TEMPERATURE
C                WILL BE ESTIMATED FROM 5 PERIODS (IPRIOD). 
C
C        STEP 3A. IPRIOD IS THE LENGTH OF THE TIME PERIOD WITHIN      
C                 THE MAX MIN WINDOW. NORMALLY EQUAL TO 3 HOURS. 
C                 HOWEVER, THE LAST PERIODS ARE ONLY 1 HOUR FOR THE
C                 MAX AND 2 HOURS FOR THE MIN. THE MAX/MIN IS
C                 FIRST ESTIMATED IN EACH OF THE FIVE PERIODS.
C                 MISSING DATA CHECKS NECESSITATED THIS APPROACH.         
C
        DO 500 KTIME=1,5
          IPRIOD=3
          IF((CMIN).AND.(KTIME.EQ.5))IPRIOD=2
          IF((CMAX).AND.(KTIME.EQ.5))IPRIOD=1
C                                                                       
C        STEP 3B. SUBROUTINE CKPRIOD DETERMINES THE MAX/MIN TEMPERATURE 
C                 CORRESPONDING TO EACH PERIOD IN THE MAX/MIN WINDOW  
C                 FOR EACH STATION. 
          CALL CKPRIOD(HRLY,ISTART,RMXMN_5P,ITMCOR,KFLAG,IMSG,
     1                 MXMN,ND1,NSTA,KTIME,MAXHRL,ZMISS,NS,IPRIOD)
 500    CONTINUE
C
C        STEP 3C. NOW WE HAVE THE 5 MAX/MIN VALUES AND UNLESS THERE
C                 WAS MISSING DATA, WE WILL NOW CALCULATE THE
C                 "ESTIMATED WINDOW" MAX/MIN TEMPERATURE FOR EACH
C                 STATION. 
C                 NOTE: IF MISSING MORE THAN 3 HOURLIES,
C                            SET EST MAX/MIN TO 9999
        IF(CMAX) THEN
C
          IF(IMSG(NS).LT.4) THEN                                 
            RMXMN2(NS)=MAX(RMXMN_5P(NS,1),RMXMN_5P(NS,2),RMXMN_5P(NS,3),
     *                     RMXMN_5P(NS,4),RMXMN_5P(NS,5)) 
          ELSE                                                       
            RMXMN2(NS)=ZZZ                                           
          ENDIF                                                      
C
        ELSEIF(CMIN)THEN                                                  
          IF(IMSG(NS).GE.4) THEN                                     
            RMXMN2(NS)=ZZZ                                             
          ELSE                                                         
            RMXMN2(NS)=MIN(RMXMN_5P(NS,1),RMXMN_5P(NS,2),RMXMN_5P(NS,3), 
     *                     RMXMN_5P(NS,4),RMXMN_5P(NS,5))
          ENDIF                                                   
C
        ENDIF
C
C        THE "ESTIMATED WINDOW" MAX OR MIN TEMPERATURE IS CONTAINED IN 
C        ARRAY RMXMN2.
C
C        STEP 3D. IF THE WINDOW TEMPERATURE IS MISSING THE MAX/MIN 
C                 ESTIMATE IS CONSIDERED TO BE MISSING AND WE GO TO THE
C                 THE NEXT STATION.
C
        IF(RMXMN2(NS).EQ.ZZZ) THEN
          RESFLD(NS)=ZZZ
          GO TO 600
        ENDIF
C
C        END OF STEP 3.
C**********************
C
C        STEP 4. *** IF THE 24 HOUR MAX/MIN IS NOT MISSING *** THEN
C                
C                CHECK IF THE 24 HOUR MAX/MIN OCCURRED WITHIN
C                THE "ESTIMATED" MAX MIN WINDOW. (THE RANGE OF 
C                HOURS FOR THE "ESTIMATED WINDOW" TEMPERATURE ARE
C                WITHIN THE 24 MAX OR MIN TEMPERATURE). 
C
C                IF THE 24 HOUR MAX/MIN WAS DETERMINED TO 
C                OCCUR OUTSIDE THE "ESTIMATED WINDOW" TIME, THEN 
C                "CALCULATED" MAX/MIN = "ESTIMATED WINDOW" MAX/MIN
C
C                IF THE 24 HOUR MAX/MIN WAS DETERMINED TO 
C                OCCUR INSIDE THE "ESTIMATED WINDOW" TIME, THEN
C                "CALCULATED" MAX/MIN = 24 HOUR MAX/MIN TEMPERATURE
C
C                SINCE THE 24 HOUR MAX/MIN COULD BE BETWEEN HOURS         
C                WE CHECK FOR THE CLOSEST TEMPERATURE FOR THE ENTIRE        
C                25 HOURS.  IF THE CLOSEST VALUE OCCURRED OUTSIDE THE
C                "ESTIMATED WINDOW", THE "ESTIMATED WINDOW" IS USED
C                IF THE CLOSEST IS INSIDE THE WINDOW, THE 24 HOUR 
C                VALUE IS USED.
C
C                IF TOO MANY HOURLY VALUES ARE MISSING OUTSIDE THE 
C                "ESTIMATED WINDOW" AN ACCURATE DETERMINATION OF 
C                IWNCHK CAN NOT BE MADE: RESFLD(NS) = 9999. AND A 
C                12 HOUR CHECK IS CONDUCTED (SEE NOTE 1. BELOW). 
C
C                *** IF THE 24 HOUR MAX/MIN IS MISSING *** THEN
C
C                A DETERMINATION OF IWNCHK CAN NOT BE MADE
C                THEREFORE, A 12 HOUR CHECK IS CONDUCTED.
C                RESFLD(NS) = RMXMN2(NS) (SEE NOTE 2. BELOW).
C
C
C        STEP 4A. CHECKING THROUGH NUMIZN = 25 HOURS OF HOURLY VALUES
C                 18Z - 18Z FOR MIN AND 06Z - 06Z FOR MAX TO 
C                 DETERMINE IF MAX/MIN IS INSIDE OR OUTSIDE THE 
C                 "ESTIMATED WINDOW". 
C
        KSTART=7
        IF(CMIN)KEND=20
        IF(CMAX)KEND=19
        LSTART=KSTART+ITMCOR(NS)                                 
        LEND=KEND+ITMCOR(NS)
C
        DO 510 MTIME=1,INUMZN
C
          IF(RMXMN_24HR(NS).NE.ZZZ) THEN
C
C              24HR AND HRLY PRESENT
C
            IF(HRLY(NS,MTIME).NE.ZZZ) THEN
              RJTPDIF=ABS(RMXMN_24HR(NS)-HRLY(NS,MTIME))
C
              IF(RJTPDIF.LE.RTMP24_DIFF(NS)) THEN
C
                IF(RTMP24_DIFF(NS).NE.RJTPDIF) THEN
                  RTMP24_DIFF(NS)=RJTPDIF
C                      WITHIN "ESTIMATED WINDOW"
                  IF((MTIME.GE.LSTART.AND.MTIME.LE.LEND).AND.
     1              (IWNCHK(NS).EQ.0)) IWNCHK(NS)=1     
C                      SWITCH FROM "ESTIMATED WINDOW" TO OUTSIDE 
                  IF((MTIME.LT.LSTART.OR.MTIME.GT.LEND).AND.
     1              (IWNCHK(NS).EQ.1)) IWNCHK(NS)=0         
                ELSE
C                    RTMP24_DIFF(NS) = RJTPDIF AND
C                    WITHIN "ESTIMATED WINDOW"
                  IF((MTIME.GE.LSTART.AND.MTIME.LE.LEND).AND.
     1              (IWNCHK(NS).EQ.0)) IWNCHK(NS)=1          
                ENDIF
C
              ENDIF
C
C                HRLY MISSING/OUTSIDE "ESTIMATED WINDOW"
C
            ELSEIF((HRLY(NS,MTIME).EQ.ZZZ).AND.((MTIME.LT.LSTART).OR.
     1        (MTIME.GT.LEND))) THEN                                
              JMSG(NS)=JMSG(NS)+1                             
            ENDIF                                                
C
          ENDIF                                                   
C
 510    CONTINUE                              
C
C         STEP 4B. CHECK IF HOURLY MAX/MIN IS INSIDE OR OUTSIDE THE
C                  "WINDOW". IF TOO MANY TEMPERATURE MISSING OUTSIDE
C                  THE WINDOW, CANNOT ADEQUATELY CHECK IF HOURLY 
C                  MAX/MIN IS INSIDE OR OUTSIDE WINDOW, THEREFORE GO 
C                  TO 12 HOUR TEST. IF IWNCHK=1, BUT NO 24 HOUR VALUE,
C                  THEN GO TO 12 HOUR TEST. 
C
C         HOURLY MAX/MIN OUTSIDE THE WINDOW 
C         (IF 24 HOUR IS MISSING RESFLD(NS)=RMXMN2(NS))
C
        IF(IWNCHK(NS).EQ.0) THEN
          RESFLD(NS)=RMXMN2(NS)
C
          IF(JMSG(NS).GE.4) THEN
            ICSKIP(NS)=1
            RESFLD(NS)=ZZZ
          ENDIF
C
        ENDIF
C
C          HOURLY MAX/MIN INSIDE THE WINDOW
C
        IF(IWNCHK(NS).EQ.1) THEN
          RESFLD(NS)=RMXMN_24HR(NS)
C
          IF(JMSG(NS).GE.4) THEN
            ICSKIP(NS)=1
            RESFLD(NS)=ZZZ
          ENDIF
C
        ENDIF
C
C            NOTE 1. IF TOO MANY MISSING OUTSIDE OF WINDOW, THE
C                    24 HOUR COMPARISON CANNOT BE MADE, 
C                    THEREFORE GO TO 12 HOUR TEMPERATURE COMPARISON.
C
C            NOTE 2. IF 24 HOUR TEMPERATURE IS MISSING, GO TO 
C                    12 HOUR TEMPERATURE COMPARISON.
C
        IF(RMXMN_24HR(NS).EQ.ZZZ) ICSKIP(NS)=1
C
C          END OF STEP 4.
C**********************
C
C          STEP 5. THE 24 HOUR "SIMPLE CHECK" WAS INVALID, MUST CHECK
C                  12 HOUR VS THE "ESTIMATED WINDOW". PERIODS ARE
C                  SEGMENTED AS A (BEFORE WINDOW), B (WINDOW AND 12
C                  HOUR PERIOD OVERLAP) AND C (REMAINDER OF WINDOW).
C                  FIRST, INITIALIZE ARRAYS RMXHRLY AND RMNHRLY.
C
        DO 515 II=1,INUMZN
          RMXHRLY(II)=-ZMISS
          RMNHRLY(II)=ZMISS
 515    CONTINUE
C
C          STEP 5A. IF 12 HOUR TEMPERATURE IS MISSING; SET RESFLD 
C                   TO WINDOW TEMPERATURE AND GO TO NEXT STATION
C
        IF(ICSKIP(NS).EQ.1)THEN
C
          IF(RMXMN_12HR(NS).EQ.ZMISS) THEN 
	    RESFLD(NS)=RMXMN2(NS)
	    GO TO 600
          ENDIF
C
C            STEP 5B. THE RMXHRLY AND RMNHRLY ARE FILLED WITH THE
C                     HOURLY TEMPERATURE OBS USED TO GENERATE
C                     A,B, AND C FOR EITHER MAX OR MIN.
C
          DO 520 J=1,INUMZN 
            RMXHRLY(J)=HRLY(NS,J)
            RMNHRLY(J)=HRLY(NS,J)
            IF(HRLY(NS,J).EQ.ZMISS) RMXHRLY(J)=-ZMISS
 520      CONTINUE 
C
C            STEP 5C. SET UP THE THE MAX OR MIN VALUES OF SEGMENTS
C                      A, B, AND C FOR EACH TIME ZONE.
C                      MIN WINDOW = 7PM - 8AM 
C                      MAX WINDOW = 7AM - 7PM
C                   1. ALEUTIANS/HAWAII
C
        IF(ITIMEZ(NS).EQ.-10) THEN
          RMNABC(1)=MIN(RMNHRLY(7),RMNHRLY(8),RMNHRLY(9),
     *                  RMNHRLY(10),RMNHRLY(11))
          RMNABC(2)=MIN(RMNHRLY(12),RMNHRLY(13),RMNHRLY(14),
     *                  RMNHRLY(15),RMNHRLY(16),RMNHRLY(17),
     *                  RMNHRLY(18),RMNHRLY(19))
          RMNABC(3)=MIN(RMNHRLY(20),RMNHRLY(21),RMNHRLY(22),
     *                  RMNHRLY(23),RMNHRLY(24),RMNHRLY(25))
          RMXABC(1)=MAX(RMXHRLY(7),RMXHRLY(8),RMXHRLY(9),
     *                  RMXHRLY(10),RMXHRLY(11))
          RMXABC(2)=MAX(RMXHRLY(12),RMXHRLY(13),RMXHRLY(14),
     *                  RMXHRLY(15),RMXHRLY(16),RMXHRLY(17),
     *                  RMXHRLY(18),RMXHRLY(19))
          RMXABC(3)=MAX(RMXHRLY(20),RMXHRLY(21),RMXHRLY(22),
     *                  RMXHRLY(23),RMXHRLY(24))
C              2. ALASKA
        ELSEIF(ITIMEZ(NS).EQ.-9) THEN
          RMNABC(1)=MIN(RMNHRLY(7),RMNHRLY(8),RMNHRLY(9),
     *                  RMNHRLY(10))
          RMNABC(2)=MIN(RMNHRLY(11),RMNHRLY(12),RMNHRLY(13),
     *                  RMNHRLY(14),RMNHRLY(15),RMNHRLY(16),
     *                  RMNHRLY(17),RMNHRLY(18),RMNHRLY(19))
          RMNABC(3)=MIN(RMNHRLY(20),RMNHRLY(21),RMNHRLY(22),
     *                  RMNHRLY(23),RMNHRLY(24))
          RMXABC(1)=MAX(RMXHRLY(7),RMXHRLY(8),RMXHRLY(9),
     *                  RMXHRLY(10))
          RMXABC(2)=MAX(RMXHRLY(11),RMXHRLY(12),RMXHRLY(13),
     *                  RMXHRLY(14),RMXHRLY(15),RMXHRLY(16),
     *                  RMXHRLY(17),RMXHRLY(18),RMXHRLY(19))
          RMXABC(3)=MAX(RMXHRLY(20),RMXHRLY(21),RMXHRLY(22),
     *                  RMXHRLY(23))
C              3.             PACIFIC
        ELSEIF(ITIMEZ(NS).EQ.-8) THEN
          RMNABC(1)=MIN(RMNHRLY(7),RMNHRLY(8),RMNHRLY(9))
          RMNABC(2)=MIN(RMNHRLY(10),RMNHRLY(11),RMNHRLY(12),
     *                  RMNHRLY(13),RMNHRLY(14),RMNHRLY(15),
     *                  RMNHRLY(16),RMNHRLY(17),RMNHRLY(18),
     *                  RMNHRLY(19))
          RMNABC(3)=MIN(RMNHRLY(20),RMNHRLY(21),RMNHRLY(22),
     *                  RMNHRLY(23))
          RMXABC(1)=MAX(RMXHRLY(7),RMXHRLY(8),RMXHRLY(9))
          RMXABC(2)=MAX(RMXHRLY(10),RMXHRLY(11),RMXHRLY(12),
     *                  RMXHRLY(13),RMXHRLY(14),RMXHRLY(15),
     *                  RMXHRLY(16),RMXHRLY(17),RMXHRLY(18),
     *                  RMXHRLY(19))
          RMXABC(3)=MAX(RMXHRLY(20),RMXHRLY(21),RMNHRLY(22))
C              4. MOUNTAIN
        ELSEIF(ITIMEZ(NS).EQ.-7) THEN
          RMNABC(1)=MIN(RMNHRLY(7),RMNHRLY(8))
          RMNABC(2)=MIN(RMNHRLY(9),RMNHRLY(10),RMNHRLY(11),
     *                  RMNHRLY(12),RMNHRLY(13),RMNHRLY(14),
     *                  RMNHRLY(15),RMNHRLY(16),RMNHRLY(17),
     *                  RMNHRLY(18),RMNHRLY(19))
          RMNABC(3)=MIN(RMNHRLY(20),RMNHRLY(21),RMNHRLY(22))
          RMXABC(1)=MAX(RMXHRLY(7),RMXHRLY(8))
          RMXABC(2)=MAX(RMXHRLY(9),RMXHRLY(10),RMXHRLY(11),
     *                  RMXHRLY(12),RMXHRLY(13),RMXHRLY(14),
     *                  RMXHRLY(15),RMXHRLY(16),RMXHRLY(17),
     *                  RMXHRLY(18),RMXHRLY(19))
          RMXABC(3)=MAX(RMXHRLY(20),RMXHRLY(21))
C              5. CENTRAL
        ELSEIF(ITIMEZ(NS).EQ.-6) THEN
          RMNABC(1)=RMNHRLY(7)
          RMNABC(2)=MIN(RMNHRLY(8),RMNHRLY(9),RMNHRLY(10),
     *                  RMNHRLY(11),RMNHRLY(12),RMNHRLY(13),
     *                  RMNHRLY(14),RMNHRLY(15),RMNHRLY(16),
     *                  RMNHRLY(17),RMNHRLY(18),RMNHRLY(19))
          RMNABC(3)=MIN(RMNHRLY(20),RMNHRLY(21))
          RMXABC(1)=RMXHRLY(7)
          RMXABC(2)=MAX(RMXHRLY(8),RMXHRLY(9),RMXHRLY(10),
     *                  RMXHRLY(11),RMXHRLY(12),RMXHRLY(13),
     *                  RMXHRLY(14),RMXHRLY(15),RMXHRLY(16),
     *                  RMXHRLY(17),RMXHRLY(18),RMXHRLY(19))
          RMXABC(3)=RMXHRLY(20)
C              6. EASTERN
        ELSEIF(ITIMEZ(NS).EQ.-5) THEN
          RMNABC(1)=9988.
          RMNABC(2)=MIN(RMNHRLY(7),RMNHRLY(8),RMNHRLY(9),
     *                  RMNHRLY(10),RMNHRLY(11),RMNHRLY(12),
     *                  RMNHRLY(13),RMNHRLY(14),RMNHRLY(15),
     *                  RMNHRLY(16),RMNHRLY(17),RMNHRLY(18),
     *                  RMNHRLY(19))
          RMNABC(3)=RMNHRLY(20)
C
          RMXABC(1)=-9988.
          RMXABC(2)=MAX(RMXHRLY(7),RMXHRLY(8),RMXHRLY(9),
     *                  RMXHRLY(10),RMXHRLY(11),RMXHRLY(12),
     *                  RMXHRLY(13),RMXHRLY(14),RMXHRLY(15),
     *                  RMXHRLY(16),RMXHRLY(17),RMXHRLY(18),
     *                  RMXHRLY(19))
          RMXABC(3)=-9988.
C              7. ATLANTIC
        ELSEIF(ITIMEZ(NS).EQ.-4) THEN
          RMNABC(1)=RMNHRLY(6)
          RMNABC(2)=MIN(RMNHRLY(7),RMNHRLY(8),RMNHRLY(9),
     *                  RMNHRLY(10),RMNHRLY(11),RMNHRLY(12),
     *                  RMNHRLY(13),RMNHRLY(14),RMNHRLY(15),
     *                  RMNHRLY(16),RMNHRLY(17),RMNHRLY(18),
     *                  RMNHRLY(19))
          RMNABC(3)=9988.
          RMXABC(1)=RMXHRLY(6)
          RMXABC(2)=MAX(RMXHRLY(7),RMXHRLY(8),RMXHRLY(9),
     *                  RMXHRLY(10),RMXHRLY(11),RMXHRLY(12),
     *                  RMXHRLY(13),RMXHRLY(14),RMXHRLY(15),
     *                  RMXHRLY(16),RMXHRLY(17),RMXHRLY(18),
     *                  RMXHRLY(19))
          RMXABC(3)=RMXHRLY(19)
        ENDIF
C
C         STEP 5C. CALL SUBROUTINE CKABC AND DETERMINE IF THE NUMBER 
C                  OF MISSING PER SEGMENT A,B, OR C IS LESS THAN 2.
C                  IF EITHER A,B OR C CONTAIN A MISSING VALUE, THE
C                  DAYTIME MAX OR NIGHTTIME MIN WILL EQUAL THE 
C                  WINDOW TEMPERATURE. 
C
        CALL CKABC(RMNHRLY,RMXHRLY,RMNABC,RMXABC,ITIMEZ(NS),
     *              ZMISS,MXMN,INUMZN)
C
        IF(CMAX) THEN
C
          DO 530 J=1,3
            IF(RMXABC(J).EQ.-ZMISS)RESFLD(NS)=RMXMN2(NS)
 530      CONTINUE
C
          IF(RESFLD(NS).EQ.RMXMN2(NS))GO TO 600
        ELSEIF(CMIN) THEN
C
          DO 535 J=1,3
            IF(RMNABC(J).EQ.ZMISS)RESFLD(NS)=RMXMN2(NS)
 535      CONTINUE
C
          IF(RESFLD(NS).EQ.RMXMN2(NS))GO TO 600
        ENDIF
C
C        END OF STEP 5.
C**********************
C
C        STEP 6. SINCE "SIMPLE 24 HOUR CHECKING  WAS NOT SUFFICIENT,
C                CONTINUE ON WITH A SERIES OF IF TESTS TO DETERMINE
C                WHETHER THE 12 HOUR MAX/MIN TEMPERATURE QUALIFIES
C                AS THE DAY/NIGHT MAX/MIN TEMPERATURE.
C        NOTE: IN 24 HOUR VS WINDOW HOURLY TEMPERATURE COMPARISON, IF
C              IDENTICAL MAX OR MIN TEMPERATURES ARE OBSERVED INSIDE
C              AND OUTSIDE THE WINDOW, ASSUME THE MIN OCCURRED INSIDE 
C              THE WINDOW. THIS SAME ASSUMPTION IS USED FOR 12 HOUR 
C              TESTING. 
C
        IF(CMIN) THEN
C                B      <=    A               B      <=    C
          IF((RMNABC(2).LE.RMNABC(1)).AND.(RMNABC(2).LE.RMNABC(3)))
     *      THEN
            RESFLD(NS)=RMXMN_12HR(NS)
C                   A      <     B              C      <     B
          ELSEIF((RMNABC(1).LT.RMNABC(2)).OR.(RMNABC(3).LT.RMNABC(2)))
     *      THEN
	    RESFLD(NS)=RMXMN2(NS)
          ENDIF
C
        ELSEIF(CMAX) THEN
C                B      >=    A               B      >=    C
	  IF((RMXABC(2).GE.RMXABC(1)).AND.(RMXABC(2).GE.RMXABC(3)))
     *      THEN
            RESFLD(NS)=RMXMN_12HR(NS)
C                   A      >     B              C      >     B
          ELSEIF((RMXABC(1).GT.RMXABC(2)).OR.(RMXABC(3).GT.RMXABC(2)))
     *      THEN
            RESFLD(NS)=RMXMN2(NS)
          ENDIF
C
        ENDIF
C
      ENDIF
C
C        END OF STEP 6.
C**********************
 600  CONTINUE
C
      RETURN
      END
C
C********************************************************
      SUBROUTINE CKABC(RMNHRLY,RMXHRLY,RMNABC,RMXABC,ITZONE,
     *                  ZMISS,MXMN,INUMZN)
C
C        AUGUST  1998   WEISS   TDL   MOS-2000
C
C        PURPOSE
C            FOR 12 HOUR TEMPERATURE COMPARISONS AT A GIVEN 
C            STATION, THIS ROUTINE WILL DETERMINE THE
C            WHETHER SEGMENTS A,B, AND C CONTAIN A SUFFICIENT 
C            HOURLY TEMPERATURE VALUES TO BE USED FOR 
C            DAYTIME/MAX AND NIGHTTIME/MIN ESTIMATES.  
C
C        VARIABLES
C
C          RMNHRLY(K) = HOLDS THE HOURLY TEMPERATURES TO GENERATE
C                       MIN TEMPERATURE SEGMENTS A,B, AND C,
C                       WHERE K=1,INUMZN (INPUT).
C          RMXHRLY(K) = HOLDS THE HOURLY TEMPERATURES TO GENERATE
C                       MAX TEMPERATURE SEGMENTS A,B, AND C,
C                       WHERE K=1,INUMZN (INPUT).
C           RMNABC(J) = HOLDS THE MIN TEMPERATURES FOR SEGMENTS 
C                       A,B AND C RESPECTIVELY. SEGMENT A IS OUTSIDE 
C                       THE WINDOW AND WITHIN THE 12 HOUR TEMPERATURE 
C                       FOR EASTERN - ALEUTIAN TIME ZONES AND 
C                       WITHIN WINDOW BUT OUTSIDE 12 HOUR TEMPERATURE
C                       FOR ATLANTIC TIME ZONE. SEGMENT B ALWAYS
C                       OVERLAPS THE WIDOW AND 12 HOUR TEMPERATURE. 
C                       SEGMENT C IS WITHIN THE WINDOW  BUT OUTSIDE 
C                       12 HOUR TEMPERATURE FOR EASTERN - ALEUTIAN 
C                       TIME ZONES. FOR THE ATLANTIC TIME ZONE 
C                       SEGMENT C IS ZERO FOR MIN AND OUTSIDE WINDOW 
C                       BUT WITHIN 12 HOUR TEMPERATURE FOR MAX, 
C                       FOR A GIVEN STATION, WHERE J=1,3 (INPUT/OUTPUT).
C           RMXABC(J) = HOLDS THE MAX TEMPERATURES FOR SEGMENTS
C                       A,B AND C RESPECTIVELY. SEE RMNABC FOR MORE 
C                       DETAILS, J=1,3 (INPUT/OUTPUT). 
C                       FOR A GIVEN STATION, WHERE J=1,3 (INPUT/OUTPUT).
C              ITZONE = ARRAY STATION'S TIME ZONE (INPUT).
C               ZMISS = MISSING VALUE OF 9999. (INPUT).
C              INUMZN = NUMBER OF HOURS (=25) USED FOR RMXHRLY AND 
C                       RMNHRLY ARRAYS (INPUT).
C                MXMN = FLAG TO INDICATE DAYTIME MAX (=2) OR
C                       NIGHTTIME MIN (=1) PROCESSING (INPUT).
C
C        ADDITIONAL VARIABLES
C                CMAX = LOGICAL PARAMETER FOR DAYTIME MAXIMUM
C                       PROCESSING (INTERNAL).
C                CMIN = LOGICAL PARAMETER FOR NIGHTTIME MINIMUM
C                       PROCESSING (INTERNAL).
C                IEND = END COUNTER OF NUMBER OF HOURS PER
C                       SEGMENT (INTERNAL).
C              ISEGHR = NUMBER OF HOURS PER SEGMENT (INTERNAL).
C              ISTART = START COUNTER OF NUMBER OF HOURS PER
C                       SEGMENT (INTERNAL).
C      MAX_COUNT(J,M) = FOR MAX ESTIMATES, THE TOTAL NUMBER OF HOURLY 
C                       OBS USED TO GENERATE SEGMENTS A,B, AND C FOR
C                       EACH TIME ZONE, WHERE M=1,7 AND J=1,3 
C                       (INTERNAL). NOTE: M=1,7 = ZONES -10 TO -4.
C       MAX_MISSNG(J) = FOR MAX ESTIMATES, THE NUMBER OF MISSING 
C                       HOURLY OBS FOR SEGMENTS A,B, AND C FOR EACH
C                       STATION, WHERE J=1,3 (INTERNAL).
C             MAXH(L) = FOR MAX ESTIMATES, NUMBER OF HOURLY OBS 
C                       PER SEGMENT (A - C) FOR ALL 7 TIME ZONES
C                       (WEST TO EAST) ,WHERE L=1,21 (INTERNAL). 
C      MIN_COUNT(J,M) = FOR MIN ESTIMATES, THE TOTAL NUMBER OF HOURLY
C                       OBS USED TO GENERATE SEGMENTS A,B, AND C FOR
C                       EACH TIME ZONE, WHERE M=1,7 AND J=1,3 
C                       (INTERNAL). NOTE: M=1,7 = ZONES -10 TO -4.
C       MIN_MISSNG(J) = FOR MIN ESTIMATES, THE NUMBER OF MISSING 
C                       HOURLY OBS FOR SEGMENTS A,B, AND C FOR EACH
C                       STATION, WHERE J=1,3 (INTERNAL).
C             MINH(L) = FOR MIN ESTIMATES, NUMBER OF HOURLY OBS 
C                       PER SEGMENT (A - C) FOR ALL 7 TIME ZONES
C                       (WEST TO EAST) ,WHERE L=1,21 (INTERNAL). 
C
      IMPLICIT NONE
C 
      LOGICAL CMAX,CMIN
C
      INTEGER ITZONE,MXMN,INUMZN
      INTEGER MAX_COUNT(3,7),MIN_COUNT(3,7),MAX_MISSNG(3),
     *        MIN_MISSNG(3),MAXH(21),MINH(21)
      INTEGER ISTART,IEND,ISEGHR,I,J,II,N,NN
C
      REAL RMNABC(3),RMXABC(3),RMNHRLY(INUMZN),RMXHRLY(INUMZN),ZMISS
C
      DATA MINH/5,8,6,4,9,5,3,10,4,2,11,3,1,12,2,0,13,1,1,13,0/
      DATA MAXH/5,8,5,4,9,4,3,10,3,2,11,2,1,12,1,0,13,0,1,12,1/
C
C******************************************************
C
C        STEP 1. INITIALIZE
C
      ISTART=0
      IEND=0
      ISEGHR=0
      CMIN=.FALSE.
      CMAX=.FALSE.
      IF(MXMN.EQ.1)CMIN=.TRUE.
      IF(MXMN.EQ.2)CMAX=.TRUE.
C
      DO 10 I=1,3
        MAX_MISSNG(I)=0
        MIN_MISSNG(I)=0
 10   CONTINUE
C
      II=0
C
      DO 15 I=1,7
C
        DO 12 J=1,3
          II=II+1
          MAX_COUNT(J,I)=MAXH(II)
          MIN_COUNT(J,I)=MINH(II)
 12     CONTINUE
C
 15   CONTINUE 
C
C        STEP 2A. FOR MAX, COUNT NUMBER OF MISSING PER SEGMENT
C        STEP 2B. FOR MIN, COUNT NUMBER OF MISSING PER SEGMENT
C
      ISTART=7
      IF(ITZONE.EQ.-4)ISTART=6
      IF(CMAX) THEN 
C
        DO 30 N=1,3
          ISEGHR=MAX_COUNT(N,ITZONE+11)
C
          IF(ISEGHR.GT.0) THEN
            IEND=ISTART+ISEGHR-1
C
            DO 25 NN=ISTART,IEND
              IF(RMXHRLY(NN).EQ.-ZMISS) MAX_MISSNG(N)=MAX_MISSNG(N)+1
 25         CONTINUE
C
            ISTART=IEND+1
            IF((ISEGHR.EQ.1).AND.(MAX_MISSNG(N).EQ.1))RMXABC(N)=-9988.
            IF((ISEGHR.GT.1).AND.(MAX_MISSNG(N).GE.2))RMXABC(N)=-ZMISS
          ENDIF
C
 30     CONTINUE
C
      ELSEIF(CMIN)THEN
C
        DO 40 N=1,3
          ISEGHR=MIN_COUNT(N,ITZONE+11)
C
          IF(ISEGHR.GT.0) THEN
            IEND=ISTART+ISEGHR-1
C
            DO 35 NN=ISTART,IEND
              IF(RMNHRLY(NN).EQ.ZMISS) MIN_MISSNG(N)=MIN_MISSNG(N)+1
 35         CONTINUE
C
            ISTART=IEND+1
            IF((ISEGHR.EQ.1).AND.(MIN_MISSNG(N).EQ.1))RMNABC(N)=9988.
            IF((ISEGHR.GT.1).AND.(MIN_MISSNG(N).GE.2))RMNABC(N)=ZMISS
          ENDIF
C
 40     CONTINUE
C
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE CKPRIOD(HRLY,ISTART,RMXMN_5P,ITMCOR,KFLAG,IMSG,
     1                   MXMN,ND1,NSTA,KTIME,MAXHRL,ZMISS,NS,IPRIOD)
C
C        JULY    1998   WEISS   TDL   MOS-2000
C                               BASED ON CODE BY 1990 CODE BY
C                               DAVE MILLER         
C        MAY     2003   GLAHN   CHANGED NWORDS TO NSTA
C
C        PURPOSE                                                        
C            FOR A GIVEN STATION, THIS ROUTINE WILL DETERMINE THE 
C            TEMPERATURE VALUE OF ONE OF 5 PERIODS WITHIN THE 
C            MAX/MIN TEMPERATURE WINDOW. MISSING DATA IS COUNTED
C            (KFLAG) SO THE MAIN PROGRAM CAN REINITIALIZE   
C            OR SET A MAX/MIN TO MISSING AS NECESSARY.     
C
C        INPUT ARGUMENTS
C           HRLY(N,K) = HOLDS THE HOURLY OBS OF EACH STATION USED
C                       FOR MIN ESTIMATES, WHERE N=1,ND1 AND
C                       K=1,MAXHRL (INPUT).
C           ISTART(J) = THE STARTING POSITION (HOUR) IN THE HRLY ARRAY
C                       OF EACH OF THE FIVE PERIODS USED IN CALCULATING
C                       THE MAX/MIN WINDOW ESTIMATED TEMPERATURE, WHERE
C                       J=1,5. THE POSITIONS ARE VALID FOR EST (INPUT).
C       RMXMN_5P(N,J) = HOLDS VALUES OF TEMPERATURE FOR EACH OF THE 
C                       5 TIME PERIODS FOR EACH STATION, WHERE N=1,ND1
C                       AND J=1,5 (INPUT/OUTPUT).
C           ITMCOR(N) = TIME ZONE CORRECTION FOR EACH STATION. ADDED
C                       TO THE GMT HOUR SO THE PROGRAM CAN ARRIVE
C                       AT THE CORRECT DAYTIME/NIGHTTIME HOURS,
C                       WHERE N=1,ND1 (INTERNAL).
C                       NOTE: REFERENCED TO THE EASTERN TIME ZONE.
C          KFLAG(K,N) = CONTAINS HOW MANY TEMPERATURES WERE MISSING
C                       PER TIME PERIOD PER STATION, WHERE K=1,5 AND
C                       N=1,ND1 (INPUT).
C             IMSG(N) = THE NUMBER OF MISSING HOURLY VALUES INSIDE THE
C                       "ESTIMATED WINDOW" PER STATION, WHERE N=1,ND1
C                       (INPUT).
C                MXMN = INDICATOR OF WHETHER YOU ARE TO CALCULATE A
C                       DAYTIME MAX (=2) OR A NIGHTTIME MIN (=1)
C                       (INPUT).
C                 ND1 = MAXIMUM NUMBER OF STATIONS (INPUT).
C                NSTA = NUMBER OF STATIONS THAT THE MAX OR MIN WILL BE
C                       CALCULATED FOR (INPUT).
C               KTIME = THE PERIOD BEING PROCESSED, 1 TO 5 (INPUT).
C              MAXHRL = MAXIMUM NUMBER OF STORED HOURLY TEMPERATURES
C                       FROM ACROSS ALL POTENTIAL TIME ZONES (=25)
C                       (INPUT).
C               ZMISS = THE FILL VALUE FOR THE RMXMN2 ARRAY: 9999 FOR
C                       MIN AND -9999 FOR MAX ESTIMATES (INPUT).
C                  NS = STATION COUNTER (INPUT).
C              IPRIOD = REPRESENTS THE LENGTH OF A TIME PERIOD WITHIN
C                       THE MAX MIN WINDOW (INPUT).
C
C         ADDITIONAL VARIABLES
C
C                CMAX = LOGICAL PARAMETER FOR DAYTIME MAXIMUM
C                       PROCESSING (INTERNAL).
C                CMIN = LOGICAL PARAMETER FOR NIGHTTIME MINIMUM
C                       PROCESSING (INTERNAL).
C                IPOS = POSITION INSIDE HRLY ARRAY OF REQUESTED HOUR 
C                       FOR A GIVEN STATION (INTERNAL).
C            RMXMN2(L) = ONE HOURLY TEMPERATURE WITHIN A TIME PERIOD FOR
C                       EACH STATION (INTERNAL).
C
      IMPLICIT NONE 
C
      LOGICAL CMAX,CMIN
C
      INTEGER ISTART(5),ITMCOR(ND1),KFLAG(5,ND1),IMSG(ND1)
      INTEGER KTIME,MXMN,ND1,NSTA,MAXHRL,IPRIOD,NS
      INTEGER I,IPOS
C
      REAL HRLY(ND1,MAXHRL),RMXMN_5P(ND1,5),RMXMN2(3),ZMISS
C
C        STEP 1. INITIALIZATION
C
      CMAX=.FALSE.
      CMIN=.FALSE.
      IF(MXMN.EQ.1)CMIN=.TRUE.
      IF(MXMN.EQ.2)CMAX=.TRUE.
      IPOS=0
C
C        STEP 2. DETERMINE MAX/MIN OR MISSING FOR A PERIOD.
C
C        STEP 2A. SET UP AN ARRAY (RMXMN2) TO STORE THE TEMPERATURES
C                 MAKING UP THE VALUES OF A PERIOD FOR EACH STATION 
C                 STATION, (TIME ZONE DEPENDENT).
C
        DO 560 I=1,IPRIOD 
          IPOS=ISTART(KTIME)-1+I+ITMCOR(NS)
          RMXMN2(I)=HRLY(NS,IPOS)
C
          IF(HRLY(NS,IPOS).EQ.ZMISS) THEN 
            IF(CMAX) RMXMN2(I)=-ZMISS
            IF(CMIN) RMXMN2(I)=ZMISS
            KFLAG(KTIME,NS)=KFLAG(KTIME,NS)+1  
            IMSG(NS)=IMSG(NS)+1
            IF(KFLAG(KTIME,NS).GE.2) IMSG(NS)=4
          ENDIF
C
 560    CONTINUE
C
C        STEP 3. SELECT MAX/MIN TEMPERATURE FROM AVAILABLE HOURS 
C                WITHIN THE PRESENTLY PROCESSING PERIOD.
C
        IF(CMAX) THEN
C
          IF(IPRIOD.EQ.3)THEN
            RMXMN_5P(NS,KTIME)=MAX(RMXMN2(1),RMXMN2(2),RMXMN2(3))
          ELSEIF(IPRIOD.EQ.1) THEN
            RMXMN_5P(NS,KTIME)=RMXMN2(1)
          ENDIF
C
        ELSEIF(CMIN) THEN
C
          IF(IPRIOD.EQ.3)THEN
            RMXMN_5P(NS,KTIME)=MIN(RMXMN2(1),RMXMN2(2),RMXMN2(3))
          ELSEIF(IPRIOD.EQ.2) THEN
            RMXMN_5P(NS,KTIME)=MIN(RMXMN2(1),RMXMN2(2))
          ENDIF
C
        ENDIF
C
      RETURN                                                            
      END                                                               
