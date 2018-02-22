C$$$  MAIN PROGRAM DOCUMENTATION BLOCK  ***                             
C                                                                       
C MAIN PROGRAM: HRLYTBL
C   PRGMMR: ALLEN            ORG: OSD211      DATE: 2000-11-30
C                                                                       
C ABSTRACT: TO READ THE BUFR LAND SURFACE FILES HOURLY AND FORMAT THEM
C           IN AN ASCII FILE IN THEIR ORIGINAL METAR UNITS
C 
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-L1-21  K. HUGHES   
C   97-10-16  K. HUGHES  MODIFIED CALL TO VISIBILITY SUBROUTINE TO ADD VVISI FOR
C                        USE BY THE CLOUD SUBROUTINE.
C   98-07-23  K. HUGHES  ADDED CALL TO DATELEN TO USE A 10-DIGIT YEAR FOR BUFR 
C                        ROUTINES.  REPLACED CALL TO GTDATE WITH GETDATE FOR
C                        Y2K COMPLIANCE.
C   98-11-19  K. HUGHES  FIXED IF TEST IN SNOW.F, MADE CHANGES IN PRESENT
C                        WEATHER ROUTINE TO ADD -DZRA AND +TSGR.
C   98-12-09  K. HUGHES  CHANGED FORMAT TO I2.2 FROM I2 TO FORCE 2-DIGITS TO
C                        PRINT FOR THE HOUR AT THE END OF THE OUTPUT FILES.  
C                        MADE CHANGES TO SNOW.F TO REMOVE SNOW FALL REPORTS.
C   99-05-19  K. HUGHES  CHANGED CALL TO VISIBILITY STRINGS TO ADD THE .REHOVI
C                        WHICH WILL DISTINGUISH AN M1/4 SM FROM 1/4 SM. THIS
C                        IS NEED BY THE VERIFICATION PROGRAM. ALSO CHANGED THE
C                        DIMENSION OF VISI FROM (1,255) TO (3,255).
C   99-08-16  R. ALLEN   CONVERTED CODE FROM CRAY TO IBM SP.
C                        ADJUSTED THE DO LOOP THAT INITIALIZES GEOG FROM 1,3 TO
C                        1,5
C   00-01-04  R. ALLEN   ADDED PRWX CODE 105 (HZ W/VIS <1 KM). ALSO ADDED
C                        CODES 128 AND 129, WHICH ARE FOR BLSN WITH 
C                        SET VISIBILITIES.  CODES 128,129  AREN'T CURRENTLY 
C                        USED BY NCEP, BUT WERE ADDED FOR COMPLETENESS.
C   00-11-30  R. ALLEN   MADE CORRECTIONS TO TEMPQC, MAXIMUM_TEMP AND
C                        MINIMUM_TEMP
C   01-01-12  R. ALLEN   CHANGED DIMENSION OF RAWRPT FROM 32 TO 255 TO 
C                        CORRECT FOR EXTRA LONG RAW REPORTS THAT 
C                        OVERFLOWED THE ARRAY.  ALSO CHANGED
C                        1ST DIMENSION OF RPTIME FROM 4 TO 3 TO BE 
C                        CONSISTENT WITH USE AND SUBROUTINE.
C   01-03-08  R. ALLEN   ADDED 9 SITES IN RUSSIA (START WITH UE OR UH)
C                        TO THE ALLOWABLE CALL LETTERS FOR USE IN LAMP'S
C                        ANALYSIS
C   01-06     R. ALLEN   A CHANGE WAS MADE TO ALLOW IN "N" SITES, BUT THAT SOURCE
C                        CODE WAS MISPLACED, SO I RECREATED THOSE CHANGES 3/2002
C   02-03-06  R.COSGROVE MADE MODIFICATIONS TO SUBROUTINES SNOW AND 
C                        PRECIPITATION BECAUSE NCO MODIFIED REPRESENTATIONS
C                        OF TRACE OF PCP IN BUFR FILES. AFFECTED MNEUMONICS
C                        WERE TP01, TP03, TP06, TP24, DOFS, TOSD
C   05-10-05  R.COSGROVE MADE MODIFICATION TO SUBROUTINE PRECIPITATION TO
C                        SAVE 1-HR PRECIP FROM MORE STATION TYPES.
C   13-01-17  E. ENGLE   CHECK NRET AFTER CALL TO UFBINT AND BEFORE CALL TO
C                        WEATHER. IF NRET IS GT 3, IT WILL BE REDUCED TO 3.
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     HRLYTBL                                                          
C                                                                       
C             HUGHES  MDL    FORTRAN90   CRAY4
C
C        PURPOSE                                                        
C            TO FORMAT THE HOURLY BUFR DATA FOR ARCHIVE AND
C            OPERATIONAL USES 
C                                                                       
C        DATA SET USED                                                  
C            FT42 -  INPUT DATA SET  BUFR FILE
C            FT06 -  OUTPUT DATA SET, QUALITY CONTROL MESSAGES, SCRIPT  
C                    OUTPUT
C            FT60 -  OUTPUT DATA SET, ASCII TABLE CONTAINING COLUMN
C                    HEADINGS AND SORTED STATIONS 
C            FT70 -  OUTPUT DATA SET, RAW METAR REPORTS
C            FT80 -  TEMPORARY OUTPUT DATA SET, ASCII TABLE BEFORE 
C                    STATIONS ARE SORTED
C            FT46 -  OUTPUT DATA SET, TEST MESSAGES, NO LONGER USED
C                                                                       
C        VARIABLES                                                      
C              LUBFR = LOGICAL UNIT NUMBER FOR BUFR FILE
C             SUBSET = TABLE A MNEMONIC
C
C        VARIABLES USED TO READ BUFR FILES (MNEMONICS)
C          (THE UNITS AND VALUES STORED IN BUFR FOR MOST ELEMENTS CAN BE
C           FOUND IN THE WMO MANUAL ON CODES VOLUME I.2
C           PART B - BINARY CODES)
C               ALSE = ALTIMETER SETTING (PASCALS)
C               AUTO = STATION TYPE
C               CLAM = CLOUD AMOUNT (WMO CODE TABLE)
C               CLAT = LATITUDE IN DEGREES (N IS POSITIVE)
C               CLON = LONGITUDE IN DEGRESS (W IS NEGATIVE)
C               DAYS = DAY
C               DOFS = DEPTH OF FRESH SNOW (METERS)
C            DTHMITM = DURATION OF TIME IN HOURS OF MINIMUM TEMPERATURE 
C            DTHMXTM = DURATION OF TIME IN HOURS OF MAXIMUM TEMPERATURE 
C            DTHDOFS = DURATION OF TIME IN HOURS OF SNOW FALL
C               HOCB = HEIGHT OF CLOUD BASE (METERS)
C               HOUR = HOUR
C               HOVI - HORIZONTAL VISIBILITY (METERS)
C               MINU = MINUTE
C               MITM = MINIMUM TEMPERATURE (KELVIN)
C               MXGS = MAX WIND SPEED / GUSTS (METERS/SECOND)
C               MXTM = MAXIMUM TEMPERATURE (KELVIN)
C               PMSL = PRESSURE REDUCED TO MSL (PASCALS)
C               PRWE = PRESENT WEATHER (WMO CODE TABLE)
C               RPID = REPORT IDENTIFIER - CALL LETTERS STORED AS REAL 
C              RRSTG = RAW REPORT STRING, USED TO MAKE RAWMETAR FILES
C               SELV = HEIGHT OF STATION 
C              THRPT = TYPE OF HOURLY REPORT
C               TMDB = TEMPERATURE/DRY BULB TEMPERATURE (KELVIN)
C               TMDP = DEW POINT TEMPERATURE (KELVIN)
C               TOSD = TOTAL SNOW DEPTH (METERS)
C               TOSS = TOTAL SUNSHINE (MINUTES)
C               TP01 = TOTAL PRECIPITATION PAST 1 HOURS (MILLIMETERS)
C               TP03 = TOTAL PRECIPITATION PAST 3 HOURS (MILLIMETERS)
C               TP06 = TOTAL PRECIPITATION PAST 6 HOURS (MILLIMETERS)
C               TP24 = TOTAL PRECIPITATION PAST 24 HOURS (MILLIMETERS)
C               VTVI = VERTICAL VISIBILITY (METERS)
C               WDIR = WIND DIRECTION (DEGREES)
C               WMOB = WMO BLOCK NUMBER(1ST 2 DIGITS),CURRENTLY NOT USED
C               WMOS = WMO STATION NUMBER(LAST 3 DIGITS),CURRENTLY NOT
C                      USED
C               WSPD = WIND SPEED (METERS/SECOND)
C                                                                       
C        VARIABLES USED TO CREATE MDL ASCII OUTPUT FILE 
C            PRESS(J)= MEAN SEA-LEVEL PRESSURE
C             TDRY(J)= DRY BULB TEMPERATURE USING CORRECT CONVERSIONS
C                      (DEGREES F)
C             TDEW(J)= DEW POINT (DEGREES F) USING CORRECT CONVERSION
C              VIS(J)= HORIZONTAL VISIBILITY IN MILES
C             ELEV(J)= ELEVATION IN FEET
C             WSPD(J)= WIND SPEED IN KNOTS
C             WDIR(J)= WIND DIRECTION IN WHOLE DEGREES
C               MNTH = MONTH
C               YEAR = YEAR
C                                                                       
C        VARIABLES USED AS COUNTERS                                     
C               ISEQ = 
C                                                                       
C        SUBPROGRAMS CALLED:                                            
C            LIBRARY:                                                   
C              BUFRLIB: 
C                DATELEN - ENTRY POINT USED TO SIGNAL TWO OR FOUR DIGIT
C                          YEAR
C                 OPENBF - OPENS A BUFR FILE
C                 READMG - ADVANCES INPUT MESSAGE POINTER
C                 READSB - READS AND UNPACKS DATA FROM A SUBSET
C                          WITHIN A BUFR MESSAGE
C                 UFBREP - LOGICAL I/O, TRANSFERS DATA FROM SUBSET
C                          BUFR FILE TO ARRAY, USED FOR MULTIPLE
C                          VALUES IN A MNEMONIC
C                 UFBINT - LOGICAL I/O, TRANSFERS DATA FROM SUBSET BUFR
C                          FILE TO ARRAY, USED FOR SINGLE VALUE 
C                          MNEMONICS
C                W3LIB:
C                 W3TAGB - WRITES MESSAGES TO THE PRODUCTION LOG FILE
C                 W3TAGE - WRITES MESSAGES TO THE PRODUCTION LOG FILE
C               TDLLIB:
C                GETDATE - READS DATE-TIME GROUP FROM NCEP'S STANDARD
C                          DATE FILE AND RETURNS THE DATE IN MDL FORMAT 
C               UNIQUE: 
C                  CLOUD - RETURNS CLOUD AMOUNTS, HEIGHTS, AND TOTAL
C                          AND PARTIAL OBSCURATIONS FOR UP TO SIX LAYERS
C              GEOGRAPHY - RETURNS STATION LATITUDE, LONGITUDE, 
C                          ELEVATION (NOT USED), STATION TYPE AND
C                          INDICATOR FOR SPECIALS (WHICH ARE NOT SAVED).
C           MAXIMUM_TEMP - RETURNS 6 HR AND 24 HR MAXIMUM TEMPERATURES
C                          IN DEGREES FAHRENHEIT
C           MINIMUM_TEMP - RETURNS 6 HR AND 24 HR MINIMUM TEMPERATURES
C                          IN DEGREES FAHRENHEIT
C          PRECIPITATION - RETURNS 1 HR, 3 HR, 6 HR, AND 24 HR
C                          PRECIPITATION AMOUNTS IN HUNDREDTHS OF INCHES
C               PRESSURE - RETURNS SEA-LEVEL PRESSURE IN MILLIBARS AND
C                          THE ALTIMETER SETTING IN INCHES OF MERCURY
C            REPORT_TIME - RETURNS DAY, HOUR, AND MINUTE OF THE METAR
C                          REPORT 
C                   SNOW - RETURNS THE DEPTH OF SNOW ON THE GROUND AND
C                          THE DEPTH OF FRESHLY FALLEN SNOW IN INCHES
C               SUNSHINE - RETURNS THE DURATION OF SUNSHINE IN MINUTES
C                          THIS IS ONLY REPORTED AT 08 GMT.
C            TEMPERATURE - RETURNS THE HOURLY TEMPERATURE AND DEW POINT 
C                          IN DEGREES FAHRENHEIT.
C                 TEMPQC - QUALITY CONTROLS THE TEMPERATURE, DEW POINT
C                          AND 6 HR MAX AND MIN TEMPERATURES, CHECKING
C                          FOR VALUES OUT OF RANGE, AND INCONSISTENCIES.
C             VISIBILITY - RETURNS THE HORIZONTAL VISIBILITY AND THE
C                          VERTICAL VISIBILITY IN MILES.
C                WEATHER - DECODES THE BUFR REPRESENTATION OF THE
C                          PRESENT WEATHER, AND RETURNS THE STANDARD
C                          METAR CHARACTER NOTATIONS.
C                  WINDS - RETURN THE WIND DIRECTION IN WHOLE DEGREES,
C                          THE WIND SPEED IN KNOTS, AND WIND GUSTS IN
C                          KNOTS
C             UNIQUE FUNCTIONS: 
C               KELVIN_F - CONVERTS KELVIN TO DEGREES FAHRENHEIT
C               METER_FT - CONVERTS METERS TO FEET.
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90                                                
C   MACHINE:  CRAY4                                                   
C                                                                       
C       THIS PROGRAM HRLYTBL.F READS THE BUFR FILES FROM THE DUMP  
C       SCRIPT AND WRITES TO THE FILE SFCOUT IN BUFR STANDARD UNITS.
C       OUTPUT FILE SFCTBL.$DAT (FORT.60) CONTAINS THE OUTPUT 
C       IN ENGLISH UNITS.
C$$$

      PROGRAM HRLYTBL
        IMPLICIT NONE

      REAL*8 GEOG(5,255),TEMPTD(2,255),
     *       VISI(3,255),PRWX(3,255),RPTIME(3,255),WIND(3,255),
     *       TDRY(255),TDEW(255),SUNSH(1,255),
     *       CLTR(1,255),TMPRAW(32,255),
     *       PRSR(2,255),PRECIP(4,255),
     *       CLOUDS(2,255),MAXTMP(2,255),MINTMP(2,255),SPECI,SNW(3,255),
     *       XTEMP,BLOCK(2,255)
      INTEGER CLDA(6),CLDH(6),VVISI
      CHARACTER(LEN=80) :: STRING,RAWSTG 
      CHARACTER(LEN=10) :: CDATE,JDATE
      CHARACTER(LEN=8) :: SUBSET,RAWRPT(255),CTEMP,CALL(51)
      CHARACTER(LEN=7) :: CPWX(3),LON
      CHARACTER(LEN=6) :: LAT,SLP
      CHARACTER(LEN=5) :: ELEV,ALT,VIS
      CHARACTER(LEN=4) :: CPCP01,CPCP03,CPCP06,CPCP24,TYPE
      CHARACTER(LEN=3) :: CHCLDA(6),CHCLDH(6),DIR,SPEED,GUST,SUN 
      CHARACTER(LEN=3) :: CDRY,CDEW,MAX6,MAX24,MIN6,MIN24,VVIS,
     *                    SNDPTH,SNDUR,SNFALL,SAE,SEE,SAW,SEW
      CHARACTER(LEN=2) :: DAY,HR,MIN
      INTEGER :: LUBFR,IDATE,IRET,NRET,KK,J,JJ,IEOF,
     *           IYR,IMO,IDA,IHR,NDATE,IERR,IUNIT,TMPTBL,TABLE,
     *           RAW,IMAX6,IMAX24,IMIN6,IMIN24

      REAL,EXTERNAL :: KELVIN_F
      REAL,EXTERNAL :: METER_FT

      EQUIVALENCE (CALL,CLTR) 
      EQUIVALENCE (XTEMP,CTEMP) 
      DATA LUBFR/42/IUNIT/30/TABLE/60/RAW/70/TMPTBL/80/

C     ***************************************************************** 
      CALL W3TAGB('HRLYTBL',2000,0335,0069,'OSD211') 
      CALL GETDATE(IUNIT,IYR,IMO,IDA,IHR,NDATE,IERR)
      CALL OPENBF(LUBFR,'IN',LUBFR) 
      CALL DATELEN(10)

      CALL READMG(LUBFR,SUBSET,IDATE,IRET)
      PRINT *,'IDATE ',IDATE,'  IRET ',IRET

      WRITE(TABLE,10) NDATE,IHR
   10 FORMAT(' ','MDL HOURLY METAR TABLE VALID ',I10,'  HOUR: ',I2,'Z')
 
      WRITE(RAW,20) NDATE,IHR
   20 FORMAT(' ','MDL RAW METAR REPORTS VALID ',I10,'  HOUR: ',I2,'Z')

      WRITE(*,40) NDATE,IHR
   40 FORMAT(' ','MDL DIAGNOSTIC MESSAGES VALID ',I10,'  HOUR: ',I2,'Z')

      WRITE(CDATE,50) NDATE
   50 FORMAT(I10)

      IF(NDATE.NE.IDATE) THEN
        WRITE(*,60) NDATE,IDATE
   60   FORMAT(//,'!! WARNING !! DATE FROM NCEP FILE ',I10,
     *  'DOES NOT MATCH THE DATE IN THE BUFR FILE !! ',I10,/)

C        THE FOLLOWING LINES ARE INSERTED AS A SUGGESTION TO
C        ADDING LOGIC TO TEST FOR DATE PROBLEMS.  WHEN TESTING
C        CODE IT IS OK IF THE DATE IN THE BUFR FILE DOES NOT MATCH
C        THE CURRENT DATE.  THE CODE SHOULD STOP IF THIS CONDITION
C        OCCURS DURING AN OPERATIONAL RUN.
C          IF(RUN.EQ.TEST) THEN OK
C            ELSE
C          IF(RUN.EQ.PROD) THEN
C            STOP 60
C            WRITE(*,60) IDATE
C            FORMAT(/,'STOP 60  BAD DATE IN BUFR FILE DO NOT PROCESS')
C          END IF

      END IF

      WRITE(*,70) NDATE
   70 FORMAT(//,"BEGIN QUALITY CONTROL MESSAGES FOR ",I10,":",/)

C            PRINT TABLE HEADER

      WRITE(TABLE,90) 
   90 FORMAT('CALL    :TYPE:  LAT :   LON :TIME:TMP:DEW: ',
     1'PRWX1 : PRWX2 :PRWX3  : VIS :WDR:WSP:GST:  MSL : ALT :',  
     2'CA1:CH1:CA2:CH2:CA3:CH3:CA4:CH4:CA5:CH5:CA6:CH6:1PCP:3PCP',
     3':6PCP:24PP:SUN:MX6:MN6:X24:N24:SND:SNF:SAE:SEE:SAW:SEW:')

  100 CALL READSB(LUBFR,IRET)
      IF(IRET.NE.0) GO TO 200      

C        1         2         3         4         5         6         7 
C23456789012345678901234567890123456789012345678901234567890123456789012

C **********************************************************************
C             GET TIME OF REPORT 
C **********************************************************************

      DO JJ=1,3
        RPTIME(JJ,1)=0.1E+12
      ENDDO
      STRING = ' DAYS HOUR MINU ' 
      CALL UFBINT(LUBFR,RPTIME,3,255,NRET,STRING) 

      CALL REPORT_TIME(RPTIME,DAY,HR,MIN)

C **********************************************************************
C             BLOCK STATION NUMBER - CURRENTLY NOT BEING USED
C **********************************************************************

C     DO JJ=1,2
C       BLOCK(JJ,1)=0.1E+12
C     ENDDO
C     STRING = ' WMOB WMOS ' 
C     CALL UFBINT(LUBFR,BLOCK,2,255,NRET,STRING) 

C     WMO=NINT(BLOCK(1,1)*1000.0 + BLOCK(2,1))
C     PRINT *,"BLOCK STATION NUMBER ",BLOCK(1,1),BLOCK(2,1)

C **********************************************************************
C             GET STATION LOCATION AND TYPE 
C **********************************************************************

      DO JJ=1,5
        GEOG(JJ,1)=0.1E+12
      ENDDO
      STRING = ' CLAT CLON SELV THRPT AUTO ' 
      CALL UFBINT(LUBFR,GEOG,5,255,NRET,STRING)

      CALL GEOGRAPHY(GEOG,LAT,LON,ELEV,TYPE,SPECI)

C        IF THE REPORT IS A SPECIAL, SKIP THE PROCESSING AND DO
C        NOT SAVE IN THE OUTPUT FILES.  PROCEED TO THE NEXT REPORT.

      IF (SPECI.EQ.1.0) THEN
        WRITE(*,120) CALL(1),HR,MIN
  120   FORMAT(' THIS IS A SPECIAL DO NOT SAVE ',A8,'REPORT TIME: ',
     *  A2,A2)
        GO TO 100
      END IF


C **********************************************************************
C             GET REPORT IDENTIFIER
C **********************************************************************

      CLTR(1,1)=0.1E+12
      CALL UFBINT(LUBFR,CLTR,1,255,NRET,' RPID ')

C       CLTR HAS BEEN EQUIVALENCED TO CALL.  CHECK ON THE
C       FIRST CHARACTER IN CALL TO SEE IF THE STATION IS IN
C       A DESIRED GEOGRAPHIC LOCATION (CANADA (C), UNITED STATES (K,P)
C       MEXICO (M), CARIBBEAN (T)), OR IF IT IS ONE OF THE 9 RUSSIAN
C       SITES

      IF ((CALL(1)(:1).NE."K").AND.(CALL(1)(:1).NE."P").AND.
     &    (CALL(1)(:1).NE."M").AND.(CALL(1)(:1).NE."C").AND.
     &    (CALL(1)(:1).NE."T").AND.(CALL(1)(:1).NE."N").AND.
     &    (CALL(1)(1:4).NE."UEEE").AND.
     &    (CALL(1)(1:4).NE."UELL").AND.
     &    (CALL(1)(1:4).NE."UHHH").AND.
     &    (CALL(1)(1:4).NE."UHMA").AND.
     &    (CALL(1)(1:4).NE."UHMD").AND.
     &    (CALL(1)(1:4).NE."UHMM").AND.
     &    (CALL(1)(1:4).NE."UHNN").AND.
     &    (CALL(1)(1:4).NE."UHPP").AND.
     &    (CALL(1)(1:4).NE."UHSS")) GO TO 100 

C     WRITE(6,130)CALL(1),LAT,LON,TYPE
C 130 FORMAT('REPORT ID, AND LAT/LON, TYPE: ',A8,1X,A6,1X,A7,1X,A4)

C **********************************************************************
C             GET RAW REPORT
C **********************************************************************

      RAWSTG= ' RRSTG '
      CALL UFBREP(LUBFR,TMPRAW,32,255,NRET,RAWSTG)
C
C        1/12/01 - RAWRPT WAS DIMENSIONED 32, EVEN THOUGH YOU LOOP 
C        THROUGH THE J COMPONENT FIRST.  MEXICAN REPORTS THAT GOT
C        LUMPED TOGETHER CAUSE OF A MISSING END-OF-MESSAGE WERE
C        OVERFLOWING THIS ARRAY AND IN TURN MESSING UP THE CALL LETTER
C        ARRAY.  IT LOOKS LIKE RAWRPT SHOULD HAVE BEEN DIMENSIONED 255
C        SINCE THAT'S THE ORDER THEY STORE THE DATA IN.
C
      IF(NRET.GT.0) THEN
        KK=0
        DO WHILE (KK.LT.NRET)
           KK=KK+1
           XTEMP=TMPRAW(1,KK)
           RAWRPT(KK)=CTEMP
        END DO 

      WRITE(RAW,140) (RAWRPT(J),J=1,NRET)
  140 FORMAT(32A8)
      ENDIF

C **********************************************************************
C             GET TEMPERATURE AND DRY BULB TEMPERATURE IN K 
C **********************************************************************

      DO JJ=1,2
        TEMPTD(JJ,1)=0.1E+12
      ENDDO
      STRING = ' TMDB TMDP '
      CALL UFBINT(LUBFR,TEMPTD,2,255,NRET,STRING) 

      CALL TEMPERATURE(TEMPTD,CDRY,CDEW,TDRY,TDEW)

C **********************************************************************
C            GET 6 HR MAX/MIN TEMPERATURE AND 24 HR MAX/MIN TEMPERATURE
C **********************************************************************

C            RETRIEVE MAXIMUM TEMPERATURES

      DO KK=1,2
        DO JJ=1,2 
          MAXTMP(KK,JJ)=0.1E+12
        ENDDO
      ENDDO

      STRING = ' .DTHMXTM MXTM '
      CALL UFBINT(LUBFR,MAXTMP,2,255,NRET,STRING) 

C            RETRIEVE MINIMUM TEMPERATURES

      DO KK=1,2
        DO JJ=1,2 
          MINTMP(KK,JJ)=0.1E+12
        ENDDO
      ENDDO

      STRING = ' .DTHMITM MITM '
      CALL UFBINT(LUBFR,MINTMP,2,255,NRET,STRING) 

      CALL MAXIMUM_TEMP(MAXTMP,MAX6,MAX24,IMAX6,IMAX24,CALL,IHR)
      CALL MINIMUM_TEMP(MINTMP,MIN6,MIN24,IMIN6,IMIN24,CALL,IHR)

C **********************************************************************
C             GET HORIZONTAL VISIBILITY AND VERTICAL VISIBILITY
C **********************************************************************

      DO JJ=1,3
        VISI(JJ,1)=0.1E+12
      ENDDO
      STRING = ' HOVI VTVI .REHOVI '
      CALL UFBINT(LUBFR,VISI,3,255,NRET,STRING) 
*      PRINT *,CALL(1),VISI(1,1),VISI(2,1),VISI(3,1)

      CALL VISIBILITY(VISI,VIS,VVIS,VVISI)

C **********************************************************************
C             GET PRESENT WEATHER, UP TO 3 REPORTS ALLOWED
C **********************************************************************

      DO JJ=1,3
        PRWX(1,JJ)=0.1E+12
        CPWX(JJ)='       '
      ENDDO
      STRING = ' PRWE '
      CALL UFBINT(LUBFR,PRWX,3,255,NRET,STRING) 
CINTEL
C        LIMIT PRESENT WEATHER TO 3 CODES.
      IF(NRET.GT.3)NRET=3
CINTEL
      CALL WEATHER(PRWX,CPWX,NRET,CALL,HR,MIN)

C **********************************************************************
C             GET CLOUD AMOUNTS AND CLOUD HEIGHT, 6 REPORTS ALLOWED
C **********************************************************************

      DO KK=1,2
        DO JJ=1,255 
          CLOUDS(KK,JJ)=0.1E+12
        ENDDO
      ENDDO
      STRING = ' CLAM HOCB '
 
      CALL UFBINT(LUBFR,CLOUDS,2,255,NRET,STRING) 

C     IF ((CALL(1)(1:4).EQ.'KIAG').OR.(CALL(1)(1:4).EQ.'KMTC'))
C     PRINT *,'CLOUD ARRAY FOR ',CALL(1)(1:4),CLOUDS
C     END IF
      CALL CLOUD(CHCLDA,CHCLDH,CLDA,CLDH,CLOUDS,TYPE,VVISI,VVIS,NRET)
C **********************************************************************
C             GET SUNSHINE
C **********************************************************************

      SUNSH(1,1)=0.1E+12
      STRING = ' TOSS '
      CALL UFBINT(LUBFR,SUNSH,1,255,NRET,STRING)

      CALL SUNSHINE(SUNSH,SUN,IHR)
 
C **********************************************************************
C             GET WIND DIRECTION AND SPEED, AND WIND GUSTS 
C **********************************************************************

      DO JJ=1,3
        WIND(JJ,1)=0.1E+12
      ENDDO

      STRING = ' WDIR WSPD MXGS ' 
      CALL UFBINT(LUBFR,WIND,3,255,NRET,STRING) 

      CALL WINDS(WIND,DIR,SPEED,GUST,CALL)

C **********************************************************************
C             GET 1 HR, 3 HR, 6 HR, AND 24 HR PRECIPITATION AMOUNTS
C **********************************************************************

      DO JJ=1,4
        PRECIP(JJ,1)=0.1E+12
      ENDDO
      STRING = ' TP01 TP03 TP06 TP24 ' 
      CALL UFBINT(LUBFR,PRECIP,4,255,NRET,STRING) 
      
      CALL PRECIPITATION(PRECIP,CPCP01,CPCP03,CPCP06,CPCP24,IHR,TYPE,
     * CALL)


C **********************************************************************
C             GET ALTIMETER SETTING AND PRESSURE REDUCED TO MSL
C **********************************************************************

      DO JJ=1,2
        PRSR(JJ,1)=0.1E+12
      ENDDO
      STRING = ' ALSE PMSL ' 
      CALL UFBINT(LUBFR,PRSR,2,255,NRET,STRING) 
 
      CALL PRESSURE(PRSR,SLP,ALT)


C **********************************************************************
C             GET SNOW DEPTH AND SNOW FALL INFORMATION         
C **********************************************************************

      DO JJ=1,3
        SNW(JJ,1)=0.1E+12
      ENDDO
      STRING = ' .DTMDOFS DOFS TOSD ' 
      CALL UFBINT(LUBFR,SNW,3,255,NRET,STRING) 
C     PRINT *,CALL(1),SNW 
 
      CALL SNOW(SNW,SNDPTH,SNFALL,SNDUR)

C **********************************************************************
C           PREPARE TABLE FOR SATELLITE DATA - CURRENTLY NOT AVAILABLE
C **********************************************************************

      SAE='   '
      SEE='   '
      SAW='   '
      SEW='   '

C **********************************************************************

      CALL TEMPQC(TDRY,TDEW,CDRY,CDEW,MAX6,MAX24,IMAX6,IMAX24,
     * MIN6,MIN24,IMIN6,IMIN24,CALL,LAT,LON,CDATE,IHR)

C        FORMAT OUTPUT IN BUFR UNITS  
C
C          CONVERT THE BUFR REAL VALUES BACK INTO THE ORIGINAL
C          METAR VALUES.  WITH THE EXCEPTION OF TEMPERATURE, ALL 
C          CONVERSIONS USE THE NCEP HANDBOOK VALUES

      WRITE(TMPTBL,150) CALL(1),TYPE,LAT,LON,HR,MIN,CDRY,CDEW,
     *CPWX(1),CPWX(2),CPWX(3),VIS,DIR,SPEED,GUST,SLP,ALT,    
     *CHCLDA(1),CHCLDH(1),CHCLDA(2),CHCLDH(2),
     *CHCLDA(3),CHCLDH(3),CHCLDA(4),CHCLDH(4),
     *CHCLDA(5),CHCLDH(5),CHCLDA(6),CHCLDH(6),
     *CPCP01,CPCP03,CPCP06,CPCP24,SUN,MAX6,MIN6,MAX24,MIN24,
     *SNDPTH,SNFALL,SAE,SEE,SAW,SEW
  150 FORMAT(A8,':',A4,':',A6,':',A7,':',A2,A2,2(':',A3),':',
     *3(A7,':'),A5,':',A3,':',A3,':',A3,':',A6,':',A5,':',
     *6(A3,':',A3,':')
     *,4(A4,':'),A3,':',4(A3,':'),2(A3,':'),
     *4(A3,':'))

      GO TO 100

  200 CALL READMG(LUBFR,SUBSET,IDATE,IEOF)
      IF(IEOF.EQ.0) GO TO 100 

      PRINT 300,IEOF,IDATE   
  300 FORMAT(//,10X,'END-OF-FILE, IDATE = ',I4,I10,/) 
C   
      WRITE(TMPTBL,400)IHR 
  400 FORMAT('ZZZZZZZZ: END-OF-HOUR ',I2.2,'Z')

      WRITE(RAW,500)IHR 
  500 FORMAT('ZZZZ: END-OF-HOUR ',I2.2,'Z')

C     ENDFILE 80
      ENDFILE 70

      CALL W3TAGE('HRLYTBL') 

C     END PROGRAM HRLYTBL 
      STOP
      END
