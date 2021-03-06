C$$$  MAIN PROGRAM DOCUMENTATION BLOCK  ***                             
C                                                                       
C MAIN PROGRAM: GFSMCXTX
C   PRGMMR: COSGROVE         ORG: OST22       DATE: 2002-04-06
C                                                                       
C ABSTRACT:  GENERATES THE GFS EXTENDED TEXT BULLETIN CONTAINING          
C            CALIBRATED OBJECTIVE MAX/MIN GUIDANCE FOR COOP AND     
C            RFC STATIONS. FORECASTS ARE GIVEN EVERY 12 HOURS             
C            FROM 24 TO 192 HOURS AFTER INITIAL RUN TIME.               
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   03-12-18  COSGROVE - USED FECN21TX AS A TEMPLATE FOR THIS NEW CODE.
C   05-01-27  COSGROVE - MODIFIED CODE TO HANDLE 12000 STATIONS
C   05-01-29  COSGROVE - MODIFIED CODE TO WORK AT 00 AND 12Z CYCLES
C                        AND USE THE 202X50 IDS INSTEAD OF 202X45.
C   05-02-09  COSGROVE - MODIFIED TO CORRECTLY HANDLE THE AWIPS ID.
C   05-03-02  COSGROVE  CHANGED THE $ RECORD SEPARATORS TO CHAR(30)
C                       STOPPED FILLING THE FIRST CHARACTER OF MSG
C                       TO ^, ADDED A RECORD SEPARATOR TO THE DATE 
C                       LINE.  ALL OF THIS IS TO SEND IT NTC.  
C                       CHANGED CALL TO WRMSGA TO WRMSGNTC.
C   15-02-04  SCALLION  CHANGED MAXSTA TO 48,000.
C   15-08-06  SCALLION  CHANGED NEW TO BE 1 IN ORDER TO READ IN NEW
C                       LINKED MESONET SITES.
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     PROGRAM GFSMCXTX                                                  
C                                                                       
C        PURPOSE                                                        
C            GENERATES THE GFS EXTENDED TX BULLETIN CONTAINING         
C            CALIBRATED OBJECTIVE MAX/MIN GUIDANCE FOR COOP      
C            STATIONS. FORECASTS ARE GIVEN EVERY 12 HOURS             
C            FROM 24 TO 192 HOURS AFTER INITIAL RUN TIME.               
C                                                                       
C        DATA SET USE                                                   
C        READ(5...      - STANDARD INPUT 
C        WRITE(6...     - STANDARD OUTPUT
C        FORT.5         - COOP STATION LIST 
C        FORT.10        - NCEP STANDARD DATE FILE (INPUT)
C        FORT.27        - MDL STATION DICTIONARY 
C        FORT.45        - MDL MOS CONSTANT FILE (INPUT)
C        FORT.48        - MDL MOS 2000 FCST FILE (INPUT)
C        FORT.60        - TELETYPE MESSAGE (OUTPUT)
C        FORT.65        - TRANSMISSION FILE (OUTPUT)
C
C                                                                       
C        VARIABLES
C
C
C        SUBROUTINE GET_NCEPDATE CALLING ARGUMENTS:
C                 IUNIT=FILE NUMBER(10) USED FOR THE NCEP DATE FILE
C                  IERR=ERROR RETURN CODE.
C                 LDATE=CURRENT OPERATIONAL DATE IN MDL FORMAT
C                   MDG=CURRENT DAY, 2 DIGITS
C                   MHG=CURRENT HOUR, 2 DIGITS
C                   MOG=CURRENT MONTH, 2 DIGITS
C                   MYG=CURRENT YEAR, 4 DIGITS
C
C        SUBROUTINE RDWMO CALLING ARGUMENTS:
C                 JUNIT=FILE NUMBER(5) USED TO ACCESS MRF STATION
C                       DIRECTORY FILE
C                  LIST=LIST OF NMOSTA CALL LETTERS OF STATIONS,
C                       LEFT JUSTIFIED (8 CHARACTERS)
C                MAXSTA=MAXIMUM NUMBER OF STATIONS (ACTUALLY 2* THE
C                       NUMBER OF SITES IN THE DICTIONARY THE CODE READS)
C                 NHEAD=NUMBER OF WMO STATION CALL LEADER HEADERS
C                       (EX. MRFFOX42AZ)
C                NMOSTA=NUMBER OF STATION CALL LETTERS READ FROM INPUT LIST 
C                  NWMO=NUMBER OF STATIONS IN WMO CALL LETTER ARRAY,
C                       WMO(I)
C                   WMO=LIST OF WMO HEADERS(CHARACTER *8)
C
C**       SUBROUTINE W3DOXDAT CALLING ARGUMENTS:
C**	     IDAT(8)=NCEP ABSOLUTE DATE AND TIME ARRAY
C                    1 - 4 DIGIT YEAR
C		     2 - MONTH OF YEAR
C                    3 - DAY OF MONTH
C                    4 - HHMM TIME ZONE DIFFERENCE FROM UTC (-0500)
C		     5 - HOUR OF DAYI 24-HR CLOCK
C                    6 - MINUTES OF HOUR
C                    7 - SECONDS OF MINUTE
C                    8 - MILLISECONDS OF SECOND
C                JDOW=INTEGER DAY OF WEEK (1 IS SUNDAY)
C		 JDOY=INTEGER DAY OF YEAR (1 IS JAN 1)
C                JDAY=INTEGER JULIAN DAY (FROM JAN 1, 4713BC)
C      
C        ADDITIONAL VARIABLES:
C**             CBULHD=BULLETIN HEADER  (CHAR*11)
C**            CDAY(8)=CHARACTER *3 ARRAY OF CALENDER DAYS(DAYS OF WEEK)
C**          CHARDY(7)=CHARACTER *3 ARRAY OF DAYS OF THE WEEK
C**             IDA(8)=ARRAY USED TO HELP CALCULATE CALENDER DAY (CDAY)
C**              IDYWK=INTEGER DAY OF THE WEEK (1 - 7)
C**             IMO(8)=ARRAY USED TO HELP CALCULATE CALENDER DAY (CDAY)
C**                INC=TIME INCREMENT
C**             IYR(8)=ARRAY USED TO HELP CALCULATE CALENDER DAY (CDAY)
C**                JDY=JULIAN DATE USED TO HELP CALCULATE CDAY
C**         KOUNT( , )=INITIALLY SET TO ZERO, INCREASED BY 1            
C**                    WHERE A FORECAST IS ENTERED IN THAT LINE.        
C**                    1ST DIMENSION = LINE                             
C**                    2ND DIMENSION = GREATER THAN OR EQUAL TO NMOSTA
C**             LBULHD=NUMBER OF CHARACTERS IN BULLETIN HEADER
C**               LINE=NUMBER OF LINES REQUIRED PER STATION
C**             LOC(8)=MSG POSITION ARRAY FOR VERTICAL LINES
C**            LOCD(8)=MSG POSITION ARRAY FOR FORECAST VALUES
C**            MM00( )=ARRAY USED TO HOLD THE MDL IDENTIFIERS FOR THE
C**                    MAX/MIN TEMPERATURE FORECAST 00Z CYCLE.
C**            MM12( )=ARRAY USED TO HOLD THE MDL IDENTIFIERS FOR THE
C**                    MAX/MIN TEMPERATURE FORECAST 12Z CYCLE.
C**              MO( )=ARRAY OF MONTH NUMBER(1-12) USED WITH W3SF13 FOR
C**                    CALCULATION OF THE JULIAN DAY.  
C**               MREC=ASCII CHARACTER FOR RECORD SEPARATOR
C**                    NECESSARY FOR TRANSMISSION
C**          MSG(80, )=AREA INTO WHICH TELETYPE MESSAGE IS BUILT        
C**                    LAST DIMENSION=NOLINE SHOULD BE GE LINE*NMOSTA+5 
C**               MYGA=LAST TWO DIGITS OF CURRENT YEAR 
C**                    GOOD FOR YEAR 2000 AND BEYOND
C**              NBLAK=BLANK CHARACTER 
C**              NBSTA=BEGINNING STATION NUMBER (USED TO DELINEATE 
C**                    BULLETINS IN WRMSGA) 
C**              NESTA=ENDING STATION NUMBER (USED TO DELINEATE 
C**                    BULLETINS IN WRMSGA) 
C**              NFILL=CHARACTER USED AS FILL IN THE FOUS14--IS TRANS- 
C**                    PARENT TO THE AFOS OR TTY SYSTEM.  
C**             NMOSTA=NUMBER OF STATIONS FOR WHICH BULLETIN IS PREPARED
C**                    RETURNED FROM RDLSTA 
C**             NOCHAR=1ST DIMENSION OF MSG( , )-MAX NUMBER OF CHARACTER
C**             NOLINE=2ND DIMENSION OF MSG( , )-MAX NUMBER OF LINES
C**             NOHEAD=NUMBER OF LINES IN THE HEADER, INCLUDING BLANK
C**                    LINES.
C**               NOUT=OUTPUT TRANSMISSION FILE UNIT NUMBER (=65)
C**              NTEMP=NUMBER OF STATIONS RUN (USUALLY EQUAL TO NMOSTA)
C**              NUNIT=OUTPUT PRINT(READABLE) FILE UNIT NUMBER (=60)
C**               VBAR=ASCII CHARACTER "BAR" (SEPARATOR LINE)
C**                    NECESSARY FOR TRANSMISSION
C
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C      UNIQUE: RDWMO(CALLS RDLSTA),RDLNK,WRMSGA
C
C     LIBRARY:                                                          
C       W3LIB: W3DOXDAT    
C       TDLLIB:CHNGDATE,PUTMOS,PUTCHAR,PRMSG,PUTQ,RDLSTA
C       MDLLIB:GET_NCEPDATE,READ_MOSDA
C       
C      PUTCHAR=SUBROUTINE WHICH COPIES CHARACTER STRINGS FROM
C              STRING TO ANOTHER; AVOIDS HAVEING TO US DO LOOPS.
C
C       PUTMOS=SUBROUTINE WHICH CONVERTS A NUMBER (INTEGER OR
C              REAL), CONVERTS THE NUMBER TO A CHARACTER STRING
C              WHICH CAN THEN BE COPIED TO THE BULLETIN MESSAGE.
C
C         PUTQ=SUBROUTINE WHICH ATTACHES CONTROL CHARACTERS AT THE
C              END OF EACH RECORD FOR AFOS TRANSMISSION.
C              (CALLED BY WRMSGA)
C
C       RDLSTA=READS CHARACTER DATA WITH A GIVEN FORMAT.
C              ASSUME A MAX RECORD LENGTH OF 80 BYTES.
C
C        RDLNK=SUBROUTINE TO CHECK FOR UPDATED CALL LETTERS IN
C              THE STATION DICTIONARY
C
C   READ_MOSDA=SUBROUTINE TO GET FORECASTS FROM RANDOM ACCESS FILE
C
C        PRMSG=PRINTS AN ASCII(SEQUENTIAL FILE) OUTPUT OF MRF
C              STATION BULLETINS
C
C       WRMSGA=GENERATES AN ASCII(DIRECT ACCESS "FORMATTED") FILE
C              WITH EBSCDIC CONTROL CHARACTERS (TRANSMISSION 
C              COMPONENTS) OF MRF STATION BULLETINS
C
C   EXIT STATES:   (STOPS OCCUR IN MAIN UNLESS OTHERWISE STATED)     
C     COND =   0 - SUCCESSFUL RUN
C          =   3 - CATASTROPHIC ERROR IN READ_MOSDA
C          =  30 - NUMBER OF STATIONS IN LIST ARE EXCEEDED
C                  STOP OCCURS IN RDWMO
C          =  70 - NO MOS FORECASTS WERE AVAILABLE (KUT=0)         
C          = 100 - PROBLEM READING MRF MOS 2000 FORECAST FILE
C          = 110 - PROBLEM READING CONSTANT FILE
C          = 115 - UNABLE TO READ THE STANDARD NCEP DATE FILE
C                                                                       
C REMARKS:                                                              
C     
C     SET MAXSTA TO TWICE THE NUMBER OF STATIONS ON INPUT RA
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  IBM SP
C   
C$$$
      PROGRAM GFSMCXTX

      PARAMETER(NOCHAR=80,MAXSTA=48000,LINE=1,NUMDAY=12)
      PARAMETER(NOHEAD=3,ND7=54)

      INTEGER IOPT,JBLOCK(MAXSTA),JNERR,NDATE,NSTA,KUNIT,
     * KOUNT(LINE,MAXSTA),IDA(NUMDAY),MO(NUMDAY),IYR(NUMDAY),
     * MM00(22),MM12(15),LOC(11),LOCD(23),ID(4),ISW,NERR,JUNIT,IUNIT,
     * LUNIT,NMOSTA,LDATE,KUT,IFLAG,IERR,MDG,MHG,MOG,MYG,NHEAD,
     * JDY,INC,JWBAN(MAXSTA),NWMO(MAXSTA),NUNIT,
     * NOUT,LBULHD,NESTA,NBSTA,IDYWK,IDAT(8),JDAY,IS0(ND7),
     * IS1(ND7),IS2(ND7),IS4(ND7),INDEX(MAXSTA,5)

      REAL FCST(MAXSTA),RNORM(MAXSTA)

      CHARACTER*1 MSG(NOCHAR,MAXSTA*LINE+NOHEAD),NBLAK,NFILL,VBAR
      CHARACTER*1 MREC
      CHARACTER*3 CDAY(12),CHARDY(7)
      CHARACTER*5 CTYPE,NCAT
      CHARACTER*6 CDESC
      CHARACTER*8 LIST(MAXSTA),CALLS(MAXSTA),CCALL(MAXSTA,6)
      CHARACTER*18 CBULHD,WMO(MAXSTA)
      CHARACTER*20 CNAME(MAXSTA)
      CHARACTER*80 CFILE,BFILE

      DATA KUNIT/48/,JUNIT/5/,IUNIT/10/,LUNIT/45/,LBULHD/11/,NOUT/65/
     * NBLAK/' '/,NFILL/' '/,NUNIT/60/,KFILD/27/,KFILX/48/,IPTST/0/
     * KFILDO/6/
      DATA CHARDY/'SUN','MON','TUE','WED','THU','FRI','SAT'/
C      DATA CBULHD/'XXXXXX KWNO'/
      DATA NCAT/'00000'/,NEW/1/

      DATA MM00/202150008,202250008,202150008,202250008,
     * 202150008,202250008,202150008,202250008,202150008,202250008,
     * 202150008,202250008,202150008,202250008,202150008,
     * 202250008,202150008,202250008,202150008,202250008,
     * 202150008,202250008/
      DATA MM12/202250008,202150008,202250008,202150008,
     * 202250008,202150008,202250008,202150008,202250008,
     * 202150008,202250008,202150008,202250008,202150008,
     * 202250008/
      DATA LOC/17,25,33,41,49,57,65,73,81,89,97/
      DATA LOCD/12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,
     *          76,80,84,88,92,96,100/

C        HOUR PROJECTIONS ARE: 
C        30,42,54,66,78,90,102,114,126,138,150,162,174,186,198
C         ALSO AVAILABLE ARE 210,222,234,246,258,270,282, FOR 00Z BUT
C         WE'RE NOT PUTTING THEM IN THE MESSAGE.  I DID PUT THE IDS
C         IN MM00 IN CASE WE EVER WANT THEM.

      DATA NDATE/0/,MDATE/0/,LDATE/0/,NHEAD/0/,IDYWK/0/
      DATA IOPT/0/,NMOSTA/0/,ISW/1/,NESTA/0/
      DATA IDAT/0,0,0,-0500,0,0,0,0/

      CALL W3TAGB('GFSMCXTX',2002,0096,0078,'OST22')                  

      ND5=MAXSTA

C   
C     SET VBAR TO CHAR(124) WHICH IS |
      VBAR=CHAR(124)
   
      MREC=CHAR(30)

C        INITIALIZE ARRAYS
      DO 112 M=1,MAXSTA
	NWMO(M)=0
	FCST(M)=0.
      DO 112 K=1,LINE
        KOUNT(K,M)=0
  112 CONTINUE


C        READ IN THE MOS 2000 DATA FILE DIRECT ACCESS 
C        FILE, "CFILE", AND THE BULLETIN "BFILE" OUTPUT 
C        FILENAME -- THIS WAS SET UP AT ONE TIME BUT IS NOT
C        USED ANYMORE.  RIGHT NOW IT JUST READ THREE BLANK
C        LINES IN THE STATION LIST FILE - FORT.5

      READ(JUNIT,1000) CFILE
      READ(JUNIT,1000) BFILE
 1000 FORMAT(A80)

C        READ BULLETIN HEADERS AND STATIONS                             
      CALL RDWMO(JUNIT,NMOSTA,LIST,WMO,NWMO,NHEAD,MAXSTA)
      WRITE(KFILDO,91)NMOSTA
 91   FORMAT(1X,'NMOSTA=',I10)
      NOLINE=(NMOSTA*LINE)+NOHEAD
      KUT=0

C
C        CALL RDLNK TO RETURN THE NEWEST CALL LETTER LINKS FOR THE
C        STATIONS IN LIST.  THE NEWEST CALL LETTER LIST WILL BE 
C        IN CCALL(J,1) 
C
      CALL RDLNK(KFILD,KFILDO,NEW,LIST,CCALL,NMOSTA,MAXSTA)  
C
C        FILL THE MESSAGE ARRAY WITH BLANKS EXCEPT FOR THE 1ST CHARACTER
      DO 110 M=1,NOLINE
      DO 110 K=1,NOCHAR
      MSG(K,M)=NBLAK
      IF(K.EQ.1)MSG(K,M)=NFILL
  110 CONTINUE

C        SET THE NUMBER OF STATIONS TO PROCESS;
C        USUALLY WILL BE NMOSTA
      NTEMP=NMOSTA
      WRITE(KFILDO,91)NTEMP
      WRITE(KFILDO,93)NHEAD
 93   FORMAT(1X,'NHEAD=',I5)

C        READ NCEP STANDARD DATE FILE
C        PUT DATE IN MDL FORMAT
C        LDATE=(MYG*1000000)+(MOG*10000)+(MDG*100)+MHG
      CALL GET_NCEPDATE(IUNIT,MYG,MOG,MDG,MHG,LDATE,IERR)
      IF(IERR.NE.0) THEN
      CALL W3TAGE('MDL_FECN21TX') 
        STOP 115 
      ENDIF

C        FIND VALID DATES FOR HEADINGS.
C        FOR 8 CALENDER DATES 
      MDATE=LDATE
C        CONVERT 1995 TO 95 (FOR DISPLAY PURPOSES)
       MYGA=MOD(MYG,100)
      IF (MHG.EQ.0) THEN
       IDA(1)=MDG
       MO(1)=MOG
C        FULL YEAR EX. 1995, 2005, 2025 TO GO INTO IYR ARRAY
       IYR(1)=MYG
       M=0
C        RETURNS JULIAN DAY
C        RETURNS INTEGER DAY OF WEEK 
	IDAT(1)=MYG
	IDAT(2)=MOG
	IDAT(3)=MDG
	CALL W3DOXDAT(IDAT,IDYWK,JDY,JDAY)
        CDAY(1)=CHARDY(IDYWK)
        IS=2
        NBAR=8
      ELSE
        IS=1
        NBAR=7
      ENDIF


      DO 116 I=IS,8
        CALL CHNGDATE(-MDATE,24,MDATE)
        IDA(I)=MOD(MDATE,10000)/100
        MO(I)=MOD(MDATE,1000000)/10000
        IYR(I)=MDATE/1000000
        M=0
	IDAT(1)=IYR(I)
	IDAT(2)=MO(I)
	IDAT(3)=IDA(I)
	CALL W3DOXDAT(IDAT,IDYWK,JDY,JDAY)
        CDAY(I)=CHARDY(IDYWK)
 116  CONTINUE

C*****
C*****   START CONSTRUCTION OF THE 
C*****   BULLETIN:
C*****

C        CREATE THREE LINE HEADER                                       
      CALL PUTCHAR(MREC,MSG(1,2),1)
      CALL PUTCHAR('GFSX-BASED MOS COOP MAX/MIN GUIDANCE',
     *       MSG(2,2),36)
      CALL PUTCHAR('/  /',MSG(41,2),4)
      CALL PUTMOS('MOG',FLOAT(MOG),0.,1.,1,12,MSG(39,2),
     *       2,0,'99',KX)
      CALL PUTMOS('MDG',FLOAT(MDG),0.,1.,1,31,MSG(42,2),
     *       2,2,'99',KX)
      CALL PUTMOS('MYG',FLOAT(MYGA),0.,1.,0,99,MSG(45,2),
     *       2,2,'99',KX)
      CALL PUTCHAR('00 UTC',MSG(51,2),6)
      CALL PUTMOS('MHG',FLOAT(MHG),0.,1.,0,18,MSG(49,2),
     *       2,2,'99',KX)

C        PLACE VALID DAYS
C        FIRST PUT A RECORD SEPARATOR AT THE BEGINNING OF THE LINE
        CALL PUTCHAR(MREC,MSG(1,3),1)
        MPOS=3
        DO 117 J=1,8
	  MPOS=MPOS+8
	  CALL PUTCHAR(CDAY(J),MSG(MPOS,3),3)
	  CALL PUTMOS('DAY',FLOAT(IDA(J)),0.,1.,0,31,MSG(MPOS+4,3),
     *         2,2,'99',KX)
 117    CONTINUE

C        PLACES SEPERATOR LINES IN MESSAGE FOR THE DATES(LINE 3) AND
C        EACH STATION REGARDLESS OF WHETHER DATA EXISTS FOR A GIVEN
C        STATION
	DO 130 LL=3,NOLINE
          DO 120 I=1,NBAR
	    CALL PUTCHAR(VBAR,MSG(LOC(I),LL),1)
 120      CONTINUE
 130    CONTINUE

C        PLACES STATION IDENTIFIER REGARDLESS OF WHETHER
C        DATA EXISTS FOR A GIVEN STATION
      DO 140 K=1,NTEMP
        CALL PUTCHAR(MREC,MSG(1,(K-1)*LINE+4),1)
        CALL PUTCHAR(LIST(K),MSG(2,(K-1)*LINE+4),6)
        KOUNT(1,K)=1
C        ABOVE STATEMENT PROVIDES FOR BLANK LINE.                       
 140  CONTINUE

CCCCCCCCCCC
CCCCCCCCCCC

C        PLACE MIN/MAX FCSTS
      ID(1)=0
      ID(2)=0
      ID(3)=18
      ID(4)=0
      INC=12
      DO 160 K=1,15
        IF (MHG.EQ.00) ID(1)=MM00(K)
        IF (MHG.EQ.12) ID(1)=MM12(K)
	ID(3)=ID(3)+INC
        CALL READ_MOSDA(KFILDO,KFILX,IPTST,ID,LDATE,CCALL,
     *       NMOSTA,FCST,MAXSTA,ND5,ND7,IS0,IS1,IS2,IS4,
     *       INDEX,IER)
        IF(IER.LT.2) KUT=KUT+1
        IF(IER.EQ.3) THEN
          CALL W3TAGE('GFSMCXTX')
          STOP 3
        ENDIF
        DO 150 J=1,NTEMP
          IF (MHG.EQ.00) CALL PUTMOS(LIST(J),FCST(J),0.,1.,-99,999,
     *           MSG(LOCD(K+1)-2,(J-1)*LINE+4),3,0,'999',KOUNT(1,J))
          IF (MHG.EQ.12) CALL PUTMOS(LIST(J),FCST(J),0.,1.,-99,999,
     *           MSG(LOCD(K)-2,(J-1)*LINE+4),3,0,'999',KOUNT(1,J))
 150    CONTINUE
 160  CONTINUE

      IF (KUT.LE.0) THEN
        WRITE(KFILDO,270)
 270    FORMAT(//'NO GFSX MOS FORECASTS FOUND.')
      CALL W3TAGE('GFSMCXTX') 
        STOP 70
      ENDIF

C
C**********************************************************
C        END OF BULLETIN
C**********************************************************

      DO 400 N=1,NHEAD
	NBSTA=NESTA+1
	NESTA=NESTA+NWMO(N)
        CBULHD=WMO(N)
	CALL WRMSGNTC(MSG,NOCHAR,NOLINE,NBSTA,NESTA,LINE,NCAT,KOUNT,
     *       LINE,NTEMP,CBULHD,LBULHD,MDG,MHG,NOHEAD,NOUT,
     *       BFILE,NHEAD)
 400  CONTINUE
C
C        CHANGE THE HEXIDECIMAL VALUE CB BACK TO THE ASCII
C        REPRESENTATION FOR A VERTICAL BAR BEFORE PRINTING.
C         3/2005 - DON'T NEED TO DO THIS ANYMORE BECAUSE WE
C         SET IT TO CHAR(124) NOW
C
C     DO 550 I=1,NOLINE
C       DO 500 J=1,NOCHAR
C         IF (MSG(J,I).EQ.VBAR) MSG(J,I)='|'
C500    CONTINUE
C550  CONTINUE
C
C        PRINT MESSAGE                                                  
C
      CALL PRMSG(MSG,NOCHAR,NOLINE,KOUNT,LINE,NMOSTA,NUNIT,NOHEAD)
C
      CALL W3TAGE('MDL_GFSMCXTX') 
      STOP
      END
