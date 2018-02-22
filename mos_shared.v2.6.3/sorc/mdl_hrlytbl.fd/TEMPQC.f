
C *********************************************************************
      SUBROUTINE TEMPQC(TDRY,TDEW,CDRY,CDEW,MAX6,MAX24,IMAX6,IMAX24,
     * MIN6,MIN24,IMIN6,IMIN24,CALL,LAT,LON,CDATE,IHR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                  
C                                                                    
C SUBPROGRAM:  TEMPQC        QUALITY CONTROLS THE TEMPERATURES      
C   PRGMMR: HUGHES           ORG: W/OSD211     DATE: 96-11-14      
C                                                                 
C ABSTRACT:  QUALITY CONTROLS THE TEMPERATURES AND DEWPOINTS
C            BASED ON REGION AND SEASON.  BY THIS POINT, THE
C            TEMPERATURES HAVE BEEN CONVERTED TO DEGREES FAHRENHEIT. 
C  
C                                                                 
C PROGRAM HISTORY LOG:                                           
C   96-07-11  FOOSE - ADDED QUALITY CONTROL CHECKS FOR TEMPERATURE 
C                     AND DEW POINT.  LIMITS ARE DETERMINED BY REGION
C                     AND SEASON.  THERE IS ALSO A CHECK TO MAKE SURE
C                     THE DEW POINT NEVER EXCEEDS THE TEMPERATURE.
C   96-11-14 HUGHES - THIS SUBROUTINE WAS ADAPTED FROM THE WORK
C                     DONE BY J. FOOSE FOR THE OBS SUBROUTINE IN MOSOBS
C   00-11-30 ALLEN  - FIXED AN ERROR IN THE TEST FOR WHICH REGION THE
C                     STATION IS LOCATED IN.  PREVIOUSLY USED -100
C                     AS REFERENCE LONGITUDE.
C                                                               
C USAGE:                                                       
C                                                             
C   SEE BELOW FOR MDL STANDARDS                              
C                                                           
C     PROGRAM OBS                                          
C                                                         
C        JULY 1989   DALLAVALLE   MDL   NAS9000                         
C                                                                       
C        PURPOSE                                                        
C            DECODES THE SURFACE AVIATION OBSERVATION AND EXTRACTS THOSE
C            ELEMENTS NEEDED BY THE MDL FORECAST EQUATIONS             
C                                                                     
C        DATA SET USE                                                
C            NONE                                                       
C                                                                       
C        VARIABLES                                                      
C                TDRY = TEMPERATURE                           
C                TDEW = DEW POINT TEMPERATURE
C                CDRY = TEMPERATURE 
C                CDEW = DEW POINT TEMPERATURE 
C                MAX6 = 6-HOUR MAX TEMP 
C               MAX24 = 24-HOUR MAX TEMP 
C               IMAX6 = 6-HOUR MAX TEMP 
C              IMAX24 = 24-HOUR MAX TEMP 
C                MIN6 = 6-HOUR MIN TEMP
C               MIN24 = 24-HOUR MIN TEMP
C               IMIN6 = 6-HOUR MIN TEMP 
C              IMIN24 = 24-HOUR MIN TEMP
C                CALL = 24-HOUR MIN TEMP
C                 LAT = 24-HOUR MIN TEMP
C                 LON = 24-HOUR MIN TEMP 
C               CDATE = 24-HOUR MIN TEMP
C                 IHR = HOUR BEING PROCESSED
C                                                                       
C        SUBPROGRAMS CALLED:                                            
C            NONE:                                                   
C                                                                       
C        REMARKS                                                        
C            WHEN THE RUSSIAN SITES WERE ADDED IN 3/2001, THEY WERE
C            ALLOWED TO STAY IN WHATEVER EXISTING REGION THEY FELL INTO.
C            THEREFORE, MOST ARE IN THE CANADA/ALASKA REGION, BUT TWO
C            FALL INTO THE NORTHEAST.  IF THOSE BOUNDS ARE EXCEEDED,
C            WE'LL LOOK INTO MAKING ANOTHER REGION                             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  CRAY4                                                     
C$$$                                                                    
      CHARACTER(LEN=3)  :: CDRY,CDEW,MAX6,MAX24,MIN6,MIN24
      CHARACTER(LEN=6)  :: LAT
      CHARACTER(LEN=7)  :: LON
      CHARACTER(LEN=8)  :: CALL(51) 
      CHARACTER(LEN=10) :: CDATE
      INTEGER           :: IDATE,IREG,ISEASON,IYEAR,IMO,IDAY,ITDLHR,
     *                     LMTST(2,4,5),LMTSD(2,4,5),IHR,TDRY,TDEW,
     *                     IMAX6,IMAX24,IMIN6,IMIN24              
      REAL              :: RLAT,RLON
C
C  FORMAT OF FOLLOWING TWO DATA STATEMENTS:
C  EACH LINE IS A SEPERATE REGION AND CONTAINS THE UPPER LIMIT FOLLOWED
C  BY THE LOWER LIMIT FOR EACH SEASON (WINTER,SPRING,SUMMER,FALL).  
C  THE FIRST DATA STATEMENT CONTAINS LIMITS FOR TEMPERATURE, WHILE
C  THE SECOND FOR DEW POINT.  

      DATA LMTST/
     *102,-26,117,-20,130,25,128,-11,    ! SOUTHEAST US
     *105,-38,126,-40,130,12,128,-25,    ! SOUTHWEST US
     *102,-60,116,-50,130,8,120,-42,     ! NORTHEAST US,UHHH,UHSS
     *105,-62,119,-50,130,0,128,-49,     ! NORTHWEST US
     *81,-80,100,-66,106,-6,94,-67/      ! ALASKA,CANADA,MOST RUSSIAN SITES
      DATA LMTSD/
     *85,-31,85,-25,85,20,85,-16,
     *85,-43,85,-45,85,-7,85,-30,
     *85,-65,85,-55,85,3,85,-47,
     *85,-67,85,-55,85,-5,85,-54,
     *74,-85,74,-71,74,-11,74,-72/

C        GET HOUR/MONTH/DAY                                             

       READ(CDATE,'(I4,I2,I2,I2)')IYEAR,IMO,IDAY,ITDLHR

C      READ IN LAT/LONS (USED TO DETERMINE REGION)
C      MAKE SURE LAT/LON IS NOT BLANK
C      CONVERT CHARACTER LAT/LON TO REAL
C
         READ(LAT,'(F6.2)',IOSTAT=IOS)RLAT
           IF(IOS.NE.0) THEN
             RLAT=99.99
           END IF
         READ(LON,'(F7.2)',IOSTAT=IOS)RLON
           IF(IOS.NE.0) THEN
             RLON=999.99
           END IF

         IF((RLAT.GT.99.).OR.(RLON.GT.999.)) THEN
           PRINT *,"MISSING LATITUDE OR LONGITUDE.  TEMPERATURE",
     *     " CHECKS NOT PERFORMED FOR STATION ",CALL(1) 
           GO TO 100            ! CHECK TEMP VS DEW
         END IF
C
C       CHECK REGION
C
      IF((RLAT.LE.35).AND.(RLON.LE.100))THEN
         IREG=1
      ELSEIF((RLAT.LE.35).AND.(RLON.GT.100))THEN
         IREG=2
      ELSEIF((RLAT.GT.35).AND.(RLAT.LE.50).AND.(RLON.LE.100))THEN
         IREG=3
      ELSEIF((RLAT.GT.35).AND.(RLAT.LE.50).AND.(RLON.GT.100))THEN
         IREG=4
      ELSEIF(RLAT.GT.50)THEN
         IREG=5
      ELSE
           TDRY=999
  	   TDEW=999 
 	 GOTO 94
      ENDIF
 
C      CHECK SEASON
 
       IF((IMO.EQ.12).OR.(IMO.EQ.1).OR.(IMO.EQ.2))THEN
 	 ISEAS=1        ! WINTER
      ELSEIF((IMO.EQ.3).OR.(IMO.EQ.4).OR.(IMO.EQ.5))THEN
 	 ISEAS=2        ! SPRING
      ELSEIF((IMO.EQ.6).OR.(IMO.EQ.7).OR.(IMO.EQ.8))THEN
 	 ISEAS=3        ! SUMMER
      ELSEIF((IMO.EQ.9).OR.(IMO.EQ.10).OR.(IMO.EQ.11))THEN
 	 ISEAS=4        ! FALL 
      ELSE
 	   TDRY=999
 	   TDEW=999
          GOTO 94
       ENDIF
C
C       OBTAIN TEMPERATURE
C
      IF(CDRY.EQ.'   ')THEN
         TDRY=999
         GOTO 93
      ENDIF

C
C      QUALITY CONTROL FOR TEMPERATURE
C
      IF((TDRY.GT.LMTST(1,ISEAS,IREG))
     *  .OR.(TDRY.LT.LMTST(2,ISEAS,IREG)))THEN
         CDRY="   " 
         PRINT *,"TEMPERATURE WAS OUT OF RANGE FOR STATION ",
     *   CALL(1),TDRY," WAS CHANGED TO MISSING."
         TDRY=9999
      ENDIF

      IF(IMAX6.LT.9999) THEN
        IF((IMAX6.GT.LMTST(1,ISEAS,IREG))
     *  .OR.(IMAX6.LT.LMTST(2,ISEAS,IREG)))THEN
           MAX6="   " 
           PRINT *,"6-HOUR MAXIMUM TEMPERATURE WAS OUT OF RANGE FOR ",
     *     "STATION ",CALL(1),IMAX6," WAS CHANGED TO MISSING."
           IMAX6=9999
        ENDIF
      ENDIF
C
      IF(IMAX24.LT.9999) THEN
        IF((IMAX24.GT.LMTST(1,ISEAS,IREG))
     *  .OR.(IMAX24.LT.LMTST(2,ISEAS,IREG)))THEN
           MAX24="   " 
           PRINT *,"24-HOUR MAXIMUM TEMPERATURE WAS OUT OF RANGE FOR",
     *     " STATION ",CALL(1),IMAX24," WAS CHANGED TO MISSING."
           IMAX24=9999
        ENDIF
      ENDIF
C 

      IF(IMIN6.LT.9999) THEN
        IF((IMIN6.GT.LMTST(1,ISEAS,IREG))
     *  .OR.(IMIN6.LT.LMTST(2,ISEAS,IREG)))THEN
           MIN6="   " 
           PRINT *,"6-HOUR MINIMUM TEMPERATURE WAS OUT OF RANGE FOR ",
     *     "STATION ",CALL(1),IMIN6," WAS CHANGED TO MISSING."
           IMIN6=9999
        ENDIF
      ENDIF
C
      IF(IMIN24.LT.9999) THEN
        IF((IMIN24.GT.LMTST(1,ISEAS,IREG))
     *  .OR.(IMIN24.LT.LMTST(2,ISEAS,IREG)))THEN
           MIN24="   " 
           PRINT *,"24-HOUR MINIMUM TEMPERATURE WAS OUT OF RANGE FOR",
     *     " STATION ",CALL(1),IMIN24," WAS CHANGED TO MISSING."
           IMIN24=9999
        ENDIF
      ENDIF

C      OBTAIN DEW POINT
C
  93  IF(CDEW.EQ.'   ')THEN
         TDEW=999
         GOTO 94
      ENDIF

C
C      QUALITY CONTROL FOR DEW POINT
C
        IF((TDEW.GT.LMTSD(1,ISEAS,IREG))
     *  .OR.(TDEW.LT.LMTSD(2,ISEAS,IREG)))THEN
         TDEW=9999
         CDEW="   "
           PRINT *,"DEWPOINT TEMPERATURE WAS OUT OF RANGE FOR ",
     *     "STATION ",CALL(1),TDEW," WAS CHANGED TO MISSING."
         GO TO 94
      ENDIF
C
C      CHECK FOR CONSISTENCY BETWEEN TEMPERATURE AND DEWPOINT
C
  100 IF (TDEW.GT.TDRY) THEN
        PRINT *,"TEMPERATURE AND DEWPOINT INCONSISTENCY, BOTH SET TO", 
     *  " MISSING FOR STATION ",CALL(1)," TDRY=",TDRY," TDEW= ",TDEW
        CDEW="   "
        CDRY="   "
      END IF 

   94 CONTINUE
C 450 RETURN
C     END SUBROUTINE TEMPQC
      END
