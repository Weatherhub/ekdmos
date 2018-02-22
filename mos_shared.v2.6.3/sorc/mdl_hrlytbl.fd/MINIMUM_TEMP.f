
C *********************************************************************
      SUBROUTINE MINIMUM_TEMP(MINTMP,MIN6,MIN24,IMIN6,IMIN24,CALL,IHR) 

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                                                                       
C SUBPROGRAM: MINIMUM_TEMP   PROCESS 6- AND 24-HR MINIMUM TEMPERATURES  
C   PRGMMR: K. HUGHES        ORG: W/OSD211     DATE: 98-07-23           
C                                                                       
C ABSTRACT: SUBROUTINE MINIMUM_TEMP EXTRACTS THE 6 HOUR AND 24
C   HOUR VALUES OF MINIMUM TEMPERATURE.  THE ORIGINAL
C   METAR VALUES ARE IN DEGREES CELSIUS, THE BUFR VALUES ARE
C   IN KELVIN, AND THE RESULTING VALUES FROM THIS SUBROUTINE
C   ARE IN DEGREES FAHRENHEIT.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-11-21  K. HUGHES                              
C   98-07-23  K. HUGHES    ADDED DOCUMENTATION, REMOVED NRET FROM CALL  
C   00-11-30  R. ALLEN     ADDED A CHECK IN CASE THE MIN TEMPS WERE
C                          BELOW -99 (I.E. WON'T FIT IN I3)
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     PROGRAM NAME                                                      
C                                                                       
C         96-11-21   KATHRYN HUGHES   MDL   CRAY                        
C                                                                       
C        PURPOSE                                                        
C            SUBROUTINE MINIMUM_TEMP EXTRACTS THE 6 HOUR AND 24
C            HOUR VALUES OF MINIMUM TEMPERATURE.  THE ORIGINAL
C            METAR VALUES ARE IN DEGREES CELSIUS, THE BUFR VALUES ARE
C            IN KELVIN, AND THE RESULTING VALUES FROM THIS SUBROUTINE
C            ARE IN DEGREES FAHRENHEIT.
C                                                                       
C        DATA SET USE                                                   
C            BEGIN HERE-INDICATE INPUT,OUTPUT OR INTERNAL               
C                                                                       
C        VARIABLES                                                      
C                NAME = INDICATE INPUT,OUTPUT,I/O, OR WORK              
C              MINTMP = ARRAY CONTAINING BUFR VALUES OF 6- AND 24-HR
C                       MINIMUM TEMPERATURE IN KELVIN            INPUT
C                MIN6 = CHARACTER VALUE OF 6-HR MINIMUM TEMPERATURE 
C                       USED TO FILL IN THE MDL HOURLY TABLE    OUTPUT
C               MIN24 = CHARACTER VALUE OF 24-HR MINIMUM TEMPERATURE 
C                       USED TO FILL IN THE MDL HOURLY TABLE    OUTPUT
C               IMIN6 = INTEGER VALUE OF 6-HR MIN TEMPERATURE   OUTPUT
C              IMIN24 = INTEGER VALUE OF 24-HR MIN TEMPERATURE  OUTPUT
C                CALL = CALL LETTERS USED FOR PRINTOUT PURPOSES  INPUT
C                 IHR = HOUR BEING PROCESSED                     INPUT
C
C                                                                       
C        SUBPROGRAMS CALLED: 
C            KELVIN_F:  FUNCTION TO CONVERT FROM KELVIN TO FAHRENHEIT     
C                                                                       
C        PROGRAM STOPS                                                  
C            NONE USED
C                                                                       
C        REMARKS                                                        
C            THE 6-HOUR VALUES ARE ONLY VALID AT 00Z, 06Z, 12Z AND 18Z
C            THE 24-HOUR VALUES ARE REPORTED AT A STATION AT LOCAL
C            MIDNIGHT.
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90                                                 
C   MACHINE:  CRAY                                                   
C$$$                                                                    
        IMPLICIT NONE

      REAL, INTENT(IN)   :: MINTMP(2,255)
      INTEGER            :: HOUR1,HOUR2,IMIN6,IMIN24,IHR
      CHARACTER(LEN=3)   :: MIN6,MIN24
      CHARACTER(LEN=8)   :: CALL(51)
      REAL,EXTERNAL      :: KELVIN_F 

C       THE 6-HOUR MINIMUM TEMPERATURE VALUES ARE ONLY VALID IF
C       THE TIME DURATION (HOUR1) IS EQUAL TO 6, AND THE REPORT
C       OCCURED AT 00Z, 06Z, 12Z, OR 18Z.

      IMIN6=9999
      IMIN24=9999

      HOUR1=NINT(MINTMP(1,1))
      HOUR2=NINT(MINTMP(1,2))

      IF (HOUR1.EQ.6) THEN
        IMIN6=NINT(KELVIN_F(MINTMP(2,1)))
      ELSE IF (HOUR1.EQ.24) THEN
        IMIN24=NINT(KELVIN_F(MINTMP(2,1)))
      END IF

      IF (HOUR2.EQ.24) THEN
        IMIN24=NINT(KELVIN_F(MINTMP(2,2)))
      END IF

      IF (IMIN6.LT.9999) THEN
        IF ((IHR.NE.0).AND.(IHR.NE.6).AND.(IHR.NE.12).AND.(IHR.NE.18))
     *  THEN
          PRINT *,"6-HOUR MINIMUM TEMPERATURE FOR STATION ",CALL(1),
     *    "WAS NOT REPORTED AT A VALID TIME." 
          IMIN6=9999
        END IF
      END IF

C       USE INTERNAL WRITE STATEMENTS TO CONVERT INTEGER VALUES OF
C       MINIMUM TEMPERATURE TO CHARACTER.
      WRITE(MIN6,100) IMIN6 
      WRITE(MIN24,100) IMIN24
  100 FORMAT(I3)
C
C       IF THE MINS ARE MORE THAN THREE DIGITS(WILL NOT FIT IN TABLE),
C       CHANGE TO MISSING
C
      IF (IMIN6.GT.999) MIN6='   '
      IF (IMIN24.GT.999) MIN24='   ' 
      IF (IMIN6.LT.-99) MIN6='   '
      IF (IMIN24.LT.-99) MIN24='   ' 

C     END SUBROUTINE MINIMUM_TEMP 
      END
