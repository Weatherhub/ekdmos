
C *********************************************************************
      SUBROUTINE MAXIMUM_TEMP(MAXTMP,MAX6,MAX24,IMAX6,IMAX24,CALL,IHR) 

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                                                                       
C SUBPROGRAM:  MAXIMUM_TEMP  PROCESS 6- AND 24-HR MAXIMUM TEMPERATURES  
C   PRGMMR: K. HUGHES        ORG: W/OSD211     DATE: 98-07-23           
C                                                                       
C ABSTRACT: SUBROUTINE MAXIMUM_TEMP EXTRACTS THE 6 HOUR AND 24
C   HOUR VALUES OF MAXIMUM TEMPERATURE.  THE ORIGINAL
C   METAR VALUES ARE IN DEGREES CELSIUS, THE BUFR VALUES ARE
C   IN KELVIN, AND THE RESULTING VALUES FROM THIS SUBROUTINE
C   ARE IN DEGREES FAHRENHEIT.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-11-21  K. HUGHES                              
C   98-07-23  K. HUGHES    ADDED DOCUMENTATION  REMOVED NRET FROM CALL  
C   00-11-30  R. ALLEN     ADDED A CHECK IN CASE THE MAX TEMPS WERE
C                          BELOW -99 (I.E. WON'T FIT IN I3)
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     PROGRAM NAME                                                      
C                                                                       
C        96-11-21   KATHRYN HUGHES   MDL   CRAY                         
C                                                                       
C        PURPOSE                                                        
C            SUBROUTINE MAXIMUM_TEMP EXTRACTS THE 6 HOUR AND 24
C            HOUR VALUES OF MAXIMUM TEMPERATURE.  THE ORIGINAL
C            METAR VALUES ARE IN DEGREES CELSIUS, THE BUFR VALUES ARE
C            IN KELVIN, AND THE RESULTING VALUES FROM THIS SUBROUTINE
C            ARE IN DEGREES FAHRENHEIT.
C 
C        DATA SET USE                                                   
C            BEGIN HERE-INDICATE INPUT,OUTPUT OR INTERNAL               
C                                                                       
C        VARIABLES                                                      
C              MAXTMP = ARRAY CONTAINING BUFR VALUES OF 6- AND 24-HR
C                       MAXIMUM TEMPERATURE IN KELVIN            INPUT
C                MAX6 = CHARACTER VALUE OF 6-HR MAX TEMPERATURE USED 
C                       TO FILL IN THE MDL HOURLY TABLE         OUTPUT
C               MAX24 = CHARACTER VALUE OF 24-HR MAX TEMPERATURE USED
C                       TO FILL IN THE MDL HOURLY TABLE         OUTPUT
C               IMAX6 = INTEGER VALUE OF 6-HR MAX TEMPERATURE    INPUT
C              IMAX24 = INTEGER VALUE OF 24-HR MAX TEMPERATURE   INPUT
C                CALL = CALL LETTERS USED FOR PRINTOUT PURPOSES  INPUT  
C                 IHR = HOUR BEING PROCESSED                     INPUT  
C                                                                       
C        SUBPROGRAMS CALLED: 
C            KELVIN_F:  FUNCITON TO CONVERT FORM KELVIN TO FAHRENHEIT
C                                                                       
C        PROGRAM STOPS                                                  
C                  NONE USED
C                                                                       
C        REMARKS                                                        
C            THE 6-HOUR VALUES ARE ONLY VALID AT 00Z, 06Z, 12Z AND 18Z
C            THE 24-HOUR VALUES ARE REPORTED AT A STATION AT LOCAL  
C            MIDNIGHT.
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90 FIXED FORMAT                          
C   MACHINE:  CRAY                                                   
C$$$                                                                    
        IMPLICIT NONE

      REAL, INTENT(IN)   :: MAXTMP(2,255)
      INTEGER            :: HOUR1,HOUR2,IMAX6,IMAX24,IHR
      CHARACTER(LEN=3)   :: MAX6,MAX24
      CHARACTER(LEN=8)   :: CALL(51)
      REAL,EXTERNAL      :: KELVIN_F 

C       THE 6-HOUR MAXIMUM TEMPERATURE VALUES ARE ONLY VALID IF
C       THE TIME DURATION (HOUR1) IS EQUAL TO 6, AND THE REPORT
C       OCCURED AT 00Z, 06Z, 12Z, OR 18Z.

      IMAX6=9999
      IMAX24=9999

      HOUR1=NINT(MAXTMP(1,1))
      HOUR2=NINT(MAXTMP(1,2))

      IF (HOUR1.EQ.6) THEN
        IMAX6=NINT(KELVIN_F(MAXTMP(2,1)))
      ELSE IF (HOUR1.EQ.24) THEN
        IMAX24=NINT(KELVIN_F(MAXTMP(2,1)))
      END IF

      IF (HOUR2.EQ.24) THEN
        IMAX24=NINT(KELVIN_F(MAXTMP(2,2)))
      END IF

      IF (IMAX6.LT.9999) THEN
        IF ((IHR.NE.0).AND.(IHR.NE.6).AND.(IHR.NE.12).AND.(IHR.NE.18))
     *   THEN
          PRINT *,"6-HOUR MAXIMUM TEMPERATURE FOR STATION ",CALL(1),
     *    "WAS NOT REPORTED AT A VALID TIME." 
          IMAX6=9999
        END IF
      END IF

C       USE INTERNAL WRITE STATEMENTS TO CONVERT INTEGER VALUES OF
C       MAXIMUM TEMPERATURE TO CHARACTER.
      WRITE(MAX6,100) IMAX6 
      WRITE(MAX24,100) IMAX24
  100 FORMAT(I3)
 
C
C       IF THE MAXS ARE MORE THAN THREE DIGITS(WILL NOT FIT IN TABLE),
C       CHANGE TO MISSING
C
      IF (IMAX6.GT.999) MAX6='   '
      IF (IMAX24.GT.999) MAX24='   ' 
      IF (IMAX6.LT.-99) MAX6='   '
      IF (IMAX24.LT.-99) MAX24='   ' 

C     END SUBROUTINE MAXIMUM_TEMP 
      END
