
C *********************************************************************
      SUBROUTINE TEMPERATURE(TEMPTD,CDRY,CDEW,TDRY,TDEW) 

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                                                                       
C SUBPROGRAM:  TEMPERATURE   FORMATS TEMPERATURE AND DEW POINT          
C   PRGMMR: K. HUGHES        ORG: W/OSD211     DATE: 96-11-21           
C                                                                       
C ABSTRACT: SUBROUTINE TEMPERATURE PULLS OUT THE HOURLY TEMPERATURE
C   AND DEW POINT TEMPERATURE.  IT WILL ALSO CONVERT THEM FROM
C   THE WMO STANDARD UNIT OF KELVIN TO DEGREES FAHRENHEIT.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-11-21  K. HUGHES                              
C   98-08-12  K. HUGHES   ADDED MORE DOCUMENTATION                           
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     PROGRAM NAME                                                      
C                                                                       
C        DATE   KATHRYN HUGHES   MDL   CRAY-J916                         
C                                                                       
C        PURPOSE                                                        
C            SUBROUTINE TEMPERATURE PULLS OUT THE HOURLY TEMPERATURE
C            AND DEW POINT TEMPERATURE.  IT WILL ALSO CONVERT THEM FROM
C            THE WMO STANDARD UNIT OF KELVIN TO DEGREES FAHRENHEIT.
C                                                                       
C        DATA SET USE                                                   
C            BEGIN HERE-INDICATE INPUT,OUTPUT OR INTERNAL               
C                                                                       
C        VARIABLES                                                      
C              TEMPTD = ARRAY FROM BUFR FILE CONTAINING REAL VALUES
C                       OF TEMPERATURE AND DEW POINT IN KELVIN    INPUT
C                CDRY = CHARACTER VALUE OF TEMPERATURE IN DEGREES
C                       FAHRENHEIT                               OUTPUT 
C                CDEW = CHARACTER VALUE OF DEW POINT IN DEGREES
C                       FAHRENHEIT                               OUTPUT 
C                TDRY = INTEGER VALUE OF TEMPERATURE               WORK
C                TDEW = INTEGER VALUE OF DEW POINT                 WORK
C            
C        SUBPROGRAMS CALLED:
C            UNIQUE:  FUNCTION KELVIN_F  CONVERTS KELVIN TO FAHRENHEIT  
C                                                                       
C        PROGRAM STOPS                                                  
C                  NONE
C                                                                       
C        REMARKS                                                        
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90                                                 
C   MACHINE:  CRAY - J916                                               
C$$$                                                                    
        IMPLICIT NONE

      REAL, INTENT(IN)   :: TEMPTD(2,255)
      INTEGER            :: TDRY,TDEW,I
      CHARACTER(LEN=3)   :: CDRY,CDEW
      REAL,EXTERNAL      :: KELVIN_F

      TDRY=NINT(KELVIN_F(TEMPTD(1,1)))
      TDEW=NINT(KELVIN_F(TEMPTD(2,1)))

C       USE INTERNAL WRITE STATEMENTS TO CONVERT INTEGER VALUES OF
C       PRECIP TO CHARACTER.
      WRITE(CDRY,100) TDRY 
      WRITE(CDEW,100) TDEW 
  100 FORMAT(I3)
 
      IF (TDRY.GT.999) CDRY='   '
      IF (TDEW.GT.999) CDEW='   '

C     END SUBROUTINE TEMPERATURE
      END
