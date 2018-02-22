
C *********************************************************************
      SUBROUTINE REPORT_TIME(RPTIME,DAY,HR,MIN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                                                                       
C SUBPROGRAM:  REPORT_TIME   PROCESSES DATE AND TIME OF THE OBSERVATION
C   PRGMMR: K. HUGHES        ORG: W/OSD211     DATE: 96-11-21           
C                                                                       
C ABSTRACT:  SUBROUTINE REPORT_TIME PROCESSES THE DATE AND TIME OF
C            THE OBSERVATION - THE TIME OF THE METAR REPORT.  THIS
C            ROUTINE ALSO INSURES THAT ALL OF THE 0'S ARE KEPT, LEADING
C            OR OTHERWISE
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-11-21  K. HUGHES                              
C   98-08-12  K. HUGHES    ADDED MORE DOCUMENTATION                            
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     PROGRAM NAME                                                      
C                                                                       
C        98-08-12  KATHRYN HUGHES   MDL   CRAY-J916                       
C                                                                       
C        PURPOSE                                                        
C            SUBROUTINE TIME PROCESSES THE DATE AND TIME THAT THE
C            OBSERVATION WAS TAKEN, OR THE TIME OF THE REPORT.  
C                                                                       
C        DATA SET USE                                                   
C            BEGIN HERE-INDICATE INPUT,OUTPUT OR INTERNAL               
C                                                                       
C        VARIABLES                                                      
C              RPTIME = BUFR ARRAY CONTAINING THE REPORT TIME      INPUT 
C                 DAY = 2-DIGIT DAY OF THE MONTH                  OUTPUT 
C                  HR = 2-DIGIT HOUR OF OBSERVATION - GMT         OUTPUT
C                 MIN = 2-DIGIT MINUTE OF THE OBSERVATION         OUTPUT 
C                                                                       
C        PROGRAM STOPS                                                  
C                  NONE                                                 
C                                                                       
C        REMARKS                                                        
C            ALL ZEROS ARE KEPT IN THE TIME REPORT                      
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90                                                 
C   MACHINE:  CRAY - J916                                                   
C$$$                                                                    
        IMPLICIT NONE

      REAL, INTENT(IN)    :: RPTIME(3,255)
      INTEGER             :: IDAY,IHR,IMIN
      CHARACTER(LEN=2)    :: DAY,HR,MIN

      IDAY=NINT(RPTIME(1,1))
      IHR=NINT(RPTIME(2,1))
      IMIN=NINT(RPTIME(3,1))

      WRITE(DAY,100) IDAY
      WRITE(HR,100) IHR
      WRITE(MIN,100) IMIN 
  100 FORMAT(I2)

      IF (IDAY.GT.99) DAY='  '
      IF (IHR.GT.99) HR='  '
      IF (IMIN.GT.99) MIN='  '
 
      IF (IDAY.LT.10) DAY(:1)="0"
      IF (IHR.LT.10) HR(:1)="0"
      IF (IMIN.LT.10) MIN(:1)="0"

C     END SUBROUTINE REPORT_TIME 
      END
