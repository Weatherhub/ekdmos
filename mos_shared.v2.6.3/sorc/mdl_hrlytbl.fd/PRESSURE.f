
C *********************************************************************
      SUBROUTINE PRESSURE(PRSR,SLP,ALT) 

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                                                                       
C SUBPROGRAM:    PRESSURE    BRIEF DESCRIP(40CHAR) GOES HERE            
C   PRGMMR: KATHRYN HUGHES   ORG: W/OSD211     DATE: 98-07-28           
C                                                                       
C ABSTRACT: START ABSTRACT HERE AND INDENT TO COLUMN 5 ON THE           
C   FOLLOWING LINES.  SEE NMC HANDBOOK SECTION 3.1.1 FOR DETAILS.       
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-11-21  K. HUGHES                              
C   98-07-28  K. HUGHES      ADDED DOCUMENTATION     
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     PROGRAM NAME                                                      
C                                                                       
C        96-11-21  K. HUGHES      MDL   CRAY-J916                   
C                                                                       
C        PURPOSE                                                        
C            COMMENTS BEGIN HERE                                        
C                                                                       
C        DATA SET USE                                                   
C            BEGIN HERE-INDICATE INPUT,OUTPUT OR INTERNAL               
C                                                                       
C        VARIABLES                                                      
C                NAME = INDICATE INPUT,OUTPUT,I/O, OR WORK              
C                                                                       
C        SUBPROGRAMS CALLED:  DELETE IF NONE CALLED FROM THIS SUBPROGRAM
C            UNIQUE:  LIST ALPHABETICALLY THOSE ACCOMPANYING SOURCE     
C            LIBRARY: LIST ALPHABETICALLY                               
C              COMMON:                                                  
C              W3LIB:                                                   
C              W4LIB:                                                   
C              GRAPHICS:                                                
C                                                                       
C        PROGRAM STOPS                                                  
C                  ## = DEFINE WHY, IF IN THIS SUBPROGRAM               
C                                                                       
C        REMARKS                                                        
C            ANYTHING APPROPRIATE CAN GO HERE--LIKE BUYER BEWARE        
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN H EXT + (OR VS FORTRAN)                           
C   MACHINE:  NAS9000                                                   
C$$$                                                                    
        IMPLICIT NONE
      REAL, INTENT(IN)   :: PRSR(2,255)
      REAL               :: ALTM,PRESS
      CHARACTER(LEN=6)   :: SLP 
      CHARACTER(LEN=5)   :: ALT 

      PRESS=(PRSR(2,1)/100.0)             
      ALTM=(PRSR(1,1)/100.0)*29.921/1013.25             

C       USE INTERNAL WRITE STATEMENTS TO CONVERT REAL VALUES OF
C       PRESSURE AND ALTIMETER SETTINGS TO CHARACTER.
      WRITE(SLP,100) PRESS
      WRITE(ALT,200) ALTM
  100 FORMAT(F6.1)
  200 FORMAT(F5.2)
 
      IF (PRESS.GT.9999.9) SLP='      '
      IF (ALTM.GT.99.9) ALT='     '

C     END SUBROUTINE PRESSURE
      END
