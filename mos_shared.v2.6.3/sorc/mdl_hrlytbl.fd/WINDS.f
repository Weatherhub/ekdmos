

C *********************************************************************
      SUBROUTINE WINDS(WIND,DIR,SPEED,GUST,CALL)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                                                                       
C SUBPROGRAM:  WINDS.F     PREPARES WIND DIRECTION, SPEED AND GUSTS   
C   PRGMMR: K. HUGHES      ORG: W/OSD211     DATE: 96-11-21           
C                                                                       
C ABSTRACT: THIS SUBROUTINE RETURNS THE WIND DIRECTION IN WHOLE
C   DEGREES, THE WIND SPEED IN KTS, AND THE WIND GUSTS IN KTS.           
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-11-21  K. HUGHES                              
C   97-10-20  K. HUGHES    ADDED SOME DOCUMENTATION    
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     PROGRAM NAME                                                      
C                                                                       
C        DATE   KATHRYN HUGHES   MDL   CRAY4                         
C                                                                       
C        PURPOSE                                                        
C            THIS SUBROUTINE RETURNS THE WIND DIRECTION IN WHOLE
C            DEGREES, THE WIND SPEED IN KTS, AND THE WIND GUSTS IN KTS.  
C                                                                       
C        DATA SET USE                                                   
C            INTERNAL               
C                                                                       
C        VARIABLES                                                      
C                CALL = CALL LETTERS
C                 DIR = WIND DIRECTION CONVERTED TO CHARACTER
C                GUST = WIND GUST AFTER BEING ROUNDED TO AN INTEGER
C                       AND THEN CONVERTED TO CHARACTER FOR PRINTING
C               SPEED = WIND SPEED AFTER BEING ROUNDED TO AN INTEGER
C                       AND THEN CONVERTED TO CHARACTER FOR PRINTING
C                WIND = ARRAY CONTAINING WIND DIRECTION, SPEED, AND
C                       GUSTS FROM BUFR
C                WDIR = WIND DIRECTION INTEGER WORK VARIABLE 
C                WSPD = WIND SPEED INTEGER WORK VARIABLE 
C                WGST = WIND GUST INTEGER WORK VARIABLE 
C
C        PROGRAM STOPS                                                  
C                  NONE                                        
C                                                                       
C        REMARKS                                                        
C            THIS SUBROUTINE DOES SOME QUALITY CONTROL ON THE WIND DATE.
C            THE VALUE OF 75 KNOTS USED TO CHECK WIND SPEED AND THE VALUE
C            OF 200 KNOTS USED TO CHECK FOR WIND GUSTS SEEMED AGREEABLE
C            TO MDL AT THE TIME THIS CODE WAS BEING WRITTEN.  VARIABLE
C            WIND DIRECTION IS STORED IN THE TABLE AS -9 
C            
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90                                                 
C   MACHINE:  CRAY4                                                   
C$$$                                                                    
        IMPLICIT NONE

C         SUBROUTINE WIND

      REAL,INTENT(IN)    :: WIND(3,255)
      INTEGER            :: WDIR,WSPD,WGST 
      CHARACTER(LEN=3)   :: DIR,SPEED,GUST
      CHARACTER(LEN=8)   :: CALL(51)          

C        CONVERT WINDS FROM METRIC (M/SEC) REAL VALUES, TO INTEGER 
C        VALUES IN KNOTS

      WDIR=NINT(WIND(1,1))
      WSPD=NINT(WIND(2,1)*1.9425)
      WGST=NINT(WIND(3,1)*1.9425)

C       USE INTERNAL WRITE STATEMENTS TO WRITE THE INTEGER VALUES
C       INTO CHARACTER VARIABLES.
 
      WRITE(DIR,100) WDIR
      WRITE(SPEED,100) WSPD
      WRITE(GUST,100) WGST
  100 FORMAT(I3)

      IF((WDIR.GT.360).AND.(WDIR.LT.999)) THEN
        PRINT *,"BAD WIND DIRECTION ",WDIR," AT STATION ",CALL(1)
        DIR="   "
      END IF

C        WIND SPEEDS GREATER THAN 75 KNOTS ARE CHANGED TO MISSING
 
      IF((WSPD.GT.75).AND.(WSPD.LT.999)) THEN
        PRINT *,"BAD WIND SPEED ",WSPD," AT STATION ",CALL(1)
        SPEED="   "
      END IF

C        WIND GUSTS GREATER THAN 200 KNOTS ARE CHANGED TO MISSING

      IF((WGST.GT.200).AND.(WGST.LT.999)) THEN
        PRINT *,"BAD WIND GUST ",WGST," AT STATION ",CALL(1)
        GUST="   "
      END IF

C         CHECK FOR VARIABLE WINDS, BUFR CHANGES VRB TO 0.
C         IF THE SPEED DOES NOT INDICATE CALM THEM WE HAVE
C         TO ASSUME IT IS VARIABLE.  CALM IS INDICATED WITH
C         A SPEED AND DIRECTION OF 0.

      IF(WDIR.EQ.0) THEN
        IF((WSPD.GT.0).AND.(WSPD.LT.999)) THEN
          DIR=" -9"
        ENDIF
      ENDIF

      IF(WGST.GE.999) GUST="   "
      IF(WDIR.GE.999) DIR="   "
      IF(WSPD.GE.999) SPEED="   "

C     END SUBROUTINE WINDS

C *********************************************************************
C                                                                     *
C                  BOTTOM OF PROGRAM HRLYTBL.F                        *
C                                                                     *
C ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ *

      END
