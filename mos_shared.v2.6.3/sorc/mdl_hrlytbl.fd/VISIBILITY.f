C *********************************************************************
      SUBROUTINE VISIBILITY(VISI,VIS,VVIS,VVISI) 

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                                                                       
C SUBPROGRAM:  VISIBILITY.F  PROCESS HORIZONTAL AND VERTICAL VISIBILITY
C   PRGMMR: K. HUGHES        ORG: W/OSD21      DATE: 99-05-19           
C                                                                       
C ABSTRACT: TO CONVERT THE HORIZONTAL AND VERTICAL VISIBILITY FROM           
C   METERS TO MILES.       
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-11-21  K. HUGHES                              
C   97-10-16  K. HUGHES    ADDED VVISI VARIABLE TO THE CALL
C   98-08-12  K. HUGHES    MODIFIED SOME OF THE DOCUMENTION
C   99-05-19  K. HUGHES    ADDED REHOVI TO DETERMINE <1/4 AND CHANGED 
C                          DIMENSION OF VISI. 
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     VISIBILITY.F                                                       
C                                                                       
C        PURPOSE                                                        
C            SUBROUTINE VISIBILITY CONVERTS THE VISIBILITY FROM
C            METERS TO MILES.
C                                                                       
C        VARIABLES                                                      
C                VISI = ARRAY CONTAINING BUFR VALUES OF HORIZONTAL AND VERTICAL
C                       VISIBILITIES       (INPUT)
C               VISIB = HORIZONTAL VISIBILITY  (WORK)
C                 VIS = HORIZONTAL VISIBILITY - CHAR FOR TABLE PRINTING (OUTPUT)
C                VVIS = VERTICAL VISIBILITY - CHARACTER FOR TABLE PRINTING (OUTPUT) 
C               VVISI = INTEGER REPRESENTATION OF VERTICAL VISIBILITY  (OUTPUT)
C              REHOVI = NCEP CODE TABLE DESCRIPTOR VALUE, WHERE 0 MEANS THE 
C                       HORIZONTAL VISIBILITY IS LESS THAN THE STORED VALUE (INPUT)
C        
C        SUBROUTINES CALLED
C            UNIQUE:  FUNCTION METER_FT -  CONVERTS FROM METERS TO FEET. 
C                                                                       
C        PROGRAM STOPS                                                  
C                  NONE                                                 
C                                                                       
C        REMARKS                                                        
C            THE VERTICAL VISIBILITY IS PASSED TO THE CLOUD SUBROUTINE WHERE
C            IT BECOMES THE CLOUD HEIGHT OF AN OBSCURATION.  REHOVI IS USED 
C            TO DISTINGUISH AN ASOS METAR REPORT OF M1/4 SM FROM 1/4 SM FOR 
C            HORIZONTAL VELOCITY.
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90                                                 
C   MACHINE:  CRAY - J916                                               
C$$$                                                                    
        IMPLICIT NONE

      REAL, INTENT(IN)   :: VISI(3,255)
      REAL               :: VISIB
      INTEGER            :: VVISI,REHOVI
      CHARACTER(LEN=5)   :: VIS      
      CHARACTER(LEN=3)   :: VVIS      
      REAL,EXTERNAL      :: METER_FT

C        PROCESS HORIZONTAL VISIBILITY, CONVERT FROM METERS TO MILES.
      VISIB=METER_FT(VISI(1,1))/5280.0 
 
C       ROUND TO THE NEAREST WHOLE NUMBER
      IF (VISIB.GE.3.0) THEN
        VISIB=ANINT(VISIB)         
      END IF

C       EXTRACT THE NCEP LOCAL CODE TABLE VALUE WHICH INDICATES
C       WHETHER THE VISIBILITY VALUE IS EXACT OR NOT.  THIS IS
C       NECESSARY BECAUSE ASOS CAN REPORT A VALUE OF <1/4 BUT
C       IT IS STORED THE SAME AS 1/4.  THE VERIFICATION PROGRAM
C       NEEDS TO DISTINGUISH THEM APART.  WE WILL SET THE <1/4
C       VALUES TO A UNIQUE VALUE OF .20.
      REHOVI=NINT(VISI(3,1))

C       USE INTERNAL WRITE STATEMENTS TO CONVERT REAL VALUES OF
C       VISIB TO CHARACTER.
      WRITE(VIS,100) VISIB
  100 FORMAT(F5.2)

      IF (VISIB.GT.100.0) VIS='     '
      IF ((VISIB.GT.39.0).AND.(VISIB.LE.100.0)) VIS='40.00'

C       FORMAT VERTICAL VISIBLITY, CONVERT TO INTEGER FROM REAL*8, THEN
C       WRITE TO A CHARACTER VARIABLE FOR PRINTING.
      VVISI=NINT((METER_FT(VISI(2,1)))/100)

      WRITE(VVIS,200) VVISI
  200 FORMAT(I3)

      IF (VVISI.EQ.0) VVIS='  0'

      IF ((VISIB.LT.0.26).AND.(VISIB.GT.0.24)) THEN 
          IF (REHOVI.EQ.0) VIS=' 0.20'
      ENDIF

C     IF (VVISI.LT.999) THEN
C       WRITE(46,300) VVIS
C 300   FORMAT(' ','VERTICAL VISIBILITY IS :',A3)
C     END IF

      IF (VVISI.GE.999) THEN
        VVIS='   '
      END IF

C     END SUBROUTINE VISIBILITY
      END
