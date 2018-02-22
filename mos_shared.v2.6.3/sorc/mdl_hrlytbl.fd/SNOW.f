
C *********************************************************************
      SUBROUTINE SNOW(SNW,SNDPTH,SNFALL,SNDUR) 

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                                                                       
C SUBPROGRAM:    SNOW        PROCESSES METAR SNOW REPORTS               
C   PRGMMR: K. HUGHES        ORG: W/OSD211     DATE: 96-11-21           
C                                                                       
C ABSTRACT: THIS ROUTINE EXTRACTS THE SNOW INFORMATION STORED
C           IN THE BUFR FILES FROM THE METAR REPORTS.  IT INCLUDES THE
C           SNOW DEPTH ON THE GROUND (4/SSS) AND THE SNOW INCREASING
C           RAPIDLY (SNINCR  IN/HR) GROUPS.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-11-21  K. HUGHES                              
C   98-08-12  K. HUGHES   ADDED MORE DOCUMENTATION, CORRECTED POSSIBLE 
C                         ERROR IN THE CODE THAT COULD HAVE CAUSED MISSING
C                         SNFALL AND SNDUR. 
C   98-12-09  K. HUGHES   REMOVED SNFALL FROM METAR SNINCR REPORTS.
C                         SCD REPORTS OF SNOWFALL WILL BE USED INSTEAD
C   02-03-06  R. COSGROVE AS OF MARCH 19TH, A TRACE OF SNOW WILL
C                         BE DENOTED AS -0.01 FOR MNEMONICS DOFS AND TOSD
C                         IN THE BUFR TANKS.  IT WAS PREVIOUSLY
C                         DENOTED AS -1.  ADDED CHECK FOR ANY VALUE COMING
C                         OUT OF BUFR THAT'S LESS THAN -0.005, AND CHANGED THAT 
C                         TO -4 AUTOMATICALLY. WE'RE ONLY CHECKING THE TOSD
C                         BECAUSE WE THROW OUT THE SNOWFALL (DOFS) FROM THE METARS.
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     PROGRAM NAME                                                      
C                                                                       
C        98-08-12  KATHRYN HUGHES MDL   CRAY-J916                       
C                                                                       
C        PURPOSE                                                        
C            TO PROCESS METAR SNOW REPORTS                              
C                                                                       
C        DATA SET USE                                                   
C                                                                       
C        VARIABLES                                                      
C                 SNW = BUFR ARRAY CONTAINING ALL THE SNOW DATA  INPUT         
C              SNDPTH = DEPTH OF SNOW ON THE GROUND IN INCHES   OUTPUT         
C              SNFALL = DEPTH OF FRESHLY FALLEN SNOW IN INCHES  OUTPUT         
C               SNDUR = TIME PERIOD VALID FOR SNFALL IN HOURS   OUTPUT         
C                                                                       
C        SUBPROGRAMS CALLED: 
C            UNIQUE:  FUNCTION METER_FT - CONVERTS FROM METERS TO FEET  
C                                                                       
C        PROGRAM STOPS                                                  
C                  NONE                                                 
C                                                                       
C        REMARKS                                                        
C            VERY FEW REPORTS OF SNOW ARE AVAILABLE IN THE METAR REPORTS.
C            INTEGRATION OF THE SCD SNOW REPORTS IS DESIRABLE        
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90                                                 
C   MACHINE:  CRAY-J916                                                   
C$$$                                                                    
        IMPLICIT NONE
      REAL, INTENT(IN)   :: SNW(3,255)
      REAL,EXTERNAL      :: METER_FT
      INTEGER            :: SDPTH,SFALL,SDUR
      CHARACTER(LEN=3)   :: SNDPTH,SNDUR,SNFALL

C       USE INTERNAL WRITE STATEMENTS TO CONVERT REAL VALUES OF
C       SUNSHINE TO CHARACTER AND CONVERT THE BUFR VALUES FROM
C       METERS BACK TO INCHES.

C       CHECK SNW(3,1) FIRST TO SEE IF IT'S A TRACE.  IF SO, SET IT TO
C       -4.  OTHERWISE, USE THE FORMULA.  DON'T DO THIS FOR SFALL, BECAUSE
C       WE THROW IT OUT ANYWAY 
C
      IF(SNW(3,1).LT.-0.005)THEN
        SDPTH=-4
      ELSE
        SDPTH=NINT(METER_FT(SNW(3,1))*12)
      ENDIF

      SFALL=NINT(METER_FT(SNW(2,1))*12)
      SDUR=NINT(SNW(1,1))

      WRITE(SNDPTH,100) SDPTH
  100 FORMAT(I3)
      WRITE(SNFALL,200) SFALL
  200 FORMAT(I3)
      WRITE(SNDUR,300) SDUR
  300 FORMAT(I3)
 
      IF (SDPTH.GT.999.) SNDPTH='   '
C
C      AS OF DECEMBER 1998, WE NO LONGER KEEP THE METAR SNOWFALL REPORTS
C     IF (SFALL.GT.999.) SNFALL='   '
      SNFALL='   '
      IF (SDUR.GT.999.) SNDUR='   '

C     END SUBROUTINE SNOW
      END
