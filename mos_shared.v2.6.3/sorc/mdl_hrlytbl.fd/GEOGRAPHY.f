
C *********************************************************************

      SUBROUTINE GEOGRAPHY(GEOG,LAT,LON,ELEV,TYPE,SPECI)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                                                                       
C SUBPROGRAM:    GEOGRAPHY   PROCESSES STATION LOCATION AND TYPE        
C   PRGMMR: KATHRYN HUGHES   ORG: W/OSD211     DATE: 98-07-28           
C                                                                       
C ABSTRACT: THIS SUBROUTINE PROVIDES STATION INFORMATION FROM THE          
C   THE ARRAY GEOG.  IT WILL RETURN A STATION'S LATITUDE IN
C   DEGREES; A STATION'S LONGITUDE IN DEGREES, AND CHANGE THE
C   SIGN OF THE LONGITUDE SO THAT WEST IS POSITIVE (OPPOSITE
C   OF STANDARD WMO BUFR NOTATION); IT CONVERTS A STATION'S
C   ELEVATION FROM METERS TO FEET; IT WILL DETERMINE THE
C   TYPE OF STATION REPORTING; AND FINALLY IT WILL RETURN
C   A VALUE WHICH INDICATES WHETHER THE REPORT IS STANDARD
C   OR A SPECIAL.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-11-21  K. HUGHES                              
C   98-07-28  K. HUGHES     ADDED DOCUMENTATION      
C   99-08-16  R. ALLEN	    CHANGED THE LIMITS ON GEOG FROM (3,255) TO (5,255)
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     GEOGRAPHY.F                                                       
C                                                                       
C        98-07-28  K. HUGHES      MDL   CRAY-J916                       
C                                                                       
C        PURPOSE                                                        
C            SUBROUTINE GEOGRAPHY PROVIDES STATION INFORMATION FROM THE
C            THE ARRAY GEOG.  IT WILL RETURN A STATION'S LATITUDE IN
C            DEGREES; A STATION'S LONGITUDE IN DEGREES, AND CHANGE THE
C            SIGN OF THE LONGITUDE SO THAT WEST IS POSITIVE (OPPOSITE
C            OF STANDARD WMO BUFR NOTATION); IT CONVERTS A STATION'S
C            ELEVATION FROM METERS TO FEET; IT WILL DETERMINE THE
C            TYPE OF STATION REPORTING; AND FINALLY IT WILL RETURN
C            A VALUE WHICH INDICATES WHETHER THE REPORT IS STANDARD
C            OR A SPECIAL.
C                                                                       
C        DATA SET USE                                                   
C            BEGIN HERE-INDICATE INPUT,OUTPUT OR INTERNAL               
C                                                                       
C        VARIABLES                                                      
C                GEOG = ARRAY CONTAINING BUFR DATA OF ALL OF THE
C                       STATION INFORMATION NEEDED BY THIS ROUTINE
C                                                              INPUT
C                 LAT = LATITUDE OF STATION IN DEGREES        OUTPUT    
C                 LON = LONGITUDE OF STATION IN DEGREES       OUTPUT   
C                ELEV = STATION ELEVATION IN FEET             OUTPUT   
C                TYPE = TYPE OF STATION - MANUAL, AUTO, ASOS  OUTPUT    
C               SPECI = USED TO ELIMINATE SPECIAL REPORTS     OUTPUT    
C                                                                       
C        SUBPROGRAMS CALLED:  DELETE IF NONE CALLED FROM THIS SUBPROGRAM
C            UNIQUE:  METER_FT  CONVERTS FROM METERS TO FEET            
C                                                                       
C        PROGRAM STOPS                                                  
C            NONE                                                       
C                                                                       
C        REMARKS                                                        
C            AN ASOS STATION IS CONSIDERED AUGMENTED BY NCEP'S DECODER
C            IF THE WORD AUTO IS MISSING FROM THE METAR REPORT
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90                                                 
C   MACHINE:  CRAY-J916                                                   
C$$$                                                                    
        IMPLICIT NONE

      REAL,INTENT(IN)    :: GEOG(5,255)
      REAL               :: RLAT,RLON,RELEV,SPECI
      INTEGER            :: AUTO
      CHARACTER(LEN=6)   :: LAT
      CHARACTER(LEN=7)   :: LON
      CHARACTER(LEN=5)   :: ELEV
      CHARACTER(LEN=4)   :: TYPE 
      REAL,EXTERNAL      :: METER_FT

      RLAT=GEOG(1,1)
      RLON=GEOG(2,1)*(-1.0)      ! CHANGE SIGNS
      RELEV=METER_FT(GEOG(3,1))
      SPECI=GEOG(4,1)
      AUTO=NINT(GEOG(5,1))

      SELECT CASE (AUTO)
      CASE (0)            ! FULLY AUTOMATED 
        TYPE = "AUTO"
      CASE (1)            ! ASOS NO PRECIP, NO HUMAN INTERVENTION 
        TYPE = "AO1 "
      CASE (3)            ! ASOS NO PRECIP, WITH HUMAN INTERVENTION 
        TYPE = "AO1A"
      CASE (2)            ! ASOS WITH PRECIP, NO HUMAN INTERVENTION 
        TYPE = "AO2 "
      CASE (4)            ! ASOS WITH PRECIP, WITH HUMAN INTERVENTION 
        TYPE = "AO2A"
      CASE (5)       
        TYPE = "AMOS"
      CASE (6)       
        TYPE = "RMOS"     ! RAMOS
      CASE (7)     
        TYPE = "AUTB"     ! AUTOB
      CASE (8)     
        TYPE = "AWOS" 
      CASE (9999:)
        TYPE = "MANU"
      CASE DEFAULT
        TYPE = "UNKN"
      END SELECT

      WRITE(LAT,100) RLAT
      WRITE(LON,200) RLON
  100 FORMAT(F6.2)
  200 FORMAT(F7.2)
     
      IF(RLAT.GT.99.9) LAT='      '
      IF(RLON.LT.-999.9) LON='       '

C     END SUBROUTINE GEOGRAPHY 
      END
