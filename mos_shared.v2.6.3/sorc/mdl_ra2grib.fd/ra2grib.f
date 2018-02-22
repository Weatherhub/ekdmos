C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  
C                .      .    .                                       .  
C MAIN PROGRAM: MDL_RA2GRIB
C   PRGMMR: ALLEN            ORG: OST22       DATE: 2001-10-04
C                                                                       
C ABSTRACT: THIS PROGRAM READS MOS FORECAST OR CONSTANT RANDOM           
C   ACCESS FILES, OBTAINING REQUESTED DATA FOR SELECTED OR ALL  
C   AVAILABLE STATIONS. THE STATION DATA IS THEN INTERPOLATED
C   ONTO A GRID SPECIFIED BY THE USER, USING A CRESSMAN 
C   ANALYSIS ROUTINE.  THE USER CAN SPECIFY THE NUMBER OF 
C   PASSES THROUGH THE CRESSMAN ANALYSIS, WHAT THE RADIUS OF
C   INFLUENCE AT EACH GRID POINT SHOULD BE, AND WHETHER OR NOT
C   A BITMAP SHOULD BE CREATED FOR THE GRID FIELD.  ONCE THE
C   GRIDDED DATA IS OBTAINED, A FILE IS CREATED CONTAINING THE
C   GRIB FIELDS FOR EACH REQUESTED DATUM.                    
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   01-01-08  ALLEN       - REWORKED GRIBMOS TO RUN ON THE NEW
C                           MOS FORECAST SYSTEM AND BE IN MOS2000 
C                           FORMAT.
C   03-10-22  COSGROVE    - INCREASED NUMBER OF STATIONS TO 6600
C                           TO HANDLE COOP SITES.  INCREASED ND1
C                           AND ND5.
C   03-12-17  COSGROVE    - DID NOT MAKE # OF STATIONS BIG ENOUGH FOR
C                           COOPS. INCREASED TO 7000
C   04-05-11  COSGROVE    - MADE CHANGES TO ALLOW FOR A 20 KM GRID WITH
C                           ROUGHLY 30,000 GRID POINTS, AND TO ALLOW 
C                           FOR THE 20KM TSVR WHICH WILL HAVE ABOUT
C                           30,000 STATIONS
C   05-01-26  COSGROVE    - DESPITE THE COMMENT ABOVE, I THINK I HAD
C                           ONLY MADE IT WORK FOR 7000 SITES.  SET 
C                           ND1 AND ND5 TO 60000 TO ALLOW FOR 30000
C                                                                       
C USAGE:                                                                
C                                                                       
C        DATA SET USE  
C        INPUT FILES:                                                 
C             FORT.10 - NCEP STANDARD DATE FILE                  
C             FORT.29 - MDL MOS ID TABLE FILE (NOT CURRENTLY USED/INCLUDED
C                                              FOR POSSIBLE FUTURE USE.) 
C             FORT.25 - VARIABLE LIST                    
C             FORT.26 - USER INPUT STATION LIST(IF MISSING, CODE WILL USE
C                                               ALL STATIONS ON THE RANDOM
C                                               ACCESS FILE)                  
C             FORT.27 - STATION DICTIONARY                  
C             FORT.35 - FILE CONTAINING GRID INFORMATION         
C             FORT.36 - FILE CONTAINING OVERLAND POINTS OVER THE        
C                       CONTIGUOUS U.S. ON THE MDR (113 BY 89) GRID     
C                       USED FOR BITMAP                          
C             FORT.45 - MDL MOS CONSTANT RANDOM ACCESS FILE                    
C             FORT.48 - MDL MOS FORECAST RANDOM ACCESS FILE                    
C
C        OUTPUT FILES:
C             FORT.12 - STANDARD OUTPUT                         
C             FORT.66 - FILE CONTAINING GRIB MESSAGES           
C                                                                       
C        VARIABLES                                                      
C                NAME = DESCRIPTION     
C              KFILDI = UNIT NUMBER OF INPUT FILE.  (INPUT)
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C                   I = COUNTER VARIABLE
C                ID() = INTEGER ARRAY USED TO BUILD THE PDS THAT WILL
C                       BE INCLUDED IN THE GRIB RECORD.      
C                 IER = ERROR RETURN CODE
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  SET BY PARAMETER.
C                LPDS = CURRENT NUMBER OF ID ARRAY ELEMENTS WHICH GET
C                       USED TO CREATE THE PDS SECTION OF THE GRIB 
C                       MESSAGE
C               MXBUF = MAXIMUM LENGTH OF ANY OF THE OUTPUT GRIB        
C                       FIELDS BEFORE PROBLEMS MAY ARISE IN THIS        
C                       PROGRAM (INPUT)             
C                 MXI = MAXIMUM NUMBER OF ROWS ALLOWED IN THE GRID      
C                       FIELD (INPUT)                                   
C                 MXJ = MAXIMUM NUMBER OF COLUMNS ALLOWED IN THE GRID   
C                       FIELD (INPUT)      
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                       WITH (I.E., INTERPOLATION DONE FOR).  NOTE THAT
C                       THIS DOES NOT INCLUDE THE NUMBER OF STATIONS IN
C                       THE DIRECTORY UNLESS, OF COURSE, THE STATION
C                       DIRECTORY IS TO BE USED AS THE STATION LIST.
C                       SET BY PARAMETER.
C                 ND4 = THE MAXIMUM NUMBER OF PREDICTORS FOR WHICH 
C                       INTERPOLATED VALUES CAN BE PROVIDED.  SET BY
C                       PARAMETER.
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), DATA( ) AND
C                       CCALLD( ); SECOND DIMENSION OF ICALLD( , ).
C                       THESE ARE GENERAL PURPOSE ARRAYS, SOMETIMES USED 
C                       FOR GRIDS.  TWO SIZES OF ARRAYS (ND5 AND ND2X3) 
C                       ARE USED IN CASE AN ARRAY NEEDS TO BE LARGER 
C                       THAN ND2X3.  ND5 CAN BE INCREASED WITHOUT 
C                       INCREASING THE SIZE OF ALL ARRAYS. SET BY 
C                       PARAMETER.
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  MAXIMUM SIZE IS FOR
C                       IS1( ) = 22 PLUS 32 CHARACTERS (ONE CHARACTER
C                       PER WORD) OF PLAIN TEXT = 54.  SET BY PARAMETER.
C                                                                       
C        SUBPROGRAMS CALLED:                                            
C            UNIQUE:    CRESS,LAT2XY,BITMAP,GETGRID,MAINGRIB,INTGRIB  
C            MDLLIB:    TIMPR         
C             W3LIB:    W3FI72,W3TAGB,W3TAGE                      
C                                                                       
C        EXIT STATES:                                                   
C                STOP = 130 ;ERROR GETTING CALL LETTER RECORD FROM
C                            FORECAST FILE WHEN STATION LIST NOT SUPPLIED.
C                       131 ;CAN NOT READ NCEPDATE FILE
C                       133 ;CYCLE AND DATE DO NOT MATCH
C                       134 ;ERROR GETTING LATITUDE FROM FORECAST FILE
C                            WHEN STATION LIST NOT SUPPLIED.
C                       1344;ERROR UNPACKING LATITUDE
C                       135 ;ERROR GETTING LONGITUDE FROM FORECAST FILE
C                            WHEN STATION LIST NOT SUPPLIED.
C                       1355;ERROR UNPACKING LONGITUDE
C                       140 ;NUMBER OF STATIONS EXCEEDS ND5
C                       148 ;CAN NOT OPEN GRIB FILE
C                       151 ;MISSING TERMINATOR AFTER GRIB FILE NAME IN CN
C                       165 ;INCONSISTENT RANDOM ACCESS FILE UNIT #S
C                       175 ;INCONSISTENT GRIB FILE UNIT #S                  
C                       9999;ERROR READING FROM CN FILE 
C                     ALL OTHER ERRORS WILL NOT STOP EXECUTION.  SEE 
C                     INDIVIDUAL SUBROUTINES FOR ERROR CODES.
C              
C                                                                       
C REMARKS:  CURRENTLY LIMITED TO 30000 STATIONS,                  
C           A MAXIMUM GRID SIZE OF 200                           
C           BY 200 POINTS, WHICH CORRESPONDS TO A MAXIMUM        
C           GRIB BULLETIN SIZE OF 40000 BYTES,
C           AND A MAXIMUM NUMBER OF MDL IDS OF 3000.
C           THIS CODE WILL ONLY WORK FOR ONE INPUT DATE.              
C                                                                      
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  IBM
C                                                                       
C$$$                                                                    
      PROGRAM RA2GRIB
C
      IMPLICIT NONE
C
      INTEGER IER,I
C
      INTEGER, PARAMETER :: ND1 = 60000, ND4 = 3000, ND5 = 60000
      INTEGER, PARAMETER :: ND7 = 54, MXBUF = 40000, MXI = 350
      INTEGER, PARAMETER :: MXJ = 350, LPDS = 25, L3264B = 32
C
      INTEGER :: KFILDI = 5, KFILDO = 6
      INTEGER, DIMENSION(25) :: ID = (/ (-1,i=1,25) /)
C     
      CALL W3TAGB('MDL_RA2GRIB',2001,0277,0075,'OST22')
      CALL TIMPR(KFILDO,KFILDO,'START RA2GRIB       ')
C
      CALL MAINGRIB(KFILDI,KFILDO,ND1,ND4,ND5,ND7,MXBUF,MXI,MXJ,
     &           LPDS,ID,L3264B,IER)
C
      CALL TIMPR(KFILDO,KFILDO,'END RA2GRIB         ')
      CALL W3TAGE('MDL_RA2GRIB')
      STOP 
      END
