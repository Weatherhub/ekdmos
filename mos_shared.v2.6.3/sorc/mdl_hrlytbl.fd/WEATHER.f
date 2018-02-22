
C *********************************************************************
      SUBROUTINE WEATHER(PRWX,CPWX,WXCNT,CALL,HR,MIN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                                                                       
C SUBPROGRAM: WEATHER.F      CONVERTS BUFR VALUES TO METAR WEATHER CODES
C   PRGMMR: K HUGHES         ORG: W/OSD211     DATE: 98-09-15           
C                                                                       
C ABSTRACT:  THIS SUBROUTINE CHANGES THE PRESENT WEATHER FROM THE WMO 
C            BUFR DESCRIPTIONS (TABLE 020003) TO THE METAR CHARACTER
C            WEATHER CODES.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-11-21  K. HUGHES    READY FOR CRAY IMPLEMENTATION               
C   97-10-15  K. HUGHES    UPDATED TO MATCH NCEP CHANGES FOR MIFG, BR,
C   97-11-13  K. HUGHES    UPDATED TO MATCH NCEP CHANGES FOR MIFG, BR,
C                          FOR AUTO STATIONS
C   98-07-28  K. HUGHES    CHANGED OUTPUT OF ICE PELLETS FROM PE TO PL,
C                          TO PREPARE FOR CHANGE IN U.S. REPORTING
C                          STANDARD EFFECTIVE 11-05-98
C   98-09-15  K. HUGHES    UPDATED TO MATCH NCEP CHANGES FOR -DZRA
C                          AND +TSGR
C   00-01-04  R. ALLEN     ADDED PRWX CODES 105 (HZ W/VIS <1 KM), 128,129 
C                          (BLSN).
C   05-10-06  R. COSGROVE  ADDED PRWX CODE 219 - FUNNEL CLOUD (FC)
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     PROGRAM NAME                                                      
C                                                                       
C        DATE   K. HUGHES    MDL   CRAY J916 - UNICOS            
C                                                                       
C        PURPOSE                                                        
C            SUBROUTINE TO CHANGE THE PRESENT WEATHER FROM THE
C            WMO BUFR DESCRIPTIONS (TABLE 020003) BACK TO THE
C            METAR CHARACTER WEATHER CODES.
C                                                                       
C        DATA SET USE                                                   
C            BEGIN HERE-INDICATE INPUT,OUTPUT OR INTERNAL               
C                                                                       
C        VARIABLES                                                      
C                CALL = STATION CALL LETTERS                            
C                CPWX = PRESENT WEATHER CHARACTER STRING                
C                  HR = HOUR OF REPORT, FOR PRINTING DIAGNOSTIC MESSAGES
C                  MN = MINUTE OF REPORT, FOR PRINTING DIAGNOSTIC MESSAGES
C                PRWX = PRESENT WEATHER STORED AS A WMO BUFR REAL NUMBER 
C             WMOPRWX = PRESENT WEATHER CONVERTED TO AN INTEGER           
C               WXCNT = NUMBER OF WEATHER TYPES FOR A GIVEN SITE'S REPORT
C                       METAR ALLOWS 6 FOR MANUAL AND 3 FOR AUTO SITES,
C                       BUT ONLY THREE ARE SAVED FOR THE HOURLY TABLE.
C                                                                       
C        PROGRAM STOPS                                                  
C            NONE              
C                                                                       
C        REMARKS                                                        
C            BUFR STORES SOME WEATHER REPORTS IN THE SAME DESCRIPTOR.
C            THIS MEANS THAT SOME INFORMATION SUCH AS INTENSITY, IS LOST
C            IN BUFR AND IS NOT RECOVERED WHEN THIS TABLE CONVERTS IT
C            BACK TO THE METAR CODE.  AN ATTEMPT WAS MADE TO SELECT THE
C            MOST COMMON AMONG WEATHER GROUPS THAT FELL INTO THE SAME
C            BUFR DESCRIPTOR.
C 
C            THE NCEP CODES UT_BFPM.F AND UT_BFPA.F WERE USED TO MAP THE
C            BUFR NUMBER BACK TO METAR WEATHER CODES.
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90                                                 
C   MACHINE:  CRAY-J916                                             
C$$$                                                                    
      IMPLICIT NONE

      REAL, INTENT(IN) :: PRWX(3,255)
      INTEGER          :: WMOPRWX(3),I,WXCNT,J
      CHARACTER(LEN=7) :: CPWX(3)
      CHARACTER(LEN=8) :: CALL(51)
      CHARACTER(LEN=2) :: HR,MIN
   
      DO J=1,3
        CPWX(J)="       "
      END DO 

      DO I=1,WXCNT
      WMOPRWX(I) = NINT(PRWX(1,I))

      SELECT CASE (WMOPRWX(I))
      CASE (4)               ! SMOKE
        CPWX(I) = "FU     " 
      CASE (5,104,105)       ! HAZE
        CPWX(I) = "HZ     " 
      CASE (6)               ! DUST
        CPWX(I) = "DU     "  
      CASE (7)               ! BLOWING DUST OR SAND
        CPWX(I) = "BLDU   "
      CASE (8)               ! DUST WHIRLS OR SAND WHIRLS
        CPWX(I) = "PO     "
      CASE (9)               ! DUSTSTORM OR SANDSTORM
        CPWX(I) = "VCDS   "
      CASE (10,110)          ! MIST 
        CPWX(I) = "BR     "
      CASE (11)              ! PATCHY FOG
        CPWX(I) = "PY     "   
      CASE (12)              ! SHALLOW FOG, MORE OR LESS CONTINUOUS MIST
        CPWX(I) = "MIFG   "
      CASE (16)              ! PCP WITHIN SIGHT BUT NOT AT THE STATION
        CPWX(I) = "VCSH   "
      CASE (17,126)          ! THUNDERSTORM WITHOUT PRECIP
        CPWX(I) = "TS     "
      CASE (18,118)          ! SQUALLS      
        CPWX(I) = "SQ     "
      CASE (19,199)          ! FUNNEL CLOUD, TORNADO OR WATERSPOUT
        CPWX(I) = "+FC    "  ! FC, +FC
      CASE (31)              ! SLIGHT OR MODERATE DUSTSTORM OR SANDSTORM
        CPWX(I) = "DS     "
      CASE (34)              ! SEVERE DUSTSTORM OR SANDSTORM
        CPWX(I) = "+DS    "
      CASE (36)              ! SLIGHT OR MODERATE DRIFTING SNOW
        CPWX(I) = "DRSN   "
      CASE (37)              ! HEAVY DRIFTING SNOW
        CPWX(I) = "+DRSN  "
      CASE (38,127,128,129)  ! SLIGHT OR MODERATE BLOWING SNOW
        CPWX(I) = "BLSN   "
      CASE (39)              ! HEAVY BLOWING SNOW
        CPWX(I) = "+BLSN  "
      CASE (40)              ! FOG OR ICE FOG AT A DISTANCE
        CPWX(I) = "VCFG   "
      CASE (41,131)          ! FOG OR ICE FOG IN PATCHES
        CPWX(I) = "BCFG   "
      CASE (44)              ! FOG OR ICE FOG, SKY VISIBLE
        CPWX(I) = "PRFG   "
      CASE (45,130)          ! FOG OR ICE FOG, SKY INVISIBLE
        CPWX(I) = "FG     "
      CASE (51,151)          ! SLIGHT, CONTINUOUS DRIZZLE
        CPWX(I) = "-DZ    "
      CASE (53,152)          ! MODERATE, CONTINOUS DRIZZLE
        CPWX(I) = "DZ     "
      CASE (55,153)          ! HEAVY, CONTINUOUS DRIZZLE
        CPWX(I) = "+DZ    "
      CASE (56,154)          ! SLIGHT, FREEZING DRIZZLE
        CPWX(I) = "-FZDZ  "
      CASE (57,155)          ! MODERATE OR HEAVY, FREEZING DRIZZLE
        CPWX(I) = "FZDZ   "
      CASE (58,157)          ! SLIGHT RAIN AND DRIZZLE
        CPWX(I) = "-RADZ  "
      CASE (59,158)          ! MODERATE OR HEAVY RAIN AND DRIZZLE
        CPWX(I) = "RADZ   "
      CASE (61,161)          ! SLIGHT RAIN
        CPWX(I) = "-RA    "
      CASE (63,162)          ! MODERATE RAIN
        CPWX(I) = "RA     "
      CASE (65,163)          ! HEAVY RAIN
        CPWX(I) = "+RA    "
      CASE (66,164)          ! SLIGHT FREEZING RAIN
        CPWX(I) = "-FZRA  "
      CASE (67,165)          ! MODERATE OR HEAVY FREEZING RAIN
        CPWX(I) = "FZRA   "
      CASE (68,167)          ! SLIGHT RAIN OR DRIZZLE AND SNOW
        CPWX(I) = "-RASN  "
      CASE (69,168)          ! MODERATE/HEAVY, RAIN OR DRIZZLE AND SNOW
        CPWX(I) = "RASN   "
      CASE (71,171)          ! SLIGHT CONTINUOUS SNOWFALL
        CPWX(I) = "-SN    "
      CASE (73,172)          ! MODERATE CONTINUOUS SNOWFALL   
        CPWX(I) = "SN     "
      CASE (75,173)          ! HEAVY CONTINUOUS SNOWFALL
        CPWX(I) = "+SN    "
      CASE (76)              ! DIAMOND DUST
        CPWX(I) = "IC     "
      CASE (77,122)          ! SNOW GRAINS
        CPWX(I) = "SG     "
      CASE (79,175)          ! ICE PELLETS
        CPWX(I) = "PL     "
      CASE (80,181)          ! SLIGHT RAIN SHOWERS
        CPWX(I) = "-SHRA  "
      CASE (81,182)          ! MODERATE OR HEAVY RAIN SHOWERS
        CPWX(I) = "SHRA   "
      CASE (83)              ! SLIGHT SHOWERS OF RAIN AND SNOW MIXED
        CPWX(I) = "-SHRASN"
      CASE (84)              ! MODERATE/HEAVY SHOWERS RAIN & SNOW MIXED
        CPWX(I) = "SHRASN "
      CASE (85,185)          ! SLIGHT SNOW SHOWERS
        CPWX(I) = "-SHSN  "
      CASE (86,186)          ! MODERATE OR HEAVY SNOW SHOWERS
        CPWX(I) = "SHSN   "
                             ! SHOWERS OF SMALL HAIL OR ICE PELLETS WITH
      CASE (88)              ! OR WITHOUT RAIN OR RAIN AND SNOW MIXED 
        CPWX(I) = "GS     "
                             ! SHOWERS OF HAIL, WITH OR WITHOUT RAIN OR
      CASE (90)              ! RAIN AND SNOW MIXED 
        CPWX(I) = "GR     "
      CASE (95,192)          ! TSTORM WITHOUT HAIL, WITH RAIN OR SNOW
        CPWX(I) = "TSRA   "
      CASE (96,193)          ! TSTORM WITH HAIL
        CPWX(I) = "TSGR   "
      CASE (97,195)          ! TSTORM, HEAVY WITHOUT HAIL, RAIN OR SNOW
        CPWX(I) = "+TSRA  "
      CASE (98)              ! TSTORM WITH DUSTSTORM OR SANDSTORM
        CPWX(I) = "TSDS   "
      CASE (121,140)         ! UNDETERMINED PRECIP FROM AUTO
        CPWX(I) = "UP     "  
      CASE (156)             ! HEAVY FREEZING DRIZZLE, AUTO ONLY
        CPWX(I) = "+FZDZ  "
      CASE (166)             ! HEAVY FREEZING RAIN, AUTO ONLY
        CPWX(I) = "+FZRA  "
      CASE (174)             ! SLIGHT ICE PELLETS, AUTO ONLY
        CPWX(I) = "-PL    "
      CASE (176)             ! HEAVY ICE PELLETS. AUTO ONLY
        CPWX(I) = "+PL    "
      CASE (183)             ! HEAVY RAIN SHOWERS, AUTO ONLY
        CPWX(I) = "+SHRA  "
      CASE (187)             ! HEAVY SNOW SHOWERS, AUTO ONLY
        CPWX(I) = "+SHSN  "
      CASE (196)             ! TSTORM, HEAVY WITH HAIL
        CPWX(I) = "+TSGR  "
      CASE (204)             ! VOLCANIC ASH 
        CPWX(I) = "VA     "     
      CASE (207)             ! BLOWING SPRAY
        CPWX(I) = "BLPY   "  ! PY
      CASE (208)             ! DRIFTING DUST (SAND)
        CPWX(I) = "DRDU   "  ! DRSA
      CASE (219)             ! FUNNEL CLOUD
        CPWX(I) = "FC     "     
      CASE (999:)
        CPWX(I) = "       "
      CASE DEFAULT
        CPWX(I) = "       "
        WRITE(6,200) CALL(1),HR,MIN,WMOPRWX(I),PRWX(1,I)
  200   FORMAT(' ','STATION ',A8,'AT TIME ',A2,A2,' HAS AN UNKNOWN', 
     &  ' PRESENT WEATHER OF: ',A7,F8.2)
      END SELECT

      END DO

C     END SUBROUTINE WEATHER 
      END
