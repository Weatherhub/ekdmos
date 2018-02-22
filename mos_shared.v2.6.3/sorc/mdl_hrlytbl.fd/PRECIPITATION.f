

C *********************************************************************

      SUBROUTINE PRECIPITATION(PRECIP,CPCP01,CPCP03,CPCP06,CPCP24,IHR,
     *                         TYPE,CALL) 

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                                                                       
C SUBPROGRAM: PRECIPITATION  PREPARES 1- 3- 6- AND 24-HR PRECIP         
C   PRGMMR: K. HUGHES        ORG: W/OSD211     DATE: 98-07-28           
C                                                                       
C ABSTRACT: PREPARES THE 1-HR 3-HR 6-HR AND 24-HR PRECIPITATION FOR
C           THE MDL HOURLY TABLES.  THE VALUES ARE READ FROM THE ARRAY
C           PRECIP WHICH CONTAINS THE PRECIPITATION DATA IN BUFR STANDARDS
C           AND CONVERTS THEM BACK TO THE METAR VALUES IN HUNDREDTHS OF
C           INCHES.  NO REPORTS OF PRECIP ARE CONSIDERED A REPORT OF
C           0 INCHES AT VALID HOURS.	
C           
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-11-21  K. HUGHES                              
C   98-07-28  K. HUGHES    ADDED DOCUMENTATION   
C   02-03-06  R. COSGROVE  AS OF MARCH 19TH, A TRACE OF PRECIP WILL
C                          BE DENOTED AS -0.1 FOR MNEMONICS TP01,TP03,TP06,
C                          AND TP24 IN THE BUFR TANKS.  IT WAS PREVIOUSLY
C                          DENOTED AS -1.  ADDED CHECK FOR ANY VALUE COMING
C                          OUT OF BUFR THAT'S LESS THAN -0.05, AND CHANGED THAT 
C                          TO -4 AUTOMATICALLY.
C   05-10-05  R. COSGROVE  CHANGED IT SO THAT WE WILL NOW SAVE 1-HR PRECIP
C                          FROM MANU, A01, A01A, AUTO IN ADDITION TO THE A02
C                          AND A02A THAT WE WERE ALREADY SAVING.
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     PRECIPITATION.F                                                   
C                                                                       
C        98-07-28  KATHRYN HUGHES    MDL   CRAY-J916                       
C                                                                       
C        PURPOSE                                                        
C            SUBROUTINE PRECIPITATION PULLS OUT THE 1 HR, 3 HR, 6 HR
C            AND 24 HR PRECIPITATION AMOUNTS OUT OF THE ARRAY PRECIP.
C            IT WILL ALSO CONVERT THEM FROM THE WMO STANDARD UNIT OF
C            KG/METER**2 TO HUNDREDTHS OF INCHES.
C            INDETERMINATE VALUES ARE CHANGED TO -9, AND TRACE AMOUNTS
C            OF PRECIPITATION ARE CHANGED TO -4.  

C                                                                       
C        DATA SET USE                                                   
C            BEGIN HERE-INDICATE INPUT,OUTPUT OR INTERNAL               
C                                                                       
C        VARIABLES                                                      
C            INPUT:
C                CALL = 8-LETTER STATION CALL LETTERS  
C                 IHR = HOUR OF METAR REPORT IN GMT  
C              PRECIP = ARRAY CONTAINING 1-,3-,6-, AND 24-HOUR
C                       PRECIP AMOUNT IN KG/METER**2
C                TYPE = MANUAL OR AUTOMATED STATION TYPE
C           OUTPUT:
C              CPCP01 = 1-HR PRECIP AMOUNT IN HUNDREDTHS OF INCHES
C                       VALID ONLY AT AO2 AND AO2A STATIONS
C              CPCP03 = 3-HR PRECIP AMOUNT IN HUNDREDTHS OF INCHES
C              CPCP06 = 6-HR PRECIP AMOUNT IN HUNDREDTHS OF INCHES
C              CPCP24 = 24-HR PRECIP AMOUNT IN HUNDREDTHS OF INCHES
C            WORK:
C               PCP01 = INTEGER REPRESENTATION OF 1-HR PRECIP AMOUNT
C               PCP03 = INTEGER REPRESENTATION OF 3-HR PRECIP AMOUNT
C               PCP06 = INTEGER REPRESENTATION OF 6-HR PRECIP AMOUNT
C               PCP24 = INTEGER REPRESENTATION OF 24-HR PRECIP AMOUNT
C              
C                                                                       
C        SUBPROGRAMS CALLED:  NONE CALLED
C                                                                       
C        PROGRAM STOPS                                                  
C                  NONE                                                   
C                                                                       
C        REMARKS                                                        
C            ONE HOUR PRECIP AMOUNTS ARE CURRENTLY ONLY ACCEPTED FROM
C            STATION TYPES AO2 AND AO2A. - CHANGED 10/2005.  
C            AS OF 3/19/2002, A TRACE IN THE BUFR
C            TANK IS DENOTED WITH -0.1.  PRIOR TO 3/19/2002, IT WAS -1. 
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90                                                 
C   MACHINE:  CRAY-J916                                                   
C$$$                                                                    
        IMPLICIT NONE

      REAL, INTENT(IN)   :: PRECIP(4,255)
      INTEGER            :: PCP01,PCP03,PCP06,PCP24,IHR
      CHARACTER(LEN=4)   :: CPCP01,CPCP03,CPCP06,CPCP24,TYPE
      CHARACTER(LEN=8)   :: CALL(51)
C
C        FIRST CHECK IF THE VALUE FROM BUFR IS A TRACE.  IF SO, CHANGE TO
C        -4.  OTHERWISE, PROCEED TO THE FORMULA.
C
      IF(PRECIP(1,1).LT.-0.05)THEN
         PCP01=-4
      ELSE
         PCP01=NINT(PRECIP(1,1)/25.4*100.)
      ENDIF

      IF ((TYPE.EQ."AO2 ").OR.(TYPE.EQ."AO2A").OR.(TYPE.EQ."MANU")
     *    .OR.(TYPE.EQ."AO1 ").OR.(TYPE.EQ."AO1A").OR.(TYPE.EQ."AUTO"))
     *     THEN
        PCP01=PCP01
        IF (PCP01.GT.9999) PCP01=-9
      ELSE
        IF ((PCP01.LT.9999).AND.(PCP01.GT.0)) THEN
         PRINT *,"ONE HOUR PRECIP NOT SAVED BUT REPORTED AT ",
     *   CALL(1),"STATION TYPE: ",TYPE," PRECIP AMOUNT: ",PCP01
        END IF
      PCP01=99999
      END IF

      IF(PRECIP(2,1).LT.-0.05)THEN
         PCP03=-4
      ELSE
         PCP03=NINT(PRECIP(2,1)/25.4*100.)
      ENDIF

      IF(PRECIP(3,1).LT.-0.05)THEN
         PCP06=-4
      ELSE
         PCP06=NINT(PRECIP(3,1)/25.4*100.)
      ENDIF

      IF(PRECIP(4,1).LT.-0.05)THEN
         PCP24=-4
      ELSE
         PCP24=NINT(PRECIP(4,1)/25.4*100.)
      ENDIF

C       USE INTERNAL WRITE STATEMENTS TO CONVERT INTEGER VALUES OF
C       PRECIP TO CHARACTER.

      WRITE(CPCP01,100) PCP01
      WRITE(CPCP03,100) PCP03
      WRITE(CPCP06,100) PCP06
      WRITE(CPCP24,100) PCP24
  100 FORMAT(I4)
 
      IF (PCP01.GT.9999) CPCP01='    '
      IF (PCP03.GT.9999) CPCP03='    ' 
      IF (PCP06.GT.9999) CPCP06='    '
      IF (PCP24.GT.9999) CPCP24='    '

C        CHANGE MISSING VALUES RETURNED FROM BUFR FILES AT
C        VALID HOURS TO A VALUE REPRESENTATIVE OF AN
C        INDETERMINATE AMOUNT (METAR EQUIVALENT OF 6////)

      IF ((IHR.EQ.3).OR.(IHR.EQ.9).OR.(IHR.EQ.15).OR.(IHR.EQ.21)) THEN
        IF (PCP03.GT.9999) CPCP03='  -9'
      END IF

      IF ((IHR.EQ.0).OR.(IHR.EQ.6).OR.(IHR.EQ.12).OR.(IHR.EQ.18)) THEN
        IF (PCP06.GT.9999) CPCP06='  -9'
      END IF

      IF (IHR.EQ.12) THEN
        IF (PCP24.GT.9999) CPCP24='  -9'
      END IF


C     END SUBROUTINE PRECIPITATION
      END
