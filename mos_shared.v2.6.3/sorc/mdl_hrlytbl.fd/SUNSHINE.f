
C *********************************************************************
      SUBROUTINE SUNSHINE(SUNSH,SUN,IHR) 

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                                                                       
C SUBPROGRAM:   SUNSHINE     PREPARES DURATION OF SUNSHINE FOR MDLTABLE 
C   PRGMMR: K HUGHES         ORG: W/OSD211     DATE: 98-07-28           
C                                                                       
C ABSTRACT: DURATION OF SUNSHINE IS REPORTED IN METAR AT A SMALL        
C   PERCENTAGE OF STATIONS.  IT IS REPORTED AT 0800 UTC IN THE 98MMM
C   GROUP.  IT IS CODED AS THE NUMBER OF MINUTES DURING THE PREVIOUS
C   CALENDAR DAY.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-11-21  K. HUGHES                              
C   98-07-28  K. HUGHES     ADDED MORE DOCUMENTATION                 
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     PROGRAM NAME                                                      
C                                                                       
C        DATE   KATHRYN HUGHES    MDL   CRAY-J916                   
C                                                                       
C        PURPOSE                                                        
C            THIS SUBROUTINE PREPARES THE BUFR VALUES OF THE
C            DURATION OF SUNSHINE FOR THE MDL HOURLY TABLES
C            THE SUNSHINE IS ONLY REPORTED AT 0800 UTC
C                                                                       
C        DATA SET USE                                                   
C            NONE - DATA IS PROCESSED FROM PASSED ARRAYS
C                                                                       
C        VARIABLES                                                      
C               SUNSH = ARRAY OF BUFR VALUES OF SUNSHINE   INPUT     
C                 SUN = MINUTES OF SUNSHINE               OUTPUT     
C                 IHR = HOUR BEING PROCESSED               INPUT   
C                                                                       
C        SUBPROGRAMS CALLED:
C            NONE                                
C                                                                       
C        PROGRAM STOPS                                                  
C            NONE                                                 
C                                                                       
C        REMARKS                                                        
C            NO SUNSHINE IS CODED AS 98000                              
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90                                                 
C   MACHINE:  CRAY-J916                                              
C$$$                                                                    
        IMPLICIT NONE
      REAL, INTENT(IN)   :: SUNSH(1,255)
      INTEGER            :: ISUN,IHR
      CHARACTER(LEN=3)   :: SUN

C       USE INTERNAL WRITE STATEMENTS TO CONVERT REAL VALUES OF
C       SUNSHINE TO CHARACTER.

      IF (IHR.EQ.8) THEN
        ISUN=NINT(SUNSH(1,1))
      ELSE
        ISUN=9999
      END IF

      WRITE(SUN,100) ISUN
  100 FORMAT(I3)
 
      IF (ISUN.GT.999.) SUN='   '

C     END SUBROUTINE SUNSHINE
      END
