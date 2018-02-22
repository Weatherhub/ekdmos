      SUBROUTINE RDWMO(JUNIT,NMOSTA,LIST,WMO,NWMO,NHEAD,MAXSTA)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    RDWMO       READS WMO HEADERS AND ASSOCIATED STATIONS  
C   PRGMMR: COSGROVE          ORG: W/OST22    DATE: 02-01-30
C                                                                       
C ABSTRACT: READS A LIST OF WMO HEADERS AND THEIR ASSOCIATED STATION    
C           LISTS.  IT KEEPS TRACK OF THE NUMBER OF STATIONS IN EACH    
C           WMO HEADER, AND KEEPS 1 LONG STATION LIST IN THE ORDER      
C           THAT THE WMO HEADERS ARE READ IN.                           
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   92-07-08  GILBERT                                                   
C   98-07-29  SHIREY   -    REMOVED W3LOG
C   02-01-30  COSGROVE -    CHANGED FORMAT STATEMENT TO READ IN 
C                           A8,1X FOR USE WITH MOS2000.
C   05-02-09  COSGROVE -    FIXED READING OF AWIPS ID
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     SUBROUTINE RDWMO                                                  
C                                                                       
C     JULY 1992   GILBERT    MDL   NAS9000 
C     NOV  1995   WEISS  MDL(RDC)  HOBBS AND CRAY C90
C                                                                       
C        PURPOSE                                                        
C           READS A LIST OF WMO HEADERS AND THEIR ASSOCIATED STATION    
C           LISTS.  IT KEEPS TRACK OF THE NUMBER OF STATIONS IN EACH    
C           WMO HEADER, AND KEEPS 1 LONG STATION LIST IN THE ORDER      
C           THAT THE WMO HEADERS ARE READ IN.                           
C                                                                       
C        DATA SET USE                                                   
C                FT05 - CONTROL DATA (INPUT)                            
C                                                                       
C        VARIABLES
C
C         DTEMP() = TEMPORARY ARRAY FOR ROUTINE RDLSTA.                 
C          IVALEN = CHARACTER STRING LENGTH (ASSUME ALL CHARACTER STRINGS
C                   ARE OF THIS LENGTH).
C           JUNIT = FILE NUMBER(5) USED TO ACCESS STATION
C                   DIRECTORY FILE
C          LIST() = LIST OF CALL LETTERS FOR ALL STATIONS
C          NMOSTA = TOTAL NUMBER OF STATIONS
C          MAXSTA = MAXIMUM NUMBER OF STATIONS
C           NHEAD = NUMBER OF WMO HEADERS 
C          WMO(I) = LIST OF WMO HEADERS                                 
C         NWMO(I) = NUMBER OF STATIONS IN WMO HEADER WMO(I).
C            TERM = TERMINATOR FOR EACH STATION LIST.
C                                                                       
C REMARKS:  STATION LISTS MUST BE IN 8(A8,1X) FORMAT 
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN                                                
C   MACHINE:  IBM SP
C                                                                       
C$$$                                                                    
      CHARACTER*8 LIST(MAXSTA),TERM,DTEMP(8)
      CHARACTER*18 WMO(MAXSTA),CFMT
      INTEGER NWMO(MAXSTA),MAXSTA,NMOSTA,LEFT,IVALEN
      DATA CFMT/'(8A8)   '/,TERM/'ZZZZZZZZ'/
      NMOSTA=0
      IVALEN=8
C
C        READ IN NUMBER OF DIFFERENT WMO HEADERS                
      READ(JUNIT,10)NHEAD
 10   FORMAT(I3)
C
C        READ IN WMO HEADER AND STATION LIST ASSOCIATED WITH THAT       
C        WMO HEADER.                                                    
      DO 100 I=1,NHEAD
        READ(JUNIT,20)WMO(I)
 20     FORMAT(A18)
        LEFT=MAXSTA-NMOSTA
        CALL RDLSTA(JUNIT,LIST(NMOSTA+1),LEFT,DTEMP,8,CFMT,NWMO(I),
     *              TERM,IVALEN)
        NMOSTA=NMOSTA+NWMO(I)
        IF (NMOSTA.GT.MAXSTA) THEN
          WRITE(6,30) NMOSTA,MAXSTA
 30       FORMAT(' NUMBER OF INPUT STATIONS = ',I4,' EXCEEDS ',I4/
     *           ' STOP IN SUBROUTINE RDWMO.')
         STOP 30
        ENDIF
 100  CONTINUE

      RETURN
      END
