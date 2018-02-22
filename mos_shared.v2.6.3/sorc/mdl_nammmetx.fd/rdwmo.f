      SUBROUTINE RDWMO(JUNIT,NMOSTA,LIST,WMO,NWMO,NHEAD,MAXSTA)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    RDWMO       READS WMO HEADERS AND ASSOCIATED STATIONS  
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 92-07-08
C           WEISS                 RDC              95-11-15
C                                                                       
C ABSTRACT: READS A LIST OF WMO HEADERS AND THEIR ASSOCIATED STATION    
C           LISTS.  IT KEEPS TRACK OF THE NUMBER OF STATIONS IN EACH    
C           WMO HEADER, AND KEEPS 1 LONG STATION LIST IN THE ORDER      
C           THAT THE WMO HEADERS ARE READ IN.                           
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   92-07-08  GILBERT                                                   
C   97-10-14  GILBERT    - CHANGED TO READ IN PIL 
C   00-05-18  ALLEN      - CHANGED EXIT(30) TO STOP 30 AND
C                          ADDED CALL TO W3TAGE
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR TDL STANDARDS                                         
C                                                                       
C     SUBROUTINE RDWMO                                                  
C                                                                       
C     JULY 1992   GILBERT    TDL   NAS9000 
C     NOV  1995   WEISS  TDL(RDC)  HOBBS AND CRAY C90
C     JUN  1997   GILBERT          - CHANGED FORTRAN STOP TO CALL EXIT()
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
C         DTEMP() = TEMPORARY ARRAY FOR ROUTINE RDDATA.                 
C           JUNIT = FILE NUMBER(5) USED TO ACCESS MRF STATION
C                   DIRECTORY FILE
C          LIST() = LIST OF CALL LETTERS FOR ALL STATIONS
C          NMOSTA = TOTAL NUMBER OF STATIONS
C          MAXSTA = MAXIMUM NUMBER OF STATIONS
C           NHEAD = NUMBER OF WMO HEADERS 
C          WMO(I) = LIST OF WMO HEADERS                                 
C         NWMO(I) = NUMBER OF STATIONS IN WMO HEADER WMO(I).
C            TERM = TERMINATOR FOR EACH STATION LIST.
C                                                                       
C REMARKS:                                                              
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  CRAY C90 & HOBBS(H-P)
C                                                                       
C$$$                                                                    
      CHARACTER*8 LIST(MAXSTA),TERM,DTEMP(8),CFMT
      CHARACTER*18 WMO(MAXSTA)
      INTEGER NWMO(MAXSTA),MAXSTA,NMOSTA,LEFT,IVALEN
      DATA CFMT/'(8A8)   '/,TERM/'ZZZZZZZZ'/
      NMOSTA=0
      IVALEN=8

C        READ IN NUMBER OF DIFFERENT WMO HEADERS (=112)                 
      READ(JUNIT,10)NHEAD
 10   FORMAT(I3)
      WRITE(6,11) NHEAD
 11   FORMAT(' NHEAD:',i3)

C        READ IN WMO HEADER AND STATION LIST ASSOCIATED WITH THAT       
C        WMO HEADER.                                                    
      DO 100 I=1,NHEAD
        READ(JUNIT,20)WMO(I)
 20     FORMAT(A18)
        WRITE(6,21) WMO(I)
 21     FORMAT(' WMO HEADER:',A18)
        LEFT=MAXSTA-NMOSTA
        CALL RDLSTA(JUNIT,LIST(NMOSTA+1),LEFT,DTEMP,8,CFMT,NWMO(I),
     *              TERM,IVALEN)
        NMOSTA=NMOSTA+NWMO(I)
        WRITE(6,22) NMOSTA 
 22     FORMAT(' AFTER RDLSTA NMOSTA = ',i4)
        IF (NMOSTA.GT.MAXSTA) THEN
          WRITE(6,30) NMOSTA,MAXSTA
 30       FORMAT(' NUMBER OF INPUT STATIONS = ',I4,' EXCEEDS ',I4/
     *           ' STOP IN SUBROUTINE RDWMO.')
          CALL W3TAGE('RDWMO')
	  STOP 30
        ENDIF
 100  CONTINUE

      RETURN
      END
