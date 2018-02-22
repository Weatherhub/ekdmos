      SUBROUTINE PRMSG(MSG,N1,N2,KOUNT,M1,M2,NUNIT,NOHEAD)              
C          GLAHN   FEBRUARY 1976  IBM 360/195                           
C          GILBERT MARCH 1991  NAS 9000  - COMPILED IN FORTRAN 77       
C                                        - ADDED NOHEAD PARAMETER
C          WEISS   SEPT 1995   HOBBS AND CRAY C90, COSMETIC CODE 
C                              CHANGES
C
C          PURPOSE                                                      
C            TO PRINT MESSAGE A TELETYPE MESSAGE IN EASY TO READ 
C            FORMAT.

CCCCCCCCC
C
C        VARIABLES
C            KOUNT(,) = 1, IF A FORECAST IS ON THAT LINE
C                  M1 = 1ST DIM. OF KOUNT(,) = NO. OF LINES
C                  M2 = 2ND DIM. OF KOUNT(,) = NO. OF STATIONS
C              MSG(,) = ARRAY CONTAINING THE ALPHANUMERIC FORE-
C                       CAST MESSAGE (INPUT)
C                  N1 = 1ST DIM. OF MSG(,) = NO. OF CHARS IN LINE
C                  N2 = 2ND DIM. OF MSG(,) = NUMBER OF LINES
C              NOHEAD = NUMBER OF LINES IN HEADER, INCLUDING BLANK LINES
C               NUNIT = DATA SET REFERENCE NUMBER FOR OUTPUT
C
CCCCCCCCC

      DIMENSION KOUNT(M1,M2)                                            
      CHARACTER*1 MSG(N1,N2)

      WRITE(NUNIT,105)                                                  
 105  FORMAT('1')

C        PRINT NOHEAD LINES OF HEADING                                  
      DO 112 K=1,NOHEAD                                                 
        WRITE(NUNIT,110) (MSG(J,K),J=2,N1)                              
 110    FORMAT(' ',150A1)                                               
 112  CONTINUE                                                          
      DO 131 J=1,M2                                                     
        DO 130 K=1,M1                                                   
          IF (KOUNT(K,J).NE.0) THEN                                     
            WRITE(NUNIT,111) (MSG(L,(J-1)*M1+K+NOHEAD),L=2,N1)          
 111        FORMAT(' ',150A1)                                           
          ENDIF                                                         
 130    CONTINUE                                                        
 131  CONTINUE                                                          
      RETURN                                                            
      END                                                               
