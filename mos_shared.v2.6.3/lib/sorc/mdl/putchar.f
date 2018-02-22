      SUBROUTINE PUTCHAR(X,Y,N)                                            
C        GLAHN   APRIL 1975   IBM 360/195                               
C        GILBERT MARCH 1991   NAS 9000    -CONVERTED TO FORTRAN 77
C        WEISS   SEPT  1995   HOBBS AND CRAY C90
C                             SUBROUTINE NAME CHANGED FROM PLAY TO
C                             PUTCHAR. NO OTHER CHANGES MADE.
C        PURPOSE                                                        
C            TO PLACE N CHARACTERS FROM X() INTO Y().                   
C        VARIABLES                                                      
C            X() = INPUT LINE (N CHARACTERS)                            
C            Y() = OUTPUT LINE (N CHARACTERS)                           
C              N = NUMBER OF CHARACTERS TO COPY FROM X() INTO Y()       
C                                                                       
      CHARACTER*1 X(N),Y(N)                                             
      DO 110 K=1,N                                                      
        Y(K)=X(K)                                                       
 110  CONTINUE                                                          
      RETURN                                                            
      END                                                               
