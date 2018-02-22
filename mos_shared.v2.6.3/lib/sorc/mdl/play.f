      SUBROUTINE PLAY(X,Y,N)                                               
C
C        GLAHN   APRIL 1975   TDL   IBM 360/195                                  
C        GILBERT MARCH 1991   TDL   NAS 9000    -CONVERTED TO FORTRAN 77 
C        GLAHN   APRIL 1998   TDL   ADAPTED TO UNIX AND MOS-2000     
C
C        PURPOSE                                                           
C            TO PLACE N CHARACTERS FROM X( ) INTO Y( ).                      
C
C        DATA SET USE
C            NONE
C
C        VARIABLES                                                         
C                X(J) = CHARACTERS TO PLACE INTO Y( ) (J=1,N).
C                       (CHARACTER*1)  (INPUT)                               
C                Y(J) = CHARACTERS FROM X(J) (J=1,N).                              
C                       (CHARACTER*1)  (OUTPUT)                               
C                   N = NUMBER OF CHARACTERS TO COPY FROM X( ) INTO Y( ).
C                       (INPUT)          
C                                                                          
      CHARACTER*1 X(N),Y(N)
C
      DO 110 K=1,N 
      Y(K)=X(K)                                                          
 110  CONTINUE 
C
      RETURN
      END
