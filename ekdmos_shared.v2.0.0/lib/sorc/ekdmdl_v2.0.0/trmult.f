      SUBROUTINE TRMULT(KFILDO,X1,L1X1,L2X1,X2,L1X2,L2X2,X3,L1X3,L2X3,
     1                  N1,N2,N3,IER)
C
C        JULY      1977   GLAHN   IBM 360/195 
C        SEPTEMBER 2006   GLAHN   MDL   MOS-2000
C                                 COPIED FROM DALLAVALLE'S IBM AND PUT
C                                 INTO MOS-2000 FORM
C        MARCH     2007   GLAHN   INSERTED REAL*8 COMMENT UNDER PURPOSE.
C                                
C        PURPOSE
C           TO MULTIPLY THE TRANSPOSE OF ONE MATRIX X1 WITH ANOTHER
C           MATRIX X2 AND FORM PRODUCT IN X3.  THE DIMENSIONS OF THE
C           ARRAYS OF EACH MATRIX ARE FURNISHED ALONG WITH THE ACTUAL
C           MATRIX SIZES.
C
C           NOTE THAT THE MATRICES ARE REAL*8.
C           
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES 
C
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C             X1(J,K) = MULTIPLICAND MATRIX (J=1,N1) (K=1,N2) IN ARRAY
C                       X1(L1X1,L2X1).  (INPUT)  (REAL*8)
C             X2(J,K) = MULTIPLIER MATRIX (J=1,N1) (K=1,N3) IN ARRAY
C                       X1(L1X2,L2X2).  (INPUT)  (REAL*8)
C             X3(J,K) = PRODUCT MATRIX (J=1,N2) (K=1,N3) IN ARRAY
C                       X1(L1X3,L2X3).  (OUTPUT)  (REAL*8)
C                 IER = ERROR RETURN.
C                       0 = GOOD RETURN.
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES USED 
C            NONE
C                   
      REAL*8 X1,X2,X3
C
      DIMENSION X1(L1X1,L2X1),X2(L1X2,L2X2),X3(L1X3,L2X3)
C
      IER=0
C
 100  DO 106 K=1,N3  
C
      DO 105 J=1,N2    
      X3(J,K)=0.       
C      
      DO 104 L=1,N1  
      X3(J,K)=X3(J,K)+X1(L,J)*X2(L,K) 
 104  CONTINUE
C
 105  CONTINUE
C
 106  CONTINUE
C
      RETURN 
      END   
