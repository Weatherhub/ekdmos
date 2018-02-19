      SUBROUTINE MINUS(KFILDO,X1,L1X1,L2X1,X2,L1X2,L2X2,X3,L1X3,L2X3,
     1                 N1,N2,IER)
C
C        SEPTEMBER 2006   GLAHN   MDL   MOS-2000
C                                 COPIED FROM CARD DECK AND PUT INTO
C                                 MOS-2000 FORM
C        MARCH     2007   GLAHN   INSERTED REAL*8 COMMENT UNDER PURPOSE.
C                                
C        PURPOSE
C           TO SUBYRACT MATRIX IN X2 FROM X1 AND PUT THE RESULT INTO
C           X3.  X3 CAN BE THE SAME AS X1 OR X2, IN WHICH CASE
C           ONE MATRIX WILL BE REPLACED WITH THE DIFFERENCE.
C
C           NOTE THAT THE MATRICES ARE REAL*8.
C           
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES 
C
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C             X1(J,K) = MINUEND MATRIX (J=1,N1) (K=1,N2) IN ARRAY
C                       X1(L1X1,L2X1).  (INPUT)  (REAL*8)
C             X2(J,K) = SUBTRAHEND MATRIX (J=1,N1) (K=1,N2) IN ARRAY
C                       X2(L1X2,L2X2).  (INPUT)  (REAL*8)
C             X3(J,K) = DIFFERNCE MATRIX (J=1,N1) (K=1,N2) IN ARRAY
C                       X3(L1X3,L2X3).  (OUTPUT)  (REAL*8)
C                  N1 = FIRST DIMENSION OF THE MATRICES.  NOTE
C                       THE DATA CAN OCCUPY ONLY A PORTION OF THE
C                       ARRAYS.  (INPUT)
C                  N2 = SECOND DIMENSION OF THE MATRIRICES.  NOTE
C                       THE DATA CAN OCCUPY ONLY A PORTION OF THE
C                       ARRAYS.  (INPUT)
C                 IER = ERROR RETURN.
C                       0 = GOOD RETURN.
C
C        1         2         3         4         5         6         7 X
C        NONSYSTEM SUBROUTINES USED 
C            NONE
C         
      REAL*8 X1,X2,X3 
C
      DIMENSION X1(L1X1,L2X1),X2(L1X2,L2X2),X3(L1X3,L2X3)
C
      IER=0
C
      DO 103 J=1,N2
C
      DO 102 K=1,N1
      X3(K,J)=X1(K,J)-X2(K,J)
 102  CONTINUE
C
 103  CONTINUE
C
      RETURN
      END
