      SUBROUTINE TRNSFR(WORK,P,ND)
C
C        AUGUST 2000   GLAHN   TDL   LAMP-2000
C
C        PURPOSE
C           TO TRANSFER ONE ARRAY TO ANOTHER.
C
C        DATA SET USE
C            NONE
C
C        VARIABLES 
C
C            WORK(ND) = THE INPUT ARRAY.  (INPUT)
C               P(ND) = THE OUTPUT ARRAY.  (OUTPUT)
C                  ND = SIZE OF THE ARRAYS.  (INPUT)
C
C        NONSYSTEM SUBROUTINES USED 
C            NONE
C            
      DIMENSION WORK(ND),P(ND)
C
      DO 120 J=1,ND
         P(J)=WORK(J)
 120  CONTINUE
C
      RETURN
      END
