      SUBROUTINE TRNWND(KFILDO,WDIR,WNDTRN,NSTA)
C
C        MARCH    1993   GLAHN   TDL   LAMP-2000
C
C        PURPOSE
C            TO TURN A WIND DIRECTION CLOCKWISE BY WNDTRN DEGREES.
C
C        DATA SET USE
C            KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C             WDIR(K) = WIND DIRECTION (K=1,NSTA).  (INPUT/OUTPUT)
C              WNDTRN = NUMBER OF DEGREES TO TURN THE WIND.  (INPUT).
C                NSTA = NUMBER OF VALUES IN WDIR( ).  (INPUT)
C     
C
C        NONSYSTEM SUBROUTINES CALLED
C            NONE.
C
      DIMENSION WDIR(NSTA)
C      
      IF(WNDTRN.EQ.0.)GO TO 160
C
      DO 150 K=1,NSTA
CD     WRITE(KFILDO,100)K,WDIR(K),WNDTRN
CD100  FORMAT(/' IN TRNWND AT 100--K,WDIR(K),WNDTRN',I5,2F10.2)
C
         IF(WDIR(K).NE.9999..AND.WDIR(K).NE.0.)THEN
            WDIR(K)=MOD(WDIR(K)+WNDTRN,360.)
            IF(WDIR(K).EQ.0.)WDIR(K)=360.
         ENDIF
CD     WRITE(KFILDO,101)K,WDIR(K),WNDTRN
CD101  FORMAT(' IN TRNWND AT 101--K,WDIR(K),WNDTRN',I5,2F10.2)
C
 150  CONTINUE
C
 160  RETURN
      END
