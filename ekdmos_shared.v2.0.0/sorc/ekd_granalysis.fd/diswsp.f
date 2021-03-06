      SUBROUTINE DISWSP(KFILDO,XDATA,NVAL,TRUNC,CONST,NSCALE,IER)
C
C        JANUARY   2007   GLAHN   TDL   MOS-2000
C        APRIL     2007   GLAHN   MODIFIED FORMAT 102;
C                                 CHANGED 10**NSCALE TO 10.**NSCALE
C
C        PURPOSE
C            TO SCALE A VARIABLE X CONST*10**NSCALE AFTER REMOVING
C            ALL VALUES LT TRUNC.  THIS CAN BE USED FOR VECTOR OR
C            GRIDDED DATA.
C   
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C            XDATA(K) = THE DATA TO SCALE (K=1,NVAL).  (INPUT-OUTPUT)
C               CONST = THE MULTIPLIER FOR SCALING.  (INPUT)
C                NVAL = THE NUMBER OF VALUES IN XDATA( ) BEING DEALT
C                       WITH.  (INPUT)
C               TRUNC = ALL VALUES BELOW TRUNC WILL BE SET TO ZERO.
C                       (INPUT)
C              NSCALE = THE POWER OF TEN FOR SCALING.  (INPUT)
C                 IER = ERROR RETURN.
C                       0 = GOOD RETURN.
C                       (OUTPUT)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES USED 
C            NONE
C
      DIMENSION XDATA(NVAL)
C
D     CALL TIMPR(KFILDO,KFILDO,'START DISWSP        ')
      IER=0
C
      FACTOR=CONST*10.**NSCALE
D     WRITE(KFILDO,102)NSCALE,CONST,TRUNC,FACTOR
D102  FORMAT(/' AT 102 IN DISWSP--NSCALE,CONST,TRUNC,FACTOR',I4,3F10.4)
C
      DO 120 K=1,NVAL
C
      IF(XDATA(K).NE.9999.)THEN
C
         IF(XDATA(K).LT.TRUNC)THEN
            XDATA(K)=0.
         ELSE
            XDATA(K)=XDATA(K)*FACTOR
         ENDIF
C
      ENDIF
C
 120  CONTINUE
C
D     WRITE(KFILDO,125)TRUNC,(XDATA(K),K=1,NVAL)
D125  FORMAT(/,' IN DISWSP--TRUNC',F8.2,/,(15F8.2))
C
      RETURN
      END
