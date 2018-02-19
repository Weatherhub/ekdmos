      SUBROUTINE SCALX(KFILDO,XDATA,NVAL,CONST,NSCALE,IER)
C
C        AUGUST    2005   GLAHN      MDL   MOS-2000
C        JUNE      2006   WIEDENFELD MDL   ADDED NINT IN IF STATMENT.
C                                          REARRANGED DOC BLOCK 
C                                          VARIABLES TO BE IN THE ORDER 
C                                          THEY WERE PASSED.
C
C        PURPOSE
C            TO SCALE A VARIABLE X CONST*10**NSCALE.  THIS CAN BE
C            USED FOR VECTOR OR GRIDDED DATA.
C   
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C            XDATA(K) = THE DATA TO SCALE (K=1,NVAL).  (INPUT-OUTPUT)
C                NVAL = THE NUMBER OF VALUES IN XDATA( ) BEING DEALT
C                       WITH.  (INPUT)
C               CONST = THE MULTIPLIER FOR SCALING.  (INPUT)
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
      IER=0
C
      FACTOR=CONST*10**NSCALE
C
      DO 120 K=1,NVAL
         IF(NINT(XDATA(K)).NE.9999)XDATA(K)=REAL(NINT(XDATA(K)*FACTOR))
 120  CONTINUE
C
D     WRITE(KFILDO,125)(XDATA(K),K=1,NVAL)
D125  FORMAT(/,' IN SCALX',/,(15F8.2))
C
      RETURN
      END
