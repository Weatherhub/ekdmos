      SUBROUTINE SMT2XM(KFILDO,P,Q,NX,NY)
C
C        JUNE  2005  CHARBA   MDL   MOS-2000
C
C        PURPOSE
C	    MAKES TWO SUCCESSIVE CALLS TO A 25-POINT SMOOTHING ROUTINE
C	    CALLED SMT25M.  SMT25M IS DIFFERENT THAN THE STANDARD MOS
C	    25-POINT SMOOTHER (SMTH25) IN THAT IT CHECKS FOR MISSING 
C	    VALUES IN THE GRID.  WHEN MISSING VALUES ARE FOUND THEY ARE
C	    RETAINED IN THE SMOOTHED FIELD. 
C
C        DATA SET USE
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C            P(IX,JY) = FIELD TO BE SMOOTHED (IX=1,NX) (JY=1,NY).
C                       (INPUT-OUTPUT)
C            Q(IX,JY) = WORK ARRAY (IX=1,NX) (JY=1,NY).  (INTERNAL)
C                  NX = SIZE OF GRID IN IX DIRECTION.  (INPUT)
C                  NY = SIZE OF GRID IN JY DIRECTION.  (INPUT)
C      
      DIMENSION P(NX,NY),Q(NX,NY)
C
C        PERFORM TWO PASSES WITH A 25-POINT SMOOTHER.
C
      CALL SMT25M(KFILDO,P,Q,NX,NY)                                    
      CALL SMT25M(KFILDO,P,Q,NX,NY)                                    
C
      RETURN                                                            
      END                                                               
