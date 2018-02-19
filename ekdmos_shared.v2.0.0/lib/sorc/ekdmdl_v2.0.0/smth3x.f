      SUBROUTINE SMTH3X(KFILDO,P,Q,NX,NY)                                    
C
C        APRIL 1999   GLAHN   TDL   MOS-2000
C
C        PURPOSE
C            SMOOTHS A 2-DIMENSIONAL ARRAY WITH A 25-POINT SMOOTHER
C            THRICE.  ON EACH PASS, THE SMOOTHED POINT IS JUST THE
C            AVERAGE OF ITSELF AND ITS 24 NEIGHBORS IN A SQUARE BOX
C            AROUND IT.  A LINEAR GRADIENT PERPENDICULAR TO THE
C            BOUNDARY IS ASSUMED FOR THE OUTER ROWS AND COLUMNS.
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
C        CALCULATE ALL POINTS EXCEPT OUTER TWO ROWS AND COLUMNS.
C
      CALL SMTH25(KFILDO,P,Q,NX,NY)                                    
      CALL SMTH25(KFILDO,P,Q,NX,NY)                                    
      CALL SMTH25(KFILDO,P,Q,NX,NY)                                    
C
      RETURN                                                            
      END                                                               
