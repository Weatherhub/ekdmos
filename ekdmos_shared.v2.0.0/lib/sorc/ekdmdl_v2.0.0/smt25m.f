      SUBROUTINE SMT25M(KFILDO,P,Q,NX,NY)
C
C        JUNE     2005  CHARBA   MDL   MOS-2000
C        FEBRUARY 2006  CHARBA   PERFORMED MINOR CLEAN-UP OF DOCUMEN-
C                                TATION AND CHANGED TEST FOR MISSING
C                                FROM FLOATING POINT TO INTEGER FORM.
C                                THIS WAS DONE TO SATISFY CODE WALK-
C                                THRU.
C        JULY     2006  CHARBA   RETURNED CHECK FOR MISSING FROM 
C                                INTEGER TO FLOATING POINT FORM ...THE
C                                FORMER CHECK RESULTED IN EXCESSIVE 
C                                COMPUTER TIME USAGE.
C        JULY     2006  CHARBA   MODIFIED THE "PURPOSE" TO NOTE THAT
C                                SMOOTHER ALSO ACTS TO PROVIDE "GRID
C                                FILL".
C
C        PURPOSE
C           SMOOTHS A 2-DIMENSIONAL ARRAY WITH A 25-POINT SMOOTHER. 
C           THE SMOOTHED POINT IS THE SIMPLE AVERAGE OF ITSELF AND ITS
C           24 NEIGHBORS IN A SQUARE BOX AROUND IT.  SMT25M WAS ADAPTED 
C	    FROM SMTH25 TO ACCOUNT FOR MISSING (9999) VALUES.  MISSING 
C	    VALUES ARE NOT USED IN THE SMOOTHING, BUT IF ONE OR MORE OF
C           THE SURROUNDING 24 GRIDPOINTS IS NON-MISSING THE (ADAPTED)
C           SMOOTHED VALUE REPLACES THE MISSING VALUE.  (IF ONLY ONE OF
C           THE 24 GRIDPOINTS IS NON-MISSING, THIS NON-SMOOTHED VALUE
C           REPLACES THE MISSING VALUE.)  THUS, THE SMOOTHING ALSO ACTS
C           TO PROVIDE GRID FILLING. 
C
C	    TO REDUCE COMPUTATION TIME THE OUTER TWO GRID ROWS AND 
C           COLUMNS ARE NOT SMOOTHED.  THUS, THESE ROWS AND COLUMNS 
C           SHOULD NOT BE USED IN APPLICATIONS.  (AN ALTERNATE VERSION
C           OF SMT25M THAT SMOOTHS AT THE BOUNDARIES IS AVAILABLE.)
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
C        SMOOTH ALL POINTS EXCEPT OUTER TWO ROWS AND COLUMNS.
C
      DO 110 JY=3,NY-2                                                     
      DO 109 IX=3,NX-2                                                     
      Q(IX,JY)=0.                                                        
      NN=0
C                                                          
      DO 105 LL=JY-2,JY+2                                                 
      DO 104 KK=IX-2,IX+2 
      IF(P(KK,LL).GT.9998.5) GO TO 104                       
      Q(IX,JY)=Q(IX,JY)+P(KK,LL)                                         
      NN=NN+1                                               
 104  CONTINUE                                                          
 105  CONTINUE
C
      IF(NN.EQ.0)THEN
         Q(IX,JY)=9999.
      ELSE
         Q(IX,JY)=Q(IX,JY)/NN    
      ENDIF 
 109  CONTINUE                                                          
 110  CONTINUE
C        
C	 MOVE SMOOTHED INTERIOR TO P( , ) AND RETURN.
C
      DO 220 JY=3,NY-2
      DO 210 IX=3,NX-2
      P(IX,JY)=Q(IX,JY)
 210  CONTINUE
 220  CONTINUE     
C
      RETURN                                                            
      END                                                               
