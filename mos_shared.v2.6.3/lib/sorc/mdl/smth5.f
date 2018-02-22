      SUBROUTINE SMTH5(KFILDO,P,Q,NX,NY)                                     
C
C        MAY 1994   GLAHN   TDL   MOS-2000
C
C        PURPOSE
C            SMOOTHS A 2-DIMENSIONAL ARRAY WITH A 5-POINT SMOOTHER. 
C            THE SMOOTHED POINT IS JUST THE AVERAGE OF ITSELF AND ITS
C            4 CLOSEST NEIGHBORS.  A LINEAR GRADIENT IS ASSUMED FOR THE
C            OUTER ROWS AND COLUMNS.
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
D     CALL TIMPR(KFILDO,KFILDO,'START SMTH5         ')
C
C        CALCULATE ALL POINTS EXCEPT OUTER ROWS AND COLUMNS.
C

      DO 110 JY=2,NY-1                                                     
      DO 109 IX=2,NX-1                                                     
      Q(IX,JY)=(P(IX,JY)+P(IX-1,JY)+P(IX+1,JY)+P(IX,JY-1)+P(IX,JY+1))/5.      
 109  CONTINUE                                                          
 110  CONTINUE
C                                                          
C        CALCULATE UPPER AND LOWER ROW VALUES, EXCEPT FOR CORNER POINTS.         
C           
      DO 115 IX=2,NX-1                                                     
      Q(IX,1)= (P(IX,1)*3. +P(IX-1,1) +P(IX+1,1))/5.                       
      Q(IX,NY)=(P(IX,NY)*3.+P(IX-1,NY)+P(IX+1,NY))/5.                   
 115  CONTINUE
C                                                          
C        CALCULATE RIGHT AND LEFT COLUMN VALUES, EXCEPT FOR CORNER POINTS.         
C           
      DO 125 JY=2,NY-1                                                     
      Q(1,JY)= (P(1,JY)*3. +P(1,JY-1) +P(1,JY+1))/5.                       
      Q(NX,JY)=(P(NX,JY)*3.+P(NX,JY-1)+P(NX,JY+1))/5.                   
 125  CONTINUE
C
C        CORNER POINTS ARE UNCHCANGED.
C        (NOTE THAT THIS IS NOT WHAT WAS USED IN THE 1975 VERSION
C        OF CORNRS IN THE MOS SYSTEM.)
C
      Q(1,1)=  P(1,1)
      Q(1,NY)= P(1,NY)
      Q(NX,1)= P(NX,1)
      Q(NX,NY)=P(NX,NY)
C
C        MOVE SMOOTHED FIELD BACK INTO ORIGINAL ARRAY.
C
      DO 200 JY=1,NY
      DO 199 IX=1,NX
      P(IX,JY)=Q(IX,JY)
 199  CONTINUE
 200  CONTINUE
C
      RETURN                                                            
      END                                                               
