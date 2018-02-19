      SUBROUTINE SMTH9(KFILDO,P,Q,NX,NY)                                     
C
C        MAY 1994   GLAHN   TDL   MOS-2000
C
C        PURPOSE
C            SMOOTHS A 2-DIMENSIONAL ARRAY WITH A 9-POINT SMOOTHER. 
C            THE SMOOTHED POINT IS JUST THE AVERAGE OF ITSELF AND ITS
C            8 CLOSEST NEIGHBORS.  A LINEAR GRADIENT IS ASSUMED FOR THE
C            OUTER ROWS AND COLUMNS.  ASSUMPTION OF LINEAR GRADIENT 
C            PERPENDICULAR TO THE EDGE RESULTS IN SMOOTHING ONLY ALONG THE
C            EDGE.  THAT IS, THE EXTRAPOLATED VALUE OUTSIDE THE GRID
C            BALANCES THE VALUE JUST INSIDE THE GRID. 
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
C        NON SYSTEM ROUTINES CALLED
C            NONE.
C      
      DIMENSION P(NX,NY),Q(NX,NY)                                     
C
D     CALL TIMPR(KFILDO,KFILDO,'START SMTH9         ')
C
C        CALCULATE ALL POINTS EXCEPT OUTER ROWS AND COLUMNS.
C
      DO 110 JY=2,NY-1                                                     
      DO 109 IX=2,NX-1                                                     
      Q(IX,JY)=0.
C                                                              
      DO 105 LL=JY-1,JY+1                                                 
      DO 104 KK=IX-1,IX+1                                                 
      Q(IX,JY)=Q(IX,JY)+P(KK,LL)                                         
 104  CONTINUE                                                          
 105  CONTINUE
C                                                          
      Q(IX,JY)=Q(IX,JY)/9.                                                
 109  CONTINUE                                                          
 110  CONTINUE                                                          
C                                                          
C        CALCULATE UPPER AND LOWER ROW VALUES, EXCEPT FOR CORNER POINTS.         
C
      DO 115 IX=2,NX-1                                                     
      Q(IX,1)= (P(IX,1) +P(IX-1,1) +P(IX+1,1))/3.                     
      Q(IX,NY)=(P(IX,NY)+P(IX-1,NY)+P(IX+1,NY))/3.                 
 115  CONTINUE                                                          
C                                                          
C        CALCULATE RIGHT AND LEFT COLUMN VALUES, EXCEPT FOR CORNER POINTS.         
C           
      DO 125 JY=2,NY-1                                                     
      Q(1,JY)= (P(1,JY) +P(1,JY-1) +P(1,JY+1))/3.                     
      Q(NX,JY)=(P(NX,JY)+P(NX,JY-1)+P(NX,JY+1))/3.                 
 125  CONTINUE                                                          
C
C*****************
C        THE SEGMENT BELOW WAS TRIED, BUT COULD GIVE A REVERSAL OF
C        GRADIENT AT THE CORNER.
C*****************
C        CALCULATE CORNER POINTS.  NEGATIVE TERMS ARE OMITTED (WITH A
C        DIVISION ADJUSTMENT) SO THAT A FIELD WITH NO NEGATIVE NUMBERS 
C        WILL NOT SMOOTH TO A NEGATIVE VALUE.  (THIS AGREES WITH 1975 
C        VERSION OF CORNRS IN THE MOS SYSTEM.)
C
C***      Q(1,1)=  (P(1,1)*7.  +P(2,1)*2.    +P(1,2)*2.)/11.
C***      Q(1,NY)= (P(1,NY)*7. +P(2,NY)*2.   +P(1,NY-1)*2.)/11.
C***      Q(NX,1)= (P(NX,1)*7. +P(NX,2)*2.   +P(NX-1,1)*2.)/11.
C***      Q(NX,NY)=(P(NX,NY)*7.+P(NX,NY-1)*2.+P(NX-1,NY)*2.)/11.
C*****************
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
      DO 130 JY=1,NY
      DO 129 IX=1,NX
      P(IX,JY)=Q(IX,JY)
 129  CONTINUE
 130  CONTINUE
      RETURN                                                            
      END                                                               
