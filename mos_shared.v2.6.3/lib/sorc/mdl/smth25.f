      SUBROUTINE SMTH25(KFILDO,P,Q,NX,NY)                                    
C
C        MAY 1994   GLAHN   TDL   MOS-2000
C
C        PURPOSE
C            SMOOTHS A 2-DIMENSIONAL ARRAY WITH A 25-POINT SMOOTHER. 
C            THE SMOOTHED POINT IS JUST THE AVERAGE OF ITSELF AND ITS
C            24 NEIGHBORS IN A SQUARE BOX AROUND IT.  A LINEAR GRADIENT
C            PERPENDICULAR TO THE BOUNDARY IS ASSUMED FOR THE OUTER ROWS 
C            AND COLUMNS.
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
      DO 110 JY=3,NY-2                                                     
      DO 109 IX=3,NX-2                                                     
      Q(IX,JY)=0.                                                        
C                                                          
      DO 105 LL=JY-2,JY+2                                                 
      DO 104 KK=IX-2,IX+2                                                 
      Q(IX,JY)=Q(IX,JY)+P(KK,LL)                                         
 104  CONTINUE                                                          
 105  CONTINUE
C                                                          
      Q(IX,JY)=Q(IX,JY)/25.                                               
 109  CONTINUE                                                          
 110  CONTINUE
C                                                          
C        CALCULATE UPPER AND LOWER INNER BOUNDARY VALUES.         
C           
      DO 115 IX=3,NX-2                                                     
      Q(IX,2)=((P(IX-2,1)+P(IX-1,1)+P(IX,1)+P(IX+1,1)+P(IX+2,1))*3.+    
     1          P(IX-2,3)+P(IX-1,3)+P(IX,3)+P(IX+1,3)+P(IX+2,3)+
     2          P(IX-2,4)+P(IX-1,4)+P(IX,4)+P(IX+1,4)+P(IX+2,4))/25.                       
      Q(IX,NY-1)=((P(IX-2,NY)+P(IX-1,NY)+P(IX,NY)+P(IX+1,NY)+             
     1 P(IX+2,NY))*3.+
     2             P(IX-2,NY-3)+P(IX-1,NY-3)+P(IX,NY-3)+P(IX+1,NY-3)+        
     3 P(IX+2,NY-3)+
     4             P(IX-2,NY-2)+P(IX-1,NY-2)+P(IX,NY-2)+P(IX+1,NY-2)+            
     5 P(IX+2,NY-2))/25.                                                  
 115  CONTINUE
C                                                          
C        CALCULATE LEFT AND RIGHT INNER BOUNDARY VALUES.         
C                                                          
      DO 125 JY=3,NY-2                                                     
      Q(2,JY)=((P(1,JY-2)+P(1,JY-1)+P(1,JY)+P(1,JY+1)+P(1,JY+2))*3.+    
     1          P(3,JY-2)+P(3,JY-1)+P(3,JY)+P(3,JY+1)+P(3,JY+2)+
     2          P(4,JY-2)+P(4,JY-1)+P(4,JY)+P(4,JY+1)+P(4,JY+2))/25.                       
      Q(NX-1,JY)=((P(NX,JY-2)+P(NX,JY-1)+P(NX,JY)+P(NX,JY+1)+             
     1 P(NX,JY+2))*3.+
     2             P(NX-3,JY-2)+P(NX-3,JY-1)+P(NX-3,JY)+P(NX-3,JY+1)+
     3 P(NX-3,JY+2)+
     4             P(NX-2,JY-2)+P(NX-2,JY-1)+P(NX-2,JY)+P(NX-2,JY+1)+            
     3 P(NX-2,JY+2))/25.                                                  
 125  CONTINUE
C                                                          
C        CALCULATE UPPER AND LOWER BOUNDARY VALUES.         
C           
      DO 135 IX=3,NX-2                                                     
      Q(IX,1)=((P(IX-2,1)+P(IX-1,1)+P(IX,1)+P(IX+1,1)+P(IX+2,1))*6.-    
     1         (P(IX-2,2)+P(IX-1,2)+P(IX,2)+P(IX+1,2)+P(IX+2,2))*2.+            
     2          P(IX-2,3)+P(IX-1,3)+P(IX,3)+P(IX+1,3)+P(IX+2,3))/25.             
      Q(IX,NY)=((P(IX-2,NY)+P(IX-1,NY)+P(IX,NY)+P(IX+1,NY)+             
     1 P(IX+2,NY))*6.-
     2          (P(IX-2,NY-1)+P(IX-1,NY-1)+P(IX,NY-1)+P(IX+1,NY-1)+       
     3 P(IX+2,NY-1))*2.+
     4           P(IX-2,NY-2)+P(IX-1,NY-2)+P(IX,NY-2)+P(IX+1,NY-2)+        
     5 P(IX+2,NY-2))/25.                                                  
 135  CONTINUE
C                                                          
C        CALCULATE LEFT AND RIGHT BOUNDARY VALUES.         
C                                                          
      DO 145 JY=3,NY-2                                                     
      Q(1,JY)=((P(1,JY-2)+P(1,JY-1)+P(1,JY)+P(1,JY+1)+P(1,JY+2))*6.-    
     1         (P(2,JY-2)+P(2,JY-1)+P(2,JY)+P(2,JY+1)+P(2,JY+2))*2.+            
     2          P(3,JY-2)+P(3,JY-1)+P(3,JY)+P(3,JY+1)+P(3,JY+2))/25.             
      Q(NX,JY)=((P(NX,JY-2)+P(NX,JY-1)+P(NX,JY)+P(NX,JY+1)+
     1 P(NX,JY+2))*6.-
     2          (P(NX-1,JY-2)+P(NX-1,JY-1)+P(NX-1,JY)+P(NX-1,JY+1)+
     3 P(NX-1,JY+2))*2.+
     4           P(NX-2,JY-2)+P(NX-2,JY-1)+P(NX-2,JY)+P(NX-2,JY+1)+
     5 P(NX-2,JY+2))/25.                                                  
 145  CONTINUE 
C                                                         
C        CALCULATE INNER BOUNDARY CORNERS WITH 9 POINT SMOOTHER.
C  
      Q(2,2)=(P(1,1)+P(2,1)+P(3,1)+
     1        P(1,2)+P(2,2)+P(3,2)+
     2        P(1,3)+P(2,3)+P(3,3))/9. 
      Q(2,NY-1)=(P(1,NY-2)+P(2,NY-2)+P(3,NY-2)+
     1           P(1,NY-1)+P(2,NY-1)+P(3,NY-1)+
     2           P(1,NY)  +P(2,NY)  +P(3,NY))/9. 
      Q(NX-1,2)=(P(NX-2,1)+P(NX-1,1)+P(NX,1)+
     1           P(NX-2,2)+P(NX-1,2)+P(NX,2)+
     2           P(NX-2,3)+P(NX-1,3)+P(NX,3))/9. 
      Q(NX-1,NY-1)=(P(NX-2,NY-2)+P(NX-1,NY-2)+P(NX,NY-2)+
     1              P(NX-2,NY-1)+P(NX-1,NY-1)+P(NX,NY-1)+
     2              P(NX-2,NY)  +P(NX-1,NY)  +P(NX,NY))/9.
C 
C        CALCULATE POINTS ADJACENT TO CORNERS WITH 9 POINT.
C 
      Q(2,1)=    (P(1,1)    +P(2,1)    +P(3,1))/3. 
      Q(1,2)=    (P(1,1)    +P(1,2)    +P(1,3))/3. 
      Q(2,NY)=   (P(1,NY)   +P(2,NY)   +P(3,NY))/3. 
      Q(1,NY-1)= (P(1,NY-2) +P(1,NY-1) +P(1,NY))/3. 
      Q(NX-1,1)= (P(NX-2,1) +P(NX-1,1) +P(NX,1))/3. 
      Q(NX,2)=   (P(NX,1)   +P(NX,2)   +P(NX,3))/3. 
      Q(NX-1,NY)=(P(NX-2,NY)+P(NX-1,NY)+P(NX,NY))/3. 
      Q(NX,NY-1)=(P(NX,NY-2)+P(NX,NY-1)+P(NX,NY))/3.
C
C*****************
C        THE SEGMENT BELOW WAS TRIED, BUT COULD GIVE A REVERSAL OF
C        GRADIENT.
C*****************
C
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
      DO 200 JY=1,NY
      DO 199 IX=1,NX
      P(IX,JY)=Q(IX,JY)
 199  CONTINUE
 200  CONTINUE
C
      RETURN                                                            
      END                                                               
