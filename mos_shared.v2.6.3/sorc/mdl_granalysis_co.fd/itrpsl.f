      SUBROUTINE ITRPSL(KFILDO,IP14,P,NX,NY,CCALL,BX,BY,LNDSEA,
     1                  SEALND,NXE,NYE,
     2                  MESH,MESHE,N4P,BB,ISTOP,IER)
C
C        JANUARY   2005   GLAHN   MDL MOS-2000
C        JANUARY   2005   GLAHN   MINOR DIAGNOSTIC CHANGES
C        FEBRUARY  2005   GLAHN   PURPOSE; DIAGNOSTIC; DISTSQ
C        FEBRUARY  2005   GLAHN   MODIFIED DIAGNOSTICS
C        JUNE      2005   GLAHN   MODIFIED /D DIAGNOSTIC PRINT
C        AUGUST    2005   GLAHN   MODIFIED USE OF LNDSEA AND 
C                                 SEALND( , )
C        OCTOBER   2005   GLAHN   MOVED COMMENTS 1 SPACE TO LEFT;
C                                 MODIFIED FORMAT 150
C        DECEMBER  2005   JPD     REPLACED FLOAT FUNCTION WITH
C                                 REAL FUNCTION; MODIFIED IF
C                                 TESTS CHECKING ON EQUALITY
C                                 OF REAL VALUES TO USE NINT
C                                 STATEMENT; MODIFIED FORMAT
C                                 STATEMENTS TO CORRESPOND TO
C                                 FORTRAN90 STANDARDS ON THE IBM
C        MARCH     2006   GLAHN   ADDED IP14 AND ISTOP( ) TO CALL
C        MARCH     2006   GLAHN   ADDED N4P AND ADDITIONAL SEARCH;
C                                 INCREMENTED ISTOP(5) AT 495
C        APRIL     2006   GLAHN   CHECKED IP14 NE 0 BEFORE WRITING
C        SEPTEMBER 2006   GLAHN   CHANGED N4PPR TO 16 RATHER THAN 12
C                                 WHEN N4P = 12
C  
C        PURPOSE
C            TO INTERPOLATE INTO FIELD P(NX,NY) AT POINT BX, BY, AND
C            RETURN VALUE IN BB.  ITRPSL IS FOR USE WITH U155/U405A.
C            IT DISTINGUISHES BETWEEN LAND AND WATER DATA POINTS AND
C            LAND AND WATER GRIDPOINTS.  BILINEAR INTERPOLATION IS
C            USED WHEN THE FOUR POINTS SURROUNDING THE BX, BY POINT
C            ARE OF USABLE TYPE; CLOSEST POINT IS USED OTHERWISE.
C            IF THERE IS NO NON-MISSING POINT OF SAME TYPE IN
C            4 SURROUNDING POINTS, N4P IS CHECKED AND IF IT =12,
C            12 ADDITIONAL POINTS ARE CHECKED.  IF NO POINT OF THE
C            CORRECT TYPE IS FOUND, THE POINT IS NOT USED AND A
C            DIAGNOSTIC FURNISHED.
C
C        DATA SET USE
C            KFILDO   - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C            IP14     - UNIT NUMBER FOR LISTING COMPUTED LAPSE
C                       RATES AND PROBLEMS WITH LAPSE RATES.  (OUTPUT)
C
C        VARIABLES
C                IP14 = UNIT NUMBER FOR LISTING COMPUTED LAPSE
C                       RATES AND PROBLEMS WITH LAPSE RATES.  (INPUT)
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C            P(IX,JY) = GRID INTO WHICH INTERPOLATION IS DESIRED
C                       (IX=1,NY) (JY=1,NY).  (INPUT)
C               NX,NY = SIZE OF P( , ).  (INPUT)
C               CCALL = STATION CALL LETTERS.  (CHARACTER*8)  (INPUT)
C               BX,BY = POINT IN P( , ) FOR WHICH VALUE IS DESIRED.
C                       (INPUT)
C              LNDSEA = LAND/SEA INFLUENCE FLAG FOR EACH STATION.
C                       0 = WILL BE USED FOR ONLY OCEAN WATER (=0)
C                           GRIDPOINTS.
C                       3 = WILL BE USED FOR ONLY INLAND WATER (=3)
C                           GRIDPOINTS.
C                       6 = WILL BE USED FOR BOTH INLAND WATER (=3)
C                           AND LAND (=9) GRIDPOINTS.
C                       9 = WILL BE USED FOR ONLY LAND (=9) GRIDPOINTS.
C                       (INPUT)
C       SEALND(IX,JY) = THE SEA/LAND MASK (IX=1,NXE) (JY=1,NYE).
C                       0 = OCEAN WATER GRIDPOINTS;
C                       3 = INLAND WATER GRIDPOINTS.
C                       9 = LAND GRIDPOINTS.
C             NXE,NYE = SIZE OF SEALND( , ).  (INPUT)
C                MESH = MESH LENGTH OF GRID IN P( , ).  (INPUT)
C               MESHE = MESH LENGTH OF GRID IN SEALND( , ).  (INPUT) 
C                 N4P = 4  INDICATES THE SURROUNDING 4 POINTS WILL BE
C                          CHECKED WHEN TRYING TO FIND A GRIDPOINT OF
C                          THE SAME TYPE AS THE DATUM AND INTERPOLATION
C                          CAN'T BE DONE.  CURRENTLY, THIS IS ALWAYS
C                          DONE (DOES NOT REQUIRE N4P=4).
C                       12 SAME AS ABOVE, EXCEPT 12 ADDITIONAL POINTS
C                          WILL BE CHECKED WHEN NONE OF THE 4 POINTS
C                          ARE OF THE CORRECT TYPE.
C                       N4P IS OPERATIVE ONLY WHEN THE DATUM AND
C                       THE SURROUNDING 4 POINTS ARE OF MIXED TYPE.
C                       (INPUT)
C                  BB = RETURNED VALUE.  (OUTPUT)
C            ISTOP(J) = ISTOP(1)--IS INCREMENTED BY 1 EACH TIME AN ERROR 
C                                 OCCURS.
C                       ISTOP(2)--IS INCREMENTED WHEN THERE ARE
C                                 FEW DATA (200) FOR AN ANALYSIS.
C                       ISTOP(3)--IS INCREMENTED WHEN A DATA RECORD 
C                                 COULD NOT BE FOUND.
C                       ISTOP(4)--IS INCREMTED WHEN A LAPSE RATE COULD
C                                 NOT BE COMPUTED OR HAS TOO FEW CASES
C                                 TO BE USED.
C                       ISTOP(5)--IS INCREMENTED WHEN NO NON-MISSING
C                                 GRIDPOINT AROUND THE DATA POINT IS
C                                 OF THE SAME TYPE.
C                       (INPUT/OUTPUT)
C                 IER = ERROR RETURN.
C                         0 = GOOD RETURN.
C                       195 = LNDSEA POINT VALUE NOT 0, 3, 6, OR 9.
C                       196 = VALUE COULD NOT BE RETURNED.
C                       (OUTPUT)
C               RMESH = RATIO OF MESH LENGTH OF GRID IN P( , ) TO 
C                       GRID IN SEALND( , ).  (INTERNAL)
C               N4PPR = VALUE TO PRINT FOR NUMBER OF SURROUONDING
C                       STATIONS.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED
C            NONE.
C
      CHARACTER*8 CCALL
C
      DIMENSION P(NX,NY)
      DIMENSION SEALND(NXE,NYE)
      DIMENSION ISTOP(5)  
C
      IER=0
      N4PPR=4
      IF(N4P.EQ.12)N4PPR=16
C
      NBX=BX 
      NBY=BY
C
C        MAKE SURE P( , ) GRID IS ACCESSED WITHIN BOUNDARIES.
C   
      IF(NBX.LT.1)THEN
         NBX=1
      ELSEIF(NBX.GT.NX-1)THEN
         NBX=NX-1
      ENDIF
C
      IF(NBY.LT.1)THEN
         NBY=1
      ELSEIF(NBY.GT.NY-1)THEN
         NBY=NY-1
      ENDIF 
C
      NBXP1=NBX+1
      NBYP1=NBY+1      
C
C        NB* WILL BE USED TO INDEX IN P( , );
C        JB* WILL BE USED TO INDEX IN SEALND( , ).
C        THEY ARE THE SAME WHEN MESH = MESHE (THE USUAL CASE);
C        THEY MUST BE ADJUSTED OTHERWISE.  IF NB* IS WITHIN
C        BOUNDARIES, SO WILL BE JB*.
C
      IF(MESH.EQ.MESHE)THEN
         JBX=NBX
         JBY=NBY
         JBXP1=NBXP1
         JBYP1=NBYP1
      ELSE
         RMESH=REAL(MESH)/REAL(MESHE)
C           RMESH IS THE RATIO OF THE MESH LENGTH OF THE ANALYSIS
C           GRID TO THE TERRAIN GRID.
         JBX=NINT((NBX-1)*RMESH)+1
         JBY=NINT((NBY-1)*RMESH)+1
         JBXP1=NINT((NBXP1-1)*RMESH)+1
         JBYP1=NINT((NBYP1-1)*RMESH)+1
      ENDIF
C
D     IF(LNDSEA.EQ.0)THEN
D        WRITE(KFILDO,110)CCALL,BX,BY,NBX,NBY,JBX,JBY,LNDSEA,
D    1                    SEALND(JBX,JBY),SEALND(JBX,JBYP1),
D    2                    SEALND(JBXP1,JBY),SEALND(JBXP1,JBYP1),
D    3                    P(NBX,NBY),P(NBX,NBYP1),
D    4                    P(NBXP1,NBY),P(NBXP1,NBYP1)
D110     FORMAT(/' AT 110 IN ITRPSL--CCALL,BX,BY,NBX,NBY,JBX,JBY,',
D    1           'LNDSEA,SEALND(4),P(4)',/,
D    1            A8,2F8.2,4I6,I3,8F8.2)
D     ENDIF
C
      IF(LNDSEA.EQ.0)THEN
C
C           THIS IS A WATER OBSERVATION.
C
         IF(NINT(SEALND(JBX,JBY)).EQ.0.AND.
     1      NINT(SEALND(JBX,JBYP1)).EQ.0.AND.
     2      NINT(SEALND(JBXP1,JBY)).EQ.0.AND.
     3      NINT(SEALND(JBXP1,JBYP1)).EQ.0)THEN
C              INTERPOLATE.            
            GO TO 200 
         ELSE
C              NOT ALL POINTS ARE WATER.
            TEST1=0.
            TEST2=0.
            GO TO 300
         ENDIF
C
      ELSEIF(LNDSEA.EQ.9)THEN
C
C           THIS IS A LAND OBSERVATION. 
C
         IF(NINT(SEALND(JBX,JBY)).EQ.9.AND.
     1      NINT(SEALND(JBX,JBYP1)).EQ.9.AND.
     2      NINT(SEALND(JBXP1,JBY)).EQ.9.AND.
     3      NINT(SEALND(JBXP1,JBYP1)).EQ.9)THEN
C              INTERPOLATE.            
            GO TO 200 
         ELSE
C              NOT ALL POINTS ARE LAND.
            TEST1=9.
            TEST2=9.
            GO TO 300
         ENDIF
C
      ELSEIF(LNDSEA.EQ.3)THEN
C
C           THIS IS AN INLAND WATER OBSERVATION. 
C
         IF(NINT(SEALND(JBX,JBY)).EQ.3.AND.
     1      NINT(SEALND(JBX,JBYP1)).EQ.3.AND.
     2      NINT(SEALND(JBXP1,JBY)).EQ.3.AND.
     3      NINT(SEALND(JBXP1,JBYP1)).EQ.3)THEN
C              INTERPOLATE.            
            GO TO 200 
         ELSE
C              NOT ALL POINTS ARE INLAND WATER.
            TEST1=3.
            TEST2=3.
            GO TO 300
         ENDIF
C
      ELSEIF(LNDSEA.EQ.6)THEN
C
C           THIS POINT CAN BE USED FOR EITHER LAND OR INLAND
C           WATER.
C
         IF(NINT(SEALND(JBX,JBY)).EQ.6.AND.
     1      NINT(SEALND(JBX,JBYP1)).EQ.6.AND.
     2      NINT(SEALND(JBXP1,JBY)).EQ.6.AND.
     3      NINT(SEALND(JBXP1,JBYP1)).EQ.6)THEN
C              INTERPOLATE.  THIS WOULD BE UNUSUAL.          
            GO TO 200 
         ELSE
C              NOT ALL POINTS ARE USABLE BY EITHER LAND 
C              OR INLAND WATER.  THE CLOSEST SITE CAN BE
C              EITHER LAND OR INLAND WATER.
            TEST1=3.
            TEST2=9.
            GO TO 300
         ENDIF
C        
      ELSE
         WRITE(KFILDO,150)LNDSEA
 150     FORMAT(/' ****LAND/WATER VALUE LNDSEA =',I6,
     1           ' NOT 0, 3, 6, OR 9 IN',
     2           ' ITRPSL AT 150.')
         IER=195
         BB=9999.
         GO TO 500
      ENDIF
C
C        START BI-LINEAR INTERPOLATION.  MAKE SURE NO VALUE
C        NEEDED IN INTERPOLATION IS MISSING.  IT HAS BEEN
C        DETERMINED THE POINTS MATCH LAND/WATER WISE.
C
 200  IF(P(NBX,NBY)    .NE.9999..AND.
     1   P(NBXP1,NBY)  .NE.9999..AND.
     2   P(NBXP1,NBYP1).NE.9999..AND.
     3   P(NBX,NBYP1)  .NE.9999.)THEN
         DX=BX-REAL(NBX) 
         DY=BY-REAL(NBY) 
         BB=P(NBX,NBY)
     1    +(P(NBXP1,NBY)-P(NBX,NBY))*DX
     2    +(P(NBX,NBYP1)-P(NBX,NBY))*DY
     3    +(P(NBX,NBY)+P(NBXP1,NBYP1)-P(NBX,NBYP1)-P(NBXP1,NBY))*DX*DY
      ELSE
C   
C           THERE IS ONE OR MORE MISSING GRIDPOINT VALUES, SO
C           INTERPOLATION CAN'T BE DONE.  FIND CLOSEST NON MISSING
C           VALUE.
C
         BB=9999.
         DISTSQ=99999999.
C
D        WRITE(KFILDO,250)CCALL,LNDSEA
D250     FORMAT(/' CLOSEST NEIGHBOR VALUE USED FOR STATION ',A8,
D    1           ', LAND/WATER TYPE =',I3)
C
         IF(P(NBX,NBY).NE.9999.)THEN
            DISTSQ=(BX-NBX)**2+(BY-NBY)**2
            BB=P(NBX,NBY)
         ENDIF  
C
         IF(P(NBXP1,NBY).NE.9999.)THEN
            D=(BX-NBXP1)**2+(BY-NBY)**2
C
            IF(D.LT.DISTSQ)THEN
               DISTSQ=D
               BB=P(NBXP1,NBY)
            ENDIF
C
         ENDIF            
C   
         IF(P(NBX,NBYP1).NE.9999.)THEN
            D=(BX-NBX)**2+(BY-NBYP1)**2
C
            IF(D.LT.DISTSQ)THEN
               DISTSQ=D
               BB=P(NBX,NBYP1)
            ENDIF
C
         ENDIF  
C  
         IF(P(NBXP1,NBYP1).NE.9999.)THEN
            D=(BX-NBXP1)**2+(BY-NBYP1)**2
C
            IF(D.LT.DISTSQ)THEN
               DISTSQ=D
               BB=P(NBXP1,NBYP1)
            ENDIF
C
         ENDIF  
C
      ENDIF              
C
      IF(NINT(BB).EQ.9999)THEN
c           THIS SITUATION CAN HAPPEN IF THE FIRST GUESS DOES NOT 
C           COVER THE TOTAL ANALYSIS AREA.  THIS IS NOT COUNTED
C           AS AN ISTOP ERROR IN THE CALLING PROGRAM
         IER=196
D        IF(IP14.NE.0)WRITE(IP14,295)CCALL,LNDSEA
D295     FORMAT(/' #### VALUE FOR STATION ',A8,' NOT USED.  ALL OF THE',
D    1           ' 4 SURROUNDING POINTS OF THE LAND/WATER TYPE =',I3,
D    2           ' ARE MISSING.')
         ISTOP(5)=ISTOP(5)+1
         IF(IP14.NE.0)WRITE(IP14,295)CCALL,LNDSEA
 295     FORMAT(/' #### VALUE FOR STATION ',A8,' NOT USED.  ALL OF THE',
     1           ' 4 SURROUNDING POINTS OF THE LAND/WATER TYPE =',I3,
     2           ' ARE MISSING.',/,
     3           '  PROBABLY OUTSIDE ANALYSIS AREA.')
         WRITE(KFILDO,295)CCALL,LNDSEA
C
      ENDIF
C      
      GO TO 500             
C
C        NOT ALL GRIDPOINTS SURROUNDING A WATER POINT ARE WATER
C        OR NOT ALL GRIDPOINTS SURROUNDING A LAND POINT ARE LAND.
C        MUST FIND CLOSEST POINT, CHECKING FOR MISSINGS AND
C        TYPE OF POINT SET ABOVE TO TEST1 AND TEST2.
C 
C        FIND CLOSEST NON MISSING VALUE OF THE CORRECT TYPE
C        (LAND OR WATER).
C
 300  BB=9999.
      DISTSQ=99999999.
C
D        WRITE(KFILDO,350)CCALL,LNDSEA
D350     FORMAT(/' CLOSEST NEIGHBOR VALUE USED FOR STATION ',A8,
D    1           ', LAND/WATER TYPE =',I3)
C
      IF(P(NBX,NBY).NE.9999..AND.
     1                    (SEALND(JBX,JBY).EQ.TEST1.OR.
     2                     SEALND(JBX,JBY).EQ.TEST2))THEN
         DISTSQ=(BX-NBX)**2+(BY-NBY)**2
         BB=P(NBX,NBY)
      ENDIF  
C
      IF(P(NBXP1,NBY).NE.9999..AND.
     1                     (SEALND(JBXP1,JBY).EQ.TEST1.OR.
     2                      SEALND(JBXP1,JBY).EQ.TEST2))THEN
         D=(BX-NBXP1)**2+(BY-NBY)**2
C
         IF(D.LT.DISTSQ)THEN
            DISTSQ=D
            BB=P(NBXP1,NBY)
         ENDIF
C
      ENDIF            
C   
      IF(P(NBX,NBYP1).NE.9999..AND.
     1                     (SEALND(JBX,JBYP1).EQ.TEST1.OR.
     2                      SEALND(JBX,JBYP1).EQ.TEST2))THEN
         D=(BX-NBX)**2+(BY-NBYP1)**2
C
         IF(D.LT.DISTSQ)THEN
            DISTSQ=D
            BB=P(NBX,NBYP1)
         ENDIF
C
      ENDIF  
C  
      IF(P(NBXP1,NBYP1).NE.9999..AND.
     1                       (SEALND(JBXP1,JBYP1).EQ.TEST1.OR.
     2                        SEALND(JBXP1,JBYP1).EQ.TEST2))THEN
         D=(BX-NBXP1)**2+(BY-NBYP1)**2
C
         IF(D.LT.DISTSQ)THEN
            DISTSQ=D
            BB=P(NBXP1,NBYP1)
         ENDIF
C
      ENDIF  
C
      IF(NINT(BB).EQ.9999.AND.N4P.EQ.12)THEN  
C
C           TO LOOK AT THE 12 OTHER POINTS, REQUIRE THAT POINTS
C           CAN'T BE OFF THE GRID.
C
         NBXM1=NBX-1
         NBYM1=NBY-1
         NBXP2=NBX+2
         NBYP2=NBY+2
C
C           TO LOOK AT THE 12 OTHER POINTS, REQUIRE THAT POINTS
C           CAN'T BE OFF THE GRID.  IT SHOULD BE UNUSUAL THAT
C           THERE ARE NOT TWO ROWS AND COLUMNS AROUND THE DATUM.
C         
         IF(NBXM1.GE.1.AND.NBYM1.GE.1.AND.
     1      NBXP2.LE.NX.AND.NBYP2.LE.NY)THEN
C
            IF(MESH.EQ.MESHE)THEN
               JBXM1=NBXM1
               JBYM1=NBYM1
               JBXP2=NBXP2
               JBYP2=NBYP2
            ELSE
C                 RMESH IS THE RATIO OF THE MESH LENGTH OF THE ANALYSIS
C                 GRID TO THE TERRAIN GRID.
               JBXM1=NINT((NBXM1-1)*RMESH)+1
               JBYM1=NINT((NBYM1-1)*RMESH)+1
               JBXP2=NINT((NBXP2-1)*RMESH)+1
               JBYP2=NINT((NBYP2-1)*RMESH)+1
            ENDIF
C
C              THESE ARE THE TWO ON THE LEFT SIDE.
C
            IF(P(NBXM1,NBY).NE.9999..AND.
     1                          (SEALND(JBXM1,JBY).EQ.TEST1.OR.
     2                           SEALND(JBXM1,JBY).EQ.TEST2))THEN
               D=(BX-NBXM1)**2+(BY-NBY)**2
C
               IF(D.LT.DISTSQ)THEN
                  DISTSQ=D
                  BB=P(NBXM1,NBY)
               ENDIF
C
            ENDIF  
C
            IF(P(NBXM1,NBYP1).NE.9999..AND.
     1                          (SEALND(JBXM1,JBYP1).EQ.TEST1.OR.
     2                           SEALND(JBXM1,JBYP1).EQ.TEST2))THEN
               D=(BX-NBXM1)**2+(BY-NBYP1)**2
C
               IF(D.LT.DISTSQ)THEN
                  DISTSQ=D
                  BB=P(NBXM1,NBYP1)
               ENDIF
C
            ENDIF  
C
C              THESE ARE THE TWO ON THE RIGHT SIDE.
C                        
            IF(P(NBXP2,NBY).NE.9999..AND.
     1                           (SEALND(JBXP2,JBY).EQ.TEST1.OR.
     2                            SEALND(JBXP2,JBY).EQ.TEST2))THEN
               D=(BX-NBXP2)**2+(BY-NBY)**2
C
               IF(D.LT.DISTSQ)THEN
                  DISTSQ=D
                  BB=P(NBXP2,NBY)
               ENDIF
C       
            ENDIF                 
C
            IF(P(NBXP2,NBYP1).NE.9999..AND.
     1                           (SEALND(JBXP2,JBYP1).EQ.TEST1.OR.
     2                            SEALND(JBXP2,JBYP1).EQ.TEST2))THEN
               D=(BX-NBXP2)**2+(BY-NBYP1)**2
C
               IF(D.LT.DISTSQ)THEN
                  DISTSQ=D
                  BB=P(NBXP2,NBYP1)
               ENDIF
C
            ENDIF
C
C              THESE ARE THE TWO BELOW.
C
            IF(P(NBX,NBYM1).NE.9999..AND.
     1                          (SEALND(JBX,JBYM1).EQ.TEST1.OR.
     2                           SEALND(JBX,JBYM1).EQ.TEST2))THEN
               D=(BX-NBX)**2+(BY-NBYM1)**2
C
               IF(D.LT.DISTSQ)THEN
                  DISTSQ=D
                  BB=P(NBX,NBYM1)
               ENDIF
C
            ENDIF  
C
            IF(P(NBXP1,NBYM1).NE.9999..AND.
     1                          (SEALND(JBXP1,JBYM1).EQ.TEST1.OR.
     2                           SEALND(JBXP1,JBYM1).EQ.TEST2))THEN
               D=(BX-NBXP1)**2+(BY-NBYM1)**2
C
               IF(D.LT.DISTSQ)THEN
                  DISTSQ=D
                  BB=P(NBXP1,NBYM1)
               ENDIF
C
            ENDIF  
C
C              THESE ARE THE TWO ABOVE.
C
            IF(P(NBX,NBYP2).NE.9999..AND.
     1                          (SEALND(JBX,JBYP2).EQ.TEST1.OR.
     2                           SEALND(JBX,JBYP2).EQ.TEST2))THEN
               D=(BX-NBX)**2+(BY-NBYP2)**2
C
               IF(D.LT.DISTSQ)THEN
                  DISTSQ=D
                  BB=P(NBX,NBYP2)
               ENDIF
C
            ENDIF  
C
            IF(P(NBXP1,NBYP2).NE.9999..AND.
     1                          (SEALND(JBXP1,JBYP2).EQ.TEST1.OR.
     2                           SEALND(JBXP1,JBYP2).EQ.TEST2))THEN
               D=(BX-NBXP1)**2+(BY-NBYP2)**2
C
               IF(D.LT.DISTSQ)THEN
                  DISTSQ=D
                  BB=P(NBXP1,NBYP2)
               ENDIF
C
            ENDIF  
C
C
C              THIS IS LL CORNER.
C
            IF(P(NBXM1,NBYM1).NE.9999..AND.
     1                          (SEALND(JBXM1,JBYM1).EQ.TEST1.OR.
     2                           SEALND(JBXM1,JBYM1).EQ.TEST2))THEN
               D=(BX-NBXM1)**2+(BY-NBYM1)**2
C
               IF(D.LT.DISTSQ)THEN
                  DISTSQ=D
                  BB=P(NBXM1,NBYM1)
               ENDIF
C
            ENDIF  
C
C              THIS IS LR CORNER.
C
            IF(P(NBXP2,NBYM1).NE.9999..AND.
     1                          (SEALND(JBXP2,JBYM1).EQ.TEST1.OR.
     2                           SEALND(JBXP2,JBYM1).EQ.TEST2))THEN
               D=(BX-NBXP2)**2+(BY-NBYM1)**2
C
               IF(D.LT.DISTSQ)THEN
                  DISTSQ=D
                  BB=P(NBXP2,NBYM1)
               ENDIF
C
            ENDIF  
C
C              THIS IS UL CORNER.
C
            IF(P(NBXM1,NBYP2).NE.9999..AND.
     1                          (SEALND(JBXM1,JBYP2).EQ.TEST1.OR.
     2                           SEALND(JBXM1,JBYP2).EQ.TEST2))THEN
               D=(BX-NBXM1)**2+(BY-NBYP2)**2
C
               IF(D.LT.DISTSQ)THEN
                  DISTSQ=D
                  BB=P(NBXM1,NBYP2)
               ENDIF
C
            ENDIF  
C
C              THIS IS UR CORNER.
C
            IF(P(NBXP2,NBYP2).NE.9999..AND.
     1                          (SEALND(JBXP2,JBYP2).EQ.TEST1.OR.
     2                           SEALND(JBXP2,JBYP2).EQ.TEST2))THEN
               D=(BX-NBXP2)**2+(BY-NBYP2)**2
C
               IF(D.LT.DISTSQ)THEN
                  DISTSQ=D
                  BB=P(NBXP2,NBYP2)
               ENDIF
C
            ENDIF  
C
         ENDIF  
C
      ENDIF            
C  
      IF(NINT(BB).EQ.9999)THEN 
C
C           CONTROL COULD HAVE REACHED HERE EITHER BECAUSE ALL 
C           POINTS WERE MISSING, OR SOME WERE NOT BUT WERE OF THE 
C           WRONG TYPE.  ONLY PRINT DIAGNOSTIC WHEN NOT ALL WERE
C           MISSING.  THIS SHOULD HAPPEN RARELY, AND MIGHT INDICATE
C           AN ERROR.  THIS IS NOT COUNTED AS AN ISTOP ERROR IN THE
C           CALLING PROGRAM
         IER=196
C
C           BELOW REPLACED FOR COMPARISON TESTING WITH V9
C
C        IF(P(NBX,NBY)    .NE.9999..OR.
C    1      P(NBXP1,NBY)  .NE.9999..OR.
C    2      P(NBXP1,NBYP1).NE.9999..OR.
C    3      P(NBX,NBYP1)  .NE.9999.)THEN
         MBX=BX
         MBY=BY
C
         IF(MBX.GE.1.AND.MBX.LT.NX.AND.
     1      MBY.GE.1.AND.MBY.LT.NY)THEN
C
            ISTOP(5)=ISTOP(5)+1
            IF(IP14.NE.0)WRITE(IP14,495)CCALL,N4PPR,LNDSEA
 495        FORMAT(/' #### VALUE FOR STATION ',A8,' NOT USED.  NONE OF',
     1              ' THE',I4,' SURROUNDING NON-MISSING POINTS IS OF',
     2              ' THE STATION LAND/WATER TYPE =',I3)
            WRITE(KFILDO,495)CCALL,N4PPR,LNDSEA
         ENDIF
C
      ENDIF
C
 500  CONTINUE
C
D     IF(IER.EQ.196)THEN
D        WRITE(KFILDO,505)CCALL,LNDSEA,
D    1                    SEALND(JBX,JBY),  
D    2                    SEALND(JBX,JBYP1),
D    3                    SEALND(JBXP1,JBY),
D    4                    SEALND(JBXP1,JBYP1),
D    5                    P(NBX,NBY),
D    6                    P(NBXP1,NBY),
D    7                    P(NBXP1,NBYP1),
D    8                    P(NBX,NBYP1)
D505     FORMAT(' IN ITRPSL AT 505--CCALL,LNDSEA,SEALND(JBX,JBY),',
D    1      'SEALND(JBXP1,JBY),SEALND(JBXP1,JBYP1),SEALND(JBX,JBYP1),',
D    2      'P( , )',/,
D    3       A8,I4,8E15.5)
D     ENDIF
D
D     WRITE(KFILDO,510)NBX,NBX+1,NBY,NBY+1,JBX,JBXP1,JBY,JBYP1,BB
D510  FORMAT(' AT 510 IN ITRPL--NBX,NBXP1,NBY,NBYP1,',
D    1                         'JBX,JBXP1,JBY,JBYP1,BB',8I7,E15.5)
D     WRITE(KFILDO,511)P(NBX,NBY),P(NBXP1,NBY),
D    1                 P(NBXP1,NBYP1),P(NBX,NBYP1)
D511  FORMAT(' AT 511 IN ITRPL--P(NBX,NBY),P(NBXP1,NBY),',
D    1       'P(NBXP1,NBYP1),P(NBX,NBYP1)',4F15.5)
C
      RETURN
      END
