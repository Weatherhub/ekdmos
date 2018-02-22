      SUBROUTINE INTRPC(KFILDO,P,NX,NY,DIR,ND1,NSTA,SDATA) 
C 
C        JUNE 2002   GLAHN   TDL   MOS-2000 
C 
C        PURPOSE 
C            TO FIND THE GRIDPOINT VALUE IN A GRID CLOSEST TO THE
C            STATION LOCATIONS.  THE STATION MUST BE WITHIN 1/2
C            GRIDLENGTH OF THE POINT IF OUTSIDE THE GRID; OTHERWISE,
C            MISSING = 9999. WILL BE RETURNED.
C 
C        DATA SET USE
C            KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C            P(IX,JY) = FIELD TO GET POINT FROM (IX=1,NX) (JY=1,NY).
C                       (INPUT)
C                  NX = THE IX (WEST-EAST) EXTENT OF P( , ).  (INPUT)
C                  NY = THE JY (SOUTH-NORTH) EXTENT OF P( , ).  (INPUT)
C            DIR(K,J) = THE IX (J=1) AND JY (J=2) POSITIONS ON THE GRID
C                       FOR STATION K (K=1,NSTA).  (INPUT)
C                 ND1 = THE MAXIMUM NUMBER OF STATIONS.  FIRST
C                       DIMENSION OF DIR( , ).  (INPUT)
C                NSTA = THE NUMBER OF STATIONS BEING DEALT WITH.
C                       (INPUT)
C            SDATA(K) = THE CLOSEST GRID VALUES FOR ALL STATIONS
C                       (K=1,NSTA).  (OUTPUT)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED
C            NONE.
C
      DIMENSION DIR(ND1,2)
      DIMENSION P(NX,NY)
      DIMENSION SDATA(NSTA)
C
      DO 160 K=1,NSTA
C
      IX=NINT(DIR(K,1))
      JY=NINT(DIR(K,2))
C
      IF(IX.GE.1.AND.IX.LE.NX.AND.
     1   JY.GE.1.AND.JY.LE.NY)THEN
         SDATA(K)=P(IX,JY)
      ELSE
         SDATA(K)=9999.
      ENDIF
C      
 160  CONTINUE
C
      RETURN 
      END 
