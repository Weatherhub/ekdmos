      SUBROUTINE INTRPL(KFILDO,P,NX,NY,XPL,YPL,NSTA,DATA) 
C 
C        JUNE  1994   GLAHN   TDL   MOS-2000 
C        MARCH 2001   GLAHN   MODIFIED NAME FROM INTRPB AND SEPARATED
C                             THE STATION X AND Y POSITION INTO TWO
C                             VARIABLES, THE WAY THEY ARE IN LAMP.
C        APRIL 2002   GLAHN   SET VALUE TO MISSING WHEN POINT IS
C                             NOT WITHIN THE GRID.
C 
C        PURPOSE 
C            TO INTERPOLATE INTO FIELD P(NX,NY) FOR ALL STATION
C            LOCATIONS.  BILINEAR INTERPOLATION IS USED.  THE POINT 
C            MUST BE INSIDE THE GRID, OR A MISSING VALUE WILL RESULT.
C 
C        DATA SET USE
C            KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C            P(IX,JY) = FIELD TO INTERPOLATE INTO (IX=1,NX) (JY=1,NY).
C                       (INPUT)
C                  NX = THE IX (WEST-EAST) EXTENT OF P( , ).  (INPUT)
C                  NY = THE JY (SOUTH-NORTH) EXTENT OF P( , ).  (INPUT)
C              XPL(K) = THE IX POSITION ON THE GRID FOR STATION K 
C                       (K=1,NSTA).  (INPUT)
C              YPL(K) = THE JY POSITION ON THE GRID FOR STATION K 
C                       (K=1,NSTA).  (INPUT)
C                NSTA = THE NUMBER OF STATIONS BEING DEALT WITH.  (INPUT)
C             DATA(K) = THE INTERPOLATED VALUES FOR ALL STATIONS (K=1,NSTA).
C                       (OUTPUT)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED
C            NONE.
C
      DIMENSION XPL(NSTA),YPL(NSTA),DATA(NSTA)
      DIMENSION P(NX,NY)
C
      DO 160 K=1,NSTA
      BX=XPL(K)
      BY=YPL(K) 
      NBX=BX 
      NBY=BY 
C
C        START BI-LINEAR INTERPOLATION. 
C 
 130  NBXP1=NBX+1 
      NBYP1=NBY+1
C
      IF(NBX.GE.1.AND.NBX.LE.NX.AND.
     1   NBY.GE.1.AND.NBY.LE.NY)THEN 
         DX=BX-FLOAT(NBX) 
         DY=BY-FLOAT(NBY) 
         DATA(K)=P(NBX,NBY)
     1      +(P(NBXP1,NBY)-P(NBX,NBY))*DX
     2      +(P(NBX,NBYP1)-P(NBX,NBY))*DY
     3      +(P(NBX,NBY)+P(NBXP1,NBYP1)-P(NBX,NBYP1)-P(NBXP1,NBY))*DX*DY 
      ELSE
         DATA(K)=9999.
      ENDIF
C
 160  CONTINUE
C
      RETURN 
      END 
