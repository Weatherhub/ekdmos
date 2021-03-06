      SUBROUTINE GVWIND(KFILDO,IDPARS,XMESHL,VWIND,FDSIN,FDM,FDH,UWIND,
     1                  NX,NY,XNP,YNP,MAPP)
C
C        FEBRUARY 1995   GLAHN   TDL   MOS-2000
C        APRIL    2003   GLAHN   SPELL CHECK 
C        AUGUST   2003   GLAHN   ADDED /D FORMAT 100;
C                                ADDED NOTE TO PURPOSE 
C        AUGUST   2003   GLAHN   ADDED MAPP TO CALL AND TO CALL TO
C                                EOVWND
C
C        PURPOSE 
C            TO COMPUTE THE GEOSTROPHIC V-WIND IN M/SEC FROM HEIGHTS
C            FOR EACH GRIDPOINT.  THIS CAN BE EITHER GRID-ORIENTED
C            OR EARTH-ORIENTED.
C
C            NOTE:  IF HEIGHTS ARE SCALED TO WHOLE METERS, THE
C                   RESULT WILL NOT BE VERY ACCURATE.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               004 102 - GRID-ORIENTED GEOSTROPHIC U-WINDS FROM HEIGHTS
C               004 112 - EARTH-ORIENTED GEOSTROPHIC U-WINDS FROM HEIGHTS
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
c                     (OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT) 
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID( ) (J=1,15).  (INPUT)
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C              XMESHL = GRID LENGTH IN M AT LATITUDE XLAT.  (INPUT)
C        VWIND(IX,JY) = COMPUTED V-WIND IN M/SEC (IX=1,NX) (JY=1,NY).
C                       (OUTPUT)
C        FDSIN(IX,JY) = SIN OF THE LATITUDE (IX=1,NX) (JY=1,NY).
C                       (INPUT)
C          FDM(IX,JY) = MAP FACTOR (IX=1,NX) (JY=1,NY).  (INPUT)
C          FDH(IX,JY) = GEOPOTENTIAL HEIGHTS IN M (IX=1,NX) (JY=1,NY).
C                       (INPUT)
C        UWIND(IX,JY) = GRID-ORIENTED U-WIND IN M/SEC (IX=1,NX) (JY=1,NY).
C                       (INTERNAL)
C                  NX = THE DIMENSION OF THE GRID IN THE IX DIRECTION.
C                       (INPUT).
C                  NY = THE DIMENSION OF THE GRID IN THE JY DIRECTION.
C                       (INPUT).
C                 XNP = NORTH POLE POSITION IN IX DIRECTION WITH RESPECT
C                       TO THE LL CORNER AS (1,1).
C                 YNP = NORTH POLE POSITION IN JY DIRECTION WITH RESPECT
C                       TO THE LL CORNER AS (1,1)
C                MAPP = MAP PROJECTION NUMBER (3=LAMBERT, 5=POLAR
C                       STEREOGRAPHIC, 7= MERCATOR).  (INPUT)
C              OMEGA2 = EARTH'S ROTATION PERIOD IN RADIANS/SEC *2.
C                       SET BY PARAMETER.
C               AGRAG = ACCELERATION OF GRAVITY IN M/SEC/SEC.
C                       SET BY PARAMETER.
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            EXTRAP, EOVWND
C
      PARAMETER (OMEGA2=14.584E-5)
      PARAMETER (AGRAV=9.81)
C
      DIMENSION IDPARS(15)
      DIMENSION VWIND(NX,NY),FDSIN(NX,NY),FDM(NX,NY),FDH(NX,NY),
     1          UWIND(NX,NY)
C
D     WRITE(KFILDO,100)NX,NY,FDH(1,1),FDH(NX,NY)
D100  FORMAT(/' IN GVWIND--NX,NY,FDH(1,1),FDH(NX,NY)',2I6,2F10.4)
C
      F=AGRAV/(OMEGA2*XMESHL*2.)
C
C        CALCULATE GRID-ORIENTED V-WIND.
C 
      DO 270 JY=2,NY-1
      DO 269 IX=2,NX-1
      H=FDH(IX+1,JY)-FDH(IX-1,JY)
      VWIND(IX,JY)=H*F*FDM(IX,JY)/FDSIN(IX,JY)
 269  CONTINUE
 270  CONTINUE
C
C        EXTRAPOLATE LINEARLY TO BOUNDARY POINTS.
C
      CALL EXTRAP(KFILDO,VWIND,NX,NY)
C
C        WHEN THE WIND IS TO BE EARTH-ORIENTED, THE U-WIND MUST
C        BE CALCULATED, AND THEN THE WIND TURNED.
C
      IF(IDPARS(2).NE.112)GO TO 300
C
      DO 280 JY=2,NY-1
      DO 279 IX=2,NX-1
      H=FDH(IX,JY+1)-FDH(IX,JY-1)
      UWIND(IX,JY)=-H*F*FDM(IX,JY)/FDSIN(IX,JY)
 279  CONTINUE
 280  CONTINUE
C
C        EXTRAPOLATE LINEARLY TO BOUNDARY POINTS.
C
      CALL EXTRAP(KFILDO,UWIND,NX,NY)
C
      CALL EOVWND(KFILDO,VWIND,UWIND,VWIND,NX,NY,XNP,YNP,MAPP)
C        NOTE THAT GRID-ORIENTED V-WIND IS REPLACED BY EARTH-ORIENTED
C        V-WIND BY EOVWND.
 300  RETURN
      END
