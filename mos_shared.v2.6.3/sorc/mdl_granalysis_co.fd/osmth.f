      SUBROUTINE OSMTH(KFILDO,P,NX,NY,MESH,
     1                 SEALND,NXE,NYE,MESHE,
     2                 IOCEXT,IOCINC,ISTOP,IER)
C
C        NOVEMBER  2007   GLAHN   MDL   MOS-2000
C        JUNE      2008   GLAHN   (MOD(2*IOCEXT,IOCINC) INSERTED VICE
C                                 (MOD(IOCEXT,IOCINC) AT 109
C        PURPOSE
C            TO SMOOTH THE OCEAN POINTS IN A GRID OVER A SQUARE
C            2*IOCEXT ON A SIDE.  IT IS ASSUMED P(NX,JY) AND 
C            SEALND(NXE,NYE) COVER THE SAME AREA.  THE GRID SIZES
C            AND MESH LENGTHS ARE CHECKED.  A MISSING POINT IS KEPT
C            AS MISSING.
C
C        DATA SET USE
C            KFILDO   - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C            P(IX,JY) = HOLDS A FIELD TO BE SMOOTHED
C                       (IX=1,NX) (JY=1,NY), WHERE NX AND NY ARE THE
C                       SIZE OF THE GRID.  (INPUT/OUTPUT)
C                  NX = SIZE OF P( , ) IN X DIRECTION.  (INPUT)
C                  NY = SIZE OF P( , ) IN Y DIRECTION.  (INPUT)
C                MESH = NOMINAL MESH LENGTH OF GRID IN P( , ) .
C                       (INPUT)
C       SEALND(IX,JY) = THE LAND/SEA MASK (IX=1,NX) (JY=1,NY). 
C                       0 = OCEAN WATER GRIDPOINTS;
C                       3 = INLAND WATER GRIDPOINTS.
C                       9 = LAND GRIDPOINTS.
C                       (INPUT)
C                 NXE = SIZE OF SEALND( , ) IN X DIRECTION.  (INPUT)
C                 NYE = SIZE OF SEALND( , ) IN Y DIRECTION.  (INPUT)
C               MESHE = NOMINAL MESH LENGTH OF GRID IN SEALND( , ).
C                       (INPUT)
C              IOCEXT = THE NUMBER OF GRIDPOINTS PLUS AND MINUS TO 
C                       SMOOTH.  SMOOTHING WILL BE OVER A SQUARE
C                       2*IOCEXT+1 GRIDPOINTS ON A SIDE.  (INPUT)
C              IOCINC = THE INCREMENT TO USE IN SMOOTHING.  IT MAY BE
C                       NOT ALL POINTS IN THE SQUARE NEED BE USED TO
C                       GET A GOOD RESULT.  IF SO, THE COMPUTATION 
C                       SHOULD BE CONSIDERABLY FASTER.  NOTE THAT
C                       IOCEXT SHOULD BE EVENLY DIVISIBLE BY IOCINC.
C                       (INPUT)
C               ISTOP = INCREMENTED BY 1 IF THERE IS AN ERROR.
C                       (INPUT/OUTPUT)
C                 IER = RETURN CODE.
C                         0 = GOOD RETURN.
C                       777 = MESHE NE MESH OR ARRAY SIZES NOT THE SAME.
C                       (OUTPUT)
C        1         2         3         4         5         6         7 X
C
      DIMENSION P(NX,NY)
      DIMENSION HOLD(NX,NY)
C        HOLD( , ) IS AN AUTOMATIC ARRAY.
      DIMENSION SEALND(NXE,NYE)
C
CD        WRITE(KFILDO,103)NX,NY,NXE,NYE,MESH,MESHE,IOCEXT,IOCINC
CD103     FORMAT(/' IN OSMTH--NX,NY,NXE,NYE,MESH,MESHE,IOCEXT,IOCINC',
CD    1           8I8)
C
C        CHECK THE SIZE OF P( , ) AND SEALND( , ).
C
      IF(NX.NE.NXE.OR.NY.NE.NYE)THEN
         WRITE(KFILDO,105)NX,NY,NXE,NYE
 105     FORMAT(/' ****THE GRID P( , ) OF SIZE', I6,' X ',I6,
     1           ' IS NOT THE SAME SIZE OF SEALND', I6,' X ',I6,'.',/,
     2           '     OCEAN IS NOT SMOOTHED.  CONTINUING.')
         ISTOP=ISTOP+1
         IER=777
         GO TO 300
      ENDIF
C
C        CHECK THE MESH LENGTHS OF P( , ) AND SEALND( , ).
C
      IF(MESH.NE.MESHE)THEN
         WRITE(KFILDO,107)MESH,MESHE
 107     FORMAT(/' ****THE GRID P( , ) HAS A DIFFERENT NOMINAL',
     1           ' MESH LENGTH =',I4,' THAN THAT OF SEALND( , ) =',
     2           I4,'.',/,
     3           '     OCEAN IS NOT SMOOTHED.  CONTINUING.')
         ISTOP=ISTOP+1
         IER=777
         GO TO 300
      ENDIF
C
C        CHECK WHETHER IOCEXT IS EVENLY DIVISIBLE BY IOCINC.
C
      IF(MOD(2*IOCEXT,IOCINC).NE.0.OR.IOCINC.GT.IOCEXT)THEN
         WRITE(KFILDO,109)IOCEXT,IOCINC
 109     FORMAT(/' ****IOCEXT =',I4,' AND IOCINC =',I4,
     1           ' ARE NOT COMPATIBLE.',/,
     2           '     OCEAN IS NOT SMOOTHED.  CONTINUING.')
         ISTOP=ISTOP+1
         IER=777
         GO TO 300
      ENDIF
C      
C        SMOOTH THE POINTS.
C
      DO 200 JY=1,NY
      DO 199 IX=1,NX
C
      IF(SEALND(IX,JY).EQ.0..AND.P(IX,JY).LT.9998.9)THEN
C           THIS IS AN OCEAN POINT THAT IS NOT MISSING.
         IXS=MAX(1,IX-IOCEXT)
         IXE=MIN(NX,IX+IOCEXT)
         JYS=MAX(1,JY-IOCEXT)
         JYE=MIN(NY,JY+IOCEXT)
         SUM=0.
         KOUNT=0
C
         DO 150 JY1=JYS,JYE,IOCINC
         DO 149 IX1=IXS,IXE,IOCINC
C
            IF(SEALND(IX1,JY1).EQ.0..AND.P(IX1,JY1).LT.9998.9)THEN
C                 THE POINT IS WATER AND NOT MISSING.
               SUM=SUM+P(IX1,JY1)
               KOUNT=KOUNT+1
            ENDIF
C
 149     CONTINUE
 150     CONTINUE
C
         IF(KOUNT.EQ.0)THEN
            HOLD(IX,JY)=P(IX,JY)
         ELSE
            HOLD(IX,JY)=SUM/KOUNT
         ENDIF
C
      ELSE
         HOLD(IX,JY)=P(IX,JY)
      ENDIF    
C
 199  CONTINUE
 200  CONTINUE
C
      DO 250 JY=1,NY
      DO 249 IX=1,NX
      P(IX,JY)=HOLD(IX,JY)
 249  CONTINUE
 250  CONTINUE
C
 300  RETURN
      END
