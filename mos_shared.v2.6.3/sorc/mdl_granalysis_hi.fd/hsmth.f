      SUBROUTINE HSMTH(KFILDO,P,NX,NY,
     1                 IOCEXT,IOCINC,ISTOP,IER)
C
C        MAY       2008   GLAHN   MDL   MOS-2000
C
C        PURPOSE
C            TO SMOOTH THE ALL POINTS IN A GRID OVER A SQUARE 
C            2*IOCEXT ON A SIDE.  A MISSING POINT IS KEPT AS MISSING.
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
C              IOCEXT = THE NUMBER OF GRIDPOINTS PLUS AND MINUS TO 
C                       SMOOTH.  SMOOTHING WILL BE OVER A SQUARE
C                       2*IOCEXT+1 GRIDPOINTS ON A SIDE.  (INPUT)
C              IOCINC = THE INCREMENT TO USE IN SMOOTHING.  IT MAY BE
C                       NOT ALL POINTS IN THE SQUARE NEED BE USED TO
C                       GET A GOOD RESULT.  IF SO, THE COMPUTATON 
C                       SHOULD BE CONSIDEABLY FASTER.  NOTE THAT
C                       IOCEXT SHOLD BE EVENLY DIVISIBLE BY IOCINC.
C                       (INPUT)
C               ISTOP = INCREMENTED BY 1 IF THERE IS AN ERROR.
C                       (INPUT/OUTPUT)
C                 IER = RETURN CODE.
C                         0 = GOOD RETURN.
C                       777 = IOCEXT AND IOCINC ARE NOT COMPATIBLE.
C                       (OUTPUT)
C        1         2         3         4         5         6         7 X
C
      DIMENSION P(NX,NY)
      DIMENSION HOLD(NX,NY)
C        HOLD( , ) IS AN AUTOMTIC ARRAY.
C
CD        WRITE(KFILDO,103)NX,NY,IOCEXT,IOCINC
CD103     FORMAT(/' IN HSMTH--NX,NY,IOCEXT,IOCINC',
CD    1           8I8)
C
C        CHECK WHETHER IOCEXT IS EVENLY DIVISIBLE BY IOCINC.
C
      IF(MOD(IOCEXT,IOCINC).NE.0.OR.IOCINC.GT.IOCEXT)THEN
         WRITE(KFILDO,109)IOCEXT,IOCINC
 109     FORMAT(/' ****IOCEXT =',I4,' AND IOCINC =',I4,
     1           ' ARE NOT COMPATIBLE',/,
     2           '.  GRID IS NOT SMOOTHED.',/,
     3           '    NOT COUNTED AS FATAL.  IN HSMTH.  CONTINUING.')
         ISTOP=ISTOP+1
         IER=777
         GO TO 300
      ENDIF
      
C        SMOTH THE POINTS.
C
      DO 200 JY=1,NY
      DO 199 IX=1,NX
C
      IF(P(IX,JY).LT.9998.9)THEN
C           THIS IS A NON MISSING POINT.
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
            IF(P(IX1,JY1).LT.9998.9)THEN
C                 THE POINT NOT MISSING.
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
