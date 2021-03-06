      SUBROUTINE VORTW1(KFILDO,XMESHL,VORT,FDM,FDU,FDV,NX,NY)
C
C        NOVEMBER 1994   GLAHN   TDL   MOS-2000 
C
C        PURPOSE 
C            TO COMPUTE THE RELATIVE VORTICITY FROM WINDS FOR EACH GRIDPOINT.
C            UNITS ARE /SEC * 100000.
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT) 
C              XMESHL = GRID LENGTH IN M AT LATITUDE XLAT.  (INPUT)
C         VORT(IX,JY) = RELATIVE VORTICITY IN UNITS /SEC * 100000 (IX=1,NX)
C                       (JY=1,NY).  (OUTPUT)
C          FDM(IX,JY) = MAP FACTOR (IX=1,NX) (JY=1,NY).  (INPUT)
C          FDU(IX,JY) = U WINDS IN M/SEC (IX=1,NX) (JY=1,NY).  (INPUT)
C          FDV(IX,JY) = V WINDS IN M/SEC (IX=1,NX) (JY=1,NY).  (INPUT)
C                  NX = THE DIMENSION OF THE GRID IN THE IX DIRECTION.
C                       (INPUT).
C                  NY = THE DIMENSION OF THE GRID IN THE JY DIRECTION.
C                       (INPUT).
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            EXTRAP
C
      DIMENSION VORT(NX,NY),FDM(NX,NY),FDU(NX,NY),FDV(NX,NY)
C
      XMESH2=XMESHL*2.
C
      DO 270 JY=2,NY-1
      DO 269 IX=2,NX-1
      VORT(IX,JY)=(FDV(IX+1,JY)-FDV(IX-1,JY)
     1            -FDU(IX,JY+1)+FDU(IX,JY-1))*FDM(IX,JY)/XMESH2
 269  CONTINUE
 270  CONTINUE
C
C        EXTRAPOLATE LINEARLY TO BOUNDARY POINTS.
C
      CALL EXTRAP(KFILDO,VORT,NX,NY)
C
C        MAKE UNITS OF RETURNED FIELD PER SECOND X 10*5.
C
      DO 290 JY=1,NY
      DO 289 IX=1,NX
      VORT(IX,JY)=VORT(IX,JY)*100000.
 289  CONTINUE
 290  CONTINUE
C
      RETURN
      END
