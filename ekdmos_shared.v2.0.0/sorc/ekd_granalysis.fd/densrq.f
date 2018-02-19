      SUBROUTINE DENSRQ(KFILDO,P,NX,NY,ND2X3)
C
C        AUGUST   2000   GLAHN   TDL   LAMP-2000
C        NOVEMBER 2000   GLAHN   ELIMINATED NXOUT, NYOUT IN CALL
C        MAY      2002   GLAHN   CHANGED PURPOSE TO INDICATE
C                                MISSING VALUES NOT ACCOMMODATED.
C        DECEMBER 2002   RUDACK  MODIFIED FORMAT STATEMENTS TO ADHERE
C                                TO THE F90 COMPILER STANDARDS FOUND ON 
C                                THE IBM SYSTEM
C
C        PURPOSE
C            TO BIBIQUADRATICALLY INTERPOLATE INTO A GRID P( , ) SO
C            THAT THE GRIDPOINTS ARE TWICE AS DENSE.  THE BOUNDARIES OF 
C            THE GRID ARE NOT CHANGED.  THE OUTPUT GRID, THEREFORE,
C            HAS HALF THE GRIDLENGTH AS THE INPUT GRID.  NOTE THAT
C            MISSING VALUES ARE NOT ACCOMMODATED.
C            (ADAPTED FROM MAY 1997 VERSION OF EXTNDQ.)
C
C        DATA SET USE
C           NONE.
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C            P(IX,JY) = THE INPUT AND OUTPUT GRIDS (IX=1,NX)  (JY=1,NY).
C                       (INPUT/OUTPUT)
C                  NX = ON INPUT, SIZE OF THE INPUT GRID IN NX
C                       DIRECTION.  ON OUTPUT, SIZE OF THE OUTPUT GRID.
C                       (INPUT/OUTPUT)
C                  NY = ON INPUT, SIZE OF THE INPUT GRID IN NY
C                       DIRECTION.  ON OUTPUT, SIZE OF THE OUTPUT GRID.
C                       (INPUT/OUTPUT)
C               ND2X3 = SIZE OF COMBINED DIMENSIONS OF P( ).  (INPUT)
C               NXOUT = SET (NX-1)*2+1.  (INTERNAL)
C               NYOUT = SET (NY-1)*2+1.  (INTERNAL)
C         WORK(IX,JY) = WORK ARRAY (IX=1,NXOUT) (JY=1,NYOUT).
C                       (AUTOMATIC)
C
C        NONSYSTEM SUBROUTINES USED 
C            TRNSFR
C
      DIMENSION P(NX,NY)
      DIMENSION WORK((NX-1)*2+1,(NY-1)*2+1)
C        WORK( , ) IS AN AUTOMATIC ARRAY.
      DIMENSION B(4)
C
      NXOUT=(NX-1)*2+1
      NYOUT=(NY-1)*2+1
C
CD     WRITE(KFILDO,100)NX,NY,NXOUT,NYOUT
CD100  FORMAT(/' IN DENSRQ AT 100--NX,NY,NXOUT,NYOUT',4I10)
C
C        DO THE THICKENING.
C
      DO 200 JY=1,NYOUT
      BY=(JY+1)/2.
      DO 199 IX=1,NXOUT
      BX=(IX+1)/2.
C
      IF(MOD(IX,2).GT.0.AND.MOD(JY,2).GT.0)THEN
C        POINT MATCHES IN INPUT AND OUTPUT GRID
         WORK(IX,JY)=P((IX+1)/2,(JY+1)/2)
         GO TO 199
      END IF
C
      NBX=BX
      NBY=BY
      NXM1=NX-1
      NYM1=NY-1
      IF(NBX-1)114,120,111
 111  IF(NBX-NXM1)112,120,115
 112  IF(NBY-1)121,130,113
 113  IF(NBY-NYM1)140,130,123
 114  NBX=1
      GO TO 120
 115  NBX=NXM1
 120  IF(NBY-1)121,130,122
 121  NBY=1
      GO TO 130
C
 122  IF(NBY-NY)130,123,123
 123  NBY=NYM1
C
C        START BI-LINEAR INTERPOLATION-EXTRAPOLATION.
C
 130  NBXP1=NBX+1
      NBYP1=NBY+1
      DX=BX-FLOAT(NBX)
      DY=BY-FLOAT(NBY)
      WORK(IX,JY)=P(NBX,NBY)+(P(NBXP1,NBY)-P(NBX,NBY))*DX+(P(NBX,NBYP1)-
     1P(NBX,NBY))*DY+(P(NBX,NBY)+P(NBXP1,NBYP1)-P(NBX,NBYP1)-P(NBXP1,
     2NBY))*DX*DY
      GO TO 199
C
C        START BI-QUADRATIC INTERPOLATION.
C
 140  DX=BX-FLOAT(NBX)
      DY=BY-FLOAT(NBY)
      NBYP2=NBY+2
      NBYP1=NBY+1
      NBYM1=NBY-1
      FCT=(DY**2-DY)/4.
      FET=(DX**2-DX)/4.
C
      DO 145 J=1,4
      N=NBX-2+J
      B(J)=P(N,NBY)+(P(N,NBYP1)-P(N,NBY))*DY+(P(N,NBYM1)+P(N,NBYP2)-
     1P(N,NBY)-P(N,NBYP1))*FCT
 145  CONTINUE
C
      WORK(IX,JY)=B(2)+(B(3)-B(2))*DX+(B(1)+B(4)-B(2)-B(3))*FET
C
 199  CONTINUE
 200  CONTINUE
C
C        THICKENED GRID IS NOW IN WORK( , ).  TRANSFER IT TO P( , )
C        PROVIDED THERE IS ROOM.  ND2X3 IS THE REAL SIZE OF P( )
C        IN THE CALLING PROGRAM.
C
      IF(NXOUT*NYOUT.GT.ND2X3)THEN
         WRITE(KFILDO,250)NXOUT,NYOUT,ND2X3
 250     FORMAT(/' ****P( , ) IN DENSRQ IS NOT LARGE ENOUGH.'/
     1           '     NXOUT =',I6,'   NYOUT =',I6,'   ND2X3 =',I8/
     2           '     STOP IN DENSRQ AT 250.')
         STOP 250
C
      ELSE
         CALL TRNSFR(WORK,P,NXOUT*NYOUT)
      ENDIF
C
      NX=NXOUT
      NY=NYOUT
      RETURN
      END
