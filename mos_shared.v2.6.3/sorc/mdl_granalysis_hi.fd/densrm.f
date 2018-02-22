      SUBROUTINE DENSRM(KFILDO,P,NX,NY,ND2X3)
C
C        JUNE      2006   GLAHN   TDL   MOS-2000 
C                                 ADAPTED FROM DENSER FOR LAMP
C        SEPTEMBER 2007   GLAHN   CORRECTED FOR MISSING VALUES
C
C        PURPOSE
C            TO LINEARLY INTERPOLATE INTO A GRID P( , ) SO THAT
C            THE GRIDPOINTS ARE TWICE AS DENSE.  THE BOUNDARIES OF 
C            THE GRID ARE NOT CHANGED.  THE OUTPUT GRID, THEREFORE,
C            HAS HALF THE GRIDLENGTH AS THE INPUT GRID.  NOTE THAT
C            MISSING VALUES ARE ACCOMMODATED.
C
C            THE DIFFERENCE BETWEEN THIS DENSRM AND DENSER IS THAT
C            DENSRM ACCOUNTS FOR MISSING VALUES AND DENSER DOES NOT.
C            IN THE INTERPOLATION, IF ONE OF THE POINTS IN THE FOUR
C            CORNERS AROUND THE POINT IS NON MISSING, THE INTERPOLATED
C            POINT WILL HAVE A NON MISSING VALUE.
C
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
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
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES USED 
C            TRNSFR
C            
      DIMENSION P(NX,NY)
      DIMENSION WORK((NX-1)*2+1,(NY-1)*2+1)
C        WORK( , ) IS AN AUTOMATIC ARRAY.
C
      NXOUT=(NX-1)*2+1
      NYOUT=(NY-1)*2+1
C
CD     WRITE(KFILDO,100)NX,NY,NXOUT,NYOUT
CD100  FORMAT(/' IN DENSRM AT 100--NX,NY,NXOUT,NYOUT',4I10)
C
C        DO THE THICKENING.
C
      DO 231 JY=1,NYOUT,2
      DO 230 IX=1,NXOUT
C
         IF(MOD(IX,2).NE.0)THEN
            WORK(IX,JY)=P(IX/2+1,JY/2+1)
         ELSE
            P1=P(IX/2+1,JY/2+1)
            P2=P(IX/2,  JY/2+1)
C
            IF(NINT(P1).EQ.9999)THEN
C
               IF(NINT(P2).EQ.9999)THEN
                  WORK(IX,JY)=9999.
               ELSE
                  WORK(IX,JY)=P2
               ENDIF
C
            ELSE
C
               IF(NINT(P2).EQ.9999)THEN
                  WORK(IX,JY)=P1
               ELSE
                  WORK(IX,JY)=(P1+P2)*.5
               ENDIF
C
            ENDIF
C
         ENDIF
C
 230  CONTINUE
 231  CONTINUE
C
      DO 241 JY=2,NYOUT,2
      DO 240 IX=1,NXOUT
         P1=WORK(IX,JY-1)
         P2=WORK(IX,JY+1)
C
         IF(NINT(P1).EQ.9999)THEN
C
            IF(NINT(P2).EQ.9999)THEN
               WORK(IX,JY)=9999.
            ELSE
               WORK(IX,JY)=P2
            ENDIF
C
         ELSE
C
            IF(NINT(P2).EQ.9999)THEN
               WORK(IX,JY)=P1
            ELSE
               WORK(IX,JY)=(P1+P2)*.5
            ENDIF
C
         ENDIF
C
 240  CONTINUE
 241  CONTINUE
C
C        THICKENED GRID IS NOW IN WORK( , ).  TRANSFER IT TO P( , )
C        PROVIDED THERE IS ROOM.  ND2X3 IS THE REAL SIZE OF P( )
C        IN THE CALLING PROGRAM.
C
      IF(NXOUT*NYOUT.GT.ND2X3)THEN
         WRITE(KFILDO,250)NXOUT,NYOUT,ND2X3
 250     FORMAT(/' ****P( , ) IN DENSRM IS NOT LARGE ENOUGH.'/
     1           '     NXOUT =',I6,'   NYOUT =',I6,'   ND2X3 =',I8/
     2           '     STOP IN DENSRM AT 250.')
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
