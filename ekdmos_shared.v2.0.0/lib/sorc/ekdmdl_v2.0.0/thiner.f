      SUBROUTINE THINER(KFILDO,P,NX,NY)
C
C        AUGUST   2000   GLAHN   TDL   LAMP-2000
C        DECEMBER 2000   GLAHN   CHANGED NXOUT,NYOUT DEFINITION TO
C                                DIVIDE BY 2 NOT MULTIPLY BY 2, AND
C                                DEFINITION OF WORK( , ) SIMILARLY
C
C        PURPOSE
C           TO THIN A GRID SO THAT THERE ARE HALF AS MANY GRID
C           SPACES IN THE OUTPUT GRID AS THE INPUT.  THE BOUNDARIES OF 
C           THE GRID ARE NOT CHANGED.  THE OUTPUT GRID, THEREFORE,
C           HAS DOUBLE THE GRIDLENGTH AS THE INPUT GRID.  NOTE THAT
C           THE INPUT DIMENSIONS MUST BE ODD.
C
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES 
C
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
C               NXOUT = SET (NX-1)/2+1.  (INTERNAL)
C               NYOUT = SET (NY-1)/2+1.  (INTERNAL)
C         WORK(IX,JY) = WORK ARRAY (IX=1,NXOUT) (JY=1,NYOUT).
C                       (AUTOMATIC)
C
C        NONSYSTEM SUBROUTINES USED 
C            TRNSFR
C            
      DIMENSION P(NX,NY)
      DIMENSION WORK((NX-1)/2+1,(NY-1)/2+1)
C        WORK( , ) IS AN AUTOMATIC ARRAY.
C
      NXOUT=(NX-1)/2+1
      NYOUT=(NY-1)/2+1
C
D     WRITE(KFILDO,100)NX,NY,NXOUT,NYOUT
D100  FORMAT(/' IN THINER AT 100--NX,NY,NXOUT,NYOUT',13X,4I10)
C
C        CHECK FOR ODD DIMENSIONS OF THE INPUT GRID.
C
      IF(MOD(NX,2).EQ.0.OR.
     1   MOD(NY,2).EQ.0)THEN
         WRITE(KFILDO,110)NX,NY
 110     FORMAT(/' ****NX =',I6,' OR NY =',I6,' IN THINER IS NOT',
     1           ' ODD.  THINNING NOT DONE; THE SAME GRID IS RETURNED.')
      ENDIF
C
C        DO THE THINNING. 
C
      DO 120 JY=1,NYOUT  
      DO 119 IX=1,NXOUT
      WORK(IX,JY)=P((IX-1)*2+1,(JY-1)*2+1)
 119  CONTINUE
 120  CONTINUE
C
      CALL TRNSFR(WORK,P,NXOUT*NYOUT)
      NX=NXOUT
      NY=NYOUT
      RETURN
      END
