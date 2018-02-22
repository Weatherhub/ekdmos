      SUBROUTINE SETLND(KFILDO,P,NX,NY,MESH,
     1                  SEALND,NXE,NYE,MESHE,CONST,IER)
C
C        MAY       2008   GLAHN   MDL   MOS-2000
C
C        PURPOSE
C            TO REPLACE A ALL VALUES OVER LAND WITH A CONSTANT.  THIS
C            IS USED WHEN A MERGED FIRST GUESS IS USED AND SCALING
C            HAS PRODUCED A NON CONSTANT FIELD.
C
C        DATA SET USE
C            KFILDO   - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C            P(IX,JY) = HOLDS  FIELD (IX=1,NX) (JY=1,NY), 
C                       WHERE NX AND NY ARE THE SIZE OF THE GRID.
C                       (INPUT/OUTPUT)
C                  NX = SIZE OF P( , ) IN X DIRECTION.  (INPUT)
C                  NY = SIZE OF P( , ) IN Y DIRECTION.  (INPUT)
C                MESH = NOMINAL MESH LENGTH OF GRID IN P( , ) .
C                       (INPUT)
C           SEALND(J) = THE LAND/SEA MASK (J=1,NXE*NYE).
C                       0 = OCEAN WATER GRIDPOINTS;
C                       3 = INLAND WATER GRIDPOINTS.
C                       9 = LAND GRIDPOINTS.
C                       (INPUT)
C                 NXE = SIZE OF SEALND( , ) IN X DIRECTION.  (INPUT)
C                 NYE = SIZE OF SEALND( , ) IN Y DIRECTION.  (INPUT)
C               MESHE = NOMINAL MESH LENGTH OF GRID IN SEALND( , ).
C                       (INPUT)
C               CONST = CONSTANT TO USE TO REPLACE P( , ) WHEN SEALND( , )
C                       NE 0.  (INPUT)
C                 IER = RETURN CODE.
C                         0 = GOOD RETURN.
C                       197 = MESHE GT MESH.  THINNING OF SEALND( , )
C                             TO GET P( , ) IS NOT DONE.
C                       (OUTPUT)
C        1         2         3         4         5         6         7 X
C
      DIMENSION P(NX,NY)
      DIMENSION SEALND(NXE,NYE)
C
      IF(MESHE.EQ.MESH)THEN
C
CD        WRITE(KFILDO,103)NX,NY,NXE,NYE,MESH,MESHE,CONST
CD103     FORMAT(/' IN SETLND--NX,NY,NXE,NYE,MESH,MESHE,CONST',7I8)
C
         DO 114 JY=1,NY
         DO 113 IX=1,NX
C
         IF(SEALND(IX,JY).GT.8.5)THEN
C
C              THIS IS A LAND POINT.
C
            IF(P(IX,JY).NE.9999.)THEN
C                 FIELD MAY HAVE MISSING VALUES IN LAND AREAS.
C                 IF SO, LEAVE THEM MISSING.
               P(IX,JY)=CONST
            ENDIF
C
C           NO CHANGE IS MADE TO P( , ) FOR A WATER POINT.
         ENDIF
C
CD        IF(IX.EQ.1200)THEN
CD        WRITE(KFILDO,112)IX,JY,SEALND(IX,JY),CONST,P(IX,JY)
CD112     FORMAT(' IN SETLND--IX,JY,SEALND(IX,JY),CONST,P(IX,JY)',
CD    1          2I6,3F10.2)
CD        ENDIF
C
 113     CONTINUE
 114     CONTINUE
C
      ELSEIF(MESHE.LT.MESH)THEN
C
C**************
C           THIS LOOP HAS NOT BEEN CHECKED OUT.
C**************
C
         IR=MESH/MESHE
         WRITE(KFILDO,115)MESH,MESHE
 115     FORMAT(/' ****THE NOMINAL GRIDLENGTH OF THE GRIDS IN P( , )',
     1           ' =',I5,/,
     2           '     IS GREATER THAN THE NOMINAL GRIDLENGTH OF THE',
     3           ' GRID IN SEALND( , ) =',I5,' IN SETLND',/,
     4           '     THIS IS ACCOMMODATED AND IS NOT COUNTED AS AN',
     5           ' ERROR, BUT IS UNUSUAL.')
C           IT IS ASSUMED THE MESH LENGTHS OF P( , ) AND SEALND( , )
C           DIFFER BY AN INTEGRAL RATIO; THIS IS GUARANTEED IN U155.
C           IT IS LIKELY, IN U155, THE TWO WILL BE THE SAME.
C
         DO 210 JY=1,NX
         DO 209 IX=1,NY
C
         IF(SEALND((IX-1)*IR+1,(JY-1)*IR+1).GT.8.5)THEN
C
C              THIS IS A LAND POINT.
C
            IF(P(IX,JY).NE.9999.)THEN
C                 FIESL MAY HAVE MISSING VALUES IN LAND AREAS.
C                 IF SO, LEAVE THEM MISSING.
               P(IX,JY)=CONST
            ENDIF
C
C           NO CHANGE IS MADE TO P( , ) FOR A WATER POINT.
         ENDIF
C
 209     CONTINUE
 210     CONTINUE
C
      ELSE
         WRITE(KFILDO,220)MESH,MESHE
 220     FORMAT(/' ****THE NOMINAL GRIDLENGTH OF THE GRIDS IN P( , )',
     1           ' =',I5,/,
     2           '     IS LESS THAN THE NOMINAL GRIDLENGTH OF THE GRID',
     3           ' IN SEALND( , ) =',I5,/,
     4           '     THIS IS NOT ACCOMMODATED IN SETLND, AND THE',
     5           ' FIELD WILL NOT BE USED TO IN THE',
     6           ' FIRST GUESS.')
         IER=197
      ENDIF
C
      RETURN
      END
