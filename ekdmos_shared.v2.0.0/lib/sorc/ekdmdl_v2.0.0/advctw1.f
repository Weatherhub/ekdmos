      SUBROUTINE ADVCTW1(KFILDO,XMESHL,FDAV,FD1,FD2,FD3,FD4,NX,NY)
C
C     AUGUST   1997   FIEBRICH  TDL   MOS-2000             
C     OCTOBER  1998   YAN       FIXED THE SIGN OF ADVECTION.
C     DECEMBER 2000   RUDACK    MODIFIED CODE TO COMPLY WITH MDL
C                               FORMAT SPECIFICATIONS
C     MAY      2003   GLAHN     REVERSED INDICES ON INITIAL LOOP;
C                               ADDED TO COMMENT FOR FDAV( , )
C
C        PURPOSE 
C            TO COMPUTE THE ADVECTION OF A GRIDDED QUANTITY, Q, FROM THE
C            U- AND V- WIND COMPONENTS.
C            UNITS ARE Q/SEC. 
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                     (OUTPUT)
C 
C        VARIABLES 
C            FD1(I,J) = MAP FACTOR (I=1,NX) (J=1,NY)  (INPUT)
C            FD2(I,J) = U WINDS IN M/SEC (I=1,NX) (J=1,NY)  (INPUT)
C            FD3(I,J) = V WINDS IN M/SEC (I=1,NX) (J=1,NY)  (INPUT)
C            FD4(I,J) = THE QUANTITY TO BE ADVECTED  (INPUT)   
C           FDAV(I,J) = ADVECTION OF QUANTITY Q IN UNITS OF Q/SEC
C                       (I=1,NX) (J=1,NY)  ON INPUT, IT HOLDS THE
C                       VARIABLE TO BE ADVECTED; ON OUTPUT, IT HOLDS
C                       THE ADVECTED VARIABLE.  (INPUT/OUTPUT)
C                 I,J = LOOP COUNT (INTERNAL)
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                       (INPUT)
C                  NX = THE DIMENSION OF THE GRID IN THE X DIRECTION
C                       (INPUT)
C                  NY = THE DIMENSION OF THE GRID IN THE Y DIRECTION
C                       (INPUT)
C              XMESHL = GRID LENGTH IN METER AT LATITUDE XLAT  (INPUT)
C 
C        NONSYSTEM SUBROUTINES USED
C            EXTRAP
C
      IMPLICIT NONE
C
      INTEGER I,J,KFILDO,NX,NY
C
      REAL FDAV(NX,NY),FD1(NX,NY),FD2(NX,NY),FD3(NX,NY),FD4(NX,NY)
      REAL XMESHL
C
C        DOUBLE THE GRID LENGTH FOR COMPUTATIONAL CONVENIENCE
C
      XMESHL=2.*XMESHL
C
C        TO PASS FDAV (THE QUANTITY TO BE ADVECTED) TO FD4
C
      DO J=1,NY
         DO I=1,NX
	    FD4(I,J)=FDAV(I,J)
         ENDDO
      ENDDO
C
C        TO COMPUTE THE ADVECTION OF FD4
C
      DO J=2,NY-1
         DO I=2,NX-1
            IF(FD4(I+1,J).NE.9999..AND.FD4(I-1,J).NE.9999..AND.
     1         FD4(I,J+1).NE.9999..AND.FD4(I,J-1).NE.9999..AND.
     2         FD2(I,J).NE.9999..AND.FD3(I,J).NE.9999..AND.
     3         FD1(I,J).NE.9999.)THEN
               FDAV(I,J)=-FD1(I,J)*((FD4(I+1,J)-FD4(I-1,J))*FD2(I,J)
     1                             +(FD4(I,J+1)-FD4(I,J-1))*FD3(I,J))
     2                             /XMESHL
            ELSE
               FDAV(I,J)=9999.
            END IF
         ENDDO
      ENDDO
C
C        CALL EXTRAP TO EXTRAPOLATE FDAV LINEARLY TO BOUNDARY POINTS
C
      CALL EXTRAP(KFILDO,FDAV,NX,NY)
C
      RETURN
      END
