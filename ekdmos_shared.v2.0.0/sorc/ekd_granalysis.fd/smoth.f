      SUBROUTINE SMOTH(P,Q,NX,NY,BQ)
C
C        MAY    1993   GLAHN, CHAMBERS   TDL   HP9000
C        AUGUST 2001   GLAHN   MODIFIED BORDERS AND CORNERS CALCULATION
C                              TO MATCH SMOTHM; MODIFIED COMMENTS
C
C        PURPOSE
C            TO SMOOTH FIELD P( , ).
C
C        DATA SET USE
C            NONE.
C
C        VARIABLES
C              P( , ) = FIELD TO SMOOTH.  (INPUT-OUTPUT)
C              Q( , ) = WORK ARRAY.  (INTERNAL)
C              NX, NY = DIMENSIONS OF P( , ) AND Q( , ).  (INPUT)
C                  BQ = SMOOTHING PARAMETER.  SMOOTHED VALUE AT POINT P
C                       = ORIGINAL VALUE PLUS BQ/4 TIMES SUM OF
C                       SURROUNDING 4 POINTS, ALL DIVIDED BY BQ+1.
C                       A MODIFIED PROCEDURE IS USED ON THE BORDERS AND
C                       CORNERS THAT ASSUMES A BQ OF 4 INDICATES A MEAN
C                       OF ALL VALUES INVOLVED.  THESE CALCULATIONS ARE
C                       CONSISTENT WITH SMOTHM WHICH DEALS WITH MISSING
C                       VALUES.  THE EQUATIONS USED ARE THE SAME AS:
C                          Q(IX,JY)=(P(IX,JY)+(SUM/ISUM)*(ISUM/4)*BQ)/
C                             ((ISUM/4)*BQ+1.)
C                       WHERE SUM IS THE SUM OF THE ISUM VALUES 
C                       (2 FOR CORNERS AND 3 FOR BORDERS). 
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED
C            NONE.
C
      DIMENSION P(NX,NY),Q(NX,NY)
C
      IF(BQ.EQ.0.)GO TO 280
      BP1=BQ+1.
      BQ4=BQ/4.
      NXM1=NX-1
      NYM1=NY-1
C
C        SMOOTH ALL EXCEPT OUTER ROWS AND COLUMNS.
C
      DO 255 J=2,NYM1
      DO 254 L=2,NXM1
         Q(L,J)=(P(L,J)+BQ4*(P(L-1,J)+P(L+1,J)+P(L,J-1)+P(L,J+1)))/BP1
 254  CONTINUE
 255  CONTINUE
C
C        SMOOTH BOTTOM AND TOP ROWS.
C
      DO 256 L=2,NXM1
         Q(L,1)=(P(L,1)+BQ4*(P(L-1,1)+P(L+1,1)+P(L,2)))/
     1      (.75*BQ+1.)
C        1         2         3         4         5         6         7 X
         Q(L,NY)=(P(L,NY)+BQ4*(P(L-1,NY)+P(L+1,NY)+P(L,NYM1)))/
     1      (.75*BQ+1.)
 256  CONTINUE
C
C        SMOOTH 1ST AND LAST COLUMNS.
C
      DO 257 J=2,NYM1
         Q(1,J)=(P(1,J)+BQ4*(P(1,J-1)+P(1,J+1)+P(2,J)))/
     1      (.75*BQ+1.)
         Q(NX,J)=(P(NX,J)+BQ4*(P(NX,J-1)+P(NX,J+1)+P(NXM1,J)))/
     1      (.75*BQ+1.)
 257  CONTINUE
C
C        SMOOTH 4 CORNER POINTS.
C
         Q(1,1)=(P(1,1)+BQ4*(P(2,1)+P(1,2)))/
     1      (.5*BQ+1.)
         Q(NX,1)=(P(NX,1)+BQ4*(P(NXM1,1)+P(NX,4)))/
     1      (.5*BQ+1.)
         Q(1,NY)=(P(1,NY)+BQ4*(P(2,NY)+P(1,NYM1)))/
     1      (.5*BQ+1.)
         Q(NX,NY)=(P(NX,NY)+BQ4*(P(NXM1,NY)+P(NX,NYM1)))/
     1      (.5*BQ+1.)
C
C        TRANSFER SMOOTHED FIELD IN Q( , ) BACK TO P( , ).
      DO 270 J=1,NY
      DO 269 L=1,NX
         P(L,J)=Q(L,J)
 269  CONTINUE
C
 270  CONTINUE
C
 280  RETURN
      END
