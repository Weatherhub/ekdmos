      SUBROUTINE SMOTHG(KFILDO,P,Q,C,NX,NY,BQ,CSTSM,
     1                  TELEV,SEALND,NXE,NYE,
     2                  RMESH,NSMTYP,NSHLN,IER)
C
c        JULY      2005   GLAHN   MDL   MOS-2000
C                                 MODIFIED FROM SMOTH
C        JULY      2005   GLAHN   MODIFIED LOOPS
C        AUGUST    2005   GLAHN   ACCOMMODATED MISSING VALUES
C        SEPTEMBER 2005   GLAHN   ADDED BQW FOR WATER POINTS
C        SEPTEMBER 2005   GLAHN   CHANGED DIFFA FROM 150 TO 100
C        SEPTEMBER 2005   GLAHN   ADDED NO SMOOTHING OF SMALL ISLANDS
C        SEPTEMBER 2005   GLAHN   ADDED SMOOTHING OF HOT PEAKS
C        SEPTEMBER 2005   GLAHN   ADDED NSMTYP TO CALL
C        SEPTEMBER 2005   GLAHN   MODIFIED SMOOTHING OVER 9 POINTS
C                                 CHANGED DEFINITION OF SEALND( , )
C        SEPTEMBER 2005   GLAHN   MODIFIED FOR SEALND( , ) = 3
C        NOVEMBER  2005   GLAHN   REMOVED SMOOTHING OF A "HOT" PEAK
C        DECEMBER  2005   GLAHN   MODIFIED COMMENTS, ESP. PURPOSE
C        JANUARY   2006   GLAHN   ADDED CHECK FOR ALL LAND BEFORE
C                                 SMOOTHING OVER 9 POINTS; REINSERTED
C                                 SMOOTH OF A HOT PEAK 
C        JANUARY   2006   GLAHN   CORRECTED IF TEST ON DIAGONALS = 9
C        JANUARY   2006   GLAHN   ADDED NINT ON CHECKS FOR 9999. 
C        MARCH     2006   GLAHN   ADDED CSTSM 
C        SEPTEMBER 2006   GLAHN   CHANGED DIFFA FROM 200 TO 100
C        JANUARY   2007   GLAHN   ADDED NSHLN( ); COMMENTS
C        JANUARY   2007   GLAHN   ADDED HEAVY COASTAL WATER SMOOTHING
C        MARCH     2007   GLAHN   ADDED DIFFA WHEN FINDING A HIGH
C                                 OR LOW SPOT IN SMOOTHING PROCEDURE;
C                                 CORRECTED HEAVY COASTAL SMOOTHING;
C                                 ADDED C( , ) TO CALL; TESTS ON GE 3.
C                                 CHANGED TO LT 3.5, AND EQ 9. TO GT 8.5
C        MARCH     2007   GLAHN   INSERTED CHECK FOR MISSING IN DO 240
C        JUNE      2007   GLAHN   COMMENTS REGARDING OCEAN SMOOTHING
C
C        PURPOSE
C            TO SMOOTH FIELD P( , ) TO RETAIN ELEVATION DIFFERENCES.
C            CONSIDER THE POINT TO BE SMOOTHED AND THE FOUR 
C            SURROUNDING POINTS, THE TESTS ARE IN ORDER:
C               (1)  IF ANY POINT IN A 9-POINT STENCIL IS MISSING,
C                    SMOOTHING IS NOT DONE.  NORMALLY, THIS SHOULD
C                    NOT OCCUR.
C               (2)  IF A LAND POINT IS AN ISLAND OR SPIT OF LAND,
C                    DON'T SMOOTH.
C               (3)  IF ALL POINTS IN A 5-POINT STENCIL ARE LAND,
C                    THEN:
C                 (A)  IF ALL POINTS IN A 9-POINT STENCIL DIFFER
C                      FROM THE AVERAGE OF POINTS IN A 5-POINT
C                      STENCIL BY LT DIFFA, THEN SMOOTH USING THE
C                      9-POINT STENCIL.
C                 (B)  IF ALL POINTS IN A 5-POINT STENCIL DIFFER
C                      FROM THE AVERAGE OF POINTS IN THE 5-POINT
C                      STENCIL BY LT DIFFA, THEN SMOOTH USING THE
C                      5--POINT STENCIL.
C                 (C)  IF THE POINT IS THE HIGHER IN ELEVATION THAN
C                      ALL SURROUNDING POINTS, SMOOTH ONLY IF THE
C                      VALUE AT THE POINT IS HIGHER THAT THE OTHER
C                      4 POINTS (E.G., A HOT HILLTOP).
C                 (D)  IF THE POINT IS THE LOWER IN ELEVATION THAN
C                      ALL SURROUNDING POINTS, DON'T SMOOTH.
C                 (E)  IF THE DIFFERENCE BETWEEN THE POINT AND EACH
C                      OF THE BORDERING POINTS IN THE X DIRECTION
C                      IS LT DIFFA, THIS IS DEEMED AN EAST WEST TROUGH
C                      OR RIDGE OR A CONTOUR ON A SLOPE AND SMOOTHING
C                      IS IN THAT DIRECTION ONLY.
C                 (F)  IF THE DIFFERENCE BETWEEN THE POINT AND EACH
C                      OF THE BORDERING POINTS IN THE Y DIRECTION
C                      IS LT DIFFA, THIS IS DEEMED A NORTH SOUTH TROUGH
C                      OR RIDGE OR A CONTOUR ON A SLOPE AND SMOOTHING
C                      IS IN THAT DIRECTION ONLY.
C                 (G)  IF THE DIFFERENCE BETWEEN THE POINT AND EACH
C                      OF THE BORDERING POINTS IN THE NORTHWEST-
C                      SOUTHEAST DIRECTION IS LT DIFFA*1.414, THIS IS
C                      DEEMED A TROUGH OR RIDGE OR A CONTOUR ON A SLOPE
C                      AND SMOOTHING IS IN THAT DIRECTION ONLY.
C                 (H)  IF THE DIFFERENCE BETWEEN THE POINT AND EACH
C                      OF THE BORDERING POINTS IN THE NORTHEAST-
C                      SOUTHWEST DIRECTION IS LT DIFFA*1.414, THIS IS
C                      DEEMED A TROUGH OR RIDGE OR A CONTOUR ON A SLOPE
C                      AND SMOOTHING IS IN THAT DIRECTION ONLY.
C                 (I)  IF BOTH G AND H ARE TRUE, THE 4 DIAGONALS ARE
C                      SMOOTHED WITH THE CENTER POINT.  THIS WOULD
C                      REPRESENT A COL IN THE TERRAIN PATTERN.
C                 (J)  OTHERWISE NO SMOOTHING IS DONE.
C               (4)  IF ALL POINTS IN A 9-POINT STENCIL ARE WATER,
C                    SMOTH OVER 9 POINTS.
C               (5)  IF ALL POINTS IN A 5-POINT STENCIL ARE WATER,
C                    SMOTH OVER 5 POINTS.  
C               (6)  IF POINTS ON A 5-POINT STENCIL ARE BOTH WATER AND
C                    LAND, SMOOTH ONLY ONCE, NO MATTER THE VALUE OF
C                    NSMTYP (EQUIVALENT TO B = CSTSM).
C            NOTE THAT IN CASE OF THREE POINT SMOOTHING, THE CENTER
C            POINT IS WEIGHTED LESS THE THE OTHER TWO POINTS.
C
C        DATA SET USE
C            KFILDO  - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              P( , ) = FIELD TO SMOOTH.  (INPUT-OUTPUT)
C              Q( , ) = WORK ARRAY.  (INTERNAL)
C              C( , ) = WORK ARRAY.  (INTERNAL)
C              NX, NY = DIMENSIONS OF P( , ) AND Q( , ).  (INPUT)
C                  BQ = SMOOTHING PARAMETER.  NORMAL SMOOTHED VALUE AT
C                       POINT P = ORIGINAL VALUE PLUS BQ/4 TIMES SUM OF
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
C                       (INPUT)
C               CSTSM = THE SMOOTHING PARAMETER IF ANY POINT HAS WATER 
C                       BUT NOT ALL ARE WATER.  USE INSTEAD OF BQ.
C                       (INPUT)
C        TELEV(IX,JY) = THE TERRAIN ELEVATION FROM THE MOS-2000 EXTERNAL
C                       RANDOM ACCESS FILE (IX=1,NXE) (JY=1,NYE).
C                       (INPUT)
C           SEALND(J) = THE LAND/SEA MASK (J=1,NXE*NYE) AT NOMINAL
C                       MESHLENGTH MESHE.
C                       0 = OCEAN WATER GRIDPOINTS;
C                       3 = INLAND WATER GRIDPOINTS.
C                       9 = LAND GRIDPOINTS.
C                       (INPUT)
C             NXE,NYE = DIMENSIONS OF TELEV( , ) AND SEALND( , ).
C                       (INPUT)
C               RMESH = IS THE RATIO OF THE MESH LENGTH OF THE ANALYSIS
C                       GRID TO THE TERRAIN TELEV( , ) AND SEA/LAND
C                       SEALND( , ) GRIDS.  (INPUT)
C              NSMTYP = TYPE OF SMOOTHING:
C                       5 = SPECIAL TERRAIN-FOLLOWING SMOOTHING.
C                       6 = TWO PASSES OF 5 ABOVE.
C                       7 = THREE PASSES OF 5 ABOVE.
C                       (INPUT)
C            NSHLN(J) = DETERMINES SMOTHING AT HIGH AND LOW ELEVATIONS.
C                       A 1 INDICATES:
C                         J=1--HIGH ELEVATION, HIGH VALUE SMOOTHED.
C                         J=2--HIGH ELEVATION, LOW VALUE SMOOTHED.
C                         J=3--HIGH ELEVATION, NOT HIGH OR LOW VALUE 
C                                              SMOOTHED.
C                         J=4--LOW ELEVATION, HIGH VALUE SMOOTHED.
C                         J=5--LOW ELEVATION, LOW VALUE SMOOTHED.
C                         J=6--LOW ELEVATION, NOT HIGH OR LOW VALUE
C                                             SMOOTHED.
C                       A 0 INDICATES NO SMOOTHING FOR THE VALUES OF J.
C                       (INPUT)
C                 IER = ERROR RETURN.
C                       0 = GOOD RETURN.
C                       (OUTPUT)
C                DIFF = THE TERRAIN DIFFERENCE IN M REQUIRED AMONG 
C                       POINTS TO USE TERRAIN AS A CONSIDERATION FOR
C                       SMOOTHING.  SET BY DATA STATEMENT.  (INTERNAL)
C               DIFFA = THE TERRAIN DIFFERENCE IN M REQUIRED ALONG 
C                       POINTS TO USE TERRAIN AS A CONSIDERATION FOR
C                       SMOOTHING.  SET BY DATA STATEMENT.  (INTERNAL)
C              DIFFAD = THE TERRAIN DIFFERENCE IN M TO NOT BE EXCEEDED
C                       ALONG A DIAGONAL TO SMOOTH ALONG THE DIAGONAL.
C                       (INTERNAL) 
C                 BQW = SMOOTHING IF ANY POINT HAS WATER, BUT NOT ALL 
C                       ARE WATER.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED
C            NONE.
C
      DIMENSION P(NX,NY),Q(NX,NY),C(NX,NY)
      DIMENSION TELEV(NXE,NYE),SEALND(NXE,NYE)
      DIMENSION NSHLN(6)
C
      DATA DIFFA/100./
C        NOTE THAT DIFFA = 125 AND GREATER SMOOTHED ACROSS OWENS
C        VALLEY (LOS ANGELES AQUEDUCT), 100 DOES NOT.
C
CD     CALL TIMPR(KFILDO,KFILDO,'START SMOTHG        ')
      IER=0
      IF(BQ.EQ.0.)GO TO 280
      BP1=BQ+1.
      BQ4=BQ/4.
      BQ8=BQ/8.
      BQ2=BQ/2.
      BQW=CSTSM/4.
      BPW=CSTSM+1.
      NXM1=NX-1
      NYM1=NY-1
      DIFFAD=DIFFA*1.414
C
C        CHECK FOR LEGITIMATE VALUES OF NSMTYP.  THIS IS A SAFETY.
C        SMOTHG IS NORMALLY CALLED BY BCD5 AND SHOULD NOT BE CALLED
C        WHEN NSMTYP IS NOT LEGITIMATE.
C
      IF(NSMTYP.LT.5.OR.NSMTYP.GT.7)THEN
         WRITE(KFILDO,102)NSMTYP
 102     FORMAT(/' ****NSMTYP =',I4,' IN SMOTHG HAS A VALUE OUTSIDE',
     1           ' THE PERMISSIBLE RANGE 5 TO 7.  DO NOT SMOOTH.')
C           IER IS NOT SET.
         GO TO 280
      ENDIF
C
      DO 275 M=1,NSMTYP-4
C        THIS DOES 1, 2, OR 3 SMOOTHING PASSES, DEPENDING ON NSMTHP.
C        (PROBABLY THE BOUNDARIES DON'T NEED TO BE SMOOTHED MORE
C        THAN ONCE.)
C
C        SMOOTH ALL EXCEPT OUTER ROWS AND COLUMNS.
C
      DO 255 JY=2,NYM1
      DO 254 IX=2,NXM1
C
      IXE=NINT((IX-1)*RMESH)+1
C        IXE IS THE IX POSITION ON THE TERRAIN GRID.
      JYE=NINT((JY-1)*RMESH)+1
C        JYE IS THE JY POSITION ON THE TERRAIN GRID.
C
C        CHECK FOR MISSINGS ON A 9-POINT STENCIL.  DON'T SMOOTH IF
C        ANY POINT IS MISSING.  THIS IS THE ONLY CHECK FOR MISSING
C        EXCEPT ON THE BOUNDARIES.
C
      IF(NINT(P(IX,JY)).EQ.9999.OR.
     1   NINT(P(IX,JY+1)).EQ.9999.OR.
     2   NINT(P(IX,JY-1)).EQ.9999.OR.
     3   NINT(P(IX+1,JY)).EQ.9999.OR.
     4   NINT(P(IX+1,JY+1)).EQ.9999.OR.
     5   NINT(P(IX+1,JY-1)).EQ.9999.OR.
     6   NINT(P(IX-1,JY)).EQ.9999.OR.
     7   NINT(P(IX-1,JY+1)).EQ.9999.OR.
     8   NINT(P(IX-1,JY-1)).EQ.9999)THEN
C           DON'T SMOOTH.
         Q(IX,JY)=P(IX,JY)
         GO TO 254
      ENDIF
C
C        CHECK FOR ISLANDS OR SMALL SPITS OF LAND.  IF FOUND, DON'T 
C        SMOOTH.
C
      IF(SEALND(IXE,JYE).GT.8.5)THEN
C
C         WRITE(KFILDO,9017)IX,JY,SEALND(IXE,JYE),
C     1                           SEALND(IXE,JYE+1),
C     2                           SEALND(IXE,JYE-1),
C     3                           SEALND(IXE+1,JYE),
C     4                           SEALND(IXE-1,JYE),
C     5                           Q(IX,JY),P(IX,JY)
C 9017    FORMAT(' AT 9017 IN SMOTHG--IX,JY,SEALND(IXE,JYE),',
C     1                                    'SEALND(IXE,JYE+1),',
C     2                                    'SEALND(IXE,JYE-1),',
C     3                                    'SEALND(IXE+1,JYE),',
C     4                                    'SEALND(IXE-1,JYE),',
C     5                        'Q(IX,JY),P(IX,JY)',/,2I6,5F4.1,2F10.3)
C
C           THIS IS A LAND GRIDPOINT.
C
         IF(JYE+4.GT.NYE.OR.JYE-4.LT.1)GO TO 110
C           THIS STATEMENT GUARDS AGAINST ACCESSING POINTS OFF
C           THE GRID.
C
         IF((SEALND(IXE,JYE+4).LT.3.5.OR.SEALND(IXE,JYE+2).LT.3.5).AND.
     1      (SEALND(IXE,JYE-4).LT.3.5.OR.SEALND(IXE,JYE-2).LT.3.5))THEN
C
C              THIS IS AN ISLAND OR SPIT OF LAND.  DON'T SMOOTH.
C
            Q(IX,JY)=P(IX,JY)
CD           WRITE(KFILDO,8000)IX,JY,P(IX,JY)
CD8000       FORMAT(' AT 8000--IX,JY,P(IX,JY)',2I6,F10.2)
            GO TO 254
C              DON'T SMOOTH LAND POINT IF OPPOSITE POINTS ARE WATER.
C              THIS MUST BE AN ISLAND.  POINTS WITHIN THREE GRIDLENGTHS
C              OF THE GRID BORDER ARE NOT CONSIDERED.
         ENDIF
C
         IF(IXE+4.GT.NXE.OR.IXE-4.LT.1)GO TO 110
C           THIS STATEMENT GUARDS AGAINST ACCESSING POINTS OFF
C           THE GRID.
C
         IF((SEALND(IXE+4,JYE).LT.3.5.OR.SEALND(IXE+2,JYE).LT.3.5).AND.
     1      (SEALND(IXE-4,JYE).LT.3.5.OR.SEALND(IXE-2,JYE).LT.3.5))THEN
C
C              THIS IS AN ISLAND OR SPIT OF LAND.  DON'T SMOOTH.
C
            Q(IX,JY)=P(IX,JY)
CD           WRITE(KFILDO,8001)IX,JY,P(IX,JY)
CD8001       FORMAT(' AT 8001--IX,JY,P(IX,JY)',2I6,F10.2)
            GO TO 254        
C              DON'T SMOOTH LAND POINT IF OPPOSITE POINTS ARE WATER.
C              THIS MUST BE AN ISLAND.  POINTS WITHIN THREE GRIDLENGTHS
C              OF THE GRID BORDER ARE NOT CONSIDERED. 
         ENDIF
C
      ENDIF
C
C        COMES HERE IF GRIDPOINT NOT MISSING AND IS EITHER WATER OR IS
C        LAND AND NOT AN ISLAND OR SPIT.
C
 110  IF(SEALND(IXE,JYE).GT.8.5.AND.
     1   SEALND(IXE,JYE+1).GT.8.5.AND.
     2   SEALND(IXE,JYE-1).GT.8.5.AND.
     3   SEALND(IXE+1,JYE).GT.8.5.AND.
     4   SEALND(IXE-1,JYE).GT.8.5)THEN
C           ALL 5 POINTS ARE LAND.  ANY POINT INVOLVING ALL WATER
C           IS SMOOTHED NORMALLY (SEE BELOW).
C
CD        WRITE(KFILDO,9000)IX,JY,IXE,JYE,TELEV(IXE,JYE),
CD    1                     TELEV(IXE+1,JYE),TELEV(IXE-1,JYE),
CD    2                     TELEV(IXE,JYE+1),TELEV(IXE,JYE-1),
CD    3                     P(IX,JY),
CD    4                     P(IX+1,JY),P(IX-1,JY),
CD    5                     P(IX,JY+1),P(IX,JY-1)
CD9000    FORMAT(' AT 9000',4I4,11F9.3)
C
C           USE ELEVATION TO GOVERN SMOOTHING.
C
C           CHECK FOR SIGNIFICANT TERRAIN DIFFERENCE.
C
         AVG=(TELEV(IXE,JYE)+TELEV(IXE+1,JYE)+TELEV(IXE-1,JYE)
     1                      +TELEV(IXE,JYE+1)+TELEV(IXE,JYE-1))/5.
C
         IF(ABS(TELEV(IXE,JYE)-AVG).LT.DIFFA.AND.
     1      ABS(TELEV(IXE+1,JYE)-AVG).LT.DIFFA.AND.
     2      ABS(TELEV(IXE-1,JYE)-AVG).LT.DIFFA.AND.
     3      ABS(TELEV(IXE,JYE+1)-AVG).LT.DIFFA.AND.
     4      ABS(TELEV(IXE,JYE-1)-AVG).LT.DIFFA)THEN
C
C              THE 5 CLOSEST POINTS CHECK.  CHECK THE DIAGONALS.  BUT
C              FIRST MAKE SURE ALL POINTS ARE LAND.
C
            IF(SEALND(IXE+1,JYE+1).GT.8.5.AND.
     1         SEALND(IXE+1,JYE-1).GT.8.5.AND.
     2         SEALND(IXE-1,JYE+1).GT.8.5.AND.
     3         SEALND(IXE-1,JYE-1).GT.8.5)THEN
C
               IF(ABS(TELEV(IXE+1,JYE+1)-AVG).LT.DIFFA.AND.
     1            ABS(TELEV(IXE+1,JYE-1)-AVG).LT.DIFFA.AND.
     2            ABS(TELEV(IXE-1,JYE+1)-AVG).LT.DIFFA.AND.
     3            ABS(TELEV(IXE-1,JYE-1)-AVG).LT.DIFFA)THEN
C
C                    NO SIGNIFICANT TERRAIN DIFFERENCE AT ALL 9 POINTS.
C                    SMOOTH 9 POINTS WEIGHTING THE CENTER USING BQ4 AND
C                    B+5 WITH THE SURROUNDING 8 POINTS.  NOTE THAT THE 
C                    AVERAGE OF ONLY THE 5 POINTS IS USED FOR COMPUTING
C                    THE AVERAGE.
C
                  Q(IX,JY)=(P(IX,JY)
     1                +BQ8*(P(IX-1,JY)+P(IX+1,JY)+P(IX,JY-1)+P(IX,JY+1)+
     2                      P(IX+1,JY+1)+P(IX+1,JY-1)+
     3                      P(IX-1,JY+1)+P(IX-1,JY-1)))/BP1
C
CD                 WRITE(KFILDO,9002)AVG,Q(IX,JY),
CD    1                    TELEV(IXE+1,JYE-1),TELEV(IXE-1,JYE+1),
CD    2                    TELEV(IXE-1,JYE-1),TELEV(IXE+1,JYE+1),
CD    3                    P(IXE+1,JYE-1),P(IXE-1,JYE+1),
CD    4                    P(IXE-1,JYE-1),P(IXE+1,JYE+1)
CD9002             FORMAT(' AT 9002--AVG,Q(IX,JY)',10F11.3)
C
                  GO TO 254
C
               ELSE
C
C                    ALL NINE POINTS ARE NOT LAND OR DO NOT CHECK WITHIN
C                    THE AVG CRITERION.  SMOOTH NORMALLY WITH 5 POINTS.
C
                  Q(IX,JY)=(P(IX,JY)+BQ4*(P(IX-1,JY)+
     1                      P(IX+1,JY)+P(IX,JY-1)+P(IX,JY+1)))/BP1
C
CD                 WRITE(KFILDO,9003)AVG,Q(IX,JY),
CD    1                    TELEV(IXE+1,JYE-1),TELEV(IXE-1,JYE+1),
CD    2                    TELEV(IXE-1,JYE-1),TELEV(IXE+1,JYE+1),
CD    3                    P(IXE+1,JYE-1),P(IXE-1,JYE+1),
CD    4                    P(IXE-1,JYE-1),P(IXE+1,JYE+1)
CD9003             FORMAT(' AT 9003--AVG,Q(IX,JY)',10F11.3)
C
                  GO TO 254
C
               ENDIF
C
            ELSE
C
C                 DIAGONALS ARE NOT ALL LAND.  SMOOTH OVER 5 POINTS.
C
               Q(IX,JY)=(P(IX,JY)+BQ4*(P(IX-1,JY)+
     1                   P(IX+1,JY)+P(IX,JY-1)+P(IX,JY+1)))/BP1
               GO TO 254
            ENDIF
C
C           CHECK FOR HIGH ELEVATION CENTER.
C
         ELSEIF(TELEV(IXE,JYE).GT.TELEV(IXE,JYE+1)+DIFFA.AND.
     1          TELEV(IXE,JYE).GT.TELEV(IXE,JYE-1)+DIFFA.AND.
     2          TELEV(IXE,JYE).GT.TELEV(IXE-1,JYE)+DIFFA.AND.
     3          TELEV(IXE,JYE).GT.TELEV(IXE+1,JYE)+DIFFA)THEN
C
C              THIS IS A HIGH TERRAIN POINT.
C
            IF(P(IX,JY).GT.P(IX,JY+1).AND.
     1         P(IX,JY).GT.P(IX,JY-1).AND.
     2         P(IX,JY).GT.P(IX+1,JY).AND.
     3         P(IX,JY).GT.P(IX-1,JY))THEN
C
C                 THIS IS A HIGH VALUE AT HIGH ELEVATION.
C                 SMOOTH DEPENDING ON NSHLN(1).
C
               IF(NSHLN(1).EQ.1)THEN
                  Q(IX,JY)=(P(IX,JY)
     1           +BQ4*(P(IX-1,JY)+P(IX+1,JY)+P(IX,JY-1)+P(IX,JY+1)))/BP1
               ELSE
                  Q(IX,JY)=P(IX,JY)
               ENDIF
C
               GO TO 254

            ELSEIF(P(IX,JY).LT.P(IX,JY+1).AND.
     1             P(IX,JY).LT.P(IX,JY-1).AND.
     2             P(IX,JY).LT.P(IX+1,JY).AND.
     3             P(IX,JY).LT.P(IX-1,JY))THEN
C
C                 THIS IS A LOW VALUE AT HIGH ELEVATION.
C                 SMOOTH DEPENDING ON NSHLN(2).
C
               IF(NSHLN(2).EQ.1)THEN
                  Q(IX,JY)=(P(IX,JY)
     1           +BQ4*(P(IX-1,JY)+P(IX+1,JY)+P(IX,JY-1)+P(IX,JY+1)))/BP1
               ELSE
                  Q(IX,JY)=P(IX,JY)
               ENDIF
C            
               GO TO 254
C
            ELSE
C
C                 THIS IS A HIGH ELEVATION, BUT NOT A HIGH OR LOW VALUE.
C                 SMOOTH DEPENDING ON THE VALUE OF NSHLN(3)
C
               IF(NSHLN(3).EQ.1)THEN
                  Q(IX,JY)=(P(IX,JY)
     1           +BQ4*(P(IX-1,JY)+P(IX+1,JY)+P(IX,JY-1)+P(IX,JY+1)))/BP1
               ELSE
                  Q(IX,JY)=P(IX,JY)
               ENDIF
C            
               GO TO 254
C
            ENDIF
C
C           CHECK FOR LOW ELEVATON CENTER.
C                
         ELSEIF(TELEV(IXE,JYE).LT.TELEV(IXE,JYE+1)-DIFFA.AND.
     5          TELEV(IXE,JYE).LT.TELEV(IXE,JYE-1)-DIFFA.AND.
     6          TELEV(IXE,JYE).LT.TELEV(IXE-1,JYE)-DIFFA.AND.
     7          TELEV(IXE,JYE).LT.TELEV(IXE+1,JYE)-DIFFA)THEN 
C
C              THIS IS A LOW TERRAIN POINT.
C
            IF(P(IX,JY).GT.P(IX,JY+1).AND.
     1         P(IX,JY).GT.P(IX,JY-1).AND.
     2         P(IX,JY).GT.P(IX+1,JY).AND.
     3         P(IX,JY).GT.P(IX-1,JY))THEN
C
C                 THIS IS A HIGH VALUE AT LOW ELEVATION.
C                 SMOOTH DEPENDING ON NSHLN(4).
C
               IF(NSHLN(4).EQ.1)THEN
                  Q(IX,JY)=(P(IX,JY)
     1           +BQ4*(P(IX-1,JY)+P(IX+1,JY)+P(IX,JY-1)+P(IX,JY+1)))/BP1
               ELSE
                  Q(IX,JY)=P(IX,JY)
               ENDIF
C            
               GO TO 254
C
            ELSEIF(P(IX,JY).LT.P(IX,JY+1).AND.
     1             P(IX,JY).LT.P(IX,JY-1).AND.
     2             P(IX,JY).LT.P(IX+1,JY).AND.
     3             P(IX,JY).LT.P(IX-1,JY))THEN
C
C                 THIS IS A LOW VALUE AT LOW ELEVATION.
C                 SMOOTH DEPENDING ON NSHLN(5).
C
               IF(NSHLN(5).EQ.1)THEN
                  Q(IX,JY)=(P(IX,JY)
     1           +BQ4*(P(IX-1,JY)+P(IX+1,JY)+P(IX,JY-1)+P(IX,JY+1)))/BP1
               ELSE
                  Q(IX,JY)=P(IX,JY)
               ENDIF
C            
               GO TO 254
C
            ELSE
C
C                 THIS IS A LOW ELEVATION, BUT NOT A HIGH OR LOW VALUE.
C                 SMOOTH DEPENDING ON THE VALUE OF NSHLN(6)
C
               IF(NSHLN(6).EQ.1)THEN
                  Q(IX,JY)=(P(IX,JY)
     1           +BQ4*(P(IX-1,JY)+P(IX+1,JY)+P(IX,JY-1)+P(IX,JY+1)))/BP1
               ELSE
                  Q(IX,JY)=P(IX,JY)
               ENDIF
C            
               GO TO 254
C          
            ENDIF
C
C               THIS GRIDPOINT IS NOT A HIGH OR LOW CENTER, BUT 
C               WE KNOW THAT THERE IS SOME ELEVATION DIFFERENCE;
C               THE TERRAIN IS NOT FLAT.
C
C               CHECK POINTS ALONG IX DIRECTION WITHIN DIFFA.
C
         ELSEIF(ABS(TELEV(IXE+1,JYE)-TELEV(IXE,JYE)).LT.DIFFA.AND.
     1          ABS(TELEV(IXE-1,JYE)-TELEV(IXE,JYE)).LT.DIFFA)THEN
C
C              THIS IS AN EAST-WEST RIDGE OR VALLEY
C              OR A CONTOUR LINE ON A SLOPE.
C              SMOOTH ALONG IT.
C
            Q(IX,JY)=(P(IX,JY)+BQ2*(P(IX+1,JY)+P(IX-1,JY)))/BP1
C
CD           WRITE(KFILDO,9006)AVG,Q(IX,JY)
CD9006       FORMAT(' AT 9006--AVG,Q(IX,JY)',2F11.3)
C
            GO TO 254
C
C             CHECK POINTS ALONG JY DIRECTION WITHIN DIFFA.
C      
         ELSEIF(ABS(TELEV(IXE,JYE+1)-TELEV(IXE,JYE)).LT.DIFFA.AND.
     1          ABS(TELEV(IXE,JYE-1)-TELEV(IXE,JYE)).LT.DIFFA)THEN
C
C              THIS IS A NORTH-SOUTH RIDGE OR VALLEY
C              OR A CONTOUR LINE ON A SLOPE.
C              SMOOTH ALONG IT.
C
            Q(IX,JY)=(P(IX,JY)+BQ2*(P(IX,JY+1)+P(IX,JY-1)))/BP1
C
CD           WRITE(KFILDO,9008)AVG,Q(IX,JY)
CD9008       FORMAT(' AT 9008--AVG,Q(IX,JY)',2F11.3)
C      
C              CHECK FOR NORTHWEST-SOUTHEAST DIAGONAL TROUGH
C              OR RIDGE.
C
            GO TO 254
C
         ELSEIF(ABS(TELEV(IXE-1,JYE+1)-TELEV(IXE,JYE)).LT.DIFFAD.AND.
     1          ABS(TELEV(IXE+1,JYE-1)-TELEV(IXE,JYE)).LT.DIFFAD)THEN
C
C              THIS IS A NORTHWEST-SOUTHEAST TROUGH OR RIDGE
C              OR A CONTOUR LINE ON A SLOPE.  HOWEVER, IT COULD
C              ALSO CHECK TO BE A NORTHEAST-SOUTHWEST ONE.
C              CHECK FOR THAT.
C
            IF(ABS(TELEV(IXE+1,JYE+1)-TELEV(IXE,JYE)).LT.DIFFAD.AND.
     1         ABS(TELEV(IXE-1,JYE-1)-TELEV(IXE,JYE)).LT.DIFFAD)THEN
C                  
C                  BOTH DIAGONALS CHECK, SO SMOOTH THE CENTER AND
C                  THE FOUR DIAGONAL POINTS.
C
               Q(IX,JY)=(P(IX,JY)+BQ4*(P(IX+1,JY-1)+P(IX-1,JY+1)+
     1                                 P(IX+1,JY+1)+P(IX-1,JY-1)))/BP1
C
CD              WRITE(KFILDO,9010)AVG,Q(IX,JY),
CD    1                    TELEV(IXE+1,JYE-1),TELEV(IXE-1,JYE+1),
CD    2                    TELEV(IXE-1,JYE-1),TELEV(IXE+1,JYE+1),
CD    3                    P(IXE+1,JYE-1),P(IXE-1,JYE+1),
CD    4                    P(IXE-1,JYE-1),P(IXE+1,JYE+1)
CD9010          FORMAT(' AT 9010--AVG,Q(IX,JY)',10F11.3)
C
               GO TO 254
C
            ELSE
C
C                 SMOOTH ONLY THE NORTHWEST-SOUTHEAST LINE.  NOTE THAT
C                 THIS IS OVER A DISTANCE 1.414 TIMES THE SMOOTHING IN
C                 AN EAST-WEST OR NORTH-SOUTH DIRECTION.
C
               Q(IX,JY)=(P(IX,JY)+BQ2*(P(IX+1,JY-1)+P(IX-1,JY+1)))/BP1
C
CD              WRITE(KFILDO,9012)AVG,Q(IX,JY),
CD    1                    TELEV(IXE+1,JYE-1),TELEV(IXE-1,JYE+1),
CD    2                    TELEV(IXE-1,JYE-1),TELEV(IXE+1,JYE+1),
CD    3                    P(IXE+1,JYE-1),P(IXE-1,JYE+1),
CD    4                    P(IXE-1,JYE-1),P(IXE+1,JYE+1)
CD9012          FORMAT(' AT 9012--AVG,Q(IX,JY)',10F11.3)
C
               GO TO 254
C      
            ENDIF
C
C              CHECK FOR NORTHEAST-SOUTHWEST DIAGONAL TROUGH
C              OR RIDGE.
C
         ELSEIF(ABS(TELEV(IXE+1,JYE+1)-TELEV(IXE,JYE)).LT.DIFFAD.AND.
     1          ABS(TELEV(IXE-1,JYE-1)-TELEV(IXE,JYE)).LT.DIFFAD)THEN
C
C              THIS IS A NORTHEAST-SOUTHWEST TROUGH OR RIDGE
C              OR A CONTOUR LINE ON A SLOPE.  NOTE THAT IT DID NOT
C              CHECK FOR A NORTHWEST-SOUTHEAST SLOPE, SO SMOOTH IN THE
C              NORTHEAST-SOUTHWEST DIRECTION.  NOTE ALSO THAT THIS IS
C              OVER A DISTANCE 1.414 TIMES THE SMOOTHING IN AN EAST-
C              WEST OR NORTH-SOUTH DIRECTION.
C
            Q(IX,JY)=(P(IX,JY)+BQ2*(P(IX-1,JY-1)+P(IX+1,JY+1)))/BP1
C
CD           WRITE(KFILDO,9014)AVG,Q(IX,JY),
CD    1                 TELEV(IXE+1,JYE-1),TELEV(IXE-1,JYE+1),
CD    2                 TELEV(IXE-1,JYE-1),TELEV(IXE+1,JYE+1),
CD    3                 P(IXE+1,JYE-1),P(IXE-1,JYE+1),
CD    4                 P(IXE-1,JYE-1),P(IXE+1,JYE+1)
CD9014       FORMAT(' AT 9014--AVG,Q(IX,JY)',10F11.3)
C
            GO TO 254
C
C              CAN'T FIND A CONTOUR LINE TO FOLLOW.
         ELSE
C  
C              DON'T SMOOTH.
C
            Q(IX,JY)=P(IX,JY) 
C
CD           WRITE(KFILDO,9015)AVG,Q(IX,JY)
CD9015       FORMAT(' AT 9015--AVG,Q(IX,JY)',2F11.3)
C
            GO TO 254
C
         ENDIF
C 
      ENDIF      
C
CCC         WRITE(KFILDO,9997)IX,JY,IXE,JYE,SEALND(IXE,JYE)
CCC 9997    FORMAT(' AT 9997--IX,JY,IXE,JYE,SEALND(IXE,JYE)',4I10,F5.1)
C
C      
C        COMES HERE IF ONE OR MORE OF THE POINTS IN A 5-POINT
C        STENCIL IS AN OCEAN OR INLAND WATER POINT.
C 
      IF(SEALND(IXE,JYE).LT.3.5.AND.
     1   SEALND(IXE,JYE+1).LT.3.5.AND.
     2   SEALND(IXE,JYE-1).LT.3.5.AND.
     3   SEALND(IXE+1,JYE).LT.3.5.AND.
     4   SEALND(IXE-1,JYE).LT.3.5)THEN
C           ALL 5 POINTS ARE WATER.
C
         IF(SEALND(IXE+1,JYE+1).LT.3.5.AND.
     1      SEALND(IXE-1,JYE-1).LT.3.5.AND.
     2      SEALND(IXE+1,JYE-1).LT.3.5.AND.
     3      SEALND(IXE-1,JYE+1).LT.3.5)THEN
C
C           ALL 9 POINTS ARE WATER, OCEAN OR INLAND.  SMOOTH
C           OVER 9 POINTS.
C
               Q(IX,JY)=(P(IX,JY)
     1            +BQ8*(P(IX-1,JY)+P(IX+1,JY)+P(IX,JY-1)+P(IX,JY+1)+
     2                  P(IX+1,JY+1)+P(IX+1,JY-1)+
     3                  P(IX-1,JY+1)+P(IX-1,JY-1)))/BP1
C           MUST FALL THROUGH.
         ELSE
C
C              ONLY 5 POINTS ARE WATER.  SMOOTH OVER 5 POINTS.
C
            Q(IX,JY)=(P(IX,JY)
     1           +BQ4*(P(IX-1,JY)+P(IX+1,JY)+P(IX,JY-1)+P(IX,JY+1)))/BP1
C           MUST FALL THROUGH.
         ENDIF
C
      ELSE
C           SOME POINTS ARE WATER AND SOME ARE LAND; DO NOT SMOOTH.
         Q(IX,JY)=P(IX,JY)
      ENDIF
C
C        AT THIS POINT, Q( , ) HAS BEEN SET FOR ALL POINTS EXCEPT
C        THE BORDERS. 
C
C        NOW SMOOTH OCEAN POINTS ON LAST PASS.  THIS OBVIATES THE
C        SMOOTHING OF 5 OR 9 WATER POINTS ABOVE ON LAST PASS.
C        IF THIS CREATES A BORDER PROBLEM, THE IF STATEMENT AND
C        FOLLOWING CAN BE OMITTED.
C
CCC         WRITE(KFILDO,9998)IX,JY,IXE,JYE,SEALND(IXE,JYE)
CCC 9998    FORMAT(' AT 9998--IX,JY,IXE,JYE,SEALND(IXE,JYE)',4I10,F5.1)
C
      IF(M.EQ.NSMTYP-4.AND.IXE.EQ.IX)THEN
C           ONLY DO THIS ONCE, AND ONLY IF THE GRIDLENGTH OF THE
C           ANALYSIS GRID AND SEALND( , ) GRID ARE THE SAME MESH
C           LENGTH.  THAT IS, IX AND IXE ARE THE SAME.
C           (JUST NOT PROGRAMMED FOR THE OTHER POSSIBILITY.
C           IT'S HIGHLY UNLIKELY THE LAST PASS IS NOT AT THE
C           SEALND( , ) GRIDLENGTH.)
C
CCC         WRITE(KFILDO,9999)IX,JY,IXE,JYE,SEALND(IXE,JYE)
CCC 9999    FORMAT(' AT 9999--IX,JY,IXE,JYE,SEALND(IXE,JYE)',4I10,F5.1)
C
         IF(SEALND(IXE,JYE).LT.1.5)THEN
C              THIS SMOOTHING IS ONLY FOR OCEAN POINTS.
            NCOUNT=0
            SUM=0.
C
            DO 240 JYY=MAX(1,JY-4),MIN(NY,JY+4)
            DO 239 IXX=MAX(1,IX-4),MIN(NX,IX+4)
C
            IF(SEALND(IXX,JYY).LT.1.5.AND.P(IXX,JYY).LE.9998.)THEN
C                 ONLY INVOLVE WATER POINTS.  CHECK FOR MISSING
C                 WITH 9998 TO BYPASS NINT.
               NCOUNT=NCOUNT+1
               SUM=SUM+P(IXX,JYY)
            ENDIF
C
 239        CONTINUE
 240        CONTINUE
C
            IF(NCOUNT.NE.0)THEN
               Q(IX,JY)=SUM/NCOUNT
C                 NOTE THAT Q( , ) HAS BEEN SET TO P( , ) ABOVE
C                 IN CASE NCOUNT = 0.
            ENDIF
C
         ENDIF
C
      ENDIF
C
 254  CONTINUE
 255  CONTINUE
C   
C        NOW SMOOTH WITHIN ONE GRIDPOINT ALONG THE OCEAN OR LAKE 
C        COASTS WITH CSTSM.
C
      IF(M.EQ.NSMTYP-4.AND.CSTSM.NE.0.)THEN
C
C           TRANSFER POINTS IN Q( , ) INTO C( , ), BECAUSE Q( , )
C           HAS ALREADY BEEN MODIFIED.  CANNOT SMOOTH INTO THE SAME
C           ARRAY.
C
         DO 245 JY=1,NY
         DO 244 IX=1,NX
         C(IX,JY)=Q(IX,JY)
 244     CONTINUE
 245     CONTINUE
C   
         DO 2455 JY=2,NYM1
         DO 2454 IX=2,NXM1
C
         IF(NINT(RMESH).EQ.1)THEN
C              THIS TEST FOR EFFICIENCY.  USUALLY RMESH = 1.
            IXE=IX
            JYE=JY
         ELSE
            IXE=NINT((IX-1)*RMESH)+1
C              IXE IS THE IX POSITION ON THE TERRAIN GRID.
            JYE=NINT((JY-1)*RMESH)+1
C              JYE IS THE JY POSITION ON THE TERRAIN GRID.
         ENDIF
C
         IF(SEALND(IXE,JYE).LE.3.5)THEN
C              THIS IS A WATER POINT.  TEST ON 3.5 TO AVOID NINT.
C
            IF(SEALND(IXE+1,JYE).GT.3.5.OR.
     1         SEALND(IXE-1,JYE).GT.3.5.OR.
     2         SEALND(IXE,JYE+1).GT.3.5.OR.
     3         SEALND(IXE,JYE-1).GT.3.5)THEN
C                 ONE OF THE SURROUNDING POINTS IS LAND.
               Q(IX,JY)=(C(IX,JY)
     1           +BQW*(C(IX-1,JY)+C(IX+1,JY)+C(IX,JY-1)+C(IX,JY+1)))/BPW
            ENDIF
C
         ELSE
C              THIS IS A LAND POINT.
C
            IF(SEALND(IXE+1,JYE).LT.3.5.OR.
     1         SEALND(IXE-1,JYE).LT.3.5.OR.
     2         SEALND(IXE,JYE+1).LT.3.5.OR.
     3         SEALND(IXE,JYE-1).LT.3.5)THEN
C                 ONE OF THE SURROUNDING POINTS IS WATER.
               Q(IX,JY)=(C(IX,JY)
     1           +BQW*(C(IX-1,JY)+C(IX+1,JY)+C(IX,JY-1)+C(IX,JY+1)))/BPW
            ENDIF
C
         ENDIF
C
 2454    CONTINUE
 2455    CONTINUE
C
      ENDIF
C
CD        WRITE(KFILDO,9018)IX,JY,SEALND(IXE,JYE),
CD    1                           SEALND(IXE,JYE+1),
CD    2                           SEALND(IXE,JYE-1),
CD    3                           SEALND(IXE+1,JYE),
CD    4                           SEALND(IXE-1,JYE),
CD    5                           Q(IX,JY),P(IX,JY)
CD9018    FORMAT(' AT 9018 IN SMOTHG--IX,JY,SEALND(IXE,JYE),',
CD    1                                    'SEALND(IXE,JYE+1),',
CD    2                                    'SEALND(IXE,JYE-1),',
CD    3                                    'SEALND(IXE+1,JYE),',
CD    4                                    'SEALND(IXE-1,JYE),',
CD    5                         'Q(IX,JY),P(IX,JY)',/,2I6,5F4.1,2F10.3)
C
C        SMOOTH BOTTOM AND TOP ROWS.
C
      DO 256 L=2,NXM1
C
         IF(NINT(P(L,1)).EQ.9999.OR.
     1      NINT(P(L-1,1)).EQ.9999.OR.
     2      NINT(P(L+1,1)).EQ.9999.OR.
     3      NINT(P(L,2)).EQ.9999)THEN
C              DON'T SMOOTH.
            Q(L,1)=P(L,1)
         ELSE
            Q(L,1)=(P(L,1)+BQ4*(P(L-1,1)+P(L+1,1)+P(L,2)))/
     1         (.75*BQ+1.)
         ENDIF
C
         IF(NINT(P(L,NY)).EQ.9999.OR.
     1      NINT(P(L-1,NY)).EQ.9999.OR.
     2      NINT(P(L+1,NY)).EQ.9999.OR.
     3      NINT(P(L,NYM1)).EQ.9999)THEN
C              DON'T SMOOTH.
            Q(L,NY)=P(L,NY)
         ELSE
            Q(L,NY)=(P(L,NY)+BQ4*(P(L-1,NY)+P(L+1,NY)+P(L,NYM1)))/
     1         (.75*BQ+1.)
         ENDIF
C
 256  CONTINUE
C
C        SMOOTH 1ST AND LAST COLUMNS.
C
      DO 257 J=2,NYM1
C
         IF(NINT(P(1,J)).EQ.9999.OR.
     1      NINT(P(1,J-1)).EQ.9999.OR.
     2      NINT(P(1,J+1)).EQ.9999.OR.
     3      NINT(P(2,J)).EQ.9999)THEN
C              DON'T SMOOTH.
            Q(1,J)=P(1,J)
         ELSE      
            Q(1,J)=(P(1,J)+BQ4*(P(1,J-1)+P(1,J+1)+P(2,J)))/
     1             (.75*BQ+1.)
         ENDIF
C
         IF(NINT(P(NX,J)).EQ.9999.OR.
     1      NINT(P(NX,J-1)).EQ.9999.OR.
     2      NINT(P(NX,J+1)).EQ.9999.OR.
     3      NINT(P(NXM1,J)).EQ.9999)THEN
C              DON'T SMOOTH
            Q(NX,J)=P(NX,J)
         ELSE
            Q(NX,J)=(P(NX,J)+BQ4*(P(NX,J-1)+P(NX,J+1)+P(NXM1,J)))/
     1              (.75*BQ+1.)
         ENDIF
C
 257  CONTINUE
C
C        SMOOTH 4 CORNER POINTS.
C
         IF(NINT(P(1,1)).EQ.9999.OR.
     1      NINT(P(2,1)).EQ.9999.OR.
     2      NINT(P(1,2)).EQ.9999)THEN
C              DON'T SMOOTH.
            Q(1,1)=P(1,1)
         ELSE
            Q(1,1)=(P(1,1)+BQ4*(P(2,1)+P(1,2)))/
     1             (.5*BQ+1.)
         ENDIF
C
         IF(NINT(P(NX,1)).EQ.9999.OR.
     1      NINT(P(NXM1,1)).EQ.9999.OR.
     2      NINT(P(NX,2)).EQ.9999)THEN
C              DON'T SMOOTH.
            Q(NX,1)=P(NX,1)
         ELSE
            Q(NX,1)=(P(NX,1)+BQ4*(P(NXM1,1)+P(NX,2)))/
     1              (.5*BQ+1.)
         ENDIF
C
         IF(NINT(P(1,NY)).EQ.9999.OR.
     1      NINT(P(2,NY)).EQ.9999.OR.
     2      NINT(P(1,NYM1)).EQ.9999)THEN
C              DON'T SMOOTH.
            Q(1,NY)=P(1,NY)
         ELSE
            Q(1,NY)=(P(1,NY)+BQ4*(P(2,NY)+P(1,NYM1)))/
     1              (.5*BQ+1.)
         ENDIF
C
         IF(NINT(P(NX,NY)).EQ.9999.OR.
     1      NINT(P(NXM1,NY)).EQ.9999.OR.
     2      NINT(P(NX,NYM1)).EQ.9999)THEN
C              DON'T SMOOTH.
            Q(NX,NY)=P(NX,NY)
         ELSE    
            Q(NX,NY)=(P(NX,NY)+BQ4*(P(NXM1,NY)+P(NX,NYM1)))/
     1               (.5*BQ+1.)
         ENDIF
C
C        TRANSFER SMOOTHED FIELD IN Q( , ) BACK TO P( , ).
      DO 270 J=1,NY
      DO 269 L=1,NX
         P(L,J)=Q(L,J)
 269  CONTINUE
C
 270  CONTINUE
C
 275  CONTINUE
C
CD     CALL TIMPR(KFILDO,KFILDO,'END   SMOTHG        ')
 280  RETURN
      END
