      SUBROUTINE SETPNT(KFILDO,DATA,LTAG,XP,YP,NSTA,P,DIST,NX,NY,ISETP,
     1                  IER)
C
C        OCTOBER   2004   GLAHN   MDL MOS-2000
C        FEBRUARY  2005   GLAHN   MODIFIED TO MAKE PARTIAL CORRECTION;
C                                 ADDED ISETP TO CALL
C        JUNE      2005   GLAHN   REMOVED CHECK OF LTAG = -3; COMMENTS;
C                                 CHECKED FOR GRIDPOINT = 9999.
C        SEPTEMBER 2005   GLAHN   INCLUDED INITIAL TEST FOR ISETP = 0
C        NOVEMBER  2005   DALLAVALLE   MODIFIED FORMAT STATEMENTS TO
C                                      CONFORM TO FORTRAN90 STANDARDS
C                                      ON THE IBM SP; CHANGED IF
C                                      TESTS FOR EQUALITY OF REAL 
C                                      VALUES
C        FEBRUARY  2006   GLAHN   ADDED PARAGRAPH IN PURPOSE
C
C        PURPOSE
C            FOR EVERY USABLE DATA POINT IN DATA( ), WHOSE POSITION ON
C            A GRID P( , ) IS REPRESENTED BY XP( ) AND YP( ), MODIFY
C            THE VALUE OF THE GRIDPOINT AT THE CLOSEST GRIDPOINT,
C            PROVIDED:
C            (1)  THE DATA POINT IS WITHIN THE GRID.
C            (2)  A CLOSER DATA POINT HAS NOT ALREADY MODIFIED THE
C                 GRIDPOINT.
C            WHEN ISETP = 1:
C              THE GRIDPOINT CORRECTION IS IN THE DIRECTION OF THE DATA
C              POINT, BUT DOES NOT TRAVERSE AN INTEGER VALUE.
C              EXAMPLES:
C                 DATA POINT = 45.4; GRIDPOINT = 48.5; NEW VALUE = 48.1
C                              45.4              44.4              44.9
C              THIS ALWAYS MAKES LESS THAN 1 UNIT CORRECTION, AND
C              KEEPS BULLSEYES DOWN WHEN CONTOURING AT INTEGER VALUES.
C              EVEN THOUGH A CORRECTION MAY BE MADE TO A CORRECTED
C              GRIDPOINT, THAT IS OK.
C            WHEN ISETP = 2:
C              THE GRIDPOINT IS SET TO THE STATION VALUE.
C
C              CAUTION:  THIS COULD MAKE A LARGE CORRECTION; HOWEVER,
C                        STATIONS TOSSED IN THE LAST PASS ARE NOT
C                        USED.
C
C            WHEN MORE THAN ONE CYCLE IS MERGED, DATA( ) CAN HOLD
C            EITHER THE DATA BEING ANALYZED OR THE ON-CYCLE DATA.
C
C            FATAL ERRORS, IER:
C               NONE.
C
C        DATA SET USE
C            KFILDO   - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C             DATA(K) = DATA POINTS (K=1,NSTA).  (INPUT)
C             LTAG(K) = DENOTES USE OF DATA IN DATA(K) FOR STATION K
C                       (K=1,NSTA).
C                       0 = USE DATA.
C                       1 = STATION OUTSIDE RADIUS OF INFLUENCE FOR
C                           AREA BEING ANALYZED OR MISSING DATUM.
C                       2 = STATION LOCATION UNKNOWN.
C                       (INPUT)
C               XP(K) = THE X POSITION FOR STATION K (K=1,NSTA) ON 
C                       THE GRID P( , ).  (INPUT)
C               YP(K) = THE Y POSITION FOR STATION K (K=1,NSTA) ON 
C                       THE GRID P( , ).  (INPUT)
C                NSTA = THE NUMBER OF DATA VALUES IN DATA( ), LTAG( ),
C                       XP( ), AND YP( ).  (INPUT)
C            P(IX,JY) = GRID (IX=1,NX) (JY=1,NY).  (INPUT/OUTPUT)
C         DIST(IX,JY) = WORK ARRAY (IX=1,NX) (JY=1,NY).  (INTERNAL)
C                  NX = NUMBER OF GRIDPOINTS IN THE XI (LEFT TO RIGHT)
C                       DIRECTION IN P( , ).  (INPUT)
C                  NY = NUMBER OF GRIDPOINTS IN THE YJ (BOTTOM TO TOP)
C                       DIRECTION IN P( , ).  (INPUT)
C               ISETP = FLAG TO INDICATE WHETHER AFTER THE LASS PASS
C                       A GRIDPOINT WILL BE SET TO THE CLOSEST
C                       STATION (=2), TO A VALUE IN THE DIRECTION
C                       OF THE STATION VALUE BUT NOT CROSS AN INTEGER
C                       BOUNDARY (=1), OR NOT (=0).  (THIS COULD BE
C                       PARTICULARIZED TO QUALITY OF DATA.)  (INPUT)
C                 IER = ERROR RETURN.
C                       0 = GOOD RETURN.
C                       (OUTPUT)
C        1         2         3         4         5         6         7 X
C
      DIMENSION DATA(NSTA),XP(NSTA),YP(NSTA),LTAG(NSTA)
      DIMENSION P(NX,NY),DIST(NX,NY)
C
      IER=0
C
      IF(ISETP.EQ.0)GO TO 200
C        NORMALLY, SETPNT WILL NOT BE ENTERED WHEN ISETP = 0.  THIS
C        IS A SAFETY CHECK.
C
C        INITIALIZE DIST( , ) WHICH IS A RECORD KEPT OF THE GRIDPOINTS
C        THAT HAVE BEEN MODIFIED AND THE DISTANCE FROM THE STATION
C        DONATING THE VALUE.
C
      DO 105 JY=1,NY
      DO 104 IX=1,NX
      DIST(IX,JY)=9999.
 104  CONTINUE
 105  CONTINUE
C
      DO 120  K=1,NSTA
C
CD     WRITE(KFILDO,107)K,LTAG(K)
CD107  FORMAT(/' STATION NO.',I6,'   LTAG( ) =',I4)
C
      IF(LTAG(K).EQ.0)THEN
C           THIS IS A DATA POINT TO USE.
C
         DX=999999.
         IX=XP(K)
         JY=YP(K)
C           IX,JY IS THE LL CORNER OF THE BOX BOUNDING DATA( ).
C           LOOK AT ALL FOUR CORNERS AROUND DATA( ).
C
         DO 110 I=IX,IX+1
         IF(I.LT.1.OR.I.GT.NX)GO TO 110
C           THE DATA POINT IS NOT WITHIN THE GRID.
C
         DO 109 J=JY,JY+1
         IF(J.LT.1.OR.J.GT.NY)GO TO 110
C           THE DATA POINT IS NOT WITHIN THE GRID.
C
         IF(P(I,J).NE.9999.)THEN
            D=(XP(K)-I)**2+(YP(K)-J)**2
C
            IF(D.LT.DX)THEN
               DX=D
               IXX=I
               JYY=J
            ENDIF
C
         ENDIF
C
 109     CONTINUE
C
 110     CONTINUE
C
         IF(DX.EQ.999999.)THEN
CD           WRITE(KFILDO,112)K
CD112        FORMAT(/' NO CLOSEST GRIDPOINT TO STATION K ='I7,
CD    1              '.  GRIDPOINT NOT SET.')
            GO TO 120
         ENDIF
C
         IF(DX.LT.DIST(IXX,JYY))THEN
            DIST(IXX,JYY)=DX
C
            IF(P(IXX,JYY).LT.DATA(K))THEN
C
C                 THIS SECTION FOR WHEN GRIDPOINT LT DATA POINT
C
               DL=DATA(K)-P(IXX,JYY)
CD              WRITE(KFILDO,115)IXX,JYY,P(IXX,JYY),K,DATA(K),DL
CD115           FORMAT(/' GRIDPOINT VALUE ',2I5,F8.2,'  DATA VALUE',
CD    1                 ' STATION NO.',I6,F8.2,
CD    2                 '  BEING REPLACED, DIFFERENCE ='F8.2)
C
               IF(ISETP.EQ.1)THEN
                  P(IXX,JYY)=MIN(INT(P(IXX,JYY)+.999)-.05,DATA(K))
               ELSE
                  P(IXX,JYY)=DATA(K)
               ENDIF
C
CD              WRITE(KFILDO,116)P(IXX,JYY)
CD116           FORMAT(' REPLACED VALUE            ',F8.2)
            ELSE
C
C                 THIS SECTION FOR WHEN GRIDPOINT GE DATA POINT
C
               IF(NINT(P(IXX,JYY)).EQ.9999)THEN
                  WRITE(KFILDO,1165)K,XP(K),YP(K),DATA(K),
     1                              IXX,JYY,P(IXX,JYY)
 1165             FORMAT(' AT 1165 IN SETPNT--K,XP(K),YP(K),DATA(K),
     1                              IXX,JYY,P(IXX,JYY)',
     2                              I6,3F8.2,2I6,F8.2)
               ENDIF
C
               DL=P(IXX,JYY)-DATA(K)
CD              WRITE(KFILDO,115)IXX,JYY,P(IXX,JYY),K,DATA(K),DL
C
               IF(ISETP.EQ.1)THEN
                  P(IXX,JYY)=MAX(INT(P(IXX,JYY))+.05,DATA(K))
               ELSE
                  P(IXX,JYY)=DATA(K)
               ENDIF
C
CD              WRITE(KFILDO,116)P(IXX,JYY)
            ENDIF
C
         ELSE
CD           WRITE(KFILDO,117)K,DATA(K),IXX,JYY,DATA(K)
CD117        FORMAT(' NOT USING STATION K =',I7' VALUE =',F7.2,
CD    1             ' AT GRIDPOINT IX,JY =', 2I7)         
         ENDIF
C
      ENDIF
C
 120  CONTINUE
C
 200  RETURN
      END
