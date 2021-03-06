      SUBROUTINE DENS(KFILDO,CCALL,XP,YP,LTAG,LNDSEA,XDATA,NSTA,K,R,
     1                DEN1,LIO,IER)
C
C        NOVEMBER  2007   GLAHN   MDL
C        NOVEMBER  2007   GLAHN   DOES NOT COUNT STATION ITSELF;
C                                 ADDED CCALL( ) TO CALL AND PRINT
C        DECEMBER  2007   GLAHN   MODIFIED FOR LIO AND DIFFERENT
C                                 DATA TYPES; MADE CHECK ON 
C                                 XDATA( ).GT.999.5
C        DECEMBER  2007   GLAHN   MODIFIED TESTS FOR LIO ABOVE 
C                                 DISTSQ= (12/26/07)
C
C        PURPOSE
C            TO COMPUTE A STATION DENSITY VARIABLE = THE NUMBER OF
C            STATIONS OF CORRECT TYPE WITH NON-MISSING DATA WITHIN
C            THE RADIUS R.  LIO CAN BE OF TYPE 0 OR 9.
C            FOR TYPE LIO = 0, USE STATION TYPES 0 AND 3;
C                         = 9, USE STATION TYPES 6 AND 9.
C
C        DATA SET USE
C            KFILDO   - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C          INPUT
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C            CCALL(K) = CALL LETTERS OF STATIONS (K=1,NSTA).
C                       (CHARACTER*8)  (INPUT)
C               XP(J) = X-POSITION OF STATION J ON GRID (J=1,NSTA).
C               YP(J) = Y-POSITION OF STATION J ON GRID (J=1,NSTA).
C             LTAG(J) = DON'T USE THIS STATION IF LTAG( ) GT 0
C                       (J=1,NSTA).
C           LNDSEA(J) = LAND/SEA INFLUENCE FLAG FOR EACH STATION
C                       (J=1,ND1).
C                       0 = WILL BE USED FOR ONLY OCEAN WATER (=0)
C                           GRIDPOINTS.
C                       3 = WILL BE USED FOR ONLY INLAND WATER (=3)
C                           GRIDPOINTS.
C                       6 = WILL BE USED FOR BOTH INLAND WATER (=3)
C                           AND LAND (=9) GRIDPOINTS.
C                       9 = WILL BE USED FOR ONLY LAND (=9) GRIDPOINTS.
C            XDATA(J) = THE DATA FOR WHICH TO FIND THE DENSITY
C                       (J=1,ND1).
C                NSTA = NUMBER OF STATIONS IN LIST. 
C                   K = POSITION OF STATION IN LIST FOR WHICH CLOSEST
C                       STATIONS ARE NEEDED. 
C                   R = RADIUS OVER WHICH TO SEARCH.
C                 LIO = THE VARIABLE TO SPECIFY WHETHER THIS RUN IS
C                       FOR LAND (=9), INLAND WATER (=3), OR 
C                       OCEAN (=0) POINTS. 
C
C          OUTPUT
C                DEN1 = THE DENSITY VARIABLE (SEE PURPOSE).
C                 IER = STATUS RETURN
C                         0 = GOOD.
C                       777 = CAN'T FIND A STATION WITHIN DISTANCE R.
C
C          INTERNAL
C              DISTSQ = DISTANCE (IN GRID UNITS) SQUARED BETWEEN
C                       TWO STATIONS.
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED
C            NONE.
C
      CHARACTER*8 CCALL(NSTA)
C
      DIMENSION XP(NSTA),YP(NSTA),LTAG(NSTA),LNDSEA(NSTA),XDATA(NSTA)
C
CD     CALL TIMPR(KFILDO,KFILDO,'START DENS          ')
C
      IER=0
      RSQ=R*R+.01
C        THE SMALL CONSTANT IS ADDED TO ASSURE A POINT IS NOT 
C        ELIMINATED BECAUSE OF ROUNDOFF.
      NCOUNT=0
      DEN1=0.
C
      DO 162 L=1,NSTA
      IF(XDATA(L).GT.9998.5)GO TO 162
C        DO NOT USE STATON WITH MISSING DATA.
C
      IF(L.EQ.K)GO TO 162
C        DO NOT COUNT STATION ITSELF.
C
      IF(ABS(XP(K)-XP(L)).GT.R)GO TO 162
      IF(ABS(YP(K)-YP(L)).GT.R)GO TO 162
C        IN A LONG LIST OF STATIONS, THE ABOVE TWO TESTS SHOULD BE
C        MORE EFFICIENT THAN ALWAYS CALCULATING THE DISTANCE.
C        ALSO, THEY SHOULD RULE OUT MORE THAN THE TWO FOLLOWING
C        TESTS.
C
      IF(LTAG(L).GT.0)GO TO 162
C
      IF(LIO.EQ.9.AND.LNDSEA(K).LT.6)GO TO 162
      IF(LIO.EQ.0.AND.LNDSEA(K).GT.3)GO TO 162
C        THESE TESTS USE LAND STATIONS OR JOINT LAND/INLAND WATER
C        STATIONS FOR LIO=9 (TO BE USED FOR LAND GRIDPOINTS),
C        AND BOTH OCEAN AND INLAND WATER WHEN LIO = 0 (TO BE USED
C        FOR ALL WATER GRIDPOINTS).
C
      DISTSQ=(XP(K)-XP(L))**2+(YP(K)-YP(L))**2
C
      IF(DISTSQ.LT.RSQ)THEN
         NCOUNT=NCOUNT+1
C
CD        WRITE(KFILDO,150)K,CCALL(K),L,CCALL(L),
CD    1                    XP(K),XP(L),YP(K),YP(L),NCOUNT
CD150     FORMAT(/,'AT 150 IN DENS--K,CCALL(K),L,CCALL(L),',
CD    2            'XP(K),XP(L),YP(K),YP(L),NCOUNT',
CD    3             I6,2X,A8,I6,2X,A8,4F7.1,I4)
C
      ENDIF
      
 162  CONTINUE
C
      DEN1=NCOUNT
CD     CALL TIMPR(KFILDO,KFILDO,'END   DENS          ')
C
      RETURN
      END
