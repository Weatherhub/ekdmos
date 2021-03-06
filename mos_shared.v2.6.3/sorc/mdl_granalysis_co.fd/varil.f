      SUBROUTINE VARIL(KFILDO,CCALL,XP,YP,LTAG,LNDSEA,ELEV,XLAPSE,
     1                 XDATA,NSTA,K,R,VAR1,LIO,IER)
C
C        FEBRUARY  2008   GLAHN   MDL
C                                 CHANGED NAME FROM VARIW
C        FEBRUARY  2008   GLAHN   PURPOSE REWORDED TO INCLUDE DISTANCE
C                                 WEIGHTING
C        FEBRUARY  2009   GLAHN   ADDED TEST ON LNDSEA(L) BELOW D 162
C                                 PULLED TEST ON LNDSEA(K)
C
C        PURPOSE
C            TO COMPUTE A VARIABILITY VARIABLE WITHIN RADIUS R =
C            THE DISTANCE WEIGHTED MEAN ABSOLUTE DIFFERENCE BETWEEN
C            ALL STATION VALUES WITHIN THE RADIUS R AND THE AVERAGE
C            OF THOSE SAME VALUES, AFTER THOSE VALUES HAVE BEEN
C            ADJUSTED BY THEIR INDIVIDUAL LAPSE RATES.  THIS SOMEWHAT
C            PARALLELS THE ANALYSIS PROCESS IN U155. 
C
C            LIO CAN BE OF TYPE 0 OR 9.
C            FOR TYPE LIO = 0, USE STATION TYPES 0 AND 3;
C                         = 9, USE STATION TYPES 6 AND 9.
C            NOTE:  WHEN THERE IS ONLY ONE STATION OR NONE WITHIN
C                   THE RADIUS, THE RETURNED VALUE VAR1 = 9999.  THIS
C                   IS MODIFIED IN WTHOL2.
C
C        DATA SET USE
C            KFILDO   - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C          INPUT
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C            CCALL(J) = CALL LETTERS OF STATIONS (J=1,NSTA). USED
C                       ONLY FOR DIAGNOSTICS.  (CHARACTER*8)  (INPUT)
C               XP(J) = X-POSITION OF STATION K ON GRID (J=1,NSTA).
C               YP(J) = Y-POSITION OF STATION K ON GRID (J=1,NSTA).
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
C             ELEV(J) = ELEVATION OF STATIONS (J=1,NSTA).  (INPUT)
C           XLAPSE(J) = CALCULATED LAPSE RATE IN UNITS OF THE VARIABLE
C                       BEING ANALYZED PER M. (J=1,KSTA).  (INPUT)
C            XDATA(J) = THE DATA FOR WHICH TO FIND THE VARIABILITY
C                       (J=1,ND1).
C                NSTA = NUMBER OF STATIONS IN LIST. 
C                   K = POSITION OF STATION IN LIST FOR WHICH THE
C                       VARIABILITY IS DESIRED.
C                   R = RADIUS OVER WHICH TO SEARCH.
C                 LIO = THE VARIABLE TO SPECIFY WHETHER THIS RUN IS
C                       FOR LAND (=9), INLAND WATER (=3), OR 
C                       OCEAN (=0) POINTS. 
C
C          OUTPUT
C                VAR1 = THE VARIABILITY VARIABLE (SEE PURPOSE).
C                 IER = STATUS RETURN
C                         0 = GOOD.
C
C          INTERNAL
C              DISTSQ = DISTANCE (IN GRID UNITS) SQUARED BETWEEN
C                       TWO STATIONS.
C            TEMPX(J) = AN ARRAY TO HOLD ESTIMATES OF THE VALUE AT
C                       WITHHELD STATION K BASED ON THE VALUE AT EACH
C                       NEIGHBORING STATION, ADJUSTED BY THE NEIGHBOR'S
C                       LAPSE RATE AND VERTICAL SEPARATION BETWEEN
C                       THE TWO STATIONS (J=1,NSTA).
C                       (AUTOMATIC)  (INTERNAL)
C            TEMPW(J) = AN ARRAY TO HOLD QUADRATIC WEIGHTS COMPUTED
C                       FROM THE HORIZONTAL DISTANCES BETWEEN THE 
C                       STATIONS AND THE RADIUS OF SEARCH (THE SAME
C                       WEIGHTING USED IN THE CORRECTION ALGORITHM)
C                       (J=1,NSTA).  (AUTOMATIC)  (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED
C            NONE.
C
      CHARACTER*8 CCALL(NSTA)
C
      DIMENSION XP(NSTA),YP(NSTA),LTAG(NSTA),LNDSEA(NSTA),XDATA(NSTA),
     1          ELEV(NSTA),XLAPSE(NSTA)
      DIMENSION TEMPX(NSTA),TEMPW(NSTA)
C        TEMPX( ) AND TEMPW( ) ARE AUTOMATIC ARRAYS LARGE ENOUGH TO
C        HOLD THE MAXIMUM NUMBER OF POINTS SURROUNDING A STATION
C        WITHIN ITS RADIUS OF SEARCH.
C
CD     CALL TIMPR(KFILDO,KFILDO,'START VARIL         ')
C
      IER=0
      NCOUNT=0
      VAR=0.
      RSQ=R*R+.01
C        THE SMALL CONSTANT IS ADDED TO ASSURE A POINT IS NOT 
C        ELIMINATED BECAUSE OF ROUNDOFF.
      VAR1=0.
C
      DO 162 L=1,NSTA
C
CD     IF(CCALL(L).EQ.'KJBR    ')THEN
CD        WRITE(KFILDO,148)K,CCALL(K),L,CCALL(L),
CD    1                    LTAG(L),LNDSEA(L),R,XDATA(L),LIO
CD148     FORMAT(/,'  AT 148 IN VARI--K,CCALL(K),L,CCALL(L),',
CD    1            'LTAG(L),LNDSEA(L),R,XDATA(L),LIO',
CD    1            I6,2X,A8,I6,2X,A8,2I4,2F8.1,I3)
CD     ENDIF
C
C        DO CALCULATIONS ONLY FOR CORRECT TYPE OF STATION.
C
      IF(LIO.EQ.9.AND.LNDSEA(L).LT.6)GO TO 162
      IF(LIO.EQ.0.AND.LNDSEA(L).GT.3)GO TO 162
C
      IF(XDATA(L).GT.9998.5)GO TO 162
C        DO NOT USE STATION WITH MISSING DATA.
C        
      IF(L.EQ.K)GO TO 162
C        DO NOT USE STATION ITSELF.  IN IMPLEMENTATION, THE
C        VARIABILIY AROUND A GRIDPOINT IS COMPUTED, AND A "STATION"
C        VALUE IS NOT AVAILABLE.
C
      IF(ABS(XP(K)-XP(L)).GT.R)GO TO 162
      IF(ABS(YP(K)-YP(L)).GT.R)GO TO 162
C        IN A LONG LIST OF STATIONS, THE ABOVE TWO TESTS SHOULD BE
C        MORE EFFICIENT THAN ALWAYS CALCULATING THE DISTANCE.
C        ALSO, THEY SHOULD RULE OUT MORE THAN THE TWO FOLLOWING
C        TESTS.
      IF(LTAG(L).GT.0)GO TO 162
C
      DISTSQ=(XP(K)-XP(L))**2+(YP(K)-YP(L))**2
C
CD     IF(CCALL(L).EQ.'KJBR    ')THEN
CD        WRITE(KFILDO,149)K,CCALL(K),L,CCALL(L),DISTSQ,RSQ
CD149     FORMAT(/,'  AT 149 IN VARI--K,CCALL(K),L,CCALL(L),DISTSQ,RSQ',
CD    1            I6,2X,A8,I6,2X,A8,2F8.1)
CD     ENDIF
C
      IF(DISTSQ.LT.RSQ)THEN
         NCOUNT=NCOUNT+1
         TEMPW(NCOUNT)=(RSQ-DISTSQ)/(RSQ+DISTSQ)
C           WT CANNOT BE NEGATIVE, BECAUSE DISTSQ LT RSQ.
         TEMPX(NCOUNT)=XDATA(L)+XLAPSE(L)*(ELEV(K)-ELEV(L))
C           TEMPX(NCOUNT) IS THE VALUE IMPLIED BY STATION L AT
C           THE WITHHELD STATION K.
         VAR=VAR+TEMPX(NCOUNT)
C
CD        WRITE(KFILDO,150)K,CCALL(K),L,CCALL(L),
CD    1                    XP(K),XP(L),YP(K),YP(L),
CD    2                    XDATA(L),XDATA(K),XLAPSE(L),ELEV(L),ELEV(K),
CD    3                    TEMPX(NCOUNT),TEMPW(NCOUNT),VAR,NCOUNT
CD150     FORMAT(/,'AT 150,VARIL--K,CCALL(K),L,CCALL(L),',
CD    1      'XP(K),XP(L),YP(K),YP(L),',
CD    2      'XDATA(L),XDATA(K),XLAPSE(L),ELEV(L),ELEV(K),',
CD    3      'TEMPX( ),TEMPW( ),VAR,NCOUNT',/,
CD    4       I6,2X,A8,I6,2X,A8,4F7.1,2F8.1,F8.4,
CD    5       1X,2F8.1,F8.2,F10.3,F8.1,I4)
C
      ENDIF
      
 162  CONTINUE
C
      IF(NCOUNT.GT.1)THEN
C           WHEN NCOUNT = 1, THERE WILL BE ZERO VARIABILITY.
C           THIS DOES NOT GIVE A GOOD PICTURE, SO SET TO 9999.
         AVG=VAR/NCOUNT
      ELSE
         WRITE(KFILDO,170)R,CCALL(K)
 170     FORMAT(/,' ####CANNOT FIND TWO STATIONS TO AVERAGE IN VARIL',
     1            '  WITHIN RADIUS R =',F6.1,' FOR STATION  ',A8,
     2            'SET VAR1=9999.  PROCEEDING.')
         VAR1=9999.
         GO TO 300    
      ENDIF
C
      DO 262 L=1,NCOUNT
C
      VAR1=VAR1+ABS(TEMPX(L)-AVG)*TEMPW(L)
C
CD     WRITE(KFILDO,260)TEMPX(L),TEMPW(L),AVG,VAR1
CD260  FORMAT(/' AT 260 IN VARIL--TEMPX(L),TEMPW(L),AVG,VAR1',
CD    1        F8.2,F8.3,2F8.3)
C
 262  CONTINUE
C
      VAR1=VAR1/NCOUNT
C
CD     WRITE(KFILDO,265)VAR1,NCOUNT
CD265  FORMAT(/' AT 265 IN VARIL--VAR1,NCOUNT',F8.2,I4)
CD     CALL TIMPR(KFILDO,KFILDO,'END   VARIL         ')
C
 300  RETURN
      END
