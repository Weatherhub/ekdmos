      SUBROUTINE FLTAGM(KFILDO,XDATA,ND1,NORUNS,XP,YP,LTAG,NSTA,
     1                  NX,NY,RMAX)
C
C        APRIL     2006   GLAHN   MOS-2000
C                                 MODIFIED FOR U155 FROM LAMP fltag.f
C        JUNE      2007   GLAHN   MODIFIED FOR PROBABILITY LEVELS
C
C        PURPOSE
C            TO SET LTAG( ) FOR U405A ROUTINE.  LTAG(K) IS SET TO 0
C            IF DATUM FROM ANY XDATA(K,L) (L=2,NORUNS) CAN BE USED FOR
C            STATION K AND THE STATION IS WITHIN THE AREA TO
C            BE USED ACCORDING TO RMAX.  OTHERWISE, LTAG(K) IS
C            SET TO 2 WHEN THE LOCATION IS MISSING OR TO A 1 IF
C            THE STATION IS OUTSIDE THE AREA.
C
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C          XDATA(K,L) = DATA FOR STATION K (K=1,NSTA) (L=2,NORUNS+1).
C                       9999 SIGNIFIES MISSING.  (INPUT)
C                 ND1 = FIRST DIMENSION OF XDATA( , ).  (INPUT)
C              NORUNS = THE NUMBER OF SETS OF DATA IN XDATA( , ).
C                       (INPUT)
C               XP(K) = HORIZONTAL GRID POSITION OF DATUM XDATA(K,L)
C                       (K=1,NSTA).  (INPUT)
C               YP(K) = VERTICAL GRID POSITION OF DATUM XDATA(K,L)
C                       (K=1,NSTA).  (INPUT)
C             LTAG(K) = DENOTES USE OF DATA IN XDATA(K,L) FOR STATION K
C                       (K=1,NSTA).
C                       0 = USE DATA.
C                       1 = STATION OUTSIDE RADIUS OF INFLUENCE FOR
C                           AREA BEING ANALYZED OR MISSING DATUM.
C                       2 = STATION LOCATION UNKNOWN.
C                       (OUTPUT)
C                NSTA = NUMBER OF ENTRIES IN XDATA( , ).  (INPUT)
C                  NX = NUMBER OF GRIDPOINTS IN THE XI (LEFT TO RIGHT)
C                       DIRECTION.  (INPUT)
C                  NY = NUMBER OF GRIDPOINTS IN THE YJ (BOTTOM TO TOP)
C                       DIRECTION.  (INPUT)
C                RMAX = THE MAXIMUM DISTANCE IN TERMS OF GRIDLENGTHS
C                       DATUM OUTSIDE THE GRID WILL BE USED.  NOTE
C                       THE ACTUAL DISTANCE DOES DEPEND ON THE
C                       GRIDLENGTH.  RMAX = THE MAXIMUM R*RSTAR
C                       IN THE CALLING PROGRAM.  (INPUT)
C               RMAXX = MAXIMUM XI GRID POSITION OF STATIONS TO BE
C                       USED ON THIS PASS.  (INTERNAL)
C               RMAXY = MAXIMUM YJ GRID POSITION OF STATIONS TO BE
C                       USED ON THIS PASS.  (INTERNAL)
C              RMINXY = MINIMUM XI AND YJ GRID POSITION OF STATIONS
C                       TO BE USED ON PASS ANALYSIS.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED
C            NONE
C
      DIMENSION XDATA(ND1,NORUNS+1),LTAG(NSTA),XP(NSTA),YP(NSTA)
C
C***D        WRITE(KFILDO,100)(XDATA(J,1),XP(J),YP(J),J=1,NSTA)
C***D100     FORMAT(/' IN FLTAG AT 100--XDATA(J,1),XP(J),YP(J),J=1,NSTA'/
C***D    1         (' ',3F8.2))
C
      RMINXY=1.-RMAX
      RMAXX=NX+RMAX
      RMAXY=NY+RMAX
C
      DO 150 K=1,NSTA
C
         LTAG(K)=0
C
         DO 135 L=2,NORUNS+1
C
         IF(XDATA(K,L).NE.9999.)THEN
C              IF ANY ONE VALUE IS PRESENT, DON'T MARK TO NOT USE.
            GO TO 136
         ENDIF
C
 135     CONTINUE
C  
         LTAG(K)=1
C           ALL DATA VALUES ARE MISSING.
         GO TO 150
C
 136     IF(XP(K).EQ.9999.)THEN
C              STATION POSITION MISSING.
            LTAG(K)=2
         ELSE
C
	    IF((XP(K).LT.RMINXY).OR.
     1         (XP(K).GT.RMAXX).OR.
     2         (YP(K).LT.RMINXY).OR.
     3         (YP(K).GT.RMAXY))THEN
               LTAG(K)=1
            ENDIF
C
         ENDIF
C
C***D      DO 149 L=2,NORUNS
C***D      WRITE(KFILDO,140)K,RMAX,RMINXY,RMAXX,RMAXY,
C***D    1                 XP(K),YP(K),XDATA(K,L),LTAG(K)
C***D140  FORMAT(' K,RMAX,RMINXY,RMAXX,RMAXY,',
C***D    1       'XP,YP,XDATA,LTAG',I5,7F8.2,I3)
C***D149  CONTINUE
C
 150  CONTINUE
C
      RETURN
      END
