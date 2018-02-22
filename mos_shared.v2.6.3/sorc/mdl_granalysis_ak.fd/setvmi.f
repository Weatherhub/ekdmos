      SUBROUTINE SETVMI(KFILDO,P,DSAV,NX,NY,
     1                  CCALL,NAME,XP,YP,XDATA,NSTA,IER)
C
C        MARCH     2008   GLAHN   TDL   MOS-2000
C        MARCH     2008   GLAHN   OMITTED SETTING LAST CATEGORY
C
C        PURPOSE
C            TO MAKE SURE THE CLOSEST GRIDPOINT IN P( , ) TO A
C            STATION HAS A VALUE WITHIN THE RANGE OF THE CATEGORY
C            IN XDATA( ).  ITABLE( , ) IS FOR VISIBILITY.
C
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C            P(IX,JY) = GRID OF VISIBILITIES IN MI (IX=1,NX) (JY=1,NY).
C                       (INPUT/OUTPUT)
C         DSAV(IX,JY) = WORK ARRAY (IX=1,NX) (JY=1,NY).  (INTERNAL)
C                  NX = X EXTENT OF GRID IN P( , ).  (INPUT)
C                  NY = Y EXTENT OF GRID IN P( , ).  (INPUT)
C            CCALL(K) = CALL LETTERS OF STATION K (K=1,NSTA).  
C                       (CHARACTER*8)  (INPUT)
C             NAME(K) = NAME OF STATION K (K=1,NSTA).  
C                       (CHARACTER*8)  (INPUT)
C               XP(K) = THE X POSITION FOR STATION K (K=1,NSTA) ON 
C                       THE GRID IN P( , ).  (INPUT)
C               YP(K) = THE Y POSITION FOR STATION K (K=1,NSTA) ON 
C                       THE GRID IN P( , ).  (INPUT)
C            XDATA(K) = CATEGORICAL VISIBILITY FORECASTS (K=1,NVAL).
C                       (INPUT)
C                NSTA = THE NUMBER OF VALUES IN XDATA( ) BEING DEALT
C                       WITH.  (INPUT)
C                 IER = ERROR RETURN.
C                       0 = GOOD RETURN.
C                       (OUTPUT)
C          TABLE(J,M) = HOLDS THE LOWER AND UPPER CATEGORY BREAKPOINTS
C                       FOR THE NOCAT CATEGORIES OF VISIBILITIES
C                       (M=1,NOCAT), (J=1,2).  (INTERNAL)
C               NOCAT = THE NUMBER OF VISIBILITY CATEGORIES FORECAST
C                       BY LAMP.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES USED 
C            NONE
C
      PARAMETER(NOCAT=7)
C
      CHARACTER*8 CCALL(NSTA)
      CHARACTER*20 NAME(NSTA)
C
      DIMENSION XDATA(NSTA),XP(NSTA),YP(NSTA)
      DIMENSION P(NX,NY),DSAV(NX,NY)
      DIMENSION TABLE(2,NOCAT)
C
      DATA TABLE/ .0,    .495,
     1           .495,   .95,
     2           .95,   1.95,
     3          1.95,   2.95,
     4          2.95,   5.05,
     5          5.05,   6.05,
     6          6.05,   8.0/
C
CD     CALL TIMPR(KFILDO,KFILDO,'START SETVMI        ')
      IER=0
      ICOUNT=0
C
C        INITIALIZE DSAV( , ).
C
      DO 110 JY=1,NY
      DO 109 IX=1,NX
      DSAV(IX,JY)=9999.
 109  CONTINUE
 110  CONTINUE
C
      DO 200 K=1,NSTA
      IF(XDATA(K).GT.9998.9)GO TO 200
      NCAT=NINT(XDATA(K))
C        THE VALUES IN XDATA( ) ARE STATION CATEGORY VALUES TO
C        WHOLE NUMBERS.
      DMIN=9999.
      IX=XP(K)
      JY=YP(K)
C
      DO 120 M=0,1
      DO 119 N=0,1
      D=(XP(K)-(IX+N))**2+(YP(K)-(JY+M))**2
C
      IF(D.LT.DMIN)THEN
         DMIN=D
         NSAV=N
         MSAV=M
      ENDIF
C
 119  CONTINUE
 120  CONTINUE
C
      IX=IX+NSAV
      JY=JY+MSAV
      D=(XP(K)-(IX))**2+(YP(K)-(JY))**2
C
      IF(D.LT.DSAV(IX,JY))THEN
C
         IF(P(IX,JY).LT.TABLE(1,NCAT))THEN
C         
CD           IF(DSAV(IX,JY).GT.9998.)THEN         
C         
CD              WRITE(KFILDO,125)CCALL(K),NAME(K),NCAT,P(IX,JY),
CD    1                          TABLE(1,NCAT)
CD125           FORMAT(' STATION 'A8,1X,A20,' OF',I3, ' ADJUSTED',
CD    1                ' CLOSEST GRIDPOINT VALUE OF 'F8.2,'  TO ',F7.2,
CD    2                ' PLUS 0.1')
CD           ELSE
CD              WRITE(KFILDO,126)CCALL(K),NAME(K),NCAT,P(IX,JY),
CD    1                          TABLE(1,NCAT)
CD126           FORMAT(' STATION 'A8,1X,A20,' OF',I3, ' ADJUSTED',
CD    1                ' CLOSEST GRIDPOINT VALUE OF 'F8.2,'  TO ',F7.2,
CD    2                ' PLUS 0.1.  SECOND ADJUSTMENT.')
CD           ENDIF
C
            P(IX,JY)=TABLE(1,NCAT)+.1
C              THE .1 IS TO MAKE SURE THE VALUE IS SOLIDLY
C              WITHIN RANGE.  IT HAS TO BE BIG ENOUGH TO SURVIVE
C              PACKING TO TENTHS OF MI, THE UNITS IN P( , ).
            ICOUNT=ICOUNT+1
C
         ELSEIF(P(IX,JY).GT.TABLE(2,NCAT).AND.NCAT.LT.NOCAT)THEN
C           DO NOT SET THE LAST CATEGORY TO A SPECIFIC VALUE,
C           BECAUSE THERE IS NO CATEGORY ABOVE IT AND THE VALUE
C           WOULD BE ARBITRARY.
C         
CD           IF(DSAV(IX,JY).GT.9998.)THEN         
CD              WRITE(KFILDO,127)CCALL(K),NAME(K),NCAT,P(IX,JY),
CD    1                          TABLE(2,NCAT)
CD127           FORMAT(' STATION 'A8,1X,A20,' OF',I3, ' ADJUSTED',
CD    2                ' CLOSEST GRIDPOINT VALUE OF 'F8.2,'  TO ',F7.2,
CD    3                ' MINUS 0.1')
CD           ELSE
CD              WRITE(KFILDO,128)CCALL(K),NAME(K),NCAT,P(IX,JY),
CD    1                          TABLE(2,NCAT)
CD128           FORMAT(' STATION 'A8,1X,A20,' OF',I3, ' ADJUSTED',
CD    1                ' CLOSEST GRIDPOINT VALUE OF 'F8.2,'  TO ',F7.2,
CD    2                ' MINUS 0.1.  SECOND ADJUSTMENT.')
CD           ENDIF
C
            P(IX,JY)=TABLE(2,NCAT)-.1
C              THE .1 IS TO MAKE SURE THE VALUE IS SOLIDLY
C              WITHIN RANGE.  IT HAS TO BE BIG ENOUGH TO SURVIVE
C              PACKING TO TENTHS OF MI, THE UNITS IN P( , ).
            ICOUNT=ICOUNT+1
            DSAV(IX,JY)=D
         ENDIF
C
      ENDIF
C
 200  CONTINUE
C
      IF(ICOUNT.GT.0)THEN
         WRITE(KFILDO,205)ICOUNT
 205     FORMAT(/,I6,' VALUES IN THE VISIBILITY GRID MODIFIED',
     1           ' BY THE STATION VALUE AFTER THE ANALYSIS.')
      ENDIF
C      
      RETURN
      END
