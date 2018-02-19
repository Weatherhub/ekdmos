      SUBROUTINE NEWXY(KFILDO,MESHIN,XPIN,YPIN,MESHOUT,XP,YP,NSTA)
C
C        AUGUST 2000   GLAHN   TDL   LAMP-2000
C
C        PURPOSE
C           TO CALCULTE X AND Y POSITIONS OF THE STATIONS IN
C           XP( ) AND YP( ) IN TERMS OF THE GRID TO BE USED.
C   
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES 
C
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              MESHIN = THE MESH LENGTH FOR THE INPUT LOCATIONS XPIN( )
C                       AND YPIN( ).  (INPUT)
C             XPIN(K) = X POSITIONS OF THE STATIONS AT MESH LENGTH MESHIN
C                       (K=1,NSTA).  (INPUT)
C             YPIN(K) = Y POSITIONS OF THE STATIONS AT MESH LENGTH MESHIN
C                       (K=1,NSTA).  (INPUT)
C             MESHOUT = THE MESH LENGTH FOR THE OUTPUT XP( ) AND YP( ).
C                       (INPUT)
C               XP(K) = X POSITIONS OF THE STATIONS AT MESH LENGTH MESHOUT
C                       (K=1,NSTA).  (OUTPUT)
C               YP(K) = Y POSITIONS OF THE STATIONS AT MESH LENGTH MESHOUT
C                       (K=1,NSTA).  (OUTPUT)
C                NSTA = NUMBER OF VALUES IN XPIN( ), YPIN( ), XP( ),
C                       AND YP( ).  (INPUT)
C
C        NONSYSTEM SUBROUTINES USED 
C            NONE
C
      DIMENSION XPIN(NSTA),YPIN(NSTA),XP(NSTA),YP(NSTA)
C
      IF(MESHIN.EQ.MESHOUT)THEN
C
         DO 110 K=1,NSTA
            XP(K)=XPIN(K)
            YP(K)=YPIN(K)
 110     CONTINUE
C
      ELSE
C
         RATIO=FLOAT(MESHIN)/MESHOUT
C
         DO 120 K=1,NSTA
C
            IF(XPIN(K).NE.9999.)THEN
               XP(K)=(XPIN(K)-1.)*RATIO+1.
               YP(K)=(YPIN(K)-1.)*RATIO+1.
            ELSE
               XP(K)=9999.
               YP(K)=9999.
            ENDIF
C
 120     CONTINUE
C
      ENDIF
C
      RETURN
      END
