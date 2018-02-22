      SUBROUTINE DIMENS(KFILDO,IDST,IDPARST,NVRBL,IDIMTB,IDIM,IBASE,
     1                  ISTOP,IER)
C
C        DECEMBER  2004   GLAHN   MDL   MOS-2000
C        DECEMBER  2004   GLAHN   SET N=2 VICE N=1 AT START; CHANGED
C                                 IDIMTB(IBASE)=J TO IDIMTB(IBASE+1)=J
C                                 IN TWO PLACES
C
C        PURPOSE
C            TO DETERMINE THE NUMBER OF "BASE" VARIABLES AND THE
C            NUMBER OF CATEGORIES EACH.
C
C            THE IDS IN IDST( , ) ARE COMPRISED OF:
C              1)  A VARIABLE THAT IDENTIFIES THE "BASE" VARIABLE.
C                  NOTHING HERE IS BASED ON IT.
C              2)  GROUPS OF STRATIFICATION VARIABLES, EACH GROUP
C                  IMMEDIATELY FOLLOWING THE PREVIOUS ONE.  THE IDS
C                  IN EACH GROUP ARE IDENTICAL EXCEPT FOR THE
C                  THRESHOLDS.  A GROUP NEEDS TO CONSIST OF 2 OR
C                  MORE IDS.
C   
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  SET BY
C                        DATA STATEMENT IN DRU175.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C           IDST(J,N) = THE INTEGER VARIABLE ID'S (J=1,4) (N=1,NVRBL).
C                       (INPUT)
C        IDPARST(J,N) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       VARIABLE IDST'S CORRESPONDING TO ID( ,N)
C                       (J=1,15), (N=1,NVRBL).
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                          0 = NOT BINARY,
C                          1 = CUMULATIVE FROM ABOVE, VALUES GE LOWER 
C                              THRESHOLD TRESHL = 1,
C                          2 = CUMULATIVE FROM BELOW, VALUES LT UPPER 
C                              THRESHOLD TRESHU = 1,
C                          3 = DISCRETE BINARY.  VALUES GE LOWER
C                              THRESHOLD AND LT UPPER THRESHOLD = 1,
C                          5 = GRID BINARY.  VALUES GE LOWER THRESHOLD.
C                          (INPUT)
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY
C                            1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK IN
C                            TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C                       (INPUT)
C               NVRBL = THE NUMBER OF VARIABLES.  THE NUMBER OF
C                       ENTRIES IN IDST( , ).  (INPUT)
C           IDIMTB(J) = THE NUMBER OF THRESHOLDS FOR EACH BASE
C                       VARIABLE, (J=2,NDIM) PLUS A DUMMY IDIMTB(1)=1.
C                       (OUTPUT)
C                IDIM = DIMENSION OF IDIMTB( ).  (INPUT)
C               IBASE = THE NUMBER OF BASE STRATIFICATION VARIABLES.
C                       THIS IS THE NUMBER OF UNIQUE ID'S, SANS THE
C                       THRESHOLDS.  (OUTPUT)
C               ISTOP = ISTOP IS INCREMENTED BY 1 EACH TIME AN ERROR
C                       OCCURS.  (INPUT/OUTPUT)
C                 IER = ERROR RETURN.
C                         0 = GOOD RETURN.
C                       777 = MORE THAN NDIM BASE VARIABLES.
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES USED 
C            NONE
C
      DIMENSION IDST(4,NVRBL),IDPARST(15,NVRBL)
      DIMENSION IDIMTB(IDIM)
C
C     IER=0
      IBASE=0 
      IDIMTB(1)=1
C        IDIMTB(1) IS A DUMMY AND NOT USED.  THE NUMBER OF CATEGORIES
C        OF BASE VARIABLES STARTS IN IDIMTB(2).  
C
D     WRITE(KFILDO,105)((IDST(J,N),J=1,4),(IDPARST(J,N),J=1,15),
D    1                  N=1,NVRBL)
D105  FORMAT(/' IN DIMENS--IDST( , ),IDPARST( , )',4I11,5X,15I4)
C
      N=2
C        SETTING N = 2 SKIPS THE "BASE" VARIABLE.
 109  J=1
 110  IF(J+N.LE.NVRBL)THEN
C
         IF(IDST(1,N).EQ.IDST(1,N+J).AND.
     1      IDST(2,N).EQ.IDST(2,N+J).AND.
     2      IDST(3,N).EQ.IDST(3,N+J).AND.
     3      IDPARST(13,N).EQ.IDPARST(13,N+J).AND.
     3      IDPARST(14,N).EQ.IDPARST(14,N+J).AND.
     3      IDPARST(15,N).EQ.IDPARST(15,N+J))THEN
C              NOTE THAT THE FULL ID SANS THE THRESHOLD IS CHECKED.
            J=J+1
            GO TO 110
         ELSE
C
            IF(J.EQ.1)THEN
               WRITE(KFILDO,111)
 111           FORMAT(/,' ****ONLY ONE STRATIFICATION VARIABLE IN',
     1                  ' A GROUP IN DIMENS AT 111.  FATAL ERROR.')
               ISTOP=ISTOP+1
               GO TO 200
            ELSE
C
               IF(IBASE+1.LT.IDIM)THEN
                  IBASE=IBASE+1
                  IDIMTB(IBASE+1)=J
               ELSE
                  WRITE(KFILDO,112)
 112              FORMAT(/,' ****MORE THAN IDIM BASE (STRATIFICATION)',
     1                     ' VARIABLES IN DIMENS AT 112.  FATAL ERROR.')
                  ISTOP=ISTOP+1
                  GO TO 200
               ENDIF
C
            ENDIF
C
         ENDIF
C      
      ELSE 
C
         IF(J.EQ.1)THEN
            WRITE(KFILDO,113)
 113        FORMAT(/,' ****ONLY ONE STRATIFICATION VARIABLE IN',
     1               ' A GROUP IN DIMENS AT 113.  FATAL ERROR.')
            ISTOP=ISTOP+1
            GO TO 200
         ELSE
C
            IF(IBASE+1.LT.IDIM)THEN
               IBASE=IBASE+1
               IDIMTB(IBASE+1)=J
            ELSE
               WRITE(KFILDO,114)
 114           FORMAT(/,' ****MORE THAN IDIM BASE (STRATIFICATION)',
     1                  ' VARIABLES IN DIMENS AT 114.  FATAL ERROR.')
               ISTOP=ISTOP+1
               GO TO 200
            ENDIF
C
         ENDIF
C
      ENDIF
C 
      N=N+J
      IF(N+1.LE.NVRBL)GO TO 109
C
D     WRITE(KFILDO,198)N,J,NVRBL,IBASE,(IDIMTB(JJ),JJ=1,IDIM)
D198  FORMAT(/' IN DIMENS AT 198--N,J,NVRBL,IBASE,IDIMTB( )',4I4,/,20I4)
C
  200 RETURN
      END
