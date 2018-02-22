      SUBROUTINE QPFCUB(KFILDO,ID,IDPARS,JD,DATA,NSTA,ISTAV,IER)
C
C        JULY        2007    GLAHN   TDL   MOS-2000 
C
C        PURPOSE 
C            SPECIFICALLY FOR PROCESSING QPF DATA THAT ARE 
C            THE CUBE ROOT OF AMOUNTS (IN INCHES).  QPFCUB TAKES
C            THE 3RD POWER OF THE VALUES AND SETS NEGATIVES TO ZERO.
C            IDPARS(8) = 4 IS NECESSARY FOR QPFCUB TO BE ENTERED.
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                     (OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT) 
C               ID(J) = THE FOUR WORD ID (J=1,4).  NOT CURRENTLY
C                       USED.  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       PREDICTOR ID CORRESPONDING TO ID( ) (J=1,15).
C                       (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY
C                            1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK
C                            IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE
C                       PORTIONS PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       JD( ) IS USED TO IDENTIFY THE BASIC MODEL 
C                       FIELDS.
C             DATA(K) = CUBE ROOT OF QPF DATA ON INPUT.  THIRD POWER OF
C                       DATA ON OUTPUT WITH NEGATIVES SET TO MISSING.
C                       (INPUT/OUTPUT)
C                NSTA = NUMBER OF STATIONS.  USED AS THE DIMENSION 
C                       OF DATA( ).  (INPUT)
C               ISTAV = 1 SINCE THE DATA RETURNED ARE STATION DATA.
C                       (OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       103 = IDPARS(8) DOES NOT INDICATE
C                             TRANSFORMATION EXPECTED.
C                             DATA ARE RETURNED AS MISSING.
C                       (OUTPUT)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            NONE
C
      DIMENSION DATA(NSTA)
      DIMENSION ID(4),JD(4),IDPARS(15)
C
D     CALL TIMPR(KFILDO,KFILDO,'START QPFCUB        ')
      IER=0 
      ISTAV=1
C
      IF(IDPARS(8).EQ.4)GO TO 105
      WRITE(KFILDO,101)(JD(J),J=1,4)
 101  FORMAT(/,'****IDPARS(8) DOES NOT INDICATE DATA TO BE',
     1         ' TRANSFORMED',
     2       /,'     PREDICTOR ',I9.9,1X,I9.9,1X,I9.9,I3,
     3        ' NOT COMPUTED IN QPFCUB.')
      IER=103
      GO TO 800
C
 105  DO 220 K=1,NSTA
C
      IF(DATA(K).NE.9999.)THEN
C
         IF(DATA(K).LE.0.)THEN
            DATA(K)=0.
         ELSE
            DATA(K)=DATA(K)**3
         ENDIF
C
      ENDIF
C
 220  CONTINUE
C
      GO TO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C
 800  DO 801 K=1,NSTA
      DATA(K)=9999.
 801  CONTINUE
C
 900  RETURN
      END      
