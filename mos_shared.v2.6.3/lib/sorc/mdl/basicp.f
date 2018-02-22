      SUBROUTINE BASICP(KFILDO,IDPARS,JD)
C 
C        APRIL 1994   GLAHN   TDL   MOS-2000 
C
C        PURPOSE 
C            TO COMPUTE A BASIC 4-WORD JD( ), WHICH IS THE SAME AS ID( )
C            EXCEPT CERTAIN "PROCESSING" INFORMATION IS OMITTED.  
C            IDPARS( ) PROVIDES THE INFORMATION IN ID( ).
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT) 
C                TEMP = SIGN OF THRESHOLD.  (CHARACTER*1)  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID'S CORRESPONDING TO ID( ) (J=1,15).
C                       (OUTPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               JD(J) = THE COMPUTED BASIC PREDICTOR ID'S (J=1,4).
C                       (OUTPUT)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            NONE
C
      DIMENSION JD(4),IDPARS(15)
C
      JD(1)=IDPARS(1)*1000000+IDPARS(2)*1000+IDPARS(4)
      JD(2)=IDPARS(5)*100000000+IDPARS(6)*10000+IDPARS(7)
      JD(3)=IDPARS(9)*1000000+IDPARS(10)*100000+IDPARS(11)*1000+
     1      IDPARS(12)
      JD(4)=0
      RETURN
      END
