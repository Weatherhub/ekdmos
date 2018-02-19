      SUBROUTINE PRSID3(KFILDO,ID,KD,KDPARS)
C 
C        FEBRUARY 1997   GLAHN   TDL   MOS-2000 
C
C        PURPOSE 
C            TO PARSE A 4-WORD ID INTO SOME OF ITS INTEGER PARTS AND TO
C            RECONSTRUCT THE FULL 4-WORD ID WITH SOME CHANGES.  THIS IS
C            USED BY RDSTRX FOR THE LOOKAHEAD FEATURE FOR PREDICTANDS.
C            IT IS ANTICIPATED THAT ONLY KDPARS(3) AND KDPARS(12)
C            WILL BE NEEDED IN THE CALLING PROGRAM.
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                     (OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT) 
C               ID(J) = THE VARIABLE ID'S (J=1,4).  (INPUT)
C               KD(J) = THE VARIABLE ID'S (J=1,4).  (OUTPUT)
C           KDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       PREDICTOR ID'S CORRESPONDING TO ID(J),
C                       WITH SOME CHANGES (J=1,15).  ONLY THE VALUES
C                       NEEDED BY THE CALLING PROGRAM ARE RETURNED.
C                       (OUTPUT)
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
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            NONE
C
      DIMENSION ID(4),KD(4),KDPARS(15)
C
C        FASHION KD(1) AND KD(4).
C
         KDPARS(1)=ID(1)/1000000
         KDPARS(2)=ID(1)/1000-KDPARS(1)*1000
         KDPARS(3)=ID(1)/100-KDPARS(1)*10000-KDPARS(2)*10
C
      IF(KDPARS(3).EQ.0.OR.KDPARS(3).GE.5)THEN
C           THRESH SHOULD BE ZERO FOR B = 0.
         KD(1)=ID(1)
         KD(4)=ID(4)
      ELSE
         KD(1)=ID(1)-KDPARS(3)*100
         KD(4)=ID(4)-(ID(4)/1000)*1000
      ENDIF
C
C        KD(2) = ID(2).
C
      KD(2)=ID(2)
C
C        KD(3) = ID(3), EXCEPT OMIT TAU, AND RETURN IT
C        IN KDPARS(12).
C
      KD(3)=(ID(3)/1000)*1000
      KDPARS(12)=ID(3)-KD(3)
C
      RETURN
      END
