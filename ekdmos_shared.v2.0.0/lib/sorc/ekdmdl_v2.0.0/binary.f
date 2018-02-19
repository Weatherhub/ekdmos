      SUBROUTINE BINARY(KFILDO,ID,IDPARS,THRESH,SDATA,NSTA,IER)
C
C        MAY   1994   GLAHN   TDL   MOS-2000 
C        JULY  1998   GLAHN   MODIFIED FOR BINARY INDICATORS 8 AND 9
C        JULY  1998   GLAHN   MODIFIED FOR BINARY INDICATORS 6 AND 7
C        MARCH 2000   DALLAVALLE   MODIFIED FORMAT STATEMENTS TO
C                             CONFORM TO FORTRAN 90 STANDARDS
C                             ON THE IBM-SP
C        MARCH 2003   GLAHN   ADDED IDPARS = 2 CAPABILITY; COUPLE
C                             COMMAS IN /D FORMATS; SPELL CHECK
C 
C        PURPOSE 
C            TO MAKE A POINT BINARY VARIABLE.  WHEN IDPARS = 1,
C            THE BINARY TAKES THE VALUE OF 1 WHEN THE ORIGINAL
C            VARIABLE IS GE THRESH.  IF THE VALUE IS INDICATED
C            AS MISSING, IT IS UNCHANGED.  WHEN IDPARS = 2, THE BINARY
C            TAKES THE VALUE OF 1 WHEN THE ORIGINAL VARIABLE IS LT
C            THRESH.  THIS IS FOR USE IN U201 AND U700 AND IN STRAT
C            IN U602; ANOTHER ROUTINE IS ALSO USED FOR U600 AND U602
C            TO USE TWO THRESHOLDS AND MAKE DISCRETE BINARIES.
C            MODIFIED IN JULY 1998 TO ACCOMMODATE IDPARS = 8 AND 9.
C            WHEN IDPARS = 8, VALUES GT THRESH ARE SET 9999 AND 0
C            OTHERWISE.  WHEN IDPARS = 9, VALUES LT THRESH ARE SET
C            TO 9999 AND 0 OTHERWISE.  THESE ARE FOR MATCHING
C            VARIABLES IN U850 VERIFICATION, AND MAY BE USED
C            FOR STRATIFICATION IN OTHER PROGRAMS.  FURTHER
C            MODIFIED IN JULY 1998 TO ACCOMMODATE IDPARS = 6 AND 7.
C            WHEN IDPARS = 6, VALUES GT THRESH ARE SET -9999 AND 0
C            OTHERWISE.  WHEN IDPARS = 7, VALUES LT THRESH ARE SET
C            TO -9999 AND 0 OTHERWISE.  THESE ARE FOR MATCHING "OR"
C            VARIABLES IN U850 VERIFICATION, AND MAY BE USED
C            FOR STRATIFICATION IN OTHER PROGRAMS.
C 
C        DATA SET USE
C            KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C               ID(J) = 4-WORD VARIABLE ID (J=1,4).  USED ONLY FOR
C                       DIAGNOSTIC PRINT.  (INPUT)
C              IDPARS = SINGLE VALUE INDICATING THE TRANSFORMATION TO
C                       MAKE.  IT SHOULD BE 1, 2, 6,7, 8, OR 9 WHEN
C                       BINARY IS ENTERED.  (INPUT)
C              THRESH = THE THRESHOLD VALUE.  SEE PURPOSE FOR USE.
C                       (INPUT)
C            SDATA(K) = THE VALUES FOR ALL STATIONS (K=1,NSTA) TO BE
C                       BINARIED.  (INPUT-OUTPUT)
C                NSTA = THE NUMBER OF STATIONS BEING DEALT WITH.  
C                       ALSO, TREATED AS DIMENSION OF SDATA( ).  (INPUT)
C                 IER = STATUS RETURN.  (OUTPUT)
C                       0  = GOOD RETURN.
C                       61 = THRESHOLD IS MISSING VALUE.
C                       62 = IDPARS VALUE NOT FOUND IN THIS ROUTINE. 
C                       63 = RESERVED.
C                       WHEN IER NE 0, SDATA( ) IS SET TO 9999.
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED
C            NONE.
C
      DIMENSION ID(4)
      DIMENSION SDATA(NSTA)
C
      IER=0
      IF(THRESH.NE.9999.)GO TO 110
C
C        THE THRESHOLD IS INDICATED AS MISSING.
C        SET THE VALUES TO MISSING.
C
      WRITE(KFILDO,101)THRESH,IDPARS,(ID(J),J=1,4)
 101  FORMAT(/,' ****THRESHOLD =',F7.0,' FOR BINARY PARAMETER IDPARS =',
     1         I3,' IN PREDICTOR',1X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,/,
     2         '     IS INDICATED AS MISSING IN SUBROUTINE BINARY.',
     2         '  ALL VALUES SET TO MISSING.')
      IER=61
      GO TO 805
C
 110  IF(IDPARS.EQ.1)THEN
C
C        MAKE THE BINARIES, CUMULATIVE FROM ABOVE.  THE BINARY IS
C        SET 1 WHEN THE ORIGINAL VALUE IS GE THRESH.
C
         DO 160 K=1,NSTA
C        WRITE(KFILDO,120)SDATA(K),THRESH
C120     FORMAT(' IN BINARY AT 120, SDATA( ),THRESH',2F10.4)
         IF(SDATA(K).EQ.9999.)GO TO 160
C
         IF(SDATA(K).GE.THRESH)THEN
            SDATA(K)=1.
         ELSE
            SDATA(K)=0.
         ENDIF   
C
 160     CONTINUE
C
      ELSEIF(IDPARS.EQ.2)THEN
C
C        MAKE THE BINARIES, CUMULATIVE FROM BELOW.  THE BINARY IS
C        SET 1 WHEN THE ORIGINAL VALUE IS LT THRESH.
C
         DO 180 K=1,NSTA
C        WRITE(KFILDO,170)SDATA(K),THRESH
C170     FORMAT(' IN BINARY AT 170, SDATA( ),THRESH',2F10.4)
         IF(SDATA(K).EQ.9999.)GO TO 180
C
         IF(SDATA(K).LT.THRESH)THEN
            SDATA(K)=1.
         ELSE
            SDATA(K)=0.
         ENDIF   
C
 180     CONTINUE
C
      ELSEIF(IDPARS.EQ.8)THEN
C
C           MAKE THE BINARIES.  THE BINARY IS SET 9999 WHEN THE ORIGINAL
C           VALUE IS GT THRESH, AND 0 OTHERWISE.
C
         DO 260 K=1,NSTA
D        WRITE(KFILDO,220)SDATA(K),THRESH
D220     FORMAT(' IN BINARY AT 220, SDATA( ),THRESH',2F10.4)
         IF(SDATA(K).EQ.9999.)GO TO 260
C
         IF(SDATA(K).GT.THRESH)THEN
            SDATA(K)=9999.
         ELSE
            SDATA(K)=0.
         ENDIF   
C
 260     CONTINUE
C
      ELSEIF(IDPARS.EQ.9)THEN
C
C           MAKE THE BINARIES.  THE BINARY IS SET 9999 WHEN THE ORIGINAL
C           VALUE IS LT THRESH, AND 0 OTHERWISE.
C
         DO 360 K=1,NSTA
D        WRITE(KFILDO,320)SDATA(K),THRESH
D320     FORMAT(' IN BINARY AT 320, SDATA( ),THRESH',2F10.4)
         IF(SDATA(K).EQ.9999.)GO TO 360
C
         IF(SDATA(K).LT.THRESH)THEN
            SDATA(K)=9999.
         ELSE
            SDATA(K)=0.
         ENDIF   
C
 360     CONTINUE
C
      ELSEIF(IDPARS.EQ.6)THEN
C
C           MAKE THE BINARIES.  THE BINARY IS SET -9999 WHEN THE 
C           ORIGINAL VALUE IS GT THRESH, AND 0 OTHERWISE.
C
         DO 460 K=1,NSTA
D        WRITE(KFILDO,420)SDATA(K),THRESH
D420     FORMAT(' IN BINARY AT 420, SDATA( ),THRESH',2F10.4)
         IF(SDATA(K).EQ.9999.)GO TO 460
C
         IF(SDATA(K).GT.THRESH)THEN
            SDATA(K)=-9999.
         ELSE
            SDATA(K)=0.
         ENDIF   
C
 460     CONTINUE
C
      ELSEIF(IDPARS.EQ.7)THEN
C
C        MAKE THE BINARIES.  THE BINARY IS SET -9999 WHEN THE
C        ORIGINAL VALUE IS LT THRESH, AND 0 OTHERWISE.
C
         DO 560 K=1,NSTA
D        WRITE(KFILDO,520)SDATA(K),THRESH
D520     FORMAT(' IN BINARY AT 520, SDATA( ),THRESH',2F10.4)
         IF(SDATA(K).EQ.9999.)GO TO 560
C
         IF(SDATA(K).LT.THRESH)THEN
            SDATA(K)=-9999.
         ELSE
            SDATA(K)=0.
         ENDIF   
C
 560     CONTINUE
C
      ELSE
         GO TO 800
C           A LEGITIMATE VALUE OF IDPARS HAS NOT BEEN FOUND.
      ENDIF
C
C        A LEGITIMATE VALUE OF IDPARS HAS BEEN FOUND.
C
      GO TO 900
C
C        AT THIS POINT, THE BINARY PARAMETER HAS NOT BEEN
C        FOUND OR THE THRESHOLD IS 9999.  PRINT DIAGNOSTIC,
C        AND SET ALL VALUES TO MISSING.
C
 800  WRITE(KFILDO,801)IDPARS,(ID(J),J=1,4)
 801  FORMAT(/,' ****THE BINARY PARAMETER IDPARS =',I3,
     1         ' IN PREDICTOR',1X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,/,
     2         '     WAS NOT FOUND IN SUBROUTINE BINARY.',
     3         '  ALL VALUES SET TO MISSING.')
      IER=62
C
 805  DO 810 K=1,NSTA
      SDATA(K)=9999.
 810  CONTINUE
C
 900  RETURN
      END 
