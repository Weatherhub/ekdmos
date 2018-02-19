      SUBROUTINE SETJDT(KFILDO,ID,IDPARS,TRESHL,JD,ITAU,KGP,MTRMS,
     1                  ND2,ND13,IER)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM: SETJDT
C   PRGMMR: GLAHN        ORG: W/OSD211    DATE: 98-02-01
C
C ABSTRACT: TO SET JD( , , ), THRESH( , ), AND ITAU( , ) TO AGREE
C           WITH ID( , , ) FOR U900.
C
C PROGRAM HISTORY LOG:
C   99-09-21  GLAHN 
C   00-05-16  CARROLL	 ADDED NCEP DOCBLOCK.
C
C USAGE:    CALL SETJDT(KFILDO,ID,IDPARS,TRESHL,JD,ITAU,KGP,MTRMS,
C                       ND2,ND13,IER) 
C   INPUT ARGUMENT LIST:
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C           ID(J,L,M) = THE 4-WORD ID (J=1,4) FOR EACH TERM
C                       (M=1,NTRMS(L)) FOR EACH EQUATION
C                       (L=1,KGP).  J=5-7 ARE NOT USED.  (INPUT)
C                 KGP = THE NUMBER OF GROUPS OF EQUATIONS.  (INPUT)
C            MTRMS(L) = THE NUMBER OF TERMS IN EACH EQUATION
C                       (L=1,KGP).  (INPUT)
C                 ND2 = MAXIMUM NUMBER OF TERMS IN ANY EQUATION.
C                       (INPUT)
C                ND13 = MAXIMUM NUMBER OF DIFFERENT EQUATIONS PER SET.
C                       THIS MIGHT BE GE 1000 FOR SINGLE STATION EQUATIONS,
C                       BUT MIGHT BE ON THE ORDER OF 30 FOR REGIONAL
C                       EQUATIONS.  (INPUT)
C
C   OUTPUT ARGUMENT LIST: 
C       IDPARS(J,L,M) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       PREDICTOR ID'S CORRESPONDING TO ID( ,L,M)
C                       (J=1,15), (L=1,KGP) (M=1,NTRMS(L)).  (OUTPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                          0 = NOT BINARY,
C                          1 = CUMULATIVE FROM ABOVE, VALUES GE LOWER
C                              THRESHOLD TRESHL = 1,
C                          2 = CUMULATIVE FROM BELOW, VALUES LT UPPER
C                              THRESHOLD TRESHU = 1.
C                          3 = DISCRETE BINARY.  VALUES GE LOWER
C                              THRESHOLD AND LT UPPER THRESHOLD = 1.
C                          5 = GRID BINARY.  VALUES GE LOWER THRESHOLD
C                          ONLY THE VALUE OF 0, 1, OR 5 SHOULD BE USED
C                          FOR PREDICTORS;
C                          0, 1, 2, OR 3 CAN BE USED FOR PREDICTANDS,
C                          BUT THESE ARE NOT INVOLVED IN U700.
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
C         TRESHL(L,M) = THE LOWER BINARY THRESHOLD CORRESPONDING TO
C                       IDPARS( ,L,M) (L=1,KGP) (M=1,NTRMS(L)).
C                       FOR U900, THE UPPER THRESHOLD IS ALWAYS LARGE.
C                       THAT IS, THE PREDICTORS CARRY WITH THEM ONLY
C                       ONE THRESHOLD, THE LOWER ONE.  (OUTPUT)
C           JD(J,L,M) = THE BASIC INTEGER VARIABLE ID'S (J=1,4)
C                       (L=1,KGP) (M=1,NTRMS(L)).  THIS IS THE SAME
C                       AS ID(J,L,M), EXCEPT THAT THE FOLLOWING PORTIONS
C                       ARE OMITTED:
C                       B = IDPARS(3, ),
C                       G = IDPARS(15, ), AND
C                       TRESHL( ).
C                       (OUTPUT)
C           ITAU(L,M) = THE NUMBER OF HOURS TO ADD TO NDATE TO GET
C                       THE VARIABLE ID(L,M) (L=1,KGP) (M=1,NTRMS(L)).
C                       THIS IS THE "LOOKAHEAD" FEATURE.  (OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN
C
C        DATA SET USE
C        INPUT FILES: 
C             FORT.xx - INDICATE NAME & PURPOSE
C
C        OUTPUT FILES: 
C             FORT.xx - INDICATE NAME & PURPOSE
C
C        VARIABLES: NO NEW
C
C        SUBPROGRAMS CALLED:
C             UNIQUE    - NONE
C          LIBRARY:
C             MDLLIB90  - PRSID2
C
C REMARKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90 (xlf compiler)
C   MACHINE:  IBM SP
C
C$$$
      DIMENSION ID(7,ND13,ND2),IDPARS(15,ND13,ND2),TRESHL(ND13,ND2),
     1          JD(4,ND13,ND2),ITAU(ND13,ND2)
      DIMENSION MTRMS(ND13)
C
      IER=0
C
C        PARSE VARIABLES INTO IDPARS( , ) AND PREPARE JD( , ).
C
      DO 143 L=1,KGP
C
      DO 142 M=1,MTRMS(L)
D     WRITE(KFILDO,140)L,M,(ID(J,L,M),J=1,4),(JD(J,L,M),J=1,4)
D140  FORMAT(/' SETJDT AT 140--ID,JD'2I4,2(4X3I11,I4))
      CALL PRSID2(KFILDO,ID(1,L,M),IDPARS(1,L,M),TRESHL(L,M))
C
C        FOR ALL PROGRAMS EXCEPT U201, IDPARS(15, ) AND IT'S ROLE IN
C        ID(4) HAS NO MEANING.  THE INTERPOLATED INPUT FILE WILL NOT
C        INCLUDE IT.  THEREFORE, SET IT TO ZERO.
C
      IDPARS(15,L,M)=0
      ID(4,L,M)=(ID(4,L,M)/10)*10
C
C        PREPARE "BASIC" VARIABLE ID'S.  FOR PROGRAMS READING U201
C        OUTPUT (E.G., U600), JD( , ) = ID( , ) EXCEPT IN ID(1, )
C        THE BINARY INDICATOR IS OMITTED AND IN JD(4, )
C        THE THRESHOLDS AND IDPARS(15, ) ARE OMITTED.
C
      JD(1,L,M)=IDPARS(1,L,M)*1000000+
     1          IDPARS(2,L,M)*1000+
     2          IDPARS(4,L,M)
      JD(2,L,M)=ID(2,L,M)
      JD(3,L,M)=ID(3,L,M)
      JD(4,L,M)=IDPARS(13,L,M)*100+
     1          IDPARS(14,L,M)*10
C
C        INITIALIZE ITAU( ).  IT IS ASSUMED THAT THE TAU
C        (IDPARS(12) FOR ANY OBSERVATION (CCC = 7XX) INDICATES
C        THAT VALUE IS TO BE ADDED TO IDATE( ) FOR THE DATE/TIME,
C        AND THE TAU IN THE ID IS ZERO.
C    
      IF(IDPARS(1,L,M).GE.700.AND.IDPARS(1,L,M).LE.799)THEN
         ITAU(L,M)=IDPARS(12,L,M)
         IDPARS(12,L,M)=0
         ID(3,L,M)=ID(3,L,M)-ITAU(L,M)
         JD(3,L,M)=JD(3,L,M)-ITAU(L,M)
      ELSE
         ITAU(L,M)=0
      ENDIF
D     WRITE(KFILDO,141)L,M,(ID(J,L,M),J=1,4),(JD(J,L,M),J=1,4)
D141  FORMAT(' SETJDT AT 141--ID,JD'2I4,2(4X3I11,I4))
 142  CONTINUE
C
 143  CONTINUE
C
      RETURN
      END
