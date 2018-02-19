      SUBROUTINE EST905(KFILDO,IP21,NDATE,CCALL,NSTA,
     1                  KGP,NGP,LGP,MTRMS,MTANDS,
     2                  ID,CONST,ESS,CORR,COEF,X,VECTOR,FINERR,
     3                  IDTAND,LOCSTA,ND1,ND2,ND3,ND13,
     4                  PLAIN,IER)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM: EST905
C   PRGMMR: GLAHN/WIEDENFELD  ORG: W/OSD211    DATE: 07-10-25
C
C   ABSTRACT: TO PROVIDE FOR U905 THE UNCERTAINTY ESTIMATE FOR THE
C             FORECASTS.  CONSIDERABLE IP21 OUTPUT IS PROVIDED FOR
C             CHECKOUT.
C
C PROGRAM HISTORY LOG:
C   07-10-25 WIEDENFELD   MODIFIED FROM EST705 TO MAKE OPERATIONAL.
C
C USAGE:    CALL EST905(KFILDO,IP21,NDATE,CCALL,NSTA,KGP,NGP,LGP,
C                       MTRMS,MTANDS,ID,CONST,ESS,CORR,COEF,X,VECTOR,
C                       FINERR,IDTAND,LOCSTA,ND1,ND2,ND3,ND13,
C                       PLAIN,IER)
C                                 
C        DATA SET USE
C            KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C            IP21   - INDICATES WHETHER (>0) OR NOT (=0) THE
C                     EQUATIONS WILL BE WRITTEN TO UNIT IP(21) FOR 
C                     VIEWING.  ALSO, THE ERROR ESTIMATES ARE
C                     WRITTEN.  (INPUT)
C
C INPUT ARGUMENT LIST:
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C                IP21 = INDICATES WHETHER (>0) OR NOT (=0) THE
C                       EQUATIONS WILL BE WRITTEN TO UNIT IP(21) FOR 
C                       VIEWING.  ALSO, THE ERROR ESTIMATES ARE
C                       WRITTEN.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH FORECASTS ARE TO BE
C                       MADE ON THIS CALL TO FCST75.  (INPUT)
C            CCALL(K) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (K=1,NSTA).  (CHARACTER*8) (INPUT)
C                NSTA = THE NUMBER OF STATIONS IN CCALL( ).  (INPUT)
C                 KGP = THE NUMBER OF GROUPS FOR EACH SET OF EQUATIONS
C                       (I=1,NSETS).  (INPUT)
C              NGP(L) = FOR EACH EQUATION (L=1,KGP) IN EACH SET
C                       (I=1,NSETS), THE NUMBER OF STATIONS IN EACH
C                       GROUP.
C              LGP(L) = FOR EACH EQUATION (L=1,LGP( )) IN EACH SET
C                       (I=1,NSETS), THE LOCATION IN LOCSTA ( ,I) OF
C                       WHERE THE FIRST STATION IN THE SET IS.  (INPUT)
C            MTRMS(L) = THE NUMBER OF TERMS IN EACH GROUP L (L=1,KGP)
C                       AND EACH EQUATION SET (I=1,NSETS).
C                       NOT ACTUALLY USED.  (INPUT)
C              MTANDS = THE NUMBER OF PREDICTANDS FOR EACH EQUATION
C                       SET I (I=1,NSETS).  (INPUT)
C           ID(J,L,M) = THE 4-WORD ID (J=1,4) FOR EACH PREDICTOR
C                       (M=1,NTRMS) IN EACH EQUATION (L=1,KGP),
C                       OF EACH EQUATION SET (I=1,NSETS).  THE VALUES
C                       (ID(5, , , ), ID(6, , , ), AND 
C                       ID(7, , , ) ARE THE 2ND, 3RD, AND 4TH 
C                       INDICES INTO ID( , , , ) OF THE NEXT
C                       OCCURRENCE OF THE SAME PREDICTOR.  THESE 
C                       INDICES ARE ALSO USED FOR ENTRY INTO 
C                       COEF( , , , ), ETC.  (INPUT)
C         CONST(L,NN) = THE EQUATION CONSTANTS FOR GROUP L (L=1,KGP), 
C                       PREDICTAND NN (NN=1,MTANDS(I)), AND EQUATION
C                       SET I (I=1,NSETS).  (INPUT)
C           ESS(L,NN) = THE STANDARD ERROR ESTIMATE FOR GROUP L 
C                       (L=1,KGP), PREDICTAND NN (NN=1,MTANDS(I))
C                       AND EQUATION SET I (I=1,NSETS).  (INPUT)
C          CORR(L,NN) = THE MULTIPLE CORRELATIONS FOR GROUP L
C                       (L=1,KGP), PREDICTAND NN (NN=1,MTANDS(I)),A
C                       AND EQUATION SET I (I=1,NSETS).  (INPUT)
C        COEF(L,M,NN) = THE COEFFICIENTS FOR GROUP L (L=1,KGP),
C                       TERM M (M=1,MTRMS(L,I)), PREDICTAND NN 
C                       (NN=1,MTANDS(I)), AND EQUATION SET I
C                       (I=1,NSETS).  (INPUT)
C            X(M,M,L) = THE PREDICTOR INVERSE X-PRODUCT PREDICTOR MATRIX 
C                       (INCLUDING THE CONSTANT) TO USE IN CALCULATING
C                       EXPECTED ERROR (M=1,MTRMS(L,I)+1) (L=1,KGP)
C                       (I=1,NSETS).  (INPUT) (REAL*8)
C       VECTOR(K,L,M) = VECTOR OF DATA FOR STATION K (K=1,NSTA) FOR
C                       GROUP L (L=1,KGP), TERM M (M=1,MTRMS(L,I)),
C                       AND EQUATION SET I (I=1,NSETS).  (INPUT)
C        FINERR(K,NN) = THE FINAL ERRORS OF ESTIMATE FOR STATION K 
C                       (K=1,NSTA), PREDICTAND NN (NN=1,MTANDS(I)),
C                       AND EQUATION SET I (I=1,NSETS).  (OUTPUT)
C        IDTAND(J,NN) = THE PREDICTAND ID'S (J=1,4) FOR PREDICTAND NN 
C                       (NN=1,MTANDS(I)), AND EQUATION SET I
C                       (I=1,NSETS).  (INPUT)
C           LOCSTA(K) = THE LOCATION IN FCST( ,NN,I) (NN=1,MTANDS(I))
C                       OF WHERE TO PUT THE FORECAST, WHERE K IS IN
C                       ORDER OF THE EQUATIONS AS READ IN FOR SET I
C                       (I=1,NSETS).  (INPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                       WITH.  USED AS DIMENSION OF CCALL( ) AND FIRST
C                       DIMENSION OF VECTOR( , , , ).  (INPUT)
C                 ND2 = MAXIMUM NUMBER OF TERMS IN ANY EQUATION.
C                       THIRD DIMENSION OF ID( , , , ) AND
C                       SECOND DIMENSION OF COEF( , , , ).  (INPUT)
C                 ND3 = MAXIMUM NUMBER OF PREDICTANDS IN ANY EQUATION.
C                       USED AS DIMENSION OF SEVERAL VARIABLES. 
C                       (INPUT)
C                ND13 = MAXIMUM NUMBER OF DIFFERENT EQUATIONS PER SET.
C                       THIS WOULD = ND1 FOR SINGLE STATION EQUATIONS,
C                       BUT MIGHT BE ON THE ORDER OF 30 FOR REGIONAL
C                       EQUATIONS.  DIMENSION OF SEVERAL VARIABLES.
C                       (INPUT)
C           PLAIN(NN) = THE PLAIN LANGUAGE DESCRIPTION OF THE 
C                       PREDICTAND NN (NN=1,MTANDS(I)), AND EQUATION
C                       SET I (I=1,NSETS).  (CHARACTER*32)  (INPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C              V(M,1) = COLUMN VECTOR HOLDING PREDICTOR VALUES IN THE 
C                       ORDER OF THE COEFFICIENTS (M=1,MTRMS(L,I)).
C                       (REAL*8)  (AUTOMATIC)  (INTERNAL)
C              T(1,M) = ROW WORK VECTOR.  (REAL*8)  (AUTOMATIC)
C                       (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES USED 
C            NONE
C
      CHARACTER*8 CCALL(ND1)
      CHARACTER*32 PLAIN(ND3)
C
      REAL*8 X(ND2+1,ND2+1,ND13)
      REAL*8 V(ND2+1,1),T(1,ND2+1),ERREST(1,1)
C        V( , ) AND T( ,1) ARE AUTOMATIC VARIABLES.
C
      DIMENSION NGP(ND13),LGP(ND13),MTRMS(ND13)
      DIMENSION ID(7,ND13,ND2)
      DIMENSION CONST(ND13,ND3),
     1          ESS(ND13,ND3),
     2          CORR(ND13,ND3)
      DIMENSION COEF(ND13,ND2,ND3)
      DIMENSION VECTOR(ND1,ND13,ND2)
      DIMENSION FINERR(ND1,ND3)
      DIMENSION IDTAND(4,ND3)
      DIMENSION LOCSTA(ND1)
C
D     CALL TIMPR(KFILDO,KFILDO,'START EST705        ')
C
C        WRITE THE EQUATIONS.
C
      V(1,1)=1.
C
      DO 409 L=1,KGP
      IF(IP21.EQ.0)GO TO 4080
C        THIS BYPASSES PRINT.      
C
      IF(NGP(L).EQ.0)GO TO 409
C        IF THIS TEST IS NOT MADE, THE EQUATIONS, ETC., WILL BE
C        WRITTEN EVEN THOUGH THERE IS NO STATION FOR THEM.
      WRITE(IP21,401)L,KGP,NDATE
 401  FORMAT(/,'EQUATION NO.',I6,' OF',I6,' FOR DATE',I12) 
C
      DO 405 NN=1,MTANDS
C
      WRITE(IP21,402)(IDTAND(J,NN),J=1,4),PLAIN(NN),
     1                CONST(L,NN),ESS(L,NN)
 402  FORMAT(/,'    PREDICTAND',3(1X,I9.9),1X,I10.3,3X,A32,
     1         '   CONST =',F10.4,'   ESS =',F10.4)
C
      WRITE(IP21,403)
 403  FORMAT(/,'       PREDICTOR',35X,'COEFFICIENT    VECTORS',
     1         ' FOR STATIONS')
C
      DO 404 M=1,MTRMS(L)
C
      IF(NGP(L).LE.3)THEN
C           PRINT SPLIT SO BLANK LINE WON'T BE LEFT WHEN EXACTLY
C           3 STATIONS.
         WRITE(IP21,4035)
     1        (ID(J,L,M),J=1,4),
     2          COEF(L,M,NN),
     3         (CCALL(LOCSTA(LGP(L)+K-1)),
     4          VECTOR(LOCSTA(LGP(L)+K-1),L,M),K=1,NGP(L))
 4035    FORMAT('      ',3(1X,I9.9),1X,I10.3,3X,F12.5,3(2X,A8,F10.2))
      ELSE
         WRITE(IP21,4036)
     1        (ID(J,L,M),J=1,4),
     2          COEF(L,M,NN),
     3         (CCALL(LOCSTA(LGP(L)+K-1)),
     4          VECTOR(LOCSTA(LGP(L)+K-1),L,M),K=1,NGP(L))
 4036    FORMAT('      ',3(1X,I9.9),1X,I10.3,3X,F12.5,3(2X,A8,F10.2),/,
     1                                           (62X,3(2X,A8,F10.2)))
      ENDIF
C
 404  CONTINUE
c
 405  CONTINUE
C
C        WRITE THE MATRIX FOR THIS EQUATION SET.
C
      WRITE(IP21,406)
 406  FORMAT(/'       MATRIX INVERSE',/)
C
      DO 408 M=1,MTRMS(L)+1
      WRITE(IP21,407)(X(M,MM,L),MM=1,MTRMS(L)+1) 
C        THIS WILL ADROITLY HANDLE UP TO 13 TERSM ONLY.
 407  FORMAT('    ',13F10.5)
 408  CONTINUE
C
C        COMPUTE ERROR BOUNDS.
C        COMPUTE TRANSPOSE XO TIMES X'X INVERSE -- XO'(X'X)-1.
C
 4080 DO 4085 K=1,NGP(L)
C
      DO 4082 M=1,MTRMS(L)
      V(M+1,1)=VECTOR(LOCSTA(LGP(L)+K-1),L,M)
C        THIS PUTS THE VECTOR IN THE ORDER OF THE PREDICTORS IN
C        X( , , ), WITH UNITY IN THE FIRST LOCATION.
C
C****      WRITE(KFILDO,8888)L,K,M,V(M+1,1)
C**** 8888 FORMAT(' AT 8888--L,K,M,V(M+1,1)',4I5,F10.5)
C
      IF(V(M+1,1).EQ.9999.)THEN
C
         DO 4081 NN=1,MTANDS
         FINERR(LOCSTA(LGP(L)+K-1),NN)=9999.
 4081    CONTINUE
C
         GO TO 4085
      ENDIF
 4082 CONTINUE
C
      CALL TRMULT(KFILDO,V,ND2+1,1,
     1                   X(1,1,L),ND2+1,ND2+1,
     2                   T,1,ND2+1,
     3                   MTRMS(L)+1,1,MTRMS(L)+1,
     4                   IER)
      CALL MULT(KFILDO,T,1,ND2+1,
     1                 V,ND2+1,1,
     2                 ERREST,1,1,
     3                 1,MTRMS(L)+1,1,
     4                 IER)
C
C****      WRITE(KFILDO,9999)ERREST(1,1)
C**** 9999 FORMAT(/' AT 9999--ERREST(1,1)--'F10.4)
C
      DO 4083 NN=1,MTANDS
C
         FINERR(LOCSTA(LGP(L)+K-1),NN)=ESS(L,NN)*SQRT(1.+ERREST(1,1))
C
 4083 CONTINUE
C
 4085 CONTINUE
C
 409  CONTINUE
C
      IF(IP21.NE.0)THEN
C
         DO 4098 K=1,NSTA
         WRITE(IP21,4097)CCALL(K),(FINERR(K,NN),NN=1,MTANDS)
 4097    FORMAT('    ',A8,3X,11F10.4,/,(15X,11F10.4))
C           THIS WILL HANDLE UP TO 13 PREDICTANDS ADROITLY.
C           THE CALCULATIONS AND PRINTING IS DONE BY SET I, BUT ALL
C           SETS ARE RETURNED FOR PACKING.
 4098    CONTINUE
C
      ENDIF
C
      RETURN
      END     
