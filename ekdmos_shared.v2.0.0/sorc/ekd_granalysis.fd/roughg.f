      SUBROUTINE ROUGHG(KFILDO,ID,ERROR,NX,NY,
     1                  CPNDFD,SEALND,TELEV,NXE,NYE,
     2                  ITABLE,RTABLE,IDIM,JDIM,M,
     3                  ITERML,ITERMW,ISTOP,IER)
C
C        DECEMBER  2007   GLAHN   MDL   MOS-2000
C        JANUARY   2008   GLAHN   ADDED TEST ON ERROR( , ) = 9999.
C        JANUARY   2008   GLAHN   LOCATION OF TEST CHANGED; INDICES
C                                 IX TO IXX AND JY TO JYY BELOW 300
C        FEBRUARY  2008   GLAHN   ADDED ITERML,ITERMW,JTERML,JTERMW
C        FEBRUARY  2008   GLAHN   CHANGED JTERM TO ITERM, L AND W
C        FEBRUARY  2009   GLAHN   ADDED TO PURPOSE
C        MARCH     2009   GLAHN   CHANGED 2ND WORD STRUCTURE
C        JULY      2010   SCALLION   OPEN MP STATEMENTS PUT IN IN
C                                 2 PLACES
C
C        PURPOSE
C            TO ACCUMULATE THE ANALYSIS ERROR IN ERROR( , ) FOR
C            THE TERMS IN THE ERROR ESTIMATION EQUATIONS INVOLVING
C            THE GRIDPOINT ROUGHNESS.  IT DEALS WITH ONLY LAND
C            EQUATIONS AND GRIDPOINTS, AS THERE IS NO ROUGHNESS OVER
C            WATER. THE ERROR( , ) IS ACCUMULATED FOR ALL TERMS
C            INVOLVING 0597XXXX, 0697XXXX, 0797XXXX, OR 0897XXXX,
C            WHERE XXXX IS A RADIUS R:
C
C               05970008 = ROUGHNESS COMPUTED WITHIN 8 GRIDLENGTHS,
C               05970004 = ROUGHNESS COMPUTED WITHIN 4 GRIDLENGTHS,
C               05970002 = ROUGHNESS COMPUTED WITHIN 2 GRIDLENGTHS, AND
C               05970001 = ROUGHNESS COMPUTED WITHIN 1 GRIDLENGTHS.
C
C            THE ROUGHNESS IS COMPUTED FOR EACH GRIDPOINT AS THE
C            MEAN ABSOLUTE DIFFERENCE BETWEEN EACH INDIVIDUAL GRIDPOINT
C            AND THE MEAN OF THOSE GRIDPOINTS SURROUNDING THE GRIDPOINT
C            IT IS BEING COMPUTED FOR.  THE CENTER GRIDPOINT ITSELF IS
C            INCLUDED IN THE COMPUTATIONS.  THE ROUGHNESS IS NOT 
C            COMPUTED OUTSIDE THE NDFD CLIPPING AREA.  IT IS ASSUMED
C            NO LAND ELEVATIONS ARE MISSING (9999.) WITHIN THE CLIPPING
C            AREA.
C
C        DATA SET USE
C            KFILDO   - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C        ERROR(IX,JY) = HOLDS THE ACCUMULATED ERROR (IX=1,NY) (JY=1,NY).
C                       (INPUT/OUTPUT)
C                  NX = THE SIZE OF THE ERROR( , ) GRID IN THE X
C                       DIRECTION.  (INPUT)
C                  NY = THE SIZE OF THE ERROR( , ) GRID IN THE Y
C                       DIRECTION.  (INPUT)
C       CPNDFD(IX,JY) = THE NDFD MASK FROM THE MOS-2000 EXTERNAL
C                       RANDOM ACCESS FILE (IX=1,NYE) (JY=1,NYE) AT
C                       NOMINAL MESHLENGTH MESHE.
C                       1 = WITHIN THE AREA; 0 = OUTSIDE.  (INPUT)
C           SEALND(J) = THE LAND/SEA MASK (IX=1,NXE) (JY=1,NYE).
C                       (INPUT)
C                       0 = OCEAN WATER GRIDPOINTS;
C                       3 = INLAND WATER GRIDPOINTS.
C                       9 = LAND GRIDPOINTS.
C                       (INPUT)
C        TELEV(IX,JY) = THE TERRAIN ELEVATION (IX=1,NXE) (JY=1,NYE).
C                       (INPUT)
C                 NXE = X-EXTENT OF SEALND( ), CPNDFD( ), AND TELEV( )
C                       AT MESH LENGTH MESHE.  MUST = NX.  (INPUT)
C                 NYE = Y-EXTENT OF SEALND( ), CPNDFD( ), AND TELEV( )
C                       AT MESH LENGTH MESHE.  MUST = NY.  (INPUT)
C       ITABLE(J,L,M) = HOLDS THE 2ND WORD IDS FOR UP TO JDIM
C                       PREDICTORS (J=1,JDIM) FOR UP TO IDIM
C                       EQUATIONS (M=1,IDIM) FOR LAND (L=1) AND
C                       WATER (L=2).   (INPUT)
C       RTABLE(J,L,M) = HOLDS THE JDIM COEFFICIENTS (J=1,JDIM) AND
C                       THE CONSTANT (J=JDIM+1) FOR THE LAND (L=1)
C                       AND WATER (L=2) EQUATION FOR IDIM EQUATIONS
C                       (M=1,IDIM), EACH EQUATION PERTAINING TO A
C                       DIFFERENT VARIABLE.  (INPUT)
C                IDIM = THE MAXIMUM NUMBER OF PAIRS (LAND AND WATER)
C                       EQUATIONS.  (INPUT)
C                JDIM = THE MAXIMUM NUMBER OF TERMS IN THE EQUATIONS.
C                       (INPUT)
C                   M = THE NUMBER OF THE EQUATION.  M WOULD VARY
C                       WITH WEATHER ELEMENT.  (INPUT)
C              ITERML = THE NUMBER OF TERMS IN THE LAND EQUATION
C                       EVALUATED.  (INPUT/OUTPUT)
C              ITERMW = THE NUMBER OF TERMS IN THE WATER EQUATION
C                       EVALUATED.  NOT ACTUALLY USED.
C                       (INPUT/OUTPUT)
C               ISTOP = INCREMENTED BY 1 WHEN AN ERROR OCCURS.
C                       (INPUT/OUTPUT)
C                 IER = 0 = GOOD RETURN.  (OUTPUT)
C                  IR = RADIUS OVER WHICH TO COMPUTE ROUGHNESS.
C                       (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED
C            NONE.
C
      DIMENSION ID(4)
      DIMENSION ERROR(NX,NY)
      DIMENSION TELEV(NXE,NYE),SEALND(NXE,NYE),CPNDFD(NXE,NYE)
      DIMENSION ITABLE(JDIM,2,IDIM),RTABLE(JDIM+1,2,IDIM)
      DIMENSION TEMP(NX*NY)
C        TEMP( ) IS AN AUTOMATIC ARRAY LARGE ENOUGH TO HOLD THE MAXIMUM
C        NUMBER OF POINTS.
C
CD     CALL TIMPR(KFILDO,KFILDO,'START ROUGHG        ')
C
      IER=0
C
C        THIS DEALS WITH ONLY LAND EQUATIONS, ALL TERMS IN EACH
C        EQUATION.  ERROR( , ) IS ACCUMULATED FOR ALL TERMS
C        INVOLVING 0597, 0697, 0797, OR 0897 IN THE 2ND WORD.
C
      DO 400 J=1,JDIM
C        THERE ARE A MAXIMUM OF JDIM TERMS IN THE EQUATION.
      DO 399 L=1,2
C        L=1 FOR LAND, 2 = WATER.
C
      IF(ITABLE(J,L,M)/10000.NE.0597.AND.
     1   ITABLE(J,L,M)/10000.NE.0697.AND.
     2   ITABLE(J,L,M)/10000.NE.0797.AND.
     3   ITABLE(J,L,M)/10000.NE.0897)GO TO 399
C        TRANSFER OUT IF THIS TERM IS NOT A ROUGHNESS  
C        PREDICTOR.
C
C           CHECK FOR LEGITIMACY OF PREDICTOR ID.  THIS ROUTINE
C           SHOULD NOT BE NECESSARY FOR WATER GRIDPOINTS.
C
      IF(L.EQ.2)THEN
         WRITE(KFILDO,102)(ID(MM),MM=1,4)
 102     FORMAT(/,' INCORRECT ID IN WATER EQUATION IN ROUGHG',
     1            ' FOR VARIABLE',3(1X,I9.9),1X,I10.3,
     2            '.  FATAL ERROR.')
         IER=777
         ISTOP=ISTOP+1
         GO TO 500
      ENDIF
C
C        THIS IS A TERM TO EVALUATE.
C
      ITERML=ITERML+1
C
C        RETRIEVE R FROM THE ID.
C
      IR=ITABLE(J,L,M)-(ITABLE(J,L,M)/10000)*10000
C        R IS THE DISTANCE TO DO THE SEARCH, TAKEN FROM THE ID.
      RSQ=IR*IR+.01
C        THE SMALL CONSTANT IS ADDED TO ASSURE A POINT IS NOT 
C        ELIMINATED BECAUSE OF ROUNDOFF, AND TO BE CONSISTENT WITH
C        OTHER SIMILAR ROUTINES.
C
      IF(NXE.NE.NX.OR.NYE.NE.NY)THEN
         WRITE(KFILDO,101)NXE,NYE,NX,NY
 101     FORMAT(/' ****NXE =',I5,' OR NYE =',I5,
     1           ' DOES NOT MATCH NX =',I5,' OR NY =',I5,
     2           ' AT 101 IN ROUGHG.  FATAL ERROR.')
         ISTOP=ISTOP+1
         IER=777
         GO TO 500
      ENDIF
C
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(JYY,IXX,JY,IX)
C
      DO 350 JYY=1,NY
      DO 349 IXX=1,NX
      TELEVA=0.
      NCOUNT=0
C
      IF(ERROR(IXX,JYY).GT.9998.)GO TO 349
C        THIS TEST IS MADE, BECAUSE THE ERROR MIGHT HAVE
C        BEEN SET TO MISSING BY SOME ROUTINE BECAUSE OF
C        MISSING DATA.
C
      IF(CPNDFD(IXX,JYY).GT..5.AND.SEALND(IXX,JYY).GT.8.5)THEN 
C           THIS GRIDPOINT IS WITHIN THE NDFD AREA AND OF TYPE
C           LAND.  DO THE COMPUTATIONS.
C
C           FOR EVERY LAND GRIDPOINT THAT IS WITHIN THE NDFD AREA,
C           FIND THE GRIDPOINTS WITHIN RADIUS R TO CALCULATE
C           THE ROUGHNESS.
C
         DO 200 JY=MAX(1,JYY-IR),MIN(NYE,JYY+IR)
         DO 199 IX=MAX(1,IXX-IR),MIN(NXE,IXX+IR)
C
         IF(SEALND(IX,JY).LT.8.5)GO TO 199
C           ONLY LAND POINTS ARE CONSIDERED.
C
         IF(((IX-IXX)**2+(JY-JYY)**2).LE.RSQ)THEN
            TELEVA=TELEVA+TELEV(IX,JY)
            NCOUNT=NCOUNT+1
            TEMP(NCOUNT)=TELEV(IX,JY)
C              SAVE THE VALUES SO A SEARCH WON'T HAVE TO BE
C              MADE AGAIN.
C
CD           IF(IXX.EQ.300.AND.JYY.EQ.400)THEN
CD              WRITE(KFILDO,198)IX,JY,IXX,JYY,RSQ,TELEVA,NCOUNT
CD198           FORMAT(' AT 198 IN ROUGHG--IX,JY,IXX,JYY,RSQ,TELEVA,',
CD    1                'NCOUNT',4I6,2F8.1,I6)
CD           ENDIF
 
          ENDIF
C
 199     CONTINUE
 200     CONTINUE
C
         TELEVA=TELEVA/NCOUNT
C           TELEVA IS THE AVERAGE ELEVATION WITHIN A CIRCLE OF RADIUS IR
C           AROUND THE POINT IXX,JYY.  IT CAN'T BE ZERO, BECAUSE THE
C           POINT ITSELF IS INCLUDED.
C
         ROUGH1=0.
C
         DO 300 IXY=1,NCOUNT
         ROUGH1=ROUGH1+ABS(TELEVA-TEMP(IXY))
C
CD        IF(IXX.EQ.300.AND.JYY.EQ.400)THEN
CD           WRITE(KFILDO,298)IX,JY,NCOUNT,ROUGH1,TELEVA,TEMP(IXY)
CD298        FORMAT(' AT 298 IN ROUGHG--',
CD    1             'IX,JY,NCOUNT,ROUGH1,TELEVA,TEMP(IXY)',
CD    2              3I5,3F10.1)
CD        ENDIF
C
 300     CONTINUE
C
         RUF=ROUGH1/NCOUNT
C           IF NCOUNT=1 AND RUF = 0, THIS IS A SPIT OF LAND OR A
C           VERY SMALL ISLAND, SO ROUGHNESS = 0 IS REASONABLE.
         ERROR(IXX,JYY)=ERROR(IXX,JYY)+RUF*RTABLE(J,L,M)
C
CD        IF(IXX.EQ.300.AND.JYY.EQ.400)THEN
CD           WRITE(KFILDO,310)J,L,M,IXX,JYY,RUF,RTABLE(J,L,M),
CD    1                       ERROR(IXX,JYY)
CD310        FORMAT(/,' AT 310 IN ROUGHG--J,L,M,IXX,JYY,RUF,',
CD    1               'RTABLE(J,L,M),ERROR(IXX,JYY)',/,
CD    2               5I6,3F10.4)
CD        ENDIF
C
      ENDIF
C
 349  CONTINUE
 350  CONTINUE
C
!$OMP END PARALLEL DO
C      
 399  CONTINUE
 400  CONTINUE
CD     CALL TIMPR(KFILDO,KFILDO,'END   ROUGHG        ')
 500  RETURN
      END      
