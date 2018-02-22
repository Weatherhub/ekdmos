      SUBROUTINE CAT2(KFILDO,FCST,AVG,CORR,
     1                LOCSTA,ND1,ND3,ND13,NSTA,
     2                KGP,NGP,LGP,MTANDS,
     3                ICAT,IER)
C
C        DECEMBER  1998   GLAHN   TDL   MOS-2000
C        MARCH     1999   GLAHN   MINOR REARRANGEMENT OF CALL
C        MAY       1999   GLAHN   NOW CALLED FOR EACH CATEGORY
C        MARCH     2000   SMB     MODIFIED FORMAT STATEMENTS TO
C                                 CONFORM TO FORTRAN 90 
C                                 STANDARDS ON IBM SP
C        APRIL     2006   RUDACK  CREATED CAT2 BY MODIFYING CAT1 TO
C                                 ALLOW FOR ICAT=2. WHEN ICAT=2, DO
C                                 PARTIAL INFLATION, I.E., INFLATE
C                                 ONLY WHEN THE FORECAST VALUE EXCEEDS
C                                 THE AVERAGE 
C
C        PURPOSE
C           TO INFLATE FORECASTS FOR U700 AND U900.  IT IS ASSUMED 
C           THE PRIMARY MISSING VALUE = 9999 AND THE SECONDARY MISSING
C           VALUE = 9997. THIS CODE DOES PARTIAL INFLATION ONLY.
C   
C        DATA SET USE
C            KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C             FCST(K) = THE FORECASTS FOR STATION K (K=1,KSTA);
C                       INFLATED ON OUTPUT.  (INPUT/OUTPUT)
C              AVG(L) = THE PREDICTAND MEANS FOR GROUP L (L=1,KGP).
C                       (INPUT)
C             CORR(L) = THE MULTIPLE CORRELATIONS FOR GROUP L
C                       (L=1,KGP).  (INPUT)
C           LOCSTA(K) = THE LOCATION IN FCST( ) OF WHERE TO PUT THE 
C                       FORECAST, WHERE K IS IN ORDER OF THE EQUATIONS
C                       AS READ.  (INPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                       WITH.  USED AS DIMENSION OF LOCSTA( ) AND 
C                       FIRST DIMENSION OF FCST( , ).  (INPUT)
C                 ND3 = MAXIMUM NUMBER OF PREDICTANDS IN ANY EQUATION.
C                       USED AS SECOND DIMENSION OF AVG( , ),
C                       CORR( , ), AND FCST( , ).SEVERAL VARIABLES. 
C                       (INPUT)
C                ND13 = MAXIMUM NUMBER OF DIFFERENT EQUATIONS.
C                       THIS WOULD = ND1 FOR SINGLE STATION EQUATIONS,
C                       BUT MIGHT BE ON THE ORDER OF 30 FOR REGIONAL
C                       EQUATIONS.  DIMENSION OF NGP( ) AND LGP( ) 
C                       AND FIRST DIMENSION OF AVT( , ) AND CORR( , ).
C                       (INPUT)
C                NSTA = THE NUMBER OF STATIONS IN CCALL( ).  (INPUT)
C                 KGP = THE NUMBER OF GROUPS OF EQUATIONS.
C                       (INPUT)
C              NGP(L) = THE NUMBER OF EQUATIONS IN EACH GROUP L
C                       (L=1,KGP).  (INPUT)
C              LGP(L) = FOR EACH EQUATION (L=1,KGP), THE LOCATION 
C                       IN LOCSTA ( ) OF WHERE THE FIRST STATION
C                       IN THE GROUP IS.  (INPUT)
C              MTANDS = THE NUMBER OF PREDICTANDS FOR THIS EQUATION
C                       SET.  (INPUT)
C                ICAT = THE CATEGORY NUMBER OF THIS EQUATION SET AND
C                       PREDICTAND TO INDICATE INFLATION = 2.  (INPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       170 = ICAT DOES NOT EQUAL 2.
C                       (OUTPUT)
C 
C        NONSYSTEM SUBROUINES USED 
C            NONE
C
      DIMENSION LOCSTA(ND1)
      DIMENSION NGP(ND13),LGP(ND13)
      DIMENSION AVG(ND13),
     2          CORR(ND13)
      DIMENSION FCST(ND1)
C
      IER=0
C
      IF(ICAT.NE.2)THEN
         WRITE(KFILDO,110)ICAT
 110     FORMAT(/,' ****ICAT =',I4,' DOES NOT EQUAL 2',
     1            ' IN CAT2.  FORECASTS ARE NOT INFLATED.')
         IER=170
         GO TO 210
      ENDIF
C
      DO 200 L=1,KGP
C
      DO 199 KK=1,NGP(L)
      K=LOCSTA(LGP(L)+KK-1)
C
      IF(FCST(K).EQ.9999..OR.
     1   FCST(K).EQ.9997.)GO TO 199
C
C        FOR PARTIAL INFLATION (ICAT=2) MODIFY THE FORECAST VALUE
C        ONLY WHEN THE ORIGINAL VALUE EXCEEDS THE AVERAGE VALUE
C
      IF(FCST(K).GE.AVG(L))THEN
         FCST(K)=(FCST(K)-AVG(L))/CORR(L)+AVG(L)
      ENDIF
 199  CONTINUE
C
 200  CONTINUE
C
 210  RETURN
      END
