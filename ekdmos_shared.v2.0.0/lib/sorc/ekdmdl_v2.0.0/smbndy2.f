      SUBROUTINE SMBNDY2(KFILDO,GRD1,NX,NY,IBNDCT,IBNDPT,
     1                  BNDLAT,BNDLON,IBNSTA,ITAU)
C
C        APRIL    2006  CHARBA  MDL   MOS-2000
C        JUNE     2006  CHARBA  MADE CHANGES TO MEET MOS2000 STANDARDS.
C                               ALSO, REMOVED GRD2 AND ADDED IER TO
C                               ARGUMENT LIST.
C        JUNE     2006  CHARBA  UPGRADED PURPOSE AND MADE COSMETIC 
C                               CHANGES TO SATISFY CODE WALK-THRU.
C        JULY     2006  CHARBA  REPLACED NINT( ) WITH GE FMISS IN CHECKS
C                               FOR MISSING VALUES.
C        AUGUST   2006  CHARBA  INCREASED ISMST FROM 15 TO 30 AND RE-
C                               MOVED IER (IT IS NO LONGER APPLICABLE).
C                               IF ISMST WERE TO BE EXCEEDED, WHICH IS
C                               EXTREMELY UNLIKELY, THE SMOOTHING WOULD
C                               NOT BE COMPLETE FOR ONE OR MORE BOUNDARY
C                               POINTS.  IF THIS WERE TO OCCUR, HOWEVER,
C                               A MESSAGE WOULD BE ISSUED, BUT THE
C                               NEGATIVE IMPACT WOULD LIKELY NOT BE
C                               NOTICEABLE.
C
C        PURPOSE
C           TO PERFORM SPATIAL SMOOTHING OF FORECAST PROBABILITIES FOR
C           POINTS ALONG COMMON REGION BOUNDARIES WHERE THE MAGNITUDE 
C           OF THE HORIZONTAL GRADIENT EQUALS OR EXCEEDS A PRESCRIBED 
C           THRESHOLD VALUE.  THE SMOOTHING FOR A BOUNDARY POINT IS
C           ACTUALLY PERFORMED OVER A 4X4 WINDOW THAT IS QUASI-CENTERED
C           ON THE POINT.  THE SMOOTHING WITHIN THE WINDOW IS VARIABLE
C           SUCH THAT THE GREATEST SMOOTHING OCCURS AT THE BOUNDARY 
C           POINT, WITH THE SMOOTHING GRADUALLY DECREASING OUTWARD FROM
C           THAT POINT.  THE SMOOTHING OPERATOR CONSISTS OF A NINE-POINT
C           WEIGHTED AVERAGE, AND A SMOOTHING INDEX SPECIFIES THE WEIGHT
C           FOR EACH POINT.  THE SMOOTHING INDEX FOR EACH POINT OF THE
C           4X4 WINDOW IS PROVIDED IN A DATA STATEMENT.  NOTE:  THE 
C           SPATIAL GRADIENT IS COMPUTED IN BOTH A POSITIVE AND NEGATIVE
C           DIRECTION FROM THE BOUNDARY POINT, AND THE ASSOCIATED 
C           SMOOTHING WINDOWS ARE SLIGHTLY DIFFERENT FOR EACH OF THEM.  
C           THUS, SEPARATE SETS OF SMOOTHING INDEX VALUES APPLY TO THE 
C           TWO SMOOTHING WINDOWS.  
C
C           SMBNDY2 IS AN ADAPTED VERSION OF SMBNDY. IT PERFORMS ADDI-
C           TIONAL SMOOTHING ON THE REGIONAL BOUNDARIES BEYOND WHAT IS 
C           DONE PREVIOUSLY BY SMBNDY.  THE DIFFERENCES FROM SMBNDY ARE:
C           (1) IT SMOOTHS ONLY LOW PROBABILITY VALUES 
C           (2) IT OPERATES ON ALL FORECAST PROJECTIONS
C           (3) IT PERFORMS UP TO TWO PASSES ONLY
C           (4) IT USES ONLY A SINGLE GRADIENT THRESHOLD VALUE, WHICH 
C               IS LOWER THAN ANY OF THE MULTIPLE THRESHOLD VALUES USED 
C               IN SMBNDY.
C                
C        DATA SET USE
C           KFILDO - UNIT NUMBER FOR DIAGNOSTIC OUTPUT.
C
C        VARIABLES
C              KFILDO = UNIT NUMBER FOR DIAGNOSTIC OUTPUT.  (INPUT)
C           GRD1(I,J) = HOLDS THE RAW PROBABILITIES ON INPUT AND THE
C                       AND THE SMOOTHED VALUES ON OUTPUT 
C                       (I=1,NX,J=1,NY).  (INPUT/OUTPUT)
C                  NX = NUMBER OF GRIDPOINTS IN THE X-DIRECTION, AND THE
C                       FIRST DIMENSION OF GRD1( , ).
C                       (INPUT)
C                  NY = NUMBER OF GRIDPOINTS IN THE Y-DIRECTION, AND THE
C                       SECOND DIMENSION OF GRD1( , ).
C                       (INPUT)
C              IBNDCT = NUMBER OF BOUNDARY POINTS TO PERFORM
C                       SMOOTHING ON.  (INPUT)
C         IBNDPT(K,L) = CONTAINS X- (L=1) AND Y- (L=2) COORDINATES OF 
C                       GRIDPOINTS THAT LIE ON REGION COMMON BOUNDARIES 
C                       (K=1,IBNSTA).  (INPUT)
C           BNDLAT(K) = CONTAINS LATITUDES OF GRIDPOINTS THAT LIE ON 
C                       REGION COMMON BOUNDARIES (K=1,IBNSTA).  (INPUT)
C           BNDLON(K) = CONTAINS LONGITUDES OF GRIDPOINTS THAT LIE ON 
C                       REGION COMMON BOUNDARIES (K=1,IBNSTA).  (INPUT)
C              IBNSTA = DIMENSION OF IBNDPT(K,L), BNDLAT(K), AND 
C                       BNDLON(K) (K=1,IBNSTA).  (INPUT)
C                ITAU = FORECAST PROJECTION FOR PROBABILITY FIELD 
C                       ...USED TO ADJUST THRESH (SEE BELOW).  (INPUT) 
C     OTHER VARIABLES   
C              THRESH = GRADIENT THRESHOLD VALUE FOR WHICH SMOOTHING
C                       FOR A POINT IS PERFORMED.  (INTERNAL)
C               RMISS = MISSING VALUE.  (INTERNAL)
C               FMISS = USED IN CHECKS FOR MISSING VALUE (=9998.5).
C                       (INTERNAL)
C               NSIZE = NUMBER OF POINTS IN THE SMOOTHING WINDOW.
C                       (INTERNAL)
C               ISMST = MAXIMUM NUMBER OF SMOOTHED VALUES TO STORE
C                       IN WSMIDX( , ,K) AND WSMVAL( , ,K) (K=1,ISMST).
C                       (INPUT)
C         IPOFFSET(J) = GRIDPOINT OFFSETS IN THE X-DIRECTION THAT
C                       DEFINE THE SMOOTHING WINDOW ASSOCIATED WITH THE
C                       POSITIVE GRADIENT DIRECTION (J=1,NSIZE).
C                       (INTERNAL)
C         JPOFFSET(J) = GRIDPOINT OFFSETS IN THE Y-DIRECTION THAT
C                       DEFINE THE SMOOTHING WINDOW ASSOCIATED WITH THE
C                       POSITIVE GRADIENT DIRECTION (J=1,NSIZE).
C                       (INTERNAL)
C         INOFFSET(J) = GRIDPOINT OFFSETS IN THE X-DIRECTION THAT
C                       DEFINE THE SMOOTHING WINDOW ASSOCIATED WITH THE
C                       NEGATIVE GRADIENT DIRECTION (J=1,NSIZE).
C                       (INTERNAL)
C         JNOFFSET(J) = GRIDPOINT OFFSETS IN THE Y-DIRECTION THAT
C                       DEFINE THE SMOOTHING WINDOW ASSOCIATED WITH THE
C                       NEGATIVE GRADIENT DIRECTION (J=1,NSIZE).
C                       (INTERNAL)
C           SMPIDX(J) = SMOOTHING WEIGHTS FOR SPATIAL GRADIENTS IN 
C                       THE POSITIVE DIRECTION (J=1,NSIZE).  (INTERNAL)
C           SMNIDX(J) = SMOOTHING WEIGHTS FOR SPATIAL GRADIENTS IN 
C                       THE NEGATIVE DIRECTION (J=1,NSIZE).  (INTERNAL)
C           ICNT(I,J) = CONTAINS THE NUMBER OF TIMES A GRIDPOINT HAS 
C                       BEEN SMOOTHED (I=1,NX,J=1,NY).  (INTERNAL)
C       WSMIDX(I,J,K) = CONTAINS THE SMOOTHING INDEX FOR EACH
C                       TIME SMOOTHING IS PERFORMED AT A GRIDPOINT 
C                       (I=1,NX,J=1,NY,K=1,ISMST).  (INTERNAL)
C       WSMVAL(I,J,K) = CONTAINS THE SMOOTHED VALUE FOR EACH TIME
C                       SMOOTHING IS PERFORMED AT A GRIDPOINT. 
C                       (I=1,NX,J=1,NY,K=1,ISMST).  (INTERNAL)
C              NDSCNT = COUNTER FOR COMMON BOUNDARY GRIDPOINTS THAT 
C                       WERE SMOOTHED.  (INTERNAL)
C              MXPASS = MAXIMUM NUMBER OF PASSES FOR WHICH SMOOTHING IS
C                       PERFORMED.  (INTERNAL)
C
C        NON-SYSTEM SUBROUTINES CALLED
C           SM9V1P
C
      PARAMETER(NSIZE=16,ISMST=30)
C
      DIMENSION GRD1(NX,NY),IPOFFSET(NSIZE),INOFFSET(NSIZE),
     1   JPOFFSET(NSIZE),JNOFFSET(NSIZE),ICNT(NX,NY),SMPIDX(NSIZE),
     2   SMNIDX(NSIZE),WSMVAL(NX,NY,ISMST),WSMIDX(NX,NY,ISMST),
     3   IBNDPT(IBNSTA,2),BNDLAT(IBNSTA),BNDLON(IBNSTA)
C
      DATA IPOFFSET/-1,0,1,2,-1,0,1,2,-1,0,1,2,-1,0,1,2/
      DATA JPOFFSET/-1,-1,-1,-1,0,0,0,0,1,1,1,1,2,2,2,2/
      DATA SMPIDX/0.40,0.50,0.50,0.40,0.50,1.00,1.00,0.50,0.50,1.00,
     1            0.70,0.40,0.40,0.50,0.40,0.30/
C
      DATA INOFFSET/-2,-1,0,1,-2,-1,0,1,-2,-1,0,1,-2,-1,0,1/
      DATA JNOFFSET/-2,-2,-2,-2,-1,-1,-1,-1,0,0,0,0,1,1,1,1/
      DATA SMNIDX/0.30,0.40,0.50,0.40,0.40,0.70,1.00,0.50,0.50,1.00,
     1            1.00,0.50,0.40,0.50,0.50,0.40/
C  
      DATA MXPASS/2/,THRESH/0.020/,PRBLOW/0.20/,FMISS/9998.5/,
     1     RMISS/9999./
C
      NPASS=1
C
C        PERFORM BOUNDARY SMOOTHING FOR CURRENT PASS (TOP OF LOOP FOR
C        SECOND PASS IS LABEL 10).  FIRST, INITIALIZE WORK ARRAYS.
C
 10   DO 30 J=1,NY
        DO 20 I=1,NX
          ICNT(I,J)=0
 20     CONTINUE 
 30   CONTINUE
C
      DO 60 M=1,ISMST
        DO 50 J=1,NY
          DO 40 I=1,NX
            WSMVAL(I,J,M)=RMISS
            WSMIDX(I,J,M)=RMISS
 40       CONTINUE
 50     CONTINUE 
 60   CONTINUE
C
C        LOOP THROUGH EACH GRIDPOINT IN IBNDPT( , ) (DO 500 LOOP).
C
      NDSCNT=0
C
      DO 500 N=1,IBNDCT
        IPT=IBNDPT(N,1)
        JPT=IBNDPT(N,2)
D       WRITE(KFILDO,65) IPT,JPT,BNDLAT(N),BNDLON(N)
D65     FORMAT(/,' COMMON BOUNDARY POINT I J LAT LON = ',2I4.4,2F9.3) 
C
C          THE BOUNDARY POINT IS NOT SMOOTHED WHEN THE PROBABILITY
C          IS GREATER THAN PRBLOW.
C
        IF(GRD1(IPT,JPT).GT.PRBLOW) GO TO 500
C
C          AT THE BOUNDARY POINT, CALCULATE MAGNITUDE OF THE PROBABILITY
C          GRADIENT, BOTH IN THE POSITIVE AND NEGATIVE DIRECTIONS.
C
        IF(GRD1(IPT+1,JPT).GE.FMISS.OR.GRD1(IPT,JPT).GE.FMISS
     1                             .OR.GRD1(IPT,JPT+1).GE.FMISS) THEN
           RMAGP=RMISS
        ELSE
           RMAGP=(((GRD1(IPT+1,JPT)-GRD1(IPT,JPT))**2 +
     1             (GRD1(IPT,JPT+1)-GRD1(IPT,JPT))**2)/2.0)**0.5
        ENDIF
C
        IF(GRD1(IPT,JPT).GE.FMISS.OR.GRD1(IPT-1,JPT).GE.FMISS
     1                           .OR.GRD1(IPT,JPT-1).GE.FMISS) THEN
           RMAGN=RMISS
        ELSE
           RMAGN=(((GRD1(IPT,JPT)-GRD1(IPT-1,JPT))**2 +
     1             (GRD1(IPT,JPT)-GRD1(IPT,JPT-1))**2)/2.0)**0.5
        ENDIF
C
C          IF RMAGP GE THRESH, APPLY SMOOTHING TO THE APPROPRIATE 4X4
C          WINDOW.
C
        IF(RMAGP.NE.RMISS.AND.RMAGP.GE.THRESH) THEN
          NDSCNT=NDSCNT+1
D         WRITE(KFILDO,70) NDSCNT,IPT,JPT,BNDLAT(N),BNDLON(N),
D    1                     GRD1(IPT,JPT),RMAGP,THRESH,NPASS
D70       FORMAT('NDSCNT IPT JPT LAT LONG PROB RMAGP THRESH PASS = ',
D    1           I3,2X,2I4.4,2F7.2,5X,3F7.3,I4)
          DO 100 K=1,NSIZE
            IPT1=IPT+IPOFFSET(K)
            JPT1=JPT+JPOFFSET(K)
C
            CALL SM9V1P(GRD1,NX,NY,SMPIDX(K),FMISS,IPT1,JPT1,SMTHPT)
C
C              INCREMENT THE COUNTER ARRAY AND STORE THE SMOOTHING INDEX
C              AND SMOOTHED VALUE.     
C
            IF(ICNT(IPT1,JPT1).LT.ISMST) THEN
              ICNT(IPT1,JPT1)=ICNT(IPT1,JPT1)+1
              WSMIDX(IPT1,JPT1,ICNT(IPT1,JPT1))=SMPIDX(K)
              WSMVAL(IPT1,JPT1,ICNT(IPT1,JPT1))=SMTHPT
            ELSE
C
C                WRITE AN ERROR MESSAGE AND ABORT WHEN MORE THAN ISMST 
C                SMOOTHINGS ARE ATTEMPTED.
C
              WRITE(KFILDO,80) ISMST,IPT1,JPT1,IPT,JPT
 80           FORMAT(//,' **** IN SMBNDY2 EXCEEDED THE MAXIMUM OF ',I2,
     1                  ' SMOOTHINGS ALLOWED AT THE POINT ',2I4.4,
     2                  ' WHILE SMOOTHING FOR THE POSITIVE GRADIENT',
     3                  ' WITH THE WINDOW CENTERED AT ',2I4.4,'.',/,
     4                  ' PART 2 OF BOUNDARY SMOOTHING NOT COMPLETE ',
     5                  ' FOR POINT')
            ENDIF
C
 100      CONTINUE
C
        ENDIF
C
C          IF RMAGN GE THRESH, APPLY SMOOTHING TO THE APPROPRIATE 4X4
C          WINDOW.
C
        IF(RMAGN.NE.RMISS.AND.RMAGN.GE.THRESH) THEN
          NDSCNT=NDSCNT+1
D         WRITE(KFILDO,160) NDSCNT,IPT,JPT,BNDLAT(N),BNDLON(N),
D    1                      GRD1(IPT,JPT),RMAGN,THRESH,NPASS
D160      FORMAT('NDSCNT IPT JPT LAT LONG PROB RMAGN THRESH PASS = ',
D    1           I3,2X,2I4.4,2F7.2,5X,3F7.3,I4)
          DO 200 K=1,NSIZE
            IPT1=IPT+INOFFSET(K)
            JPT1=JPT+JNOFFSET(K)
C
            CALL SM9V1P(GRD1,NX,NY,SMNIDX(K),FMISS,IPT1,JPT1,SMTHPT)
C
C              INCREMENT THE COUNTER ARRAY AND STORE THE SMOOTHING INDEX
C              AND SMOOTHED VALUE.     
C
            IF(ICNT(IPT1,JPT1).LT.ISMST) THEN
              ICNT(IPT1,JPT1)=ICNT(IPT1,JPT1)+1
              WSMIDX(IPT1,JPT1,ICNT(IPT1,JPT1))=SMNIDX(K)
              WSMVAL(IPT1,JPT1,ICNT(IPT1,JPT1))=SMTHPT
            ELSE
C
C                WRITE AN ERROR MESSAGE AND ABORT WHEN MORE THAN ISMST 
C                SMOOTHINGS ARE ATTEMPTED.
C
              WRITE(KFILDO,180) ISMST,IPT1,JPT1,IPT,JPT
 180          FORMAT(//,' **** IN SMBNDY2 EXCEEDED THE MAXIMUM OF ',I2,
     1                  ' SMOOTHINGS ALLOWED AT THE POINT ',2I4.4,
     2                  ' WHILE SMOOTHING FOR THE NEGATIVE GRADIENT',
     3                  ' WITH THE WINDOW CENTERED AT ',2I4.4,'.',/,
     4                  ' PART 2 OF BOUNDARY SMOOTHING NOT COMPLETE ',
     5                  ' FOR POINT')
            ENDIF
C     
 200      CONTINUE
C
        ENDIF
C
 500  CONTINUE
C
C        IF NO POINTS ON COMMON BOUNDARIES WERE SMOOTHED FOR THIS PASS,
C        THEN RETURN.
C
      IF(NDSCNT.EQ.0 ) GO TO 900
C
C        NOW THAT ALL SMOOTHING IS COMPLETED, FOR EACH POINT FIND THE 
C        HIGHEST SMOOTHING INDEX USED TO CALCULATE A SMOOTHED VALUE
C        FOR THAT POINT.  WHEN THAT INDEX IS FOUND, THE CORRESPONDING
C        SMOOTHED VALUE IS PLACED INTO THE GRD1( , ) ARRAY.
C
      DO 600 J=1,NY
      DO 580 I=1,NX
        RMAX=-9999.
        IF(ICNT(I,J).NE.0) THEN
          DO 550 M=1,ICNT(I,J)
            IF(WSMIDX(I,J,M).GE.RMAX) THEN
              RMAX=WSMIDX(I,J,M)
              SMVAL=WSMVAL(I,J,M)
            END IF
 550      CONTINUE
C        
C            DIAGNOSTICS
C
D         WRITE (KFILDO,560) I,J,ICNT(I,J),GRD1(I,J),RMAX,SMVAL
D560      FORMAT('FOR I J = ',2I4.4,' # SMOOTH VALUES = ',I3,
D    1           ' RAW VALUE = ',F9.3,' HIGHEST SMOOTHING INDEX = ',
D    2           F5.2,' SMOOTHED VALUE SAVED = ',F9.3)
C
          GRD1(I,J)=SMVAL
C
        ENDIF
 580  CONTINUE
 600  CONTINUE
C
C        INCREMENT PASS NUMBER AND PERFORM ANOTHER SMOOTHING PASS WHEN
C        MAX NUMBER OF PASSES NOT EXCEEDED.
C
      NPASS=NPASS+1
      IF(NPASS.LE.MXPASS) GO TO 10
C
 900  RETURN
      END
