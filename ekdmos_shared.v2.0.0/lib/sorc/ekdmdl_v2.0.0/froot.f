      SUBROUTINE FROOT(KFILDO,K,ND1,YDATA,THRESH,NPROB,BETA)
C
C        OCTOBER 1999   RUDACK TDL   MOS-2000
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                     (OUTPUT) 
C
C         PURPOSE: 
C                 FINDS AN INTERVAL IN WHICH THE ROOT (BETA) OF FUNCTION
C                 "F" EXISTS AND THEN USES THE BISECTION METHOD TO ESTIMATE
C                 THE ROOT(BETA).
C
C         DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                     (OUTPUT) 
C 
C         VARIABLES 
C             KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                      (INPUT)
C                  K = STATION COUNTER (K=1,ND1). (INPUT)
C                ND1 = NUMBER OF TOTAL STATIONS (K=1,ND1). (INPUT)
C         YDATA(K,J) = ADJUSTED PROBABILITIES (K=1,ND1) (J=1,6). (INPUT)
C          THRESH(L) = PRECIPITATION AMOUNTS FOR WHICH PROBABILITIES EXIST. 
C                      (L=1,5 FOR 6-HR PROJECTION) (L=1,6 FOR 12-HR & 24-HR
C                      PROJECTION).  (INPUT)
C              NPROB = NUMBER OF ADJUSTED PROBABILITIES USED TO DETERMINE THE
C                      WEIBULL DISTRIBUTION.  (INPUT)
C               BETA = VALUE OF BETA. (OUTPUT)
C             ERRABS = VALUE USED AS STOPPING CRITERIA FOR THE BISECTION
C                      METHOD. (INTERNAL)
C              ITMAX = MAXIMUM NUMBER OF ITERATIONS OF THE BISECTION METHOD
C                      ALLOWED. (INTERNAL) 
C                  A = LEFTMOST ENDPOINT OF THE INTERVAL. (INTERNAL)
C                 FA = THE RETURNED VALUE FOR THE FUNCTION F(A). (INTERNAL)
C                  B = RIGHTMOST ENDPOINT OF THE INTERVAL. (INTERNAL)
C                 FB = THE RETURNED VALUE FOR THE FUNCTION F(B). (INTERNAL)
C
C          NONSYSTEM SUBROUTINES CALLED 
C                   GAMMA
C 
      DATA ERRABS/0.0001/
      DATA ITMAX/50/
C
C      DIMENSION YDATA(ND1,6),THRESH(6)
C
C        FIND INTERVAL IN WHICH ROOT (BETA) IS KNOWN TO EXIST.
C
      A=0.01
      B=0.0
C
      CALL GAMMA(KFILDO,K,ND1,YDATA,THRESH,NPROB,A,FA)
C         
      DO 100 J=1,20
C
         B=B+0.2
C
         CALL GAMMA(KFILDO,K,ND1,YDATA,THRESH,NPROB,B,FB)
C
         IF((FA*FB).LT.0.0) GOTO 200
C        A=B
         FA=FB
C
 100     CONTINUE
C
C        IF YOU FALL THROUGH THE LOOP, NO INTERVAL WAS FOUND WHERE THE
C        FUNCTION HAS OPPOSITE SIGNS.
C
      WRITE(KFILDO,150) 
 150  FORMAT(/,' ROOT NOT FOUND IN "FROOT". PROPER INTERVAL NOT FOUND.')
C
      N=9999
      BETA=9999.0
      RETURN
C
C        FIND ROOT(BETA) USING BISECTION METHOD.
C       
 200  N=1
 
C
 300  IF(N.LE.ITMAX) THEN
         BETA=A+((B-A)/2.)
C
      CALL GAMMA(KFILDO,K,ND1,YDATA,THRESH,NPROB,BETA,Y)
C     

      IF(ABS(Y).LT.ERRABS) RETURN
C
      N=N+1
      IF((FA*Y).GT.0.0)THEN
         A=BETA
         FA=Y
C
      ELSE
C    
         B=BETA
         FB=Y
C
      ENDIF
C
      GOTO 300
C
      ELSE
C
         WRITE(KFILDO,320) N
 320     FORMAT(/,'FAILED TO FIND BETA AFTER ',I2,' ITERATIONS.')
C
         BETA=9999.0
         RETURN
C
      ENDIF
C
      END
     
    
