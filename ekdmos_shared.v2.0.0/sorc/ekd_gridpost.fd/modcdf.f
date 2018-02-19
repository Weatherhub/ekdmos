      SUBROUTINE MODCDF(KFILDO,KFIL10,DATAMEM,DATACDF,NMEM,RCNT,ND2X3,
     1                  NCDF,ND5)
C
C	 AUG	   2010      WAGNER    MDL       MOS 2000
C
C        PURPOSE
C            TO COMPUTE SIMPLE CDFS FOR MODEL VARIABLES TO BE USED
C            AS A FIRST GUESS AROUND ALASKA
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. 
C		      (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. 
C		        (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C   DATAMEM(ND5,NMEM) = HOLDS THE ENSEMBLE MEMBERS FOR EACH STATION.
C                       (INPUT)
C   DATACDF(ND5,NCDF) = HOLDS THE MODEL VALUES ASSOCIATED WITH EACH
C                       PROBABILITY LEVEL. (OUTPUT)
C               NMEM  = THE MAX NUMBER OF ENSEMBLE MEMBERS. (INPUT)
C         RCNT(ND2X3) = THE NUMBER OF NON-MISSING ENSEMBLE MEMBERS
C                       FOR EACH STATION. (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF THE
C                       GRID IS NOT KNOWN BEFORE THE HEIGHTS ARE
C                       FETCHED.  (INPUT)
C                NCDF = NUMBER OF POINTS ON THE CDF TO CALCULATE.
C                       (INPUT)
C                 ND5 = DIMENSION OF IPACK( ),IWORK( ), AND THETAE( ).
C			(INPUT)
C        PTABLE(NCDF) = TABLE OF CDF VALUES. (INTERNAL)
C                PGAP = THE PERCENTILE DIFFERENCE BETWEEN ENSEMBLE
C                       MEMBERS. (INTERNAL)
C       CDFVALS(NCDF) = GENERATED CDF VALUES FOR A STATION. (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C      PARAMETER (NMEM=21)
C      PARAMETER (NCDF=11)
C
      DIMENSION DATAMEM(ND5,NMEM),DATACDF(ND5,NCDF)
      DIMENSION RCNT(ND2X3),CDFVALS(NMEM)
      DIMENSION PTABLE(11) 
C      DIMENSION PTABLE(NCDF) 
C                        
      DATA PTABLE/.05,
     1            .1,
     2            .2,
     3            .3,
     4            .4,
     5            .5,
     6            .6,
     7            .7,
     8            .8,
     9            .9,
     A            .95/
C
C
      DO 100 I=1,ND2X3
      DO 1001 J=1,NMEM
         CDFVALS(J)=9999.
 1001 CONTINUE
C
C	IF WE HAVE NO MEMBERS, SET DATACDF TO MISSING
         IF (RCNT(I).EQ.0.)THEN
            DO 102 K=1,NCDF
               DATACDF(I,K)=9999.
 102        CONTINUE
	   GO TO 100
         ENDIF
C
C        SORT THE VALUES FROM LOWEST TO HIGHEST
C
         NMOVE=1
 110     IF(NMOVE.GT.0)THEN
            NMOVE=0
            DO 120 J=1,RCNT(I)-1
               IF(DATAMEM(I,J+1).LT.DATAMEM(I,J))THEN
                  X1=DATAMEM(I,J)
                  X2=DATAMEM(I,J+1)
                  DATAMEM(I,J)=X2
                  DATAMEM(I,J+1)=X1
                  NMOVE=1
               ENDIF
 120        CONTINUE
            GO TO 110
         ENDIF
            
C
C        CALCULATE THE MODEL VALUES FOR EACH CDF VALUE
C
         PGAP=1/RCNT(I)
         DO 125 K=1,RCNT(I)
            CDFVALS(K)=PGAP*K
 125     CONTINUE
C
         DO 130 K=1,NCDF
         DO 131 J=1,RCNT(I)-1
            IF((PTABLE(K).LE.PGAP).OR.((PTABLE(K).GT.CDFVALS(J)).
     1              AND.(PTABLE(K).LE.CDFVALS(J+1))))THEN
               IF(DATAMEM(I,K+1).NE.DATAMEM(I,K)) THEN
                  SLOPE=((CDFVALS(J+1)-CDFVALS(J))/
     1                    (DATAMEM(I,K+1)-DATAMEM(I,K)))
                  YINT=CDFVALS(J)-SLOPE*DATAMEM(I,K)
                  DATACDF(I,K)=(PTABLE(K)-YINT)/SLOPE
               ELSE
                  DATACDF(I,K)=DATAMEM(I,K)
               ENDIF
            ENDIF
 131     CONTINUE
            IF(PTABLE(K).GT.CDFVALS(RCNT(I)))THEN
C        WE'RE IN THE RIGHT TAIL
               IF(DATAMEM(I,K).NE.DATAMEM(I,K-1))THEN
                  SLOPE=((CDFVALS(J)-CDFVALS(J-1))/
     1                 (DATAMEM(I,K)-DATAMEM(I,K-1)))
                  YINT=CDFVALS(J)-SLOPE*DATAMEM(I,K)
                  DATACDF(I,K)=(PTABLE(K)-YINT)/SLOPE
               ELSE
                  DATACDF(I,K)=DATAMEM(I,K)
               ENDIF
            ENDIF
C
 130     CONTINUE
 100  CONTINUE
C
      RETURN
      END
