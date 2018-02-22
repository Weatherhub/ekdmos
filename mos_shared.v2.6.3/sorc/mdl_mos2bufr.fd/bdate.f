      SUBROUTINE BDATE(NSTA,KDATA,IOPT,SCALE,REF,ND1,NUMPRJ,LPROJ,
     *                 IVPRJ,NVPRJ,NDATE)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BDATE       STORES INITIAL DATE/TIME                   
C   PRGMMR: WEISS            ORG: W/OSD211   DATE: 01-01-26             
C                                                                       
C ABSTRACT: STORES INITIAL DATE/TIME INTO KDATA(,).  THE VALUE IN IOPT  
C           INDICATES WHETHER TO STORE THE YEAR, MONTH, DAY, OR         
C           CYCLE.  THE VALUE IS SCALED ACCORDINGLY.                    
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   93-10-22  GILBERT
C   01-01-26  WEISS   REVISED FOR MODIFIED BUFR CODE
C                                                                       
C USAGE:                                                                
C        JANUARY 2001    WEISS    MDL    IBM SP                         
C                                                                       
C        PURPOSE                                                        
C           STORES INITIAL DATE/TIME INTO KDATA(,).  THE VALUE IN IOPT 
C           INDICATES WHETHER TO STORE THE YEAR, MONTH, DAY, OR         
C           CYCLE.  THE VALUE IS SCALED ACCORDINGLY.                    
C                                                                       
C        DATA SET USE                                                   
C            NONE                                                       
C                                                                       
C        VARIABLES
C                NSTA = NUMBER OF STATIONS (INPUT).
C          KDATA(M,N) = HOLDS DATA FOR A FORECAST ELEMENT FOR EACH
C                       STATION AND EACH PROJECTION. 1ST DIMENSION IS
C                       # OF STATIONS, AND 2ND DIMENSION IS NUMBER OF
C                       PROJECTIONS.  CONTAINS MISSING "9999" FOR
C                       PROJECTION FOR WHICH FORECASTS ARE NOT VALID,
C                       M=1,ND1 AND N=1,NUMPRJ (OUTPUT).
C                IOPT = VALUE INDICATING WHETHER TO STORE YEAR, MONTH,
C                       DAY OR CYCLE (INPUT).
C               SCALE = BUFR TABLE B; SCALING VALUE (INPUT).
C                 REF = BUFR TABLE B; REFERENCE VALUE (INPUT).
C                 ND1 = MAXIMUM NUMBER OF STATIONS (INPUT).                    
C              NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE (INPUT).  
C            LPROJ(N) = LIST OF PROJECTIONS FOR WHICH FORECASTS ARE    
C                       ENCODED, N=1,NUMPRJ (INPUT).
C            IVPRJ(L) = CONTAINS VALID PROJECTIONS FOR THE CURRENT     
C                       FORECAST ELEMENT, L=1,NVPRJ (INPUT).
C               NVPRJ = NUMBER OF VALID PROJECTIONS (INPUT).
C               NDATE = INITIAL DATE OF FORECAST RUN (INPUT).
C
C        ADDITIONAL VARIABLES
C               IMULT = INTEGER SCALLING FACTOR (=1) (INTERNAL).
C                IREF = INTEGER REFERENCE NUMBER (INTERNAL). 
C               ITEMP = SCALLED TIME VARIABLE (THE VALUE STAYS THE SAME)
C                       (INTERNAL). 
C               NTEMP = UNSCALLED TIME VARIABLE (INTERNAL).
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  IBM SP                                                       
C                                                                       
C$$$
      IMPLICIT NONE
C
      INTEGER NSTA,KDATA(ND1,NUMPRJ),IOPT,ND1,NUMPRJ,
     *        LPROJ(NUMPRJ),IVPRJ(NVPRJ),NVPRJ,NDATE
      INTEGER IMULT,IREF,ITEMP,NTEMP,N,L,I
C
      REAL SCALE,REF
C
      IMULT=10**NINT(SCALE)
      IREF=NINT(REF)
      N=1
C        YEAR
      IF (IOPT.EQ.7) THEN 
	NTEMP=NDATE/1000000
C        MONTH
      ELSEIF (IOPT.EQ.8) THEN
        NTEMP=MOD(NDATE,1000000)/10000
C        DAY
      ELSEIF (IOPT.EQ.9) THEN
        NTEMP=MOD(NDATE,10000)/100
C        CYCLE
      ELSEIF (IOPT.EQ.10) THEN
        NTEMP=MOD(NDATE,100)
      ENDIF
      ITEMP=(NTEMP*IMULT)-IREF
C
C        DO FOR EACH PROJECTION                                         
      DO 200 L=1,NUMPRJ
C        IF NOT A SPECIFIED PROJECTION, INSERT MISSING (9999).          
        IF (N.GT.NVPRJ) N=1
        IF (LPROJ(L).EQ.IVPRJ(N)) THEN
C        ADD SCALED SPECIFIED VALUE TO KDATA(,) FOR EACH STATION        
          DO 100 I=1,NSTA
            KDATA(I,L)=ITEMP
 100      CONTINUE
          N=N+1
        ELSE
          DO 150 I=1,NSTA
            KDATA(I,L)=9999
 150      CONTINUE
        ENDIF
 200  CONTINUE
      RETURN
      END
