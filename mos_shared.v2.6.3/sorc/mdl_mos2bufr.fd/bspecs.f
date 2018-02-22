      SUBROUTINE BSPECS(NSTA,KDATA,SCALE,REF,ND1,NUMPRJ,LPROJ,
     *                  IVPRJ,NVPRJ)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BSPECS      STORES DIFFERENT USER SPECIFIED VALUES     
C   PRGMMR: WEISS            ORG: W/OSD211   DATE: 01-01-26             
C                                                                       
C ABSTRACT: STORES USER SPECIFIED VALUES FOR SPECIFIED PROJECTIONS      
C           INTO KDATA(,) TO BE PACKED LATER.  ALSO, THE SPECIFIED      
C           VALUES ARE SCALED ACCORDINGLY.                              
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   92-07-17  GILBERT
C   01-01-26  WEISS   REVISED FOR MODIFIED BUFR CODE
C                                                                       
C USAGE:                                                                
C        JANUARY 2001    WEISS    MDL    IBM SP                       
C                                                                       
C        PURPOSE                                                        
C            STORES USER SPECIFIED VALUES FOR SPECIFIED PROJECTIONS     
C            INTO KDATA(,) TO BE PACKED LATER.  ALSO, THE SPECIFIED     
C            VALUES ARE SCALED ACCORDINGLY.                             
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
C                       PROJECTION FOR WHICH FORECASTS ARE NOT VALID.
C                       M=1,ND1 AND N=1,NUMPRJ (OUTPUT). 
C               SCALE = BUFR TABLE B; SCALING VALUE (INPUT).
C                 REF = BUFR TABLE B; REFERENCE VALUE (INPUT).
C                 ND1 = MAXIMUM NUMBER OF STATIONS (INPUT).
C              NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE (INPUT).
C            LPROJ(N) = LIST OF PROJECTIONS FOR WHICH FORECASTS ARE
C                       ENCODED, N=1,NUMPRJ (INPUT).
C            IVPRJ(L) = CONTAINS VALID PROJECTIONS FOR THE CURRENT
C                       FORECAST ELEMENT, L=1,NVPRJ (INPUT).
C               NVPRJ = NUMBER OF VALID PROJECTIONS (INPUT).
C
C        ADDITIONAL VARIABLES
C             IVALS() = USER SPECIFIED VALUE TO BE INSERTED IN THE
C                       BUFR MESSAGE (INPUT/INTERNAL).
C               IMULT = INTEGER SCALLING FACTOR (INTERNAL).  
C                IREF = INTEGER REFERENCE NUMBER (INTERNAL).
C               ITEMP = SCALLED SPECIFIED VARIABLE (INTERNAL).
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
      INTEGER NSTA,KDATA(ND1,NUMPRJ),ND1,NUMPRJ,
     *        LPROJ(NUMPRJ),IVPRJ(NVPRJ),NVPRJ
      INTEGER IVALS(20),IMULT,IREF,ITEMP,N,L,I
C
      REAL SCALE,REF

      IMULT=10**NINT(SCALE)
      IREF=NINT(REF)
      N=1
C
C        READ SPECIFIED VALUES FROM INPUT CARDS.                        
      READ(5,50) (IVALS(I),I=1,NVPRJ)
 50   FORMAT(20I3)
C
C        DO FOR EACH PROJECTION                                         
      DO 200 L=1,NUMPRJ
C        IF NOT A SPECIFIED PROJECTION, INSERT MISSING (9999).          
        IF (N.GT.NVPRJ) N=1
        IF (LPROJ(L).EQ.IVPRJ(N)) THEN
C        ADD SCALED SPECIFIED VALUE TO KDATA(,) FOR EACH STATION        
          ITEMP=(IVALS(N)*IMULT)-IREF
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
