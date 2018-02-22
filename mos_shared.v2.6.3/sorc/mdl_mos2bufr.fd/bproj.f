      SUBROUTINE BPROJ(NSTA,KDATA,LPROJ,SCALE,REF,ND1,NUMPRJ)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BPROJ       STORES PROJECTIONS                         
C   PRGMMR: WEISS            ORG: W/OSD211   DATE: 01-01-26
C                                                                       
C ABSTRACT: STORES THE LIST OF PROJECTIONS IN KDATA(,) FOR EACH         
C   STATION.  SCALING IS DONE BEFORE ADDING TO KDATA(,).                
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   92-07-17  GILBERT
C   01-01-26  WEISS   REVISED FOR MODIFIED BUFRMOS.
C                                                                       
C USAGE:                                                                
C        JANUARY 2001     WEISS     MDL    IBM SP
C                                                                       
C        PURPOSE                                                        
C            STORES THE LIST OF PROJECTIONS IN KDATA(,) FOR EACH        
C            STATION.  SCALING IS DONE BEFORE ADDING TO KDATA(,).       
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
C            LPROJ(N) = LIST OF PROJECTIONS FOR WHICH FORECASTS ARE
C                       ENCODED, N=1,NUMPRJ (INPUT).
C               SCALE = BUFR TABLE B; SCALING VALUE (INPUT).
C                 REF = BUFR TABLE B; REFERENCE VALUE (INPUT).
C                 ND1 = MAXIMUM NUMBER OF STATIONS (INPUT).
C              NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE (INPUT).
C
C        ADDITIONAL VARIABLES
C               IMULT = INTEGER SCALLING FACTOR (=1) (INTERNAL).
C                IREF = INTEGER REFERENCE NUMBER (INTERNAL).
C               ITEMP = SCALLED TIME VARIABLE (THE VALUE STAYS THE SAME)
C                       (INTERNAL).
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
      INTEGER NSTA,KDATA(ND1,NUMPRJ),LPROJ(NUMPRJ),ND1,NUMPRJ
      INTEGER IMULT,IREF,ITEMP,I,L
      REAL SCALE,REF
C
      IMULT=10**NINT(SCALE)
      IREF=NINT(REF)
      DO 200 L=1,NUMPRJ
        ITEMP=(LPROJ(L)*IMULT)-IREF
        DO 100 I=1,NSTA
          KDATA(I,L)=ITEMP
 100    CONTINUE
 200  CONTINUE
      RETURN
      END
