      SUBROUTINE BUFOPT(KFILDO,KFILX,NSTA,CCALL,KDATA,ID,LPROJ,
     *                  SCALE,REF,NDATE,ND1,IVPRJ,NVPRJ,NUMPRJ,IOPT,
     *                  IP12,ND5,ND7,INDEX,ICND)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BUFOPT      CALLS APPROPRIATE DATA PROCESSOR.          
C   PRGMMR: WEISS            ORG: W/OSD211   DATE: 01-01-26             
C                                                                       
C ABSTRACT: CALLS THE APPROPRIATE SUBROUTINE TO                         
C           READ THE REQUESTED FORECAST DATA AND DO ANY NECESSARY       
C           CONVERSIONS AND/OR SCALING TO PREPARE THE DATA FOR PACKING. 
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   92-07-13  GILBERT                                                   
C   96-04-15  GILBERT  - ADDED IOPT 13 FOR PROCESSING OF
C                        STATION LATITUDES AND LONGITUDES.
C   01-01-26  WEISS    - REVISED FOR MODIFIED BUFRMOS. THE ROUTINES OF
C                        BTMPDP, BWSPD, BWDIR, BCLDMR, BNMLFQ, AND   
C                        BCATGR HAVE BEEN COMBINED INTO BGENER.
C   04-11-18  COSGROVE - ADDED ARRAY INDEX TO CALL TO BUFOPT BECAUSE IT WAS
C                        GETTING CLOBBERED ON BLUE BECAUSE IT WAS A LOCAL
C                        ARRAY.
C                                                                       
C USAGE:                                                                
C                                                                       
C        JANUARY 2001   WEISS   MDL    IBM SP                          
C                                                                       
C        PURPOSE                                                        
C            CALLS THE APPROPRIATE SUBROUTINE TO                        
C            READ THE REQUESTED FORECAST DATA AND DO ANY NECESSARY      
C            CONVERSIONS AND/OR SCALING TO PREPARE THE DATA FOR PACKING.
C                                                                       
C        DATA SET USE                                                   
C              FT06 - PRINT FILE.  (OUTPUT)                             
C                                                                       
C        VARIABLES
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT)
C               KFILX = UNIT NUMBER OF RANDOM ACCESS FILE TO BE READ; MUST
C                       BE A VALUE BETWEEN 45 AND 49; WHILE THIS CAN BE
C                       CHANGED EVENTUALLY, FOR THIS PARTICULAR IMPLEMENTATION
C                       IN MAY 2000, THIS CONVENTION WAS FOLLOWED. (INPUT)
C                NSTA = NUMBER OF STATIONS (INPUT).
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST (CHARACTER*8).
C                       (INPUT)
C          KDATA(M,N) = HOLDS DATA FOR A FORECAST ELEMENT FOR EACH
C                       STATION AND EACH PROJECTION. 1ST DIMENSION IS
C                       # OF STATIONS, AND 2ND DIMENSION IS NUMBER OF
C                       PROJECTIONS.  CONTAINS MISSING "9999" FOR
C                       PROJECTION FOR WHICH FORECASTS ARE NOT VALID,
C                       M=1,ND1 AND N=1,NUMPRJ (OUTPUT).
C               ID(4) = MOS FORECAST IDENTIFIER (INPUT).
C            LPROJ(N) = LIST OF PROJECTIONS FOR WHICH FORECASTS ARE
C                       ENCODED, N=1,NUMPRJ (INPUT).
C               SCALE = BUFR TABLE B; SCALING VALUE (INPUT).
C                 REF = BUFR TABLE B; REFERENCE VALUE (INPUT). 
C               NDATE = DATE IN MDL FORMAT (INPUT). 
C                 ND1 = MAXIMUM # OF STATIONS, USED FOR DIMENSIONING 
C                       (INPUT).
C            IVPRJ(L) = CONTAINS VALID PROJECTIONS FOR THE CURRENT
C                       FORECAST ELEMENT, L=1,NVPRJ (INPUT).
C               NVPRJ = NUMBER OF VALID PROJECTIONS (INPUT).
C              NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE (INPUT).
C                IOPT = VALUE INDICATING WHICH SUBROUTINE TO 
C                       SELECT FOR EACH FORECAST ELEMENT/INPUT
C                       PARAMETER (INPUT). 
C                IP12 = INDICATES WHETHER (>0) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE EXTERNAL RANDOM ACCESS FILES
C                       WILL BE LISTED TO UNIT IP12 "READ_MOSDA" (INPUT).
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), DATA( ) AND
C                       CCALLD( ).  MUST BE AT LEAST AS BIG AS ND1
C                       "READ_MOSDA" (INPUT).
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED "READ_MOSDA" (INPUT).
C                ICND = ERROR FLAG (OUTPUT).
C
C        ADDITIONAL VARIABLES
C          INDEX(K,L) = ARRAY CONTAINING LOCATION OF STATION K IN FILE
C                       DIRECTORY L, WHERE K=1,NSTA AND L=1,15
C                       "READ_MOSDA" (INPUT/OUTPUT)
C            RDATA(L) = WORK ARRAY, HOLDING FORECASTS IN PROCESSING
C                       ROUTINES, L=1,NSTA. (INPUT/OUTPUT).
C
C
C
C REMARKS: THE PROCESS PICKED BY BUFOPT IS DETERMINED BY IOPT.          
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  IBM SP                                                       
C                                                                       
C$$$
      IMPLICIT NONE
C
      INTEGER KFILDO,KFILX,NSTA,KDATA(ND1,NUMPRJ),ID(4),
     *        LPROJ(NUMPRJ),NDATE,ND1,IVPRJ(NVPRJ),NVPRJ,NUMPRJ,
     *        IOPT,IP12,ND5,ND7,ICND
      INTEGER INDEX(ND1,15),I
C
      REAL RDATA(ND1),SCALE,REF
C
      CHARACTER*8 CCALL(ND1,6)
C******************************************************************
C
      ICND=0
C        INSERT PROJECTION LIST.                                        
      IF (IOPT.EQ.0) THEN
        CALL BPROJ(NSTA,KDATA,LPROJ,SCALE,REF,ND1,NUMPRJ)
        GOTO 900
      ENDIF
C        INSERT SPECIFIED USER VALUE = ID(4)
      IF (IOPT.EQ.1) THEN
        CALL BSPEC(NSTA,KDATA,ID(4),SCALE,REF,ND1,NUMPRJ,LPROJ,
     *             IVPRJ,NVPRJ)
        GOTO 900
      ENDIF
C
C        FOR CALL LETTERS...CONTINUE ON
      IF (IOPT.EQ.2) THEN
        GOTO 900
      ENDIF
C
C        GET TEMPS/DEW POINTS, WIND SPEEDS, WIND DIRECTIONS,
C        CATEGORICAL FORECASTS, "GENERIC", M.R. CLOUD FORECASTS, 
C        AND PROCESS
      IF (IOPT.EQ.3) THEN
         CALL BGENER(KFILDO,KFILX,NSTA,CCALL,RDATA,KDATA,ID,LPROJ,
     *               SCALE,REF,NDATE,ND1,NUMPRJ,IVPRJ,NVPRJ,
     *               IP12,ND5,ND7,INDEX,ICND)
         GOTO 900
      ENDIF
C
C        GET M.R. NORMALS AND/OR RELATIVE FRQUENCIES AND PROCESS
      IF (IOPT.EQ.11) THEN
         CALL BGENER(KFILDO,KFILX,NSTA,CCALL,RDATA,KDATA,ID,LPROJ,
     *               SCALE,REF,NDATE,ND1,NUMPRJ,IVPRJ,NVPRJ,
     *               IP12,ND5,ND7,INDEX,ICND)
         GOTO 900
      ENDIF
C
C        GET CATEGORICALS REQUIRING MAPPING (TRANSLATION) 
      IF (IOPT.EQ.12) THEN
        CALL BCMAPER(KFILDO,KFILX,NSTA,CCALL,RDATA,KDATA,ID,LPROJ,
     *               SCALE,REF,NDATE,ND1,NUMPRJ,IVPRJ,NVPRJ,
     *               IP12,ND5,ND7,INDEX,ICND)
        GOTO 900
      ENDIF
C
C        GET PROBABILITY FORECASTS AND PROCESS                          
      IF (IOPT.EQ.4) THEN
         CALL BPROB(KFILDO,KFILX,NSTA,CCALL,RDATA,KDATA,ID,LPROJ,
     *              SCALE,REF,NDATE,ND1,NUMPRJ,IVPRJ,NVPRJ,
     *              IP12,ND5,ND7,INDEX,ICND)
         GOTO 900
      ENDIF
C
C        INSERT SPECIFIED VALUES THAT ARE DIFFERENT FOR EACH PROJECTION.
      IF (IOPT.EQ.5) THEN
        CALL BSPECS(NSTA,KDATA,SCALE,REF,ND1,NUMPRJ,LPROJ,
     *              IVPRJ,NVPRJ)
        GOTO 900
      ENDIF
C
C        GET STATION LONGITUDES AND PROCESS                            
      IF (IOPT.EQ.6) THEN
         CALL LATLON(KFILDO,KFILX,NSTA,CCALL,RDATA,KDATA,ID,LPROJ,
     *               SCALE,REF,NDATE,ND1,NUMPRJ,IVPRJ,NVPRJ,
     *               IP12,ND5,ND7,INDEX,ICND)
        GOTO 900
      ENDIF
C
C        INSERT INITIAL DATE/TIME: YEAR DETERMINED BY IOPT.
      IF (IOPT.EQ.7) THEN
        CALL BDATE(NSTA,KDATA,IOPT,SCALE,REF,ND1,NUMPRJ,LPROJ,
     *             IVPRJ,NVPRJ,NDATE)
        GOTO 900
      ENDIF
C
C        INSERT INITIAL DATE/TIME: MONTH DETERMINED BY IOPT.  
C        BY VALUE IN ID(4) 
      IF (IOPT.EQ.8) THEN
        CALL BDATE(NSTA,KDATA,IOPT,SCALE,REF,ND1,NUMPRJ,LPROJ,
     *             IVPRJ,NVPRJ,NDATE)
        GOTO 900
      ENDIF
C
C        INSERT INITIAL DATE/TIME: DAY OF MONTH DETERMINED BY IOPT.
      IF (IOPT.EQ.9) THEN
        CALL BDATE(NSTA,KDATA,IOPT,SCALE,REF,ND1,NUMPRJ,LPROJ,
     *             IVPRJ,NVPRJ,NDATE)
        GOTO 900
      ENDIF
C
C        INSERT INITIAL DATE/TIME: HOUR(CYCLE) DETERMINED BY IOPT.
      IF (IOPT.EQ.10) THEN
        CALL BDATE(NSTA,KDATA,IOPT,SCALE,REF,ND1,NUMPRJ,LPROJ,
     *             IVPRJ,NVPRJ,NDATE)
        GOTO 900
      ENDIF
C
C        SET ICND = 1 IF NO PROCESSING DONE                             
      ICND=1
      WRITE(6,800) IOPT
 800  FORMAT(' IOPT =',I4,'  NOT FOUND IN BUFR OPTION LIST.')
 900  IF (ICND.NE.0) THEN
        WRITE(6,1000) (ID(I),I=1,4)
 1000   FORMAT(' NO FORECASTS FOUND FOR ID = ',4I10)
      ENDIF
      RETURN
      END
