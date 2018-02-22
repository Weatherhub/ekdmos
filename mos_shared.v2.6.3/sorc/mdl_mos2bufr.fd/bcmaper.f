      SUBROUTINE BCMAPER(KFILDO,KFILX,NSTA,CCALL,RDATA,KDATA,ID,LPROJ,
     *                   SCALE,REF,NDATE,ND1,NUMPRJ,IVPRJ,NVPRJ,
     *                   IP12,ND5,ND7,INDEX,ICND)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BGENER      GETS DATA THAT DO NOT NEED CONVERSION 
C   PRGMMR: WEISS            ORG: W/OSD211   DATE: 01-01-31            
C                                                                       
C ABSTRACT: READS CATEGORICAL FORECASTS FOR ALL PROJECTIONS.    
C           PERFORMS THE APPROPRIATE SCALING AND STORES THE    
C           DATA IN ARRAY KDATA(,) FOR LATER USE IN PACKING.
C           MISSING VALUES (9999) ARE INSERTED FOR PROJECTIONS AT  
C           WHICH THE DATA ARE NOT FOUND OR VALID.
C           ALSO PERFORMS THE APPROPRIATE TRANSLATIONS (MAPPING) OF
C           CERTAIN CATEGORICAL FORECASTS TO BUFR NOTATION. THIS 
C           FINAL STEP IS DONE BY CALLING BCMAPPER.
C                                                                       
C PROGRAM HISTORY LOG:                                                 
C   93-07-29  GILBERT                                                  
C   01-01-31  WEISS   REVISED FOR MODIFIED BUFR MOS. GTMOS IS 
C                     REPLACED WITH READ_MOSDA
C                                                                       
C USAGE:                                                                
C        JANUARY 2001    WEISS    MDL    IBM SP                          
C                                                                       
C        PURPOSE
C            READS CATEGORICAL FORECASTS FOR ALL PROJECTIONS.
C            PERFORMS THE APPROPRIATE SCALING AND STORES THE
C            DATA IN ARRAY KDATA(,) FOR LATER USE IN PACKING.
C            MISSING VALUES (9999) ARE INSERTED FOR PROJECTIONS AT 
C            WHICH THE DATA ARE NOT FOUND OR VALID. 
C            ALSO PERFORMS THE APPROPRIATE TRANSLATIONS (MAPPING) OF
C            CERTAIN CATEGORICAL FORECASTS TO BUFR NOTATION. THIS 
C            FINAL STEP IS DONE BY CALLING BCMAPPER.
C                                                                       
C        DATA SET USE                                                   
C            FT06     - PRINT FILE (OUTPUT)                             
C            MOSMATXX - MOSMAT FILE  (XX = 00 OR 12)   (INPUT)          
C                                                                       
C        VARIABLES
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE (INPUT).
C               KFILX = UNIT NUMBER OF RANDOM ACCESS FILE TO BE READ; MUST
C                       BE A VALUE BETWEEN 45 AND 49; WHILE THIS CAN BE
C                       CHANGED EVENTUALLY, FOR THIS PARTICULAR IMPLEMENTATION
C                       IN MAY 2000, THIS CONVENTION WAS FOLLOWED.  (INPUT)
C                NSTA = NUMBER OF STATIONS (INPUT).
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST (CHARACTER*8).
C                       (INPUT)
C            RDATA(L) = WORK ARRAY, HOLDING FORECASTS IN PROCESSING
C                       ROUTINES, L=1,NSTA. (INPUT/OUTPUT).                 
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
C               NDATE = CURRENT DATE (MDL FORMAT).
C                 ND1 = MAXIMUM NUMBER OF STATIONS (INPUT).
C              NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE (INPUT).
C            IVPRJ(L) = CONTAINS VALID PROJECTIONS FOR THE CURRENT
C                       FORECAST ELEMENT, L=1,NVPRJ (INPUT).
C               NVPRJ = NUMBER OF VALID PROJECTIONS (INPUT).
C                IP12 = INDICATES WHETHER (>0) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE EXTERNAL RANDOM ACCESS FILES
C                       WILL BE LISTED TO UNIT IP12. (INPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), DATA( ) AND
C                       CCALLD( ).  MUST BE AT LEAST AS BIG AS ND1.  (INPUT)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED. 
C                       SHOULD BE SET TO 54 FOR CONSISTENCY WITH OTHER
C                       MOS-2000 SOFTWARE.  (INPUT)
C          INDEX(K,L) = ARRAY CONTAINING LOCATION OF STATION K IN FILE
C                       DIRECTORY L, WHERE K=1,NSTA AND L=1,15
C                       (INPUT/OUTPUT).
C                ICND = ERROR FLAG (OUTPUT).
C
C        ADDITIONAL VARIABLES
C               RMULT = REAL SCALLING FACTOR (INTERNAL).
C              IRDATA = INTEGER VALUE OF RDATA USED FOR MISSING
C                       VALUE TESTING (INTERNAL).
C                 NUM = COUNTER FOR TOTAL NUMBER OF FORECASTS (PROJECTIONS)
C                       ACTUALLY READ AND STORED IN BUFR (INTERNAL).
C                 IER = ERROR RETURN CODE FOR READ_MOSDA (INTERNAL).
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
      INTEGER NSTA,KDATA(ND1,NUMPRJ),ID(4),LPROJ(NUMPRJ),
     *        KFILX,NDATE,ND1,NUMPRJ,IVPRJ(NVPRJ),NVPRJ,
     *        KFILDO,IP12,ND5,ND7,INDEX(NSTA,15),ICND
      INTEGER I,II,L,N,NUM,IER,IRDATA,IS0,IS1,IS2,IS4
C
      REAL RDATA(NSTA),SCALE,REF,RMULT
C
      CHARACTER*8 CCALL(ND1,6)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
C
      N=1
      NUM=0
      RMULT=10.0**SCALE
      ICND=0
C
C        DO FOR EACH PROJECTION                                         
      DO 400 L=1,NUMPRJ
C        IF NOT A VALID PROJECTION, FILL KDATA(,) WITH MISSING (9999)   
C        VALUES.                                                        
        IF (N.GT.NVPRJ) GOTO 250
        IF (LPROJ(L).NE.IVPRJ(N)) GOTO 250
        N=N+1
C        READ FORECAST.                                                 
	ID(3)=LPROJ(L)
        CALL READ_MOSDA(KFILDO,KFILX,IP12,
     *                  ID,NDATE,CCALL,NSTA,
     *                  RDATA,ND1,ND5,ND7,IS0,IS1,IS2,IS4,
     *                  INDEX,IER)
        IF (IER.EQ.1) WRITE(KFILDO,90)IER,(ID(II),II=1,4)
 90     FORMAT('READ_MOSDA WARNING: MISSING STATIONS, IER=',I3,/,
     *  'BGENER: PARAMETER ID = ',4I10)
C
        IF (IER.EQ.2) THEN
          WRITE(KFILDO,95)IER,(ID(II),II=1,4)
 95       FORMAT('READ_MOSDA ERROR: DATE INCORRECT',
     *           ' IER=',I3,/,
     *           'BGENER: PARAMETER ID = ',4I10)
	  GO TO 250
        ENDIF
        IF(IER.EQ.3) THEN
          CALL W3TAGE('TDL_BUFRMOS')
          STOP 3
        ENDIF
C
        NUM=NUM+1
C
C        MULTIPLY SCALING FACTOR AND SUBTRACT REFERENCE VALUE FROM EACH 
C        DATA VALUE                                                     
        DO 200 I=1,NSTA
          IRDATA=NINT(RDATA(I)) 
	  IF ((IRDATA.NE.9999).AND.(IRDATA.NE.9997)) THEN
            RDATA(I)=(RDATA(I)*RMULT)-REF
            KDATA(I,L)=NINT(RDATA(I))
          ELSE
            KDATA(I,L)=9999
          ENDIF
 200    CONTINUE
        GOTO 400
C
 250    DO 300 I=1,NSTA
          KDATA(I,L)=9999
 300    CONTINUE
 400  CONTINUE
C        IF NO FORECASTS WERE FOUND SET ICND = 1.                       
      IF(NUM.EQ.0)ICND=1
C
C        CALL BCMAPPER TO TRANSLATE SPECIFIED CATEGORICAL
C        FORECAST TO BUFR NOTATION.
      IF (ICND.EQ.0) CALL BCMAP(KDATA,ND1,NUMPRJ,NSTA,ID)
 900  RETURN
      END
