      SUBROUTINE BCMAP(KDATA,ND1,NUMPRJ,NSTA,ID)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BCMAP       MAPS MOS CATEGORIES TO BUFR CATEGORIES     
C   PRGMMR: WEISS            ORG: W/OSD211   DATE: 01-03-21         
C                                                                       
C ABSTRACT: MAPS (TRANSLATES) MOS CATEGORICAL FORECAST VALUES TO THE
C           CORRESPONDING BUFR CODE TABLE VALUES.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   93-12-03  GILBERT                                                   
C   01-01-31  WEISS   REVISED FOR MODIFIED BUFRMOS
C   01-03-14  WEISS   UPDATED IDS FOR THE AVN SHORT RANGE MESSAGE
C   01-03-21  WEISS   UPDATED IDS AND TRANSLATIONS FOR THE MRF
C                     MEDIUM RANGE MESSAGE
C   02-01-02  RLC     ADDED MAPPING FOR AVN OBVIS CATEGORICAL FCSTS
C                     CORRECTED AVN QPF CATEGORICAL IDS
C   03-07-30  RLC     CHANGED MRF FORECAST IDS FROM MODEL NUMBER 
C                     9 TO 8 FOR GFS TRANSITION
C   04-09-03  MALONEY CHANGED AVN REFERENCES TO GFS; ADDED LOOPS
C                     TO HANDLE NEW CEILING AND VISIBILITY CATS;
C                     ADDED PROCESSING FOR NEW GFS/ETA SNOWFALL;
C                     ADDED IDS TO ALLOW ETA MOS PROCESSING. 
C   05-08-04  MALONEY MADE CHANGES TO NEW VISIBILITY MAPPING
C   10-03-24  MALONEY ADDED OPAQUE SKY COVER ID TO CATEGORY MAPPING
C                                                                       
C USAGE:                                                                
C        MARCH 2001     WEISS   IBM SP                              
C                                                                       
C        PURPOSE                                                        
C             MAPS (TRANSLATES) MOS CATEGORICAL FORECAST VALUES TO THE
C             CORRESPONDING BUFR CODE TABLE VALUES.
C                                                                       
C        DATA SET USE                                                   
C             NONE                                                      
C                                                                       
C        VARIABLES                                                      
C           KDATA(N,L) = HOLDS DATA FOR A FORECAST ELEMENT FOR EACH     
C                        STATION AND EACH PROJECTION. 1ST DIMENSION IS  
C                        # OF STATIONS, AND 2ND DIMENSION IS NUMBER OF  
C                        PROJECTIONS. CONTAINS MISSING "9999" FOR      
C                        PROJECTION FOR WHICH FORECASTS ARE NOT VALID.
C                        N=1,ND1, L=1,NUMPRJ (INPUT/OUTPUT).
C                  ND1 = MAXIMUM NUMBER OF STATIONS (INPUT).
C               NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE (INPUT).
C                 NSTA = NUMBER OF STATIONS (INPUT).
C                ID(4) = MOS FORECAST IDENTIFIER (INPUT).
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
      INTEGER KDATA(ND1,NUMPRJ),ND1,NUMPRJ,NSTA,ID(4)
      INTEGER I,J
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C        MAP CATEGORICAL VALUES FOR 6-H SNOW AMT.                        
CC      IF (ID(1).EQ.208401006) THEN
CC        DO 110 I=1,NSTA
CC          DO 100 J=1,NUMPRJ
CC            IF (KDATA(I,J).EQ.1) THEN
CC              KDATA(I,J)=2
CC            ELSEIF (KDATA(I,J).EQ.2) THEN
CC              KDATA(I,J)=1
CC            ELSEIF (KDATA(I,J).EQ.3) THEN
CC              KDATA(I,J)=0
CC            ENDIF
CC 100      CONTINUE
CC 110    CONTINUE
C        MAP CATEGORICAL VALUES FOR 12-H SNOW AMT.                      
CC      ELSEIF (ID(1).EQ.208402006) THEN
CC        DO 130 I=1,NSTA
CC          DO 120 J=1,NUMPRJ
CC            IF (KDATA(I,J).EQ.1) THEN
CC              KDATA(I,J)=5
CC            ELSEIF (KDATA(I,J).EQ.2) THEN
CC              KDATA(I,J)=4
CC            ELSEIF (KDATA(I,J).EQ.4) THEN
CC              KDATA(I,J)=1
CC            ELSEIF (KDATA(I,J).EQ.5) THEN
CC              KDATA(I,J)=0
CC            ENDIF
CC 120      CONTINUE
CC 130    CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        MAP CATEGORICAL VALUES FOR 6-H QPF
C        GFS: SHORT RANGE
      IF ((ID(1).EQ.203221008).OR.
     +    (ID(1).EQ.203221007)) THEN
        DO 110 I=1,NSTA
          DO 100 J=1,NUMPRJ
            IF (KDATA(I,J).NE.9999) KDATA(I,J)=KDATA(I,J)-1
 100      CONTINUE
 110    CONTINUE
C
C        MAP CATEGORICAL VALUES FOR 12-H QPF
C        GFS: SHORT/MEDIUM RANGE
      ELSEIF ((ID(1).EQ.203321008) .OR.
     *        (ID(1).EQ.203331008) .OR.
     *        (ID(1).EQ.203331007) .OR.
     *        (ID(1).EQ.203321007)) THEN
        DO 130 I=1,NSTA
          DO 120 J=1,NUMPRJ
            IF (KDATA(I,J).EQ.1) THEN
              KDATA(I,J)=0
            ELSEIF (KDATA(I,J).EQ.2) THEN
              KDATA(I,J)=1
            ELSEIF (KDATA(I,J).EQ.3) THEN
              KDATA(I,J)=2
            ELSEIF (KDATA(I,J).EQ.4) THEN
              KDATA(I,J)=3
            ELSEIF (KDATA(I,J).EQ.5) THEN
              KDATA(I,J)=4
            ELSEIF (KDATA(I,J).EQ.6) THEN
              KDATA(I,J)=6
C
C        QPF: > 2.00 INCHES 
            ELSEIF (KDATA(I,J).EQ.7) THEN
              KDATA(I,J)=7
C
CC        FUTURE DEFINITIONS (2.00-2.99, > 3.00)
CC            ELSEIF (KDATA(I,J).EQ.7) THEN
CC              KDATA(I,J)=8
CC            ELSEIF (KDATA(I,J).EQ.8) THEN
CC              KDATA(I,J)=9
C
            ENDIF
 120      CONTINUE
 130    CONTINUE
C
C        MAP CATEGORICAL VALUES FOR 24-H QPF
C        GFS: SHORT/MEDIUM RANGE
      ELSEIF ((ID(1).EQ.203421008) .OR.
     *        (ID(1).EQ.203431008) .OR.
     *        (ID(1).EQ.203431007) .OR.
     *        (ID(1).EQ.203431007)) THEN
        DO 150 I=1,NSTA
          DO 140 J=1,NUMPRJ
            IF (KDATA(I,J).EQ.1) THEN
              KDATA(I,J)=0
            ELSEIF (KDATA(I,J).EQ.2) THEN
              KDATA(I,J)=1
            ELSEIF (KDATA(I,J).EQ.3) THEN
              KDATA(I,J)=2
            ELSEIF (KDATA(I,J).EQ.4) THEN
              KDATA(I,J)=3
            ELSEIF (KDATA(I,J).EQ.5) THEN
              KDATA(I,J)=4
            ELSEIF (KDATA(I,J).EQ.6) THEN
              KDATA(I,J)=6
C
C        QPF: > 2.00 INCHES 
            ELSEIF (KDATA(I,J).EQ.7) THEN
              KDATA(I,J)=7
C
CC        FUTURE DEFINITIONS (2.00-2.99, > 3.00)
CC            ELSEIF (KDATA(I,J).EQ.7) THEN
CC              KDATA(I,J)=8
CC            ELSEIF (KDATA(I,J).EQ.8) THEN
CC              KDATA(I,J)=9
C
            ENDIF
 140      CONTINUE
 150    CONTINUE
C
C        MAP CATEGORICAL VALUES FOR PTYPE
C        MRF: MEDIUM RANGE
      ELSEIF ((ID(1).EQ.208556008).OR.
     +        (ID(1).EQ.208556007)) THEN 
        DO 170 I=1,NSTA
          DO 160 J=1,NUMPRJ
            IF (KDATA(I,J).EQ.1) THEN
              KDATA(I,J)=1
            ELSEIF (KDATA(I,J).EQ.2) THEN
              KDATA(I,J)=2
            ELSEIF (KDATA(I,J).EQ.3) THEN
              KDATA(I,J)=4
            ELSEIF (KDATA(I,J).EQ.4) THEN
              KDATA(I,J)=3
            ENDIF
 160      CONTINUE
 170    CONTINUE
C
C        MAP CATEGORICAL VALUES CLOUD AMT
C        GFS: SHORT RANGE (OLD!)
      ELSEIF (ID(1).EQ.208341008) THEN
        DO 190 I=1,NSTA
          DO 180 J=1,NUMPRJ
            IF (KDATA(I,J).EQ.1) THEN
              KDATA(I,J)=0
            ELSEIF (KDATA(I,J).EQ.2) THEN
              KDATA(I,J)=11
            ELSEIF (KDATA(I,J).EQ.3) THEN
              KDATA(I,J)=12
            ELSEIF (KDATA(I,J).EQ.4) THEN
              KDATA(I,J)=8
            ENDIF
 180      CONTINUE
 190    CONTINUE
C        MAP CATEGORICAL VALUES CLOUD AMT
C        TOTAL (208351) OR OPAQUE (208381)
C        GFS/ETA: SHORT RANGE *NEW*
      ELSEIF (ID(1).EQ.208351008.OR.
     +        ID(1).EQ.208351007.OR.
     +        ID(1).EQ.208381008.OR.
     +        ID(1).EQ.208381007) THEN
        DO 195 I=1,NSTA
          DO 185 J=1,NUMPRJ
            IF (KDATA(I,J).EQ.1) THEN
              KDATA(I,J)=0
            ELSEIF (KDATA(I,J).EQ.2) THEN
              KDATA(I,J)=13
            ELSEIF (KDATA(I,J).EQ.3) THEN
              KDATA(I,J)=11
            ELSEIF (KDATA(I,J).EQ.4) THEN
              KDATA(I,J)=12
            ELSEIF (KDATA(I,J).EQ.5) THEN
              KDATA(I,J)=8
            ENDIF
 185      CONTINUE
 195    CONTINUE
C
C
C        MAP CATEGORICAL VALUES CLOUD AMT
C        MRF: MEDIUM RANGE
      ELSEIF (ID(1).EQ.208391008) THEN
        DO 210 I=1,NSTA
          DO 200 J=1,NUMPRJ
            IF (KDATA(I,J).EQ.1) THEN
              KDATA(I,J)=1
            ELSEIF (KDATA(I,J).EQ.2) THEN
              KDATA(I,J)=4
            ELSEIF (KDATA(I,J).EQ.3) THEN
              KDATA(I,J)=7
            ENDIF
 200      CONTINUE
 210    CONTINUE
C
C        MAP CATEGORICAL VALUES OBSTRUCTION TO VISION
C        GFS: SHORT RANGE
      ELSEIF ((ID(1).EQ.208291008).OR.
     +        (ID(1).EQ.208291007)) THEN
        DO 230 I=1,NSTA
          DO 220 J=1,NUMPRJ
            IF (KDATA(I,J).EQ.1) THEN
              KDATA(I,J)=4
            ELSEIF (KDATA(I,J).EQ.2) THEN
              KDATA(I,J)=2
            ELSEIF (KDATA(I,J).EQ.3) THEN
              KDATA(I,J)=5
            ELSEIF (KDATA(I,J).EQ.4) THEN
              KDATA(I,J)=3
            ELSEIF (KDATA(I,J).EQ.5) THEN
              KDATA(I,J)=1
            ENDIF
 220      CONTINUE
 230    CONTINUE
C
C        MAP CATEGORICAL CEILING HGT
C        GFS/ETA (NEW!)
C
      ELSEIF ((ID(1).EQ.208051008).OR.
     +        (ID(1).EQ.208051007)) THEN
        DO 235 I=1,NSTA
          DO 225 J=1,NUMPRJ
            IF (KDATA(I,J).EQ.1) THEN
              KDATA(I,J)=1
            ELSEIF (KDATA(I,J).EQ.2) THEN
              KDATA(I,J)=2
            ELSEIF (KDATA(I,J).EQ.3) THEN
              KDATA(I,J)=3
            ELSEIF (KDATA(I,J).EQ.4) THEN
              KDATA(I,J)=8
            ELSEIF (KDATA(I,J).EQ.5) THEN
              KDATA(I,J)=9
            ELSEIF (KDATA(I,J).EQ.6) THEN
              KDATA(I,J)=5
            ELSEIF (KDATA(I,J).EQ.7) THEN
              KDATA(I,J)=6
            ELSEIF (KDATA(I,J).EQ.8) THEN
              KDATA(I,J)=7
            ENDIF
 225      CONTINUE
 235    CONTINUE
C
C        MAP CATEGORICAL VISIBILITY
C        GFS/ETA (NEW!)
C
      ELSEIF ((ID(1).EQ.208131008).OR.
     +        (ID(1).EQ.208131007)) THEN
        DO 250 I=1,NSTA
          DO 240 J=1,NUMPRJ
            IF (KDATA(I,J).EQ.1) THEN
              KDATA(I,J)=8
            ELSEIF (KDATA(I,J).EQ.2) THEN
              KDATA(I,J)=9
            ELSEIF (KDATA(I,J).EQ.3) THEN
              KDATA(I,J)=10
            ELSEIF (KDATA(I,J).EQ.4) THEN
              KDATA(I,J)=11
            ELSEIF (KDATA(I,J).EQ.5) THEN
              KDATA(I,J)=5
            ELSEIF (KDATA(I,J).EQ.6) THEN
              KDATA(I,J)=6
            ELSEIF (KDATA(I,J).EQ.7) THEN
              KDATA(I,J)=7
            ENDIF
 240      CONTINUE
 250    CONTINUE
C
C        MAP 24H SNOWFALL CAT GUIDANCE
C        GFS/ETA (NEW!)
C
      ELSEIF ((ID(1).EQ.208461008).OR.
     +        (ID(1).EQ.208461007)) THEN
        DO 270 I=1,NSTA
          DO 260 J=1,NUMPRJ
            IF (KDATA(I,J).EQ.1) THEN
              KDATA(I,J)=0
            ELSEIF (KDATA(I,J).EQ.2) THEN
              KDATA(I,J)=9
            ELSEIF (KDATA(I,J).EQ.3) THEN
              KDATA(I,J)=3
            ELSEIF (KDATA(I,J).EQ.4) THEN
              KDATA(I,J)=4
            ELSEIF (KDATA(I,J).EQ.5) THEN
              KDATA(I,J)=6
            ELSEIF (KDATA(I,J).EQ.6) THEN
              KDATA(I,J)=7
            ENDIF
 260      CONTINUE
 270    CONTINUE
      ENDIF
      RETURN
      END
