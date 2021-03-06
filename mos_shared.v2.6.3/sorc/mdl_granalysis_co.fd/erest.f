      SUBROUTINE EREST(KFILDO,ID,CCALL,
     1                 XP,YP,XDATA,LTAG,LNDSEA,ELEV,XLAPSE,NSTA,
     2                 P,FD4,SVNEAR,ERROR,NX,NY,MESH,
     3                 CPNDFD,SEALND,TELEV,NXE,NYE,MESHE,
     4                 RELVAR,VARTAB,
     5                 BQ,NSMTYP,
     6                 IOCEXT,IOCINC,
     7                 L3264B,L3264W,ISTOP,IER)
C        DECEMBER  2007   GLAHN   MDL   MOS-2000
C        JANUARY   2008   GLAHN   ADDED PRINTING OF EQUATION
C        FEBRUARY  2008   GLAHN   SET ERROR( , ) ON ERROR RETURN;
C                                 ADDED ERROR RETURNS AFTER CALLS; SET
C                                 IER=0 UPON ENTRY; ADDED RWATO AND
C                                 RWATI TO CALL AND CALL TO CLOS2G AND
C                                 VARIG; FD5( , ) CHANGED TO SVNEAR( , )
C        FEBRUARY  2008   GLAHN   ADDED ITERML,ITERMW,JTERML,JTERMW
C        FEBRUARY  2008   GLAHN   ADDED P( , )AND XLAPSE( ) TO CALL
C                                 TO CLOS2G; REMOVED R FROM CALL
C        FEBRUARY  2008   GLAHN   ADDED VARILG; ADDED SMOTHG CAPABILITY
C        MARCH     2008   COSGROVE  ADDED COMMAS TO FORMATS 121,316 FOR
C                                 IBM COMPILE
C        MARCH     2009   GLAHN   CHANGED 2ND WORD STRUCTURE 
C        APRIL     2009   GLAHN   ADDED ORSMTH; ADDED IOCEXT AND IOCINC
C                                 TO CALL
C        JUNE      2009   IM      ADDED DEW POINT
C   
C        PURPOSE
C            TO USE A REGRESSION EQUATION TO ESTIMATE THE ERROR
C            OF THE ANALYSIS AT LAND GRIDPOINTS AND ANOTHER
C            EQUATION FOR WATER (INLAND AND OCEAN) GRIDPOINTS.
C            THE REGRESSION EQUATIONS ARE DEVELOPED BY U600 BASED
C            ON ERRORS CALCULATED AT WITHHELD STATIONS AS A
C            FUNCTION OF ANALYSIS PARAMETERS SUCH AS DISTANCE
C            TO STATIONS, ROUGHNESS, AND VARIABILITY AND
C            DENSITY OF THE DATA.
C
C            NOTE:  NOT ALL POSSIBLE PREDICTORS HAVE BEEN
C            IMPLEMENTED BECAUSE THEY LIKELY WILL NOT BE USED.
C
C            THE VARIABLES AND IDS ACCOMMODATED ARE
C               CCCFFFBDD  0XX97YYYY   TRROHHTTT ISW, WHERE XX =
C                1,2   -- THE DISTANCE TO THE TWO CLOSEST STATIONS
C                3,4   -- DATA VARIABILITY, WITH LAPSE BUT NOT WEIGHTS
C                5-8   -- THE 4 TERRAIN ROUGHNESS VARIABLES
C                9-12  -- DATA VARIABILITY, NO LAPSE OR WEITHTS
C                13-16 -- DATA VARIABILITY, WITH LAPSE AND WEIGHTS
C                         AND ITS NEIGHBORS, ACCOUNTING FOR XLAPSE( )
C                17    -- ABSOLUTE DIFFERENCE IN ELEVATION BETWEEN
C                         STATION AND ITS CLOSEST NEIGHBOR
C                18    -- ABSOLUTE DIFFERENCE BETWEEN STATION DATA
C                         VALUE AND VALUE OF NEIGHBOR AFTER LAPSE
C                         APPLIED
C                19    -- THE PRODUCT OF 17 AND 1
C                20    -- THE "ERROR," THE DIFFERENCE BETWEEN THE
C                         ACTUAL VALUE AND THE INTERPOLATED VALUE
C                         FROM THE ANALYSIS
C                YYYY = RADIUS OVER WHICH TO COMPUTE STATISTICS.
C
C            IDIM, IDTAB( ), ITABLE( , , ), AND RTABLE( , , )
C            HAVE TO BE INCREASED WHEN A NEW VARIABLE IS ADDED.
C            
C
C        DATA SET USE
C            KFILDO   - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C               ID(J) = THE VARIABLE ID'S OF THE ANALYSIS (J=1,4).
C                       (INPUT)
C            CCALL(K) = CALL LETTERS OF STATIONS (K=1,NSTA).
C                       USED FOR DIAGNOSTICS ONLY.  (CHARACTER*8)
C                       (INPUT)
C               XP(K) = THE X POSITION FOR STATION K (K=1,NSTA) ON 
C                       THE ANALYSIS GRID AREA AT THE CURRENT NOMINAL
C                       GRID MESH LENGTH MESH.  (INPUT)
C               YP(K) = THE Y POSITION FOR STATION K (K=1,NSTA) ON 
C                       THE ANALYSIS GRID AREA AT THE CURRENT NOMINAL
C                       GRID MESH LENGTH MESH.  (INPUT)
C            XDATA(K) = THE DATA BEING ANALYZED (K=1,ND1).  (INPUT)
C             LTAG(K) = DENOTES USE OF DATA IN DATA(K) FOR STATION K
C                       (K=1,NSTA).
C                       0 = USE DATA.
C                       1 = STATION OUTSIDE RADIUS OF INFLUENCE FOR
C                           AREA BEING ANALYZED OR MISSING DATUM.
C                       2 = STATION LOCATION UNKNOWN.
C                       (INPUT/OUTPUT)
C           LNDSEA(K) = LAND/SEA INFLUENCE FLAG FOR EACH STATION
C                       (K=1,ND1).
C                       0 = WILL BE USED FOR ONLY OCEAN WATER (=0)
C                           GRIDPOINTS.
C                       3 = WILL BE USED FOR ONLY INLAND WATER (=3)
C                           GRIDPOINTS.
C                       6 = WILL BE USED FOR BOTH INLAND WATER (=3)
C                           AND LAND (=9) GRIDPOINTS.
C                       9 = WILL BE USED FOR ONLY LAND (=9) GRIDPOINTS.
C                       (INPUT)
C             ELEV(K) = ELEVATION OF STATIONS (K=1,NSTA).  (INPUT)
C           XLAPSE(K) = CALCULATED LAPSE RATE IN UNITS OF THE VARIABLE
C                       BEING ANALYZED PER M. (K=1,KSTA).  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C            P(IX,JY) = THE CURRENT ANALYSIS (IX=1,NX) (JY=1,NY).
C                       (INPUT)
C          FD4(IX,JY) = WORK ARRAY (IX=1,NX) (JY=1,NY).  (INTERNAL)
C       SVNEAR(IX,JY) = WORK ARRAY (IX=1,NX) (JY=1,NY).  HOLDS THE
C                       DISTANCE TO THE NEAREST STATION AS CALCULATED
C                       BY CLOS2G.  FD5( , ) IN CALLING U405A.
C                       (INTERNAL)
C        ERROR(IX,JY) = ERROR GRID (IX=1,NX) (JY=1,NY).  THIS IS
C                       FD3( , ) IN CALLING ROUTINE U405A.  SET TO
C                       MISSING WHEN ERROR CANNOT BE EVALUATED.
C                       (OUTPUT)
C                  NX = THE SIZE OF THE  ERROR( , ), FD4( , ) AND
C                       SVNEAR( , ) GRIDS IN THE X DIRECTION.  (INPUT)
C                  NY = THE SIZE OF THE  ERROR( , ), FD4( , ) AND
C                       SVNEAR( , ) GRIDS IN THE Y DIRECTION.  (INPUT)
C                MESH = THE MESH LENGTH OF THE GRIDS TO WHICH THE
C                       POSITIONS XP( ) AND YP( ) REFER.  (INPUT)
C       CPNDFD(IX,JY) = THE CLIPPING MASK (IX=1,NX) (JY=1,NY) AT
C                       NOMINAL MESHLENGTH MESHE.
C                       1 = WITHIN THE AREA; 0 = OUTSIDE.  (INPUT)
C       SEALND(IX,JY) = THE SEA/LAND (IX=1,NX) (JY=1,NY) AT
C                       NOMINAL MESHLENGTH MESHE.
C                       9 = LAND; VALUES BELOW WATER.  (INPUT)
C        TELEV(IX,JY) = THE TERRAIN (IX=1,NX) (JY=1,NY) AT
C                       NOMINAL MESHLENGTH MESHE.  (INPUT)
C                 NXE = X-EXTENT OF TELEV( ), SEALND( ), AND CPNDFD( )
C                       AT MESH LENGTH MESHE.  (INPUT)
C                 NYE = Y-EXTENT OF TELEV( ), SEALND( ), AND CPNDFD( )
C                       AT MESH LENGTH MESHE.  (INPUT)
C               MESHE = THE NOMINAL MESH LENGTH OF THE TERRAIN GRID.
C                       IT IS MANDATORY MESHE = MESH.  (INPUT)
C           RELVAR(J) = THE RADII OVER WHICH TO COMPUTE THE WEIGHTED
C                       VARIABILITY (J=1,4).  (INPUT)
C           VARTAB(J) = THE FACTOR TO INCREASE RELVAR( ) FOR LAND (J=1)
C                       AND WATER (J=2). (INPUT)
C                  BQ = SMOOTHING PARAMETER.  NORMAL SMOOTHED VALUE AT
C                       POINT P = ORIGINAL VALUE PLUS BQ/4 TIMES SUM OF
C                       SURROUNDING 4 POINTS, ALL DIVIDED BY BQ+1.
C                       A MODIFIED PROCEDURE IS USED ON THE BORDERS AND
C                       CORNERS THAT ASSUMES A BQ OF 4 INDICATES A MEAN
C                       OF ALL VALUES INVOLVED.  THESE CALCULATIONS ARE
C                       CONSISTENT WITH SMOTHM WHICH DEALS WITH MISSING
C                       VALUES.  THE EQUATIONS USED ARE THE SAME AS:
C                          Q(IX,JY)=(P(IX,JY)+(SUM/ISUM)*(ISUM/4)*BQ)/
C                             ((ISUM/4)*BQ+1.)
C                       WHERE SUM IS THE SUM OF THE ISUM VALUES 
C                       (2 FOR CORNERS AND 3 FOR BORDERS). 
C                       (INPUT)
C              NSMTYP = TYPE OF SMOOTHING:
C                       5 = SPECIAL TERRAIN-FOLLOWING SMOOTHING.
C                       6 = TWO PASSES OF 5 ABOVE.
C                       7 = THREE PASSES OF 5 ABOVE.
C                       (INPUT)
C              IOCEXT = THE NUMBER OF GRIDPOINTS PLUS AND MINUS TO 
C                       SMOOTH IN ORSMTH.  THIS IS ALSO THE NUMBER OF
C                       GRID LENGTHS TO SMOOTH IN EACH DIRECTION.
C                       (INPUT)
C              IOCINC = THE INCREMENT TO USE IN SMOOTHING IN ORSMTH.
C                       NOTE THAT IOCEXT SHOULD BE EVENLY DIVISIBLE
C                       BY IOCINC.  (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT).
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).
C                       (INPUT)
C               ISTOP = INCREMENTED BY 1 WHEN AN ERROR OCCURS.
C                       (INPUT/OUTPUT)
C                 IER =   0 = GOOD RETURN.
C                       777 = ERROR FIELD COULD NOT BE COMPUTED.
C                             RETURNED FROM SUBROUINTES, AS WELL
C                             AS EREST.
C                       (OUTPUT)
C              IFIRST = 0 ON FIRST ENTRY.  IF A CLIPPING GRID IS NOT
C                       AVAILABLE, A DIAGNOSTIC IS WRITTEN AND IFIRST
C                       IS SET OT 1.  IT IS SAVED FROM ENTRY TO ENTRY.
C                       THE DIAGNOSTIC SHOULD BE WRITTEN ONLY ONCE
C                       PER RUN.  (INTERNAL)  (SAVED)
C       NEARSV(IX,JY) = WORK ARRAY (IX=1,NX) (JY=1,NY).  HOLDS THE
C                       LOCATION IN THE LIST OF THE NEAREST STATION
C                       AS CALCULATED BY CLOS2G.  (AUTOMATIC)
C                       (INTERNAL)
C            IDTAB(J) = THE FIRST WORD ID OF THE ANALYSIS FOR
C                       THE ERROR IS BEING COMPUTED WITH THE 
C                       COEFFICIENTS IN RTABLE(J,L,M) (J=1,IDIM).
C                       THE FIRST WORD IDS OF ALL PREDICTORS IN
C                       AN EQUATION ARE THE SAME; THE SECOND
C                       WORD IS IN ITABLE(J,L,M).  (INTERNAL)
C       ITABLE(J,L,M) = HOLDS THE 2ND WORD IDS FOR UP TO JDIM
C                       PREDICTORS (J=1,JDIM) FOR UP TO IDIM
C                       EQUATIONS (M=1,IDIM) FOR LAND (L=1) AND
C                       WATER (L=2).  THESE EQUATIONS ARE SET
C                       IN EREST BY A DATA STATEMENT.   (INTERNAL)
C       RTABLE(J,L,M) = HOLDS THE JDIM COEFFICIENTS (J=1,JDIM) AND
C                       THE CONSTANT (J=JDIM+1) FOR THE LAND (L=1)
C                       AND WATER (L=2) EQUATION FOR IDIM EQUATIONS
C                       (M=1,IDIM), EACH EQUATION PERTAINING TO A
C                       DIFFERENT VARIABLE.  (INTERNAL)
C       SAVECL(IX,JY) = WORK ARRAY (IX=1,NY) (JY=1,NY) USED TO SAVE
C                       ONE OF THE CLOSEST NEIGHBOR FIELDS COMPUTED
C                       BY CLOS2G.  (AUTOMATIC)  (INTERNAL)
C              ITERML = THE NUMBER OF TERMS IN THE LAND EQUATION BEING
C                       EVALUATED.  (INTERNAL)
C              ITERMW = THE NUMBER OF TERMS IN THE WATER EQUATION BEING
C                       EVALUATED.  (INTERNAL)
C              JTERML = THE NUMBER OF TERMS IN THE LAND EQUATION
C                       EVALUATED.  (INTERNAL)
C              JTERMW = THE NUMBER OF TERMS IN THE WATER EQUATION
C                       EVALUATED.  (INTERNAL)
C               CSTSM = THE SMOOTHING PARAMETER IF ANY POINT HAS WATER 
C                       BUT NOT ALL ARE WATER.  USE INSTEAD OF BQ.
C                       SET TO 0.
C                       (INTERNAL)
C               RMESH = IS THE RATIO OF THE MESH LENGTH OF THE ANALYSIS
C                       GRID TO THE TERRAIN TELEV( , ) AND SEA/LAND
C                       SEALND( , ) GRIDS.  SET TO 1.  (INTERNAL)
C            NSHLN(J) = DETERMINES SMOOTHING AT HIGH AND LOW ELEVATIONS.
C                       A 1 INDICATES:
C                         J=1--HIGH ELEVATION, HIGH VALUE SMOOTHED.
C                         J=2--HIGH ELEVATION, LOW VALUE SMOOTHED.
C                         J=3--HIGH ELEVATION, NOT HIGH OR LOW VALUE 
C                                              SMOOTHED.
C                         J=4--LOW ELEVATION, HIGH VALUE SMOOTHED.
C                         J=5--LOW ELEVATION, LOW VALUE SMOOTHED.
C                         J=6--LOW ELEVATION, NOT HIGH OR LOW VALUE
C                                             SMOOTHED.
C                       A 0 INDICATES NO SMOOTHING FOR THE VALUES OF J.
C                       USED FOR SMOTHG.  SET TO 11111.
C                       (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED
C            ROUGHG, DENSG, CLOS2G, VARIG
C
      PARAMETER (IDIM=2,
     1           JDIM=8)
C        IDIM MUST BE THE SAME AS THE SECOND DIMENSION OF ITABLE( , ),
C        JTABLE( , ), AND RTABLE( , ).  ALSO, JDIM MUST BE THE DIMENSION
C        OF IVAR( )
C
      CHARACTER*8 CCALL(NSTA)
C
      DIMENSION ID(4)
      DIMENSION LTAG(NSTA),LNDSEA(NSTA),XP(NSTA),YP(NSTA),ELEV(NSTA),
     1          XDATA(NSTA)
      DIMENSION P(NX,NY),ERROR(NX,NY),FD4(NX,NY),SVNEAR(NX,NY)
      DIMENSION SEALND(NXE,NYE),TELEV(NXE,NYE),CPNDFD(NXE,NYE)
      DIMENSION NEARSV(NX,NY)
C        NEARSV( , ) IS AN AUTOMATIC ARRAY.
      DIMENSION IDTAB(IDIM),ITABLE(JDIM,2,IDIM),
     1          RTABLE(JDIM+1,2,IDIM)
      DIMENSION RELVAR(4),VARTAB(2),NSHLN(6)
C
      DATA IDTAB/722000,723130/
C        THE ABOVE IS FOR HOURLY TEMPERATURE OBS.
      DATA NSHLN/1,1,1,1,1,1/
      DATA IFIRST/0/
C
      SAVE IFIRST
C
CCCC      DATA ITABLE
CCCC     1   /14970045, 06970004, 12970027,  08970001, 05970008, 0,  0,  0,
CCCC     2           0,        0,        0,         0,        0, 0,  0,  0/
CCCC      DATA RTABLE
CCCC     1 /  .8586000,  .001437,  .124020,   .005608, .0021200,.0, .0, .0,
CCCC     X  .90056,
CCCC     2           0.,       0.,       0.,       0.,       0.,0., 0., 0.,
CCCC     X   9999./
C        THE ABOVE IS AN ACTUAL 5-PREDICTOR EQUATION,13.26 PERCENT RV
C        Thu Feb 10          2008 U600 RUN  (RAN OK)
C
CCCC  DATA ITABLE
CCCC 1   /14970045, 06970004, 12970027,  08970001, 05970008, 0,  0,  0,
CCCC 2    14970045,        0, 12970027,         0,        0, 0,  0,  0/
CCCC  DATA RTABLE
CCCC 1 /  .8586000,  .001437,  .124020,   .005608, .0021200,.0, .0, .0,
CCCC X  .90056,
CCCC 2    .8586000,       0.,  .124020,       0.,       0.,0., 0., 0.,
CCCC X  .90056/
C        THE ABOVE LAND IS AN ACTUAL 5-PREDICTOR EQUATION,13.26 PERCENT RV
C        THE TWO NON-ROUGHNESS TERMS WERE USED TO TEST WATER.

      DATA ITABLE
     1   /08970001,18970055,12970027,02970055, 0, 0, 0, 0,     !T land
     2    18970110,10970090,       0,       0, 0, 0, 0, 0,     !T water
     3    18970055,11970035,08970001,       0, 0, 0, 0, 0,     !Td land
     4    18970110,02970110,09970110,       0, 0, 0, 0, 0/     !Td water
      DATA RTABLE
     1   /.0055024,.4484100,.1466600,.0288100,.0,.0,.0,.0,     !T land 
     2    .2539200,                                            !T const
     3    .4398600,.1229100,      .0,      .0,.0,.0,.0,.0,     !T water
     4    .5949300,                                            !T const
     5    .4665200,.1511300,.0037373,      .0,.0,.0,.0,.0,     !Td land 
     6    .4287500,                                            !Td const
     7    .5777500,.0205500,.1048100,      .0,.0,.0,.0,.0,     !Td water
     8    .3578900/                                            !Td const

C
      CALL TIMPR(KFILDO,KFILDO,'START EREST         ')
C
      IER=0
      ITERML=0
      ITERMW=0
      JTERML=0
      JTERMW=0
C
CD     WRITE(KFILDO,105)BQ,NSMTYP,(NSHLN(J),J=1,6)
CD105  FORMAT(/' AT 105 IN EREST--BQ,NSMTYP,(NSHLN(J),J=1,6)',
CD    1         F6.2,I4,4X,6I2)
C
C        CHECK MESH LENGTHS FOR SAFETY.
C
      IF(MESH.NE.MESHE)THEN
C
         IF(IFIRST.EQ.0)THEN
            WRITE(KFILDO,110)
 110        FORMAT(/,' ****THE CLIPPING, TERRAIN, AND SEALAND',
     1               ' GRIDS ARE NOT',
     2               ' AT THE SAME MESH LENGTH AS THE GRID BEING',
     3               ' USED.',/,
     4               '     THE ERROR ESTIMATE CANNOT BE MADE.')
            IFIRST=1
         ENDIF
C
         ISTOP=ISTOP+1
         IER=777
         GO TO 320
C
      ENDIF
C
C        DETERMINE WHICH EQUATION TO EVALUATE.
C
      DO 120 M=1,IDIM
      IF(ID(1)/1000.EQ.IDTAB(M))GO TO 126
 120  CONTINUE
C
      WRITE(KFILDO,121)(ID(J),J=1,4)
 121  FORMAT(/' ****VARIABLE ',3(1X,I9.9),1X,I10.3,
     1        '  NOT ACCOMMODATED IN SUBROUTINE EREST.  AN ERROR',
     2        ' ESTIMATE CANNOT BE MADE.')
      IER=777
      ISTOP=ISTOP+1
      GO TO 320
C
C        PRINT EQUATION BEING EVALUATED.  M IS THE INDEX IN 
C        ITABLE( , ,M) AND RTABLE( , ,M).
C
 126  WRITE(KFILDO,127)
 127  FORMAT(/' EQUATIONS BEING EVALUATED',/,/,
     1        '              LAND                WATER',/,
     2        '   TERM    ID      COEF        ID      COEF')
      WRITE(KFILDO,128)(J,(ITABLE(J,L,M),RTABLE(J,L,M),L=1,2),J=1,JDIM)
 128  FORMAT(/,(I6,I9,F10.5,I11,F10.5))
      WRITE(KFILDO,129)(RTABLE(JDIM+1,L,M),L=1,2)
 129  FORMAT('   CONSTANT',F13.5,F20.5) 
C
C        INITIALIZE THE ERROR GRID IN ERROR( , ) WITH THE 
C        EQUATION CONSTANT.
C  
      DO 138 JY=1,NY
      DO 137 IX=1,NX
C
      IF(CPNDFD(IX,JY).GT..5)THEN
C           EVALUATE ONLY WITHIN THE CLIPPING AREA.
C
         IF(SEALND(IX,JY).GT.8.5)THEN
            ERROR(IX,JY)=RTABLE(JDIM+1,1,M)
         ELSE
            ERROR(IX,JY)=RTABLE(JDIM+1,2,M)
         ENDIF
      ELSE
         ERROR(IX,JY)=9999.
C           THE ERROR OUTSIDE THE CLIPPING AREA WILL BE RETURNED
C           AS MISSING.
      ENDIF
C   
 137  CONTINUE
 138  CONTINUE
C
CD     WRITE(KFILDO,1382)(ERROR(800,JY),JY=1,NY)
CD1382 FORMAT(/' AT 1382 IN EREST--(ERROR(800,JY),JY=1,NY)',/,
CD    1        (10F10.2))
C
C        CLOS2G FINDS THE CLOSEST 2 STATIONS AND EVALUATES THE
C        EQUATION FOR THOSE TERMS, IF THEY ARE IN THE EQUATION.
C
CD     CALL TIMPR(KFILDO,KFILDO,'CALLING CLOS2G      ')
C        
      CALL CLOS2G(KFILDO,ID,
     1            CCALL,XP,YP,LTAG,LNDSEA,ELEV,XLAPSE,XDATA,NSTA,
     2            P,FD4,NEARSV,SVNEAR,ERROR,NX,NY,
     3            CPNDFD,SEALND,TELEV,NXE,NYE,
     4            ITABLE,RTABLE,IDIM,JDIM,M,
     5            ITERML,ITERMW,ISTOP,IER)
CD     WRITE(KFILDO,1385)ITERML,ITERMW
CD1385 FORMAT(/' AT 1385 IN EREST AFTER CLOS2G--ITERML,ITERMW',2I4)
CD     WRITE(KFILDO,139)(ERROR(800,JY),JY=1,NY)
CD139  FORMAT(/' AT 139 IN EREST--(ERROR(800,JY),JY=1,NY)',/,
CD    1        (10F10.2))
      IF(IER.NE.0)GO TO 320
C
CD     CALL TIMPR(KFILDO,KFILDO,'CALLING VARIG       ')
C  
      CALL VARIG(KFILDO,ID,CCALL,XP,YP,LTAG,LNDSEA,XDATA,NSTA,
     1           ERROR,NX,NY,CPNDFD,SEALND,NXE,NYE,
     2           ITABLE,RTABLE,IDIM,JDIM,M,RELVAR,VARTAB,
     3           ITERML,ITERMW,ISTOP,IER)
C
CD     WRITE(KFILDO,1395)ITERML,ITERMW
CD1395 FORMAT(/' AT 1395 IN EREST AFTER VARIG--ITERML,ITERMW',2I4)
CD     WRITE(KFILDO,169)(ERROR(800,JY),JY=1,NY)
CD169  FORMAT(/' AT 169 IN EREST--(ERROR(800,JY),JY=1,NY)',/,
CD    1        (10F10.2))
C
      IF(IER.NE.0)GO TO 320
C
CD     CALL TIMPR(KFILDO,KFILDO,'CALLING VARIWG      ')
C
      CALL VARIWG(KFILDO,ID,
     1            CCALL,XP,YP,LTAG,LNDSEA,XDATA,ELEV,XLAPSE,NSTA,
     1            ERROR,NX,NY,
     2            CPNDFD,SEALND,TELEV,NXE,NYE,
     3            ITABLE,RTABLE,IDIM,JDIM,M,RELVAR,VARTAB,
     4            ITERML,ITERMW,ISTOP,IER)
C
CD     WRITE(KFILDO,1785)ITERML,ITERMW
CD1785 FORMAT(/' AT 1785 IN EREST AFTER VARIWG--ITERML,ITERMW',2I4)
CD     WRITE(KFILDO,179)(ERROR(800,JY),JY=1,NY)
CD179  FORMAT(/' AT 179 IN EREST--(ERROR(800,JY),JY=1,NY)',/,
CD    1        (10F10.2))
      IF(IER.NE.0)GO TO 320
C
CD     CALL TIMPR(KFILDO,KFILDO,'CALLING ROUGHG      ')
C
      CALL ROUGHG(KFILDO,ID,ERROR,NX,NY,
     1            CPNDFD,SEALND,TELEV,NXE,NYE,
     2            ITABLE,RTABLE,IDIM,JDIM,M,
     3            ITERML,ITERMW,ISTOP,IER)
C
CD     WRITE(KFILDO,1885)ITERML,ITERMW
CD1885 FORMAT(/' AT 1885 IN EREST AFTER ROUGHG--ITERML,ITERMW',2I4)
CD     WRITE(KFILDO,189)(ERROR(800,JY),JY=1,NY)
CD189  FORMAT(/' AT 189 IN EREST--(ERROR(800,JY),JY=1,NY)',/,
CD    1        (10F10.2))
C
CD     CALL TIMPR(KFILDO,KFILDO,'CALLING VARILG      ')
C
      CALL VARILG(KFILDO,ID,
     1            CCALL,XP,YP,LTAG,LNDSEA,XDATA,ELEV,XLAPSE,NSTA,
     1            ERROR,NX,NY,
     2            CPNDFD,SEALND,TELEV,NXE,NYE,
     3            ITABLE,RTABLE,IDIM,JDIM,M,RELVAR,VARTAB,
     4            ITERML,ITERMW,ISTOP,IER)
C
CD     WRITE(KFILDO,1985)ITERML,ITERMW
CD1985 FORMAT(/' AT 1985 IN EREST AFTER VARILG--ITERML,ITERMW',2I4)
CD     WRITE(KFILDO,199)(ERROR(800,JY),JY=1,NY)
CD199  FORMAT(/' AT 199 IN EREST--(ERROR(800,JY),JY=1,NY)',/,
CD    1        (10F10.2))
      IF(IER.NE.0)GO TO 320
C
C        MAKE SURE ALL TERMS HAVE BEEN EVALUATED.
C
      DO 300 J=1,JDIM
      IF(ITABLE(J,1,M).NE.0)JTERML=JTERML+1
      IF(ITABLE(J,2,M).NE.0)JTERMW=JTERMW+1
 300  CONTINUE
C
      IF(JTERML.EQ.ITERML)THEN
         WRITE(KFILDO,305)ITERML,(ID(J),J=1,4)
 305     FORMAT(/,' ALL',I3,' TERMS HAVE BEEN EVALUATED ',
     1            ' IN LAND  EQUATION ',3(1X,I9.9),1X,I10.3)
      ELSE
         WRITE(KFILDO,306)ITERML,JTERML,(ID(J),J=1,4)
 306     FORMAT(/,' ****ONLY',I3,' OF',I3,' TERMS HAVE BEEN',
     1            ' EVALUATED IN LAND  EQUATION ',
     2              3(1X,I9.9),1X,I10.3)
         IER=777
         ISTOP=ISTOP+1
      ENDIF
C
      IF(JTERMW.EQ.ITERMW)THEN
         WRITE(KFILDO,315)ITERMW,(ID(J),J=1,4)
 315     FORMAT(' ALL',I3,' TERMS HAVE BEEN EVALUATED ',
     1          ' IN WATER EQUATION ',3(1X,I9.9),1X,I10.3)
      ELSE
         WRITE(KFILDO,316)ITERMW,JTERMW,(ID(J),J=1,4)
 316     FORMAT(/,' ****ONLY',I3,' OF',I3,' TERMS HAVE BEEN',
     1            ' EVALUATED IN WATER EQUATION ',
     2              3(1X,I9.9),1X,I10.3)
         IER=777
         ISTOP=ISTOP+1
      ENDIF
C
      IF(IER.NE.0)GO TO 320
C
C        SMOOTH WITH TERRAIN FOLLOWING SMOOTHER IF DESIRED.
C
      IF(BQ.NE.0..AND.NSMTYP.GE.5)THEN
         CSTSM=0.
C           THIS IS PARAMETER FOR SMOOTHING AT WATER/LAND.
C           USE IT AS ZERO.
         RMESH=1.
C           RMESH IS THE RATIO OF THE MESH LENGTH OF THE ANALYSIS
C           GRID TO THE TERRAIN TELEV( , ) AND SEA/LAND
C           SEALND( , ) GRIDS.  IT IS 1 HERE.
 
         CALL SMOTHG(KFILDO,ERROR,FD4,SVNEAR,NX,NY,BQ,CSTSM,
     1               TELEV,SEALND,NXE,NYE,
     2               RMESH,NSMTYP,NSHLN,IER)
C           THERE IS NO NON-ZERO ERROR RETURN.
      ENDIF
C
      IF(IOCEXT.GT.0)THEN
         CALL ORSMTH(KFILDO,ERROR,NX,NY,MESH,
     1               SEALND,NXE,NYE,MESHE,
     2               IOCEXT,IOCINC,ISTOP,IER)
      ENDIF
C
      GO TO 340
C  
C        MAKE SURE ERROR( , ) IS SET TO MISSING IF ANY
C        ERROR OCCURS.
C
 320  DO 325 JY=1,NY
      DO 324 IX=1,NX
      ERROR(IX,JY)=9999.
 324  CONTINUE
 325  CONTINUE
C
 340  CONTINUE
C
CD     CALL TIMPR(KFILDO,KFILDO,'END   EREST         ')
 350  RETURN
      END      
