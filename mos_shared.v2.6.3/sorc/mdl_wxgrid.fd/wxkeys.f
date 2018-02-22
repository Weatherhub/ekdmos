      SUBROUTINE WXKEYS(KFILDO,KFILAC,ND2X3,IP20,IP21,ND1,
     1                  ID,IDPARS,NDATE,NSTA,CCALL,
     2                  NSVRLOC,NX,NY,JSTA,NPCAT,PPHASE,NXPIN,NXTSW,
     3                  GPPI,GTPI,GSPI,NXWX1,NXWX2,NXWX3,WXID,
     4                  RRMIS,IER)
C
C        SEPTEMBER 2009   HUNTEMANN   MDL   MOS-2000
C        APRIL     2011   HUNTEMANN   MDL   OVERHAULED TO BRING IN 
C                                           THUNDER
C        MARCH     2013   HUNTEMANN   MDL   UPDATED DOCUMENTATION,
C                                           REMOVED OUTDATED CODE,
C                                           ADDED IMPLICIT NONE,
C                                           ADDED JSTA TO CALL AND
C                                           REMOVED CALCULATION OF JSTA
C                                           FROM THIS ROUTINE.
C
C        PURPOSE
C           TO CONVERT FIVE-DIGIT EXPLICIT WEATHER AND THUNDER INTEGERS 
C           TO WEATHER STRINGS, INDEX THE STRINGS, AND WRITE A GRID 
C           OF THE INDICES. CALLED BY GENWX.
C
C           TO ORDER THE STRINGS, A CASE STATEMENT IS USED
C           WITH A CODING SCHEME TO INDICATE THE PLACEMENT OF
C           THE EXPLICIT WEATHER, THUNDER, AND SEVERE SUBKEYS. 
C           THE LOCATION OF SEVERE THUNDERSTORM SUBKEYS IS DETERMINED BY
C           THE FLAG ISVRLOC.
C              
C           ICODE = X Y Z
C                       Z = NUMBER OF EXPLICIT WEATHER ELEMENTS
C                     Y = LOCATION OF THUNDER RELATIVE TO EXPLICIT WX
C                   X = LOCATION OF SEVERE
C                       0 - NO SEVERE
C                       1 - START OF KEY
C                       2 - END OF KEY
C   
C           EXAMPLES:
C
C              ICODE  KEY   EXAMPLE WEATHER KEY
C              -----  ---- -------------------------------------------- 
C              000    NOWX  <NoCov>:<NoWx>:<NoInten>:<NoVis>:
C              001    1     Lkly:S:-:<NoVis>:
C              002    12    Lkly:S:-:<NoVis>:^Lkly:R:-:<NoVis>
C              011    T1    Num:T:<NoInten>:<NoVis>:^Lkly:RW:-:<NoVis>:
C              021    1T    Def:RW:-:<NoVis>:^Sct:T:<NoInten>:<NoVis>:
C              033    12T3  Def:RW:-:<NoVis>:^Lkly:SW:-:<NoVis>:^
C                           Sct:T:<NoInten>:<NoVis>:^SChc:IP:-:<NoVis>:
C              121    S1T   Sct:T:+:<NoVis>:^Def:RW:-:<NoVis>:^
C                           Lkly:T:<NoInten>:<NoVis>:
C              221    1TS   Def:RW:-:<NoVis>:^Lkly:T:<NoInten>:<NoVis>:^
C                           Iso:T:+:<NoVis>
C
C           THIS ROUTINE IS FOR GRIDPOINT DATA.  GRIDPOINT DATA ARE 
C           RETURNED IN WXID( ).
C   
C        DATA SET USE
C           KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C           KFILAC - UNIT NUMBER FOR ASCII OUTPUT OF WEATHER KEY 
C                    LIST (OUTPUT)
C             IP20 - UNIT NUMBER FOR ASCII OUTPUT OF WEATHER KEY
C                    LIST WITH INDICES FOR TROUBLESHOOTING (OUTPUT)
C             IP21 - STATION VALUES OF WEATHER KEYS FOR STATIONS
C                     IN STATION LIST. (INPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              KFILAC = UNIT NUMBER FOR ASCII OUTPUT OF WEATHER KEY 
C                       LIST (INPUT)
C               ND2X3 = DIMENSION OF GRIDPOINT ARRAYS AND WXID( ).  
C                       MUST BEAT LEAST AS LARGE AS THE LARGEST 
C                       GRID. (INPUT)
C                IP20 = INDICATES WHETHER (>0) OR NOT (=0) A DIAGNOSTIC
C                       WEATHER KEY LIST PRINTED WITH CORRESPONDING
C                       INDICES FROM THE GRID WILL BE WRITTEN TO IP20.
C                       SAME OUTPUT AS KFILAC, EXCEPT WITH INDICES FOR 
C                       CHECKOUT. (INPUT)
C                IP21 = STATION VALUES OF WEATHER KEYS FOR STATIONS
C                       IN STATION LIST. (INPUT)
C               ID(J) = THE PREDICTOR ID (J=1,4).  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTORID CORRESPONDING TO ID( ) 
C                       (J=1,15).  (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT WITH.
C                       (INPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST, EXCEPT POSSIBLY 
C                       CCALLD( ).  EQUIVALENCED TO ICALL( , , ). 
C                       (CHARACTER*8)  (INPUT)
C
C         WEATHER GRID INPUT/OUTPUT VARIABLES
C             NSVRLOC = FLAG TO DETERMINE ORDER OF ASCII WEATHER SUBKEYS.
C                          1 - SEVERE ALWAYS FIRST
C                          2 - SCT,NUM,DEF SEVERE ALWAYS FIRST
C                          3 - SHOW LESS SEVERE AT LATER PROJECTIONS
C                       (INPUT) 
C                  NX = NUMBER OF GRIDPOINTS IN X DIRECTION. (INPUT)
C                  NY = NUMBER OF GRIDPOINTS IN Y DIRECTION. (INPUT)
C             JSTA(K) = ARRAY CONTAINING THE GRIDPOINT LOCATIONS OF
C                       THE STATIONS IN THE STATION LIST. 
C                       (K=1,NSTA) (INPUT)
C          NPCAT(I,J) = PRECIPITATION CATEGORY GRIDPOINT FORECASTS 
C                       FOR EACH SUBKEY. SEE SUBROUTINE WXPPHS
C                       FOR DETAILS. (I=1,3) (J=1,ND2X3) 
C                       (INPUT)
C         PPHASE(I,J) = POTENTIAL INDEX GRIDPOINT FORECASTS FOR EACH 
C                       PRECIPITATION PHASE.
C                       PPHASE(1,J) = FREEZING POTENTIAL INDEX
C                       PPHASE(2,J) = FROZEN POTENTIAL INDEX
C                       PPHASE(3,J) = LIQUID POTENTIAL INDEX
C                       (I=1,3) (J=1,ND2X3) (INPUT)
C            NXPIN(J) = PRECIPITATION INTENSITY CATEGORY GRIDPOINT
C                       FORECASTS. SEE SUBROUTINE WXPINT FOR MORE
C                       INFORMATION. (J=1,ND2X3) (INPUT)
C            NXPCH(J) = PRECIPITATION CHARACTER CATEGORY GRIDPOINT
C                       FORECASTS. SEE SUBROUTINE WXPCHR FOR MORE
C                       INFORMATION. (J=1,ND2X3) (INPUT)
C            NXTSW(J) = EXPLICIT THUNDER CODE REPRESENTATION
C                       AT GRIDPOINTS FOR THUNDERSTORMS AND SEVERE
C                       THUNDERSTORMS. SEE SUBROUTINES WXTSTM AND WXTSVR
C                       FOR DETAILS. (J=1,ND2X3) (INPUT)
C             GPPI(J) = PRECIPITATION POTENTIAL INDEX GRIDPOINT 
C                       FORECASTS. (J=1,ND2X3) (INPUT)
C             GTPI(J) = THUNDER POTENTIAL INDEX GRIDPOINT FORECASTS.
C                       SEE SUBROUTINE WXTSTM FOR DETAILS.
C                       (J=1,ND2X3) (INPUT)
C             GSPI(J) = SEVERE POTENTIAL INDEX GRIDPOINT FORECASTS. 
C                       SEE SUBROUTINE WXTSVR FOR DETAILS.
C                       (J=1,ND2X3) (INPUT)
C            NXWX1(J) = EXPLICIT WEATHER CODE REPRESENTATION
C                       AT GRIDPOINTS FOR THE FIRST SUBKEY. SEE
C                       SUBROUTINE WXEXPL FOR MORE INFORMATION.
C                       (J=1,ND2X3) (INPUT)
C            NXWX2(J) = EXPLICIT WEATHER CODE REPRESENTATION
C                       AT GRIDPOINTS FOR THE SECOND SUBKEY. SEE
C                       SUBROUTINE WXEXPL FOR MORE INFORMATION.
C                       (J=1,ND2X3) (INPUT)
C            NXWX3(J) = EXPLICIT WEATHER CODE REPRESENTATION
C                       AT GRIDPOINTS FOR THE THIRD SUBKEY. SEE
C                       SUBROUTINE WXEXPL FOR MORE INFORMATION.
C                       (J=1,ND2X3) (INPUT)
C             WXID(J) = RETURNED DATA (J=1,ND5). (OUTPUT)
C               RRMIS = MISSING DATA FLAG. (INPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       103 = ID NOT ACCOMMODATED 
C                       300 = ERROR IN WEATHER KEY CODE
C                       SEE CALLED ROUTINES FOR OTHER VALUES.
C                       (INTERNAL-OUTPUT)
C
C           INTERNAL VARIABLES
C                NOWX = STRING FOR NON-MISSING GRIDPOINTS WITHOUT 
C                       WEATHER. (INTERNAL)
C           ITABLE(I) = CCCFFF OF THE WEATHER GRID FORECASTS. 
C                       (I=1) (INTERNAL)
C           NXWX(N,J) = ARRAY OF EXPLICIT WEATHER SUBKEYS.
C                       (N=1,3) (J=1,ND2X3) (INTERNAL)
C       WXSUBKEY(N,J) = CHARACTER STRINGS FOR WEATHER SUBKEYS. 
C                       (N=1,5) (J=1,ND2X3) (INTERNAL)
C      WXKEYSTRING(J) = CHARACTER STRING FOR FULL WEATHER KEY.
C                       (J=1,ND2X3) (INTERNAL)
C               ISVR1 = PROJECTION FOR END OF FIRST NDFD FORECAST
C                       PERIOD (FOR DETERMINING ORDER OF SEVERE WEATHER
C                       SUBKEYS).
C               ISVR2 = PROJECTION FOR END OF SECOND NDFD FORECAST
C                       PERIOD (FOR DETERMINING ORDER OF SEVERE WEATHER
C                       SUBKEYS).
C            NUMINDEX = COUNTER FOR WEATHER KEY LIST
C         IWXINDEX(J) = THE INDEX OF THE WXKEY ON THE KEY LIST.
C                       (J=1,ND2X3) (INTERNAL)
C             LINE(J) = THE FULL WEATHER KEY STRING. (J=1,100)
C
C           TEMPORARY VARIABLES
C               ICODE = FLAG USED FOR DETERMINING ORDER OF WEATHER 
C                       SUBKEYS.  SEE EXPLANATION AT TOP OF THIS
C                       ROUTINE.
C                  NP = COMPONENT OF EXPLICIT WEATHER SUBKEY INDICATING 
C                       COVERAGE/PROBABILITY
C                NSSS = COMPONENT OF EXPLICIT WEATHER SUBKEY INDICATING 
C                       WEATHER TYPE
C                  NI = COMPONTENT OF EXPLICIT WEATHER SUBKEY INDICATING 
C                       INTENSITY
C             PSTRING = STRING ASSOCIATED WITH PROBABILITY CATEGORY 
C                       INDICATED BY NP
C             SSTRING = STRING ASSOCIATED WITH PRECIPITATION TYPE 
C                       INDICATED BY NSSS
C             ISTRING = STRING ASSOCIATED WITH INTENSITY INDICATED BY NI
C                  NT = COMPONENT OF THUNDERSTORM SUBKEY INDICATING 
C                       THUNDERSTORM CATEGORY
C                  NS = COMPONENT OF THUNDERSTORM SUBKEY INDICATING 
C                       SEVERE THUNDERSTORM CATEGORY
C             TSTRING = STRING ASSOCIATED WITH THUNDERSTORM CATEGORY
C             VSTRING = STRING ASSOCIATED WITH SEVERE THUNDERSTORM 
C                       CATEGORY
C
C     NONSYSTEM SUBROUTINES USED 
C        UPDAT
C
C***********************************************************************
C
      IMPLICIT NONE
C
C     DECLARE MOS2K SYSTEM VARIABLES:
C
      CHARACTER*8 CCALL(ND1,6)
C
      INTEGER KFILDO,KFILAC,ND2X3,IP20,IP21,ND1,
     1        ID(4),IDPARS(15),NDATE,NSTA,
     2        IER
C
C     DECLARE WEATHER GRID VARIABLES (INPUT/OUTPUT):
C
      INTEGER NSVRLOC,NX,NY,JSTA(NSTA),
     1        NPCAT(3,ND2X3),NXPIN(ND2X3),NXTSW(ND2X3),
     2        NXWX1(ND2X3),NXWX2(ND2X3),NXWX3(ND2X3)
C
      REAL    RRMIS,GPPI(ND2X3),GTPI(ND2X3),GSPI(ND2X3),PPHASE(3,ND2X3),
     1        WXID(ND2X3)
C
C     DECLARE WEATHER GRID VARIABLES (INTERNAL):
C
      CHARACTER*100 WXSUBKEY(5,ND2X3),LINE(500),WXKEYSTRING(ND2X3),NOWX
      CHARACTER*5 PSTRING,SSTRING,ISTRING
      CHARACTER*17 TSTRING,VSTRING
C
      INTEGER I,J,K,L,NP,NI,NSSS,NT,NS,
     1        ICODE,NCYCLE,ISVR1,ISVR2,MDATE,
     2        NUMINDEX,NXWX(3,ND2X3),IWXINDEX(ND2X3)
C
      INTEGER ITABLE(1) / 228500 /
C
C        INITIALIZE VARIABLES
C
      NOWX='<NoCov>:<NoWx>:<NoInten>:<NoVis>:'
      IER=0
C
C***********************************************************************
C
C***D WRITE(KFILDO,90)(ID(J),J=1,4)
  90  FORMAT(' *********** IN WXKEYS *************'/' ',4I10)
C
C        VERIFY THE PROCESSING INDICATOR, IDPARS(1) AND IDPARS(2).
C
      IF(ITABLE(1).EQ.IDPARS(1)*1000+IDPARS(2).AND.
     1                               IDPARS(7).EQ.0)GO TO 108
C
      WRITE(KFILDO,103)(ID(L),L=1,4)
  103 FORMAT(/,' ****WXKEYS ENTERED FOR VARIABLE',
     1        2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2        ' NOT ACCOMMODATED.')
      IER=103
      WXID=RRMIS
      GO TO 370
C  
  108 CONTINUE
C
C        LOOP THROUGH GRIDPOINTS   
      DO 100 J=1,ND2X3
C
C        INTIALIZE FLAG FOR CASE STATEMENT TO ORDER STRINGS
         ICODE=0
C
C        SET ALL GRID POINT INDICES TO MISSING
         IWXINDEX(J)=RRMIS
C
C        INITIALIZE ALL WEATHER KEY STRINGS AS BLANK
         WXKEYSTRING(J)=" "
C
C        SET UP SUBKEY CODES
         NXWX(1,J)=NXWX1(J)
         NXWX(2,J)=NXWX2(J)
         NXWX(3,J)=NXWX3(J)
C
C        IF THERE IS NO EXPLICIT WEATHER CODE AT THIS GRIDPOINT,
C        THEN THE WEATHER GRID WILL BE MISSING AT THIS POINT.
C        THIS LOGIC AND LOGIC IN PRIOR ROUTINES WILL PREVENT 
C        "DRY THUNDERSTORMS" IN THE MOS WEATHER GRID. THOUGH DRY
C        THUNDERSTORMS MAY BE DESIRABLE, THEY ARE NOT CURRENTLY
C        IMPLEMENTED.
C
         IF(NXWX(1,J).EQ.RRMIS)GO TO 100
C
C        GET WEATHER KEY COMPONENTS
C
C        LOOP OVER THE THREE EXPLICIT WEATHER CODES.
C
         DO 200 I=1,3
C
C        CHECK FOR VALID EXPLICIT WEATHER CODE AT THIS GRIDPOINT
C
            IF(NXWX(I,J).NE.RRMIS.AND.NXWX(I,J).NE.0)THEN
C
C        SET UP FLAG FOR ORDER OF WEATHER KEY
C
               ICODE=ICODE+1
C
C        CARVE UP THE EXPLICIT WEATHER CODE FOR TRANSLATION TO STRINGS.
C
               NP=INT(NXWX(I,J)/10000)
               NI=INT(MOD(NXWX(I,J),10))
               NSSS=(INT(MOD(NXWX(I,J),10000))-NI)/10
C
C        SET PROBABILITY/COVERAGE
C
               SELECT CASE (NP)
                  CASE(1)
                     PSTRING='SChc:'
                  CASE(2)
                     PSTRING='Chc:'
                  CASE(3)
                     PSTRING='Lkly:'
                  CASE(5)
                     PSTRING='Def:'
                  CASE DEFAULT
C
C        THERE WAS SOME ERROR IN THE PROBABILITY SECTION OF AN EXPLICIT
C        WEATHER KEY.  SET THIS GRIDPOINT TO NO WEATHER AND MOVE TO NEXT
C        GRIDPOINT.
C
  127  FORMAT(/' ****PROBLEM ASSIGNING WEATHER STRING', 
     1         ' IN WXKEYS FOR VARIABLE = ',
     2         3I10.9,I11.3,'.',/,'\tJ=',I8,
     3         '\tNXWX1(J)=',I5,'\tNXWX2(J)=',I5,
     4         '\tNXWX3(J)=',I5,'\tNXTSW(J)=',I5)
C
C        THIS LINE CAN PRODUCE VOLUMINOUS OUTPUT - USE WITH CAUTION
C**D  WRITE(KFILDO,127)ID,J,NXWX1(J),NXWX2(J),NXWX3(J),NXTSW(J)
C
                     WXKEYSTRING(J)=NOWX
                     IER=300
                     GO TO 100
               END SELECT
C
C        SET CLASS
C
               SELECT CASE (NSSS)
                  CASE(101)
                     SSTRING='R:'
                  CASE(102)
                     SSTRING='RW:'
                  CASE(103)
                     SSTRING='L:'
                  CASE(201)
                     SSTRING='ZR:'
                  CASE(202)
                     SSTRING='ZR:'
                  CASE(203)
                     SSTRING='ZL:'
                  CASE(211)
                     SSTRING='IP:'
                  CASE(212)
                     SSTRING='IP:'
                  CASE(213)
                     SSTRING='IP:'
                  CASE(301)
                     SSTRING='S:'
                  CASE(302)
                     SSTRING='SW:'
                  CASE(303)
                     SSTRING='S:'
                  CASE DEFAULT
C
C        THERE WAS SOME ERROR IN THE PHASE SECTION OF AN EXPLICIT
C        WEATHER KEY.  SET THIS GRIDPOINT TO NO WEATHER AND MOVE TO NEXT
C        GRIDPOINT.
C
C        THIS LINE CAN PRODUCE VOLUMINOUS OUTPUT - USE WITH CAUTION
C**D  WRITE(*,127)ID,J,NXWX1(J),NXWX2(J),NXWX3(J),NXTSW(J)
C
                     WXKEYSTRING(J)=NOWX
                     GO TO 100
                     IER=300
               END SELECT
C
C        SET INTENSITY
C
               SELECT CASE (NI)
                  CASE(2)
                     ISTRING='-:'
                  CASE(3)
                     ISTRING='m:'
                  CASE(4)
                     ISTRING='+:'
                  CASE DEFAULT
C
C        THERE WAS SOME ERROR IN THE INTENSITY SECTION OF AN EXPLICIT
C        WEATHER KEY.  SET THIS GRIDPOINT TO NO WEATHER AND MOVE TO NEXT
C        GRIDPOINT.
C
C        THIS LINE CAN PRODUCE VOLUMINOUS OUTPUT - USE WITH CAUTION
C**D  WRITE(*,127)ID,J,NXWX1(J),NXWX2(J),NXWX3(J),NXTSW(J)
C
                     WXKEYSTRING(J)=NOWX
                     GO TO 100
                     IER=300
               END SELECT
C
C        GET SUBKEY.  THE VISIBILITY COMPONENT OF THE WEATHER SUBKEYS IS
C        POORLY DEFINED, AND SO SET TO <NoVis>.
C
               WXSUBKEY(I,J)=TRIM(PSTRING)//TRIM(SSTRING)//
     1                       TRIM(ISTRING)//'<NoVis>:'
            ENDIF
C
C        END LOOP OVER EXPLICIT WEATHER CODES
C
 200     CONTINUE
C
C        CHECK FOR VALID THUNDERSTORM CODE
C
         IF(NXTSW(J).NE.RRMIS.AND.NXTSW(J).NE.0)THEN
C
C        CARVE UP THE EXPLICIT THUNDERSTORM CODE FOR TRANSLATION
C        TO STRINGS.
C
            NT=INT(NXTSW(J)/10000)
            NS=INT(MOD(NXTSW(J),10000))/1000
C
C        SET UP FLAG FOR ORDER OF THUNDERSTORMS IN WEATHER KEY
C
            IF(NPCAT(1,J).NE.0.AND.NPCAT(1,J).NE.RRMIS)THEN
C
C        THUNDERSTORMS SHOULD GO BEFORE ALL EXPLICIT WEATHER KEYS.
C
               IF(GTPI(J).GE.PPHASE(1,J).AND.
     1            GTPI(J).GE.PPHASE(2,J).AND.
     2            GTPI(J).GE.PPHASE(3,J))THEN
C
                  ICODE=ICODE+10
C
C        THUNDERSTORMS SHOULD GO AFTER THE FIRST EXPLICIT WEATHER KEY.
C
               ELSEIF((GTPI(J).GE.PPHASE(1,J).AND.
     1                 GTPI(J).GE.PPHASE(2,J)).OR.
C
     2                (GTPI(J).GE.PPHASE(1,J).AND.
     3                 GTPI(J).GE.PPHASE(3,J)).OR.
C
     4                (GTPI(J).GE.PPHASE(2,J).AND.
     5                 GTPI(J).GE.PPHASE(3,J)))THEN
C
                  ICODE=ICODE+20
C
C        THUNDERSTORMS SHOULD GO AFTER THE SECOND EXPLICIT WEATHER KEY.
C
               ELSEIF(GTPI(J).GE.PPHASE(1,J).OR.
     1                GTPI(J).GE.PPHASE(2,J).OR.
     2                GTPI(J).GE.PPHASE(3,J))THEN
C
                  ICODE=ICODE+30
C
C        IF THE CATEGORY OF THUNDERSTORMS IS SLIGHT CHANCE OR GREATER,
C        THUNDERSTORMS SHOULD GO AFTER THE THIRD EXPLICIT WEATHER KEY.
C
C              ELSEIF(GTPI(J).GE.15.)THEN
               ELSEIF(NT.GE.1..AND.NT.NE.RRMIS)THEN
                  ICODE=ICODE+40
               ENDIF
            ENDIF
C
C        BECAUSE SEVERE THUNDERSTORMS IS A HIGH-IMPACT ELEMENT,
C        IMAGES PRODUCED BY DEGRIB/SUPERIMAGEGEN ALLOW WEATHER KEYS WITH
C        SEVERE THUNDERSTORMS TO SUPERCEDE OTHER COLORS WHEN T+ IS
C        PLACED IN THE FIRST TWO SLOTS OF THE WEATHER STRING, REGARDLESS
C        OF SEVERE CATEGORY.  THIS SWITCH ALLOWS THE USER TO DOWNPLAY
C        LOWER SEVERE CATEGORIES IN THE SUPERIMAGEGEN WEB IMAGES.
C
            SELECT CASE (NSVRLOC)
C
C        SEVERE ALWAYS FIRST
C
               CASE(1)
                  IF(NS.NE.0) ICODE=ICODE+100
C
C        SCT, NUM, DEF SEVERE ALWAYS FIRST, ISO SEVERE LAST
C
               CASE(2)
                  IF(NS.GT.1)THEN
                     ICODE=ICODE+100
                  ELSEIF(NS.GT.0)THEN
                     ICODE=ICODE+200
                  ENDIF
C
C        ENSURE THAT THE IMAGES PRODUCED BY DEGRIB/SUPERIMAGEGEN
C        RESEMBLE NDFD (SHOW LESS RED FOR LATER PROJECTIONS).
C           - 6-H TO ISVR1: SEVERE ALWAYS GOES FIRST
C           - ISVR1 TO ISVR2: SCT/NUM/DEF SEVERE GOES FIRST, ELSE LAST
C           - ISVR2 TO 84-H: SEVERE ALWAYS GOES LAST
C
               CASE(3)
                  NCYCLE=MOD(NDATE,100)
                  IF(NCYCLE.EQ.0)THEN
                     ISVR1=36
                     ISVR2=60
                  ELSE
                     ISVR1=24
                     ISVR2=48
                  ENDIF
C
                  IF(NS.NE.0)THEN
                     IF(IDPARS(12).LE.ISVR1)THEN
                        ICODE=ICODE+100
                     ELSEIF(IDPARS(12).LE.ISVR2)THEN
                        IF(NS.GT.1)THEN
                           ICODE=ICODE+100
                        ELSE
                           ICODE=ICODE+200
                        ENDIF
                     ELSE
                        IF(NS.GT.2)THEN
                           ICODE=ICODE+100
                        ELSE
                           ICODE=ICODE+200
                        ENDIF
                     ENDIF
                  ENDIF
C
               CASE DEFAULT 
                  ICODE=ICODE+100
            END SELECT
C
C        SET THUNDERSTORM CATEGORY
C
            IF(NT.EQ.5)THEN
               TSTRING='Def:T:<NoInten>:'
            ELSEIF(NT.EQ.3)THEN
               TSTRING='Num:T:<NoInten>:'
            ELSEIF(NT.EQ.2)THEN
               TSTRING='Sct:T:<NoInten>:'
            ELSEIF(NT.EQ.1)THEN
               TSTRING='Iso:T:<NoInten>:'
            ELSE
C
C        THESE WAS SOME ERROR IN THE THUNDER SECTION OF THE THUNDER
C        WEATHER KEY.  SET THIS GRIDPOINT TO NO THUNDER.
C
C        THIS LINE CAN PRODUCE VOLUMINOUS OUTPUT - USE WITH CAUTION
C**D  WRITE(KFILDO,127)ID,J,NXWX1(J),NXWX2(J),NXWX3(J),NXTSW(J)
C
               TSTRING=' '
            ENDIF
C
C        SET SEVERE CATEGORY
C
            IF(NS.EQ.5)THEN
               VSTRING='Def:T:+:'
            ELSEIF(NS.EQ.3)THEN
               VSTRING='Num:T:+:'
            ELSEIF(NS.EQ.2)THEN
               VSTRING='Sct:T:+:'
            ELSEIF(NS.EQ.1)THEN
               VSTRING='Iso:T:+:'
            ELSE
C
C        THESE WAS SOME ERROR IN THE SEVERE THUNDER SECTION OF THE THUNDER
C        WEATHER KEY.  SET THIS GRIDPOINT TO NO SEVERE THUNDER AND MOVE TO NEXT
C        GRIDPOINT.
C
C        THIS LINE CAN PRODUCE VOLUMINOUS OUTPUT - USE WITH CAUTION
C**D  WRITE(KFILDO,127)ID,J,NXWX1(J),NXWX2(J),NXWX3(J),NXTSW(J)
C
               VSTRING=' '
            ENDIF
C
C        ASSEMBLE THUNDER AND SEVERE THUNDER SUBKEYS FOR WEATHER KEYS
C        WITH THUNDER AND SEVERE THUNDER PRESENT.
C
            IF(NT.NE.0)THEN
               WXSUBKEY(4,J)=TRIM(TSTRING)//'<NoVis>:'
            ENDIF
C
            IF(NS.NE.0)THEN
               WXSUBKEY(5,J)=TRIM(VSTRING)//'<NoVis>:'
            ENDIF
C
C        END VALID THUNDERSTORM CHECK
C
         ENDIF
C    
C        NOW PUT ALL AVAILABLE SUBKEYS TOGETHER
C           WXSUBKEY(1,J) IS FIRST EXPLICIT WEATHER ELEMENT
C           WXSUBKEY(2,J) IS SECOND EXPLICIT WEATHER ELEMENT
C           WXSUBKEY(3,J) IS THIRD EXPLICIT WEATHER ELEMENT
C           WXSUBKEY(4,J) IS THUNDER ELEMENT
C           WXSUBKEY(5,J) IS SEVERE ELEMENT
C
C        SEE NOTE IN HEADER BLOCK FOR INFORMATION ON ICODE VALUES.
C
         SELECT CASE (ICODE)
C 
            CASE(0)
C              DON'T DO ANYTHING.
               WXKEYSTRING(J)=NOWX
            CASE(1)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(1,J))
            CASE(2)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(1,J))//'^'//
     1                        TRIM(WXSUBKEY(2,J))
            CASE(3)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(1,J))//'^'//
     1                        TRIM(WXSUBKEY(2,J))//'^'//
     2                        TRIM(WXSUBKEY(3,J))
            CASE(10)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(4,J))
            CASE(11)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(4,J))//'^'//
     1                        TRIM(WXSUBKEY(1,J))
            CASE(12)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(4,J))//'^'//
     1                        TRIM(WXSUBKEY(1,J))//'^'//
     2                        TRIM(WXSUBKEY(2,J))
            CASE(13)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(4,J))//'^'//
     1                        TRIM(WXSUBKEY(1,J))//'^'//
     2                        TRIM(WXSUBKEY(2,J))//'^'//
     3                        TRIM(WXSUBKEY(3,J))
            CASE(21)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(1,J))//'^'//
     1                        TRIM(WXSUBKEY(4,J))
            CASE(22)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(1,J))//'^'//
     1                        TRIM(WXSUBKEY(4,J))//'^'//
     2                        TRIM(WXSUBKEY(2,J))
            CASE(23)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(1,J))//'^'//
     1                        TRIM(WXSUBKEY(4,J))//'^'//
     2                        TRIM(WXSUBKEY(2,J))//'^'//
     3                        TRIM(WXSUBKEY(3,J))
            CASE(32)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(1,J))//'^'//
     1                        TRIM(WXSUBKEY(2,J))//'^'//
     2                        TRIM(WXSUBKEY(4,J))
            CASE(33)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(1,J))//'^'//
     1                        TRIM(WXSUBKEY(2,J))//'^'//
     2                        TRIM(WXSUBKEY(4,J))//'^'//
     3                        TRIM(WXSUBKEY(3,J))
            CASE(43)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(1,J))//'^'//
     1                        TRIM(WXSUBKEY(2,J))//'^'//
     2                        TRIM(WXSUBKEY(3,J))//'^'//
     3                        TRIM(WXSUBKEY(4,J))
C
C        NOTE: IF SEVERE IS FIRST SUBKEY, DO NOT PUT THUNDER IN 
C              SECOND SLOT. THIS IS SO THAT SAMPLE POINTS ON PLOTS
C              GENERATED USING SUPERIMAGEGEN ARE NOT REDUNDANT 
C              (E.G., T+/T).
C
            CASE(111)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(5,J))//'^'//
     1                        TRIM(WXSUBKEY(1,J))//'^'//
     2                        TRIM(WXSUBKEY(4,J))
            CASE(112)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(5,J))//'^'//
     1                        TRIM(WXSUBKEY(1,J))//'^'//
     2                        TRIM(WXSUBKEY(4,J))//'^'//
     3                        TRIM(WXSUBKEY(2,J))
            CASE(113)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(5,J))//'^'//
     1                        TRIM(WXSUBKEY(1,J))//'^'//
     2                        TRIM(WXSUBKEY(4,J))//'^'//
     3                        TRIM(WXSUBKEY(2,J))//'^'//
     4                        TRIM(WXSUBKEY(3,J))
            CASE(121)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(5,J))//'^'//
     1                        TRIM(WXSUBKEY(1,J))//'^'//
     2                        TRIM(WXSUBKEY(4,J))
            CASE(122)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(5,J))//'^'//
     1                        TRIM(WXSUBKEY(1,J))//'^'//
     2                        TRIM(WXSUBKEY(4,J))//'^'//
     3                        TRIM(WXSUBKEY(2,J))
            CASE(123)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(5,J))//'^'//
     1                        TRIM(WXSUBKEY(1,J))//'^'//
     2                        TRIM(WXSUBKEY(4,J))//'^'//
     3                        TRIM(WXSUBKEY(2,J))//'^'//
     4                        TRIM(WXSUBKEY(3,J))
            CASE(132)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(5,J))//'^'//
     1                        TRIM(WXSUBKEY(1,J))//'^'//
     2                        TRIM(WXSUBKEY(2,J))//'^'//
     3                        TRIM(WXSUBKEY(4,J))
            CASE(133)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(5,J))//'^'//
     1                        TRIM(WXSUBKEY(1,J))//'^'//
     2                        TRIM(WXSUBKEY(2,J))//'^'//
     3                        TRIM(WXSUBKEY(4,J))//'^'//
     4                        TRIM(WXSUBKEY(3,J))
            CASE(143)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(5,J))//'^'//
     1                        TRIM(WXSUBKEY(1,J))//'^'//
     2                        TRIM(WXSUBKEY(2,J))//'^'//
     3                        TRIM(WXSUBKEY(3,J))//'^'//
     4                        TRIM(WXSUBKEY(4,J))
C
C        NOTE: IF SEVERE IS FIRST SUBKEY, DO NOT PUT THUNDER IN 
C              SECOND SLOT. THIS IS SO THAT SAMPLE POINTS ARE NOT 
C              REDUNDANT (E.G., T+/T)
C
            CASE(211)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(4,J))//'^'//
     1                        TRIM(WXSUBKEY(1,J))//'^'//
     2                        TRIM(WXSUBKEY(5,J))
            CASE(212)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(4,J))//'^'//
     1                        TRIM(WXSUBKEY(1,J))//'^'//
     2                        TRIM(WXSUBKEY(2,J))//'^'//
     3                        TRIM(WXSUBKEY(5,J))
            CASE(213)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(4,J))//'^'//
     1                        TRIM(WXSUBKEY(1,J))//'^'//
     2                        TRIM(WXSUBKEY(2,J))//'^'//
     3                        TRIM(WXSUBKEY(3,J))//'^'//
     4                        TRIM(WXSUBKEY(5,J))
            CASE(221)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(1,J))//'^'//
     1                        TRIM(WXSUBKEY(4,J))//'^'//
     2                        TRIM(WXSUBKEY(5,J))
            CASE(222)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(1,J))//'^'//
     1                        TRIM(WXSUBKEY(4,J))//'^'//
     2                        TRIM(WXSUBKEY(2,J))//'^'//
     3                        TRIM(WXSUBKEY(5,J))
            CASE(223)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(1,J))//'^'//
     1                        TRIM(WXSUBKEY(4,J))//'^'//
     2                        TRIM(WXSUBKEY(2,J))//'^'//
     3                        TRIM(WXSUBKEY(3,J))//'^'//
     4                        TRIM(WXSUBKEY(5,J))
            CASE(232)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(1,J))//'^'//
     1                        TRIM(WXSUBKEY(2,J))//'^'//
     2                        TRIM(WXSUBKEY(4,J))//'^'//
     3                        TRIM(WXSUBKEY(5,J))
            CASE(233)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(1,J))//'^'//
     1                        TRIM(WXSUBKEY(2,J))//'^'//
     2                        TRIM(WXSUBKEY(4,J))//'^'//
     3                        TRIM(WXSUBKEY(3,J))//'^'//
     4                        TRIM(WXSUBKEY(5,J))
            CASE(243)
               WXKEYSTRING(J)=TRIM(WXSUBKEY(1,J))//'^'//
     1                        TRIM(WXSUBKEY(2,J))//'^'//
     2                        TRIM(WXSUBKEY(3,J))//'^'//
     3                        TRIM(WXSUBKEY(4,J))//'^'//
     4                        TRIM(WXSUBKEY(5,J))
C
            CASE DEFAULT 
               WXKEYSTRING(J)=NOWX
         END SELECT
C
 100  CONTINUE
C
C        NOW THAT EACH GRIDPOINT HAS AN ASSIGNED WEATHER KEY STRING,
C        LOOP THROUGH THE GRID AGAIN AND ASSIGN INDICES.
C
C        START INDEX COUNTER
C
      NUMINDEX=1
C
      DO 300 K=1,ND2X3
C
C        IF THERE IS WEATHER, ASSIGN AN INDEX
C
         IF(NXWX(1,K).NE.RRMIS)THEN 
C   
C        IF GRIDPOINT DOES NOT HAVE AN INDEX, GIVE IT ONE
C
            IF(IWXINDEX(K).EQ.RRMIS)THEN
C
C        ASSIGN THE INDEX AND STORE LINE FOR WRITING.
C
               IWXINDEX(K)=NUMINDEX-1
               LINE(NUMINDEX)=TRIM(WXKEYSTRING(K))
C
C        LOOP THROUGH REMAINING GRIDPOINTS AND CHECK FOR 
C        IDENTICAL WEATHER KEYS
C
               DO 401 L=K+1,ND2X3
                  IF(NXWX(1,K).EQ.NXWX(1,L).AND.
     1               NXWX(2,K).EQ.NXWX(2,L).AND.
     2               NXWX(3,K).EQ.NXWX(3,L))THEN  
C
C        BECAUSE IT IS POSSIBLE TO HAVE MISSING OR ZERO THUNDER
C        WITH NON-MISSING WEATHER, MAKE THIS CHECK.  THE MOS
C        THUNDERSTORM GRID DOMAIN DOES NOT MATCH THE MOS POP GRID
C        DOMAINS AS OF JANUARY 2013.  IDEALLY, THE CHECK OF NWTSW 
C        AGAINST 9999 AND 0 WOULD BE REMOVED, RETAINING JUST 
C        IWXINDEX(K).EQ.NXTSW(L) CHECK.
C
                     IF(NXTSW(K).NE.9999.AND.NXTSW(K).NE.0)THEN
                        IF(NXTSW(K).EQ.NXTSW(L))THEN
                           IWXINDEX(L)=NUMINDEX-1
                        ENDIF
                     ELSE
                        IWXINDEX(L)=NUMINDEX-1
                     ENDIF
                  ENDIF
 401           CONTINUE
C
C
C        ADD TO THE INDEX COUNTER
C
               NUMINDEX=NUMINDEX+1
            ENDIF
         ENDIF
C
C        SET WXID EQUAL TO IWXINDEX.  IN WXKEYS ROUTINE WE REQUIRE THE
C        DATA (GRID OF INDICES) TO BE INTEGERS, BUT CALLING ROUTINE 
C        GENWX REQUIRES RETURNED DATA TO BE REAL.
C        
         WXID(K)=IWXINDEX(K)
C
 300  CONTINUE
C
C        WRITE WEATHER KEYS TO KFILAC FOR U135. KFILAC WILL BE
C        THE GRIB2 SECTION 2 CONTROL FILE IN U135.
C
 305  IF(NUMINDEX-1.EQ.0)THEN
         WRITE(KFILAC,310)(ID(K),K=1,4),1
         WRITE(KFILAC,320)NOWX
         WRITE(KFILAC,330)999999,777777
      ELSE
         WRITE(KFILAC,310)(ID(K),K=1,4),(NUMINDEX-1)
 310     FORMAT(I9.9,' ',I9.9,' ',I9.9,' ',I3.3,' ',I3)
         WRITE(KFILAC,320)(LINE(K),K=1,(NUMINDEX-1))
 320     FORMAT(A100)
         WRITE(KFILAC,330)999999,777777
 330     FORMAT(I9,1X,I9)
      ENDIF
C
C        WRITE INDICES AND WEATHER KEYS TO IP20 IF REQUESTED.
C
      IF(IP20.GT.0)THEN
         WRITE(IP20,340)(ID(I),I=1,4),(NUMINDEX-1)
 340     FORMAT(/' DATA FOR ',I9.9,' ',I9.9,' ',I9.9,' ',I3.3,' ',I3)
         DO K=1,NUMINDEX-1
            WRITE(IP20,350)K-1,LINE(K)
 350        FORMAT(I3,' ',A100)
         ENDDO
         WRITE(IP20,330)999999,777777
      ENDIF
C
C        WRITE WEATHER KEYS AT STATIONS TO IP21 IF REQUESTED.
C
      IF(IP21.GT.0)THEN
C
C        DETERMINE VALID DATE FOR IP21
C
         CALL UPDAT(NDATE,IDPARS(12),MDATE)
 360     FORMAT(/,A10,2X,A10,2X,A5,3X,A8,3X,A8,3X,
     1             A8,3X,A8,3X,A8,3X,A8,3X,A8,3X,A8,3X,A11)
         WRITE(IP21,360)"FCST TIME ","VALID TIME","STN  ","PPI","ZPI",
     1   "FPI","RPI","TPI","SPI","NXPIN","WXID","WEATHER KEY"
         DO K=1,NSTA
            IF(JSTA(K).GT.0.AND.JSTA(K).LE.ND2X3)THEN
 365           FORMAT(I10,2X,I10,2X,A5,3X,F8.1,3X,F8.1,3X,F8.1,3X,
     1                F8.1,3X,F8.1,3X,F8.1,3X,I5,3X,F8.1,3X,A100)
C                 
               WRITE(IP21,365) NDATE,MDATE,CCALL(K,1),
     1            GPPI(JSTA(K)),PPHASE(1:3,JSTA(K)),
     2            GTPI(JSTA(K)),GSPI(JSTA(K)),
     3            NXPIN(JSTA(K)),WXID(JSTA(K)),WXKEYSTRING(JSTA(K))
            ELSE
 366           FORMAT(I10,2X,I10,2X,A5,3X,A13)
               WRITE(IP21,366) NDATE,MDATE,CCALL(K,1),"OUT OF BOUNDS"
            ENDIF
         ENDDO
      ENDIF
C
 370  RETURN
      END
