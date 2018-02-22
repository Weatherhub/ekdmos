       SUBROUTINE GRDCNST(KFILDO,IDPARS,JD,NDATE,
     1                    KFILRA,RACESS,NUMRA,CCALL,
     2                    NAME,STALAT,STALON,SDATA,
     3                    DIR,ND1,NSTA,NGRIDC,NGRID,ND11,NSLAB,
     4                    IPACK,IWORK,DATA,ND5,
     5                    IS0,IS1,IS2,IS4,ND7,FD1,FD2,ND2X3,
     6                    IP12,ISTAV,L3264B,IER)
C
C         FEB 2006   CARROLL                MDL MOS-2000
C         MAR 2006   CARROLL                RESTRUCTURED LOOPS, GOT RID
C                                           OF AUTOMATIC ARRAYS.  FIXED
C                                           TO HANDLE MULTIPLE CONSTANT
C                                           FILES.
c         AUG 2007   RLC 		    MODIFIED TO DO THIS USING
c                                           5-DAY NORMALS ALSO
C                                           
C              
C        PURPOSE
C            TO CALCULATE THE NORMAL DAILY MAXIMUM AND MINIMUM TEMPERATURE
C            USING PRISM 5-KM 30 DAY MEAN CLIMATOLOGICAL VALUES.  THE VALUES
C            ARE ON THE GFS POLAR STEREOGRAPHIC GRID AND ARE SPACIALLY
C            INTERPOLATED TO A STATION BASED ON THE NEAREST NEIGHBOR AND 
C            TEMPORALLY INTERPOLATED LINEARLY.  THE ABILITY TO SMOOTH FIELDS
C            IS ACCOMMODATED.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               432 540 - NORMAL MAXIMUM TEMPERATURE - from monthly
C               432 541 - NORMAL MINIMUM TEMPERATURE - from monthly
C               422 540 - NORMAL MAXIMUM TEMPERATURE - from 5-day
C               422 541 - NORMAL MINIMUM TEMPERATURE - from 5-day
C
C        DATA SET USE
C            KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT(PRINT) FILE.
C                     (OUTPUT)
C              IP12 = INDICATES WHETHER (>0) OR NOT (=0) THE LIST OF
C                     STATIONS ON THE INPUT FILES WILL BE PRINTED TO
C                     THE FILE WHOSE UNIT NUMBER IS IP12.
C
C        VARIABLES
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST, EXCEPT POSSIBLY
C                       CCALLD( ) AND CCALLP( ).  (CHARACTER*8) (INPUT)
C           DATA(ND5) = ARRAY TO HOLD THE RETURNED DATA AS STATION DATA
C                       TO CALCULATE THE NORMAL MAX/MIN TEMPERATURE.
C                       (INTERNAL)
C           DIR(K,J,) = THE IX (J=1) AND JY (J=2) POSITIONS ON THE
C                       GRID FOR STATION K (K=1,NSTA).  (INPUT)
C              FD1(J) = WORK ARRAY USED TO HOLD THE FIRST INTERPOLATED
C                       MAX/MIN VALUE USED TO LINEARLLY COMPUTE THE DAILY
C                       VALUE (J=1,ND2X3). (INTERNAL)
C              FD2(J) = WORK ARRAY USED TO HOLD THE SECOND INTERPOLATED
C                       MAX/MIN VALUE USED TO LINEARLLY COMPUTE THE DAILY
C                       VALUE (J=1,ND2X3). (INTERNAL)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       PREDICTOR ID CORRESPONDING TO ID() (J=1,15).
C                       (INPUT)
C                       J=1--CCC      (CLASS OF VARIABLE),
C                       J=2--FFF      (SUBCLASS OF VARIABLE),
C                       J=3--B        (BINARY INDICATOR),
C                       J=4--DD       (DATA SOURCE, MODEL NUMBER),
C                       J=5--V        (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY
C                                      1 LAYER)
C                       J=7--LTLTLTLT (TOP OF LAYER)
C                       J=8--T        (TRANSFORMATION)
C                       J=9--RR       (RUN TIME OFFSET, ALWAYS +
C                                      AND BACK IN TIME)
C                       J=10-OT       (TIME APPLICATION)
C                       J=11-OH       (TIME PERIOD IN HOURS)
C                       J=12-TAU      (PROJECTION IN HOURS)
C                       J=13-I        (INTERPOLATION TYPE)
C                       J=14-S        (SMOOTHING INDICATOR)
C                       J=15-G        (GRID INDICATOR)
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                       100 = THE TWO GRIDS NEEDED ARE NOT THE SAME SIZE
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE
C                             THE LAPSE RATE.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C                IP12 = INDICATES WHETHER (>0) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE INPUT FILES WILL BE PRINTED TO
C                       THE FILE WHOSE UNIT NUMBER IS IP12. (INPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5). (INTERNAL)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).
C                       (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).
C                       (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       IS2(3) AND IS2(4) ARE USED BY THE CALLING
C                       PROGRAM AS THE GRID DIMENSIONS.
C                       (INTERNAL-OUTPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).
C                       (INTERNAL)
C               ISMTH = VARIABLE THAT HOLDS THE SMOOTHING TYPE
C                       DESIRED.  EQUAL TO IDPARS(14). (INTERNAL)
C               ISTAV = 1 SINCE THE DATA RETURNED ARE STATION DATA.
C                       (OUTPUT)
C            IWORK(J) = WORK ARRAY (J=1,ND5). (INTERNAL)
C                   J = LOOP CONTROL VARIABLE. (INTERNAL)
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE PORTIONS
C                       PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       JD( ) IS USED TO IDENTIFY THE BASIC MODEL FIELDS
C                       AS READ FROM THE ARCHIVE.  (INPUT)
C           JDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       CONSTANT ID CORRESPONDING TO NORMALS OBTAINED IN
C                       CONSTG. (J=1,15). (INTERNAL).
C               JNTRP = VARIABLE THAT HOLDS THE SPACIAL INTERPOLATION
C                       TYPE.  THIS ROUTINE ONLY SUPPORTS INTERPOLATION
C                       4, OR NEAREST NEIGHBOR.  (INTERNAL)
C                   K = LOOP CONTROL VARIABLE. (INTERNAL)
C               KD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4) FOR THE
C                       MONTHLY MEAN MAX/MIN PRIOR TO OR EQUAL TO THE
C                       DATE DESIRED. (INTERNAL)
C             KDX,KDY = DIMENSIONS OF GRID RETURNED FOR THE FIRST MAX/MIN
C                       NORMAL TEMPERATURE USED TO GENERATE THE DAILY NORMAL
C                       TEMPERATURE. (INTERNAL)
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT)
C           KFILRA(J) = HOLDS THE UNIT NUMBERS FOR ACCESSING THE MOS-2000
C                       EXTERNAL RANDOM ACCESS FILES (J=1,NUMRA).  (INPUT)
C                   L = LOOP CONTROL VARIABLE. (INTERNAL)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64). (INPUT)
C               LD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4) FOR THE
C                       MONTHLY MEAN MAX/MIN ON OR AFTER THE
C                       DATE DESIRED. (INTERNAL)
C             LDX,LDY = DIMENSIONS OF GRID RETURNED FOR THE LAST MAX/MIN
C                       NORMAL TEMPERATURE USED TO GENERATE THE DAILY NORMAL
C                       TEMPERATURE. (INTERNAL)
C               MD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4) FOR THE
C                       MAX/MIN CONSTANT ID. (INTERNAL)
C                MDOY = JULIAN DAY OF YEAR.  (INTERNAL)
C             NAME(J) = NAMES OF STATIONS (J=1,NSTA).  USED FOR PRINTOUT
C                       ONLY.  (CHARACTER*20)  (INPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       DIMENSION OF SEVERAL VARIABLES.  (INPUT)
C               ND2X3 = DIMENSION OF IPACK( ), IWORK( ), FD1( ),
C                       FD2( ), FD3( ), AND FD4( ).  (INPUT)
C                 ND5 = DIMENSION OF IPACK( ), AND IWORK( ) AND
C                       FDRV( ) TO ND5.  (INPUT)
C                 ND7 = DIMENSION OF IS0(),IS1(),IS2(), AND IS4().
C                       NOT ALL LOCATIONS ARE USED. (INPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN
C                       BE DEALT WITH ON THIS RUN.  LAST DIMENSION
C                       OF NGRIDC(,). (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C               NGRID = THE NUMBER OF GRID COMBINATIONS IN NGRIDC( , ),
C                       MAXIMUM OF ND11.  (INPUT-OUTPUT)
C         NGRIDC(L,M) = HOLDS THE GRID CHARACTERISTICS (L=1,6) FOR
C                       EACH GRID COMBINATION (M=1,NGRID).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=
C                            POLAR STEREOGRAPHIC).
C                       L=2--GRID LENGTH IN METERS.
C                       L=3--LATITUDE AT WHICH THE GRID LENGTH IS
C                            CORRECT *1000.
C                       L=4--GRID ORIENTATION IN DEGREES * 1000.
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *1000.
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES
C                            *1000.
C               NSLAB = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID. (OUTPUT)
C              NSLABK = SAME AS NSLAB.  RETURNED FROM CALLING GRCOMB
C                       FOR THE FIRST NORMAL MAX/MIN. (INTERNAL)
C              NSLABL = SAME AS NSLAB.  RETURNED FROM CALLING GRCOMB
C                       FOR THE LAST NORMAL MAX/MIN. (INTERNAL)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT WITH
C                       (INPUT).
C               NUMRA = THE NUMBER OF UNIT NUMBERS AND NAMES IN KFILRA( )
C                       AND RACESS( ).  (INPUT)
C                   R = INTERPOLATION FACTOR, THE FRACTION OF THE WAY
C                       MDOY IS FROM THE FIRST OF THE TWO VALUES TO BE
C                       USED IN INTERPOLATION. (INTERNAL)
C           RACESS(J) = THE FILE NAME ASSOCIATED WITH KFILRA.
C                       (CHARACTER*60) (INPUT).
C            SDATA(J) = DATA ARRAY TO HOLD RETURNED DATA AS
C                       STATION DATA. IN THIS CASE, THE NORMAL MAX/MIN
C                       TEMPERATURE(J=1,NSTA). (OUTPUT)
C           STALAT(J) = LATITUDE OF STATIONS (J=1,NSTA).  (INPUT)
C           STALON(J) = LONGITUDE OF STATIONS (J=1,NSTA).  (INPUT)
C
C     NON SYSTEM SUBROUTINES USED
C        COMPID, CONSTG, GRCOMB, INTRPC, PRSID1, SMTH5, 
C        SMTH9, SMTH25, SMTH2X, SMTH3X
C
      IMPLICIT NONE
C
      CHARACTER*60 RACESS(NUMRA)
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*20 NAME(ND1)
C
      INTEGER IDPARS(15),JDPARS(15)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER NGRIDC(6,ND11)
      INTEGER ID(4),JD(4),LD(4),MD(4),KD(4)
      INTEGER KFILRA(NUMRA)
      INTEGER I,IER,IP12,ISMTH,ISTAV,J,JNTRP,K,
     1        KFILDO,L3264B,
     2        KDX,KDY,LDX,LDY,
     3        ND1,ND5,ND7,ND11,ND2X3,
     4        NDATE,NGRID,
     5        NSLAB,NSLABK,NSLABL,NSTA,
     6        NUMRA,MDOY
C
C
      REAL DATA(ND5)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3)
      REAL STALAT(ND1),STALON(ND1),SDATA(ND1)
      REAL DIR(ND1,2,ND11)
      REAL R
C
      IER=0
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE NORMAL
C        MAXIMUM OR MINIMUM TEMPERATURE.
C
      IF(((IDPARS(1).NE.432).AND.(IDPARS(1).NE.422)).OR.
     1                            ((IDPARS(2).NE.540).AND.
     2                            (IDPARS(2).NE.541)))THEN
         WRITE(KFILDO,FMT='(1X,''****IDPARS(1) DOES NOT INDICATE '',
     1         ''CLIMATOLOGICAL NORMAL TEMPERATURES. '',
     2         /''      PREDICTOR '',I9.9,I10.9,I10.9,I4.3,'' NOT '',
     3         ''ACCOMODATED IN GRDCNST.  IER ='',I4)') (JD(J),J=1,4)
         IER=103
         GOTO 800
      ENDIF
C
C        MAKE SURE THAT THE NEAREST NEIGHBOR INTERPOLATION IS BEING
C        USED.
C
      IF(IDPARS(13).NE.4) THEN
         WRITE(KFILDO,FMT='(1X,''****THE TYPE OF INTERPOLATION IS '',
     1       ''NOT ACCOMMODATED BY SUBROUTINE GRDCNST, IDPARS(13) = '',
     2       I2)') IDPARS(13)
         IER=160
         GOTO 800
      ENDIF
C
C        MAKE SURE A CORRECT SMOOTHING TYPE IS SPECIFIED.
C
C
      IF(IDPARS(14).LT.0.OR.IDPARS(14).GT.5) THEN
         WRITE(KFILDO,FMT='(1X,''****THE TYPE OF SMOOTHING IS NOT '',
     1      ''ACCOMMODATED BY SUBROUTINE GRDCNST, IDPARS(14)= '',I2)')
     2      IDPARS(14)
         IER=150
         GOTO 800
      ENDIF
C
C        SET UP JDPARS TO THE RIGHT VARIABLE ID FOR THE MAX/MIN
C        TEMPERATURE TO BE OBTAINED IN CONSTG AND PARSE THE ID.
C
      JD(1)=IDPARS(1)*1000000+IDPARS(2)*1000
      JD(2)=0
      JD(3)=IDPARS(12)
      JD(4)=0
C
      CALL PRSID1(KFILDO,JD,JDPARS)
C
C        CALL COMPID TO FETCH THE MONTHLY NORMAL MAX OR MIN FROM
C        ON OR BEFORE THE DAY REQUESTED (KD) AND ON OR AFTER THE
C        THE DAY REQUESTED (LD) NEEDED FOR INTERPOLATION.
C
C        THE INTERPOLATION FACTOR, THE FRACTION OF THE WAY
C        MDOY IS FROM THE FIRST OF THE TWO VALUES TO BE
C        USED IN INTERPOLATION.  IF INTERPOLATION
C        IS NOT NEEDED, R IS SET TO 0.  (OUTPUT)
C
      CALL COMPID(KFILDO,JD,JDPARS,NDATE,KD,LD,MDOY,R,IER)
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,FMT='(1X,''****CALL TO COMPID FAILED IN '',
     1      ''GETTING THE NORMAL MAX/MIN IDS FOR THE 2 PERIODS '',
     2      ''NEEDED.  IER = '',I3,''.'' )') IER
          GOTO 800
      ENDIF
C
C        SET UP LOOP TO GET THE TWO MONTHLY NORMAL MAX OR MIN 
C        TEMPERATURES NEEDED TO GET THE DAILY NORMAL.
C
      DO 700 I=1,2
C
C        FIND THE FILE UNIT NUMBER AND NAME BY ASSOCIATION
C        WITH CCC.  THESE VALUES ARE HARDWIRED.
C
         IF(I.EQ.1) THEN
            DO 105 J=1,NUMRA
C
C        THIS IF TEST ALLOWS CONSTANT DATA TO BE READ FROM UNITS
C        42, 43, OR 44, AND TO ALLOW ANY ID'S TO BE READ FROM THE
C        GRIDDED FILES
C        FILES.
C
               IF(KFILRA(J).EQ.42.OR.KFILRA(J).EQ.43.OR.
     1            KFILRA(J).EQ.44) THEN
                     CALL CONSTG(KFILDO,KFILRA(J),RACESS(J),KD,
     1                           IPACK,IWORK,DATA,ND5,
     2                           IS0,IS1,IS2,IS4,ND7,
     3                           ISTAV,L3264B,IER)
               END IF
 105        CONTINUE
C
               IF(IER.NE.0) THEN
                  WRITE(KFILDO,FMT='(1X,''****CALL TO CONSTG '',
     1               ''COMPUTING THE FIRST NORMAL MAX/MIN FAILED. '',
     2               '' IER = '',I3,''.'' )') IER
                     GOTO 800
               END IF
C
C        OBTAIN THE GRID CHARACTERISTICS.
C
               KDX=IS2(3)
               KDY=IS2(4)
C
C        CALL GRCOMB TO ESTABLISH THE GRID PARAMETERS AND LOCATE
C        STATIONS ON THE GRID.
C
               CALL GRCOMB(KFILDO,IP12,IS2,ND7,NGRIDC,ND11,NGRID,
     1                     NSLABK,CCALL,NAME,STALAT,STALON,DIR,ND1,
     2                     NSTA,IER)
C
               IF(IER.NE.0) THEN
                  WRITE(KFILDO,FMT='(''****FIRST CALL TO GRCOMB '',
     1                  ''FAILED IN GRDCNST.  IER='',I3,''.'')') IER
                  GOTO 800
               ENDIF
         ENDIF 
C
C        CALL CONSTG TO FETCH THE SECOND TEMPERATURE NEEDED TO COMPUTE
C        THE DAILY MEAN.
C
         IF(I.EQ.2) THEN
C
            DO 115 J=1,NUMRA
C
C        THIS IF TEST ALLOWS CONSTANT DATA TO BE READ FROM UNITS
C        42, 43, OR 44, AND TO ALLOW ANY ID'S TO BE READ FROM THE
C        GRIDDED FILES
C        FILES.
C
            IF(KFILRA(J).EQ.42.OR.KFILRA(J).EQ.43.OR.
     1         KFILRA(J).EQ.44)THEN
C
               CALL CONSTG(KFILDO,KFILRA(J),RACESS(J),LD,
     1                     IPACK,IWORK,DATA,ND5,
     2                     IS0,IS1,IS2,IS4,ND7,
     3                     ISTAV,L3264B,IER)
            ENDIF
 115     CONTINUE
C
            IF(IER.NE.0)THEN
               WRITE(KFILDO,FMT='(''****CALL TO CONSTG COMPUTING '',
     1            ''THE SECOND NORMAL MAX/MIN FAILED.  IER = '',I3,
     2            ''.'')') IER
               GOTO 800
            END IF
C
C        OBTAIN THE GRID CHARACTERISTICS.
C
            LDX=IS2(3)
            LDY=IS2(4)
C
C        CALL GRCOMB A SECOND TIME SO GRID CHARACTERISTICS CAN BE
C        MATCHED.
C
            CALL GRCOMB(KFILDO,IP12,IS2,ND7,NGRIDC,ND11,NGRID,
     1                  NSLABL,CCALL,NAME,STALAT,STALON,DIR,
     2                  ND1,NSTA,IER)
C
               IF(IER.NE.0) THEN
                  WRITE(KFILDO,FMT='(''****SECOND CALL TO GRCOMB '',
     1                  ''FAILED IN GRDCNST'','' IER='',I3,
     2                  ''.'')') IER
                  GOTO 800
               ENDIF
C
C        CHECK TO MAKE SURE THE GRID CHARACTERISTICS OF THE NSLABS MATCH
C        AFTER THE CALLS TO GRCOMB.
C
               IF(NSLABK.NE.NSLABL)THEN
                  WRITE(KFILDO,180)NSLABK,NSLABL,(JD(J),J=1,4)
 180                 FORMAT(/' ****THE GRID CHARACTERISTICS OF THE ',
     1               ' 2 CALLS TO CONSTG DO NOT MATCH ',
     2               '.',I3,2X,I3,
     3               /'     VARIABLE ',I9.9,2I10.9,I4.3,
     4               ' NOT COMPUTED IN GRDCNST.  FATAL ERROR')
C
                  IER=100
                  GOTO 800
               END IF
C
C         CHECK TO MAKE SURE THE X AND Y COMPONENTS OF THE GRIDS MATCH.
C
               IF((KDX.NE.LDX).OR.(LDY.NE.LDY)) THEN
                   WRITE(KFILDO,FMT='(''****GRID CHARACTERISTS DO '',
     1             ''NOT MATCH IN CONSTGRD.  KDX='',I10,'' AND KDY='',
     2             I10,'' AND LDX='',I10,'' AND LDY='',I10,''.'')')
               GOTO 800
               ENDIF
         ENDIF
C
C        SMOOTH GRID POINT DATA BEFORE INTERPOLATION.
C
         ISMTH=IDPARS(14)
         IF(ISMTH.EQ.0) GOTO 500
C 
            SELECT CASE(ISMTH) 
C
C        FIVE-POINT SMOOTHING
C
               CASE(1)
                  CALL SMTH5 (KFILDO,DATA,IWORK,KDX,KDY)
C
C        NINE-POINT SMOOTHING
C
               CASE(2)
                  CALL SMTH9 (KFILDO,DATA,IWORK,KDX,KDY)
C
C        25-POINT SMOOTHING
C
               CASE(3)
                  CALL SMTH25 (KFILDO,DATA,IWORK,KDX,KDY)
C
C        81-POINT SMOOTHING
C
               CASE(4)
                  CALL SMTH2X (KFILDO,DATA,IWORK,KDX,KDY)
C
C        169-POINT SMOOTHING
C
               CASE(5)
                  CALL SMTH3X (KFILDO,DATA,IWORK,KDX,KDY)
C
            END SELECT
C
C        INTERPOLATE FROM GRIDPOINTS TO STATIONS.  NOTE:  ONLY
C        NEAREST NEIGHBOR (I=4) IS ACCOMODATED.
C        NSLAB IS SET TO NSLABL FOR UNIFORMITY OF CODE.
C
 500   IF(I.EQ.1) THEN
          CALL INTRPC(KFILDO,DATA,KDX,KDY,DIR(1,1,NSLABK),ND1,NSTA,FD1)
       ELSEIF(I.EQ.2) THEN
          CALL INTRPC(KFILDO,DATA,LDX,LDY,DIR(1,1,NSLABL),ND1,NSTA,FD2)
       END IF
 700   CONTINUE
C
C        INTERPOLATE NORMAL MAX OR MIN TEMPORALLY AND COMPUTATE THE
C        DESIRED NORMAL MAX OR MIN TEMPERATURE.
C
      DO 725 K=1,NSTA
         IF((NINT(FD1(K)).EQ.9999).OR.(NINT(FD2(K)).EQ.9999)) THEN
            SDATA(K)=9999.
         ELSE
            SDATA(K)=(FD2(K)-FD1(K))*R+FD1(K)
         ENDIF
 725  CONTINUE
C
      GOTO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C 
 800  DO 801 J=1,ND1
        SDATA(J)=9999.
 801  CONTINUE
C
 900  ISTAV=1
      RETURN
      END
