      SUBROUTINE WINDSP(KFILDO,KFIL10,ID,IDPARS,JD,NDATE,SDATA,DIR,ND1,
     1                  NSTA,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     2                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                  IS0,IS1,IS2,IS4,ND7,FD1,FD2,FD3,FD4,FD5,FD6,
     4                  ND2X3,ISTAV,L3264B,IER)
C
C        APRIL 2004    SU   MDL   MOS-2000
C
C        PURPOSE
C
C           TO COMPUTE WIND SPEED AT A STATION BY USING THE MODEL OUTPUT U- AND
C           V-COMPONENTS AT FOUR CORNER POINTS OF THE GRID BOX THAT COVERS THE
C           STATION.  SMOOTHING IS DONE ON THE WIND COMPONENTS PER USER'S
C           CHOICE.  INTERPOLATION IS DONE AFTER SMOOTHING, AND THE TYPE OF 
C           INTERPOLATION IS ALSO SELECTED BY THE USER.  THE UNIT OF WIND SPEED
C           SO COMPUTED IS THE SAME AS THAT OF THE COMPONENTS FROM THE MODEL 
C           OUTPUT.  IF EITHER U- OR V-COMPONENT, OR BOTH ARE MISSING, THE WIND
C           SPEED IS GIVEN A MISSING VALUE OF 9999.
C
C           THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C           INTO             FROM
C             004260           004000, 004100 [GRID U- AND V-WIND (P) IN M/SEC]
C             004261           004020, 004120 [GRID U- AND V-WIND (Z) IN M/SEC]
C        
C        DATA SET USE
C
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (FOR SUBROUTINE RDDISK, THROUGH SUBROUTINE GFETCH)
C
C        VARIABLES
C
C               ID(J) = THE PREDICTOR ID (J=1,4).  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID( ) (J=1,15).  (INPUT)
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
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.  (INPUT)
C            SDATA(K) = INTERPOLATED DATA TO RETURN, WHEN STATION DATA ARE
C                       BEING GENERATED (K=1,NSTA).  (OUTPUT)
C          DIR(K,J,M) = THE IX (J=1) AND JY (J=2) POSITIONS ON THE GRID
C                       FOR THE COMBINATION OF GRID CHARACTERISTICS M
C                       (M=1,NGRID) AND STATION K (K=1,NSTA) IN NGRIDC( ,M).
C                       (INPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       FIRST DIMENSION OF DIR( , , ).  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT WITH.
C                       (INPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN BE
C                       DEALT WITH ON THIS RUN.  LAST DIMENSION OF
C                       NGRIDC( , ) AND DIR( , , ).  (INPUT)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND
C                       IN NGRIDC( , ) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  SEE LSTORE(10, ).  FOR THE
C                       COMPUTATION ROUTINES RETURNING A GRID, THIS
C                       VALUE MUST BE OUTPUT BY GFETCH.  (OUTPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(J) = ARRAY TO HOLD RETURNED DATA WHEN THE DATA ARE
C                       AT GRIDPOINTS. (J=1,ND5).  (OUTPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND DATA( ).
C                       (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA
C                       STORED (L=1,12) (J=1,LITEMS).  (INPUT/OUTPUT)
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK,
C                              THIS IS MINUS THE RECORD NUMBER WHERE
C                              THE DATA START.
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDL GRIB, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --NUMBER OF THE SLAB IN DIR( , ,L) AND
C                              IN NGRIDC( ,L) DEFINING THE CHARACTERISTICS
C                              OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,NPRED) FOR WHICH THIS
C                              VARIABLE IS NEEDED, WHEN IT IS NEEDED ONLY
C                              ONCE FROM LSTORE( , ).  WHEN IT IS NEEDED
C                              MORE THAN ONCE, THE VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING MOSTORE( , ).
C                              LATER USED AS A WAY OF DETERMINING WHETHER
C                              TO KEEP THIS VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , ) THAT
C                       HAVE BEEN USED IN THIS RUN.  (INPUT/OUTPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL
C                       DATA ARE STORED ON DISK.  (INPUT/OUTPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NFETCH = THE NUMBER OF TIMES GFETCH HAS BEEN ENTERED.  GFETCH
C                       KEEPS TRACK OF THIS AND RETURNS THE VALUE.  (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C  FD1(J),FD2(J), ETC = WORK ARRAYS (J=1,ND2X3).  THESE MAY BE USED IN
C                       ROUTINES AS 2-DIMENSIONAL ARRAYS, WHERE THE
C                       TOTAL ARRAY SIZE IS ND2*ND3=ND2X3 AS DECLARED IN
C                       THE CALLING PROGRAM.  (INTERNAL)
C               LD(J) = VARIABLE ID (J=1,4).  (INTERNAL)
C               ND2X3 = DIMENSION OF FD1( ), FD2( ), ETC.  (INPUT)
C               ISTAV = 1 WHEN THE DATA RETURNED ARE STATION DATA.
C                       0 WHEN THE DATA RETURNED ARE GRID DATA OR DATA
C                         ARE NOT AVAILABLE FOR RETURN.  (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING USED
C                       (EITHER 32 OR 64).  (INPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        -1 = PREDICTOR NOT DEFINED IN OPTION.
C                        45 = THE TYPE OF SMOOTHING IS NOT ACCOMMODATED.
C                        46 = THE TYPE OF INTERPOLATION IS NOT ACCOMMODATED.
C                       100 = GRID CHARACTERISTICS ARE NOT CONSISTENT BETWEEN
C                             U- AND V-COMPONENTS.
C                       103 = THE REQUESTED VARIABLE IS NOT ACCOMMODATED.
C                       SEE CALLED ROUTINES FOR OTHER VALUES.
C                       (INTERNAL-OUTPUT)
C
C        NON SYSTEM SUBROUTINES CALLED
C
C           GFETCH, SMTH5, SMTH9, SMTH25, SMTH2X, SMTH3X, INTRPA, INTRPB, INTRPC
C 
C*******************************************************************************
C
      IMPLICIT NONE
      INTEGER KFILDO,KFIL10,
     1        NDATE,ND1,NSTA,ND11,NSLAB,ND5,ND9,LITEMS,ND10,
     2        NBLOCK,NFETCH,ND7,ND2X3,ISTAV,L3264B,IER
      INTEGER ICCC,I,ILEVEL,NWORDS,NPACK,NTIMES,MISSP,MISSS,J,
     1        L,NX,NY,NSLABU,
     2        ISMTH,JNTRP,N
      INTEGER ID(4),IDPARS(15),JD(4),LD(4),
     1        IPACK(ND5),IWORK(ND5),
     2        LSTORE(12,ND9),
     3        IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER IFFF(2,2)
      REAL    SDATA(ND1),DIR(ND1,2,ND11),DATA(ND5),
     1        CORE(ND10),
     2        FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),FD5(ND2X3),
     3        FD6(ND2X3)
C
      DATA ICCC/004/,IFFF/000,100,020,120/
C-------------------------------------------------------------------------------
C
C     WRITE(KFILDO,
C    1     FMT='(1X,''**** ID = '',4(1X,I9),/1X,''**** JD = '',4(1X,I9),
C    2     /1X,''**** LD = '',4(1X,I9))') (ID(I),I=1,4),(JD(J),J=1,4),
C    3     (LD(L),L=1,4)
C
C-------------------------------------------------------------------------------
C        (1) CHECK WHETHER THE REQUESTED VARIABLE IS ACCOMMODATED 
C            BY THIS SUBROUTINE
C
      IF((IDPARS(1).EQ.004.AND.IDPARS(2).EQ.260)
     1        .OR.(IDPARS(1).EQ.004.AND.IDPARS(2).EQ.261)) GO TO 100
C
        WRITE(KFILDO,FMT='(1X,''**** THE REQUESTED VARIABLE IS NOT '',
     1        ''ACCOMMODATED BY SUBROUTINE WINDSP'',
     2        /6X,''IDPARS(1) = '',I3,6X,''IDPARS(2) = '',I3)')
     3        (IDPARS(I),I=1,2)
        IER=103
        GO TO 9999
C
C        INITIALIZE THE ERROR CODE
C
 100  IER=0
C
C        SET ISTAV TO INDICATE THAT SUBROUTINE WINDSP WILL RETURN VECTOR DATA
C        (STATION DATA) TO THE CALLING PROGRAM.
C        NOTE: THIS SETTING WOULD MAKE SUBROUTINES PRED21 AND PRED22 SKIP 
C              SMOOTHING AND INTERPOLATION PROCESSES, WHICH ARE PERFORMED IN 
C              THIS SUBROUTINE ALREADY.  
C
      ISTAV=1
C
C-------------------------------------------------------------------------------
C        (2) GET GRID POINT DATA OF U-COMPONENT:
C            CHECK WHETHER THE REQUEST IS FOR ISOBARIC LEVEL OR CONSTANT HEIGHT
C            LEVEL, THEN CONSTRUCT THE ID FOR GRID WIND COMPONENTS AND FETCH THE
C            DATA.
C
      ILEVEL=IDPARS(2)
C
      SELECT CASE(ILEVEL)
C
C        ISOBARIC LEVEL
C
       CASE(260)
           LD(1)=ICCC*1000000+IFFF(1,1)*1000+IDPARS(4)
           LD(2)=ID(2)
           LD(3)=ID(3)
           LD(4)=0
C
C        CONSTANT HEIGHT LEVEL
C
       CASE(261)
           LD(1)=ICCC*1000000+IFFF(1,2)*1000+IDPARS(4)
           LD(2)=ID(2)
           LD(3)=ID(3)
           LD(4)=0
      END SELECT   
C
C        FETCH THE DATA OF GRID U-COMPONENT.
C
        CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4            IER)
C
        IF(IER.NE.0) THEN
          WRITE(KFILDO,FMT='(1X,
     1        ''**** DATA CANNOT BE FOUND BY GFETCH, LD = '',
     2        4I10,''  IER = '',I4)') (LD(J),J=1,4),IER
          GO TO 9999
        ENDIF
C
          NX=IS2(3)
          NY=IS2(4) 
          NSLABU=NSLAB
C
C-------------------------------------------------------------------------------
C        (3) GET GRID POINT DATA OF V-COMPONENT:
C            CHECK WHETHER THE REQUEST IS FOR ISOBARIC LEVEL OR CONSTANT HEIGHT
C            LEVEL, THEN CONSTRUCT THE ID FOR GRID WIND COMPONENTS AND FETCH THE
C            DATA.
C
      ILEVEL=IDPARS(2)
C
      SELECT CASE(ILEVEL)
C
C        ISOBARIC LEVEL
C
       CASE(260)
           LD(1)=ICCC*1000000+IFFF(2,1)*1000+IDPARS(4)
           LD(2)=ID(2)
           LD(3)=ID(3)
           LD(4)=0
C
C        CONSTANT HEIGHT LEVEL
C
       CASE(261)
           LD(1)=ICCC*1000000+IFFF(2,2)*1000+IDPARS(4)
           LD(2)=ID(2)
           LD(3)=ID(3)
           LD(4)=0
      END SELECT   
C
C        FETCH THE DATA OF GRID U-COMPONENT.
C
        CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4            IER)
C
        IF(IER.NE.0) THEN
          WRITE(KFILDO,FMT='(1X,
     1        ''**** DATA CANNOT BE FOUND BY GFETCH, LD = '',
     2        4I10,''  IER = '',I4)') (LD(J),J=1,4),IER
          GO TO 9999
        ENDIF
C
C-------------------------------------------------------------------------------
C        (4) CHECK THE CONSISTENCY OF GRID CHARACTERISTICS ASSOCIATED WITH 
C            FD1 (FOR U-COMPONENT) AND FD2 (FOR V-COMPONENT)
C
      IF(NSLABU.NE.NSLAB.OR.NX.NE.IS2(3).OR.NY.NE.IS2(4)) THEN
        WRITE(KFILDO,FMT='(1X,''**** THE GRID CHARACTERISTICS '',
     1        ''ARE DIFFERENT FOR U- AND V-COMPONENTS.'',
     2        /6X,''NSLABU = '',I3,6X,''NSLAB = '',I3,
     3        /6X,''NX = '',I3,6X,''IS2(3) = '',I3,
     4        /6X,''NY = '',I3,6X,''IS2(4) = '',I3)')
     5        NSLABU,NSLAB,NX,IS2(3),NY,IS2(4)
        IER=100
        GO TO 9999
      ENDIF 
C
C-------------------------------------------------------------------------------
C        (5) SMOOTH GRID POINT DATA AS REQUESTED
C
      ISMTH=IDPARS(14)
C
      IF(ISMTH.LT.0.OR.ISMTH.GT.5) THEN
        WRITE(KFILDO,FMT='(1X,''**** THE TYPE OF SMOOTHING IS NOT '',
     1        ''ACCOMMODATED BY SUBROUTINE WINDSP, IDPARS(14) = '',I2)') 
     2        IDPARS(14)
        IER=45
        GO TO 9999
      ENDIF
C 
       SELECT CASE(ISMTH) 
C
C        NO SMOOTHING
        CASE(0) 
          CONTINUE
C
C        FIVE-POINT SMOOTHING
        CASE(1)
          CALL SMTH5 (KFILDO,FD1,FD3,NX,NY)
          CALL SMTH5 (KFILDO,FD2,FD4,NX,NY)
C
C        NINE-POINT SMOOTHING
        CASE(2)
          CALL SMTH9 (KFILDO,FD1,FD3,NX,NY)
          CALL SMTH9 (KFILDO,FD2,FD4,NX,NY)
C
C        25-POINT SMOOTHING
        CASE(3)
          CALL SMTH25 (KFILDO,FD1,FD3,NX,NY)        
          CALL SMTH25 (KFILDO,FD2,FD4,NX,NY)        
C
C        81-POINT SMOOTHING
        CASE(4)
          CALL SMTH2X (KFILDO,FD1,FD3,NX,NY)        
          CALL SMTH2X (KFILDO,FD2,FD4,NX,NY)        
C
C        169-POINT SMOOTHING
        CASE(5)
          CALL SMTH3X (KFILDO,FD1,FD3,NX,NY)        
          CALL SMTH3X (KFILDO,FD2,FD4,NX,NY)        
C
       END SELECT     
C
C-------------------------------------------------------------------------------
C        (6) INTERPOLATE U- AND V-COMPONENTS FROM GRID POINTS TO STATIONS
C
      JNTRP=IDPARS(13)
      IF(JNTRP.NE.1.AND.JNTRP.NE.2.AND.JNTRP.NE.4) THEN
       WRITE(KFILDO,FMT='(1X,''**** THE TYPE OF INTERPOLATION IS NOT '',
     1       ''ACCOMMODATED BY SUBROUTINE WINDSP, IDPARS(13) = '',I2)') 
     2        IDPARS(13)
        IER=46
        GO TO 9999
      ENDIF
C
C        NOTE: PASSING A 3-D ARRAY DIR INTO A 2-D ARRAY IN INTRP, INTRPA,
C        INTRPB, AND INTRPC BY PASSING ADDRESSES DIR(1,1,NSLABU) AND
C        DIR(1,1,NSLABV).  THE INTERPOLATING SUBROUTINES INTRPA, ETC WILL
C        FOLD THE DATA SEQUENCE INTO THE 2-D ARRAY AND IDENTIFY THE GRID 
C        COORDINATES IX AND IY OF STATIONS.  THEN INTERPOLATE THE DATA TO 
C        STATIONS.
C
       SELECT CASE(JNTRP)
C
C        BIQUADRATIC INTERPOLATION
        CASE(1)
          CALL INTRPA(KFILDO,FD1,NX,NY,DIR(1,1,NSLAB),ND1,NSTA,FD5)
          CALL INTRPA(KFILDO,FD2,NX,NY,DIR(1,1,NSLAB),ND1,NSTA,FD6)
C
C        BILINEAR INTERPOLATION
        CASE(2)
          CALL INTRPB(KFILDO,FD1,NX,NY,DIR(1,1,NSLAB),ND1,NSTA,FD5)
          CALL INTRPB(KFILDO,FD2,NX,NY,DIR(1,1,NSLAB),ND1,NSTA,FD6)
C
C        CLOSEST GRID POINT
        CASE(4)
          CALL INTRPC(KFILDO,FD1,NX,NY,DIR(1,1,NSLAB),ND1,NSTA,FD5)
          CALL INTRPC(KFILDO,FD2,NX,NY,DIR(1,1,NSLAB),ND1,NSTA,FD6)
C
       END SELECT
C
C-------------------------------------------------------------------------------
C        (7) CALCULATE WIND SPEED VALUES AT STATIONS
C
      DO N=1,NSTA
        SDATA(N)=SQRT(FD5(N)*FD5(N)+FD6(N)*FD6(N))
      END DO
      GO TO 1000
C
C-------------------------------------------------------------------------------
C        (8) TERMINATION OF EXECUTION
C
C            ABNORMAL TERMINATION
C
 9999 DO N=1,NSTA
         SDATA(N)=9999.
      END DO
C
      WRITE(KFILDO,
     1     FMT='(1X,''**** ABNORMAL TERMINATION OF SUBROUTINE WINDSP.'',
     2                   5X,''IET = '',I3)') IER
C
C            COMPLETION OF EXECUTION
C
 1000 CONTINUE
C
C     WRITE(KFILDO,
C    1     FMT='(1X,''**** ID = '',4(1X,I9),/1X,''**** JD = '',4(1X,I9),
C    2     /1X,''**** LD = '',4(1X,I9))') (ID(I),I=1,4),(JD(J),J=1,4),
C    3     (LD(L),L=1,4)
C
      RETURN
      END
