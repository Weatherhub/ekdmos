      SUBROUTINE MEANRH(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                  NGRIDC,ND11,NSLAB,IPACK,IWORK,FDMRH,ND5,
     2                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                  IS0,IS1,IS2,IS4,ND7,
     4                  FD1,FD2,FD3,FD4,FD5,FD6,FD7,ND2X3,
     5                  ISTAV,L3264B,MISTOT,IER)
C
C        JANUARY  1999   SU      TDL MOS-2000
C
C        DECEMBER 2000   RUDACK  MODIFIED CODE TO COMPLY WITH MDL
C                                FORMAT SPECIFICATIONS
C        OCTOBER  2002   WEISS   CHANGE ND5 TO ND2X3
C        APRIL    2003   GLAHN   MODIFIED LINES IN CALL; SET
C                                DIMENSIONS OF IPACK( ), IWORK( ) AND
C                                FDMRH( ) = ND5;  MODIFIED LINES IN
C                                CALLS TO SPECHUM AND ND5 TO ND2X3;
C                                COUNTED MISTOT AFTER CALL TO GFETCH;
C                                ELIMINATED *** IN DIAGNOSTICS
C        MAY      2003   GLAHN   REARRANGED TYPE STATEMENTS; IMPROVED
C                                DIAGNOSTICS
C
C        PURPOSE
C            TO COMPUTE MEAN RELATIVE HUMIDITY FOR A SPECIFIED LAYER
C            BOUNDED BY TWO ISOBARIC LEVELS.  THE OUTPUT DATA ARE
C            GIVEN WITH UNITS OF PERCENT (%).
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C                003 040 - MEAN RH FOR NGM ARCHIVE.
C                          NECESSARY DATA AVAILABLE AT:
C                          1000, 950, 900, 850, 800,
C                          750, 700, 500 AND 300 MB.
C                003 041 - MEAN RH FOR MRF AND AVN ARCHIVE
C                          (PRIOR TO OCT 1, 1998).
C                          NECESSARY DATA AVAILABLE AT:
C                          1000, 925, 850, 700, 500 AND 300 MB.
C                003 042 - MEAN RH FOR ETA ARCHIVE.
C                          NECESSARY DATA AVAILABLE AT:
C                          1000, 950, 900, 850, 800, 750,
C                          700, 600, 500, 400 AND 300 MB)
C                003 043 - MEAN RH FOR MRF AND AVN ARCHIVE
C                          (SUBSEQUENT TO SEPT 30, 1998).
C                          NECESSARY DATA AVAILABLE AT:
C                          1000, 975, 950, 925, 900, 850,
C                          800, 750, 700, 600, 500 AND 300 MB.
C
C
C        DATA SET USE
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                     (INPUT-OUTPUT)
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                     (OUTPUT) 
C 
C        VARIABLES 
C             CORE(J) = ARRAY TO STORE OR RETRIEVE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL,
C                       DATA ARE STORED ON DISK.  UPON RETURN TO THE
C                       CALLING PROGRAM, THE ARRAY WILL BE IN THE SIZE
C                       OF LX*LY.  (OUTPUT).
C               DELTX = AN INTEGRATION FACTOR WHOSE VALUE DEPENDS ON
C                       THE DISTANCE BETWEEN A SPECIFIC LEVEL AND ITS
C                       ADJACENT LEVEL(S).  (INTERNAL).
C              FD1(J) = WORK ARRAY TO HOLD PRESSURE (PA), USED IN
C                       SUBROUTINE SPECHUM.  (J=1,ND2X3) (INTERNAL)
C              FD2(J) = WORK ARRAY TO HOLD TEMPERATURE (DEG K), USED IN
C                       SUBROUTINE SPECHUM.  (J=1,ND2X3) (INTERNAL)
C              FD3(J) = WORK ARRAY TO HOLD RELATIVE HUMIDITY (%)
C                       (J=1,ND2X3).  (INTERNAL)
C              FD4(J) = WORK ARRAY TO HOLD SPECIFIC HUMIDITY (GM/GM)
C                       (J=1,ND2X3).  (INTERNAL)
C              FD5(J) = WORK ARRAY TO HOLD SATURATED SPECIFIC HUMIDITY
C                       (GM/GM) (J=1,ND2X3).  (INTERNAL)
C              FD6(J) = ACCUMULATOR WORK ARRAY TO HOLD THE SUM OF LEVEL
C                       SPECIFIC HUMIDITY(GM/GM) (J=1,ND2X3) (INTERNAL)
C              FD7(J) = ACCUMULATOR WORK ARRAY TO HOLD THE SUM OF LEVEL
C                       SATURATED SPECIFIC HUMIDITY(GM/GM) (J=1,ND2X3).
C                       (INTERNAL)
C            FDMRH(J) = DATA ARRAY TO HOLD MEAN RELATIVE HUMIDITY (%)
C                       (J=1,ND5).  (OUTPUT)
C                  IB = INDEX OF THE BOTTOM LEVEL OF THE SPECIFIED
C                       ISOBARIC LAYER.  (INTERNAL)
C                ICCC = FIRST WORD OF THE PARSED ID, I.E. VARIABLE
C                       CLASS WHICH EQUALS TO 003 IN THIS SUBROUTINE.
C                       (INTERNAL)
C                 I,J = LOOP COUNT.  (INTERNAL)
C           IDPARS(J) = PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID(J) (J=1,15) DEFINED IN
C                       THE CALLING PROGRAM.  (INPUT)
C                       J=1  -- CCC (CLASS OF VARIABLE)
C                       J=2  -- FFF (SUBCLASS OF VARIABLE)
C                       J=3  -- B (BINARY INDICATOR)
C                       J=4  -- DD (DATA SOURCE, MODEL NUMBER)
C                       J=5  -- V (VERTICAL APPLICATION)
C                       J=6  -- LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1
C                               LEVEL)
C                       J=7  -- LTLTLTLT (TOP OF LAYER)
C                       J=8  -- T (TRANSFORMATION)
C                       J=9  -- RR (RUN TIME OFFSET -- PREVIOUS CYCLE.
C                               IT IS ALWAYS A POSITIVE NUMBER AND
C                               COUNTED BACKWARDS IN TIME.)
C                       J=10 -- OT (TIME APPLICATION)
C                       J=11 -- OH (TIME PERIOD IN HOURS)
C                       J=12 -- TAU (PROJECTION IN HOURS)
C                       J=13 -- I (INTERPOLATION TYPE)
C                       J=14 -- S (SMOOTHING INDICATOR)
C                       J=15 -- G (GRID INDICATOR)
C           IDPASH(J) = PARSED, SAME AS IDPARS(J) BUT USED TO STORE
C                       THE PREDICTOR ID OF SPECIFIC HUMIDITY (J=1,15).
C                       (INTERNAL}
C              IDTMPL = IDENTIFIER OF A TEMPLATE WHICH CONTAINS THE
C                       ISOBARIC LEVELS ASSOCIATED WITH THE OUTPUT
C                       DATA OF A SPECIFIED NWP MODEL (INTERNAL).
C                       1 -- NGM
C                       2 -- MRF (ALSO, OLD AVN)
C                       3 -- ETA
C                       4 -- AVN
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                       100 = GRID CHARACTERISTICS ARE DIFFERENT FOR
C                             THE 2 FIELDS.
C                       103 = IDPARS() ARE NOT ACCOMMODATED IN THIS
C                             SUBROUTINE.
C                       SEE GFETCH AND SPECHUM FOR OTHER VALUES WHEN
C                       IER.NE.0 AND DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C             IFFF(J) = SECOND WORD OF THE PARSED ID, I.E. VARIABLE
C                       SUBCLASS WHICH IS MEAN RH IN THIS SUBROUTINE
C                       (J=1,4).  (INTERNAL)
C                       040 -- NGM
C                       041 -- MRF (ALSO, OLD AVN)
C                       042 -- ETA
C                       043 -- AVN
C               IFFFQ = SAME AS IFFF BUT USED TO STORE THE SUBCLASS ID
C                       OF SPECIFIC HUMIDITY.  (INTERNAL)
C               IFFFR = SAME AS IFFF BUT USED TO STORE THE SUBCLASS ID
C                       OF RELATIVE HUMIDITY.  (INTERNAL)
C                  IL = LOOP COUNT FOR ISOBARIC LEVELS (INTERNAL).
C             ILIM(J) = NUMBER OF ISOBARIC LEVELS IN THE TEMPLATES
C                       (J=1,4).  (INTERNAL)
C               ILIMJ = TEMPORARY STORAGE FOR THE UPPER BOUND OF LOOP
C                       INDEX.  (INTERNAL)
C            IPACK(J) = WORK ARRAY (J=1,ND5) (INTERNAL)
C              IS0(J) = MOS-2000 GRIB SECTION 0 IDS (J=1,3). (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 IDS (J=1,22+). (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 IDS (J=1,12)
C                       IS2(3) AND IS2(4) ARE USED BY THE CALLING
C                       PROGRAM AS GRID DIMENSION.  (INTERNAL-OUTPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 IDS (J=1,4).  (INTERNAL)
C               ISTAV = 0 -- WHEN THE DATA RETURNED ARE GRID DATA OR
C                            DATA ARE NOT AVAILABLE FOR RETURN.
C                       1 -- WHEN THE DATA RETURNED ARE STATION DATA.
C                       (OUTPUT)
C                  IT = INDEX OF THE TOP LEVEL OF THE SPECIFIED
C                       ISOBARIC LAYER.  (INTERNAL)
C       ITEMPLAT(I,J) = STORAGE FOR ISOBARIC LEVELS IN TEMPLATES
C                       (I=1,12, J=1,4).  (INTERNAL)
C         ITMPL,JTMPL = DIMENSIONS OF THE TEMPLATE ARRAY (INTERNAL).
C                  IV = VERTICAL PROCESSING FLAG.  CURRENTLY SET TO BE
C                       ZERO; MAY BE ASSIGNED DIFFERENT VALUE(S) IN
C                       THE FUTURE.  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5) (INTERNAL)
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).  THIS
C                       IS THE SAME AS ID(J) EXCEPT THAT THE PORTIONS
C                       PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3)
C                       T = IDPARS(8)
C                       I = IDPARS(13)
C                       S = IDPARS(14)
C                       G = IDPARS(15)
C                       JD( ) IS USED TO HELP IDENTIFY THE BASIC MODEL
C                       FIELDS AS READ FROM THE ARCHIVE (INPUT)
C             JDRH(J) = SAME AS JD(J) EXCEPT FOR RELATIVE HUMIDITY
C                       (J=1,4).  (INTERNAL)
C             JDSH(J) = SAME AS JD(J) EXCEPT FOR SPECIFIC HUMIDITY
C                       (J=1,4).  (INTERNAL)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS DEPENDING ON THE
C                       MACHINE BEING USED (EITHER 32 OR 64).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN.  (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS) (INPUT-OUTPUT)
C                       L=1,4-- THE 4 IDS FOR THE DATA
C                       L=5  -- LOCATION OF STORED DATA.  WHEN IN CORE,
C                               THIS IS THE LOCATION IN CORE( ) WHERE
C                               THE DATA START.  WHEN ON DISK, THIS IS
C                               MINUS THE RECORD NUMBER WHERE THE DATA
C                               START.
C                       L=6  -- THE NUMBER OF 4-BYTE WORDS STORED
C                       L=7  -- 2 FOR DATA PACKED IN TDL GRIB FORMAT,
C                               1 OTHERWISE
C                       L=8  -- DATE/TIME OF THE DATA IN FORMAT
C                               YYYYMMDDHH
C                       L=9  -- NO. OF TIMES DATA HAVE BEEN RETRIEVED
C                       L=10 -- NUMBER OF THE SLAB IN DIR( , ,L) AND IN
C                               NGRIDC(,L) DEFINING THE CHARACTERISTICS
C                               OF THE GRID
C                       L=11 -- NUMBER OF PREDICTORS IN THE SORTED LIST
C                               IN IS( ,N) (N=1,NPRED) FOR WHICH THIS
C                               VARIABLE IS NEEDED ONLY ONCE FROM
C                               LSTORE( , ).  WHEN IT IS NEEDED MORE
C                               THAN ONCE, THE VALUE IS SET TO 7777.
C                       L=12 -- USED INITIALLY IN ESTABLISHING
C                               MSTORE( , ).  LATER USED TO DETERMINE
C                               WHETHER TO KEEP THIS VARIABLE
C           LXRH,LYRH = DIMENSIONS OF THE GRID RETURNED WITH THE DATA
C                       OF RELATIVE HUMIDITY.  (INTERNAL)
C           LXSH,LYSH = DIMENSIONS OF THE GRID RETURNED WITH THE DATA
C                       OF SPECIFIC HUMIDITY.  (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       ZERO FROM CALLING GFETCH WHEN THERE IS NO
C                       MISSING VALUE.  (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       ZERO FROM CALLING GFETCH WHEN THERE IS NO
C                       SECONDARY MISSING VALUE.  (INTERNAL)
C              MISTOT = TOTAL NUMBER OF MISSING ITEMS ENCOUNTED IN
C                       UNPACKING GRIDS.  (INPUT-OUTPUT)
C              NBLOCK = BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM DISK
C                       FILE.  (INPUT)
C               ND(J) = WORK ARRAY HOLDING THE 4 ID WORDS OF RELATIVE
C                       HUMIDITY DATA BEING RETRIEVED INTO FD3( )
C                       (J=1,4).  (INTERNAL)
C               ND2X3 = DIMENSION OF FD1( ), FD2( ), ETC.  (INPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND FDSH( ).
C                       (INPUT)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4().
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN BE
C                       DEALT WITH IN THIS RUN.  LAST DIMENSION OF
C                       NGRIDC( , ).  (INPUT)
C               NDATE = DATE/TIME FOR WHICH THE PREDICTOR IS NEEDED.
C                       (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.  IT IS
C                       A RUNNING COUNT FROM THE BEGINNING OF THE MAIN
C                       PROGRAM.  THIS COUNT IS MAINTAINED IN CASE THE
C                       USER NEEDS IT FOR DIAGNOSTICS, ETC.. (OUTPUT)
C         NGRIDC(L,M) = HOLDING THE GRID CHARACTERISTICS (L=1,6) FOR
C                       EACH GRID COMBINATION(M=1,NGRID).  (INPUT-OUTPUT)
C                       L=1 -- MAP PROJECTION NUMBER(3=LAMBERT, 5=POLAR
C                              STEREOGRAPHIC)
C                       L=2 -- GRID LENGTH IN METERS
C                       L=3 -- LATITUDE AT WHICH GRID LENGTH IS CORRECT
C                              *1000
C                       L=4 -- GRID ORIENTATION IN DEGREES *1000
C                       L=5 -- LATITUDE OF LL CORNER IN DEGREES *1000
C                       L=6 -- LONGITUDE OF LL CORNER IN DEGREES *1000
C               NPACK = 2 FOR TDL GRIB PACKED DATA; 1 OTHERWISE.  THIS
C                       IS RETURNED FROM CALLING GFETCH . (INTERNAL)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND IN
C                       NGRIDC( , ) DEFINING THE CHARACTERISTICS OF THE
C                       GRID.  IT IS USED TO IDENTIFY THE DATA SOURCE,
C                       I.E., THE MODEL.  (OUTPUT)
C              NSLABL = SAME AS NSLAB.  RETURNED FROM CALLING GFETCH
C                       FOR PRESSURE.  (INTERNAL)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT
C                       THE RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ).  (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN FD1( ), FD2( ), AND
C                       FD3( ).  THIS IS RETURNED FROM CALLING GFETCH.
C                       (INTERNAL)
C
C        NON-SYSTEM SUBROUTINES USED
C            GFETCH, PRSID1, SPECHUM
C
      IMPLICIT NONE
C
      INTEGER, PARAMETER :: ITMPL=12,JTMPL=4
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER IFFF(4),JDRH(4),JDSH(4),IDPASH(15)
      INTEGER ITEMPLAT(ITMPL,JTMPL),ILIM(JTMPL)
      INTEGER KFILDO,KFIL10,I,IB,ICCC,IDTMPL,IER,IFFFQ,IFFFR,
     1        IL,ILIMJ,ISTAV,IT,IV,J,L3264B,LITEMS,LXRH,LXSH,LYRH,LYSH,
     2        MISSP,MISSS,MISTOT,ND2X3,ND5,ND7,ND9,ND10,ND11,
     3        NBLOCK,NDATE,NFETCH,NPACK,NSLAB,NSLABL,NTIMES,NWORDS
C
      REAL FDMRH(ND5)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),FD5(ND2X3),
     1     FD6(ND2X3),FD7(ND2X3)
      REAL CORE(ND10)
      REAL DELTX
C
      DATA ICCC/003/,IFFF/040,041,042,043/
      DATA IV/0/
      DATA IFFFQ/030/,IFFFR/000/
C
      DATA ILIM/9,6,11,12/
      DATA (ITEMPLAT(I,1),I=1,9)
     1               /1000,950,900,850,800,750,700,500,300/,
     2     (ITEMPLAT(I,2),I=1,6)
     3               /1000,925,850,700,500,300/,
     4     (ITEMPLAT(I,3),I=1,11)
     5               /1000,950,900,850,800,750,700,600,500,400,300/,
     6     (ITEMPLAT(I,4),I=1,12)
     7               /1000,975,950,925,900,850,800,750,700,600,500,300/
C
C        RESET ERROR CODE
C
      IER=0
      ISTAV=0
C
C        INITIALIZE ACCUMULATOR WORK ARRAYS.  THE SIZE NX BY NY IS
C        NOT KNOWN YET, AND GFETCH IS IN THE LAYER LOOP.  LEAVE IT 
C        HERE.
C
      DO I=1,ND2X3
        FD6(I)=0.
        FD7(I)=0.
      END DO
C
C        *****************************************************
C        BEGINNING OF PREDICTOR ID VERIFICATION ---           
C        VERIFY INDIVIDUAL WORDS OF THE PREDICTOR IDENTIFIER: 
C        *****************************************************
C
C        VERIFY THE VARIABLE CLASS
C
      IF(IDPARS(1).NE.ICCC) THEN
        WRITE(KFILDO,100) IDPARS(1),(JD(J),J=1,4)
 100  FORMAT(/' ****THE VALUE OF IDPARS(1) IS NOT FOR THE MOISTURE',
     1        ' VARIABLES IN MEANRH.  IDPARS(1) = ',I3,'.',
     2       /'     VARIABLE ',I9.9,2I10.9,I4.3,' NOT COMPUTED.')
        IER=103
        GO TO 700
C
      END IF
C
C        VERIFY THE SUBCLASS OF VARIABLE AND ASSIGN A LAYER TEMPLATE
C
      IF(IDPARS(2).EQ.IFFF(1)) THEN
        IDTMPL=1
      ELSEIF(IDPARS(2).EQ.IFFF(2)) THEN
        IDTMPL=2
      ELSEIF(IDPARS(2).EQ.IFFF(3)) THEN
        IDTMPL=3
      ELSEIF(IDPARS(2).EQ.IFFF(4)) THEN
        IDTMPL=4
      ELSE
        WRITE(KFILDO,101) IDPARS(2),(JD(J),J=1,4)
 101  FORMAT(/' ****THE VALUE OF IDPARS(2) IS NOT FOR THE MEAN',
     1        ' RELATIVE HUMIDITY IN MEANRH.',
     2        '  IDPARS(2) = ',I3,'.',
     3       /'     VARIABLE ',I9.9,2I10.9,I4.3,' NOT COMPUTED.')
        IER=103
        GO TO 700
C
      END IF
C
C        VERIFY THE VERTICAL PROCESSING FLAG
C        *******************************************************
C        REMARK: IV IS CURRENTLY SET TO BE ZERO (0).            
C                THE FOLLOWING BLOCK IS RESERVED FOR FUTURE USE 
C                OF DIFFERENT VERTICAL PROCESSING FLAG.         
C        *******************************************************
C
      IF(IDPARS(5).NE.IV) THEN
        WRITE(KFILDO,103) IDPARS(5),(JD(J),J=1,4)
 103  FORMAT(/' ****THE VALUE OF IDPARS(5) IS NOT A VERTICAL',
     1        ' PROCESSING FLAG',
     2        ' THAT CAN BE HANDLED BY SUBROUTINE MEANRH.',
     3        '  IDPARS(5) = ',I1,'.',
     4       /'     VARIABLE ',I9.9,2I10.9,I4.3,' NOT COMPUTED.')
        IER=103
        GO TO 700
C
      END IF
C
C        VERIFY THE RELATIVE POSITION OF UPPER AND LOWER BOUNDARIES
C        OF THE GIVEN ISOBARIC LAYER
C
      IF(IDPARS(6).EQ.IDPARS(7).OR.IDPARS(6).LT.IDPARS(7)) THEN
        WRITE(KFILDO,104) (IDPARS(I),I=6,7),(JD(J),J=1,4)
 104  FORMAT(/' ****THE TOP AND BOTTOM LEVELS ARE IDENTICAL',
     1        ' OR REVERSED',
     2        ' AND CANNOT BE HANDLED BY SUBROUTINE MEANRH.',
     3       /'     IDPARS(6) = ',I4,5X,'IDPARS(7) = ',I4,'.',
     4        '  VARIABLE',I9.9,2I10.9,I4.3,' NOT COMPUTED.')
        IER=103
        GO TO 700
      END IF
C
C        VERIFY THE BOTTOM BOUNDARY
C
      IF(IDPARS(6).EQ.0000) THEN
        WRITE(KFILDO,105) IDPARS(6),(JD(J),J=1,4)
 105  FORMAT(/' ****THE VALUE OF IDPARS(6) IS 0000 IN MEANRH AND',
     1        ' MUST BE REPLACED BY',
     2        ' THE PRESSURE ON THE GROUND LEVEL OR',
     3        ' THE LOWEST ISOBARIC LEVEL,',
     4       /'     WHERE THE VALUES OF TEMPERATURE AND',
     5        ' SPECIFIC HUMIDITY ARE AVAILABLE.',
     6        '  IDPARS(6) = ',I3,'.',
     2       /'     VARIABLE ',I9.9,2I10.9,I4.3,' NOT COMPUTED.')
        IER=103
        GO TO 700
C
      ELSEIF(IDPARS(6).GT.1000.OR.IDPARS(6).LE.300) THEN
        WRITE(KFILDO,106) IDPARS(6),(JD(J),J=1,4)
 106  FORMAT(/' ****THE BOTTOM LEVEL IS OUT OF BOUNDS IN MEANRH.',
     1        '  IDPARS(6) = ',I4,'.',
     2       /'     VARIABLE ',I9.9,2I10.9,I4.3,' NOT COMPUTED.')
        IER=103
        GO TO 700
      ELSE
        GO TO 150
      END IF
C
C        VERIFY THE TOP BOUNDARY
C
 150  IF(IDPARS(7).GE.1000.OR.IDPARS(7).LT.300) THEN
        WRITE(KFILDO,151) IDPARS(7),(JD(J),J=1,4)
 151  FORMAT(/' ****THE TOP LEVEL IS OUT OF BOUNDS IN MEANRH.',
     1        '  IDPARS(7) = ',I4,'.',
     2       /'     VARIABLE ',I9.9,2I10.9,I4.3,' NOT COMPUTED.')
        IER=103
        GO TO 700
C
      END IF
C
C        *********************************
C        END OF PREDICTOR ID VERIFICATION 
C        *********************************
C
C        SET UP A FRAMEWORK OF AN ISOBARIC LAYER WHICH IS BOUNDED BY
C        IDPARS(6) AND IDPARS(7), AND
C        IS OBTAINED FROM A TEMPLATE DESIGNATED BY THE PREDICTOR ID
C
C        LOCATE THE BOTTOM OF THE LAYER:
C
      ILIMJ=ILIM(IDTMPL)
C
      DO I=1,ILIMJ
        IF(ITEMPLAT(I,IDTMPL).EQ.IDPARS(6)) THEN
          IB=I
          GO TO 305
        END IF
      END DO
C
      WRITE(KFILDO,301) IDPARS(6),(JD(J),J=1,4)
 301  FORMAT(/' ****THE BOTTOM LEVEL DOES NOT MATCH ANY LEVEL IN THE',
     1        ' TEMPLATE IN MEANRH.  IDPARS(6) = ',I4,'.',
     2       /'     VARIABLE ',I9.9,2I10.9,I4.3,' NOT COMPUTED.')
      IER=103
      GO TO 700
C
C        LOCATE THE TOP LEVEL
C
 305  DO I=1,ILIMJ
C
        IF(ITEMPLAT(I,IDTMPL).EQ.IDPARS(7)) THEN
          IT=I
          GO TO 400
        END IF
C
      END DO
C
      WRITE(KFILDO,308) IDPARS(7),(JD(J),J=1,4)
 308  FORMAT(/' ****THE TOP LEVEL DOES NOT MATCH ANY LEVEL IN THE',
     1        ' TEMPLATE IN MEANRH.  IDPARS(7) = ',I4,'.',
     2       /'     VARIABLE ',I9.9,2I10.9,I4.3,' NOT COMPUTED.')
      IER=103
      GO TO 700
C
C        ************************************************************
C        COMPUTE SPECIFIC HUMIDITY AND SATURATION SPECIFIC HUMIDITY, 
C        FETCH DATA FOR RELATIVE HUMIDITY, AND                       
C        INTEGRATE THE DATA FOR SPEC. HUM. AND SAT. SPEC. HUM. OVER  
C        THE ISOBARIC LAYER.                                         
C        ************************************************************
C
C        BASIC PREDICTOR IDENTIFIER FOR SPECIFIC HUMIDITY AND
C        RELATIVE HUMIDITY (PART 1)
C
 400    JDSH(1)=(ICCC*1000+IFFFQ)*1000+IDPARS(4)
        JDSH(3)=IDPARS(9)*1000000+IDPARS(12)
        JDSH(4)=0
C
        JDRH(1)=(ICCC*1000+IFFFR)*1000+IDPARS(4)
        JDRH(3)=JDSH(3)
        JDRH(4)=0
C
      DO 600 IL=IB,IT
C
C        BASIC PREDICTOR IDENTIFIER FOR SPECIFIC HUMIDITY AND
C        RELATIVE HUMIDITY (PART 2)
C
        JDSH(2)=ITEMPLAT(IL,IDTMPL)
        JDRH(2)=JDSH(2)
C
C          *******************************************
C          BEGIN HANDLING DATA ON INDIVIDUAL LEVEL IL 
C          *******************************************
C
C          COMPUTE DATA FOR SPECIFIC HUMIDITY
C
        CALL PRSID1(KFILDO,JDSH,IDPASH)
C
        CALL SPECHUM(KFILDO,KFIL10,IDPASH,JDSH,NDATE,
     1               NGRIDC,ND11,NSLAB,IPACK,IWORK,FD4,ND2X3,
     2               LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3               IS0,IS1,IS2,IS4,ND7,
     4               FD1,FD2,FD3,ND2X3,
     5               ISTAV,L3264B,MISTOT,IER)
C
        IF(IER.NE.0) GO TO 700
        LXSH=IS2(3)
        LYSH=IS2(4)
C
C          PRINT GRIDPOINT VALUES OF SPECIFIC HUMIDITY (SPEC. H.)
C          ON AN ISOBARIC SURFACE  -----  FOR VERIFICATION
C
D     WRITE(KFILDO,905) (JDSH(I),I=1,4),NDATE,(FD4(J),J=1,LXSH*LYSH)
D905  FORMAT(/' ****GRIDPOINT VALUES FOR VARIABLE ',4I12,' FOR DATE ',
D    1       I12,(/10F10.5))
C
C        FETCH DATA FOR RELATIVE HUMIDITY
C
        CALL GFETCH(KFILDO,KFIL10,JDRH,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLABL,MISSP,MISSS,L3264B,1,IER)
        IF(MISSP.NE.0)MISTOT=MISTOT+1
C
        IF(IER.NE.0) GO TO 700
        LXRH=IS2(3)
        LYRH=IS2(4)
C
C          PRINT GRIDPOINT VALUES OF RELATIVE HUMIDITY (R.H.)
C          ON AN ISOBARIC SURFACE  -----  FOR VERIFICATION
C
D     WRITE(KFILDO,910) (JDRH(I),I=1,4),NDATE,(FD3(J),J=1,LXRH*LYRH)
D910  FORMAT(/' ****GRIDPOINT VALUES FOR VARIABLE ',4I12,' FOR DATE ',
D    1       I12,(/10F10.5))
C
C        CHECK GRID CHARACTERISTICS OF DATA FIELDS (LEVEL RELATIVE
C        HUMIDITY AND SPECIFIC HUMIDITY)
C
        IF(NSLAB.NE.NSLABL.OR.LXSH.NE.LXRH.OR.LYSH.NE.LYRH) THEN
          IER=100
          WRITE(KFILDO,405) (JDSH(J),J=1,4),(JDRH(J),J=1,4),
     1      (NGRIDC(J,NSLAB),J=1,6),(NGRIDC(J,NSLABL),J=1,6),
     2      LXSH,LXRH,LYSH,LYRH,IER
 405  FORMAT(/' ****GRID CHARACTERISTICS FOR SPECIFIC HUMIDITY',
     1        ' AND RELATIVE HUMIDITY DO NOT MATCH IN MEANRH:',
     2       /'     JDSH(J) =          ',I10,3(',',2X,I10),
     3       /'     JDRH(J) =          ',I10,3(',',2X,I10),
     4       /'     NGRIDC(J,NSLAB) =  ',I10,5(',',2X,I10),
     5       /'     NGRIDC(J,NSLABL) = ',I10,5(',',2X,I10),
     6       /'     LXSH = ',I3,5X,'LYSH = ',I3,
     7       /'     LXRH = ',I3,5X,'LYRH = ',I3,
     8       /'     IER = ',I3,'.  VARIABLE NOT COMPUTED.')
         GO TO 700
        END IF
C
C          LIMIT THE VALUES OF RELATIVE HUMIDITY IN FD3( ).
C
        DO I=1,LXRH*LYRH
          IF(FD3(I).LT.2.) FD3(I)=2.
          IF(FD3(I).GT.100.) FD3(I)=100.
        END DO
C
C        COMPUTE SATURATED SPECIFIC HUMIDITY
C
        DO I=1,LXSH*LYSH
          FD3(I)=FD3(I)/100.
          FD5(I)=FD4(I)/FD3(I)
        END DO
C
C        ***************************************************
C        END OF DATA MANIPULATION ON INDIVIDUAL LEVEL IL;   
C        BEGIN HANDLING DATA ON MULTIPLE LEVELS IL AND IL-1 
C        ***************************************************
C
C        INTEGRATE SPECIFIC HUMIDITY AND SATURATED SPECIFIC HUMIDITY
C        BY USING STEPWISE TRAPEZOIDAL RULE
C
        IF(IL.EQ.IB) THEN
          DELTX=0.5*(ITEMPLAT(IL+1,IDTMPL)-ITEMPLAT(IL,IDTMPL))
         ELSEIF(IL.EQ.IT) THEN
          DELTX=0.5*(ITEMPLAT(IL,IDTMPL)-ITEMPLAT(IL-1,IDTMPL))
         ELSE
          DELTX=0.5*(ITEMPLAT(IL+1,IDTMPL)-ITEMPLAT(IL-1,IDTMPL))
        END IF
C
        DO I=1,LXSH*LYSH
           FD6(I)=FD6(I)+FD4(I)*DELTX
           FD7(I)=FD7(I)+FD5(I)*DELTX
        END DO
C
 600  END DO
C
C        *****************************************************
C        END OF INTEGRATION OF SPEC. HUM. AND SAT. SPEC. HUM. 
C        *****************************************************
C
C        COMPUTE MEAN RELATIVE HUMIDITY
C
      DO I=1,LXSH*LYSH
        FDMRH(I)=100.*FD6(I)/FD7(I)
      END DO
C
C        PRINT GRIDPOINT VALUES OF MEAN RELATIVE HUMIDITY (M.R.H.)
C        FOR THE SPECIFIED LAYER  -----  FOR VERIFICATION
C
D     WRITE(KFILDO,930) (JD(I),I=1,4),NDATE,(FDMRH(J),J=1,LXSH*LYSH)
D930  FORMAT(/' ****GRIDPOINT VALUES FOR VARIABLE ',4I12,' FOR DATE ',
D    1       I12,(/10F10.5))
C
      GO TO 800
C
 700  DO I=1,ND2X3
       FDMRH(I)=9999.
      END DO
C
 800  RETURN
      END
