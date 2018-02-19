      SUBROUTINE MODELKTOF(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                     NGRIDC,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     2                     LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                     IS0,IS1,IS2,IS4,ND7,
     4                     FD1,ND2X3,
     5                     ISTAV,L3264B,MISTOT,IER)
C
C
C        JUNE     2007   MALONEY   MDL   MOS-2000  (FROM MODELMXMN)
C        SEP      2007   COSGROVE  MODIFIED TO ALSO CONVERT TEMPS
C                                  ON CONSTANT HEIGHT SURFACES
C
C        PURPOSE
C            THIS SUBROUTINE CONVERTS MODEL TEMPERATURES FROM KELVIN
C            TO FAHRENHEIT.  IT IS EXPECTED THAT THIS CODE WILL BE
C            PRIMARILY USED TO MAKE A FIRST GUESS FIELD FOR ALASKA
C            GRIDDED MOS.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C                002 300 - TEMPERATURE AT A CONSTANT PRESSURE IN DEG F
C                002 301 - TEMPERATURE AT A CONSTANT HEIGHT IN DEG F
C
C        DATA SET USE
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT(PRINT) FILE.
C                       (OUTPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                       ACCESS.(INPUT-OUTPUT)
C
C        VARIABLES
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE(,) (J=1,ND10).
C                       WHEN CORE() IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C             DATA( ) = THE TEMPERATURE IN DEG F.  (OUTPUT)
C                   I = LOOP CONTROL VARIABLE.  (INTERNAL)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID() (J=1,15).
C                       (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY
C                                      1 LAYER)
C                       J=7--LTLTLTLT (TOP OF LAYER)
C                       J=8--T (TRANSFORMATION)
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND 
C                                BACK IN TIME)
C                       J=10-OT (TIME APPLICATION)
C                       J=11-OH (TIME PERIOD IN HOURS)
C                       J=12-TAU (PROJECTION IN HOURS)
C                       J=13-I (INTERPOLATION TYPE)
C                       J=14-S (SMOOTHING INDICATOR)
C                       J=15-G (GRID INDICATOR)
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                       101 = GRID SIZE IS TOO BIG FOR ???(), WHOSE 
C                             DIMENSION IS ND5.
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE
C                             MX OR MX TEMP.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
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
C               ISTAV = 0 SINCE THE DATA RETURNED ARE GRID DATA.
C                       (OUTPUT)
C         ITABLE(I,J) = VALUES OF CCCFFF ACCOMMODATED (I=1) AND
C                       THE ASSOCIATED VARIABLE NEEDED (I=2)
C                       (J=1,NDIM).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5). (INTERNAL)
C                   J = LOOP CONTROL VARIABLE
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT
C                       THE PORTIONS PERTAINING TO PROCESSING
C                       ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       ID() IS USED TO HELP IDENTIFY THE BASIC MODEL
C                       FIELDS AS READ FROM THE ARCHIVE.  (INPUT)
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE(,)
C                       THAT HAVE BEEN USED IN THIS RUN.  (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE
C                       DATA STORED (L=1,12) (J=1,LITEMS).
C                       (INPUT-OUTPUT)
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE() WHERE
C                              THE DATA START.  WHEN ON DISK,
C                              THIS IS MINUS THE RECORD NUMBER WHERE
C                              THE DATA START.
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDL GRIB, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --NUMBER OF THE SLAB IN DIR(, ,L) AND
C                              IN NGRIDC(,L) DEFINING THE
C                              CHARACTERISTICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID(,N) (N=1,NPRED) FOR WHICH
C                              THIS VARIABLE IS NEEDED, WHEN IT IS
C                              NEEDED ONLY ONCE FROM LSTORE(,).
C                              WHEN IT IS NEEDED MORE THAN ONCE, THE 
C                              VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING
C                              MSTORE(,). LATER USED AS A WAY OF
C                              DETERMINING WHETHER TO KEEP THIS
C                              VARIABLE.
C            MDPARS() = PARSED ID USED IN SUBROUTINE PRSID1 FOR
C                       SUBROUTINE WETBULBT.  (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 WHEN DATA ARE NOT PACKED.  (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 WHEN DATA ARE NOT PACKED.  (INTERNAL)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  
C                       ALL WORK ARRAYS ARE DIMENSIONED ND2X3. (INPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND DATA( ). 
C                       (INPUT)
C                 ND7 = DIMENSION OF IS0(),IS1(),IS2(), AND IS4().
C                       NOT ALL LOCATIONS ARE USED. (INPUT)
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C                ND10 = DIMENSION OF CORE().  (INPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN
C                       BE DEALT WITH ON THIS RUN.  LAST DIMENSION 
C                       OF NGRIDC(,).  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING COUNT FROM THE BEGINNING OF
C                       THE PROGRAM.  THIS COUNT IS MAINTAINED IN
C                       CASE THE USER NEEDS IT(DIAGNOSTICS, ETC.).
C                       (OUTPUT)
C         NGRIDC(L,M) = HOLDS THE GRID CHARACTERISTICS (L=1,6) FOR
C                       EACH GRID COMBINATION.  (M=1,NGRID).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=
C                            POLAR STEREOGRAPHIC).
C                       L=2--GRID LENGTH IN METERS.
C                       L=3--LATITUDE AT WHICH THE GRID LENGTH IS
C                            CORRECT *1000.
C                       L=4--GRID ORIENTATION IN DEGREES * 1000.
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *1000.
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES
C                            *1000.
C               NPACK = 2 FOR TDL GRIB PACKED DATA; 1 FOR NOT PACKED
C                       THIS IS RETURNED FROM GFETCH.  (INTERNAL)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND
C                       IN NGRIDC( , ) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  (OUTPUT)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE,
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS 
C                       STORED IN LSTORE(9,).  (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA().  THIS 
C                       IS RETURNED FROM GFETCH.  (INTERNAL)
C                ICCC = CCC OF NEEDED FIELD.  (INTERNAL)
C                IFFF = FFF OF NEEDED FIELD.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C     NON-SYSTEM SUBROUTINES USED
C         GFETCH,PRSID1
C
      IMPLICIT NONE
C
      INTEGER NDIM
      PARAMETER(NDIM=2)
C     
      INTEGER IDPARS(15),JD(4)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER MD(4)
      INTEGER ITABLE(2,NDIM)

      INTEGER ICCC,IFFF,I,IER,ISTAV,
     1        J,KFILDO,KFIL10,L3264B,LITEMS,
     2        MISSP,MISSS,MISTOT,
     3        NBLOCK,ND2X3,ND5,ND7,ND9,ND10,ND11,
     4        NDATE,NFETCH,NPACK,NSLAB,
     5        NTIMES,NWORDS,JJ
C
      REAL DATA(ND5)
      REAL FD1(ND2X3)
      REAL CORE(ND10)
C
      DATA ITABLE/002300,002000,
     1            002301,002001/
C
      IER  =0
      ISTAV=0
C
C        FIND CCCFFF OF ID(1,1) IN ITABLE(1, ).
C
      DO 100 JJ=1,NDIM
      IF(ITABLE(1,JJ).EQ.(IDPARS(1)*1000+IDPARS(2)))
     1 GO TO 108
 100  CONTINUE
C
         IER=103
         WRITE(KFILDO,105)(JD(J),J=1,4),IER
 105     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE',
     1           ' TEMP. IN DEG F.',
     2          /'     PREDICTOR ',I9.9,I10.9,I10.9,I4.3,
     3           ' NOT ACCOMMODATED IN MODELKTOF.  IER = ',I3)
         GO TO 800
C
C        FETCH THE TEMPERATURE IN KELVIN
C
 108  MD(1)= ITABLE(2,JJ)*1000 + IDPARS(3)*100 +IDPARS(4)
      MD(2)= IDPARS(7)
      MD(3)= IDPARS(9) * 1000000 + IDPARS(12)
      MD(4)= 0
C
      CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,
     4            1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GOTO 800
C  
C        NOW CONVERT KELVIN TO FAHRENHEIT
C
      DO I=1,IS2(3)*IS2(4)
C
         IF(NINT(FD1(I)).NE.9999) THEN
	    DATA(I)=(FD1(I)-273.15)*1.8+32.0
         ELSE
            DATA(I)=9999.
         ENDIF
C
      ENDDO
C
      GO TO 900
C
C        IF THERE WAS A PROBLEM IN GFETCH IT WOULD COME HERE TO
C        SET DATA TO MISSING
C
 800  DO I=1,ND2X3
         DATA(I)=9999.
      ENDDO
C
 900  RETURN
      END
