      SUBROUTINE WINDDR(KFILDO,KFIl10,IDPARS,JD,NDATE,
     1                     NGRIDC,ND11,NSLAB,IPACK,IWORK,FDIR,ND5,
     2                     LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                     IS0,IS1,IS2,IS4,ND7,
     4                     FD1,FD2,FD3,FD4,ND2X3,
     5                     ISTAV,L3264B,MISTOT,IER)
C
C       JULY      1998   SFANOS  TDL   MOS-2000          
C       OCTOBER   1998   SFANOS  MODIFIED CODE FOR
C                                ISO=2 AND ISO=3
C       NOVEMBER  1998   SFANOS  REDUCED WORK ARRAYS
C                                FROM 7 TO 4
C       MARCH     1999   SFANOS  CHECK FOR WIND 
C                                DIRECTION OF ZERO
C       MAY       2000   DALLAVALLE  MODIFIED FORMAT STATEMENTS
C                                TO CONFORM TO FORTRAN 90
C                                STANDARDS ON THE IBM-SP
C       JUNE      2000   HUGHES  FIXED A CALL TO GFETCH FOR THE V-
C                                COMPONENT OF THE WIND DIRECTION,
C                                CHANGED AN '.EQ.0' TO '.LT..001',
C                                AND REMOVED TABS
C       SEPTEMBER 2002   SFANOS  FIXED A CALL TO GFETCH FOR V-
C                                COMPONENT OF WIND DIRECTION
C       DECEMBER  2002   WEISS   CHANGED ND5 TO ND2X3
C       MAY       2003   GLAHN   CHANGED ND2X3 TO ND5 IN IPACK( ),
C                                IWORK( ), DATA( ); REARRANGED TYPE
C                                STATEMENTS; CHANGED ND5 TO ND2X3 IN
C                                SEVERAL CALLS; WHITE SPACE; TOOK
C                                REFERENCE TO SUBROUTING WSPEED OUT
C                                OF PURPOSE; DIAGNOSTICS; CHANGE
C                                TO IDPARS(7) ELIMINATED; REARRANGED
C                                CALL; ELIMINATED LARGE SECTIONS OF
C                                CODE CALLING GFETCH FOR U AND V WIND
C
C       PURPOSE 
C           TO COMPUTE WIND DIRECTION IN DEGREES.  THE SINGLE LEVEL
C           IS CONTAINED IN JD(2).
C
C           THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               004 200 - WIND DIRECTION VARIABLE ON A 
C                         CONSTANT PRESSURE SURFACE
C               004 201 - WIND DIRECTION VARIABLE ON A 
C                         CONSTANT HEIGHT SURFACE
C               004 206 - WIND DIRECTION VARIABLE ON A 
C                         SIGMA SURFACE (NOT OPERATIVE)
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.  
C                     (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C 
C        VARIABLES 
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA 
C                       IDENTIFIED IN LSTORE( , ) (J=1,ND10).  
C                       WHEN CORE( ) IS FULL DATA ARE STORED ON DISK.  
C                       (INPUT)
C             FDIR(K) = WIND DIRECTION IN DEGREES CALCULATED FROM
C                       THE U AND V COMPONENTS OF THE WIND.
C                       UPON RETURN, THE ARRAY WILL BE NX X NY. (OUTPUT)
C              FD1(K) = WORK ARRAY U COMPONENT OF WIND (K=1,ND2X3). 
C                       (INTERNAL)
C              FD2(K) = WORK ARRAY V COMPONENT OF WIND (K=1,ND2X3).
C                       (INTERNAL)
C              FD3(K) = WORK ARRAY USED FOR EARTH ORIENTED U COMPONENT IN 
C                       SUBROUTINE EOWND (K=1,ND2X3) (INTERNAL)
C              FD4(K) = WORK ARRAY USED FOR EARTH ORIENTED V COMPONENT IN
C                       SUBROUTINE EOWND (K=1,ND2X3) (INTERNAL)
C                   I = LOOP COUNTER
C                IXJY = LOOP COUNTER
C       ICCCFFF ( , ) = CONTAINS IDPARS(1) AND IDPARS(2) IDS FOR THE 
C                       U- AND V-WINDS ON THREE SURFACES--PRESSURE,
C                       HEIGHT, AND SIGMA.
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID( ) (J=1,15).
C                       (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 
C                            LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK IN 
C                               TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       100 = THE TWO GRIDS NEEDED ARE NOT THE SAME SIZE
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE 
C                             WIND DIRECTION.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3). 
C                       (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  
C                       (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       IS2(3) AND IS2(4) ARE USED BY THE CALLING
C                       PROGRAM AS THE GRID DIMENSIONS.  
C                       (INTERNAL-OUTPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C                 ISO = 1 FOR ISOBARIC, 2 FOR ISOHYETAL SURFACE, 3
C                       FOR SIGMA SURFACE (INTERNAL)
C               ISTAV = 0 SINCE THE DATA RETURNED ARE GRID DATA.  
C                       (OUTPUT)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C                   J = LOOP COUNTER
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE 
C                       PORTIONS PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       ID( ) IS USED TO HELP IDENTIFY THE BASIC MODEL
C                       FIELDS AS READ FROM THE ARCHIVE.  (INPUT)
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. 
C                       (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , ) 
C                       THAT HAVE BEEN USED IN THIS RUN.  (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS).  (INPUT-OUTPUT)
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
C                              IN NGRIDC( ,L) DEFINING THE 
C                              CHARACTERISTICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,NPRED) FOR WHICH 
C                              THIS VARIABLE IS NEEDED, WHEN IT IS 
C                              NEEDED ONLY ONCE FROM LSTORE( , ).  
C                              WHEN IT IS NEEDED MORE THAN ONCE, THE 
C                              VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING 
C                              MSTORE( , ).  LATER USED AS A WAY OF 
C                              DETERMINING WHETHER TO KEEP THIS VARIABLE
C            MDIRU(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD3,FD4 (J=1,4). (INTERNAL)
C            MDIRV(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD3,FD4 (J=1,4). (INTERNAL)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF THE
C                       GRID IS NOT KNOWN BEFORE FDTK AND FDDP ARE 
C                       FETCHED.  (INPUT)
C                 ND5 = DIMENSION OF IPACK( ),IWORK( ), AND FDIR( ).
C                       (INPUT)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN BE
C                       DEALT WITH ON THIS RUN.  LAST DIMENSION OF
C                       NGRIDC( , ).  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.  
C                       (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING  COUNT FROM THE BEGINNING OF THE
C                       PROGRAM.  THIS COUNT IS MAINTAINED IN CASE THE  
C                       USER NEEDS IT (DIAGNOSTICS, ETC.).  (OUTPUT)
C         NGRIDC(L,M) = HOLDS THE GRID CHARACTERISTICS (L=1,6) FOR EACH
C                       GRID COMBINATION (M=1,NGRID).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=POLAR
C                            STEREOGRAPHIC). 
C                       L=2--GRID LENGTH IN METERS,
C                       L=3--LATITUDE AT WHICH GRID LENGTH IS CORRECT 
C                            *1000,
C                       L=4--GRID ORIENTATION IN DEGREES *1000,
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *1000,
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES *1000.
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND
C                       IN NGRIDC( , ) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  (OUTPUT) 
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            EOWND, PRSID1
C
      IMPLICIT NONE
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER ICCCFFF(2,3),MDIRU(4),MDIRV(4),
     1        MUPARS(15),MVPARS(15)
      INTEGER I,J,IER,ISO,ISTAV,IXJY,KFILDO,KFIL10,LITEMS,L3264B,MISTOT,
     1        NBLOCK,ND2X3,ND5,ND7,ND9,ND10,ND11,NDATE,NFETCH,NSLAB
C
      REAL FDIR(ND5)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3)
      REAL CORE(ND10)
C
      DATA ((ICCCFFF(I,J),J=1,3),I=1,2) /004010,004011,004066,
     1                                   004110,004111,004166/
C        IDS IN TABLE ABOVE ARE:
C          EARTH U (PRESSURE), EARTH U ( HEIGHT), EARTH U (SIGMA)
C          EARTH V (PRESSURE), EARTH V ( HEIGHT), EARTH V (SIGMA)
C
      IER=0
      ISTAV=0
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C
      IF(IDPARS(1).NE.004.OR.(IDPARS(2).NE.200.AND.
     1   IDPARS(2).NE.201.AND.IDPARS(2).NE.206)) THEN
         WRITE(KFILDO,101)(JD(J),J=1,4)
 101     FORMAT(/,' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE',
     1            ' WIND DIRECTION PREDICTOR.  ',I9.9,
     2            2I10.9,I4.3,' NOT COMPUTED IN WINDDR.')
         IER=103
         GOTO 800
      END IF
C
C        SET ISO TO ACCESS ICCCFFF( , ).
C
      IF (IDPARS(2).EQ.200) THEN
        ISO=1
C          THIS IS WIND DIR ON A CONSTANT PRESSURE SURFACE.
      ELSEIF(IDPARS(2).EQ.201)THEN
        ISO=2
C          THIS IS WIND DIR ON A CONSTANT HEIGHT SURFACE.
      ELSEIF(IDPARS(2).EQ.206)THEN
        ISO=3
C          THIS IS WIND DIR ON A CONSTANT SIGMA SURFACE.
        GO TO 800
C        SIGMA SURFACE COMPUTATIONS NOT IMPLEMENTED.
      ENDIF
C
C        CREATE ID FOR EARTH ORIENTED U-WIND COMPONENT.
C
      MDIRU(1)=ICCCFFF(1,ISO)*1000+IDPARS(4)
      MDIRU(2)=IDPARS(7)
      MDIRU(3)=IDPARS(9)*1000000+IDPARS(12)
      MDIRU(4)=0
C
C        CALL EOWND TO COMPUTE EARTH ORIENTED U WIND.
C
      CALL PRSID1(KFILDO,MDIRU,MUPARS)
      CALL EOWND(KFILDO,KFIL10,MUPARS,MDIRU,NDATE,
     1           NGRIDC,ND11,NSLAB,IPACK,IWORK,FD1,ND2X3,
     2           LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3           IS0,IS1,IS2,IS4,ND7,
     4           FD3,FD4,ND2X3,
     5           ISTAV,L3264B,MISTOT,IER)
      IF(IER.NE.0)GOTO 800
C
C        CREATE ID FOR EARTH ORIENTED V-WIND COMPONENT.
C
      MDIRV(1)=ICCCFFF(2,ISO)*1000+IDPARS(4)
      MDIRV(2)=IDPARS(7)
      MDIRV(3)=IDPARS(9)*1000000+IDPARS(12)
      MDIRV(4)=0
C
C        CALL EOWND TO COMPUTE EARTH ORIENTED V WIND.
C
      CALL PRSID1(KFILDO,MDIRV,MVPARS)
      CALL EOWND(KFILDO,KFIL10,MVPARS,MDIRV,NDATE,
     1           NGRIDC,ND11,NSLAB,IPACK,IWORK,FD2,ND2X3,
     2           LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3           IS0,IS1,IS2,IS4,ND7,
     4           FD3,FD4,ND2X3,
     5           ISTAV,L3264B,MISTOT,IER)
      IF(IER.NE.0)GOTO 800
C
C        EARTH-ORIENTED U- AND V-WINDS IN THE UNITS OF THE
C        GRID-ORIENTED U- AND V-WINDS ACCESSED ARE NOW IN
C        FD1( ) AND FD2( ), RESPECTIVELY.  NOTE THAT NO 
C        CHECKING OF GRID CHARACTERISTICS IS NECESSARY BECAUSE
C        COMPUTATIONS CAME FROM THE SAME GRIDS IN EOWND.
C        NOW, COMPUTE WIND DIRECTION IN DEGREES.
C
      DO 300 IXJY=1,IS2(3)*IS2(4)
C
        IF(FD1(IXJY).EQ.9999..OR.FD2(IXJY).EQ.9999.)THEN
          FDIR(IXJY)=9999.
          GOTO 300
        END IF
C
C        CHECK FOR WIND SPEED OF NEAR ZERO.
C
      IF(SQRT((FD1(IXJY)*FD1(IXJY))+(FD2(IXJY)*FD2(IXJY)))
     1     .LT..05)THEN
          FDIR(IXJY)=0.
          GOTO 300
      END IF
C
      IF(ABS(FD2(IXJY)).LT..0001)THEN
          FDIR(IXJY)=SIGN(90.,FD1(IXJY))+180.
        ELSE
          FDIR(IXJY)=57.29578*ATAN2(FD1(IXJY),FD2(IXJY))+180.
      END IF
C
 300  CONTINUE
C
      GOTO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C
 800  DO 801 J=1,ND2X3
        FDIR(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END      
