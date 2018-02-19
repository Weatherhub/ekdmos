      SUBROUTINE MDIV(KFILDO,KFIL10,JD,IDPARS,NDATE,
     1                NGRIDC,ND11,NSLAB,IPACK,IWORK,FDMD,ND5,
     2                LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                IS0,IS1,IS2,IS4,ND7,
     4                FD1,FD2,FD3,FD4,FD5,FD6,FDSINS,FDMS,ND2X3,
     5                ISTAV,L3264B,MISTOT,IER)
C
C        AUGUST   1997   FIEBRICH  MOS-2000             
C        NOVEMBER 1998   YAN       MOS-2000.  PROGRAM RESTRUCTURED.
C        DECEMBER 2000   RUDACK    MODIFIED CODE TO COMPLY WITH MDL 
C                                  FORMAT SPECIFICATIONS
C        NOVEMBER 2002   WEISS     CHANGED ND5 TO ND2X3
C        APRIL    2003   GLAHN     MODIFIED LINES IN CALL;  SET
C                                  DIMENSIONS OF IPACK( ), IWORK( )
C                                  AND FDMD( ) = ND5; SPELL CHECK;
C                                  REARRANGED CALLS TO DIVW, SPECHUM,
C                                  AND ADVCTW AND REPLACED ND5 WITH
C                                  ND2X3 IN CALLS TO THEM
C        MAY      2003   GLAHN     REARRANGED TYPE STATEMENTS
C        AUGUST   2003   GLAHN     MODIFIED FORMATS 200, 300 AND 600;
C                                  ADDED FORMAT 201; MODIFIED TO USE
C                                  WINDS AT 10 M WHEN A CONSTANT
C                                  HEIGHT SURFACE OF 2 IS SPECIFIED;
C                                  ADDED ND( ) AND NDPARS( )
C        OCTOBER  2003   SMB       MODIFIED FORMAT STATEMENT 100 FOR 
C                                  THE IBM
C                      
C        PURPOSE 
C            TO COMPUTE MOISTURE DIVERGENCE ON AN ISOBARIC, OR A CONST-
C            ANT HEIGHT, OR A SIGMA SURFACE.  SPECIFIC HUMIDITY AND U
C            AND V WIND COMPONENTS ARE REQUIRED TO COMPUTE THE TWO TERMS
C            IN THE MOISTURE DIVERGENCE: (1) DIVERGENCE TERM, AND (2)
C            ADVECTION TERM.
C            
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               003 500 - MOISTURE DIVERGENCE ON PRESSURE SFC
C               003 501 - MOISTURE DIVERGENCE ON HEIGHT SFC
C                         WORKS ONLY FOR 2 M, BECAUSE SPECHUM IS CALLED.
C                         10-M WINDS USED FOR 2-M SPECIFIC HUMIDITY 
C                         [IDPARS(7)=2)].
C               003 506 - MOISTURE DIVERGENCE ON SIGMA SFC
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
C                       OF LX*LY (OUTPUT).
C              FD1(J) = WORK ARRAY TO HOLD EITHER WIND DIVERGENCE OR
C                       THE DIVERGENCE TERM OF MOISTURE DIVERGENCE (J=1,
C                       ND2X3) (INTERNAL)
C              FD2(J) = WORK ARRAY TO HOLD EITHER SPECIFIC HUMIDITY OR
C                       THE ADVECTION TERM OF MOISTURE DIVERGENCE (J=1,
C                       ND2X3) (INTERNAL)
C              FD3(J) = DUMMY WORK ARRAY (J=1,ND2X3) (INTERNAL)
C              FD4(J) = DUMMY WORK ARRAY (J=1,ND2X3) (INTERNAL)
C              FD5(J) = DUMMY WORK ARRAY (J=1,ND2X3) (INTERNAL)
C              FD6(J) = DUMMY WORK ARRAY (J=1,ND2X3) (INTERNAL)
C             FDMD(J) = DATA ARRAY TO HOLD MOISTURE DIVERGENCE (S^-1)
C                       (ND2X3) (OUTPUT)
C           FDMS(I,J) = ARRAY TO SAVE THE MAP FACTOR IN SUBROUTINE DIVW
C                       AND ADVCTW (I=1,LX) (J=1,LY).  IT IS ONLY USED
C                       AS A PLACEHOLDER FOR SUBROUTINE CALLS. (INTERNAL)
C         FDSINS(I,J) = ARRAY TO SAVE THE SINE OF THE LATITUDE IN
C                       SUBROUTINES DIVW AND ADVCTW (I=1,LX) (J=1,LY). 
C                       IT IS ONLY USED AS A PLACEHOLDER FOR SUBROUTINE
C                       CALLS. (INTERNAL)
C                 I,J = LOOP COUNT (INTERNAL)
C       ICCCFFF ( , ) = CONTAINS IDPARS(1) AND IDPARS(2) ID FOR THE 
C                       METEOROLOGICAL FIELDS BEING FETCHED OR COMPUTED.
C                       COLUMN 1 CONTAINS ID FOR ISOBARIC SURFACE,
C                       COLUMN 2 CONTAINS ID FOR CONSTANT HEIGHT SURFACE
C                       AND COLUMN 3 CONTAINS ID FOR SIGMA SURFACE.  ROW
C                       1 IS FOR WIND DIVERGENCE IN S^-1, ROW 2 IS FOR
C                       SPECIFIC HUMIDITY IN G/G, AND ROW 3 IS FOR U
C                       WIND COMPONENT IN M/S. (INTERNAL)
C           IDPARS(J) = PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID(J) (J=1,15) DEFINED IN
C                       THE CALLING PROGRAM (INPUT)
C                       J=1 -- CCC (CLASS OF VARIABLE)
C                       J=2 -- FFF (SUBCLASS OF VARIABLE)
C                       J=3 -- B (BINARY INDICATOR)
C                       J=4 -- DD (DATA SOURCE, MODEL NUMBER)
C                       J=5 -- V (VERTICAL APPLICATION)
C                       J=6 -- LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 
C                              LAYER)
C                       J=7 -- LTLTLTLT (TOP OF LAYER)
C                       J=8 -- T (TRANSFORMATION)
C                       J=9 -- RR (RUN TIME OFFSET -- PREVIOUS CYCLE.
C                              IT IS ALWAYS A POSITIVE NUMBER AND
C                              COUNTED BACKWARDS IN TIME)
C                       J=10 -- OT (TIME APPLICATION)
C                       J=11 -- OH (TIME PERIOD IN HOURS)
C                       J=12 -- TAU (PROJECTION IN HOURS)
C                       J=13 -- I (INTERPOLATION TYPE)
C                       J=14 -- S (SMOOTHING INDICATOR)
C                       J=15 -- G (GRID INDICATOR)
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                       100 = GRID CHARACTERISTICS ARE DIFFERENT FOR THE
C                             2 FIELDS.
C                       103 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED
C                             IN THIS SUBROUTINE
C                       SEE GFETCH FOR OTHER VALUES WHEN IER.NE.0 AND
C                       DATA ARE RETURNED AS MISSING (INTERNAL-OUTPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL)
C              IS0(J) = MOS-2000 GRIB SECTION 0 IDS (J=1,3) (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 IDS (J=1,22+) (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 IDS (J=1,12)
C                       IS2(3) AND IS2(4) ARE USED BY THE CALLING
C                       PROGRAM AS GRID DIMENSION (INTERNAL-OUTPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 IDS (J=1,4) (INTERNAL)
C                 ISO = VARIABLE TO INDICATE WHICH SURFACE TO COMPUTE
C                       MOISTURE DIVERGENCE ON.  (1 FOR ISOBARIC, 2 FOR
C                       CONSTANT HEIGHT, 3 FOR SIGMA SURFACE) (INTERNAL)
C               ISTAV = 0 -- WHEN THE DATA RETURNED ARE GRID DATA OR
C                            DATA ARE NOT AVAILABLE FOR RETURN
C                       1 -- WHEN THE DATA RETURNED ARE STATION DATA
C                       (OUTPUT)
C            IWORK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL)
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).  THIS IS
C                       THE SAME AS ID(J) EXCEPT THAT THE PORTIONS
C                       PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3)
C                       T = IDPARS(8)
C                       I = IDPARS(13)
C                       S = IDPARS(14)
C                       G = IDPARS(15)
C                       JD( ) IS USED TO HELP IDENTIFY THE BASIC MODEL
C                       FIELDS AS READ FROM THE ARCHIVE (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                       (INPUT)
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS DEPENDING ON THE
C                       MACHINE BEING USED (EITHER 32 OR 64) (INPUT)
C               LD(J) = WORK ARRAY HOLDING THE 4 ID WORDS OF WIND
C                       DIVERGENCE BEING COMPUTED (J=1,4). (INTERNAL)
C           LDPARS(J) = PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO WIND DIVERGENCE (J=1,15).
C                       (INTERNAL)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN (INPUT)
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
C                       L=9  -- NUMBER OF TIMES DATA HAVE BEEN RETRIEVED
C                       L=10 -- NUMBER OF THE SLAB IN DIR( , ,L) AND IN
C                               NGRIDC( ,L) DEFINING THE CHARACTERISTICS
C                               OF THE GRID
C                       L=11 -- NUMBER OF PREDICTORS IN THE SORTED LIST
C                               IN IS( ,N) (N=1,NPRED) FOR WHICH THIS
C                               VARIABLE IS NEEDED ONLY ONCE FROM
C                               LSTORE( , ).  WHEN IT IS NEEDED MORE
C                               THAN ONCE, THE VALUE IS SET TO 7777.
C                       L=12 -- USED INITIALLY IN ESTABLISHING
C                               MSTORE( , ).  LATER USED TO DETERMINE
C                               WHETHER TO KEEP THIS VARIABLE
C               LX,LY = DIMENSIONS OF THE GRID RETURNED FROM CALLING
C                       DIVW FOR WIND DIVERGENCE (INTERNAL)
C               MD(J) = WORK ARRAY HOLDING THE 4 ID WORDS OF SPECIFIC
C                       HUMIDITY BEING COMPUTED (J=1,4) (INTERNAL)
C           MDPARS(J) = PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO SPECIFIC HUMIDITY.(J=1,15).
C                       (INTERNAL)
C              MISTOT = TOTAL NUMBER OF MISSING ITEMS ENCOUNTED IN
C                       UNPACKING GRIDS (INPUT-OUTPUT)
C               MX,MY = DIMENSIONS OF THE GRID RETURNED FROM CALLING
C                       SPECHUM FOR SPECIFIC HUMIDITY (INTERNAL)
C              NBLOCK = BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM DISK
C                       FILE (INPUT)
C               ND2X3 = DIMENSION OF FD1( ), FD2( ), FD3( ), FD4( ),
C                       FD5( ), FD6( ), FDMS( ), AND FDSINS( ) (INPUT)
C                 ND5 = DIMENSION OF IPACK( ),IWORK( ), AND FDMD( ).
C                       (INPUT)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED. (INPUT)
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ) (INPUT)
C                ND10 = DIMENSION OF CORE( ) (INPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN BE
C                       DEALT WITH IN THIS RUN.  LAST DIMENSION OF
C                       NGRIDC( , ) (INPUT)
C               NDATE = DATE/TIME FOR WHICH THE PREDICTOR IS NEEDED
C                       (INPUT)
C               ND(J) = WORK ARRAY HOLDING THE 4 ID WORDS OF FOR 
C                       ENTERING WIND ADVECTION (J=1,4) (INTERNAL)
C           NDPARS(J) = PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO WIND ADVECTION (J=1,15).
C                       (INTERNAL)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.  IT IS
C                       A RUNNING COUNT FROM THE BEGINNING OF THE MAIN
C                       PROGRAM.  THIS COUNT IS MAINTAINED IN CASE THE
C                       USER NEEDS IT FOR DIAGNOSTICS, ETC. (OUTPUT)
C         NGRIDC(L,M) = HOLDING THE GRID CHARACTERISTICS (L=1,6) FOR
C                       EACH GRID COMBINATION (M=1,NGRID) (INPUT-OUTPUT)
C                       L=1 -- MAP PROJECTION NUMBER (3=LAMBERT, 5=POLAR
C                              STEREOGRAPHIC)
C                       L=2 -- GRID LENGTH IN METERS
C                       L=3 -- LATITUDE AT WHICH GRID LENGTH IS CORRECT
C                              *1000
C                       L=4 -- GRID ORIENTATION IN DEGREES *1000
C                       L=5 -- LATITUDE OF LL CORNER IN DEGREES *1000
C                       L=6 -- LONGITUDE OF LL CORNER IN DEGREES *1000
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND IN
C                       NGRIDC( , ) DEFINING THE CHARACTERISTICS OF THE
C                       GRID.  IT IS USED TO IDENTIFY THE DATA SOURCE,
C                       I.E., THE MODEL. (OUTPUT)
C              NSLABL = SAME AS NSLAB.  RETURNED FROM CALLING DIVW FOR
C                       WIND DIVERGENCE (INTERNAL)
C              NSLABM = SAME AS NSLAB.  RETURNED FROM CALLING SPECHUM
C                       FOR SPECIFIC HUMIDITY (INTERNAL)
C              NSLABN = SAME AS NSLAB.  RETURNED FROM CALLING ADVCTW FOR
C                       THE ADVECTION TERM (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED 
C            ADVCTW, DIVW, PRSID1, SPECHUM
C
      IMPLICIT NONE
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER ICCCFFF(3,3)
      INTEGER LD(4),MD(4),ND(4),LDPARS(15),MDPARS(15),NDPARS(15)
      INTEGER I,IER,ISO,ISTAV,J,KFILDO,KFIL10,L3264B,LITEMS,LX,LY,
     1        MISTOT,MX,MY,NBLOCK,ND2X3,ND5,ND7,ND9,ND10,ND11,NDATE,
     2        NFETCH,NSLAB,NSLABL,NSLABM,NSLABN,NX,NY
C
      REAL FDMD(ND5)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),FD5(ND2X3),
     1     FD6(ND2X3),FDMS(ND2X3),FDSINS(ND2X3)
      REAL CORE(ND10)
C
      DATA ((ICCCFFF(I,J),J=1,3),I=1,3) /006110,006111,006116,
     1                                   003030,003031,003036,
     2                                   004000,004020,004016/
C
      IER=0
      ISTAV=0
C
C        CHECK IF THIS CODE ACCOMMODATES MOISTURE DIVERGENCE
C
      IF(IDPARS(1).NE.003.OR.(IDPARS(2).NE.500.AND.
     1   IDPARS(2).NE.501.AND.IDPARS(2).NE.506))THEN
	 IER=103
         WRITE(KFILDO,100)(JD(J),J=1,4),IER
 100     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE MOISTURE'
     1          ,' DIVERGENCE.',
     2          /'     PREDICTOR ',3(I10.9),I4.3,' NOT',
     3           ' COMPUTED IN MDIV.  IER=',I3)
         IER=103
         GO TO 900
      END IF
C
C        DETERMINE IF MOISTURE DIVERGENCE IS TO BE COMPUTED ON AN
C        ISOBARIC, OR A CONSTANT HEIGHT, OR A SIGMA SURFACE 
C
      IF(IDPARS(2).EQ.500)THEN
C
C           THIS IS DATA FOR A CONSTANT PRESSURE SURFACE
C
         ISO=1
      ELSE IF(IDPARS(2).EQ.501)THEN
C
C           THIS IS DATA FOR A CONSTANT HEIGHT SURFACE
C
         ISO=2
      ELSE 
C
C           THIS IS DATA FOR A SIGMA SURFACE
C
         ISO=3
      ENDIF
C 
C        TO OBTAIN WIND DIVERGENCE IN COMPUTING MOISTURE DIVERGENCE
C
C        CREATE IDS FOR OBTAINING WIND DIVERGENCE
C
      LD(1)=ICCCFFF(1,ISO)*1000+IDPARS(4)
C
      IF(ISO.EQ.2.AND.IDPARS(7).EQ.2)THEN
C           FOR A CONSTANT HEIGHT SURFACE OF 2 M, THE MATCHING
C           WIND MUST BE AT 10 M.
         LD(2)=10
      ELSE
         LD(2)=IDPARS(7)
      ENDIF
C
      LD(3)=IDPARS(9)*1000000+IDPARS(12)
      LD(4)=0
C
C        CALL PRSID1 TO PARSE IDS LD( ) TO LDPARS( )    
C
      CALL PRSID1(KFILDO,LD,LDPARS)
C
C        CALL TO OBTAIN WIND DIVERGENCE FD1
C
      CALL DIVW(KFILDO,KFIL10,LDPARS,LD,NDATE,
     1          NGRIDC,ND11,NSLABL,IPACK,IWORK,FD1,ND2X3,
     2          LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3          IS0,IS1,IS2,IS4,ND7,
     4          FD3,FD4,FD5,FD6,FDSINS,FDMS,ND2X3,
     5          ISTAV,L3264B,MISTOT,IER)
C
      IF(IER.NE.0)THEN
C
         IF(IER.EQ.47)THEN
            WRITE(KFILDO,200)IER
 200        FORMAT(/' ****WIND COMPONENT NOT AVAILABLE IN ROUTINE',
     1              ' DIVW.  MOISTURE DIVERGENCE FAILED IN MDIV',
     2              ' AT 200.  IER=',I3)
            GOTO 900
         ELSE
            WRITE(KFILDO,201)IER
 201        FORMAT('     WIND DIVERGENCE ROUTINE DIVW FAILED IN',
     1             ' COMPUTING DIVERGENCE IN MDIV AT 201.  IER=',I3)
C              THIS DIAGNOSTIC FOLLOWS ONE IN DIVW.
         ENDIF
C
      ENDIF
C
      LX=IS2(3)
      LY=IS2(4)
      NSLAB=NSLABL
C
C        TO OBTAIN SPECIFIC HUMIDITY IN COMPUTING MOISTURE DIVERGENCE
C
C        CREATE IDS FOR OBTAINING SPECIFIC HUMIDITY
C
      MD(1)=ICCCFFF(2,ISO)*1000+IDPARS(4)
      MD(2)=IDPARS(7)
      MD(3)=IDPARS(9)*1000000+IDPARS(12)
      MD(4)=0
C
C        CALL PRSID1 TO PARSE IDS MD( ) TO MDPARS( )
C        
      CALL PRSID1(KFILDO,MD,MDPARS)
C
C        CALL SPECHUM TO OBTAIN SPECIFIC HUMIDITY FD2
C
      CALL SPECHUM(KFILDO,KFIL10,MDPARS,MD,NDATE,
     1             NGRIDC,ND11,NSLABM,IPACK,IWORK,FD2,ND2X3,
     2             LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3             IS0,IS1,IS2,IS4,ND7,
     4             FD3,FD4,FD5,ND2X3,
     5             ISTAV,L3264B,MISTOT,IER)
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,300)IER
 300     FORMAT('     SPECIFIC HUMIDITY ROUTINE SPECHUM FAILED IN',
     1          ' COMPUTING DIVERGENCE IN MDIV.  IER =',I3)
C           THIS DIAGNOSTIC FOLLOWS ONE IN SPECHUM.
         GOTO 900
      ENDIF
C
      MX=IS2(3)
      MY=IS2(4)
C
      IF(NSLAB.NE.NSLABM.OR.LX.NE.MX.OR.LY.NE.MY)THEN
C
C           THE GRID CHARACTERISTICS ARE NOT THE SAME
C
         IER=100
         WRITE(KFILDO,400)(LD(J),J=1,4),(NGRIDC(J,NSLAB),J=1,6),LX,LY,
     1                    (MD(J),J=1,4),(NGRIDC(J,NSLABM),J=1,6),MX,MY, 
     2                    IER
 400     FORMAT(/' ****DIFFERENT GRID CHARACTERISTICS.  PREDICTOR NOT ',
     1           'COMPUTED IN MDIV.  VALUES FROM NGRIDC( , ) AND X*Y ',
     2           'ARE: ',2(/,2X,3I10.9,I4.3,4X,6I10,4X,I3,'*',I3),
     3           '  IER=',I3)
         GOTO 900
      ENDIF
C
C        WIND DIVERGENCE OBTAINED FROM CALLING DIVW IS IN THE UNIT OF 
C        10^-5 PER SECOND.  THEY NEED TO BE CONVERTED TO THE BASIC UNIT
C        OF PER SECOND.
C
      DO I=1,LX*LY
         IF(FD1(I).NE.9999.)FD1(I)=FD1(I)*0.00001
      ENDDO
C
C        COMPUTE THE DIVERGENCE TERM OF MOISTURE DIVERGENCE:
C        Q(DU/DX+DV/DY)
C
      DO 500 I=1,LX*LY
         IF(FD1(I).NE.9999.AND.FD2(I).NE.9999.)THEN
            FD1(I)=FD2(I)*FD1(I)
         ELSE
            FD1(I)=9999.
         END IF
C
 500  CONTINUE
C
C        CREATE IDS FOR ENTERING ADVCTW.
C
      ND(1)=ICCCFFF(1,ISO)*1000+IDPARS(4)
C
      IF(ISO.EQ.2.AND.IDPARS(7).EQ.2)THEN
C           FOR A CONSTANT HEIGHT SURFACE OF 2 M, THE MATCHING
C           WIND MUST BE AT 10 M.
         ND(2)=10
      ELSE
         ND(2)=IDPARS(7)
      ENDIF
C
      ND(3)=IDPARS(9)*1000000+IDPARS(12)
      ND(4)=0
C
C        CALL PRSID1 TO PARSE IDS LD( ) TO LDPARS( )    
C
      CALL PRSID1(KFILDO,ND,NDPARS)
C
C        COMPUTE THE ADVECTION TERM OF MOISTURE DIVERGENCE:
C        UDQ/DX+VDQ/DY
C
      CALL ADVCTW(KFILDO,KFIL10,NDPARS,ICCCFFF(3,ISO),NDATE,
     1            NGRIDC,ND11,NSLABN,IPACK,IWORK,FD2,ND2X3,
     2            LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3            IS0,IS1,IS2,IS4,ND7,
     4            FD3,FD4,FD5,FD6,FDSINS,FDMS,ND2X3,
     5            ISTAV,L3264B,MISTOT,IER)
C
      IF(IER.NE.0)THEN
          WRITE(KFILDO,600)IER
 600     FORMAT('     ATTEMPT TO COMPUTE THE ADVECTION TERM FAILED IN',
     1          ' MDIV.  IER=',I3)
C           THIS DIAGNOSTIC FOLLOWS ONE IN ADVCTW.
         GOTO 900
      ENDIF
c
      NX=IS2(3)
      NY=IS2(4)
C
C        COMPUTE THE MOISTURE DIVERGENCE BY ADDING FD1 AND FD2
C
      DO 700 I=1,LX*LY
C
         IF(FD1(I).NE.9999..AND.FD2(I).NE.9999.)THEN
            FDMD(I)=FD1(I)+FD2(I)
C
C              MULTIPLY THE MOISTURE DIVERGENCE FIELD BY 10^8.  THIS
C              FACTOR WAS DECIDED BY COMPUTING THE RANGE, MEAN, AND
C              STANDARD DEVIATION OF THE MOISTURE DIVERGENCE FIELD AT
C              1000 MB AND 500 MB DURING BOTH A SUMMER AND A WINTER
C              CASE, AND THEN DETERMINING AN APPROPRIATE MULTIPLICATION
C              FACTOR WHICH WOULD OBTAIN VALUES WHOSE METEOROLOGICAL
C              SIGNIFICANCE IS CONTAINED IN THE INTEGER PART OF THE
C              NUMBER.
C
            FDMD(I)=FDMD(I)*100000000.
         ELSE
            FDMD(I)=9999.
         END IF
C
 700  CONTINUE
C
      GOTO 999
C
C        FILL IN THE MOISTURE DIVERGENCE DATA ARRAY WITH MISSING VALUES
C
 900  DO I=1,ND2X3
         FDMD(I)=9999.
      ENDDO
C
 999  RETURN
      END
