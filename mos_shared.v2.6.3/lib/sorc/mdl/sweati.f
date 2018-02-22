      SUBROUTINE SWEATI(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                  NGRIDC,ND11,NSLAB,IPACK,IWORK,FDSI,ND5,
     2                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                  IS0,IS1,IS2,IS4,ND7,
     4                  FD1,FD2,FD3,FD4,FD5,FD6,FD7,ND2X3,
     5                  ISTAV,L3264B,MISTOT,IER)
C
C
C        JULY     1998   SFANOS   TDL   MOS-2000
C        OCTOBER  1998   SFANOS   CHANGED ERROR MESSAGE NUMBERS, PUT IN
C                                 NEW ERROR MESSAGES AFTER MODIFYING
C                                 WINDDR
C        NOVEMBER 1998   SFANOS   MODIFIED WORK ARRAYS
C        DECEMBER 1998   SFANOS   CHANGED STRUCTURE OF CALLS TO GET 
C                                 DATA IN RIGHT ORDER
C        DECEMBER 2002   WEISS    CHANGED ND5 TO ND2X3
C        MAY      2003   GLAHN    MODIFIED LINES IN CALL;  SET
C                                 DIMENSIONS OF IPACK( ), IWORK( ) AND 
C                                 FDSI( ) = ND5; REARRANGED ARGUMENTS
C                                 TO SUBROUTINE CALLS; COMBINED CHECKS
C                                 FOR GRID CHARACTERISTICS
C        OCTOBER  2003   SMB      CORRECTED FORMAT STATEMENTS 145, 155,
C                                 165, 175, AND 185 FOR THE IBM
C
C        PURPOSE
C            TO COMPUTE THE SWEAT INDEX.
C            THE FORMULA FOR SWEAT INDEX IS
C            DATA = 12*TD(850)+20*(TT-49)+2V(850)+V(500)
C                   + 125(S+0.2) WHERE
C                   TT= TOTAL TOTAL INDEX (CALCULATED FROM
C                       SUBROUTINE TTOTALS)
C                   V850 AND V500 ARE WIND SPEEDS
C                   S= SIN(500 MB WND DIRECTION - 850 MB WND
C                      DIRECTION). SEE CALCULATIONS FOR WHEN 
C                      S = 0.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               007 220 - SWEAT INDEX VARIABLE ON AN ISOBARIC SURFACE
C
C        DATA SET USE
C            KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT(PRINT) FILE.
C                     (OUTPUT)
C            KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                     ACCESS.(INPUT-OUTPUT)
C
C        VARIABLES
C              ABZERO = PARAMETER STATEMENT THAT ALLOWS A
C                       VALUE OF -273.15 K.
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE(,) (J=1,ND10).
C                       WHEN CORE() IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C              FD1(K) = WORK ARRAY TO HOLD 500 MB WIND DIRECTION
C                       RETRIEVED FROM CALLING
C                       SUBROUTINE WINDDR.  IN DEGREES. (K=1,
C                       ND2X3). (INTERNAL)
C              FD2(K) = WORK ARRAY TO HOLD TOTAL TOTALS INDEX.
C                       OBTAINED FROM CALLING SUBROUTINE TTOTALS.
C                       (K=1,ND2X3). (INTERNAL)
C              FD3(K) = WORK ARRAY TO HOLD 850 MB DEWPOINT.
C                       OBTAINED FROM CALLING SUBROUTINE DEWPT. IN
C                       DEGREES K. (K=1,ND2X3). (INTERNAL)
C              FD4(K) = 850 MB WIND SPEED OBTAINED FROM SUBROUTINE
C                       WSPEED THEN CHANGED FROM M/S TO KNOTS FOR
C                       SWEAT INDEX EQUATION. (K=1,ND2X3). (INTERNAL)
C              FD5(K) = 500 MB WIND SPEED OBTAINED FROM SUBROUTINE
C                       WSPEED THEN CHANGED FROM M/S TO KNOTS FOR
C                       SWEAT INDEX EQUATION. (K=1,ND2X3). (INTERNAL)
C              FD6(K) = TEMPORARY HOLDING ARRAY USED DUE TO THE USE
C                       OF FDSI(K) OR THE DATA ARRAY AS A WORK ARRAY.
C                       (K=1,ND2X3). (INTERNAL)
C              FD7(K) = THE SHEAR TERM FOR THE SWEAT INDEX
C                       EQUATION. (K=1,ND2X3). (INTERNAL)
C             FDSI(K) = DATA ARRAY THAT HOLDS THE SWEAT INDEX VALUE
C                       IT IS ALSO USED AS A WORK ARRAY THAT GETS THE
C                       850 WIND DIRECTION.  THIS IS USED TO CONSERVE
C                       SPACE. (K=1,ND5). (OUTPUT)
C           ICCCFFF() = CONTAINS IDPARS(1) AND IDPARS(2) ID FOR THE
C                       METEOROLOGICAL PARAMETERS BEING USED.
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
C        IDPARSDR8(J) = SEE IDPARS(J).  IDPARSDR8 IS THE PARSED 
C                       COMPONENTS OF MDR8(). (J=1,15)
C         IDPARSW5(J) = SEE IDPARS(J).  IDPARSW5 IS THE PARSED
C                       COMPONENTS OF MW5(). (J=1,15)
C         IDPARSW8(J) = SEE IDPARS(J).  IDPARSW8 IS THE PARSED
C                       COMPONENTS OF MW8(). (J=1,15).
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                       100 = THE TWO GRIDS NEEDED ARE NOT THE SAME SIZE
C                       101 = GRID SIZE IS TOO BIG FOR AN ARRAY, WHOSE 
C                             DIMENSION IS ND5.
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE
C                             SWEAT INDEX.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C                  IJ = LOOP CONTROL VARIABLE
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
C                       FIELDS AS READ FROM THE ARCHIVE. (INPUT)
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64). (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE(,)
C                       THAT HAVE BEEN USED IN THIS RUN. (INPUT)
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
C              MD8(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD1() (J=1,4). (INTERNAL)
C             MDR5(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD3() (J=1,4). (INTERNAL)
C             MDR8(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD2() (J=1,4). (INTERNAL)
C           MDX8,MDY8 = DIMENSIONS OF GRID RETURNED FOR DEWPOINT
C                       TEMPERATURE (INTERNAL)
C         MDDX5,MDDY5 = DIMENSIONS OF GRID RETURNED FOR 500 WIND
C                       DIRECTION (INTERNAL)
C         MDDX8,MDDY8 = DIMENSIONS OF GRID RETURNED FOR 850 WIND
C                       DIRECTION (INTERNAL)
C         MDWX5,MDWY5 = DIMENSIONS OF GRID RETURNED FOR 500 WIND SPEED
C                       (INTERNAL)
C         MDWX8,MDWY8 = DIMENSIONS OF GRID RETURNED FOR 850 WIND SPEED
C                       (INTERNAL)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C           MTTX,MTTY = DIMENSIONS OF GRID RETURNED FOR TOTAL TOTALS
C                       (INTERNAL)
C              MW5(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD4() (J=1,4). (INTERNAL)
C              MW8(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED
C                       INTO FD5() (J=1,4). (INTERNAL)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE. (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF
C                       THE GRID IS NOT KNOWN BEFORE FDTK AND FDDP
C                       ARE FETCHED.  (INPUT)
C                 ND5 = DIMENSION OF IPACK( ),IWORK( ) AND FDSI( ). 
C                       (INPUT)
C                 ND7 = DIMENSION OF IS0(),IS1(),IS2(), AND IS4().
C                       NOT ALL LOCATIONS ARE USED. (INPUT)
C                 ND9 = THE SECOND DIMENSION OF LSTORE(,). (INPUT)
C                ND10 = DIMENSION OF CORE(). (INPUT)
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN
C                       BE DEALT WITH ON THIS RUN.  LAST DIMENSION 
C                       OF NGRIDC(,). (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING COUNT FROM THE BEGINNING OF
C                       THE PROGRAM.  THIS COUNT IS MAINTAINED IN
C                       CASE THE USER NEEDS IT(DIAGNOSTICS, ETC.).
C                       (OUTPUT)
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
C              NSLABD = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  USED AFTER SUBROUTINE DEWPOINT
C                       TO CHECK IF NSLABD = NSLAB. (INTERNAL) 
C            NSLABDR5 = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  USED AFTER SUBROUTINE WINDDR
C                       TO CHECK IF NSLABDR5 = NSLAB. (INTERNAL)
C            NSLABDR8 = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  USED AFTER SUBROUTINE WINDDR
C                       TO CHECK IF NSLABDR8 = NSLAB. (INTERNAL)
C             NSLABTT = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  USED AFTER SUBROUTINE TTOTALS
C                       TO CHECK IF NSLABTT = NSLAB. (INTERNAL)
C            NSLABWD5 = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  USED AFTER SUBROUTINE WSPEED
C                       TO CHECK IF NSLABWD5 = NSLAB. (INTERNAL)
C            NSLABWD8 = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  USED AFTER SUBROUTINE WSPEED
C                       TO CHECK IF NSLABWD8 = NSLAB. (INTERNAL)
C                  PI = 3.141593
C        1         2         3         4         5         6         7 X
C
C     NON SYSTEM SUBROUTINES USED
C        DEWPT,GFETCH,PRSID1,TTOTALS,WSPEED,WINDDR
C
      IMPLICIT NONE
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER ICCCFFF(4),MD8(4),MDR5(4),MDR8(4),MW5(4),MW8(4),ND(4)
      INTEGER IDPARSDR5(15),IDPARSDR8(15),IDPARSW5(15),
     1        IDPARSW8(15),JDPARS(15),MDPARS(15)
      INTEGER IER,IJ,ISTAV,J,KFILDO,KFIL10,L3264B,LITEMS,
     1        MDDX5,MDDX8,MDDY5,MDDY8,
     2        MDWX5,MDWY5,MDWX8,MDWY8,
     3        MISTOT,MDX8,MDY8,MTTX,MTTY,
     4        NBLOCK,ND2X3,ND5,ND7,ND9,ND10,
     5        ND11,NDATE,NFETCH,NSLAB,NSLABD,
     6        NSLABDR5,NSLABDR8,NSLABTT,NSLABWD5,NSLABWD8
C
      REAL FDSI(ND5)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),
     1     FD5(ND2X3),FD6(ND2X3),FD7(ND2X3)
      REAL CORE(ND10)
      REAL ABSZRO,PI
C
      DATA ABSZRO/-273.15/,PI/3.141593/
      DATA ICCCFFF/003100,
     1             007210, 
     2             004210,
     3             004200/
C
      IER=0
      ISTAV=0
C
D     WRITE(KFILDO,100)
D100  FORMAT(/,' ENTERING SWEATI')
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C
      IF(IDPARS(1).NE.007.OR.IDPARS(2).NE.220)THEN
         WRITE(KFILDO,101)(JD(J),J=1,4)
 101     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1           'SWEAT INDEX.  VARIABLE ',I9.9,2I10.9,I4.3,
     2           ' NOT COMPUTED IN SWEATI. ')
         IER=103
         GOTO 800
      END IF
C
C        CREATE ID FOR 850 MB WIND DIRECTION.
C        
      MDR8(1)=ICCCFFF(4)*1000+IDPARS(4)
      MDR8(2)=850
      MDR8(3)=IDPARS(9)*1000000+IDPARS(12)
      MDR8(4)=0
C
C        GET 850 MB WIND DIRECTION.
C
      CALL PRSID1(KFILDO,MDR8,IDPARSDR8)
      CALL WINDDR(KFILDO,KFIL10,IDPARSDR8,MDR8,NDATE,
     1            NGRIDC,ND11,NSLABDR8,IPACK,IWORK,FDSI,ND5,
     2            LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,
     3            NFETCH,IS0,IS1,IS2,IS4,ND7,FD4,FD5,FD6,
     4            FD7,ND2X3,
     5            ISTAV,L3264B,MISTOT,IER)
      IF(IER.NE.0)GOTO 800
      MDDX8=IS2(3)
      MDDY8=IS2(4)
      NSLAB=NSLABDR8
C        NSLAB IS FOR RETURN TO CALLING PROGRAM.
C
C        CREATE ID FOR 500 MB WIND DIRECTION.
C
      MDR5(1)=ICCCFFF(4)*1000+IDPARS(4)
      MDR5(2)=500
      MDR5(3)=IDPARS(9)*1000000+IDPARS(12)
      MDR5(4)=0
C
C        GET 500 MB WIND DIRECTION.
C
      CALL PRSID1(KFILDO,MDR5,IDPARSDR5)
      CALL WINDDR(KFILDO,KFIL10,IDPARSDR5,MDR5,NDATE,
     1            NGRIDC,ND11,NSLABDR5,IPACK,IWORK,FD1,ND2X3,
     2            LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3            IS0,IS1,IS2,IS4,ND7,
     4            FD4,FD5,FD6,FD7,ND2X3,
     5            ISTAV,L3264B,MISTOT,IER)
      IF(IER.NE.0)GOTO 800
      MDDX5=IS2(3)
      MDDY5=IS2(4)
C     
C        CHECK GRID CHARACTERISTICS.
C
      IF(MDDX5.NE.MDDX8.OR.MDDY5.NE.MDDY8.OR.NSLABDR8.NE.NSLABDR5)THEN
C         THE GRID CHARACTERISTICS ARE NOT THE SAME
        WRITE(KFILDO,145)(MDR5(J),J=1,4),(NGRIDC(J,NSLABDR5),J=1,6),
     1                                    MDDX5,MDDY5,
     2                   (MDR8(J),J=1,4),(NGRIDC(J,NSLABDR8),J=1,6),
     3                                    MDDX8,MDDY8
 145    FORMAT(/' ****THE GRIDS NEEDED IN SWEATI HAVE DIFFERENT',
     1         ' CHARACTERISTICS AT 145.  PREDICTOR NOT COMPUTED.',
     2         ' VALUES FROM NGRIDC(,) AND MX,MY.',
     3         (/5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
      END IF
C
C        CREATE ID FOR TOTAL TOTALS.
C
      ND(1)=ICCCFFF(2)*1000+IDPARS(4)
      ND(2)=850
      ND(3)=MDR8(3)
      ND(4)=0
C
C        CALL TOTAL TOTALS.
C
      CALL PRSID1(KFILDO,ND,JDPARS)
      CALL TTOTALS(KFILDO,KFIL10,JDPARS,ND,NDATE,
     1            NGRIDC,ND11,NSLABTT,IPACK,IWORK,FD2,ND2X3,
     2            LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3            IS0,IS1,IS2,IS4,ND7,
     4            FD3,FD4,FD5,FD6,FD7,ND2X3,
     5            ISTAV,L3264B,MISTOT,IER)
      IF(IER.NE.0)GOTO 800
      MTTX=IS2(3)
      MTTY=IS2(4)
C
C        CHECK GRID CHARACTERISTICS.
C
      IF(MTTX.NE.MDDX5.OR.MTTY.NE.MDDY5.OR.NSLABDR5.NE.NSLABTT)THEN
C        THE GRID CHARACTERISTICS ARE NOT THE SAME
        WRITE(KFILDO,155)(ND(J),J=1,4),(NGRIDC(J,NSLABTT),J=1,6),
     1                                  MTTX,MTTY,
     2                   (MD8(J),J=1,4),(NGRIDC(J,NSLABDR5),J=1,6),
     3                                  MDDX5,MDDY5
 155    FORMAT(/' ****THE GRIDS NEEDED IN SWEATI HAVE DIFFERENT',
     1         ' CHARACTERISTICS AT 155.  PREDICTOR NOT COMPUTED.',
     2         ' VALUES FROM NGRIDC(,) AND MX,MY.',
     3         (/5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
      END IF
C
C        CREATE ID FOR 850 MB DEWPOINT TEMPERATURE.
C
      MD8(1)=ICCCFFF(1)*1000+IDPARS(4)
      MD8(2)=850
      MD8(3)=IDPARS(9)*1000000+IDPARS(12)
      MD8(4)=0
C
C        CALL DEWPT TO RETRIEVE THE DEWPOINT TEMPERATURE.
C
      CALL PRSID1(KFILDO,MD8,MDPARS)
      CALL DEWPT(KFILDO,KFIL10,MDPARS,MD8,NDATE,
     1           NGRIDC,ND11,NSLABD,IPACK,IWORK,FD3,ND2X3,
     2           LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3           IS0,IS1,IS2,IS4,ND7,
     4           FD4,FD5,FD6,FD7,ND2X3,
     5           ISTAV,L3264B,MISTOT,IER)
      IF(IER.NE.0)GOTO 800
      MDX8=IS2(3)
      MDY8=IS2(4)
C
C        CHECK GRID CHARACTERISTICS.
C
      IF(MDX8.NE.MTTX.OR.MTTY.NE.MDY8.OR.NSLABD.NE.NSLABTT)THEN
C        THE GRID CHARACTERISTICS ARE NOT THE SAME
        WRITE(KFILDO,165)(ND(J),J=1,4),(NGRIDC(J,NSLABTT),J=1,6),
     1                                  MTTX,MTTY,
     2                   (MD8(J),J=1,4),(NGRIDC(J,NSLABD),J=1,6),
     3                                  MDX8,MDY8
 165    FORMAT(/' ****THE GRIDS NEEDED IN SWEATI HAVE DIFFERENT',
     1          ' CHARACTERISTICS AT 165.  PREDICTOR NOT COMPUTED.',
     2          '  VALUES FROM NGRIDC(,) AND MX,MY ARE:',
     3          (/5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
      END IF
C
C        CREATE ID FOR WIND SPEED AT 850 MB.
C
      MW8(1)=ICCCFFF(3)*1000+IDPARS(4)
      MW8(2)=850
      MW8(3)=MDR8(3)
      MW8(4)=0
C
C        CALL WIND SPEED TO GET 850 WIND.
C
      CALL PRSID1(KFILDO,MW8,IDPARSW8)
      CALL WSPEED(KFILDO,KFIL10,IDPARSW8,MW8,NDATE,
     1            NGRIDC,ND11,NSLABWD8,IPACK,IWORK,FD4,ND2X3,
     2            LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3            IS0,IS1,IS2,IS4,ND7,
     4            FD6,FD7,ND2X3,
     5            ISTAV,L3264B,MISTOT,IER)
      IF(IER.NE.0)GOTO 800
      MDWX8=IS2(3)
      MDWY8=IS2(4)
C
C        CHECK GRID CHARACTERISTICS.
C
      IF(MDX8.NE.MDWX8.OR.MDY8.NE.MDWY8.OR.NSLABD.NE.NSLABWD8)THEN
C        THE GRID CHARACTERISTICS ARE NOT THE SAME
        WRITE(KFILDO,175)(ND(J),J=1,4),(NGRIDC(J,NSLABD),J=1,6),
     1                                  MDX8,MDY8,
     2                   (MW8(J),J=1,4),(NGRIDC(J,NSLABWD8),J=1,6),
     3                                  MDWX8,MDWY8
 175    FORMAT(/' ****THE GRIDS NEEDED IN SWEATI HAVE DIFFERENT',
     1         ' CHARACTERISTICS AT 175.  PREDICTOR NOT COMPUTED.',
     2         ' VALUES FROM NGRIDC(,) AND MX,MY ARE:',
     3         (/5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
      END IF
C
C        CREATE ID FOR 500 MB WIND SPEED.
C
      MW5(1)=ICCCFFF(3)*1000+IDPARS(4)
      MW5(2)=500
      MW5(3)=MDR8(3)
      MW5(4)=0
C
C        CALL WIND SPEED TO GET 500 WIND.
C
      CALL PRSID1(KFILDO,MW5,IDPARSW5)
      CALL WSPEED(KFILDO,KFIL10,IDPARSW5,MW5,NDATE,
     1            NGRIDC,ND11,NSLABWD5,IPACK,IWORK,FD5,ND2X3,
     2            LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3            IS0,IS1,IS2,IS4,ND7,
     4            FD6,FD7,ND2X3,
     5            ISTAV,L3264B,MISTOT,IER)
      IF(IER.NE.0)GOTO 800
      MDWX5=IS2(3)
      MDWY5=IS2(4)
C
C        CHECK GRID CHARACTERISTICS.
C
      IF(MDWX8.NE.MDWX5.OR.MDWY8.NE.MDWY5.OR.NSLABWD5.NE.NSLABWD8)THEN
C        THE GRID CHARACTERISTICS ARE NOT THE SAME
         WRITE(KFILDO,185)(MW5(J),J=1,4),(NGRIDC(J,NSLABWD5),J=1,6),
     1                                    MDWX5,MDWY5,
     2                    (MW8(J),J=1,4),(NGRIDC(J,NSLABWD8),J=1,6),
     3                                    MDWX8,MDWY8
 185    FORMAT(/' ****THE GRIDS NEEDED IN SWEATI HAVE DIFFERENT',
     1          ' CHARACTERISTICS AT 185.  PREDICTOR NOT COMPUTED.',
     2          ' VALUES FROM NGRIDC(,) AND MX,MY ARE:',
     3          (/5X,I9.9,I10.9,I10.9,I4.3,4X,6I10,4X,2I5))
        IER=100
        GOTO 800
      END IF
C
C        COMPUTATION OF SWEAT INDEX.
C
      DO 300 IJ=1,MDX8*MDY8
        FD6(IJ)=FDSI(IJ)
        FD7(IJ)=SIN((FD1(IJ)-FD6(IJ))*(PI/180.0))
C        FD7 IS NOW THE ENTIRE SHEAR TERM FOR SWEAT INDEX
        FD7(IJ)=125*(FD7(IJ)+0.2)
C
C        CHECK IN SECOND TERM OF SWEAT EQUATION.
C
        IF(FD2(IJ).LT.49.)THEN
          FD2(IJ)=0.
        ELSE
          FD2(IJ)=20*(FD2(IJ)-49.)
        END IF
C
C        DEWPOINT NEEDED IN CELSIUS FOR FORMULA.
C
        IF(FD3(IJ).LT.(-ABSZRO))THEN 
          FD3(IJ)=0.
        ELSE
          FD3(IJ)=FD3(IJ)+ABSZRO
        END IF
C
C        NEEDED TO CONVERT TO KNOTS FROM M/S FOR FORMULA.
C
        FD5(IJ)=FD5(IJ)*1.94
        FD4(IJ)=FD4(IJ)*1.94
C
C        EXCEPTIONS TO THE SPEEDS AND DIRECTIONS FOR 
C        SWEAT INDEX FORMULA.
C
        IF(FD6(IJ).LT.130.OR.FD6(IJ).GT.250)FD7(IJ)=0.0
        IF(FD1(IJ).LT.210.OR.FD1(IJ).GT.310)FD7(IJ)=0.0
        IF((FD1(IJ)-FD6(IJ)).LT.0)FD7(IJ)=0.0
        IF(FD5(IJ).LT.15.AND.FD4(IJ).LT.15.)FD7(IJ)=0.0
C       
C        SWEAT INDEX CALCULATION
C
        FDSI(IJ)=(12*FD3(IJ)+FD2(IJ)+2*FD4(IJ)+
     1            FD5(IJ)+FD7(IJ))
 300  CONTINUE
      GOTO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C 
 800  DO 801 J=1,ND2X3
        FDSI(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END
