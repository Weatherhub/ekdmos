      SUBROUTINE MODELMXMN(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                     NGRIDC,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     2                     LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                     IS0,IS1,IS2,IS4,ND7,
     4                     FD1,FD2,ND2X3,
     5                     ISTAV,L3264B,MISTOT,IER)
C
C
C        JULY     2002   COSGROVE  MDL   MOS-2000
C        DECEMBER 2002   WEISS     CHANGED ND5 TO ND2X3
C        APRIL    2003   GLAHN     MODIFIED LINES IN CALL;  SET
C                                  DIMENSIONS OF IPACK( ), IWORK( )
C                                  AND FDMRH( ) = ND5;  MODIFIED LINES
C                                  IN CALLS TO GFETCH; COMMENTS;
C                                  COUNTED MISTOT AFTER GFETCH;
C                                  REMOVED INITIALIZATION OF DATA( );
C                                  MADE END ERROR LOOP 1 TO ND2X3;
C                                  ELIMINATED K AND MD2( ) IN TYPE;
C                                  CHANGED IER FROM 200 TO 187;
C                                  REVISED ORDER OF TYPES; CHANGED
C                                  .AND. TO .OR. IN CHECKING IDS
C        MAY      2003   GLAHN     REARRANGED TYPE STATEMENTS
C        JUNE     2003   GLAHN     USED IDPARS(7) FOR UUUU IN FETCHING
C                                  TEMPERATURES RATHER THAN ALWAYS 2
C        DEC      2007   RLC       ADDED LINES TO CONVERT TO F FOR THE
C                                  APPROPRIATE IDS
C
C        PURPOSE
C            THIS SUBROUTINE DETERMINES THE 12-HR MAXIMUM OR MINIMUM 
C            TEMPERATURE USING THE TWO 6-HR VALUES THAT COMPRISE THE 
C            12-HR PERIOD OF INTEREST.  THIS ROUTINE WAS WRITTEN 
C            BECAUSE WE USED TO HAVE 12-HR MX/MN IN THE MRF MODEL 
C            ARCHIVE, BUT WE ONLY HAVE 6-HR MX/MN IN THE GFS ARCHIVE.
C            THE 12-HR MX/MN CAN ONLY BE COMPUTED FOR PROJECTIONS THAT
C            ARE A MULTIPLE OF 6.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C                002 051 - 12-HR MAXIMUM TEMPERATURE(K)
C                002 061 - 12-HR MINIMUM TEMPERATURE(K)
C                002 351 - 12-HR MAXIMUM TEMPERATURE(F)
C                002 361 - 12-HR MINIMUM TEMPERATURE(F)
C
C        HISTORY:   
C        02-07       COSGROVE	THIS SUBROUTINE IS NEW TO U201.
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
C             DATA( ) = THE 12-H MX/MN TEMPERATURE COMPUTED FROM THE TWO 6-H 
C                       VALUES.  (OUTPUT)
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
C                       100 = THE TWO GRIDS NEEDED ARE NOT THE SAME SIZE
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
      INTEGER IDPARS(15),JD(4)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER MD(4),MD1(4)
      INTEGER ICCC,IFFF,LX,LY,NSLAB1,I,IER,ISTAV,
     1        J,KFILDO,KFIL10,L3264B,LITEMS,
     2        MISSP,MISSS,MISTOT,IMXMN,
     3        NBLOCK,ND2X3,ND5,ND7,ND9,ND10,ND11,
     4        NDATE,NFETCH,NPACK,NSLAB,
     5        NTIMES,NWORDS
C
      REAL DATA(ND5)
      REAL FD1(ND2X3),FD2(ND2X3)
      REAL CORE(ND10)
C
      IER  =0
      ISTAV=0
C
C        MAKE SURE THE REQUESTED PREDICTOR IS THE MX OR MX TEMPERATURE 
C
       IF((IDPARS(1).NE.002).OR.((IDPARS(2).NE.051).AND.
     &    (IDPARS(2).NE.061).AND.(IDPARS(2).NE.351).AND.
     &    (IDPARS(2).NE.361)))THEN
	 IER=103
	 WRITE(KFILDO,100)(JD(J),J=1,4),IER
 100     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE',
     1           ' 12-H MX/MN TEMP.',
     2          /'     PREDICTOR ',I9.9,I10.9,I10.9,I4.3,
     3           ' NOT ACCOMMODATED IN MODELMXMN.  IER = ',I3)
	 GO TO 800
      ENDIF
C
C        CHECK IF PROJECTION IS LESS THAN 12 HOURS AND IS A MULTIPLE
C        OF 6
C
      IF((IDPARS(12).LT.12).OR.(MOD(IDPARS(12),6).NE.0))THEN
         WRITE(KFILDO,115)IDPARS(12)
 115     FORMAT(/,' ****BAD PROJECTION IN IDPARS(12) =',I5,
     1            '.  PROJECTION MUST BE GREATER THAN OR EQUAL TO ',
     2            '12 AND A MULTIPLE OF 6 IN MODELMXMN.')
         IER=187
         GOTO 800
      END IF
C
C        CHECK TO SEE IF THE USER WANTS MAXIMUM TEMPERATURE OR MINIMUM 
C        TEMPERATURE.  SET FLAG AND 6-HR ID ACCORDINGLY 
C
        IF((IDPARS(2).EQ.051).OR.(IDPARS(2).EQ.351))THEN
           IMXMN=1
           ICCC = 002
           IFFF = 011
        ELSEIF((IDPARS(2).EQ.061).OR.(IDPARS(2).EQ.361))THEN
           IMXMN=2
           ICCC = 002
           IFFF = 021
        ENDIF
C
C        NOW GO GET THE 6-HR MX/MN VALID ENDING AT THE SAME TIME AS 
C        THE 12-HR TEMP
C
      MD(1)= ICCC * 1000000 + IFFF * 1000 + IDPARS(4)
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
C       RECORD THE GRID DIMENSIONS TO USE FOR VERIFICATION IN LATER
C       CALLS TO GFETCH
C
      LX = IS2(3)
      LY = IS2(4)
      NSLAB1 = NSLAB
C
C        NOW GO BACK 6 HOURS AND GET THE OTHER TEMPERATURE.  SET UP THE 
C        MD1 ARRAY AND NDATE 
C
      MD1(1)= ICCC * 1000000 + IFFF * 1000 + IDPARS(4)
      MD1(2)=IDPARS(7)
      MD1(3)=IDPARS(9)*1000000 +(IDPARS(12) - 6)
      MD1(4)=0
C
C        CALL GFETCH TO GET THE SECOND 6-HR MX/MN TEMPERATURE 
C
      CALL GFETCH(KFILDO,KFIL10,MD1,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,
     4            1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0) GOTO 800
C
C         CHECK THE GRID DIMENSIONS
C
      IF((NSLAB.NE.NSLAB1).OR.(IS2(3).NE.LX).OR.(IS2(4).NE.LY))THEN
         WRITE(KFILDO,200)NSLAB1,NSLAB
 200     FORMAT(/,' ****THE GRID CHARACTERISTICS OF THE SECOND TEMP',
     &            ' ARE DIFFERENT IN MODELMXMN.',I3,2X,I3)
         IER=100
         GOTO 800
      ENDIF
C  
       DO I=1,LX*LY
C
	 IF(IMXMN.EQ.1)THEN
            IF((NINT(FD1(I)).EQ.9999).OR.(NINT(FD2(I)).EQ.9999))
     &           DATA(I)=9999.
	    DATA(I)=MAX(FD1(I),FD2(I))
C
C              CONVERT TO FAHRENHEIT IF REQUESTED
C
            IF(IDPARS(2).EQ.351) THEN
               DATA(I)=(DATA(I)-273.15)*1.8+32.0
            END IF
C
         ELSEIF(IMXMN.EQ.2)THEN
            IF((NINT(FD1(I)).EQ.9999).OR.(NINT(FD2(I)).EQ.9999))
     &           DATA(I)=9999.
	    DATA(I)=MIN(FD1(I),FD2(I))
C
C              CONVERT TO FAHRENHEIT IF REQUESTED
C
            IF(IDPARS(2).EQ.361) THEN
               DATA(I)=(DATA(I)-273.15)*1.8+32.0
            END IF
C
         ELSE
            DATA(I)=9999.
         ENDIF
C
       ENDDO
C
       GO TO 900
C
C         IF THERE WAS A PROBLEM IN GFETCH IT WOULD COME HERE TO
C         SET DATA TO MISSING
C
 800   DO I=1,ND2X3
         DATA(I)=9999.
       ENDDO
C
 900  RETURN
      END

