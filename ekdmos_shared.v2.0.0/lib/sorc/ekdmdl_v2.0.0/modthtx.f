      SUBROUTINE MODTHTX(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                  NGRIDC,ND11,NSLABT,IPACK,IWORK,THT,ND5,
     2                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                  IS0,IS1,IS2,IS4,ND7,
     4                  FDTK,FDRH,ND2X3,
     5                  ISTAV,L3264B,MISTOT,IER)
C
C        SEPTEMBER 1998   HUGHES   TDL   MOS-2000
C        NOVEMBER  2002   WEISS    CHANGED ND5 TO ND2X3
C        MAY       2003   GLAHN    MODIFIED LINES IN CALL;  SET
C                                  DIMENSIONS OF IPACK( ), IWORK( ) AND 
C                                  EPT( ) = ND5; REARRANGED ARGUMENTS
C                                  TO SUBROUTINE CALLS; SUBSTITUTED
C                                  AUTOMATIC ARRAY FDLC( ) FOR FD3( );
C                                  COMMENTS; PUT ICCCFFF( ) INTO DATA
C                                  STATEMENT
C
C        PURPOSE
C            TO COMPUTE THE HEAT INDEX IN DEGREES FAHRENHEIT.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               002 003 - HEAT INDEX.
C            
C           DATA SET USE
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                      (OUTPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                       ACCESS.(INPUT-OUTPUT)
C
C        VARIABLES
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE(,) (J=1,ND10).
C                       WHEN CORE() IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C              THT(J) = DATA ARRAY WHICH RETURNS THE EQUIVALENT 
C                       POTENTIAL TEMPERATURE (THETAE) (J=1,ND5).
C                       (OUTPUT)
C             FDTK(K) = WORK ARRAY TO HOLD THE TEMPERATURE (K=1,ND2x3).
C                       (INTERNAL).
C             FDRH(K) = WORK ARRAY TO HOLD THE RELATIVE HUMIDITY
C                       (K=1,ND2X3). (INTERNAL)
C              FD4(K) = WORK ARRAY (K=1,ND2X3). (INTERNAL)
C                   I = LOOP CONTROL VARIABLE
C          ICCCFFF(J) = CONTAINS THE CCCFFF ID (SEE IDPARS) FOR THE
C                       HEAT INDEX (J=1), THE SURFACE TEMPERATURE (J=2), 
C                       AND THE RELATIVE HUMIDITY (J=3).
C                       (INTERNAL)
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
C                       100 = GRID CHARACTERISTICS NOT THE SAME FOR 
C                             TWO REQUESTED FIELDS.
C                       103 = IDPARS(1) AND IDPARS(2) DO NOT INDICATE
C                             THETAE.
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
C                       JD() IS USED TO HELP IDENTIFY THE BASIC MODEL
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
C                              MOSTORE(,). LATER USED AS A WAY OF
C                              DETERMINING WHETHER TO KEEP THIS
C                              VARIABLE.
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 WHEN DATA ARE NOT PACKED. (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       0 WHEN DATA ARE NOT PACKED.  (INTERNAL)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE. (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF
C                       THE GRID IS NOT KNOWN BEFORE FDTK AND FDRH
C                       ARE FETCHED.  (INPUT)
C                 ND5 = DIMENSION OF IPACK(), IWORK(), AND EPT().
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
C               NPACK = 2 FOR TDL GRIB PACKED DATA; 1 FOR NOT PACKED
C                       THIS IS RETURNED FROM GFETCH. (INTERNAL)
C              NSLABT = THE NUMBER OF THE SLAB IN DIR(, ,) AND
C                       IN NGRIDC(,) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID. FETCHED FROM TEMPERATURE.
C                       (OUTPUT)
C              NSLABR = THE NUMBER USED TO COMPARE TO NSLABT THAT
C                       IS RETURNED FROM SUBROUTINE GFETCH AFTER
C                       FETCHING THE RELATIVE HUMIDITY AND IS USED
C                       AS A CHECK
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE,
C                       THAT THE RECORD HAS BEEN FETCHED.  THIS IS 
C                       STORED IN LSTORE(9,). (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA().  THIS 
C                       IS RETURNED FROM GFETCH (INTERNAL)
C        1         2         3         4         5         6         7 X
C
      IMPLICIT NONE
C
      REAL, PARAMETER :: ABSZRO=-273.15
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IPACK(ND5),IWORK(ND5)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER NGRIDC(6,ND11)
      INTEGER ICCCFFF(3),MT(4),MR(4),ML(4),MRPARS(15),MLPARS(15)
      INTEGER IER,IJ,ISTAV,J,KFILDO,KFIL10,L3264B,LITEMS,
     1        MISSP,MISSS,MISTOT,MLX,MLY,MTX,MTY,MRX,MRY,
     2        NBLOCK,ND2X3,ND5,ND7,ND9,ND10,ND11,NDATE,NFETCH,
     3        NPACK,NSLABT,NSLABR,NTIMES,NWORDS
C
      REAL THT(ND5)
      REAL FDTK(ND2X3),FDRH(ND2X3)
      REAL CORE(ND10)
      REAL F,TF
      REAL HI0,ADJL,ADJU
C
      DATA ICCCFFF/002003,
     1             002001,
     2             003001/
C
      IER=0
      ISTAV=0
 
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTOR.
C        CCCFFF = 002003.   
 
      IF((IDPARS(1).NE.002).OR.(IDPARS(2).NE.003)) THEN
        WRITE(KFILDO,200)(JD(J),J=1,4)
 200    FORMAT(/,' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1           ' HEAT INDEX PREDICTOR. ',I9.9,2I10.9,I4.3,
     2           ' NOT COMPUTED IN THETAE.')
        IER=103
        GOTO 800
      END IF
 
C        CREATE ID FOR 2-M TEMPERATURE
C
      MT(1)=ICCCFFF(2)*1000+IDPARS(4)
      MT(2)=IDPARS(7)
      MT(3)=IDPARS(9)*1000000+IDPARS(12)
      MT(4)=0
 
C        FETCH FDTK( ), TEMPERATURE.
C
      CALL GFETCH(KFILDO,KFIL10,MT,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDTK,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLABT,MISSP,MISSS,L3264B,
     4            1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)THEN
          WRITE(KFILDO,140)
 140      FORMAT(/' ****ERROR FROM GFETCH OCCURRED FOR',
     1            ' SURFACE TEMPERATURE',
     2            '.  MODTHTX CANNOT RUN, PLEASE READ WRITE UP.')
        GOTO 800
      ENDIF

      MTX=IS2(3)
      MTY=IS2(4)

C        CREATE ID FOR RELATIVE HUMIDITY.
C
      MR(1)=ICCCFFF(3)*1000+IDPARS(4)
      MR(2)=IDPARS(7)
      MR(3)=IDPARS(9)*1000000+IDPARS(12)
      MR(4)=0
 
C        FETCH FDRH( ), THE RELATIVE HUMIDITY.
C
      CALL GFETCH(KFILDO,KFIL10,MR,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FDRH,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLABR,MISSP,MISSS,L3264B,
     4            1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)THEN
          WRITE(KFILDO,141)
 141      FORMAT(/' ****ERROR FROM GFETCH OCCURRED FOR',
     1            ' RELATIVE HUMIDITY',
     2            '.  MODTHTX CANNOT RUN, PLEASE READ WRITE UP.')
        GOTO 800
      ENDIF

      MRX=IS2(3)
      MRY=IS2(4)

      IF(NSLABR.NE.NSLABT.OR.MRX.NE.MTX.OR.MRY.NE.MTY)THEN
        WRITE(KFILDO,400)NSLABT,NSLABR
 400    FORMAT(/' ****THE CHARACTERISTICS OF THE RELATIVE HUMIDITY',
     1          ' GRID ARE DIFFERENT FROM THE TEMPERATURE GRID',
     2          I3,2X,I3,'.',
     3         /'     HEAT INDEX CAN NOT BE COMPUTED IN MODTHTX.')
        IER=100
        GOTO 800
      END IF
C 
C        COMPUTE HEAT INDEX
C
      F=5./9.
      DO 600 IJ=1,(MTX*MTY)
         IF(NINT(FDTK(IJ)).EQ.9999)THEN
            TF=9999.
         ELSE
            TF=(FDTK(IJ)+ABSZRO)/F+32.
         ENDIF
C
         IF((NINT(TF).EQ.9999).OR.(NINT(FDRH(IJ)).EQ.9999))THEN
	    THT(IJ)=9999.
         ELSEIF(TF.LT.70.)THEN
	    THT(IJ)=TF
         ELSE
C
C      CALCULATING THE HEAT INDEX ACCORDING TO A FORMULA
C      GIVEN IN APPENDIX B OF THE NWS INSTRUCTION 10-515.
C      FD1 CONTAINS THE MODEL TEMPERATURE IN DEGREES FAHRENHEIT.
C      FD2 CONTAINS THE MODEL RELATIVE HUMIDITY IN PERCENT.
C
       HI0=-42.379+2.04901523*TF+10.14333127*FDRH(IJ)
     1   -0.22475541*TF*FDRH(IJ)-0.00683783*TF*TF
     2   -0.05481717*FDRH(IJ)*FDRH(IJ)
     3   +0.00122874*TF*TF*FDRH(IJ)+0.00085282*TF*FDRH(IJ)*FDRH(IJ)
     4   -0.00000199*TF*TF*FDRH(IJ)*FDRH(IJ)

       ADJL=0.
       IF((FDRH(IJ) .LT. 13.) .AND. 
     1    (TF .GE. 80. .AND. TF .LE. 112.)) THEN
          ADJL=0.25*(13.-FDRH(IJ))*SQRT(ABS(17.-ABS(TF-95.))/17.)
       ENDIF

       ADJU=0.
       IF((FDRH(IJ) .GT. 85.) .AND. 
     1    (TF .GE. 80. .AND. TF .LE. 87.)) THEN
          ADJU=0.1*(FDRH(IJ)-85.)*0.2*(87.-TF)
       ENDIF

            THT(IJ)=HI0-ADJL+ADJU
	 ENDIF 
C
C         WRITE(*,*)IJ,FDTK(IJ),TF,FDRH(IJ),THT(IJ)
 600  CONTINUE
C
      GOTO 900
 
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
  
 800  DO 801 J=1,ND2X3
        THT(J)=9999.
 801  CONTINUE

 900  RETURN
      END
