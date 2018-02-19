      SUBROUTINE PTP3BIN(KFILDO,KFIL10,IDPARS,JD,NDATE,NSLAB,IPACK,
     1                   IWORK,FD1,ND5,LSTORE,ND9,LITEMS,CORE,ND10,
     2                   NBLOCK,NFETCH,IS0,IS1,IS2,IS4,ND7,ISTAV,
     4                   L3264B,MISTOT,IER)
C
C        FEBRUARY  2005  SCALLION    MDL   CREATED
C        OCTOBER   2005  WIEDENFELD  MDL   MODIFIED NAME OF CODE.
C        MAY       2006  SCALLION    MDL   UPDATED ROUTINE FOLLWING  
C                                          05/12/06 CODE WALKTHROUGH:
C                                          REMOVED EXTRANEOUS VARIABLES
C                                          FROM ROUTINE, CHANGED DATE
C                                          OF PROGRAM CREATION, CHANGED
C                                          DIMENSION OF FD1 TO ND5, 
C                                          UPDATED EXPLANATION OF IER 
C                                          IN VARIABLE DEFINITIONS,
C                                          CHANGED INTEGER DECLARATIONS
C                                          TO DIMENSION, INCREMENTED
C                                          IENTER, IMPROVED LOGIC AROUND
C                                          ERROR CHECKS, CHANGED ND2X3
C                                          TO ND5 IN CALL TO GFETCH,
C                                          CHANGED CASE TO BE IF/ELSEIF
C                                          CLEANED UP COMMENTS AND FIXED
C                                          INDENTATION.
C
C        PURPOSE 
C            TO TAKE THE SEVEN CATEGORIES FOR ADVECTED LAMP PRECIP TYPE
C            AND CONVERT THEM INTO THREE BINARY PREDICTORS FOR OCCURRENCE
C            OF FREEZING, FROZEN AND LIQUID.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               008 551 - FREZ. PRECIP BIN.
C               008 552 - FROZ. PRECIP BIN.
C               008 553 - LIQ. PRECIP BIN.
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                     (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                     (INPUT-OUTPUT)
C 
C        VARIABLES 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                       (INPUT)
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                       (INPUT)
C           IDPARS(J) = PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID(J) (J=1,15) DEFINED IN
C                       THE CALLING PROGRAM (INPUT)
C                       J=1  -- CCC (CLASS OF VARIABLE)
C                       J=2  -- FFF (SUBCLASS OF VARIABLE)
C                       J=3  -- B (BINARY INDICATOR)
C                       J=4  -- DD (DATA SOURCE, MODEL NUMBER)
C                       J=5  -- V (VERTICAL APPLICATION)
C                       J=6  -- LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1
C                               LAYER)
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
C               NDATE = DATE/TIME FOR WHICH THE PREDICTOR IS NEEDED
C                       (INPUT)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND IN
C                       NGRIDC( , ) DEFINING THE CHARACTERISTICS OF THE
C                       GRID.  IT IS USED TO IDENTIFY THE DATA SOURCE,
C                       I.E., THE MODEL. (OUTPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5) (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5) (INTERNAL)
C              FD1(J) = WORK ARRAY TO HOLD LAMP PTYPE (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND FDDP( ).
C                       (INPUT)
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
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ) (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN (INPUT)
C             CORE(J) = ARRAY TO STORE OR RETRIEVE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL,
C                       DATA ARE STORED ON DISK.  UPON RETURN TO THE
C                       CALLING PROGRAM, THE ARRAY WILL BE IN THE SIZE
C                       OF LX*LY (OUTPUT).
C                ND10 = DIMENSION OF CORE( ) (INPUT)
C              NBLOCK = BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM DISK
C                       FILE (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.  IT IS
C                       A RUNNING COUNT FROM THE BEGINNING OF THE MAIN
C                       PROGRAM.  THIS COUNT IS MAINTAINED IN CASE THE
C                       USER NEEDS IT FOR DIAGNOSTICS, ETC. (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 IDS (J=1,3) (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 IDS (J=1,22+) (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 IDS (J=1,12)
C                       IS2(3) AND IS2(4) ARE USED BY THE CALLING
C                       PROGRAM AS GRID DIMENSION (INTERNAL-OUTPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 IDS (J=1,4) (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED (INPUT)
C               ISTAV = 0 -- WHEN THE DATA RETURNED ARE GRID DATA OR
C                            DATA ARE NOT AVAILABLE FOR RETURN
C                       1 -- WHEN THE DATA RETURNED ARE STATION DATA
C                       (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS DEPENDING ON THE
C                       MACHINE BEING USED (EITHER 32 OR 64) (INPUT)
C              MISTOT = TOTAL NUMBER OF MISSING ITEMS ENCOUNTED IN
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                       102 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED
C                             IN THIS SUBROUTINE
C                       SEE GFETCH FOR OTHER VALUES WHEN IER.NE.0 AND
C                       DATA ARE RETURNED AS MISSING
C                       (INTERNAL-OUTPUT)
C                       UNPACKING GRIDS (INPUT-OUTPUT)
C
C         ADDITIONAL VARIABLES
C                 I,J = LOOP COUNT (INTERNAL)
C               LX,LY = DIMENSIONS OF THE GRID RETURNED FROM CALLING
C                       GFETCH FOR PTYPE (INTERNAL)
C               MD(J) = WORK ARRAY HOLDING THE 4 ID WORDS OF PTYPE
C                       DATA BEING RETRIEVED (J=1,4) (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       ZERO FROM CALLING GFETCH WHEN THERE IS NO
C                       MISSING VALUE (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       ZERO FROM CALLING GFETCH WHEN THERE IS NO
C                       SECONDARY MISSING VALUE (INTERNAL)
C               NPACK = 2 FOR TDL GRIB PACKED DATA; 1 FOR NOT PACKED.
C                       THIS IS RETURNED FROM CALLING GFETCH (INTERNAL)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT
C                       THE RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ). (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN FD1( ) AND FD2( ).
C                       THIS IS RETURNED FROM CALLING GFETCH. (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NON-SYSTEM SUBROUTINES USED
C            GFETCH
C
      PARAMETER(NDIM=3)
C
      DIMENSION IDPARS(15),JD(4),MD(4)
      DIMENSION FD1(ND5)
      DIMENSION IPACK(ND5),IWORK(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      INTEGER   ITEMP
      INTEGER   I,IER,ISTAV,JKFIL10,KFILDO,L3264B,LITEMS,LX,LY,MISSP,
     1          MISSS,MISTOT,NBLOCK,ND2X3,ND5,ND7,ND9,ND10,ND11,
     2          NDATE,NFETCH,NPACK,NSLAB,NTIMES,NWORDS
C
      DIMENSION CORE(ND10)
      DIMENSION ITABLE(2,NDIM)
      DATA IENTER/0/
C
      DATA ITABLE/008551,008501005,
     1            008552,008501005,
     2            008553,008501005/
C
      IER=0
      ISTAV=0
      IENTER=IENTER+1
C
C        CHECK IF THIS CODE ACCOMMODATES LAMP PTYPE BIN
C       
      DO 105 JJ=1,NDIM 
         IF((ITABLE(1,JJ).EQ.IDPARS(1)*1000+IDPARS(2))) THEN
            GOTO 400
         ENDIF
 105  CONTINUE
C
      WRITE(KFILDO,107)(JD(L),L=1,4)
 107  FORMAT(/,' ****PTP3BIN ENTERED FOR VARIABLE',
     1        2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2         ' NOT ACCOMMODATED.')
      IER=102
      GO TO 800
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        CONSTRUCT THE LAMP ID FOR PTYPE.  DATA WILL BE RETURNED
C        IN FD1( ).
C
 400  MD(1)=ITABLE(2,JJ)
      MD(2)=0
      MD(3)=IDPARS(12)
      MD(4)=0
C
      CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND5,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
C
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      LX=IS2(3)
      LY=IS2(4)
C
C
      IF((IER.NE.0).AND.(IENTER.GE.1)) THEN
         IF(IENTER.EQ.1) THEN
            WRITE(KFILDO,405) IER,(MD(J),J=1,4)
 405        FORMAT(/,' ****ERROR IN PTP3BIN, FOR FIRST PROCESS DATE,',
     1               ' LAMP PTYPE IS MISSING',
     2             /,' POSSIBLE DATA GAP, **** ALL OUTPUT VALUES',
     3               ' ARE SET TO MISSING ****, IER =',I5,
     4               ' FOR VARIABLE',4I12)
         ENDIF
         GO TO 800
      ENDIF
C
C         IF THE ID IS 008551, COMPUTE A FREEZING/NO FREEZING BINARY;
C         008552, A SNOW/NO SNOW BINARY; AND 008553 A RAIN/NO RAIN
C         BINARY.  THEN RETURN THE APPROPRIATE VALUE OF FD1 TO
C         U201.
C
C        FREEZING
C
       IF(IDPARS(2).EQ.551) THEN
          DO 500 J=1,LX*LY
             ITEMP=NINT(FD1(J))
             IF(ITEMP.EQ.1.OR.ITEMP.EQ.2.OR.ITEMP.EQ.3) THEN
                FD1(J)=1.
             ELSE
                FD1(J)=0.
             ENDIF
 500      CONTINUE
          GO TO 920 
C
C       FROZEN
C
       ELSEIF(IDPARS(2).EQ.552)THEN
          DO 505 J=1,LX*LY
             ITEMP=NINT(FD1(J))
             IF(ITEMP.EQ.4) THEN
                FD1(J)=1.
             ELSE
                FD1(J)=0.
             ENDIF
 505      CONTINUE
          GO TO 920 
C
C       LIQUID
C
       ELSEIF(IDPARS(2).EQ.553)THEN
          DO 510 J=1,LX*LY
             ITEMP=NINT(FD1(J))
             IF(ITEMP.EQ.5.OR.ITEMP.EQ.6.OR.ITEMP.EQ.7) THEN
                FD1(J)=1.
             ELSE
                FD1(J)=0.
             ENDIF
 510      CONTINUE
          GO TO 920 
C
       ENDIF
C
C        FILL IN THE LAMP PTYPE BIN ARRAY WITH MISSING VALUES
C
 800  DO 810 J=1,ND2X3
         FD1(J)=9999.
 810  CONTINUE
C
 920  RETURN
      END      
