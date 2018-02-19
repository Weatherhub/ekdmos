      SUBROUTINE LMPTDPB(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                   ND11,NSLAB,IPACK,IWORK,DATA,ND5,LSTORE,
     2                   ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                   IS0,IS1,IS2,IS4,ND7,ISTAV,L3264B,
     4                   MISTOT,IER)
C
C        DECEMBER  2005  SCALLION      MDL   MOS-2000
C        JUNE      2006  SCALLION      CLEANED UP CODE.
C        JUNE      2006  SCALLION      UPDATED ROUTINE FOLLOWING 6/7/2006
C                                      WALKTHROUGH: UPDATED DOCUMENTATION,
C                                      CHANGED FD1 TO DATA, CHANGED IER=102
C                                      TO 103, INCREMENTED AND SAVED IENTER,
C                                      IMPROVED LOGIC AROUND ERROR CHECK,
C                                      CHANGED IDPARS TO MOD(ITABLE(1,JJ),
C                                      1000) IN ID CHECK, USED NINT IN
C                                      EQUIVALENCE CHECK, DIMENSIONED
C                                      'DO 800' LOOP USING ND5.
C
C        PURPOSE
C            TO MAKE QUASI-BINARY PREDICTORS OUT OF CLAM TEMPERATURE AND
C            WHICH REPRESENT THE HOW LIKELY PRECIPITATION WOULD FALL AS
C            EITHER FROZEN (SNOW) OR FREEZING (SLEET/FREEZING RAIN)
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               002 305 - LAMP TEMPERATURE BIN. (FRZN)
C               002 306 - LAMP TEMPERATURE BIN. (FRZG)
C               003 305 - LAMP DEWPOINT BIN. (FRZN)
C               003 306 - LAMP DEWPOINT BIN. (FRZG)
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                     (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                     (INPUT-OUTPUT)
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                       (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
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
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN BE
C                       DEALT WITH IN THIS RUN.  LAST DIMENSION OF
C                       NGRIDC( , ) (INPUT)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND IN
C                       NGRIDC( , ) DEFINING THE CHARACTERISTICS OF THE
C                       GRID.  IT IS USED TO IDENTIFY THE DATA SOURCE,
C                       I.E., THE MODEL. (OUTPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5) (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5) (INTERNAL)
C             DATA(J) = DATA TO RETURN (J=1,ND5) (INTERNAL)
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
C                       UNPACKING GRIDS (INPUT-OUTPUT)
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                       103 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED
C                             IN THIS SUBROUTINE
C                       SEE GFETCH FOR OTHER VALUES WHEN IER.NE.0 AND
C                       DATA ARE RETURNED AS MISSING
C                       (INTERNAL-OUTPUT)
C
C         ADDITIONAL VARIABLES
C               LX,LY = DIMENSIONS OF THE GRID RETURNED FROM CALLING
C                       GFETCH FOR PTYPE (INTERNAL)
C               MD(J) = WORK ARRAY HOLDING THE 4 ID WORDS OF CLAM
C                       TDP DATA BEING RETRIEVED (J=1,4) (INTERNAL)
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
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA( ).
C                       THIS IS RETURNED FROM CALLING GFETCH. (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NON-SYSTEM SUBROUTINES USED
C            GFETCH
C
      PARAMETER(NDIM=4)
C
      DIMENSION IDPARS(15),JD(4),MD(4)
      DIMENSION DATA(ND5)
      DIMENSION IPACK(ND5),IWORK(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      INTEGER I,IER,ISO,ISTAV,J,KFIL10,KFILDO,L3264B,LITEMS,LX,LY,MISSP,
     +        MISSS,MISTOT,NBLOCK,ND5,ND7,ND9,ND10,ND11,
     +        NDATE,NFETCH,NPACK,NSLAB,NTIMES,NWORDS
C
      DIMENSION ITABLE(2,NDIM)
      DATA IENTER/0/
      SAVE IENTER
C
      DATA ITABLE/002305,002301,
     1            002306,002301,
     2            003305,003301,
     3            003306,003301/
C
      IER=0
      ISTAV=0
C
      IENTER=IENTER+1
C
C        CHECK IF THIS CODE ACCOMMODATES LAMP TDP BIN
C       
      DO 105 JJ=1,NDIM 
         IF((ITABLE(1,JJ).EQ.IDPARS(1)*1000+IDPARS(2))) THEN
            GOTO 400
         ENDIF
 105  CONTINUE
C
      WRITE(KFILDO,107)(JD(L),L=1,4)
 107  FORMAT(/,' ****LMPTDPB ENTERED FOR VARIABLE',
     1         2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2       ' NOT ACCOMMODATED.')
      IER=103
      GO TO 800
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        CONSTRUCT THE ID FOR CLAM TEMPERATURE OR DEWPOINT.  
C        DATA WILL BE RETURNED IN DATA( ).
C
 400  MD(1)=ITABLE(2,JJ)*1000 +IDPARS(4)
      MD(2)=IDPARS(7)
      MD(3)=IDPARS(12)
      MD(4)=0
C
      CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,DATA,ND5,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
C
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      LX=IS2(3)
      LY=IS2(4)
C
      IF((IER.NE.0).AND.(IENTER.GE.1)) THEN
         IF(IENTER.EQ.1) THEN
            WRITE(KFILDO,405) IER,(MD(J),J=1,4)
 405        FORMAT(/,' ****ERROR IN LMPTDPB, FOR FIRST PROCESS DATE,',
     1               ' CLAM TEMPERATURE OR DEWPOINT IS MISSING',
     2             /,' POSSIBLE DATA GAP, **** ALL OUTPUT VALUES',
     3               ' ARE SET TO MISSING ****, IER =',I5,
     4               ' FOR VARIABLE',4I12)
         ENDIF
         GO TO 800
      ENDIF
C
C         CHECK IF DEALING WITH FROZEN OR FREEZING.  THEN, ASSIGN
C         THE APPROPRIATE VALUE.  IF CLAM TEMP/DEWPT IS MISSING, SET
C         THE PREDICTOR TO MISSING
C
C             FOR FROZEN PRECIP
C
      IF(MOD(ITABLE(1,JJ),1000).EQ.305)THEN
      DO 500 J=1,LX*LY
         IF(DATA(J).LE.32.)THEN
            DATA(J)=1.
         ELSEIF(DATA(J).GT.32.AND.DATA(J).LT.35) THEN
            DATA(J)=0.75
         ELSEIF(DATA(J).GE.35.AND.DATA(J).LT.38) THEN
            DATA(J)=0.50
         ELSEIF(DATA(J).GE.38.AND.DATA(J).LT.45) THEN
            DATA(J)=0.25
         ELSEIF(NINT(DATA(J)).NE.9999.) THEN
            DATA(J)=0.
         ELSE
            DATA(J)=9999.
         ENDIF
 500  CONTINUE
      GO TO 920
C
C             FOR FREEZING PRECIP
C
      ELSEIF(MOD(ITABLE(1,JJ),1000).EQ.306) THEN
      DO 550 J=1,LX*LY
         IF(DATA(J).LT.10.) THEN
            DATA(J)=0.
         ELSEIF(DATA(J).GE.10.AND.DATA(J).LT.20) THEN
            DATA(J)=0.25
         ELSEIF(DATA(J).GE.20.AND.DATA(J).LT.25) THEN
            DATA(J)=0.50
         ELSEIF(DATA(J).GE.25.AND.DATA(J).LT.30) THEN
            DATA(J)=0.75
         ELSEIF(DATA(J).GE.30.AND.DATA(J).LT.33) THEN
            DATA(J)=1.
         ELSEIF(DATA(J).GE.33.AND.DATA(J).LT.35) THEN
            DATA(J)=0.75
         ELSEIF(DATA(J).GE.35.AND.DATA(J).LT.37) THEN
            DATA(J)=0.50
         ELSEIF(DATA(J).GE.37.AND.DATA(J).LT.45) THEN
            DATA(J)=0.25
         ELSEIF(NINT(DATA(J)).NE.9999.) THEN
            DATA(J)=0.
         ELSE
            DATA(J)=9999.
         ENDIF
 550  CONTINUE
      GO TO 920 
C
      ENDIF
C
C        FILL IN THE LAMP WIND DIR BIN ARRAY WITH MISSING VALUES
C
 800  DO 810 J=1,ND5
        DATA(J)=9999.
 810  CONTINUE
C
 920  RETURN
      END      
