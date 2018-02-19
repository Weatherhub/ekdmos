      SUBROUTINE OBSTDPB(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                   SDATA,ND1,NSTA,IPACK,IWORK,FD1,ND2X3,
     2                   LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                   IS0,IS1,IS2,IS4,ND7,ISTAV,L3264B,IER)
C
C        DECEMBER  2005  SCALLION       MDL   MOS-2000
C        JUNE      2006  SCALLION       CLEANED UP CODE.  
C        JUNE      2006  SCALLION       UPDATED ROUTINE FOLLOWING 6/7/2006
C                                       WALKTHROUGH: UPDATED PURPOSE AND 
C                                       DOCUMENTATION, INCREMENTED AND
C                                       SAVED IENTER, CHANGED IER=102 TO
C                                       103, IMPROVED LOGIC AROUND ERROR
C                                       CHECK, CHANGED IDPARS TO
C                                       MOD(ITABLE(1,JJ),1000) IN ID CHECK, 
C                                       USED NINT IN EQUIVALENCE CHECK, 
C                                       DIMENSIONED 'DO 800' USING ND1.
C
C        PURPOSE
C            TO MAKE QUASI-BINARY PREDICTORS OUT OF OBSERVED
C            TEMPERATURE AND DEWPOINT, WHICH REPRESENT HOW LIKELY
C            PRECIPITATION WOULD FALL AS EITHER FROZEN (SNOW) OR
C            FREEZING (SLEET/FREEZING RAIN)
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               702 502 - OBS WEIGHTED TEMPERATURE BIN. (FRZN)
C               702 503 - OBS WEIGHTED TEMPERATURE BIN. (FRZG)
C               703 502 - OBS WEIGHTED DEWPOINT BIN. (FRZN)
C               703 503 - OBS WEIGHTED DEWPOINT BIN. (FRZG)
C
C        DATA SET USE 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                     (INPUT-OUTPUT)
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                     (OUTPUT) 
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
C            SDATA(K) = DATA TO RETURN (K=1,ND1) (OUTPUT).
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                       WITH.  DIMENSION OF SDATA (INPUT).
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING
C                       DEALT WITH (INPUT).
C            IPACK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL)
C              FD1(J) = WORK ARRAY TO HOLD OBS TDP (J=1,ND1) (INTERNAL)
C               ND2X3 = DIMENSION OF FD1( ) (INPUT)
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
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                       103 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED
C                             IN THIS SUBROUTINE
C                       SEE GFETCH FOR OTHER VALUES WHEN IER.NE.0 AND
C                       DATA ARE RETURNED AS MISSING
C                       (INTERNAL-OUTPUT)
C
C         ADDITIONAL VARIABLES
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
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND IN
C                       NGRIDC( , ) DEFINING THE CHARACTERISTICS OF THE
C                       GRID.  IT IS USED TO IDENTIFY THE DATA SOURCE,
C                       I.E., THE MODEL. (OUTPUT)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT
C                       THE RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ). (INTERNAL)
C              NWORDS = NUMBER OF WORDS RETURNED IN FD1( ).
C                       THIS IS RETURNED FROM CALLING GFETCH. (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NON-SYSTEM SUBROUTINES USED
C            GFETCH
C
      PARAMETER(NDIM=4)
C
      DIMENSION IDPARS(15),JD(4),MD(4)
      DIMENSION FD1(ND2X3),SDATA(ND1)
      DIMENSION IPACK(ND2X3),IWORK(ND2X3)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
C
      DIMENSION ITABLE(2,NDIM)
      DATA IENTER/0/
      SAVE IENTER
C
      DATA ITABLE/702502,702000,
     1            702503,702000,
     2            703502,703100,
     3            703503,703100/
C
      IER=0
      ISTAV=1
C
      IENTER=IENTER+1
C
C        CHECK IF THIS CODE ACCOMMODATES OBS TDP BIN
C       
      DO 105 JJ=1,NDIM 
         IF((ITABLE(1,JJ).EQ.IDPARS(1)*1000+IDPARS(2))) THEN
            GOTO 400
         ENDIF
 105  CONTINUE
C
      WRITE(KFILDO,107)(JD(L),L=1,4)
 107  FORMAT(/,' ****OBSTDPB ENTERED FOR VARIABLE',
     1         2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2       ' NOT ACCOMMODATED.')
      IER=103
      GO TO 800
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        CONSTRUCT THE ID FOR OBS TEMPERATURE OR DEWPOINT.  DATA WILL BE
C        RETURNED IN FD1( ).  ALTHOUGH ND2X3 IS USED TO DIMENSION ARRAY,
C        THE DATA WILL BE IN VECTOR FORMAT.
C
 400  MD(1)=ITABLE(2,JJ)*1000 +IDPARS(4)
      MD(2)=IDPARS(7)
      MD(3)=IDPARS(9)*1000000+IDPARS(12)
      MD(4)=0
C
      CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
C
      IF((IER.NE.0).AND.(IENTER.GE.1)) THEN
         IF(IENTER.EQ.1) THEN
            WRITE(KFILDO,405) IER,(MD(J),J=1,4)
 405        FORMAT(/,' ****ERROR IN OBSTDPB, FOR FIRST PROCESS DATE,',
     1               ' OBS TEMPERATURE OR DEWPOINT IS MISSING',
     2             /,' POSSIBLE DATA GAP, **** ALL OUTPUT VALUES',
     3               ' ARE SET TO MISSING ****, IER =',I5,
     4               ' FOR VARIABLE',4I12)
         ENDIF
         GO TO 800
      ENDIF
C
C         CHECK IF DEALING WITH FROZEN OR FREEZING.  THEN, ASSIGN
C         THE APPROPRIATE VALUE.  IF OBS TEMP/DEWPT IS MISSING, SET
C         THE PREDICTOR TO MISSING
C
C             FOR FROZEN PRECIP
C
      IF(MOD(ITABLE(1,JJ),1000).EQ.502)THEN
         DO 500 J=1,NSTA
            IF(FD1(J).LE.32.)THEN
               SDATA(J)=1.
            ELSEIF(FD1(J).GT.32.AND.FD1(J).LT.35) THEN
               SDATA(J)=0.75
            ELSEIF(FD1(J).GE.35.AND.FD1(J).LT.38) THEN
               SDATA(J)=0.50
            ELSEIF(FD1(J).GE.38.AND.FD1(J).LT.45) THEN
               SDATA(J)=0.25
            ELSEIF(NINT(FD1(J)).NE.9999.) THEN
               SDATA(J)=0.
            ELSE
               SDATA(J)=FD1(J)
            ENDIF
  500    CONTINUE
      GO TO 920
C
C             FOR FREEZING PRECIP
C
      ELSEIF(MOD(ITABLE(1,JJ),1000).EQ.503)THEN
         DO 550 J=1,NSTA
            IF(FD1(J).LT.10.) THEN
               SDATA(J)=0.
            ELSEIF(FD1(J).GE.10.AND.FD1(J).LT.20) THEN
               SDATA(J)=0.25
            ELSEIF(FD1(J).GE.20.AND.FD1(J).LT.25) THEN
               SDATA(J)=0.50
            ELSEIF(FD1(J).GE.25.AND.FD1(J).LT.30) THEN
               SDATA(J)=0.75
            ELSEIF(FD1(J).GE.30.AND.FD1(J).LT.33) THEN
               SDATA(J)=1.
            ELSEIF(FD1(J).GE.33.AND.FD1(J).LT.35) THEN
               SDATA(J)=0.75
            ELSEIF(FD1(J).GE.35.AND.FD1(J).LT.37) THEN
               SDATA(J)=0.50
            ELSEIF(FD1(J).GE.37.AND.FD1(J).LT.45) THEN
               SDATA(J)=0.25
            ELSEIF(NINT(FD1(J)).NE.9999.) THEN
               SDATA(J)=0.
            ELSE
               SDATA(J)=FD1(J)
            ENDIF
 550     CONTINUE
      GO TO 920
C
      END IF
C
C        FILL IN THE OBS TDP BIN ARRAY WITH MISSING VALUES
C
 800  DO 810 J=1,ND1
         SDATA(J)=9999.
 810  CONTINUE
C
 920  RETURN
      END      
