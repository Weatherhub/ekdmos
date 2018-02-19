      SUBROUTINE PACKG(KFILDO,KFILGO,ID,IDPARS,
     1                 ISCALD,ISCALE,NGRIDC,
     2                 IPLAIN,PLAIN,NDATE,NYR,NMO,NDA,NHR,
     3                 FD1,DATA,ND2X3,NX,NY,IPACK,IWORK,ND5,MINPK,
     4                 IS0,IS1,IS2,IS4,ND7,XMISSP,XMISSS,
     5                 NWORDS,NTOTBG,NTOTRG,
     6                 L3264B,L3264W,ISTOP,IER)
C
C        DECEMBER  2000   GLAHN   TDL   MOS-2000
C                                 MODIFIED FROM PACKV
C        FEBRUARY  2001   GLAHN   CORRECTED IS1(2) FOR GRIDDED DATA
C        JUNE      2002   GLAHN   ADDED MERCATOR TO COMMENTS
C        NOVEMBER  2002   GLAHN   CHANGED ND1 TO ND2X3 AND ISDATA TO
C                                 FD1; ADDED COMMENT
C        OCTOBER   2004   GLAHN   SPELL CHECK; REMOVED COMMENT 
C                                 IMMEDIATELY AFTER CALL TO PACK2D;
C                                 PURPOSE MODIFIED; ADDED PRINT OF
C                                 BYTES AND RECORDS WRITTEN FOR /D
C        JUNE      2012   ENGLE   MODIFIED CALL TO INCLUDE PLAIN.
C                                 PLAIN LANGUAGE IS NOW PACKED BY
C                                 USING IACHAR FUNCTION.
C
C        PURPOSE
C           TO PACK AND WRITE GRIDPOINT DATA TO A MOS-2000 SEQUENTIAL
C           FILE.  PACKED DATA ARE WRITTEN UNLESS KFILGO = 0 OR THERE
C           IS AN ERROR.  THE "MODEL NUMBER" IS ELIMINATED FOR WRITING
C           FOR "CONSTANT" VARIABLES WITH CCC IN THE RANGE 400 TO 499.
C   
C        DATA SET USE
C            KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C            KFILGO - UNIT NUMBER OF TDLPACK OUTPUT FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              KFILGO = UNIT NUMBER OF TDLPACK OUTPUT FILE.  (INPUT)
C               ID(J) = THE INTEGER VARIABLE ID (J=1,4)  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       VARIABLE
C                       ID CORRESPONDING TO ID( ) (J=1,15).  (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY
C                            1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK
C                            IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C              ISCALD = THE DECIMAL SCALING CONSTANT TO USE WHEN
C                       PACKING THE DATA.  (INPUT)
C              ISCALE = THE BINARY SCALING CONSTANT TO USE WHEN
C                       PACKING THE DATA.  NORMALLY, THIS IS 0.  (INPUT)
C           NGRIDC(L) = HOLDS THE GRID CHARACTERISTICS (L=1,6).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=POLAR
C                            STEREOGRAPHIC, 7=MERCATOR). 
C                       L=2--GRID LENGTH IN METERS,
C                       L=3--LATITUDE AT WHICH GRID LENGTH IS CORRECT,
C                       L=4--GRID ORIENTATION IN DEGREES, AND
C                       L=5--LATITUDE OF LL CORNER IN DEGREES,
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES,
C               PLAIN = THE PLAIN LANGUAGE DESCRIPTION OF THE PREDICTORS
C                       EQUIVALENCED TO IPLAIN( , ). (CHARACTER*32)
C         IPLAIN(L,J) = 32 CHARACTERS (L=1,L3264W) (J=1,4) OF PLAIN
C                       LANGUAGE DESCRIPTION OF VARIABLE.
C                       NOTE THAT THIS REQUIRES TWO 32-BIT WORDS TO HOLD
C                       THE DESCRIPTION BUT ONLY ONE 64-BIT WORD.
C                       (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH VARIABLES ARE BEING
C                       DEALT WITH.  (INPUT)
C                 NYR = YEAR, 4 DIGITS.  (INPUT)
C                 NMO = MONTH.  (INPUT)
C                 NDA = DAY OF MONTH.  (INPUT)
C                 NHR = HOUR, 2 DIGITS.  (INPUT)
C                  NX = THE X EXTENT OF THE GRID.  (INPUT)
C                  NY = THE Y EXTENT OF THE GRID.  (INPUT)
C              FD1(K) = USED IN PACK2D (K=1,ND2X3).  (INTERNAL)
C             DATA(K) = GRIDPOINT DATA FOR WRITING (K=1,ND2X3).  (INPUT)
C               ND2X3 = DIMENSION OF FD1( ) AND DATA( ).
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ).  GE ND2X3 IN CALLING
C                       PROGRAM.  (INPUT)
C               MINPK = MINIMUM GROUP SIZE WHEN PACKING THE VALUES.
C                       (INPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,ND7).
C                       (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,ND7).
C                       (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,ND7).
C                       (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,ND7). 
C                       (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C              XMISSP = PRIMARY MISSING VALUE.  (INPUT)
C              XMISSP = SECONDARY MISSING VALUE.  (INPUT)
C              NWORDS = THE NUMBER OF WORDS IN IPACK( ), CALCULATED
C                       FROM IOCTET.  (OUTPUT)
C              NTOTBG = THE TOTAL NUMBER OF BYTES ON THE FILE ASSOCIATED
C                       WITH UNIT NO. KFILGO (THE TDLPACK FILE).
C                       IT IS UPDATED WHEN THE DATA IN IPACK( ) ARE
C                       WRITTEN.  (INPUT-OUTPUT)
C              NTOTRG = THE TOTAL NUMBER OF RECORDS ON THE TAPE.  IT IS
C                       UPDATED AS NEEDED IN WRITEP.  (INPUT-OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).  
C                       (INPUT)
C               ISTOP = INCREMENTED BY ONE EACH TIME AN ERROR IS 
C                       ENCOUNTERED.  (INPUT-OUTPUT)
C                 IER = STATUS RETURN.
C                        0 = GOOD RETURN.
C                       16 = ND7 NOT LARGE ENOUGH.  SET ND7 GE 54.
C                       SEE ROUTINES PACK2D, UNPKBG, AND WRITEP 
C                       FOR OTHER VALUES.  (INTERNAL-OUTPUT)
C              IOCTET = THE PACKED RECORD SIZE IN OCTETS (BYTES).
C                       PROVIDED BY PACK2D.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES USED 
C            TIMPR, UNPKBG, PACK2D, WRITEP
C
      CHARACTER*32 PLAIN
      DIMENSION FD1(ND2X3),DATA(ND2X3)
      DIMENSION IPACK(ND5),IWORK(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION IPLAIN(L3264W,4)
      DIMENSION ID(4),IDPARS(15),NGRIDC(6)
C
      IER=0
D     WRITE(KFILDO,100)ID
D100  FORMAT(/' **** ENTERING PACKG, ID( ) ='4I12)
C        PACK THE DATA.  ELIMINATE THE "MODEL NUMBER" FROM 
C        THE "CONSTANT" VARIABLES.
C
D     WRITE(KFILDO,110)KFILGO,ND2X3,NX,NY,XMISSP,XMISSS,ND5,MINPK,L3264B
D110  FORMAT(/' KFILGO,ND2X3,NX,NY,XMISSP,XMISSS,ND5,MINPK,L3264B',
D    1          4I9,2F8.1,3I7)
      IDPAW4=IDPARS(4)
      IDW1=ID(1)
      IF(IDPARS(1).LT.400.OR.IDPARS(1).GT.499)GO TO 370
      IDPAW4=0
      IDW1=IDPARS(1)*1000000+IDPARS(2)*1000+IDPARS(3)*100
C
C        LOAD IS1( ) FOR PACKING.

 370  IS1(2)=1
C        IS1(2) = 1 SIGNIFIES GRIDDED DATA.
      IS1(3)=NYR
      IS1(4)=NMO
      IS1(5)=NDA
      IS1(6)=NHR
      IS1(7)=0
      IS1(8)=NDATE
      IS1(9)=IDW1
      IS1(10)=ID(2)
      IS1(11)=ID(3)
      IS1(12)=ID(4)
      IS1(13)=IDPARS(12)
      IS1(14)=0
      IS1(15)=IDPAW4
      IS1(16)=0
      IS1(17)=ISCALD
      IS1(18)=ISCALE
      IS1(19)=0
      IS1(20)=0
      IS1(21)=0
      IS1(22)=32
      IF(ND7.GE.54)GO TO 375
C        IS1( ) WILL BE OVERFLOWED IN NEXT SECTION.
      WRITE(KFILDO,374)
 374  FORMAT(/' ****IS1( ) IN UNPACK ABOUT TO BE OVERFLOWED.',
     1        '  INCREASE ND7 TO 54.')
      IER=16
      ISTOP=ISTOP+1
      GO TO 400
C
 375  LOC=1
C        LOC = WORD POSITION IN IPLAIN(1,1) TO START UNPACKING.
C        UNPKBG UPDATES IT.
      IPOS=1
C        IPOS = BIT POSITION IN IPLAIN(1,1) TO START UNPACKING.
C        UNPKBG UPDATES IT.
C
CINTEL
C
C        USE IACHAR FUNCTION TO PUT ONE CHARACTER (BYTE) FROM
C        PLAIN INTO ONE IS1( ) WORD.
C
      DO J=1, 32
         IS1(J+22)=IACHAR(PLAIN(J:J))
      END DO
C      DO 380 J=1,32
C      CALL UNPKBG(KFILDO,IPLAIN(1,1),4*L3264W,LOC,IPOS,
C     1            IS1(J+22),8,L3264B,IER,*900)
CC        NOTE THAT THIS PUTS ONE BYTE PER IS1( ) WORD.
CC        RETURN HERE ONLY WHEN IER = 0; OTHERWISE, TO 900.
C 380  CONTINUE
CINTEL
C
C        LOAD IS2( ) FOR PACKING.  ALL OTHER VALUES ARE PROVIDED
C        BY THE PACKING ROUTINES.
C
      IS2(2)=NGRIDC(1)
      IS2(3)=NX
      IS2(4)=NY
      IS2(5)=NGRIDC(5)
      IS2(6)=NGRIDC(6)
      IS2(7)=NGRIDC(4)
      IS2(8)=NGRIDC(2)
      IS2(9)=NGRIDC(3)
C
      CALL PACK2D(KFILDO,DATA,FD1,IWORK,NX,NY,IS0,IS1,IS2,IS4,
     1            ND7,XMISSP,XMISSS,IPACK,ND5,
     2            MINPK,LX,IOCTET,L3264B,IER)
      IF(IER.NE.0)ISTOP=ISTOP+1
      IF(IER.NE.0)GO TO 400
C
C        WRITE THE PACKED DATA.
C
      NWORDS=IOCTET*8/L3264B
C        NWORDS IS USED AS THE DIMENSION OF IPACK( ) IN WRITEP.
C        IOCTET IS RETURNED FROM PACK2D AS EVENLY DIVISIBLE BY 8
C        BY PADDING IPACK( ) IF NECESSARY.  THEREFORE, TRUNCATION
C        DOES NOT OCCUR IN COMPUTATION OF NWORDS.
C
      IF(KFILGO.EQ.0)GO TO 400
C        DO NOT WRITE WHEN KFILGO = 0.
      CALL WRITEP(KFILDO,KFILGO,IPACK,NWORDS,NTOTBG,NTOTRG,
     1            L3264B,IER)
C
      IF(IER.EQ.0)THEN
D        WRITE(KFILDO,158)KFILGO,NTOTBG,NTOTRG
D158     FORMAT(/' WRITING DATA IN PACKG ON UNIT NO.',I4/
D    1        '    NTOTBG ='I10  '   TOTAL BYTES   WRITTEN TO',
D    2                           ' SEQUENTIAL FILE'/
D    3        '    NTOTRG ='I10  '   TOTAL RECORDS WRITTEN TO',
D    4                           ' SEQUENTIAL FILE')
      ELSE
         ISTOP=ISTOP+1
      ENDIF
C
 400  CONTINUE
      RETURN
C
 900  WRITE(KFILDO,901)IER
 901  FORMAT(/,' ****ERROR IN UNPKBG IN PACKG, IER =',I4)
      ISTOP=ISTOP+1
      RETURN
      END
