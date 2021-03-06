      SUBROUTINE CONST(KFILDO,KFIL10,IP12,
     1                 ID,IDPARS,JD,NDATE,
     2                 KFILRA,RACESS,NUMRA,
     3                 CCALL,ICALLD,CCALLD,
     4                 ISDATA,SDATA,ND1,NSTA,
     5                 IPACK,IWORK,DATA,ND5,
     6                 LSTORE,ND9,LITEMS,CORE,ND10,LASTL,
     7                 NBLOCK,LASTD,NSTORE,NFETCH,
     8                 IS0,IS1,IS2,IS4,ND7,
     9                 ISTAV,L3264B,L3264W,IER)
C 
C        NOVEMBER  1996   GLAHN   TDL   MOS-2000 
C        JUNE      1997   GLAHN   SLIGHT CHANGE TO COMMENTS, NOT CODE
C                                 D COMPILE OPTION REMOVED OR COMMENTED OUT
C        JANUARY   1998   GLAHN   ADDED SUBSTITUTE STATIONS IN CCALL( , );
C                                 MODIFIED DIAGNOSTICS
C        MAY       1998   GLAHN   ADDED ITIMEZ( ) 
C        SEPTEMBER 1998   GLAHN   MODIFIED PRINT WHEN NUMRA = 0 AT 111
C        OCTOBER   1998   GLAHN   REDUCED CALL SEQUENCE AND CALLED FINDST
C        NOVEMBER  1998   GLAHN   CHANGED DIMENSIONS OF CCALLD( )
C                                 AND ICALLD( , ) FROM ND1 TO ND5;
C                                 ELIMINATED SDATA1(ND1)
C        NOVEMBER  1998   GLAHN   OMITTED PRINT TO IP16 EXCEPT IN FINDST
C        NOVEMBER  1998   GLAHN   ADDED IER RETURN FROM FINDST
C        DECEMBER  1998   GLAHN   NCOMBO CHANGED FROM 9999 TO 999;
C                                 CHECK MSTA = NVALUE REMOVED
C        JULY      1999   GLAHN   ADDED IP12; DELETED IP16;
C                                 HARDWIRED UNIT NUMBERS TO CCC
C        AUGUST    1999   GLAHN   CHANGED IP16 IN CALL TO FINDST TO IP12
C        AUGUST    1999   GLAHN   ELIMINATED CALL TO COMPID FOR CCC
C                                 NE 4XX
C        MARCH     2000   DALLAVALLE   MODIFIED FORMAT STATEMENTS TO
C                                 CONFORM TO FORTRAN 90 STANDARDS 
C                                 ON IBM SP
C        MAY       2000   GLAHN   CORRECTED COMMENT FOR JD( ); SPELLING
C        MAY       2000   GLAHN   ELIMINATED DIAGNOSTIC WHEN NUMRA = 0
C        SEPTEMBER 2000   GLAHN   ADDED CALL TO CONSTG WHEN KFILRA = 44;
C                                 CHANGED DIMENSION OF KFILRA TO NUMRA;
C                                 DEFINED IGIVE, USED IT IN CALL TO UNPACK
C        FEBRUARY  2002   GLAHN   CHANGED DIMENSION OF RACESS FROM 5 TO
C                                 NUMRA
C        MAY       2002   GLAHN   ADDED TO COMMENT FOR SDATA( )
C        APRIL     2003   GLAHN   REMOVED CAPABILITY TO GET GRIDDED
C                                 CONSTANTS
C        JUNE      2003   GLAHN   REINSTATED FORMAT WHEN NUMRA = 0.
C        AUGUST    2003   GLAHN   REMOVED ISTAV = 0 AT END AND STATEMENT
C                                 NUMBER ON RETURN STATEMENT
C        AUGUST    2004   GLAHN   MADE SOME /D STATEMENTS C****D
C        OCTOBER   2012   ENGLE   CHANGED CALL FROM RDTDLM TO RDTDLMC WHEN
C                                 READING STATION CALL LETTERS.
C
C        PURPOSE 
C            TO PROVIDE FOR MOS-2000 PROGRAMS CONSTANTS, FORECASTS,
C            AND OTHER DATA THAT ARE AVAILABLE IN MOS-2000 EXTERNAL
C            RANDOM ACCESS FILES.  ONLY VECTOR DATA ARE ACCESSED; USE
C            CONST1 FOR POSSIBILITY OF GRIDDED DATA.  FOR CONSTANTS IN
C            CCC = 4XX SERIES, INTERPOLATION MAY BE DONE TO GET THE DATE
C            WANTED.  THE CCC IN THE MOS-2000 ID DETERMINES THE UNIT
C            NUMBER:
C
C               CCC RANGE      KFILX      USE          TYPE OF DATA
C                400-499        45       INPUT         VECTOR
C                500-699        46       INPUT         VECTOR
C                800-899        47       INPUT         VECTOR
C                200-299        48       INPUT         VECTOR
C                200-299        49       OUTPUT/INPUT  VECTOR
C
C            THE FOLLOWING 4XX CONSTANTS ARE ACCOMMODATED IN CONST AND
C            COMPID:
C               40XXXX = TRULY CONSTANT VALUES; DO NOT DEPEND ON TIME
C               41XXXX = DAILY VALUES; NO INTERPOLATION
C               422XXX = 5-DAY VALUES (JAN 5, JAN 10, ... 
C                        DEC 31 = 365TH DAY); INTERPOLATION ASSUMED
C               43XXXX = MONTHLY VALUES; INTERPOLATION ASSUMED
C               44XXXX = 6-MONTH SEASONS (APRIL-SEPTEMBER, ETC);
C                        NO INTERPOLATION
C               45XXXX = 3-MONTH SEASONS (MARCH-MAY, JUNE-AUGUST, ETC.);
C                        NO INTERPOLATION
C               46XXXX = YEARLY VALUES
C               47XXXX = MONTHLY VALUES; NO INTERPOLATION
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C            IP12   - INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                     STATIONS ON THE INPUT FILES WILL BE PRINTED TO 
C                     THE FILE WHOSE UNIT NUMBER IS IP12.
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C                IP12 = INDICATES WHETHER (>0) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE EXTERNAL RANDOM ACCESS FILES
C                       WILL BE LISTED TO UNIT IP12.  (INPUT)
C               ID(J) = THE PREDICTOR ID (J=1,4).  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID( ) (J=1,15).  (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               JD(J) = NOT USED IN CONST.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.  (INPUT)
C           KFILRA(J) = THE UNIT NUMBERS FOR WHICH RANDOM ACCESS FILES
C                       ARE AVAILABLE (J=1,NUMRA).  (INPUT)
C           RACESS(J) = THE FILE NAMES ASSOCIATED WITH KFILRA(J) (J=1,NUMRA).
C                       (CHARACTER*60)  (INPUT)
C               NUMRA = THE NUMBER OF VALUES IN KFILRA( ) AND RACESS( ).
C                       (INPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST, EXCEPT POSSIBLY 
C                       CCALLD( ).  EQUIVALENCED TO ICALL( , , ). 
C                       (CHARACTER*8)  (INPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN INTEGER
C                       VARIABLE (L=1,L3264W) (K=1,ND5).  THIS ARRAY IS USED 
C                       TO READ THE STATION DIRECTORY FROM A MOS-2000
C                       EXTERNAL FILE.  EQUIVALENCED TO CCALLD( ). 
C                       (CHARACTER*8)  (INTERNAL)
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).  THIS ARRAY IS USED 
C                       IN CONST TO READ THE STATION DIRECTORY.  EQUIVALENCED 
C                       TO ICALLD( , ).  (CHARACTER*8)  (INTERNAL)
C           ISDATA(K) = WORK ARRAY (K=1,ND1).  (INTERNAL)
C            SDATA(K) = CONSTANT DATA RETURNED WHEN DATA ARE VECTOR
C                       (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       DIMENSION OF SEVERAL VARIABLES.  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT WITH.
C                       (INPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(J) = WORK ARRAY. (J=1,ND5).  (OUTPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), DATA( ) AND
C                       CCALLD( ).  (INPUT)
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
C                              IN NGRIDC( ,L) DEFINING THE CHARACTERISTICS
C                              OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,NPRED) FOR WHICH THIS
C                              VARIABLE IS NEEDED, WHEN IT IS NEEDED ONLY
C                              ONCE FROM LSTORE( , ).  WHEN IT IS NEEDED
C                              MORE THAN ONCE, THE VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING MSTORE( , ).
C                              LATER USED AS A WAY OF DETERMINING WHETHER
C                              TO KEEP THIS VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , ) THAT 
C                       HAVE BEEN USED IN THIS RUN.
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL
C                       DATA ARE STORED ON DISK.  (OUTPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C               LASTL = THE LAST LOCATION IN CORE( ) USED FOR MOS-2000 INTERNAL
C                       STORAGE.  INITIALIZED TO 0 ON FIRST ENTRY TO GSTORE.
C                       ALSO INITIALIZED IN U201 IN CASE GSTORE IS NOT ENTERED.
C                       MUST BE CARRIED WHENEVER GSTORE IS TO BE CALLED.
C                       (INPUT/OUTPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C               LASTD = TOTAL NUMBER OF PHYSICAL RECORDS ON DISK FOR MOS-2000
C                       INTERNAL STORAGE.  MUST BE CARRIED WHENEVER GSTORE
C                       IS TO BE CALLED.  (INPUT)
C              NSTORE = THE NUMBER OF TIMES GSTORE HAS BEEN ENTERED.  GSTORE
C                       KEEPS TRACK OF THIS AND RETURNS THE VALUE.  (OUTPUT)
C              NFETCH = THE NUMBER OF TIMES GFETCH HAS BEEN ENTERED.  GFETCH
C                       KEEPS TRACK OF THIS AND RETURNS THE VALUE.  (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C               ISTAV = 1 WHEN THE DATA RETURNED ARE STATION DATA.  
C                       0 WHEN THE DATA RETURNED ARE GRID DATA OR DATA
C                         ARE NOT AVAILABLE FOR RETURN.  (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING USED
C                       (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS, EITHER 1 OR 2.  (INPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       160 = CANNOT FIND A UNIT NUMBER AND NAME IN THE
C                             LIST OF MOS-2000 EXTERNAL FILES TO ACCESS.
C                       164 = NUMBER OF INDEX VALUES FROM GFETCH DOES
C                             NOT AGREE WITH THE NUMBER OF VALUES RETURNED
C                             BY RDTDLM OR WITH NSTA.
C                       WHEN NO OTHER ERROR OCCURS, THE RETURN IS WITH
C                       IER = THAT FROM FINDST.
C                       SEE CALLED ROUTINES FOR OTHER VALUES.
C                       (INTERNAL-OUTPUT)
C               KFILX = THE UNIT NUMBER ON WHICH USE THE MOS-2000
C                       EXTERNAL FILE IS TO BE ACCESSED FOR THIS VARIABLE.
C                       SEE PURPOSE.  (INTERNAL)
C              RANAME = THE FILE NAME ASSOCIATED WITH KFILX.
C                       (CHARACTER*60)  (INTERNAL)
C               KD(J) = IDS FOR DIRECTORY RECORD IN THE MOS-2000 INTERNAL
C                       STORAGE SYSTEM (STATION CALL LETTERS) AND IDS
C                       FOR THE DATA (J=1,4).  (INTERNAL)
C               LD(J) = IDS FOR CALL LETTERS RECORD IN MOS-2000
C                       EXTERNAL RANDOM ACCESS FILES (J=1,4).
C                       (INTERNAL)
C              IERSAV = IER FROM FINDST.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES USED 
C            GSTORE, GFETCH, RDTDLM, DOY, UNPACK, FINDST
C
      CHARACTER*8 CCALL(ND1,6),
     1            CCALLD(ND5)
      CHARACTER*60 RACESS(NUMRA),RANAME
C
      DIMENSION ISDATA(ND1),SDATA(ND1)
      DIMENSION ID(4),IDPARS(15),JD(4),KD(4),LD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5),ICALLD(L3264W,ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION KFILRA(NUMRA)
C
      DATA NCOMBO/999/
      DATA NRRDAT/2100010100/
C
D     WRITE(KFILDO,100)
D100  FORMAT(/,' ENTERING CONST')
C
      IER=0
      IERSAV=0
      ISTAV=1
C        ISTAV = 1 INDICATES DATA ARE AT STATIONS.
C
C        FIND THE FILE UNIT NUMBER AND NAME BY ASSOCIATION
C        WITH CCC.  THESE VALUES ARE HARDWIRED.
C 
      DO 105 J=1,NUMRA
C
      IF(KFILRA(J).EQ.45.AND.
     1   IDPARS(1).GE.400.AND.
     2   IDPARS(1).LE.499)GO TO 120
      IF(KFILRA(J).EQ.46.AND.
     1   IDPARS(1).GE.500.AND.
     2   IDPARS(1).LE.699)GO TO 120
      IF(KFILRA(J).EQ.47.AND.
     1   IDPARS(1).GE.800.AND.
     2   IDPARS(1).LE.899)GO TO 120
      IF(KFILRA(J).EQ.48.AND.
     1   IDPARS(1).GE.200.AND.
     2   IDPARS(1).LE.299)GO TO 120
      IF(KFILRA(J).EQ.49.AND.
     1   IDPARS(1).GE.200.AND.
     2   IDPARS(1).LE.299)GO TO 120
 105  CONTINUE
C
C        NO UNIT NUMBER MATCHES WITH CCC IN ID.
C
      IF(NUMRA.NE.0)THEN
         WRITE(KFILDO,111)IDPARS(1),(ID(J),J=1,4),
     1                    (KFILRA(J),RACESS(J),J=1,NUMRA)
 111     FORMAT(/,' ****CANNOT FIND A MOS-2000 RANDOM ACCESS FILE',
     1            ' UNIT NUMBER IN CONST THAT MATCHES CCC =',I5,
     2            '  IN THE ID.',
     3          /,'     LOOKING FOR ',3(1X,I9.9),1X,I10.3,
     4          /,'     UNIT NUMBERS AND NAMES AVAILABLE ARE',
     5          /,('    ',I4,2X,A60))
      ELSE
         WRITE(KFILDO,112)IDPARS(1)
 112     FORMAT(/,' ****CANNOT FIND A MOS-2000 RANDOM ACCESS FILE',
     1           ' UNIT NUMBER FOR CCC =',I5,'.  NO FILES PROVIDED.')
      ENDIF
C
      IER=160
      GO TO 290
C
 120  KFILX=KFILRA(J)
      RANAME=RACESS(J)
C
C        RETRIEVE THE DIRECTORY RECORD IF IT IS AVAILABLE.
C        THIS IS THE CORRESPONDENCE BETWEEN THE NSTA STATIONS IN
C        CCALL( , ) AND THE STATIONS IN THE MOS-2000 RANDOM ACCESS
C        FILE.  THE ID FOR THE RECORD IS JUST THE FILE UNIT 
C        NUMBER IN KD(2) ALONG WITH THE CALL LETTERS DESIGNATOR
C        IN ID(1); ALL VARIABLES ON THE FILE WILL, OF COURSE, HAVE
C        THE SAME STATION INDEX AND THE CALL LETTERS DIRECTORY
C        WILL HAVE THE SAME IDS REGARDLESS OF THE FILE.
C
      KD(1)=400001000
      KD(2)=KFILX
      KD(3)=0
      KD(4)=0
      CALL GFETCH(KFILDO,KFIL10,KD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,ISDATA,ND1,
     2            MSTA,NPACK,0,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
C        THE INDEX VARIABLE IS TO BE IN ISDATA( ).
C        THE RETURNED VALUES NTIMES AND MISSS ARE NOT
C        NEEDED OR USED.  IT IS ASSUMED MSTA = NSTA, SINCE THIS
C        ROUTINE STORED THE DATA.  THE DATE OF SUCH RECORDS IS 0.
C        NOTE THAT ND1 IS DIMENSION OF IPACK( ), ETC., NOT ND5,
C        BECAUSE DIMENSION OF ISDATA( ) IS ND1.  THIS SHOULD BE
C        SUFFICIENT AND ND5 MUST BE GE ND1.
C
      IF(IER.EQ.0)THEN
C
         IF(MSTA.NE.NSTA)THEN
            WRITE(KFILDO,250)MSTA,NSTA
 250        FORMAT(/' ****NUMBER OF VALUES RETURNED IN CONST FROM',
     1              ' GFETCH =',I6,' DOES NOT EQUAL THE NUMBER',
     2              ' EXPECTED =',I6,'.',
     3             /'     DATA RETURNED AS MISSING.')
            IER=164
            GO TO 290
C
         ENDIF
C
         GO TO 260
C
      ENDIF
C
      IF(IER.NE.47)GO TO 290
C        IF IER NE 0 NOR 47, A FATAL ERROR HAS OCCURRED FOR THIS
C        VARIABLE.  THE DATA ARRAY SDATA( ) WILL BE SET TO MISSING
C        AND ISTOP INCREMENTED IN PRED1 OR PRED2.

C        INDEX RECORD NOT AVAILABLE, BECAUSE IER EQ 47.  GET THE
C        STATION INDEX FROM THE MOS-2000 EXTERNAL RANDOM ACCESS
C        FILE.  NOTE THAT LD(1) = KD(1).
C
      LD(1)=400001000
      LD(2)=0
      LD(3)=0
      LD(4)=0
CINTEL
C      CALL RDTDLM(KFILDO,KFILX,RANAME,LD,ICALLD,ND5*L3264W,NVALUE,
C     1               L3264B,IER)
      CALL RDTDLMC(KFILDO,KFILX,RANAME,LD,CCALLD,ND5*L3264W,NVALUE,
     1               L3264B,IER)
CINTEL
      MSTA=NVALUE/L3264W
C        THE CALL LETTERS ARE 8 BYTES EACH.  THIS IS TWO WORDS
C        ON A 32-BIT MACHINE.  THE NUMBER OF WORDS WRITTEN AND
C        READ MUST ACCOUNT FOR THIS.  THE ACTUAL NUMBER OF CALL
C        LETTERS MSTA AS RETURNED FROM RDTDLM IS NVALUE/L3264W.
      IF(IER.NE.0)GO TO 290
C
C        FIND THE LOCATION OF THE STATIONS IN CCALL( ) IN THE ARRAY
C        CCALLD( ), WHICH IS EQUIVALENCED TO ICALLD( , ).
C
      CALL FINDST(KFILDO,IP12,RANAME,CCALL,ND1,NSTA,
     1            CCALLD,MSTA,ISDATA,IER)
C           THE ONLY ERROR FROM FINDST IS IER = 120, WHICH 
C           MEANS ONE OR MORE STATIONS COULDN'T BE FOUND.
C           THIS IS NOT SUFFICIENT REASON TO STOP.  A DIAGNOSTIC
C           WILL HAVE BEEN PRINTED BY FINDST.  IF A STATION
C           CAN'T BE FOUND, ITS INDEX IN ISDATA( ) WILL BE
C           99999999.  SAVE IER FOR POSSIBLE RETURN.
      IERSAV=IER
C
C        STORE THE STATION INDEX IN THE INTERNAL RANDOM ACCESS
C        FILE SO THAT IT IS THERE NEXT TIME, AND FINDST DOESN'T
C        HAVE TO BE CALLED MULTIPLE TIMES.
C
      CALL GSTORE(KFILDO,KFIL10,KD,NCOMBO,LSTORE,ND9,LITEMS,
     1            ISDATA,NSTA,1,NRRDAT,0,
     2            CORE,ND10,LASTL,NBLOCK,LASTD,NSTORE,L3264B,IER)
C        NOTE THAT ISDATA( ) IS INTEGER.  ALTHOUGH THE CORRESPONDING
C        VARIABLE IN GSTORE IS REAL, THIS IS OK.
C
C        PUT THE IDS INTO KD( ) AS THEY RESIDE IN THE MOS-2000
C        EXTERNAL FILES.  FOR "CONSTANTS" (CCC = 4XX), THE
C        IDS ARE COMPUTED IN COMPID.  FOR OTHER DATA 
C        (E.G., FORECASTS WITH CCC = 2XX) KD( ) = ID( ).
C
 260  IF(KFILX.EQ.45)THEN
         CALL COMPID(KFILDO,ID,IDPARS,NDATE,KD,LD,MDOY,R,IER)
      ELSE
         KD(1)=ID(1)
         KD(2)=ID(2)
         KD(3)=ID(3)
         KD(4)=ID(4)
      ENDIF
C
      IF(IER.NE.0)GO TO 290
C
C        RETRIEVE THE DATA AND UNPACK.
C
      CALL RDTDLM(KFILDO,KFILX,RANAME,KD,IPACK,ND5,NVD,
     1               L3264B,IER)
      IF(IER.NE.0)GO TO 290
C
      IGIVE=2
C        IGIVE IN CALL TO UNPACK MEANS TO GET DATA AS
C        WELL AS FILL THE ISX( ) ARRAYS.
      CALL UNPACK(KFILDO,IPACK,IWORK,DATA,ND5,
     1            IS0,IS1,IS2,IS4,ND7,MISSP,MISSS,
     2            IGIVE,L3264B,IER)
      NVALUE=IS4(3)     
      IF(IER.NE.0)GO TO 290
C        IER NE 0 SHOULD BE TREATED AS A FATAL ERROR.  A DIAGNOSTIC
C        WILL HAVE BEEN PRINTED BY UNPACK.
C
      DO 270 K=1,NSTA
C
      IF(ISDATA(K).EQ.99999999)THEN
         SDATA(K)=9999.
      ELSE
         SDATA(K)=DATA(ISDATA(K))
      ENDIF
C
 270  CONTINUE
C***D     WRITE(KFILDO,271)(KD(J),J=1,4),NDATE,
C***D    1            (ISDATA(J),SDATA(J),J=1,NSTA)
C***D271  FORMAT(' ISDATA, SDATA IN CONST, KD( ) =',4I12,
C***D    1       '  NDATE =',I10,/,(' ',I8,F10.2))
C
C        AT THIS POINT ONE SET OF CONSTANTS HAVE BEEN RETRIEVED
C        AND STORED IN SDATA( ).  WHEN INTERPOLATION IS NEEDED,
C        A SECOND RETRIEVAL IS NECESSARY, AS WELL AS THE 
C        INTERPOLATION.  DO THAT NOW WHEN NECESSARY.  FOR
C        CCC = 4XX, THE INTERPOLATION FACTOR HAS BEEN RETURNED
C        FROM COMPID.  FOR OTHER DATA, CONST IS DONE.
C
      IF(KFILX.NE.45)GO TO 300
      IF(IDPARS(1).EQ.422.OR.
     1   IDPARS(1)/10.EQ.43)GO TO 275
      GO TO 300
C
 275  IF(KD(2).EQ.LD(2))GO TO 300
C        THE ABOVE TEST IS RELEVANT TO 5-DAY TEMPERATURES.
C      
      CALL RDTDLM(KFILDO,KFILX,RANAME,LD,IPACK,ND5,NVD,
     1               L3264B,IER)
      IF(IER.NE.0)GO TO 290
C
      CALL UNPACK(KFILDO,IPACK,IWORK,DATA,ND5,
     1            IS0,IS1,IS2,IS4,ND7,MISSP,MISSS,3,L3264B,
     2            IER)
      NVALUE=IS4(3)     
      IF(IER.NE.0)GO TO 290
C        IER NE 0 SHOULD BE TREATED AS A FATAL ERROR.  A DIAGNOSTIC
C        WILL HAVE BEEN PRINTED BY UNPACK.
C
      DO 280 K=1,NSTA
C
      IF(ISDATA(K).EQ.99999999)THEN
         SDATA(K)=9999.
      ELSE
C
         IF(SDATA(K).EQ.9999..OR.
     1      DATA(ISDATA(K)).EQ.9999.)THEN
            SDATA(K)=9999.
         ELSE
            SDATA(K)=(DATA(ISDATA(K))-SDATA(K))*R+SDATA(K)
         ENDIF
C
      ENDIF
C
 280  CONTINUE
C***D     WRITE(KFILDO,281)(ISDATA(K),SDATA(K),K=1,NSTA)
C***D281  FORMAT(' ISDATA, DATA(ISDATA), SDATA IN CONST',/,
C***D    1       (' ',I8,F10.2))
      GO TO 300
C
C        SET VALUES TO MISSING WHEN THERE IS AN ERROR.  THIS
C        IS FOR SAFETY; THE ARRAY WILL ALSO BE SET LATER, AND
C        ISTOP INCREMENTED.
C
 290  DO 295 K=1,NSTA
      SDATA(K)=9999.
 295  CONTINUE
C
C        FOR IER NE 0, THE DATA ARRAY SDATA( ) WILL BE SET TO
C        MISSING AND ISTOP INCREMENTED IN PRED1 OR PRED2.
C        IF DATA ARE GOOD, IER IS RETURNED AS FROM FINDST,
C        WHICH WHEN NOT ZERO MEANS ONE OR MORE STATIONS
C        COULD NOT BE FOUND.
C
 300  IF(IER.EQ.0)THEN
         IER=IERSAV
      ENDIF
C
      RETURN
      END
