      SUBROUTINE DIRFUV(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JD,ITAU,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,IER)
C 
C        MARCH     1999   GLAHN   TDL   MOS-2000
C        APRIL     1999   GLAHN   ADDED TEST FOR BOTH U AND V = 0
C        APRIL     1999   GLAHN   ADDED ITAU TO CALL
C        APRIL     1999   GLAHN   CHANGED NDATE TO MDATE, ADDED ITIME
C        APRIL     2000   RUDACK  REPLACED GFETCH WITH RETVEC; ADDED TO
C                                 CALL; ADDED NDIM AND YDATA( );
C                                 COMMAS IN COMMENTS FOR IBM
C        APRIL     2000   GLAHN   ADDED DIMENSION FOR ISDATA( );
C                                 IMPROVED COMMENTS; CHECKED SPELLING;
C                                 ADDED NINT WHEN CHECKING FOR 9999;
C                                 ELIMINATED ITIME AND ISTAV; INSURED
C                                 /, IN **** COMMENTS
C        APRIL     2000   RUDACK  ADDED COMMENTS; ADDED DATA
C                                 LDPARS/15*0/ 
C        AUGUST    2002   MCALOON UPDATED ID'S TO ACCOMMODATE NGM WIND
C                                 FCST BY MOD. PERFECT PROG.
C        MARCH     2006   COSGROVE  CHANGED TESTS FOR CALM WIND FROM
C                                 EQUIVALENCING A REAL TO CHECKING
C                                 IF THE |U| AND/OR |V| IS LESS THAN
C                                 0.1
C        JUNE      2008   GLAHN   ADDED 3 ITEMS TO ITABLE( , )
C        SEPTEMBER 2008   GLAHN   COMMENT IN PURPOSE
C        FEBRUARY  2009   COSGROVE PUT BACK WIND ID REMOVED IN
C                                 GLAHN 2008 CHANGES.  HE HAD CHANGED
C                                 204200 TO 204225.  CHANGED IT BACK.
C
C        PURPOSE 
C            TO COMPUTE WIND DIRECTION FROM U AND V COMPONENTS.
C            THE VARIABLES ACCOMMODATED ARE INDICATED IN ITABLE(1,J).
C            WHEN BOTH U AND V ARE 0, THE WIND DIRECTION IS SET TO
C            MISSING (NO DIRECTION CAN BE DETERMINED.)
C            POSSIBLE MISSING VALUES OF 9999. ARE TREATED AS 
C            MISSING = 9999.  VECTOR DATA FROM THE MOS-2000 INTERNAL
C            STORAGE SYSTEM WILL BE UNPACKED, WILL BE IN THE ORDER
C            NEEDED.  DIRFUV ASSUMES THERE WILL BE NO SECONDARY
C            MISSING VALUES OF 9997; THESE SHOULD HAVE ALREADY BEEN
C            TREATED.  IT IS NOTED THAT IF DIRECTION IS COMPUTED
C            FROM THE U AND V COMPONENTS WHICH THEMSELVES WERE
C            COMPUTED FROM OBSERVED SPEED AND DIRECTION, THE
C            DIRECTION WILL NOT BE RECOVERED EXACTLY.
C
C	 IDPARS(1) & IDPARS(2) OF U AND V COMPONENTS
C        ARE MAPPED: INTO       FROM 
C                    004200     004010, 004110 (U-V COMP. OF WINDS ON
C                                               P-SURFACE)
C                    004201     004011, 004111 (U-V COMP. OF WINDS ON
C                                               CONST. HT. SFC.)
C                    004202     004012, 004112 (GEOSTROPHIC U-V COMP. ON
C                                               P-SURFACE)
C                    704200     704010, 704110 (U-V WIND COMPONENT (KTS)
C                                               - EARTH ORIENTED)
C                    204225     204010, 204110 (LMP WIND U-V COMPONENTS)
C                    204250     204020, 204120 (MOS WIND U-V COMPONENTS)
C                    224200     224010, 224110 (SFC. WIND U-V COMPONENTS
C                                               - NGM MOS FCST)
C                    224250     224060, 224160 (MOS GRIDDED)
C                    224225     224010, 224110 (LAMP GRIDDED)
C                    724250     724020, 724120 (OBS GRIDDED)
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                     (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C            IP12   - INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                     STATIONS ON THE INPUT FILES WILL BE PRINTED TO 
C                     THE FILE WHOSE UNIT NUMBER IS IP12.  (OUTPUT)
C         KFILRA(J) - THE UNIT NUMBERS FOR WHICH RANDOM ACCESS FILES
C                     ARE AVAILABLE (J=1,NUMRA).  (INPUT)
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C                IP12 = INDICATES WHETHER (>0) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE EXTERNAL RANDOM ACCESS FILES
C                       WILL BE LISTED TO UNIT IP12.  (INPUT)
C           KFILRA(J) = THE UNIT NUMBERS FOR WHICH RANDOM ACCESS FILES
C                       ARE AVAILABLE (J=1,NUMRA).  (INPUT)
C           RACESS(J) = THE FILE NAMES ASSOCIATED WITH KFILRA(J) (J=1,NUMRA).
C                       (CHARACTER*60)  (INPUT)
C               NUMRA = THE NUMBER OF VALUES IN KFILRA( ) AND RACESS( ).
C                       (INPUT)
C               ID(J) = THE PREDICTOR ID (J=1,4).  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID( ) (J=1,15).
C                       (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 
C                            1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK IN 
C                            TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE
C                       PORTIONS PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       JD( ) IS USED TO IDENTIFY THE BASIC MODEL FIELDS
C                       AS READ FROM THE ARCHIVE.  (INPUT)
C                ITAU = THE NUMBER OF HOURS AHEAD TO FIND A VARIABLE.
C                       THIS HAS ALREADY BEEN CONSIDERED IN MDATE, BUT
C                       IS NEEDED FOR CALL TO RETVEC.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C               MDATE = NDATE UPDATED WITH ITAU( ).  (INPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST, EXCEPT POSSIBLY 
C                       CCALLD( ).  EQUIVALENCED TO ICALL( , , ). 
C                       (CHARACTER*8)  (INPUT)
C           ISDATA(K) = WORK ARRAY (K=1,ND1).  (INTERNAL)
C            XDATA(K) = U-COMPONENT OF WIND, THEN THE WIND DIRECTION IS
C                       RETURNED IN XDATA(K) (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF XDATA( ).  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN 
C                       INTEGER VARIABLE (L=1,L3264W) (K=1,ND5).
C                       THIS ARRAY IS USED TO READ THE STATION DIRECTORY
C                       FROM A MOS-2000 EXTERNAL FILE.  EQUIVALENCED
C                       TO CCALLD( ).   (CHARACTER*8)  (INTERNAL)
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).  EQUIVALENCED
C                       TO ICALLD( , ).  (INTERNAL)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(K) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND DATA( ).
C                       (INPUT)
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
C                              DETERMINING WHETHER TO KEEP THIS 
C                              VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA 
C                       IDENTIFIED IN LSTORE( , ) (J=1,ND10).  WHEN 
C                       CORE( ) IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NFETCH = THE NUMBER OF TIMES RETVEC HAS BEEN ENTERED.
C                       RETVEC KEEPS TRACK OF THIS AND RETURNS THE
C                       VALUE.  (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3). 
C                       (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+). 
C                       (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12). 
C                       (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4). 
C                       (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).  
C                       CALCULATED BY PARAMETER, BASED ON L3464B.
C                       (INPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       102 = ID NOT ACCOMMODATED.
C                       SEE RETVEC FOR OTHER VALUES.  (OUTPUT)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       XDATA( ) FOR THE U COMPONENT (J=1,4).
C                       (INTERNAL)
C               MD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       YDATA( ) FOR THE V COMPONENT (J=1,4).
C                       (INTERNAL)
C           LDPARS(J) = PARSED ID FILLED IN SUBROUTINE PRSID1 FOR
C                       U-COMPONENT (J=1,15). (INTERNAL)
C           MDPARS(J) = PARSED ID FILLED IN SUBROUTINE PRSID1 FOR
C                       V-COMPONENT (J=1,15). (INTERNAL)
C         ITABLE(I,J) = CCCFFF OF THE DIRECTION (I=1) AND THE NEEDED
C                       U COMPONENT (I=2) AND V COMPONENT (I=3)
C                       (J=1,NDIM).  (INTERNAL)
C                NDIM = SECOND DIMENSION OF ITABLE( , ).  SET BY
C                       PARAMETER.  (INTERNAL)
C            YDATA(K) = V-COMPONENT OF WIND RETRIEVED IN RETVEC.
C                       (K=1,NSTA) (AUTOMATIC).  (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            PRSID1,RETVEC
C
      PARAMETER (NDIM=10)
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION ISDATA(ND1),XDATA(ND1),YDATA(ND1)
C        YDATA( ) IS AN AUTOMATIC ARRAY.
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION LD(4),MD(4),KFILRA(5)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9),ICALLD(L3264W,ND5)
      DIMENSION CORE(ND10)
      DIMENSION ITABLE(3,NDIM),LDPARS(15),MDPARS(15)
C
      DATA LDPARS/15*0/
      DATA MDPARS/15*0/
      DATA ITABLE/004200, 004010, 004110,
     1            004201, 004011, 004111,
     2            004202, 004012, 004112,
     3            704200, 704010, 704110,
     4            204200, 204010, 204110,
     5            204250, 204020, 204120,
     6            224200, 224010, 224110,
     7            224250, 224060, 224160,
     8            224225, 224010, 224110,
     9            724250, 724020, 724120/
C
      IER=0
C
C        FIND THE LOCATION IN THE TABLE.
C
      DO 105 JJ=1,NDIM
      IF(ITABLE(1,JJ).EQ.IDPARS(1)*1000+IDPARS(2))GO TO 115
 105  CONTINUE
C
      WRITE(KFILDO,110)(JD(L),L=1,4),IDPARS(1),IDPARS(2)
 110  FORMAT(/,' ****DIRFUV ENTERED FOR VRBL',
     1         2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2       ' WITH CCCFFF = ',2I3,' IN IDPARS(1) AND IDPARS(2)',
     3       ' NOT ACCOMMODATED.')
      IER=102
      GO TO 300
C
C        GET THE U COMPONENT.
C
 115  LD(1)=ITABLE(2,JJ)*1000+IDPARS(4)
      LD(2)=JD(2)
      LD(3)=JD(3)
      LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
C
      CALL PRSID1(KFILDO,LD,LDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            LD,LDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NSTA,
     3            ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4            LSTORE,ND9,LITEMS,CORE,ND10,
     5            NBLOCK,NFETCH,
     6            IS0,IS1,IS2,IS4,ND7,
     7            L3264B,L3264W,IER)
C
C        JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C        IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,130)(LD(J),J=1,4)
130      FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN DIRFUV',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 300
      ENDIF
C
C        GET THE V COMPONENT.
C
      MD(1)=ITABLE(3,JJ)*1000+IDPARS(4)
      MD(2)=JD(2)
      MD(3)=JD(3)
      MD(4)=LD(4)
C
      CALL PRSID1(KFILDO,MD,MDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            MD,MDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,YDATA,ND1,NSTA,
     3            ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4            LSTORE,ND9,LITEMS,CORE,ND10,
     5            NBLOCK,NFETCH,
     6            IS0,IS1,IS2,IS4,ND7,
     7            L3264B,L3264W,IER)
C
C        JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C        IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,132)(MD(J),J=1,4)
 132     FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN DIRFUV',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 300
      ENDIF
C
      DO 211 J=1,NSTA
         IF(NINT(XDATA(J)).NE.9999.AND.NINT(YDATA(J)).NE.9999)THEN
C
            IF(ABS(YDATA(J)).LT.0.1)THEN
C
               IF(ABS(XDATA(J)).LT.0.1)THEN
                  XDATA(J)=9999.
               ELSE
                  XDATA(J)=SIGN(90.,XDATA(J))+180.
               ENDIF
C
            ELSE
               XDATA(J)=57.29578*ATAN2(XDATA(J),YDATA(J))+180.
            ENDIF
C
         ELSE
            XDATA(J)=9999.
         ENDIF
C
 211  CONTINUE
C 
      GO TO 350   
C
C        THIS VARIABLE CANNOT BE COMPUTED.  SET XDATA( ) TO MISSING.
C        THIS IS FOR SAFETY; XDATA( ) SHOULD ALREADY BE SET TO MISSING.
C
 300  DO 310 J=1,NSTA
      XDATA(J)=9999.
 310  CONTINUE 
C
 350  RETURN
      END     
                  
