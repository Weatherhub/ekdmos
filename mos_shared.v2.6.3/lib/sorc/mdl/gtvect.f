      SUBROUTINE GTVECT(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,IPRINT,
     1                  ID,IDPARS,TRESHL,JD,ITAU,NWHERE,NVRBL,N,
     2                  NDATE,CCALL,ISDATA,SDATA,XDATA,ND1,NCAT,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  LASTL,LASTD,NBLOCK,NSTORE,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,ISTOP,ISTAB,IER)
C
C        JANUARY   1997   GLAHN   MOS-2000
C        JANUARY   1997   GLAHN   ELIMINATED NELEV( ), STALAT( ),
C                                 STALON( ) 
C        MARCH     1997   GLAHN   MODIFIED FOR LOOKAHEAD FEATURE
C        AUGUST    1997   GLAHN   INCREASED DIMENSIONS OF CCALL( ) TO
C                                 ACCOMMODATE SUBSTITUTE STATIONS.
C                                 WILL NOT ENTER OPTX WHEN NWHERE( ) = 1
C        DECEMBER  1997   GLAHN   STATEMENT 435 CHANGED TO ALLOW SAVING
C                                 DATA WHEN N+1 = NVRBL;
C                                 NOPP1 REMOVED, NTYPVR( ) ADDED
C        DECEMBER  1997   GLAHN   ADDED EXTRA SEARCH WHEN ID( ) NE JD( );
C                                 ADDED ISTAB TO CALL
C        FEBRUARY  1998   GLAHN   ITIME MODIFIED TO -IDPARS(12, )
C                                 ALSO REQUIRED A CHANGE TO GFETCH
C        APRIL     1998   GLAHN   ADDED TEST FOR AEV DATA AT 423 AND 4245
C        MAY       1998   GLAHN   SET NWHERE(N) = 4 WHEN VARIABLE NOT
C                                 IDENTIFIED IN OPTX, AND TESTED FOR
C                                 THAT BEFORE ENTERING OPTX
C        MAY       1998   GLAHN   REMOVED NTYPVR( ); ADDED ITAU( );
C                                 INCREASED LD(6) TO LD(7); ADDED
C                                 ADDITIONAL CHECKS IN 4245
C        JUNE      1998   GLAHN   ADDED ITAU TO CALL TO OPTX
C        SEPTEMBER 1998   GLAHN   ADDED TEST TO NOT SAVE CONSTANTS
C        OCTOBER   1998   GLAHN   REMOVED NUMIN AND INDEXC( )
C        FEBRUARY  1999   GLAHN   CHANGED NDATE TO MDATE IN PRINT AT 530 
C        APRIL     1999   GLAHN   ADDED MDATE IN CALL TO OPTX
C        JULY      1999   GLAHN   ADDED KFILRA( ), RACESS( ), NUMRA;
C                                 ADDED IP12, NCAT, ISTAB TO CALL TO OPTX
C        SEPTEMBER 1999   GLAHN   ADDED DATA STATEMENT FOR LD( )
C        MARCH     2000   DALLAVALLE   MODIFIED FORMAT STATEMENTS TO CONFORM
C                                 TO FORTRAN 90 STANDARDS ON THE
C                                 IBM SP
C        MAY       2000   GLAHN   CHANGED DIAGNOSTIC FORMAT 231
C        FEBRUARY  2002   GLAHN   COMMENT ABOUT NWHERE( ); SPELLING
C        JANUARY   2004   GLAHN   CHANGED TO ACCOMMODATE NWHERE( ) = 0
C        MARCH     2004   GLAHN   CHANGED CHECK ON 8XX TO 9XX BELOW 435
C                                 TO NOT SAVE STRATIFICATION VARIABLES
C        NOVEMBER  2004   GLAHN   MODIFIED MDATE TO ZERO FOR CCC = 4XX
C        AUGUST    2009   GLAHN   COMMAS IN FORMAT 421
C        AUGUST    2009   GLAHN   MODIFIED STATEMENT ABOVE 410 FROM
C                                 "IF(IDPARS(1,N)/100.EQ.4)THEN" TO\
C                                 INCLUDE ".AND.NWHERE(N).NE.1)";
C                                 ADDED SAVE LD
C
C        PURPOSE
C           TO OBTAIN FOR VRBL62, FCST62, VRBL64, AND SIMILAR ROUTINES
C           VARIABLES THAT MUST BE RETRIEVED FROM THE MOS-2000 INTERNAL
C           STORAGE SYSTEM OR COMPUTED BY OPTX.  THIS ROUTINE 
C           SHOULD WORK FOR OTHER VECTOR PROGRAMS SUCH AS
C           U600 AND U700.  ONE VARIABLE IS RETURNED PER CALL.
C           THE COMPLETE LIST IS FURNISHED BECAUSE OF THE LOOKAHEAD
C           FEATURE.
C   
C        DATA SET USE
C            KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C            IP12   - INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                     STATIONS ON THE INPUT FILES WILL BE PRINTED TO 
C                     THE FILE WHOSE UNIT NUMBER IS IP12.
C            KFILRA(J) - UNIT NUMBERS FOR READING MOS-2000 EXTERNAL
C                     RANDOM ACCESS FILES (J=1,NUMRA).  (INPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C                IP12 = INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE INPUT FILES WILL BE PRINTED TO 
C                       THE FILE WHOSE UNIT NUMBER IS IP12.  (INPUT)
C           KFILRA(J) = UNIT NUMBERS FOR READING MOS-2000 EXTERNAL
C                       RANDOM ACCESS FILES (J=1,NUMRA).  (INPUT)
C           RACESS(J) = FILE NAMES FOR MOS-2000 EXTERNAL RANDOM ACCESS
C                       FILES HOLDING CONSTANT DATA READ ON UNIT NOS.
C                       KFILRA(J) IN OPTX (J=1,NUMRA).  (CHARACTER*60)
C                       (INPUT)
C               NUMRA = NUMBER OF VALUES IN KFILRA( ) AND RACESS( ).
C                       (INPUT)
C              IPRINT = USED TO KEEP A DIAGNOSTIC FROM BEING PRINTED
C                       (IPRINT = 0) FOR EACH MISSING VARIABLE
C                       WHEN THE ENTIRE DAY IS MISSING.  (INPUT)
C             ID(J,N) = THE INTEGER PREDICTOR ID'S (J=1,4) (N=1,NVRBL).
C                       (INPUT)
C         IDPARS(J,N) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID( ) (J=1,15) (N=1,NVRBL).
C                       (INPUT)
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
C              TRESHL = THE LOWER BINARY THRESHOLD ASSOCIATED WITH IDPARS( ,N).
C                       (INPUT)
C             JD(J,N) = THE BASIC INTEGER PREDICTOR ID (J=1,4) (N=1,NVRBL).
C                       THIS IS THE SAME AS ID(J,N), EXCEPT THAT THE PORTIONS
C                       PERTAINING TO PROCESSING THAT CAN BE DONE
C                       IN VECTOR PROGRAMS ARE OMITTED:
C                       B = IDPARS(3, ),
C                       G = IDPARS(15, ), AND
C                       THRESH( ).
C                       G HAS NO MEANING IN VECTOR PROGRAMS.
C                       JD( , ) IS USED TO IDENTIFY THE BASIC MODEL FIELDS
C                       AS READ FROM THE ARCHIVE.  (INPUT)
C             ITAU(N) = THE NUMBER OF HOURS TO ADD TO NDATE TO GET 
C                       THE VARIABLE N (N=1,NVRBL).
C                       THIS IS THE "LOOKAHEAD" FEATURE.  (INPUT)
C           NWHERE(N) = INDICATES WHERE THE VARIABLE IS TO COME FROM (N=1,ND4)
C                       0 = UNDETERMINED; TRY BOTH GFETCH AND OPTX.
C                       1 = FROM INPUT FILE.  THIS MAY ALREADY BE IN THE 
C                           MOS-2000 STORAGE SYSTEM BECAUSE OF THE LOOKAHEAD
C                           FEATURE.
C                       2 = BINARY FROM BASIC VARIABLE IN VRBL61.
C                       3 = FROM OPTX.
C                       4 = A DUPLICATE (PREDICTORS ONLY).  NOT VALID
C                           IN GTVECT.
C                       (INPUT)
C               NVRBL = THE NUMBER OF VARIABLES NEEDED AND IDENTIFIED IN 
C                       ID( , ), ETC.  ALSO TREATED AS THE DIMENSION OF THE
C                       VARIABLES ID( , ), ETC.  (INPUT)
C                   N = THE NUMBER OF THE VARIABLE IN ID( ,N), ETC.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTORS ARE TO BE
C                       FURNISHED ON THIS CALL TO GTVECT.  (INPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST.  (CHARACTER*8)
C                       (INPUT)
C           ISDATA(K) = WORK ARRAY (K=1,ND1).  (INTERNAL)
C            SDATA(K) = DATA SAVED FOR REUSE (K=1,NSTA).  (INPUT/OUTPUT)
C            XDATA(K) = THE DATA FOR THE NSTA STATIONS BEING PROCESSED
C                       (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       MUST BE LE ND5.  (INPUT)
C                NCAT = A CATEGORY NUMBER USED BY U710 AND U910 FOR
C                       INPUT TO OPTX.  (INPUT)
C                NSTA = THE NUMBER OF STATIONS IN CCALL( ).  (INPUT)
C         ICALLD(L,K) = 8-CHARACTER STATION CALL LETTERS AS CHARACTERS
C                       IN AN INTEGER VARIABLE (L=1,L3264W) (K=1,NSTA).
C                       NOTE THAT THIS REQUIRES TWO 32-BIT WORDS TO HOLD
C                       THE DESCRIPTION BUT ONLY ONE 64-BIT WORD.
C                       NEEDED IN CONST6 FOR ARGUMENT TO RDTDLM.
C                       EQUIVALENCED TO CCALLD( ).  (INTERNAL)
C           CCALLD(K) = 8-CHARACTER STATION CALL LETTERS (K=1,ND5).
C                       EQUIVALENCED TO ICALLD( , ).  (CHARACTER*8)
C                       (INTERNAL)
C            IPACK(J) = HOLDS THE TDL GRIB RECORD (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  FURNISHED TO UNPACK, BUT
C                       NOT ACTUALLY USED BECAUSE ONLY THE ID'S ARE
C                       UNPACKED.  (INTERNAL)
C             DATA(J) = WORK ARRAY (J=1,ND5).  FURNISHED TO UNPACK, BUT
C                       NOT ACTUALLY USED BECAUSE ONLY THE ID'S ARE
C                       UNPACKED.  (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND DATA( ).
C                       (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED IN THE MOS-2000 INTERNAL STORAGE SYSTEM
C                       (L=1,11) (J=1,LITEMS).  (INPUT)
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
C                       L=10 --NOT USED.
C                       L=11 --THE NUMBER OF THE FIRST PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,NVRBL) FOR WHICH THIS
C                              VARIABLE IS NEEDED, WHEN IT DOES NOT NEED
C                              TO BE STORED AFTER DAY 1.  WHEN THE VARIABLE
C                              MUST BE STORED (TO BE ACCESSED THROUGH OPTION)
C                              FOR ALL DAYS, ID(11,N) IS 7777 + THE NUMBER
C                              OF THE FIRST PREDICTOR IN THE SORTED LIST
C                              FOR WHICH THIS VARIABLE IS NEEDED.
C                       L=12 --USED INITIALLY IN ESTABLISHING MSTORE( , ).
C                              LATER USED AS A WAY OF DETERMINING WHETHER
C                              TO KEEP THIS VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ) AND MSTORE( , ).
C                       (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , ) THAT 
C                       HAVE BEEN USED IN THIS RUN.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL
C                       DATA ARE STORED ON DISK.  (INPUT-OUTPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C               LASTL = THE LAST LOCATION IN CORE( ) USED.  THIS MAY BE
C                       MODIFIED, ALONG WITH ITEMS, IF COMPACTION IS
C                       DONE BY GCPAC.  INITIALIZED TO ZERO ON FIRST 
C                       ENTRY TO GSTORE.  (INPUT-OUTPUT)
C               LASTD = TOTAL NUMBER OF PHYSICAL RECORDS ON DISK.  INITIALIZED
C                       TO ZERO ON FIRST ENTRY TO GSTORE.  (INPUT-OUTPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NSTORE = RUNNING COUNT OF NUMBER OF TIMES DATA ARE STORED BY 
C                       GSTORE.  INITIALIZED TO ZERO THE FIRST TIME GSTORE
C                       IS CALLED.  GSTORE KEEPS TRACK OF THIS AND RETURNS
C                       THE VALUE.  (OUTPUT)
C              NFETCH = THE NUMBER OF TIMES GFETCH HAS BEEN ENTERED.  GFETCH
C                       KEEPS TRACK OF THIS AND RETURNS THE VALUE.  (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING USED
C                       (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).  (INPUT) 
C               ISTOP = INCREMENTED BY ONE EACH TIME AN ERROR IS ENCOUNTERED.
C                       (INPUT-OUTPUT)
C               ISTAB = USUALLY  ZERO, BUT SET TO ONE WHEN CERTAIN
C                       SUBROUTINES ARE CALLED FROM OPTX.  (OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       139 = MISSING DATA FOR THIS VARIABLE.
C                       (OUTPUT)
C               LD(J) = HOLDS INFORMATION ABOUT THE DATA IN SDATA( )
C                       (J=1,7).
C                       J=1--JD(1,N)
C                       J=2--ID(2,N)
C                       J=3--ID(3,N) 
C                       J=4--IDPARS(3,N)
C                       J=5--IDPARS(13,N)
C                       J=6--IDPARS(14,N)
C                       J=7--MDATE, THE DATE OF THE DATA
C                       (INTERNAL)
C               MDATE = NDATE UPDATED WITH ITAU( ).  (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES USED 
C            OPTX, TIMPR, UPDAT, GFETCH
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(NUMRA)
C
      DIMENSION ISDATA(ND1),SDATA(ND1),XDATA(ND1)
      DIMENSION ID(4,NVRBL),IDPARS(15,NVRBL),JD(4,NVRBL),
     1          ITAU(NVRBL),NWHERE(NVRBL)
      DIMENSION ICALLD(L3264W,ND5),IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION KFILRA(NUMRA)
      DIMENSION LD(7)
C
      DATA LD/7*0/
C        DATA STATEMENT FOR LD( ) CAUSES DATA TO BE SAVED
C        FROM ENTRY TO ENTRY.
      SAVE LD
C
C***D     CALL TIMPR(KFILDO,KFILDO,'START GTVECT        ')
      IER=0
      ISTAB=0
C        ISTAB = 0 MEANS VARIABLE IS NOT BINARY.
C
C***D     WRITE(KFILDO,405)N,(JD(J,N),J=1,4),(ID(J,N),J=1,4),NWHERE(N)
C***D405  FORMAT(' IN GTVECT, LOOKING FOR VRBL NO. ',
C***D    1       I3,3(1XI9.9),1XI10.3,3(1XI9.9),1XI10.3,'  NWHERE ='I4)
C
C        SET MDATE AND ITIME FOR THIS VARIABLE.  IF TAU = 0, 
C        THEN IF CCC = 4XX (A CONSTANT), MDATE = 0.
C
      IF(ITAU(N).EQ.0)THEN
C
         IF(IDPARS(1,N)/100.EQ.4.AND.NWHERE(N).NE.1)THEN
C              IF A CONSTANT CAME FROM THE INPUT SEQUENTIAL FILE,
C              NWHERE(N) = 1, AND IT WILL HAVE A DATE.
C              IF IT CAME FROM AN EXTERNAL RA FILE THROUGH OPTX,
C              NWHERE(N) = 3, AND THE DATE IS ZERO.
C              CORRECTION MADE 8/7/09.
            MDATE=0
         ELSE
            MDATE=NDATE
         ENDIF
C
         ITIME=0
      ELSE
         CALL UPDAT(NDATE,ITAU(N),MDATE)
         ITIME=-ITAU(N)
      ENDIF
C
D     WRITE(KFILDO,409)(LD(J),J=1,7),(JD(J,N),J=1,4),
D    1                  NWHERE(N),ITAU(N),NDATE,MDATE
D409  FORMAT(' IN GTVECT--(LD(J),J=1,7),(JD(J,N),J=1,4),',
D    1       'NWHERE(N),ITAU(N),NDATE,MDATE',/,
D    2        3(1X,I9.9),3I4,I11,3(1X,I9.9),3I4,2I11)
      IF(NWHERE(N).EQ.1)GO TO 423
      IF(NWHERE(N).EQ.3)GO TO 425
      IF(NWHERE(N).EQ.2)GO TO 415
      IF(NWHERE(N).EQ.0)GO TO 423
      IF(NWHERE(N).EQ.4)GO TO 411
C        NWHERE(N) = 4 MAY NOT BE USED IN U660.
C        THE TEST IS LEFT HERE FOR SAFETY.
      WRITE(KFILDO,410)NWHERE(N),N,NDATE
 410  FORMAT(/,' ****NWHERE( ) VARIABLE =',I3,
     1         ' INCORRECT FOR VARIABLE NO. ',I4,
     2         ' IN GTVECT AT 410.  DATE =',I11)
 411  ISTOP=ISTOP+1
C
 412  DO 413 K=1,NSTA
      XDATA(K)=9999.
 413  CONTINUE
C
      IER=139
      GO TO 530
C
C        IS THIS VARIABLE REUSABLE AND STORED IN SDATA( )?
C
 415  IF(JD(1,N).NE.LD(1).OR.
     1   ID(2,N).NE.LD(2).OR.
     2   ID(3,N).NE.LD(3).OR.
     3   IDPARS(3,N).GE.5.OR.
     4   IDPARS(13,N).NE.LD(5).OR.
     5   IDPARS(14,N).NE.LD(6).OR.
     6   MDATE.NE.LD(7))GO TO 423
C           IF THE NWHERE( ) VARIABLE WORKS PROPERLY, THE ABOVE
C           TRANSFER SHOULD NOT BE MADE.
C
C        TAKE DATA FROM SDATA( ).  ISTAB = 0 BECAUSE A BINARY
C        IS NOT SAVED IN SDATA( ).
C
      DO 420  K=1,NSTA
      XDATA(K)=SDATA(K)
 420  CONTINUE
C
D     WRITE(KFILDO,421)N,(LD(J),J=1,7)
D421  FORMAT(/' RESTORING DATA FOR VARIABLE NO.',I4,' IN GTVECT   ',
D    1        3(1X,I9.9),3I4,I11)
      GO TO 530
C
C        TRY TO FIND BASIC VARIABLE IN LSTORE AND RETURN IT IN DATA( ).
C        NOTE THAT JD( , ) IS USED IN THE CALL.  ALSO, THE DATE  
C        HAS BEEN ADJUSTED FOR LOOKAHEAD.
C
 423  ISTAB=0
C        SET ISTAB = 0 TO INDICATE NON-BINARY.
      IF(JD(1,N).EQ.ID(1,N).AND.
     1   JD(4,N).EQ.ID(4,N))GO TO 4245
C        THE ABOVE TEST KEEPS GFETCH FROM BEING CALLED TWICE 
C        FOR NO REASON.  THAT IS, THE FULL AND PARTIAL IDS
C        ARE THE SAME.  JD(2, ) AND JD(3, ) ALWAYS EQUAL
C        ID(2, ) AND ID(3, ), RESPECTIVELY.
      CALL GFETCH(KFILDO,KFIL10,JD(1,N),N,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,XDATA,ND1,
     2            NWORDS,NPACK,MDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSOURC,MISSP,MISSS,L3264B,ITIME,
     4            IER)
C        NOTE THAT JD( ) IS USED IN THE ABOVE CALL.
C
      IF(IER.EQ.0)THEN
         IF(NWHERE(N).EQ.0)NWHERE(N)=1
         GO TO 435
      ENDIF
C
C        IF THE ABOVE FETCH FAILED, MUST TRY FULL ID BECAUSE 
C        IN U660 THE VARIABLES MAY NOT BE ORDERED.
C
 4245 CALL GFETCH(KFILDO,KFIL10,ID(1,N),N,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,XDATA,ND1,
     2            NWORDS,NPACK,MDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSOURC,MISSP,MISSS,L3264B,ITIME,
     4            IER)
C        NOTE THAT ID( ) IS USED IN THE ABOVE CALL.
      IF(IDPARS(3,N).NE.0)ISTAB=1
C        SET ISTAB = 1 TO INDICATE BINARY WHEN APPROPRIATE.
C
      IF(IER.EQ.0)THEN
         IF(NWHERE(N).EQ.0)NWHERE(N)=1
         GO TO 435
      ELSE
C
         IF(NWHERE(N).EQ.0)THEN
            GO TO 425 
         ELSE         
            IER=139
            ISTOP=ISTOP+1
            GO TO 530
         ENDIF
C
      ENDIF
C
C        MUST COMPUTE THIS PREDICTOR.
C        NOTE THAT 9997 AS WELL AS 9999 MUST BE HANDLED.
C
 425  CALL OPTX(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1          ID(1,N),IDPARS(1,N),TRESHL,JD(1,N),ITAU(N),
     2          NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NCAT,NSTA,
     3          ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4          LSTORE,ND9,LITEMS,CORE,ND10,
     5          LASTL,LASTD,NBLOCK,NSTORE,NFETCH,
     6          IS0,IS1,IS2,IS4,ND7,
     7          L3264B,L3264W,ISTAB,IER)
C        AN ERROR IN OPTX WILL GENERATE A DIAGNOSTIC.  ISTOP IS 
C        INCREMENTED, EVEN WHEN IER = 47, WHICH JUST MEANS DATA
C        COULD NOT BE FOUND.  AN IER = 120 FROM FINDST FROM CONST
C        MEANS NOT ALL STATIONS COULD BE FOUND IN THE DIRECTORY.
C        THIS SHOULD BE COUNTED AS AN ISTOP(1) ERROR IN SOME
C        ROUTINES RATHER THAN AN ISTOP(2) INCREMENT FOR MISSING
C        DATA.  HOWEVER, OPTX SHOULD HAVE BEEN ENTERED FOR THIS
C        FILE PREVIOUSLY, AND IER = 120 SHOULD NOT OCCUR HERE.
      IF(IER.EQ.0)THEN
         IF(NWHERE(N).EQ.0)NWHERE(N)=3
C           WHEN DATA ARE NOT FOUND FOR THE FIRST DAY, IT MAY
C           BE UNDETERMINED WHERE THEY ARE TO COME FROM (I.E.,
C           NWHERE( ) = 0).  WHEN COMPUTED IN OPTX, IT IS NOW
C           DETERMINED.
C
D     WRITE(KFILDO,427)(ID(J,N),J=1,4),NWHERE(N),IER
D427  FORMAT(/' OUT OF OPTX IN GTVECT--(ID(J,N),J=1,4),NWHERE(N),IER',/,
D    1        '            ',3(1X,I9.9),1X,I9.3,2I6)
         GO TO 435
      ELSE
         ISTOP=ISTOP+1
C
         IF(IER.EQ.120)THEN
            IER=0
C              THIS IS JUST A MISSING STATION, NOT FATAL.
         ELSE
            IER=139
C              THE DATA ARE MISSING.  ALL VALUES HAVE BEEN RETURNED
C              AS 9999. 
            GO TO 530
         ENDIF
C
      ENDIF
C   
C        DETERMINE WHETHER OR NOT THE VARIABLE IS TO BE USED FOR
C        THE NEXT VARIABLE.  IF SO, SAVE IT IN SDATA( ).
C
 435  IF(N.EQ.NVRBL)GO TO 530
C        NO REASON TO SAVE THE LAST VARIABLE.
      IF(IER.NE.0)GO TO 530
C        MISSING DATA ARE NOT SAVED.
      IF(ISTAB.EQ.1)GO TO 530
C        DON'T SAVE A BINARY VARIABLE.
      IF(NWHERE(N+1).NE.2)GO TO 530
C        DON'T SAVE UNLESS THE NEXT VARIABLE CAN BE COMPUTED.
C        THE CHECKS BELOW ARE NECESSARY BECAUSE IT IS
C        POSSIBLE THAT NWHERE(N+1) = 2 PERTAINS TO USING
C        A PREVIOUS VARIABLE.
      IF(IDPARS(1,N).GE.400.AND.IDPARS(1,N).LE.499)GO TO 530 
C        CONSTANTS ARE NOT SAVED.  THIS IS BECAUSE RELATIVE
C        FREQUENCIES HAVE A THRESHOLD BUT B = 0.  THE THRESHOLD
C        IS NOT PART OF THE SAVE PROCESS, AND THE SAME
C        RELATIVE FREQUENCY MAY BE USED MORE THAN ONCE WHEN
C        A DIFFERENT ONE IS NEEDED.  THIS IS A SAFETY FEATURE;
C        NWHERE( ) SHOULD KEEP CONTROL OUT OF THE ABOVE TEST.       
      IF(IDPARS(1,N)/100.EQ.9)GO TO 530 
C        STRATIFICATION VARIABLES ARE NOT SAVED.
C
      IF(JD(1,N).NE.JD(1,N+1).OR.
     1   ID(2,N).NE.ID(2,N+1).OR.
     2   ID(3,N).NE.ID(3,N+1).OR.
     3   IDPARS(3,N+1).GE.5.OR.
     4   IDPARS(13,N).NE.IDPARS(13,N+1).OR.
     5   IDPARS(14,N).NE.IDPARS(14,N+1).OR.
     6   ITAU(N).NE.ITAU(N+1))GO TO 530
C
C        NOTE THAT IDPARS(3, ) GE 5 DENOTES A GRID BINARY.
C        A GRID BINARY AND A POINT BINARY CANNOT BE USED TOGETHER.
C        A GRID BINARY WOULD NEVER BE ABLE TO BE A RESTORED VARIABLE 
C
      LD(1)=JD(1,N)
      LD(2)=ID(2,N)
      LD(3)=ID(3,N)
      LD(4)=IDPARS(3,N)
      LD(5)=IDPARS(13,N)
      LD(6)=IDPARS(14,N)
      LD(7)=MDATE
C
      DO 440 K=1,NSTA
      SDATA(K)=XDATA(K)
 440  CONTINUE
C
D     WRITE(KFILDO,442)N,(LD(J),J=1,7)
D442  FORMAT(/,' SAVING DATA FOR VARIABLE NO.',I7,' IN GTVECT   ',
D    1        3(1X,I9.9),3I4,I11)
C
C        AT THIS POINT, THE VARIABLE EXISTS IN XDATA( ).  THE FULL 
C        IDENTIFICATION OF THE DATA IS IN IS1( ), IS2( ), AND IS4( ).
C
 530  IF(IER.EQ.0)GO TO 540
      IF(IPRINT.EQ.0)GO TO 540
      WRITE(KFILDO,531)(ID(J,N),J=1,4),MDATE
C        IF ANY OF THE INPUT FILES HAVE NO DATA FOR THIS DATE,
C        THIS DIAGNOSTIC IS NOT PRINTED.
 531     FORMAT(' ****CANNOT OBTAIN VARIABLE       ',
     1           I9.9,1X,I9.9,1X,I9.9,1X,I10.3,' FOR DATE ',I11,'.')
 540  RETURN
      END
