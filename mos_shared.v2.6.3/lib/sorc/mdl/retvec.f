      SUBROUTINE RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JD,ITAU,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,IER)
C
C        AUGUST   1999   GLAHN   MOS-2000
C        AUGUST   1999   GLAHN   CHANGED 4TH ARGUMENT IN CALL TO 
C                                GFETCH TO 7777
C        FEBRUARY 2000   GLAHN   CORRECTED COMMENT FOR DATA( ); SPELLING
C        APRIL    2000   GLAHN   IER = 120 RETURNED FROM CONST (FROM
C                                FINDST) SET TO ZERO BEFORE RETURN
C        MAY      2000   GLAHN   CHANGED DIAGNOSTIC FORMAT 231
C        NOVEMBER 2004   GLAHN   IMPROVED COMMENT FOR JD( )
C        FEBRUARY 2005   GLAHN   CHANGED DIMENSIONS OF KFILRA( )
C                                RACESS( ) FROM 5 TO NUMRA
C        MARCH    2006   GLAHN   COUPLE OF CHANGES TO COMMENTS
C        APRIL    2007   SMB     MODIFIED FORMAT STATEMENT 131
C                                TO CONFORM TO IBM STANDARDS.
C
C        PURPOSE
C           TO OBTAIN FOR SUBROUTINES THAT MAY BE USED IN BOTH
C           U710 AND U910 DATA THAT MAY BE IN EITHER THE INTERNAL
C           OR EXTERNAL MOS-2000 FILE SYSTEMS.  THE INTERNAL FILE
C           IS ACCESSED FIRST THROUGH GFETCH, THEN IF THE DATA
C           ARE NOT FOUND THE EXTERNAL FILE IS ACCESSED THROUGH
C           CONST.
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
C               ID(J) = THE INTEGER PREDICTOR ID'S (J=1,4).  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID( ) (J=1,15).
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
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE PORTIONS
C                       PERTAINING TO PROCESSING THAT CAN BE DONE
C                       IN VECTOR PROGRAMS ARE OMITTED:
C                       B = IDPARS(3),
C                       G = IDPARS(15), AND
C                       THRESH( ).
C                       G HAS NO MEANING IN VECTOR PROGRAMS.
C                       JD( ) IS USED TO IDENTIFY THE BASIC MODEL FIELDS
C                       AS READ FROM THE ARCHIVE.  NOT ACTUALLY USED,
C                       EVEN IN CALLED CONST.  (INPUT)
C                ITAU = THE NUMBER OF HOURS TO ADD TO NDATE TO GET 
C                       THE VARIABLE.  THIS IS THE "LOOKAHEAD" FEATURE.
C                       (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH VARIABLE IS NEEDED.
C                       (INPUT)
C               MDATE = NDATE UPDATED WITH ITAU.  (INPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST.  (CHARACTER*8)
C                       (INPUT)
C           ISDATA(K) = WORK ARRAY (K=1,ND1).  (INTERNAL)
C            XDATA(K) = THE DATA FOR THE NSTA STATIONS BEING PROCESSED
C                       (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       MUST BE LE ND5.  (INPUT)
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
C             DATA(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
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
C                              LIST IN ID( ) FOR WHICH THIS
C                              VARIABLE IS NEEDED, WHEN IT DOES NOT NEED
C                              TO BE STORED AFTER DAY 1.  WHEN THE VARIABLE
C                              MUST BE STORED (TO BE ACCESSED THROUGH OPTION)
C                              FOR ALL DAYS, ID(11) IS 7777 + THE NUMBER
C                              OF THE FIRST PREDICTOR IN THE SORTED LIST
C                              FOR WHICH THIS VARIABLE IS NEEDED.
C                       L=12 --USED INITIALLY IN ESTABLISHING MSTORE( , ).
C                              LATER USED AS A WAY OF DETERMINING WHETHER
C                              TO KEEP THIS VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ) AND MSTORE( , ).
C                       (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , ) THAT 
C                       HAVE BEEN USED IN THIS RUN.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIVE THE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL
C                       DATA ARE STORED ON DISK.  (INPUT-OUTPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NFETCH = THE NUMBER OF TIMES GFETCH HAS BEEN ENTERED.  GFETCH
C                       KEEPS TRACK OF THIS AND RETURNS THE VALUE.
C                       (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING USED
C                       (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).  (INPUT) 
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       139 = MISSING DATA FOR THIS VARIABLE.
C                       (OUTPUT)
C               MDATE = NDATE UPDATED WITH ITAU.  (INTERNAL)
C               NPACK = RETURNED FROM GFETCH.  NOT NEEDED.
C              NSOURC = RETURNED FROM GFETCH.  NOT NEEDED.
C              NTIMES = RETURNED FROM GFETCH.  NOT NEEDED.
C 
C        NONSYSTEM SUBROUTINES USED 
C            TIMPR, GFETCH, CONST
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(NUMRA)
C
      DIMENSION ISDATA(ND1),XDATA(ND1)
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION ICALLD(L3264W,ND5),IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION KFILRA(NUMRA)
C
      DATA LASTLD/99999999/,
     1     LASTDD/99999999/,
     2     NSTORD/99999999/
C
C***D     CALL TIMPR(KFILDO,KFILDO,'START RETVEC        ')
      IER=0
C
D     WRITE(KFILDO,105)(ID(J),J=1,4)
D105  FORMAT(' IN RETVEC, LOOKING FOR VRBL ',
D    1       3(1X,I9.9),1X,I10.3)
C
C        SET ITIME.
C
      ITIME=-ITAU
C
C        TRY TO FIND VARIABLE IN THE MOS-2000 INTERNAL RANDOM
C        ACCESS FILE.
C
      CALL GFETCH(KFILDO,KFIL10,ID,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,XDATA,ND1,
     2            NWORDS,NPACK,MDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSOURC,MISSP,MISSS,L3264B,ITIME,
     4            IER)
C     
      IF(IER.NE.0)THEN
D        WRITE(KFILDO,125)(ID(L),L=1,4)
D125     FORMAT(' ****VARIABLE NOT RETRIEVED BY GFETCH IN RETVEC'
D    1           2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 200
C     
      ELSEIF(NWORDS.NE.NSTA)THEN
         WRITE(KFILDO,131)NWORDS,NSTA
 131     FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1           ' NOT EQUAL TO NSTA =',I6,
     2           ' IN RETVEC.  DATA SET TO MISSING.')
         GO TO 200
      ELSE
         GO TO 240
C           THIS IS THE GOOD RETURN.
C           AT THIS POINT, THE VARIABLE EXISTS IN XDATA( ).
C           THE FULL DENTIFICATION OF THE DATA IS IN IS1( ),
C           IS2( ), AND IS4( ).
      ENDIF
C
C        LOOK FOR CONSTANT DATA, TO BE PROVIDED IN THE
C        MOS-2000 EXTERNAL RANDOM ACCESS FILES.  NOTE THAT
C        NDATE (NOT MDATE) IS USED BECAUSE THE TAU IN THE ID
C        IS USED WITH NDATE TO GET THE DATE/TIME OF THE
C        DATA WANTED.
C
 200  IF((IDPARS(1).GE.400.AND.IDPARS(1).LE.699).OR.
     1   (IDPARS(1).GE.800.AND.IDPARS(1).LE.899).OR.
     2   (IDPARS(1).GE.200.AND.IDPARS(1).LE.299))THEN
         CALL CONST(KFILDO,KFIL10,IP12,
     1              ID,IDPARS,JD,NDATE,
     2              KFILRA,RACESS,NUMRA,
     3              CCALL,ICALLD,CCALLD,
     4              ISDATA,XDATA,ND1,NSTA,
     5              IPACK,IWORK,DATA,ND5,
     6              LSTORE,ND9,LITEMS,CORE,ND10,LASTLD,
     7              NBLOCK,LASTDD,NSTORD,NFETCH,
     8              IS0,IS1,IS2,IS4,ND7,
     9              ISTAV,L3264B,L3264W,IER)
         IF(IER.EQ.0)THEN
            GO TO 240
         ELSEIF(IER.EQ.120)THEN
            IER=0
            GO TO 240
C              THE ABOVE TEST IS NECESSARY, BECAUSE FINDST
C              WILL RETURN 120 WHEN A STATION CANNOT BE FOUND
C              IN THE DIRECTORY.  THIS IS NOT FATAL, AND IER
C              IS CHANGED TO 0.
C
C              AT THIS POINT, THE VARIABLE EXISTS IN XDATA( ).
C              THE FULL DENTIFICATION OF THE DATA IS IN IS1( ),
C              IS2( ), AND IS4( ).
         ENDIF
C
      ENDIF
C
      WRITE(KFILDO,231)(ID(J),J=1,4),MDATE
 231     FORMAT(' ****CANNOT OBTAIN VARIABLE       ',
     1           I9.9,1X,I9.9,1X,I9.9,1X,I10.3,' FOR DATE ',I11,'.')
 240  RETURN
      END

