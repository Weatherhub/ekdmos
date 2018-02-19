      SUBROUTINE OPTY(KFILDO,KFIL10,KFILAO,KFILAI,IP12,IP15,
     1                KFILRA,RACESS,NUMRA,
     2                ID,IDPARS,JD,TRESHL,TRESHU,ITAU,NVRBL,
     3                NDATE,CCALL,ISDATA,SDATA,ND1,NSTA,
     4                XDATA,SDDATA,ND2,KER,ISD,SD,DS,NN,M,
     5                ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     6                LSTORE,ND9,LITEMS,CORE,ND10,
     7                NBLOCK,NFETCH,MODRUN,
     8                IS0,IS1,IS2,IS4,ND7,
     9                XAXIS,PDF,CDF,ND11,NPCDF,CDFTH,XCDF,NCDFTH,
     A                L3264B,L3264W,ISTAB,ISTOP,IER)
C 
C        FEBRUARY  2007   GLAHN     TDL   MOS-2000 
C                                   ADAPTED FROM OPTX FOR U715
C        MAY       2007   GLAHN     COMMENTS ABOUT ISD
C        NOVEMBER  2010   VEENHUIS  ADDED CALLS TO AVGPRB AND DISTAP 
C                                   SUBROUTINES.
C        FEBRUARY  2011   VEENHUIS  ADDED KFILAI TO INPUT CALL LIST.
C                                   ADDED KFILAI TO CALL TO DISTF.
C        APRIL     2011   WAGNER    ADDED MODRUN.
C
C        PURPOSE 
C            TO CALL VARIOUS SUBROUTINES TO COMPUTE THE CDF BASED 
C            ON THE ELEMENTS AND SPECIFY THE KERNEL. 
C
C        DATA SET USE 
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C            KFIL10    - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                        ACCESS.  (INPUT-OUTPUT) 
C            KFILAO    - UNIT NUMBER OF ASCII OUTPUT FILE.  ZERO MEANS
C                        OUTPUT WILL NOT BE WRITTEN.  (OUTPUT)
C            IP12      - LIST OF STATIONS ON THE INPUT FILES.  (OUTPUT)
C            IP15      - LIST OF DATA IN DIST.  (OUTPUT)
C            KFILRA(J) - UNIT NUMBERS FOR EXTERNAL RANDOM ACCESS FILES
C                        (J=1,5).  (INPUT)
C            KFILAI    - UNIT NUMBER OF SPREAD-SKILL INPUT ASCII FILE(INTERNAL)
C 
C        VARIABLES 
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C              KFILAO = UNIT NUMBER OF ASCII OUTPUT FILE.
C                       ZERO MEANS OUTPUT WILL NOT BE WRITTEN.  (INPUT)
C                IP12 = INDICATES WHETHER (>0) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE EXTERNAL RANDOM ACCESS FILES
C                       WILL BE LISTED TO UNIT IP12.  (INPUT)
C                IP15 = LIST OF DATA IN DIST.  (INPUT)
C           KFILRA(J) = THE UNIT NUMBERS FOR THE MOS-2000 EXTERNAL
C                       RANDOM ACCESS FILES (J=1,NUMRA)
C           RACESS(J) = THE FILE NAME FOR THE MOS-2000 EXTERNAL RANDOM
C                       ACCESS FILE (J=1,NUMRA).  (CHARACTER*60)
C               NUMRA = THE NUMBER OF VALUES IN KFILRA( ) AND RACESS( ).
C                       (INPUT)
C             ID(J,N) = THE VARIABLE ID (J=1,4) (N=1,NVRBL).  (INPUT)
C         IDPARS(J,N) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       VARIABLE ID CORRESPONDING TO ID( ) (J=1,15)
C                       (N=1,NVRBL).
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
C                       (INPUT)
C             JD(J,N) = THE BASIC INTEGER VARIABLE ID (J=1,4)
C                       (N=1,NVRBL).  THIS IS THE SAME AS ID(J), EXCEPT
C                       THAT THE FOLLOWING PORTIONS ARE OMITTED:
C                       B = IDPARS(3),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       (INPUT)
C           TRESHL(N) = THE LOWER BINARY THRESHOLD ASSOCIATED WITH 
C                       IDPARS( ,N) (N=1,NVRBL).  (INPUT)
C           TRESHU(N) = THE UPPER BINARY THRESHOLD ASSOCIATED WITH
C                       IDPARS( ,N) (N=1,NVRBL).  (INPUT)
C                ITAU = THE NUMBER OF HOURS AHEAD TO FIND A VARIABLE.
C                       THIS DOES NOT APPLY TO ALL SUBROUTINES.
C                       NO PRESENT USE; SHOULD BE ZERO.  (INPUT)
C               NVRBL = THE NUMBER OF VARIABLES IN ID( , ), ETC.
C                       (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH VARIABLE IS NEEDED.
C                       (INPUT)
C              MODRUN - USED TO SET THE OUTPUT DD. READ FROM CONTROL FILE.
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST.  (CHARACTER*8)
C                       (INPUT)
C           ISDATA(K) = WORK ARRAY (K=1,ND1). (INTERNAL)
C            SDATA(K) = WORK ARRAY (K=1,ND1).  (INTERNAL)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                       WITH.  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C          XDATA(K,L) = THE ARRAY USED BY SUBROUTINE DISTF FOR
C                       VECTOR VALUES (K=1,ND1) (L=1,ND2).  (INTERNAL)
C         SDDATA(K,L) = USED FOR THE STANDARD ERRORS (K=1,ND1)
C                       (L=1,ND2).
C                 ND2 = MAXIMUM NUMBER OF ENSEMBLE MEMBERS.
C              KER(N) = DESIGNATES THE KERNAL TO BE USED FOR VARIABLE N
C                       (N=1,ND4).
C                       1 = NORMAL (GAUSSIAN).
C                       (INPUT)
C              ISD(N) = DESIGNATES WHETHER THE KERNAL WIDTH FOR
C                       VARIABLE N (N=1,ND4) IS TO BE TAKEN FROM SD( )
C                       IN THE VARIABLE RECORD OR FROM A PACKED INPUT
C                       RECORD.
C                       0 = COMES FROM PACKED RECORD;
C                       1 = USE SD( ) READ WITH THE VARIABLE.
C                       2 = WHEN THERE ARE MULTIPLE ENSEMBLES OR
C                           ONLY ONE ENSEMBLE AND SD( ) > 10, 
C                           CALL KERNELW; OTHERWISE, CALL KERNEL.
C                       (INPUT)
C               SD(N) = THE CONSTANT KERNAL WIDTH FOR THIS VARIABLE N
C                       (N=1,ND4) WHEN ISD(N) = 1.  (INPUT)
C               DS(N) = SCALING FACTOR FOR THIS VARIABLE (N=1,ND4).
C                       (INPUT)      
C                  NN = ON INPUT, THE FIRST VARIABLE IN THE ID LIST NOT
C                       ALREADY USED.  THIS IS THE VARIABLE TO PROCESS.
C                       ON OUTPUT, THE FIRST VARIABLE IN THE ID LIST NOT
C                       USED.  WHEN ALL VARIABLES HAVE BEEN PROCESSED,
C                       NN IS RETURNED = 9999.  (INPUT/OUTPUT)
C                   M = THE NUMBER OF MEMBERS AVERAGED IN DISTF.
C                       (OUTPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN 
C                       INTEGER VARIABLE (L=1,L3264W) (K=1,ND1).
C                       NOTE THAT THIS REQUIRES TWO 32-BIT WORDS TO HOLD
C                       THE DESCRIPTION BUT ONLY ONE 64-BIT WORD.
C                       NEEDED IN CONST FOR ARGUMENT TO RDTDLM.
C                       EQUIVALENCED TO CCALLD( ).
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).  EQUIVALENCED
C                       TO ICALLD( , ).  (INTERNAL)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ), WORK( ), DATA( ), AND
C                       CALLD( ), AND SECOND DIMENSION OF ICALLD( , ).
C                       (INPUT)
C         LSTORE(L,J) = THE ARRAY TO HOLD INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS).
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK, 
C                              THIS IS MINUS THE RECORD NUMBER WHERE 
C                              THE DATA START.  NOTE THAT WHEN A FIELD 
C                              CANNOT BE STORED IN CORE( ), IT IS PUT
C                              ON DISK.  IT MAY BE THAT A LATER FIELD 
C                              WILL FIT, AND IT IS PUT IN CORE( ).
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDLPACK, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --THE NUMBER IN THE LIST OF INPUT SOURCES
C                              THIS VARIABLE CAME FROM.
C                       L=11 --FOR U715, THIS WILL BE 7777, INDICATING
C                              THE VARIABLE IS ALWAYS STORED IN THE
C                              INTERNAL STORAGE FACILITY.
C                       L=12 --MINUS THE NUMBER OF HOURS THIS VARIABLE
C                              MUST BE KEPT.  LATER SET TO A DATE WHEN
C                              THIS VARIABLE CAN BE DISCARDED.
C                       (INPUT)
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       FILLED.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE( , ) (J=1,ND10).  WHEN
C                       CORE( ) IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NFETCH = THE NUMBER OF TIMES GFETCH HAS BEEN ENTERED.
C                       GFETCH KEEPS TRACK OF THIS AND RETURNS THE
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
C            XAXIS(J) = THE DATA VALUES ALONG THE X-AXIS, EACH
C                       VALUE CORRESPONDING TO A VALUE IN PDF(J) AND
C                       CDF(J) (J=1,NPCDF).  (OUTPUT) 
C              PDF(J) = THE PDF WRITTEN TO UNIT KFILAO (J=1,NPCDF).
C                       (OUTPUT)
C              CDF(J) = THE CDF WRITTEN TO UNIT KFILAO (J=1,NPCDF).
C                       (OUTPUT) 
C                ND11 = THE MAXIMUM NUMBER OF VALUES IN XAXIS( ),
C                       PDF( ), AND CDF( ).  (INPUT)
C               NPCDF = THE NUMBER OF VALUES IN PDF( ), CDF( ), AND
C                       XAXIS( ).  (OUTPUT)
C            CDFTH(J) = THE THRESHOLDS, OR PROBABILITY LEVELS, FOR
C                       OUTPUTTING THE CDF VALUES (J=1,NCDFTH).
C                       (INPUT)
C           XCDF(K,J) = THE VALUES FOR STATION K (K=1,NSTA) OF THE CDF
C                       FOR EACH OF THE LEVELS IN CDFTH(J) (J=1,NCDFTH)
C                       (OUTPUT)
C              NCDFTH = NUMBER OF VALUES IN CDFTH( ) AND XCDF( ).
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).  
C                       CALCULATED BY PARAMETER, BASED ON L3464B.
C                       (INPUT)
C               ISTAB = USUALLY  ZERO, BUT SET TO ONE IN CERTAIN
C                       SUBROUTINES.  (OUTPUT)
C            ISTOP(J) = FOR J=1, ISTOP IS INCREMENTED BY 1 EACH TIME
C                       AN ERROR OCCURS THAT MAY BE FATAL.
C                       FOR J=2, ISTOP IS INCREMENTED BY 1 WHENEVER AN
C                       INPUT DATA RECORD IS NOT FOUND.
C                       FOR J=3, ISTOP IS INCREMENTED BY 1 WHENEVER AN
C                       AN UNUSUAL CIRCUMSTANCE OCCURS WHICH IS NOT FATAL.
C                       (INPUT/OUTPUT)
C                 IER = STATUS RETURN.
C                        0 = GOOD RETURN.
C                       99 = KER( ) COULD NOT BE RECOGNIZED IN OPTY.
C                       SEE CALLED ROUTINES FOR OTHER VALUES.
C                       (INTERNAL-OUTPUT)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES USED 
C            DISTF, DISTAP, AVGPRB
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION ISDATA(ND1),SDATA(ND1)
      DIMENSION XDATA(ND1,ND2),SDDATA(ND1,ND2)
      DIMENSION ID(4,NVRBL),IDPARS(15,NVRBL),
     1          TRESHL(NVRBL),TRESHU(NVRBL),JD(4,NVRBL),
     2          ITAU(NVRBL),
     3          KER(NVRBL),ISD(NVRBL),SD(NVRBL),DS(NVRBL)
      DIMENSION ICALLD(L3264W,ND5),IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION CDFTH(NCDFTH),XCDF(ND1,NCDFTH)
      DIMENSION XAXIS(ND11),PDF(ND11),CDF(ND11)
      DIMENSION KFILRA(5),ISTOP(3)
C
      IER=0
      ISTAB=0
C        ISTAB IS RETURNED AS 0 (NON BINARY) UNLESS SET OTHERWISE.
C
D     WRITE(KFILDO,100)(ID(J,NN),J=1,4),(IDPARS(J,NN),J=1,15),
D    1                  L3264W,NSTA,NN,KER(NN),IER
D100  FORMAT(' IN OPTY, ID, IDPARS, L3264W, NSTA, NN, KER(NN), IER',/,
D    1       '    ',4I10,15I5,5I4)
C
      IF(KER(NN).EQ.1)THEN
         CALL DISTF(KFILDO,KFIL10,KFILAO,KFILAI,IP12,IP15,
     1              KFILRA,RACESS,NUMRA,
     2              ID,IDPARS,JD,TRESHL,TRESHU,ITAU,NVRBL,
     3              NDATE,CCALL,ISDATA,SDATA,ND1,NSTA,
     4              XDATA,SDDATA,ND2,KER,ISD,SD,DS,NN,M,
     5              ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     6              LSTORE,ND9,LITEMS,CORE,ND10,
     7              NBLOCK,NFETCH,MODRUN,
     8              IS0,IS1,IS2,IS4,ND7,
     9              XAXIS,PDF,CDF,ND11,NPCDF,CDFTH,XCDF,NCDFTH,
     A              L3264B,L3264W,ISTOP,IER)
C
         IF(IER.EQ.0.OR.
     1      IER.EQ.120.OR.
     2      IER.EQ.777)GO TO 300
C               ER = 777 FROM DISTF WHEN SD = 0 FOUND.
C
C
      ELSEIF(KER(NN).EQ.2)THEN
         CALL DISTAP(KFILDO,KFIL10,KFILAO,IP12,IP15,
     1              KFILRA,RACESS,NUMRA,
     2              ID,IDPARS,JD,TRESHL,TRESHU,ITAU,NVRBL,
     3              NDATE,CCALL,ISDATA,SDATA,ND1,NSTA,
     4              XDATA,SDDATA,ND2,KER,ISD,SD,DS,NN,M,
     5              ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     6              LSTORE,ND9,LITEMS,CORE,ND10,
     7              NBLOCK,NFETCH,
     8              IS0,IS1,IS2,IS4,ND7,
     9              XAXIS,PDF,CDF,ND11,NPCDF,CDFTH,XCDF,NCDFTH,
     A              L3264B,L3264W,ISTOP,IER)
C
         IF(IER.EQ.0.OR.
     1      IER.EQ.120.OR.
     2      IER.EQ.777)GO TO 300
C
      ELSEIF(KER(NN).EQ.3)THEN
         CALL AVGPRB(KFILDO,KFIL10,KFILAO,IP12,IP15,
     1               KFILRA,RACESS,NUMRA,
     2               ID,IDPARS,JD,TRESHL,TRESHU,ITAU,NVRBL,
     3               NDATE,CCALL,ISDATA,SDATA,ND1,NSTA,
     4               XDATA,SDDATA,ND2,KER,NN,M,
     5               ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     6               LSTORE,ND9,LITEMS,CORE,ND10,
     7               NBLOCK,NFETCH,
     8               IS0,IS1,IS2,IS4,ND7,
     9               L3264B,L3264W,ISTOP,IER)
        IF(IER.EQ.0) THEN
          GO TO 300
        ENDIF
C
      ELSEIF(KER(NN).EQ.4)THEN
C         CALL DISTGW(KFILDO,KFIL10,KFILAO,KFILAI,IP12,IP15,
         CALL DISTWB(KFILDO,KFIL10,KFILAO,KFILAI,IP12,IP15,
     1               KFILRA,RACESS,NUMRA,
     2               ID,IDPARS,JD,TRESHL,TRESHU,ITAU,NVRBL,
     3               NDATE,CCALL,ISDATA,SDATA,ND1,NSTA,
     4               XDATA,SDDATA,ND2,KER,ISD,SD,DS,NN,M,
     5               ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     6               LSTORE,ND9,LITEMS,CORE,ND10,
     7               NBLOCK,NFETCH,MODRUN,
     8               IS0,IS1,IS2,IS4,ND7,
     9               XAXIS,PDF,CDF,ND11,NPCDF,CDFTH,XCDF,NCDFTH,
     A               L3264B,L3264W,ISTOP,IER)
        IF(IER.EQ.0) THEN
          GO TO 300
        ENDIF
C
      ELSE
C      
         IER=99
C           IER = 99 AT THIS POINT INDICATES THE KER COULD
C           NOT BE RECOGNIZED.
      ENDIF
C
C        AT THIS POINT,
C           IER = 0, GOOD RETURN FROM ONE OF THE ABOVE.
C           IER = 99, KER COULD NOT BE RECOGNIZED ABOVE.
C           IER = 120  ENTERED ROUTINE, FOUND DATA, BUT
C                     JUST A STATION MISSING IN DIRECTORY.
C                     (THIS WILL LIKELY NOT OCCUR HERE
C                     BECAUSE THE FINDST IER = 120, 
C                     CALLED FROM CONST, IS SET = 0 IN
C                     RETVEC; KEEP THE TEST FOR 120 AS
C                     WELL AS 0 FOR SAFETY.)
C           IER = SOME OTHER VALUE, FOUND IDS, BUT ERROR
C                 OF SOME SORT.  SPECIFICALLY, IER = 777
C                 FROM DISTF WHEN SD = 0 FOUND.
C
D     IF(IER.EQ.99)THEN
D        WRITE(KFILDO,198)(ID(J,NN),J=1,4),NDATE
D198     FORMAT(/,' ****VRBL NOT IDENTIFIED IN OPTY  ',
D    1           I9.9,1X,I9.9,1X,I9.9,1X,I10.3,' FOR DATE ',I11,'.')
D     ELSE
D       WRITE(KFILDO,1980)(ID(J,NN),J=1,4),NDATE
D1980    FORMAT(/,' ****VRBL NOT COMPUTED   IN OPTY  ',
D    1           I9.9,1X,I9.9,1X,I9.9,1X,I10.3,' FOR DATE ',I11,'.')      
D     ENDIF
C
      DO 200 K=1,NSTA
      XDATA(K,1)=9999.
 200  CONTINUE
C
 300  RETURN
      END 
