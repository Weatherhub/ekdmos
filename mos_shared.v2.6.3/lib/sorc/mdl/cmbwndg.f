      SUBROUTINE CMBWNDG(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                   IDPARS,JD,ITAU,
     2                   NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NSTA,
     3                   ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                   LSTORE,ND9,LITEMS,CORE,ND10,
     5                   NBLOCK,NFETCH,
     6                   IS0,IS1,IS2,IS4,ND7,
     7                   L3264B,L3264W,IER)
C 
C        JANUARY    2007   RUDACK   MDL   MOS-2000
C        JANUARY    2007   RLC      MADE SOME CHANGES TO COMMENTS.
C                                   COMMENTED OUT CHECK FOR CASES
C                                   WHERE GUST < SPEED.  
C
C        PURPOSE 
C            TO MERGE THE WIND GUST AND WIND SPEED FORECASTS AT STATIONS 
C            TO BE USED AS INPUT TO THE GMOS ANALYSIS CODE.  SET THE 
C            STATION TO THE GUST IF IT IS NON-ZERO, OTHERWISE SET 
C            IT TO THE WIND SPEED.  IF THE WIND SPEED OR WIND GUST 
C            FORECAST IS MISSING, A MISSING OUTPUT VALUE IS ASSIGNED 
C            TO THAT STATION.  
C
C        THE FOLLOWING CCCFFF IS ACCOMMODATED AND MAPPED FROM
C                204385 <-------- 204335 AND 204380  
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
C            XDATA(K) = POST PROCESSED WIND SPEED.  THIS VALUE CAN EITHER
C                       BE A MOS WIND SPEED OR WIND GUST FORECAST 
C                       (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF XDATA( ) AND ISDATA( ).
C                       (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN 
C                       INTEGER VARIABLE (L=1,L3264W) (K=1,ND5).
C                       THIS ARRAY IS USED TO READ THE STATION 
C                       DIRECTORY FROM A MOS-2000 EXTERNAL FILE.
C                       EQUIVALENCED TO CCALLD( ).  (CHARACTER*8) 
C                       (INTERNAL)
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5). 
C                       EQUIVALENCED TO ICALLD( , ).  (INTERNAL)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
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
C
C     INTERNAL VARIABLES:
C
C            ZDATA(K) = ADJUSTED MOS WIND SPEED FORECASTS IN KTS (K=1,NSTA).  
C                       (INTERNAL) (AUTOMATIC ARRAY)
C            FDATA(K) = ADJUSTED MOS WIND GUST FORECASTS IN KTS (K=1,NSTA).  
C                       (INTERNAL) (AUTOMATIC ARRAY)
C               MD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       ZDATA( ) (MOS WIND SPEED FORECASTS), 
C                       FDATA( ) (MOS WIND GUST FORECASTS) (J=1,4).
C                       (INTERNAL)
C           MDPARS(J) = PARSED VALUES CORRESPONDING TO MD( ) (J=1,15)
C                       (INTERNAL)
C           ITABLE(J) = VALUES OF CCCFFF ACCOMMODATED (J=1) AND
C                       THE ASSOCIATED VARIABLE NEEDED (J=2,3).
C                       (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            PRSID1, RETVEC
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION XDATA(ND1),ISDATA(ND1)
      DIMENSION ZDATA(ND1),FDATA(ND1)
C               (AUTOMATIC ARRAYS)
      DIMENSION IDPARS(15),MDPARS(15)
      DIMENSION JD(4),MD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10),ICALLD(L3264W,ND5)
      DIMENSION KFILRA(5)
      DIMENSION ITABLE(3)
C
C        DATA TABLE
C
      DATA MDPARS/15*0/  
      DATA ITABLE/204385, 204335, 204380/
C
      IER=0
C
C        FIND CCCFFF OF JD(1) IN ITABLE(1, ).
C
      IF(ITABLE(1).EQ.IDPARS(1)*1000+IDPARS(2)) GO TO 120
C
      WRITE(KFILDO,107)(JD(L),L=1,4)
 107  FORMAT(/,' ****CMBWNDG ENTERED FOR VARIABLE',
     1         2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2       ' NOT ACCOMMODATED.')
      IER=102
      GO TO 300
C
C        FILL MD( ) AND MDPARS( ).
C
 120  MD(1)=ITABLE(2)*1000+IDPARS(4)
      MD(2)=JD(2)
      MD(3)=JD(3)
      MD(4)=0
C
C        RETRIEVE THE MOS ADJUSTED WIND SPEED FORECASTS.
C
      CALL PRSID1(KFILDO,MD,MDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            MD,MDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,ZDATA,ND1,NSTA,
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
         WRITE(KFILDO,125)(MD(J),J=1,4)
  125    FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN CMBWNDG',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 300
      ENDIF
C
C        FILL MD( ) AND MDPARS( ).
C
      MD(1)=ITABLE(3)*1000+IDPARS(4)
      MD(2)=JD(2)
      MD(3)=JD(3)
      MD(4)=0
C
C        RETRIEVE THE ADJUSTED MOS WIND GUST FORECASTS.
C
      CALL PRSID1(KFILDO,MD,MDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            MD,MDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,FDATA,ND1,NSTA,
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
         WRITE(KFILDO,135)(MD(J),J=1,4)
  135    FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN CMBWNDG',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 300
      ENDIF
C
C        IF A MOS WIND GUST IS FORECASTED,  SET THE WIND GUST
C        TO XDATA( ).  IF A WIND GUST IS NOT FORECASTED (I.E.,
C        FDATA( ) = 0), SET XDATA( ) TO THE WIND SPEED.  IF
C        EITHER THE WIND GUST OR WIND SPEED FORECAST IS MISSING
C        FOR A PARTICULAR STATION, SET XDATA( ) TO MISSING.
C    
      DO 211 K=1,NSTA
         IF((NINT(ZDATA(K)).NE.9999).AND.
     1      (NINT(FDATA(K)).NE.9999)) THEN
C
C            COMMENTED OUT THE FOLLOWING FOR NOW BECAUSE WE DO NOT
C            BELIEVE IT COULD HAPPEN:
C              GUARD THAT THE MOS WIND SPEED FORECAST DOES NOT
C              EXCEED THE MOS WIND GUST FORECAST.  IF THIS OCCURS,
C              SET THE MOS WIND SPEED TO THE WIND GUST SPEED.
C 
C           IF(ZDATA(K).GT.FDATA(K)) FDATA(K)=ZDATA(K)
C
            IF(NINT(FDATA(K)).NE.0) THEN
               XDATA(K)=FDATA(K)
            ELSE
               XDATA(K)=ZDATA(K)
            ENDIF
         ELSE
            XDATA(K)=9999.
         ENDIF
 211  CONTINUE
C
      GO TO 350   
C
C        THIS VARIABLE CANNOT BE COMPUTED.  SET XDATA( ) TO MISSING.
C        THIS IS FOR SAFETY; XDATA( ) SHOULD ALREADY BE SET TO MISSING.
C
 300  DO 310 K=1,NSTA
         XDATA(K)=9999.
 310  CONTINUE 
C
 350  RETURN
      END     
