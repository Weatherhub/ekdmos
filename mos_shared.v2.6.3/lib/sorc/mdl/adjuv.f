      SUBROUTINE ADJUV(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JD,ITAU,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,IER)
C 
C        DEC   2004  COSGROVE NEW ROUTINE
C
C        PURPOSE 
C            TO CALCULATE THE U AND V WINDS FROM THE WIND
C            SPEED AND DIRECTION AFTER POST-PROCESSING.
C            IF THE WIND SPEED VALUES ARE ZERO, U AND V ARE
C            SET TO ZERO.  THIS ROUTINE FOLLOWS THE ADJUSTMENT
C            OF THE WIND TO 10 M.
C            IDPARS (1) AND IDPARS(2) ARE
C            MAPPED  INTO        FROM 
C                    204020      204010 (SFC. U WIND)
C                    204120      204110 (SFC. V WIND)
C
C            POSSIBLE MISSING VALUES OF 9999. ARE TREATED AS 
C            MISSING = 9999.  VECTOR DATA FROM THE MOS-2000 INTERNAL
C            STORAGE SYSTEM WILL BE UNPACKED, AND WILL BE IN THE ORDER
C            NEEDED.  ADJUV ASSUMES THERE WILL BE NO SECONDARY
C            MISSING VALUES OF 9997; THESE SHOULD HAVE ALREADY BEEN
C            TREATED.
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
C           RACESS(J) = THE FILE NAMES ASSOCIATED WITH KFILRA(J)
C                       (J=1,NUMRA).
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
C            XDATA(K) = WIND COMPONENT, MODIFIED AS NECESSARY, IS RETURNED 
C                       (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF XDATA( ) AND ISDATA( ). 
C                       (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN INTEGER
C                       VARIABLE (L=1,L3264W) (K=1,ND5).  THIS ARRAY IS USED 
C                       TO READ THE STATION DIRECTORY FROM A MOS-2000
C                       EXTERNAL FILE.  EQUIVALENCED TO CCALLD( ). 
C                       (CHARACTER*8)  (INTERNAL)
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
C                       XDATA( ).  (INTERNAL)
C            YDATA(K) = HOLDS THE WIND SPEED (K=1,NSTA)  (INTERNAL)
C            ZDATA(K) = HOLDS THE WIND DIRECTION (K=1,NSTA)  (INTERNAL)
C         ITABLE(I,J) = VALUES OF IDPARS(2) FOR I= : 
C                        1 = ID REQUESTED (ADJUV)
C                        2 = ID OF U- OR V-WIND
C                        3 = ID OF WIND SPEED AFTER 10M ADJ
C                        4 = ID OF WIND DIRECTION AFTER 10M ADJ
C                       (J=1,NDIM).  (INTERNAL)
C                NDIM = SECOND DIMENSION OF ITABLE( , ).  SET BY
C                       PARAMETER.  (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            PRSID1,RETVEC
C
C
      REAL PI,RADPDG
C
      PARAMETER (PI=3.1416,
     1           RADPDG=PI/180.)
      PARAMETER (NDIM=2)
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION ISDATA(ND1),XDATA(ND1),YDATA(ND1),ZDATA(ND1)
C        YDATA( ) IS AN AUTOMATIC ARRAY
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION LD(4),KFILRA(5)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9),ICALLD(L3264W,ND5)
      DIMENSION CORE(ND10)
      DIMENSION ITABLE(4,NDIM),LDPARS(15)
C
      DATA LDPARS/15*0/
C                          U/V    SPEED   DIR
      DATA ITABLE/204020, 204010, 204335, 204235,
     1            204120, 204110, 204335, 204235/
C
      IER=0
C
C        GET THE IDS FOR THE FIELD TO RETRIEVE.
C
      DO 115 JJ=1,NDIM
      IF(ITABLE(1,JJ).EQ.IDPARS(1)*1000+IDPARS(2)) GO TO 120
 115  CONTINUE
C
      WRITE(KFILDO,117)(JD(L),L=1,4)
 117  FORMAT(/,' ****ADJUV ENTERED FOR VARIABLE',
     1         2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2       ' NOT ACCOMMODATED.')
      IER=102
      GO TO 300
C
C        FETCH THE WIND SPEED.  THIS IS THE SPEED AFTER IT
C        HAS BEEN CORRECTED FOR NEGATIVE WIND SPEEDS IN ZRONEG
C        AND THE HEIGHT ADJUSTED TO 10M IN ADJSPD
C
 120  LD(1)=ITABLE(3,JJ)*1000+IDPARS(4)
      LD(2)=JD(2)
      LD(3)=JD(3)
      LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
C      
C        LD( ) IS IN BASIC PREDICTOR FORMAT.
C
      CALL PRSID1(KFILDO,LD,LDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            LD,LDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,YDATA,ND1,NSTA,
     3            ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4            LSTORE,ND9,LITEMS,CORE,ND10,
     5            NBLOCK,NFETCH,
     6            IS0,IS1,IS2,IS4,ND7,
     7            L3264B,L3264W,IER)
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,125)(LD(J),J=1,4)
  125    FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN ADJUV',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 300
      ENDIF
C
C        FETCH THE WIND DIRECTION.  THIS IS THE DIR AFTER IT
C        HAS BEEN ADJUSTED TO 10M AND CORRECTED FOR CALM WINDS. 
C
      LD(1)=ITABLE(4,JJ)*1000+IDPARS(4)
      LD(2)=JD(2)
      LD(3)=JD(3)
      LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
C      
C        LD( ) IS IN BASIC PREDICTOR FORMAT.
C
      CALL PRSID1(KFILDO,LD,LDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            LD,LDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,ZDATA,ND1,NSTA,
     3            ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4            LSTORE,ND9,LITEMS,CORE,ND10,
     5            NBLOCK,NFETCH,
     6            IS0,IS1,IS2,IS4,ND7,
     7            L3264B,L3264W,IER)
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,125)(LD(J),J=1,4)
         GO TO 300
      ENDIF
C
C
C        FETCH THE U- OR V-WIND COMPONENT. 
C
      LD(1)=ITABLE(2,JJ)*1000+IDPARS(4)
      LD(2)=JD(2)
      LD(3)=JD(3)
      LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
C      
C        LD( ) IS IN BASIC PREDICTOR FORMAT.
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
      IF(IER.NE.0)THEN
         WRITE(KFILDO,225)(LD(J),J=1,4)
  225    FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN ADJUV',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 300
      ENDIF
C
C        IF THE WIND SPEED IS ZERO AND NOT MISSING SET THE U-
C        OR V-WIND COMPONENT TO ZERO.  OTHERWISE, RECALCULATE
C        THE U- OR V-COMPONENT.
C
      DO 250 K=1,NSTA
         IF(NINT(XDATA(K)).EQ.9999.OR.NINT(YDATA(K)).EQ.9999.OR.
     1      NINT(ZDATA(K)).EQ.9999)THEN
             XDATA(K)=9999. 
         ELSEIF((NINT(YDATA(K)).EQ.0.).AND.(NINT(XDATA(K)).NE.9999))THEN 
            XDATA(K)=0.
         ELSE
C
C                 CHECK/CALCULATE THE U-COMPONENT. 
C
            IF(ITABLE(1,JJ).EQ.204020) THEN
C
               XDATA(K)=-1.*YDATA(K)*SIN(ZDATA(K)*RADPDG)
C
C                 MUST BE THE V-COMPONENT TO CALCULATE.
C
            ELSE 
               XDATA(K)=-1.*YDATA(K)*COS(ZDATA(K)*RADPDG)
            ENDIF
C
         ENDIF
 250  CONTINUE
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
