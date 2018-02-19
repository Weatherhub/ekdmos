      SUBROUTINE IFRCAT(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JD,ITAU,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,IER)
C 
C        MAY       2005   RUDACK  MDL   MOS-2000
C
C        PURPOSE
C            TO PLACE ALL POSSIBLE FLIGHT CATEGORIES INTO BINS RANGING FROM
C            VALUES OF 1 TO 5.  THE FLIGHT CATEGORIES ARE BASED ON THE FOLLOWING
C            CRITERIA:  
C
C            FLIGHT CATEGORY    CEILING(FEET)               VISIBILITY(STATUTE MILES)
C            ---------------    -------------               -------------------------                 
C              1 - VLIFR        LT 200 AND/OR               LT 1/2 SM
C              2 - LIFR         GTE 200 TO LT 500 AND/OR    GTE 1/2 TO LT 1 SM
C              3 - IFR          GTE 500 TO LT 1000 AND/OR   GTE 1 TO LT 3 SM
C              4 - MVFR         GTE 1000 TO LTE 3000 AND/OR GTE 3 TO LTE 5 SM
C              5 - VFR          GT 3000 AND                 GT 5 SM
C
C            IN ADDITION, THIS SUBROUTINE WILL IDENTIFY ALL IFR CONDITIONS
C            EXCLISIVELY WITH A BINARY VALUE OF ONE.  ALL OTHER CONDITIONS
C            ARE SET TO VALUE OF ZERO.
C            
C            THIS SUBROUTINE IS DESIGNED TO PROCESS OBSERVATIONS 
C            AS WELL AS MOS OR LAMP CATEGORICAL FORECASTS.  IT IS
C            ASSUMED THAT IF OBSERVATIONAL DATA IS TO BE PROCESSED,
C            THE OBSERVED CEILING HEIGHT HAS ALREADY BEEN
C            CALCULATED IN U201 AND IS INPUT TO THIS SUBROUTINE.
C
C            IDPARS(1) & IDPARS(2) OF VISIBILITY AND CEILING HEIGHT ARE MAPPED 
C            INTO      FROM
C            708210    708100 AND 708000  (OBSERVATIONS-{IFR-LIFR-VLIFR})
C            708223    708100 AND 708000  (OBSERVATIONS-{IFR})
C            208211    208131 AND (208071 OR 208051)  (CATEGORICAL FORECASTS{IFR-LIFR-VLIFR})
C            208223    208131 AND (208071 OR 208051)  (CATEGORICAL FORECASTS-{IFR})
C
C            NOTE:  OBSERVED VISIBILITY IS PROCESSED IN TERMS OF STATUTE
C            MILES AND OBSERVED CEILING HEIGHT IS PROCESSED IN TERMS OF
C            HUNDREDS OF FEET.  OBSERVED UNLIMITED CEILING HEIGHT IS
C            CODED AS "888". 
C
C            CATEGORICAL FORECASTS OF VISIBILITY AND CEILING HEIGHT ARE 
C            PROCESSED ACCORDING TO THE FOLLOWING DEFINITIONS:
C
C            VISIBILITY:       CATEGORY       DISTANCE 
C                              --------       ---------
C                                 1           < 1/2 STATUTE MILES (VLIFR)
C                                 2           1/2 - < 1 STATUTE MILES (LIFR)
C                                 3           1 - < 2 STATUTE MILES (IFR)
C                                 4           2 - < 3 STATUTE MILES (IFR)
C                                 5           3 - 5 STATUTE MILES (MVFR)
C                                 6           6 STATUTE MILES (VFR)
C                                 7           > 6 STATUTE MILES (VFR)
C
C            CEILING HEIGHT:   CATEGORY       HEIGHT 
C                              --------       ---------
C                                 1           < 200 FEET (VLIFR)      
C                                 2           200-400 FEET (LIFR)
C                                 3           500-900 FEET (IFR)
C                                 4           1000-1900 FEET (MVFR)
C                                 5           2000-3000 FEET (MVFR)
C                                 6           3100-6500 FEET (VFR)
C                                 7           6600-12000 FEET (VFR)
C                                 8           > 12000 FEET OR UNLIMITED CEILING (VFR)
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
C            XDATA(K) = HOLDS THE OBSERVATION OF VISIBILITY OR CATEGORICAL
C                       VISIBILITY FORECAST (INTERNAL), THEN FLIGHT CATEGORY
C                       VALUE (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF XDATA( ), ISDATA( ) AND
C                       YDATA( ).  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN
C                       INTEGER VARIABLE (L=1,L3264W) (K=1,ND5).
C                       THIS ARRAY IS USED TO READ THE STATION DIRECTORY
C                       FROM A MOS-2000 EXTERNAL FILE.  EQUIVALENCED 
C                       TO CCALLD( ).  (CHARACTER*8)  (INTERNAL)
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
C                       XDATA( ) AND YDATA( ) (J=1,4).  (INTERNAL)
C           LDPARS(J) = PARSED VALUES CORRESPONDING TO LD( ) (J=1,15)
C                       (INTERNAL)
C            YDATA(K) = WORK ARRAY FOR HOLDING CEILING HEIGHT OBSERVATIONS
C                       OR CEILING HEIGHT CATEGORICAL FORECASTS (K=1,ND1).  
C                       (AUTOMATIC)
C         ITABLE(I,J) = VALUES OF CCCFFF ACCOMMODATED (I=1) AND
C                       THE ASSOCIATED VARIABLES NEEDED (I=2,3)
C                       (J=1,NDIM).  (INTERNAL)
C                NDIM = SECOND DIMENSION OF ITABLE( , ).  SET BY
C                       PARAMETER.  (INTERNAL)
C                JERR = TOTAL NUMBER OF ERRORS ENCOUNTERED.  IF
C                       JERR=2, PROCESSING CANNOT BE DONE.
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            RETVEC,PRSID1
C
      PARAMETER(NDIM=4)
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION XDATA(ND1),ISDATA(ND1),YDATA(ND1)
C        YDATA( ) IS AN AUTOMATIC ARRAY.
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10),ICALLD(L3264W,ND5)
      DIMENSION LD(4),LDPARS(15),KFILRA(5)
      DIMENSION ITABLE(4,NDIM)
C  
      DATA LDPARS/15*0/  
      DATA ITABLE/708210,708100,708000,0,
     1            708223,708100,708000,0,
     2            208211,208131,208071,208051,
     3            208223,208131,208071,208051/
C
      IER=0
      JERR=0
C
C        FIND CCCFFF OF ID(1) IN ITABLE(1, ).
C
      DO 105 JJ=1,NDIM
      IF(ITABLE(1,JJ).EQ.IDPARS(1)*1000+IDPARS(2)) GO TO 120
 105  CONTINUE
C
      WRITE(KFILDO,107)(JD(L),L=1,4)
 107  FORMAT(/,' ****IFRCAT ENTERED FOR VARIABLE',
     1       2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2       ' NOT ACCOMMODATED.')
      IER=102
      GO TO 300
C
C        RETRIEVE THE OBSERVED OR CATEGORICAL FORECAST OF 
C        VISIBILITY.

 120  LD(1)=ITABLE(2,JJ)*1000+IDPARS(4)
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
         WRITE(KFILDO,125)(LD(J),J=1,4)
  125    FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN IFRCAT',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         JERR=1
      ENDIF
C
C        RETRIEVE THE OBSERVED OR CATEGORICAL FORECAST OF 
C        CEILING HEIGHT.  NOTE THAT THE LAMP AND GFS-MOS
C        CEILING HEIGHT BEST CATEGORY FORECAST IDS ARE DIFFERENT
C        BECAUSE THE LAMP CEILING HEIGHT BEST CATEGORY FORECAST 
C        IS CALCULATED IN A CUMULATIVE MANNER WHILE THE MOS 
C        CEILING HEIGHT BEST CATEGORY FORECAST IS CALCULATED
C        IN A DISCRETE MANNER.
C
      IF((MOD(IDPARS(4),10).EQ.5).OR.(MOD(IDPARS(4),10).EQ.0)) THEN
C           PROCESS LAMP OR OBSERVATION 
         LD(1)=ITABLE(3,JJ)*1000+IDPARS(4)
      ELSE
C           PROCESS MOS
         LD(1)=ITABLE(4,JJ)*1000+IDPARS(4)
      ENDIF
C
      LD(2)=JD(2)
      LD(3)=JD(3)
      LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
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
C        JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C        IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,127)(LD(J),J=1,4)
  127    FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN IFRCAT',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         JERR=JERR+1
      ENDIF
C
C        IF BOTH VISIBILITY AND CEILING HEIGHT RECORDS CANNOT BE FOUND,
C        SET THE FLIGHT CATEGORY VALUES FOR ALL STATIONS TO MISSING.
C
      IF(JERR.EQ.2) GOTO 300
C
      IF((ID(1)/1000.EQ.708210).OR.(ID(1)/1000.EQ.708223)) THEN
C
C        COMPUTE THE FLIGHT CATEGORY VALUE USING OBSERVATIONS OF 
C        VISIBILITY AND CEILING HEIGHT. 
C
         DO 211 K=1,NSTA
C
            IF((XDATA(K).LT..5).OR.(YDATA(K).LT.2.)) THEN
C                 VLIFR CONDITIONS ARE REPORTED. 
               XDATA(K)=1.
C
            ELSEIF((NINT(XDATA(K)).NE.9999).AND.
     1             (NINT(YDATA(K)).NE.9999))THEN
               IF(((XDATA(K).GE..5).AND.(XDATA(K).LT.1.)).OR.
     1            ((YDATA(K).GE.2.).AND.(YDATA(K).LT.5.))) THEN
C                    LIFR CONDITIONS ARE REPORTED. 
                  XDATA(K)=2.
               ELSEIF(((XDATA(K).GE.1.).AND.(XDATA(K).LT.3.)).OR.
     1                ((YDATA(K).GE.5.).AND.(YDATA(K).LT.10.))) THEN
C                    IFR CONDITIONS ARE REPORTED. 
                  XDATA(K)=3.
               ELSEIF(((XDATA(K).GE.3.).AND.(XDATA(K).LE.5.)).OR.
     1                ((YDATA(K).GE.10.).AND.(YDATA(K).LE.30.))) THEN
C                    MVFR CONDITIONS ARE REPORTED. 
                  XDATA(K)=4.
               ELSEIF((XDATA(K).GT.5.).AND.(YDATA(K).GT.30.)) THEN
C                    VFR CONDITIONS ARE REPORTED. 
                  XDATA(K)=5.
               ENDIF
C
            ELSE
C
               XDATA(K)=9999.
C         
            ENDIF
C
 211     CONTINUE
C
      ELSE
C
C           COMPUTE THE FLIGHT CATEGORY VALUE USING CATEGORICAL FORECASTS 
C           OF VISIBILITY AND CEILING HEIGHT.
C
         DO 250 K=1,NSTA
C
            IF((NINT(XDATA(K)).EQ.1).OR.(NINT(YDATA(K)).EQ.1))THEN
C                 VLIFR CONDITIONS ARE FORECASTED.
               XDATA(K)=1.
C
            ELSEIF((NINT(XDATA(K)).NE.9999).AND.
     1             (NINT(YDATA(K)).NE.9999))THEN
               IF((NINT(XDATA(K)).EQ.2).OR.
     1            (NINT(YDATA(K)).EQ.2))THEN
C                    LIFR CONDITIONS ARE FORECASTED.
                  XDATA(K)=2.
               ELSEIF((NINT(XDATA(K)).EQ.3).OR.
     1                (NINT(XDATA(K)).EQ.4).OR.
     2                (NINT(YDATA(K)).EQ.3.)) THEN
C                    IFR CONDITIONS ARE FORECASTED.
                  XDATA(K)=3.
               ELSEIF((NINT(XDATA(K)).EQ.5).OR.
     1                (NINT(YDATA(K)).EQ.4).OR.
     2                (NINT(YDATA(K)).EQ.5)) THEN
C                    MVFR CONDITIONS ARE FORECASTED.
                  XDATA(K)=4.
               ELSEIF((NINT(XDATA(K)).GE.6).AND. 
     1                (NINT(YDATA(K)).GE.6)) THEN
C                    VFR CONDITIONS ARE FORECASTED.
                  XDATA(K)=5.
               ENDIF
C
            ELSE
C
               XDATA(K)=9999.
C
            ENDIF

 250     CONTINUE
C
      ENDIF
C
C        ASSIGN BINARY NUMBER TO IFR CONDITIONS EXCLUSIVELY.
C
      IF((ID(1)/1000.EQ.708223).OR.(ID(1)/1000.EQ.208223)) THEN
         DO 275 K=1,NSTA
            IF(NINT(XDATA(K)).NE.9999) THEN
               IF(NINT(XDATA(K)).EQ.3) THEN
                  XDATA(K)=1.
               ELSE
                  XDATA(K)=0.
               ENDIF
            ENDIF
 275     CONTINUE
      ENDIF
C
      IER=0
      GOTO 350
C 
C        THIS VARIABLE CANNOT BE COMPUTED.  SET XDATA( ) TO MISSING.
C        THIS IS FOR SAFETY; XDATA( ) SHOULD ALREADY BE SET TO MISSING.
C
 300  DO 310 K=1,ND1
         XDATA(K)=9999.
 310  CONTINUE 
C
 350  RETURN
      END     
