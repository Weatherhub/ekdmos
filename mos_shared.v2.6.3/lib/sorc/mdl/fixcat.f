      SUBROUTINE FIXCAT(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JD,ITAU,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,IER)
C 
C        NOVEMBER 2003   MALONEY   MDL   MOS-2000
C
C        PURPOSE 
C            TO MAP THE NEW 8-CATEGORY CEILING FORECASTS AND THE
C            NEW 5-CATEGORY CLOUD FORECASTS INTO VALUES WHICH 
C            ARE CONSISTENT WITH THE "OLD" 7- AND 4-CATEGORY SYSTEMS.
C            CATEGORIES                
C            CEILING:     -- NEW --        ==>      -- OLD --
C                     1 = < 200 FT              1 = < 200 FT
C                     2 =  200 -  400 FT        2 =  200 -  400 FT
C                     3 =  500 -  900 FT        3 =  500 -  900 FT
C                     4 = 1000 - 1900 FT  **    4 = 1000 - 3000 FT
C                     5 = 2000 - 3000 FT  **  (LCAT)
C                     6 = 3100 - 6500 FT        5 = 3100 - 6500 FT
C                     7 = 6600 -12000 FT        6 = 6600 -12000 FT
C                     8 = > 12000 FT (UNLIM)    7 = > 12000 FT (UNLIM)
C
C            CLOUD:       -- NEW --        ==>      -- OLD --
C                     1 = CLEAR                 1 = CLEAR
C                     2 = FEW             **    2 = SCATTERED
C                     3 = SCATTERED       **  (LCAT)
C                     4 = BROKEN                3 = BROKEN
C                     5 = OVERCAST              4 = OVERCAST
C
C            IDPARS (1) AND IDPARS(2) 
C            ARE MAPPED  INTO        FROM 
C                       208 041    208 051
C                       208 341    208 351
C
C            IF A CCCFFF IS REMOVED OR ADDED TO ITABLE( , ), CHANGE THE
C            PARAMETER NDIM APPROPRIATELY; THESE SHOULD BE THE ONLY
C            CHANGES NECESSARY, EXCEPT FOR SWITCHING IN OPTX.
C            IF THE DATUM = 9999 OR 9997, IT IS LEFT UNCHANGED.
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
C            XDATA(K) = CATEGORICAL FORECAST TO CHANGE (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF XDATA( ).  (INPUT)
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
C                              MOSTORE( , ).  LATER USED AS A WAY OF
C                              DETERMINING WHETHER TO KEEP THIS 
C                              VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETIREVE THE DATA 
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
C                       XDATA( ) (J=1,4).  (INTERNAL)
C           LDPARS(J) = PARSED VALUES CORRESPONDING TO LD( ) (J=1,15)
C                       (INTERNAL)
C         ITABLE(I,J) = VALUES OF CCCFFF ACCOMMODATED (I=1) AND
C                       THE ASSOCIATED VARIABLE NEEDED (I=2)
C                       (J=1,NDIM).  (INTERNAL)
C                NDIM = SECOND DIMENSION OF ITABLE( , ).  SET BY
C                       PARAMETER.  (INTERNAL)
C                LCAT = IF XDATA(K) IS GREATER THAN OR EQUAL TO LCAT,
C                       REDUCE XDATA BY ONE.  REFER TO THE TABLE ABOVE
C                       IN THIS DOCBLOCK.  (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            RETVEC, PRSID1
C
      PARAMETER(NDIM=2)
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION XDATA(ND1)
      DIMENSION ID(4),IDPARS(15),JD(4),LD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5),KFILRA(5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9),ICALLD(L3264W,ND5)
      DIMENSION CORE(ND10),LDPARS(15)
      DIMENSION ITABLE(2,NDIM)
C
      DATA ITABLE/208041, 208051,
     +            208341, 208351/
C
      IER=0
C
C        VERIFY THE PROCESSING INDICATOR, IDPARS(1).
C
      DO 105 JJ=1,NDIM
      IF(ITABLE(1,JJ).EQ.IDPARS(1)*1000+IDPARS(2)) GO TO 120
 105  CONTINUE
C
      WRITE(KFILDO,107)(JD(L),L=1,4)
 107  FORMAT(' ****FIXCAT ENTERED FOR VARIABLE',
     1         2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2       ' NOT ACCOMMODATED.')
      IER=102
      GO TO 300
C
C        FETCH THE ORIGINAL CATEGORY.
C
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
  125    FORMAT(' ****VARIABLE NOT RETRIEVED BY RETVEC IN FIXCAT',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
         GO TO 300
      ENDIF
C
C        SET LCAT AS APPROPRIATE.  REFER TO THE TABLE ABOVE.
C
      IF (JJ.EQ.1) LCAT=5
      IF (JJ.EQ.2) LCAT=3
C
C        CHANGE CATEGORY AS REQUIRED.  IF VALUE IS MISSING, LEAVE IT.
C
      DO 200 K=1,NSTA
C
         IF(NINT(XDATA(K)).EQ.9999) GO TO 200
         IF(NINT(XDATA(K)).GE.LCAT) XDATA(K)=XDATA(K)-1.
C
 200  CONTINUE
C 
      GO TO 350   
C
C        THIS VARIABLE CANNOT BE COMPUTED.  SET THE ARRAY TO MISSING.
C
 300  DO 310 K=1,NSTA
         XDATA(K)=9999.
 310  CONTINUE 
C
 350  RETURN
      END     
