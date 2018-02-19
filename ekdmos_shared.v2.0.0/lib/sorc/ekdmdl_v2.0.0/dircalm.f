      SUBROUTINE DIRCALM(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                   ID,IDPARS,JD,ITAU,
     2                   NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NSTA,
     3                   ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                   LSTORE,ND9,LITEMS,CORE,ND10,
     5                   NBLOCK,NFETCH,
     6                   IS0,IS1,IS2,IS4,ND7,
     7                   L3264B,L3264W,IER)
C 
C        JANUARY   2000   SFANOS   TDL   MOS-2000
C        APRIL     2000   GLAHN    ELIMINATED ISTAB, ICCCFFF( ),
C                                  XDATA3( ); CHANGED XDATA( ) TO
C                                  YDATA( ); EXPANDED ITABLE( , );
C                                  SPELLING CHECKED; ADDED NINT
C                                  WHEN CHECKING FOR 9999; INSURED
C                                  /, IN **** COMMENTS
C        APRIL     2000   RUDACK   REMOVED SETTING DIR = 8888 WHEN
C                                  WHEN SPEED GE 1 AND DIR MISSING;
C                                  ADDED DATA LDPARS/15*0/ 
C        MAY       2000   JPD      CHANGED CHECK FOR WIND SPEED
C                                  TO EQ 0 RATHER THAN LE 1 AT LINE 170 
C        AUGUST    2002   MCALOON  ADDED ID'S TO ACCOMMODATE NGM MOS
C                                  WIND FCST BY MOD. PERFECT PROG.
C        MARCH     2003   MCALOON  ADDED ID'S TO ACCOMMODATE HEIGHT
C                                  ADJUSTED WIND DIRECTION
C
C        PURPOSE
C            THIS SUBROUTINE CHECKS THE WIND SPEED TO SEE IF IT IS
C            LE 1 (ROUNDED).  IF IT IS, CHECK TO MAKE THE DIRECTION
C            ZERO.  SEE ITABLE( , ) FOR THE MAPPING IDS.  IF A CCCFFF
C            IS REMOVED OR ADDED TO ITABLE( , ), CHANGE THE PARAMETER
C            NDIM APPROPRIATELY; THESE SHOULD BE THE ONLY CHANGES
C            NECESSARY, EXCEPT FOR SWITCHING IN OPTX.  IF THE 
C            DATUM = 9999, IT IS LEFT UNCHANGED.
C
C            IDPARS(1) & IDPARS(2) OF WIND DIRECTION AND WIND SPEED  
C            ARE MAPPED INTO      FROM 
C                       204220    204200 AND 204320 (UNINFLATED WIND SPEED)
C                       204225    204200 AND 204325 (INFLATED WIND SPEED)                      
C                       224225    224200 AND 224325 (INFL WSPD FROM NGM MOS)  
C                       204235    204225 AND 204335 (HEIGHT ADJUSTED WSPD)
C
C        DATA SET USE 
C            KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                     (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT) 
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
C            XDATA(K) = WIND DIRECTION, BEFORE AND AFTER MODIFICATION
C                       (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF XDATA( ), ISDATA( ), AND 
C                       YDATA( ).  (INPUT)
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
C                       YDATA( ) (J=1,4).  (INTERNAL)
C               MD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       XDATA( ) (J=1,4).  (INTERNAL)
C            YDATA(K) = WORK ARRAY; HOLDS WIND DIRECTION (K=1,NSTA).
C                       (AUTOMATIC)
C         ITABLE(I,J) = VALUES OF CCCFFF ACCOMMODATED (I=1) AND
C                       THE ASSOCIATED VARIABLES NEEDED (I=2 AND 3)
C                       (J=1,NDIM).  (INTERNAL)
C                NDIM = SECOND DIMENSION OF ITABLE( , ).  SET BY
C                       PARAMETER.  (INTERNAL)
C           MDPARS(J) = PARSED ID FILLED IN SUBROUTINE PRSID1 FOR DIRECTION.
C                       (J=1,15). (INTERNAL)
C           LDPARS(J) = PARSED ID FILLED IN SUBROUTINE PRSID1 FOR SPEED.
C                       (J=1,15). (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            PRSID1,RETVEC
C
      PARAMETER (NDIM=4)
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION ISDATA(ND1),XDATA(ND1),YDATA(ND1)
C        YDATA( ) IS AN AUTOMATIC ARRAY.
      DIMENSION IPACK(ND5),IWORK(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION KFILRA(5),LD(4),LDPARS(15),MD(4),MDPARS(15)
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION CORE(ND10)
      DIMENSION ICALLD(L3264W,ND5),DATA(ND5)
      DIMENSION ITABLE(3,NDIM)
C
      DATA LDPARS/15*0/
      DATA ITABLE/204220, 204200, 204320,
     1            204225, 204200, 204325,
     2            224225, 224200, 224325,
     3            204235, 204225, 204335/
C
      IER=0
C     
C        FIND CCCFFF OF ID(1) IN ITABLE(1, ).
C                                                      
      DO 105 JJ=1,NDIM      
         IF(ITABLE(1,JJ).EQ.(IDPARS(1)*1000+IDPARS(2))) GO TO 108
 105  CONTINUE
C     
      WRITE(KFILDO,107)(JD(L),L=1,4)
 107  FORMAT(/,' ****DIRCALM ENTERED FOR VARIABLE',
     1        2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2       ' NOT ACCOMMODATED.')
      IER=102
      GOTO 300
C     
C        FILL LD( ) AND LDPARS( ) AND CALL SUBROUTINE
C        RETVEC TO FETCH AND RETURN IN XDATA( ) AND
C        YDATA( )
C     
 108  LD(1)=ITABLE(3,JJ)*1000+IDPARS(4)
      LD(2)=JD(2)
      LD(3)=JD(3)
C
      MD(1)=ITABLE(2,JJ)*1000+IDPARS(4)
      MD(2)=JD(2)
      MD(3)=JD(3)
C          
C        FETCH THE SPEED INTO YDATA( ).
C         
      LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
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
C        ERROR CHECK AFTER CALLING RETVEC
C
      IF(IER.NE.0)THEN
        WRITE(KFILDO,125)(LD(J),J=1,4)
 125    FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN DIRCALM',
     1         2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
        GOTO 300
      ENDIF
C
C        FETCH THE DIRECTION INTO XDATA( ).
C
      MD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
      CALL PRSID1(KFILDO,MD,MDPARS)
      CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1            MD,MDPARS,JD,ITAU,
     2            NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NSTA,
     3            ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4            LSTORE,ND9,LITEMS,CORE,ND10,
     5            NBLOCK,NFETCH,
     6            IS0,IS1,IS2,IS4,ND7,
     7            L3264B,L3264W,IER)
C
C        ERROR CHECK AFTER CALLING RETVEC
C
      IF(IER.NE.0)THEN
        WRITE(KFILDO,127)(MD(J),J=1,4)
 127    FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN DIRCALM',
     1         2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
        GOTO 300
      ENDIF
C
C        JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C        IN CALL TO CONST, BUT IS NOT USED THERE EITHER.
C
C        CHECKING ON WIND SPEED AND DIRECTION; WHERE THE
C        WORK OCCURS.
C        
      DO 170 K=1,NSTA
C
C        ISDATA(K) IS A WORK ARRAY WHICH HOLDS THE NEAREST INTEGER
C        OF THE WIND SPEED (YDATA( )).  ISDATA(K) IS THEN COMPARED
C        TO SEE IF THE SPEED EQUALS 0.  IF IT IS SET DIRECTION TO ZERO.
C
        ISDATA(K)=NINT(YDATA(K))
        IF(ISDATA(K).EQ.0) THEN
          XDATA(K)=0.
        ENDIF
C
 170  CONTINUE
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
