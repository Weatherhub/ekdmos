      SUBROUTINE TMPCMP(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JD,ITAU,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,IER)
C
C        JUNE      1999   RUDACK   TDL   MOS-2000
C        JULY      1999   GLAHN    MINOR COMMENTS CHANGES; NDIM ADDED,
C                                  ORDER OF FETCHING VARIABLES CHANGED
C        AUGUST    1999   RUDACK   CHANGED CALL FROM GFETCH TO RETVEC
C        SEPTEMBER 1999   GLAHN    ADDED YDATA( )
C        APRIL     2000   RUDACK   ADDED ID CHECK FOR DIFFERENT CYCLES
C                                  AND PROJECTIONS
C        APRIL     2000   GLAHN    CHECKED SPELLING, FORMATS MADE
C                                  FORTRAN 90 COMPATIBLE; ADDED /, IN 
C                                  **** COMMENTS 
C        APRIL     2000   RUDACK   ADDED DATA LDPARS/15*0/; COMPLETION
C        MAY       2000   RUDACK   ADDED ERROR CHECK 'IERF' TO INSURE
C                                  PROPER PROGRAM FLOW
C        OCTOBER   2001   COSGROVE MODIFIED ROUTINE TO WORK FOR 9-HR
C                                  PROJECTION OF 6/18Z AVN.  ALSO ADJUSTED
C                                  THE WAY THE LOGICAL VARIABLE PROJTN IS
C                                  INITIALIZED AND USED TO CORRECT ERROR.
C        JANUARY   2005   RLC/JW   MODIFIED ROUTINE TO ALLOW FOR LAMP AND
C                                  TO HANDLE 3-HR TEMPS OUT TO 192 AT
C                                  00/12, AND OUT TO 84 AT 6/18
C        PURPOSE
C            MAKES SURE THAT THE TEMPERATURE AT THE PROJECTION HOUR IS
C            EITHER EQUAL TO OR GREATER THAN THE DEW POINT TEMPERATURE AT
C            THAT CORRESPONDING PROJECTION HOUR.  IF THE DEW POINT IS
C            GREATER THAN THE TEMPERATURE, THE VALUE RETURNED IS THE
C            AVERAGE OF THE TWO.
C
C            IDPARS(1) & IDPARS(2) OF TEMPERATURE AND DEW POINT 
C            ARE MAPPED INTO      FROM 
C                       202020    202000 AND 203000 (PROJ. OF
C                                                    NON-AVERAGED TEMPS)
C                       202020    202010 AND 203010 (PROJ. OF
C                                                    AVERAGED TEMPS)
C                       203020    203000 AND 202000 (PROJ. OF 
C                                                    NON-AVERAGED TEMPS)
C                       203020    203010 AND 202010 (PROJC. OF
C                                                    AVERAGED TEMPS)
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
C            XDATA(K) = TEMPERATURE OR DEW POINT DEPENDING ON ID( )
C                       (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF XDATA( ).  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN
C                       INTEGER VARIABLE (L=1,L3264W) (K=1,ND5). 
C                       THIS ARRAY IS USED TO READ THE STATION 
C                       DIRECTORY FROM A MOS-2000 EXTERNAL FILE. 
C                       EQUIVALENCED TO CCALLD( ).  (CHARACTER*8)
C                       (INTERNAL)
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
C                IERF = HOLDS THE IER VALUE RETURNED FROM THE FIRST CALL
C                       TO RETVEC.  (INTERNAL) 
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       XDATA( ) (J=1,4).  (INTERNAL)
C           LDPARS(J) = PARSED VALUES CORRESPONDING TO LD( ) (J=1,15)
C                       (INTERNAL)
C            YDATA(K) = WORK ARRAY WHICH STORES 2ND VALUE RETRIEVED 
C                       (K=1,ND1).  (AUTOMATIC)
C         ITABLE(I,J) = VALUES OF CCCFFF ACCOMMODATED (I=1) AND
C                       THE ASSOCIATED VARIABLE NEEDED (I=2-5)
C                       (J=1,NDIM).  (INTERNAL)
C                NDIM = SECOND DIMENSION OF ITABLE( , ).  SET BY
C                       PARAMETER.  (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            RETVEC,PRSID1
C
      PARAMETER(NDIM=2)
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
      LOGICAL PROJAVG
C
      DIMENSION XDATA(ND1),ISDATA(ND1),YDATA(ND1)
C        YDATA( ) IS AN AUTOMATIC ARRAY.
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10),ICALLD(L3264W,ND5)
      DIMENSION LD(4),LDPARS(15),KFILRA(5)
      DIMENSION ITABLE(5,NDIM)
C
      DATA LDPARS/15*0/  
      DATA ITABLE/202020,202000,203000,202010,203010,
     1            203020,203000,202000,203010,202010/
C
C        NOTE THAT FOR RETURNING TEMPERATURE, TEMPERATURE
C        IS IN XDATA( ) AND DEW POINT IN YDATA( ). BUT
C        FOR RETURNING DEW POINT, DEW POINT IS IN XDATA( )
C        AND TEMPERATURE IN YDATA( ).  TESTS DEPEND ON THIS.
C
      IER=0
      IERF=0
      PROJAVG=.FALSE.
      NCYCLE=MOD(NDATE,100)
C     
C        FIND THE CCCFFF IN ID(1) IN ITABLE(1, ).
C
      DO 100 JJ=1,NDIM
      IF(ITABLE(1,JJ).EQ.(IDPARS(1)*1000+IDPARS(2))) GO TO 120
 100  CONTINUE
C     
      WRITE(KFILDO,107)(JD(L),L=1,4)
 107  FORMAT(/,' ****TMPCMP ENTERED FOR VARIABLE',
     1        2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2        ' NOT ACCOMMODATED.')
      IER=102
      GO TO 300
C
C        CHECK THE PROJECTION TO DETERMINE IF THE AVERAGED OR NON-AVERAGED 
C        TEMPERATURE OR DEW POINT IS BEING USED. FORM THE FOUR WORD ID FOR 
C        FIRST INPUT VALUE.  LAMP(DD=5) DOES NOT DO THIS AVERAGING.
C
C       THIS FIRST SECTION IS FOR THE LAMP FORECASTS
C
 120  IF(IDPARS(4).EQ.5)THEN
        LD(1)=ITABLE(2,JJ)*1000+IDPARS(4)
C
C        THIS SECTION IS FOR THE MOS FORECASTS
C
      ELSE
        IF((NCYCLE.EQ.0).OR.(NCYCLE.EQ.12)) THEN
C
           IF((IDPARS(12).EQ.15).OR.(IDPARS(12).EQ.27).OR. 
     1                              (IDPARS(12).EQ.39).OR.
     2                              (IDPARS(12).EQ.51).OR.
     3                              (IDPARS(12).EQ.63).OR.
     4                              (IDPARS(12).EQ.75).OR.
     5                              (IDPARS(12).EQ.87).OR.
     6                              (IDPARS(12).EQ.99).OR.
     7                              (IDPARS(12).EQ.111).OR.
     8                              (IDPARS(12).EQ.123).OR.
     9                              (IDPARS(12).EQ.135).OR.
     A                              (IDPARS(12).EQ.147).OR.
     B                              (IDPARS(12).EQ.159).OR.
     C                              (IDPARS(12).EQ.171).OR.
     D                              (IDPARS(12).EQ.183)) THEN
C
              LD(1)=ITABLE(4,JJ)*1000+IDPARS(4)
              PROJAVG=.TRUE. 
C
           ELSE
C
              LD(1)=ITABLE(2,JJ)*1000+IDPARS(4)          
C
           ENDIF                    
C        
        ELSEIF((NCYCLE.EQ.6).OR.(NCYCLE.EQ.18)) THEN
C
C         NOTE: FOR THE GFS 81H IS AVERAGED, FOR THE ETA IT ISN'T
           IF((IDPARS(12).EQ.09).OR.(IDPARS(12).EQ.21).OR. 
     1                              (IDPARS(12).EQ.33).OR.
     2                              (IDPARS(12).EQ.45).OR.
     3                              (IDPARS(12).EQ.57).OR.
     4                              (IDPARS(12).EQ.69).OR.
     5          ((IDPARS(4).EQ.8).AND.(IDPARS(12).EQ.81))) THEN
C
              LD(1)=ITABLE(4,JJ)*1000+IDPARS(4)
              PROJAVG=.TRUE.
C
           ELSE
C 
              LD(1)=ITABLE(2,JJ)*1000+IDPARS(4)
C
           ENDIF
C
        ENDIF  
      ENDIF
C
      LD(2)=JD(2)
      LD(3)=JD(3)
      LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)     
C 
C        FETCH THE FIRST VARIABLE.
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
      IERF=IER
      IF(IER.NE.0)THEN
         WRITE(KFILDO,125)(LD(J),J=1,4)
 125     FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN TMPCMP',
     1             2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
      ENDIF
C     
C         FORM THE FIRST WORD ID FOR THE SECOND INPUT VALUE.
C
      IF(PROJAVG) THEN
C
         LD(1)=ITABLE(5,JJ)*1000+IDPARS(4)
C
      ELSE
C
         LD(1)=ITABLE(3,JJ)*1000+IDPARS(4)
C
      ENDIF
C 
C          FETCH THE SECOND VARIABLE.
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
          WRITE(KFILDO,133)(LD(J),J=1,4)
 133      FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC IN TMPCMP',
     1             2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
       ENDIF
C
C         IF THE TEMPERATURE AND DEW POINT ARE BOTH MISSING SET 
C         THE RETURNED VALUE IN XDATA TO MISSING.
C       
       IF((IERF.NE.0).AND.(IER.NE.0)) GOTO 300
C     
       IF(JJ.EQ.1)GO TO 141
C
C         THIS LOOP FOR RETURNING DEW POINT.
C                
       DO 140 K=1,NSTA
C
          IF(NINT(XDATA(K)).EQ.9999.OR.
     1       NINT(YDATA(K)).EQ.9999) GO TO 140
C
          IF(YDATA(K).LT.XDATA(K))THEN
             XDATA(K)=((XDATA(K)+YDATA(K))/2.)
          ENDIF
C
 140   CONTINUE
C
       GO TO 350
C
C         THIS LOOP FOR RETURNING TEMPERATURE.
C
 141   DO 150 K=1,NSTA
C
          IF(NINT(XDATA(K)).EQ.9999.OR.
     1       NINT(YDATA(K)).EQ.9999) GO TO 150
C
          IF(YDATA(K).GT.XDATA(K))THEN
             XDATA(K)=((XDATA(K)+YDATA(K))/2.)
          ENDIF
C
 150  CONTINUE   
      IER=0
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

     
                  
