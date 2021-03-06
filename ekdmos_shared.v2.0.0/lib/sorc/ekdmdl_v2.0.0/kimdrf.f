      SUBROUTINE KIMDRF(KFILDO,KFIL10,IP12,IP16,IDPARS,JD,NDATE,
     1                  KFILRA,RACESS,NUMRA,
     2                  CCALL,ICALLD,CCALLD,
     3                  ISDATA,SDATA,DIR,ND1,NSTA,
     4                  NGRIDC,ND11,NSLAB,IPACK,IWORK,DATA,ND5,
     5                  LSTORE,ND9,LITEMS,CORE,ND10,
     6                  LASTL,NBLOCK,LASTD,NSTORE,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     8                  FD1,FD2,FD3,FD4,FD5,FD6,FD7,ND2X3,
     9                  ISTAV,L3264B,L3264W,MISTOT,IER)
C
C	 JANUARY   2005  CHARBA   KIMDRF IS A NEW VERSION OF KINXRF THAT
C				  USES A MODIFIED K INDEX (CALLS SUBROU-
C				  TINE KIMOD) AND THE LAMP 2-H MONTHLY 
C				  TSTM RELATIVE FREQUENCIES.  KIMDRF 
C				  IS SET UP TO PROVIDE TWO VARIABLES:
C				  ONE IS MODIFIED K INDEX AND THE OTHER 
C				  IS THE PRODUCT OF THE MODIFIED K INDEX
C				  AND THE LAMP 2-H MONTHLY TSTM RELATIVE
C				  FREQUENCIES.  
C	MAY        2005  CHARBA   CHANGED ID FOR THE PRODUCT VARIABLE
C				  AND IMPROVED DOCUMENTATION.
C	SEPTEMBER  2005  CHARBA   CHANGED TO ACCOMMODATE SWITCH IN THE 
C				  DD IN ID(1) FROM 08 T0 05.  ALSO, SEE
C				  IMPORTANT NOTE JUST PRIOR TO CALL TO
C				  CONST.
C       MAY        2006  CHARBA   ADDED DIMENSIONING OF ICALLD( , ND5),
C                                 WHICH WAS MISSING.  THIS WAS ALSO
C                                 MISSING IN THE ORIGINAL LIBARAY VER-
C                                 SION.  TESTS HAVE SHOWN THE ROUTINE
C                                 YIELDS THE SAME RESULT WITH OR WITH-
C                                 OUT DIMENSIONING OF THIS VARIABLE.
C                                 ALSO MADE COSMETIC CHANGES TO SATISY 
C                                 WALK-THRU.
C
C        PURPOSE 
C            SUBROUTINE KINXRF COMPUTES THE INTERACTIVE PREDICTOR KF, DEFINED
C            BY THE PRODUCT OF THE K STABILITY INDEX AND THE DAILY THUNDERSTORM
C            FREQUENCY (F).  K=(850T-500T)+850TD+(700TD-700T), WHERE T IS THE
C            TEMPERATURE AND TD IS THE DEW POINT.  F IS OBTAINED BY LINEAR 
C            INTERPOLATION BETWEEN THE MONTHLY CLIMATOLOGICAL VALUES OF  
C            LIGHTNING FREQUENCY STORED ON DISK.  THE K INDEX AND THUNDERSTORM
C            FREQUENCY INPUT FILES ARE STORED ON DISK IN TDLPACK FORMAT. 
C            THE FREQUENCY FILES ARE IN THE FORM OF RANDOM-ACCESS (CONSTANT)
C            FILES ON DISK.  THE CONSTANT FILES ARE CREATED BY U350 AND U352.
C            KF VALUES ARE RETURNED TO THE CALLING PROGRAM (OPTION) IN ARRAY  
C            SDATA.  PRIMARY USE OF THE KF PREDICTOR IS TO PREDICT BOTH WARM
C            AND COOL SEASON NON-SEVERE GENERAL THUNDERSTORM ACTIVITY.   
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C                007 195 - MODIFIED K INDEX (KI)           
C                007 440 - MOD. KI * PREVIOUS 2-H MONTHLY TSTM RF
C
C        DATA SET USE
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT/OUTPUT) 
C
C        VARIABLES
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C                IP12 = CONTAINS DIAGNOSTIC PRINT OUTPUT FROM FINDST 
C                       (STATIONS ON THE RANDOM  ACCESS FILE), WHICH IS
C                       CALLED FROM CONST.  (INPUT)
C                IP16 = NOT USED.  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID() (J=1,15).
C                       (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY
C                                      1 LAYER)
C                       J=7--LTLTLTLT (TOP OF LAYER)
C                       J=8--T (TRANSFORMATION)
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND 
C                                BACK IN TIME)
C                       J=10-OT (TIME APPLICATION)
C                       J=11-OH (TIME PERIOD IN HOURS)
C                       J=12-TAU (PROJECTION IN HOURS)
C                       J=13-I (INTERPOLATION TYPE)
C                       J=14-S (SMOOTHING INDICATOR)
C                       J=15-G (GRID INDICATOR)
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT
C                       THE PORTIONS PERTAINING TO PROCESSING
C                       ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15).
C                       JD() IS USED TO HELP IDENTIFY THE BASIC MODEL
C                       FIELDS AS READ FROM THE ARCHIVE. (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C           KFILRA(J) = HOLDS THE UNIT NUMBERS FOR ACCESSING THE MOS-2000
C                       EXTERNAL RANDOM ACCESS FILES (J=1,NUMRA).  (INPUT)
C           RACESS(J) = THE FILE NAMES CORRESPONDING TO KFILRA(J) (J=1,NUMRA).
C                       (CHARACTER*60)  (INPUT)
C               NUMRA = THE NUMBER OF UNIT NUMBERS AND NAMES IN KFILRA( )
C                       AND RACESS( ).  (INPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST, EXCEPT POSSIBLY 
C                       CCALLD( ) AND CCALLP( ).  (CHARACTER*8)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN INTEGER
C                       VARIABLE (L=1,L3264W) (K=1,ND5).
C                       EQUIVALENCED TO CCALLD( ).  (INTERNAL)
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).  THIS LIST IS USED 
C                       IN L1D1 TO READ THE REGION LISTS.  (CHARACTER*8)
C                       (INTERNAL)
C           ISDATA(K) = WORK ARRAY (K=1,ND1).  (INTERNAL)
C            SDATA(K) = RELATIVE FREQUENCY (CONSTANT) DATA RETURNED (K=1,NSTA)
C                       FROM SUBROUTINE CONST (OUTPUT).
C                       ALSO CONTAINS PRODUCT OF RELATIVE FREQUENCY DATA AND
C                       K STABILITY INDEX. (OUTPUT)
C           DIR(K,J,) = THE IX (J=1) AND JY (J=2) POSITIONS ON THE
C                       GRID FOR STATION K (K=1,NSTA).  (INPUT)  
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       DIMENSION OF SEVERAL VARIABLES.  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT WITH.
C                       (INPUT)
C         NGRIDC(L,M) = HOLDS THE GRID CHARACTERISTICS (L=1,6) FOR EACH GRID
C                       COMBINATION (M=1,NGRID).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=POLAR
C                            STEREOGRAPHIC). 
C                       L=2--GRID LENGTH IN MILLIMETERS,
C                       L=3--LATITUDE AT WHICH GRID LENGTH IS CORRECT *10000,
C                       L=4--GRID ORIENTATION IN DEGREES *10000,
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *10000,
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES *10000.
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN BE
C                       DEALT WITH ON THIS RUN.  LAST DIMENSION OF
C                       NGRIDC( , ) AND DIR( , , ).  (INPUT)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND
C                       IN NGRIDC( , ) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  SEE LSTORE(10, ).  FOR THE
C                       COMPUTATION ROUTINES RETURNING A GRID, THIS
C                       VALUE MUST BE OUTPUT BY GFETCH.  (OUTPUT) 
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(J) = ARRAY TO HOLD RETURNED DATA WHEN THE DATA ARE
C                       AT GRIDPOINTS. (J=1,ND5).  (OUTPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND DATA( ).
C                       (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS).  (INPUT/OUTPUT)
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
C                              IN NGRIDC( ,L) DEFINING THE CHARACTERISTICS
C                              OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,NPRED) FOR WHICH THIS
C                              VARIABLE IS NEEDED, WHEN IT IS NEEDED ONLY
C                              ONCE FROM LSTORE( , ).  WHEN IT IS NEEDED
C                              MORE THAN ONCE, THE VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING MSTORE( , ).
C                              LATER USED AS A WAY OF DETERMINING WHETHER
C                              TO KEEP THIS VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , ) THAT 
C                       HAVE BEEN USED IN THIS RUN.  (INPUT/OUTPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE(,) (J=1,ND10).
C                       WHEN CORE() IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C               LASTL = THE LAST LOCATION IN CORE( ) USED FOR MOS-2000 INTERNAL
C                       STORAGE.  INITIALIZED TO 0 ON FIRST ENTRY TO GSTORE.
C                       ALSO INITIALIZED IN U201 IN CASE GSTORE IS NOT ENTERED.
C                       (INPUT-OUTPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C               LASTD = TOTAL NUMBER OF PHYSICAL RECORDS ON DISK FOR MOS-2000
C                       INTERNAL STORAGE.  MUST BE CARRIED WHENEVER GSTORE
C                       IS TO BE CALLED.  (INPUT)
C              NSTORE = THE NUMBER OF TIMES GSTORE HAS BEEN ENTERED.  GSTORE
C                       KEEPS TRACK OF THIS AND RETURNS THE VALUE.  (OUTPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING COUNT FROM THE BEGINNING OF
C                       THE PROGRAM.  THIS COUNT IS MAINTAINED IN
C                       CASE THE USER NEEDS IT(DIAGNOSTICS, ETC.).
C                       (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).
C                       (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).
C                       (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       IS2(3) AND IS2(4) ARE USED BY THE CALLING
C                       PROGRAM AS THE GRID DIMENSIONS.
C                       (INTERNAL-OUTPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).
C                       (INTERNAL)
C                 ND7 = DIMENSION OF IS0(),IS1(),IS2(), AND IS4().
C                       NOT ALL LOCATIONS ARE USED. (INPUT)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  THE SIZE OF
C                       THE GRID IS NOT KNOWN BEFORE FDTK AND FDDP
C                       ARE FETCHED.  ALL WORK ARRAYS ARE DIMENSIONED
C                       ND2X3.  (INPUT)
C  FD1(J),FD2(J),ETC  = WORK ARRAYS (J=1,ND2X3).  (INTERNAL)
C               ISTAV = 0 FOR THE MOD. KI (GRID) AND 1 FOR THE PRODUCT
C			VARIABLE (STATION).  (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64). (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS, EITHER 1 OR 2.  (INPUT)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C                 IER = STATUS RETURN
C                         0 = GOOD RETURN
C                        47 = CONSTANT DATA COULD NOT BE FETCHED.
C                       103 = PREDICTOR NOT IDENTIFIED TO COMPUTE;
C                             WHEN IER NE 0, DATA ARE RETURNED AS MISS-
C                             ING. 
C                       SEE KIMOD, AND CONST FOR OTHER VALUES.
C                       (OUTPUT) 
C
C        NONSYSTEM SUBROUTINES CALLED
C           KIMOD, PRSID1, INTRPB, CONST, UPDAT, SMTH5, SMTH9, SMTH25
C
C                       (INTERNAL-OUTPUT)
C        1         2         3         4         5         6         7 X
C
      CHARACTER*8 CCALL(ND1,6),CCALLD(ND5) 
      CHARACTER*60 RACESS(NUMRA) 
C
      DIMENSION IDPARS(15),JD(4),LDPARS(15),LD(4),KFILRA(NUMRA)
      DIMENSION DIR(ND1,2,ND11),SDATA(ND1),ISDATA(ND1)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5),ICALLD(L3264W,ND5)
      DIMENSION FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),FD5(ND2X3),
     1          FD6(ND2X3),FD7(ND2X3)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION NGRIDC(6,ND11)
      DIMENSION CORE(ND10)
C
C***********************************************************************
C
C        STEP 1. INITIALIZATION
C
      IER =0
C
C        MAKE SURE THIS SUBROUTINE DEALS WITH THE PREDICTORS:
C           007195 = MODIFIED KI (KIMOD)
C           007440 = KIMOD * PREVIOUS 2-H MONTHLY TSTM RF
C
      IF((IDPARS(1).NE.007).OR.(IDPARS(2).NE.195.AND.IDPARS(2).NE.440))
     1   THEN
         IER=103
         WRITE(12,100)(JD(J),J=1,4),IER
 100     FORMAT(/,' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE KIMDRF',
     1            ' PREDICTOR. ',I9.9,2I10.9,I4.3,' NOT COMPUTED IN ',
     2            'KIMDRF...SET IER =',I4,' AND SUPPLY MISSING VALUES.')
         GO TO 200
      ENDIF
C
C        STEP 2. COMPUTE THE MODIFIED K STABILITY INDEX ON NCEP MODEL
C	         GRID.
C  
      LD(1)=007195005
      LD(2)=JD(2)
      LD(3)=JD(3)  
      LD(4)=0 
      CALL PRSID1(KFILDO,LD,LDPARS)
      CALL KIMOD(KFILDO,KFIL10,KFILRA,RACESS,NUMRA,LDPARS,LD,
     1           NDATE,SDATA,ND1,NSTA,NGRIDC,ND11,NSLAB,
     2           IPACK,IWORK,DATA,ND5,
     3           LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     4           IS0,IS1,IS2,IS4,ND7,
     5           FD1,FD2,FD3,FD4,FD5,FD6,FD7,ND2X3,
     6           ISTAV,L3264B,MISTOT,IER)
C
      IF(IER.NE.0) GO TO 200
C
      IF(IDPARS(2).EQ.195) RETURN
C
C        STEP 3. INTERPOLATE THE MODIFIED K INDEX FROM THE NCEP
C                MODEL GRID TO THE STATION (GRIDPOINT) LIST 
C
      ISTAV=1
      NX=IS2(3)
      NY=IS2(4)
      JSTA=NSTA
      KK=0
C
C        FIRST, SMOOTH K INDEX IF DESIRED. 
C
      IF(IDPARS(14).EQ.1)CALL SMTH5 (KFILDO,DATA,FD1,NX,NY)
      IF(IDPARS(14).EQ.2)CALL SMTH9 (KFILDO,DATA,FD1,NX,NY)
      IF(IDPARS(14).EQ.3)CALL SMTH25(KFILDO,DATA,FD1,NX,NY)
 
      CALL INTRPB(KFILDO,DATA,NX,NY,DIR(1,1,NSLAB),ND1,NSTA,FD1) 
C
C        STEP 4. FETCH THE MONTHLY LIGHTNING RELATIVE FREQUENCIES FROM 
C	         THE RANDOM-ACCESS CONSTANT FILE.  CALL CONST; THE RE-
C		 TURNED RFS ARE IN SDATA( ).
C
      LD(1)=437501*1000
      LD(2)=030000000
      LD(3)=IDPARS(12)
      LD(4)=500000000
      CALL PRSID1(KFILDO,LD,LDPARS)
C
C	 GET THE VALID DATE FOR THE RF FIELD.  THE VALID DATE PERTAINS
C	 TO THE MOS CYCLE, NOT THE LAMP CYCLE.  THUS, THE RR MUST BE
C	 SUBTRACTED FROM THE LAMP DATE-TIME TO GET THE MOS DATE-TIME.
C
      LDATE=NDATE
      NRR=IDPARS(9)
      IF(NRR.GT.0)THEN
         CALL UPDAT(NDATE,-NRR,LDATE)
      ENDIF
C
C	 IMPORTANT NOTE:  THE CALL TO CONST BELOW OBTAINS A VECTOR 
C	                  FIELD (TSTM RF).  ISTAV IS SET TO 1 THEREIN,
C			  SO IT IS NOT NECESSARY TO SET IT TO 1 HERE.
C			  WHEN ISTAV IS 1 PRED21/22 WILL NOT PERFORM 
C			  GRID BINARY, SMOOTHING, OR INTERPOLATION OPER-
C			  TIONS EVEN THOUGH THE RELEVANT ID PARAMETERS 
C			  ARE TURNED ON.  IN FACT, CHKIDS EXPECTS A NON-
C			  ZERO "I" WHEN "C1" HAS A VALUE OF 0.  THUS, TO
C			  AVERT AN ERROR MESSAGE FROM CHKIDS FOR THE 
C			  VARIABLE CREATED BELOW, SET "I" TO A VALID 
C			  NON-ZERO VALUE EVEN THOUGH PRED21/22 DOES NOT 
C			  PERFORM THE INDICATED INTERPOLATION.
C
      CALL CONST(KFILDO,KFIL10,IP12,
     1           LD,LDPARS,JD,LDATE,
     2           KFILRA,RACESS,NUMRA,
     3           CCALL,ICALLD,CCALLD,
     4           ISDATA,SDATA,ND1,NSTA,
     5           IPACK,IWORK,DATA,ND5,
     6           LSTORE,ND9,LITEMS,CORE,ND10,LASTL,
     7           NBLOCK,LASTD,NSTORE,NFETCH,
     8           IS0,IS1,IS2,IS4,ND7,
     9           ISTAV,L3264B,L3264W,IER)
C
      IF(IER.NE.0) THEN     
         IER=47
         WRITE(KFILDO,130) (LD(J),J=1,4),LDATE,IER
 130     FORMAT(/,' ****ERROR, IN KIMDRF, COULD NOT',
     1            ' FETCH CONSTANT DATA FOR ',4I10,2X,I10,
     2            ' FROM RANDOM ACCESS VECTOR FILE ...SUPPLY MISSING ',
     3            'VALUES AND SET IER = ',I4)
         GO TO 200
      ENDIF
C
C        STEP 5. COMPUTE THE PRODUCT OF THE K INDEX AND THE 
C                DAILY RELATIVE FREQUENCY.  (SMOOTHING OF THE PRODUCT IS
C		 DESIRABLE BUT TOO MUCH WORK IS REQUIRED.)  
C
      DO 140 J=1,NSTA   
      SDATA(J)=FD1(J)*SDATA(J)
  140 CONTINUE
C
      GO TO 300
C 
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C
  200 DO 210 J=1,ND1
      SDATA(J)=9999.
  210 CONTINUE
C
  300 RETURN
      END
