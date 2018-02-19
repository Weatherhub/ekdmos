      SUBROUTINE KIMOD(KFILDO,KFIL10,KFILRA,RACESS,NUMRA,IDPARS,JD,
     1                 NDATE,SDATA,ND1,NSTA,NGRIDC,ND11,NSLAB,
     2                 IPACK,IWORK,DATA,ND5,
     2                 LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                 IS0,IS1,IS2,IS4,ND7,
     4                 FD1,FD2,FD3,FD4,FD5,FD6,FD7,ND2X3,
     5                 ISTAV,L3264B,MISTOT,IER)
C
C        JANUARY  2005 CHARBA  MDL  MOS 2000                      
C	 MAY      2005 CHARBA  CORRECTED VARIABLE ID CHECK.
C        JUNE     2005 CHARBA  REPLACED INGEST OF LAMP GRID TERRAIN FROM
C			       FROM SEQUENTIAL FILE TO CONSTANT RANDOM
C			       ACCESS FILE. 
C        AUGUST   2005  CHARBA CHANGED TO ACCOMMODATE NEW ID(1) FOR
C                              TERRAIN GRID INGEST.  PREVIOUSLY, DD WAS
C                              MISTAKENLY SET TO 05.  IN THE NEW ID(1)
C                              THE DD VALUE IS 00.
C        SEPTMBER 2005  CHARBA CHANGED TO ACCOMMODATE SWITCH IN ID(1) 
C			       FROM 007195008 TO 007195005.
C        MAY      2006  CHARBA CHANGED TO MAKE COSMETIC AND SYNTAX
C                              CORRECTIONS BASED ON CODE WALK-THRU.
C        JUNE     2006  CHARBA CORRECTED CODING ERRORS THAT INVOLVE THE
C                              TIME INTERPOLATION.  THE ERRORS RESULTED 
C                              IN CORE-DUMP FAILURE IN SOME EXECUTABLES
C                              BUT IN SUCCESSFUL RUNS THE RESULTS WERE 
C                              CORRECT.  ALSO DID ADDITIONAL CODE 
C                              CLEAN-UP.
C
C        PURPOSE 
C            TO COMPUTE THE MODIFIED K INDEX, WHICH IS IDENTICAL TO THE
C	     THE CONVENTIONAL K INDEX, EXCEPT THE SIMPLE AVERAGE OF THE 
C	     ESTIMATED MSL TEMPERATURE AND THE 850-MB TEMPERATURE IS 
C	     SUBSTITUTED FOR THE 850-MB TEMPERATURE AND THE SIMPLE AVER-
C	     AGE OF THE ESTIMATED MSL DEWPOINT TEMPERATURE AND 850-MB
C	     DEWPOINT TEMPERATURE IS SUBSTITUTED FOR THE 850-MB DEW-
C	     POINT TEMPERATURE.  THE ESTIMATED MSL TEMPERATURE AND DEW-
C	     POINT IS BASED ON LAMP SFC TEMPERATURE AND DEWPOINT.  WHEN
C	     NCEP MODEL TEMPERATURE AND DEWPOINT FIELDS ABOVE THE SFC
C	     ARE NEEDED AT NON-THREE-HOURLY TIMES, THEY ARE TIMEWISE
C	     INTERPOLATED.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C	                     007 195  MODIFIED K INDEX
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. 
C		      (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. 
C		        (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C               NUMRA = NUMBER OF RANDOM ACCESS FILES.  (INPUT)
C           KFILRA(J) = UNIT NUMBERS FOR RANDOM ACCESS FILES
C                       (J=1,NUMRA).  (INPUT)
C           RACESS(J) = NAMES OF RANDOM ACCESS FILES (J=1,NUMRA).
C                       (INPUT/CHARACTER*60)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PRE-
C	 		DICTOR ID CORRESPONDING TO ID( ) (J=1,15).
C                       (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 
C			     LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK IN 
C			     TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE POR-
C			TIONS PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       JD( ) IS USED TO HELP IDENTIFY THE BASIC MODEL 
C			FIELDS
C                       AS READ FROM THE ARCHIVE.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.  
C			(INPUT)
C            SDATA(J) = WORK ARRAY (J=1,ND1).  NOT ACTUALLY USED...SEE
C                       TIMGRD.  (INTERNAL)
C                 ND1 = DIMENSION OF SDATA( ).  (INPUT)
C                NSTA = NUMBER OF STATIONS.  NOT ACTUALLY USED...SEE
C                       TIMGRD.  (INPUT)
C         NGRIDC(L,M) = HOLDS THE GRID CHARACTERISTICS (L=1,6) FOR EACH 
C			GRID COMBINATION (M=1,NGRID).
C                       L=1--MAP PROJECTION NUMBER (3=LAMBERT, 5=POLAR
C                            STEREOGRAPHIC). 
C                       L=2--GRID LENGTH IN MILLIMETERS,
C                       L=3--LATITUDE AT WHICH GRID LENGTH IS CORRECT 
C			     *10000,
C                       L=4--GRID ORIENTATION IN DEGREES *10000,
C                       L=5--LATITUDE OF LL CORNER IN DEGREES *10000,
C                       L=6--LONGITUDE OF LL CORNER IN DEGREES *10000.
C                ND11 = MAXIMUM NUMBER OF GRID COMBINATIONS THAT CAN BE
C                       DEALT WITH ON THIS RUN.  LAST DIMENSION OF
C                       NGRIDC( , ).  (INPUT)
C               NSLAB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE HEIGHTS, WHEN THE HEIGHTS
C                       HAVE BEEN CORRECTLY RETURNED.  WHEN IER NE 0, 
C			THIS VALUE SHOULD NOT BE USED.  (OUTPUT) 
C		  ND5 = DIMENSION OF IPACK( ),IWORK( ), AND DATA( ).
C			(INPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(K) = CALCULATED VARIABLE THAT IS RETURNED ON GRID 
C		        (K=1,ND5).  (OUTPUT)
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
C                              IN NGRIDC( ,L) DEFINING THE CHARACTERIS-
C			       TICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,NPRED) FOR WHICH 
C			       THIS VARIABLE IS NEEDED, WHEN IT IS NEED-
C			       ED ONLY ONCE FROM LSTORE( , ).  WHEN IT 
C			       IS NEEDED MORE THAN ONCE, THE VALUE IS 
C			       SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING 
C			       MOSTORE( , ).
C                              LATER USED AS A WAY OF DETERMINING WHE-
C			       THER TO KEEP THIS VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , ) 
C			THAT HAVE BEEN USED IN THIS RUN.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETIREVE THE DATA IDENTI-
C			FIED IN LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS
C		        FULL DATA ARE STORED ON DISK.  (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING  COUNT FROM THE BEGINNING OF THE
C                       PROGRAM.  THIS COUNT IS MAINTAINED IN CASE THE 
C			USER NEEDS IT (DIAGNOSTICS, ETC.).  (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  
C			(INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  
C			(INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       IS2(3) AND IS2(4) ARE USED BY THE CALLING
C                       PROGRAM AS THE GRID DIMENSIONS.  (INTERNAL-
C		        OUTPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  
C			(INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C               ND2X3 = DIMENSION OF THE VARIABLES BELOW. (INPUT)
C              FD1(K) = GRID WORK ARRAY (K=1,ND2X3).  (INTERNAL)
C              FD2(K) = GRID WORK ARRAY (K=1,ND2X3).  (INTERNAL)
C              FD3(K) = GRID WORK ARRAY (K=1,ND2X3).  (INTERNAL)
C              FD4(K) = GRID WORK ARRAY (K=1,ND2X3).  (INTERNAL)
C              FD5(K) = GRID WORK ARRAY (K=1,ND2X3).  (INTERNAL)
C              FD6(K) = GRID WORK ARRAY (K=1,ND2X3).  (INTERNAL)
C              FD7(K) = GRID WORK ARRAY (K=1,ND2X3).  (INTERNAL)
C               ISTAV = SET TO 0 TO INDICATE A GRID FIELD IS BEING RE-
C                       TURNED. (OUTPUT).
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64).  (INPUT)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        52 = WHEN THE "SLAB" OF THE GFS TEMP AND 
C                             DEWPT GRIDS DO NOT MATCH.
C                       103 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED.
C                       SEE GFETCH, CONSTG, TIMGRD, AND DEWPT FOR OTHER 
C                       VALUES.  (INTERNAL-OUTPUT)
C                       NOTE:  WHEN IER NE 0, DATA ARE RETURNED AS 
C                       MISSING.
C              FD8(K) = AUTOMATIC GRID WORK ARRAY (K=1,ND2X3).  
C                       (INTERNAL)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT 
C			THE RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ).  (INTERNAL)
C               LD(J) = HOLDS THE 4 VARIABLE ID WORDS OF SEVERAL VARIA-
C			BLES TO FETCH OR COMPUTE (J=1,4).  (INTERNAL)
C	    LDPARS(J) = AS FOR IDPARS( ), EXCEPT CORRESPONDING TO LD( )
C			(J=1,15).  (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS 
C			ZERO WHEN DATA ARE NOT PACKED.  (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C		        ZERO WHEN DATA ARE NOT PACKED.  (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH, PRSID1, DEWPT, TIMGRD
C
      CHARACTER*60 RACESS(NUMRA)
C      
      DIMENSION NGRIDC(6,ND11)
      DIMENSION SDATA(ND1)
      DIMENSION IDPARS(15),JD(4),LD(4),LDPARS(15),KFILRA(NUMRA)
      DIMENSION FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3),FD5(ND2X3),
     1          FD6(ND2X3),FD7(ND2X3)
      DIMENSION FD8(ND2X3)                           ! AUTOMATIC ARRAY
      DIMENSION DATA(ND5),IPACK(ND5),IWORK(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION ICCCFFF(5)
C
      DATA ICCCFFF/409020000,             ! LAMP TERRAIN
     1             002301005,             ! LAMP SFC TEMP
     2             003301005,             ! LAMP SFC DEWPOINT
     3             002000008,             ! GFS TEMPERATURE  
     4             003100008/             ! GFS DEWPOINT      
C 	 
C	 CHECK WHETHER THIS ROUTINE APPLIES TO ID(1).
C
D     CALL TIMPR(KFILDO,KFILDO,'START KIMOD         ')
C
      IF(IDPARS(1).EQ.007.AND.IDPARS(2).EQ.195) GO TO 30
C
      WRITE(KFILDO,20) JD
 20   FORMAT(/,' **** VARIABLE ',I9.9,2I10.9,I4.3,' CANNOT BE COMPU',
     1         'TED IN KIMOD ...SET IER=103 AND SUPPLY MISSING VALUES')
      IER=103
      GO TO 800   
C
 30   IER=0
      ISTAV=0
C
C	 FETCH LAMP TERRAIN FROM GRID RANDOM ACCESS CONSTANT FILE.
C
      LD(1)=ICCCFFF(1)
      LD(2)=0
      LD(3)=0
      LD(4)=0
C
      DO 35 J=1,NUMRA
         IF(KFILRA(J).EQ.44) THEN
C              IT IS SAFE TO USE ND2X3 AS DIMENSION OF FD1( ) SINCE
C              THE TERRAIN IS ON THE 80-KM LAMP GRID.
            CALL CONSTG(KFILDO,KFILRA(J),RACESS(J),LD,
     1                  IPACK,IWORK,FD1,ND2X3,
     2                  IS0,IS1,IS2,IS4,ND7,
     3                  ISTAV,L3264B,IER)
            IF(IER.NE.0)GO TO 800
            NWORDS=IS2(3)*IS2(4)
            GO TO 40
         ENDIF
 35   CONTINUE
C
C        GRID CONSTANT FILE MISSING.  MISSING VALUES SUPPLIED FOR
C        FOR MODIFIED K-INDEX.
C
      GO TO 800
C
C	 FETCH LAMP SURFACE TEMP.
C
 40   LD(1)=ICCCFFF(2)
      LD(2)=000000002
      LD(3)=IDPARS(12)-IDPARS(9)
C
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GO TO 800
C
C	 FETCH LAMP SURFACE DEWPOINT.
C
      LD(1)=ICCCFFF(3)
C
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GO TO 800
C
C	 COMPUTE MSL TEMPERATURE (FD2( )) AND DEWPOINT (FD3( )) ...NO 
C        CHECKS FOR MISSING.
C
      F=5./9.
C
      DO 100 J=1,NWORDS
        IF(FD3(J).GT.FD2(J)) FD3(J)=FD2(J)
C            NOTE: 273.16 SHOULD BE 273.15, ACCORDING TO MOS2000 NOTES.
C            SINCE 273.16 WAS USED FOR DEVELOPMENT, RETAIN IT IN OPERA-
C            TIONS FOR CONSISTENCY.
        FD2(J)=F*(FD2(J)-32.)+273.16+FD1(J)*.00976
        FD3(J)=F*(FD3(J)-32.)+273.16+FD1(J)*.0017
 100  CONTINUE
C
C	 GET 850-MB TEMP AND DEWPT, 700-MB TEMP AND DEWPT, AND 500-MB
C	 TEMP ...THE VARIABLES NEEDED TO COMPUTE THE K INDEX.  WHEN TAU
C	 IS NOT EVENLY DIVISIBLE BY 3 HOURS, THESE VARIABLES MUST BE 
C	 TIME-INTERPOLATED.  FOR BASIC VARIABLES, TIMGRD IS USED;  FOR
C	 DEWPT (A DERIVED VARIABLE), THE TIME INTERPOLATION IS PERFORMED
C	 DIRECTLY.
C
      NTAU=MOD(IDPARS(12),3)  
      F=FLOAT(NTAU)/3.
C
C	 CREATE ID FOR 850-MB TEMP (FD1( ))
C
      LD(1)=ICCCFFF(4)
      LD(2)=850
      LD(3)=IDPARS(9)*1000000+IDPARS(12)
      LD(4)=0
C
      IF(NTAU.EQ.0) THEN
        CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLABT,MISSP,MISSS,L3264B,1,IER)
        IF(MISSP.NE.0)MISTOT=MISTOT+1
        IF(IER.NE.0)GOTO 800
C
      ELSE
        CALL PRSID1(KFILDO,LD,LDPARS)
        CALL TIMGRD(KFILDO,KFIL10,LD,LDPARS,LD,NDATE,
     1              SDATA,ND1,NSTA,NSLABT,IPACK,IWORK,FD1,ND2X3,
     2              LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3              IS0,IS1,IS2,IS4,ND7,
     4              FD6,FD7,ND2X3,
     5              ISTAV,L3264B,MISTOT,IER)
        IF(IER.NE.0)GOTO 800     
      ENDIF
C
      NWORDS=IS2(3)*IS2(4)
C
C        TAKE SIMPLE AVERAGE OF 850-MB TEMP (FD1( )) AND MSL TEMP 
C        (FD2( )) AND SAVE IN FD1( ).
C
      DO 120 J=1,NWORDS
        FD1(J)=(FD1(J)+FD2(J))/2.0
 120  CONTINUE
C
C	 CREATE ID FOR 850-MB DEWPT (FD2( ))
C
      LD(1)=ICCCFFF(5)
C
      IF(NTAU.EQ.0) THEN
        CALL PRSID1(KFILDO,LD,LDPARS)
        CALL DEWPT(KFILDO,KFIL10,LDPARS,LD,NDATE,NGRIDC,ND11,NSLABD,
     1             IPACK,IWORK,FD2,ND2X3,
     2             LSTORE,ND9,LITEMS,CORE,ND10,
     3             NBLOCK,NFETCH,IS0,IS1,IS2,IS4,ND7,
     4             FD4,FD5,FD6,FD7,ND2X3,
     5             ISTAV,L3264B,MISTOT,IER)
C          NOTE:  FD2( ) IS DIMENSIONED ND5 IN DEWPT, BUT IT IS SAFE TO
C                 USE A DIMENSION OF ND2X3.  WHILE THIS RESULTS IN A
C                 DUPLICATION OF ND2X3 IN THE ARGUMENT LIST, IT DOES NO
C                 HARM.	
        IF(IER.NE.0)GO TO 800
        NWORDS=IS2(3)*IS2(4)
C
      ELSE
C
C          GET 850-MB DEWPT AT FIRST 3-HOURLY TIME.
C
        LD(3)=IDPARS(9)*1000000+IDPARS(12)-NTAU
        CALL PRSID1(KFILDO,LD,LDPARS)
        CALL DEWPT(KFILDO,KFIL10,LDPARS,LD,NDATE,NGRIDC,ND11,NSLABD,
     1             IPACK,IWORK,FD2,ND2X3,
     2             LSTORE,ND9,LITEMS,CORE,ND10,
     3             NBLOCK,NFETCH,IS0,IS1,IS2,IS4,ND7,
     4             FD4,FD5,FD6,FD7,ND2X3,
     5             ISTAV,L3264B,MISTOT,IER)
C          NOTE:  FD2( ) IS DIMENSIONED ND5 IN DEWPT, BUT IT IS SAFE TO
C                 USE A DIMENSION OF ND2X3.  WHILE THIS RESULTS IN A
C                 DUPLICATION OF ND2X3 IN THE ARGUMENT LIST, IT DOES NO
C                 HARM.	
        IF(IER.NE.0)GO TO 800
        NWORDS=IS2(3)*IS2(4)
C
C          GET 850-MB DEWPT AT 2ND 3-HOURLY TIME
C
        LD(3)=LD(3)+3
        CALL PRSID1(KFILDO,LD,LDPARS)
        CALL DEWPT(KFILDO,KFIL10,LDPARS,LD,NDATE,NGRIDC,ND11,NSLABD,
     1             IPACK,IWORK,DATA,ND5,
     2             LSTORE,ND9,LITEMS,CORE,ND10,
     3             NBLOCK,NFETCH,IS0,IS1,IS2,IS4,ND7,
     4             FD4,FD5,FD6,FD7,ND2X3,
     5             ISTAV,L3264B,MISTOT,IER)
        IF(IER.NE.0)GO TO 800
C
C          DO THE TIME INTERPOLATION
C
        DO 130 J=1, NWORDS
          FD2(J)=FD2(J)+F*(DATA(J)-FD2(J))       
 130    CONTINUE
C
      ENDIF
C
      IF(NSLABD.NE.NSLABT) THEN
        IER=52
        WRITE(KFILDO,1200) NSLABD,NSLABT,IER
 1200   FORMAT(/,' ****IN KIMOD FOR 850-MB DEWPT INTERPOLATION, NSLAB ',
     1         'FOR 850-MB DEWPT = ',I3,' DOES NOT EQUAL NSLAB FOR ',
     2         '850-MB TEMP = ',I3,'.  SET IER = ',I3)
        GO TO 800
      ENDIF
C
C        TAKE SIMPLE AVERAGE OF 850-MB DEWPT (FD2( )) AND 
C        MSL DEWPT (FD3( )) AND SAVE IN FD2( ).
C
      DO 140 J=1,NWORDS
        FD2(J)=(FD2(J)+FD3(J))/2.0
 140  CONTINUE
C
C        GET 700-MB TEMP AND PUT INTO FD3( ).
C
      LD(1)=ICCCFFF(4)
      LD(2)=700
      LD(3)=IDPARS(9)*1000000+IDPARS(12)
C
      IF(NTAU.EQ.0) THEN
        CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD3,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLABT,MISSP,MISSS,L3264B,1,IER)
        IF(MISSP.NE.0)MISTOT=MISTOT+1
        IF(IER.NE.0)GOTO 800
C
      ELSE
        CALL PRSID1(KFILDO,LD,LDPARS)
        CALL TIMGRD(KFILDO,KFIL10,LD,LDPARS,LD,NDATE,
     1              SDATA,ND1,NSTA,NSLABT,IPACK,IWORK,FD3,ND2X3,
     2              LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3              IS0,IS1,IS2,IS4,ND7,
     4              FD6,FD7,ND2X3,
     5              ISTAV,L3264B,MISTOT,IER)
        IF(IER.NE.0)GOTO 800
      ENDIF
C
        NWORDS=IS2(3)*IS2(4)
C
      IF(NSLABT.NE.NSLABD) THEN
        IER=52
        WRITE(KFILDO,1400) NSLABT,NSLABD,IER
 1400   FORMAT(/,' ****IN KIMOD FOR 700-MB TEMP INTERPOLATION, NSLAB ',
     1         'FOR 700-MB TEMP = ',I3,' DOES NOT EQUAL NSLAB FOR ',
     2         '850-MB DEWPOINT = ',I3,'.  SET IER = ',I3)
        GO TO 800
      ENDIF
C
C	 GET 700-MB DEWPT AND PUT INTO FD4( ).
C
      LD(1)=ICCCFFF(5)
C
      IF(NTAU.EQ.0) THEN
        CALL PRSID1(KFILDO,LD,LDPARS)
        CALL DEWPT(KFILDO,KFIL10,LDPARS,LD,NDATE,NGRIDC,ND11,NSLABD,
     1             IPACK,IWORK,FD4,ND2X3,
     2             LSTORE,ND9,LITEMS,CORE,ND10,
     3             NBLOCK,NFETCH,IS0,IS1,IS2,IS4,ND7,
     4             FD5,FD6,FD7,FD8,ND2X3,
     5             ISTAV,L3264B,MISTOT,IER)
C          NOTE:  FD4( ) IS DIMENSIONED ND5 IN DEWPT, BUT IT IS SAFE TO
C                 USE A DIMENSION OF ND2X3.  WHILE THIS RESULTS IN A
C                 DUPLICATION OF ND2X3 IN THE ARGUMENT LIST, IT DOES NO
C                 HARM.	
        IF(IER.NE.0)GO TO 800
        NWORDS=IS2(3)*IS2(4)
C
      ELSE
C
C          GET 700-MB DEWPT AT FIRST 3-HOURLY TIME
C
        LD(3)=IDPARS(9)*1000000+IDPARS(12)-NTAU
        CALL PRSID1(KFILDO,LD,LDPARS)
        CALL DEWPT(KFILDO,KFIL10,LDPARS,LD,NDATE,NGRIDC,ND11,NSLABD,
     1             IPACK,IWORK,FD4,ND2X3,
     2             LSTORE,ND9,LITEMS,CORE,ND10,
     3             NBLOCK,NFETCH,IS0,IS1,IS2,IS4,ND7,
     4             FD5,FD6,FD7,FD8,ND2X3,
     5             ISTAV,L3264B,MISTOT,IER)
C          NOTE:  FD4( ) IS DIMENSIONED ND5 IN DEWPT, BUT IT IS SAFE TO
C                 USE A DIMENSION OF ND2X3.  WHILE THIS RESULTS IN A
C                 DUPLICATION OF ND2X3 IN THE ARGUMENT LIST, IT DOES NO
C                 HARM.	
        IF(IER.NE.0)GO TO 800
        NWORDS=IS2(3)*IS2(4)
C
C          GET 700-MB DEWPT AT 2ND 3-HOURLY TIME
C
        LD(3)=LD(3)+3
        CALL PRSID1(KFILDO,LD,LDPARS)
        CALL DEWPT(KFILDO,KFIL10,LDPARS,LD,NDATE,NGRIDC,ND11,NSLABD,
     1             IPACK,IWORK,DATA,ND5,
     2             LSTORE,ND9,LITEMS,CORE,ND10,
     3             NBLOCK,NFETCH,IS0,IS1,IS2,IS4,ND7,
     4             FD5,FD6,FD7,FD8,ND2X3,
     5             ISTAV,L3264B,MISTOT,IER)
        IF(IER.NE.0)GO TO 800
C
C          DO THE TIME INTERPOLATION
C
        DO 150 J=1,NWORDS
          FD4(J)=FD4(J)+F*(DATA(J)-FD4(J))       
 150    CONTINUE 
C
      ENDIF
C
      IF(NSLABD.NE.NSLABT) THEN
        IER=52
        WRITE(KFILDO,1440) NSLABD,NSLABT,IER
 1440   FORMAT(/,' ****IN KIMOD FOR 700-MB DEWPT INTERPOLATION, NSLAB ',
     1         'FOR 700-MB DEWPT = ',I3,' DOES NOT EQUAL NSLAB FOR ',
     2         '700-MB TEMP = ',I3,'.  SET IER = ',I3)
        GO TO 800
      ENDIF
C
C        GET 500-MB TEMP AND PUT INTO FD5( ).
C
      LD(1)=ICCCFFF(4)
      LD(2)=500
      LD(3)=IDPARS(9)*1000000+IDPARS(12)
C
      IF(NTAU.EQ.0) THEN
        CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD5,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLABT,MISSP,MISSS,L3264B,1,IER)
        IF(MISSP.NE.0)MISTOT=MISTOT+1
        IF(IER.NE.0)GOTO 800
C
      ELSE
        CALL PRSID1(KFILDO,LD,LDPARS)
        CALL TIMGRD(KFILDO,KFIL10,LD,LDPARS,LD,NDATE,
     1              SDATA,ND1,NSTA,NSLABT,IPACK,IWORK,FD5,ND2X3,
     2              LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3              IS0,IS1,IS2,IS4,ND7,
     4              FD6,FD7,ND2X3,
     5              ISTAV,L3264B,MISTOT,IER)
        IF(IER.NE.0)GOTO 800
      ENDIF
C
      NWORDS=IS2(3)*IS2(4)
C
      IF(NSLABT.NE.NSLABD) THEN
        IER=52
        WRITE(KFILDO,1650) NSLABT,NSLABD,IER
 1650   FORMAT(/,' ****IN KIMOD FOR 500-MB TEMP INTERPOLATION, NSLAB ',
     1         'FOR 500-MB TEMP = ',I3,' DOES NOT EQUAL NSLAB FOR ',
     2         '700-MB DEWPT = ',I3,'.  SET IER = ',I3)
        GO TO 800
      ENDIF
C
C	 COMPUTE THE MODIFIED K INDEX AND PUT INTO DATA( ).  THE K INDEX
C	 IS DEFINED AS KI = (T850-T500) + TD850 - (T700-TD700)
C
      DO 180 J=1,NWORDS
        DATA(J)=(FD1(J)-FD5(J))+ FD2(J)-(FD3(J)-FD4(J))
 180  CONTINUE
C
      GO TO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C
 800  DO 810 J=1,ND5
      DATA(J)=9999.
 810  CONTINUE
C
 900  RETURN
      END
