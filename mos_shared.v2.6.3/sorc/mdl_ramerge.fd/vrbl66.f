      SUBROUTINE VRBL66(KFILDO,KFILRA,NUMRA,
     1                  ID,IDPARS,JD,NWHERE,NVRBL,
     2                  NDATE,CCALL,XDATA,ND1,
     3                  AA,NSTA,DATA,ND5,
     4                  IS0,IS1,IS2,IS4,ND7,
     5                  IP12,ISTOP,IER)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***
C
C SUBPROGRAM: VRBL66
C   PRGMMR: GLAHN           ORG: W/OST22     DATE: 1996-01-01
C
C ABSTRACT:  TO OBTAIN FOR U660 ALL VARIABLES IDENTIFIED IN ID( , ),
C   AND IDPARS( , ).  "BASIC" VARIABLES (THE VARIABLES SANS PROCESSING 
C   INFORMATION) ARE IDENTIFIED IN JD( , ), RESPECTIVELY.  VARIABLES 
C   ARE OBTAINED THROUGH CALLING SUBROUTINE OPTX_MERGE.  
C
C PROGRAM HISTORY LOG:
C   98-01-01  GLAHN
C   01-03-02  MALONEY    ADDED NCEP DOCBLOCK, W3TAGE CALL
C   01-03-29  MALONEY    EDITED CALL TO OPTX_ARCHIVE
C   01-04-03  MALONEY    CLEANED OUT CODE
C   01-05-09  MALONEY    RE-ADDED NUMRA TO CALL TO OPTX_ARCHIVE
C   05-02-28  DALLAVALLE ELIMINATED CALL TO OPTX_ARCHIVE; ADDED
C                        CALL TO READ_MOSDA SO AS TO OBTAIN
C                        VECTOR DATA DIRECTLY FROM THE RANDOM
C                        ACCESS FILES.
C
C USAGE:  CALL VRBL66(KFILDO,KFILRA,NUMRA,ID,IDPARS,JD,NWHERE,
C                     NVRBL,NDATE,CCALL,XDATA,ND1,AA,NSTA,ND5,
C                     IS0,IS1,IS2,IS4,ND7,IP12,ISTOP,IER)
C   INPUT ARGUMENT LIST:
C
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C           KFILRA(J) = UNIT NUMBERS FOR READING MOS-2000 EXTERNAL
C                       RANDOM ACCESS FILES (J=1,NUMRA).  (INPUT)
C               NUMRA = NUMBER OF VALUES IN KFILRA( ) AND RACESS( ).
C                       (INPUT)
C             ID(J,N) = THE INTEGER VARIABLE ID'S (J=1,4) (N=1,NVRBL).
C                       (INPUT)
C         IDPARS(J,N) = THE PARSED, INDIVIDUAL COMPONENTS OF THE VARIABLE
C                       ID CORRESPONDING TO ID( ) (J=1,15) (N=1,NVRBL).
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
C                       J=15--G (GRID INDICATOR).  HAS NO MEANING EXCEPT IN
C                             U201.
C             JD(J,N) = THE BASIC INTEGER VARIABLE ID (J=1,4) (N=1,NVRBL).
C                       THIS IS THE SAME AS ID(J,N), EXCEPT THAT SOME PORTIONS
C                       ARE OMITTED:
C                       B = IDPARS(3, ),
C                       G = IDPARS(15, ), AND
C                       TRESHL( ).
C                       JD( , ) IS USED TO IDENTIFY WHICH CALCULATIONS
C                       CAN BE MADE DIRECTLY IN U600, WHICH IS ONLY FORMING
C                       BINARIES.  THE "G" VARIABLE HAS NO MEANING IN U660,
C                       IT BEING ONLY FOR POSSIBLE USE IN U201.
C                       (INPUT)
C               NVRBL = THE NUMBER OF VARIABLES NEEDED AND IDENTIFIED IN 
C                       ID( , ), ETC.  ALSO TREATED AS THE DIMENSION OF THE
C                       VARIABLES ID( , ), ETC.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH VARIABLES ARE TO BE
C                       FURNISHED ON THIS CALL TO VRBL66.  (INPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST.  (CHARACTER*8)
C                       (INPUT/OUTPUT)
C            XDATA(K) = THE DATA FOR THE NSTA STATIONS BEING PROCESSED
C                       (K=1,NSTA).  (INTERNAL)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       DIMENSION OF SEVERAL ARRAYS.  (INPUT)
C                NSTA = THE NUMBER OF STATIONS IN CCALL( , ).  (INPUT)
C                 ND5 = PASSED TO OPTX_MERGE AND THEN TO READ_MOSDA.
C                       (INPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C                IP12 = INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE INPUT FILES WILL BE PRINTED TO 
C                       THE FILE WHOSE UNIT NUMBER IS IP12.  (INPUT)
C            ISTOP(J) = FOR J=1, ISTOP( ) IS INCREMENTED BY 1 EACH TIME
C                       AN ERROR OCCURS THAT MAY BE FATAL.
C                       FOR J=2, ISTOP( ) IS INCREMENTED BY 1 WHENEVER AN
C                       INPUT DATA RECORD IS NOT FOUND.  (INPUT-OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        39 = NWORDS FROM GFETCH DO NOT EQUAL NSTA.
C                       SEE OTHER ROUTINES FOR OTHER VALUES.
C
C   OUTPUT ARGUMENT LIST:  
C           NWHERE(N) = INDICATES WHERE THE VARIABLE IS TO COME FROM (N=1,ND4)
C                       0 = UNDETERMINED
C                       1 = FROM INPUT FILE
C                       2 = BINARY FROM BASIC VARIABLE.  IT IS POSSIBLE
C                           THAT A VARIABLE REUSABLE IN VRBL66 WILL
C                           NOT BE IN VRBL67, SO THIS MUST BE ALLOWED
C                           FOR THERE.
C                       3 = FROM OPTX.  THIS INCLUDES THE CASES WHERE A
C                           POINT BINARY IS MADE FROM A COMPUTED VARIABLE.
C                       (OUTPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST.  (CHARACTER*8)
C                       (INPUT/OUTPUT)
C             AA(N,K) = MATRIX OF VALUES TO RETURN (N=1,NVRBL) (K=1,NSTA).
C                       (OUTPUT)
C            ISTOP(J) = FOR J=1, ISTOP( ) IS INCREMENTED BY 1 EACH TIME
C                       AN ERROR OCCURS THAT MAY BE FATAL.
C                       FOR J=2, ISTOP( ) IS INCREMENTED BY 1 WHENEVER AN
C                       INPUT DATA RECORD IS NOT FOUND.  (INPUT-OUTPUT)
C
C        DATA SET USE
C        INPUT FILES:
C      FORT.KFILRA(J) - UNIT NUMBERS FOR EXTERNAL RANDOM ACCESS FILES
C                       (J=1,5).  (INPUT/OUTPUT)
C                       (INPUT)
C
C        OUTPUT FILES:
C      FORT.KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  SET BY
C                       DATA STATEMENT IN DRU660.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C           KFILRA(J) = UNIT NUMBERS FOR READING MOS-2000 EXTERNAL
C                       RANDOM ACCESS FILES (J=1,NUMRA).  (INPUT)
C               NUMRA = NUMBER OF VALUES IN KFILRA( ) AND RACESS( ).
C                       (INPUT)
C             ID(J,N) = THE INTEGER VARIABLE ID'S (J=1,4) (N=1,NVRBL).
C                       (INPUT)
C         IDPARS(J,N) = THE PARSED, INDIVIDUAL COMPONENTS OF THE VARIABLE
C                       ID CORRESPONDING TO ID( ) (J=1,15) (N=1,NVRBL).
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
C                       J=15--G (GRID INDICATOR).  HAS NO MEANING EXCEPT IN
C                             U201.
C             JD(J,N) = THE BASIC INTEGER VARIABLE ID (J=1,4) (N=1,NVRBL).
C                       THIS IS THE SAME AS ID(J,N), EXCEPT THAT SOME PORTIONS
C                       ARE OMITTED:
C                       B = IDPARS(3, ),
C                       G = IDPARS(15, ), AND
C                       TRESHL( ).
C                       JD( , ) IS USED TO IDENTIFY WHICH CALCULATIONS
C                       CAN BE MADE DIRECTLY IN U600, WHICH IS ONLY FORMING
C                       BINARIES.  THE "G" VARIABLE HAS NO MEANING IN U660,
C                       IT BEING ONLY FOR POSSIBLE USE IN U201.
C                       (INPUT)
C           NWHERE(N) = INDICATES WHERE THE VARIABLE IS TO COME FROM (N=1,ND4)
C                       0 = UNDETERMINED
C                       1 = FROM INPUT FILE
C                       2 = BINARY FROM BASIC VARIABLE.  IT IS POSSIBLE
C                           THAT A VARIABLE REUSABLE IN VRBL66 WILL
C                           NOT BE IN VRBL67, SO THIS MUST BE ALLOWED
C                           FOR THERE.
C                       3 = FROM OPTX.  THIS INCLUDES THE CASES WHERE A
C                           POINT BINARY IS MADE FROM A COMPUTED VARIABLE.
C                       (OUTPUT)
C               NVRBL = THE NUMBER OF VARIABLES NEEDED AND IDENTIFIED IN 
C                       ID( , ), ETC.  ALSO TREATED AS THE DIMENSION OF THE
C                       VARIABLES ID( , ), ETC.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH VARIABLES ARE TO BE
C                       FURNISHED ON THIS CALL TO VRBL66.  (INPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST.  (CHARACTER*8)
C                       (INPUT/OUTPUT)
C            XDATA(K) = THE DATA FOR THE NSTA STATIONS BEING PROCESSED
C                       (K=1,NSTA).  (INTERNAL)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       DIMENSION OF SEVERAL ARRAYS.  (INPUT)
C             AA(N,K) = MATRIX OF VALUES TO RETURN (N=1,NVRBL) (K=1,NSTA).
C                       (OUTPUT)
C                NSTA = THE NUMBER OF STATIONS IN CCALL( , ).  (INPUT)
C                 ND5 = PASSED TO OPTX_ARCHIVE AND THEN TO READ_MOSDA.
C                       (INPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C                IP12 = INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE INPUT FILES WILL BE PRINTED TO 
C                       THE FILE WHOSE UNIT NUMBER IS IP12.  (INPUT)
C            ISTOP(J) = FOR J=1, ISTOP( ) IS INCREMENTED BY 1 EACH TIME
C                       AN ERROR OCCURS THAT MAY BE FATAL.
C                       FOR J=2, ISTOP( ) IS INCREMENTED BY 1 WHENEVER AN
C                       INPUT DATA RECORD IS NOT FOUND.  (INPUT-OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        39 = NWORDS FROM GFETCH DO NOT EQUAL NSTA.
C                       SEE OTHER ROUTINES FOR OTHER VALUES.
C                       (INTERNAL-OUTPUT)
C               ITIME = TO FURNISH TO GFETCH TO INDICATE NO TIME OFFSET
C                       IS TO BE MADE BECAUSE OF RR AND AT THE SAME TIME
C                       FOR PREDICTANDS, MINUS THE VALUE OF TAU.  THIS
C                       LATTER IS STORED IN LSTORE(12, ) BY GFETCH TO
C                       ALLOW LMSTR6 TO CALCULATE THE MAXIMUM TAU BY
C                       INPUT UNIT NUMBER.  (INTERNAL)
C              NWORDS = NUMBER OF WORDS IN DATA( ) RETURNED FROM GFETCH.
C                       (INTERNAL)
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT THE 
C                       DATA HAVE BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ).  (INTERNAL)
C              NSOURC = THE "MODEL NUMBER" OR SOURCE OF DATA TAKEN FROM
C                       LSTORE(10, ) BY GFETCH.  (INTERNAL)
C               MISSP = WHEN PRIMARY MISSING VALUE INDICATORS ARE IN THE
C                       DATA RETURNED FROM GFETCH, THEY WILL HAVE THE 
C                       VALUE FLOAT(MISSP).  OTHERWISE, MISSP = 0.
C                       (INTERNAL)
C               MISSS = WHEN SECONDARY MISSING VALUE INDICATORS ARE IN THE
C                       DATA RETURNED FROM GFETCH, THEY WILL HAVE THE 
C                       VALUE FLOAT(MISSS).  OTHERWISE, MISSS = 0.
C                       VRBL66 TREATS THIS MISSING VALUE AS ZERO.
C                       (INTERNAL)
C 
C        SUBPROGRAMS CALLED:
C             UNIQUE:   - OPTX_MERGE  
C          LIBRARY:
C             MDLLIB90  - TIMPR, UPDAT  
C                W3LIB  - W3TAGE
C
C        EXIT STATES:
C          COND =    0  - SUCCESSFUL RUN
C                 1345  - PARAMETER ND9 IS TOO SMALL (SHOULD NOT HAPPEN)
C
C REMARKS: 
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90 (xlf90 compiler)
C   MACHINE:  IBM SP
C
C$$$
C
      CHARACTER*8 CCALL(ND1,6)
C
      DIMENSION XDATA(ND1)
      DIMENSION DATA(ND5)
      DIMENSION ID(4,NVRBL),IDPARS(15,NVRBL),
     1          JD(4,NVRBL),
     2          NWHERE(NVRBL)
      DIMENSION AA(NVRBL,NSTA)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION ISTOP(2),KFILRA(5)
      DIMENSION INDEX(ND1,15)
C
      IER=0
      NFIRST=0
      ICOUNT=0
C
C        INITIALIZE THE MATRIX AA( , ).
C 
      DO 105 K=1,NSTA
C
      DO 104 N=1,NVRBL
      AA(N,K)=9999.
 104  CONTINUE
C
 105  CONTINUE
C
      DO 107 N=1,NVRBL
      NWHERE(N)=0  
 107  CONTINUE
C
C        FIND ALL VARIABLES FOR THE DATE IN NDATE.
C        READ THROUGH THE NUMRA RANDOM ACCESS FILES IN ORDER.
C        SINCE UNIT 49 IS RESERVED FOR WRITING, UNIT 49
C        IS NOT READ.
C
      DO 500 J=1,NUMRA
      IF(KFILRA(J).NE.49)THEN
         NFIRST=NFIRST+1
         ICOUNT=0
         DO 400 N=1,NVRBL
         IER=0
C
C        THE DATA WILL BE RETURNED IN DATA( ).
C
         CALL READ_MOSDA(KFILDO,KFILRA(J),IP12,
     1                   ID(1,N),NDATE,CCALL,NSTA,
     2                   DATA,ND1,ND5,ND7,IS0,IS1,
     3                   IS2,IS4,INDEX,IER)
C
C        NOW FILL THE AA MATRIX
C
         IF(IER.EQ.0.OR.IER.EQ.120)THEN
            NWHERE(N)=NWHERE(N)+1
            ICOUNT=ICOUNT+1
            IF(NFIRST.EQ.1)THEN
               DO 185 K=1,NSTA
               AA(N,K)=DATA(K)
 185           CONTINUE
            ELSE
               DO 190 K=1,NSTA
               IF(NINT(AA(N,K)).EQ.9999.AND.
     1            NINT(DATA(K)).NE.9999)THEN
                  AA(N,K)=DATA(K)
               ENDIF
 190           CONTINUE
            ENDIF
            IF(IER.EQ.120)THEN
               ISTOP(1)=ISTOP(1)+1
C              IER = 120 FROM FINDST IN READ_MOSDA MEANS ONE OR MORE
C              STATIONS NOT FOUND IN THE DIRECTORY.  THIS IS NOT FATAL.
               IER=0
            ENDIF
C
         ELSE
            ISTOP(2)=ISTOP(2)+1
C              AN ERROR IN READ_MOSDA GENERATES A DIAGNOSTIC AND DATA IN
C              DATA( ) HAVE BEEN SET TO 9999.  ISTOP(2) IS INCREMENTED,
C              EVEN WHEN IER = 47, WHICH JUST MEANS DATA COULD NOT 
C              BE FOUND.  (IT IS POSSIBLE THAT ERRORS OTHER THAN JUST
C              MISSING DATA OCCURRED.)  ARRAY AA( , ) IS NOT TOUCHED
C              SINCE ARRAY WAS INITIALIZED WITH 9999.
         ENDIF
C
 400     CONTINUE
         WRITE(KFILDO,410)ICOUNT,KFILRA(J)
 410     FORMAT(/,' FOUND AND READ ',I6,' RECORDS IN ',
     1          'RANDOM ACCESS FILE UNIT NUMBER ',I2)
C
      ENDIF
C
 500  CONTINUE
      DO 600 N=1,NVRBL
      IF(NWHERE(N).EQ.0)THEN
         WRITE(KFILDO,595)(ID(L,N),L=1,4),NDATE
 595     FORMAT(/,' ****VRBL NOT READ IN ANY RANDOM ACCESS FILE',
     1          /,'     ID = ',I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2          ' FOR DATE ',I11,'.')
      ENDIF
 600  CONTINUE
C
      RETURN
      END
