      SUBROUTINE CHNGID(KFILDO,KFILRA,NUMRA,IP12,
     1                  ID,IDPARS,JD,
     2                  NDATE,CCALL,XDATA,ND1,NSTA,
     3                  ND5,IS0,IS1,IS2,IS4,ND7,IOPT,
     4                  INDEX,IER)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***
C
C SUBPROGRAM: CHNGID 
C   PRGMMR: MALONEY         ORG: W/OST22     DATE: 2001-01-15
C
C ABSTRACT: TO CHANGE CERTAIN MOS-2000 IDS.  SEE ITABLE( , ),
C   ITABL2( , ), AND ITABL3( , ) FOR THE MAPPING IDS.  ADDITIONAL
C   IDS MAY BE ADDED TO ITABLE( , ), ITABL2( , ), AND ITABL3( , ),
C   BUT REMEMBER TO CHANGE THE PARAMETER NDIM/NDIM2/NDIM3 APPROPRIATELY.
C
C PROGRAM HISTORY LOG:
C   01-01-15  MALONEY 
C   01-02-01  MALONEY    ADDED IDS FOR MRF
C   01-03-01  MALONEY    ADDED NCEP DOCBLOCK
C   01-03-28  MALONEY    CLEANED UP CALL 
C   01-03-29  MALONEY    INTRODUCED IOPT
C   01-04-03  MALONEY    ADDED B TO ITABLE MATCHING
C   01-05-09  MALONEY    CLEANED UP COMMENTS, ADDED IOPT=5, ADDED NUMRA
C                        INTO CALL TO ALLOW MULTIPLE RANDOM ACCESS FILES
C
C USAGE:  CALLED BY OPTX  
C
C        DATA SET USE
C        INPUT FILES:
C             FORT.XX - INDICATE NAME AND PURPOSE
C           KFILRA(J) - THE UNIT NUMBERS FOR WHICH RANDOM ACCESS FILES
C                       ARE AVAILABLE (J=1,5). 
C
C        OUTPUT FILES:  (INCLUDING WORK FILES)
C             FORT.XX - INDICATE NAME AND PURPOSE
C              KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C              IP12   - INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE INPUT FILES WILL BE PRINTED TO 
C                       THE FILE WHOSE UNIT NUMBER IS IP12. 
C
C        VARIABLES: 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT) 
C           KFILRA(J) = THE UNIT NUMBERS FOR WHICH RANDOM ACCESS FILES
C                       ARE AVAILABLE (J=1,NUMRA).  (INPUT)
C               NUMRA = THE NUMBER OF R.ACCESS FILES AVAILABLE.  (INPUT)
C                IP12 = INDICATES WHETHER (>0) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE EXTERNAL RANDOM ACCESS FILES
C                       WILL BE LISTED TO UNIT IP12.  (INPUT)
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
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST.  EQUIVALENCED TO 
C                       ICALL( , , ). (CHARACTER*8)  (INPUT)
C            XDATA(K) = DATA TO BE ARCHIVED (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  DIMENSION OF XDATA( ). (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C                 ND5 = NEEDED FOR WORK IN READ_MOSDA.  (INPUT)      
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
C                IOPT = INDICATES PROCESSING TO DO IN CHNGID.
C                         0 = DO NOTHING (WILL NOT ENTER CHNGID)
C                         1 = CHANGE CCCFFF (IDPARS(1) AND (2))
C                         2 = CHANGE DD (IDPARS(4))
C                         3 = CHANGE LLLL (IDPARS(6))
C                         4 = BOTH 1 AND 2
C                         5 = BOTH 1 AND 3
C                         9 = DO ALL (DEBUG)
C                       OTHER NON-ZERO VALUES MAY BE ADDED.  (INPUT)
C               INDEX = USED FOR READ_MOSDA (INPUT/OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       102 = ID NOT ACCOMMODATED.
C                       SEE RETVEC FOR OTHER VALUES.  (OUTPUT)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       XDATA( ) (J=1,4).  (INTERNAL)
C           LDPARS(J) = PARSED VALUES CORRESPONDING TO LD( ) (J=1,15)
C                       (INTERNAL)
C         ITABLE(I,J) = VALUES OF CCCFFF ACCOMMODATED (I=1) AND
C                       THE ASSOCIATED CCC (I=2) AND FFF (I=3) NEEDED.
C                       (J=1,NDIM).  (INTERNAL)
C         ITABL2(I,J) = VALUES OF DD ACCOMODATED (I=1) AND THE
C                       ASSOCIATED MODEL NEEDED (I=2) (J=1,NDIM2).
C                       (INTERNAL)
C         ITABL3(I,J) = VALUES OF IDPARS(6) ACCOMODATED (I=1) AND
C                       THE ASSOCIATED IDPARS(6) NEEDED (I=2) (J=1,NDIM3).
C                       (INTERNAL)
C                NDIM = SECOND DIMENSION OF ITABLE( , ).  SET BY
C                       PARAMETER.  (INTERNAL)
C               NDIM2 = SECOND DIMENSION OF ITABL2( , ).  SET BY
C                       PARAMETER.  (INTERNAL)
C               NDIM3 = SECOND DIMENSION OF ITABL3( , ).  SET BY
C                       PARAMETER.  (INTERNAL)
C 
C        SUBPROGRAMS CALLED:
C             UNIQUE: - READ_MOSDA
C          LIBRARY:
C           MDLLIB90: - READ_MOSDA 
C
C        EXIT STATES:
C          COND =    0 - SUCCESSFUL RUN
C                  !=0 - ERROR RETURNED FROM READ_MOSDA
C
C REMARKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90 (xlf compiler)
C   MACHINE:  IBM SP
C
C$$$
C
      PARAMETER (NDIM=15, NDIM2=21, NDIM3=2)
C
      CHARACTER*8 CCALL(ND1,6)
C
      DIMENSION XDATA(ND1),INDEX(ND1,15)
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LD(4),LDPARS(15),KFILRA(5)
      DIMENSION ITABLE(3,NDIM),ITABL2(2,NDIM2),ITABL3(2,NDIM3)
C
C        DATA TABLES
C
      DATA LDPARS/15*0/  
C
C        ITABLE   CCCFFFB  CCC  FFF
C 
      DATA ITABLE/2080410, 208, 040,
     1            2083410, 208, 340,
     2            2085460, 208, 545,
     3            2081210, 208, 120,
     4            2082910, 208, 290,
     5            2083910, 208, 390,
     6            2085560, 208, 555,
     7            2033210, 203, 320,
     8            2034210, 203, 420,
     9            2033301, 203, 322,
     A            2034301, 203, 423,
     B            2073301, 207, 322,
     C            2073751, 207, 367,
     D            2074301, 207, 422,
     E            2074751, 207, 467/
C
C        ITABL2   DD  DD
C
      DATA ITABL2/60, 08,
     1            61, 08,
     2            62, 08,
     3            63, 08,
     4            64, 08,
     5            65, 08,
     6            66, 08,
     7            67, 08,
     8            68, 08,
     9            69, 08,
     A            70, 09,
     B            71, 09,
     C            72, 09,
     D            73, 09,
     E            74, 09,
     F            75, 09,
     G            76, 09,
     H            77, 09,
     I            78, 09,
     J            79, 09,
     K            39, 09/
C
C        ITABL3   LLLL  LLLL
C
      DATA ITABL3/1000, 0000,
     1            2000, 0000/
C
      IER=0
C
C        FIND CCCFFF OF ID(1) IN ITABLE(1, ).
C
      IF(IOPT.EQ.1.OR.IOPT.EQ.4.OR.IOPT.EQ.5.OR.IOPT.EQ.9) THEN
         DO 105 JJ=1,NDIM
            IF(ITABLE(1,JJ).EQ.
     +         IDPARS(1)*10000+IDPARS(2)*10+IDPARS(3)) THEN
                IDPARS(1)=ITABLE(2,JJ)
                IDPARS(2)=ITABLE(3,JJ)
                GO TO 109
            ENDIF
 105     CONTINUE
      ENDIF
C
C        FIND DD OF ID(1) IN ITABL2(1, ).
C
 109  IF(IOPT.EQ.2.OR.IOPT.EQ.4.OR.IOPT.EQ.9) THEN
         DO 110 KK=1,NDIM2
            IF(ITABL2(1,KK).EQ.IDPARS(4)) THEN
               IDPARS(4)=ITABL2(2,KK)
               GO TO 114
            ENDIF
 110     CONTINUE
      ENDIF
C
C        FIND LLLL OF ID(2) IN ITABL3(1, ).
C
 114  IF(IOPT.EQ.3.OR.IOPT.EQ.5.OR.IOPT.EQ.9) THEN
         DO 115 LL=1,NDIM3
            IF(ITABL3(1,LL).EQ.IDPARS(6)) THEN
               IDPARS(6)=ITABL3(2,LL)
               GO TO 200
            ENDIF
 115     CONTINUE
      ENDIF 
C
C        FILL LD( ) AND LDPARS( ) 
C
 200  LD(1)=IDPARS(1)*1000000+IDPARS(2)*1000+IDPARS(3)*100+IDPARS(4)
      LD(2)=IDPARS(5)*100000000+IDPARS(6)*10000+IDPARS(7)
      LD(3)=JD(3)
C
      IF (IDPARS(3).EQ.0) THEN
         LD(4)=IDPARS(13)*100+IDPARS(14)*10+IDPARS(15)
      ELSE
	 LD(4)=ID(4)
      ENDIF
C
C****      WRITE(KFILDO,202) (LD(N),N=1,4), (ID(M),M=1,4)
C**** 202  FORMAT(/,'LD = ',I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
C****     +       /,'ID = ',I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
C        READ THE DATA INTO THE DESIRED ID
C
      DO 210 N=1,NUMRA
C
         CALL READ_MOSDA(KFILDO,KFILRA(N),IP12,
     2                   LD,NDATE,CCALL,NSTA,
     3                   XDATA,ND1,ND5,ND7,IS0,IS1,
     4                   IS2,IS4,INDEX,IER)
         IF(IER.EQ.0) GOTO 350
C
 210  CONTINUE
C
C        THIS VARIABLE CANNOT BE RETRIEVED.  SET XDATA( ) TO MISSING.
C        THIS IS FOR SAFETY; XDATA( ) SHOULD ALREADY BE SET TO MISSING.
C
         WRITE(KFILDO,225)(LD(J),J=1,4)
 225     FORMAT(/,' ****VARIABLE NOT RETRIEVED BY READ_MOSDA IN CHNGID',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
C
 300  DO 310 K=1,NSTA
         XDATA(K)=9999.
 310  CONTINUE 
C
 350  RETURN
      END     
