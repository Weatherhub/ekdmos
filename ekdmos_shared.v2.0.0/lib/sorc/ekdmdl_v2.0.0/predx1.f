      SUBROUTINE PREDX1(KFILDO,IDPARS,THRESH,
     1                  SMULT,SADD,ORIGIN,CINT,
     2                  PLAIN,UNITS,NDATE,KHR,DATA,ND5,
     3                  IS2,ND7,IP14,ISTOP,IER)
C
C        JULY 1994      GLAHN   TDL   MOS-2000
C        OCTOBER 2003   SMB   CORRECTED FORMAT STATEMENT 213
C                             FOR THE IBM 
C
C        PURPOSE
C           TO GRIDPRINT A FIELD FOR PRED1 OR PRED2.
C   
C        DATA SET USE
C            KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID( ) (J=1,15).  (INPUT)
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
C                       J=15--G (GRID INDICATOR).
C              THRESH = THE BINARY THRESHOLD ASSOCIATED WITH IDPARS( ).
C                       (INPUT)
C               SMULT = THE MULTIPLICATIVE FACTOR WHEN CONTOURING OR
C                       GRIDPRINTING THE DATA.  (INPUT)
C                SADD = THE ADDITIVE FACTOR WHEN CONTOURING OR
C                       GRIDPRINTING THE DATA.  (INPUT)
C              ORIGIN = THE CONTOUR ORIGIN, APPLIES TO THE UNITS IN UNITS.
C                       (INPUT)
C                CINT = THE CONTOUR INTERVAL, APPLIES TO THE UNITS IN
C                       UNITS.  (INPUT)
C               PLAIN = THE PLAIN LANGUAGE DESCRIPTION OF THE PREDICTORS.
C                       (CHARACTER*32)
C               UNITS = THE UNITS OF THE DATA THAT APPLY AFTER MULTIPLYING
C                       BY SMULT AND ADDING SADD.
C                       (CHARACTER*12)  (INPUT)
C               NDATE = CURENT DATE IN FORM YYYYMMDDHH.  (INPUT)
C                 KHR = HOURS TO OFFSET DATE WHEN PRINTING.  THIS WOULD
C                       NORMALLY BE -IDPARS(9).  (INPUT)
C             DATA(J) = ARRAY HOLDING DATA TO CONTOUR (J=1,ND5).  (INPUT)
C                 ND5 = DIMENSION OF DATA( ).  (INPUT)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INPUT)
C                 ND7 = DIMENSION OF IS2( ).  NOT ALL LOCATIONS ARE USED.
C                       (INPUT)
C                IP14 = INDICATES WHETHER (>1) OR NOT (=0) GRIDPOINT FIELDS
C                       WILL BE CONTOURED AND WRITTEN TO UNIT IP14 FOR
C                       VIEWING.  (INPUT)
C               ISTOP = INCREMENTED BY ONE EACH TIME AN ERROR IS ENCOUNTERED.
C                       (INPUT-OUTPUT)
C                 IER = STATUS RETURN.
C                       0 = GOOD RETURN.  (INTERNAL-OUTPUT)
C                 NYR = YEAR, 4 DIGITS.  (INTERNAL)
C                 NMO = MONTH.  (INTERNAL)
C                 NDA = DAY OF MONTH.  (INTERNAL)
C                 NHR = HOUR, 2 DIGITS.  (INTERNAL)
C               TITLE = HOLDS TITLE FOR MAP.  (CHARACTER*74)
C 
C        NONSYSTEM SUBROUINES USED 
C            PRTGR, TIMPR, UPDAT
C
      CHARACTER*12 UNITS
      CHARACTER*12 FMT
      CHARACTER*32 PLAIN
      CHARACTER*74 TITLE/' '/
C
      DIMENSION DATA(ND5)
      DIMENSION IS2(ND7)
      DIMENSION IDPARS(15)
C
      DATA FMT/'(FX.X)'/
C
      IER=0
C
C        COMPUTE DATE TO PRINT AND BREAK INTO COMPONENTS.
C
      CALL UPDAT(NDATE,KHR,LDATE)
      NYR=LDATE/1000000
      NMO=LDATE/10000-NYR*100
      NDA=LDATE/100-NYR*10000-NMO*100
      NHR=MOD(LDATE,100)
C      
C        FORM THE TITLE FOR GRIDPRINTING.  WHEN THE PREDICTOR IS
C        A GRID BINARY, THE UNITS WILL BE BLANK.  USE THE SPACE
C        TO PUT THE THRESHOLD.  FOR VARIABLES INVOLVING SUBROUTINE
C        TIMEP (IDPARS(10).NE.0), THE MAP TITLE WILL NOT FULLY
C        REFLECT THE CORRECT DATE/TIME AND TAU
C
      WRITE(TITLE(1:7),211)IDPARS(12)
 211  FORMAT(I3,'-HR ')
      TITLE(8:39)=PLAIN(1:32)
      TITLE(40:40)=' '
      TITLE(41:52)=UNITS
      IF(TITLE(34:35).NE.'GB')GO TO 212
C
      IF(THRESH-IFIX(THRESH*10000.)/10000..EQ.0.)KD=4
      IF(THRESH-IFIX(THRESH*1000.)/1000..EQ.0.)KD=3
      IF(THRESH-IFIX(THRESH*100.)/100..EQ.0.)KD=2
      IF(THRESH-IFIX(THRESH*10.)/10..EQ.0.)KD=1
      IF(THRESH-IFIX(THRESH).EQ.0.)KD=0
      KR=KD+2
      IF(IFIX(THRESH).GT.9)KR=KD+3      
      IF(IFIX(THRESH).GT.99)KR=KD+4      
      IF(IFIX(THRESH).GT.999)KR=KD+5      
      IF(IFIX(THRESH).GT.9999)KR=KD+6      
      IF(IFIX(THRESH).GT.99999)KR=KD+7
      WRITE(FMT(3:3),'(I1)')KR
      WRITE(FMT(5:5),'(I1)')KD      
      WRITE(TITLE(41:52),FMT)THRESH      
 212  WRITE(TITLE(53:74),213)NYR,NMO,NDA,NHR
 213  FORMAT(' FROM ',I4,1X,I2.2,1X,I2.2,1X,I2.2,'00 ')
C        TAKE THE "T" OUT OF THE ID, BECAUSE GRIDPRINTING IS DONE
C        BEFORE THE TRANSFORM.
      IF(TITLE(33:33).EQ.'T')TITLE(33:33)=' '
C        TAKE THE "B" OUT OF THE ID, BECAUSE A POINT BINARY IS MADE
C        AFTER INTERPOLATION.  HOWEVER, LEAVE IN THE "GB" INDICATING
C        A GRID BINARY.
      IF(TITLE(35:35).EQ.'B'.AND.
     1   TITLE(34:34).NE.'G')TITLE(35:35)=' '
C
      CALL PRTGR(IP14,DATA,IS2(3),IS2(4),CINT,ORIGIN,
     1           SMULT,SADD,0,TITLE,IER)                
      IF(IER.NE.0)ISTOP=ISTOP+1
      RETURN
      END
