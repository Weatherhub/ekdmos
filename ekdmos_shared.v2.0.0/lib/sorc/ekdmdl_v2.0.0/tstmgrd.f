      SUBROUTINE TSTMGRD(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                   IPACK,IWORK,DATA,ND5,
     2                   LSTORE,ND9,LITEMS,CORE,ND10,
     3                   FD1,ND2X3,
     4                   NSLAB,NBLOCK,NFETCH,
     5                   IS0,IS1,IS2,IS4,ND7,
     6                   ISTAV,L3264B,IER)
C
C        JUNE     2006   CHARBA       TDL   MOS-2000 
C        JUNE     2006   CHARBA       TSTMGRD, WHICH IS AN ALTERNATIVE TO
C                                     SVRVEC, USES A GRID FILE RATHER 
C                                     THAN A VECTOR FILE AS INPUT.
C
C        PURPOSE 
C           SUBROUTINE TSTMGRD RETRIEVES AND UNPACKS OBSERVED, HOURLY 
C           GRIDDED DATA IN TDLPACK GRID FORMAT.  EXAMPLES ARE LIGHTNING
C           FLASHES, REPORTS OF TORNADOES, LARGE HAIL, AND DAMAGING
C           WINDS, AND PILOT REPORTS OF CLEAR-AIR-TURBULENCE AND 
C           AIRCRAFT ICING.  TSTMGRD SUMS HOURLY THE HOURLY INPUT GRIDS
C           FOR A TIME INTERVAL (HOURS) DESIGNATED BY THE PREDICTAND ID.
C           TIME INTERVALS NORMALLY USED ARE 2, 3, 6, 12, AND 24 HOURS.i
C           THE SUMMED GRIDS ARE RETURNED TO THE CALLING PROGRAM 
C           (OPTION).
C
C           THE HOUR IN THE DATE-TIME FOR THE INPUT GRIDS DENOTES THE 
C           BEGINNING OF THE 60-MINUTE PERIOD.  NOTE THAT THE HOUR 
C           IN THE DATE-TIME FOR THE OUTPUT PREDICTAND DATA DENOTES THE
C           END OF THE LAST SUMMED 60-MINUTE PERIOD.
C
C           THE FOLLOWING INPUT IDPARS(1) AND IDPARS(2) ARE 
C           ACCOMMODATED:  
C
C           707 - 380 (ALL 1-H C-G LIGHTNING FLASHES IN GRIDBOX)	
C
C           THE CORRESPONDING OUTPUT IDPARS(1) IS IDENTICAL, WHEREAS 
C           THE OUTPUT IDPARS(2) (FFF) IS DESIGNATED BELOW:
C
C                 FFF = XX0 FOR 1-HR INTERVAL
C                  "  = XX1  "  3-HR     " 
C                  "  = XX2  "  6-HR     "
C                  "  = XX3  " 12-HR     "
C                  "  = XX4  " 24-HR     " 
C                  "  = XX5  "  2-HR     " 
C
C           NOTE: WHEN FFF = XX0, THE ORIGINAL ARCHIVED 1-H VALUES
C           ARE RETURNED UNCHANGED TO THE CALLING PROGRAM.  NEVERTHELESS
C           THE HOUR IN THE OUTPUT DATE-TIME SWITCHES FROM THE 
C           BEGINNING OF THE 60-MINUTE PERIOD TO THE END OF THE 
C           60-MINUTE PERIOD.
C
C           EXAMPLE OF THE PREDICTAND ID (CCCFFF): 
C
C             LIGHTNING REPORTS SUMMED OVER 2-HR PERIOD = 707385      
C
C        DATA SET USE 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE 
C                       (OUTPUT). 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C
C                       (INPUT-OUTPUT). 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                       (INPUT). 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM 
C                       ACCESS (INPUT-OUTPUT).
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTAND ID CORRESPONDING TO ID( ) (J=1,15)
C                       (INPUT).
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 
C                            LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK 
C                            IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               JD(J) = THE BASIC INTEGER PREDICTAND ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE 
C                       PORTIONS PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND THRESH.
C                       JD( ) IS USED TO IDENTIFY THE BASIC MODEL 
C                       FIELDS AS READ FROM THE ARCHIVE (INPUT).
C               NDATE = THE DATE/TIME FOR WHICH PREDICTAND IS NEEDED.
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)  
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)  
C             DATA(J) = ARRAY TO HOLD RETURNED DATA (J=1,ND5).  (OUTPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND DATA( ).
C                       (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS), (INPUT-OUTPUT).
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK, 
C                              THIS IS MINUS THE RECORD NUMBER WHERE 
C                              THE DATA START.
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDL GRIB, 1 FOR 
C                              NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN 
C                              RETRIEVED.
C                       L=10 --NUMBER OF THE SLAB IN DIR( , ,L) AND
C                              IN NGRIDC( ,L) DEFINING THE 
C                              CHARACTERISTICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTAND IN THE 
C                              SORTED LIST IN ID( ,N) (N=1,NPRED)
C                              FOR WHICH THIS VARIABLE IS NEEDED, WHEN 
C                              IT IS NEEDED ONLY ONCE FROM 
C                              LSTORE( , ).  WHEN IT IS NEEDED MORE
C                              THAN ONCE, THE VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING 
C                              MOSTORE( , ). LATER USED AS A WAY OF 
C                              DETERMINING WHETHER TO KEEP THIS 
C                              VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ), (INPUT). 
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN.  (INPUT)  
C             CORE(J) = THE ARRAY TO STORE OR RETIREVE THE DATA 
C                       IDENTIFIED IN LSTORE( , ) (J=1,ND10). WHEN
C                       CORE( ) IS FULL DATA ARE STORED ON DISK.
C                       (OUTPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              FD1(J) = WORK ARRAY (J=1,ND2X3).  (INTERNAL)
C               ND2X3 = DIMENSION OF FD1( ).  (INPUT)  
C               NSLAB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ).  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED.  (OUTPUT)  
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)  
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING COUNT FROM THE BEGINNING OF 
C                       THE PROGRAM.  THIS COUNT IS MAINTAINED IN 
C                       CASE THE USER NEEDS IT (DIAGNOSTICS, ETC.). 
C                       (INTERNAL)  
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).
C                       (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  
C                       (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).
C                       (INTERNAL)
C                 ND7 = DIMENSION OF IS0, IS1, IS2, AND IS4. NOT ALL
C                       LOCATIONS ARE USED.  (INPUT)
C               ISTAV = 0 SINCE THE DATA RETURNED ARE GRID DATA.
C                       (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64).  (INPUT)
C                 IER = STATUS RETURN.
C                       0 = GOOD RETURN.
C                       SEE GFETCH FOR OTHER VALUES.  (INTERNAL-OUTPUT)
C
C         ADDITIONAL VARIABLES
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT 
C                       THE RECORD HAS BEEN FETCHED. THIS IS STORED 
C                       IN LSTORE(9, ) (INTERNAL).
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD1( ) (J=1,4).  (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR. RETURNED AS 
C		        ZERO WHEN DATA ARE NOT PACKED.  (INTERNAL) 
C               MISSS = SECONDARY MISSING VALUE INDICATOR. RETURNED AS
C		        ZERO WHEN DATA ARE NOT PACKED.  (INTERNAL) 
C              NWORDS = NUMBER OF WORDS RETURNED IN DATA( ).  (INTERNAL)
C
C        NON-SYSTEM SUBROUTINES CALLED 
C           UPDAT, GFETCH
C
      PARAMETER (NUMVAR=1)
C
      DIMENSION IDPARS(15),JD(4),LD(4),ITABLE(3,NUMVAR)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
C
      DIMENSION FD1(ND2X3)
      DIMENSION CORE(ND10),LSTORE(12,ND9)
C
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
C
      DATA ((ITABLE(I,J),I=1,3),J=1,NUMVAR)
     1     /707385,707380,2/ 
C          NUMVAR      = NUMBER OF PREDICTORS ACCOMMODATED
C          ITABLE(1,1) = OUTPUT ID(1)
C          ITABLE(2,1) = INPUT ID(1)
C          ITABLE(3,1) = NUMBER OF HOURS TO SUM
C
C        INITIALIZATION
C
      IER=0
      ISTAV=0
C
      DO 100 J=1,ND5
         DATA(J)=0.0 
 100  CONTINUE
C
C        MAKE SURE THIS SUBROUTINE DEALS ACCOMMODATES THE PREDICTOR.
C
      DO 110 N=1,NUMVAR
        IF(IDPARS(1).EQ.(ITABLE(1,N)/1000).AND.
     1     IDPARS(2).EQ.(MOD(ITABLE(1,N),1000))) GO TO 130
 110  CONTINUE
        WRITE(KFILDO,120) IDPARS(1),IDPARS(2),(JD(J),J=1,4)
 120    FORMAT(/,' ****IN TSTMGRD IDPARS(1) AND IDPARS(2) = ',2I4,
     1         ' NOT ACCOMMODATED ...SET IER=103 AND SUPPLY',
     2         ' MISSING VALUES FOR ',4I10.9)
      IER=103
      GO TO 600
C
C        LOOP OVER NUMBER OF HOURS TO SUM INPUT GRIDS.        
C         
 130  NHR=ITABLE(3,N)
C
      LD(1)=ITABLE(2,N)*1000+IDPARS(3)*100+IDPARS(4)
      LD(2)=0
      LD(3)=0
      LD(4)=0
C
      DO 300 N=1,NHR
C
C          FETCH THE PREVIOUS HOUR OF GRID INPUT DATA INTO FD1( ).
C
        KHR=-N
        CALL UPDAT(NDATE,KHR,JDATE)
C
        CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2              NWORDS,NPACK,JDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4              IER)
C
        IF(IER.NE.0) THEN
           WRITE(KFILDO,210) LD,JD
 210       FORMAT(' ****IN TSTMGRD, COULD NOT FETCH ',4I10.9,
     1            ' SUPPLY MISSING VALUES FOR ',4I10.9,' AND RETURN')
           GO TO 600
        END IF
C
C          SUM HOURLY VALUES IN DATA( ).
C          NWORDS IS NUMBER OF POINTS IN INPUT GRID.
C
        DO 240 J=1,NWORDS
           IF(NINT(FD1(J)).EQ.9999) GO TO 240
           DATA(J)=DATA(J)+FD1(J)  
 240    CONTINUE
C
 300  CONTINUE
C
C        PROCESSING COMPLETED
C
      GO TO 700
C
C        SET OUTPUT GRID TO MISSING VALUES WHEN AN ERROR HAS OCCURRED.
C
 600  DO 650 K=1,ND5
         DATA(K)=9999.
 650  CONTINUE
C
 700  RETURN
      END     
