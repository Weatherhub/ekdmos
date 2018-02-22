      SUBROUTINE OBSCPPRCP(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                     CCALL,SDATA,ND1,NSTA,
     2                     IPACK,IWORK,FD1,ND2X3,
     3                     LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     4                     IS0,IS1,IS2,IS4,ND7,
     5                     ISTAV,L3264B,IER)
C
C        NOV  2002   SFANOS   TDL   MOS-2000
C        JAN  2005   ANTOLIK  CHANGED ND5 TO ND2X3 IN SUBROUTINE CALL AND 
C                             IN CALL GFETCH. CHANGED DIMENSIONS OF IPACK, 
C                             IWORK, FD1. REMOVED FD2.
C        FEB  2005   RLC      ADDED ND2X3 TO THE INTEGER LIST SINCE WE
C                             USED IMPLICIT NONE.
C
C        PURPOSE 
C           THIS SUBROUTINE WILL DETERMINE IF PRECIPITATION
C           OCCURRED OVER A GIVEN PERIOD. 
C           THIS IS BASED ON THE NCDC COOP DATA.
C
C           THE FOLLOWING IDS IS ACCOMODATED IN THIS SUBROUTINE:
C           703327 - 24-H COOP PRECIP
C           
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE 
C                     (OUTPUT). 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                     (INPUT-OUTPUT). 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                       (INPUT). 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM 
C                       ACCESS (INPUT).
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID(J) (J=1,15)
C                       DEFINED IN THE CALLING PROGRAM (INPUT).
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 
C                            LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET -- PREVIOUS CYCLE.  
C                            ALWAYS + AND COUNTED BACKWARDS IN TIME),
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
C                       G = IDPARS(15), AND THRESH.
C                       JD( ) IS USED TO IDENTIFY THE BASIC MODEL 
C                       FIELDS AS READ FROM THE ARCHIVE (INPUT).
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED
C                       (INPUT).
C            SDATA(K) = DATA TO RETURN (K=1,NSTA) (OUTPUT) 
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  (INPUT).
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT WITH
C                       (INPUT).
C            IPACK(J) = WORK ARRAY (J=1,ND5) (INTERNAL).  
C            IWORK(J) = WORK ARRAY (J=1,ND5) (INTERNAL).  
C              FD1(J) = WORK ARRAYS (J=1,ND5). FD1 CONTAINS THE 24-H
C                       OBSERVED PRECIP. (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ),IWORK( ) AND FD1( ),
C                       (INPUT).
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
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE 
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
C                       THAT HAVE BEEN USED IN THIS RUN (INPUT).  
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA 
C                       IDENTIFIED IN LSTORE( , ) (J=1,ND10). WHEN
C                       CORE( ) IS FULL, DATA ARE STORED ON DISK
C                       (OUTPUT).
C                ND10 = DIMENSION OF CORE( ), (INPUT).
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE (INPUT).  
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING  COUNT FROM THE BEGINNING OF 
C                       THE PROGRAM.  THIS COUNT IS MAINTAINED IN 
C                       CASE THE USER NEEDS IT (DIAGNOSTICS, ETC.). 
C                       (OUTPUT).  
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3)
C                       (INTERNAL).
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+)  
C                       (INTERNAL).
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12)
C                       (INTERNAL).
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4)
C                       (INTERNAL).
C                 ND7 = DIMENSION OF IS0, IS1, IS2, AND IS4. NOT ALL
C                       LOCATIONS ARE USED (INPUT).
C               ISTAV = 1 WHEN THE DATA RETURNED ARE STATION DATA
C                       0 THE DATA RETURNED ARE GRID DATA OR
C                       DATA ARE NOT AVAILABLE FOR RETURN. 
C                       (OUTPUT).
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64) (INPUT).
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       103 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED
C                             IN THIS SUBROUTINE.
C                        52 = NWORDS DOES NOT EQUAL NSTA.
C                       SEE GFETCH FOR OTHER VALUES WHEN IER.NE.0 AND
C                       DATA ARE RETURNED AS MISSING (INTERNAL-OUTPUT)
C
C      ADDITIONAL VARIABLES 
C                  HR = VARIABLE USED TO CHECK IF CORRECT HOUR IS
C                       BEING USED. (INTERNAL)
C               ISTRT = NUMBER OF HOURS TO BEGIN LOOKBACK. (INTERNAL)
C                IFIN = NUMBER OF HOURS TO FINISH LOOKBACK. (INTERNAL)
C         ITABLE(I,J) = 1ST VALUE IS THE ENDING PERIOD OF 24-H PRECIP
C                       2ND VALUE IS THE NUMBER TO SUBTRACT FOR THE
C                       ENDING PERIOD (I.E. IF 18 IS IST VALUE AND
C                       1 IS THE 2ND VALUE, 17 IS THE HOUR TO END WITH)
C                       3RD VALUE IS THE NUMBER TO SUBTRACT FOR THE STARTING
C                       PERIOD
C                   I = VARIABLE COUNTER. (INTERNAL)                   
C                   J = VARIABLE COUNTER. (INTERNAL)
C                  JJ = VARIABLE COUNTER. (INTERNAL)
C                   K = VARIABLE COUNTER. (INTERNAL)
C                   L = VARIABLE COUNTER. (INTERNAL)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD1( ) (INTERNAL).
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       ZERO FROM CALLING GFETCH WHEN THERE IS NO
C                       SECONDARY MISSING VALUE (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       ZERO FROM CALLING GFETCH WHEN THERE IS NO 
C                       SECONDARY MISSING VALUE. (INTERNAL)
C               NPACK = 2 FOR TDL GRIB PACKED DATA; 1 FOR NOT PACKED.
C                       THIS IS RETURNED FROM CALLING GFETCH (INTERNAL)
C               NSLAB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE FIRST FIELD.  THIS IS THE
C                       VALUE OF NSLAB RETURNED.  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED (OUTPUT).  
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT 
C                       THE RECORD HAS BEEN FETCHED. THIS IS STORED 
C                       IN LSTORE(9, ) (INTERNAL).
C              NWORDS = NUMBER OF WORDS RETURNED IN FD1( ).
C                       (INTERNAL).
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH
C
      IMPLICIT NONE 
      CHARACTER*8 CCALL(ND1,6)
C
      INTEGER HR,I,IER,IFIN,ISTAV,ISTRT,J,JJ,
     +        KFILDO,KFIL10,L,NDATE,ND1,NSTA,
     +        L3264B,ND2X3,ND9,LITEMS,ND10,NBLOCK,NFETCH,
     +        NSLAB,NTIMES,NWORDS,ND7,NPACK,MISSP,MISSS,
     +        NDATE1,NDIM
      INTEGER IPACK(ND2X3),IWORK(ND2X3)
      INTEGER IDPARS(15),LDPARS(15)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER LD(4),JD(4)
      INTEGER ITABLE(3,2)
C
      REAL SDATA(ND1),NDATA(ND1)
      REAL FD1(ND2X3)
      REAL CORE(ND10)
C
      DATA NDIM/2/ 
      DATA ITABLE/18,1,7,
     1            06,3,9/
C
C          INITIALIZE THE NECESSARY VARIABLES, INCLUDING SDATA
C
      IER=0
      ISTAV=1
      DO I=1,ND1
         SDATA(I)=9999.
         FD1(I)=9999.
      ENDDO
C
C        SET HOUR FROM CONTROL FILE (USED FOR CHECK)
C
      HR=MOD(NDATE,100)
C
C        IF TEST TO GET RIGHT START AND ENDING TIMES. 
C        IF FOUND, SKIP NEXT ERROR CHECK AND CONTINUE.  IF NOT
C        FOUND, ENTER ERROR CHECK AND LEAVE.
C
      DO L=1,NDIM
        IF(ITABLE(1,L).EQ.HR)THEN
          ISTRT = ITABLE(2,L)
          IFIN  = ITABLE(3,L)
          GOTO 1010
        END IF
      END DO
C
C        CHECK TO MAKE SURE HOUR ENTERED IN CONTROL
C        FILE IS CORRECT WITH ITABLE
C
      WRITE(KFILDO,101)(ITABLE(1,JJ),JJ=1,NDIM),HR
 101  FORMAT(/,' PROJECTION IN ITABLE IS ',I2,
     +             ' AND IDPARS(12) IS ',I2,
     +             ' CHECK CONTROL FILE. ')    
      IER=101
      GOTO 900
C
C        CHECK IF THE VARIABLE ID REQUESTED IN THE CALL IS 
C        PRECIP OVER 24-H PERIOD
C
 1010 IF((IDPARS(1).NE.703).OR.(IDPARS(2).NE.327))THEN
         IER=103
         WRITE(KFILDO,102)(JD(J),J=1,4),IER
 102     FORMAT(/,' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     +          'CO-OP PRECIP PREDICTOR ',I9.9,2I10.9,
     +          I4.3,' NOT ACCOMMODATED IN SUBROUTINE ',
     +          'POP.  IER=',I3)
         GOTO 900
      ENDIF
C
      DO I=IFIN,ISTRT,-1
C
        CALL UPDAT(NDATE,-I,NDATE1)
C
C        SET UP THE LD ARRAY TO HOLD THE ID FOR THE  
C        24-H OBSERVED PRECIP TYPE
C
        LD(1)=703317000 + IDPARS(4)
        LD(2)=IDPARS(7)
        LD(3)=IDPARS(9) * 1000000 + IDPARS(12)
        LD(4)=0
C
C        CALL GFETCH TO GET THE PRECIP OVER 24 H 
C        AND STORE IT IN FD1.
C
        CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     +              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     +              NWORDS,NPACK,NDATE1,NTIMES,CORE,ND10,
     +              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     +              IER) 
C
C        CHECK TO SEE IF IER NE 0
C        PRINT ERROR MESSAGE IF NEEDED.
C
        IF(IER.NE.0) THEN
          WRITE(KFILDO,140)NDATE1,IER
 140      FORMAT(/,'****ERROR FROM GFETCH OCCURRED ON',
     +             ' PROCESS DATE - ',I12,
     +             ' POP CANNOT RUN, PLEASE READ WRITE UP.',
     +             ' IER=',I3)
          GO TO 900
        ENDIF
C
C        IF NWORDS NE NSTA, SET ALL VALUES TO MISSING AND
C        WRITE ERROR MESSAGE
C
        IF(NWORDS.NE.NSTA)THEN
          IER=52
          WRITE(KFILDO,150)NWORDS,NSTA,IER
 150      FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     +             ' NOT EQUAL TO NSTA =',I6,
     +             ' IN POP.  DATA SET TO MISSING.',
     +             ' IER=',I3)
          GOTO 900
        END IF
C
C        CHECK 24-H PRCP AMOUNT.  AFTER CALLING GFECTH, CHECK AND
C        SEE IF THERE WAS A REPORT OF PRECIP.  IF THERE WAS, KEEP
C        AMOUNT; IF 0, SET TO 0; IF MISSING, SET TO 9999
C
        DO J=1,NSTA
           IF(NINT(FD1(J)).EQ.9999.)THEN
             IF(SDATA(J).NE.9999.)THEN
C
C         DATA THERE SO GO TO NEXT ONE
C
               GOTO 710
             ELSE
               SDATA(J)=9999.
             END IF
           ELSE
             IF(FD1(J).LT..0035)THEN
               SDATA(J)=0.
             ELSE
               SDATA(J)=FD1(J)
C               SDATA(J)=1.
             END IF
           END IF
 710    ENDDO
      ENDDO
      GO TO 910
C
C        IF THERE WAS AN ERROR IN POP, THE CODE WILL JUMP TO
C        HERE.  SET SDATA TO MISSING BEFORE RETURNING. 
C
 900  DO J=1,ND1
         SDATA(J)=9999.
      END DO
C
 910  RETURN
      END     
