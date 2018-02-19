      SUBROUTINE OBSPTYPE(KFILDO,KFIL10,IDPARS,JD,NDATE,
     +                SDATA,ND1,NSTA,IPACK,IWORK,FD1,ND2X3,
     +                LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     +                IS0,IS1,IS2,IS4,ND7,
     +                ISTAV,L3264B,IER)
C
C        MARCH     1999   ALLEN   TDL   MOS-2000
C        DECEMBER  2002   WEISS   CHANGED ND5 TO ND2X3
C        MAY       2003   GLAHN   CHANGED .AND. TEST TO .OR ON 
C                                 IDPARS( ) ABOVE 100; CHANGED FORMAT
C                                 100; INDENTION; CHANGED IER FROM 47
C                                 TO IER=57 FOR DEC. 1, 1996 DATA
C                                 PROBLEM
C        NOVEMBER  2003   SMB     ONE OF THE CASE STATMENTS HAD BEEN 
C                                 COMMENTED OUT, SO THAT WAS FIXED.
C                                 ALSO ADDED BACK A LINE IN PURPOSE.
C
C        PURPOSE 
C            THIS SUBROUTINE WILL CONVERT THE OBSERVATIONAL PRESENT
C            WEATHER REPORTS INTO THEIR RESPECTIVE PRECIPITATION TYPE
C            CATEGORIES OF FREEZING, FROZEN, OR LIQUID.  THE HOURLY
C            DATA ARCHIVE CHANGED FROM SAO TO METAR ON DECEMBER 1, 1996,
C            SO THERE IS ONE SET OF PRESENT WEATHER NUMBERS FOR THE SAO
C            DATA AND ONE SET OF NUMBERS FOR THE METAR DATA.  THIS 
C            ROUTINE CONTAINS TWO SEPARATE ALGORITHMS TO PROCESS THE
C            DATA DEPENDING ON NDATE.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               708 501 - PRECIPITATION TYPE                   
C 
C            ****THE USER CAN NOT PROCESS
C            DATES THAT CROSS THIS DECEMBER 1, 1996 BARRIER BECAUSE
C            THE MASS STORE WILL NOT BE INITIALIZED PROPERLY.  YOU MUST
C            PROCESS ONE RUN THAT GOES THROUGH NOV. 30, 1996 AND THEN 
C            START A NEW RUN FOR ANY DATA DEC. 1, 1996 AND ON. 
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
C            IPACK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).  
C            IWORK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).  
C              FD1(J) = WORK ARRAYS (J=1,ND2X3). FD1 CONTAINS THE PRESENT
C                       WEATHER. (INTERNAL)
C               ND2X3 = DIMENSION OF IPACK( ),IWORK( ), FD1( ),
C                       AND FD2( ) (INPUT).
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
C                        52 = NWORDS DOES NOT EQUAL NSTA.
C                        57 = SUBROUTINE DOES NOT PROCESS DATA
C                             BECAUSE OF DEC 1996 CUTOFF.
C                       103 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED
C                             IN THIS SUBROUTINE.
C                       SEE GFETCH FOR OTHER VALUES WHEN IER.NE.0 AND
C                       DATA ARE RETURNED AS MISSING (INTERNAL-OUTPUT)
C
C      ADDITIONAL VARIABLES 
C              IENTER = INITIALLY SET TO ZERO IN DATA STATEMENT.
C                       INCREMENTS ONE OR MORE TIMES WHEN THIS
C                       SUBROUTINE IS ENTERED.  (INTERNAL).
C            IWORD(3) = HOLDING VARIABLE FOR THE FFF PORTION OF
C                       THE VARIABLE ID (INTERNAL).
C                IPWX = HOLDING VARIABLE FOR THE PRESENT WEATHER CODE. (INTERNAL)
C              ISDATA = HOLDING VARIABLE FOR THE VALUE OF SDATA. (INTERNAL)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD1( ) "PRESENT WEATHER GROUPS" (J=1,4) (INTERNAL).
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
C              NWORDS = NUMBER OF WORDS RETURNED IN FD1( ), AND FD2( ).
C                       (INTERNAL).
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH
C
      IMPLICIT NONE
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IPACK(ND2X3),IWORK(ND2X3)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER LD(4),IWORD(3)
      INTEGER KFILDO,KFIL10,NDATE,ND1,NSTA,ISTAV,IENTER,L3264B,
     +        ND2X3,IER,ND9,LITEMS,ND10,NBLOCK,NFETCH,NSLAB,
     +        IPWX,ISDATA,NTIMES,NWORDS,ND7,MISSP,MISSS,NPACK,J,I 
C
C
      REAL SDATA(ND1)
      REAL FD1(ND2X3)
      REAL CORE(ND10)
C
      DATA IENTER/0/,IWORD/500,510,520/
      SAVE IENTER

      IER=0
      ISTAV=1
C
C          INITIALIZE INCLUDING SDATA
C
      DO I=1,NSTA
       SDATA(I)=9999.
      ENDDO
C
C        CHECK IF THE VARIABLE ID REQUESTED IN THE CALL IS 
C        PRECIPITATION TYPE 
C
      IF((IDPARS(1).NE.708).OR.(IDPARS(2).NE.501))THEN
	 IER=103
	 WRITE(KFILDO,100)(JD(J),J=1,4),IER
 100     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1           'PRECIPITATION TYPE.',
     2          /'     PREDICTOR ',I9.9,2I10.9,I4.3,
     3           ' NOT ACCOMMODATED IN SUBROUTINE ',
     4           'OBSPTYPE.  IER =',I3)
         GO TO 900
      ENDIF
C
C        FIRST CHECK THAT THE USER IS NOT TRYING TO SPAN THE DECEMBER
C        1, 1996 CUTOFF BETWEEN THE OLD PRESENT WEATHER DATA AND THE
C        NEW PRESENT WEATHER DATA.  IF THEY ARE, WRITE A WARNING TO
C        THE OUTPUT FILE SAYING THAT ALL THE DATA WILL BE MISSING.
C
      IF((NDATE.EQ.1996120100).AND.(IENTER.NE.0))THEN
         IER=57
         WRITE(KFILDO,120)IER
 120     FORMAT(/' ****OBSPTYPE CAN NOT PROCESS DATA BECAUSE OF THE',
     &           ' DECEMBER 1,1996 CUTOFF.  SEE WRITEUP.  IER = ',I4)
         GO TO 900
      ENDIF
C
C        NOW CHECK THE DATE TO DECIDE WHETHER YOU USE ALGORITHM 2 FOR 
C        THE DATES MAY 16,1979 THROUGH NOVEMBER 30,1996, OR ALGORITHM 1
C        FOR DECEMBER 1, 1996 AND ON.
C
      IF(NDATE.GE.1996120100)THEN
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C        THIS IS ALGORITHM 1 FOR THE DATA FROM DECEMBER 1, 1996
C        AND ON
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        THIS LOOP WILL GO THROUGH THE ALGORITHM ONCE 
C        FOR EACH OF THE 3 PRESENT WEATHER GROUPS, THUS CREATING
C        A 'CUMULATIVE' PRECIP TYPE
C
         DO 200  I=1,3
C
C            SET UP THE LD ARRAY TO HOLD THE ID FOR THE PRESENT 
C            WEATHER GROUP THAT WILL BE FETCHED AND PROCESSED
C
            LD(1)=708000000 + IWORD(I) * 1000
            LD(2)=IDPARS(7)
            LD(3)=IDPARS(9) * 1000000 +IDPARS(12)
            LD(4)=0
C
C              CALL GFETCH TO GET THE PRESENT WEATHER GROUP AND STORE IT
C              IN FD1.
C
            CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     +                  IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     +                  NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     +                  NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     +                  IER)
C
C            INCREMENT IENTER VARIABLE BY 1.
C
            IENTER=IENTER+1
C
C              CHECK TO SEE IF IER NE 0, AND IF THIS IS THE FIRST
C              PROCESS DATE.  PRINT ERROR MESSAGE IF NEEDED.
C
            IF((IER.NE.0).AND.(IENTER.EQ.1)) THEN
               WRITE(KFILDO,140)NDATE
 140          FORMAT(/' ****ERROR FROM GFETCH OCCURRED ON',
     +                ' 1ST PROCESS DATE ',I12,
     +                '.  OBSPTYPE CANNOT RUN, PLEASE READ WRITE UP.')
	      GO TO 900
            ENDIF
C
         IF(IER.NE.0) GO TO 900
C
C            IF NWORDS DOES NOT EQUAL NSTA, SET ALL VALUES TO MISSING.
C
         IF(NWORDS.NE.NSTA)THEN
            IER=52
	    WRITE(KFILDO,160)NWORDS,NSTA,IER
 160        FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     +              ' NOT EQUAL TO NSTA =',I6,
     +              ' IN OBSTYPE.. DATA SET TO MISSING.',
     +              '  IER =',I3)
            GO TO 900
         ENDIF
C
C            SET THE VALUE OF SDATA DEPENDING ON THE COMBINATIONS OF
C            THE PRESENT WEATHER REPORTS
C
         DO 170 J=1,NSTA
C
C              FIRST YOU MUST CHECK WHAT CATEGORY THE PRESENT WEATHER
C              GROUP FALLS IN.  THEN YOU MUST CHECK TO SEE IF A
C              PREVIOUS PRESENT WEATHER GROUP HAD CAUSED SDATA TO
C              ALREADY BE SET TO A CATEGORY
C
            IPWX = NINT(FD1(J))
            ISDATA = NINT(SDATA(J))
C
            SELECT CASE(IPWX)
C
C                 FREEZING PRECIPITATION
C
               CASE(56,57,66,67,156,166)
C
                  SELECT CASE(ISDATA)
                     CASE(1:7,9999)
                        SDATA(J)=1.
                  END SELECT
C
               CASE(79,174,176)
C
                  SELECT CASE(ISDATA)
                     CASE(1)
                        SDATA(J)=1.
                     CASE(2,4:7)
                        SDATA(J)=2.
                     CASE(3,9999)
                        SDATA(J)=3.
                  END SELECT
C
C                 FROZEN PRECIPITATION
C
               CASE(71:75,77,85,86,187)
C
                  SELECT CASE(ISDATA)
                     CASE(1)
                        SDATA(J)=1.
                     CASE(2,3)
                        SDATA(J)=2.
                     CASE(4,7,9999)
                        SDATA(J)=4.
                     CASE(5,6)
                        SDATA(J)=5.
                  END SELECT
C
C                 LIQUID PRECIPITATION
C
               CASE(68,69,83,84)
C
                  SELECT CASE(ISDATA)
                     CASE(1)
                        SDATA(J)=1.
                     CASE(2,3)
                        SDATA(J)=2.
                     CASE(4:7,9999)
                        SDATA(J)=5.
                  END SELECT
C
               CASE(51:55,58:65,80,81,183)
C
                  SELECT CASE(ISDATA)
                     CASE(1)
                        SDATA(J)=1.
                     CASE(2,3)
                        SDATA(J)=2.
                     CASE(4,5)
                        SDATA(J)=5.
                     CASE(6,7,9999)
                        SDATA(J)=6.
                  END SELECT
C
               CASE(95,97)
C
                  SELECT CASE(ISDATA)
                     CASE(1)
                        SDATA(J)=1.
                     CASE(2,3)
                        SDATA(J)=2.
                     CASE(4)
                        SDATA(J)=4.
                     CASE(5)
                        SDATA(J)=5.
                     CASE(6)
                        SDATA(J)=6.
                     CASE(7,9999)
                        SDATA(J)=7.
                  END SELECT
C 
               CASE DEFAULT
                  SDATA(J)=SDATA(J)
C
            END SELECT
C
 170      CONTINUE
C
 200     CONTINUE
         GO TO 910
   
      ELSE IF((NDATE.GE.1979051600).AND.(NDATE.LE.1996113023))THEN
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C        THIS IS ALGORITHM 2 FOR THE DATA FROM MAY 16,1979 TO NOVEMBER
C        30, 1996.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        SET UP THE LD ARRAY TO HOLD THE ID FOR THE PRESENT 
C        WEATHER GROUP THAT WILL BE FETCHED AND PROCESSED
C
         LD(1)=708000000 + 500000 +IDPARS(4) 
         LD(2)=IDPARS(7)
         LD(3)=IDPARS(9) * 1000000 +IDPARS(12)
         LD(4)=0
C
C           CALL GFETCH TO GET THE PRESENT WEATHER GROUP AND STORE IT
C           IN FD1.
C
         CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     +               IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     +               NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     +               NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     +               IER)
C
C           INCREMENT IENTER VARIABLE BY 1.
C
         IENTER=IENTER+1
C
C           CHECK TO SEE IF IER NE 0, AND IF THIS IS THE FIRST
C           PROCESS DATE.  PRINT ERROR MESSAGE IF NEEDED.
C
         IF((IER.NE.0).AND.(IENTER.EQ.1)) THEN
            WRITE(KFILDO,241)NDATE
 241        FORMAT(' ****ERROR FROM GFETCH OCCURRED ON',
     +             ' 1ST PROCESS DATE ',I12,
     +             '.  OBSPTYPE CANNOT RUN, PLEASE READ WRITE UP.')
	    GO TO 900
         ENDIF
C
         IF(IER.NE.0) GO TO 900
C
C           IF NWORDS DOES NOT EQUAL NSTA, SET ALL VALUES TO MISSING.
C
         IF(NWORDS.NE.NSTA)THEN
            IER=52
	    WRITE(KFILDO,261)NWORDS,NSTA,IER
 261        FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     +              ' NOT EQUAL TO NSTA =',I6,
     +              ' IN OBSTYPE.  DATA SET TO MISSING.',
     +              '  IER =',I3)
         GO TO 900
         ENDIF
C
C        SET THE VALUE OF SDATA DEPENDING ON THE PRESENT WEATHER REPORT
C        
         DO 250 J=1,NSTA
            IPWX = NINT(FD1(J))
C
            SELECT CASE(IPWX)
C
C                 FREEZING PRECIPITATION
C
               CASE(10:16)
                  SDATA(J)=1.
               CASE(26)
                  SDATA(J)=2.
               CASE(17:19)
                  SDATA(J)=3.
C
C                 FROZEN PRECIPITATION
C
               CASE(20:25)
                  SDATA(J)=4.
C
C                 LIQUID PRECIPITATION
C
               CASE(27)
                  SDATA(J)=5.
               CASE(1:9)
                  SDATA(J)=6.
C
               CASE DEFAULT
                  SDATA(J)=9999.
            END SELECT
C
 250     CONTINUE
C
         GO TO 910 
C
      ENDIF   
C
C        NOTE THAT IT WILL DORP THROUGH TO HERE IF THE DATE IS
C        BEFORE 1979051600.
C
C        IF THERE WAS AN ERROR IN GFETCH OR FOR SOME OTHER REASON,
C        THE CODE WILL JUMP TO HERE.  SET SDATA TO MISSING BEFORE
C        RETURNING. 
C
 900  DO J=1,NSTA
	 SDATA(J)=9999.
      END DO
C
 910  RETURN
      END     
