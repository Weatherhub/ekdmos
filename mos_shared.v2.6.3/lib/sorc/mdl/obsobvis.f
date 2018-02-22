      SUBROUTINE OBSOBVIS(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                    SDATA,ND1,NSTA,IPACK,IWORK,FD1,FD2,ND2X3,
     2                    LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                    IS0,IS1,IS2,IS4,ND7,
     4                    ISTAV,L3264B,IER)
C
C        DECEMBER  1999   SFANOS   TDL   MOS-2000
C                                  MODIFIED FROM ALLEN'S
C                                  SUBROUTINE OBSPTYPE
C        JUNE      2000   SFANOS   ADDED IF STATEMENTS; ERROR
C                                  CHECKS ADDED
C        NOVEMBER  2000   RUDACK   MODIFIED CODE TO CONFORM 
C                                  WITH MDL FORMAT SPECIFICATIONS
C        DECEMBER  2002   WEISS    CHANGED ND5 TO ND2X3
C        MAY       2003   GLAHN    PULLED GFETCH OUT OF NSTA LOOP;
C                                  AUGMENTED DIAGNOSTICS; COMMENTS
C
C        PURPOSE 
C            THIS SUBROUTINE WILL CONVERT THE OBSERVATIONAL PRESENT
C            WEATHER REPORTS INTO THEIR RESPECTIVE VISIBILITY TYPE
C            CATEGORIES.  THE HOURLY DATA ARCHIVE CHANGED FROM SAO
C            TO METAR ON DECEMBER 1, 1996, SO THERE IS
C            ONE SET OF PRESENT WEATHER NUMBERS FOR THE SAO
C            DATA AND ONE SET OF NUMBERS FOR THE METAR DATA.  THIS 
C            ROUTINE CONTAINS TWO SEPARATE ALGORITHMS TO PROCESS THE
C            DATA DEPENDING ON NDATE.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
c
C               708 251 - CATEGORY OF VISIBILITY      
C 
C            THE CATEGORIES FOR THE VISIBILITY TYPE ARE:
C               1 = NONE OF THE FOLLOWING
C               2 = HAZE,SMOKE,DUST
C               3 = FOG
C               4 = DENSE FOG
C               5 = BLOWING PHENOMENA (SNOW,SAND,DUST)
C
C            FOR DATA PRIOR TO DEC. 1, 1996, THESE VALUES ARE OBTAINED
C            FROM OBSTRUCTIONS TO VISION AND VISIBILITY; AFTER THAT
C            DATE, THE 3 WEATHER TYPES ARE PROCESSED.
C
C            THE USER CANNOT PROCESS DATES THAT CROSS THE
C            DECEMBER 1, 1996, BARRIER BECAUSE THE MASS STORAGE WILL 
C            NOT BE INITIALIZED PROPERLY.  PROCESS ONE RUN THAT GOES
C            THROUGH NOV. 30, 1996 AND THEN START A NEW RUN FOR ANY
C            DATA DEC. 1, 1996 AND ON. 
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
C              FD1(J) = WORK ARRAY (J=1,ND2X3). FD1 CONTAINS THE PRESENT
C                       WEATHER. (INTERNAL)
C              FD2(J) = WORK ARRAY (J=1,ND2X3). FD2 CONTAINS THE 
C                       PRESENT VISIBILITY. (INTERNAL)
C                 ND5 = FORMER DIMENSION OF IPACK( ),IWORK( ), FD1( ),
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
C                              MSTORE( , ). LATER USED AS A WAY OF 
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
C             ICCCFFF = CONTAINS IDPARS(1) AND IDPARS(2) ID FOR THE 
C                       VISIBILITY.
C              IENTER = NUMBER OF TIMES THIS SUBROUTINE IS ENTERED.
C                       (INTERNAL).
C            IWORD(J) = HOLDING VARIABLE FOR THE FFF PORTION OF
C                       THE VARIABLE ID (J=1,3) (INTERNAL).
C                IPWX = HOLDING VARIABLE FOR THE PRESENT WEATHER CODE.
C                       (INTERNAL)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD1() -PRESENT WEATHER GROUPS (J=1,4) (INTERNAL).
C              MDV(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD2(). (J=1,4) (INTERNAL)
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
C                SOBS = USED AS A HOLDING VARIABLE SO THE 3 PRESENT
C                       WEATHER GROUPS ARE LOOKED AT AND NOT
C                       OVERWRITTEN. (INTERNAL)
C        1         2         3         4         5         6         7 X
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
      INTEGER LD(4),IWORD(3),MDV(4)
      INTEGER I,ICCCFFF,J,KFILDO,KFIL10,NDATE,ND1,NSTA,ISTAV,IENTER,
     1        L3264B,ND2X3,IER,ND9,LITEMS,ND10,NBLOCK,NFETCH,NSLAB,
     2        IPWX,NTIMES,NWORDS,ND7,MISSP,MISSS,NPACK
C
      REAL SDATA(ND1)
      REAL FD1(ND2X3),FD2(ND2X3)
      REAL CORE(ND10)
      REAL SOBS
C
      DATA IENTER/0/,
     1     IWORD/500,510,520/,
     2     ICCCFFF/708100/
      SAVE IENTER
C
C        INITIALIZE THE NECESSARY VARIABLES, INCLUDING SDATA.
C
      IER=0
      ISTAV=1
C
      IENTER=IENTER+1
C     
C        SET SDATA TO 9999.
C
      DO 25 I=1,ND1
        SDATA(I)=9999.
 25   CONTINUE
C
C        CHECK IF THE VARIABLE ID REQUESTED IN THE CALL IS 
C        VISIBILITY TYPE. 
C
      IF((IDPARS(1).NE.708).AND.(IDPARS(2).NE.251))THEN
        IER=103
        WRITE(KFILDO,100)(JD(J),J=1,4),IER
 100    FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1          'OBSERVATION OF VISIBILITY.',
     2         /'     VARIABLE ',I9.9,2I10.9,I4.3,
     3          ' NOT ACCOMMODATED IN SUBROUTINE OBSOBVIS.  IER =',I3)
        GOTO 900
      ENDIF
C
C        FIRST CHECK THAT THE USER IS NOT TRYING TO SPAN THE DECEMBER
C        1, 1996 CUTOFF BETWEEN THE OLD PRESENT WEATHER DATA AND THE
C        NEW PRESENT WEATHER DATA.  IF THEY ARE, WRITE A WARNING TO
C        THE OUTPUT FILE SAYING THAT ALL THE DATA WILL BE MISSING.
C
      IF((NDATE.EQ.1996120100).AND.(IENTER.NE.0))THEN
        IER=57
        WRITE(KFILDO,120)(JD(J),J=1,4),IER
 120    FORMAT(/' ****OBSOBVIS CANNOT PROCESS DATA BECAUSE OF THE',
     1          ' DECEMBER 1,1996 CUTOFF.  SEE WRITEUP.',
     2         /'     VALUES OF VARIABLE ',I9.9,2I10.9,I4.3,
     3          ' SET TO MISSING.  IER = ',I4)
        GOTO 900
      ENDIF
C
C        NOW CHECK THE DATE TO DECIDE WHETHER TO USE ALGORITHM 2 FOR 
C        THE DATES MAY 16,1979 THROUGH NOVEMBER 30,1996, OR ALGORITHM 1
C        FOR DECEMBER 1, 1996 AND ON.
C
      IF(NDATE.GE.1996120100)THEN
C
C          THIS IS ALGORITHM 1 FOR THE DATA FROM DECEMBER 1, 1996
C          AND ON.  THIS LOOP WILL GO THROUGH THE ALGORITHM ONCE 
C          FOR EACH OF THE 3 PRESENT WEATHER GROUPS, THUS CREATING
C          A 'CUMULATIVE' VISIBILITY TYPE.
C
        DO 200 I=1,3
C
C            SET UP THE LD ARRAY TO HOLD THE ID FOR THE PRESENT 
C            WEATHER GROUP THAT WILL BE FETCHED AND PROCESSED
C
          LD(1)=708000000+IWORD(I)*1000+IDPARS(4)
          LD(2)=IDPARS(7)
          LD(3)=IDPARS(9)*1000000+IDPARS(12)
          LD(4)=0
C
C            CALL GFETCH TO GET THE PRESENT WEATHER GROUP IN FD1( ).
C
          CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1                IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2                NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3                NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,
     4                1,IER)
C
C            CHECK TO SEE IF IER NE 0, AND IF THIS IS THE FIRST
C            PROCESS DATE.  PRINT ERROR MESSAGE IF NEEDED.
C
          IF((IER.NE.0).AND.(IENTER.EQ.1)) THEN
            WRITE(KFILDO,140)NDATE,I,(JD(J),J=1,4),IER
 140        FORMAT(/' ****ERROR FROM GFETCH OCCURRED ON',
     1              ' 1ST PROCESS DATE = ',I12,
     2              ' FETCHING PRESENT WEATHER GROUP NO.', I3,' AT 140.'
     3             /'     VALUES OF VARIABLE ',I9.9,2I10.9,I4.3,
     4              ' SET TO MISSING.  IER = ',I4)
            GOTO 900
          ENDIF
C
          IF(IER.NE.0) GO TO 900
C            ERROR IS PRINTED ONLY ON FIRST DATE.
C
C            IF NWORDS DOES NOT EQUAL NSTA, SET ALL VALUES TO MISSING.
C
          IF(NWORDS.NE.NSTA)THEN
            IER=52
            WRITE(KFILDO,160)NWORDS,NSTA,(JD(J),J=1,4),IER
 160        FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1              ' NOT EQUAL TO NSTA =',I6,
     2              ' AT 160 IN OBSOBVIS.',
     3             /'     VALUES OF VARIABLE ',I9.9,2I10.9,I4.3,
     4              ' SET TO MISSING.  IER = ',I4)
            GOTO 900
          ENDIF
C
C            SET THE VALUE OF SDATA( ) DEPENDING ON THE COMBINATIONS OF
C            THE PRESENT WEATHER REPORTS.
C
          DO 170 J=1,NSTA
C
C              FIRST CHECK WHAT CATEGORY THE PRESENT WEATHER GROUP FALLS
C              INTO.  THEN CHECK TO SEE IF A PREVIOUS PRESENT WEATHER
C              GROUP HAS CAUSED SDATA( ) TO ALREADY BE SET TO A CATEGORY.
C
            IPWX = NINT(FD1(J))
C
            SELECT CASE(IPWX)
C
C                   CASE NONE
C
              CASE(0,8,9,11,16,17,18,19,40,51,53,55,56,
     1             57,58,59,61,63,65,66,67,68,69,71,73,75,
     2             76,77,79,80,81,83,84,85,86,88,90,95,
     3             96,97,121,156,166,174,176,183,187,
     4             196,204,207)
                 SOBS=1.
C
C                   HAZE,SMOKE,DUST 
C
              CASE(4,5,6)
                 SOBS=2.
C
C                   FOG
C
              CASE(10,41,44)
                 SOBS=3.
C
C                   DENSE FOG
C
              CASE(45)
                 SOBS=4.
C
C                   BLOWING PHENOMENA
C
              CASE(7,31,34,36,37,38,39,98,208)
                 SOBS=5.
C
C                   CASE DEFAULT
C
              CASE(9999)
                 SOBS=9999.
C
            END SELECT
C
C              IF TEST TO CHECK TO MAKE SURE SDATA(J) IS NOT
C              OVERWRITTEN.
C
            IF(NINT(SDATA(J)).EQ.9999)THEN
	      SDATA(J)=SOBS
            ELSEIF((NINT(SDATA(J)).EQ.1).AND.(NINT(SOBS).NE.9999))THEN
C                SDATA( ) = 1 MEANS NONE; REPLACE WITH WHATEVER.
              SDATA(J)=SOBS
            ENDIF
C
 170      CONTINUE 
C
 200    CONTINUE
C
        GOTO 910
C  
      ELSE IF((NDATE.GE.1979051600).AND.(NDATE.LE.1996113023))THEN
C
C          THIS IS ALGORITHM 2 FOR THE DATA FROM MAY 16,1979 TO
C          NOVEMBER 30, 1996.
C
C          SET UP THE LD ARRAY TO HOLD THE ID FOR THE OBSTRUCTION
C          TO VISION GROUP THAT WILL BE FETCHED AND PROCESSED.
C
        LD(1)=708250000+IDPARS(4) 
        LD(2)=IDPARS(7)
        LD(3)=IDPARS(9)*1000000+IDPARS(12)
        LD(4)=0
C
C          CALL GFETCH TO GET THE OBSTRUCTION TO VISION IN FD1( ).
C
        CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1              IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2              NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3              NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4              IER)
C
C          CHECK TO SEE IF IER NE 0, AND IF THIS IS THE FIRST
C          PROCESS DATE.  PRINT ERROR MESSAGE IF NEEDED.
C
        IF((IER.NE.0).AND.(IENTER.EQ.1)) THEN
            WRITE(KFILDO,220)NDATE,(JD(J),J=1,4),IER
 220        FORMAT(/' ****ERROR FROM GFETCH OCCURRED ON',
     1              ' 1ST PROCESS DATE = ',I12,
     2              ' AT 220 FETCHING OBSTRUCTION TO VISION.'
     3             /'     VALUES OF VARIABLE ',I9.9,2I10.9,I4.3,
     4              ' SET TO MISSING.  IER = ',I4)
           GOTO 900
        ENDIF
C
        IF(IER.NE.0)GOTO 900
C           ERROR IS PRINTED ONLY ON FIRST DATE.
C
C           IF NWORDS DOES NOT EQUAL NSTA, SET ALL VALUES TO MISSING.
C
        IF(NWORDS.NE.NSTA)THEN
           IER=52
           WRITE(KFILDO,240)NWORDS,NSTA,(JD(J),J=1,4),IER
 240       FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1             ' NOT EQUAL TO NSTA =',I6,
     2             ' IN OBSOBVIS AT 240 FETCHING OBSTRUCTION',
     3             ' TO VISION.',
     4            /'     VALUES OF VARIABLE ',I9.9,2I10.9,I4.3,
     5             ' SET TO MISSING.  IER = ',I4)
           GOTO 900
        ENDIF
C
C          CALL GFETCH TO GET VISIBILITY IN FD2( ).
C
        MDV(1)=ICCCFFF*1000+IDPARS(4)
        MDV(2)=IDPARS(7)
        MDV(3)=IDPARS(9)*1000000+IDPARS(12)
        MDV(4)=0
        CALL GFETCH(KFILDO,KFIL10,MDV,7777,LSTORE,ND9,LITEMS,
     1                 IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2                 NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3                 NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4                 IER)
C
C
        IF((IER.NE.0).AND.(IENTER.EQ.1)) THEN
            WRITE(KFILDO,250)NDATE,(JD(J),J=1,4),IER
 250        FORMAT(/' ****ERROR FROM GFETCH OCCURRED ON',
     1              ' 1ST PROCESS DATE = ',I12,
     2              ' AT 250 FETCHING VISIBILITY.'
     3             /'     VALUES OF VARIABLE ',I9.9,2I10.9,I4.3,
     4              ' SET TO MISSING.  IER = ',I4)
            GOTO 900
        ELSE
           GOTO 900
C             ERROR IS PRINTED ONLY ON FIRST DATE.
        ENDIF
C
C          IF NWORDS DOES NOT EQUAL NSTA, SET ALL VALUES TO MISSING.
C
        IF(NWORDS.NE.NSTA)THEN
           IER=52
           WRITE(KFILDO,340)NWORDS,NSTA,(JD(J),J=1,4),IER
 340       FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1             ' NOT EQUAL TO NSTA =',I6,
     2             ' IN OBSOBVIS AT 340 FETCHING VISIBILITY.',
     3            /'     VALUES OF VARIABLE ',I9.9,2I10.9,I4.3,
     4             ' SET TO MISSING.  IER = ',I4)
           GOTO 900
        ENDIF
C
C            SET THE VALUE OF SDATA DEPENDING ON THE PRESENT WEATHER
C            REPORT.
C        
        DO 300 J=1,NSTA
           IPWX = NINT(FD1(J))
           SELECT CASE(IPWX)
C
C              OBSTRUCTION TO VISIBILITY CATS.
C
             CASE(0)
                SDATA(J)=1.
             CASE(1,2)
                SDATA(J)=2.
             CASE(3)
                SDATA(J)=5.
             CASE(4,5,6)
C
C                  CHECK FOR VISIBILITY WITH FOG.  IF 5/8 MI OR
C                  GREATER, SET OBSTRUCTION TO VISION TO LIGHT
C                  FOG MIST.  IF LESS THAN 5/8 MI, SET OBSTRUCTION 
C                  TO DENSE FOG.
C
                IF(NINT(FD2(J)).EQ.9999)THEN
                   SDATA(J)=9999.
                ELSEIF(FD2(J).GE..60)THEN
                   SDATA(J)=3.
                ELSE
                   SDATA(J)=4.
                END IF
C
C                  CASE DEFAULT
C
              CASE(9999)
                 SDATA(J)=9999.
           END SELECT
C
 300    CONTINUE
C
        GOTO 910 
      ENDIF
C
C        NOTE THAT IT WILL DORP THROUGH TO HERE IF THE DATE IS
C        BEFORE 1979051600.
C
C        IF THERE WAS AN ERROR IN GFETCH OR FOR SOME OTHER REASON,
C        THE CODE WILL JUMP TO HERE.  SET SDATA TO MISSING BEFORE
C        RETURNING. 
C
 900  DO 905 J=1,ND1
        SDATA(J)=9999.
 905  CONTINUE
C
 910  RETURN
      END     
