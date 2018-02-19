      SUBROUTINE RDEQN95(KFILDO,KFILEQ,EQNNAM,NDATE,
     1                  IP4,IP13,IP14,IP15,IP19,IP20,IP21,
     2                  CCALL,IFOUND,NSTA,IALL,INITF,CCALLD,   
     3                  KGP,NGP,LGP,
     4                  MTRMS,MTANDS,
     5                  IDEQN,IDTAND,MEMNUM,
     6                  LOCSTA,CONST,
     7                  AVG,CORR,
     8                  ESS,COEF,X,
     9                  ND1,ND2,ND3,ND5,ND13,IER)
C
C        OCTOBER   2007   JRW     ADOPTED FROM RDEQN9.  CREATED FOR USE BY
C                                 U905.
C        NOVEMBER  2007   JRW     ADDED MEMNUM TO CONTROL FILE. MEMNUM
C                                 IS USED TO DEFINE THE PREDICTOR
C                                 AND PREDICTAND ID, BY MODIFYING THE DD.
C
C        PURPOSE
C            TO READ ONE SET OF EQUATIONS FROM A FILE FOR U705 OR
C            U905.   A SET IS DEFINED AS A GROUP OF EQUATIONS ALL
C            HAVING THE SAME PREDICTANDS.  TEMPERATURE EQUATIONS
C            DERIVED SIMULTANEOUSLY FOR SEVERAL 3-HOURLY PROJECTIONS
C            AND A MAXIMUM WOULD BE ONE SET, WHETHER SINGLE STATION OR
C            REGIONALIZED, BECAUSE THE PREDICTANDS ARE THE SAME. 
C            ANOTHER GROUP OF TEMPERATURE EQUATIONS FOR OTHER 
C            PROJECTIONS WOULD BE ANOTHER SET, AS WOULD BE A SET OF
C            DEW POINT EQUATIONS NOT DERIVED SIMULTANEOUSLY WITH THE 
C            TEMPERATURE EQUATIONS.  EACH SET IS READ FROM A PARTICULAR
C            LOGICAL FILE, WHERE A LOGICAL FILE HAS A UNIT NUMBER IN 
C            KFILEQ AND A NAME IN EQNNAM.  HOWEVER, A UNIT NUMBER AND 
C            ASSOCIATED NAME COULD BE THE SAME AS IN A PREVIOUS CALL 
C            IF THE EQUATIONS ARE ON THE SAME PHYSICAL FILE.  THAT IS,
C            ALL SETS OF, SAY, TEMPERATURE EQUATIONS COULD BE ON THE 
C            SAME PHYSICAL FILE, EVEN THOUGH THE PROJECTIONS MIGHT BE
C            DIFFERENT, AND AS SUCH DIVIDED INTO SETS.  THE LIST
C            OF STATIONS CONSISTING OF ALL STATIONS WITH THE
C            THE EQUATIONS IS PROVIDED BACK IN CCALL( ,1) WHEN 
C            IALL = 1; OTHERWISE, WHEN IALL = 0, CCALL( ,1) AND
C            CCALL( ,2) WILL CONTAIN THE PRIMARY AND BACKUP 
C            STATIONS, AND THE EQUATION STATION LIST IS KEYED
C            TO EITHER.  WHEN CALLED FROM U705, EQNNAM IS THE FILE
C            NAME AND MUST MATCH THE INTERNAL FILE NAME FOR 
C            SAFETY.  WHEN CALLED FROM U905, THE INTERNAL FILE
C            NAME IS NOT PRESENT, BUT IS REPLACED WITH A DATE/TIME,
C            WHICH MUST MATCH NDATE.
C   
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C            KFILEQ    - UNIT NUMBER FOR READING EQUATION FILE.
C                        (INPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              KFILEQ = UNIT NUMBER FOR READING EQUATION FILE.  (INPUT)
C              EQNNAM = FOR U705, NAME OF FILE CORRESPONDING TO KFILEQ.
C                       FOR U905, THIS IS NOT USED (NDATE = 9999). 
C                       (CHARACTER*60)  (INPUT)
C               NDATE = FOR U905, NDATE IS THE DATE/TIME BEING PROCESSED.
C                       THIS MUST BE WITHIN THE BEGINNING AND ENDING
C                       DATES (MONTH/DAY) AND FOR THE HOUR (CYCLE)
C                       SPECIFIED IN THE EQUATION FILE.
C                       FOR U705, THIS MUST BE 9999.
C                 IP4 = INDICATES WHETHER (>0) OR NOT (=0) THE LIST
C                       OF STATIONS TO MAKE FORECASTS FOR IS TO BE
C                       WRITTEN TO UNIT IP4.  THIS WILL HAPPEN ONLY
C                       IF IALL = 1.  (INPUT)
C                IP13 = INDICATES WHETHER (>0) OR NOT (=0) A DIAGNOSTIC
C                       WILL BE PROVIDED FOR A STATION IN THE LIST
C                       CCALL( , ) THAT DOES NOT HAVE AN EQUATION IN 
C                       THE SET BEING PROCESSED.  THIS WOULD BE
C                       USEFUL ONLY WHEN IALL = 0.
C                IP14 = INDICATES WHETHER (>0) OR NOT (=0) A DIAGNOSTIC
C                       WILL BE WRITTEN WHEN AN EQUATION SET IS FOUND
C                       THAT DOES NOT MATCH NDATE.  (THIS IS NOT USED
C                       FOR U705; NDATE = 9999.)
C                IP15 = INDICATES WHETHER (>0) OR NOT (=0) A DIAGNOSTIC
C                       WILL BE WRITTEN TO UNIT IP15 WHEN A STATION
C                       WITH THE EQUATIONS IS NOT IN THE MASTER LIST
C                       IN CCALL( ).  WHEN IALL = 1, THIS WOULD NOT
C                       OCCUR, BECAUSE IN THAT CASE CCALL( ) CONSISTS
C                       OF ALL THE STATIONS WITH THE EQUATIONS.
C                IP19 = INDICATES WHETHER (>0) OR NOT (=0) THE
C                       PREDICTAND MULTIPLE CORRELATIONS WILL BE
C                       WRITTEN TO UNIT IP19 FOR VIEWING.  (INPUT)
C                IP20 = INDICATES WHETHER (>0) OR NOT (=0) THE
C                       PREDICTAND MEANS WILL BE WRITTEN TO UNIT IP20
C                       FOR VIEWING.  (INPUT)
C                IP21 = INDICATES WHETHER (>0) OR NOT (=0) THE
C                       EQUATIONS WILL BE WRITTEN TO UNIT IP21 FOR 
C                       VIEWING.  (INPUT)
C          CCALL(K,J) = THE LIST OF STATIONS TO MAKE FORECASTS FOR
C                       (J=1) (K=1,NSTA).  WHEN IALL = 0, THIS WILL
C                       BE INPUT AND THE STATIONS WITH THE
C                       EQUATIONS ARE KEYED TO EITHER (J=1) OR (J=2),
C                       WHERE, NORMALLY, CCALL( ,1) WILL CONTAIN THE
C                       ICAO STATION IDENTIFIERS AND CCALL( ,2) WILL
C                       CONTAIN THE OLD STATION CALL LETTERS.  THESE
C                       COULD BE REVERSED; BUT THE STATIONS WITH
C                       THE EQUATIONS WILL BE KEYED TO EITHER.
C                       WHEN IALL NE 0, CCALL( ,1) WILL BE OUTPUT
C                       AND WILL, BE THE STATIONS WITH THE EQUATIONS.
C                       (CHARACTER*8) (INPUT OR OUTPUT)
C           IFOUND(K) = INITIALLY SET TO ZERO (K=1,ND1).  SET TO 1
C                       WHEN AN EQUATION FOR A STATION IN THE
C                       CCALL( , ) LIST IS FOUND IN A SET SO THAT
C                       DUPLICATES CAN BE DETECTED.  THIS IS ISDATA( )
C                       IN U705 AND U905.  (INTERNAL)
C                NSTA = THE NUMBER OF STATIONS TO MAKE FORECASTS FOR.
C                       WHEN IALL = 0, THIS WILL BE INPUT.  WHEN
C                       IALL NE 0, THIS WILL BE OUTPUT.
C                       (INPUT OR OUTPUT)
C                IALL = 0 WHEN THE MASTER LIST OF STATIONS IS INPUT IN
C                         CCALL( ,1) AND IS TO BE USED.
C                     = 1 WHEN THE MASTER LIST OF STATIONS IS GENERATED 
C                         IN RDEQN95 AND IS COMPOSED OF ALL STATIONS IN 
C                         THE EQUATIONS.  THIS LIST WILL BE IN THE ORDER 
C                         THE STATIONS ARE ENCOUNTERED IN THE EQUATIONS.
C                       (INPUT OR OUTPUT)
C               INITF = 0 WHEN EVERY STATION IN THE LIST IN CCALL( ,1) 
C                         HAS AN EQUATION AND INITIALIZATION OF 
C                         FCST( , , ) DOES NOT HAVE TO BE DONE IN
C                         FCST71 AND FCST72.
C                     = 1 OTHERWISE.
C                       (OUTPUT)
C           CCALLD(K) = SCRATCH ARRAY (K=1,ND1)  (CHARACTER*8)  
C                       (INTERNAL)
C                 KGP = THE NUMBER OF EQUATIONS FOR THIS SET.  (OUTPUT)
C              NGP(L) = FOR EACH EQUATION (L=1,KGP) IN THIS SET, THE 
C                       NUMBER OF STATIONS IN EACH GROUP.  (OUTPUT)
C              LGP(L) = FOR EACH EQUATION (L=1,KGP) IN THIS SET, THE 
C                       LOCATION IN LOCSTA ( ,I) OF WHERE THE FIRST 
C                       STATION IN THE SET IS.  (OUTPUT)
C            MTRMS(L) = THE NUMBER OF TERMS IN EACH EQUATION 
C                       (L=1,KGP) FOR THIS SET.  (OUTPUT)
C              MTANDS = THE NUMBER OF PREDICTANDS FOR EACH EQUATION. 
C                       (OUTPUT)
C        IDEQN(J,L,M) = THE 4-WORD ID (J=1,4) FOR EACH PREDICTOR
C                       (M=1,NTRMS) IN EACH EQUATION (L=1,KGP),
C                       FOR THIS EQUATION SET.  (THE VALUES
C                       (IDEQN(5, , ), IDEQN(6, , ), AND 
C                       IDEQN(7, , ) ARE USED LATER.  (OUTPUT)
C        IDTAND(J,NN) = THE PREDICTAND ID'S (J=1,4) AND PREDICTAND NN 
C                       (NN=1,MTANDS) FOR THIS EQUATION SET.  (OUTPUT)
C              MEMNUM = USED TO DETERMIN WHICH ENSEMBLE MEMBER THE EQUATION
C                       WILL BE APPLIED TO. IF ZERO USE THE EQUATION DD
C                       IF NON ZERO APPLY NUMBER TO PREDICTAND
C                       AND PREDICTOR DD FROM EQUATION. (INPUT)
C           LOCSTA(K) = THE LOCATION IN FCST ( ,N) OF WHERE TO PUT THE 
C                       FORECAST, WHERE K IS IN ORDER OF THE EQUATIONS
C                       AS READ IN (K=1,NSTA).  (OUTPUT)
C         CONST(L,NN) = THE EQUATION CONSTANTS FOR GROUP L (L=1,KGP) 
C                       AND PREDICTAND NN (NN=1,MTANDS) FOR THIS
C                       EQUATION SET.  (OUTPUT)
C           AVG(L,NN) = THE PREDICTAND MEANS FOR GROUP L (L=1,KGP) AND 
C                       PREDICTAND NN (NN=1,MTANDS) FOR THIS EQUATION 
C                       SET.  (OUPTUT)
C          CORR(L,NN) = THE MULTIPLE CORRELATIONS FOR GROUP L 
C                       (L=1,KGP) AND PREDICTAND NN (NN=1,MTANDS) FOR
C                       THIS EQUATION SET.  (OUTPUT)
C        COEF(L,M,NN) = THE COEFFICIENTS FOR GROUP L (L=1,KGP),
C                       TERM M (M=1,MTRMS), AND PREDICTAND NN 
C                       (NN=1,MTANDS) FOR THIS EQUATION SET.  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                       WITH.  USED AS DIMENSION OF SEVERAL VARIABLES.
C                       (INPUT)
C                 ND2 = MAXIMUM NUMBER OF TERMS IN ANY EQUATION.
C                       USED AS DIMENSION OF SEVERAL VARIABLES.  (INPUT)
C                 ND3 = MAXIMUM NUMBER OF PREDICTANDS IN ANY EQUATION.
C                       USED AS DIMENSION OF SEVERAL VARIABLES.  (INPUT)
C                 ND5 = THE MAXIMUM NUMBER OF CALL LETTERS FOR A SET
C                       OF EQUATIONS.  THIS COULD BE GT ND1.  DIMENSION
C                       CCALLD( ).
C                ND13 = MAXIMUM NUMBER OF DIFFERENT EQUATIONS PER SET.
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       165 = EXTERNAL AND INTERNAL FILE NAMES DON'T
C                             MATCH WHEN NDATE = 9999.
C                       166 = NUMBER OF PREDICTANDS = ZERO OR GT ND3.
C                       167 = NUMBER OF TERMS IN EQUATION = 0 OR GT ND2.
C                       168 = SYSTEM ERROR READING.
C                       169 = NUMBER OF EQUATIONS GT ND1 OR GT ND13.
C                       171 = NO EQUATION FOR ONE OR MORE STATIONS
C                             IN THE STATION LIST.
C                        20 = ERROR OR END OF FILE ON UNIT KFILEQ OR
C                             DATES ON EQUATION FILE DO NOT MATCH NDATE
C                             (FROM RDC OR RDEQN95).
C                        21 = LIST TOO LONG FOR DIMENSION ND ON UNIT
C                             KFIL (FROM RDC).
C               NTAND = THE NUMBER OF PREDICTANDS FOR THIS PARTICULAR
C                       SET OF EQUATIONS.  (INTERNAL)
C              NCOUNT = COUNTS TOTAL NUMBER OF STATIONS TO MAKE 
C                       FORECASTS FOR IN ALL SETS.  (INTERNAL)
C              JCOUNT = COUNTS NUMBER OF STATIONS TO MAKE FORECASTS FOR
C                       FOR EACH SET.  (INTERNAL)
C              ICOUNT = REGULATES PRINT SPACING IN DO 195 LOOP.
C                       (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUINES USED 
C            IERX, RDC
C     
      CHARACTER*4 STATE
      CHARACTER*8 CCALL(ND1,6),CTEMP(14)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 EQNNAM,EQNCHK
C
      REAL*8 X(ND2+1,ND2+1,ND13)
C
      DIMENSION IFOUND(ND1),LOCSTA(ND1)
      DIMENSION NGP(ND13),LGP(ND13),MTRMS(ND13)
      DIMENSION IDEQN(7,ND13,ND2)
      DIMENSION CONST(ND13,ND3),
     1          AVG(ND13,ND3),
     2          ESS(ND13,ND3),
     2          CORR(ND13,ND3)
      DIMENSION COEF(ND13,ND2,ND3)
      DIMENSION IDTAND(4,ND3)
C
      DATA IFIRST/0/
C
      IFIRST=IFIRST+1
      IER=0
      INITF=0
      NCOUNT=1
      KGP=0
      IF(IALL.EQ.1)NSTA=0 
C        WHEN IALL = 0, THE NUMBER OF STATIONS WILL BE DETERMINED
C        IN RDEQN95.
C
C        INITIALIZE IFOUND( ).
C
      DO 105 K=1,ND1
      IFOUND(K)=0
      LOCSTA(K)=0
 105  CONTINUE
C
      IF(NDATE.EQ.9999)THEN
C
C           READ AND CHECK THE FILE NAME.  THIS IS FOR U705.
C
         STATE=' 110'
         READ(KFILEQ,110,IOSTAT=IOS,ERR=900,END=900)EQNCHK
 110     FORMAT(' ',A60)
C
         IF(EQNCHK.NE.EQNNAM)THEN
            WRITE(KFILDO,111)EQNNAM,EQNCHK
 111        FORMAT(/,' ****INPUT EQUATION FILE NAME     ',A60,/,
     1               '     DOES NOT MATCH INTERNAL NAME ',A60,
     2               ' AT 111 IN RDEQN95.',/,
     2               '     READING OF EQUATIONS ABORTED.')
            IER=165
            GO TO 950
         ENDIF
C
      ELSE
C
C           READ AND CHECK THE DATE.  THIS IS FOR U905.
C
         STATE=' 111'
         READ(KFILEQ,1115,IOSTAT=IOS,ERR=900,END=900)
     1        IHOUR,MONDAS,MONDAE
 1115    FORMAT(1X,I4,5X,I4,1X,I4)
         CALL DOY(NDATE,JY,JM,JD,JH,MDAY)
C
         IF(IHOUR.EQ.JH*100)THEN
C
            IF(MONDAS.LE.MONDAE)THEN
               IF(JM*100+JD.GE.MONDAS.AND.
     1            JM*100+JD.LE.MONDAE)GO TO 1119
            ELSE
               IF(JM*100+JD.LE.MONDAE.OR.
     1            JM*100+JD.GE.MONDAS)GO TO 1119
            ENDIF
C 
         ENDIF
C                           
         IF(IP14.NE.0)WRITE(IP14,1117)IHOUR,MONDAS,MONDAE,JY,JM,JD,JH,
     1                                EQNNAM
 1117    FORMAT(/,' ****EQUATION SET FOR HOUR',I4.2,
     1            '  STARTING AND ENDING DATES',2I6.4,
     2            '  DOES NOT MATCH NDATE',I6,I3.2,I2.2,I3.2,/,
     3            '     ON FILE ',A60,/,
     4            '     PROCEEDING TO ANOTHER EQUATION SET.')
         IER=20
         GO TO 950
      ENDIF
C    
C        READ NUMBER OF PREDICTANDS.
C
 1119 STATE=' 112'
      READ(KFILEQ,112,IOSTAT=IOS,ERR=900,END=900)NTAND
 112  FORMAT(1X,I4)
C
      IF(NTAND.EQ.0)THEN
         WRITE(KFILDO,113)EQNNAM
 113     FORMAT(/,' ****NUMBER OF PREDICTANDS EQUALS ZERO AT 113',
     1            ' IN RDEQN95 FOR FILE ',A60,/,
     2            '     READING OF EQUATIONS ABORTED.')
         IER=166
         GO TO 950
C
      ENDIF
C
      IF(NTAND.GT.ND3)THEN
         WRITE(KFILDO,1130)NTAND,ND3,EQNNAM
 1130    FORMAT(/,' ****NUMBER OF PREDICTANDS =',I4,' GT ND3 =',I4,
     1            ' AT 1130 IN RDEQN95 FOR FILE ',A60,/,
     2            '     READING OF EQUATIONS ABORTED.')
         IER=166
         GO TO 950
C
      ENDIF
C
C        READ PREDICTAND DEFINITIONS.
C        CHECK TO SEE IF MEMNUM IS NON-ZERO.  IF SO REDEFINE
C        PREDICTAND DD'S
C
      WRITE(KFILDO,1135)EQNNAM
 1135 FORMAT(/,' PREDICTANDS FOR EQUATION SET ON FILE ',A60)
      IF(IP21.NE.0.AND.IP21.NE.KFILDO)WRITE(IP21,1135)EQNNAM
C
      STATE=' 114'
C
      DO 115 N=1,NTAND
      READ(KFILEQ,114,IOSTAT=IOS,ERR=900,END=900)
     1                (IDTAND(J,N),J=1,4)
 114  FORMAT(1X,I9,2I10,I11)
      IF(MEMNUM.NE.0)IDTAND(1,N)=((IDTAND(1,N)/100)*100)+MEMNUM
 115  CONTINUE
C
      DO 120 N=1,NTAND
      WRITE(KFILDO,117)(IDTAND(J,N),J=1,4)
 117  FORMAT(3(1X,I9.9),1X,I10.3)
      IF(IP21.NE.0.AND.IP21.NE.KFILDO)
     1     WRITE(IP21,117)(IDTAND(J,N),J=1,4)
C
 120  CONTINUE
C
C        READ CALL LETTERS FOR THE EQUATIONS TO FOLLOW.
C        THIS SECTION APPLIES WHEN ONLY THE STATIONS INPUT
C        EARLIER ARE TO BE USED, NOT ALL THOSE WITH THE 
C        EQUATIONS.
C
 121  IF(IALL.EQ.0)THEN
C
C
         CALL RDC(KFILDO,KFILDO,KFILEQ,CCALLD,ND5,CTEMP,14,
     1            '(14(1X,A8))',NSTA1,'99999999',IER1)
         IF(IER1.NE.0)THEN
            WRITE(KFILDO,122)EQNNAM
 122        FORMAT(/,' ****READING OF EQUATIONS ABORTED AT 122',
     1              ' IN RDEQN95 ON FILE ',A60)
            GO TO 800
         ENDIF
C
         IF(NSTA1.EQ.0)GO TO 190
C           AN EMPTY STATION LIST TERMINATES READING FOR THIS
C           SET.  THERE SHOULD BE A TERMINATOR ON THE INPUT
C           OR A DIAGNOSIC WILL OCCUR IN RDC.
C 
         IF(IP21.NE.0)THEN
            WRITE(IP21,1220)NSTA1,(CCALLD(J),J=1,NSTA1)
 1220       FORMAT(/,' ****************************************',/,/,
     1               ' ',I6,' STATIONS IN LIST STORED WITH',
     2               ' THESE EQUATIONS',/,('   ',10(' ',A8)))
         ENDIF
C
         KGP=KGP+1
C
         IF(KGP.GT.ND1)THEN
            WRITE(KFILDO,1225)KGP,ND1,EQNNAM
 1225       FORMAT(/,' ****NUMBER OF STATIONS TO MAKE FORECASTS',
     1               ' FOR =',I6,' EXCEEDS ND1 =',I6,
     2               ' AT 1225 IN RDEQN95.',/,'     ON FILE ',A60,/,
     3               '     READING OF EQUATIONS ABORTED.')
            KGP=KGP-1
            IER=169
            GO TO 950
C
         ENDIF
C
         IF(KGP.GT.ND13)THEN
            WRITE(KFILDO,1226)ND13,EQNNAM
 1226       FORMAT(/,' ****NUMBER OF GROUPS TO MAKE FORECASTS',
     1               ' FOR EXCEEDS ND13 =',I6,' AT 1226 IN RDEQN95.',/,
     2               '     ON FILE ',A60,/,
     3               '     READING OF EQUATIONS ABORTED.')
            KGP=KGP-1
            IER=169
            GO TO 950
C
         ENDIF
C
         JCOUNT=0
         LGP(KGP)=NCOUNT
C
C           SET UP START AND END OF LOOP ON L FOR NSTA.
C
         NSTART=1
         NEND=NSTA
C
         DO 125 K=1,NSTA1
C
C           IF THE STATION READ IS NOT TO BE USED, DON'T 
C           COUNT IT.  AS OF 9/2000, CHANGED 6-WAY IF TEST
C           TO LOOP THROUGH ALL THE LINKS OF CCALL.
C           NOTE THAT WE ARE MOST LIKELY TO FIND MATCH IN
C           THE FIRST LINK, SO WE CHECK ALL STATIONS IN THIS 
C           LINK BEFORE PROCEEDING TO SECOND, ETC.  RDEQN95
C           STARTS A NEW SEARCH ON CCALL(L, ) AT THE NEXT
C           ENTRY RATHER THAN AT THE BEGINNING OF THE LOOP.
C           NOTE THAT THE NSTA LOOP NEEDS TO BE INSIDE THE NSTA1
C           LOOP SO THAT THE FORECASTS WILL BE IN THE ORDER OF
C           THE STATION LIST IN CCALL( , ).
C
         DO 1244 MM=1,6
C
         IF(MM.NE.1) THEN
           NSTART=1
           NEND=NSTA
         ENDIF
C
C
 1227    DO 124 L=NSTART,NEND
C***         WRITE(KFILDO,1228)K,MM,L,CCALLD(K),CCALL(L,MM),IFOUND(L)
C*** 1228    FORMAT(' IN RDEQN95 AT 1228,K,MM,L,CCALLD(K),CCALL(L,MM),',
C***     1          'IFOUND(L),',3I6,2X,A8,1X,A8,I4)
C
         IF(CCALLD(K).EQ.CCALL(L,MM))THEN
C
            IF(NCOUNT.GT.ND1)THEN
               WRITE(KFILDO,1229)ND1,EQNNAM
 1229          FORMAT(/,' ****NUMBER OF STATIONS TO MAKE FORECASTS',
     1                  ' FOR EXCEEDS ND1 =',I6,' AT 1229 IN RDEQN95.',/,
     2                  '     ON FILE ',A60,/,
     3                  '     READING OF EQUATIONS ABORTED.')
               IER=169
               GO TO 950
            ENDIF
C
            IF(IFOUND(L).EQ.1)THEN
               WRITE(KFILDO,1232)CCALLD(K),EQNNAM
 1232          FORMAT(' ****STATION ',A8,' IS IN AN EQUATION STATION',
     1                ' LIST MORE THAN ONCE ON FILE '/
     2                '      ',A60,'   THE FIRST EQUATION IS USED.')
            ELSE
               IFOUND(L)=1
               LOCSTA(NCOUNT)=L
C                 STATIONS NOT IN CCALL( , ) ARE NOT KEPT OR COUNTED.
               NCOUNT=NCOUNT+1
               JCOUNT=JCOUNT+1
C                 JCOUNT COUNTS THE STATIONS IN THIS EQUATION SET.
            ENDIF
C
            GO TO 1248
C
         ENDIF
C
 124     CONTINUE
C 
C           MAY HAVE TO START LOOP AGAIN WHEN STATION NOT FOUND.
C
         IF(NSTART.NE.1)THEN
            NEND=NSTART-1
            NSTART=1
            GO TO 1227
C
         ENDIF
C
 1244    CONTINUE
C
C           A DROP THROUGH HERE MEANS STATION CCALLD(K) COULD NOT
C           BE FOUND.
C
         IF(IP15.NE.0)WRITE(IP15,1245)CCALLD(K)
 1245    FORMAT('     STATION ',A8,' WITH EQUATION NOT IN STATION',
     1          ' LIST TO MAKE FORECASTS FOR.')
C
C           UPDATE NSTART AND NEND.
C
 1248    IF(L.LT.NSTA)THEN
            NSTART=L+1
         ELSE
            NSTART=1
         ENDIF
C
         NEND=NSTA
 125     CONTINUE
C
         NGP(KGP)=JCOUNT
      ELSE
C
C           ALL STATIONS WITH THE EQUATIONS ARE TO BE USED.  EVEN
C           DUPLICATES ARE KEPT.  THE ORDER WILL BE AS READ WITH
C           THE EQUATIONS.  THIS MEANS THAT MORE THAN ONE FORECAST 
C           COULD BE MADE FOR A STATION, EITHER FROM THE SAME
C           EQUATION OR FROM DIFFERENT ONES.
C
         CALL RDC(KFILDO,KFILDO,KFILEQ,CCALL(NCOUNT,1),
     1            ND1-NCOUNT+1,CTEMP,14,'(14(1X,A8))',NSTA1,
     2            '99999999',IER1)
C
         IF(IER1.NE.0)THEN
            WRITE(KFILDO,126)EQNNAM
 126        FORMAT(/,' ****READING ABORTED AT 126 IN RDEQN95,',
     1              ' FOR FILE ',A60)
            GO TO 800
         ENDIF
C
         IF(NSTA1.EQ.0)GO TO 190
C 
         IF(IP21.NE.0)WRITE(IP21,127)
 127     FORMAT(/,' ****************************************',/)
C
C           AN EMPTY STATION LIST TERMINATES READING FOR THIS
C           SET.  THERE SHOULD BE A TERMINATOR ON THE INPUT
C           OR A DIAGNOSIC WILL OCCUR IN RDC.
C
         IF(KGP.GT.ND1)THEN
            WRITE(KFILDO,130)ND1,EQNNAM
 130        FORMAT(/,' ****NUMBER OF STATIONS TO MAKE FORECASTS',
     1               ' FOR EXCEEDS ND1 =',I6,' AT 130 IN RDEQN95.',/,
     2               '     FOR FILE ',A60,/,
     3               '     READING OF EQUATIONS ABORTED.')
            IER=169
            GO TO 950
C
         ELSE
            KGP=KGP+1
            NGP(KGP)=NSTA1
            LGP(KGP)=NCOUNT
C
            DO 131 K=1,NSTA1
            LOCSTA(NCOUNT)=NCOUNT
C***D           WRITE(KFILDO,1305)KGP,NGP(KGP),
C***D    1                        LGP(KGP),
C***D    2                        NSTA1,NCOUNT,
C***D    3                        LOCSTA(NCOUNT)
C***D1305       FORMAT(' KGP,NGP(KGP),',
C***D    1              'LGP(KGP),',
C***D    2              'NSTA1,NCOUNT,',
C***D    3              'LOCSTA(NCOUNT) IN RDEQN95',
C***D    4              ' AT 1305'/10I6) 
            NCOUNT=NCOUNT+1
 131        CONTINUE
C
            NSTA=NCOUNT-1
         ENDIF
C
      ENDIF
C
      IF(IP21.EQ.0)GO TO 1335
      IF(NGP(KGP).EQ.0)THEN
         WRITE(IP21,132)
 132     FORMAT(' EQUATIONS DO NOT APPLY TO ANY STATIONS',
     1          ' IN THE STATION LIST')
      ELSE
         WRITE(IP21,133)NGP(KGP),(CCALL(LOCSTA(K),1),
     2                  K=LGP(KGP),LGP(KGP)+NGP(KGP)-1)
 133     FORMAT(' FOLLOWING EQUATION(S) APPLY TO THE FOLLOWING',I6,
     2          ' STATIONS',/,('   ',10(' ',A8)))
      ENDIF
C
C        READ THE NUMBER OF TERMS IN THE EQUATIONS.
C
 1335 STATE=' 134'
      READ(KFILEQ,134,IOSTAT=IOS,ERR=900,END=900)NTRMS
 134  FORMAT(' ',I4)
C
      IF(NTRMS.EQ.0)THEN
         WRITE(KFILDO,135)EQNNAM
 135     FORMAT(/,' ****ZERO TERMS INDICATED FOR NEXT EQUATION',
     1            ' AT 135 IN RDEQN95 ON FILE ',A60,/,
     2            '     READING OF EQUATIONS ABORTED.')
         IER=167
         GO TO 950
      ENDIF
C
      IF(NTRMS.GT.ND2)THEN
         WRITE(KFILDO,1350)NTRMS,ND2,EQNNAM
 1350    FORMAT(/,' ****NUMBER OF TERMS INDICATED FOR NEXT',
     1            ' EQUATION =',I4,' IS GT ND2 =',I4,/,
     2            '     AT 1350 IN RDEQN95 ON FILE ',A60,/,
     3            '     READING OF EQUATIONS ABORTED.')
         IER=167
         GO TO 950
      ENDIF
C
C        READ THE PREDICTOR ID'S.
C
      STATE=' 136'
      READ(KFILEQ,136,IOSTAT=IOS,ERR=900,END=900)
     1            ((IDEQN(J,KGP,M),J=1,4),M=1,NTRMS)
 136  FORMAT(' ',I9.9,2I10,I11)
C
C        CHECK TO SEE IF MEMNUM IS NON-ZERO.  IF SO REDEFINE
C        PREDICTOR DD'S
C
      IF(MEMNUM.NE.0)THEN
         DO 1361 M=1,NTRMS
            IDEQN(1,KGP,M)=(((IDEQN(1,KGP,M))/100)*100)+MEMNUM
 1361    CONTINUE
      ENDIF
C
C        READ THE PREDICTAND AVERAGES, CORRELATIONS, AND CONSTANTS.
C        SINCE THE DECIMAL POINT IS WITH THE EQUATIONS, IT DOESN'T
C        MATTER WHETHER E12.5, E12.7, OR EVEN E12.0 IS USED FOR
C        READING.
C 
      STATE=' 137'
      READ(KFILEQ,138,IOSTAT=IOS,ERR=900,END=900)
     1                   (AVG(KGP,N),N=1,NTAND)
      STATE=' 138'
      READ(KFILEQ,138,IOSTAT=IOS,ERR=900,END=900)
     1                   (CORR(KGP,N),N=1,NTAND)
      STATE='1385'
      READ(KFILEQ,138,IOSTAT=IOS,ERR=900,END=900)
     1                   (ESS(KGP,N),N=1,NTAND)
      STATE=' 139'
      READ(KFILEQ,138,IOSTAT=IOS,ERR=900,END=900)
     1                   (CONST(KGP,N),N=1,NTAND)
 138  FORMAT(' ',8(E15.9)) 
C     
C        READ THE COEFFICIENTS.
C
      STATE=' 140'
C
      DO 140 M=1,NTRMS
      READ(KFILEQ,138,IOSTAT=IOS,ERR=900,END=900)
     1                 (COEF(KGP,M,N),N=1,NTAND)
 140  CONTINUE
C
C        READ THE CROSS PRODUCT MATRIX.  THE MATRIX IS
C        NPRED1 SQUARE, WHERE NPED1 = NTRMS + 1.
C        NPRED1 IS CONSISTENT WITH THE TERMINOLOGY IN U605.
C
      NPRED1=NTRMS+1
C
      DO 143 L=1,NPRED1
      READ(KFILEQ,142)(X(M,L,KGP),M=1,NPRED1)
 142  FORMAT(' ',8(E15.9))
C        THIS IS THE SAME FORMAT USED IN WOPEQ5 FOR WRITING THE
C        EQUATIONS AND OTHER INFORMATION.
 143  CONTINUE
C
      MTRMS(KGP)=NTRMS
      IF(MTRMS(KGP).EQ.0)KGP=KGP-1
C        THE ABOVE STATEMENT CAUSES THE PREVIOUSLY READ
C        EQUATION TO NOT BE KEPT BECAUSE THERE ARE NO
C        STATIONS WITH IT FOR WHICH FORECASTS ARE TO BE MADE.
      MTANDS=NTAND
C
C        WRITE PREDICTAND MEANS IF DESIRED.
C
      IF(IP20.NE.0)THEN
         WRITE(IP20,144)NGP(KGP),CCALL(LOCSTA(LGP(KGP)),1)
 144     FORMAT(/,' THE FOLLOWING APPLIES TO',I7,
     1            ' STATIONS, THE GROUP STARTING WITH  ',A8)
C
         WRITE(IP20,145)KGP,(AVG(KGP,N),N=1,MIN(6,NTAND))
 145     FORMAT(' AVERAGE FOR EQUATION NO.',I4,22X,6F12.5)
         IF(NTAND.GT.6)WRITE(IP20,146)(AVG(KGP,N),N=7,NTAND)
 146     FORMAT(51X,6F12.5)
      ENDIF   
C
C        WRITE MULTIPLE CORRELATIONS IF DESIRED.
C
      IF(IP19.NE.0)THEN
         IF(IP19.NE.IP20.AND.IP19.NE.IP21.AND.IP20.NE.IP21)THEN
            WRITE(IP19,149)NGP(KGP),CCALL(LOCSTA(LGP(KGP)),1)
 149        FORMAT(/,' THE FOLLOWING APPLIES TO',I7,
     1               ' STATIONS, THE GROUP STARTING WITH  ',A8)
         ENDIF
         WRITE(IP19,150)(CORR(KGP,N),N=1,MIN(6,NTAND))
 150     FORMAT(' CORRELATION COEFFICIENTS',26X,6F12.5)
         IF(NTAND.GT.6)WRITE(IP19,151)(CORR(KGP,N),N=7,NTAND)
 151     FORMAT(51X,6F12.5)
      ENDIF   
C
C        WRITE STANDARD ERROR ESTIMATES IF DESIRED.
C
      IF(IP19.NE.0)THEN
         WRITE(IP19,155)(ESS(KGP,N),N=1,MIN(6,NTAND))
 155     FORMAT(' STANDARD ERRORS OF ESTIMATE',23X,6F12.5)
         IF(NTAND.GT.6)WRITE(IP19,156)(ESS(KGP,N),N=7,NTAND)
 156     FORMAT(51X,6F12.5)
      ENDIF
C
C        WRITE EQUATIONS IF DESIRED.
C
      IF(IP21.NE.0)THEN
         WRITE(IP21,160)(CONST(KGP,N),N=1,MIN(6,NTAND))
 160     FORMAT(' REGRESSION EQUATION(S)',/,'    CONSTANT',39X,6E12.5)
         IF(NTAND.GT.6)WRITE(IP21,161)(CONST(KGP,N),N=7,NTAND)
 161     FORMAT(51X,6E12.5)
C
         DO 180 M=1,NTRMS
         WRITE(IP21,170)
     1        (IDEQN(J,KGP,M),J=1,4),
     2        (COEF(KGP,M,N),N=1,MIN(6,NTAND))
 170     FORMAT(3X,3(1X,I9.9),1X,I10.3,7X,6E12.5)
         IF(NTAND.GT.6)WRITE(IP21,171)(COEF(KGP,M,N),N=7,NTAND)
 171     FORMAT(51X,6E12.5)
C
 180     CONTINUE
C
C           WRITE CALL LETTERS FOR THIS SET.
C
C***D        WRITE(KFILDO,182)KGP,LGP(KGP),NGP(KGP)
C***D182     FORMAT(' IN RDEQN95 AT 182',4I10)
      ENDIF
C
C        WRITE THE INVERSE CROSS PRODUCT MATRIX IF DESIRED.
C
      IF(IP21.NE.0)THEN
         WRITE(IP21,181)
 181     FORMAT(/,' INVERSE OF NPRED+1 SQUARE SYMMETRIC  X''X  MATRIX')
C
         DO 185 L=1,NPRED1
         WRITE(IP21,184)(X(M,L,KGP),M=1,NPRED1)
 184     FORMAT(' ',10F12.8)
 185     CONTINUE
C
      ENDIF
C
      GO TO 121
C
 190  IF(IP21.NE.0)WRITE(IP21,191)
 191  FORMAT(/,' ****************************************')
      IF(IALL.NE.0)GO TO 200
C
C        WHEN A STATION LIST IS FURNISHED, CHECK TO SEE 
C        WHETHER AN EQUATION EXISTS FOR ALL STATIONS FOR
C        THIS SET.
C
      ICOUNT=0
C        ICOUNT USED TO DOUBLE SPACE ONLY FOR THE FIRST MISSING
C        STATION.
C
      DO 195 K=1,NSTA
      IF(IFOUND(K).NE.0)GO TO 195
      ICOUNT=ICOUNT+1
      IF(IP13.EQ.0)GO TO 194
C        THE BELOW MAY GIVE VOLUMINOUS PRINT IN OPERATIONS
C        BECAUSE THERE MAY BE NO EQUATIONS FOR LOTS OF STATIONS
C        FOR SOME PREDICTANDS.
C
      IF(ICOUNT.EQ.1)THEN
         WRITE(IP13,192)CCALL(K,1),EQNNAM
 192     FORMAT(/,' ****',A8,' NO EQUATION FOUND FOR THIS STATION',
     1            ' ON FILE ',A60)
      ELSE
         WRITE(IP13,193)CCALL(K,1),EQNNAM
 193     FORMAT(' ****',A8,' NO EQUATION FOUND FOR THIS STATION',
     1          ' ON FILE ',A60)
      ENDIF
C
 194  IER=171
      INITF=1
C        FCST( , , ) NEEDS TO BE INITIALIZED TO 9999 WHEN
C        THERE IS NOT AN EQUATION FOR ALL STATIONS FOR ALL
C        SETS.
 195  CONTINUE
C 
 200  CONTINUE
C
C        IF ALL STATIONS WITH THE EQUATIONS ARE TO BE USED,
C        PRINT THE LIST IF IP4 NE 0.  IF IALL = 0, THE LIST
C        WILL HAVE ALREADY BEEN PRINTED.
C
      IF(IALL.NE.0.AND.IP4.NE.0)THEN
         WRITE(IP4,210)NSTA,(CCALL(K,1),K=1,NSTA)
 210     FORMAT(/,' ',I6,' STATIONS TO MAKE FORECASTS FOR,',
     1           ' ALL THOSE WITH THE EQUATIONS',/,
     2          ('    ',A8,9(' ',A8)))
      ENDIF
C
D     WRITE(KFILDO,218)KGP
D218  FORMAT(/' KGP'/(' 'I4))
D     WRITE(KFILDO,219)MTANDS
D219  FORMAT(/' MTANDS'/(' 'I4))
D     WRITE(KFILDO,220)(NGP(M),M=1,KGP)
D220  FORMAT(/' NGP'/(' '30I4))
D     WRITE(KFILDO,221)(LGP(M),M=1,KGP)
D221  FORMAT(/' LGP'/(' '30I4))
D     WRITE(KFILDO,222)(MTRMS(M),M=1,KGP)
D222  FORMAT(/' MTRMS'/(' '30I4))
D     WRITE(KFILDO,223)(LOCSTA(K),K=1,NSTA)
D223  FORMAT(/' LOCSTA'/(' '20I6))
C
 800  IF(IER.EQ.0)IER=IER1
C        IER1 COMES FROM RDC.
      GO TO 950
C
 900  CALL IERX(KFILDO,KFILDO,IOS,'RDEQN95',STATE)
      IER=168
C
 950  CONTINUE      
C
      RETURN
      END       

          
