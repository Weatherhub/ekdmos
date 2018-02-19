      SUBROUTINE LMSTR4(KFILDO,NDATE,LSTORE,LITEMS,
     1                  MSTORE,MITEMS,ND9,INCCYL,NCEPNO,MINVEC,MINMOD,
     2                  IDATE,NDATES,ISTOP,IER)
C
C        JUNE      2004   GLAHN   TDL   MOS-2000
C        JULY      2004   GLAHN   ADDED IDATE( ) CHECK WITH INCCYL
C        SEPTEMBER 2004   GLAHN   COMMENTS ONLY
C        OCTOBER   2004   GLAHN   CHANGED TO APPLY MINMOD TO CCC=2XX
C        OCTOBER   2004   GLAHN   REMOVED +2 FOR HOURS TO SAVE DATA;
C                                 SAVING DATA MAX(MINXXX-INCCYL,0)
C
C        PURPOSE
C           TO INITIALIZE MSTORE AND PREPARE LSTORE FOR GCPAC FOR U155.
C
C           THREE TYPES OF VECTOR DATA ARE PROVIDED FOR:
C              (1)  OBSERVATIONS (CCC=7XX)
C                   TAUS ARE ASSUMED TO BE ZERO; CYCLES ARE
C                   ENTERED ACCORDING TO INCCYL
C              (2)  LAMP FORECASTS (CCC=2XX)
C                   TAUS ARE WHAT ARE FOUND IN LSTORE(3, ); CYCLES
C                   ARE ENTERED ACCORDING TO INCCYL
C              (3)  MOS FORECASTS (CCC=2XX)
C                   TAUS ARE WHAT ARE FOUND IN LSTORE(3, ); CYCLES
C                   ARE ENTERED ACCORDING TO INCCYL, BUT ONLY IF
C                   DIVISIBLE BY 6 (ASSUMES MOS FORECASTS ARE MADE
C                   AT EVERY 6 HOURS).  NCEPNO IS USED TO DISTINGUISH
C                   MOS FROM LAMP; NOTE THAT GSM = 08 OR SOME OTHER
C                   MODEL COULD BE USED.
C
C           TWO TYPES OF GRIDDED DATA ARE PROVIDED FOR:
C              (1)  MODEL FORECASTS (CCC=0XX)
C                   TAUS ARE ENTERED FOR MINMOD-INCCYL+2 HOURS PAST THAT
C                   IN LSTORE(3, ) TO GIVE MINMOD HOUR BACKUP CAPABILITY,
C                   BUT ONLY IN 3-H INCREMENTS. THE EXTRA 2 HOURS ARE 
C                   FOR POSSIBLE INTERPOLATION.  CYCLES ARE ENTERED
C                   ACCORDING TO INCCYL, BUT ONLY IF DIVISIBLE BY 6
C                   (ASSUMES MODELS ARE RUN EVERY 6 HOURS).
C              (2)  MOS FORECASTS (CCC=2XX)
C                   THE PRESENT IMPLEMENTATION TREATS MODEL 
C                   AND MOS FORECATS THE SAME (SEE ABOVE).
C
C           NOTE THAT A FIRST GUESS FROM LAMP IS NOT EXPECTED.  IF
C           VECTOR LAMP DATA ARE BEING ANALYZED, THE FIRST GUESS
C           WOULD LIKELY BE FROM MOS GIRDDED DATA.
C                   
C        DATA SET USE
C            KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C               NDATE = THE DAY 1 DATE/TIME.  (INPUT)
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
C                              IN NGRIDC( ,L) DEFINING THE 
C                              CHARACTERISTICS OF THIS GRID.  USED
C                              TO DETERMINE WHETHER THE DATA ARE
C                              VECTOR (=0) OR NOT.
C                       L=11 --SET TO 7777.
C                       L=12 --USED IN ESTABLISHING WHAT DATA TO KEEP.
C              LITEMS = THE NUMBER OF ITEMS IN LSTORE( , ).
C         MSTORE(L,J) = THE ARRAY HOLDING THE VARIABLES NEEDED AS
C                       INPUT, AFTER DAY 1, AND ASSOCIATED INFORMATION 
C                       (L=1,7) (J=1,MITEMS).  (OUTPUT)
C                       L=1,4 --THE 4 ID'S FOR THE DATA.
C                       L=5   --INDICATES WHETHER OR NOT TO STORE THE
C                               VARIABLE; SET TO 7777.
C                       L=6   --THE CYCLE (HR) FOR WHICH THIS ITEM
C                               IS NEEDED.
C                       L=7   --NUMBER OF HOURS TO SAVE.
C                       NOTE THAT MSTORE IN U155 AND LMSTR4 IS NOT
C                       EXACTLY THAT IN OTHER PROGRAMS.
C              MITEMS = THE NUMBER OF ITEMS IN MSTORE( , ). 
C                       (INPUT/OUTPUT)
C                 ND9 = MAXIMUM NUMBER OF FIELDS STORED IN LSTORE( , )
C                       AND MSTORE( , ).  SECOND DIMENSION OF
C                       LSTORE( , ) AND MSTORE( , ).  (INPUT)
C              INCCYL = INCREMENT IN HOURS BETWEEN DATE/TIMES THAT
C                       ARE PUT INTO IDATE( ) BY SUBROUTINE DATPRO.
C                       USED IN LMSTR4 TO ASSURE THAT DATA ARE SAVED FOR
C                       CYCLES AT THIS FREQUENCY.  (INPUT)
C              NCEPNO = DD OF NCEP FORECASTS.  (INPUT)
C              MINVEC = THE MINIMUM NUMBER OF HOURS OF BACKUP DATA
C                       TO SAVE FOR VECTOR DATA.  (INPUT)
C              MINMOD = THE MINIMUM NUMBER OF HOURS OF BACKKUP DATA
C                       TO SAVE FOR GRIDPOINT DATA.  (INPUT)
C            IDATE(J) = THE LIST OF DATES FOR THE RUN.  (INPUT)
C              NDATES = THE NUMBER OF DATES IN IDATE( ).  (INPUT)
C               ISTOP = INCREASED BY WHEN WHEN AN ERROR OR POTENTIAL
C                       ERROR IS FOUND.  (INPUT/OUTPUT)
C                 IER = STATUS RETURN.  (OUTPUT)
C                        0 = GOOD RETURN.
C             JCYL(J) = WORK ARRAY.  COMPUTE ALL THE CYCLE TIMES NEEDED
C                       FOR VECTOR DATA (J=1,NOCYL).  (INTERNAL)
C             MCYL(J) = WORK ARRAY.  COMPUTE ALL THE CYCLE TIMES NEEDED
C                       FOR GRIDPOINT DATA (J=1,MOCYL).  (INTERNAL)
C               NOCYL = NUMBER OF VALUES IN JCYL( ).  (INTERNAL)
C               MOCYL = NUMBER OF VALUES IN MCYL( ).  (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES USED
C            NONE
C
      DIMENSION IDATE(NDATES)
      DIMENSION LSTORE(12,ND9),MSTORE(7,ND9)
      DIMENSION JCYL(24),MCYL(24)
C
      DATA JCYL/24*0/,
     1     MCYL/24*0/     
C
      IER=0
C
D     WRITE(KFILDO,100)((LSTORE(I,J),I=1,12),J=1,LITEMS)
D100  FORMAT(/' LSTORE STARTING LMSTR4'/('  ',3I10,I11,3I8,I12,3I8,I12))
C
C        COMPUTE ALL THE POSSIBLE CYCLE TIMES IN JCYL( ), MAXIMUM OF 24.
C        ONLY WHEN INCCYL NE 24 WILL NOCYL BE GT 1.  THIS IS USED
C        FOR VECTOR DATA, THE DATA TO ANALYZE.
C
      JCYLST=MOD(NDATE,100)
C        JCYLST = THE FIRST CYCLE TIME NEEDED FOR VECTOR DATA TO ANALYZE.
      JCYL(1)=JCYLST
      NOCYL=1
C
      DO 101 J=2,24
      JCYLX=MOD(JCYLST+(J-1)*INCCYL,24)
      IF(JCYLX.EQ.JCYLST)GO TO 102
C        IF THE COMPUTED CYCLE EQUALS THE FIRST ONE, IT IS IN
C        A REPEAT MODE.  GO OUT.
      NOCYL=NOCYL+1
      JCYL(NOCYL)=JCYLX
 101  CONTINUE
C
 102  CONTINUE
C        THIS CONTINUE PUT HERE SO THE PRINT BELOW CAN BE USED
C        AS COMPILED.
D     WRITE(KFILDO,1025)NOCYL,(JCYL(J),J=1,NOCYL)
D1025 FORMAT(/' IN LMSTR4--NOCYL,(JCYL(J),J=1,NOCYL)',25I3)
C
C        COMPUTE ALL THE POSSIBLE CYCLE TIMES FOR GRIDPOINT DATA IN
C        MCYL( ), MAXIMUM OF 6.  THIS IS USED FOR GRIDPOINT DATA,
C        THE DATA TO USE AS A FIRST GUESS.
C
      MCYLST=(MOD(NDATE,100)/6)*6
C        MCYLST = THE FIRST CYCLE TIME OF THE GRIDDED FIRST GUESS,
C        ASSUMING FIELDS CAN BE AVAILABLE EVERY 6 HOURS AND THE MOST
C        RECENT ONE FOR THE DATA TO ANALYZE (THE DATE IN NDATE) IS
C        USED FOR THE VECTOR DATA WITH DATE NDATE.
      MCYL(1)=MCYLST
      MOCYL=1
C
      DO 103 J=2,24
      MCYLX=MOD(MCYLST+(J-1)*INCCYL,24)
      IF(MOD(MCYLX,6).NE.0)GO TO 103
      IF(MCYLX.EQ.MCYLST)GO TO 104
C        IF THE COMPUTED CYCLE EQUALS THE FIRST ONE, IT IS IN
C        A REPEAT MODE.  GO OUT.
      MOCYL=MOCYL+1
      MCYL(MOCYL)=MCYLX
 103  CONTINUE
C
 104  CONTINUE
C        THIS CONTINUE PUT HERE SO THE PRINT BELOW CAN BE USED
C        AS COMPILED.
D     WRITE(KFILDO,1043)MOCYL,(MCYL(J),J=1,MOCYL)
D1043 FORMAT(/' IN LMSTR4--MOCYL,(MCYL(J),J=1,MOCYL)',25I3)
 
C        FOR EACH ENTRY IN LSTORE( , ) FOR WHCIH LSTORE(9, ) NE 0,
C        FOR VECTOR DATA:
C           IF ENTRY NOT ALREADY MADE, MAKE ENTRY FOR EACH CYCLE (HR)
C           IN OF NOCYL ENTRIES IN JCYL( ).
C        FOR GRID DATA:
C           IF ENTRY NOT ALREADY MADE, MAKE ENTRY FOR EACH 6-H
C           CYCLE TIME AND EACH PROJECTION (CHECK THIS)
C
      DO 200 L=1,LITEMS
C***      WRITE(KFILDO,105)L,LSTORE(1,L),LITEMS,MITEMS,NOCYL,NDATE,ND9,
C***     1                 INCCYL,NCEPNO
C*** 105  FORMAT(/' --L,LSTORE,LITEMS,MITEMS,NOCYL,NDATE,ND9,INCCYL,NCEPNO',
C***     1         9I11)
         IF(LSTORE(9,L).EQ.0)GO TO 200
C            WHEN LSTORE(9,L) = 0, DATA WERE NOT USED, SO DON'T STORE.
C    
C            DATA IN LSTORE( ,L) WERE USED.
C            MAKE ENTRIES IN MSTORE( , ) AS NECESSARY.
C
         IF(LSTORE(10,L).EQ.0)THEN
C              THIS IS VECTOR DATA.
C
            DO 115 M=1,MITEMS
C              THIS WILL NOT EXECUTE WHEN MITEMS = 0.

               IF(LSTORE(1,L).EQ.MSTORE(1,M).AND.
     1            LSTORE(2,L).EQ.MSTORE(2,M).AND.
     2            LSTORE(3,L).EQ.MSTORE(3,M).AND.
     3            LSTORE(4,L).EQ.MSTORE(4,M))THEN
C                    A MATCH MEANS IDS ARE ALREADY IN MSTORE( , ).
C                    WOULDN'T OCCUR UNLESS A DUPLICATE ID.
                  GO TO 200
               ENDIF
C
 115        CONTINUE 
C
C              DROP THROUGH HERE, EITHER BECAUSE OF NO TRIP THROUGH
C              THE LOOP OR BECAUSE OF A NON MATCH, MEANS ID MUST BE
C              INSERTED.  OBS AND MOS FORECASTS ARE TREATED DIFFERENTLY.
C
            IF(LSTORE(1,L)/100000000.EQ.7.OR.
     1         LSTORE(1,L)/100000000.EQ.2)THEN
C
C              THIS IS AN OBSERVATION OR A MOS FORECAST. IT SHOULD NOT
C              BE A DUPLICATE.  HOWEVER, AN ENTRY MUST BE MADE FOR
C              EACH CYCLE THAT MIGHT BE NEEDED.  (WHEN INCCYL = 24,
C              ONLY ONE WILL BE NEEDED.)
C
               DO 120 J=1,NOCYL
C
                  IF(MITEMS.GE.ND9)THEN
                     WRITE(KFILDO,117)ND9,(LSTORE(I,L),I=1,4)
 117                 FORMAT(/' ****MITEMS ABOUT TO OVERFLOW ND9 =',I7,
     1                       ' IN LMSTR4 FOR LSTORE( , ) = ',4I12,'.'/
     2                       '     NO MORE ITEMS CAN BE ADDED',
     3                       ' TO MSTORE( , ).  PROCEEDING.')
                     ISTOP=ISTOP+1
                     GO TO 200
                  ENDIF                     
C
                  MITEMS=MITEMS+1
                  MSTORE(1,MITEMS)=LSTORE(1,L)
                  MSTORE(2,MITEMS)=LSTORE(2,L)
                  MSTORE(3,MITEMS)=LSTORE(3,L)
                  MSTORE(4,MITEMS)=LSTORE(4,L)
                  MSTORE(5,MITEMS)=7777
                  MSTORE(6,MITEMS)=JCYL(J)
                  MSTORE(7,MITEMS)=MINVEC
                  CALL UPDAT(LSTORE(8,L),MAX(MINVEC-INCCYL,0),
     1                                              LSTORE(12,L))
C                    MINVEC IS THE NUMBER OF HOURS OF DATA TO 
C                    KEEP.  DATA NEED BE KEPT ONLY MINVEC-INCCYL
C                    HOURS; THIS ASSUMES VECTOR DATA ARE NOT NEEDED
C                    FOR BACKUP, AND IF MINVEC GT 0, THEN MORE
C                    THAN ONE HOUR OF DATA ARE NEEDED IN THE 
C                    ANALYSES.
 120           CONTINUE
C
            ELSE
                WRITE(KFILDO,127)(LSTORE(I,L),I=1,4)
 127            FORMAT(/' ****A VECTOR ENTRY IN LSTORE( , ) =',4I11,
     1                  ' FOUND IN LMSTR4 WITH CCC NE 7XX OR 2XX.'/
     2                  '     NOTHING IS DONE WITH IT.  PROCEEDING.')
                ISTOP=ISTOP+1
            ENDIF
                         
         ELSE
C              THIS IS GRIDDED DATA.  IT IS ASSUMED THIS IS FOR A
C              FIRST GUESS FROM THE NCEP MODEL DESIGNATED BY NCEPNO.
C              THE GUESS COULD BE A MODEL FIELD OR A GRIDDED MOS
C              FORECAST.  SINCE LSTORE( , ) ENTRIES ARE NOT TIED
C              TO WHAT THEY WERE USED FOR, THE DD IN ID(1) CAN'T
C              BE USED.
C
C              TO PROVIDE FOR FULL BACKUP OF NCEP MODEL FIELDS,
C              ALL CYCLES DIVISIBLE BY 6 UP TO MINMOD-INCCYL HOURS 
C              ARE INSERTED AND ALL TAUS OF THE ONE FOUND PLUS
C              MINMOD-INCCYL+2 HOURS AT 3-H INCREMENTS.
C
            IF(MOD(LSTORE(1,L),100).NE.NCEPNO)THEN
               WRITE(KFILDO,137)(LSTORE(I,L),I=1,4),NCEPNO
 137           FORMAT(/' ****GRIDDED ENTRY IN LSTORE(1, ) =',4I11,
     1                 ' FOUND WITH DD NE NCEP MODEL NUMBER NCEPNO =',
     2                  I4,'.'/
     3                 '     NOTHING IS DONE WITH IT.  PROCEEDING.')
               ISTOP=ISTOP+1
c
            ELSEIF(LSTORE(1,L)/100000000.EQ.0.OR.
     1             LSTORE(1,L)/100000000.EQ.2)THEN
C
C                 THIS IS A MODEL OR MOS GRID.  CYCLES DIVISIBLE BY
C                 6 AND TAUS OUT TO MINMOD-INCCYL+2 HOURS ARE KEPT FOR 
C                 BACKUP PURPOSES.
C            
               DO 140 J=1,MOCYL
                  ITAU=MOD(LSTORE(3,L),1000)
C  
                  DO 139 N=ITAU,ITAU+MINMOD+2,3 
C                       AN INCREMENT OF MINMOD-INCCYL+2 HOURS PROVIDES
C                       FOR A MINMOD-HOUR BACKUP.  THE EXTRA
C                       2 HOURS IS TO HANDLE THE SITUATION WHEN THE
C                       FIRST HOUR OF THE RUN IS DIVISIBLE BY 3 AND
C                       SUBSEQUENT ONES ARE NOT AND INTERPOLATION IS
C                       REQUIRED.  SINCE SEVERAL TAUS ARE BEING
C                       INSERTED FOR EACH ONE FOUND, THERE COULD BE
C                       DUPLICAATES PUT INTO MSTORE( , ), SO TEST
C                       FOR IT.
C
                     DO 138 K=1,MITEMS
C
                        IF(MSTORE(1,K).EQ.LSTORE(1,L).AND.
     1                     MSTORE(2,K).EQ.LSTORE(2,L).AND.
     2                     MSTORE(3,K).EQ.(LSTORE(3,L)/1000)*1000+N.AND.
     3                     MSTORE(4,K).EQ.LSTORE(4,L).AND.
     4                     MSTORE(6,K).EQ.MCYL(J))GO TO 139
C                             A MATCH MEANS IDS ARE ALREADY IN MSTORE( , ).
C
 138                 CONTINUE                                   
C  
C                       DROP THROUGH HERE MEANS THIS IS NOT A
C                       DUPLICATE.  INSERT INTO MSTORE( , ).
C
                     IF(MITEMS.GE.ND9)THEN
                        WRITE(KFILDO,117)ND9,(LSTORE(I,L),I=1,4)
                        ISTOP=ISTOP+1
                        GO TO 200
                     ENDIF                     
C
                     MITEMS=MITEMS+1
                     MSTORE(1,MITEMS)=LSTORE(1,L)
                     MSTORE(2,MITEMS)=LSTORE(2,L)
                     MSTORE(3,MITEMS)=(LSTORE(3,L)/1000)*1000+N
                     MSTORE(4,MITEMS)=LSTORE(4,L)
                     MSTORE(5,MITEMS)=7777
                     MSTORE(6,MITEMS)=MCYL(J)
                     MSTORE(7,MITEMS)=MINMOD
                     CALL UPDAT(LSTORE(8,L),MINMOD,LSTORE(12,L))
C
                     IF(LSTORE(12,L).LE.NDATE)THEN                     
                        CALL UPDAT(LSTORE(12,L),-MINMOD,LSTORE(12,L))                     
                     ENDIF
C
C                       ARRANGE TO KEEP DATA FOR MINMOD-INCCYL HOURS FOR
C                       BACKUP.  MUST LOOK FOR PROJECTIONS NOT
C                       NECESSARILY USED IN LSTORE( , ) IN ORDER TO
C                       SAVE THE DATA.
C
 1383                MINPRO=0
                     MAXPRO=0
C
                     DO 1384 LL=1,LITEMS
C
                     IF(LSTORE(1,LL).EQ.LSTORE(1,L).AND.
     1                  LSTORE(2,LL).EQ.LSTORE(2,L).AND.
     2                  LSTORE(3,LL)/1000.EQ.LSTORE(3,L)/1000.AND.
     3                  LSTORE(4,LL).EQ.LSTORE(4,L))THEN
C
                        MINPRO=MIN(MINPRO,MOD(LSTORE(3,L),1000))
                        MAXPRO=MAX(MAXPRO,MOD(LSTORE(3,L),1000))
C                          THIS FINDS THE MINIMUM AND MAXIMUM TAU USED
C                          FOR THIS ID, AND ARRANGES TO SAVE THE DATA.
C
                        IF(MOD(LSTORE(3,LL),1000).LE.
     1                     MOD(LSTORE(3,L),1000)+MINMOD)THEN
                           CALL UPDAT(LSTORE(8,LL),MINMOD,LSTORE(12,LL))
C
                           IF(LSTORE(12,LL).LE.NDATE)THEN                     
                              CALL UPDAT(LSTORE(12,LL),-MINMOD,
     1                                                  LSTORE(12,LL))                     
                           ENDIF
C
                        ENDIF
C
                     ENDIF
C
 1384                CONTINUE
C    
                     DO 1387 NN=MINPRO,MAXPRO,3                       
C
C                       CHECK FOR DUPLICATES.
C
                     DO 1386 JJ=1,MITEMS
C
                     IF(MSTORE(1,JJ).EQ.LSTORE(1,L).AND.
     1                  MSTORE(2,JJ).EQ.LSTORE(2,L).AND.
     2                  MSTORE(3,JJ).EQ.(LSTORE(3,L)/1000)*1000+NN.AND.
     3                  MSTORE(4,JJ).EQ.LSTORE(4,L).AND.
     4                  MSTORE(6,JJ).EQ.MCYL(J).AND.
     5                  MSTORE(7,JJ).EQ.MINMOD)THEN
                        GO TO 1387
                     ENDIF
C
 1386                CONTINUE
C
                     IF(MITEMS.GE.ND9)THEN
                        WRITE(KFILDO,117)ND9,(LSTORE(I,L),I=1,4)
                        ISTOP=ISTOP+1
                        GO TO 200
                     ENDIF
C
                     MITEMS=MITEMS+1
                     MSTORE(1,MITEMS)=LSTORE(1,L)
                     MSTORE(2,MITEMS)=LSTORE(2,L)
                     MSTORE(3,MITEMS)=(LSTORE(3,L)/1000)*1000+NN
                     MSTORE(4,MITEMS)=LSTORE(4,L)
                     MSTORE(5,MITEMS)=7777
                     MSTORE(6,MITEMS)=MCYL(J)
                     MSTORE(7,MITEMS)=MINMOD
 1387                CONTINUE
C
 139              CONTINUE
C
 140           CONTINUE
C
            ELSE
                WRITE(KFILDO,157)(LSTORE(I,L),I=1,4)
 157            FORMAT(/' ****A GRIDPOINT ENTRY IN LSTORE( , ) =',4I11,
     1                  ' FOUND IN LMSTR4 WITH CCC NE 7XX OR 2XX.'/
     2                  '     NOTHING IS DONE WITH IT.  PROCEEDING.')
                ISTOP=ISTOP+1
            ENDIF
C
         ENDIF
C
 200  CONTINUE            
C
C        CHECK TO MAKE SURE DATES AGREE WITH IDATE(1) AND INCCYL.
C
      DO 210 M=1,NDATES
C
      DO 208 J=1,NOCYL
      IF(MOD(IDATE(M),100).EQ.JCYL(J))GO TO 210
 208  CONTINUE
C        DROP THROUGH HERE MEANS THERE IS A DATE PROBLEM.
C
      WRITE(KFILDO,209)IDATE(M)
 209  FORMAT(/' ****DATE',I12,' DOES NOT AGREE WITH THE FIRST DATE',
     1        ' IN THE DATE LIST AND INCCYL.  DATA MAY NOT BE',
     2        ' SAVED IN INTERNAL STORAGE.')
      ISTOP=ISTOP+1
 210  CONTINUE

C        ARRANGE TO TOSS DATA BY GCPAC NOT NEEDED.
C
      DO 215 L=1,LITEMS
      IF(LSTORE(12,L).LE.LSTORE(8,L))LSTORE(1,L)=0
 215  CONTINUE
C
D     WRITE(KFILDO,220)((LSTORE(I,L),I=1,12),L=1,LITEMS)
D220  FORMAT(/' LSTORE ENDING LMSTR4'/('  ',3I10,I11,3I8,I12,3I8,I12))
      RETURN
      END
