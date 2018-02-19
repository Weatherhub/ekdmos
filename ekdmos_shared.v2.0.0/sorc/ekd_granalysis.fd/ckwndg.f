      SUBROUTINE CKWNDG(KFILDO,KFIL10,P,FD4,FD6,NX,NY,ND2X3,
     1                  ID,LSTORE,ND9,LITEMS,NWORDS,NDATE,
     2                  IS0,IS1,IS2,IS4,ND7,
     3                  IPACK,IWORK,ND5,
     4                  CORE,ND10,NBLOCK,NFETCH,L3264B,ISTOP,IER)
C
C        MAY       2008   GLAHN   MDL   MOS-2000
C        SEPTEMBER 2008   GLAHN   ADDED FD6( ); MODIFIED TO SINGLE
C                                 DIMENSIONED VARIABLES.
C        OCTOBER   2008   GLAHN   CORRECTED ICOUNT
C 
C        PURPOSE 
C            TO CHECK A 10-M WIND GUST IN P( , ) WITH A 10-M WIND
C            SPEED ANALYSIS AND IF THE GUST IS LESS THAN THE SPEED,
C            THE GUST GRIDPOINT IS SET EQUAL TO THE SPEED.  THE WIND
C            GUST ANALYSIS IS REALLY THE SUM OF THE SPEED AND GUST,
C            SO THE GUST SHOULD ALWAYS BE GE TO THE SPEED.  THE WIND
C            ANALYSIS IS IN INTERNAL STORAGE AND IS READ INTO FD4( , ).
C            THE GUST ID IS IN ID( ).  ASSUME THE SPEED IS SAME, EXCEPT
C            THE CCC = 360 RATHER THAN THE INCOMING 385.
C
C            A RETURNED GRID IN FD6( , ) WILL CONTIAN THE CORRECTIONS
C            MADE IN KT.  THESE WILL BE POSITIVE.
C
C            CALLED WITH CCC = 385 GUST.  ASSUMES SPEED CCC = 360.
C
C        DATA SET USE 
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C           KFIL10 - UNIT NUMBR FOR RANDOM FILE ACCESS.  (INPUT)
C
C        VARIABLES 
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C              KFIL10 = THE UNIT NUMBR FOR RANDOM FILE ACCESS.  (INPUT)
C              P(IXY) = THE GUST ANALYSIS TO CHECK (IXY=1,NX*NY).
C                       (INPUT/OUTPUT)
C            FD4(IXY) = WORK ARRAY (IXY=1,NX*NY).  (INTERNAL)
C                  NX = X-EXTENT OF P( , ) AND FD4( , ). (INPUT)
C                  NY = Y-EXTENT OF P( , ) AND FD4( , ). (INPUT)
C               ND2X3 = THE SIZE OF THE ARRAYS IN P( , ) AND
C                       FD4( , ).  (INPUT)
C               ID(J) = THE 4-WORD ID OF THE VARIABLE DEWPOINT GRID
C                       (J=1,4).  (INPUT)
C         LSTORE(L,J) = THE ARRAY TO HOLD INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS).  THIS IS
C                       INITIALIZED TO ZERO AS NEEDED ON FIRST ENTRY 
C                       ONLY.  (INPUT-OUTPUT)
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
C                              COUNTED IN GFETCH.
C                       L=10 --FOR U201, NSLAB, THE NUMBER OF THE SLAB 
C                              IN DIR( , ,L) AND IN NGRIDC( ,L) DEFINING
C                              THE CHARACTERISTICS OF THIS GRID.
C                              FOR OTHER ROUTINES NOT REQUIRING GRID
C                              DEFINITIONS, THIS NUMBER MAY MEAN
C                              SOMETHING ELSE.  FOR INSTANCE, IN U600 IT
C                              IS THE "MODEL NUMBER" OR SOURCE OF THE
C                              DATA STORED.
C                       L=11 --VARIOUS USES, DEPENDING ON PROGRAM.
C                       L=12 --USED INITIALLY IN ESTABLISHING
C                              MSTORE( , ).  LATER USED AS A WAY OF
C                              DETERMINING WHETHER TO KEEP THIS
C                              VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT ARE FILLED.  (INPUT)  
C              NWORDS = NUMBER OF WORDS RETURNED FROM GFETCH.
C                       (INTERNAL)
C               NDATE = DATE/TIME OF THE DATA PROCESSED IN FORMAT
C                       YYYYMMDDHH.  THIS IS STORED IN LSTORE(8, ).  
C                       (INPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,4).
C                       (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,21+).
C                       (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).
C                       (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND
C                       IS4( ).  (INPUT)
C            IPACK(J) = WORK ARRAY FOR GFETCH (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY FOR GFETCH (J=1,ND5).  (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ) AND WORK( ).  (INPUT)
C             CORE(J) = THE LINEAR ARRAY WHERE THE DATA ARE TO BE
C                       STORED, WHEN SPACE IS AVAILABLE (J=1,ND10).
C                       (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE RANDOM DISK FILE.
C                       (INPUT)
C              NFETCH = A COUNT OF THE NUMBER OF TIMES GFETCH IS
C                       ENTERED.  IT IS A RUNNING COUNT FROM THE
C                       BEGINNING OF THE PROGRAM.  (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C            ISTOP(J) = ISTOP(1)--IS INCREMENTED BY 1 EACH TIME AN ERROR 
C                                 OCCURS.
C                       ISTOP(2)--IS INCREMENTED WHEN THERE ARE
C                                 FEW DATA (200) FOR AN ANALYSIS.
C                       ISTOP(3)--IS INCREMENTED WHEN A DATA RECORD 
C                                 COULD NOT BE FOUND.
C                       ISTOP(4)--IS INCREMENTED WHEN A LAPSE RATE COULD
C                                 NOT BE COMPUTED OR HAS TOO FEW CASES
C                                 TO BE USED.
C                       ISTOP(5)--IS INCREMENTED WHEN NO NON-MISSING
C                                 GRIDPOINT AROUND THE DATA POINT IS
C                                 OF THE SAME TYPE.
C                       ISTOP(6)--IS INCREMENTED WHEN THERE IS A PROBLEM
C                                 WITH MAKING BOGUS STATIONS.
C                       (INPUT/OUTPUT)
C                 IER = STATUS RETURN.  (OUTPUT)
C                        0 = GOOD RETURN.
C                       OTHER VALUES FROM GFETCH.
C               LD(J) = THE VARIABLE IDS TO RETRIEVE BY GFETCH (J=1,4).
C                       (INTERNAL)
C               NPACK = RETURNED FROM GFETCH.  NOT NEEDED.  (INTERNAL)
C              NSOURC = RETURNED FROM GFETCH.  NOT NEEDED.  (INTERNAL)
C              NTIMES = RETURNED FROM GFETCH.  NOT NEEDED.  (INTERNAL)
C               MISSP = RETURNED FROM GFETCH.  NOT NEEDED.  (INTERNAL)
C               MISSS = RETURNED FROM GFETCH.  NOT NEEDED.  (INTERNAL)
C              ICOUNT = COUNTS TIMES GUST GRID WAS MODIFIED.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C                         
C        NON SYSTEM SUBROUTINES CALLED 
C           NONE
C
      DIMENSION ID(4)
      DIMENSION P(NX*NY),FD4(NX*NY),FD6(NX*NY)
      DIMENSION IPACK(ND5),IWORK(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION LD(4),ISTOP(6)
C
C        INITIALIZE AND INCREMENT AS NEEDED.
C
      IER=0
      ICOUNT=0
C
C        CHECK THE CCC = 385.
C
      IF((ID(1)-(ID(1)/1000000)*1000000)/1000.NE.385)THEN
         WRITE(KFILDO,105)(ID(J),J=1,4)
 105     FORMAT(/' ****CCC NOT CORRECT IN CKWNDG.  ID = ',3I10.9,I10.3,
     1           '.  CHECKING OF WIND GUST GRID ABORTED.')
         GO TO 800
      ENDIF
C
C        GET THE WIND SPEED ANALYSIS.
C
      LD(1)=ID(1)-385000+360000
C        THIS MAKES THE CCC OF 385 = 360 FOR WIND SPEED 
      LD(2)=ID(2)
      LD(3)=ID(3)
      LD(4)=ID(4)
      ITIME=0
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD4,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSOURC,MISSP,MISSS,L3264B,ITIME,
     4            IER)
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,130)(LD(J),J=1,4)
  130    FORMAT(/,' ****WIND SPEED NOT RETRIEVED BY GFETCH IN CKWNDG',
     1            2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,/
     2            '     GUST-WPEED GRIDS CANNOT BE CHECKED.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 800
      ENDIF
C
      IF(NWORDS.NE.NX*NY)THEN
         WRITE(KFILDO,131)NWORDS,NX*NY
 131     FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1            ' NOT EQUAL TO NX*NY =',I6,' IN CKWNDG.'/
     2            '     GUST-WPEED GRIDS CANNOT BE CHECKED.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 800
      ENDIF
C
C        CHECK GRIDS.  WHEN DEW POINT EXCEEDS TEMPERATURE, SET DEW TO
C        TEMPERATURE.
C
      DO 150 IXY=1,NX*NY
      FD6(IXY)=9999.
C
      IF(FD4(IXY).GT.P(IXY)+.045)THEN
C           THE SPEED AND GUST GRID WILL BE THE SAME IN MANY PLACES.
C           THE .01 ALLOWS FOR ROUNDOFF.  THAT IS, IF THEY ARE CLOSE
C           TO WITHIN .045 KT, THEY ARE CONSIDERED EQUAL.
         DIFF=FD4(IXY)-P(IXY)
         FD6(IXY)=DIFF
         P(IXY)=FD4(IXY)
         ICOUNT=ICOUNT+1
      ELSE
         FD6(IXY)=0.
      ENDIF
C
 150  CONTINUE
C
      WRITE(KFILDO,155)ICOUNT
 155  FORMAT(/' GUST GRID WAS MODIFIED BY WIND SPEED',I8,' TIMES.')
C
 800  CONTINUE
      RETURN
      END
