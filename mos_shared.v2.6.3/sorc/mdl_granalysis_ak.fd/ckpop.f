      SUBROUTINE CKPOP(KFILDO,KFIL10,P,FD4,FD5,FD6,NX,NY,ND2X3,
     1                 ID,LSTORE,ND9,LITEMS,NWORDS,NDATE,
     2                 IS0,IS1,IS2,IS4,ND7,
     3                 IPACK,IWORK,ND5,
     4                 CORE,ND10,NBLOCK,NFETCH,L3264B,ISTOP,IER)
C
C        JUNE      2008   GLAHN   MDL   MOS-2000
C                                 ADAPTED FROM CKTDP
C        SEPTEMBER 2008   GLAHN   ELIMINATED NEGATIVES BEFORE CHECKING
C        SEPTEMBER 2008   GLAHN   ADDED FD6( ) FOR CHANGE GRID
C        JULY      2009   GLAHN   ADDED PERCENTAGE CHANGE DIAGNOSTIC
C 
C        PURPOSE 
C            TO CHECK A 12-H POP ANALYSIS IN P( ) WITH TWO
C            6-H POP ANALYSES AND MAKE TWO CHECKS:
C              1)  IF EITHER OF THE 6-H POPS EXCEED THE 12-H POP,
C                  SET THE 12-H POP TO THE LARGER OF THE 6-H.
C              2)  IF THE 12-H POP EXCEEDS THE SUM OF THE TWO 6-H POPS,
C                  SET THE 12-H TO THE SUM OF THE 6-H.
C            NOTE THIS DOES NOT CHANGE THE 6-H VALUES.  THE 6-H
C            POP ANALYSES ARE IN INTERNAL STORAGE.  THE ONE WITH THE
C            SAME TAU AS THE 12-H IS READ ITO FD4( ) AND THE ONE WITH
C            A TAU 6 HOURS PREVIOUS IS READ INTO FD5( ).
C
C            CALLED WITH CCCFFF = 223330 12-H POP.  
C            ASSUMES 6-H CCCFFF = 223220 FOR 6-H POP.
C
C        DATA SET USE 
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C           KFIL10 - UNIT NUMBR FOR RANDOM FILE ACCESS.  (INPUT)
C
C        VARIABLES 
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C              KFIL10 = THE UNIT NUMBR FOR RANDOM FILE ACCESS.  (INPUT)
C              P(IXY) = THE 12-H POP ANALYSIS TO CHECK (IXY=1,NX*NY).
C                       (INPUT/OUTPUT)
C            FD4(IXY) = WORK ARRAY (IXY=1,NX*NY).  (INTERNAL)
C            FD5(IXY) = WORK ARRAY (IXY=1,NX*NY).  (INTERNAL)
C            FD6(IXY) = HOLDS THE CHANGES MADE TO P( ) SCALED *10
C                       (IXY=1,NX*NY).  (OUTPUT)
C                  NX = X-EXTENT OF THE GRIDS IN P( ), FD4( ),
C                       FD5( ), AND FD6( ). (INPUT)
C                  NY = Y-EXTENT OF THE GRIDS IN P( ), FD4( ),
C                       FD5( ), AND FD6( ). (INPUT)
C                       ALL ARRAYS ARE TREATED AS SIGLE DIMENSION
C                       BECAUSE THEY ARE ALL OF THE SAME SIZE.
C               ND2X3 = THE SIZE OF THE ARRAYS P( ) FD4( ) AND
C                       FD5( ).  (INPUT)
C               ID(J) = THE 4-WORD ID OF THE 12-H POP GRID
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
C              ICOUNT = COUNTS THE TIMES THE 12-H POP WAS INCREASED.
C                       (INTERNAL)
C              JCOUNT = COUNTS THE TIMES THE 12-H POP WAS DECREASED.
C                       (INTERNAL)
C              NCOUNT = COUNTS THE NUMBER OF GRIDPOINTS WITH DATA.
C                       (INTERNAL)
C               FRACT = PERCENTAGE OF 12-H POPS WITH DATA CHANGED.
C                       (INTERNAL)
C        1         2         3         4         5         6         7 X
C                         
C        NON SYSTEM SUBROUTINES CALLED 
C           NONE
C
      DIMENSION ID(4)
      DIMENSION P(NX*NY),FD4(NX*NY),FD5(NX*NY),FD6(NX*NY)
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
      JCOUNT=0
C
C        CHECK THE CCCFFF = 223330.
C
      IF(ID(1)/1000.NE.223330)THEN
         WRITE(KFILDO,105)(ID(J),J=1,4)
 105     FORMAT(/' ****CCCFFF NOT CORRECT IN CKPOP, ID = ',3I10.9,I10.3,
     1           '.  CHECKING OF 12-H POP GRID ABORTED.')
         GO TO 800
      ENDIF
C
C        GET THE 6-H POP ANALYSIS WITH THE SAME TAU.
C
      LD(1)=ID(1)-110000
C        THIS MAKES THE FFF OF 330 = 220 FOR 6-H POP.
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
  130    FORMAT(/,' ****FIRST 6-H POP NOT RETRIEVED BY GFETCH IN',
     1            ' CKPOP',2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,/
     2            '     12-H POP GRID CANNOT BE CHECKED.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 800
      ENDIF
C
      IF(NWORDS.NE.NX*NY)THEN
         WRITE(KFILDO,131)NWORDS,NX*NY
 131     FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1            ' NOT EQUAL TO NX*NY =',I6,' IN CKPOP.'/
     2            '     12-H POP GRID CANNOT BE CHECKED.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 800
      ENDIF
C
C        GET THE 6-H POP ANALYSIS WITH THE PREVIOUS 6-H TAU.
C
      LD(3)=ID(3)-6
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD5,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSOURC,MISSP,MISSS,L3264B,ITIME,
     4            IER)
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,140)(LD(J),J=1,4)
  140    FORMAT(/,' ****SECOND 6-H POP NOT RETRIEVED BY GFETCH IN',
     1            ' CKPOP',2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,/
     2            '     12-H POP GRID CANNOT BE CHECKED.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 800
      ENDIF
C
      IF(NWORDS.NE.NX*NY)THEN
         WRITE(KFILDO,141)NWORDS,NX*NY
 141     FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1            ' NOT EQUAL TO NX*NY =',I6,' IN CKPOP.'/
     2            '     12-H POP GRID CANNOT BE CHECKED.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 800
      ENDIF
C
C        CHECK 12-H GRID.
C
      NOUNT=0
C        NCOUNT COUNTS THE NUMBER OF GRIDPOINTS WITH DATA.
C
      DO 150 IXY=1,NX*NY
      FD6(IXY)=9999.
C
      IF(P(IXY).GT.9998.9.OR.
     1   FD4(IXY).GT.9998.9.OR.
     2   FD5(IXY).GT.9998.9)GO TO 150
C
C        ELIMINATE NEGATIVES BEFORE CHECKING.  
C
      NCOUNT=NCOUNT+1
      FD4A=MAX(FD4(IXY),0.)
      FD5A=MAX(FD5(IXY),0.)
      POPMAX=MAX(FD4A,FD5A)
C
      IF(POPMAX.GT.P(IXY))THEN
         FD6(IXY)=(POPMAX-P(IXY))*10.
C
CD        IF(ICOUNT.LT.100)THEN
CD           WRITE(KFILDO,145)IXY,FD4A,FD5A,POPMAX,P(IXY),FD6(IXY)
CD145        FORMAT(' IN CKPOP AT 145, INCREASE--IXY,FD4A,FD5A,POPMAX,',
CD    1             'P(IXY),FD6(IXY)',I8,5F10.4)
CD        ENDIF
C
         P(IXY)=POPMAX
C           THE 12-H POP IS INCREASED TO THE MAX OF THE 6-H GRIDS.
         ICOUNT=ICOUNT+1
      ELSEIF(P(IXY).GT.FD4A+FD5A)THEN
         FD6(IXY)=(FD4A+FD5A-P(IXY))*10.
C
CD        IF(JCOUNT.LT.100)THEN
CD           WRITE(KFILDO,146)IXY,FD4A,FD5A,P(IXY)
CD146        FORMAT(' IN CKPOP AT 146, DECREASE--IXY,FD4A,FD5A,P(IXY)',
CD    1             I8,3F10.4)
CD        ENDIF
C
         P(IXY)=FD4A+FD5A
C           THE 12-H POP IS DECREASED TO THE SUM OF THE 6-H GRIDS.
         JCOUNT=JCOUNT+1
      ELSE
         FD6(IXY)=0.
C           WHEN THERE IS NO CHANGE TO P( ), FD6( ) = 0.
      ENDIF
C
 150  CONTINUE
C
      FRACT=(REAL(ICOUNT+JCOUNT)/NCOUNT)*100.
      WRITE(KFILDO,155)ICOUNT,JCOUNT
 155  FORMAT(/' THE 12-H POP GRID WAS INCREASED',I8,' TIMES,',
     1        ' AND DECREASED',I8,' TIMES BASED THE TWO 6-H POPS.')
      WRITE(KFILDO,156)FRACT,NCOUNT
 156  FORMAT(' THIS IS ',F5.1,' PERCENT OF THE ',I8,
     1       ' GRIDPOINTS WITH DATA.')
C
 800  CONTINUE
      RETURN
      END
