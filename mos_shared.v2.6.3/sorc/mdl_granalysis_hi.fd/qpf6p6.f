      SUBROUTINE QPF6P6(KFILDO,KFIL10,P,FD4,FD5,NX,NY,ND2X3,TRUNC,
     1                  ID,LSTORE,ND9,LITEMS,NWORDS,NDATE,
     2                  IS0,IS1,IS2,IS4,ND7,
     3                  IPACK,IWORK,ND5,
     4                  CORE,ND10,NBLOCK,NFETCH,L3264B,ISTOP,IER)
C
C        AUGUST    2008   GLAHN   MDL   MOS-2000
C                                 ADAPTED FROM CKPOP
C        OCTOBER   2009   GAW     CHANGED CCCFFF OF 12-H QPF FROM
C                                 223370 TO 223380, FOR A CONSTRAINED
C                                 12-H QPF CALCULATED FROM TWO 6-H
C                                 QPF VALUES.
C 
C        PURPOSE 
C            TO COMPUTE A 12-H QPF GRID FROM TWO 6-H QPF GRIDS.
C            THE 6-H GRIDS SHOULD NOT CONTAIN NEGATIVE VALUES.  
C            HOWEVER, AS WRITTEN TO INTERNAL STORAGE, THEY HAVE
C            NOT HAD SMALL VALUES SET TO ZERO. SO THAT HAS TO 
C            BE DONE.
C
C            CALLED WITH CCCFFF = 223380 FOR 12-H QPF.  
C            ASSUMES     CCCFFF = 223270 FOR 6-H QPF.
C
C        DATA SET USE 
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C           KFIL10 - UNIT NUMBER FOR RANDOM FILE ACCESS.  (INPUT)
C
C        VARIABLES 
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C              KFIL10 = THE UNIT NUMBER FOR RANDOM FILE ACCESS.  (INPUT)
C              P(IXY) = THE 12-H QPF GRID IS RETURNED (IXY=1,NX*NY).
C                       (OUTPUT)
C            FD4(IXY) = WORK ARRAY (IXY=1,NX*NY) FOR THE ON-TIME
C                       6-H GRID.  (INTERNAL)
C            FD5(IXY) = WORK ARRAY (IXY=1,NX*NY) FOR THE PREVIOUS
C                       6-H GRID.  (INTERNAL)
C                  NX = X-EXTENT OF THE GRIDS IN P( ), FD4( ),
C                       AND FD5( ). (INPUT)
C                  NY = Y-EXTENT OF THE GRIDS IN P( ), FD4( ),
C                       AND FD5( ). (INPUT)
C                       ALL ARRAYS ARE TREATED AS SINGLE DIMENSION
C                       BECAUSE THEY ARE ALL OF THE SAME SIZE.
C               ND2X3 = THE SIZE OF THE ARRAYS P( ) FD4( ) AND
C                       FD5( ).  (INPUT)
C               TRUNC = THE TRUNCATION VALUE, PROBABLY = .999.
C                       (INPUT)
C               ID(J) = THE 4-WORD ID OF THE 12-H QPF GRID
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
C        1         2         3         4         5         6         7 X
C                         
C        NON SYSTEM SUBROUTINES CALLED 
C           NONE
C
      DIMENSION ID(4)
      DIMENSION P(NX*NY),FD4(NX*NY),FD5(NX*NY)
      DIMENSION IPACK(ND5),IWORK(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION LD(4),ISTOP(6)
C
      IER=0
C
C        CHECK THE CCCFFF = 223330.
C
CD     WRITE(KFILDO,103)(ID(J),J=1,4),NX,NY,TRUNC
CD103  FORMAT(/' AT 103 IN QPF6P6--(ID(J),J=1,4),NX,NY,TRUNC',6I10,F8.4)
C
      IF(ID(1)/1000.NE.223380)THEN
         WRITE(KFILDO,105)(ID(J),J=1,4)
 105     FORMAT(/' ****CCCFFF NOT CORRECT IN QPF6P6, ID = ',
     1            3I10.9,I10.3,
     2           '.  COMPUTING 12-H QPF GRID ABORTED.')
         GO TO 800
      ENDIF
C
C        GET THE 6-H QPF ANALYSIS WITH THE SAME TAU.
C
      LD(1)=ID(1)-110000
      LD(1)=LD(1)+2000
C        THIS MAKES THE FFF OF 380 = 272 FOR 6-H QPF.
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
  130    FORMAT(/,' ****FIRST 6-H QPF NOT RETRIEVED BY GFETCH IN',
     1            ' QPF6P6',2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,/
     2            '     12-H QPF GRID CANNOT BE COMPUTED.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 800
      ENDIF
C
      IF(NWORDS.NE.NX*NY)THEN
         WRITE(KFILDO,131)NWORDS,NX*NY
 131     FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1            ' NOT EQUAL TO NX*NY =',I6,' IN QPF6P6.'/
     2            '     12-H QPF GRID CANNOT BE COMPUTED.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 800
      ENDIF
C
C        GET THE 6-H QPF ANALYSIS WITH THE PREVIOUS 6-H TAU.
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
  140    FORMAT(/,' ****SECOND 6-H QPF NOT RETRIEVED BY GFETCH IN',
     1            ' QPF6P6',2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,/
     2            '     12-H QPF GRID CANNOT BE COMPUTED.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 800
      ENDIF
C
      IF(NWORDS.NE.NX*NY)THEN
         WRITE(KFILDO,141)NWORDS,NX*NY
 141     FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1            ' NOT EQUAL TO NX*NY =',I6,' IN QPF6P6.'/
     2            '     12-H QPF GRID CANNOT BE COMPUTED.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 800
      ENDIF
C
C        COMPUTE 12-H GRID.
C
      DO 150 IXY=1,NX*NY
C
      IF(FD4(IXY).GT.9998.9.OR.FD5(IXY).GT.9998.9)THEN
         P(IXY)=9999.
      ELSE
CCCCCC         IF(FD4(IXY).LE.TRUNC)FD4(IXY)=0.
CCCCCC         IF(FD5(IXY).LE.TRUNC)FD5(IXY)=0.
CCCCC           THE ARCHIVED 6-H QPFS WERE TRUNCATED BUT THE GRIDS
CCCCC           WRITTEN TO INTERNAL STORAGE WERE NOT.  THIS SHOULD
CCCCC           MAKE THE 12-H QPF THE EXACT SUM OF THE TWO 6-H,
CCCCC           PROVIDED TRUNC IS THE SAME.
         P(IXY)=FD4(IXY)+FD5(IXY)
      ENDIF
C
 150  CONTINUE
C
 800  CONTINUE
      RETURN
      END
