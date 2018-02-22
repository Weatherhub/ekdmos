      SUBROUTINE CKQPF(KFILDO,KFIL10,IP16,P,FD4,FD6,NX,NY,ND2X3,
     1                 TRUNC,ID,IPLAIN,PLAIN,
     2                 LSTORE,ND9,LITEMS,NWORDS,NDATE,
     3                 IS0,IS1,IS2,IS4,ND7,
     4                 IPACK,IWORK,ND5,
     5                 CORE,ND10,NBLOCK,NFETCH,L3264B,L3264W,
     6                 ISTOP,IER)
C
C        AUGUST    2008   GLAHN   MDL   MOS-2000
C                                 ADAPTED FROM CKTDP
C                                 THIS REPLACES DISQ06
C        SEPTEMBER 2008   GLAHN   ADDED FD6( ) FOR CHANGE GRID
C        JUNE      2009   GLAHN   ADDED IPLAIN( , ) AND L3254W TO CALL;
C                                 CHANGED PLAIN TO IPLAIN( , ) IN 160
C 
C        PURPOSE 
C            TO CHECK TWO QPF GRIDS OF THE SAME PROJECTION, ONE
C            ANALYZED WITH LAPSE CORRECTION AND ONE WITHOUT.
C            THE DESIRE IS TO LEAVE IN THE LAPSE EFFECT WHERE
C            THERE IS PRECIP, BUT REMOVE IT OVER SURROUNDING
C            AREAS.  THE TWO GRIDS ARE CHECKED POINT BY POINT;
C            THE FINAL IS THE ONE ANALYZED WITH LAPSE CORRECTION,
C            BUT WITH THE EXTREANOUS PRECIP PATCHES AROUND THE
C            MAIN AREAS REMOVED.  ANYWHERE THE ONE ANALYZED
C            WITHOUT THE LAPSE CORRECTION DOES NOT HAVE PRECIP, 
C            IT IS REMOVED IN THE ONE WITH THE LAPSE CORRECTION.
C
C            THE ONE WITH LAPSE IS IN P( , ); THE ONE WITHOUT
C            IS READ INTO FD4( , ).
C
C            FD6( ) IS SET ZERO UNLESS
C               (1) THE GRID WITH TERRAIN IS GT TRUNC, AND
C               (2) THE GRID WITHOUT TERRAIN IS LT TRUNC.
C
C            CALLED WITH CCCFFF = 223370 (12-H) OR 223270 (6-H).  
C
C        DATA SET USE 
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C           KFIL10 - UNIT NUMBR FOR RANDOM FILE ACCESS.  (INPUT)
C            IP16     - UNIT NUMBER FOR INDICATING WHEN A RECORD IS
C                       WRITTEN TO THE SEQUENTIAL OR RANCOM ACCESS
C                       FILE.  (OUTPUT)
C
C        VARIABLES 
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C              KFIL10 = THE UNIT NUMBR FOR RANDOM FILE ACCESS.  (INPUT)
C                IP16 = INDICATES WHETHER (>0) OR NOT (=0) 
C                       A STATEMENT WILL BE OUTPUT TO IP16
C                       WHEN A RANDOM ACCESS FILE IS WRITTEN.  (INPUT)
C              P(IXY) = THE QPF ANALYSIS TO CHECK (IXY=1,NX*NY).
C                       (INPUT/OUTPUT)
C            FD4(IXY) = WORK ARRAY (IXY=1,NX*NY).  THE ANALYSIS 
C                       WITHOUT LAPSE CORRECTION IS READ INTO FD4( ).
C                       (INTERNAL)
C            FD6(IXY) = HOLDS THE CHANGES MADE TO P( ) SCALED *10
C                       (IXY=1,NX*NY).  (OUTPUT)
C                  NX = X-EXTENT OF THE GRIDS IN P( ), FD4( ) AND
C                       FD6( ).  (INPUT)
C                  NY = Y-EXTENT OF THE GRIDS IN P( ), FD4( ) AND
C                       FD6( ).  (INPUT)
C                       THE ARRAYS ARE TREATED AS SIGLE DIMENSION
C                       FOR EFFICIENCY; THEY CAN BE BECAUSE THEY ARE
C                       OF THE SAME SIZE.
C               ND2X3 = THE SIZE OF THE ARRAYS P( ) AND FD4( ).
C                       (INPUT)
C               TRUNC = THE TRUNCATION VALUE, PROBABLY = .999.
C                       (INPUT)
C               ID(J) = THE 4-WORD ID OF THE QPF GRID (J=1,4).  (INPUT)
C         IPLAIN(L,J) = 32 CHARACTERS (L=1,L3264W) (J=1,4) OF PLAIN
C                       LANGUAGE DESCRIPTION OF VARIABLES.
C                       NOTE THAT THIS REQUIRES TWO 32-BIT WORDS TO HOLD
C                       THE DESCRIPTION BUT ONLY ONE 64-BIT WORD.
C                       EQUIVALENCED TO PLAIN( ) IN DRU155.  (INPUT)
C               PLAIN = THE PLAIN LANGUAGE DESCRIPTION OF THE VARIABLE
C                       IN ID( ).  EQUIVALENCED TO IPLAIN( , ) IN
C                       DRU155.  (CHARACTER*32)  (INPUT)
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
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).
C                       (INPUT)
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
C              ICOUNT = COUNTS THE TIMES THE QPF GRID WAS SET TO ZERO.
C        1         2         3         4         5         6         7 X
C                         
C        NON SYSTEM SUBROUTINES CALLED 
C           NONE
C
C     CHARACTER*32 PLAIN,RACK
C
      DIMENSION ID(4)
      DIMENSION P(NX*NY),FD4(NX*NY),FD6(NX*NY)
      DIMENSION IPLAIN(L3264W,4)
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
C        CHECK THE CCCFFF = 223370 OR 223270.
C
      IF(ID(1)/1000.NE.223370.AND.
     1   ID(1)/1000.NE.223270)THEN
         WRITE(KFILDO,105)(ID(J),J=1,4)
 105     FORMAT(/' ****CCCFFF NOT CORRECT IN CKQPF, ID = ',3I10.9,I10.3,
     1           '.  CHECKING OF QPF GRID ABORTED.')
         GO TO 800
      ENDIF
C
C        GET THE GRID WITHOUT THE LAPSE CORRECTION. 
C
      LD(1)=ID(1)+1000
C        THIS MAKES THE FFF OF X70 = X71 FOR THE GRID WITHOUT
C        LAPSE CORRECTION
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
  130    FORMAT(/,' ****QPF GRID NOT RETRIEVED BY GFETCH IN',
     1            ' CKQPF',2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,/
     2            '     GRID CANNOT BE CHECKED.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 800
      ENDIF
C
      IF(NWORDS.NE.NX*NY)THEN
         WRITE(KFILDO,131)NWORDS,NX*NY
 131     FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1            ' NOT EQUAL TO NX*NY =',I6,' IN CKQPF.'/
     2            '     GRID CANNOT BE CHECKED.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 800
      ENDIF
C
C        CHECK THE GRID.
C
      DO 150 IXY=1,NX*NY
      FD6(IXY)=9999.
C
      IF(P(IXY).GT.9998.9.OR.
     1   FD4(IXY).GT.9998.9)GO TO 150
C
      IF(P(IXY).GT.TRUNC)THEN
C
         IF(FD4(IXY).LT.TRUNC)THEN
            FD6(IXY)=P(IXY)
            P(IXY)=0.
            NCOUNT=NCOUNT+1
         ELSE
            FD6(IXY)=0.
         ENDIF
C
      ELSE
         P(IXY)=0.
         FD6(IXY)=0.
      ENDIF
C
 150  CONTINUE
C
C        WRITE THE CORRECTED GRID TO INTERNAL STORAGE FOR 
C        ROUTINE QPF6P6 WHEN THIS IS 6-H QPF.
C
      IF(ID(1)/1000.EQ.223270)THEN
C
         LD(1)=ID(1)+2000
C           THE CORRECTED 6-H GRID WILL HAVE CCCFFF = 223272.
         LD(2)=ID(2)
         LD(3)=ID(3)
         LD(4)=ID(4)
         CALL GSTORE(KFILDO,KFIL10,LD,0,LSTORE,ND9,LITEMS,
     1               P,NX*NY,1,0,NDATE,
     2               CORE,ND10,LASTL,NBLOCK,LASTD,NSTORE,L3264B,IER)
C           "NSLAB" IS STORED AS ZERO SIGNIFYING THE DATA ARE NOT
C            PACKED AND CAN BE TREATED AS VECTOR DATA.
C
         IF(IER.EQ.0)THEN
C
            IF(IP16.NE.0)THEN
               WRITE(IP16,160)(LD(JJ),JJ=1,4),
     1                        ((IPLAIN(I,JJ),I=1,L3264W),JJ=1,4),NDATE
 160           FORMAT(/' WRITING DATA TO UNIT KFIL10',3I10.9,I10.3,3X,
     1                  8A4,' FOR DATE',I12)
            ENDIF
C
         ELSE 
            ISTOP(1)=ISTOP(1)+1
            WRITE(KFILDO,170)(LD(JJ),JJ=1,4)
 170        FORMAT('     ERROR WRITING VARIABLE',3(1X,I9.9),1X,I10.3,
     1             ' TO INTERNAL STORAGE.',/,
     2             '  SOME COMPUTATIONS (PRE- OR POST-PROCESSING)',
     3             ' MAY NOT BE ABLE TO BE MADE.  PROCEEDING.')
         ENDIF
C
      ENDIF
C
CD     WRITE(KFILDO,180)ICOUNT
CD180  FORMAT(/' THE QPF GRID HAD',I8,' POINTS WITH GT TRUNC AMOUNT',
CD    1        ' REMOVED THAT HAD LT TRUNC WITHOUT TERRAIN CORRECTION.')
C
 800  CONTINUE
      RETURN
      END
