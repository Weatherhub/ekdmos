      SUBROUTINE CONVPR(KFILDO,KFIL10,ID,IDPARS,
     2                  P,NX,NY,
     3                  LSTORE,ND9,LITEMS,NDATE,
     4                  IS0,IS1,IS2,IS4,ND7,
     5                  IPACK,IWORK,DATA,ND5,
     6                  CORE,ND10,NBLOCK,NFETCH,NSLAB,
     7                  L3264B,ISTOP,IER)
C
C        MARCH     2008   GLAHN   TDL   MOS-2000
C        JUNE      2008   GLAHN   IMPROVED DIAGNOSTICS AT 115, 130
C        OCTOBER   2008   COSGROVE   ADDED COMMAS TO FORMAT 130 FOR IBM
C
C        PURPOSE
C            TO MAKE SURE THE PROBABILITY OF A CUMULATIVE VISIBILITY
C            CATEGORY IS AT LEAST AS HIGH AS THE NEXT LOWER CATEGORY.
C            ITABLE IS FOR VISIBILITY.
C
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C            KFIL10    - UNIT NUMBER FOR INTERNAL RANDOM ACCESS STORAGE.
C                       (INPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              KFIL10 = UNIT NUMBER FOR INTERNAL RANDOM ACCESS STORAGE.
C                       (INPUT)
C               ID(J) = ID OF VARIABLE BEING ANALYZED (J=1,4).
C                       (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       ID'S CORRESPONDING TO ID( )
C                       (J=1,15).  (INPUT)
C            P(IX,JY) = GRID OF VISIBILITIES (IX=1,NX) (JY=1,NY).
C                       (INPUT/OUTPUT)
C                  NX = X EXTENT OF GRID IN P( , ).  (INPUT)
C                  NY = Y EXTENT OF GRID IN P( , ).  (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS).  (INPUT/OUTPUT)
C                 ND9 = MAXIMUM NUMBER OF FIELDS STORED IN LSTORE( , ).
C                       SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS J IN LSTORE( ,L).  
C                       (INPUT/OUTPUT)
C               NDATE = THE DATE/TIME OF THE RUN.  (INPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,4).
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,21+).
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       (INPUT)
C            IPACK(J) = WORK ARRAY FOR GFETCH (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY FOR GFETCH (J=1,ND5).  (INTERNAL)
C         DATA(IX,JY) = WORK ARRAY (IX=1,NX) (JY=1,NY).  DIMENSIONED
C                       ND5 IN CALLING PROGRAM.  (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ), AND IWORK( ).
C                       (INPUT)
C             CORE(J) = SPACE ALLOCATED FOR SAVING PACKED GRIDPOINT 
C                       FIELDS (J=1,ND10).  WHEN THIS SPACE IS 
C                       EXHAUSTED, SCRATCH DISK WILL BE USED.  THIS IS 
C                       THE SPACE USED FOR THE MOS-2000 INTERNAL RANDOM 
C                       ACCESS SYSTEM.  (INPUT)
C                ND10 = THE MEMORY IN WORDS ALLOCATED TO THE SAVING OF 
C                       DATA CORE( ).  WHEN THIS SPACE IS EXHAUSTED,
C                       SCRATCH DISK WILL BE USED.  (INPUT)
C              NBLOCK = BLOCK SIZE IN WORDS OF INTERNAL MOS-2000 DISK
C                       STORAGE.  (INPUT)
C              NFETCH = INCREMENTED EACH TIME DATA ARE FETCHED BY
C                       GFETCH.  IT IS A RUNNING COUNT FROM THE
C                       BEGINNING OF THE PROGRAM.  THIS COUNT 
C                       IS MAINTAINED IN CASE THE USER NEEDS IT
C                       (DIAGNOSTICS, ETC.).  (OUTPUT)
C               NSLAB = SLAB OF THE GRID CHARACTERISTICS.  RETURNED
C                       BY GFETCH.  USED FOR CHECKING FOR EQUAL
C                       CHARACTERISTICS OF GRIDS READ.  (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C            ISTOP(J) = ISTOP(1)--IS INCREMENTED BY 1 EACH TIME AN ERROR 
C                                 OCCURS.
C                       ISTOP(3)--IS INCREMENTED WHEN A DATA RECORD 
C                                 COULD NOT BE FOUND.
C                       (INPUT/OUTPUT)
C                 IER = ERROR RETURN.
C                       0 = GOOD RETURN.
C                       (OUTPUT)
C            TABLE(M) = HOLDS THE 4TH WORD ID FOR THE NOCAT CATEGORIES
C                       OF VISIBILITY (M=1,NOCAT).  (INTERNAL)
C               NOCAT = THE NUMBER OF VISIBILITY CATEGORIES FORECAST
C                       BY LAMP MINUS 1.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES USED 
C            NONE
C
      PARAMETER(NOCAT=6)
C
      DIMENSION ID(4),IDPARS(15),LD(4)
      DIMENSION P(NX,NY),DATA(NX,NY)
      DIMENSION IPACK(ND5),IWORK(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION ITABLE(NOCAT)
      DIMENSION ISTOP(5)
C
      DATA ITABLE/ 495000000,
     1             950000000,
     2             195001000,
     3             295001000,
     4             505001000,
     5             605001000/
C
CD     CALL TIMPR(KFILDO,KFILDO,'START CONVPR        ')
      IER=0
      ICOUNT=0
C
C        FIND THE CATEGORY.
C
      DO 110 J=1,NOCAT
C
      IF(ID(4).EQ.ITABLE(J))THEN
C
        IF(J.EQ.1)THEN
C              THIS IS THE LOWEST CATEGORY.  CAN'T CHECK A LOWER ONE.
            WRITE(KFILDO,105)
 105        FORMAT(/,' ****SETCFT ENTERED FOR FIRST PROBABILITY',
     1               ' CATEGORY.  CAN''T CHECK A LOWER ONE IN CONVPR.',
     2               '     CHANGE U405A .CN FILE.')
            IER=777
            ISTOP(1)=ISTOP(1)+1
            GO TO 210
         ELSE
            GO TO 120
C              J HAS BEEN DEFINED.
         ENDIF
C
      ENDIF
C
 110  CONTINUE
C
C        DROP THROUGH HERE MEANS THE CATEGORY WAS NOT FOUND.
C
      WRITE(KFILDO,115)(ID(L),L=1,4)
 115  FORMAT(/,' ****COULDN''T FIND THRESHOLD FOR ID ',4I10,
     1         ' IN TABLE IN CONVPR.',/,
     2         '     GRID NOT CHECKED',
     3         ' FOR CONSISTENCY WITH LOWER CATEGORY.')
      ISTOP(1)=ISTOP(1)+1
C        NOT COUNTED AS FATAL.	
      GO TO 210
C
 120  LD(1)=ID(1)
      LD(2)=ID(2)
      LD(3)=ID(3)
      LD(4)=ITABLE(J-1)
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,DATA,ND5,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
      IF(IER.NE.0)THEN
         WRITE(KFILDO,130)(LD(L),L=1,4),(ID(L),L=1,4)
 130     FORMAT(/,' ****COULDN''T FETCH GRID ',4I10, 
     1            ' FROM INTERNAL STORAGE IN CONVPR.',/,
     2            '     GRID ',4I10,' NOT CHECKED',
     3            ' FOR CONSISTENCY WITH LOWER CATEGORY.')
         ISTOP(1)=ISTOP(1)+1
         ISTOP(3)=ISTOP(3)+1
         IER=0
C           NOT COUNTED AS FATAL.	
         GO TO 210
      ENDIF
C         
      DO 140 JY=1,NY
      DO 139 IX=1,NX    
C
      IF(DATA(IX,JY).GT.P(IX,JY))THEN
         P(IX,JY)=DATA(IX,JY)
         ICOUNT=ICOUNT+1
      ENDIF
C
 139  CONTINUE
 140  CONTINUE
C 
      IF(ICOUNT.GT.0)THEN
         WRITE(KFILDO,205)ICOUNT
 205     FORMAT(/,I6,' VALUES IN THE VISIBILITY PROBABILITY GRID',
     1           ' MODIFIED BY THE VALUES IN THE NEXT LOWER CATEGORY.')
      ELSE
         WRITE(KFILDO,206)
 206     FORMAT(/' VISIBILITY PROBABILITY GRID CONSISTENT WITH NEXT',
     1           ' LOWER ONE.')
      ENDIF
C      
 210  RETURN
      END
