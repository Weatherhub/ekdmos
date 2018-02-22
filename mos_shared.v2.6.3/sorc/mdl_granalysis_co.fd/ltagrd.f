      SUBROUTINE LTAGRD(KFILDO,KFIL10,NDATE,
     1                  ID,IDPARS,JD,ITABLE,
     2                  CCALL,NAME,LTAG,NSTA,ND1,
     3                  LSTORE,ND9,LITEMS,
     4                  IS0,IS1,IS2,IS4,ND7,
     5                  IPACK,IWORK,DATA,ND5,
     6                  CORE,ND10,NBLOCK,NFETCH,
     7                  L3264B,ISTOP,IER)
C
C        JULY      2008   GLAHN   TDL   MOS-2000
C        AUGUST    2008   GLAHN   COMMENTS; ADDED LTAGSV( )
C        SEPTEMBER 2008   GLAHN   ADDED ISTOP(1) AT 102, 103
C
C        PURPOSE
C            TO READ A RECORD FROM INTERNAL STORAGE HOLDING THE 
C            LTAG( ) VALUES FROM A PREVIOUS ANALYSIS.  THIS IS USED
C            TO UPDATE THE LTAG( ) VALUES FOR THE VARIABLE BEING
C            ANALYZED.  WHEN THE LTAG( ) READ IS -1, IT INDICATES
C            THE STATION HAD BEEN TOSSED, SO IT SHOULD NOT BE 
C            USED IN THIS ANALYSIS EITHER (E.G., SPEED AND COMPONENTS).
C            WHEN A -1 IS FOUND, THE CURRENT LTAG( ) IS SET TO +1
C            TO INDICATE THIS STATION SHOULD NOT BE USED.  AN
C            ALLOCATED ARRAY LTAGSV( ) IS SAVED TO RETAIN PREVIOUS -1'S.
C
C        DATA SET USE
C            KFILDO   - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C            KFIL10   - UNIT NUMBER FOR INTERNAL RANDOM ACCESS STORAGE.
C                       (INPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C              KFIL10 = UNIT NUMBER FOR INTERNAL RANDOM ACCESS STORAGE.
C                       (INPUT/OUTPUT)
C               NDATE = DATE/TIME, YYYYMMDDHH, OF ANALYSIS RUN.
C                       (INPUT)
C               ID(J) = 4-WORD ID OF VARIABLE TO READ FROM INTERNAL
C                       STORAGE (J=1,4).  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       VARIABLE ID'S CORRESPONDING TO ID( ,N)
C                       (J=1,15), (N=1,ND4).
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 
C                            1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK 
C                            IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C                       (INPUT)
C               JD(J) = THE BASIC INTEGER VARIABLE ID'S (J=1,4) 
C                       (N=1,ND4).
C                       THIS IS THE SAME AS ID(J,N), EXCEPT THAT THE
C                       PORTIONS PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3, ),
C                       T = IDPARS(8,),
C                       I = IDPARS(13, ),
C                       S = IDPARS(14, ),
C                       G = IDPARS(15, ), AND
C                       THRESH( ).
C                       NOT ACTUALLY USED.  (INPUT)
C           ITABLE(J) = HOLDS THE 4-WORD IDS (J=1,4) OF THE VARIABLE
C                       FOR WHICH THE LTAG( ) RECORD IS AVAILABLE IN
C                       INTERNAL STORAGE.  (INPUT)     
C            CCALL(K) = CALL LETTERS OF STATIONS BEING DEALT WITH.
C                       (CHARACTER*8)  (INPUT)
C             NAME(K) = NAMES OF STATIONS (K=1,NSTA).  (CHARACTER*20)
C                       (INPUT)
C             LTAG(J) = DENOTES USE OF DATA CORRESPONDING TO CCALL(J).
C                       +2 = NOT USED FOR ANY PURPOSE.  FLTAG SETS
C                            A VALUE +2 WHEN THE STATION LOCATION
C                            IS MISSING.
C                       +1 = PERMANENTLY DISCARDED FOR THE VARIABLE
C                            BEING ANALYZED.  INCLUDES DATA FAR
C                            OUTSIDE THE GRID, AS DEFINED BY RMAX
C                        0 = USE ON CURRENT PASS THROUGH DATA.
C                       -1 = DO NOT USE ON THIS PASS (INCOMING).
C                            LTAGRD SETS THIS TO +1 TO INDICATE TO
C                            NOT USE FOR THIS ANALYSIS.
C                       -3 = ACCEPT THIS STATION ON EVERY PASS.  THIS
C                            FEATURE MAY OR MAY NOT BE IMPLEMENTED IN
C                            THE CALLING PROGRAM.
C                       (INPUT/OUTPUT)
C                 ND1 = DIMENSION OF LTAG( ).  (INPUT)
C                NSTA = NUMBER OF STATIONS BEING USED; THE NUMBER
C                       OF VALUES IN LTAG( ) AND DATA( ).  (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS).  (INPUT/OUTPUT)
C                 ND9 = MAXIMUM NUMBER OF FIELDS STORED IN LSTORE( , ).
C                       SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS J IN LSTORE( ,L).  
C                       (INPUT/OUTPUT)
C              NTIMES = THE NUMBER OF TIMES THIS VARIABLE HAS BEEN
C                       ACCESSED.  (INTERNAL)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,4).
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,21+).
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       (INPUT)
C            IPACK(J) = WORK ARRAY FOR GFETCH (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY FOR GFETCH (J=1,ND5).  (INTERNAL)
C             DATA(J) = WORK ARRAY FOR GFETCH (J=1,ND5) AND COMPUTATIONS.
C                       (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ), WORK( ), AND DATA( ).
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
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C            ISTOP(J) = ISTOP(1) IS INCREMENTED BY 1 WHENEVER AN ERROR 
C                       OCCURS AND THE PROGRAM PROCEEDS.  ISTOP IS
C                       INCREMENTED WHEN THE FIRST CHOICE OF FIRST
C                       GUESS IS NOT AVAILABLE (I.E., MGUESS NE 
C                       IGUESS(1)).  ISTOP(3) IS INCREMENTED BY 1
C                       WHEN A DATA RECORD COULD NOT BE FOUND.
C                       (INPUT/OUTPUT)
C            ISTOP(J) = ISTOP(1) IS INCREMENTED BY 1 EACH TIME AN ERROR
C                                OCCURS.
C                       ISTOP(3) IS INCREMENTED WHEN A DATA RECORD 
C                                CANNOT BE FOUND.
C                       (INPUT/OUTPUT)
C                 IER = ERROR CODE. 
C                         0 = GOOD RETURN.
C                       103 = COULD NOT IDENTIFY ID IN INTERNAL TABLE.
C                        OTHER VALUES FROM CALLED ROUTNES.  EVERY
C                        ERROR IS FATAL FOR THIS ELEMENT.
C                       (OUTPUT)
C               NSLAB = SLAB OF THE GRID CHARACTERISTICS.  RETURNED
C                       BY GFETCH.  USED FOR CHECKING FOR EQUAL
C                       CHARACTERISTICS OF GRIDS READ.  (INTERNAL)
C            LTAGSV(K) = ARRAY TO SAVE LTAG( ) TO PRESERVE THE -1.
C                       (ALLOCATED)  (INTERNAL)  (SAVED)
C              IFIRST = INITIALLY ZERO, THEN SET TO 1.  (INTERNAL)
C                       (SAVED) 
C              NDATSV = SAVES THE NDATE.  (INTERNAL)  (SAVED) 
C              ITAUSV = SAVES THE TAU.  (INTERNAL)  (SAVED) 
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES USED 
C            GFETCH, TIMPR
C
      CHARACTER*8 CCALL(ND1)
      CHARACTER*20 NAME(ND1)
C
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION LTAG(ND1)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION ISTOP(3),LD(4),ITABLE(4)
C
      ALLOCATABLE LTAGSV(:)
C        LTAGSV( ) IS AN ALLOCATED SAVED ARRAY.
      SAVE LTAGSV
      SAVE IFIRST,NDATSV,ITAUSV
C
      DATA IFIRST/0/
C
      IER=0
CD     CALL TIMPR(KFILDO,KFILDO,'START LTAGRD        ')
C
C        INITIALIZE LTAGSV( ) ON FIRST ENTRY OR WHEN DATE OR TAU
C        CHANGES.  NOTHING IS EVER USED FROM IT EXCEPT -1'S.
C
      IF(IFIRST.EQ.0)THEN
C
         ALLOCATE (LTAGSV(NSTA),STAT=IOS)
C    
         IF(IOS.EQ.1)THEN
C
            WRITE(KFILDO,102)
 102        FORMAT(/' ****ALLOCATION OF LTAGSV( ) FAILED IN LTAGRD',
     1              ' AT 102.  ARRAY ALREADY ALLOCATED.')
            ISTOP(1)=ISTOP(1)+1
            IER=777
            GO TO 150
C
         ELSEIF(IOS.EQ.2)THEN
            WRITE(KFILDO,103)
 103        FORMAT(/' ****ALLOCATION OF LTAGSV( ) FAILED IN LTAGRD',
     1              ' AT 103.  ARRAY NOT ALLOCATED.')
            ISTOP(1)=ISTOP(1)+1
            IER=777
            GO TO 150
         ENDIF
C
      ENDIF
C
      IF(NDATE.NE.NDATSV.OR.ITAUSV.NE.IDPARS(12))THEN
         IFIRST=0
         NDATSV=NDATE
         ITAUSV=IDPARS(12)
      ENDIF
C
      IF(IFIRST.EQ.0)THEN
         IFIRST=1
C
         DO 105 K=1,NSTA
         LTAGSV(K)=0
 105     CONTINUE
C
      ENDIF
C           GET THE LTAG VALUES.  ITABLE( ) IS THE ITABLE( ,7)
C           ENTRY IN ITABLE IN U405A.
C
      LD(1)=ITABLE(1)+IDPARS(4)
      LD(2)=970000
      LD(3)=ITABLE(3)+IDPARS(12)
      LD(4)=ITABLE(4)
C            
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,DATA,ND5,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER)
C
      IF(IER.NE.0)THEN
         ISTOP(3)=ISTOP(3)+1
         WRITE(KFILDO,111)
 111     FORMAT(/' ****CANNOT READ DESIRED LTAG( ) RECORD IN',
     1           ' LTAGRD AT 111.',
     2           '  CANNOT USE LTAG( ) FROM PREVIOUS ANALYSIS.')
         GO TO 150
C           IF THE RECORD IS NOT AVAILABLE, THE PREVIOUS
C           TOSSED INFORMATION CANNOT BE USED. 
      ENDIF
C
      IF(NWORDS.NE.NSTA)THEN
         ISTOP(1)=ISTOP(1)+1
         WRITE(KFILDO,113)
 113     FORMAT(/' ****NUMBER OF WORDS RETURNED FROM GFETCH NOT',
     1           ' CONSISTENT EQUAL TO NSTA IN LTAGRD AT 113.',
     2           '  CANNOT USE LTAG( ) FROM PREVIOUS ANALYSIS.')
         GO TO 150
      ENDIF
C
      DO 120 K=1,NSTA
C
      LT=NINT(DATA(K))
C
      IF(LT.EQ.-1.OR.LTAGSV(K).EQ.-1)THEN
         LTAG(K)=1
         LTAGSV(K)=-1
C           IT'S NOT NECESSARY THT LTAGSV( ) KEEP ANYTHING BUT THE -1'S.
C           IF THESE RE NOT KEPT, THE TOSSED ONES WILL NOT CARRY OVER
C           INTO A SUBSEQUENT ANALYSIS.
         LD(2)=0
         WRITE(KFILDO,115)CCALL(K),NAME(K),(ID(J),J=1,4),
     1                   (LD(J),J=1,4)
 115     FORMAT(' STATION ',A8,A20,' NOT USED FOR VARIABLE ',
     1          3I10.9,I10.3,/,
     2          25X,'BECAUSE IT WAS TOSSED FOR VARIABLE ',
     3          3I10.9,I10.3,' OR A PREVIOUS ONE.')
      ENDIF
C
 120  CONTINUE
C
CD     WRITE(KFILDO,125)(K,CCALL(K),NAME(K),LTAG(K),K=1,NSTA)
C        NOTE THAT THESE VALUES ARE NOT SCALED--JUST WHAT IS
C        BEING ANALYZED.  SCALING COULD BE DONE HERE OR IN A
C        FOLLOWING ROUTINE.
CD125  FORMAT(/,' IN LTAGRD AT 125',/,(I6,2X,A8,A20,F8.2))
C
CD     CALL TIMPR(KFILDO,KFILDO,'END   LTAGRD        ')
C
 150  RETURN
      END
