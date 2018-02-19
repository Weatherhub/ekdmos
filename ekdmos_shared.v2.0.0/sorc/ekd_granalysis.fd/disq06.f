      SUBROUTINE DISQ06(KFILDO,KFIL10,NDATE,ID,IDPARS,JD,
     1                  P,NXY,TRUNC,CONST,NSCALE,
     2                  LSTORE,ND9,LITEMS,
     3                  IS0,IS1,IS2,IS4,ND7,
     4                  IPACK,IWORK,DATA,ND5,
     5                  CORE,ND10,NBLOCK,NFETCH,
     6                  L3264B,ISTOP,IER)
C
C        JULY      2007   GLAHN   TDL   MOS-2000
C                                 ADAPTED FROM SCLQ06 AND DISQPF
C        DECEMBER  2007   GLAHN   ADDED CHECK ON P( ) IN DO 250 LOOP
C
C        PURPOSE
C            QPF ANALYZED WITH TERRAIN LEAVES VERY SMALL BUT
C            NON-ZERO VALUES IN THE HILLS OF OTHERWISE ZERO AMOUNTS.
C            THE PROCESS IS TO FIRST ANALYZE THE QPF WITH NO TERRAIN
C            AND SAVE THE RESULT IN INTERNAL RA STORAGE.
C            THEN ANALYZE IT WITH TERRAIN, READ THE FIRST W/O TERRAIN
C            ANALYSIS, AND SET ALL VALUES IN THE FINAL GRID WITH ZERO
C            VALUES IN THE W/O TERRAIN GRID TO ZERO.
C     
C            THEN SCALE THE FINAL TO VALUE X CONST*10**NSCALE AFTER
C            REMOVING ALL VALUES LT TRUNC.  THIS IS DESIGNED FOR 
C            GRIDDED DATA.
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
C               ID(J) = 4-WORD ID OF VARIABLE TO MODIFY (J=1,4).
C                       (INPUT)
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
C              P(IXY) = THE GRID TO MODIFY (IXY=1,NXY).  (INPUT/OUTPUT)
C                 NXY = THE SIZE OF THE GRID IN P( )  (INPUT)
C               TRUNC = ALL VALUES BELOW TRUNC WILL BE SET TO ZERO.
C                       (INPUT)
C               CONST = THE MULTIPLIER FOR SCALING.  (INPUT)
C              NSCALE = THE POWER OF TEN FOR SCALING.  (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS).  (INPUT/OUTPUT)
C                 ND9 = MAXIMUM NUMBER OF FIELDS STORED IN LSTORE( , ).
C                       SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS J IN LSTORE( ,L).  
C                       (INPUT)
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
C             CORE(J) = SPACE ALLOCATED FOR INTERNAL RANDOM ACCESS 
C                       (J=1,ND10).  WHEN THIS SPACE IS EXHAUSTED,
C                       SCRATCH DISK WILL BE USED.  THIS IS 
C                       THE SPACE USED FOR THE MOS-2000 INTERNAL RANDOM 
C                       ACCESS SYSTEM.  (INPUT)
C                ND10 = THE MEMORY IN WORDS ALLOCATED TO THE SAVING OF 
C                       DATA CORE( ).  WHEN THIS SPACE IS EXHAUSTED,
C                       SCRATCH DISK WILL BE USED.  (INPUT)
C              NBLOCK = BLOCK SIZE IN WORDS OF INTERNAL MOS-2000 DISK
C                       STORAGE.  (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.  IT IS
C                       A RUNNING COUNT FROM THE BEGINNING OF THE PROGRAM.
C                       THIS COUNT IS MAINTAINED IN CASE THE USER NEEDS 
C                       IT (DIAGNOSTICS, ETC.).  (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C            ISTOP(J) = ISTOP(1) IS INCREMENTED BY 1 WHENEVER AN ERROR 
C                       OCCURS AND THE PROGRAM PROCEEDS.  ISTOP(3) IS
C                       INCREMENTED BY 1 WHEN A DATA RECORD COULD
C                       NOT BE FOUND.  (INPUT/OUTPUT)
C                 IER = ERROR CODE. 
C                         0 = GOOD RETURN.
C                       103 = COULD NOT IDENTIFY ID IN INTERNAL TABLE
c                             OR COULD NOT RETRIEVE THE NEEDED GRID.
C                       777 = WHEN A CALLED ROUTINE DID NOT FURNISH
C                             AN IER.
C                        OTHER VALUES FROM CALLED ROUTNES.  EVERY
C                        ERROR IS FATAL FOR THIS ELEMENT.
C                       (OUTPUT) 
C              MISTOT = RUNNING TOTAL OF RETRIEVED GRIDS WITH ONE OR
C                       MORE MISSING VALUES.  (INTERNAL)
C               NSLAB = SLAB OF THE GRID CHARACTERISTICS.  RETURNED
C                       BY GFETCH.  (INTERNAL)
C              NWORDS = THE NUMBER OF VALUES RETURNED IN DATA( ).
C                       (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES USED 
C            GFETCH
C
      PARAMETER (IDLST=1)
C
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION P(NXY)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION ISTOP(3),IDTAB(4,IDLST),MDTAB(4,IDLST),LD(4)
C
      DATA IDTAB/223270000,0,0,0/
      DATA MDTAB/223271000,0,0,0/
C
      IER=0
C
      IF(CONST.EQ.1..AND.NSCALE.EQ.0)THEN
         FACTOR=1.
      ELSE
         FACTOR=CONST*10.**NSCALE
      ENDIF
C
D     WRITE(KFILDO,100)(ID(J),J=1,4)
D100  FORMAT(/' AT 100 IN DISQ06--(ID(J),J=1,4)',4I11)
D     WRITE(KFILDO,101)CONST,NXY,NSCALE,ND5,ND9,ND10
D101  FORMAT(/' AT 101 IN DISQ06--CONST,NXY,NSCALE,ND5,ND9,ND10',
D    1        F10.3,5I10)
C
C        DETERMINE WHETHER VARIABLE IS IN THE LIST.
C        THE DD IS NOT IN THE TABLE IN CASE THE MODEL CHANGES.
C        THE TAU IS NOT IN THE TABLE TO MAKE IT GENERIC, BUT
C        IS IN ID(3).
C
      DO 105 L=1,IDLST
C
      IF(ID(1)/100   .EQ.IDTAB(1,L)/100.AND.
     1   ID(2)       .EQ.IDTAB(2,L).AND.
     2   ID(3)/1000  .EQ.IDTAB(3,L).AND.
     3   ID(4)       .EQ.IDTAB(4,L))THEN
         GO TO 112
C           THIS DEFINES L.
      ENDIF
C
 105  CONTINUE
C
C        DROP THROUGH HERE MEANS THE ID WAS NOT FOUND.
C
      ISTOP(1)=ISTOP(1)+1
      WRITE(KFILDO,110)(ID(J),J=1,4),IER
 110  FORMAT(/' ****VARIABLE ',I9.9,I10.9,I10.9,I4.3,' NOT',
     1        ' ACCOMMODATED IN SUBROUTINE DISQ06.  IER =',I3)
      IER=103
      ISTOP(1)=ISTOP(1)+1
      GO TO 900
C
C
 112  LD(1)=MDTAB(1,L)+IDPARS(4)
C        THE DD IS ADDED.
      LD(2)=MDTAB(2,L)
      LD(3)=MDTAB(3,L)+IDPARS(12)
C        THE TAU IS ADDED.
      LD(4)=MDTAB(4,L)
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,DATA,ND5,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER) 
C
      IF(IER.NE.0)THEN
         ISTOP(3)=ISTOP(3)+1
         ISTOP(1)=ISTOP(1)+1
         WRITE(KFILDO,120)(LD(M1),M1=1,4)
 120     FORMAT(/' ****COULD NOT FIND NECESSARY GRID IN DISQ06',
     1           3I10.9,I10,' IN INTERNAL STORAGE.  THE ANALYSIS',
     2           'WILL NOT BE MODIFIED.')
         IER=103
         ISTOP(1)=ISTOP(1)+1
         GO TO 900
      ENDIF
C
C        SINCE THIS FETCHED GRID WAS WRITTEN INTERNALLY, IT MUST 
C        BE AT THE CORRECD SIZE AND MESHB.  THE ONLY CHECKING IS
C        TO SEE THE CORRECT NUMBER OF WORDS IS RETURNED.
C
      IF(NWORDS.NE.NXY)THEN
         WRITE(KFILDO,125)NWORDS,NXY
 125     FORMAT(/' ****INCORRECT NUBMER OF WORDS =',I8,
     1           ' RETURNED BY GFETCH IN DISQ06.  SHOULD BE',I8, 
     2           '.  GRID IS NOT MODIFIED.')  
         IER=103
         ISTOP(1)=ISTOP(1)+1
         GO TO 900
      ENDIF
C
C        SET VALUES IN P( ) TO ZERO WHEN DATA( ) = 0.
C
      DO 250 IXY=1,NXY
C
      IF(P(IXY).LT.9998.)THEN
C           P( ) HAS BEEN CLIPPED.  MISSING IS 9999. 
C
         IF(DATA(IXY).LE.TRUNC)THEN
            P(IXY)=0.
         ELSEIF(P(IXY).LE.TRUNC)THEN
            P(IXY)=0.
         ELSE
            P(IXY)=P(IXY)*FACTOR
         ENDIF
C
      ENDIF
C
 250  CONTINUE
C
 900  RETURN
      END
