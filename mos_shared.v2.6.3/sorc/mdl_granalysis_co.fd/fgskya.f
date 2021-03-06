      SUBROUTINE FGSKYA(KFILDO,KFIL10,NDATE,ID,IDPARS,JD,
     1                  XDATA,FD2,CCALL,ND1,NVAL,
     2                  NX,NY,NCAT,CONST,NSCALE,
     3                  NPROJ,ALATL,ALONL,ORIENT,XLAT,MESH,ITRPX,
     4                  LSTORE,ND9,LITEMS,
     5                  IS0,IS1,IS2,IS4,ND7,
     6                  IPACK,IWORK,DATA,ND5,
     7                  CORE,ND10,NBLOCK,NFETCH,MISTOT,
     8                  L3264B,ISTOP,IER)
C
C        MAY       2008   GLAHN   TDL   MOS-2000
C                                 MODIFIED FROM SCLSKY
C        OCTOBER   2008   COSGROVE   ADDED COMMAS FOR IBM COMPILE
C
C        PURPOSE
C            TO COMPUTE A FIRST GUESS CLOUD AMOUNT FIELD IN
C            PERCENT (FRACTIONAL COVERAGE * 100) BASED ON AN
C            EXPECTED VALUE SCHEME BY USING CATEGORICAL AMOUNTS
C            AND PROBABILITIES OF THOSE CATEGORIES.  THE CATEGORICAL
C            AMOUNTS CAN BE ASSIGNED VALUES BASED ON THE VALUES IN
C            TABLE ( , ) TO GIVE THE DESIRED RESULTS.  THIS ROUTINE
C            WILL LIKELY BE USED ONLY FOR GRIDDEED DATA, BUT THE
C            ARRAYS ARE SINGLE DIMENSION AND CAN BE USED FOR 
C            EITHER WITHIN THE MOS-2000 FRAMEWORK.  THIS
C            WAS WRITTEN FOR OPAQUE SKY COVER, AND ITABLE( , , )
C            AND TABLE( , ) ARE SPECIFIC TO THOSE CATEGORIES.
C
C            NOTE THIS IS SPECIFIC TO SKY COVER.  THE PROBABILITIES
C            ARE FOR DISCRETE CATEGORIES, OF WHICH THERE ARE 5.
C            AS ORIGINALLY WRITTEN THE LOWER CATEGORY BOUNDS ARE
C            USED FOR THE LOWER TWO CATEGORIES, THE UPPER BOUNDS
C            ARE USED FOR THE UPPER TWO CATEGORIES, AND THE MIDPOINT
C            OF THE RANGE IS USED FOR THE MID CATEGORY.  NOTE THIS
C            IS NOT INTENDED TO BE A "TRUE" EXPECTED AMOUNT, BUT
C            RATHER A VALUE THAT CAN BE USED AS A FIRST GUESS. 
C
C            THE "A" IN FGSKYA DESIGNATES THIS FOR ALASKA; THE 
C            CONCEPT MAY HAVE USE IN OTHER AREAS, BUT MIGHT HAVE
C            TO BE MODIFIED
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
C               ID(J) = 4-WORD ID OF VARIABLE TO PROVIDE FIRST GUESS FOR
C                       (J=1,4).  (INPUT)
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
C            XDATA(K) = CATEGORICAL VALUES ON INPUT; PSEUDO EXPECTED
C                       AMOUNT ON OUTPUT (K=1,NVAL).  (INPUT/OUTPUT)
C              FD2(K) = WORK ARRAY (K=1,NVAL).  (INTERNAL)
C            CCALL(K) = CALL LETTERS OF STATIONS WHEN ENTRY IS FOR
C                       VECTOR DATA (NX = 0) (J=1,NVAL).  WHEN 
C                       ENTRY IS FOR A GRID, CCALL( ) IS DUMMY.
C                       (INPUT)
C                 ND1 = FIRST DIMENSION OF XDATA( ) AND DIMENSION
C                       OF FD2( ).  (INPUT)
C                NVAL = NUMBER OF STATIONS BEING USED; THE NUMBER
C                       OF VALUES IN XDATA( ).  (INPUT)
C                  NX = THE X-EXTENT OF THE GRID NEEDED WHEN A GRID
C                       IS BEING ACCESSED; OTHERWISE, MUST BE ZERO.
C                       FGSKYA CAN BE USED FOR EITHER VECTOR DATA
C                       OR GRIDDED DATA.  WHEN GRIDDED, THE GRID
C                       MUST BE PUT ONTO THE GRID BEING USED BY
C                       THE CALLING PROGRAM.  (INPUT)
C                  NY = THE Y-EXTENT OF THE GRID NEEDED WHEN A GRID
C                       IS BEING ACCESSED.  (SEE NX ABOVE.)  (INPUT)
C                NCAT = NUMBER OF PROBABILITY CATEGORIES.  (INPUT)
C               CONST = THE MULTIPLIER FOR SCALING THE CATEGORICAL
C                       OUTPUT.  (INPUT)
C              NSCALE = THE POWER OF TEN FOR SCALING THE CATEGORICAL
C                       OUTPUT.  (INPUT)
C               NPROJ = NUMBER OF MAP PROJECTION TO WHICH THIS GRID
C                       APPLIES.
C                       3 = LAMBERT.
C                       5 = POLAR STEREOGRAPHIC.
C                       7 = MERCATOR.
C                       (INPUT)
C               ALATL = LATITUDE IN DEGREES OF THE LOWER LEFT CORNER
C                       POINT (1,1) OF THE ANALYSIS GRID.  (INPUT)
C               ALONL = LONGITUDE (WEST) IN DEGREES OF THE LOWER LEFT
C                       CORNER POINT OF THE ANALYSIS GRID.  (INPUT)
C              ORIENT = ORIENTATION W LONGITUDE, PARALLEL TO GRID
C                       COLUMNS, IN DEGREES.  (INPUT)
C                XLAT = N LATITUDE AT WHICH THE MESH LENGTH APPLIES.
C                       (INPUT)
C                MESH = NOMINAL MESH LENGTH OF THE GRID NEEDED.  NOT
C                       ACTUALLY USED FOR VECTOR DATA.  (INPUT)
C               ITRPX = THE TYPE OF INTERPOLATION BEING USED.
C                       (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS).  (INPUT/OUTPUT)
C                 ND9 = MAXIMUM NUMBER OF FIELDS STORED IN LSTORE( , ).
C                       SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS J IN LSTORE( ,L).  
C                       (INPUT/OUTPUT)
C              NTIMES = THE NUMBER OF TIMES GFETCH HAS BEEN ACCESSED.
C                       (INPUT/OUTPUT)
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
C              MISTOT = RUNNING TOTAL OF RETRIEVED GRIDS WITH ONE OR
C                       MORE MISSING VALUES.  (INPUT/OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C            ISTOP(J) = ISTOP(1) IS INCREMENTED BY 1 WHENEVER AN ERROR 
C                       OCCURS AND THE PROGRAM PROCEEDS.  ISTOP IS
C                       INCREMENTED WHEN THE FIRST CHOICE OF FIRST
C                       GUESS IS NOT AVAILABLE (I.E., MGUESS NE 
C                       IGUESS(1)).  ISTOP(3) IS INCREMENTED BY 1
C                       WHEN A DATA RECORD COULD NOT BE FOUND.
C                       (INPUT/OUTPUT)
C                NVAL = THE NUMBER OF VALUES IN XDATA( ) BEING DEALT
C                       WITH.  (INPUT)
C            ISTOP(J) = ISTOP(1) IS INCREMENTED BY 1 EACH TIME AN ERROR
C                                OCCURS.
C                       ISTOP(2) IS INCREMENTED WHEN LESS THAN
C                                200 STATIONS ARE AVAILABLE FOR AN
C                                ANALYSIS.
C                       ISTOP(3) IS INCREMENTED WHEN A DATA RECORD 
C                                CANNOT BE FOUND.
C                       (INPUT/OUTPUT)
C                 IER = ERROR CODE. 
C                         0 = GOOD RETURN.
C                       103 = COULD NOT IDENTIFY ID IN INTERNAL TABLE.
C                       777 = WHEN A CALLED ROUTINE DID NOT FURNISH
C                             AN IER.
C                        OTHER VALUES FROM CALLED ROUTNES.  EVERY
C                        ERROR IS FATAL FOR THIS ELEMENT.
C                       (OUTPUT) 
C               NSLAB = SLAB OF THE GRID CHARACTERISTICS.  RETURNED
C                       BY GFETCH.  USED FOR CHECKING FOR EQUAL
C                       CHARACTERISTICS OF GRIDS READ.  (INTERNAL)
C       ITABLE(I,J,L) = HOLDS THE 4-WORD IDS OF THE NCAT PROBABILITIES
C                       (I=1,4) (J=1,NCAT) (L=1,2).  THE THIRD
C                       DIMENSION IS TO ACCOMMODATE BOTH THE DATA TO
C                       ANALYZE (L=1) AND THE FIRST GUESS (L=2).
C                       THE IDCAT ENTRY IS THE 4-WORD ID OF THE 
C                       VARIABLE BEING PROCESSED SANS THE DD AND TAU
C                       (E.G., THE CATEGORICAL VARIABLE).  (INTERNAL)     
C          TABLE(I,J) = HOLDS THE LOWER AND UPPER CATEGORY VALUES OF
C                       THE VARIABLES WHOSE IDS ARE IN ITABLE( , , )
C                       (I=1,2), (J=1,NCAT-1).  (SEE ITABLE( , , )) FOR
C                       MORE EXPLANATION.  (INTERNAL)     
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES USED 
C            SZGRDM, GFETCH, CKGRID, LMLLIJ, PSLLIJ, MCLLIJ, NOMINL,
C            CUTIT
C
      PARAMETER (IDCAT=6)
C
      CHARACTER*8 CCALL(ND1)
C
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION XDATA(ND1),FD2(ND1)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION ISTOP(3),ITABLE(4,IDCAT,2),LD(4),TABLE(2,IDCAT-1)
C
      DATA ITABLE/208384300,0,0,150001000,
     1            208384300,0,0,250001000,
     2            208384300,0,0,400001000,
     3            208384300,0,0,700001000,
     4            208384300,0,0,999905000,
     5            208381000,0,0,0,
C
     5            218384300,0,0,150001000,
     6            218384300,0,0,250001000,
     7            218384300,0,0,400001000,
     8            218384300,0,0,700001000,
     9            218384300,0,0,999905000,
     A            218381000,0,0,0/
C
      DATA TABLE/          .00,.05,
     1                     .05,.25,
     2                     .25,.50,
     3                     .50,.87,
     4                     .87,1.0/
C
      IER=0
C      WRITE(KFILDO,100)NX,NY
C 100  FORMAT(/' AT 100 IN FGSKYA--NX,NY',2I6)
C
C        DETERMINE WHETHER VARIABLE IS IN THE LIST.
C        THE DD IS NOT IN THE TABLE IN CASE THE MODEL CHANGES.
C        THE TAU IS NOT IN THE TABLE TO MAKE IT GENERIC, BUT
C        IS IN ID(3).
C
CD     WRITE(KFILDO,101)NCAT,CONST,NSCALE,
CD    1                ((ITABLE(J,NCAT,L),J=1,4),L=1,2)
CD101  FORMAT(/' AT 101 IN FGSKYA--CAT,CONST,NSCALE',
CD    1        '((ITABLE(J,IDCAT.L),J=1,4),L=1,2',I6,F6.2,I6/
CD    2         (4I11))
CD     WRITE(KFILDO,102)(ITABLE(M1,J,1),M1=1,4),(IDPARS(M1),M1=1,15)
CD102  FORMAT(/' AT 102--(ITABLE(M1,J,1),M1=1,4),(IDPARS(M1),M1=1,15)',
CD    1          4I12/(15I8))
C
      IF(NCAT.NE.IDCAT-1)THEN
         WRITE(KFILDO,103)NCAT,IDCAT-1
 103     FORMAT(/' ****NCAT = ',I3,' NOT CORRECT IN FGSKYA.',
     1           '  SHOULD BE', I3,'.  FATAL ERROR.')
         IER=777
         GO TO 900
      ENDIF
C
      DO 105 L=1,2
C
      IF(ID(1).EQ.ITABLE(1,IDCAT,L)+IDPARS(4).AND.
     1   ID(2).EQ.ITABLE(2,IDCAT,L).AND.
     2   (ID(3)/1000).EQ.(ITABLE(3,IDCAT,L)/1000).AND.
     3   ID(4).EQ.ITABLE(4,IDCAT,L))THEN
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
     1        ' ACCOMMODATED IN SUBROUTINE FGSKYA.  IER =',I3)
      IER=103
      GO TO 900
C
C        FIND THE NCAT PROBABILITIES.  THE CATEGORICAL
C        VALUES ARE IN XDATA( ) ON INPUT AND WILL BE MODIFIED.
C
 112  FACTOR=CONST*10.**NSCALE
C
CD     WRITE(KFILDO,113)ND1,NVAL,L,(IDPARS(M1),M1=1,15)
CD113  FORMAT(/' AT 113--,ND1,NVAL,L,(IDPARS(M1),M1=1,15)',
CD    1          3I12/(15I8))
C
CD     WRITE(KFILDO,1135)NVAL,FACTOR,(XDATA(M),M=1,NVAL)
CD1135 FORMAT(' AT 1135 IN FGSKYA--NVAL,FACTOR,(XDATA(M),M=1,NVAL)',
CD    1        I12,F8.2,/,(15F8.1))
C
C        SET FD2( ) = 0.
C
      DO 115 K=1,NVAL
      FD2(K)=0.
 115  CONTINUE
C
      DO 200 J=1,NCAT
C
CD     WRITE(KFILDO,116)J,L,(ITABLE(M1,J,L),M1=1,4)
CD116  FORMAT(/' AT 116--J,L,(ITABLE(M1,J,L),M1=1,4)',
CD    1          6I12)
C
C        GET THE PROBABILITY OF CATEGORY J.  CAN BE A GRID OR VECTOR.
C
      LD(1)=ITABLE(1,J,L)+IDPARS(4)
C        THE DD IS ADDED.
      LD(2)=ITABLE(2,J,L)
      LD(3)=ITABLE(3,J,L)+IDPARS(12)
C        THE TAU IS ADDED.
      LD(4)=ITABLE(4,J,L)
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,DATA,ND5,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,IER) 
C
      IF(IER.NE.0)THEN
         ISTOP(3)=ISTOP(3)+1
         ISTOP(1)=ISTOP(1)+1
         WRITE(KFILDO,120)(LD(M1),M1=1,4)
 120     FORMAT(/' ****COULD NOT FIND PROBABILITY RECORD',
     1           3I10.9,I10,'.  FATAL ERROR IN FGSKYA AT 120.')
         GO TO 900
      ENDIF
C
      IF(NX.EQ.0)GO TO 1490
C        WHEN NX NE 0, THIS IS A GRID, AND MUST BE PROCESSED
C        ONTO THE CORRECT GRID.
C      
      IF(MISSP.NE.0)MISTOT=MISTOT+1
C
C        IF THIS GRID COULD NOT BE OBTAINED OR THE GRID CHARACTERISTICS
C        WERE NOT WHAT WAS EXPECTED, COUNT IT AS A GRID THAT COULD 
C        NOT BE OBTAINED BY INCREMENTING ISTOP(3).  IT IS ALSO A
C        FATAL ERROR FOR THIS ELEMENT.
C
C        CHECK GRID PARAMETERS.
C
      CALL CKGRID(KFILDO,LD,NPROJ,ORIENT,XLAT,IS2,ND7,IER)
C
      IF(IER.NE.0)THEN
         ISTOP(3)=ISTOP(3)+1
         GO TO 900
      ENDIF
C
C        CKGRID ASSURES THE MAP PROJECTION (NPROJ), THE ORIENTATION
C        (ORIENT), AND THE LATITUDE OF MESH LENGTH (XLAT) ARE
C        WHAT ARE EXPECTED AND THAT THE MESH LENGTH IS ONE OF
C        THE PERMISSIBLE ONES.  IT IS NOT ASSURED AT THIS POINT
C        THAT THE LOCATION OF THE GRID, THE MESH LENGTH, OR THE
C        DIMENSIONS OF THE GRID ARE WHAT ARE WANTED.
C
C        THE INPUT GRID AT ITS SIZE AND LOCATION IS IN DATA( ).
C        POSITION THIS GRID (WITH THE SAME GRID LENGTH) OVER THE 
C        ANALYSIS AREA.  ALATL AND ALONL REFER TO THE MESH
C        LENGTH MESHB.  IS2(8) IS IN MILLIMETERS; PSLLIJ, LMLLIJ,
C        AND MCLLIJ NEED METERS.  IS2(5) AND IS2(6) ARE IN
C        TENTHS OF MILLIDEGRESS.
C 
      IF(NPROJ.EQ.3)THEN
         CALL LMLLIJ(KFILDO,ALATL,ALONL,IS2(8)/1000.,ORIENT,XLAT,
     1               REAL(IS2(5)/10000.),REAL(IS2(6)/10000.),
     2               XIFG,YJFG)
      ELSEIF(NPROJ.EQ.5)THEN
         CALL PSLLIJ(KFILDO,ALATL,ALONL,IS2(8)/1000.,ORIENT,XLAT,
     1               REAL(IS2(5)/10000.),REAL(IS2(6)/10000.),
     2               XIFG,YJFG)
      ELSEIF(NPROJ.EQ.7)THEN
         CALL MCLLIJ(KFILDO,ALATL,ALONL,IS2(8)/1000.,XLAT,
     1               REAL(IS2(5)/10000.),REAL(IS2(6)/10000.),
     2               XIFG,YJFG)
      ELSE
         WRITE(KFILDO,146)NPROJ
 146           FORMAT(/' ****MAP PROJECTION NUMBER NPROJ =',I3,
     1           ' NOT 3, 5, OR 7.  FATAL ERROR IN FGSKYA AT',
     2           ' 146.')
         ISTOP(1)=ISTOP(1)+1
         IER=777
         GO TO 900
      ENDIF
C
      CALL NOMINL(KFILDO,IS2(8)/1000000.,MESHI,TRASH,NPROJ,IER)
C        MESHI IS THE GRID INPUT MESH LENGTH.
C        IS2(8) IS IN KM*1000000; MESHI IS IN KM.
C
      IF(IER.NE.0)THEN
         ISTOP(1)=ISTOP(1)+1
         GO TO 900
      ENDIF
C 
      RATIO=FLOAT(MESH)/MESHI
      NXI=NINT((NX-1)*RATIO)+1
      NYI=NINT((NY-1)*RATIO)+1
      NXOFF=NINT(XIFG)-1
      NYOFF=NINT(YJFG)-1
C
CD     WRITE(KFILDO,147)RATIO,MESH,MESHI,NXI,NYI,NXOFF,NYOFF
CD147  FORMAT(/' AT 147 IN FGSKYA--',
CD    1        'RATIO,MESH,MESHI,NXI,NYI,NXOFF,NYOFF',F8.5,6I8)
CD     WRITE(KFILDO,1470)XIFG,YJFG,IS2(3),IS2(4)
CD1470 FORMAT(' AT 1470 IN FGSKYA--XIFG,YJFG,IS2(3),IS2(4)',
CD    1        2F12.5,2I12)
C
      IF(NXOFF.NE.0.OR.NYOFF.NE.0.OR.IS2(3).NE.NXI.
     1                      OR.IS2(4).NE.NYI)THEN
         CALL CUTIT(KFILDO,DATA,IS2(3),IS2(4),NXOFF,NYOFF,
     1              DATA,NXI,NYI,IER)
C           IS2(3) AND IS2(4) ARE THE INPUT GRID DIMENSIONS IN
C           DATA( ).  NXI AND NYI ARE THE OUTPUT GRID DIMENSIONS
C           IN DATA( ).  THERE IS NO NEED TO CALL CUTIT IF 
C           THE INPUT AND OUTPUT GRIDS ARE THE SAME.
      ENDIF
C
      IF(IER.NE.0)THEN
         ISTOP(1)=ISTOP(1)+1
         GO TO 900
      ENDIF
C
      CALL SZGRDM(KFILDO,DATA,NXI,NYI,MESHI,MESH,ITRPX,
     1            IS2(3)*IS2(4))
C        NXI AND NYI ARE THE DIMENSIONS IN OF THE INPUT
C        GRID IN DATA( ).  THEY ARE CHANGED, IF NECESSARY, TO
C        BE THE DIMENSIONS OR THE OUTPUT GRID IN DATA( ).  THEY
C        WILL NOW AGREE WITH NX,NY CALCULATED PREVIOUSLY.
C
      IF(NXI.NE.NX.OR.NYI.NE.NY)THEN
         WRITE(KFILDO,148)NX,NXI,NY,NYI
 148           FORMAT(/' ****NX AND NXI =,',2I6,' OR NY AND NYI =',2I6,
     1           ' DO NOT AGREE AT 148 IN FGSKYA.  FATAL ERROR.')
         ISTOP(1)=ISTOP(1)+1
         IER=777
         GO TO 900
      ENDIF
C
CD           WRITE(KFILDO,149)MESHI,MESH,NXI,NYI,NX,NY
CD149        FORMAT(/' AT 149 IN FGSKYA--MESHI,MESH,NXI,NYI,NX,NY',
CD    1        6I6)
C
C        THE GRID READ IN DATA( ) HAS NOW BEEN PUT ONTO THE SAME
C        GRID AS THE INCOMING ONE IN XDATA( ), SO THE GRIDPOINTS
C        MATCH ONE FOR ONE.
C
C        FIND THE MAX AND MIN PROBABILITY FOR THIS CATEGORY.
C
 1490 DO 150 K=1,NVAL
C
      IF(XDATA(K).GT.9998.9)THEN
C           WHEN THE CATEGORICAL VALUE IS MISSING, THE COMPUTED
C           VALUE WILL BE MISSING.
         FD2(K)=9999.
C
      ELSEIF(DATA(K).LT.9998.)THEN
C           IT IS POSSIBLE THE PROBABILITY OF A CATEGORY IS
C           MISSING, BUT THE CATEGORICAL AMOUNT AND OTHER
C           PROBABILITIES ARE OK.
         IDATA=NINT(XDATA(K))
C
         IF(IDATA.GE.1.AND.IDATA.LE.5)THEN
C            GUARD ABAINST CATEGORICAL AMOUNT BEING OUT OF
C            RANGE.
C
            IF(IDATA.LE.2)THEN
               FD2(K)=FD2(K)+DATA(K)*TABLE(1,J)*FACTOR
            ELSEIF(IDATA.GE.4)THEN
               FD2(K)=FD2(K)+DATA(K)*TABLE(2,J)*FACTOR
            ELSE
               FD2(K)=FD2(K)+
     1            DATA(K)*((TABLE(2,J)+TABLE(1,J))/2.)*FACTOR
            ENDIF
C
         ELSE
            WRITE(KFILDO,1492)IDATA
 1492       FORMAT(/' ****CATEGORICAL CLOUD AMOUNT =,=',I6,
     1              ' OUT OF RANGE 1 THROUGH 5.  SET IT MISSING.',
     2              '  PROCEEDING.')
            ISTOP(1)=ISTOP(1)+1
            FD2(K)=9999.
         ENDIF
C
CCC         WRITE(KFILDO,1495)J,K,IDATA,DATA(K),FD2(K)
CCC 1495    FORMAT(' J,K,IDATA,DATA(K),FD2(K)',3I4,2F10.3)
      ENDIF
C
 150  CONTINUE
C
 200  CONTINUE
C
C        THE COMPUTATIONS WERE IN FD2( ); PUT THEM IN XDATA( ).
C
      DO 210 K=1,NVAL
      XDATA(K)=FD2(K)
 210  CONTINUE
C
CD     WRITE(KFILDO,225)(XDATA(K),K=1,NVAL)
CD225  FORMAT(/,' IN FGSKYA AT 225',/,(15F8.2))
C
 900  RETURN
      END
