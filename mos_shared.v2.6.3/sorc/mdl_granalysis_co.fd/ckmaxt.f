      SUBROUTINE CKMAXT(KFILDO,KFIL10,P,FD6,NX,NY,NAREA,
     1                  ID,IDPARS,LSTORE,ND9,LITEMS,
     2                  NWORDS,NDATE,JDATE,
     3                  MESH,MESHE,NCLIPY,CPNDFD,NXE,NYE,
     4                  ALATL,ALONL,ORIENT,XLAT,NPROJ,
     5                  IS0,IS1,IS2,IS4,ND7,
     6                  IPACK,IWORK,ND5,
     7                  CORE,ND10,NBLOCK,NFETCH,L3264B,ISTOP,IER)
C
C        MARCH     2008   GLAHN   MDL   MOS-2000
C        AUGUST    2008   GLAHN   ADDED COUNTS AND AVERAGES
C        AUGUST    2008   GLAHN   ADDED CHECKING FOR NDFD GRID
C        SEPTEMBER 2008   GLAHN   CORRECTED INDEXING ON IROW( , )
C                                 IN SEVERAL PLACES.
C        SEPTEMBER 2008   GLAHN   ADDED FD6( ) FOR CHANGE GRID
C        SEPTEMBER 2008   GLAHN   REDUCED IROW( , ) TO IROW( )
C        SEPTEMBER 2008   GLAHN   ADDED ALATL,ALONL,ORIENT,XLAT,NPROJ
C        NOVEMBER  2008   COSGROVE CHANGED ASSIGNMENT OF ID FOR 3 HOUR
C                                 SPOT TEMPS TO ALLOW PROJ >100
C        NOVEMBER  2008   GLAHN   FORMAT 145 TEXT; INCREMENTED
C                                 ISTOP(3) WHEN GRID NOT FOUND
C        DECEMBER  2008   GLAHN   REMOVED ISTOP(1)=ISTOP(1)+1 AFTER
C                                 GFETCH
C        MAY       2009   GLAHN   MODIFIED FORMATS 130 AND /D151
C        MAY       2009   GLAHN   MAJOR OVERHAUL TO MORE ACCURATELY
C                                 COMPUTE THE AVERAGE CHANGES
C
C        PURPOSE 
C            TO CHECK A SURFACE DAYTIME MAXIMUM TEMPERATURE ANALYSIS
C            IN P( , ) WITH UP TO 5 SURFACE TEMPERATURE ANALYSES.
C            THE MAX TEMP AT EVERY GRIDPOINT IS MADE AS GREAT AS ANY
C            OF THE TEMPERATURE ANANLYSES CHECKED.  THE TEMPERATURE
C            ANALYSES ARE IN INTERNAL STORAGE AND ARE READ INTO AN
C            AUTOMATIC ARRAY TEMP(NX,NY,IPROJ).
C
C            CORRECTING MAX TEMPERATURE WITH TEMPERATURE MAY CREATE
C            PROBLEMS.  MAX TEMPERATURE HAS MORE FORECASTS, BUT
C            SOME MAY BE IN ERROR BECAUSE OF THE CO-OP REPORTING
C            PROCEDURES.  ADJUSTING THE INDIVIDUAL TEMPERATURE
C            ANALYSES IS MORE COMPLEX, AND THEY HAVE ALREADY BEEN
C            WRITTEN OUT ONCE.  OVERWRITING ON RA FILE IS NO PROBLEM,
C            BUT WRITING SEQUENTIAL WOULD REQUIRE ANOTHER ID.
C
C            THE TIME ZONES ARE APPROXIMATE IN THE CONUS, THE 
C            WESTERN EXTREMITY OF THE
C               EASTERN TIME ZONE = 86.5 DEG,
C               CENTRAL TIME ZONE = 104 DEG, AND
C               WESTERN TIME ZONE = 115 DEG W, ALL AT 45 DEG N.
C            TO BE MORE EXACT, THERE WOULD HAVE TO BE A TIME ZONE
C            GRID MASK.  CKMAXT SHOULD WORK FOR DIFFERENT GRID
C            RESOLUTIONS, MESH.  IT SHOULD WORK FOR PUERTO RICO,
C            BUT THE HAWAII GRID HAS YET TO BE DEFINED AND WILL
C            LIE IN TWO TIME ZONES.
C
C            A RETURNED GRID IN FD6( , ) WILL CONTAIN THE CORRECTIONS
C            MADE IN DEG F *10.  THESE WILL BE POSITIVE.
C
C            CALLED WITH  CCCFFF = 222120 MAX TEMP.
C            ASSUMES TEMP CCCFFF = 222020 2-M TEMP.
C            THIS WORKS FOR MOS AND EKDMOS
C
C**********************************************************************
C            NOTE:  THIS HAS NOT BEEN COMPLETED FOR HAWAII OR PUERTO
C                   RICO.  AREAS NOT YET DEFINED.
C**********************************************************************
C
C        DATA SET USE 
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C           KFIL10 - UNIT NUMBER FOR RANDOM FILE ACCESS.  (INPUT)
C
C        VARIABLES 
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C              KFIL10 = THE UNIT NUMBER FOR RANDOM FILE ACCESS.  (INPUT)
C            P(IX,JY) = THE MAX TEMP ANALYSIS TO CHECK (IX=1,NX)
C                       (JY=1,NY).  (INPUT/OUTPUT)
C          FD6(IX,JY) = HOLDS THE CHANGES MADE TO P( , )
C                       (IX=1,NX) (JY=1,NY).  (OUTPUT)
C                  NX = X-EXTENT OF P( , ). (INPUT)
C                  NY = Y-EXTENT OF P( , ). (INPUT)
C               NAREA = THE AREA OVER WHICH THE ANALYSIS IS MADE:
C                       1 = CONUS,
C                       2 = ALASKA,
C                       3 = HAWAII,
C                       4 = PUERTO RICO.
C               ID(J) = THE 4-WORD ID OF THE VARIABLE MAX TEMP GRID
C                       (J=1,4).  (INPUT)
C         IDPARS(J,N) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       VARIABLE ID'S CORRESPONDING TO ID( ,N)
C                       (J=1,15), (N=1,ND4).  (INPUT)
C                       J=12--TAU (PROJECTION IN HOURS).
C         LSTORE(L,J) = THE ARRAY TO HOLD INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS).  (INPUT)
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT ARE FILLED.  (INPUT)  
C              NWORDS = NUMBER OF WORDS RETURNED FROM GFETCH.
C                       (INTERNAL)
C               NDATE = DATE/TIME OF THE DATA PROCESSED IN FORMAT
C                       YYYYMMDDHH.  (INPUT)
C            JDATE(J) = NDATE PARSED INTO ITS 4 COMPONENTS:
C                       J=1 IS YYYY
C                       J=2 IS MM
C                       J=3 IS DD
C                       J=4 IS HH
C                       (INPUT)
C                MESH = THE NOMINAL MESH LENGTH OF THE GRID BEING DEALT
C                       WITH WHOSE DIMENSIONS ARE NX AND NY.  (INTERNAL)
C               MESHE = THE NOMINAL MESH LENGTH OF CLIPPING GRID
C                       CPNDFD( , )  (INPUT)
C              NCLIPY = 1 WHEN THE NDGD MASK GRID IS AVAILABLE AND 
C                         IN CPNDFD( ).
C                       0 OTHERWISE.
C                       (INPUT)
C       CPNDFD(IX,JY) = THE NDFD MASK FROM THE MOS-2000 EXTERNAL
C                       RANDOM ACCESS FILE (IX=1,NXE) (JY=1,NYE) AT
C                       NOMINAL MESHLENGTH MESHE.  (INPUT)
C                 NXE = X-EXTENT OF CPNDFD( ) AT MESH LENGTH MESHE.
C                       (INPUT)
C                 NYE = Y-EXTENT OF CPNDFD( ) AT MESH LENGTH MESHE.
C                       (INPUT)
C               ALATL = LATITUDE IN DEGREES OF THE LOWER LEFT CORNER
C                       POINT (1,1) OF THE ANALYSIS GRID.  NOTE
C                       THAT THIS REMAINS CONSTANT FOR ALL GRIDS
C                       AFTER THE INPUT GRID IS POSITIONED.  IT DOES
C                       NOT PERTAIN TO THE DISPOSABLE GRID.  (INPUT)
C               ALONL = LONGITUDE (WEST) IN DEGREES OF THE LOWER LEFT
C                       CORNER POINT OF THE ANALYSIS GRID.  NOTE
C                       THAT THIS REMAINS CONSTANT FOR ALL GRIDS
C                       AFTER THE INPUT GRID IS POSITIONED.  (INPUT)
C              ORIENT = ORIENTATION W LONGITUDE, PARALLEL TO GRID
C                       COLUMNS, IN DEGREES.  (INPUT)
C                XLAT = N LATITUDE AT WHICH THE MESH LENGTH APPLIES.
C                       (INPUT)
C               NPROJ = NUMBER OF MAP PROJECTION TO WHICH THIS GRID
C                       APPLIES.
C                       3 = LAMBERT.
C                       5 = POLAR STEREOGRAPHIC.
C                       7 = MERCATOR.
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
C       ITABLE(M,N,L) = TABLE OF IPROJ PROJECTIONS OF 3-H TEMP TO
C                       CHECK WITH MAX TEMP (M=1,IPROJ) FOR EACH OF 
C                       ITZONE TIME ZONES (N=1,ITZONE) FOR EITHER
C                       STANDARD (L=1) OR DAYLIGHT SAVINGS TIME (L=2).
C                       (INTERNAL)
C               IPROJ = THE MAXIMUM NUMBER OF 3-H TEMP VALUES TO CHECK
C                       WITH THE MAX FOR ALL TIME ZONES FOR ANY ONE
C                       MAX TEMP PROJECTION. (SET BY PARAMETER TO 7)
C                       (INTERNAL) 
C              ITZONE = THE TOTAL TIME ZONES THAT CAN BE INVOLVED:
C                       1 FOR PUERTO RICO,
C                       4 FOR CONUS,
C                       1 FOR ALASKA,
C                       2 FOR HAWAII.
C                       (SET BY PARAMETER TO 8)  (INTERNAL)
C             IROW(K) = THE FIRST ROW IN IUSE USED FOR EACH OF
C                       THE AREAS (K=1,4).  DEFINED IN DATA STATEMENT.
C                       (INTERNAL)
C         ICOL(K,I,L) = THE FIRST (I=1) AND LAST (I=2) COLUMNS IN
C                       ITABLE( , , ) FOR STANDARD (L=1) AND DAYLIGHT
C                       SAVINGS TIME (L=2) FOR EACH AREA OF 4 AREAS
C                       (K=1,4) NEEDED.  THIS WILL ALSO BE THE COLUMNS
C                       IN TEMP( , , ) THAT ARE FILLED WITH DATA FROM
C                       INTERNAL STORAGE FOR CHECKING.  (INTERNAL)
C         IUSE(K,I,L) = THE FIRST (I=1) AND LAST (I=2) COLUMNS OF DATA
C                       IN TEMP( , , ) ACTUALLY USED IN CHECKING FOR
C                       8 TIME ZONES (K=1,8), FOR STANDARD (L=1) AND
C                       DAYLIGHT SAVINGS TIME (L=2).  (INTERNAL)
C                  IT = 1 FOR STANDARD TIME,
C                       2 FOR DAYLIGHT TIME.
C                       (INTERNAL)
C                  IC = SET TO 00 WHEN CYCLE IS 00 OR 06, AND TO 12 WHEN
C                       CYCLE IS 12 OR 18.  (INTERNAL)
C              ICOUNT = NUMBER OF TIMES MAX WAS CORRECTED.  (INTERNAL)
C              DIFMAX = MAXIMUM DIFFERENCE CORRECTED.  (INTERNAL)
C              DIFAVG = AVERAGE DIFFERENCE CORRECTED.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C                         
C        NON SYSTEM SUBROUTINES CALLED 
C           NONE
C
      PARAMETER (IPROJ=7)
      PARAMETER (ITZONE=8)
C
      DIMENSION ID(4),IDPARS(15)
      DIMENSION P(NX,NY),FD6(NX,NY)
      DIMENSION TEMP(NX,NY,IPROJ)
C        TEMP( , , ) IS AN AUTOMATIC ARRAY.
      DIMENSION CPNDFD(NXE,NYE)
      DIMENSION IPACK(ND5),IWORK(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION LD(4),ISTOP(6),JDATE(4)
      DIMENSION ITABLE(IPROJ,ITZONE,2),IROW(4),ICOL(4,2,2),IUSE(8,2,2)
C
      DATA ITABLE/12, 15, 18, 21, 99, 99, 99,
     1            12, 15, 18, 21, 24, 99, 99,
     2            99, 15, 18, 21, 24, 99, 99,
     3            99 ,15, 18, 21, 24, 99, 99,
     4            99, 15, 18, 21, 24, 27, 99,
     5            99, 99, 18, 21, 24, 27, 99,
     6            99, 99, 18, 21, 24, 27, 99,
     7            99, 99, 18, 21, 24, 27, 30,
C
     8            12, 15, 18, 21, 99, 99, 99,
     9            12, 15, 18, 21, 99, 99, 99,
     A            12, 15, 18, 21, 24, 99, 99,
     B            99, 15, 18, 21, 24, 99, 99,
     C            99, 15, 18, 21, 24, 99, 99,
     D            99, 15, 18, 21, 24, 99, 99,
     E            99, 99, 18, 21, 24, 27, 99,
     F            99, 99, 18, 21, 24, 27, 99/
      DATA IROW/2, 6, 7, 1/
      DATA ICOL/1, 3, 3, 1,
     1          6, 6, 7, 4,
     2          1, 2, 3, 1,
     3          5, 6, 6, 4/
      DATA IUSE/1, 1, 2, 2, 2, 3, 3, 3,
     1          4, 5, 5, 5, 6, 6, 6, 7,
     2          1, 1, 1, 2, 2, 2, 3, 3,
     3          4, 4, 5, 5, 5, 6, 6, 6/
C
      IER=0
C
      IF(MESHE.NE.MESH)THEN
         WRITE(KFILDO,103)MESHE,MESH
 103     FORMAT(/,' ****MESHE =',I4,' NOT EQUAL TO MESH =',I4,
     1            ' IN CKMAXT.  DIFFERENCES NOT COMPUTED.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 800
      ENDIF
C
      IF(NCLIPY.EQ.0)THEN
         WRITE(KFILDO,104)NCLIPY
 104     FORMAT(/,' ****CLIPPING GRID NOT AVAILABLE IN CKMAXT.',
     1            '  NCLIPY =',I6,'.  DIFFERENCES NOT COMPUTED.')
         ISTOP(1)=ISTOP(1)+1
         GO TO 800
      ENDIF
C
      X1=1
      X2=1
      X3=1
C        X1, X2, X3 SET FOR SAFETY AND PRINT.
C
C        CHECK THE CCCFFF = 222120 FOR MAX TEMP.
C
      IF(ID(1)/1000.NE.222120)THEN
         WRITE(KFILDO,105)(ID(J),J=1,4)
 105     FORMAT(/' ****CCCFFF NOT CORRECT IN CKMAXT.  ID = ',
     1            3I10.9,I5.3,
     2           '.  CHECKING OF MAX TEMPERATURE GRID ABORTED.')
         GO TO 800
      ENDIF
C
C        CHECK COMPATIBILITY OF MAP PROJECTION AND AREA.  IT IS 
C        ASSUMED THE CONUS IS LAMBERT, ALASKA IS PS, AND HAWAII
C        AND PUERTO RICO ARE MERCATOR.
C
      IF((NAREA.EQ.1.AND.NPROJ.NE.3).OR.
     1   (NAREA.EQ.2.AND.NPROJ.NE.5).OR.
     2   (NAREA.EQ.3.AND.NPROJ.NE.7).OR.
     3   (NAREA.EQ.4.AND.NPROJ.NE.7))THEN
         WRITE(KFILDO,106)NAREA,NPROJ,(ID(J),J=1,4)
 106     FORMAT(/' ****NAREA =',I2,' AND NPROJ =',I2,
     1           ' NOT COMPATIBLE.  ID = ',
     2            3I10.9,I5.3,
     3           '.  CHECKING OF MAX TEMPERATURE GRID ABORTED.')
         GO TO 800
      ENDIF
C
C        GET THE ACTUAL GRID LENGTH FROM MESH.         
C
      CALL ACTUAL(KFILDO,MESH,XMESH,TRASH,NPROJ,IER)
      XMESH=XMESH*1000.
C        XMESH IS NEEDED BELOW IN METERS, NOT KM.
C
CD     WRITE(KFILDO,107)MESH,XMESH
CD107  FORMAT(/' AT 107 IN CKMAXT--MESH,XMESH',I4,F12.5)
C 
C        FIND WHETHER THE TIME IS STANDARD OR DAYLIGHT.
C        USE IT =1 FOR STANDARD AND IT =2 FOR DAYLIGHT.
C        AS A CRUDE SWITCH, ASSUME APRIL THROUGH OCTOBER
C        IS DAYLIGHT SAVINGS TIME.  THE TIME ZONES AND
C        STANDARD/DAYLIGHT SAVINGS SWITCH DO NOT HAVE TO
C        BE KNOWN ACCURATELY, ESPECIALLY WITH ONLY THE
C        3-HOURLY RESOLUTION OF THE TEMPERATURE DATA.
C
      IF(JDATE(2).LE.3.OR.JDATE(2).GE.10)THEN
         IT=1
      ELSE
         IT=2
      ENDIF
C
C        DEFINE THE CYCLE IC = 00 FOR 00Z AND 12 FOR 12Z.
C
      IC=(JDATE(4)/12)*12
C        IC WILL BE EITHER 00 OR 12 AND WILL TREAT 06Z LIKE 00,
C        AND 18Z LIKE 12.
C
C        GET THE 3-H TEMPERATURE ANALYSES.
C     
      DO 135 M=ICOL(NAREA,1,IT),ICOL(NAREA,2,IT)
C        FOR EACH AREA, THERE WILL BE UP TO 5 PROJECTIONS OF
C        3-H TEMP TO CHECK.
      LD(1)=ID(1)-100000
C        THIS MAKES THE MAX TEMP FFF OF 120 = 020 FOR 3-H TEMP
      LD(2)=ID(2)
      LD(3)=(ID(3)/1000)*1000+12+(M-1)*3+((IDPARS(12)/24)-1)*24+IC
C        ITABLE HAS 3-H PROJECTIONS FOR 30-H MAX.  THEY MUST
C        BE INCREASED BY 24 HOURS EVERY 24-H CYCLE INDICATED
C        BY IDPARS(12) OF THE MAX TEMP.
      LD(4)=ID(4)
      ITIME=0
C
CD     WRITE(KFILDO,110)(LD(J),J=1,4),M
CD110  FORMAT(/,' AT 110 IN CKMAXT--(LD(J),J=1,4),M',4I10,I4)
C
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,TEMP(1,1,M),NX*NY,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSOURC,MISSP,MISSS,L3264B,ITIME,
     4            IER)
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,130)(LD(J),J=1,4)
 130     FORMAT(/,' ****TEMPERATURE NOT RETRIEVED BY GFETCH IN CKMAXT',
     1            2X,3(I10.9),1X,I10.3,/,
     2            '     THIS GRID NOT AVAILABLE FOR CHECKING',
     3            ' WITH MAX TEMP.  MAY NOT BE ONE NEEDED.')
         ISTOP(3)=ISTOP(3)+1
C
      ELSEIF(NWORDS.NE.NX*NY)THEN
         WRITE(KFILDO,131)NWORDS,NX*NY,(LD(J),J=1,4)
 131     FORMAT(/,' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1            ' NOT EQUAL TO NX*NY =',I6,' IN CKMAXT FOR.',
     2            2X,3(I10.9),1X,I10.3,/,
     3            '     THIS GRID NOT AVAILABLE FOR CHECKING',
     4            ' WITH MAX TEMP.  MAY NOT BE ONE NEEDED.')
         ISTOP(1)=ISTOP(1)+1
      ENDIF
C
 135  CONTINUE
C   
C        INITIALIZE CHANGE GRID.
C 
      DO 140 JY=1,NY
      DO 139 IX=1,NX
      FD6(IX,JY)=0.
 139  CONTINUE
 140  CONTINUE
C  
C        CHECK GRIDS.  WHEN 3-H TEMP EXCEEDS MAX TEMP, SET MAX TEMP TO
C        TEMPERATURE.  MULTIPLE LOOPS AND SOME HARDWIRING KEEP DOWN
C        COMPLEXITY.
C
C        THIS LOOP IS FOR CONUS.
C
      IF(NAREA.EQ.1)THEN
C
C           CONUS, EASTERN TIME ZONE
C
         IF(NPROJ.EQ.3)THEN
            CALL LMLLIJ(KFILDO,40.,86.5,XMESH,ORIENT,XLAT,
     1                  ALATL,ALONL,X1,YJ)
         ELSEIF(NPROJ.EQ.5)THEN
            CALL PSLLIJ(KFILDO,40.,86.5,XMESH,ORIENT,XLAT,
     1                  ALATL,ALONL,X1,YJ)
         ELSEIF(NPROJ.EQ.7)THEN
            CALL MCLLIJ(KFILDO,40.,86.5,XMESH,XLAT,
     1                  ALATL,ALONL,X1,YJ)
         ENDIF
C
         DO 150 ICK=IUSE(IROW(NAREA),1,IT),IUSE(IROW(NAREA),2,IT)
C
CD        WRITE(KFILDO,144)NAREA,IC,IT,IROW(NAREA)
CD144     FORMAT(' IN CKMAXT--NAREA,IC,IT,IROW(NAREA)',
CD    1          5I6)
CD        WRITE(KFILDO,145)ICK,NINT(X1),NINT(X2),NINT(X3)
CD145     FORMAT(' IN CKMAXT--ICK,NINT(X1),NINT(X2),NINT(X3)',4I4)
C
         DO 149 IX=NINT(X1),NX
         DO 148 JY=1,NY
C
         IF(P(IX,JY).LT.9998.5)THEN     
C
            IF(TEMP(IX,JY,ICK).LT.9998.5)THEN
               PPP=P(IX,JY)
C
               IF(TEMP(IX,JY,ICK).GT.PPP)THEN
C
                  IF(CPNDFD(IX,JY).NE.0.)THEN
C                       DIFFERENCES ARE COMPUTED ONLY OVER THE NDFD
C                       GRID.
                     DIF=TEMP(IX,JY,ICK)-PPP
                     FD6(IX,JY)=MAX(FD6(IX,JY),DIF)
CD                    IF(DIF.GT.+8.)WRITE(KFILDO,146)DIF,IX,JY,ICK
CD146                 FORMAT(/,'****LARGE MAX TEMP CHANGE--',
CD    1                        'DIF,IX,JY,ICK',F6.1,3I6)
                  ENDIF
C
               ENDIF
C
            ENDIF
C
         ENDIF
C
 148     CONTINUE
 149     CONTINUE
 150     CONTINUE
C
C           CONUS, CENTRAL TIME ZONE
C
         IF(NPROJ.EQ.3)THEN
            CALL LMLLIJ(KFILDO,40.,104.,XMESH,ORIENT,XLAT,
     1                  ALATL,ALONL,X2,YJ)
         ELSEIF(NPROJ.EQ.5)THEN
            CALL PSLLIJ(KFILDO,40.,104.,XMESH,ORIENT,XLAT,
     1                  ALATL,ALONL,X2,YJ)
         ELSEIF(NPROJ.EQ.7)THEN
            CALL MCLLIJ(KFILDO,40.,104.,XMESH,XLAT,
     1                  ALATL,ALONL,X2,YJ)
         ENDIF
C
         DO 160 ICK=IUSE(IROW(NAREA)+1,1,IT),
     1              IUSE(IROW(NAREA)+1,2,IT)
C
CD        WRITE(KFILDO,145)ICK,NINT(X1),NINT(X2),NINT(X3)
C
         DO 159 IX=NINT(X2),NINT(X1)-1
         DO 158 JY=1,NY
C
         IF(P(IX,JY).LT.9998.5)THEN     
C
            IF(TEMP(IX,JY,ICK).LT.9998.5)THEN
               PPP=P(IX,JY)
C
               IF(TEMP(IX,JY,ICK).GT.PPP)THEN
C
                  IF(CPNDFD(IX,JY).NE.0.)THEN
C                       DIFFERENCES ARE COMPUTED ONLY OVER THE NDFD
C                       GRID.
                     DIF=TEMP(IX,JY,ICK)-PPP
                     FD6(IX,JY)=MAX(FD6(IX,JY),DIF)
CD                    IF(DIF.GT.+8.)WRITE(KFILDO,146)DIF,IX,JY,ICK
                  ENDIF
C
               ENDIF
C
            ENDIF
C
         ENDIF
 158     CONTINUE
 159     CONTINUE
 160     CONTINUE
C
C           CONUS, MOUNTAIN TIME ZONE
C
         IF(NPROJ.EQ.3)THEN
            CALL LMLLIJ(KFILDO,40.,115.,XMESH,ORIENT,XLAT,
     1                  ALATL,ALONL,X3,YJ)
         ELSEIF(NPROJ.EQ.5)THEN
            CALL PSLLIJ(KFILDO,40.,115.,XMESH,ORIENT,XLAT,
     1                  ALATL,ALONL,X3,YJ)
         ELSEIF(NPROJ.EQ.7)THEN
            CALL MCLLIJ(KFILDO,40.,115.,XMESH,XLAT,
     1                  ALATL,ALONL,X3,YJ)
         ENDIF
C
         DO 170 ICK=IUSE(IROW(NAREA)+2,1,IT),
     1              IUSE(IROW(NAREA)+2,2,IT)
C
CD        WRITE(KFILDO,145)ICK,NINT(X1),NINT(X2),NINT(X3)
C
         DO 169 IX=NINT(X3),NINT(X2)-1
         DO 168 JY=1,NY
C
         IF(P(IX,JY).LT.9998.5)THEN     
C
            IF(TEMP(IX,JY,ICK).LT.9998.5)THEN
               PPP=P(IX,JY)
C
               IF(TEMP(IX,JY,ICK).GT.PPP)THEN
C
                  IF(CPNDFD(IX,JY).NE.0.)THEN
C                       DIFFERENCES ARE COMPUTED ONLY OVER THE NDFD
C                       GRID.
                     DIF=TEMP(IX,JY,ICK)-PPP
                     FD6(IX,JY)=MAX(FD6(IX,JY),DIF)
CD                    IF(DIF.GT.+8.)WRITE(KFILDO,146)DIF,IX,JY,ICK
                  ENDIF
C
               ENDIF
C
            ENDIF
C
         ENDIF
 168     CONTINUE
 169     CONTINUE
 170     CONTINUE
C
C           CONUS, PACIFIC TIME ZONE
C
         DO 180 ICK=IUSE(IROW(NAREA)+3,1,IT),
     1              IUSE(IROW(NAREA)+3,2,IT)
C
CD        WRITE(KFILDO,145)ICK,NINT(X1),NINT(X2),NINT(X3)
C
         DO 179 IX=1,NINT(X3)-1
         DO 178 JY=1,NY
C
         IF(P(IX,JY).LT.9998.5)THEN     
C
            IF(TEMP(IX,JY,ICK).LT.9998.5)THEN
               PPP=P(IX,JY)
C
               IF(TEMP(IX,JY,ICK).GT.PPP)THEN
C
                  IF(CPNDFD(IX,JY).NE.0.)THEN
C                       DIFFERENCES ARE COMPUTED ONLY OVER THE NDFD
C                       GRID.
                     DIF=TEMP(IX,JY,ICK)-PPP
                     FD6(IX,JY)=MAX(FD6(IX,JY),DIF)
CD                    IF(DIF.GT.+8.)WRITE(KFILDO,146)DIF,IX,JY,ICK
                  ENDIF
C
               ENDIF
C
            ENDIF
C
         ENDIF
 178     CONTINUE
 179     CONTINUE
 180     CONTINUE
C
C           ALASKA, ALASKA TIME ZONE.  
C
      ELSEIF(NAREA.EQ.2)THEN
C           BECAUSE ONLY 1 TIME ZONE IS INVOLVED, LOOPS
C           ARRANGED FOR EFFICIENCY.
C
         DO 190 ICK=IUSE(IROW(NAREA),1,IT),IUSE(IROW(NAREA),2,IT)
C
CD        WRITE(KFILDO,145)ICK,NINT(X1),NINT(X2),NINT(X3)
C
         DO 189 JY=1,NY
         DO 188 IX=1,NX
C
         IF(P(IX,JY).LT.9998.5)THEN     
C
            IF(TEMP(IX,JY,ICK).LT.9998.5)THEN
               PPP=P(IX,JY)
C
               IF(TEMP(IX,JY,ICK).GT.PPP)THEN
C
                  IF(CPNDFD(IX,JY).NE.0.)THEN
C                       DIFFERENCES ARE COMPUTED ONLY OVER THE NDFD
C                       GRID.
                     DIF=TEMP(IX,JY,ICK)-PPP
                     FD6(IX,JY)=MAX(FD6(IX,JY),DIF)
CD                    IF(DIF.GT.+8.)WRITE(KFILDO,146)DIF,IX,JY,ICK
                  ENDIF
C
               ENDIF
C
            ENDIF
C
         ENDIF
 188     CONTINUE
 189     CONTINUE
 190     CONTINUE
C
C           THIS LOOP IS FOR HAWAII.
C           (AREA NOT YET DEFINED; WILL PROBABLY NEED TWO LOOPS
C           FOR 2 TIME ZONES.)  (CAREFUL--ARRANGEMENT OF LOOPS)
C
      ELSEIF(NAREA.EQ.3)THEN
C
         DO 200 IX=IUSE(IROW(NAREA),1,IT),IUSE(IROW(NAREA),2,IT)
C
CD        WRITE(KFILDO,145)ICK,NINT(X1),NINT(X2),NINT(X3)
C
         DO 199 ICK=1,5
         DO 198 JY=1,NY
C
         IF(P(IX,JY).LT.9998.5)THEN     
C
            IF(TEMP(IX,JY,ICK).LT.9998.5)THEN
               PPP=P(IX,JY)
C
               IF(TEMP(IX,JY,ICK).GT.PPP)THEN
C
                  IF(CPNDFD(IX,JY).NE.0.)THEN
C                       DIFFERENCES ARE COMPUTED ONLY OVER THE NDFD
C                       GRID.
                     DIF=TEMP(IX,JY,ICK)-PPP
                     FD6(IX,JY)=MAX(FD6(IX,JY),DIF)
CD                    IF(DIF.GT.+8.)WRITE(KFILDO,146)DIF,IX,JY,ICK
                  ENDIF
C
               ENDIF
C
            ENDIF
C
         ENDIF
 198     CONTINUE
 199     CONTINUE
 200     CONTINUE
C
C           PUERTO RICO, ATLANTIC TIME ZONE.
C
      ELSEIF(NAREA.EQ.4)THEN
C
         DO 220 ICK=IUSE(IROW(NAREA),1,IT),IUSE(IROW(NAREA),2,IT)
C
CD        WRITE(KFILDO,145)ICK,NINT(X1),NINT(X2),NINT(X3)
C
         DO 219 JY=1,NY
         DO 218 IX=1,NX
C
         IF(P(IX,JY).LT.9998.5)THEN     
C
            IF(TEMP(IX,JY,ICK).LT.9998.5)THEN
               PPP=P(IX,JY)
C
               IF(TEMP(IX,JY,ICK).GT.PPP)THEN
C
                  IF(CPNDFD(IX,JY).NE.0.)THEN
C                       DIFFERENCES ARE COMPUTED ONLY OVER THE NDFD
C                       GRID.
                     DIF=TEMP(IX,JY,ICK)-PPP
                     FD6(IX,JY)=MAX(FD6(IX,JY),DIF)
CD                    IF(DIF.GT.+8.)WRITE(KFILDO,146)DIF,IX,JY,ICK
                  ENDIF
C
               ENDIF
C
            ENDIF
C
         ENDIF
 218     CONTINUE
 219     CONTINUE
 220     CONTINUE
C
      ENDIF
C  
      ICOUNT=0
C        ICOUNT COUNTS THE NUMBER OF GRIDPOINT CORRECTIONS.
      JCOUNT=0
C        JCOUNT COUNTS THE NUMBER OF NDFD POINTS.
      DIFAVG=0.
C        DIFAVG ACCUMULATES THE CORRECTIONS.
      DIFMAX=0.
C        DIFMAX ACCUMULATES THE MAXIMUM CORRECTION.
C
      DO 230 JY=1,NY
      DO 229 IX=1,NX
C
      IF(CPNDFD(IX,JY).NE.0)THEN
         JCOUNT=JCOUNT+1
C       
         IF(FD6(IX,JY).GT.0.)THEN
            ICOUNT=ICOUNT+1
            DIFAVG=DIFAVG+FD6(IX,JY)
            DIFMAX=MAX(DIFMAX,FD6(IX,JY))
            P(IX,JY)=P(IX,JY)+FD6(IX,JY)
         ENDIF
C
      ELSE
         FD6(IX,JY)=9999.
C           THE DIFFERENCE GRID IS SET MISSING OUTSIDE THE NDFD GRID.
      ENDIF
C
 229  CONTINUE
 230  CONTINUE
C
      IF(ICOUNT.GT.0)THEN
         DIFAVG=DIFAVG/ICOUNT
         IPERCT=NINT((REAL(ICOUNT)/REAL(JCOUNT))*100.)
      ENDIF
C
      WRITE(KFILDO,255)ICOUNT,IDPARS(12),IPERCT,DIFAVG,DIFMAX
 255  FORMAT(/' MAX TEMP GRID WAS INCREASED BY TEMPERATURE',I8,' TIMES',
     1        ' FOR PROJECTION',I4,
     2        '   PERCENT OF NDFD POINTS CHANGED =',I4,':'/
     3        '    AVERAGE CORRECTION =',F5.1,';',
     4        '  MAXIMUM CORRECTION  =',F5.1)
C
 800  CONTINUE
C
      RETURN
      END
