      SUBROUTINE TIMGRD(KFILDO,KFIL10,ID,IDPARS,JD,NDATE,
     1                  SDATA,ND1,NSTA,NSLAB,IPACK,IWORK,DATA,ND5,
     2                  LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     3                  IS0,IS1,IS2,IS4,ND7,
     4                  FD1,FD2,ND2X3,
     5                  ISTAV,L3264B,MISTOT,IER)
C
C        SEPTEMBER 2003   GLAHN   TDL   MOS-2000
C        SEPTEMBER 2003   GLAHN   ADDED MISTOT TO CALL
C
C        PURPOSE 
C            TO INTERPOLATE FROM 3-HR GRIDS TO INTERMEDIATE
C            HOURS.  FOR INSTANCE, AVN 3-H FORECASTS CAN BE
C            INTERPOLATED TO HOURLY VALUES FOR LAMP.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               00X XXX - MODEL FORECASTS, ONLY WHEN TAU NOT EVENLY 
C                         DIVISIBLE BY 3 AND DD NE 5    
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C               ID(J) = THE VARIABLE WANTED (J=1,4).  (INPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID( ) (J=1,15).
C                       (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE PORTIONS
C                       PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       JD( ) IS USED TO HELP IDENTIFY THE BASIC MODEL
C                       FIELDS AS READ FROM THE ARCHIVE.  (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C            SDATA(J) = WORK ARRAY.  NOT ACTUALLY USED.  (INTERNAL)
C                 ND1 = DIMENSION OF SDATA( ).  (INPUT)
C                NSTA = NUMBER OF STATIONS.  (NOT ACTUALLY USED.)
C                       (INPUT)
C               NSLAB = THE NUMBER OF THE SLAB IN DIR( , , ) AND
C                       IN NGRIDC( , ) DEFINING THE CHARACTERISTICS
C                       OF THIS GRID.  (OUTPUT) 
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(K) = RETURNED DATA (J=1,ND5).  (OUTPUT)  (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ),IWORK( ), AND DATA( ).
C                       (INPUT)
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
C                              IN NGRIDC( ,L) DEFINING THE CHARACTERISTICS
C                              OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,NPRED) FOR WHICH THIS
C                              VARIABLE IS NEEDED, WHEN IT IS NEEDED ONLY
C                              ONCE FROM LSTORE( , ).  WHEN IT IS NEEDED
C                              MORE THAN ONCE, THE VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING MSTORE( , ).
C                              LATER USED AS A WAY OF DETERMINING WHETHER
C                              TO KEEP THIS VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , ) THAT 
C                       HAVE BEEN USED IN THIS RUN.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA IDENTIFIED IN
C                       LSTORE( , ) (J=1,ND10).  WHEN CORE( ) IS FULL
C                       DATA ARE STORED ON DISK.  (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING  COUNT FROM THE BEGINNING OF THE 
C                       PROGRAM.  THIS COUNT IS MAINTAINED IN CASE THE USER 
C                       NEEDS IT (DIAGNOSTICS, ETC.).  (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       IS2(3) AND IS2(4) ARE USED BY THE CALLING
C                       PROGRAM AS THE GRID DIMENSIONS.  (INTERNAL-OUTPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C              FD1(K) = WORK ARRAY (K=1,ND2X3).  (INTERNAL)
C              FD2(K) = WORK ARRAY (K=1,ND2X3).  (INTERNAL)
C               ND2X3 = DIMENSION OF SEVERAL VARIABLES.  (INPUT)
C               ISTAV = 0 SINCE THE DATA RETURNED ARE GRID DATA.  (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING USED
C                       (EITHER 32 OR 64).  (INPUT)
C              MISTOT = TOTAL NUMBER OF TIMES A MISSING INDICATOR
C                       HAS BEEN ENCOUNTERED IN UNPACKING GRIDS.
C                       (INPUT-OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        52 = NUMBER OF VALUES RETURNED FROM GFETCH FOR
C                             FIRST GRID DOES NOT EQUAL NUMBER OF VALUES
C                             RETURNED FROM THE SECOND GRID OR THE NSLAB
C                             NUMBERS OF THE TWO GRIDS DO NOT MATCH.
C                       103 = IDPARS(1) AND IDPARS(12) DO NOT INDICATE 
C                             FORECASTS FOR WHICH INTERPOLATION IS 
C                             NEEDED.
C                       SEE GFETCH FOR OTHER VALUES.
C                       WHEN IER NE 0, DATA ARE RETURNED AS MISSING.
C                       (INTERNAL-OUTPUT)
C               NSLAB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ).  (INTERNAL) 
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT THE 
C                       RECORD HAS BEEN FETCHED.  THIS IS STORED IN
C                       LSTORE(9, ).  (INTERNAL)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD1( ) (J=1,4).  (INTERNAL)
C               MD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD2( ) (J=1,4).  (INTERNAL)
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED.  (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS ZERO
C                       WHEN DATA ARE NOT PACKED.  (INTERNAL)
C                NXY1 = NUMBER OF WORDS RETURNED IN FD1( ) FOR FIRST
C                       GRID.  (INTERNAL).
C                NXY2 = NUMBER OF WORDS RETURNED IN FD2( ) FOR SECOND
C                       GRID.  (INTERNAL).
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH
C
      DIMENSION ID(4),IDPARS(15),JD(4)
      DIMENSION SDATA(ND1)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION FD1(ND2X3),FD2(ND2X3)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION LD(4),MD(4)
C
C***D     CALL TIMPR(KFILDO,KFILDO,'START TIMGRD        ')
C
      IER=0
      ISTAV=0
C
C        IT IS ASSUMED MODEL FORECASTS ARE AT 3-HR INCREMENTS
C        (STARTING AT 0).  THE VARIABLE IS ACCOMMODATED ONLY
C        FOR CCC = 00X, MOD(IDPARS(12),3) NE 0, AND DD NE 5.
C        DD = 5 IS NOT ACCOMMODATED BECAUSE LAMP FORECASTS ARE
C        ALREADY AT HOURLY INTERVALS.
C
      IF(IDPARS(1)/10.EQ.0.AND.MOD(IDPARS(12),3).NE.0.AND.
     1   IDPARS(4).NE.5)GO TO 115
C
      WRITE(KFILDO,110)(JD(L),L=1,4)
 110  FORMAT(/' ****TIMGRD ENTERED FOR PREDICTOR',
     1          2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2        ' NOT ACCOMMODATED.')
      IER=103
      GO TO 800
C
C        GET THE FIRST 3-HOURLY VALUE.  THE ID INCLUDES THE RUN
C        OFFSET TIME.
C
 115  NTAU=MOD(IDPARS(12),3)
      LD(1)=JD(1)
      LD(2)=JD(2)
      LD(3)=IDPARS(9)*1000000+IDPARS(12)-NTAU
      LD(4)=JD(4)
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2            NXY1,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB1,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GO TO 800
C
C        GET THE SECOND 3-HOURLY VALUE.  THE ID INCLUDES THE RUN
C        OFFSET TIME.
C
      MD(1)=LD(1)
      MD(2)=LD(2)
      MD(3)=LD(3)+3
      MD(4)=LD(4)
      CALL GFETCH(KFILDO,KFIL10,MD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD2,ND2X3,
     2            NXY2,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB2,MISSP,MISSS,L3264B,1,IER)
      IF(MISSP.NE.0)MISTOT=MISTOT+1
      IF(IER.NE.0)GO TO 800
C
      IF(NXY1.NE.NXY2)THEN
         IER=52
         WRITE(KFILDO,215)NXY1,NXY2
 215     FORMAT(/' ****NUMBER OF VALUES IN FIRST GRID = ',I8,
     1           ' DOES NOT EQUAL NUMBER OF VALUES IN SECOND GRID = ',
     2           I8,' IN TIMGRD.  IER = ',I3)
         GO TO 800
      ENDIF
C
      IF(NSLAB1.NE.NSLAB2)THEN
         IER=52
         WRITE(KFILDO,216)NSLAB1,NSLAB2
 216     FORMAT(/' ****NSLAB NUMBER OF OF FIRST GRID = ',I3,
     1           ' DOES NOT EQUAL NSLAB NUMBER OF SECOND GRID = ',
     2           I3,' IN TIMGRD.  IER = ',I3)
         GO TO 800
      ELSE
         NSLAB=NSLAB1
      ENDIF
C
C        COMPUTE THE TIME INTERPOLATED VALUE.
C
      F=FLOAT(NTAU)/3.
C
      DO 220 J=1,NXY1
      DATA(J)=(FD2(J)-FD1(J))*F+FD1(J)
 220  CONTINUE
C
C***      WRITE(KFILDO,225)(DATA(J),J=1,NXY1)
C*** 225  FORMAT(/' DATA( ) IN TIMGRD'/(15F8.1))
      GO TO 900
C
C        SET OUTPUT FIELD TO MISSING WHEN AN ERROR HAS OCCURRED.
C
 800  DO 801 J=1,ND2X3
      DATA(J)=9999.
 801  CONTINUE
C
 900  RETURN
      END      
