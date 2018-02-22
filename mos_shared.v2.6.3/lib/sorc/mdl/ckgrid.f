      SUBROUTINE CKGRID(KFILDO,ID,NPROJ,ORIENT,XLAT,IS2,ND7,IER)
C
C        NOVEMBER  2004   GLAHN   MDL   MOS-2000
C                                 ADAPTED FROM CHKGRD
C        AUGUST    2005   GLAHN   MODIFIED FOR 4-KM HRAP AND 32- AND
C                                 4-KM ETA
C        SEPTEMBER 2007   GLAHN   REMOVED FLOAT IN DO 150 LOOP
C
C        PURPOSE
C            TO CHECK THE MAP PROJECTION, ORIENTATION, AND LATITUDE
C            OF MESH LENGTH SPECIFICATION FOR A GRID WHOSE 
C            CHARACTERISTICS ARE IN IS2( ).  THE MESH LENGTH 
C            IS EXPECTED TO BE IN INCREMENTS OF BEDIENTS DIVIDED 
C            BY SOME POWER OF 2.  THIS VARIES BY MAP PROJECTION.
C            BECAUSE THE FULL RESOLUTION IS NOT CARRIED IN TDLPACK,
C            SOME TOLERANCE IS ALLOWED.  THIS MAKES EXACT CHECKING
C            POSSIBLE ONLY TO A CERTAIN PRECISION.
C
C        DATA SET USE
C            KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C          INPUT
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C               ID(J) = 4-WORD OF VARIABLE BEING CHECKED (J=1,4).
C                       (INPUT)
C               NPROJ = NUMBER OF MAP PROJECTION BEING USED.
C                       3 = LAMBERT.
C                       5 = POLAR STEREOGRAPHIC.
C                       7 = MERCATOR.
C                       (INPUT)
C              ORIENT = ORIENTATION W LONGITUDE, PARALLEL TO GRID
C                       COLUMNS, IN DEGREES OF THE GRID BEING USED.
C                       (INPUT)
C                XLAT = LATITUDE AT WHICH GRID LENGTH APPLIES.
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INPUT)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       (INPUT)
C                 IER = ERROR CODE. 
C                       199 = GRID DID NOT CHECK.  
C                         0 = OTHERWISE.
C                       (OUTPUT)
C         XMSTAB(I,J) = THE ALLOWABLE GRIDLENGTHS FOR THREE MAP
C                       PROJECTIONS (I=1,3) (J=1,9).  THESE ARE IN FULL
C                       RESOLUTION STARTING FROM A DEFINITION OF
C                       "BEDIENT" ON EACH MAP PROJECTION.   A BEDIENT
C                       WAS INITIALLY DEFINED BY NMC FOR A POLAR
C                       STEREOGRAPHIC MAP PROJECTION, AND HAS BEEN 
C                       EXTENDED TO LAMBERT AND MERCATOR.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED
C
      DIMENSION ID(4)
      DIMENSION IS2(ND7)
      DIMENSION XMSTAB(3,11)
C
      DATA XMSTAB/325.082,      381.,           320.,
     1            162.541,      190.5,          160.,
     2             81.2705,      95.25,          80.,
     3             40.63525,     47.625,         40.,
     4             20.317625,    23.8125,        20.,
     5             10.1588125,   11.90625,       10.,
     6              5.07940625,   5.953125,       5.,
     7              2.539703125,  2.9765625,      2.2,
     8              1.2698515625, 1.48828125,     1.25,
     9             32.46341,      4.762,          0.,
     A              4.057926,     0.,             0./
C
      IER=0
C
      IF(IS2(2).NE.NPROJ)THEN
         WRITE(KFILDO,110)IS2(2),NPROJ,ID
 110     FORMAT(/' ****MAP PROJECTION NO. =',I3,
     1           ' NOT EXPECTED =',I3,/
     2           '     FOR VARIABLE',3I10.0,I10,' IN CKGRID AT 110.')
         IER =199
      ENDIF
C
      IF(IS2(9).NE.NINT(XLAT*10000.))THEN
         WRITE(KFILDO,120)IS2(9),XLAT,ID
 120     FORMAT(/' ****LATITUDE OF MESH LENGTH*10000. =',I10,
     1           ' NOT EXPECTED =',F10.0,/,
     2           '     FOR VARIABLE',3I10.9,I10,' IN CKGRID AT 120.')
         IER=199
      ENDIF
C
      IF(IS2(7).NE.NINT(ORIENT*10000.))THEN
         WRITE(KFILDO,130)IS2(7),ORIENT,ID
 130     FORMAT(/' ****LONGITUDE OF ORIENTATION*10000. =',I10,
     1           ' NOT EXPECTED =',F10.0,/,
     2           '     FOR VARIABLE',3I10.9,I10,' IN CKGRID AT 130.')
         IER=199
       ENDIF
C
C        CHECK GRID LENGTH.  THE FULL RESOLUTION OF THE SMALLER
C        VALUES MAY NOT BE CARRIED.
C
C        DETERMINE THE MAP PROJECTION INDEX I.
C
      IF(NPROJ.EQ.3)THEN
         I=1
      ELSEIF(NPROJ.EQ.5)THEN
         I=2
      ELSEIF(NPROJ.EQ.7)THEN
         I=3
      ELSE
         WRITE(KFILDO,140)NPROJ
 140     FORMAT(/' ****MAP PROJECTION NRPOJ =',I4,' IS NOT 3, 5, OR 7',
     1           ' IN CKGRID AT 140.  IER = 199.')
         IER=199
         GO TO 165
      ENDIF
C
      DO 150 J=1,9
D        WRITE(KFILDO,149)I,J,IS2(8),XMSTAB(I,J)
D149     FORMAT(' IN CKGRID--I,J,IS2(8),XMSTAB(I,J)',3I10,F15.10)
C
         IF((IS2(8)/1000000.)-XMSTAB(I,J).LT..0001)GO TO 165
CCCC         IF(IS2(8).EQ.NINT(XMSTAB(I,J)*1000000.))GO TO 165
C           THIS CHECKS TO THE RESOLUTIN OF TDLPACKING.
 150  CONTINUE
C
      WRITE(KFILDO,160)IS2(8),ID
 160  FORMAT(/' ****MESH LENGTH*1000000. =',I10,
     1        ' NOT EXPECTED FOR VARIABLE',3I10.9,I10,/
     2        '     IN CKGRID AT 160.')
      IER = 199
C
 165  RETURN
      END
