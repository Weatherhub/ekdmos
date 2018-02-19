      SUBROUTINE ACTUAL(KFILDO,MESH,XMESH,XMESHN,NPROJ,IER)
C
C        NOVEMBER 2004   GLAHN   TDL   MOS-2000
C                                ADAPTED FROM MSHXMS FOR LAMP
C        JANUARY  2006   GLAHN   CORRECTED COMMENT FOR NPROJ
C
C        PURPOSE
C           TO FIND THE ACTUAL MESH LENGTH (XMESH) AS A FUNCTION
C           OF NOMINAL MESH LENGTH (MESH), AND TO FIND FLOATING 
C           POINT MESH LENGTH (XMESHN) WHICH IS A TRUE HALVING
C           FOR EACH MESH LENGTH.  NOMINAL MESH LENGTHS BETWEEN 
C           320 AND 1 ARE ACCOMMODATED.
C   
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES 
C
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C                MESH = NOMINAL MESH LENGTH IN KM.  (INPUT)
C               XMESH = THE ACTUAL MESH LENGTH CORRESPONDING TO
C                       MESH AND XMESHN IN KM.  (OUTPUT)
C              XMESHN = NOMINAL MESH LENGTH THAT IS A TRUE HALVING
C                       OF EACH MESH LENGTH IN KM.  MESH AND XMESHN
C                       ARE THE SAME FROM 320 TO 5, BUT FROM THEN
C                       ON, THE INTEGER VALUES ARE NOT HALVES OF
C                       THE NEXT ONE.  (OUTPUT)
C               NPROJ = MAP PROJECTION.
C                       3 = LAMBERT
C                       5 = POLAR STEREOGRAPHIC
C                       7 = MERCATOR
C                       (INPUT)
C                 IER = ERROR RETURN.
C                         0 = GOOD RETURN.
C                       183 = CAN'T FIND MAP PROJECTION NUMBER.
C                       184 = CAN'T FIND NOMINAL MESH LENGTH IN TABLE.
C                       (OUTPUT)
C            MSTAB(J) = THE NOMINAL GRID SIZES POSSIBLE TO DEAL
C                       WITH (J=1,9) .  THESE VALUES WILL
C                       MATCH THE INPUT MESHIN AND MESHOUT.
C                       (INTERNAL)
C             STAB(J) = THE FLOATING POINT VERSION OF MSTAB(J, )
C                       (J=1,9).  (INTERNAL)
C         XMSTAB(I,J) = ACTUAL GRID LENGTH CORRESPONDING TO 
C                       MSTAB(J) AND STAB(J) (J=1,9) FOR EACH OF
C                       THREE MAP PROJECTIONS (I=1,3).
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES USED 
C            NONE
C            
      DIMENSION MSTAB(9),STAB(9),XMSTAB(3,9)
C
      DATA MSTAB / 320,   160,    80,     40,      20,       10,
     1               5,     3,     1/ 
      DATA  STAB / 320.,  160.,   80.,    40.,     20.,      10.,
     1               5.,    2.5,   1.25/   
      DATA XMSTAB/325.082,      381.,           320.,
     1            162.541,      190.5,          160.,
     2             81.2705,      95.25,          80.,
     3             40.63525,     47.625,         40.,
     4             20.317625,    23.8125,        20.,
     5             10.1588125,   11.90625,       10.,
     6              5.07940625,   5.953125,       5.,
     7              2.539703125,  2.9765625,      2.2,
     8              1.2698515625, 1.48828125,     1.25/
C
      IER=0
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
         WRITE(KFILDO,110)NPROJ
 110     FORMAT(/' ****MAP PROJECTON NRPOJ =',I4,' IS NOT 3, 5, OR 7',
     1           ' IN ACTUAL AT 110.  IER = 183.')
         IER=183
         GO TO 165
      ENDIF
C
      DO 140 J=1,9
         IF(MESH.EQ.MSTAB(J))GO TO 160
 140  CONTINUE
C
      WRITE(KFILDO,145)MESH
 145  FORMAT(/' ****CANNOT FIND NOMINAL MESH LENGTH MESH =',I8,
     1        ' IN MSTAB( ) TABLE IN ACTUAL,  IER = 184.')
      IER = 184
      GO TO 165
C
 160  XMESHN=STAB(J)
      XMESH=XMSTAB(I,J)
 165  CONTINUE
C
D     WRITE(KFILDO,170)XMESH,XMESHN,MESH
D170  FORMAT(' DONE IN ACTUAL--XMESH,XMESHN,MESH',2F16.10,I6)
      RETURN
      END
