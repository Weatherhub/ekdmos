      SUBROUTINE MSHXMS(KFILDO,MESH,XMESHN,XMESH)
C
C        AUGUST   2000   GLAHN   TDL   LAMP-2000
C        JUNE     2002   GLAHN   CORRECTED LAST 3 VALUES IN XMSTAB
C        DECEMBER 2002   RUDACK  MODIFIED FORMAT STATEMENTS TO ADHERE
C                                TO THE F90 COMPILER STANDARDS FOUND ON 
C                                THE IBM SYSTEM
C
C        PURPOSE
C           TO FIND ACTUAL MESH LENGTH (XMESHN) AS A FUNCTION OF
C           NOMINAL MESH LENGTH (MESH), AND TO FIND FLOATING POINT
C           MESH LENGTH WHICH IS A TRUE HALVING FOR EACH MESH LENGTH
C           (XMESH).  NOMINAL MESH LENGTHS BETWEEN 320 AND 1 ARE
C           ACCOMMODATED.
C   
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES 
C
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C                MESH = NOMINAL MESH LENGTH.  (INPUT)
C              XMESHN = NOMINAL MESH LENGTH THAT IS A TRUE HALVING
C                       OF EACH MESH LENGTH.  MESH AND XMESHN
C                       ARE THE SAME FROM 320 TO 5, BUT FROM THEN
C                       ON, THE INTEGER VALUES ARE NOT HALVES OF
C                       THE NEXT ONE.  (OUTPUT)
C               XMESH = THE ACTUAL MESH LENGTH CORRESPONDING TO
C                       MESH AND XMESHN.  (OUTPUT)
C            MSTAB(J) = THE NOMINAL GRID SIZES POSSIBLE TO DEAL
C                       WITH.  THESE VALUES WILL MATCH THE INPUT
C                       MESHIN AND MESHOUT (J=1,9).  (INTERNAL)
C             STAB(J) = THE FLOATING POINT VERSION OF MSTAB( )
C                       (J=1,9).  (INTERNAL)
C           XMSTAB(J) = ACTUAL GRID LENGTH CORRESPONDING TO 
C                       MSTAB( ) AND STAB( ) (J=1,9)
C
C        NONSYSTEM SUBROUTINES USED 
C            NONE
C            
      DIMENSION MSTAB(9),STAB(9),XMSTAB(9)
C
      DATA  STAB /320.,  160.,   80.,    40.,     20.,      10.,
     1                  5.,      2.5,       1.25/
      DATA MSTAB / 320,   160,    80,     40,      20,       10,
     1                   5,         3,          1/
      DATA XMSTAB/381., 190.5, 95.25, 47.625, 23.8125, 11.90625,
     1            5.953125, 2.9765625, 1.48828125/
C
C*** OLD VALUES, CORRECTED JUNE 2002
C    1            5.951325, 2.9756625, 1.48783125/
C*** OLD VALUES, CORRECTED JUNE 2002
C
C        DETERMINE WHETHER ANY ACTION IS NECESSARY.
C
      DO 140 J=1,9
         IF(MESH.EQ.MSTAB(J))GO TO 160
 140  CONTINUE
C
      WRITE(KFILDO,145)MESH
 145  FORMAT(/' ****CANNOT FIND NOMINAL MESH LENGTH MESH =',I8,
     1        ' IN MSTAB( ) TABLE.'/
     2        '     STOP IN MSHXMS AT 145.')
      STOP 145
C
 160  XMESHN=STAB(J)
      XMESH=XMSTAB(J)
C
D     WRITE(KFILDO,170)MESH,XMESHN,XMESH
D170  FORMAT(' DONE IN MSHXMS--MESH,XMESHN,XMESH',I6,2F10.2)
C
      RETURN
      END
