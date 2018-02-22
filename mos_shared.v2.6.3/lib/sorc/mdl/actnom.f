      SUBROUTINE ACTNOM(KFILDO,MAPP,MESHIN,MESH,XMESH,IFYF,IER)
C
C        JULY     2003   GLAHN   TDL   LAMP-2000
C
C        PURPOSE
C            TO USE AS INPUT THE ACTUAL MESH LENGTH*1000000 IN
C            MESHIN [AS IT IS STORED IN NGRIDC(2, ) AND IS2(8)] AND 
C            OUTPUT:
C             1)  THE INTEGER NOMINAL MESH LENGTH (2.5 ROUNDED TO 3)
C                 IN MESH,
C             2)  THE NOMINAL MESH LENGTH THAT IS A TRUE HALVING
C                 OF GRID LENGTHS IN BEDIENTS IN XMESH, AND
C             3)  THE VALUE USED AS Y IN FFF = FYF DEALING WITH
C                 TERRAIN IN, FOR INSTANCE, UPSLOP IN IFYF.
C            MESH LENGTHS BETWEEN 1 BEDIENT (381 KM) 320 AND 1/256 
C            BEDIENTS (1.487831 KM) ARE ACCOMMODATED, EXCEPT THE
C            1 B AND 1/2 B GRIDLENGTHS HAVE NO CORRESPONDING IFYF.
C            IN MATCHING THE GRIDLENGTHS, A TOLERANCE OF 5 (=.000005 KM)
C            IS ALLOWED.
C******* CORRECT VALUES FOR MSTABMC FOR THE MERCATOR MAP ARE NOT YET
C            AVAILABLE.
C   
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES 
C
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C                MAPP = MAP PROJECTION
C                       3 = LAMBERT
C                       5 = POLAR STEREOGRAPHIC
C                       7 = MERCATOR
C              MESHIN = ACTUAL MESH LENGTH IN KM*1000000.  (INPUT)
C                MESH = NOMINAL MESH LENGTH, WHICH IS SUCCESSIVE
C                       HALVING OF 320, ROUNDED TO INTEGER BELOW 5.
C                       (OUTPUT)
C               XMESH = NOMINAL MESH LENGTH WHICH IS SUCCESSIVE
C                       HALVING OF 320.  MESH AND XMESH
C                       ARE THE SAME FROM 320 TO 5, BUT FROM THEN
C                       ON, THE INTEGER VALUES ARE NOT HALVES OF
C                       THE NEXT ONE.  (OUTPUT)
C                IFYF = THE "Y" PART OF FYF CORRESPONDING TO
C                       MESHIN, USED IN TERRAIN RELATED IDS
C                       (E.G., UPSLOP, INTRPD).  (OUTPUT)
C                 IER = ERROR RETURN.
C                         0 = GOOD RETURN.
C                        60 = MAP PROJECTION IS NOT 3, 5, OR 7.
C                       188 = GRID LENGTH IS NOT ONE EXPECTED..
C          KSTABLM(J) = ACTUAL GRID LENGTHS IN KM * 1000000
C                       POSSIBLE TO DEAL WITH FOR A LAMBERT MAP
C                       (J=1,9).  (INTERNAL)
C           STABLM(J) = THE NOMINAL GRID LENGTHS CORRESPONDING TO
C                       KSTABLM(J) (J=1,9).  (INTERNAL)
C          MSTABLM(J) = THE INTEGER VERSION OF STABLM( ) (J=1,9).
C                       (INTERNAL)
C          KSTABPS(J) = ACTUAL GRID LENGTHS IN KM * 1000000
C                       POSSIBLE TO DEAL WITH FOR A POLAR
C                       STEREOGRAPHIC MAP (J=1,9).  (INTERNAL)
C           STABPS(J) = THE NOMINAL GRID LENGTHS CORRESPONDING TO
C                       KSTABPS(J) (J=1,9).  (INTERNAL)
C          MSTABPS(J) = THE INTEGER VERSION OF STABPS( ) (J=1,9).
C                       (INTERNAL)
C          KSTABMC(J) = ACTUAL GRID LENGTHS IN KM * 1000000
C                       POSSIBLE TO DEAL WITH FOR A MERCATOR MAP
C                       (J=1,9).  (INTERNAL)
C           STABMC(J) = THE NOMINAL GRID LENGTHS CORRESPONDING TO
C                       KSTABMC(J) (J=1,9).  (INTERNAL)
C          MSTABMC(J) = THE INTEGER VERSION OF MSTABLM( ) (J=1,9).
C                       (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES USED 
C            NONE
C            
      DIMENSION STABLM(9),KSTABLM(9),MSTABLM(9),
     1          STABPS(9),KSTABPS(9),MSTABPS(9),
     2          STABMC(9),KSTABMC(9),MSTABMC(9),MFYF(9)
C
      DATA  STABLM /     320.,      160.,      80.,     40.,      20.,
     1                    10.,        5.,       2.5,     1.25/
      DATA KSTABLM /      320,       160,        80,      40,       20, 
     1                     10,         5,         3,       1/
      DATA MSTABLM /325081984, 162540992, 81270496, 40635248, 20317624,
     1               10158812,   5079406,  2539703,  1269851/
C        THESE VALUES FOR THE LAMBERT WERE GENERATED STARTING FROM
C        THE NOMINAL 5-KM GRID (5079406 IN THE GRIB2 FILES AND 
C        SUCCESSIVELY HALVING AND DOUBLING ON EITHER SIDE.
C
      DATA  STABPS /     320.,      160.,      80.,     40.,      20.,
     1                    10.,        5.,       2.5,     1.25/
      DATA KSTABPS /      320,       160,        80,      40,       20, 
     1                     10,         5,         3,       1/
      DATA MSTABPS /381000000, 190500000, 95250000, 47625000, 23812500,
     1               11906250,   5951325,  2975662,  1487831/
C
      DATA  STABMC /     320.,      160.,      80.,     40.,      20.,
     1                    10.,        5.,       2.5,     1.25/
      DATA KSTABMC /      320,       160,        80,      40,       20, 
     1                     10,         5,         3,       1/
      DATA MSTABMC /381000000, 190500000, 95250000, 47625000, 23812500,
     1               11906250,   5951325,  2975662,  1487831/
C
      DATA MFYF    /     9999,      9999,        0,        1,        2,
     1                      3,         4,        5,        6/
C
C        DETERMINE WHETHER ANY ACTION IS NECESSARY.
C
      IF(MAPP.EQ.3)THEN
C      
         DO 110 J=1,9
            IF(ABS(MESHIN-MSTABLM(J)).LT.5)GO TO 119
 110     CONTINUE
C
         WRITE(KFILDO,115)MESHIN
 115     FORMAT(/' ****CANNOT FIND INPUT MESH LENGTH MESHIN =',I11,
     1           ' IN MSTABLM( ) TABLE FOR LAMBERT MAP.')
         IER=188
         GO TO 420
C
 119     XMESH=STABLM(J)
         MESH=MSTABLM(J)
         IFYF=MFYF(J)
      ELSEIF(MAPP.EQ.5)THEN
C      
         DO 210 J=1,9
            IF(ABS(MESHIN-MSTABPS(J)).LT..000005)GO TO 219
 210     CONTINUE
C
         WRITE(KFILDO,215)MESHIN
 215     FORMAT(/' ****CANNOT FIND INPUT MESH LENGTH MESHIN =',I11,
     1           ' IN MSTABPS( ) TABLE FOR POLAR STEREOGRAPHIC MAP.')
         IER=188
         GO TO 420
C
 219     XMESH=STABPS(J)
         MESH=MSTABPS(J)
         IFYF=MFYF(J)
      ELSEIF(MAPP.EQ.7)THEN
C      
         DO 310 J=1,9
            IF(ABS(MESHIN-MSTABMC(J)).LT..000005)GO TO 319
 310     CONTINUE
C
         WRITE(KFILDO,315)MESHIN
 315     FORMAT(/' ****CANNOT FIND INPUT MESH LENGTH MESHIN =',I11,
     1           ' IN MSTABMC( ) TABLE FOR MERCATOR MAP.')
         IER=188
         GO TO 420
C
 319     XMESH=STABMC(J)
         MESH=MSTABMC(J)
         IFYF=MFYF(J)
      ELSE
         WRITE(KFILDO,350)MAPP
 350     FORMAT(/' ****MAP PROJECTION =',I5,' NOT CORRECT IN ACTNOM.',
     1           '  MUST BE 3, 5, OR 7.')
         IER=60
      ENDIF  
C
D     WRITE(KFILDO,410)MAPP,MESHIN,MESH,XMESH,IFYF
D410  FORMAT(' DONE IN ACTNOM--MAPP,MESHIN,MESH,XMESH,IFYF',
D    1        I4,2I11,F10.4,I6)
 420  RETURN
      END
