      SUBROUTINE SAMEGR(KFILDO,IDA,IDB,NSLABA,NSLABB,NXA,NXB,NYA,NYB,
     1                  PLAINA,PLAINB,ROUTE,NGRIDC,ND11,IER)
C
C        JULY     2003   GLAHN   TDL   LAMP-2000
C        OCTOBER  2003   SMB     MODIFIED FORMAT STATEMENT 150
C                                FOR THE IBM
C
C        PURPOSE
C            TO CHECK THE NSLAB, NX, AND NY OF TWO GRIDS, A AND B,
C            AND IF THEY ARE NOT THE SAME TO WRITE A DIAGNOSTIC
C            AND RETURN WITH IER = 100.
C   
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES 
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              IDA(J) = THE 4-WORD ID OF GRID A (J=1,4).  (INPUT)
C              IDB(J) = THE 4-WORD ID OF GRID B (J=1,4).  (INPUT)
C              NSLABA = NSLAB NUMBER FOR GRID A.  (INPUT)
C              NSLABB = NSLAB NUMBER FOR GRID B.  (INPUT)
C                 NXA = NX GRID EXTENT OF GRID A.  (INPUT) 
C                 NXB = NX GRID EXTENT OF GRID B.  (INPUT) 
C                 NYA = NY GRID EXTENT OF GRID A.  (INPUT) 
C                 NYB = NY GRID EXTENT OF GRID B.  (INPUT) 
C              PLAINA = 20 CHARACTERS PLAIN LANGUAGE TO IDENTIFY GRID A.
C                       (CHARACTER*20)  (INPUT)
C              PLAINB = 20 CHARACTERS PLAIN LANGUAGE TO IDENTIFY GRID B.
C                       (CHARACTER*20)  (INPUT)
C               ROUTE = 10 CHARACTER ROUTINE NAME.  (CHARACTER*10)
C         NBRIDC(J,M) = 6 VALUES (J=1,6) PER SLAB (M=1,ND11) DEFINING
C                       GRID (EXCEPT FOR NX AND NY EXTENTS).  FOR
C                       DIAGNOSTIC PRINT ONLY.  (INPUT)
C                ND11 = SECOND DIMENSION OF NGRIDC( , ). (INPUT)
C                 IER = ERROR CODE.
C                        0 = GOOD RETURN; GRIDS ARE THE SAME.
C                      100 = GRIDS ARE NOT THE SAME.
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES USED 
C            NONE
C
      CHARACTER(*) PLAINA,PLAINB,ROUTE
C
      DIMENSION NGRIDC(6,ND11)
      DIMENSION IDA(4),IDB(4)
C
      IER=0
C
      IF(NSLABA.EQ.NSLABB.AND.
     1   NXA.EQ.NXB.AND.
     2   NYA.EQ.NYB)GO TO 200
C
C        THE GRIDS ARE NOT THE SAME.  WRITE DIAGNOSTIC.
C
      WRITE(KFILDO,150)PLAINA,PLAINB,ROUTE,
     1                 (IDA(J),J=1,4),(NGRIDC(J,NSLABA),J=1,6),
     2                  NXA,NYA,
     3                 (IDB(J),J=1,4),(NGRIDC(J,NSLABB),J=1,6),
     4                  NXB,NYB
 150  FORMAT(/' ****THE GRID CHARACTERISTICS OF THE ',A,
     1        ' ARE NOT THE SAME AS THE ',A,
     2        ' IN ',A,'.',/,
     3        '     THE TWO IDS, SIX NGRID( , ) VALUES, AND',
     4        ' GRID EXTENTS ARE:',/,
     3        ('     ',I9.9,2I10.9,I4.3,4X,6I10,4X,2I5))
      IER=100

 200  RETURN
      END
