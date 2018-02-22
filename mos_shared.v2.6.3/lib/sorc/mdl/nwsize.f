      SUBROUTINE NWSIZE(KFILDO,XIN,XOUT,
     1                  NXIN,NYIN,NXOUT,NYOUT,ND2X3,IER)
C
C        OCTOBER   2004   GLAHN   MDL   MOS-20000
C                                 MODIFIED FROM NEWSIZE (LAMPLIB)
C        DECEMBER  2004   GLAHN   CORRECTED FORMAT 120
C        FEBRUARY  2005   GLAHN   MODIFIED FOR MESH LENGTHS WHEN
C                                 RATIOS OF INTEGERS NOT POWERS OF 2;
C                                 CHANGED TWO CALL VARIABLES TO REAL
C        FEBRUARY  2008   GLAHN   CORRECTED IF(ND2X3.LT.NINT(XIN*XOUT))
C                                 TO        IF(ND2X3.LT.NXOUT*NYOUT)
C        MARCH     2008   GALHN   SPELL CHECK
C
C        PURPOSE
C           TO CALCULATE A NEW GRID SIZE NXOUT AND NYOUT FOR A MESH
C           LENGTH MESHOUT BASED ON AN OLD GRID SIZE NXIN AND NYIN AND
C           MESH LENGTH MESHIN.  NX*NY IS CHECKED TO BE LE ND2X3.
C   
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES 
C
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C                 XIN = THE INPUT MESH LENGTH.  (INPUT)
C                XOUT = THE OUTPUT MESH LENGTH.  (INPUT)
C                NXIN = SIZE OF THE GRID IN NX DIRECTION.  (INPUT)
C                NYIN = SIZE OF THE GRID IN NY DIRECTION.  (INPUT)
C               NXOUT = NEW SIZE OF THE GRID IN NX DIRECTION
C                       WITH MESH LENGTH MESHI.  (OUTPUT)
C               NYOUT = NEW SIZE OF THE GRID IN NY DIRECTION.
C                       WITH MESH LENGTH MESHOUT.  (OUTPUT)
C               ND2X3 = SIZE OF ARRAY THAT NX*NY MUST FIT INTO.
C                       (INPUT)
C                 IER = ERROR RETURN.
C                         0 = GOOD RETURN.
C                       186 = ND2X3 NOT BIG ENOUGH TO ACCOMMODATE NEW
C                             DIMENSIONS NXOUT*NYOUT.
C                 XIN = ACTUAL MESH LENGTH CORRESPONDING TO MESHIN.
C                       (INTERNAL)
C                XOUT = ACTUAL MESH LENGTH CORRESPONDING TO MESHOUT.
C                       (INTERNAL)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES USED 
C            ACTUAL
C
      IER=0
      RATIO=XIN/XOUT
C      
      IF(ABS(XIN-XOUT).LT..001)THEN
         NXOUT=NXIN
         NYOUT=NYIN
C
      ELSE
         NXOUT=NINT((NXIN-1)*RATIO+1)
         NYOUT=NINT((NYIN-1)*RATIO+1)
C
      ENDIF
C
D     WRITE(KFILDO,100)XIN,XOUT,RATIO,NXIN,NYIN,
D    1                 NXOUT,NYOUT
D100  FORMAT(/,' IN NWSIZE AT 100--  XIN,   XOUT,     RATIO,',
D    1         '  NXIN,  NYIN,',
D    2         ' NXOUT, NYOUT',/,
D    3         '                  ',F7.4,   F8.4,     F11.2,
D    4          4I7)
C
      IF(ND2X3.LT.NXOUT*NYOUT)THEN
         NXNY=NXOUT*NYOUT
         WRITE(KFILDO,120)ND2X3,NXNY
 120     FORMAT(/,' ****ND2X3 =',I8,' NOT LARGE ENOUGH FOR',
     1            ' NX*NY =',I8,' IN NWSIZE AT 120.')
         IER=186
      ENDIF
C
      RETURN
      END
