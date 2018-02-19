      SUBROUTINE DERIVY(KFILDO,NPRJCT,XMESHL,ORIENT,XLAT,
     1                  VAR,YDER,NX,NY,XNP,YNP,IER)
C
C        JULY      1993  GLAHN      MDL   MOS2000
C        JANUARY   2004  WIEDENFELD MODIFIED TO CONFORM TO FORTRAN 90
C                                   FORMAT SPECIFICATIONS.
C        DECEMBER  2004  CHARBA     CHANGED CODE FOR POLAR STEREOGRAPHIC
C                                   PROJECTION FROM 1 TO 5 TO CONFORM TO
C                                   MOS2000 CONVENTION.  ALSO, REMOVED
C				    KFIL10 AND REPLACED KFIL12 WITH 
C				    KFILDO.
C        MAY       2005  CHARBA     IMPROVED DOCUMENTATION.
C        JUNE      2006  CHARBA     MADE COSMETIC CHANGES TO SATISFY
C                                   CODE WALK-THRU.
C
C        PURPOSE
C            TO COMPUTE SPATIAL DERIVATIVE IN Y-DIRECTION FOR VARIABLE
C            IN VAR( , ).  INCORPORATES POLAR STEREOGRAPHIC MAP FACTOR.
C            OUTPUT IS IN UNITS OF VAR( , ) PER METER.
C
C        DATA SET USE
C           KFILDO  - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT)
C              NPRJCT = NUMBER OF MAP PROJECTION.  (INPUT)
C              XMESHL = MESH LENGTH OF GRID AT XLAT DEG N LATITUDE IN 
C			KM.  (INPUT)
C              ORIENT = ORIENTATION OF GRID, W LONGITUDE, IN DEGREES.
C                       COLUMNS ARE PARALLEL TO THIS LONGITUDE.  (INPUT)
C                XLAT = LATITUDE AT WHICH XMESHL APPLIES.  (INPUT)
C          VAR(IX,JY) = VARIABLE FOR WHICH TO GET Y-DERIVITIVE
C                       (IX=1,NX) (JY=1,NY).  (INPUT)
C         YDER(IX,JY) = DERIVITIVE OF VARIABLE IN VAR( , ) IN Y-DIREC-
C			TION.  RESULTS IN UNITS OF VAR( , ) PER METER.  
C			(OUTPUT)
C               NX,NY = DIMENSIONS OF VAR( , ) AND XDER( , ), AND NUM-
C                       BER OF GRID POINTS IN THE X- AND Y-DIRECTIONS,
C                       RESPECTIVELY.  (INPUT)
C             XNP,YNP = X- AND Y-GRID COORDINATES, RESPECTIVELY, OF THE
C                       NORTH POLE GRID WITH RESPECT TO (1,1) OF THE
C                       GRID.  (INPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN
C                        60 = MAP PROJECTION AND GRID ORIENTATION
C                             VALUES NOT EXPECTED.
C                  DG = GRID LENGTH IN M.  (INTERNAL)
C                DGX2 = GRID LENGTH IN M X 2.  (INTERNAL)
C              PTOEQ2 = DISTANCE FROM POLE TO EQUATOR IN GRID UNITS
C                       SQUARED.  FOR A 1 BEDIENT XMESHL OF 381 KM,
C                       PTOEQ2 = EXACTLY 31.2 SQUARED.  THIS IN CONSIS-
C			TENT WITH E354, SUBROUTINE AVEXT.  IT DIFFERS 
C			SLIGHTLY FROM TDL M201 CONSTANT OF 978 PAIRED 
C			WITH 381 KM, WHICH IS 31.273 SQUARED.  NMC 
C			ROUTINE W3FB05 GIVES 973.71 AS THE EQUIVALENT 
C			CONSTANT, WHICH IS 31.2043 SQUARED.  (INTERNAL)
C              OPSLAT = ONE PLUS SIN OF LATITUDE AT WHICH XMESHL 
C			APPLIES.  (INTERNAL)
C
C        NON-SYSTEM SUBROUTINES CALLED
C            NONE
C
      DIMENSION VAR(NX,NY),YDER(NX,NY)
C
      RADDEG=(2*3.14159)/360.
C
      IER=0
C
C        IS THIS A POLAR STEREOGRAPHIC MAP PROJECTION WITH GRID ORIENTED
C        ALONG 105 DEG W LONGITUDE?
C
      IF(NPRJCT.EQ.5.AND.ORIENT.EQ.105.)GO TO 105
      WRITE(KFILDO,104) NPRJCT,ORIENT
 104  FORMAT(' **** IN DERIVY, INCORRECT MAP PROJECTION  AND GRID',
     1       ' ORIENTATION VALUES = ',I3,2X,F8.2,
     2       '.  CORRECT VALUES ARE 5 AND 105.',
     3       ' ...SET IER = 60 AND RETURN')
      IER=60
      GO TO 900
C
 105  DG=XMESHL*1000.
C        XMESHL IS IN KM.  MULTIPLY BY 1000 TO GET TO M.
      DGX2=XMESHL*2000.
C        XMESHL IS IN KM.  MULTIPLY BY 1000 TO CONVERT TO M, AND MULTI-
C        PLY BY 2 BECAUSE DISTANCE IS OVER 2 GRID LENGTHS.
      PTOEQ2=(11887.2/XMESHL)**2
C        PTOEQ2 IS DISTANCE FROM POLE TO EQUATOR IN GRID UNITS SQUARED.
      OPSLAT=1.+SIN(XLAT*RADDEG)
C        OPSLAT IS ONE PLUS SIN OF LATITUDE AT WHICH XMESHL APPLIES.
C
      DO 130 JY=2,NY-1
        DYSQ=(YNP-JY)**2
C
        DO 120 IX=1,NX
          DXSQ=(XNP-IX)**2
          SINPHI=(PTOEQ2-(DXSQ+DYSQ))/(PTOEQ2+(DXSQ+DYSQ))
          RMAPF=OPSLAT/(1.+SINPHI)
          YDER(IX,JY)=(RMAPF/DGX2)*(VAR(IX,JY+1)-VAR(IX,JY-1))
D         WRITE(KFILDO,119)IX,JY,DXSQ,DYSQ,SINPHI,RMAPF
D119      FORMAT(' ',2I4,4F14.6)
 120    CONTINUE
C
 130  CONTINUE
C
      DO 140 IX=1,NX
        DXSQ=(XNP-IX)**2
        DYSQ=(YNP-1.)**2
        SINPHI=(PTOEQ2-(DXSQ+DYSQ))/(PTOEQ2+(DXSQ+DYSQ))
        RMAPF=OPSLAT/(1.+SINPHI)
        YDER(IX,1)=(RMAPF/DG)*(VAR(IX,2)-VAR(IX,1))
D       WRITE(KFILDO,135)IX,DXSQ,DYSQ,SINPHI,RMAPF
D135    FORMAT('     ',I4,4F14.6)
C
        DYSQ=(YNP-NY)**2
        SINPHI=(PTOEQ2-(DXSQ+DYSQ))/(PTOEQ2+(DXSQ+DYSQ))
        RMAPF=OPSLAT/(1.+SINPHI)
        YDER(IX,NY)=(RMAPF/DG)*(VAR(IX,NY)-VAR(IX,NY-1))
D       WRITE(KFILDO,135)IX,DXSQ,DYSQ,SINPHI,RMAPF
 140  CONTINUE
C
 900  RETURN
      END
