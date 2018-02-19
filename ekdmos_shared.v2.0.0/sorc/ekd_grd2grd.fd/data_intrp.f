      SUBROUTINE DATA_INTRP(KFILDO,ND2X3,ND5,NX,NY,
     1                      JP2,DATA,GDATA,XYDIR,
     2                      IS3,IS4)
C
C$$$   SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: DATA_INTRP
C   PRGMMR: RUDACK         ORG: W/OST22          DATE: 2002-07-01
C
C ABSTRACT: TO INTERPOLATE DATA FROM THE INPUT GRID TO THE 
C           OUTPUT GRID.  DATA POINTS ON THE OUTPUT GRID THAT  
C           ARE OUTSIDE THE BOUNDRIES OF THE INPUT GRID ARE SET
C           TO MISSING.
C
C
C PROGRAM HISTORY LOG:
C   02-07-01  RUDACK
C   05-11-01  MALONEY ADDED NCEP DOCBLOCK.
C
C USAGE:  CALLED BY INTRP_OUT
C
C        DATA SET USE:
C        OUTPUT FILES:
C          FORT.KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C               ND2X3 = DIMENSION OF FD1( ) AND GDATA( ).  MUST BE
C                       AT LEAST AS LARGE AS THE LARGEST GRID.  (INPUT)
C                 ND5 = DIMENSION OF DATA( ).  (INPUT) 
C                  NX = X-EXTENT OF THE OUTPUT GRID.  (INPUT)
C                  NY = Y-EXTENT OF THE OUTPUT GRID.  (INPUT)
C                 JP2 = INDICATES THE TYPE OF INTERPOLATION TO BE 
C                       PERFORMED ON THE OUTPUT GRID.  (INPUT)
C             DATA(J) = CONTAINS THE VALUES OF THE VARIABLE ON THE 
C                       INPUT GRID (J=1,ND5).  (INPUT)
C            GDATA(J) = STORES THE INTERPOLATED DATA OF THE OUTPUT GRID
C                       (J=1,ND2X3).  (OUTPUT)
C          XYDIR(K,J) = THE IX (J=1) AND JY (J=2) POSITIONS ON THE OUTPUT
C                       GRID (K=1,NX*NY) FOR A PARTICULAR GRID 
C                       CHARACTERISTIC.  (INPUT)  
C                 IS3 = X-EXTENT OF THE INPUT GRID.  (INPUT)
C                 IS4 = Y-EXTENT OF THE INPUT GRID.  (INPUT)
C              FD1(J) = WORK ARRAY (J=1,ND2X3).  (ALLOCATABLE ARRAY) (INTERNAL)
C
C        SUBPROGRAMS CALLED:  INTRPA, INTRPB, INTRP, INTRPC
C          UNIQUE: NONE
C          LIBRARY:
C           MOSLIB - INTRPA, INTRPB, INTRP, INTRPC
C
C        EXIT STATES:
C          COND =    0 - SUCCESSFUL RUN
C                   SEE OTHER ROUTINES FOR OTHER VALUES.
C
C REMARKS:  NONE
C
C ATTRIBUTES:
C   LANGUAGE:  FORTRAN 90 (xlf90 compiler)
C   MACHINE:  IBM SP
C
C$$$
C
      REAL, ALLOCATABLE, DIMENSION(:) :: FD1
C
      DIMENSION GDATA(ND2X3),XYDIR(ND2X3,2)
      DIMENSION DATA(ND5)
C
      ALLOCATE(FD1(ND2X3))
C
C        CONTROL HERE SHOULD MEAN NON-MISSING DATA.
C
      IF(JP2.EQ.1)THEN
         CALL INTRPA(KFILDO,DATA,IS3,IS4,
     1               XYDIR,ND2X3,NX*NY,GDATA)
C        INTRPA IS BIQUADRATIC INTERPOLATION WHERE POSSIBLE,
C        BILINEAR OTHERWISE.
C
      ELSEIF(JP2.EQ.2)THEN
         CALL INTRPB(KFILDO,DATA,IS3,IS4,
     1               XYDIR,ND2X3,NX*NY,GDATA)
C        INTRPB IS BILINEAR.
C
      ELSEIF(JP2.EQ.3)THEN
         CALL INTRP(KFILDO,DATA,FD1,IS3,IS4,
     1              XYDIR,ND2X3,NX*NY,GDATA)
C        INTRP IS INTERPOLATION FOR PRECIPITATION AMOUNT.  THE
C        PROCESS IS BILINEAR AFTER PREPARATION OF THE FIELD 
C        TO PUT THE ZERO LINE ABOUT HALFWAY BETWEEN POSITIVE 
C        AND ZERO GRIDPOINTS.
C
      ELSEIF(JP2.EQ.4)THEN
         CALL INTRPC(KFILDO,DATA,IS3,IS4,
     1               XYDIR,ND2X3,NX*NY,GDATA)
C        INTRPC FINDS VALUES CLOSEST TO A GRIDPOINT.
      ENDIF
C
C        SET IS3 AND IS4 TO REAL VALUES.
C 
      XMAX=IS3
      YMAX=IS4
C
C        IF A POINT ON THE OUTPUT GRID EXTENDS BEYOND THE 
C        BOUNDRY OF THE INPUT GRID, SET THAT VALUE TO MISSING.
C
      K=1
      DO 200 JY=1,NY
         DO 199 IX=1,NX
C
            IF((XYDIR(K,1).LT..80).OR.
     1         (XYDIR(K,1).GT.(XMAX+.20)).OR.
     2         (XYDIR(K,2).LT..80).OR.
     3         (XYDIR(K,2).GT.(YMAX+.20))) GDATA(K)=9999.
C
            K=K+1
C
 199     CONTINUE
 200  CONTINUE
C
      DEALLOCATE(FD1)
C
      RETURN
      END
