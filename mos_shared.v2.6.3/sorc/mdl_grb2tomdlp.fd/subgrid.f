      SUBROUTINE SUBGRID(GDATA,JGRID,NNX,NNXS,
     1                   NNYS,IIOFFX,IIOFFY)
C
C$$$   SUBPROGRAM DOCUMENTATION BLOCK
C
C        MAY       2003   RUDACK   MDL  (ADAPTED FROM AN ALGORITHM 
C                                       FOUND IN GRID2TDLP) 
C        SEPTEMBER 2005   RUDACK        MODIFIED CODE TO MEET OPERATIONAL 
C                                       REQUIREMENTS.
C
C        PURPOSE
C           TO CREATE A SUBGRID OF THE INPUT GRIB DATA FIELD.
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C            GDATA(J) = THE DATA FROM THE GRIB FILE TO BE MANIPULATED 
C                       (J=1,NX*NY).  (INPUT/OUTPUT)
C               JGRID = TOTAL NUMBER OF GRIDPOINTS ON THE INPUT GRID
C                       (JGRID=NX*NY).  (INPUT)
C                 NNX = X-EXTENT OF THE INPUT GRID.  (INPUT)
C                NNXS = X-EXTENT OF THE OUTPUT SUBGRID.  (INPUT)
C                NNYS = Y-EXTENT OF THE OUTPUT SUBGRID.  (INPUT)
C              IIOFFX = X-OFFSET OF THE SUBGRID FROM THE INPUT GRID. 
C                       (INPUT)
C              IIOFFY = Y-OFFSET OF THE SUBGRID FROM THE INPUT GRID. 
C                       (INPUT)
C             JOFFSET = OFFSET GRIDPOINT RELATIVE TO THE ORIGIN 
C                       OF THE INPUT GRID.  (INTERNAL) 
C
C SUBROUTINES CALLED: NONE
C
C REMARKS:  NONE
C
C ATTRIBUTES:
C   LANGUAGE:  FORTRAN 90 (xlf90 compiler)
C   MACHINE:  IBM SP
C$$$
C
      DIMENSION GDATA(JGRID)
C
      LL=0
      DO 150 IY=1,NNYS
         DO 140 IX=1,NNXS
            LL=LL+1
            JOFFSET=(IIOFFY+IY-1)*NNX+(IIOFFX+IX)
            GDATA(LL)=GDATA(JOFFSET)
 140     CONTINUE
 150  CONTINUE
C
      RETURN
      END
