      SUBROUTINE ROUGH(KFILDO,CCALL,SEALND,TELEV,NXE,NYE,IXX,JYY,
     1                 IR,ROUGH1,LIO,IER)
C
C        NOVEMBER  2007   GLAHN   MDL   MOS-2000
C        NOVEMBER  2007   GLAHN   2ND EDITION
C        DECEMBER  2007   GLAHN   ADDED LIO; RETURN ROUGH1 = 0
C                                 WHEN LIO NE 9
C
C        PURPOSE
C            TO COMPUTE A TERRAIN ROUGHNESS VARIABLE ROUGH1 WITHIN
C            RADIUS R = THE MEAN ABSOLUTE ELEVATION DIFFERENCE
C            BETWEEN ALL GRIDPOINTS WITHIN THE RADIUS R AND THE
C            AVERAGE OF THOSE SAME GRIDPOINTS.  THIS HAS NO USE
C            FOR WATER POINTS AND WHEN LIO NE 9, ROUGH1 = 0
C            UNON RETURN.  IT IS ASSUMED NO LAND ELEVATIONS ARE
C            MISSING (9999.).
C
C        DATA SET USE
C            KFILDO   - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C               CCALL = CALL LETTERS OF STATION BEING PROCESSED.  USED
C                       ONLY FOR DIAGNOSTICS.  (CHARACTER*8)  (INPUT)
C           SEALND(J) = THE LAND/SEA MASK (IX=1,NXE) (JY=1,NYE).
C                       (INPUT)
C                       0 = OCEAN WATER GRIDPOINTS;
C                       3 = INLAND WATER GRIDPOINTS.
C                       9 = LAND GRIDPOINTS.
C                       (INPUT)
C        TELEV(IX,JY) = THE TERRAIN ELEVATION (IX=1,NXE) (JY=1,NYE).
C                       (INPUT)
C                 NXE = X-EXTENT OF TELEV( ) AND SEALND( ).  (INPUT)
C                 NYE = Y-EXTENT OF TELEV( ) AND SEALND( ).  (INPUT)
C                 IXX = THE IX POSITION ON THE GRID WHERE THE ROUGHNESS
C                       IS TO BE COMPUTED.  (INPUT)
C                 JYY = THE JY POSITION ON THE GRID WHERE THE ROUGHNESS
C                       IS TO BE COMPUTED.  (INPUT)
C                  IR = RADIUS OVER WHICH TO COMPUTE ROUGHNESS.
C              ROUGH1 = THE MEAN ROUGHNESS.  (OUTPUT)
C                 LIO = THE VARIABLE TO SPECIFY WHETHER THIS RUN IS
C                       FOR LAND (=9), INLAND WATER (=3), OR 
C                       OCEAN (=0) POINTS.  (OUTPUT) 
C                 IER = 0 = GOOD RETURN.  (OUTPUT)
C        1         2         3         4         5         6         7 X
C
C        NONSYSTEM SUBROUTINES CALLED
C            NONE.
C
      CHARACTER*8 CCALL
C
      DIMENSION TELEV(NXE,NYE),SEALND(NXE,NYE)
C
CD     CALL TIMPR(KFILDO,KFILDO,'START ROUGH         ')
C
      IER=0
C
      IF(LIO.NE.9)THEN
         ROUGH1=0.
         GO TO 310
      ENDIF
C
      RSQ=IR*IR+.01
C        THE SMALL CONSTANT IS ADDED TO ASSURE A POINT IS NOT 
C        ELIMINATED BECAUSE OF ROUNDOFF.
      TELEVA=0.
      NCOUNT=0
      ROUGH1=0.
C
      DO 200 JY=MAX(1,JYY-IR),MIN(NYE,JYY+IR)
      DO 199 IX=MAX(1,IXX-IR),MIN(NXE,IXX+IR)
C
      IF(SEALND(IX,JY).LT.8.5)GO TO 199
C        ONLY LAND POINTS ARE CONSIDERED.
C
      IF(((IX-IXX)**2+(JY-JYY)**2).LE.RSQ)THEN
         TELEVA=TELEVA+TELEV(IX,JY)
         NCOUNT=NCOUNT+1
C
CD        WRITE(KFILDO,198)IX,JY,RSQ,TELEVA,NCOUNT
CD198     FORMAT(' AT 198 IN ROUGH--IX,JY,RSQ,TELEVA,NCOUNT',
CD    1          2I6,2F8.1,I6)
      ENDIF
C         
 199  CONTINUE
 200  CONTINUE
C
      TELEVA=TELEVA/NCOUNT
C        TELEVA IS THE AVERAGE ELEVATION WITHIN A CIRCLE OF RADIUS R
C        AROUND THE POINT IXX,JYY.
C
      NCOUNT=0
C
      DO 300 JY=MAX(1,JYY-IR),MIN(NYE,JYY+IR)
      DO 299 IX=MAX(1,IXX-IR),MIN(NXE,IXX+IR)
C
      IF(SEALND(IX,JY).LT.8.5)GO TO 299
C        ONLY LAND POINTS ARE CONSIDERED.
C
      IF(((IX-IXX)**2+(JY-JYY)**2).LE.RSQ)THEN
         ROUGH1=ROUGH1+ABS(TELEVA-TELEV(IX,JY))
         NCOUNT=NCOUNT+1
C
CD        WRITE(KFILDO,298)IX,JY,NCOUNT,ROUGH1,TELEVA,TELEV(IX,JY)
CD298     FORMAT(' AT 298 IN ROUGH--',
CD    1          'IX,JY,NCOUNT,ROUGH1,TELEVA,TELEV(IX,JY)',
CD    2           3I5,3F10.1)
      ENDIF
C         
 299  CONTINUE
 300  CONTINUE
C
      ROUGH1=ROUGH1/NCOUNT
CD     WRITE(KFILDO,305)IR,CCALL,ROUGH1,NCOUNT
CD305  FORMAT(/,' AT 305 IN ROUGH--IR,CCALL,ROUGH1,NCOUNT',
CD    1           I3,2X,A8,2F8.1,I10)
C
CD     CALL TIMPR(KFILDO,KFILDO,'END   ROUGH         ')
 310  RETURN
      END      
