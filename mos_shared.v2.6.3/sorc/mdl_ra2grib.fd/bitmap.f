      SUBROUTINE BITMAP(LUNIT,IBMAP,IIMY,JJMY,IIMDR,JJMDR,IOVLND,ISPRD,
     1                  KOFF,ISAME,BCUT,BORLAT,BORL,BDXM,BOR,YORLAT,
     2                  YORL,YDXM,YOR,MAPPROJU,TANLATU,MAPPROJB,
     3                  TANLATB,KERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BITMAP      CREATES A LAND/SEA MASK FOR A GRID         
C   PRGMMR: SETTELMAIER      ORG: W/OSD211   DATE: 95-08-30             
C                                                                       
C ABSTRACT: CREATES A MASK TO BE APPLIED TO A GRID OF DATA TO ONLY      
C   DISPLAY THAT PORTION OF THE DATA THAT LIES ROUGHLY OVER THE LAND    
C   OF THE CONTIGUOUS UNITED STATES                                     
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   95-08-30  SETTELMAIER       
C   01-05-12  ALLEN	MODIFIED SUBROUTINE TO WORK FOR BOTH 
C			POLAR STEREOGRAPHIC AND LAMBERT CONFORMAL
C                       MAP PROJECTIONS.         
C   01-08-18  ALLEN     REPLACED CALL TO RDLSTI WITH RDI
C   04-05-11  RLC       INCREASED MMIJ AND MXOVLND TO BE ABLE
C                       TO USE A 20KM CONUS GRID
C                                                                       
C USAGE:                                                                
C                                                                       
C     SEE BELOW FOR MDL STANDARDS                                       
C                                                                       
C        AUGUST 1995   SETTELMAIER   MDL   NAS9000                      
C                                                                       
C        PURPOSE                                                        
C            CREATES A MASK TO BE APPLIED TO A GRID OF DATA TO ONLY     
C            DISPLAY THAT PORTION OF THE DATA THAT LIES ROUGHLY OVER THE
C            LAND OF THE CONTIGUOUS UNITED STATES                       
C                                                                       
C        VARIABLES                                                      
C              ALAT() = STATION LATITUDE (INPUT)                        
C              ALON() = STATION LONGITUDE (INPUT)                       
C               ANX() = ARRAY OF X COORDINATES OF THE OVERLAND POINTS   
C               ANY() = ARRAY OF Y COORDINATES OF THE OVERLAND POINTS   
C                BCUT = CUTOFF USED IN BITMAPPING ROUTINE TO DETERMINE  
C                       WHETHER OR NOT TO TURN ON OR OFF A GRID         
C                       POINT (INPUT)                                   
C                BDXM = GRID SPACING MULTIPLIED BY 1000.                
C                BMAP = VARIABLE FOR TEMPORARILY HOLDING THE            
C                       INTERPOLATED VALUE                              
C               BMODL = GRID ON WHICH THE LAND/SEA MASK POINTS ARE TO   
C                       BE READ IN FROM. CURRENTLY MDR GRID FOR CONUS,
C                       AWIN FOR AK.            
C                 BOR = ORIENTATION OF THE LAND/SEA MASK GRID,    
C                       EAST LONGITUDE PARALLEL TO Y-AXIS               
C                BORL = EAST LONGITUDE OF ORIGIN OF LAND/SEA MASK GRID  
C              BORLAT = LATITUDE OF THE ORIGIN OF THE LAND/SEA GRID          
C                  DX = VARIABLE TO HOLD THE GRID SPACING IN THE X      
C                       DIRECTION                                       
C                  DY = VARIABLE TO HOLD THE GRID SPACING IN THE Y      
C                       DIRECTION                                       
C                   I = COUNTER VARIABLE                                
C             IBMAP() = ARRAY TO HOLD THE BITMAP USED TO MASK THE DATA  
C                       FIELD                                           
C                  II = TEMPORARY HOLDER OF THE OVERLAND POINT'S        
C                       I POSITION ON THE LANDSEA GRID                    
C               IIMDR = NUMBER OF GRIDPOINTS IN THE X DIRECTION ON THE  
C                       LAND/SEA MASK GRID                         
C                IIMY = NUMBER OF GRIDPOINTS IN THE X DIRECTION ON THE  
C                       USER-SPECIFIED GRID                             
C              IOVLND = VARIABLE USED TO INDICATE THE NUMBER OF         
C                       OVERLAND POINTS                                 
C               ISAME = VARIABLE TO INDICATE WHETHER OR NOT THE NEXT    
C                       DATA FIELD IS BEING REQUESTED ON THE SAME GRID  
C                       OR NOT.  IF IT IS, THEN SOME VARIABLES NEED NOT 
C                       BE RECALCULATED                                 
C               ISIXN = VARIABLE CONTAINING SIX NINES USED AS A         
C                       TERMINATOR                                      
C               ISPRD = VARIABLE TO INDICATE WHETHER OR NOT TO          
C                       SPREAD THE BITMASK OFF THE GRID                 
C             ITEMP() = ARRAY USED TO READ IN THE FORMAT OF THE INPUT   
C                       DATA TO BE READ IN BY RDLSTA                    
C                   J = COUNTER VARIABLE                                
C                  JJ = TEMPORARY HOLDER OF THE OVERLAND POINT'S        
C                       J POSITION ON THE (MDR) GRID                    
C               JJMDR = NUMBER OF GRIDPOINTS IN THE Y DIRECTION ON THE  
C                       LAND/SEA MASK GRID (MDR FOR CONUS, AWIN FOR AK)         
C                JJMY = NUMBER OF GRIDPOINTS IN THE Y DIRECTION ON THE  
C                       USER-SPECIFIED GRID                             
C                KERR = ERROR RETURN CODE RETURNED FROM BITMAP          
C                       =1 IF NUMBER OF GRIDPOINTS OF USER-SPECIFIED
C                          GRID EXCEEDS MAXIMUM ALLOWED.  IN THAT CASE
C                          A BITMAP WILL NOT BE CREATED.
C                  KK = COUNTER VARIABLE                                
C                KOFF = NUMBER OF GRIDPOINTS TO SPREAD THE BITMASK OFF  
C                       THE LAND/SEA MASK (INPUT)                       
C                  KP = VARIABLE USED TO CALCULATE THE INTERPOLATED     
C                       VALUE                                           
C                KYP1 = VARIABLE USED TO CALCULATE THE INTERPOLATED     
C                       VALUE                                           
C              LAND() = ARRAY TO HOLD THE OVERLAND POINTS LOCATION      
C                       AS THEY ARE READ IN VIA RDLSTA                  
C                 LOC = VARIABLE USED TO CALCULATE THE INTERPOLATED     
C                       VALUE                                           
C                  LP = VARIABLE USED TO CALCULATE THE INTERPOLATED     
C                       VALUE                                           
C               LUNIT = UNIT NUMBER OF THE FILE CONTAINING THE LAND/SEA 
C                       MASK POINTS                                     
C                LXP1 = VARIABLE USED TO CALCULATE THE INTERPOLATED     
C                       VALUE                                           
C                   M = LOOP COUNTER VARIABLE                           
C            MAPPROJB = MAP PROJECTION OF THE GRID THE LAND/SEA MASK
C                       IS ON.
C            MAPPROJU = MAP PROJECTION OF THE GRID THE USER HAS  
C                       SPECIFIED FOR OUTPUTTING THE DATA.
C               MXBUF = MAXIMUM LENGTH OF ANY OF THE OUTPUT GRIB        
C                       FIELDS                                          
C                MXIJ = MAXIMUM NUMBER OF POINTS IN EITHER THE X OR     
C                       Y DIRECTION ON THE USER-SPECIFIED GRID          
C              MXOVLN = VARIABLE TO HOLD THE MAXIMUM NUMBER OF OVERLAND 
C                       POINTS ALLOWED                                  
C                   N = LOOP COUNTER VARIABLE                           
C              RIIMDR = VARIABLE TO HOLD THE "REAL" VALUE OF THE NUMBER 
C                       OF POINTS IN THE X DIRECTION ON THE MDR GRID    
C              RJJMDR = VARIABLE TO HOLD THE "REAL" VALUE OF THE NUMBER 
C                       OF POINTS IN THE Y DIRECTION ON THE MDR GRID    
C             TANLATB = LATITUDE OF TANGENT CONE FOR LAND/SEA MASK GRID,
C                       IF LAMBERT CONFORMAL
C             TANLATU = LATITUDE OF TANGENT CONE FOR USER'S DATA GRID,
C                       IF LAMBERT CONFORMAL
C                 VAL = VARIABLE USED TO CALCULATE THE INTERPOLATED     
C                       VALUE                                           
C              XCUT() = ARRAY USED TO HOLD SOME OF THE CALCULATED       
C                       VALUES FROM THE INTERPOLATION PROCESS           
C                  XE = VARIABLE USED IN "SPREADING" THE MASK GRID      
C                XI() = I COORDINATES OF THE OVERLAND POINTS ON THE     
C                       USER-SPECIFIED GRID                             
C                XJ() = J COORDINATES OF THE OVERLAND POINTS ON THE     
C                       USER-SPECIFIED GRID                             
C                  XN = VARIABLE USED IN "SPREADING" THE MASK GRID      
C             XRCUT() = ARRAY USED TO HOLD SOME OF THE CALCULATED       
C                       VALUES FROM THE INTERPOLATION PROCESS           
C                  XS = VARIABLE USED IN "SPREADING" THE MASK GRID      
C              XTST() = ARRAY USED TO HOLD SOME OF THE CALCULATED       
C                       VALUES FROM THE INTERPOLATION PROCESS           
C                  XW = VARIABLE USED IN "SPREADING" THE MASK GRID      
C                YDXM = GRID SPACING OF THE USER-SPECIFIED GRID         
C                       MULTIPLIED BY 1000.                             
C                 YOR = ORIENTATION OF THE USER-SPECIFIED GRID,         
C                       EAST LONGITUDE PARALLEL TO Y-AXIS               
C                YORL = EAST LONGITUDE OF ORIGIN OF THE USER-SPECIFIED  
C                       GRID                                            
C              YORLAT = LATITUDE OF THE ORIGIN OF THE USER-SPECIFIED    
C                       GRID                                            
C                                                                       
C REMARKS: 
C   THE FILES THAT CONTAIN THE LANDSEA MASKS ARE CREATED IN SUCH
C   A WAY THAT THE VERTICAL GRIDPOINT IS FIRST, THEN THE HORIZONTAL
C   GRIDPOINT.  IN THE CODE, THEY ARE ORIGINALLY ASSIGNED SUCH THAT
C   THE VERTICAL COORDINATE IS I, NOT J.  SO THERE IS A LINE OF CODE
C   THAT CORRECTS THIS.
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 77                                                
C   MACHINE:  CRAY                                                      
C                                                                       
C$$$                                                                    
      PARAMETER(MXBUF=40000,MXOVLN=70000,MXIJ=350)
      CHARACTER*3 MAPPROJU,MAPPROJB
      DIMENSION ALAT(MXBUF),ALON(MXBUF),ANY(MXBUF)
      DIMENSION LAND(MXOVLN),IBMAP(MXBUF),ANX(MXBUF)
      DIMENSION XRCUT(IIMDR,JJMDR),XCUT(JJMDR,IIMDR),
     &          XTST(JJMDR,IIMDR)
      DIMENSION XI(MXBUF),XJ(MXBUF),VAL(MXBUF),ITEMP(12)
      DATA ISIXN/999999/,IOUT/6/
C
      RIIMDR=REAL(IIMDR)
      RJJMDR=REAL(JJMDR)
      KERR=0
      IF(IIMY.GT.MXIJ.OR.JJMY.GT.MXIJ) THEN
        KERR=1
        WRITE(6,5) IIMY,JJMY,MXIJ
  5     FORMAT(' IIMY ',I4,' OR JJMY ',I4,' EXCEEDS THE MAXIMUM ALLOWED'
     1         ,/,I5,' IN SUBROUTINE BITMAP. NO BITMAP CREATED.')
        GO TO 130
      ENDIF
C                                                                       
C        INITIALIZE THE BITMAP ARRAY, THE ARRAYS HOLDING THE I AND J    
C        COORDINATES ON THE GRID FOR WHICH A BITMAP IS DESIRED, AND     
C        THE ARRAY THAT WILL HOLD THE INTERPOLATED VALUES.              
C                                                                       
      M=0
      DO 10 J=1,JJMY
      DO 10 I=1,IIMY
         M=M+1
         IBMAP(M)=0
         ANX(M)=REAL(I)
         ANY(M)=REAL(J)
         VAL(M)=0.
  10  CONTINUE
C                                                                       
C        READ IN OVERLAND POINTS ON THE LANDSEA GRID FROM THE DATA FILE     
C                                                                       
      REWIND LUNIT
      CALL RDI(IOUT,IOUT,LUNIT,LAND,MXOVLN,ITEMP,12,'(12(I6))',
     &         IOVLND,ISIXN,IER)
C                                                                       
C        INITIALIZE THE VALUES AT LANDSEA GRIDPOINTS TO ZEROES.             
C                                                                       
      DO 20 J=1,IIMDR
      DO 20 I=1,JJMDR
         XCUT(I,J)=0.0
         XRCUT(J,I)=0.0
  20  CONTINUE
C                                                                       
C        SET TO OVERLAND BLOCK VALUES (LAND) AT THE LANDSEA   
C        GRIDPOINTS TO ONES.                                               
C                                                                       
      DO 30 J=1,IOVLND
         IF(LAND(J).EQ.0) GO TO 30
         II=LAND(J)/1000
         JJ=LAND(J)-(II*1000)
         XCUT(II,JJ)=1.0
  30  CONTINUE
C                                                                       
C        ZERO GRID BLOCKS ON OUTER OVERWATER BOUNDARY OF XCUT ARRAY     
C        TO PREVENT INLAND CONTOUR TRUNCATION                           
C                                                                       
      IF(ISPRD.EQ.1.AND.KOFF.GT.0) THEN
        DO 40 J=1,IIMDR
        DO 40 I=1,JJMDR
           XTST(I,J)=XCUT(I,J)
  40    CONTINUE
        DO 70 KK=1,KOFF
           DO 50 J=2,IIMDR-1
           DO 50 I=2,JJMDR-1
              IF(XCUT(I,J).EQ.1.0) GO TO 50
              XE=XCUT(I+1,J)
              XW=XCUT(I-1,J)
              XN=XCUT(I,J+1)
              XS=XCUT(I,J-1)
              IF(XE.EQ.1.0) XTST(I,J)=1.0
              IF(XW.EQ.1.0) XTST(I,J)=1.0
              IF(XN.EQ.1.0) XTST(I,J)=1.0
              IF(XS.EQ.1.0) XTST(I,J)=1.0
  50       CONTINUE
           DO 60 J=1,IIMDR
           DO 60 I=1,JJMDR
              XCUT(I,J)=XTST(I,J)
  60       CONTINUE
  70    CONTINUE
      ENDIF
C                                                                       
C        THE LANDSEA FILE LISTS THE DATA POINTS WITH THE VERTICAL COORD
C        FIRST, THEN THE HORIZONTAL COORDINATE.  WHEN READ IN THE 
C        VERTICAL COORD IS SET TO "I", THE HORIZONTAL TO "J".  SO WE
C        MUST "FLIP" THE XCUT ARRAY OF 0'S AND 1'S SO THAT THE I,J'S
C        TAKE ON THEIR TRADITIONAL I=HORIZONTAL, J=VERTICAL REPRESENTATION.
C        THIS MUST BE DONE FOR BOTH THE ALASKA LANDSEA ON THE AWIN GRID, 
C        AND THE CONUS LANDSEA ON THE MDR GRID.
C
C        IF THE GRID ANALYZED ON IS THE SAME GRID THAT THE BITMAP IS 
C        ON (ISAME=1) THEN NO INTERPOLATION NEEDS TO BE DONE AND THE
C        IBMAP ARRAY CAN BE SET RIGHT NOW.                                                 
C                                                                       
      M=0
      DO 90 I=1,JJMDR
      DO 80 J=1,IIMDR
         M=M+1
         XRCUT(J,I)=XCUT(I,J)
         IF(ISAME.EQ.1) IBMAP(M)=INT(XRCUT(J,I))
  80  CONTINUE
  90  CONTINUE
      IF(ISAME.EQ.1) THEN
        WRITE(6,100)
  100   FORMAT(' GRID DATA HAS BEEN ANALYZED TO AND GRID CONTAINING BIT
     1MAP ARE THE SAME.',/,' I"M NOT GOING TO INTERPOLATE HERE IN BITMAP
     2')
        GOTO 130
      ENDIF
C                                                                       
C        FOR EACH GRIDPOINT ON THE GRID FOR WHICH A BITMAP IS DESIRED,  
C        CALCULATE THE LATITUDE AND LONGITUDE (W3FB07) AND THEN         
C        THE I AND J COORDINATES OF THOSE GRIDPOINTS ON THE AS THEY     
C        WOULD LIE ON THE MDR GRID (W3FB06).  THEN DO A BI-LINEAR       
C        INTERPOLATION TO OBTAIN VALUES AT THE NON-MDR GRIDPOINTS THAT  
C        LIE WITHIN THE MDR GRID.  IF THE GRIDPOINTS LIE OUTSIDE OF THE 
C        MDR GRID, SET THEIR CORRESPONDING VALUE TO 0.  IF, AFTER       
C        INTERPOLATION, THE VALUES WITHIN THE MDR GRID HAVE VALUES      
C        GREATER THAN OR EQUAL TO 0.5, SET THEM EQUAL TO ONE, IE. LAND  
C        POINTS, OTHERWISE SET THEM TO 0.                               
C                                                                       
      DO 120 N=1,IIMY*JJMY
       IF(MAPPROJU.EQ.'NPS')THEN
         CALL W3FB07(ANX(N),ANY(N),YORLAT,YORL,YDXM,YOR,
     1            ALAT(N),ALON(N))
       ELSEIF(MAPPROJU.EQ.'LAM')THEN
         CALL W3FB12(ANX(N),ANY(N),YORLAT,YORL,YDXM,YOR,
     1               TANLATU,ALAT(N),ALON(N),JERR)
       ENDIF
C
C
       IF(MAPPROJB.EQ.'NPS')THEN
         CALL W3FB06(ALAT(N),ALON(N)-360.,BORLAT,BORL,BDXM,BOR,
     1            XI(N),XJ(N))
       ELSEIF(MAPPROJB.EQ.'LAM')THEN
         CALL W3FB11(ALAT(N),ALON(N)-360.,BORLAT,BORL,BDXM,BOR,
     1            TANLATB,XI(N),XJ(N))
       ENDIF
C
C
         IF(XJ(N).GT.RJJMDR.OR.XJ(N).LT.1.) GO TO 110
         IF(XI(N).GT.RIIMDR.OR.XI(N).LT.1.) GO TO 110
         LP=XI(N)
         KP=XJ(N)
         DX=XI(N)-LP
         DY=XJ(N)-KP
         LXP1=LP+1
         KYP1=KP+1
         LOC=XRCUT(LP,KP)
         BMAP=LOC+(XRCUT(LXP1,KP)-LOC)*DX+(XRCUT(LP,KYP1)-LOC)*DY
     1       +(LOC+XRCUT(LXP1,KYP1)-XRCUT(LP,KYP1)-XRCUT(LXP1,KP))*DX*DY
         VAL(N)=BMAP
         IF(VAL(N).GE.BCUT) THEN
           VAL(N)=1.
         ELSE
           VAL(N)=0.
         ENDIF
         IBMAP(N)=INT(VAL(N))
         GO TO 120
 110     VAL(N)=0.
         IBMAP(N)=INT(VAL(N))
 120  CONTINUE
 130  RETURN
      END
