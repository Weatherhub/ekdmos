      SUBROUTINE LAT2XY(JUNIT,NSTA,ALAT,ALON,UMODL,BMODL,LUNIT,
     1                  IOVLND,ISPRD,KOFF,BCUT,IBT,IBMAP,MXBUF,IGDS,
     2                  STX,STY,III,JJJ,LERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    LAT2XY      RETRIEVES GRID DISPLAY INFORMATION         
C   PRGMMR: SETTELMAIER      ORG: W/OSD211   DATE: 95-08-30             
C                                                                       
C ABSTRACT: RETRIEVES GRID INFORMATION NEEDED FOR DISPLAY PURPOSES      
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   95-08-30  SETTELMAIER                                               
C   01-05-12  ALLEN	MODIFIED SUBROUTINE TO CALL GETGRID
C 			AND ALLOW FOR BOTH POLAR STEREOGRAPHIC
C  	                AND LAMBERT CONFORMAL GRIDS DURING 
C                       CONVERSION FOR MOS2000
C                                                                       
C USAGE:                                                                
C                                                                       
C     SEE BELOW FOR MDL STANDARDS                                       
C                                                                       
C        AUGUST 1995   SETTELMAIER   MDL   NAS9000                      
C                                                                       
C        PURPOSE                                                        
C            RETRIEVES GRID INFORMATION NEEDED FOR DISPLAY PURPOSES     
C                                                                       
C        VARIABLES                                                      
C              ALAT() = STATION LATITUDE (INPUT)                        
C              ALON() = STATION LONGITUDE (INPUT)                       
C                BCUT = CUTOFF USED IN BITMAPPING ROUTINE TO DETERMINE  
C                       WHETHER OR NOT TO TURN ON OR OFF A GRID         
C                       POINT (INPUT)                                   
C                BDXM = GRID SPACING MULTIPLIED BY 1000.                
C               BMODL = GRID ON WHICH THE LAND/SEA MASK POINTS ARE TO   
C                       BE READ IN FROM. CURRENTLY MDR GRID.            
C                 BOR = ORIENTATION OF THE LAND/SEA MASK GRID (MDR),    
C                       EAST LONGITUDE PARALLEL TO Y-AXIS               
C                BORL = EAST LONGITUDE OF ORIGIN OF LAND/SEA MASK GRID  
C             CMODL() = ARRAY TO HOLD GRID ID'S ON THE MDL GRID FILE TO 
C                       BE USED FOR SEARCHING PURPOSES                  
C                DX() = VARIABLE TO HOLD THE GRID SPACING OF THE GRIDS  
C                       READ FROM THE GRID FILE                         
C                 DXM = GRID SPACING OF THE USER-SPECIFIED GRID         
C                       MULTIPLIED BY 1000.
C                DY() = VARIABLE TO HOLD THE GRID SPACING OF THE GRIDS  
C                       READ FROM THE GRID FILE                         
C               UMODL = VARIABLE TO HOLD THE USER-SPECIFIED GRID ID     
C                   I = COUNTER VARIABLE                                
C                IBII = NUMBER OF GRIDPOINTS IN THE X DIRECTION ON THE  
C                       LAND/SEA MASK GRID (MDR)                        
C                IBJJ = NUMBER OF GRIDPOINTS IN THE Y DIRECTION ON THE  
C                       LAND/SEA MASK GRID (MDR)                        
C             IBMAP() = ARRAY TO HOLD THE BITMAP USED TO MASK THE DATA  
C                       FIELD                                           
C                 IBT = VARIABLE USED TO INDICATE WHETHER OR NOT        
C                       (1 OR 0) TO CREATE A BITMAP MASK.               
C              IGDS() = ARRAY TO HOLD USER-SPECIFIED GRID INFORMATION   
C                       TO BE USED IN THE GDS SECTION OF THE FINAL GRIB 
C                       MESSAGE                                         
C             IGRIB() = ARRAY TO HOLD THE NCEP-ASSIGNED GRIB VALUE GRID 
C                       IDENTIFIERS.                                    
C             IGRID() = ARRAY TO HOLD THE NCEP-ASSIGNED GRID IDENTIFIERS
C                II() = ARRAY TO HOLD THE II COORDINATES OF THE GRIDS   
C                       READ IN FROM THE MDL GRID FILE                  
C                 III = NUMBER OF GRIDPOINTS IN THE X DIRECTION ON THE  
C                       USER-SPECIFIED GRID                             
C             IOFFX() = ARRAY TO HOLD THE OFFSET OF THE GRID FROM THE   
C                       MDL GRID IN THE X DIRECTION                     
C             IOFFY() = ARRAY TO HOLD THE OFFSET OF THE GRID FROM THE   
C                       MDL GRID IN THE Y DIRECTION                     
C              IOVLND = VARIABLE USED IN SUBROUTINE BITMAP TO INDICATE  
C                       THE NUMBER OF OVERLAND POINTS                   
C                IPOS = VARIABLE USED TO MARK WHEN THE USER-SPECIFIED   
C                       GRID HAS BEEN FOUND IN THE MDL GRID FILE        
C               ISAME = VARIABLE TO INDICATE WHETHER OR NOT THE NEXT    
C                       DATA FIELD IS BEING REQUESTED ON THE SAME GRID  
C                       OR NOT.  IF IT IS, THEN SOME VARIABLES NEED NOT 
C                       BE RECALCULATED                                 
C               ISPRD = INDICATOR OF WHETHER OR NOT, 1 OR 0, ANY        
C                       SPREADING OF THE OVERLAND BITMAP MASK SHOULD BE 
C                       DONE                                    
C                IX() = X DIMENSION OF NCEP'S GRID (I BELIEVE TO BE USED
C                       IF THE USER-SPECIFIED GRID IS A SUBSET OF AN    
C                       NCEP GRID)                                      
C                IY() = Y DIMENSION OF NCEP'S GRID (I BELIEVE TO BE USED
C                       IF THE USER-SPECIFIED GRID IS A SUBSET OF AN    
C                       NCEP GRID)                                      
C                JERR = ERROR RETURN CODE FROM GETGRID                   
C                JJ() = ARRAY TO HOLD THE JJ COORDINATES OF THE GRIDS   
C                       READ IN FROM THE MDL GRID FILE                  
C                 JJJ = NUMBER OF GRIDPOINTS IN THE Y DIRECTION ON THE  
C                       USER-SPECIFIED GRID                             
C                JPOS = VARIABLE USED TO MARK WHEN THE LAND/SEA         
C                       GRID HAS BEEN FOUND IN THE MDL GRID FILE        
C               JUNIT = UNIT NUMBER OF THE GRID DESCRIPTION FILE (INPUT)
C                KERR = ERROR RETURN CODE RETURNED FROM BITMAP          
C                KOFF = NUMBER OF GRIDPOINTS TO SPREAD THE BITMASK OFF  
C                       THE LAND/SEA MASK                               
C                LERR = ERROR RETURN CODE RETURNED FROM THIS SUBROUTINE 
C                       =1 IF ERROR IN SUBROUTINE GETGRID
C                       =2 IF ERROR IN SUBROUTINE BITMAP
C               LUNIT = UNIT NUMBER OF THE FILE CONTAINING THE LAND/SEA 
C                       MASK POINTS                                     
C            MAPPROJB = MAP PROJECTION OF THE GRID THE LAND/SEA MASK
C                       IS ON.
C            MAPPROJU = MAP PROJECTION OF THE GRID THE USER HAS  
C                       SPECIFIED FOR OUTPUTTING THE DATA.
C              MAXMOD = MAXIMUM NUMBER OF GRIDS THAT CAN BE READ IN     
C                       FROM THE MDL GRID FILE, CURRENTLY 10.           
C               MXBUF = MAXIMUM LENGTH OF ANY OF THE OUTPUT GRIB        
C                       FIELDS                                          
C                NSTA = NUMBER OF STATIONS                              
C              NUMMOD = HOLDS THE NUMBER OF GRIDS READ FROM THE MDL GRID
C                       FILE                                            
C                  OR = ORIENTATION OF THE USER-SPECIFIED GRID,         
C                       EAST LONGITUDE PARALLEL TO Y-AXIS               
C            ORIENT() = ARRAY TO HOLD THE ORIENTATION OF THE GRIDS      
C                       READ IN FROM THE MDL GRID FILE                  
C                 ORL = EAST LONGITUDE OF ORIGIN OF USER-SPECIFIED GRID 
C             ORLAT() = ARRAY HOLDING THE LATITUDE OF THE ORIGIN OF     
C                       THE GRIDS READ IN FROM THE MDL GRID FILE        
C            ORLONG() = ARRAY HOLDING THE LONGITUDE OF THE ORIGIN OF    
C                       THE GRIDS READ IN FROM THE MDL GRID FILE        
C             POLEX() = ARRAY TO HOLD THE X COORDINATE OF THE NORTH     
C                       POLE FOR THE GRIDS READ IN FROM THE MDL GRID    
C                       FILE                                            
C             POLEY() = ARRAY TO HOLD THE Y COORDINATE OF THE NORTH     
C                       POLE FOR THE GRIDS READ IN FROM THE MDL GRID    
C                       FILE                                            
C             TANLATB = LATITUDE OF TANGENT CONE FOR LAND/SEA MASK GRID,
C                       IF LAMBERT CONFORMAL
C             TANLATU = LATITUDE OF TANGENT CONE FOR USER'S DATA GRID,
C                       IF LAMBERT CONFORMAL
C               STX() = ARRAY TO HOLD THE STATION'S X COORDINATE ON THE 
C                       USER-SPECIFIED GRID                             
C               STY() = ARRAY TO HOLD THE STATION'S Y COORDINATE ON THE 
C                       USER-SPECIFIED GRID                             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 77                                                
C   MACHINE:  CRAY                                                      
C                                                                       
C$$$                                                                    
      PARAMETER (MAXMOD=14)
      DIMENSION STX(NSTA),STY(NSTA),IBMAP(MXBUF)
      DIMENSION ALAT(NSTA),ALON(NSTA)
      CHARACTER*3 MAPPROJU,MAPPROJB
      CHARACTER*4 UMODL,BMODL
      DIMENSION IGDS(18)
C                                                                       
C        GET GRID DESCRIPTION CONSTANTS                                 
C                                                                       
      REWIND JUNIT
      JERR=0
      LERR=0
      ISAME=0
      CALL GETGRID(UMODL,IGRIDU,IGRIBU,IIU,JJU,IX,IY,IOFFX,IOFFY,
     1     POLEX,POLEY,ORLATU,ORLONU,ORIENU,DXU,DY,MAPPROJU,TANLATU,
     2     JUNIT,JERR)
      IF (JERR.NE.0) THEN
        WRITE(6,10) JERR
 10     FORMAT('**** JERR NE ZERO COMING OUT OF GETGRID WITHIN ', 
     1         'SUBROUTINE LAT2XY',/,' GETTING USER-SPECIFIED GRID ',
     2         'INFORMATION',I3)
        LERR=1
      ENDIF
      JERR=0
      CALL GETGRID(BMODL,IGRIDB,IGRIBB,IIB,JJB,IX,IY,IOFFX,IOFFY,
     1     POLEX,POLEY,ORLATB,ORLONB,ORIENB,DXB,DY,MAPPROJB,TANLATB,
     2     JUNIT,JERR)
      IF(UMODL.EQ.BMODL) ISAME=1
      IF (JERR.NE.0) THEN
        WRITE(6,20) JERR
 20     FORMAT('**** JERR NE ZERO COMING OUT OF GTGRID WITHIN ',
     1 'SUBROUTINE LAT2XY',/,' GETTING BITMAP GRID INFORMATION',I3)
        LERR=1
      ENDIF
C                                                                       
C        CALCULATE X,Y COORDINATES FOR EACH STATION                     
C                                                                       
      DXM=DXU*1000.
      OR=360.-ORIENU
      ORL=360.-ORLONU
      BOR=360.-ORIENB
      BORL=360.-ORLONB
      BDXM=DXB*1000.
      IGDS(2)=IGRIBU
      IGDS(4)=IIU
      IGDS(5)=JJU
      IGDS(6)=ORLATU*1000
      IGDS(7)=ORL*1000
      IGDS(9)=OR*1000
      IGDS(10)=DXM
      IGDS(11)=DXM
      DO 30 I=1,NSTA
C
C     NOW CALL THE W3LIB ROUTINE TO COMPUTE THE STATION'S LAT/LON
C     TO THE CORRESPONDING I,J ON THE SPECIFIED GRID.  W3FB06 WORKS
C     FOR POLAR STEREOGRAPHIC GRIDS, W3FB11 WORKS FOR LAMBERT
C     CONFORMAL GRIDS
C
      IF(MAPPROJU.EQ.'NPS')THEN
        CALL W3FB06(ALAT(I),-ALON(I),ORLATU,ORL,DXM,
     1  OR,STX(I),STY(I))
      ELSEIF(MAPPROJU.EQ.'LAM')THEN
        CALL W3FB11(ALAT(I),-ALON(I),ORLATU,ORL,DXM,
     1  OR,TANLATU,STX(I),STY(I))
      ELSE
        WRITE(6,25)TANLATU
 25     FORMAT('UNRECOGNIZED MAP PROJECTION: ',A4)
      ENDIF
 30   CONTINUE
      III=IIU
      JJJ=JJU
      IBII=IIB
      IBJJ=JJB
      IF(IBT.EQ.1) THEN
        CALL BITMAP(LUNIT,IBMAP,III,JJJ,IBII,IBJJ,IOVLND,ISPRD,KOFF,
     1            ISAME,BCUT,ORLATB,BORL,BDXM,BOR,ORLATU,ORL,
     2            DXM,OR,MAPPROJU,TANLATU,MAPPROJB,TANLATB,KERR)
        IF(KERR.NE.0) THEN
          WRITE(6,40) KERR
 40       FORMAT('**** KERR NE ZERO COMING OUT OF SUBROUTINE BITMAP, ',
     1           'IT IS=',I2)
          LERR=2
        ENDIF
      ENDIF
      RETURN
      END
