      SUBROUTINE MAINGRIB(KFILDI,KFILDO,ND1,ND4,ND5,ND7,MXBUF,MXI,
     &                 MXJ,LPDS,ID,L3264B,IER)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                  
C                .      .    .                                       .  
C SUBPROGRAM: MAINGRIB
C   PRGMMR: ALLEN            ORG: OSD211      DATE: 2000-03-01
C                                                                       
C ABSTRACT: THIS PROGRAM READS MOS FORECAST OR CONSTANT RANDOM         
C   ACCESS FILES, OBTAINING REQUESTED DATA FOR SELECTED OR ALL  
C   AVAILABLE STATIONS. THE STATION DATA IS THEN INTERPOLATED
C   ONTO A GRID SPECIFIED BY THE USER, USING A CRESSMAN 
C   ANALYSIS ROUTINE.  THE USER CAN SPECIFY THE NUMBER OF 
C   PASSES THROUGH THE CRESSMAN ANALYSIS, WHAT THE RADIUS OF
C   INFLUENCE AT EACH GRID POINT SHOULD BE, AND WHETHER OR NOT
C   A BITMAP SHOULD BE CREATED FOR THE GRID FIELD.  ONCE THE
C   GRIDDED DATA IS OBTAINED, A FILE IS CREATED CONTAINING THE
C   GRIB FIELDS FOR EACH REQUESTED DATUM.                    

C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   01-01-08  ALLEN       - REWORKED GRIBMOS TO RUN ON THE NEW
C                           MOS AND BE IN MOS2000 FORMAT.                 
C   01-10-02  ERICKSON    - REMOVED CALL TO W3TAG
C   12-09-27  ENGLE       - CCALL NOW DIMENSIONED (ND1,6), WAS (ND1)
C                                                                       
C USAGE:    CALL PROGRAM-NAME(ARGS)
C   INPUT ARGUMENT LIST:
C              KFILDI = UNIT NUMBER OF INPUT FILE.  (INPUT)
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                       WITH (I.E., INTERPOLATION DONE FOR).  NOTE THAT
C                       THIS DOES NOT INCLUDE THE NUMBER OF STATIONS IN
C                       THE DIRECTORY UNLESS, OF COURSE, THE STATION
C                       DIRECTORY IS TO BE USED AS THE STATION LIST.
C                       SET BY PARAMETER.
C                 ND4 = THE MAXIMUM NUMBER OF PREDICTORS FOR WHICH 
C                       INTERPOLATED VALUES CAN BE PROVIDED.  SET BY
C                       PARAMETER.
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), DATA( ) AND
C                       CCALLD( ); SECOND DIMENSION OF ICALLD( , ).
C                       THESE ARE GENERAL PURPOSE ARRAYS, SOMETIMES USED 
C                       FOR GRIDS.  TWO SIZES OF ARRAYS (ND5 AND ND2X3) 
C                       ARE USED IN CASE AN ARRAY NEEDS TO BE LARGER 
C                       THAN ND2X3.  ND5 CAN BE INCREASED WITHOUT 
C                       INCREASING THE SIZE OF ALL ARRAYS. SET BY 
C                       PARAMETER.
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  MAXIMUM SIZE IS FOR
C                       IS1( ) = 22 PLUS 32 CHARACTERS (ONE CHARACTER
C                       PER WORD) OF PLAIN TEXT = 54.  SET BY PARAMETER.
C               MXBUF = MAXIMUM LENGTH OF ANY OF THE OUTPUT GRIB        
C                       FIELDS BEFORE PROBLEMS MAY ARISE IN THIS        
C                       PROGRAM (INPUT)                                 
C                 MXI = MAXIMUM NUMBER OF ROWS ALLOWED IN THE GRID      
C                       FIELD (INPUT)                                   
C                 MXJ = MAXIMUM NUMBER OF COLUMNS ALLOWED IN THE GRID   
C                       FIELD (INPUT)                                   
C                LPDS = CURRENT NUMBER OF ID ARRAY ELEMENTS WHICH GET
C                       USED TO CREATE THE PDS SECTION OF THE GRIB 
C                       MESSAGE
C                ID() = INTEGER ARRAY USED TO BUILD THE PDS THAT WILL
C                       BE INCLUDED IN THE GRIB RECORD.    
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  SET BY PARAMETER.
C
C   OUTPUT ARGUMENT LIST:
C                IERR = CONTAINS THE ERROR CODE RETURNED FROM VARIOUS       
C                       SUBROUTINES (OUTPUT)                     
C                                                                       
C        DATA SET USE  
C        INPUT FILES:                                                 
C             FORT.10 - NCEP STANDARD DATE FILE                  
C             FORT.29 - MDL MOS ID TABLE FILE                    
C             FORT.25 - VARIABLE LIST                    
C             FORT.26 - USER INPUT STATION LIST                  
C             FORT.27 - STATION DICTIONARY                  
C             FORT.35 - FILE CONTAINING GRID INFORMATION         
C             FORT.36 - FILE CONTAINING OVERLAND POINTS OVER THE        
C                       CONTIGUOUS U.S. ON THE MDR (113 BY 89) GRID     
C                       USED FOR BITMAP                          
C             FORT.45 - MDL MOS CONSTANT FILE                    
C             FORT.48 - MDL MOS FORECAST FILE                    
C
C        OUTPUT FILES:
C             FORT.12 - STANDARD OUTPUT                         
C             FORT.66 - FILE CONTAINING GRIB MESSAGES           
C                                                                       
C        VARIABLES                                                      
C               BGRID = GRID ON WHICH THE LAND/SEA MASK POINTS ARE      
C                       READ IN FROM VIA FT36, CURRENTLY MDR (CHAR*4)   
C                 BOT = BOTTOM VALUE USED IN CRESSMAN ANALYSIS ROUTINE  
C                       TO COMPARE AGAINST DATA VALUES                  
C            CCALL(,) = ARRAY CONTAINING USER-SPECIFIED STATION CALL
C                       LETTERS    
C               CGRID = GRID ON WHICH THE STATION DATA IS TO BE         
C                       ANALYZED TO (CHAR*4)                    
C               DGRID = VARIABLE USED TO TEST WHETHER OR NOT GRID INFO  
C                       HAS BEEN OBTAINED YET (CHAR*4)                  
C             FIELD() = ARRAY CONTAINING DATA AFTER THE CRESSMAN ANALYSIS
C               FINIT = INITIALIZATION CONSTANT TO BE APPLIED TO THE 
C                       GRID PRIOR TO THE CRESSMAN ANALYSIS 
C               FLD() = ARRAY CONTAINING DATA RETURNED FROM MOSMAT-TYPE 
C                       FILES.                                 
C             GRID(,) = GRID POINT VALUES OF THE DATA ON THE USER-      
C                       SPECIFIED GRID AS USED IN SUBROUTINE CRESS      
C            IBDSFL() = ARRAY USED IN SUBROUTINE W3FI72          
C             IBMAP() = ARRAY CONTAINING THE BITMAP MASK ON THE USER-   
C                       SPECIFIED GRID SHOULD IT BE DESIRED     
C                 IBT = VARIABLE TO INDICATE THE USER'S PREFERENCE FOR  
C                       A BITMAP MASK WHERE 1=YES, 0=NO          
C                IBTD = HOLDER FOR IBT VALUE FROM PREVIOUS READ         
C                ID() = ARRAY CONTAINING INPUT TO CREATE THE PDS SECTION
C                       OF THE GRIB MESSAGE                      
C            IDCODE() = 4-PIECE MDL ID OF THE REQUESTED DATA     
C               IZERO = DUMMY VARIABLE USED IN SUBROUTINE CALLS  
C              IGDS() = ARRAY CONTAINING INPUT TO CREATE THE GDS SECTION
C                       OF THE GRIB MESSAGE                      
C              IGFLAG = VARIABLE TO INDICATE THE PRESENCE OF USER-      
C                       SUPPLIED IGDS ARRAY WHERE 1=YES, 0=NO    
C                   I = COUNTER VARIABLE IN A LOOP                      
C                IIII = CONTAINS THE NUMBER OF ROWS IN THE GRID THAT THE
C                       DATA IS TO BE ANALYZED TO. THIS IS INITIALLY    
C                       OUTPUT FROM SUBROUTINE LAT2XY AND THEN IS USED  
C                       AS INPUT ELSEWHERE                              
C              IOVLND = HOLDS THE NUMBER OF OVERLAND GRID POINTS ON THE 
C                       GRID USED TO CREATE THE BITMAP MASK. FOR THE    
C                       MDR GRID CURENTLY USED, THE VALUE IS XXXX.      
C              IPFLAG = VARIABLE TO INDICATE THE PRESENCE OF A USER-    
C                       SUPPLIED PDS IN THE ARRAY PDS WHERE 1 MEANS IT  
C                       IS PRESENT AND 0 MEANS CREATE THE PDS FROM THE  
C                       USER-SUPPLIED ID() ARRAY                 
C                IPRT = VARIABLE TO INDICATE WHETHER OR NOT TO PRINT OUT
C                       DIAGNOSTICS WHERE 0 MEANS TO NOT PRINT OUT      
C                       ANYTHING, 1 MEANS PRINT OUT SOME INFORMATION        
C                       ABOUT THE VARIABLES AND 2 MEANS PRINT OUT THE 
C 			STATION
C                       DATA FOR THE VARIABLES.  
C               ISPRD = INDICATOR OF WHETHER OR NOT, 1 OR 0, ANY SPREADING  
C                       OF THE OVERLAND BITMAP MASK SHOULD BE DONE 
C              ISPRDD = HOLDER FOR ISPRD FROM PREVIOUS READ             
C                ITOP = VARIABLE TO HOLD THE NUMBER OF GRID POINTS FOUND
C                       ON THE GRID IN USE. IT IS USED IN THE TRUNCATION
C                       SECTION FOR THE PROBABILITIES.
C                ITOT = CONTAINS THE LENGTH OF THE GRIB MESSAGE CREATED 
C                       IN SUBROUTINE W3FI72                    
C               ITRNC = VARIABLE INDICATING WHETHER OR NOT, 0 OR 1, 
C                       TRUNCATION NEEDS TO BE PERFORMED. THIS IS SET BY 
C                       THE USER IN THE VARIABLE LIST.  USED PRIMARILY FOR
C                       PERCENTS.
C              ITWFFV = VARIABLE HOLDING THE VALUE 255                  
C                JJJJ = CONTAINS THE NUMBER OF COLUMNS IN THE GRID THAT 
C                       THE DATA IS TO BE ANALYZED TO. THIS IS INITIALLY
C                       OUTPUT FROM SUBROUTINE LAT2XY AND THEN IS USED  
C                       AS INPUT ELSEWHERE                              
C              KBUF() = HOLDS THE GRIB MESSAGE FOR EACH DATUM AS IT IS  
C                       OUTPUT FROM W3FI72                      
C                KOFF = NUMBER OF GRIDPOINTS TO SPREAD THE BITMASK OFF  
C                       THE LAND/SEA MASK                     
C               KOFFD = HOLDER FOR KOFF FROM THE PREVIOUS READ          
C                LPDS = CURRENT NUMBER OF ID ARRAY ELEMENTS WHICH GET
C                       USED TO CREATE THE PDS SECTION OF THE GRIB 
C                       MESSAGE
C               NDATA = VARIABLE HOLDING THE NUMBER OF REQUESTED DATA   
C                       ITEMS                                           
C               NDATE = CONTAINS THE CURRENT DATE IN THE FORMAT:  
C                       YR*1000000 + MO*10000 + DA*100 + HR     
C               NG(,) = ARRAY VARIABLY DIMENTIONED IN SUBROUTINE CRESS
C                NORM = VARIABLE TO INDICATE WHETHER OR NOT, 1 OR 0,    
C                       THE REQUESTED DATA IS A NORMAL                  
C               NPASS = HOLDS THE NUMBER OF PASSES TO BE MADE FOR THAT  
C                       DATA IN SUBROUTINE CRESS                
C                NPTS = VARIABLE RETURNED FROM W3FI72 INDICATING THE    
C                       NUMBER OF GRID POINTS IN THE ARRAY FLD          
C                NSTA = VARIABLE HOLDING THE NUMBER OF STATIONS 
C               PDS() = CHARACTER ARRAY USED IN W3FI72 TO HOLD THE      
C                       USER-SUPPLIED PDS SHOULD IPFLAG BE SET TO 1     
C                       (CHAR*1)                                        
C                RADM = RADIUS OF SPHERE OF INFLUENCE TO BE APPLIED AROUND
C                       EACH DATA POINT WITHIN SUBROUTINE CRESS (REAL*4)    
C            STALAT() = ARRAY CONTAINING STATION'S LATITUDE     
C            STALON() = ARRAY CONTAINING STATION'S LONGITUDE    
C              STAX() = ARRAY USED IN SUBROUTINES LAT2XY AND CRESS   
C              STAY() = ARRAY USED IN SUBROUTINES LAT2XY AND CRESS
C                SW() = ARRAY USED IN SUBROUTINE CRESS                  
C               TOP() = ARRAY USED IN SUBROUTINE CRESS                  
C                WS() = ARRAY USED IN SUBROUTINE CRESS                  
C                                                                       
C        SUBPROGRAMS CALLED:                                            
C            UNIQUE:    INTGRIB,CRESS,LAT2XY 
C            MDLLIB:    READ_MOSDA                     
C            W3LIB:     W3FI72,WRTYE                      
C                                                                       
C        EXIT STATES:                                                   
C                COND =       0 ; SUCCESSFUL RUN                        
C                     = NONZERO ; SOMETHING WENT WRONG                  
C                                                                       
C REMARKS:  CURRENTLY LIMITED TO                   
C           A MAXIMUM GRID SIZE OF 200                           
C           BY 200 POINTS, WHICH CORRESPONDS TO A MAXIMUM        
C           GRIB BULLETIN SIZE OF 40000 BYTES,
C           AND A MAXIMUM NUMBER OF VARIABLE IDS OF 3000              
C                                                                      
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  IBM
C                                                                       
C$$$                                                                    
      IMPLICIT NONE
C
      INTEGER :: ND1,ND4,ND5,ND7,MXBUF,MXI,MXJ,LPDS,IER
      INTEGER :: IYR,IMO,IDA,IHR,IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER :: IDCODE(4),IDS(4,ND4),ID(LPDS), IGDS(18),IBDSFL(9)
      INTEGER :: NG(MXI,MXJ),L3264B,IBMAP(MXBUF),ISCALE
      INTEGER :: NDATA,I,IIII,JJJJ,KFILCP,KFILDI,KFILDO,KFILGB
      INTEGER :: KFILGD,KFILLW,KFILP,KFILRA(15),NDATE,NSTA,FILEO
      INTEGER :: INDEX(ND1,5),II,ICYL,ITRNC,ISPRD,KOFF,NCYC,NPASS
      INTEGER :: NPTS,ITOT,ITOP,IMDNUM,NUMRA,IPRT,IOVLND,NORM,IBT
C
      INTEGER :: IPFLAG=0,IGFLAG=1,IZERO=0,ITWFFV=255
      INTEGER :: ISPRDD=-1,KOFFD=-1,IBTD=-1
C
      REAL :: WS(MXI,MXJ),SW(MXI,MXJ),STALAT(ND1),STALON(ND1),FLD(ND1)
      REAL :: BOT(MXI,MXJ),TOP(MXI,MXJ),ADD,RADM,BCUT
      REAL :: STAX(ND1),STAY(ND1),FIELD(MXBUF),GRID(MXI,MXJ),FINIT
C      
      CHARACTER(LEN=1) :: KBUF(MXBUF),PDS(28)
      CHARACTER(LEN=4) :: CGRID,DGRID,BGRID
      CHARACTER(LEN=8) :: CCALL(ND1,6)
      CHARACTER(LEN=10):: ENVVAR
C
      DATA IBDSFL/9*0/,IGDS/0,255,5,4*0,8,4*0,64,5*0/
      DATA DGRID/'ZZZZ'/
     
C
C     INITIALIZE THE BITMAP AND THE FLD VARIABLE
C
      DO I=1,MXBUF
       IBMAP(I)=0
      ENDDO
      DO I=1,ND1
       FLD(I)=-9999.
      ENDDO
C 
      NDATA=0
C
C        CALL INTGRIB TO READ IN THE CONTROL INFORMATION
C        FROM THE GRIB.CN FILE
C
      CALL INTGRIB(KFILDI,KFILDO,IPRT,LPDS,ICYL,BGRID,IOVLND,
     &             BCUT,ID,NDATE,IYR,IMO,IDA,IHR,KFILRA,NUMRA,
     &             KFILGD,KFILLW,KFILGB,CCALL,STALAT,STALON,
     &             NSTA,KFILP,KFILCP,ND1,ND5,ND7,L3264B,IER)
      IF(IER.NE.0)THEN
        WRITE(KFILDO,100)IER
 100    FORMAT(/' ****THE NUMBER OF ERRORS IN INTGRIB WAS ',I4)
      ENDIF
C                                                                       
C        READ IN THE REQUESTED VARIABLES, ONE AT A TIME FROM FORT.25.
C        IN ADDITION TO THE ID (LINE 1), READ IN FIELD INITIALIZATION  
C        CONSTANT, GRID TO ANALYZE ON, # OF PASSES, RADIUS OF INFLUENCE,
C        BITMAP (Y=1,N=0), SPREAD LAND POINTS (Y=1,N=0), HOW MANY GRID       
C        POINTS TO SPREAD, SEVERAL OF THE PDS ENTRIES, TRUNCATION (Y=1,N=0),
C        AND THE SCALING FACTOR (ALL ON LINE 2).           
C                                                                       
  200 NORM=0
      READ(KFILP,210,ERR=365) IDCODE,FINIT,CGRID,NPASS,RADM,IBT,
     1               ISPRD,KOFF,ID(8),ID(9),ID(10),ID(11),
     2               ID(17),ID(18),ID(19),ID(20),ID(21),ID(22),
     3               ITRNC,ISCALE
  210 FORMAT(4I10,/,F4.0,1X,A4,I4,F8.2,I2,14I4)
C
C     KEEP TRACK OF THE NUMBER OF VARIABLES PROCESSED UNTIL THE         
C     TERMINATOR--999999--IS FOUND.                                  
C                                                                       
      NDATA=NDATA+1
C                                                                       
C     IF THE CODE READ IN IS IN A CERTAIN RANGE, THEN IT MUST BE A      
C     NORMAL, SO SET NORM=1.                                            
C                                                                       
      IF((IDCODE(1).GE.400000000).AND.(IDCODE(1).LT.900000000)) NORM=1
      IF(IDCODE(1).EQ.999999) GO TO 370
C
C     PRINT THE VARIABLE ID TO THE OUTPUT FILE FOR OPERATIONS
C
      WRITE(KFILDO,215)NDATA,IDCODE,CGRID
 215  FORMAT(I4,':',4I10,2X,'OUTPUT GRID=',A4)
      IF(IPRT.GE.1)
     1   WRITE(KFILDO,220) NDATA,IDCODE,
     2              CGRID,NPASS,RADM,IBT
  220 FORMAT(/,' DATA ITEM #',I4,':',/,' IDCODE=(4)',4I10,/,
     1' WILL BE ANALYZED ON THE ',A4,' GRID WITH ',I3,' PASSES',/,
     2' AND A RADIUS OF INFLUENCE OF =',F4.1,' GRIDPOINTS',/,
     3' BITMAP? (1=YES,0=NO) = ',I2)
C                                                                       
C        IF DESIRED DATA IS NOT A NORMAL (I.E. FCST DATA):                               
C                                                                       
      IF(NORM.EQ.0) THEN
C                                                                       
C        GET DESIRED DATA FROM THE MOSFCST FILE KFILRA(1) 
C        FOR ALL THE STATIONS
C                   
         CALL READ_MOSDA(KFILDO,KFILRA(1),0,IDCODE,NDATE,CCALL,NSTA,
     &                    FLD,ND1,ND5,ND7,IS0,IS1,IS2,IS4,INDEX,IER)                        
          IF(IER.NE.0)WRITE(KFILDO,230) IER,IDCODE
  230      FORMAT('**** IER=',I4,' UNABLE TO RETRIEVE DATA FROM 
     &            READ_MOSDA.',/,'ID = ',4I10)
C                                                                       
C        SET THE PIECES OF THE ID ARRAY THAT WILL BE USED TO CREATE     
C        THE DATE WITHIN THE GRIB MESSAGE.  THE DATE IS THAT FROM
C        THE MDLPACK'D RECORD FOR FORECAST VARIABLES                           
C        ID(12)=YR OF CEN, ID(23)=CENTURY
C                                                                       
       ID(12)=MOD(IS1(3)-1,100)+1
       ID(13)=IS1(4)
       ID(14)=IS1(5)
       ID(15)=IS1(6)
       ID(16)=IS1(7)
       ID(23)=((IS1(3)-1)/100)+1
C
      ELSE
C                                                                       
C        GET DESIRED DATA FROM THE CONSTANT-TYPE FILE KFILRA(2)
C        FOR ALL THE STATIONS                                                       
C                                                                       
         CALL READ_MOSDA(KFILDO,KFILRA(2),0,IDCODE,NDATE,CCALL,NSTA,
     &                    FLD,ND1,ND5,ND7,IS0,IS1,IS2,IS4,INDEX,IER)                        
        IF(IER.NE.0)WRITE(KFILDO,260) IER,IDCODE
  260    FORMAT('**** IER=',I4,' UNABLE TO RETRIEVE DATA FROM ',
     &            'READ_MOSDA.',/,'ID = ',4I10)
C                                                                       
C        SET THE PIECES OF THE ID ARRAY THAT WILL BE USED TO CREATE     
C        THE DATE WITHIN THE GRIB MESSAGE.  FOR CONSTANT DATA, THE DATE
C        IN THE MDLPACK'D RECORD IS 0, SO TAKE THE DATE INFORMATION
C        FROM THE NCEPDATE.
C        ID(12)=YR OF CEN, ID(23)=CENTURY
C                                                                       
       ID(12)=MOD(IYR-1,100)+1
       ID(13)=IMO
       ID(14)=IDA
       ID(15)=IHR
C        SET THE HOUR TO 00
       ID(16)=00
       ID(23)=((IYR-1)/100)+1
C
      ENDIF
C                                                                       
C        PRINT OUT THE REQUESTED DATA                                   
C                                                                       
      IF(IPRT.GE.2) THEN
        WRITE(KFILDO,280) NDATE,(CCALL(II,1),FLD(II),II=1,NSTA)
  280   FORMAT(/,' DATA FOR NDATE=',I10,/,(4(' ',A4,'-',F10.4,4X)))
      ENDIF
C                                                                       
C        ASSIGN THE GRIB IDENTIFIERS TO THE DATA BASED ON THE ID     
C                                                                       
C        SET THE BITMAP FLAG (Y=1,N=0) ACCORDING TO THE INFORMATION
C        IN THE VARIABLE LIST
C
       ID(7)=IBT
C
C        SET THE SCALING FACTOR. THE USER INDICATES IN THE VARIABLE
C        LIST WHETHER TO USE THE SCALING FACTOR IN THE PACKED DATA
C        (ISCALE = -99), OR TO USE THE ALTERNATE ONE PROVIDED. 
C
       IF(ISCALE.EQ.-99)THEN
         ID(25)=IS1(17)
       ELSE
         ID(25)=ISCALE
       ENDIF
C
C        NOW SET ALL MISSING VALUES IN THE DATA TO -9999.  THIS IS
C        NECESSARY FOR THE SUBROUTINE CRESS
C
      DO 300 I=1,NSTA
         IF(FLD(I).EQ.-9997..OR.FLD(I).EQ.9997..OR.FLD(I).EQ.
     &     9999..OR.FLD(I).EQ.-9999.) THEN
           FLD(I)=-9999.
         ENDIF
 300     CONTINUE
C                                                                       
C        IF THE GRID TO BE ANALYZED ON CHANGES WITH A PARTICULAR IDCODE,
C        OR IF THIS IS THE FIRST VARIABLE,  
C        A CALL TO LAT2XY WILL BE DONE TO OBTAIN NEW STATION X-Y        
C        COORDINATES ON THAT GRID. IF THE GRID REMAINS THE SAME, THE    
C        CALL TO LAT2XY WILL BE SKIPPED.  LAT2XY ALSO COMPUTES
C        THE BITMAP FOR THAT GRID IF NECESSARY                                
C                                                                       
      IF(CGRID.NE.DGRID.OR.ISPRD.NE.ISPRDD.OR.KOFF.NE.KOFFD.
     1   OR.IBT.NE.IBTD) THEN
        CALL LAT2XY(KFILGD,NSTA,STALAT,STALON,CGRID,BGRID,KFILLW,
     1              IOVLND,ISPRD,KOFF,BCUT,IBT,IBMAP,MXBUF,IGDS,STAX,
     2              STAY,IIII,JJJJ,IER)
        IF(IER.NE.0) WRITE(KFILDO,310) IER
  310     FORMAT(' IER IS ',I5,' OUT OF SUBROUTINE LAT2XY')
C
C        SET THE PORTION OF THE PDS THAT CONTAINS THE GRID NUMBER.  
C        ALSO, HOLD ON TO THE VALUES FOR CGRID, IPSRD, KOFF, AND IBT
C        TO DETERMINE WHETHER OR NOT TO CALL LAT2XY THE NEXT TIME AROUND.
C
        ID(5)=IGDS(2)
        DGRID=CGRID
        KOFFD=KOFF
        ISPRDD=ISPRD
        IBTD=IBT
      ENDIF
C                                                                       
C        PASS THE DATA THROUGH A CRESSMAN ANALYSIS ROUTINE ON A         
C        USER-SPECIFIED GRID.                                           
C                                                                       
      CALL CRESS(ND1,MXI,MXJ,WS,SW,BOT,TOP,NG,NPASS,RADM,NSTA,STAX,
     1           STAY,FLD,FINIT,IIII,JJJJ,GRID,FIELD,IER)
      IF(IER.NE.0) WRITE(KFILDO,320) IER
 320    FORMAT(' IER IS NOT ZERO COMING OUT OF CRESS.  IT IS= ',I4)
C
C        IF THE TRUNCATION/MULTIPLICATION FLAG IS TURNED ON (ITRUNC = 1), 
C        THEN MULTIPLY THE FIELD BY 100 TO CONVERT FROM DECIMAL TO 
C        PERCENT, THEN SET ALL VALUES LESS THAN ZERO TO 0 AND ALL 
C        VALUES GREATER THAN 100 TO 100.  PRESENTLY, THIS IS USED ONLY
C        FOR PROBABILITIES
C
      IF(ITRNC.EQ.1) THEN
      ITOP=IIII*JJJJ
      DO 330 I=1,ITOP
         IF(IBMAP(I).EQ.1) THEN
           FIELD(I)=FIELD(I)*100.0
           IF((NINT(FIELD(I)).LT.0).AND.
     &         (NINT(FIELD(I)).NE.-9999)) THEN
             FIELD(I)=0.0
           ENDIF
           IF(NINT(FIELD(I)).GT.100) THEN
             FIELD(I)=100.0
           ENDIF
         ENDIF
  330 CONTINUE
      ENDIF
C                                                                       
C        CREATE THE GRIB MESSAGE USING THE NCEP UTILITY W3FI72.                               
C                                                                       
C        IF THE GRID IS NON-STANDARD (TO NCEP) THEN PASS THE GDS 
C        (IFLAG=1) AND THE GRID NUMBER OF 255, OTHERWISE PASS THE GRID 
C        NUMBER WHICH IS IN ID(5)
C
      IF(ID(5).NE.ITWFFV) IGFLAG=0
      ID(6)=IGFLAG
C
      CALL W3FI72(IZERO,FIELD,IBMAP,IZERO,IPFLAG,ID,PDS,IGFLAG,ID(5),
     1IGDS,IZERO,IZERO,IBMAP,IIII*JJJJ,IBDSFL,NPTS,KBUF,ITOT,IER)
C
      IF(ITOT.GT.MXBUF) WRITE(KFILDO,340) ITOT,MXBUF
  340   FORMAT(' ITOT FROM W3FI72 ',I6,' IS GREATER THAN MXBUF ',I6)
      IF(IER.NE.0) THEN
        WRITE(KFILDO,350) IER
  350   FORMAT(' IER IS NOT ZERO COMING OUT OF W3FI72.  IT IS= ',I6)
      ELSE
C                                                                       
C        IF THERE ARE NO ERRORS IN CREATING THE GRIB MESSAGE, WRITE IT  
C        TO A FILE.                                                     
C        ON THE IBM, WE CAN NOT MAKE A FILE UNBLOCKED THROUGH ASSIGN
C        USE BAOPEN AND WRYTE TO GET PROPER FORMAT.  I LEFT THE OLD
C        WRITE STATEMENT THAT WAS USED ON THE CRAY IN, JUST IN CASE
C                                                                       
        CALL WRYTE(KFILGB,ITOT,KBUF)
C       WRITE(KFILGB) (KBUF(LLL),LLL=1,ITOT)
      ENDIF
C                                                                       
C        FILL THE DATA-HOLDING ARRAY WITH MISSINGS AND GO BACK TO READ  
C        THE NEXT VARIABLE IN THE INPUT LIST.                                         
C                                                                       
      DO 360 II=1,ND1
        FLD(II)=-9999.
 360  CONTINUE
      GO TO 200
C
C        IF THERE WAS AN ERROR READING THE VARIABLE LIST, COME HERE.
C        WRITE OUT A MESSAGE, SET FLD TO MISSING, AND MOVE ON TO THE 
C        NEXT VARIABLE IN THE LIST
C
 365  WRITE(KFILDO,366)NDATA+1
 366  FORMAT(/'****ERROR READING VARIABLE NUMBER ',I4,' FROM VARIABLE ',
     &       'LIST.  MOVING ON TO NEXT VARIABLE.')
      DO II=1,ND1
        FLD(II)=-9999.
      ENDDO
      GO TO 200
C                                                                       
C        PRINT OUT A MESSAGE INDICATING THAT THE CODE HAS COMPLETED.    
C                                                                       
 370  WRITE(KFILDO,400)
 400  FORMAT(' ALL REQUESTED DATA HAS BEEN OBTAINED.')
      RETURN
      END
