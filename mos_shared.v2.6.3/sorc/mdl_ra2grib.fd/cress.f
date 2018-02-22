      SUBROUTINE CRESS(MXSTA,MXI,MXJ,WS,SW,BOT,TOP,NG,NPASS,RADM,NSTA
     1                 ,STAX,STAY,STDATA,FINIT,II,JJ,GRID,FIELD,JERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    CRESS       PERFORMS CRESSMAN ANALYSIS                 
C   PRGMMR: SETTELMAIER      ORG: W/OSD211   DATE: 95-08-30             
C                                                                       
C ABSTRACT: PERFORMS A CRESSMAN ANALYSIS ON RANDOMLY SPACED DATA        
C   POINTS RESULTING IN A UNIFORMLY-SPACED GRID OF DATA POINTS.         
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   95-08-30  SETTELMAIER                                               
C   96-11-25  SETTELMAIER - UPDATED ROUTINE TO HANDLE VALUES GREATER
C                           THAN 9999. THROUGH THE USE OF THE HITOP
C                           AND HIBOT VARIABLES.  THIS WAS CHANGED 
C                           SINCE WE HAD TROUBLE HANDLING THE LARGE
C                           VALUES IN THE SOLAR ENERGY VARIABLE.
C   01-08-16  ALLEN       - NO CHANGES WERE MADE TO INCORPORATE THIS
C                           ROUTINE FOR USE WITH MOS2000 EXCEPT FOR
C                           REMOVING SOME WRITE STATEMENTS THAT WERE
C                           COMMENTED OUT.
C                                                                       
C USAGE:                                                                
C                                                                       
C     SEE BELOW FOR MDL STANDARDS                                       
C                                                                       
C        AUGUST 1995   SETTELMAIER   MDL   NAS9000                      
C                                                                       
C        PURPOSE                                                        
C            PERFORMS A CRESSMAN ANALYSIS ON RANDOMLY SPACED DATA       
C            POINTS RESULTING IN A UNIFORMLY-SPACED GRID OF DATA POINTS.
C                                                                       
C        VARIABLES                                                      
C                ASTA = VARIABLE TO KEEP TRACK OF THE NUMBER OF STATIONS
C                       FOR WHICH VALID DATA IS FOUND (REAL)
C             FIELD() = ARRAY CONTAINING DATA AFTER THE CRESSMAN        
C                       ANALYSIS   
C               FINIT = INITIALIZATION CONSTANT TO BE APPLIED TO THE 
C                       GRID PRIOR TO THE CRESSMAN ANALYSIS (INPUT)
C              ICOUNT = VARIABLE USED TO KEEP TRACK OF TOTAL NUMBER OF
C                       GRID POINTS AS THE 1-D FIELD ARRAY IS BEING 
C                       FILLED FROM THE 2-D GRID ARRAY
C                  II = CONTAINS THE NUMBER OF ROWS IN THE GRID THAT 
C                       THE DATA IS TO BE ANALYZED TO. 
C                  JJ = CONTAINS THE NUMBER OF COLUMNS IN THE GRID THAT 
C                       THE DATA IS TO BE ANALYZED TO. 
C         IL,IM,JL,JM = INTEGER HOLDERS FOR COMPARISONS BETWEEN THE
C                       STATION DATA AND THE RESULTING GRID DATA
C                  XL = TEMPORARY REAL STORAGE OF IL
C                  YL = TEMPORARY REAL STORAGE OF JL
C                ITER = LOOP COUNTER FOR NUMBER OF PASSES LOOP
C                JERR = CONTAINS THE ERROR CODE RETURNED FROM CRESS   
C                       =3 WHEN ENTIRE DATA RECORD IS MISSING - NOT 
C                          NECESSARILY A PROBLEM 
C               MXSTA = MAXIMUM NUMBER OF STATIONS THIS PROGRAM CAN     
C                       HANDLE                                   
C                 MXI = MAXIMUM NUMBER OF ROWS ALLOWED IN THE GRID      
C                       FIELD                                    
C                 MXJ = MAXIMUM NUMBER OF COLUMNS ALLOWED IN THE GRID   
C                       FIELD                                    
C               NPASS = NUMBER OF SMOOTHING PASSES TO MAKE OVER THE GRID
C                NSTA = NUMBER OF STATIONS                              
C                  PP = TEMPORARY REAL VARIABLE                         
C           STDATA( ) = CONTAINS DATA FOR STATIONS                      
C             STAX( ) = ARRAY CONTAINING X-COORD OF STA IN THE GRID     
C             STAY( ) = ARRAY CONTAINING Y-COORD OF STA IN THE GRID     
C            TOP( , ) = ARRAY CONTAINING THE LARGEST VALUE IN SPHERE OF 
C                       INFLUENCE AROUND A GRID POINT  
C               TOTAL = VARIABLE USED TO ARRIVE AT THE AVERAGE VALUE OF
C                       THE FIELD OF GRIDDED DATA, SHOULD INITIALIZATION
C                       NOT BE DESIRED
C            BOT( , ) = SAME AS TOP EXCEPT CONTAINING THE SMALLEST VALUE
C           GRID( , ) = GRID POINT VALUES OF THE DATA ON THE GRID       
C             NG( , ) = NO. OF STA VALUES IN THE RAD. OF INFLUENCE      
C                       AROUND A GRID POINT                             
C                RADM = RADIUS OF SPHERE(GRID POINTS) OF INFLUENCE      
C                RADL = RADIUS OF SPHERE - 1 (EQUALS RADM - 1) 
C                 RSQ = RADIUS SQUARED OF SPHERE OF INFLUENCE-DIST      
C                       SQUARED AT WHICH WEIGHTING FUNCTION TO CORRECT  
C                       THE ANALYSIS GOES TO 0                          
C             SW( , ) = SUM OF ANALYSIS CORRECTION FACTORS AT GRID PTS  
C             WS( , ) = SUM OF CORRECTIONS(FACTOR*ERROR)AT EACH GRID PT 
C                  GT = VALUE OF TEMP GRID PT FIELD INTERPOLATED TO STA 
C                  DV = DIFFERENCE BETWEEN THE ACTUAL STA VALUE AND THE 
C                       INTERPOLATED VALUE                              
C                  YJ = DISTANCE(NEGATIVE OF GRID POINT(Y-DIR) FROM     
C                       STATION IN SPHERE OF INFLUENCE                  
C                  XI = DIST(NEGATIVE) OF GRID POINT(X-DIR) FROM STA    
C                       IN SPHERE OF INFLUENCE                          
C                  DD = DIST OF STATION FROM GRID POINT                 
C                  D2 = ADDITIONAL DIST OF STATION FROM GRID POINT INFO 
C                  DX = A DIST RELATIVE TO THE X-DIRECTION              
C                  DY = A DIST RELATIVE TO THE Y-DIRECTION              
C                 CCF = CRESSMAN CORRECTION FACTOR                      
C                  PP = CORRECTED GRID POINT VALUE                      
C                 NGP = NUMBER OF GRID POINTS (EQUALS II TIMES JJ)
C                   X = TEMPORARY HOLDER FOR THE STATION'S X COORDINATE
C                   Y = TEMPORARY HOLDER FOR THE STATION'S Y COORDINATE
C                  IX = INTEGER HOLDER FOR THE X VARIABLE
C                  JY = INTEGER HOLDER FOR THE Y VARIABLE
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 77                                                
C   MACHINE:  CRAY                                                      
C                                                                       
C$$$                                                                    
      INTEGER NG(MXI,MXJ)
      REAL STDATA(MXSTA),STAX(MXSTA),STAY(MXSTA),TOP(MXI,MXJ),
     1     BOT(MXI,MXJ),GRID(MXI,MXJ),SW(MXI,MXJ),WS(MXI,MXJ),
     2     FIELD(MXI*MXJ),TOTAL,ASTA
C      DATA HITOP/-999900000000000000000./,HIBOT/999900000000000000000./
      DATA HITOP/-9999./,HIBOT/9999./
      TOTAL=0.
      ASTA=0.
      JERR=0
      NGP=II*JJ
      DO 10 J=1,NSTA
         IF(ABS(STDATA(J)).LT.9996.5.OR.ABS(STDATA(J)).GT.9999.5) THEN
           TOTAL=TOTAL+STDATA(J)
           ASTA=ASTA+1.
         ENDIF
  10  CONTINUE
      IF(ASTA.EQ.0.) THEN
        JERR=3
        WRITE(6,15) JERR
  15    FORMAT('**** JERR= ',I1,' IN SUBROUTINE CRESS RIGHT BEFORE
     1 DIVISION BY ZERO',/,'NO DATA AVAILABLE.')
        RETURN
      ENDIF
      TOTAL=TOTAL/ASTA
      IF(FINIT.NE.999.) TOTAL=FINIT
      DO 20 J=1,NGP
         FIELD(J)=TOTAL
  20  CONTINUE
C                                                                       
C        NOW DO THE CRESSMAN ANALYSIS                                   
C                                                                       
      DO 40 J=1,JJ
        DO 30 I=1,II
C                                                                       
C       INITIALIZE THE ARRAYS                                           
C                                                                       
          TOP(I,J)=HITOP
          BOT(I,J)=HIBOT
          GRID(I,J)=0.
          NG(I,J)=0
  30    CONTINUE
  40  CONTINUE
      RADL=RADM-1.
      DO 100 N=1,NSTA
        IF (STDATA(N).EQ.-9997..OR.STDATA(N).EQ.9997..OR.STDATA(N).EQ.
     1     9999..OR.STDATA(N).EQ.-9999.) GOTO 100
C                                                                       
C        FOR EACH STATION, CHECK THE X/Y COORDINATES                    
C        INITIALIZE THE GRID PT ARRAY USING AVAILABLE DATA WITHIN A     
C        RADIUS OF RADM GRID POINTS OF THE GRID POINT OF INTEREST.      
C                                                                       
        JL=MAX1(1.,STAY(N)-RADL)
        JM=MIN1(FLOAT(JJ),STAY(N)+RADM)
        IL=MAX1(1.,STAX(N)-RADL)
        IM=MIN1(FLOAT(II),STAX(N)+RADM)
        DO 90 J=JL,JM
          DO 80 I=IL,IM
            IF(STDATA(N)-TOP(I,J))60,70,50
  50        TOP(I,J)= STDATA(N)
  60        BOT(I,J)= AMIN1(STDATA(N),BOT(I,J))
  70        GRID(I,J)=GRID(I,J)+STDATA(N)
            NG(I,J)= NG(I,J)+ 1
  80      CONTINUE
  90    CONTINUE
  100 CONTINUE
C                                                                       
C        FORM 1ST GUESS FIELD BY COMPUTING AN AVERAGE VALUE FIELD       
C                                                                       
      DO 120 J=1,JJ
        DO 110 I=1,II
C                                                                       
C         GRID=-9999 IF NO STA VALUES INFLUENCE A PARTICULAR GRID POINT 
C                                                                       
          IF(NG(I,J).GT.0) THEN
            GRID(I,J)=GRID(I,J)/FLOAT(NG(I,J))
          ELSE
C           GRID(I,J)=-9999.                                            
            GRID(I,J)=TOTAL
          ENDIF
C                                                                       
C        THE 1ST GUESS IS THE AVERAGE VALUE OF ALL STA OBS SURROUNDING  
C        THE PARTICULAR GRID POINT.                                     
C                                                                       
  110   CONTINUE
  120 CONTINUE
      RADM=RADM+1.
      RADL=RADM-1.
C                                                                       
C        DO NPASS ITERATIONS OR PASSES FOR THE ANALYSIS                 
C                                                                       
      DO 200 ITER=1,NPASS
        RADM= RADM - 1.
        RADL= RADL - 1.
        RSQ= RADM**2
        DO 140 J= 1,JJ
          DO 130 I= 1,II
            WS(I,J)= 0.
            SW(I,J)= 0.
  130     CONTINUE
  140   CONTINUE
C                                                                       
C        ON EACH PASS, USE ALL OF THE STATION VALUES                    
C                                                                       
        DO 170 N= 1,NSTA
          IF (STDATA(N).EQ.-9997..OR.STDATA(N).EQ.9997..OR.STDATA(N).EQ.
     1       9999..OR.STDATA(N).EQ.-9999.) GO TO 170
          X=STAX(N)
          Y=STAY(N)
          IX= X
          JY= Y
          DX= X - FLOAT(IX)
          DY= Y-FLOAT(JY)
C                                                                       
C        FROM THE GUESS FIELD INTERPOLATE TO EACH STATION               
C                                                                       
          GT= GRID(IX,JY)+(GRID(IX+1,JY)-GRID(IX,JY))*DX
     2        +(GRID(IX,JY+1)-GRID(IX,JY))*DY
     3        +(GRID(IX,JY)-GRID(IX+1,JY)-
     4        GRID(IX,JY+1)+GRID(IX+1,JY+1))*DX*DY
C                                                                       
C        DIFFERENCE BETWEEN THE STA VALUE(OBS) AND THE INTERP. VALUE    
C                                                                       
          DV=STDATA(N) - GT
C                                                                       
C        SET UP THE LIMITS OF THE SPHERE OF INFLUENCE                   
C                                                                       
          JL=MAX1(1.,Y-RADL)
          YL=JL
          JM=MIN1(FLOAT(JJ),Y+RADM)
          IL=MAX1(1.,X-RADL)
          XL=IL
          IM=MIN1(FLOAT(II),X+RADM)
          YJ=YL-Y-1.
          DO 160 J=JL,JM
            YJ= YJ+ 1.
            DD= YJ*YJ
            XI=XL-X-1.
            DO 150  I=IL,IM
              XI=XI+ 1.
C                                                                       
C        CALCULATE THE STA DIST FROM THE GRID PT                        
C                                                                       
              D2= DD +XI**2
C                                                                       
C        CHECK IF THE STA IS CLOSE TO THE GRID PT TO INFLUENCE IT       
C                                                                       
              IF(D2.LE.RSQ) THEN
C                                                                       
C        CALCULATE THE CORRECTION FACTOR                                
C                                                                       
                CCF= (RSQ-D2)/(RSQ+D2)
                SW(I,J)=SW(I,J)+CCF
                WS(I,J)= WS(I,J) + CCF*DV
              ENDIF
  150       CONTINUE
  160     CONTINUE
  170   CONTINUE
        DO 190 J=1,JJ
          DO 180 I=1,II
            IF(SW(I,J).GE.0.00001) THEN
C                                                                       
C        GRID PT VALUE IS CORRECTED BY AN AVERAGE CORRECTION VALUE      
C                                                                       
              PP=GRID(I,J)+WS(I,J)/SW(I,J)
              GRID(I,J)=AMAX1(BOT(I,J),AMIN1(TOP(I,J),PP))
            ENDIF
  180     CONTINUE
  190   CONTINUE
  200 CONTINUE
      ICOUNT=0
      DO 220 J=1,JJ
        DO 210 I=1,II
          ICOUNT=ICOUNT+1
          GRID(I,J)=AMIN1(HIBOT,AMAX1(HITOP,GRID(I,J)))
          FIELD(ICOUNT)=GRID(I,J)
  210   CONTINUE
  220 CONTINUE
      RETURN
      END
