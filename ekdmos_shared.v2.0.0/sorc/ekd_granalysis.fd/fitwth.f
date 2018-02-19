      SUBROUTINE FITWTH(KFILDO,IP24,ID,NDATE,CCALL,XP,YP,
     1                  XDATA,ND1,WHOLD,LNDSEA,ELEV,NSTA,NWITH,
     2                  P,NX,NY,MESH,SEALND,NXE,NYE,MESHE,N4P,ISTOP,IER)
C
C        JUNE      2005   GLAHN   TDL   MOS-2000
C        JULY      2005   GLAHN   ADDED FREQUENCY TABLES
C        JULY      2005   GLAHN   CHANGED ITABLE(12) FROM 8 TO 10
C        AUGUST    2005   GLAHN   CHANGED TO PRINT 4 DECIMAL PLACES
C        AUGUST    2005   GLAHN   MODIFIED COMMENTS FOR LNDSEA( )
C        JANUARY   2006   GLAHN   CLARIFIED PURPOSE
C        MARCH     2006   GLAHN   ADDED ISTOP( ) TO CALL AND IP14 AND
C                                 ISTOP( ) TO CALL TO ITRPSL
C        MARCH     2006   GLAHN   ADDED N4P
C        APRIL     2006   GLAHN   CHANGED IP14 TO IP24
C        OCTOBER   2006   GLAHN   ADDED FT01 AND FT02; ADDED CHECK
C                                 FOR XDATA( ) = 9999 FOR WITHHELD
C                                 STATIONS
C        JANUARY   2007   GLAHN   ADDED WRITING OF ORIGINAL CYCLE 1
C                                 DATA TO FT01 AND FT02
C        JUNE      2007   GLAHN   MOVED XDATA,ND1 TO LINE 2 IN CALL
C        NOVEMBER  2007   GLAHN   COMMENT AT 112 AND 123
C        DECEMBER  2007   GLAHN   ADDED ISTOP(6) CAPABILITY; SPELL CHECK
C        FEBRUARY  2009   GLAHN   ADDED DIAGNOSTIC LINE TO UNITS
C                                 IFT01 AND IFT02
C
C        PURPOSE
C            TO DETERMINE THE MAE FIT TO NWITH WITHHELD STATIONS, THE
C            NON-WITHHELD STATIONS, AND THE TOTAL GROUP.  NOTE THAT THIS
C            IS FOR THE COMPLETE ANALYSIS AREA, NOT JUST THE DISPOSABLE
C            AREA.
C
C            IN ADDITION, THE CALL LETTERS AND THE INTERPOLATED VALUE
C            ARE WRITTEN TO FT01 FOR THE WITHHELD STATIONS AND TO FT02 
C            FOR THE NONWITHHELD STATIONS.  EACH IS HEADED WITH
C            VARIABLE ID AND DATE/TIME.  THE ON/OFF SWITCH FOR THIS
C            CAPABILITY IS IFT0102.  THS IS HARDWIRED INTO A DATA
C            STATEMENT.
C         
C
C        DATA SET USE
C            KFILDO   - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C                IP24 = UNIT NUMBER FOR WRITING FIT TO WITHHELD
C                       STATIONS, IF ANY, AND NON-WITHHELD STATIONS OVER
C                       WHOLE ANALYSIS AREA WHEN NWITH NE 0.
C                       (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C                IP24 = UNIT NUMBER FOR LISTING COMPUTED LAPSE
C                       RATES,PROBLEMS WITH LAPSE RATES, AND SCORES
C                       OF DATA FIT FOR WITHHELD AND NON-WITHHELD POINTS.
C                       (INPUT)
C               ID(J) = THE VARIABLE ID'S BEING DEALT WITH(J=1,4).
C                       (INPUT)
C               NDATE = THE DATE/TIME OF THE RUN.  (INPUT)
C            CCALL(J) = STATION CALL LETTERS (J=1,NSTA).  (CHARACTER*8)
C                       (INPUT)
C               XP(K) = THE X POSITION FOR STATION K (K=1,NSTA) ON 
C                       THE ANALYSIS GRID AREA AT THE CURRENT NOMINAL
C                       GRID MESH LENGTH MESH.  (INPUT)
C               YP(K) = THE Y POSITION FOR STATION K (K=1,NSTA) ON 
C                       THE ANALYSIS GRID AREA AT THE CURRENT NOMINAL
C                       GRID MESH LENGTH MESH.  (INPUT)
C          XDATA(K,J) = HOLDS THE DATA ANALYZED (K=1,NSTA) (J=1,6).
C                       ALL VALUES = 9999 SHOULD HAVE BEEN ELIMINATED.
C                       XDATA(K,1) HOLDS THE DATA ANALYZED; 
C                       XDATA(K,2) HOLDS THE CYCLE 1 DATA.  (INPUT)
C                 ND1 = FIRST DIMENSION OF XDATA( , ).  (INPUT)
C            WHOLD(K) = ARRAY HOLDING WITHHELD DATA (K=1,NSTA).
C                       A VALUE WITHHELD WILL BE IN WHOLD( );
C                       OTHERWISE, WHOLD( ) = -9999.  (INPUT).
C           LNDSEA(K) = LAND/SEA INFLUENCE FLAG FOR EACH STATION
C                       (K=1,ND1).
C                       0 = WILL BE USED FOR ONLY OCEAN WATER (=0)
C                           GRIDPOINTS.
C                       3 = WILL BE USED FOR ONLY INLAND WATER (=3)
C                           GRIDPOINTS.
C                       6 = WILL BE USED FOR BOTH INLAND WATER (=3)
C                           AND LAND (=9) GRIDPOINTS.
C                       9 = WILL BE USED FOR ONLY LAND (=9) GRIDPOINTS.
C                       (INPUT)
C             ELEV(K) = ELEVATION OF STATIONS (K=1,NSTA).  NOT ACTUALLY
C                       USED.  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C               NWITH = NUMBER OF STATIONS WITHHELD.  (INPUT)
C            P(IX,JY) = THE ANALYSIS INTO WHICH INTERPOLATION IS DESIRED
C                       (IX=1,NY) (JY=1,NY).  (INPUT)
C               NX,NY = SIZE OF P( , ).  (INPUT)
C                MESH = THE NOMINAL MESH LENGTH OF THE GRID BEING DEALT
C                       WITH WHOSE DIMENSIONS ARE NX AND NY, AND 
C                       THE STATION LOCATIONS IN XP( ) AND YP( ) ARE
C                       IN REFERENCE TO.  (INPUT)
C           SEALND(J) = THE LAND/SEA MASK (J=1,NXE*NYE).  0 = WATER;
C                       1 = LAND.  (INPUT)
C                 NXE = X-EXTENT OF SEALND( ) AT NOMINAL MESH LENGTH
C                       MESHE.  (INPUT)
C                 NYE = Y-EXTENT OF SEALND( ) AT NOMINAL MESH LENGTH
C                       MESHE.  (INPUT)
C               MESHE = THE NOMINAL MESH LENGTH OF THE TERRAIN GRID.
C                       (INPUT)
C                 N4P = 4 INDICATES THE SURROUNDING 4 POINTS WILL BE
C                         CHECKED WHEN TRYING TO FIND A GRIDPOINT OF
C                         THE SAME TYPE AS THE DATUM AND INTERPOLATION
C                         CAN'T BE DONE.  CURRENTLY, THIS IS ALWAYS
C                         DONE (DOES NOT REQUIRE N4P=4).
C                       12 SAME AS ABOVE, EXCEPT 12 ADDITIONAL POINTS
C                         WILL BE CHECKED WHEN NONE OF THE 4 POINTS
C                         ARE OF THE CORRECT TYPE.
C                       N4P IS OPERATIVE ONLY WHEN THE DATUM AND
C                       THE SURROUNDING 4 POINTS ARE OF MIXED TYPE.
C                       (INPUT)
C            ISTOP(J) = ISTOP(1)--IS INCREMENTED BY 1 EACH TIME AN ERROR 
C                                 OCCURS.
C                       ISTOP(2)--IS INCREMENTED WHEN THERE ARE
C                                 FEW DATA (200) FOR AN ANALYSIS.
C                       ISTOP(3)--IS INCREMENTED WHEN A DATA RECORD 
C                                 COULD NOT BE FOUND.
C                       ISTOP(4)--IS INCREMETED WHEN A LAPSE RATE COULD
C                                 NOT BE COMPUTED OR HAS TOO FEW CASES
C                                 TO BE USED.
C                       ISTOP(5)--IS INCREMENTED WHEN NO NON-MISSING
C                                 GRIDPOINT AROUND THE DATA POINT IS
C                                 OF THE SAME TYPE.
C                       ISTOP(6)--IS INCREMENTED WHEN THERE IS A PROBLEM
C                                 WITH MAKING BOGUS STATIONS.
C                       (INPUT/OUTPUT)
C                 IER =   0 = GOOD RETURN.
C                       777 = NUMBER OF NON-MISSING VERIFICATION
C                             STATIONS NOT EQUAL TO NUMBER OF WITHHELD
C                             STATIONS.
C                       (OUTPUT)
C             IFT0102 = 1 = WRITE CALL LETTERS AND INTERPOLATED VALUE
C                           FOR EACH WITHHELD STATION TO FT01 AND
C                           FOR EACH NON-WITHHELD STATION TO FT02.
C                           SET BY DATA STATEMENT.
C               IFT01 = UNIT NUMBER FOR WRITING WITHHELD STATIONS
C                       (INTERNAL)
C               IFT02 = UNIT NUMBER FOR WRITING NON-WITHHELD STATIONS
C                       (INTERNAL)
C                  
C        1         2         3         4         5         6         7 X
C
      PARAMETER (NCAT=14)
C
      CHARACTER*8 CCALL(NSTA)
C
      DIMENSION ID(4)
      DIMENSION XDATA(ND1,6),LNDSEA(NSTA),ELEV(NSTA),WHOLD(NSTA),
     1          XP(NSTA),YP(NSTA)
      DIMENSION P(NX*NY)
      DIMENSION SEALND(NXE*NYE)
      DIMENSION TABLE(NCAT),NCT(NCAT),NCTWH(NCAT),
     1                     FNCT(NCAT),FNCTWH(NCAT)
      DIMENSION ISTOP(6)
C
      DATA IFIRST/0/
      DATA RUWH/0./,
     1     RUNWH/0./,
     2     NRUNWH/0/,
     3     NRUWH/0/,
     4     ARUNWH/9999./,
     5     ARUWH/9999./
      DATA TABLE/-15.,-10.,-6.,-4.,-2.,-1.,0.,1.,2.,4.,6.,10.,15.,9999./
      DATA NCT  /NCAT*0/,
     1     NCTWH/NCAT*0/
      DATA IFT0102/1/
C
      SUMNWH=0.
      SUMWH=0.
      SUMALL=0.
      NUMNWH=0
      NUMWH=0
      NUMALL=0
      IFT01=1
      IFT02=2
C
      IF(IFT0102.EQ.1)THEN
         WRITE(IFT01,110)(ID(J),J=1,4),NDATE
 110     FORMAT(/,' DATA FOR WITHHELD STATIONS FOR',
     1               ' VARIABLE',3I10.9,I10.3,'  FOR DATE',I12//,
     2            '        INTERPOLATED  DATA ANALYZED  FIRST CYCLE',
     3            '  SECOND CYCLE (IF ANY)')
         WRITE(IFT02,111)(ID(J),J=1,4),NDATE
 111     FORMAT(/,' DATA FOR NON WITHHELD STATIONS FOR',
     1               ' VARIABLE',3I10.9,I10.3,'  FOR DATE',I12//,
     2            '        INTERPOLATED  DATA ANALYZED  FIRST CYCLE',
     3            '  SECOND CYCLE (IF ANY)')
      ENDIF
C        
      DO 150 K=1,NSTA
c
      IF(WHOLD(K).LT.-9998.)THEN
C        MAKE TEST THIS WAY IN CASE EXACT FLOATING POINT IS NOT
C        MATCHED FOR -9999.
C
C*********************************************************************
C           THIS SECTION FOR NON-WITHHELD DATA.
C           XDATA( , ) FOR NON WITHHELD STATIONS CAN BE MISSING.
C*********************************************************************
C
         IF(XDATA(K,1).NE.9999.)THEN
            IF(XP(K).LT.1..OR.
     1         YP(K).LT.1..OR.
     2         XP(K).GE.NX.OR.
     3         YP(K).GE.NY)GO TO 150
C              THE ABOVE ASSURES THERE WILL BE 4 SURROUNDING GRIDPOINTS
C              FOR INTERPOLATION.
C
            CALL ITRPSL(KFILDO,IP24,P,NX,NY,CCALL(K),XP(K),YP(K),
     1                  LNDSEA(K),SEALND,NXE,NYE,
     2                  MESH,MESHE,N4P,BB,ISTOP,IER)
C              BB IS THE "INTERPOLATED" VALUE FROM THE GRID IN P( , ).
C              BILINEAR INTERPOLATION IS USED WHEN THE 4 POINTS NEEDED,
C              AS SPECIFIED IN SEALND( , ), ARE OF THE TYPE INDICATED
C              BY LNDSEA( ).  OTHERWISE, THE CLOSEST GRIDPOINT OF THE
C              CORRECT TYPE IS USED.  IF NONE IS AVAILABLE, THE VALUE
C              RETURNED IS MISSING.
C
            IF(BB.NE.9999.)THEN
               DIFF=BB-XDATA(K,1)
C
               IF(IFT0102.EQ.1)THEN
                  WRITE(IFT02,112)CCALL(K),BB,(XDATA(K,J),J=1,3)
C                    THIS WILL PRINT THE DATA ANALYZED, THE FIRST CYCLE
C                    DATA, AND THE 2ND CYCLE DATA (WHICH MAY BE MISSING).
 112              FORMAT(A8,F9.1,F15.1,F13.1,F12.1)
               ENDIF
C
               DO 115 J=1,NCAT
C
               IF(DIFF.LE.TABLE(J))THEN
                  NCT(J)=NCT(J)+1
                  GO TO 118
               ENDIF
C
 115           CONTINUE
C
               WRITE(KFILDO,116)
 116           FORMAT(/' ****THIS IS A FATAL ERROR.  STOP IN FITWTH',
     1                 ' AT 116.')
               STOP 116
C               
 118           SUMNWH=SUMNWH+ABS(DIFF)
               RUNWH=RUNWH+ABS(DIFF)
               NUMNWH=NUMNWH+1
               NRUNWH=NRUNWH+1
               SUMALL=SUMALL+ABS(DIFF)
               NUMALL=NUMALL+1
            ENDIF
C
CD           WRITE(KFILDO,120)K,XP(K),YP(K),BB,XDATA(K,1),NUMNWH,SUMNWH
CD120        FORMAT(' AT 120 IN FITWTH--K,XP(K),YP(K),BB,XDATA(K,1),',
CD    1             'NUMNWH,SUMNWH',I6,2F8.2,2F8.2,I6,F10.2)
         ENDIF
C
      ELSE
C
C*********************************************************************
C           THIS SECTION FOR WITHHELD DATA.  IN WITHHOLDING DATA,
C           ONLY STATIONS WITH DATA WERE WITHHELD, SO NO NEED TO
C           CHECK FOR MISSINGS.
C*********************************************************************
C
CD        WRITE(KFILDO,122)K,CCALL(K),WHOLD(K),XP(K),YP(K),XDATA(K,1),
CD    1                    LNDSEA(K)
CD122     FORMAT(' AT 122 IN FITWTH--K,CCALL(K),WHOLD(K),XP(K),YP(K),',
CD    1          'XDATA(K,1),LNDSEA(K)',I6,2X,A8,4F10.1,I6)
C
         IF(XP(K).LT.1..OR.
     1      YP(K).LT.1..OR.
     2      XP(K).GE.NX.OR.
     3      YP(K).GE.NY)GO TO 150
C           THE ABOVE ASSURES THERE WILL BE 4 SURROUNDING GRIDPOINTS
C           FOR INTERPOLATION.
C
         CALL ITRPSL(KFILDO,IP24,P,NX,NY,CCALL(K),XP(K),YP(K),
     1               LNDSEA(K),SEALND,NXE,NYE,
     2               MESH,MESHE,N4P,BB,ISTOP,IER)
C           BB IS THE "INTERPOLATED" VALUE FROM THE GRID IN P( , ).
C           BILINEAR INTERPOLATION IS USED WHEN THE 4 POINTS NEEDED,
C           AS SPECIFIED IN SEALND( , ), ARE OF THE TYPE INDICATED
C           BY LNDSEA( ).  OTHERWISE, THE CLOSEST GRIDPOINT OF THE
C           CORRECT TYPE IS USED.  IF NONE IS AVAILABLE, THE VALUE
C           RETURNED IS MISSING.
C
         IF(BB.NE.9999.)THEN
            DIFF=BB-WHOLD(K)
C
            IF(IFT0102.EQ.1)THEN
               WRITE(IFT01,123)CCALL(K),BB,(XDATA(K,J),J=1,3)
C                 THIS WILL PRINT THE DATA ANALYZED, THE FIRST CYCLE
C                 DATA, AND THE 2ND CYCLE DATA (WHICH MAY BE MISSING).
 123           FORMAT(A8,F9.1,F15.1,F13.1,F12.1)
            ENDIF
C
            DO 125 J=1,NCAT
C
            IF(DIFF.LE.TABLE(J))THEN
               NCTWH(J)=NCTWH(J)+1
               GO TO 128
            ENDIF
C
 125        CONTINUE
C
            WRITE(KFILDO,126)
 126        FORMAT(/' ****THIS IS A FATAL ERROR.  STOP IN FITWTH',
     1           ' AT 126.')
            STOP 126
C               
 128        SUMWH=SUMWH+ABS(DIFF)
            RUWH=RUWH+ABS(DIFF)
            NUMWH=NUMWH+1
            NRUWH=NRUWH+1
            SUMALL=SUMALL+ABS(DIFF)
            NUMALL=NUMALL+1
         ELSE
            WRITE(KFILDO,124)K,CCALL(K)
 124        FORMAT(/,' ****ERROR IN FITWTH AT 124.  INTERPOLATION WAS',
     1               ' NOT DONE FOR STATION NO.',I6,2X,A8,
     2               '.  PROCEEDING.')
            ISTOP(1)=ISTOP(1)+1
         ENDIF
C
CD        WRITE(KFILDO,130)K,XP(K),YP(K),BB,WHOLD(K),NUMWH,SUMWH
CD130     FORMAT(' AT 130 IN FITWTH--K,XP(K),YP(K),BB,WHOLD(K),',
CD    1          'NUMWH,SUMWH',I6,2F8.2,2F8.2,I6,F10.2)
      ENDIF
C
 150  CONTINUE
C
      IF(NUMWH.NE.NWITH)THEN
         WRITE(KFILDO,155)NUMWH,NWITH
 155     FORMAT(/' ****NUMBER OF VERIFICATION STATIONS =',I6,
     1           ' NOT EQUAL TO NUMBER OF WITHHELD STATIONS =',I6,
     2           '.  PROCEEDING.')
         IER=777
      ENDIF
C
C        COMPUTE AVERAGE FOR NON-WITHHELD STATIONS.
C
      IF(NUMNWH.NE.0)THEN
         AVGNWH=SUMNWH/NUMNWH
      ELSE
         AVGNWH=9999.
      ENDIF
C
      IF(NRUNWH.NE.0)THEN
         ARUNWH=RUNWH/NRUNWH
      ENDIF
C
C        COMPUTE AVERAGE FOR WITHHELD STATIONS.
C
      IF(NUMWH.NE.0)THEN
         AVGWH=SUMWH/NUMWH
      ELSE
         AVGWH=9999.
      ENDIF
C
      IF(NRUWH.NE.0)THEN
         ARUWH=RUWH/NRUWH
      ENDIF
C
C        COMPUTE AVERAGE FOR ALL STATIONS.
C
      IF(NUMALL.NE.0)THEN
         AVGALL=SUMALL/NUMALL
      ELSE
         AVGALL=9999.
      ENDIF
C
C        COMPUTE RELATIVE FREQUENCIES IN CATEGORIES.
C
      DO 165 J=1,NCAT
      FNCT(J)=FLOAT(NCT(J))/NRUNWH
      FNCTWH(J)=FLOAT(NCTWH(J))/NRUWH
 165  CONTINUE
C
      WRITE(IP24,169)
 169  FORMAT(/,' ***********************************************')
      WRITE(IP24,170)
 170  FORMAT(/,/,' ITERATION   NUMBER WITHHELD  MAE WITHHELD     ',
     1           ' NUMBER NONWITHHELD  MAE NONWITHHELD     ',
     2           ' TOTAL NUMBER  TOTAL MAE',/)
      IFIRST=IFIRST+1
C
      WRITE(IP24,180)IFIRST,NUMWH,AVGWH,NUMNWH,AVGNWH,NUMALL,AVGALL
 180  FORMAT(I7,I14,F18.4,I20,F20.4,I18,F14.4)
      WRITE(IP24,181)NRUWH,ARUWH,NRUNWH,ARUNWH
 181  FORMAT('RUNNING',I14,F18.4,I20,F20.4)
      WRITE(IP24,190)(TABLE(J),J=1,NCAT)
 190  FORMAT(/,' FREQUENCIES OF DIFFERENCES BETWEEN DATA AND ANALYSIS',/
     1      ,/,' LE ',14F9.0)
      WRITE(IP24,191)(NCTWH(J),J=1,NCAT)
 191  FORMAT(/,' WH ',14I9)
      WRITE(IP24,192)(FNCTWH(J),J=1,NCAT)
 192  FORMAT(  ' RF ',14F9.3)
      WRITE(IP24,193)(NCT(J),J=1,NCAT)
 193  FORMAT(/,' NWH',14I9)
      WRITE(IP24,192)(FNCT(J),J=1,NCAT) 
      RETURN
      END
