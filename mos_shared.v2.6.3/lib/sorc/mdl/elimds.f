      SUBROUTINE ELIMDS(KFILDO,CCALL,CCALLS,NAME,NELEV,IWBAN,
     1                  STALAT,STALON,ITIMEZ,IFOUND,K,ND1,LSTA)
C 
C        JANUARY   2001   GLAHN   TDL   MOS-2000 
C
C        PURPOSE 
C            TO ELIMINATE A DUPLICATE STATION IN CCALL( , ) AND THE 
C            INFORMATION ASSOCIATED WITH IT.    
C
C        DATA SET USE 
C            KFILDO   - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT) 
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,LSTA). 
C                       (CHARACTER*8)  (INPUT/OUTPUT)
C             NAME(K) = NAMES OF STATIONS (K=1,LSTA)  (CHARACTER*20)
C                       (INPUT/OUTPUT)
C            NELEV(K) = ELEVATION OF STATIONS (K=1,LSTA).
C                       (INPUT/OUTPUT)
C            IWBAN(K) = WBAN NUMBERS OF STATIONS (K=1,LSTA). 
C                       (INPUT/OUTPUT) 
C           STALAT(K) = LATITUDE OF STATIONS (K=1,LSTA). 
C                       (INPUT/OUTPUT)
C           STALON(K) = LONGITUDE OF STATIONS (K=1,LSTA).
C                       (INPUT/OUTPUT)
C           ITIMEZ(K) = TIME ZONE INDICATOR.  THE NUMBER OF HOURS
C                       THE STATION IS DIFFERENT FROM UTC (K=1,LSTA).
C                       (INPUT/OUTPUT)
C           IFOUND(K) = USED TO KEEP TRACK OF THE STATIONS FOUND IN THE
C                       DIRECTORY (K=1,LSTA).  (INTERNAL)
C                 ND1 = SIZE OF ARRAYS CCALL( , ), NELEV( ), IWBAN( ), 
c                       STALAT( ), STALON( ) AND IFOUND( ).
C                       THIS IS THE MAXIMUM NUMBER OF STATIONS IN THE
C                       LIST TO BE RETURNED.  (INPUT) 
C                LSTA = NUMBER OF STATONS IN LIST AFTER ELIMINATION OF
C                       THE KTH STATION.  (INPUT/OUTPUT)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            NONE. 
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLS(ND1)
      CHARACTER*20 NAME(ND1)
C
      DIMENSION NELEV(ND1),IWBAN(ND1),ITIMEZ(ND1),
     1          STALAT(ND1),STALON(ND1),IFOUND(ND1)
C
D     CALL TIMPR(KFILDO,KFILDO,'START ELIMDS        ')
C
C        INITIALIZE ARRAYS.
C
      DO 120 L=K,LSTA-1
         CCALL(L,1)=CCALL(L+1,1)
         CCALL(L,2)=CCALL(L+1,2)
         CCALL(L,3)=CCALL(L+1,3)
         CCALL(L,4)=CCALL(L+1,4)
         CCALL(L,5)=CCALL(L+1,5)
         CCALL(L,6)=CCALL(L+1,6)
         CCALLS(L)=CCALLS(L+1)
         NAME(L)=NAME(L+1)
         NELEV(L)=NELEV(L+1)
         IWBAN(L)=IWBAN(L+1)
         ITIMEZ(L)=ITIMEZ(L+1)
         STALAT(L)=STALAT(L+1)
         STALON(L)=STALON(L+1)
         IFOUND(L)=IFOUND(L+1)
 120  CONTINUE
C         
      LSTA=LSTA-1
      RETURN
      END
