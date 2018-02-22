      SUBROUTINE SORTBG(KFILDO,DTASRT,INDEX,NVRBL)                                     
C                                                                       
C        MARCH 2001   GLAHN   TDL   MOS-2000
C        ADAPTED FROM SORTI4 DECEMBER 1998, WHICH WAS                          
C        ADAPTED FROM GLAHN SEPTEMBER 1976 SORT1, WHICH WAS
C        ADAPTED FROM NESS ISORT (M. WHITNEY)   (SHELL SORT?)     
C                                                                       
C        PURPOSE                                                          
C            TO SORT NVRBL VALUES IN DTASRT( ) FROM LOW TO HIGH, 
C            CARRYING THE INDEX INTO THE ORIGINAL LIST IN INDEX( ).
C            USED IN SUBROUTINE LIST IN LAMPLIB IN LAMP-2000.
C
C        DATA SET USE
C            KFILDO   - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       DIAGNOSTICS.  (INPUT)
C           DTASRT(K) = INPUT VALUES TO SORT, LOW TO HIGH 
C                       (L=1,NVRBL).  (INPUT-OUTPUT).
C            INDEX(K) = INDEX VALUES SUCH THAT ON OUTPUT INDEX(K) HOLDS
C                       THE LOCATION WHERE IN THE ORIGINAL LIST THE
C                       VARIABLE NOW IN DTASRT( ) CAME FROM (K=1,NVRBL).
C                       (INPUT/OUTPUT)
C               NVRBL = DIMENSIONS OF DTASRT( ) AND INDEX( ).  (INPUT)
C                IEND = THE LOCATION IN DTASRT( ) TO STOP ORDERING.
C                       (INTERNAL)                        
C        1         2         3         4         5         6         7 X
C                                                                       
      DIMENSION DTASRT(NVRBL),INDEX(NVRBL)                            
C
C        SORT.
C
      IEND=NVRBL
      M=IEND   
C
 105  M=M/2       
      IF(M.EQ.0)GO TO 150   
      KK=IEND-M 
      J=1                                                               
C
 115  N=J                                                               
      L=N+M                                                             
 120  IF(DTASRT(L).GE.DTASRT(N))GO TO 140
C
C        EXCHANGE VALUES IN DTASRT( ).
C
      FEEP=DTASRT(N)
      DTASRT(N)=DTASRT(L)
      DTASRT(L)=FEEP
C        
C        EXCHANGE VALUES IN INDEX( ).
C                                    
      KEEP=INDEX(N)       
      INDEX(N)=INDEX(L)
      INDEX(L)=KEEP
C
      N=N-M      
      L=N+M              
      IF(N.GT.0)GO TO 120
C                                                  
 140  J=J+1        
      IF(J-KK)115,115,105
C      
 150  CONTINUE             
C
      RETURN
      END
