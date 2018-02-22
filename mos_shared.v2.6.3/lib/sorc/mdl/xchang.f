      SUBROUTINE XCHANG(IVAR,NUMROW,INDEX,IWORK,NVRBL)                                     
C                                                                       
C        JUNE 1995   GLAHN   TDL   MOS-2000                           
C                                                                       
C        PURPOSE                                                          
C            TO ARRANGE NVRBL VALUES IN IVAR( , ) ACORDING TO THE
C            VALUES IN INDEX( ).  INDEX( ) HAS PROBABLY BEEN
C            PROVIDED BY A SORTING ROUTINE, AND THE IVAR( , ) VARIABLE
C            IS TO BE ARRANGED THE SAME WAY THE SORTED VARIABLE IS.
C            IT IS ASSUMED IVAR( , ) IS 4-BYTE WORDS, AND CAN BE
C            EITHER REAL OR INTEGER.       
C
C        DATA SET USE 
C            NONE
C 
C        VARIABLES                                                        
C           IVAR(I,J) = INPUT VALUES TO ARRANGE ACCORDING TO THE VALUES
C                       IN INDEX( ) (I=1,NUMROW) (J=1,NVRBL).  (INPUT/OUTPUT)
C              NUMROW = THE FIRST DIMENSION OF IVAR( , ).  ALL NUMROW
C                       ROWS WILL BE INTERCHANGED.  (INPUT)
C            INDEX(J) = INDEX VALUES CREATED BY A SORT ROUTINE.  INDEX(J)
C                       INDICATES WHERE THE VARIABLE TO BE PUT INTO
C                       IVAR( ,J) IS TO COME FROM (J=1,NVRBL).  (INPUT)
C            IWORK(J) = WORK ARRAY (J=1,NVRBL).  (INTERNAL)
C               NVRBL = NUMBER OF ITEMS IN IVAR( , ) AND INDEX( ).  (INPUT)                         
C                                                                       
      DIMENSION IVAR(NUMROW,NVRBL),INDEX(NVRBL),IWORK(NVRBL) 
C
C        INITIALIZE IWORK( ) TO INDICATE WHERE THE VARIABLE IS TO GO.
C
      DO 101 J=1,NVRBL
C
      DO 100 I=1,NVRBL
      IF(INDEX(I).NE.J)GO TO 100
      IWORK(J)=I
      GO TO 101
 100  CONTINUE
C        THIS LOOP SHOULD NOT EVER END.
C
 101  CONTINUE                             
C
C        EXCHANGE VALUES IN IVAR( ).  MUST KEEP IWORK( ) CURRENT.
C 
      DO 120 J=1,NVRBL
D     WRITE(12,117)J,((IVAR(K,N),K=1,NUMROW),IWORK(N),
D    1                 N=1,NVRBL)
D117  FORMAT(/' XCHANG TEST'I2,/(2XI9.9,1XI9.9,1XI9.9,1XI10.3,I6))
C
      DO 119 M=1,NVRBL
      IF(IWORK(M).NE.J)GO TO 119
      IF(J.EQ.M)GO TO 120
C
      DO 118 I=1,NUMROW                                  
      KEEP=IVAR(I,J)                                                      
      IVAR(I,J)=IVAR(I,M)
      IVAR(I,M)=KEEP
 118  CONTINUE
C
      KEEP=IWORK(J)
      IWORK(J)=IWORK(M)
      IWORK(M)=KEEP
      GO TO 120
      
 119  CONTINUE
C
 120  CONTINUE
C
      RETURN                                                            
      END                                                               
