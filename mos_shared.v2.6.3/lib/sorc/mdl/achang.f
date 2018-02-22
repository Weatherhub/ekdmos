      SUBROUTINE ACHANG(VAR,NCHAR,INDEX,IWORK,NVRBL)                                     
C                                                                       
C       DECEMBER 2000   GLAHN   TDL   MOS/LAMP-2000                           
C                                                                       
C        PURPOSE                                                          
C            TO ARRANGE NVRBL CHARACTER VALUES IN VAR( ) ACORDING TO
C            THE VALUES IN INDEX( ).  INDEX( ) HAS PROBABLY BEEN
C            PROVIDED BY A SORTING ROUTINE, AND THE VAR( ) VARIABLE
C            IS TO BE ARRANGED THE SAME WAY THE SORTED VARIABLE IS.
C            IT IS ASSUMED VAR( ) IS CHARACTER*NCHAR.   ACHANG WAS
C            ADAPTED FROM XCHANG.     
C
C        DATA SET USE 
C            NONE
C 
C        VARIABLES                                                        
C              VAR(J) = INPUT VALUES TO ARRANGE ACCORDING TO THE VALUES
C                       IN INDEX( ) (J=1,NVRBL).  (INPUT/OUTPUT)
C               NCHAR = THE NUMBER OF CHARACTERS IN VAR( ).  LIMIT
C                       IS 60 CHARACTERS BECAUSE OF SIZE OF DUMMY.  (INPUT)
C            INDEX(J) = INDEX VALUES CREATED BY A SORT ROUTINE.  INDEX(J)
C                       INDICATES WHERE THE VARIABLE TO BE PUT INTO
C                       VAR(J) IS TO COME FROM (J=1,NVRBL).  (INPUT)
C            IWORK(J) = WORK ARRAY (J=1,NVRBL).  (INTERNAL)
C               NVRBL = NUMBER OF ITEMS IN VAR( ) AND INDEX( ).  (INPUT)                         
C
      CHARACTER*(*) VAR(NVRBL)
      CHARACTER*60 DUMMY
                                                                      
      DIMENSION INDEX(NVRBL),IWORK(NVRBL) 
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
C
      DO 119 M=1,NVRBL
      IF(IWORK(M).NE.J)GO TO 119
      IF(J.EQ.M)GO TO 120
C
      DUMMY(1:NCHAR)=VAR(J)(1:NCHAR)                                                      
      VAR(J)(1:NCHAR)=VAR(M)(1:NCHAR)
      VAR(M)(1:NCHAR)=DUMMY(1:NCHAR)
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
