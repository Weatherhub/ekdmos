      SUBROUTINE SORTEM(KFILDO,JD,INDEX,THRESH,NVRBL)
C                                                                       
C        DECEMBER 1998   GLAHN   TDL   MOS-2000 
C                                                                       
C        PURPOSE                                                          
C            TO SORT NVRBL 4-WORD INTEGER VARIABLES AND ONE REAL
C            VARIABLE AS A GROUP FROM LOW TO HIGH.  THIS 
C            ROUTINE IS USED IN RDPRED IN U201 AND RDVRBL IN U600.
C            THE INDEX VALUES, INDICATING FOR EACH LOCATION WHERE
C            THE ORIGINAL VARIABLE WENT, IS PROVIDED BACK TO THE
C            CALLING PROGRAM FOR POSSIBLE REARRANGEMENT OF OTHER
C            VARIABLES TO CORRESPOND TO JD( , ).     
C
C        DATA SET USE 
C            KFILDO   - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
c                       (OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
c                       (INPUT) 
C             JD(J,L) = INPUT VALUES AS 4-WORD INTEGERS TO SORT, LOW 
C                       TO HIGH (J=1,4) (L=1,NVRBL).  (INPUT-OUTPUT).
C            INDEX(L) = INDEX VALUES SUCH THAT ON OUTPUT INDEX(L) HOLDS
C                       THE LOCATION WHERE IN THE ORIGINAL LIST THE
C                       VARIABLE NOW IN JD( , ) CAME FROM  (L=1,NVRBL).
C                       (OUTPUT)
C           THRESH(J) = THE BINARY THRESHOLD CORRESPONDING TO JD( )
C                       (J=1,NVRBL).  (INPUT/OUTPUT)
C               NVRBL = NUMBER OF ITEMS IN JD( , ) AND INDEX( ).
C                       (INPUT)                         
C                                                                       
C        NONSYSTEM SUBROUINES USED 
C           NONE
C
      DIMENSION JD(4,NVRBL),INDEX(NVRBL),THRESH(NVRBL)
C
C        INITIALIZE INDEX( ).
C
      DO 102 K=1,NVRBL
      INDEX(K)=K
 102  CONTINUE
C
D     WRITE(KFILDO,105)((JD(J,K),J=1,4),THRESH(K),INDEX(K),
D    1                   K=1,NVRBL)
D105  FORMAT(' IN SORTEM AT 105'/('      '4I11,F11.4,I4))
C
      IF(NVRBL.LE.1)GO TO 160
C        NOTE THAT INDEX( ) IS ALWAYS INITIALIZED.
C
 110  MOVE=0
C
      DO 150 J=1,NVRBL-1
C
      IF(JD(1,J).LT.JD(1,J+1))GO TO 150
      IF(JD(1,J).GT.JD(1,J+1))GO TO 140
C        DROP THROUGH HERE WHEN FIRST WORDS ARE EQUAL.
C
      IF(JD(2,J).LT.JD(2,J+1))GO TO 150
      IF(JD(2,J).GT.JD(2,J+1))GO TO 140
C        DROP THROUGH HERE WHEN SECOND WORDS ARE EQUAL.
C
      IF(JD(3,J).LT.JD(3,J+1))GO TO 150
      IF(JD(3,J).GT.JD(3,J+1))GO TO 140
C        DROP THROUGH HERE WHEN THIRD WORDS ARE EQUAL.
C
      IF(JD(4,J).LT.JD(4,J+1))GO TO 150
      IF(JD(4,J).GT.JD(4,J+1))GO TO 140
C        DROP THROUGH HERE WHEN FOURTH WORDS ARE EQUAL.
C
      IF(THRESH(J).LE.THRESH(J+1))GO TO 150   
C    
C        EXCHANGE JTH AND J+1TH ENTRIES IN ALL 4 COLUMNS 
C        OF JD(4, ) AND OF THRESH( ).
C
 140  DO 145 K=1,4
      KEEP=JD(K,J+1)
      JD(K,J+1)=JD(K,J)
      JD(K,J)=KEEP
 145  CONTINUE
C
C        EXCHANGE THRESHOLD.
C
      TEMP=THRESH(J+1)
      THRESH(J+1)=THRESH(J)
      THRESH(J)=TEMP
C
C        EXCHANGE VALUES IN INDEX( ).
C  
      KEEP=INDEX(J+1)
      INDEX(J+1)=INDEX(J)
      INDEX(J)=KEEP
C
      MOVE=MOVE+1
C      
 150  CONTINUE
C
D     WRITE(KFILDO,155)((JD(J,K),J=1,4),THRESH(K),INDEX(K),
D    1                  K=1,NVRBL)
D155  FORMAT(' IN SORTEM AT 155'/('      '4I11,F11.4,I4))
C
C
      IF(MOVE.NE.0)GO TO 110
C
C        IF NO MOVES WERE MADE, SORT IS DONE.
C
 160  RETURN
      END
 
