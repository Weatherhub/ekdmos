      SUBROUTINE LMSTR3(KFILDO,NRRDAT,LSTORE,LITEMS,
     1                  MSTORE,MITEMS,ND9,IER)
C
C        DECEMBER  2002   GLAHN   NDFD  MOS-2000
C
C        PURPOSE
C           TO USE MSTORE( , ) AND PREPARE LSTORE( , ) FOR GCPAC
C           FOR U855.  FOR EACH VARIABLE IN MSTORE( , ), THE DATE
C           IN LSTORE(12, ) MUST BE THE DATE OF THE DATA IN 
C           LSTORE(8, ) PLUS THE MAXIMUM HOURS TO KEEP IN 
C           MSTORE(7, ), PROVIDED IT IS LE NRRDAT, THE NEXT
C           DATE/TIME IN THE LIST.  OTHERWISE, LSTORE(1, ) = 0.
C           (THIS MIGHT ALSO BE NEEDED FOR U201.)
C   
C        DATA SET USE
C            KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              NRRDAT = THE NEXT DATE/TIME IN THE LIST.  NORMALLY, THIS
C                       WOULD BE THE CURRENT DATE/TIME + INCCYL.
C                       (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS).  (INPUT-OUTPUT)
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK, 
C                              THIS IS MINUS THE RECORD NUMBER WHERE 
C                              THE DATA START.
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDL GRIB, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --NUMBER OF THE SLAB IN DIR( , ,L) AND
C                              IN NGRIDC( ,L) DEFINING THE 
C                              CHARACTERISTICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE FIRST PREDICTOR IN THE
C                              SORTED LIST IN ID( ,N) (N=1,NPRED) FOR
C                              WHICH THIS VARIABLE IS NEEDED, WHEN IT 
C                              DOES NOT NEED TO BE STORED AFTER DAY 1.
C                              WHEN THE VARIABLE MUST BE STORED (TO BE
C                              ACCESSED THROUGH OPTION) FOR ALL DAYS,
C                              LSTORE(11,N) IS 7777 + THE NUMBER OF THE 
C                              FIRST PREDICTOR IN THE SORTED LIST FOR 
C                              WHICH THIS VARIABLE IS NEEDED.
C                       L=12 --USED INITIALLY IN ESTABLISHING
C                              MSTORE( , ).  LATER USED AS A WAY OF
C                              DETERMINING WHETHER TO KEEP THIS
C                              VARIABLE.
C              LITEMS = THE NUMBER OF ITEMS IN LSTORE( , ).
C         MSTORE(L,J) = THE ARRAY HOLDING THE VARIABLES NEEDED AS
C                       INPUT, AFTER DAY 1, AND ASSOCIATED INFORMATION 
C                       (L=1,7) (J=1,MITEMS).  (OUTPUT)
C                       L=1,4 --THE 4 ID'S FOR THE DATA.
C                       L=5   --INDICATES WHETHER OR NOT TO STORE THE
C                               VARIABLE AND THE FIRST PREDICTOR TO USE 
C                               IT FOR.
C                       L=6   --INITIALLY, THE EARLIEST DATE/TIME FOR
C                               WHICH THIS VARIABLE IS NEEDED FOR THE
C                               DATE BEING PROCESSED.  UPON EXIT, THE 
C                               VALUE WILL HAVE BEEN SET TO THE CYCLE
C                               TIME.  THERE WILL BE AN ENTRY IN
C                               MSTORE( , ) FOR EACH CYCLE FOR WHICH 
C                               THE VARIABLE IS NEEDED.  MSTORE( , ) 
C                               IS NOT CHANGED AFTER EXIT.
C                       L=7   --THE MAXIMUM TIME OFFSET RR (SEE 
C                               IDPARS(9, ) CORRESPONDING TO
C                               MSTORE(6, )
C                       NOTE THAT MSTORE IN U201 AND LMSTR3 IS NOT
C                       EXACTLY THAT IN U600 AND RDVECT.  U201 DOES
C                       NOT USE RDVECT.
C              MITEMS = THE NUMBER OF ITEMS IN MSTORE( , ). 
C                       (INPUT/OUTPUT)
C                 ND9 = MAXIMUM NUMBER OF FIELDS STORED IN LSTORE( , )
C                       AND MSTORE( , ).  SECOND DIMENSION OF
C                       LSTORE( , ) AND MSTORE( , ).  (INPUT)
C                 IER = STATUS RETURN.  (OUTPUT)
C                        0 = GOOD RETURN.
C        NONSYSTEM SUBROUTINES USED
C            NONE
C
      DIMENSION LSTORE(12,ND9),MSTORE(7,ND9)
C
      IER=0
C
D     WRITE(KFILDO,100)((LSTORE(I,J),I=1,12),J=1,LITEMS)
D100  FORMAT(/' LSTORE STARTING LMSTR3'/('  '3I10,I11,3I8,I12,3I8,I12))
C
C        DETERMINE WHETHER DATA IN LSTORE( , ) SHOULD BE SAVED
C        BECAUSE OF ENTRIES IN MSTORE( , ).  IF SO, ARRANGE TO SAVE.
C        IF MITEMS = 0, THE DO 1045 LOOP WILL NOT BE EXECUTED.
C
 104  DO 1045 M=1,MITEMS
C
      DO 1044 L=1,LITEMS
C
      IF(MSTORE(1,M).EQ.LSTORE(1,L).AND.
     1   MSTORE(2,M).EQ.LSTORE(2,L).AND.
     2   MSTORE(3,M).EQ.LSTORE(3,L).AND.
     3  (MSTORE(4,M).EQ.LSTORE(4,L).OR.
     4   MSTORE(4,M)+1.EQ.LSTORE(4,L)))THEN
C           THE VALUES IN MSTORE(4, ) HAVE "G" = 0, BUT THE VALUES IN
C           LSTORE(4, ) MAY HAVE "G" = 1.  ABOVE ALLOWS FOR BOTH.
         CALL UPDAT(LSTORE(8,L),MSTORE(7,M),LSTORE(12,L))
      ENDIF
C     
 1044 CONTINUE
C
 1045 CONTINUE
C
C        ARRANGE TO DISCARD ALL DATA NOT NEEDED.
C
      DO 215 J=1,LITEMS
      IF(LSTORE(12,J).LT.NRRDAT)LSTORE(1,J)=0
 215  CONTINUE
C
D     WRITE(KFILDO,220)((LSTORE(I,J),I=1,12),J=1,LITEMS)
D220  FORMAT(/' LSTORE ENDING LMSTR3'/('  '3I10,I11,3I8,I12,3I8,I12))
 230  RETURN
      END
