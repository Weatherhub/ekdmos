      SUBROUTINE BPACK(NSTA,KDATA,CSTA,NBITS,MBUF,NWMO,INOFST,NBUL,
     *          MAXSTA,NUMPRJ,MAXMES,IOPT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BPACK       PACKS DATA FIELDS FOR BUFR SECTION 4       
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 92-07-14             
C                                                                       
C ABSTRACT: COMPRESSES A DATA FIELD AND PACKS IT INTO A BUFR DATA       
C           FIELD FOR SECTION 4.  THE COMPRESSION IS DONE BY SUBTRACTING
C           THE MINIMUM VALUE OF THE FIELD FROM EACH VALUE, AND THEN    
C           STORING THE RESULTING VALUES IN AS SMALL A NUMBER OF BITS   
C           AS POSSIBLE.  THE RESULTING BUFR FIELD CONTAINS THE         
C           MINIMUM VALUE, THE NUMBER OF BITS REQUIRED TO STORE EACH    
C           DATA ELEMENT, AND THE COMPRESSED DATA FIELD.  IF ALL        
C           DATA ELEMENTS IN A FIELD ARE EQUAL, THEN THAT VALUE IS THE  
C           MINIMUM VALUE AND THE NUMBER OF BITS REQUIRED (NBINC) IS    
C           SET TO ZERO.                                                
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   92-07-14  GILBERT                                                   
C   01-02     SHIREY  CHANGED DIMENSION OF ITEMP FROM 1000 TO 16000
C                     ITEMP MUST BE BIG ENOUGH TO HANDLE ALL DESCRIPTORS
C                     (STATIONS*PROJECTIONS).  ALSO CHANGED THE WAY 
C                     STATION CALL LETTERS ARE "PACKED".  SINCE WE ARE
C                     NOW USING 32 BIT ROUTINES THERE ARE 2 CALLS TO SBYTE
C                     EACH 32 BITS LONG, INSTEAD OF ONE CALL TO SBYTE
C                     64 BITS LONG.
C   05-07-22  MALONEY INCREASED ITEMP TO 45000 TO HANDLE NEW MEX BUFR
C   12-09-13  ENGLE   CHANGED ALL CALLS TO SBYTE TO SBYTEC; CALL
C                     TO SBYTES HAS BEEN CHANGED TO SBYTEC AND MOVED
C                     TO INSIDE DO LOOP 400.
C   12-10-25  ENGLE   ADDED VARIABLE CTEMP; INSIDE DO LOOP 500, ADDED
C                     CALL TO CKSYSEND, THEN SWAP STATION CALL LETTERS
C                     IN 4-BYTE INCREMENTS IF THE SYSTEM IS LITTLE-ENDIAN;
C                     MODIFIED 2 CALLS TO SBYTEC TO USE CTEMP INSTEAD OF
C                     CSTA.
C   14-10-28  ENGLE/  INCREASED ITEMP TO 50000 TO HANDLE NEW MEX BUFR 
C           SCALLION  STATIONS.
C                                                                       
C USAGE:                                                                
C                                                                       
C        PURPOSE                                                        
C           COMPRESSES A DATA FIELD AND PACKS IT INTO A BUFR DATA       
C           FIELD FOR SECTION 4.  THE COMPRESSION IS DONE BY SUBTRACTING
C           THE MINIMUM VALUE OF THE FIELD FROM EACH VALUE, AND THEN    
C           STORING THE RESULTING VALUES IN AS SMALL A NUMBER OF BITS   
C           AS POSSIBLE.  THE RESULTING BUFR FIELD CONTAINS THE         
C           MINIMUM VALUE, THE NUMBER OF BITS REQUIRED TO STORE EACH    
C           DATA ELEMENT, AND THE COMPRESSED DATA FIELD.  IF ALL        
C           DATA ELEMENTS IN A FIELD ARE EQUAL, THEN THAT VALUE IS THE  
C           MINIMUM VALUE AND THE NUMBER OF BITS REQUIRED (NBINC) IS    
C           SET TO ZERO.                                                
C                                                                       
C        DATA SET USE                                                   
C            FT06 - PRINT FILE  (OUTPUT).                               
C                                                                       
C        VARIABLES                                                      
C           KDATA(,) = HOLDS DATA FOR A FORECAST ELEMENT FOR EACH       
C                      STATION AND EACH PROJECTION. 1ST DIMENSION IS    
C                      # OF STATIONS, AND 2ND DIMENSION IS NUMBER OF    
C                      PROJECTIONS.  CONTAINS MISSING "9999"'S FOR      
C                      PROJECTION FOR WHICH FORECASTS ARE NOT VALID.    
C             MAXSTA = MAXIMUM # OF STATIONS (USED FOR DIMENSIONING)    
C             NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE.                
C            MBUF(,) = HOLDS FORECASTS AFTER THEY ARE PACKED INTO BUFR  
C                      FORMAT.  1ST DIMENSION IS # OF BYTES IN BUFR     
C                      MESSAGE, AND 2ND DIMENSION IS # OF BULLETINS     
C                      (WMO HEADERS).  (CHAR*1)
C             MAXMES = MAXIMUM NUMBER OF BYTES IN BUFR BULLETIN.        
C               NBUL = NUMBER OF BUFR BULLETINS (WMO HEADERS)           
C            NWMO(I) = NUMBER OF STATIONS IN BULLETIN I.                
C          INOFST(I) = OFFSET POSITION IN MBUF(,I)                      
C            ITEMP() = WORK ARRAY                                       
C               IPOS = HOLDS POSITION OF THE DATA FOR THE NEXT          
C                      STATION IN KDATA(,)                              
C               IMIN = MINIMUM DATA VALUE IN FIELD                      
C               IMAX = MAXIMUM DATA VALUE IN FIELD                      
C              NBINC = FEWEST NUMBER OF BITS REQUIRED TO HOLD EACH      
C                      ELEMENT IN THE COMPRESSED FIELD.                 
C              IMISS = 1, IF MISSING VALUES ARE PRESENT                 
C                                                                       
C REMARKS: CHARACTER DATA CANNOT BE COMPRESSED, SO A BRANCH IS IN THIS  
C          ROUTINE TO HANDLE STATION CALL LETTERS.  NO OTHER CHARACTER  
C          DATA IS EXPECTED TO BE IN TDL'S BUFR MESSAGES.               
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  NAS                                                       
C                                                                       
C$$$                                                                    
      CHARACTER*1 MBUF(MAXMES,NBUL)
      CHARACTER*8 CSTA(NSTA),CTEMP
      INTEGER KDATA(MAXSTA,NUMPRJ),NWMO(NBUL),
     *        INOFST(NBUL),ITEMP(50000)
      IPOS=0
C        DO FOR EACH BULLETIN                                           
      DO 800 N=1,NBUL
        IMISS=0
C        DETERMINE NUMBER OF WORDS NEEDED FOR WORK ARRAY AND CHECK VALUE
        ITOT=NWMO(N)*NUMPRJ
        IF (ITOT.GT.50000) THEN
          WRITE(6,10)ITOT
 10       FORMAT(' NUMBER OF WORDS =',I6,' EXCEEDS DIMENSION OF ',
     *         'TEMPORARY ARRAY IN BPACK.')
          CALL EXIT(10)
        ENDIF
C        
C        IF DATA VALUES TO PACK ARE NOT ASCII CHARACTERS (E.G.
C        STATION CALL LETTERS) DO THE REGULAR COMPRESSION
C        PACKING.  CALL LETTERS ARE HANDLED DIFFERENTLY IN THE
C        ELSE BLOCK.
C
         IF (IOPT.NE.2) THEN
          IMIN=2**30
          IMAX=0
          M=0
C        FIND MAXIMUM AND MIMIMUM VALUE OF ALL FORECASTS IN A BULLETIN. 
C        THESE VALUES WILL BE USED FOR ADDITIONAL DATA COMPRESSION.     
          DO 200 I=1,NWMO(N)
            DO 100 L=1,NUMPRJ
              M=M+1
              ITEMP(M)=KDATA((IPOS+I),L)
              IF (ITEMP(M).NE.9999) THEN
                IF (ITEMP(M).LT.IMIN)IMIN=ITEMP(M)
                IF (ITEMP(M).GT.IMAX)IMAX=ITEMP(M)
              ELSE
                IMISS=1
              ENDIF
 100        CONTINUE
 200      CONTINUE
C        INCREMENT COUNTER FOR STATION LIST 
          IPOS=IPOS+NWMO(N)
C        M SHOULD ALWAYS EQUAL ITOT AT THIS POINT.               
          IF(M.NE.ITOT)WRITE(6,*)'SOMETHING UNEXPECTED IN BPACK'
C        IF IMAX = IMIN, ALL DATA ARE THE SAME AND SOME ADDITIONAL      
C        COMPRESSION CAN BE USED.          
          IF (IMAX.EQ.IMIN) THEN
            IF (IMISS.EQ.0) THEN
              NBINC=0
            ELSE
              NBINC=1
            ENDIF
          ELSE
C        FIND THE DIFFERENCE BETWEEN THE LARGEST AND SMALLEST VALUES    
C        AND DETERMINE THE MINIMUM NUMBER OF BITS NEEDED TO HOLD        
C        THAT DIFFERENCE.                          
            NBINC=1
            IDIFF=IMAX-IMIN
 300        IF (IDIFF.GE.(2**NBINC)-1) THEN
              NBINC=NBINC+1
              GOTO 300
            ENDIF
          ENDIF
C        PUT MINIMUM VALUE IN THE DATA FIELD FOR THE BUFR MESSAGE       
          CALL SBYTEC(MBUF(1,N),IMIN,INOFST(N),NBITS)
          INOFST(N)=INOFST(N)+NBITS
C        ADD NUMBER OF BITS NEEDED TO HOLD DATA VALUES TO MESSAGE       
          CALL SBYTEC(MBUF(1,N),NBINC,INOFST(N),6)
          INOFST(N)=INOFST(N)+6
C        SUBTRACT THE MINIMUM VALUE FROM EACH DATA VALUE AND PACK IT    
C        INTO THE BUFR FIELD.  IF THE DATA VALUE IS MISSING, SET ALL    
C        THE BITS EQUAL TO 1.                  
          IF (NBINC.NE.0) THEN
            DO 400 I=1,ITOT
              IF (ITEMP(I).NE.9999) THEN
                ITEMP(I)=ITEMP(I)-IMIN
              ELSE
                ITEMP(I)=(2**NBINC)-1
              ENDIF
CINTEL
              CALL SBYTEC(MBUF(1,N),ITEMP(I),INOFST(N),NBINC)
              INOFST(N)=INOFST(N)+NBINC
 400        CONTINUE
C            CALL SBYTES(MBUF(1,N),ITEMP,INOFST(N),NBINC,0,ITOT)
C            INOFST(N)=INOFST(N)+(NBINC*ITOT)
CINTEL
          ENDIF
        ELSE
C        THE FOLLOWING BLOCK IS USED FOR STATION CALL LETTERS. 
C        THEY ARE TREATED A LITTLE DIFFERENTLY.   
          INOFST(N)=INOFST(N)+NBITS
          NCH=NBITS/8
          CALL SBYTEC(MBUF(1,N),NCH,INOFST(N),6)
          INOFST(N)=INOFST(N)+6
          DO 550 I=1,NWMO(N)
            DO 500 L=1,NUMPRJ
CINTEL
              CALL CKSYSEND(KFILDO,'NOPRINT',ISYSEND,IER)
              IF(ISYSEND.EQ.-1)THEN
                 CTEMP=CSTA(IPOS+I)
                 CALL BSWAP(CTEMP(1:4)) 
                 CALL BSWAP(CTEMP(5:8)) 
              ENDIF
              CALL SBYTEC(MBUF(1,N),CTEMP(1:4),INOFST(N),32)
              CALL SBYTEC(MBUF(1,N),CTEMP(5:8),(INOFST(N)+32),32)
C              CALL SBYTEC(MBUF(1,N),CSTA(IPOS+I)(1:4),INOFST(N),32)
C              CALL SBYTEC(MBUF(1,N),CSTA(IPOS+I)(5:8),(INOFST(N)+32),32)
CINTEL
              INOFST(N)=INOFST(N)+NBITS
 500        CONTINUE
 550      CONTINUE
C        INCREMENT COUNTER FOR STATION LIST 
         IPOS=IPOS+NWMO(N)
        ENDIF
 800  CONTINUE
      RETURN
      END
