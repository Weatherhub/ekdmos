      SUBROUTINE OVERDATE2(KFILDO,KFILIN,LDATES,ND9,N)
C
C$$$   SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: OVERDATE
C   PRGMMR: RUDACK          ORG: W/OSD21          DATE: 2003-06-01
C
C ABSTRACT: TO READ ALL THE DATES ON A GRIB2 FILE AND PLACE
C           THEM IN CHRONOLOGICAL ORDER.
C
C PROGRAM HISTORY LOG:
C        JUNE       2003   RUDACK     MDL   MOS-2000 (ADAPTED FROM OVERDATE.F)
C        SEPTEMBER  2005   RUDACK           MODIFIED CODE TO MEET OPERATIONAL 
C                                           REQUIREMENTS.
C        NOVEMBER   2007   WIEDENFELD MDL   FINISHE ADDING OPERATIONAL REQUIREMENTS
C
C USAGE: CALLED BY RDGRIB2
C
C DATA SET USE:
C    INPUT FILE:
C       FORT.KFILIN - UNIT NUMBER FOR THE CURRENT GRIB2 INPUT DATA 
C                     FILE.  (INPUT) 
C    OUTPUT FIEL:
C       FORT.KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C VARIABLES:
C            KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  INITIALLY,
C                     THIS IS SET BY DATA STATEMENT.  LATER, IN 
C                     IPOPEN, IF IP(1) NE 0, KFILDO IS SET = IP(1).
C                     THIS ALLOWS CHANGING THE "DEFAULT" PRINT FILE ON 
C                     THE FLY.  OTHERWISE, ON SOME SYSTEMS, THE OUTPUT
C                     FILE MIGHT HAVE THE SAME NAME AND BE
C                     OVERWRITTEN.  WHEN THE OUTPUT FILE IS NOT THE
C                     ORIGINAL DEFAULT, THE NAME IS GENERATED AND CAN
C                     BE DIFFERENT FOR EACH RUN.  (OUTPUT)
C            KFILIN = UNIT NUMBER FOR THE CURRENT GRIB2 INPUT DATA
C                     FILE.  (INPUT)
C         LDATES(J) = CONTAINS ALL THE DATES ON THE GRIB2 FILE IN
C                     CHRONOLOGICAL ORDER (J=1,ND9).  (OUTPUT)
C               ND9 = MAXIMUM NUMBER OF RECORDS THAT ARE ANTICIPATED 
C                     TO BE ON A PARTICULAR GRIB2 FILE.  SET BY PARAMETER
C                     IN DRU130. (INPUT)
C                 N = THE TOTAL NUMBER OF DATES IN THE GRIB2 FILE.
C                     (OUTPUT) 
C
C SUBROUTINES CALLED:  SKGB, BAREAD, W3TAGE
C
C EXIT STATES:
C   COND =   0 - SUCCESSFUL RUN
C           50 - NUMBER OF BYTES READ BY RAREAD IS INCORRECT.
C           75 - NUMBER OF RECORDS READ IS INCORRECT.
C
C REMARKS:  NONE
C
C ATTRIBUTES:
C   LANGUAGE:  FORTRAN 90 (xlf90 compiler)
C   MACHINE:  IBM SP
C$$$
C
      PARAMETER(MSK1=32000,MSK2=4000,MGRIB=9999999)
C
      CHARACTER CGRIB(MGRIB)
C
      DIMENSION LDATES(ND9)
C
      N=0
      ISEEK=0
C
C        INITIALIZE 'LDATES( )' PRIROR TO USE.
C 
      DO 25 J=1,ND9
         LDATES(J)=0
 25   CONTINUE
C
      CALL SKGB(KFILIN,ISEEK,MSK1,LSKIP,LGRIB)
C
      DO WHILE(LGRIB.GT.0.AND.LGRIB.LE.MGRIB)
         CALL BAREAD(KFILIN,LSKIP,LGRIB,NGRIB,CGRIB)
         IF(NGRIB.NE.LGRIB) THEN
            WRITE(KFILDO,50) KFILIN 
 50         FORMAT(/,' ****THE EXPECTED NUMBER OF BYTES TO BE READ IN',
     1               ' BY BAREAD DID NOT EQUAL THE ACTUAL',/,4X,
     2               ' NUMBER OF BYTES READ IN BAREAD FOR THE FILE',
     3               ' WITH UNIT NUMBER',1X,I2,2X,'STOP 50 IN',
     4               ' OVERDATE.')
            CALL W3TAGE('OVERDATE2')
            STOP 50
         ENDIF
C
         N=N+1
C
C           CHECK THAT THE ARRAY SIZE FOR LDATES HAS NOT EXCEEDED 
C           EXCEEDED ND9.  IF IT HAS BEEN EXCEEDED, STOP THE PROGRAM.
C
         IF(N.GT.ND9) THEN
            WRITE(KFILDO,75) KFILIN
 75         FORMAT(/,' ****THE NUMBER OF DATES READ ON THE INPUT',
     1               ' FILE WITH UNIT NUMBER ',I2,' EXCEEDS',/,5X,'THE',
     2               ' NUMBER ALLOWED.  INCREASE THE PARAMETER SIZE OF',
     3               ' ND9 IN THE DRIVER.',/,5X,'STOP 75 IN OVERDATE.')
            CALL W3TAGE('OVERDATE2')
            STOP 75
         ENDIF   
C
C           PLACE THE PARSED DATE INTO VARIABLES FOR PROCESSING.
C         
         I4YEAR=(256*MOVA2I(CGRIB(16+13)))+MOVA2I(CGRIB(16+14))
         IMONTH=MOVA2I(CGRIB(16+15))
         IDAY=MOVA2I(CGRIB(16+16))
         IHR=MOVA2I(CGRIB(16+17))
C
C           CONSTRUCT THE 10 INTEGER DATE (E.G. 2003012112).          
C 
         JYEAR=I4YEAR*100+IMONTH  
         KYEAR=JYEAR*100+IDAY
         LYEAR=KYEAR*100+IHR
         LDATES(N)=LYEAR
C
C           FIND THE NEXT DATE ON THE GRIB2 FILE.       
C
         ISEEK=LSKIP+LGRIB
         CALL SKGB(KFILIN,ISEEK,MSK2,LSKIP,LGRIB)
      ENDDO
C
C        SORT THE DATES INTO CHRONOLOGICAL ORDER.
C
      DO 200 J=1,N-1
C
C           FIND THE CURRENT MINIMUM VALUE.
C  
         JLOW=J 
C         
         DO 100 K=(J+1),N
            IF(LDATES(K).LT.LDATES(JLOW)) JLOW=K
 100     CONTINUE
C
C           SWAP THE DATES. 
C
         ITEMP=LDATES(J)
         LDATES(J)=LDATES(JLOW)
         LDATES(JLOW)=ITEMP
C
 200  CONTINUE 
C
      RETURN
      END
