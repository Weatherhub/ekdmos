      SUBROUTINE PUTMOS(KSTA,FCST,ADD,XMULT,LIMITL,LIMITU,MSG,NDIG,    
     * LEADZ,MISS,KOUNT)

C        GLAHN   APRIL 1975   IBM 360/195                               
C                FEBRUARY 1977   'X' INSERTION ADDED
C
C        GILBERT MARCH 1991  NAS 9000   - CONVERTED TO FORTRAN 77
C
C        WEISS   SEPT  1995  HOBBS AND CRAY C90 CONVERSION.
C                            CONVERSION ENTAILS TWO OBJECTIVES:
C                            1. MAKE THE CODE CONFORM CLOSER
C                               TO FORTRAN 77 ANSI STANDARDS.
C                            2. STRUCTURE THE CODE BY ELIMINATING
C                               GO TOs AND EQUIVALENCE STATEMENTS
C                               FOR EASE OF FUTURE EDITING AS WELL
C                               AS PORTABILITY.
C
C        WEISS   NOV   1995  A FORECAST VALUE OF EITHER 9999. OR
C                            9997. WILL YIELD THE VALUE GIVEN IN 
C                            THE CHARACTER STRING "MISS'.
C
C        PURPOSE:
C
C            TO PLACE NUMERICAL VALUES INTO A CHARACTER STRING,
C            WHICH CAN THEN BE PUT INTO A TELETYPE BULLETIN. THIS
C            SUBROUTINE CAN ONLY HANDLE VALUES OF NDIG FROM ONE TO
C            THREE. FOR EX. IF NDIG=3, THE MISSING VALUE REPLACEMENT 
C            STRING GIVEN IN ARRAY MISS WILL BE 3 CHARACTERS IN LENGTH. 
C            NUMERICALLY, THIS WOULD YIELD A RANGE OF VALUES FROM
C            -99 TO 999.
C
CCCCCCCCCCCCC
C
C        VARIABLES:
C
C         ADD=ADDITIVE CONSTANT
C        FCST=VALUE TO BE PROCESSED INTO BULLETIN
C       KOUNT=INCREASED BY 1 EACH TIME A NON-MISSING
C             VALUE IS INSERTED.
C        KSTA=CALL LETTERS OF STATION
C       LEADZ=LEADING ZEROS TO BE INSERTED UNTIL LEADZ
C             CHARACTER POSITIONS ARE FILLED. MAXIMUM OF 3.
C      LIMITL=NCST WILL BE TRUNCATED AT LIMITL AT LOWER END.            
C      LIMITU=NCST WILL BE TRUNCATED AT LIMITU AT UPPER END.            
C        MISS=CHARACTER STRING MISSING INDICATOR INSERTION. NDIG               
C             CHARACTERS ARE INSERTED STARTING FROM THE RIGHT WHEN      
C             FCST = 9999. OR WHEN FCST = 9997.
C        MSGX=RIGHTMOST POSITION TO START NUMBER INSERTION
C         MSG=OUTPUT CHARACTER STRING (3 CHARACTERS OR LESS)
C        NDIG=NUMBER OF DIGITS OF NCST TO PLACE IN
C             MSG WORKING BACKWARDS. MAXIMUM OF 3.
C       XMULT=MULTIPLICATIVE CONSTANT. INTEGER VALUE
C             NCST=(FCST+ADD)*XMULT +.5
C
CCCCCCCCCCCCC

      CHARACTER*1 LU(10),MINUS,MSG(NDIG),MISS(NDIG),MXXXX,
     &            NBLAK
      REAL ZMISS,ZINSRT 
      LOGICAL KEEP,FALSE
      DATA LU/'1','2','3','4','5','6','7','8','9','0'/                  
      DATA MINUS/'-'/,MXXXX/'X'/,NBLAK/' '/
      ZMISS=9999.
      ZINSRT=9997.
      KEEP=.TRUE.
      FALSE=.FALSE.
      NEG=0

C        INITIALIZE THE MSG ARRAY

      DO 15 I=1,NDIG
	MSG(NDIG)=NBLAK
 15   CONTINUE

      IF((FCST.NE.ZMISS).AND.(FCST.NE.ZINSRT))THEN
         RND=+.5                                                           
         IF(FCST.LT.0)RND=-.5                                              
         NCST=(FCST+ADD)*XMULT+RND                                         
         IF(NCST.LT.LIMITL)NCST=LIMITL                                     
         IF(NCST.GT.LIMITU)NCST=LIMITU                                     
         M=IABS(NCST)                                                      
         NSH=M/100                                                         
         J=M-NSH*100                                                       
         NST=J/10                                                          
         NSU=J-NST*10

         IF((NSH-9).LE.0)THEN

C        NDIG=3
	    IF((NDIG.EQ.3).AND.KEEP)THEN
C        START DIGIT INSERTION
	       IF(NSH.GT.0) MSG(NDIG-2)=LU(NSH)
	       IF(NSH.EQ.0)THEN
		  IF(LEADZ.GE.3) MSG(NDIG-2)=LU(10)
C        FOR LEADZ3 LT 3 GO TO NDIG=2
               ENDIF
	       IF(NSH.LT.0) KEEP=.FALSE.
            ENDIF

C        NDIG=2 OR THE 2ND DIGIT FOR NDIG=3
            IF((NDIG.EQ.2).AND.(NSH.NE.0)) KEEP=.FALSE.

	    IF((NDIG.GE.2).AND.KEEP)THEN
	       IF(NST.GT.0) MSG(NDIG-1)=LU(NST)
	       IF(NST.EQ.0)THEN
		  IF(LEADZ.GE.2) MSG(NDIG-1)=LU(10)
		  IF(LEADZ.LT.2)THEN
		     IF(NSH.GT.0) MSG(NDIG-1)=LU(10)
		     IF(NSH.LT.0) KEEP=.FALSE.
C        FOR NSH EQ 0 GO TO NDIG=1
                  ENDIF
               ENDIF
	       IF(NST.LT.0) KEEP=.FALSE.
            ENDIF

C        NDIG=1 OR THE 2ND DIGIT FOR NDIG=2 & 3RD DIGIT FOR NDIG=3
            IF((NDIG.EQ.1).AND.((NSH.NE.0).OR.(NST.NE.0)))KEEP=.FALSE.

	    IF((NDIG.GE.1).AND.KEEP)THEN
	       IF(NSU.GE.0)THEN
		  IF(NSU.GT.0) MSG(NDIG)=LU(NSU)
		  IF(NSU.EQ.0) MSG(NDIG)=LU(10)
		  KOUNT=KOUNT+1
               ENDIF
               IF(NSU.LT.0) KEEP=.FALSE.
            ENDIF

C        IF NDIG EQ 1 NCST MUST BE POSITIVE(ONE BYTE MAXIMUM)
            IF((NCST.LT.0).AND.KEEP) THEN
	       IF(NDIG.EQ.3)THEN
		  IF(NCST.LE.-10) MSG(NDIG-2)= MINUS
		  IF((NCST.LT.0).AND.(NCST.GT.-10))THEN
		     MSG(NDIG-1)= MINUS
                  ENDIF
               ENDIF
	       IF(NDIG.EQ.2) MSG(NDIG-1)= MINUS
	       IF(NDIG.EQ.1) KEEP=.FALSE.
            ENDIF
         ENDIF

         IF(.NOT. KEEP)THEN
            WRITE(6,131) KSTA
 131        FORMAT('0INSERTION PROBLEM FOR STATION',2X,A8,
     &      '  MISSING INSERTED')
	    DO 180 K=1,NDIG
	      MSG(K)=MISS(K)
 180        CONTINUE
	 ENDIF
      ELSE

C            START 'X' INSERTION FOR FCST = 9997.                       
C            START MISSING INDICATOR INSERTION FOR FCST = 9999.

	 DO 185 K=1,NDIG
	   IF(FCST.EQ.ZMISS) MSG(K)=MISS(K)
C          IF(FCST.EQ.ZINSRT)MSG(K)=MXXXX
	   IF(FCST.EQ.ZINSRT)MSG(K)=MISS(K)
 185     CONTINUE
      ENDIF
		  
      RETURN                                                            
      END 
