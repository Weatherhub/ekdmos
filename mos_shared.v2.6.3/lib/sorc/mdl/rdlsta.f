      SUBROUTINE RDLSTA(NUNIT,DATA,ND,DTEMP,NT,FMT,NVAL,TERM,IVALEN)
C        GLAHN      OCT 1975  TDL FOR M201 ADAPTED FROM 
C                             HOLLENBAUGH'S CDC 6600 LIST
C        JENSENIUS  OCT 1989  MODIFIED TO RUN IN FORTRAN77
C        WEISS      NOV 1995  HOBBS AND CRAY C90. SUBROUTINE RDLSTA
C                             ORIGINATED FROM SUBROUTINE RDDATA. DUE TO
C                             CONVERSION CONSIDERATIONS, RDDATA WAS 
C                             SEGMENTED INTO A CHARACTER, INTEGER AND
C                             REAL VERSIONS. THE CHARACTER VERSION IS
C                             CALLED "RDLSTA".
C
C        PURPOSE
C            TO READ CHARACTER DATA WITH A GIVEN FORMAT. ASSUME THE 
C            MAXIMUN LENGTH OF EACH RECORD IS 80 CHARACTERS 
C        
C                                                                       
C        VARIABLES:                                                     
C                                                                       
C          DATA = ARRAY NAME
C         DTEMP = TEMPORARY ARRAY USED TO READ ONE RECORD
C                 TEMPORARY ARRAY DTEMP MUST BE OF AT LEAST SIZE NT
C          DTST = ARRAY OF 80 BLANKS
C           FMT = CONTAINS FORMAT OF DATA
C        IVALEN = CHARACTER STRING LENGTH (ASSUME ALL CHARACTER STRINGS
C                 ARE OF THIS LENGTH).
C            ND = SIZE OF ARRAY NAME 
C         NUNIT = DATA SET REFERENCE NUMBER FROM WHICH TO READ DATA
C            NT = NUMBER OF WORDS PER RECORD
C          NVAL = COUNT OF ELEMENTS IN ARRAY RETURNED                   
C          TERM = TERMINATOR                                            
C                                                                       
      CHARACTER*(*) DATA(ND),DTEMP(NT),TERM                               
      CHARACTER*(*) FMT
      CHARACTER*80 DTST
      LOGICAL VALUE

      DATA DTST(1:80)/' '/
      VALUE=.TRUE.
      NVAL=0

C        START READING RECORDS FROM THE DATASET

 115  READ(NUNIT,FMT)DTEMP                                              
      DO 125 K=1,NT
         IF(DTEMP(K).EQ.TERM) VALUE=.FALSE.
         IF((DTEMP(K).NE.DTST(1:IVALEN)).AND.VALUE)THEN
	    NVAL=NVAL+1
	    IF(NVAL.GT.ND)THEN
	       WRITE(6,131) NUNIT
 131           FORMAT(///' LIST TOO LONG ON UNIT NO. ',
     *         I2,'  STOP 131 IN RDDATA')
	       PRINT FMT,DTEMP
	       STOP 131
            ENDIF
	    DATA(NVAL)=DTEMP(K)
         ENDIF

C        RDLSTA WILL SKIP A WORD FILLED WITH BLANKS.
	 IF((K.EQ.NT).AND.VALUE) GO TO 115
	 IF((DTEMP(K).EQ.DTST(1:IVALEN)).AND.VALUE) GO TO 125
 125  CONTINUE
	 
      RETURN                                                            
      END                                                               
