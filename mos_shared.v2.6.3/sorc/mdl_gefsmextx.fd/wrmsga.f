      SUBROUTINE WRMSGA(MSG,N1,N2,NBSTA,NESTA,LINES,NCAT,KOUNT,M1,M2,   
     * NAME,JA,MDG,MHG,NOHEAD,LUNIT,BFILE,NHEAD)                        
C                                                                       
C        GLAHN   OCTOBER 1975   - IBM 360/195                           
C        DALLAVALLE  MAY 1988   - NAS 9000 - ADDED DOCBLOCK             
C        GILBERT   MARCH 1991   - CONVERTED TO FORTRAN 77, AND          
C                                 ADDED SOME PARAMETERS TO MAKE THE     
C                                 SUBROUTINE FOR MORE GENERAL USE.      
C                               - ADDED ALGORITHM TO DETERMINE THE      
C                                 NUMBER OF STATIONS IN EACH BULLETIN   
C        GILBERT   MAY 1991     - REMOVED A LARGE ARRAY TO MAKE SUBR.   
C                                 MORE SPACE EFFICIENT.                 
C        GILBERT   APR 1992     - DECREASED MAXIMUM BULLETIN SIZE TO    
C                                 3840 BYTES.                           
C        GILBERT   NOV 1992     - CORRECTED AN ERROR IN THE CALCULATION 
C                                 OF IST AND LST, WHICH CAUSED THE      
C                                 SUBROUTINE TO WRITE 1281 BYTE RECORDS 
C                                 INSTEAD OF 1280 BYTES.                
C        GILBERT   DEC 1992     - CORRECTED ERROR THAT CHECKS THE NO.   
C                                 OF STATIONS TO PUT IN EACH BULLETIN   
C                                 (ADDED KK COUNTER).                   
C        GILBERT   OCT 1993     - CHANGED TO ADD AS MANY STATIONS       
C                                 AS POSSIBLE TO EACH BULLETIN.
C          WEISS   OCT 1995     - CONVERTED FOR CRAY C90 AND HP (HOBBS)
C                                 OPERATIONS. CONTROL CHARACTERS
C                                 (TRANSMISSION COMPONENTS) AND BULLETIN
C                                 ARE NOW IN ASCII
C        COSGROVE  AUG 2005     - CONVERTED FROM THE FORMAT REQUIRED TO
C                                 DISSEMINATE VIA THE STATFILE TO VIA NTC.
C        ENGLE     SEP 2012     - CHANGED ENVVAR FROM 'XLFUNIT_  ' TO 'FORT  '
C
C        PURPOSE                                                        
C            FINALIZE PREPARATION OF MESSAGE AND WRITE TO TRANS-     
C            MISSION FILE.

C        VARIABLES
C               BFILE=OUTPUT FILE NAME
C              ICNT()=NUMBER OF CHARACTERS PER STATION
C        KOUNT(M1,M2)=FOR EACH LINE (M1) AND EACH STATION (M2),
C                     KOUNT( , ) = 0 UNLESS NON-MISSING FORECASTS HAVE
C                     BEEN INSERTED IN THAT LINE.  USED TO ELIMINATE
C                     LINES FROM BULLETIN.  KOUNT( , ) BEING NEGATIVE
C                     INDICATES A BLANK LINE TO BE TRANSMITTED UNLESS NO
C                     FORECASTS EXIST, IN WHICH CASE NOTHING WILL BE
C                     TRANSMITTED.
C                  JA=NUMBER OF CHARACTERS IN THE BULLETIN WMO HEADER
C                     TYPICALLY 11.
C              LENBUL=MAXIMUM LENGTH OF BULLETIN (INT. MULT. OF LRECL)
C               LINES=NUMBER OF LINES PER STATION
C               LRECL=USED TO BE RECORD LENGTH (BYTES) OF TRANS FILE.  NOW
C                     IT'S A SOMEWHAT ARBITRARY SIZE.
C               LUNIT=DATA SET REFERENCE NUMBER FOR TRANSMISSION FILE.
C              MAXSTA=MAXIMUM NUMBER OF STATIONS
C                 MDG=DAY, 2 DIGITS
C                 MHG=HOUR, 2 DIGITS
C          MSG(N1,N2)=CONTAINS MESSAGE BEFORE FINAL PREPARATION
C                MT()=HOLDS THE WHOLE MESSAGE FOR A BULLETIN INCLUDING
C                     TRANSMISSION COMPONENTS.
C            NAME(JA)=HOLDS BULLETIN NAME, UP TO 20 CHARACTERS
C               NBSTA=NUMBER OF FIRST STATION FOR BULLETIN
C                NCAT=CATELOGUE NUMBER, 5 CHARACTERS
C               NCHHD=NUMBER OF CHARACETERS IN THE HEADER
C               NESTA=NUMBER OF LAST STATION FOR BULLETIN
C               NHEAD=NUMBER OF BULLETIN HEADERS (IF APPLICABLE)
C                     OTHERWISE NHEAD IS EQUAL TO ONE
C              NOHEAD=NUMBER OF LINES IN HEADER, INCLUDING BLANKS
C              NSTAPB=NUMBER OF STATIONS PER BULLETIN
C              NUMREC=NUMBER OF RECORDS PER BULLETIN
C            TMPBUL()=HOLDS ONE BULLETIN (MAY INCLUDE SEVERAL STATIONS)

C        FUNCTIONS:
C                CHAR=INTRINSIC FUNCTION WHICH RETURNS THE ASCII CHARACT
C                     OF AN ASCII CHARACTER REPRESENTATION.
                                                                       
C        COMMENTS                                                       
C            THIS PROGRAM CONFORMS TO NMC'S OFFICE NOTE 100, EXCEPT     
C            FOR THE LENGTH OF THE BULLETIN CRITERIA.  O.N. 100 STATES  
C            THAT EACH BULLETIN SHOULD NOT EXCEED 2000 BYTES.  OSO      
C            STATED THAT THE MAXIMUM LENGTH OF A BULLETIN SHOULD BE     
C            3800 BYTES.  WE CHOSE A MAXIMUM OF 3840 BYTES, WHICH IS    
C            THREE TIMES THE RECORD LENGTH OF 1280 BYTES, FOR EASE OF   
C            COMPUTATION AND PROGRAMMING.  THE MAXIMUM NUMBER OF        
C            STATIONS THAT WILL FIT IN A BULLETIN IS CALCULATED AND     
C            THE ROUTINE ADDS AS MANY STATIONS AS POSSIBLE TO EACH      
C            BULLETIN.   NOTE:  IN AUGUST 2005 THIS ROUTINE WAS CHANGED.
C            WE NO LONGER WRITE OUT THE FILE DIRECT ACCESS, SO THE 1280 
C            RECORD LENGTH IS NOT NEEDED.  AT THIS POINT IN TIME, WE 
C            BELIEVE THAT THE MAXIMUM MESSAGE SIZE SHOULD BE LESS THAN
C            15000 BYTES, BUT IN THE INTEREST OF CAUSING THE LEAST RIPPLES
C            WE'LL LEAVE IT AS IT IS...  1280 * 4 = 5120.                                                 
C                                                                       
      PARAMETER (MAXSTA=2500,LRECL=1280)                                
      DIMENSION KOUNT(M1,M2),ICNT(MAXSTA)
      INTEGER IREC,NSTART,NHEAD 
      CHARACTER*18 NAME
      CHARACTER*80 BFILE,ENVVAR,FILEO
      CHARACTER*9  CTEMP
      CHARACTER*5  NCAT
      CHARACTER*3  MCRCRLF
      CHARACTER*1 MSG(N1,N2),NETB,NFIL,           
     *            MT(4*LRECL),TMPBUL(MAXSTA),
     *            MCRR,MLNF,MRECS

      DATA TMPBUL/2500*' '/
      DATA NSTART/0/

C        NSTART IS TREATED AS AN INTERNAL VARIABLE
      SAVE NSTART
      SAVE IREC

C        ASCII CONTROL CHARACTER DEFINITION
C        ALSO SET UP CARRIAGE RETURN/CARRIAGE RETURN/LINE FEED
      NETB=CHAR(62)
      NFIL=' '
      MCRR=CHAR(13)
      MLNF=CHAR(10)
      MRECS=CHAR(30)
      MCRCRLF(1:1)=MCRR
      MCRCRLF(2:2)=MCRR
      MCRCRLF(3:3)=MLNF

      DO 100 I=1,4*LRECL 
        MT(I)=NFIL
 100  CONTINUE
      DO 101 I=1,MAXSTA
	ICNT(I)=0
 101  CONTINUE

      IF (NSTART.EQ.0)THEN
C        USE NCEP UTILITIES GETENV AND BAOPENWT TO OPEN OUTPUT FILE
C        IF THIS IS THE FIRST TIME ENTERING THE SUBROUTINE.
C        BAOPENWT CREATES A NEW FILE EACH TIME IT OPENS A FILE. IT IS
C        BELIEVED THAT THIS FILE IS WRITE-ONLY. 
CINTEL
      ENVVAR='FORT  '
      WRITE(ENVVAR(5:6),FMT='(I2)')LUNIT
C      ENVVAR='XLFUNIT_  '
C      WRITE(ENVVAR(9:10),FMT='(I2)')LUNIT
CINTEL
      CALL GETENV(ENVVAR,FILEO)
      CALL BAOPENWT(LUNIT,FILEO,IRET)
        IF(IRET.NE.0)THEN
          WRITE(*,102)
 102      FORMAT(/'**** ERROR OPENING TEXT OUTPUT FILE.  STOP IN ',
     &          'WRMSGNTC AT 102')
          STOP 102
        ENDIF
       ENDIF

C        INCREMENT NSTART TO EVENTUALLY EQUAL NHEAD
       NSTART=NSTART+1

C       FIRST 19 BYTES OF THE ARRAY MT SAVE FOR FILE FIELD SEPARATOR
      CALL PUTCHAR('                   ',MT(1),19)

C        PLACES BULLETIN NAME
      CALL PUTCHAR(NAME(1:11),MT(20),JA)

C        PLACES BLANK
      CALL PUTCHAR(' ',MT(JA+20),1)

C        PLACES DAY
      CALL PUTMOS('DAY',FLOAT(MDG),0.,1.,1,31,MT(JA+21),2,2,'99',KT)

C        PLACES HOUR
      CALL PUTMOS('HR',FLOAT(MHG),0.,1.,0,24,MT(JA+23),2,2,'99',KT)

C        PLACES MINUTES
      CALL PUTCHAR('00',MT(JA+25),2)

C        INSERT CR CR LINE-FEED CONTROL CHARACTERS
      CALL PUTCHAR(MCRCRLF,MT(JA+27),3)

C        INSERT PIL
      CTEMP=NAME(13:18)//MCRCRLF
      CALL PUTCHAR(CTEMP,MT(JA+30),9)
      NOC=JA+39                                                         
C        NOC IS PLACE TO PUT NEXT CHARACTER                        
C        WRITE HEADER RECORDS TO MT( )
C        IF HEADER CONTAINS MORE THAN ONE RECORD
      DO 200 I=2,NOHEAD                                                 
        CALL PUTQ(MSG(1,I),MT(NOC),NOC,N1)                              
 200  CONTINUE

      NCHHD=NOC                                                         
C        COPY EACH LINE FOR STATION K TO TMPBUL (IF FORECASTS WERE      
C        WRITTEN) TO DETERMINE STATION WITH LARGEST # OF BYTES (IMAX)
      IMAX=0

      DO 400 K=NBSTA,NESTA                                              
        NOC=1                                                           
        DO 300 J=1,LINES                                                
          IF (KOUNT(J,K).NE.0) THEN                                     
            CALL PUTQ(MSG(1,(K-1)*LINES+J+NOHEAD),TMPBUL(NOC),NOC,N1)   
          ENDIF                                                         
 300    CONTINUE                                                        
        ICNT(K)=NOC                                                     
        IF (ICNT(K).GT.IMAX) IMAX=ICNT(K)                               
 400  CONTINUE

C        DETERMINE THE MAXIMUM NUMBER OF STATIONS THAT WILL FIT IN A    
C        BULLETIN.  ANY ADDITIONAL STATIONS WILL BE PUT IN ANOTHER      
C        BULLETIN WITH THE SAME WMO HEADER. (NSTAPB IS TYPICALLY 2)
      LENBUL=4*LRECL                                                    
      NSTAPB=(LENBUL-NCHHD)/IMAX                                        


C        START FILLING BULLETIN
C******************************
C

C        WRMSG GENERATES A TX BULLETIN FOR EVERY NSTAPB STATIONS
      NOC=NCHHD                                                         
      KK=0

      DO 1000 K=NBSTA,NESTA                                             
        KK=KK+1

C        WRITE A STATION MESSAGE TO THE BULLETIN ARRAY                  
C        IN THE STATFILE DAYS, THE ^ IN THE FIRST COLUMN WAS DROPPED
C        BY THE MAINFRAME COMPUTER, AND THEREFORE A BUNCH OF THE 
C        LINES IN THE BULLETIN GOT SHIFTED ONE COLUMN TO
C        THE LEFT.  FOR NTC WE CHANGED THE ^ TO A SPACE, SO TO FIX THE
C        SPACING, IF THE LINE STARTS WITH A BLANK SPACE, PUT THE LINE
C        FROM COLUMN 2 TO THE END INTO PUTQ RATHER THAN FROM 1 TO THE END.
C        IN OTHER WORDS... FORCE THE SHIFT TO THE LEFT HERE RATHER THAN 
C        HAVING IT HAPPEN MYSTERIOUSLY IN THE GATEWAY SOMEWHERE.
        DO 650 J=1,LINES                                                
          IF (KOUNT(J,K).NE.0) THEN                                     
           IF((MSG(1,(K-1)*LINES+J+NOHEAD).EQ.' ').AND.
     1         (MSG(2,(K-1)*LINES+J+NOHEAD).NE.' '))THEN
            CALL PUTQ(MSG(2,(K-1)*LINES+J+NOHEAD),MT(NOC),NOC,N1)       
           ELSE
            CALL PUTQ(MSG(1,(K-1)*LINES+J+NOHEAD),MT(NOC),NOC,N1)       
           ENDIF
          ENDIF                                                         
 650    CONTINUE


C        DETERMINE HOW MANY RECORDS ARE NEEDED TO HOLD THE BULLETIN. 
C        END OF BULLETIN RECORD REMOVED FOR NTC
        IF ((MOD(KK,NSTAPB).EQ.0).OR.(K.EQ.NESTA)) THEN                 
C          CALL PUTCHAR('%',MT(NOC),1)                                  
           WRITE(6,651) NAME,NOC                            
 651       FORMAT(' BULLETIN ',A11,' CONTAINS ',I6,' BYTES.')

C        NUMREC IS (3100/1280)+1 OR APROX. 3 
	   NUMREC=(NOC/LRECL)+1                                          
           IF (MOD(NOC,LRECL).EQ.0) NUMREC=NUMREC-1

C        WRITE THE BULLETIN OUT TO THE TX FILE  

C        USE NCEP UTILITY MKFLDSEP TO ADD THE FILE FIELD SEPARATOR.
      CALL MKFLDSEP(MT,2,0,NOC-19,LENOUT)
C
C        WRITE THE BULLETIN OUT TO THE FILE USING NCEP UTILITY WRYTE
      CALL WRYTE(LUNIT,NOC,MT)

C        RESET MT() TO START THE NEXT BULLETIN                          
           NOC=JA+39                                                    
           DO 900 I=NOC,4*LRECL                                         
             MT(I)=NFIL                                                 
 900       CONTINUE                                                     
        ENDIF

 1000 CONTINUE

      GO TO 600

 99    WRITE(6,305)IOS
 305   FORMAT(1X,'OPEN IN WRMSG FAILED: IOS=',I5)
       CALL EXIT(215)

C        NOW CLOSE THE OUTPUT FILE USING THE NCEP UTILITY BACLOSE
 600   IF(NSTART.EQ.NHEAD)THEN
         CALL BACLOSE(LUNIT,IER)
       ENDIF

      RETURN                                                            
      END                                                               
