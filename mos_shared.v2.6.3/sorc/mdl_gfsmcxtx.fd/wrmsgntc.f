      SUBROUTINE WRMSGNTC(MSG,N1,N2,NBSTA,NESTA,LINES,NCAT,KOUNT,M1,M2,   
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
C        COSGROVE  FEB 2005     - MODIFIED TO SEND OUT MESSAGES IN
C                                 ONE BIG CHUNK RATHER THAN SEGEMENTING
C                                 IT.  THE SIZE IS 391*1280 = 500480 BYTES
C        COSGROVE  MAR 2005     - REWORKED ROUTINE TO SEND TEXT PRODUCT
C                                 NTC.  THIS INVOLVED REMOVING THE 1280-TYE
C                                 CHUNKS, AND CHANGING SOME OF THE SYMBOLS
C                                 WE WERE USING.  ALSO REMOVED MSOH AND
C                                 END-OF-BULLETIN CHARACTER.  GOT RID OF
C                                 ALL THE PARTS THAT WERE BREAKING THE
C                                 MESSAGES UP INTO SMALLER CHUNKS.
C        ENGLE     SEP 2012     - CHANGED ENVVAR FROM 'XLFUNIT_  ' TO 'FORT  '
C
C        PURPOSE                                                        
C            FINALIZE PREPARATION OF MESSAGE AND WRITE TO TRANS-     
C            MISSION FILE.
C
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
C               LRECL=RECORD LENGTH (BYTES) OF TRANS FILE. TRANSMISSION
C                     FILE MUST BE BLOCKED IN RECORDS OF 1280 BYTES.
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
C                                                                       
      PARAMETER (MAXSTA=10000,LRECL=1280)                                
      DIMENSION KOUNT(M1,M2),ICNT(MAXSTA)
      INTEGER IREC,NSTART,NHEAD 
      CHARACTER*18 NAME
      CHARACTER*80 BFILE,ENVVAR,FILEO
      CHARACTER*9  CTEMP
      CHARACTER*5  NCAT
      CHARACTER*3  MCRCRLF
      CHARACTER*1 MSG(N1,N2),NETB,NFIL,MCRR,MLNF,MRECS,           
     *            MT(N1*N2),TMPBUL(MAXSTA)

      DATA TMPBUL/10000*' '/
      DATA NSTART/0/

C        NSTART IS TREATED AS AN INTERNAL VARIABLE
      SAVE NSTART
      SAVE IREC

C        ASCII CONTROL CHARACTER DEFINITION
      MCRR=CHAR(13)
      MLNF=CHAR(10)
      MRECS=CHAR(30)
      MCRCRLF(1:1)=MCRR
      MCRCRLF(2:2)=MCRR
      MCRCRLF(3:3)=MLNF
 
      NETB=CHAR(62)
      NFIL=' '

      DO 100 I=1,N1*N2 
        MT(I)=NFIL
 100  CONTINUE
C
C      USE BAOPEN TO OPEN THE OUTPUT FILE
C
CINTEL
      ENVVAR='FORT  '
      WRITE(ENVVAR(5:6),FMT='(I2)')LUNIT
C      ENVVAR='XLFUNIT_  '
C      WRITE(ENVVAR(9:10),FMT='(I2)')LUNIT
CINTEL
      CALL GETENV(ENVVAR,FILEO)
      CALL BAOPEN(LUNIT,FILEO,IRET)
      IF(IRET.NE.0)THEN
         WRITE(*,102)
 102    FORMAT(/'****  ERROR OPENING THE TEXT OUTPUT FILE.  STOP IN',
     &           ' WRMSGA AT 102')
         STOP 102
      ENDIF


C        INCREMENT NSTART TO EVENTUALLY EQUAL NHEAD
       NSTART=NSTART+1

C       FIRST 19 BYTES OF THE ARRAY MT SAVE FOR FFS
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
C        NOC IS PLACE TO PUT NEXT CHARACTER (62)                        
C        WRITE HEADER RECORDS TO MT( )
C        IF HEADER CONTAINS MORE THAN ONE LINE
      DO 200 I=2,NOHEAD                                                 
        CALL PUTQ(MSG(1,I),MT(NOC),NOC,N1)                              
 200  CONTINUE

      NCHHD=NOC                                                         

C        START FILLING BULLETIN
C******************************
C
      NOC=NCHHD                                                         
C*******************

      DO 1000 K=NBSTA,NESTA                                             

C        WRITE A STATION MESSAGE TO THE BULLETIN ARRAY                  
        DO 650 J=1,LINES                                                
          IF (KOUNT(J,K).NE.0) THEN                                     
            CALL PUTQ(MSG(1,(K-1)*LINES+J+NOHEAD),MT(NOC),NOC,N1)       
          ENDIF                                                         
 650    CONTINUE

 1000 CONTINUE

C        USE THE UTILITY MKFLDSEP TO ADD THE FILE FIELD SEPARATOR
         CALL MKFLDSEP(MT,2,0,NOC-19,LENOUT)

C        WRITE THE BULLETIN OUT TO THE TX FILE USING NCEP UTILITY
           CALL WRYTE(LUNIT,NOC,MT)

      GO TO 600

 99    WRITE(6,305)IOS
 305   FORMAT(1X,'OPEN IN WRMSG FAILED: IOS=',I5)
       CALL EXIT(215)

C       MUST NOW CLOSE THE DIRECT ACCESS DATA FILE
 600  IF(NSTART.EQ.NHEAD)THEN 
	 CALL BACLOSE(LUNIT,IER)
      ENDIF


      RETURN                                                            
      END                                                               
