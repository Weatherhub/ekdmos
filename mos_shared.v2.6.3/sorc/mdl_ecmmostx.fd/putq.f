      SUBROUTINE PUTQ(M,N,NOC,LL)                                       
C        GLAHN   MAY 1975   IBM 360/195                                 
C        GILBERT MARCH 1991 NAS 9000   - CONVERTED TO FORTRAN 77
C        WEISS   OCT   1995 HOBBS AND CRAY C90. NO CHANGES MADE
C                           TO CODE.
C        RLC     MAR   2005 IN ORDER TO SEND TEXT PRODUCTS NTC WE
C                           HAD TO CHANGE THE >>@ TO THE PROPER
C                           REPRESENTATION.
C
C        PURPOSE                                                        
C            TO FIND NUMBER OF NON-BLANK CHARACTERS IN LINE M(LL) AND   
C            PLACE THEM IN N( ) FOLLOWED BY THE 3 CHARACTERS <<@.       
C            NOC IS INCREASED BY THE TOTAL NUMBER OF CHARACTERS ADDED TO
C            N.  FOR USE WITH WRMSG.  LL IS NUMBER OF CHARACTERS IN M( )
C            AND WOULD USUALLY BE THE DIMENSION OF M.  PUTQ IS PRESENTLY
C            DIMENSIONED FOR A MAXIMUM LINE LENGTH OF 80.
C
C        VARIABLES
C               LL = NUMBER OF CHARACTERS IN M()
C              M() = INPUT CHARACTER LINE                               
C              N() = OUTPUT CHARACTER LINE                              
C              NOK = POSITION IDENTIFIER                                
C              NOC = TOTAL NUMBER OF CHARACTERS IN THE LINE             
C             MCRR = DECIMAL REPRESENTATION OF A CARRIAGE RETURN
C             MLNF = DECIMAL REPRESENTATION OF A LINE FEED
C          MCRCRLF = DECIMAL REPRESENTATION OF >>@ 

C                                                                       
      CHARACTER*1 M(LL),N(LL+3),NBLAK,MCRR,MLNF                                   
      CHARACTER*3 MCRCRLF
C
      DATA NBLAK/' '/                                                   
      DO 110 K=1,LL                                                     
        IF(M(LL+1-K).NE.NBLAK)GO TO 120                                 
 110  CONTINUE                                                          
      NOK=0                                                             
      GO TO 130                                                         
 120  NOK=LL+1-K                                                        
      CALL PUTCHAR(M,N,NOK,LL)                                             
      MCRR=CHAR(13)
      MLNF=CHAR(10)
      MCRCRLF(1:1)=MCRR
      MCRCRLF(2:2)=MCRR
      MCRCRLF(3:3)=MLNF
 130  CALL PUTCHAR(MCRCRLF,N(NOK+1),3,LL)                                    
      NOC=NOC+NOK+3                                                     
      RETURN                                                            
      END                                                               
