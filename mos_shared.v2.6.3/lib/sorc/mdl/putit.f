      SUBROUTINE PUTIT(KFILDO,NWHAT,NWHERE,N)                                     
C
C        APRIL 1981   GLAHN   TDL   IBM 360/195                            
C        JUNE  1990   GILBERT TDL   NAS 9000   CONVERTED TO FORTRAN 77 
C        APRIL 1998   GLAHN   TDL   ADAPTED TO UNIX AND MOS-2000    
C                                                                       
C        PURPOSE                                                           
C            PLACES N DIGITS FROM VARIABLE NWHAT INTO ARRAY                
C            NWHERE(I),I=1,N.                                              
C
C        DATA SET USE                                                      
C            KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES                                                         
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C               NWHAT = VALUE FROM WHICH DIGITS ARE TAKEN TO
C                       PUT INTO NWHERE( ).  (INPUT)
C           NWHERE(J) = DIGITS FROM NWHAT.  (CHARACTER*1)  (OUTPUT)
C                   N = NUMBER OF DIGITS TO TAKE FROM NWHAT AND
C                       PLACE IN NWHERE( ), MAXIMUM OF 20.  (INPUT)
C
      CHARACTER*1 NWHERE(80),NA(11) 
C
      DIMENSION ND(10)
C
      DATA ND/0,1,2,3,4,5,6,7,8,9/
      DATA NA/'0','1','2','3','4','5','6','7','8','9',' '/
C
      IF(N.LE.20)GO TO 110 
      WRITE(KFILDO,105)N 
 105  FORMAT(/' ****N =',I6,' TOO LARGE.  STOP IN PUTIT AT 105.')
      STOP 105
C
 110  NFIRST=0
C
      DO 130 K=1,N              
      M=N+1-K 
      NQ=(NWHAT-(NWHAT/10**M)*10**M)/10**(M-1)
C
      DO 120 L=1,10
      IF(NQ.EQ.ND(L))GO TO 125 
 120  CONTINUE
C
      WRITE(KFILDO,121)
 121  FORMAT(/' ****ERROR IN PUTIT.  STOP AT 121.')
      STOP 121 
C
 125  IF(L.NE.1)GO TO 128
      IF(NFIRST.EQ.1)GO TO 129 
      NWHERE(K)=NA(11) 
      GO TO 130 
C
 128  NFIRST=1
 129  NWHERE(K)=NA(L)
 130  CONTINUE                                                             
C
      RETURN
      END 
