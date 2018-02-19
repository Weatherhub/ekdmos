      SUBROUTINE RDDATM(KFILDO,KFILX,JREC,RECORD,NSIZE,KSIZE,CFROM,IER)
C 
C        NOVEMBER 1996   GLAHN   TDL   MOS-2000
C        APRIL    2000   DALLAVALLE    MODIFIED FORMAT STATEMENTS TO
C                                      CONFORM TO FORTRAN 90 STANDARDS
C                                      ON THE IBM SP
C 
C        PURPOSE 
C            TO READ A LOGICAL DATA RECORD FROM THE EXTERNAL MOS-2000
C            DIRECT ACCESS FILE SYSTEM.  THE LOGICAL RECORD CAN SPAN 
C            MORE THAN ONE PHYSICAL RECORD.  CALLED BY FLOPNM, RDTDLM. 
C 
C        DATA SET USE 
C            KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT) 
C            KFILX  - UNIT NUMBER FOR MOS-2000 FILE.  (INPUT) 
C 
C        VARIABLES 
C 
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT) 
C               KFILX = UNIT NUMBER FOR MOS-2000 FILE.  (INPUT) 
C                JREC = RECORD NUMBER * 1000 OF 1ST PHYSICAL RECORD
C                       TO READ FROM FILE NUMBER KFILX.  (INPUT)
C           RECORD(J) = DATA READ FROM LOGICAL RECORD (J=1,NSIZE).
C                       (OUTPUT) 
C               NSIZE = NO. OF DATA WORDS TO READ.  SEE RECORD( ).
C                       (INPUT) 
C               KSIZE = SIZE OF PHYSICAL RECORD.  (INPUT),
C               CFROM = 6 CHARACTERS TO IDENTIFY CALLING PROGRAM. 
C                       (CHARACTER*6)  (INPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       152 = RECORD NUMBER OF PHYSICAL RECORD SIZE
C                             INCORRECT.
C                       OTHER VALUES FROM SYSTEM.
C                       (OUTPUT)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            NONE 
C 
      CHARACTER*6 CFROM
C
      DIMENSION RECORD(NSIZE)
C
      IER=0 
C
      MREC=JREC/1000
C
      IF(MREC.LE.0.OR.
     1   KSIZE.LE.0)THEN
         WRITE(KFILDO,110)MREC,KSIZE,KFILX,CFROM
 110     FORMAT(/,' ****EITHER THE RECORD NUMBER = ',I4,
     1            ' OR THE PHYSICAL RECORD SIZE =',I7,
     2            ' IS IN ERROR IN RDDATM FOR UNIT NO.',I3,'.',/,
     3            '     RDDATM CALLED FROM ',A6)
         IER=152
         GO TO 902
C
      ENDIF
C
      K=0
      NSTART=1
C        NSTART IS THE FIRST WORD TO FILL IN RECORD( ).
      NREMIN=NSIZE
C        NREMIN IS THE NUMBER OF WORDS REMAINING TO READ.
 120  NEND=NSTART-1+MIN(KSIZE,NREMIN)
      READ(KFILX,REC=MREC+K,IOSTAT=IOS,ERR=900)
     1        (RECORD(J),J=NSTART,NEND)
      K=K+1
      IF(K*KSIZE.GE.NSIZE)GO TO 150
      NSTART=K*KSIZE+1
      NREMIN=NREMIN-KSIZE
      GO TO 120
C
 150  CONTINUE
D     WRITE(KFILDO,170)CFROM,MREC,(RECORD(J),J=1,NSIZE)
D170  FORMAT(/' IN RDDATM FROM'2XA6,' RECORD NO. ='I5/(' '9F10.2))
      GO TO 902 
C 
 900  WRITE(KFILDO,901)KFILX,CFROM,IOS
 901  FORMAT(/,' ****TROUBLE READING DATA RECORD ON UNIT NO. ',I3,
     1         ' IN RDDATM FROM ',A6,'.  IOSTAT = ',I4)
      IER=IOS
C
 902  RETURN
C
      END 
