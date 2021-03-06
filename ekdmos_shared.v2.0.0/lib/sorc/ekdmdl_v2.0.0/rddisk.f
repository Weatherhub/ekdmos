      SUBROUTINE RDDISK(KFILDO,KFIL,JREC,DATA,NWORDS,NBLOCK,NOREC,IER)
C 
C       APRIL 1994   GLAHN   TDL   MOS-2000
C       APRIL 2000   DALLAVALLE    MODIFIED FORMAT STATEMENTS TO 
C                                  CONFORM TO FORTRAN 90 STANDARDS
C                                  ON THE IBM SP
C 
C        PURPOSE 
C            TO READ A LOGICAL DATA RECORD.  THE LOGICAL RECORD 
C            CAN SPAN MORE THAN ONE PHYSICAL RECORD.
C 
C        DATA SET USE 
C            KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT) 
C            KFIL   - UNIT NUMBER FOR FILE TO READ.  (INPUT) 
C 
C        VARIABLES 
C 
C            INPUT 
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE. 
C                KFIL = UNIT NUMBER FOR FILE TO READ. 
C                JREC = RECORD NUMBER OF 1ST PHYSICAL RECORD
C                       TO READ FROM FILE ON UNIT KFIL.
C              NWORDS = NUMBER OF DATA WORDS TO BE RETURNED.  SEE DATA( ). 
C              NBLOCK = SIZE OF PHYSICAL DISK RECORD IN WORDS. 
C                       AS MANY RECORDS AS ARE NEEDED WILL BE USED 
C                       TO READ THE NWORDS OF DATA.  THIS IS CORRECT
C                       FOR EITHER A 32- OR 64-BIT MACHINE.
C
C            OUTPUT 
C            DATA(J) = DATA RETURNED (J=1,NWORDS). 
C              NOREC = THE NUMBER OF PHYSICAL RECORDS READ.
C                IER = ERROR RETURN.
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            NONE 
C 
      DIMENSION DATA(NWORDS)
C
D     WRITE(KFILDO,101)KFIL,JREC,NWORDS,NBLOCK
D101  FORMAT(' ####IN RDDISK'6I10)
      IER=0
      NOREC=0
C       
      IF(NWORDS.GT.NBLOCK)GO TO 140
C
C        ONLY ONE PHYSICAL RECORD IS NEEDED.
C
D     WRITE(KFILDO,102)JREC,NWORDS
D102  FORMAT(/' RDDISK, JREC,NWORDS ='2I8)
      READ(KFIL,REC=JREC,IOSTAT=IOS,ERR=900)(DATA(J),J=1,NWORDS) 
      NOREC=NOREC+1
      GO TO 950
C 
C        MORE THAN ONE PHYSICAL RECORD IS NEEDED.
C
 140  NWDS1=NWORDS
C        NWDS1 = NO. OF WORDS YET TO READ INTO DATA( ).
      NWDS2=MIN(NBLOCK,NWORDS)
C        NWDS2 = NO. OF WORDS TO READ FROM NEXT RECORD INTO DATA( ).
      NWDS3=0
C        NWDS3 = NO. OF WORDS ALREADY READ INTO DATA( ).
D     WRITE(KFILDO,144)JREC,NBLOCK,NOREC,NWORDS,NWDS1,NWDS2,NWDS3
D144  FORMAT(/' RDDISK, JREC,NBLOCK,NOREC,NWORDS,NWDS1,NWDS2,NWDS3 ='
D    1          7I6)
      READ(KFIL,REC=JREC,IOSTAT=IOS,ERR=900)(DATA(J),J=1,NWDS2)
      NOREC=NOREC+1
C
 145  IF(NWDS1.LE.NWDS2)GO TO 950
      NWDS3=NWDS3+NWDS2
      NWDS1=NWORDS-NWDS3
      NWDS2=MIN(NBLOCK,NWDS1)
D     WRITE(KFILDO,144)JREC,NBLOCK,NOREC,NWORDS,NWDS1,NWDS2,NWDS3
      READ(KFIL,REC=JREC+NOREC,IOSTAT=IOS,ERR=900)
     1      (DATA(J),J=NWDS3+1,NWDS3+NWDS2)
      NOREC=NOREC+1
      GO TO 145
C
 900  WRITE(KFILDO,901)JREC,KFIL,IOS
 901  FORMAT(/,' ****ERROR READING LOGICAL RECORD ',I5,' ON UNIT ',I4,
     1         ', IOSTAT =',I5,'  IN RDDISK.',/,
     2         '     IER IS RETURNED WITH THE VALUE OF IOSTAT.')
      IER=IOS
 950  CONTINUE
      RETURN 
      END 
