      SUBROUTINE SWITCH(KFILDO,IN,KFILIN,NAMIN,JFOPEN,LKHERE,MSDATE,
     1                  NUMIN,ND6,NDATE,IRD,IP23,ISTOP,IER)
C
C        JUNE      1997   GLAHN   MOS-2000
C        SEPTEMBER 1997   GLAHN   ADDED IP23 CLOSING FILE DIAGNOSTIC
C        SEPTEMBER 1997   GLAHN   CHANGES FOR TRAILER USE CHANGE
C        SEPTEMBER 1997   GLAHN   ADDED MSDATE TO CALL
C        SEPTEMBER 1997   GLAHN   REMOVED EOF WRITE STATEMENT
C        DECEMBER  1998   GLAHN   COMMENT CORRECTED FOR IRD
C        APRIL     2000   DALLAVALLE   MODIFIED FORMAT STATEMENTS TO
C                                      CONFORM TO FORTRAN 90 STANDARDS
C                                      ON THE IBM SP
C        MAY 8     2000   MCE     REMOVED FILENAME FROM OPEN STMNT
C                                 FOR OPERATIONS
C        DECEMBER  2002   GLAHN   ADDED INCREMENTING ISTOP ON FILE
C                                 OPENING ERROR
C        DECEMBER  2006   GLAHN   ADDED NOTE UNDER PURPOSE
C        JULY      2012   ENGLE   ADDED CONVERTX; ADDED CALL TO CKFILEND
C                                 BEFORE OPENING A TDLPACK VECTOR FILE;
C                                 MODIFIED OPEN STATEMENT TO INCLUDE
C                                 CONVERTX= SPECIFIER.
C
C        PURPOSE
C           TO SWITCH FILES FOR RDSTR2 AND PRED22.  JFOPEN( ) AND
C           LKHERE( ) ARE APPROPRIATELY SET ON OUTPUT.
C
C           NOTE:  IF A FILE ON A PARTICULAR UNIT NUMBER CANNOT BE
C                  OPENED, IT IS LIKELY OTHER FILES ON THAT SAME
C                  UNIT NUMBER WILL NOT BE USED BY THE CALLING
C                  PROGRAMS.
C   
C        DATA SET USE
C            KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C              IP23 - UNIT NUMBER FOR DIAGNOSTICS.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C                  IN = THE NUMBER OF THE FILE BEING DEALT WITH.
C                       (INPUT)
C           KFILIN(J) = UNIT NUMBERS FOR INPUT DATA (J=1,NUMIN).
C                       (INPUT)
C            NAMIN(J) = NAME OF THE INPUT FILES BEING PROCESSED 
C                       (J=1,NUMIN).  (CHARACTER*60)  (INPUT)
C           JFOPEN(J) = FOR EACH FILE IN KFILIN(J), JFOPEN(J) IS 1 WHEN
C                       THE FILE IS OPEN, IS 0 WHEN IT HAS ALREADY BEEN
C                       USED AND IS 2 WHEN THE FILE HAS NOT BEEN OPENED 
C                       (J=1,NUMIN).  (INPUT/OUTPUT)
C           LKHERE(J) = KEEPS TRACK OF WHICH FILES AN EOF HAS BEEN 
C                       REACHED (J=1,NUMIN).  INITIALLY SET TO 1; SET
C                       TO ZERO WHEN AN EOF HAS BEEN REACHED.
C                       (INPUT/OUPUT)
C           MSDATE(J) = KEEPS TRACK OF WHETHER ANY DATA ARE AVAILABLE
C                       FOR A PARTICULAR DATE ON AN INPUT FILE(S)
C                       (J=1,NUMIN).  (INPUT/OUTPUT)
C               NUMIN = THE NUMBER OF VALUES IN KFILIN( ),
C                       NAMIN( ), JFOPEN( ), AND LKHERE( ).  (INPUT)
C                 ND6 = SIZE OF KFILIN( ), ETC.  (INPUT)
C               NDATE = THE DATE/TIME BEING PROCESSED.  FOR DIAGNOSTICS
C                       ONLY.  (INPUT)
C                 IRD = 1 WHEN THE DIRECTORY RECORD ON THE NEXT FILE
C                       MUST BE READ.  0 OTHERWISE.  (OUTPUT)
C                IP23 = INDICATES WHETHER (>0) OR NOT (=0) STATEMENTS
C                       ABOUT EOF AND FILE OPENINGS AND CLOSINGS WILL
C                       BE OUTPUT FOR PRINTING ON UNIT IP23.  (INPUT)
C               ISTOP = INCREMENTED BY ONE EACH TIME AN ERROR IS
C                       ENCOUNTERED.  (INPUT-OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        31 = TROUBLE OPENING OR SWITCHING FILE.
C                       (OUTPUT)
C            CONVERTX = CHARACTER HOLDING THE KEYWORD USED TO OPEN
C                       RANDOM ACCESS FILE WITH THE CORRECT ENDIAN.
C                       (CHARACTER*20).
C 
C        NONSYSTEM SUBROUINES USED 
C            CKFILEND
C
      CHARACTER*60 NAMIN(ND6)
CINTEL
      CHARACTER*20 CONVERTX
CINTEL
C
      DIMENSION KFILIN(ND6),LKHERE(ND6),MSDATE(ND6),JFOPEN(ND6)
C
CINTEL
      CONVERTX='BIG_ENDIAN'
CINTEL
      IER=0
      IRD=0
C
      CLOSE(UNIT=KFILIN(IN),IOSTAT=IOS,ERR=1136)
      IF(IP23.NE.0)WRITE(IP23,1108)KFILIN(IN),NDATE,NAMIN(IN)
 1108 FORMAT(' CLOSING FILE ON UNIT NO.',I3,
     1       ' PROCESSING DATE',I11,
     2       '    FILE = ',A60)
      GO TO 1138
C
 1136 WRITE(KFILDO,1137)KFILIN(IN),IOS
      IF(IP23.NE.0.AND.IP23.NE.KFILDO)
     1             WRITE(IP23,1137)KFILIN(IN),NDATE,IOS,NAMIN
 1137 FORMAT(' ****ERROR CLOSING FILE ON UNIT NO.',I3,
     1       ' PROCESSING DATE',I11,' IN SWITCH AT 1137,',
     2       ' IOSTAT =',I5,/,
     3       '     FILE = ',A60)
      ISTOP=ISTOP+1
 1138 JFOPEN(IN)=0
      LKHERE(IN)=0
C
      IF(IN.EQ.NUMIN)THEN
         WRITE(KFILDO,1139)KFILIN(IN),NDATE,NAMIN(IN)
         IF(IP23.NE.0.AND.IP23.NE.KFILDO)
     1               WRITE(IP23,1139)KFILIN(IN),NDATE,NAMIN(IN)
 1139    FORMAT(' END OF  DATA ON UNIT NO.',I3,
     1          ' PROCESSING DATE',I11,
     2          '    FILE = ',A60)
         JFOPEN(IN)=0
C            THIS IS THE LAST DATA SET.  NOT NECESSARILY AN ERROR.
         GO TO 400
C
      ENDIF
C
      IF(KFILIN(IN).NE.KFILIN(IN+1))THEN
         WRITE(KFILDO,1139)KFILIN(IN),NDATE,NAMIN(IN)
         IF(IP23.NE.0.AND.IP23.NE.KFILDO)
     1          WRITE(IP23,1139)KFILIN(IN),NDATE,NAMIN(IN)
C           NEXT DATA SET DOES NOT HAVE THE SAME UNIT NUMBER.
C           LET "IN" BE INCREMENTED.
         GO TO 400
C
      ENDIF
C
      IF(JFOPEN(IN+1).EQ.2)THEN
C           JFOPEN(IN+1) SHOULD BE 2 WHEN
C           KFILIN(IN) = KFILIN(IN+1)
         JFOPEN(IN+1)=1
         MSDATE(IN+1)=MSDATE(IN)
C           MSDATE( ) VALUE CARRIED OVER BECAUSE IT PERTAINS TO
C           THE SAME DATE, EVEN THOUGH THE FILE WAS SWITCHED.
CINTEL
         CALL CKFILEND(KFILDO,KFILIN(IN+1),NAMIN(IN+1),ISYSEND,
     1                 IFILEND,CONVERTX,IER)
         OPEN(UNIT=KFILIN(IN+1),
     1        FORM='UNFORMATTED',STATUS='OLD',
     2        CONVERT=CONVERTX,IOSTAT=IOS,ERR=1143)
C         OPEN(UNIT=KFILIN(IN+1),
C     1        FORM='UNFORMATTED',STATUS='OLD',
C     2        IOSTAT=IOS,ERR=1143)
CINTEL
         IF(IP23.NE.0)WRITE(IP23,1142)KFILIN(IN+1),NDATE,NAMIN(IN+1)
 1142    FORMAT(' OPENING FILE ON UNIT NO.',I3,
     1          ' PROCESSING DATE',I11,'    FILE = ',A60)
C           LET IN BE INCREMENTED.
         IF(KFILIN(IN).GE.80)IRD=1
C           IRD = 1 INDICATES THAT DIRECTORY RECORD ON A NEW 
C           FILE MUST BE READ.
         GO TO 400
C
 1143    WRITE(KFILDO,1144)KFILIN(IN+1),NDATE,IOS,NAMIN(IN+1)
         IF(IP23.NE.0.AND.IP23.NE.KFILDO)
     1            WRITE(IP23,1144)KFILIN(IN+1),NDATE,IOS,NAMIN(IN+1)
 1144    FORMAT(/,' ****ERROR OPENING FILE ON UNIT NO.',I3,
     1           ' PROCESSING DATE',I11,' IN SWITCH AT 1144,',
     2           ' IOSTAT =',I5,/,
     3           '     FILE = ',A60)
         ISTOP=ISTOP+1
         JFOPEN(IN+1)=0
         LKHERE(IN+1)=0
         IER=31
C
      ELSE
         WRITE(KFILDO,1145)KFILIN(IN+1),NDATE,NAMIN(IN+1)
         IF(IP23.NE.0.AND.IP23.NE.KFILDO)
     1          WRITE(IP23,1145)KFILIN(IN+1),NDATE,NAMIN(IN+1)
 1145    FORMAT(/,' ****TROUBLE IN SWITCHING TO FILE ON UNIT NO.',I3,
     1            ' PROCESSING DATE',I11,' IN SWITCH AT 1145.',/,
     2            '     FILE = ',A60)
         JFOPEN(IN+1)=0
         LKHERE(IN+1)=0
         IER=31
      ENDIF
C
 400  RETURN
      END
