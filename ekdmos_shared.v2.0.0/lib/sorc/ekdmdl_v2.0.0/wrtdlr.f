      SUBROUTINE WRTDLR(KFILDO,KFILX,CFILX,ID,ICALL,CCALL,ND1,NSTA,
     1                  ICALLD,CCALLD,ND5,IPACK,NSIZE,
     2                  NREPLA,NCHECK,L3264B,L3264W,IER) 
C 
C        MARCH    1999   GLAHN   MOS-2000
C        JULY     1999   GLAHN   GO TO 220, NOT 200 AT IFIRST.NE.0
C                                REMOVED DIAGNOSTIC AFTER WRITING
C        APRIL    2000   DALLAVALLE   MODIFIED FORMAT STATEMENTS TO
C                                     CONFORM TO FORTRAN 90 STANDARDS
C                                     ON THE IBM SP
C        SEPTEMBER 2012   ENGLE   ADDED CALL TO WRTDLMC TO WRITE STATION
C                                 CALL LETTERS TO RANDOM ACCESS FILE.
C 
C        PURPOSE 
C            TO WRITE A TDLPACK RECORD TO A MOS-2000 RANDOM ACCESS FILE.
C            THE STATION DIRECTORY IS CHECKED ON THE FIRST ENTRY.
C 
C        DATA SET USE. 
C            KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT) 
C            KFILX  - UNIT NUMBER FOR TDL FILE.  (INPUT) 
C 
C        VARIABLES 
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (INPUT) 
C               KFILX = UNIT NUMBER FOR TDL FILE TO WRITE.  (INPUT) 
C               CFILE = THE NAME OF THE FILE TO WRITE.  (CHARACTER*60)
C                       (INPUT)
C               ID(J) = THE 4 MOS IDS OF THE RECORD TO WRITE (J=1,4).
C                       (INPUT)
C          ICALL(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN 
C                       INTEGER VARIABLE (L=1,L3264W) (K=1,NSTA).
C                       NOTE THAT THIS REQUIRES TWO 32-BIT WORDS TO HOLD
C                       THE DESCRIPTION BUT ONLY ONE 64-BIT WORD.
C                       EQUIVALENCED TO CCALL( ).  (INPUT)
C            CCALL(K) = 8-CHARACTAER STATION CALL LETTERS (K=1,NSTA).
C                       EQUIVALENCED TO ICALL( ).  (INPUT)
C                 ND1 = THE NUMBER OF STATIONS IN ICALL( , ) AND 
C                       CCALL( ).  (INPUT)
C                NSTA = THE NUMBER OF STATIONS TO BE WRITTEN TO
C                       THE RANDOM ACCESS FILE.  (INPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN
C                       INTEGER VARIABLE (L=1,L3264W) (K=1,ND5).
C                       EQUIVALENCED TO CCALLD( ).  (INTERNAL)
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).
C                       EQUIVALENCED TO ICALLD( , ).  (INTERNAL)
C                 ND5 = SIZE OF ICALLD( , ) AND CCALLD( ).  MUST BE
C                       LARGE ENOUGH TO HOLD DIRECTORY RECORD
C                       TO READ AND CHECK.  (INPUT)
C            IPACK(J) = THE DATA TO WRITE (J=1,NSIZE).  (INPUT)
C               NSIZE = THE NUMBER OF WORDS OF DATA IN IPACK( ).
C                       (INPUT)
C              NREPLA = RECORD REPLACEMENT FLAG. 
C                       0 = NOT REPLACING RECORD. 
C                       1 = REPLACING, ERROR IF RECORD NOT FOUND TO 
C                           REPLACE. 
C                       2 = REPLACING, WRITE NEW RECORD IF RECORD NOT 
C                           FOUND TO REPLACE.
C                      (INPUT)
C              NCHECK = IDENTIFICATION CHECKING FLAG. 
C                       0 = DON'T CHECK FOR DUPLICATES.
C                       1 = CHECK FOR DUPLICATES, ERROR IF FOUND.
C                       (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).  
C                       (INPUT)
C                 IER = STATUS CODE.
C                       0 = GOOD RETURN.
C                       175 = ERROR READING STATION DIRECTORY.
C                       176 = NUMBER OF STATIONS OR DIRECTORY READ
C                             FROM THE RANDOM ACCESS FILE DOES NOT
C                             MATCH NSTA OR CCALL( ).
C                       OTHER VALUES FROM CALLED ROUTINES.
C                       (OUTPUT)
C               JD(J) = MOS IDS OF STATION IDS (J=1,4).  (INTERNAL)
C              NWRITE = 0 IF AN ERROR OCCURRS INVOLVING THE DIRECTORY
C                       RECORD; DATA ARE NOT WRITTEN.
C                       1 OTHERWISE.
C                       THIS IS CARRIED FROM ENTRY TO ENTRY; ONCE AN
C                       ERROR HAS OCCURRED, NO MORE WRITING IS DONE.
C                       (INTERNAL)
C 
C        NON SYSTEM SUBROUTINES CALLED 
C           RDTDLM, WRTDLM
C
C
      CHARACTER*8 CCALL(ND1),CCALLD(ND5)
      CHARACTER*60 CFILX
C
      DIMENSION ICALL(L3264W,ND1),ICALLD(L3264W,ND5)
      DIMENSION ID(4),JD(4)
C
      DATA IFIRST/0/
      DATA NWRITE/1/
C
      IER=0
      IF(NWRITE.EQ.0)GO TO 300
      IF(IFIRST.NE.0)GO TO 220
      IFIRST=1
C      
C        READ CALL LETTERS IF THEY EXIST AND CHECK THEM
C        TO MAKE ADDITION OF RECORDS POSSIBLE.
C
      JD(1)=400001000
      JD(2)=0
      JD(3)=0
      JD(4)=0
CINTEL
C      CALL RDTDLM(KFILDO,KFILX,CFILX,JD,ICALLD,ND5*L3264W,NSTA1,
C     1            L3264B,IER)
      CALL RDTDLMC(KFILDO,KFILX,CFILX,JD,CCALLD,ND5*L3264W,NSTA1,
     1            L3264B,IER)
CINTEL
C
      IF(IER.EQ.155)THEN
C           THE DIRECTORY DID NOT EXIST.  THIS IS NOT AN ERROR.
         WRITE(KFILDO,111)CFILX
 111     FORMAT('     THE DIRECTOY DOES NOT EXIST ON FILE ',A60,/,
     1          '     SO WRITE THE CALL LETTERS.')
         GO TO 200
C
      ELSEIF(IER.NE.0)THEN
         WRITE(KFILDO,112)IER
 112     FORMAT('     ERROR READING STATION DIRECTORY',
     1          ' IN RANDOM ACCESS FILE IN WRTDLR AT 112.  IER =',I4)
         IER=175
         GO TO 300
      ENDIF
C
C        GOOD READ.
C
      NSTA1=NSTA1/L3264W
C        THE CALL LETTERS ARE 8 BYTES EACH.  THIS IS TWO WORDS
C        ON A 32-BIT MACHINE.  THE NUMBER OF WORDS WRITTEN AND
C        READ MUST ACCOUNT FOR THIS.  THE ACTUAL NUMBER OF CALL
C        LETTERS IS NSTA1/L3264W.
C
C        CALL LETTERS WERE READ.  DO THEY MATCH?
C
      IF(NSTA1.EQ.NSTA)GO TO 125
      WRITE(KFILDO,120)NSTA1,NSTA,
     1     (CCALL(J),CCALLD(J),J=1,MAX(NSTA1,NSTA))
 120  FORMAT(/,' ****NUMBER OF CALL LETTERS READ FROM',
     1         ' RANDOM ACCESS OUTPUT FILE =',I5,/,
     2         '     DOES NOT EQUAL THE NUMBER TO BE WRITTEN =',I5,
     3         '.  DATA NOT WRITTEN.',/,('     ',A8,1X,A8))
C        VALUES BEYOND NSTA1 IN CCALL( ) WILL NOT BE
C        CHARACTER ORIENTED, AND PROBABLY NOT PRINTABLE AS A8.
      IER=176
      NWRITE=0
      GO TO 300
C
 125  MATCH=0
C
      DO 130 J=1,NSTA
      IF(CCALL(J).EQ.CCALLD(J))GO TO 130
      WRITE(KFILDO,126)CCALL(J),CCALLD(J)
 126  FORMAT(/,' ****MISMATCH OF CALL LETTERS TO BE WRITTEN',
     1         ' AND THOSE ON RANDOM ACCESS FILE.',2(2X,A8))
      MATCH=1
 130  CONTINUE
C
      IF(MATCH.EQ.0)GO TO 220
      WRITE(KFILDO,134)(CCALL(J),CCALLD(J),J=1,NSTA)
 134  FORMAT(/,' DATA NOT WRITTEN.  STATIONS TO WRITE',
     1         ' AND THOSE IN DIRECTORY',/,(' ',A8,2X,A8))
      IER=176
      NWRITE=0
      GO TO 300
C  
C        WRITE CALL LETTERS RECORD WHEN SUCH A RECORD DOES
C        NOT EXIST.
C
CINTEL
C 200  CALL WRTDLM(KFILDO,KFILX,CFILX,JD,ICALL,NSTA*L3264W,
C     1               0,0,L3264B,IER)
 200  CALL WRTDLMC(KFILDO,KFILX,CFILX,JD,CCALL,NSTA*L3264W,
     1               0,0,L3264B,IER)
CINTEL
C        THE CALL LETTERS ARE 8 BYTES EACH.  THIS IS TWO WORDS
C        ON A 32-BIT MACHINE.  THE NUMBER OF WORDS WRITTEN AND
C        READ MUST ACCOUNT FOR THIS.
C
      IF(IER.NE.0)THEN
         WRITE(KFILDO,205)IER
 205     FORMAT(/,' ****ERROR WRITING STATION DIRECTORY',
     1            ' ON RANDOM ACCESS FILE IN WRTDLR AT 205.  IER =',
     2           I4,/,'     NO DATA WILL BE WRITTEN ON RANDOM',
     3            ' ACCESS FILE.')
         NWRITE=0
         GO TO 300
      ENDIF
C
C        WRITE DATA RECORD.
C
 220  CALL WRTDLM(KFILDO,KFILX,CFILX,ID,IPACK,NSIZE,
     1            NREPLA,NCHECK,L3264B,IER)
C        ANY PROBLEM WRITING WILL HAVE PRODUCED A DIAGNOSTIC.
      WRITE(KFILDO,255)(ID(J),J=1,4),KFILX,CFILX
 255  FORMAT(' DATA ',4I11,' WRITTEN TO RANDOM ACCESS FILE,',/,
     1       '              UNIT =',I3,' FILE =',A60)
C
 300  RETURN
      END
           
