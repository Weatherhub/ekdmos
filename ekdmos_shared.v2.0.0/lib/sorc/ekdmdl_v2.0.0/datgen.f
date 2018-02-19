      SUBROUTINE DATGEN(KFILDO,IP2,IDATE,JDATE,INCCYL,NWORK,LD,ND8,IER)
C 
C        FEBRUARY  1994   GLAHN   TDL   MOS-2000
C        SEPTEMBER 1996   GLAHN   CHANGE TO KEEP LAST DATE WITHIN BOUNDS.
C        AUGUST    1999   GLAHN   CHANGE TO NOT GIVE ERROR WHEN ND8 EXACTLY
C                                 EQUALS THE NUMBER OF DATES;
C                                 REMOVED MTEST( ) 
C        MARCH     2000   DALLAVALLE   MODIFIED FORMAT STATEMENTS TO
C                                 CONFORM TO FORTRAN 90 STANDARDS
C                                 ON THE IBM SP
C        MAY       2000   GLAHN   CHANGED PRINT WHEN LD EXCEEDS ND8
C        MARCH     2005   GLAHN   CHANGED KFILDO TO IP2 ABOVE FORMAT 140
C 
C        PURPOSE  
C            GIVEN A BEGINNING (IDATE) AND ENDING (JDATE) DATE,
C            THIS ROUTINE WILL GENERATE ALL THE DATES BETWEEN
C            THOSE DATES.  THESE DATES, INCLUDING THE BEGINNING 
C            AND ENDING DATE, ARE STORED IN NWORK(J), J=LD,LD+N),
C            WHERE N-1 IS THE NUMBER OF NEW DATES GENERATED.
C
C        DATA SET USE 
C          KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT) 
C           IP2   - UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT) 
C
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       ALL DIAGNOSTICS ARE PUT HERE.  (INPUT) 
C                 IP2 = UNIT NUMBER FOR OUTPUT (PRINT) FILE.  WHEN IP2 NE 0
C                       AND NE KFILDO, DIAGNOSTICS ARE ALSO PUT HERE.  (INPUT) 
C               IDATE = BEGINNING DATE FROM WHICH TO START GENERATING 
C                       ADDITIONAL DATES.  THESE ARE IN THE FORM:  4 DIGITS 
C                       OF THE YEAR TIMES 1000000 PLUS THE NUMBER OF THE 
C                       MONTH TIMES 10000 PLUS THE DAY TIMES 100 PLUS THE
C                       HOUR.  WHEN LD IS GT 1, A CHECK IS MADE TO ASSURE
C                       IDATE IS AFTER NWORK(LD-1).  (INPUT) 
C               JDATE = THE ENDING DATE IN THE SEQUENCE TO GENERATE.
C                       (INPUT)  
C              INCCYL = INCREMENT IN HOURS BETWEEN DATE/TIMES THAT
C                       ARE PUT INTO NWORK( ).  MUST BE GT 0.  (INPUT)
C            NWORK(J) = ARRAY WHERE THE DATES WILL BE PUT, STARTING 
C                       WITH IDATE IN NWORK(LD).  (INPUT-OUTPUT) 
C                  LD = ON ENTRY, LD IS THE LOCATION IN NWORK( ) WHERE
C                       IDATE IS TO GO.  ON EXIT, LD IS THE LOCATION
C                       IN NWORK( ) OF JDATE.  LD INCREASES AS THE DATES
C                       ARE BEING GENERATED.  (INPUT-OUTPUT)
C                 ND8 = DIMENSION OF NWORK( ).  (INPUT)
C                 IER = STATUS RETURN.  (OUTPUT)
C                        0 = GOOD RETURN.
C                       26 = ENDING DATE NOT AFTER BEGINNING DATE OR
C                            INCCYL IS NOT GREATER THAN 0.
C                       27 = VALUE OF LD IS NEGATIVE.
C                       28 = DATE ARRAY TOO SMALL.
C 
C        NONSYSTEM SUBROUINES USED 
C            NONE 
C
      DIMENSION NWORK(ND8)
C
      IER=0
C
C        MAKE SURE ENDING DATE IS AFTER BEGINNING DATE.
C
      IF(IDATE.LT.JDATE)GO TO 145 
      WRITE(KFILDO,140)JDATE,IDATE 
      IF(IP2.NE.0.AND.IP2.NE.KFILDO)WRITE(IP2,140)JDATE,IDATE 
 140  FORMAT(/,' ****ENDING DATE =',I11,' NOT AFTER BEGINNING DATE =',
     1       I11,'.  RETURN FROM DATGEN AT 140.') 
      IER=26
      GO TO 200
C
C        MAKE SURE LD IS POSITIVE.
C
 145  IF(LD.GT.0.AND.LD.LE.ND8)GO TO 150 
      WRITE(KFILDO,146)LD 
      IF(IP2.NE.0.AND.IP2.NE.KFILDO)WRITE(IP2,146)LD 
 146  FORMAT(/,' ****INPUT VALUE OF LD =',I10,' IS LE ZERO OR GT.ND8.',
     1         '  RETURN FROM DATGEN AT 146.')
      IER=27
      GO TO 200
C
C        MAKE SURE INCCYL IS GREATER THAN ZERO.
C
 150  IF(INCCYL.GT.0)GO TO 155
      WRITE(KFILDO,151)INCCYL 
      IF(IP2.NE.0.AND.IP2.NE.KFILDO)WRITE(IP2,151)INCCYL 
 151  FORMAT(/,' ****INPUT VALUE OF INCCYL =',I6,' IS LE ZERO.',
     1         '  RETURN FROM DATGEN AT 151.')
      IER=26
      GO TO 200
C
C        INSERT DATES INTO NWORK( ).
C
 155  NWORK(LD)=IDATE
 156  CALL UPDAT(NWORK(LD),INCCYL,JTEMP)
      IF(JTEMP.GT.JDATE)GO TO 200
      LD=LD+1
      IF(LD.GT.ND8)GO TO 167
      NWORK(LD)=JTEMP
      GO TO 156
C
 167  WRITE(KFILDO,170)ND8,NWORK(LD-1)
      IF(IP2.NE.0.AND.IP2.NE.KFILDO)WRITE(IP2,170)ND8,NWORK(LD-1)
 170  FORMAT(/' ****SIZE',I4,' OF DATE ARRAY TOO SMALL, LAST DATE', 
     1         ' STORED WAS',I12,'.  RETURN FROM DATGEN AT 170.') 
      LD=ND8
      IER=28
C
 200  RETURN 
      END
