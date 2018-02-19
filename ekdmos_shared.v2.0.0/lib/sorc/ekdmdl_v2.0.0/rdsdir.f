      SUBROUTINE RDSDIR(KFILDO,KFILD,NEW,CCALL1,CCALL2,CCALL3,
     1                  CCALL4,CCALL5,CCALL6,
     2                  NAME,NBLOCK,NELEV,STALAT,STALON,ITIMEZ,
     3                  ITYPE,OPEN,IDATE,IWBAN,IRES,COMENT,IER)
C 
C        NOVEMBER 1997   GLAHN   TDL   MOS-2000
C        NOVEMBER 1997   GLAHN   VARIABLE OPEN INITIALIZED
C        APRIL    2000   DALLAVALLE   MODIFIED FORMAT STATEMENTS TO
C                                     CONFORM TO FORTRAN 90 STANDARDS
C                                     ON THE IBM SP
C
C        PURPOSE 
C            TO READ A RECORD FROM THE STATION DIRECTORY.  ALL
C            ELEMENTS IN THE DIRECTORY ARE RETURNED FOR THE NEXT
C            STATION IN THE LIST.  THE DIRECTORY WILL BE IN
C            ALPHABETICAL ORDER BY THE ICAO CALL LETTERS, SO THE
C            STATIONS RETURNED WILL BE IN THAT ORDER.  THERE ARE
C            UP TO 6 STATION IDENTIFIERS IN EACH RECORD--THE ICAO 
C            IDENTIFIER, THE OLD CALL LETTERS IN USE BEFORE
C            THE ICAO IDENTIFIER CAME INTO VOGUE, AND UP TO 4
C            ADDITIONAL CALL LETTERS BY WHICH THE STATION WAS
C            KNOWN PREVIOUSLY.  ALL 6 WILL BE RETURNED, BUT SOME
C            MAY BE BLANK.  THE FIRST ONE RETURNED WILL BE THE ICAO
C            WHEN NEW = 1, AND THE SECOND ONE IN THE DIRECTORY
C            LIST WHEN NEW = 0.
C    
C
C        DATA SET USE 
C            KFILDO   - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (OUTPUT) 
C            KFILD    - UNIT NUMBER FROM WHICH TO READ THE
C                       STATION DIRECTORY.  IT IS ASSUMED THE FILE
C                       HAS BEEN OPENED.  (INPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT) 
C               KFILD = UNIT NUMBER FROM WHICH TO READ THE 
C                       STATION DIRECTORY.  IT IS ASSUMED THE FILE
C                       HAS BEEN OPENED.  (INPUT) 
C                 NEW = 1 WHEN NEW ICAO CALL LETTERS ARE TO BE RETURNED
C                       IN CCALL1 AND OLD CALL LETTERS IN CCALL2;
C                       0 WHEN OLD CALL LETTERS ARE TO BE RETURNED IN
C                       CCALL1 AND NEW ICAO CALL LETTERS IN CCALL2.
C                       (INPUT)
C                NAME = NAMES OF STATION. (CHARACTER*20)  (OUTPUT)
C              NBLOCK = WMO BLOCK/STATION NUMBER.  (OUTPUT)
C               NELEV = ELEVATION OF STATION.  (OUTPUT)
C              STALAT = LATITUDE OF STATION.  (OUTPUT)
C              STALON = LONGITUDE OF STATION.  (OUTPUT)
C              ITIMEZ = THE NUMBER OF HOURS THE STATION IS AWAY
C                       FROM UTC.  (OUTPUT)
C               ITYPE = TYPE OF STATION.  (OUTPUT)
C                OPEN = INDICATES WHETHER OR NOT ANOTHER STATION
C                       IS PRESENT IN THE DICTIONARY WITH A 
C                       DIFFERENT LOCATION, BUT WITH THE SAME CALL
C                       LETTERS.  A BLANK INDICATES THE CALL LETTERS
C                       ARE CURRENT AND AN "O" INDICATES THE STATION
C                       HAS CLOSED AND THE CALL LETTERS HAVE BEEN
C                       REUSED FOR ANOTHER STATION.  (OUTPUT)
C               IDATE = A DATE INDICATING THE DATE THAT DATA WERE
C                       FIRST RECEIVED FROM THIS STATION, WHEN KNOWN.
C                       IT MAY ALSO INDICATE WHEN THE LOCATION
C                       LAT/LONG WERE CORRECTED.  (OUTPUT)
C               IWBAN = WBAN NUMBER OF STATION.  (OUTPUT) 
C                IRES = RESERVED.  (COLUMNS 136-145 IN THE DIRECTORY.)
C                       (OUTPUT)
C              COMENT = COMMENTS.  (COLUMNS 147-206)  (CHARACTER*60)
C                       (OUTPUT)
C                 IER = STATUS RETURN.  (OUTPUT)
C                         0 = GOOD RETURN.
C                        33 = ERROR ON UNIT KFILD WHEN READING THE
C                             DIRECTORY.
C                       137 = END OF FILE FOUND ON UNIT KFILD WHEN 
C                             READING THE DIRECTORY.
C           CCALLT(J) = TO READ CALL LETTERS INTO FROM DIRECTORY
C                       (J=1,2).  (CHARACTER*8)  (INTERNAL)
C              SIGNLA = SIGN OF THE LATITUDE AS READ FROM THE DIRECTORY.
C                       WILL BE "N" FOR NORTH LATITUDE OR "S" FOR SOUTH
C                       LATITUDE.  WHEN "S", THE LATITUDE WILL BE STORED
C                       AS NEGATIVE.  (CHARACTER*1)  (INTERNAL)
C              XLATDD = LATITUDE IN DEGREES.  (INTERNAL)
C              SIGNLO = SIGN OF THE LONGITUDE AS READ FROM THE 
C                       DIRECTORY.  WILL BE "E" FOR EAST LONGITUDE
C                       OR "W" FOR WEST LONGITUDE.  WHEN "E", THE 
C                       LONGITUDE IS ADJUSTED SO THAT ALL VALUES ARE
C                       WEST.  (CHARACTER*1)  (INTERNAL)
C              XLONDD = LONGITUDE IN DEGREES.  (INTERNAL)
C               BLANK = 60 BLANKS.  (CHARACTER*8)  (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            NONE. 
C
      CHARACTER*1 SIGNLA,SIGNLO,OPEN
      CHARACTER*8 CCALL1,CCALL2,CCALL3,CCALL4,CCALL5,CCALL6,CCALLT(2)
      CHARACTER*20 NAME
      CHARACTER*60 COMENT,BLANK
C
      DATA BLANK/' '/
C
      IER=0
      COMENT=BLANK
      NAME=BLANK(1:20)
      OPEN=BLANK(1:1)
C        THE ABOVE STATEMENTS ARE TO PRECLUDE NULLS BEING RETURNED.
C        THE COMMENT FIELD MAY BE SHORT, AND NAME(18) IS NOT READ.
C        SOME DIRECTORY RECORDS MAY END WITH THE LAT/LONG FIELDS;
C        THIS IS FOR SAFETY.  THE READ BELOW WORKS EVEN THOUGH
C        SOME FIELDS ARE NOT PRESENT OR ARE LEFT BLANK.
C    
      READ(KFILD,122,IOSTAT=IOS,ERR=123,END=125)CCALLT(1),
     1     CCALLT(2),NAME(1:17),NAME(19:20),NBLOCK,
     2     NELEV,SIGNLA,XLATDD,SIGNLO,XLONDD,ITIMEZ,ITYPE,
     3     OPEN,CCALL3,CCALL4,CCALL5,CCALL6,IDATE,IWBAN,IRES,COMENT
 122  FORMAT(A8,1X,A8,1X,A17,4X,A2,1X,I6,1X,I5,1X,A1,F7.4,1X,A1,F8.4,
     1       1X,I3,1X,I1,1X,A1,4(1X,A8),1X,I10,1X,I5,1X,I10,1X,A60)
      GO TO 130
C
C        THIS SECTION FOR EOF OR ERROR READING.  SET ALL
C        VALUES TO ZERO OR BLANK.
C
 123  IER=33
      WRITE(KFILDO,124)KFILD,IOS
 124  FORMAT(/,'****ERROR READING STATION DIRECTORY ON UNIT',I3,
     1         '.  IOSTAT =',I5)
      GO TO 126
 125  IER=137
 126  CCALL1=BLANK(1:8)
      CCALL2=BLANK(1:8)
      CCALL3=BLANK(1:8)
      CCALL4=BLANK(1:8)
      CCALL5=BLANK(1:8)
      CCALL6=BLANK(1:8)
      NAME=BLANK(1:20)
      NBLOCK=0
      NELEV=0
      STALAT=0.
      STALON=0.
      ITIMEZ=0
      ITYPE=0
      OPEN=BLANK(1:1)
      IDATE=0
      IWBAN=0
      IRES=0
      COMENT=BLANK
      GO TO 160
C
C        GOOD READ.
C
 130  IF(NEW.EQ.1)THEN
         CCALL1=CCALLT(1)
         CCALL2=CCALLT(2)
      ELSE
         CCALL1=CCALLT(2)
         CCALL2=CCALLT(1)
      ENDIF
C
      STALAT=XLATDD
      IF(SIGNLA.EQ.'S')STALAT=-STALAT
C        ABOVE STATEMENT MAKES SOUTH LATITUDE NEGATIVE.
      STALON=XLONDD
      IF(SIGNLO.EQ.'E')STALON=360.-STALON     
C        ABOVE STATEMENT MAKES ALL LONGITUDES WEST, RANGE 0-360.
      
 160  RETURN
      END
