      SUBROUTINE READ_MOSDA(KFILDO,KFILX,IP12,
     1                      ID,NDATE,CCALL,NSTA,
     2                      SDATA,ND1,ND5,ND7,IS0,IS1,
     3                      IS2,IS4,INDEX,IER)
C 
C        NOVEMBER  1996   GLAHN   TDL   MOS-2000 
C        JUNE      1997   GLAHN   SLIGHT CHANGE TO COMMENTS, NOT CODE
C                                 D COMPILE OPTION REMOVED OR COMMENTED OUT
C        JANUARY   1998   GLAHN   ADDED SUBSTITUTE STATIONS IN CCALL( , );
C                                 MODIFIED DIAGNOSTICS
C        MAY       1998   GLAHN   ADDED ITIMEZ( ) 
C        SEPTEMBER 1998   GLAHN   MODIFIED PRINT WHEN NUMRA = 0 AT 111
C        OCTOBER   1998   GLAHN   REDUCED CALL SEQUENCE AND CALLED FINDST
C        NOVEMBER  1998   GLAHN   CHANGED DIMENSIONS OF CCALLD( )
C                                 AND ICALLD( , ) FROM ND1 TO ND5;
C                                 ELIMINATED SDATA1(ND1)
C        NOVEMBER  1998   GLAHN   OMITTED PRINT TO IP16 EXCEPT IN FINDST
C        NOVEMBER  1998   GLAHN   ADDED IER RETURN FROM FINDST
C        DECEMBER  1998   GLAHN   NCOMBO CHANGED FROM 9999 TO 999;
C                                 CHECK MSTA = NVALUE REMOVED
C        JULY      1999   GLAHN   ADDED IP12; DELETED IP16;
C                                 HARDWIRED UNIT NUMBERS TO CCC
C        AUGUST    1999   GLAHN   CHANGED IP16 IN CALL TO FINDST TO IP12
C        AUGUST    1999   GLAHN   ELIMINATED CALL TO COMPID FOR CCC
C                                 NE 4XX
C        MARCH     2000   DALLAVALLE   MODIFIED FORMAT STATEMENTS TO
C                                 CONFORM TO FORTRAN 90 STANDARDS 
C                                 ON IBM SP
C        MAY       2000   GLAHN   CORRECTED COMMENT FOR JD( ); SPELLING.
C        MAY       2000   DALLAVALLE/DREWRY   ADAPTED CONST SO AS TO PROVIDE
C                                             A SUBROUTINE TO READ DATA
C                                             LOCATED IN THE EXTERNAL MOS
C                                             RANDOM ACCESS FILE; REMOVED
C                                             MUCH OF THE LOGIC IN CONST. 
C        MAR       2001   MCE     MODIFIED TO ADD UNIT NUMBERS FOR RA FILES
C                                 W/O ID RESTRICTIONS.  FOR ENS USE, AMONG
C                                 OTHER POSSIBILITIES.  THIS CHANGE REQUIRES
C                                 INDEX TO BE DIMENSIONED ND1,15  
C        MAR       2001   RLA     MODIFIED SO THAT IT WOULD PASS BACK THE
C                                 PACKING INFORMATION IN IS0,IS1,IS2, AND
C                                 IS4.  THIS IS FOR USE IN THE GRIB-WRITING
C                                 CODE.
C        MAR       2001   JCM     EDITED DATE CHECK TO CHECK ALL IDS BETWEEN
C                                 200 AND 299 -- NOT JUST UNITS 48 AND 49.
C        MAY       2001   JCM     CORRECTED WRITE AT 250, CHANGED IS1 TO IS1(8)
C        MARCH     2005   JPD     MODIFIED IF TESTS SO THAT MOS FORECAST
C                                 IDS COULD BE READ ON INPUT UNIT 46,
C                                 AS WELL AS UNITS 48 AND 49.
C        OCTOBER   2012   ENGLE   CHANGED CALL FROM RDTDLM TO RDTDLMC WHEN
C                                 READING STATION CALL LETTERS.
C
C        PURPOSE 
C            TO PROVIDE CONSTANTS, FORECASTS,
C            AND OTHER DATA THAT ARE AVAILABLE IN MOS-2000 EXTERNAL
C            RANDOM ACCESS FILES.  THE CCC IN THE MOS-2000 ID DETERMINES 
C            THE UNIT NUMBER:
C
C               CCC RANGE      KFILX      USE
C                400-499        45       INPUT
C                200-299        46       INPUT
C                800-899        47       INPUT
C                200-299        48       INPUT
C                200-299        49       OUTPUT/INPUT
C
C            THE FOLLOWING 4XX CONSTANTS ARE ACCOMMODATED IN 
C            COMPID:
C               40XXXX = TRULY CONSTANT VALUES; DO NOT DEPEND ON TIME
C               41XXXX = DAILY VALUES; NO INTERPOLATION
C               422XXX = 5-DAY VALUES (JAN 5, JAN 10, ... 
C                        DEC 31 = 365TH DAY); INTERPOLATION ASSUMED
C               43XXXX = MONTHLY VALUES; INTERPOLATION ASSUMED
C               44XXXX = 6-MONTH SEASONS (APRIL-SEPTEMBER, ETC);
C                        NO INTERPOLATION
C               45XXXX = 3-MONTH SEASONS (MARCH-MAY, JUNE-AUGUST, ETC.);
C                        NO INTERPOLATION
C               46XXXX = YEARLY VALUES
C               47XXXX = MONTHLY VALUES; NO INTERPOLATION
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT) 
C            KFILX  - UNIT NUMBER OF RANDOM ACCESS FILE TO BE READ; MUST
C                     BE A VALUE BETWEEN 45,...,49. (INPUT)
C            IP12   - INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                     STATIONS ON THE INPUT FILES WILL BE PRINTED TO 
C                     THE FILE WHOSE UNIT NUMBER IS IP12.
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT) 
C               KFILX = UNIT NUMBER OF RANDOM ACCESS FILE TO BE READ; MUST
C                       BE A VALUE BETWEEN 45 AND 49; WHILE THIS CAN BE
C                       CHANGED EVENTUALLY, FOR THIS PARTICULAR IMPLEMENTATION
C                       IN MAY 2000, THIS CONVENTION WAS FOLLOWED.  (INPUT)
C                IP12 = INDICATES WHETHER (>0) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE EXTERNAL RANDOM ACCESS FILES
C                       WILL BE LISTED TO UNIT IP12.  (INPUT)
C               ID(J) = THE VARIABLE ID (J=1,4).  (INPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,4).
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,21+).
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).
C               NDATE = THE DATE/TIME FOR WHICH VARIABLE IS NEEDED.  (INPUT)
C          CCALL(K,J) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (J=1) AND 5 POSSIBLE OTHER STATION
C                       CALL LETTERS (J=2,6) THAT CAN BE USED INSTEAD
C                       IF THE PRIMARY (J=1) STATION CANNOT BE FOUND 
C                       IN AN INPUT DIRECTORY (K=1,NSTA).  ALL STATION
C                       DATA ARE KEYED TO THIS LIST (CHARACTER*8).
C                       (INPUT)                                
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT WITH.
C                       (INPUT)
C            SDATA(K) = VARIABLE DATA RETURNED (K=1,NSTA).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH.
C                       DIMENSION OF SEVERAL VARIABLES.  (INPUT)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), DATA( ) AND
C                       CCALLD( ).  MUST BE AT LEAST AS BIG AS ND1.  (INPUT)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  
C                       SHOULD BE SET TO 54 FOR CONSISTENCY WITH OTHER
C                       MOS-2000 SOFTWARE.  (INPUT)
C          INDEX(K,L) = ARRAY CONTAINING LOCATION OF STATION K IN FILE
C                       DIRECTORY L, WHERE K=1,NSTA AND L=1,15.  
C                       (INPUT/OUTPUT) 
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                         1 = UNABLE TO FIND A STATION OR STATIONS IN
C                             THE DIRECTORY OF THE RANDOM ACCESS FILE;
C                             ORDINARILY, THIS SHOULD NOT BE A FATAL 
C                             ERROR.  THIS ERROR ONLY IS SET WHEN
C                             FINDST RETURNS A AN ERROR CODE OF 160.
C                         2 = UNABLE TO READ A DATA RECORD FOR A SPECIFIC
C                             DATE; THIS ERROR WILL OCCUR IF THE NCEP
C                             DATE DOES NOT MATCH THE DATE OF THE TDLPACK
C                             RECORD OR IF THE TDLPACK RECORD CAN NOT BE
C                             UNPACKED.  IN THIS CASE, THE DATA IN THE
C                             SDATA ARRAY WILL BE RETURNED AS 9999.
C                         3 = CATASTROPHIC ERROR; EITHER THE UNIT NUMBER
C                             WAS INCORRECT OR THE STATION DIRECTORY COULD
C                             NOT BE READ FROM THE RANDOM ACCESS FILE.
C                       (INTERNAL-OUTPUT)
C
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).  THIS ARRAY IS USED 
C                       TO READ THE STATION DIRECTORY.   
C                       (CHARACTER*8)  (INTERNAL)
C             DATA(J) = ARRAY TO HOLD RETURNED DATA WHEN A SECOND CALL TO THE
C                       RANDOM ACCESS FILE IS REQUIRED.  THIS IS ONLY DONE
C                       WHEN THE CONSTANT FILE IS READ, AND A MONTHLY REL. 
C                       FREQ OR MX/MN TEMPERATURE NORMAL MUST BE INTERPOLATED 
C                       TO THE DAY OF THE MONTH. (J=1,ND5).  (INTERNAL)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID CORRESPONDING TO ID( ) (J=1,15).  (INTERNAL)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C              IERSAV = IER FROM FINDST.  (INTERNAL)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C           ISDATA(K) = WORK ARRAY (K=1,ND1).  (INPUT-OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).  (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).  (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C               KD(J) = IDS FOR DIRECTORY RECORD IN THE MOS-2000 INTERNAL
C                       STORAGE SYSTEM (STATION CALL LETTERS) AND IDS
C                       FOR THE DATA (J=1,4).  (INTERNAL)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING USED
C                       (EITHER 32 OR 64).  (INTERNAL)
C              L3264W = NUMBER OF WORDS IN 64 BITS, EITHER 1 OR 2.  (INTERNAL)
C               LD(J) = IDS FOR CALL LETTERS RECORD IN MOS-2000
C                       EXTERNAL RANDOM ACCESS FILES (J=1,4).
C                       (INTERNAL)
C               NUMRA = THE MAXIMUM NUMBER OF RANDOM ACCESS FILES THAT CAN BE ACCESSED; 
C                       CURRENTLY SET TO 5. (INTERNAL)
C
C        NONSYSTEM SUBROUTINES USED 
C            RDTDLM, UNPACK, FINDST
C
      PARAMETER (L3264B=32)
      PARAMETER (L3264W=64/L3264B)
C
      CHARACTER*8 CCALL(ND1,6),
     1            CCALLD(ND5)
      CHARACTER*60 RANAME
C
      DIMENSION ISDATA(ND1),SDATA(ND1)
      DIMENSION ID(4),IDPARS(15),KD(4),LD(4)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION KFILRA(15),NFIRST(15),INDEX(ND1,15)
C
      DATA NFIRST/15*0/,NUMRA/5/,RANAME/' '/
C
      SAVE NFIRST,NUMRA 
C  
      IER=0
      IERSAV=0
      JF = 0
C
C        FIND THE FILE UNIT NUMBER AND NAME BY ASSOCIATION
C        WITH CCC.  THESE VALUES ARE HARDWIRED.
C
      CALL PRSID1(KFILDO,ID,IDPARS) 
      IF(KFILX.EQ.45.AND.
     1   IDPARS(1).GE.400.AND.
     2   IDPARS(1).LE.499) JF = 1
      IF(KFILX.EQ.46.AND.
     1   IDPARS(1).GE.200.AND.
     2   IDPARS(1).LE.299) JF = 2
      IF(KFILX.EQ.47.AND.
     1   IDPARS(1).GE.800.AND.
     2   IDPARS(1).LE.899) JF = 3
      IF(KFILX.EQ.48.AND.
     1   IDPARS(1).GE.200.AND.
     2   IDPARS(1).LE.299) JF = 4
      IF(KFILX.EQ.49.AND.
     1   IDPARS(1).GE.200.AND.
     2   IDPARS(1).LE.299) JF = 5
      IF((KFILX.GE.35).AND.(KFILX.LE.44)) JF=KFILX-29
C
      IF (JF .NE. 0) GOTO 120
C        NO UNIT NUMBER MATCHES WITH CCC IN ID.
C
      IER=3
      WRITE(KFILDO,111)IDPARS(1),(ID(J),J=1,4),KFILX,IER
 111  FORMAT(/,' ****COULD NOT MATCH THE RANDOM ACCESS FILE',
     1         ' UNIT NUMBER IN READ_MOSDA WITH CCC =',I5,
     2         '  IN THE ID.',/,
     3         '     LOOKING FOR ',3(1X,I9.9),1X,I10.3,/,
     4         '     THE UNIT NUMBER IS ',I4,
     5         '  ERROR CODE IN READ_MOSDA = ',I2)
C
      GO TO 290
C
C        RETRIEVE THE DIRECTORY RECORD.
C        THIS IS THE CORRESPONDENCE BETWEEN THE NSTA STATIONS IN
C        CCALL( , ) AND THE STATIONS IN THE MOS-2000 RANDOM ACCESS
C        FILE. ALL VARIABLES ON THE FILE WILL, OF COURSE, HAVE  
C        THE SAME STATION INDEX AND THE CALL LETTERS DIRECTORY
C        WILL HAVE THE SAME IDS REGARDLESS OF THE FILE.
C
 120  IF (NFIRST(JF) .EQ. 0) THEN
         LD(1)=400001000
         LD(2)=0
         LD(3)=0
         LD(4)=0
CINTEL
C         CALL RDTDLM(KFILDO,KFILX,RANAME,LD,CCALLD,ND5,
C     1               NVALUE,L3264B,IER)
         CALL RDTDLMC(KFILDO,KFILX,RANAME,LD,CCALLD,ND5,
     1               NVALUE,L3264B,IER)
CINTEL
         IF(IER.NE.0)THEN
           IER=3
           GO TO 290
         ENDIF
C
         MSTA=NVALUE/L3264W
	 NFIRST(JF) = MSTA
C
C           THE CALL LETTERS ARE 8 BYTES EACH.  THIS IS TWO WORDS
C           ON A 32-BIT MACHINE.  THE NUMBER OF WORDS WRITTEN AND
C           READ MUST ACCOUNT FOR THIS.  THE ACTUAL NUMBER OF CALL
C           LETTERS MSTA AS RETURNED FROM RDTDLM IS NVALUE/L3264W.
C
C           FIND THE LOCATION OF THE STATIONS IN CCALL( ) IN THE ARRAY
C           CCALLD( ), WHICH IS EQUIVALENCED TO ICALLD( , ).
C
         CALL FINDST(KFILDO,IP12,RANAME,CCALL,ND1,NSTA,
     1               CCALLD,MSTA,ISDATA,IER)
C              THE ONLY ERROR FROM FINDST IS IER = 120, WHICH 
C              MEANS ONE OR MORE STATIONS COULDN'T BE FOUND.
C              THIS IS NOT SUFFICIENT REASON TO STOP.  A DIAGNOSTIC
C              WILL HAVE BEEN PRINTED BY FINDST.  IF A STATION
C              CAN'T BE FOUND, ITS INDEX IN ISDATA( ) WILL BE
C              99999999.  SAVE IER FOR POSSIBLE RETURN.
         IF(IER.EQ.120)IERSAV=1
	 DO 150 K=1,NSTA
	    INDEX(K,JF) = ISDATA(K)
  150    CONTINUE
      ENDIF
C
      IF(KFILX.EQ.45)THEN
         CALL COMPID(KFILDO,ID,IDPARS,NDATE,KD,LD,MDOY,R,IER)
      ELSE
         KD(1)=ID(1)
         KD(2)=ID(2)
         KD(3)=ID(3)
         KD(4)=ID(4)
      ENDIF
C
      IF(IER.NE.0)THEN     
        IER=2
        GO TO 290
      ENDIF
C
C        RETRIEVE THE DATA AND UNPACK.
C
      CALL RDTDLM(KFILDO,KFILX,RANAME,KD,IPACK,ND5,NVD,
     1               L3264B,IER)
      IF(IER.NE.0)THEN     
        IER=2
        GO TO 290
      ENDIF
C
      CALL UNPACK(KFILDO,IPACK,IWORK,DATA,ND5,
     1            IS0,IS1,IS2,IS4,ND7,MISSP,MISSS,3,L3264B,
     2            IER)
      IF(IER.NE.0)THEN     
        IER=2
        GO TO 290
      ENDIF
      NVALUE=IS4(3)     
C
C        IER NE 0 SHOULD BE TREATED AS A FATAL ERROR.  A DIAGNOSTIC
C        WILL HAVE BEEN PRINTED BY UNPACK.
C
C     FOR FORECAST DATA, CHECK THE DATE OF THE RETRIEVED DATA
C
C     IF(KFILX.EQ.48.OR.KFILX.EQ.49)THEN
      IF(IDPARS(1).GE.200.AND.IDPARS(1).LE.299) THEN
        IF(IS1(8).NE.NDATE)THEN
          WRITE(KFILDO,250)IS1(8),NDATE,(KD(I),I=1,4)
 250      FORMAT(/,' DATE OF FORECASTS ON MOS FORECAST FILE ',I10,
     1            ' DOES NOT MATCH NCEP DATE ',I10,/,
     2            ' VARIABLE ',3I10.9,1X,I10.10,' CAN NOT BE RETRIEVED')
          IER=2
          GO TO 290
        ENDIF
      ENDIF
C
      DO 270 K=1,NSTA
      IF(INDEX(K,JF).EQ.99999999)THEN
         SDATA(K)=9999.
      ELSE
         SDATA(K)=DATA(INDEX(K,JF))
      ENDIF
C
 270  CONTINUE
D     WRITE(KFILDO,271)(KD(J),J=1,4),NDATE,
D    1            (ISDATA(J),SDATA(J),J=1,NSTA)
D271  FORMAT(' ISDATA, SDATA IN CONST, KD( ) =',4I12,
D    1       '  NDATE =',I10,/,(' ',I8,F10.2))
C
C        AT THIS POINT ONE SET OF DATA HAS BEEN RETRIEVED
C        AND STORED IN SDATA( ).  WHEN INTERPOLATION IS NEEDED,
C        A SECOND RETRIEVAL IS NECESSARY, AS WELL AS THE 
C        INTERPOLATION.  DO THAT NOW WHEN NECESSARY.  FOR
C        CCC = 4XX, THE INTERPOLATION FACTOR HAS BEEN RETURNED
C        FROM COMPID.  FOR OTHER DATA, READ_MOSDA IS DONE.
C
      IF(KFILX.NE.45)GO TO 300
      IF(IDPARS(1).EQ.422.OR.
     1   IDPARS(1)/10.EQ.43)GO TO 275
      GO TO 300
C
 275  IF(KD(2).EQ.LD(2))GO TO 300
C        THE ABOVE TEST IS RELEVANT TO 5-DAY TEMPERATURES.
C      
      CALL RDTDLM(KFILDO,KFILX,RANAME,LD,IPACK,ND5,NVD,
     1               L3264B,IER)
      IF(IER.NE.0)THEN     
        IER=2
        GO TO 290
      ENDIF
C
      CALL UNPACK(KFILDO,IPACK,IWORK,DATA,ND5,
     1            IS0,IS1,IS2,IS4,ND7,MISSP,MISSS,3,L3264B,
     2            IER)
      NVALUE=IS4(3)     
      IF(IER.NE.0)THEN     
        IER=2
        GO TO 290
      ENDIF
C
C        IER NE 0 SHOULD BE TREATED AS A FATAL ERROR.  A DIAGNOSTIC
C        WILL HAVE BEEN PRINTED BY UNPACK.
C
      DO 280 K=1,NSTA
C
      IF(INDEX(K,JF).EQ.99999999)THEN
         SDATA(K)=9999.
      ELSE
C
         IF(SDATA(K).EQ.9999..OR.
     1      DATA(INDEX(K,JF)).EQ.9999.)THEN
            SDATA(K)=9999.
         ELSE
            SDATA(K)=(DATA(INDEX(K,JF))-SDATA(K))*R+SDATA(K)
         ENDIF
C
      ENDIF
C
 280  CONTINUE
D     WRITE(KFILDO,281)(ISDATA(K),SDATA(K),K=1,NSTA)
D281  FORMAT(' ISDATA, DATA(ISDATA), SDATA IN CONST',/,
D    1       (' ',I8,F10.2))
      GO TO 300
C
C        SET VALUES TO MISSING WHEN THERE IS AN ERROR OF 2 OR 3.
C
 290  DO 295 K=1,NSTA
      SDATA(K)=9999.
 295  CONTINUE
C
 300  IF(IER.EQ.0)THEN
         IER=IERSAV
      ENDIF
C
      RETURN
      END
