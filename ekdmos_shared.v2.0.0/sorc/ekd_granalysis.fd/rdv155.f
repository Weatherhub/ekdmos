      SUBROUTINE RDV155(KFILDO,IP6,IP7,IP9,KFILP,KFILCP,ID,IDPARS,
     1                  THRESH,JD,JP,ANLTAB,INLTAB,ISCALD,IWRITS,IWRITA,
     2                  ICOMPT,SMULT,SADD,ORIGIN,CINT,PLAIN,UNITS,ND4,
     3                  NPRED,ISTOP,IER)
C 
C        MAY       2004   GLAHN   TDL   MOS-2000 
C                                 ADAPTED FROM RDLVRB
C        JANUARY   2005   GLAHN   MODIFIED COMMENT ABOUT JP
C        JULY      2005   GLAHN   ELIMINTATED CHECKING FOR DUPLICATES
C        AUGUST    2005   GLAHN   INCREASED SIZE OF ANLTAB TO 14
C        SEPTEMBER 2005   GLAHN   PUT COMMAS IN SOME FORMATS; READ IN
C                                 UNIT NUMBER WITH FILE NAME FOR IBM;
C                                 ADDED INLTAB( ) TO CALL PER TRIMARCO
C        APRIL     2006   GLAHN   INCREASED ANLTAB AND ATEMP FROM
C                                 CHARACTER*14 TO *17
C        NOVEMBER  2006   GLAHN   CORRECTED PRINT TO IP6
C        FEBRUARY  2008   GLAHN   ADDED IWRITA( )
C        APRIL     2008   GLAHN   ADDED ICOMPT( )
C        OCTOBER   2009   ENGLE   MODIFIED FORMAT STATEMENT
C                                 182 FOR INTEL COMPILER
C
C        PURPOSE 
C            TO READ A VARIABLE LIST AND ASSOCIATED INFORMATION FROM
C            A FILE ON UNIT KFILP.  KFILP CAN BE THE DEFAULT INPUT FILE,
C            OR CAN BE A SEPARATE FILE.  ALSO, VARIABLE NAMES AND OTHER
C            INFORMATION FROM THE VARIABLE CONSTANT FILE CAN BE
C            RETRIEVED FROM THE FILE READ ON UNIT KFILCP AND MATCHED 
C            WITH THE VARIABLES.  EACH VARIABLE ID IN ID( , ) IS
C            DUPLICATED IN JD( , ) BUT WITH THE PROCESSING PORTIONS
C            OMITTED; THE RESULT IS CALLED THE "BASIC" ID.  PLAIN
C            LANGUAGE IS READ, MATCHED WITH THE VARIABLES, AND 
C            SOME INSERTIONS MADE FOR COMPUTED PREDICTORS BY ROUTINE
C            SETPLN.  ALL GRID BINARIES ARE GIVEN THE ISCALD( )
C            VALUE = 2, AND ALL POINT BINARIES ARE GIVEN THE ISCALD( )
C            VALUE = 0 BY SETPLN, EXCEPT CCC=2XX, 8XX, AND 4XX ARE GIVEN
C            THE VALUE 3. HOWEVER, THIS CAN BE CHANGED FOR
C            SPECIFIC VARIABLES BY INSERTING AN EXACT CCCFFFB MATCH 
C            IN THE PLAIN LANGUAGE FILE.
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT) 
C            IP6    - UNIT NUMBER FOR OUTPUT (PRINT) FILE FOR VARIABLE
C                     LIST AS READ IN.  (OUTPUT) 
C            IP7    - UNIT NUMBER FOR OUTPUT (PRINT) FILE FOR VARIABLE
C                     LIST PARSED INTO ITS 15 COMPONENTS.  (OUTPUT)
C            IP9    - UNIT NUMBER FOR OUTPUT (PRINT) FILE FOR VARIABLE
C                     LIST INCLUDING INFORMATION FROM THE VARIABLE
C                     CONSTANT FILE.  (OUTPUT)
C            KFILP  - UNIT NUMBER FROM WHICH TO READ VARIABLE LIST.
C                     IT IS ASSUMED FILE HAS BEEN OPENED.  (INPUT) 
C           KFILCP  - UNIT NUMBER FROM WHICH TO READ VARIABLE NAMES AND
C                     OTHER ASSOCIATED INFORMATION.  IT IS ASSUMED FILE 
C                     HAS BEEN OPENED.  (INPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT) 
C                 IP6 = INDICATES WHETHER (>0) OR NOT (=0) THE VARIABLE 
C                       ID WILL BE WRITTEN TO UNIT IP6 AS THE VARIABLES
C                       ARE READ IN.  THIS SHOULD BE USEFUL FOR CHECKOUT
C                       OTHERWISE, IP7, AND/OR IP9 MAY BE PREFERRED.
C                       (INPUT) 
C                 IP7 = INDICATES WHETHER (>0) OR NOT (=0) THE VARIABLE 
C                       LIST WILL BE WRITTEN TO UNIT IP7.  IF THERE ARE 
C                       INPUT ERRORS, THE VARIABLE LIST WILL BE WRITTEN 
C                       TO THE DEFAULT OUTPUT FILE UNIT KFILDO AS WELL AS 
C                       TO UNIT IP7 IF THEY ARE DIFFERENT.  (INPUT)
C                 IP9 = INDICATES WHETHER (>0) OR NOT (=0) THE VARIABLE 
C                       LIST WILL BE WRITTEN TO UNIT IP9.  THE DIFFERENCE
C                       BETWEEN IP7 AND IP9 IS THAT IP9 DOES NOT INCLUDE
C                       THE PARSED ID'S IN IDPARS( , ), BUT RATHER DOES
C                       INCLUDE INFORMATION TAKEN FROM THE VARIABLE
C                       CONSTANT FILE READ ON UNIT KFILCP.  (INPUT) 
C               KFILP = UNIT NUMBER FROM WHICH TO READ VARIABLE LIST.
C                       IT IS ASSUMED FILE HAS BEEN OPENED.  (INPUT)
C              KFILCP = UNIT NUMBER FROM WHICH TO READ VARIOUS VARIABLE
C                       CONSTANTS AND NAMES.  IT IS ASSUMED FILE HAS BEEN
C                       OPENED.  (INPUT)
C             ID(J,N) = THE INTEGER VARIABLE ID'S (J=1,4) (N=1,NPRED).
C                       (OUTPUT)
C         IDPARS(J,N) = THE PARSED, INDIVIDUAL COMPONENTS OF THE VARIABLE
C                       ID'S CORRESPONDING TO ID( ,N) (J=1,15), (N=1,NPRED).
C                       (OUTPUT)
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
C           THRESH(N) = THE BINARY THRESHOLD CORRESPONDING TO IDPARS( ,N)
C                       (N=1,ND4).  (OUTPUT)
C             JD(J,N) = THE BASIC INTEGER VARIABLE ID'S (J=1,4) (N=1,ND4).
C                       THIS IS THE SAME AS ID(J,N), EXCEPT THAT THE PORTIONS
C                       PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3, ),
C                       T = IDPARS(8,),
C                       I = IDPARS(13, ),
C                       S = IDPARS(14, ),
C                       G = IDPARS(15, ), AND
C                       THRESH( ).
C                       JD( , ) IS USED TO IDENTIFY THE BASIC MODEL FIELDS
C                       AS READ FROM THE ARCHIVE.  (OUTPUT)
C                       JD(4, ) IS USED TEMPORARILY IN THE SORTING 
C                       PROCESS AND IN KEEPING TRACK OF WHICH VARIABLES
C                       HAVE BEEN FOUND AND PROCESSED; THE ARRAY = 0 ON
C                       RETURN.
C             JP(J,N) = JP( ,N) INDICATES WHETHER (>0) OR NOT (=0) VARIABLE N
C                       WILL BE OUTPUT FOR VIEWING (N=1,NPRED).
C                       J=1--GRIDPOINT VALUES,
C                       J=2--GRIDPRINT WITH CONTOURS, AND
C                       J=3--INTERPOLATED VALUES.
C                       THIS ALLOWS INDIVIDUAL VARIABLE PRINT CONTROL.
C                       (OUTPUT)
C           ANLTAB(N) = THE CONTROL FILE NAME FOR THE VARIABLE (N=1,NPRED).
C                       (CHARACTER*17)  (OUTPUT)
C           INLTAB(N) = THE UNIT NUMBER FOR ANLTAB(N) NEEDED FOR IBM
C                       (N=1,NPRED).  (OUTPUT)
C           ISCALD(N) = THE SCALING CONSTANT TO USE WHEN PACKING THE 
C                       INTERPOLATED DATA (N=1,ND4).  (OUTPUT)
C                       USED TEMPORARILY IN XCHANG. (INTERNAL)
C           IWRITS(N) = 1 WHEN ANALYSIS FOR VARIABLE N IS TO BE WRITTEN
C                       TO INTERNAL STORAGE; 0 OTHERWISE (N=1,ND4).
C                       (OUTPUT)
C           IWRITA(N) = 1 WHEN ASCII DATA FOR VARIABLE N IS TO BE WRITTEN
C                       TO FILE UNIT NUMBER KFIOVO; 0 OTHERWISE (N=1,ND4).
C                       (OUTPUT)
C           ICOMPT(N) = SIGNALS WHETHER THE VARIABLE IS TO BE ANALYZED
C                       OR COMPUTED (N=1,ND4).
C                       0 WHEN THE VARIABLE IS TO BE ANALYZED; THE USUAL
C                         CASE.
C                       1 WHEN THE VARIABLE IS NOT TO BE ANALYZED BUT TO
C                         BE COMPUTED FROM OTHER ALRADY ANALYZED 
C                         VARIABLES.
C                       (OUTPUT)
C            SMULT(N) = THE MULTIPLICATIVE FACTOR WHEN CONTOURING OR
C                       GRIDPRINTING THE DATA (N=1,ND4).  (OUTPUT)
C                       USED TEMPORARILY IN XCHANG AS AN INTEGER ARRAY.
C                       (INTERNAL)
C             SADD(N) = THE ADDITIVE FACTOR WHEN CONTOURING OR
C                       GRIDPRINTING THE DATA (N=1,ND4).  (OUTPUT)
C            PLAIN(N) = THE PLAIN LANGUAGE DESCRIPTION OF THE VARIABLES
C                       (N=1,ND4).  (CHARACTER*32)  (OUTPUT)
C            UNITS(J) = THE UNITS OF THE DATA THAT APPLY AFTER MULTIPLYING
C                       BY SMULT(N) AND ADDING SADD(N) (N=1,ND4).
C                       (CHARACTER*12)  (OUTPUT)
C           ORIGIN(N) = THE CONTOUR ORIGIN, APPLIES TO THE UNITS IN UNITS(N)
C                       (N=1,ND4).  (OUTPUT)
C             CINT(N) = THE CONTOUR INTERVAL, APPLIES TO THE UNITS IN
C                       UNITS(N) (N=1,ND4).  (OUTPUT)
C                 ND4 = MAXIMUM NUMBER OF VARIABLES THAT CAN BE DEALT WITH 
C                       IN ONE RUN.  SECOND DIMENSION OF ID( , ) AND
C                       IDPARS( , ) AND DIMENSION OF THRESH( ).  (INPUT)
C               NPRED = THE NUMBER OF VARIABLES.  (OUTPUT)
C               ISTOP = INCREASED BY 1 WHENEVER AN ERROR IS ENCOUNTERED.
C                       (INPUT-OUTPUT)
C                 IER = STATUS RETURN.  (OUTPUT)
C                        0 = GOOD RETURN.
C                       40 = ERROR READING VARIABLE LIST.
C                       41 = DIMENSION ND4 ABOUT TO BE EXCEEDED.
C            ITEMP(J) = WORK ARRAY (J=1,6).  (INTERNAL)
C                       J=1--FIRST ID = ID(1, ),
C                       J=2--SECOND ID = ID(2, ),
C                       J=3--THIRD ID = ID(3, ),
C                       J=4--LAST PORTION OF ID = ID(4, ),
C                       J=5--FRACTIONAL PART OF THRESH, AND
C                       J=6--TEN'S EXPONENT OF ITEMP(5).
C                TEMP = SIGN OF THRESHOLD.  (CHARACTER*1)  (INTERNAL)
C                NERR = COUNTS ERRORS FOR PURPOSES OF KNOWING WHEN TO
C                       PRINT DIAGNOSTICS.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            PRSID, BASICP, SORTEM, XCHANG, CKIDS
C
      CHARACTER*1 TEMP
      CHARACTER*17 ATEMP,ANLTAB(ND4)
      CHARACTER*12 UNITS(ND4)
      CHARACTER*32 PLAIN(ND4)
C
      DIMENSION ID(4,ND4),IDPARS(15,ND4),THRESH(ND4),JD(4,ND4),
     1          JP(3,ND4),ISCALD(ND4),IWRITS(ND4),IWRITA(ND4),
     2          ICOMPT(ND4),INLTAB(ND4),
     3          SMULT(ND4),SADD(ND4),ORIGIN(ND4),CINT(ND4)
      DIMENSION ITEMP(6),JTEMP(3)
C
      IER=0
      NERR=0
      N=1
C
C        READ PREDICTOR ID.
C
 102  READ(KFILP,103,IOSTAT=IOS,ERR=104,END=150)(ITEMP(J),J=1,4),
     1            TEMP,ITEMP(5),ITEMP(6),(JTEMP(J),J=1,3),
     2            ATEMP,MTEMP,IW,IA,IC
 103  FORMAT(I9,1X,I9,1X,I9,1X,I3,1X,A1,1X,I4,1X,I3,4X,3I2,1X,A17,
     1       I4,3I2)
      IF(IP6.NE.0.AND.N.EQ.1)WRITE(IP6,1030)
 1030 FORMAT(/' VARIABLES AS READ BY RDV155')
      IF(IP6.NE.0)WRITE(IP6,1031)(ITEMP(J),J=1,4),
     1            TEMP,ITEMP(5),ITEMP(6),(JTEMP(J),J=1,3),
     2            ATEMP,MTEMP,IW,IA,IC
 1031 FORMAT(' ',I9.9,1X,I9.9,1X,I9.9,1X,I3.3,1X,A1,1X,I4,1X,I3,4X,4I2,
     1           1X,A17,I4,2I2)
      GO TO 120
C
 104  NERR=NERR+1
      ISTOP=ISTOP+1
      IER=40
 
      WRITE(KFILDO,107)N,IOS
 107  FORMAT(/' ****ERROR READING VARIABLE ID NO.',I4,
     1        '.  IOSTAT =',I5,'.  VARIABLE SKIPPED.')
      IF(IP6.NE.KFILDO.AND.IP6.NE.0)WRITE(IP6,107)N,IOS
      GO TO 102
C      
 120  IF(ITEMP(1).EQ.999999)GO TO 150
C
C        STORE THIS VARIABLE ID, UNLESS ND4 WILL BE EXCEEDED.
C
      IF(N.LE.ND4)GO TO 125
      IER=41
      NERR=NERR+1
      ISTOP=ISTOP+1
      WRITE(KFILDO,122)ND4
 122  FORMAT(/' ****ND4 = ',I4,' TOO SMALL IN RDV155.')
      GO TO 180
C
C        PARSE ID'S INTO 15 COMPONENT PARTS ID(J, ) (J=1,15) AND
C        THRESH( ).
C
 125  CALL PRSID(KFILDO,ITEMP,TEMP,ID(1,N),IDPARS(1,N),THRESH(N),ISTOP)
C
C        PREPARE "BASIC" VARIABLE ID'S, THE VARIABLE ID'S WITHOUT
C        THE "PROCESSING" INFORMATION.
C
      CALL BASICP(KFILDO,IDPARS(1,N),JD(1,N))
C
C        STORE PRINT PARAMETERS AND THE DESIGNATOR FOR WRITING
C        TO INTERNAL STORAGE.
C
      JP(1,N)=JTEMP(1)
      JP(2,N)=JTEMP(2)
      JP(3,N)=JTEMP(3)
      IWRITS(N)=IW
      IWRITA(N)=IA
      ICOMPT(N)=IC
C
C        STORE THE CONTROL FILE NAME AND UNIT NUMBER.
C
      ANLTAB(N)(1:17)=ATEMP(1:17)
      INLTAB(N)=MTEMP
C
C        OMIT VARIABLE IF IT IS A DUPLICATE.
C
      IF(N.EQ.1)GO TO 130
C
      DO 129 J=1,N-1
      IF(ID(1,J).NE.ID(1,N).OR.
     1   ID(2,J).NE.ID(2,N).OR.
     2   ID(3,J).NE.ID(3,N).OR.
     3   ID(4,J).NE.ID(4,N))GO TO 129
      WRITE(KFILDO,128)(ID(L,N),L=1,4)
 128  FORMAT(/' ****DUPLICATE VARIABLE DELETED',4I11)
      ISTOP=ISTOP+1
      GO TO 102
C
 129  CONTINUE
C
 130  N=N+1
      GO TO 102
C      
 150  NPRED=N-1
C
C        WRITE VARIABLE LIST WHEN REQUIRED.  LIST IS ALWAYS WRITTEN
C        TO DEFAULT OUTPUT WHEN THERE HAS BEEN AN ERROR.
C
 180  IF(NERR.NE.0)WRITE(KFILDO,181)NPRED
 181  FORMAT(/' ',I4,' VARIABLES AND PARSED IDS IN ORDER READ IN')
      IF(NERR.NE.0)WRITE(KFILDO,182)(N,(ID(J,N),J=1,4),
     1                              (IDPARS(J,N),J=1,15),
     2                              THRESH(N),THRESH(N),
     3                              (JP(J,N),J=1,3),N=1,NPRED)
 182  FORMAT('  NO.     ID(1)     ID(2)     ID(3)      ID(4)',
     2       '   CCC FFF B DD    V LLLL UUUU    T RR  O HH',
     3       ' TAU    I S G    THRESHOLD            PRINT'/ 
     4      (' ',I4,1X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     5       2X,I4.3,I4.3,I2,I3,3X,I2,I5,I5,3X,
     6       I2,I3,I3,I3,I4,3X,I2,I2,I2,F13.6,E11.4,3I2))       
      IF(NERR.NE.0.AND.KFILDO.NE.IP7.OR.NERR.EQ.0.AND.IP7.NE.0)
     1   WRITE(IP7,181)NPRED
      IF(NERR.NE.0.AND.KFILDO.NE.IP7.OR.NERR.EQ.0.AND.IP7.NE.0)
     1   WRITE(IP7,182)(N,(ID(J,N),J=1,4),
     2                 (IDPARS(J,N),J=1,15),
     3                 THRESH(N),THRESH(N),
     4                 (JP(J,N),J=1,3),N=1,NPRED)
C
C        READ THE PLAIN LANGUAGE AND OTHER INFORMATION
C        FROM THE VARIABLE CONSTANT FILE ON UNIT KFILCP
C        AND INITIALIZE PLAIN( ) AND OTHER VARIABLES.
C        THEN CLOSE UNIT KFILCP.
C
      CALL SETPLN(KFILDO,KFILCP,
     1            ID,IDPARS,JD,ISCALD,SMULT,SADD,
     2            ORIGIN,CINT,PLAIN,UNITS,ND4,NPRED,ISTOP,IER)
C
C     CLOSE(KFILCP)
C
      IF(IP9.NE.0)WRITE(IP9,230)NPRED
 230  FORMAT(/' ',I4,' VARIABLES AND INFORMATION FROM',
     1              ' CONSTANT FILE:  IWRITS: 1=INTERNAL RA;',
     3              '    IWRITA: 1=ASCII KFILVO;',
     3              '    CMP:  1=COMPUTE ONLY')
      IF(IP9.NE.0)WRITE(IP9,235)(N,(ID(J,N),J=1,4),
     1            PLAIN(N),ISCALD(N),
     2            (JP(J,N),J=1,3),IWRITS(N),IWRITA(N),ICOMPT(N),
     3            ANLTAB(N),INLTAB(N),N=1,NPRED)
 235  FORMAT('  NO.     ID(1)     ID(2)     ID(3)      ID(4)',
     1       '       PLAIN LANGUAGE ID         ISCALD',
     2       '   JP  IWRITS IWRITA CMP CONTROL FILE  UNIT NO.'/ 
     3      (' ',I4,1X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,2X,
     4       A32,I3,2X,3I2,I5,I6,I6,2X,A17,I4))
      RETURN
      END 
