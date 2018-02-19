      SUBROUTINE RDVR75(KFILDO,IP6,IP7,IP8,IP9,KFILP,KFILCP,
     1                  ID,IDPARS,TRESHL,TRESHU,JD,JP,PLAIN,ITAU,KER,
     2                  ISD,SD,DS,ISCALD,NCAT,ND4,
     3                  NVRBL,ISTOP,IER)
C 
C        FEBRUARY  2007   GLAHN   TDL   MOS-2000 
C                                 DERIVED FROM RDVR79; USED FOR U715
C        MAY       2007   GLAHN   COMMENTS ABOUT ISD( )   
C        SEPTEMBER 2007   GLAHN   CHANGED DEFINTION OF SD( )
C        APRIL     2010   GLAHN   LEFT "G" IN ID(4) AND JD(4) FOR
C                                 MULTIPLE MODELS
C
C        PURPOSE 
C            TO READ FOR U715 THROUGH INT715 A VARIABLE LIST AND 
C            ASSOCIATED INFORMATION FROM A FILE ON UNIT KFILP. 
C            KFILP CAN BE THE DEFAULT INPUT FILE, OR CAN BE A 
C            SEPARATE FILE.  ALSO, VARIABLE NAMES AND OTHER
C            INFORMATION FROM THE VARIABLE CONSTANT FILE ON UNIT
C            KFILCP CAN BE MATCHED WITH THE VARIABLES.  SOME INSERTIONS
C            ARE MADE FOR COMPUTED VARIABLES.  EACH VARIABLE ID IN 
C            ID( , ) IS DUPLICATED IN JD( , ) BUT WITH SOME PORTIONS
C            OMITTED; THE RESULT IS CALLED THE "BASIC" ID.  (THIS BASIC 
C            ID IS NOT DEFINED THE SAME WAY IN U201.)   THIS ROUTINE IS
C            USED FOR U715 (AND ALSO U915 ?); A SIMILIAR ONE RDPRED IS
C            USED FOR U201, RDVRBL FOR U600 AND RDVR79 FOR U710.
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                     (OUTPUT) 
C            IP6    - UNIT NUMBER FOR OUTPUT (PRINT) FILE FOR VARIABLE
C                     LIST AS READ IN.  (OUTPUT) 
C            IP7    - UNIT NUMBER FOR OUTPUT (PRINT) FILE FOR VARIABLE
C                     LIST PARSED INTO ITS 15 COMPONENTS.  (OUTPUT)
C            IP8    - UNIT NUMBER FOR OUTPUT (PRINT) FILE FOR VARIABLE
C                     LIST PARSED INTO ITS COMPONENTS.  (OUTPUT)
C            IP9    - UNIT NUMBER FOR OUTPUT (PRINT) FILE FOR VARIABLE
C                     LIST INCLUDING INFORMATION FROM THE VARIABLE
C                     CONSTANT FILE.  (OUPUT)
C            KFILP  - UNIT NUMBER FROM WHICH TO READ VARIABLE LIST.
C                     IT IS ASSUMED FILE HAS BEEN OPENED.  (INPUT) 
C           KFILCP  - UNIT NUMBER FROM WHICH TO READ VARIABLE NAMES AND
C                     OTHER ASSOCIATED INFORMATION.  IT IS ASSUMED FILE 
C                     HAS BEEN OPENED.  (INPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT) 
C                 IP6 = INDICATES WHETHER (>0) OR NOT (=0) THE VARIABLE 
C                       ID WILL BE WRITTEN TO UNIT IP6 AS THE VARIABLES
C                       ARE READ IN.  THIS SHOULD BE USEFUL FOR CHECKOUT
C                       OTHERWISE, IP7, IP8, AND/OR IP9 MAY BE PREFERRED.
C                       (INPUT) 
C                 IP7 = INDICATES WHETHER (>0) OR NOT (=0) THE VARIABLE
C                       LIST WILL BE WRITTEN TO UNIT IP7 PARSED INTO ITS
C                       15 INTEGER AND ONE REAL*4 COMPONENT.  IF THERE ARE 
C                       INPUT ERRORS, THE VARIABLE LIST WILL BE WRITTEN 
C                       TO THE DEFAULT OUTPUT FILE UNIT KFILDO AS WELL AS 
C                       TO UNIT IP7 IF THEY ARE DIFFERENT.  (INPUT)
C                 IP8 = SAME AS IP7, EXCEPT TRESHL( ) AND TRESHU( )HAVE
C                       BEEN SET AND PRINTED.  (INPUT)
C                 IP9 = INDICATES WHETHER (>0) OR NOT (=0) THE VARIABLE 
C                       LIST WILL BE WRITTEN TO UNIT IP9.  THE DIFFERENCE
C                       BETWEEN IP8 AND IP9 IS THAT IP9 DOES NOT INCLUDE
C                       THE PARSED ID'S IN IDPARS( , ), BUT RATHER DOES
C                       INCLUDE INFORMATION TAKEN FROM THE VARIABLE
C                       CONSTANT FILE READ ON UNIT KFILCP.  (INPUT) 
C               KFILP = UNIT NUMBER FROM WHICH TO READ VARIABLE LIST.
C                       IT IS ASSUMED FILE HAS BEEN OPENED.  (INPUT)
C              KFILCP = UNIT NUMBER FROM WHICH TO READ VARIOUS VARIABLE
C                       CONSTANTS AND NAMES.  IT IS ASSUMED FILE HAS BEEN
C                       OPENED.  (INPUT)
C             ID(J,N) = THE INTEGER VARIABLE ID'S (J=1,4) (N=1,NVRBL).
C                       (OUTPUT)
C         IDPARS(J,N) = THE PARSED, INDIVIDUAL COMPONENTS OF THE VARIABLE
C                       ID'S CORRESPONDING TO ID( ,N) (J=1,15), (N=1,NVRBL).
C                       (OUTPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                          0 = NOT BINARY,
C                          1 = CUMULATIVE FROM ABOVE, VALUES GE LOWER THRESHOLD
C                              TRESHL = 1,
C                          2 = CUMULATIVE FROM BELOW, VALUES LT UPPER THRESHOLD
C                              TRESHU = 1.
C                          3 = DISCRETE BINARY.  VALUES GE LOWER THRESHOLD AND
C                              LT UPPER THRESHOLD = 1.
C                          5 = GRID BINARY.  VALUES GE LOWER THRESHOLD
C                          ONLY THE VALUE OF 0, 1, OR 5 SHOULD BE USED FOR
C                          PREDICTORS;
C                          0, 1, 2, OR 3 CAN BE USED FOR PREDICTANDS.
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
C                       J=15--G (GRID INDICATOR).  THIS NORMALLY HAS NO
C                             MEANING, BUT IS USED IN U715.
C           TRESHL(N) = AS READ, THE BINARY THRESHOLD CORRESPONDING TO 
C                       IDPARS( ,N) (N=1,ND4).  THIS IS USED IN SUBROUTINE
C                       THSET TO SET THE LOWER AND UPPER THRESHOLDS AS
C                       TRESHL( ) AND TRESHU( ), RESPECTIVELY.  NOTE
C                       THAT, SINCE THE VARIABLES ARE NOT SORTED BY U715
C                       (AS THEY ARE IN U600), THE SETTING OF THRESHOLDS
C                       FOR A DISCRETE BINARY WILL BE DONE CORRECTLY
C                       ONLY IF THE VARIABLES INVOLVED ARE SEQUENTIAL
C                       IN THE INPUT LIST .  (OUTPUT)
C           TRESHU(N) = THE UPPER THRESHOLD (SEE TRESHL( )).  (OUTPUT)
C             JD(J,N) = THE BASIC INTEGER VARIABLE ID'S (J=1,4) (N=1,ND4).
C                       THIS IS THE SAME AS ID(J,N), EXCEPT THAT THE FOLLOWING
C                       PORTIONS ARE OMITTED:
C                       B = IDPARS(3, ),
C                       TRESHL( ) AND
C                       TRESHU( ).
C                       JD( , ) IS USED TO IDENTIFY WHICH CALCULATIONS
C                       CAN BE MADE DIRECTLY IN U715, WHICH IS ONLY FORMING
C                       BINARIES.  THE "G" VARIABLE HAS NO MEANING IN U715,
C                       IT BEING ONLY FOR POSSIBLE USE IN U201.  (OUTPUT)
C             JP(J,N) = CONTROLS THE OUTPUT BY VARIABLE (N=1,ND4).
C                       J=1--NOT USED.
C                       J=2--NOT USED.
C                       J=3--INDICATES WHETHER (>0) OR NOT (=0) VARIABLE N
C                            WILL BE WRITTEN TO UNIT IP(15) NOT UNDER
C                            CONTROL OF THE FORMAT PROVIDED BUT TO THE
C                            RESOLUTION PACKED.
C                       (OUTPUT)
C            PLAIN(N) = THE PLAIN LANGUAGE DESCRIPTION OF THE VARIABLES
C                       (N=1,ND4).  (CHARACTER*32)  (OUTPUT)
C             ITAU(N) = THE NUMBER OF HOURS TO ADD TO NDATE TO GET 
C                       THE VARIABLE N (N=1,ND4).  NO CURRENT REASON 
C                       FOR THIS TO BE OTHER THAN ZERO.
C              KER(N) = DESIGNATES THE KERNAL TO BE USED FOR VARIABLE N
C                       (N=1,ND4).
C                       1 = NORMAL (GAUSIAN).
C                       (OUTPUT)
C              ISD(N) = DESIGNATES WHETHER THE KERNAL WIDTH FOR VARIABLE N
C                       (N=1,ND4) IS TO BE TAKEN FROM SD( ) IN THE VARIABLE
C                       RECORD OR FROM A PACKED INPUT RECORD.
C                       0 = COMES FROM PACKED RECORD;
C                       2 = WHEN THERE ARE MULTIPLE ENSEMBLES OR
C                           ONLY ONE ENSEMBLE AND SD( ) > 10, 
C                           CALL KERNELW; OTHERWISE, CALL KERNEL.
C                       (OUTPUT)
C               SD(N) = A FACTOR TO USE IN THE SPREAD ADJUSTMENT FOR
C                       MULTIPLE ENSEMBLES FOR THIS VARIABLE N
C                       (N=1,ND4).  (OUTPUT)
C               DS(N) = SCALING FACTOR FOR THE STANDARD DEVIATION FOR
C                       THIS VARIABLE (N=1,ND4).  (OUTPUT)      
C           ISCALD(N) = THE DECIMAL SCALING CONSTANT TO USE WHEN PACKING THE 
C                       COLLATED DATA (N=1,ND4).  NO BINARY SCALING IS
C                       PROVIDED FOR.  ISCALD COMES FROM THE VARIABLE
C                       CONSTANT FILE, MODIFIED TO BE 2 FOR GRID BINARIES,
C                       AND 0 FOR BINARIES.  ZERO WHEN NOT FOUND IN THE
C                       FILE.  (OUTPUT)
C             NCAT(N) = A CATEGORY NUMBER FOR EACH VARIABLE (N=1,ND4).
C                       0 = THIS VARIABLE IS IN A SERIES, NOT THE FIRST.
C                       M = THIS VARIABLE IS THE FIRST OF A SERIES OF 
C                           M VARIABLES.
C                       THIS IS NOT CURRENTLY USED IN U715.  (OUTPUT)
C                 ND4 = MAXIMUM NUMBER OF VARIABLES THAT CAN BE DEALT WITH 
C                       IN ONE RUN.  SECOND DIMENSION OF ID( , ) AND
C                       IDPARS( , ) AND DIMENSION OF TRESHL( ), TRESHU( ),
C                       AND ITAU( ).  (INPUT)
C               NVRBL = THE NUMBER OF VARIABLES.  (OUTPUT)
C               ISTOP = INCREASED BY 1 WHENEVER AN ERROR IS ENCOUNTERED.
C                       (INPUT-OUTPUT)
C                 IER = STATUS RETURN.  (OUTPUT)
C                        0 = GOOD RETURN.
C                       40 = ERROR READING VARIABLE LIST.
C                       41 = DIMENSION ND4 ABOUT TO BE EXCEEDED.
C                       42 = NO VARIABLES.
C            ITEMP(J) = WORK ARRAY (J=1,6).  (INTERNAL)
C                       J=1--FIRST ID = ID(1, ),
C                       J=2--SECOND ID = ID(2, ),
C                       J=3--THIRD ID = ID(3, ),
C                       J=4--LAST PORTION OF ID = ID(4, ),
C                       J=5--FRACTIONAL PART OF TRESHL, AND
C                       J=6--TEN'S EXPONENT OF ITEMP(5).
C                TEMP = SIGN OF THRESHOLD.  (CHARACTER*1)  (INTERNAL)
C            JTEMP(J) = TEMPORARY ARRAY FOR READING INFORMATION BEFORE
C                       STORING (J=1,4).  (INTERNAL)
C                NERR = COUNTS ERRORS FOR PURPOSES OF KNOWING WHEN TO
C                       PRINT DIAGNOSTICS.  IF NERR EXCEEDS 100,
C                       RDVRBL WILL STOP.  (INTERNAL)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            PRSID, THSET, SETPLN
C
      CHARACTER*1 TEMP
      CHARACTER*12 UNITS(ND4)
C        UNITS( ) IS AN AUTOMATIC ARRAY FOR CALL TO SETPLN.
      CHARACTER*32 PLAIN(ND4)
C
      DIMENSION ID(4,ND4),IDPARS(15,ND4),TRESHL(ND4),TRESHU(ND4),
     1          JD(4,ND4),JP(3,ND4),ITAU(ND4),NCAT(ND4),
     2          KER(ND4),ISD(ND4),SD(ND4),DS(ND4),
     3          ISCALD(ND4)
      DIMENSION SMULT(ND4),SADD(ND4),ORIGIN(ND4),CINT(ND4)
C        SMULT( ), SADD( ), ORIGIN( ), AND CINT( ) ARE AUTOMATIC
C        ARRAYS FOR CALL TO SETPLN.
      DIMENSION ITEMP(6),JTEMP(4)
C
      DATA TEMP/' '/
C         
      IER=0
      NERR=0
C
      N=1
C
C        READ VARIABLE ID.
C
 102  READ(KFILP,103,IOSTAT=IOS,ERR=104,END=150)(ITEMP(J),J=1,4),
     1            TEMP,ITEMP(5),ITEMP(6),(JTEMP(J),J=1,4),
     2            JKER,JISD,FSD,FDS
 103  FORMAT(I9,1X,I9,1X,I9,1X,I3,1X,A1,1X,I4,1X,I3,4X,3I2,I3,2I2,2F5.0)
      IF(IP6.NE.0.AND.N.EQ.1)WRITE(IP6,1030)
 1030 FORMAT(/,' VARIABLES AS READ BY RDVR75')
C      IF(IP6.NE.0)WRITE(IP6,1031)(ITEMP(J),J=1,4),
      WRITE(IP6,1031)(ITEMP(J),J=1,4),
     1            TEMP,ITEMP(5),ITEMP(6),(JTEMP(J),J=1,4),
     2            JKER,JISD,FSD,FDS
 1031 FORMAT(' ',I9.9,1X,I9.9,1X,I9.9,1X,I3.3,1X,A1,'.',I4.4,'E',I3.2,
     1          4X,3I2,1X,I3,2I2,2F6.2)
      GO TO 120
C
 104  NERR=NERR+1
      ISTOP=ISTOP+1
      IER=40
 
      WRITE(KFILDO,107)N,IOS
 107  FORMAT(/' ****ERROR READING VARIABLE ID NO.',I4,
     1        '.  IOSTAT =',I5,'.  VARIABLE SKIPPED.')
      IF(IP6.NE.KFILDO)WRITE(IP6,107)N,IOS
C        ATTEMPT TO READ ANOTHER RECORD.  HOWEVER, SOME ERRORS WILL
C        KEEP REPEATING AND FILL UP THE PRINT FILE.  GO BACK ONLY IF
C        IT SEEMS THERE WAS AN ERROR ON AN INDIVIDUAL RECORD.  A
C        LIMIT OF NERR = 100 HAS BEEN SET.
C
      IF(NERR.GE.100)THEN
         WRITE(KFILDO,110)
 110     FORMAT(/,' ****READING ERRORS IN RDVR75 HALTED PROGRAM',
     1            ' AT 110.')
         STOP 110
      ENDIF
C
      IF(IOS.EQ.900.OR.
     1   IOS.EQ.915.OR.
     2   IOS.EQ.922.OR.
     3   IOS.EQ.933.OR.
     4   IOS.EQ.945.OR.
     5   IOS.EQ.958)GO TO 102
      GO TO 150
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
 122  FORMAT(/,' ****ND4 = ',I6,' TOO SMALL IN RDVR75.')
      GO TO 180
C
C        PARSE ID'S INTO 15 COMPONENT PARTS ID(J, ) (J=1,15) AND 
C        TRESHL( ).
C
 125  CALL PRSID(KFILDO,ITEMP,TEMP,ID(1,N),IDPARS(1,N),TRESHL(N),ISTOP)
C
C        PREPARE "BASIC" VARIABLE ID'S.  FOR PROGRAMS READING U201
C        OUTPUT (E.G., U715), JD( , ) = ID( , ) EXCEPT IN ID(1, ) 
C        THE BINARY INDICATOR IS OMITTED AND IN JD(4, ) THE 
C        THRESHOLDS AND IDPARS(15, ) ARE OMITTED.
C
      JD(1,N)=IDPARS(1,N)*1000000+IDPARS(2,N)*1000+IDPARS(4,N)
      JD(2,N)=ID(2,N)
      JD(3,N)=ID(3,N)
      JD(4,N)=IDPARS(13,N)*100+
     1        IDPARS(14,N)*10+IDPARS(15,N)
C
C        STORE PRINT AND OTHER PARAMETERS.
C
      JP(1,N)=JTEMP(1)
      JP(2,N)=JTEMP(2)
      JP(3,N)=JTEMP(3)
      ITAU(N)=JTEMP(4)
      KER(N)=JKER
      ISD(N)=JISD
      SD(N)=FSD
      DS(N)=FDS
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
      IF(ITAU(J).NE.ITAU(N))GO TO 129
C        A VARIABLE WITH THE SAME ID'S BUT WITH A DIFFERENT
C        ITAU IS ALLOWED.
      WRITE(KFILDO,128)(ID(L,N),L=1,4)
      IF(IP6.NE.KFILDO.AND.IP6.NE.0)WRITE(IP6,128)(ID(L,N),L=1,4)
 128  FORMAT(/,' ****DUPLICATE VARIABLE DELETED',
     1        1X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
      ISTOP=ISTOP+1
      GO TO 102
C
 129  CONTINUE
C
C        INSIST ON SEQUENTIAL DISCRETE BINARIES HAVING LOWER
C        TO HIGHER THRESHOLDS.
C
      IF(IDPARS(3,N).NE.3.OR.
     1   IDPARS(3,N-1).NE.3.OR.
     2   TRESHL(N).GT.TRESHL(N-1))GO TO 130
      IF(ID(1,N).NE.ID(1,N-1).OR.
     1   ID(2,N).NE.ID(2,N-1).OR.
     2   ID(3,N).NE.ID(3,N-1).OR.
     3   IDPARS(13,N).NE.IDPARS(13,N-1).OR.
     4   IDPARS(14,N).NE.IDPARS(14,N-1).OR.
     5   IDPARS(15,N).NE.IDPARS(15,N-1))GO TO 130
      WRITE(KFILDO,1295)(ID(L,N),L=1,4)
      IF(IP6.NE.KFILDO.AND.IP6.NE.0)WRITE(IP6,1295)(ID(L,N),L=1,4)
 1295 FORMAT(/,' ****THRESHOLDS NOT IN ORDER LOW TO HIGH.',
     1         '   VARIABLE DELETED',
     2         1X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
      ISTOP=ISTOP+1
      GO TO 102
C
 130  N=N+1
      GO TO 102
C      
 150  NVRBL=N-1
      IF(NVRBL.EQ.0)GO TO 228
C
C        NOW SET NCAT( ) SUCH THAT THE FIRST VARIABLE IN A SERIES
C        HAS THE NUMBER OF VALUES IN THE SERIES, AND ALL OTHER VALUES
C        IN THE SERIES HAS A VALUE OF ZERO.  NOTE THAT A "SINGLE"
C        VARIABLE (A VARIABLE NOT IN A SERIES) ALSO HAS A VALUE OF 1.

D     WRITE(KFILDO,151)(NCAT(N),N=1,NVRBL)
D151  FORMAT(/,' AT 151 IN RDVR75--NCAT',40I4)
C
      NCAT(1)=1
      M=0
C
      DO 160 N=2,NVRBL
C
D     WRITE(KFILDO,155)(ID(J,N),J=1,4),
D    1                 (ID(J,N-1),J=1,4),
D    2                 (IDPARS(J,N),J=1,15),
D    3                 (IDPARS(J,N-1),J=1,15)
D155  FORMAT(' AT 155 IN RDVR75--ID( ,N),ID( ,N-1)',4I12,/,
D    1       '                                    ',4I12,/,
D    2       '                                    ',15I4,/,
D    3       '                                    ',15I4)
      IF(IDPARS(3,N).EQ.1.OR.
     1   IDPARS(3,N).EQ.2.OR.
     2   IDPARS(3,N).EQ.3.OR.
     3   (IDPARS(1,N).EQ.203.AND.(IDPARS(2,N).EQ.250.OR.
     4                            IDPARS(2,N).EQ.260.OR.
     5                            IDPARS(2,N).EQ.350.OR.
     6                            IDPARS(2,N).EQ.360.OR.
     7                            IDPARS(2,N).EQ.450.OR.
     8                            IDPARS(2,N).EQ.460)))THEN
C              THE CCCFFF = 203250, FOR INSTANCE, ARE PROBABILITY
C              FRACTICLES AND NEED TO BE PART OF A SEQUENCE
C**********THIS IS LEFT IN BUT MAY NOT BE NEEDED FOR U715**********
D           WRITE(KFILDO,156)N,M
D156        FORMAT(/,' AT 156 IN RDVR75--N,M',2I4)
C
            IF(ID(1,N).EQ.ID(1,N-1).AND.
     1         ID(2,N).EQ.ID(2,N-1).AND.
     2         ID(3,N).EQ.ID(3,N-1).AND.
     3         IDPARS(13,N).EQ.IDPARS(13,N-1).AND.
     4         IDPARS(14,N).EQ.IDPARS(14,N-1).AND.
     5         IDPARS(15,N).EQ.IDPARS(15,N-1))THEN
C
D                 WRITE(KFILDO,157)N,M
D157              FORMAT(/,' AT 157 IN RDVR75--N,M',2I4)
C
                  IF(M.EQ.0)THEN
                     M=N-1
                  ENDIF
C
                  NCAT(M)=NCAT(M)+1
                  NCAT(N)=0
            ELSE
D                 WRITE(KFILDO,158)N,M
D158              FORMAT(/,' AT 158 IN RDVR75--N,M',2I4)
                  M=0
                  NCAT(N)=1
            ENDIF
C
      ELSE
         NCAT(N)=1
      ENDIF
C
 160  CONTINUE
C
D     WRITE(KFILDO,161)(NCAT(N),N=1,NVRBL)
D161  FORMAT(/,' AT 161 IN RDVR75--NCAT',10I4)
C
C        WRITE VARIABLE LIST WHEN REQUIRED.  LIST IS ALWAYS WRITTEN
C        TO DEFAULT OUTPUT WHEN THERE HAS BEEN AN ERROR.
C
 180  IF(NERR.NE.0)WRITE(KFILDO,181)NVRBL
 181  FORMAT(/,' ',I4,' VARIABLES AND PARSED IDS IN ORDER READ IN')
      IF(NERR.NE.0)WRITE(KFILDO,182)(N,(ID(J,N),J=1,4),
     1                  (IDPARS(J,N),J=1,15),TRESHL(N),
     2                  (JP(J,N),J=1,3),ITAU(N),NCAT(N),N=1,NVRBL)
 182  FORMAT('  NO.     ID(1)     ID(2)     ID(3)      ID(4)',
     2       '   CCC FFF B DD    V LLLL UUUU    T RR  O HH',
     3       ' TAU    I S G     THRESHOLD JP( )  ITAU NC',/,
     4      (' ',I4,1X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     5       2X,I4.3,I4.3,I2,I3,3X,I2,I5,I5,3X,
     6       I2,I3,I3,I3,I4,3X,I2,I2,I2,F14.6,3I2,I5,I4))       
C
      IF(NERR.NE.0.AND.KFILDO.NE.IP7.OR.NERR.EQ.0.AND.IP7.NE.0)THEN
         WRITE(IP7,181)NVRBL
         WRITE(IP7,182)(N,(ID(J,N),J=1,4),
     1                 (IDPARS(J,N),J=1,15),TRESHL(N),
     2                 (JP(J,N),J=1,3),ITAU(N),NCAT(N),N=1,NVRBL)
      ENDIF
C
C        SET UPPER AND LOWER THRESHOLDS FOR ALL VARIABLES.
C
      CALL THSET(KFILDO,ID,IDPARS,NVRBL,TRESHL,TRESHU,IER)
      IF(IER.NE.0)ISTOP=ISTOP+1
C
C        WRITE VARIABLE LIST AS DESIRED.
C
      IF(IP8.NE.0)WRITE(IP8,191)NVRBL
 191  FORMAT(/,' ',I4,' VARIABLES AND PARSED IDS')
      IF(IP8.NE.0)WRITE(IP8,192)(N,(ID(J,N),J=1,4),
     1                 (IDPARS(J,N),J=1,15),TRESHL(N),TRESHU(N),
     3                 (JP(J,N),J=1,3),ITAU(N),N=1,NVRBL)
 192  FORMAT('  NO.     ID(1)     ID(2)     ID(3)      ID(4)',
     2       '   CCC FFF B DD    V LLLL UUUU    T RR  O HH',
     3       ' TAU    I S G   L--THRESHOLDS--U  JP( ) IT',/,
     4      (' ',I4,1X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     5       2X,I4.3,I4.3,I2,I3,3X,I2,I5,I5,3X,
     6       I2,I3,I3,I3,I4,3X,I2,I2,I2,F10.2,F10.2,3I2,I3))       
C
C        READ THE PLAIN LANGUAGE AND OTHER INFORMATION
C        FROM THE VARIABLE CONSTANT FILE, AND INITIALIZE
C        PLAIN( ) AND OTHER VARIABLES.
C
      CALL SETPLN(KFILDO,KFILCP,
     1            ID,IDPARS,JD,ISCALD,SMULT,SADD,
     2            ORIGIN,CINT,PLAIN,UNITS,ND4,NVRBL,ISTOP,IER)
C
 228  IF(NVRBL.EQ.0)THEN
         WRITE(KFILDO,229)
 229     FORMAT(/,' ****NO VARIABLES FOUND TO PROCESS.')
         ISTOP=ISTOP+1
         IER=42
         GO TO 250
      ENDIF
C
      IF(IP9.NE.0)WRITE(IP9,231)NVRBL
 231  FORMAT(/,' ',I4,' VARIABLES AND INFORMATION FROM VARIABLE',
     1              ' CONSTANT FILE')
      IF(IP9.NE.0)WRITE(IP9,235)(N,(ID(J,N),J=1,4),
     1            PLAIN(N),ISCALD(N),
     2            TRESHL(N),TRESHU(N),ITAU(N),NCAT(N),
     3            KER(N),ISD(N),SD(N),DS(N),N=1,NVRBL)
 235  FORMAT('  NO.     ID(1)     ID(2)     ID(3)      ID(4)',
     1       '       PLAIN LANGUAGE ID        ISCALD',
     2       '    L--THRESHOLDS--U ITAU NCAT KER ISD   SD   DS',/,
     3      (' ',I4,1X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,2X,
     4       A32,I2,2X,F9.3,F11.3,I4,I5,2I4,F6.2,F5.2))
 250  RETURN
      END 
