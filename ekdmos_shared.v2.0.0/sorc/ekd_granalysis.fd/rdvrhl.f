      SUBROUTINE RDVRHL(KFILDO,KFILSL,FILESL,IP14,
     1                  CCALL,NAME,VRAD,ELEVLO,ELEVHI,R,
     2                  NPROJ,ORIENT,BMESH,XLAT,ALATL,ALONL,
     3                  ND1,NSTA,ISTOP,IER)   
C
C        OCTOBER   2007   GLAHN   MDL   MOS-2000
C        FEBRUARY  2008   GLAHN   ADDED GO TO 119 BELOW 137
C        MARCH     2008   COSGROVE   ADDED COMMAS TO FORMATS 132,137,
C                                 AND 145 FOR IBM COMPILE
C        MAY       2008   GLAHN   MODIFIED TO BE CALLED FROM U405A
C                                 TO BE SPECIFIC TO ELEMENT
C        MAY       2008   GLAHN   MODIFIED DIAGNOSTIC WRITING; ADDED
C                                 IWRIT1 AND IWRIT2
C        JUNE      2008   GLAHN   ADDED READING AND PRINTING  OF FILE
C                                 NAME ON UNIT KFILSL
C        OCTOBER   2008   COSGROVE   ADDED COMMAS TO FORMATS FOR IBM
C                                 COMPILE
C        MARCH     2009   GLAHN   ADDED TO DIAGNOSTIC 145
C        MAY       2009   GLAHN   UPDATED FOR EXPANDED U178 FILE ID
C        MAY       2009   GLAHN   ADDED NPROJ,ORIENT,BMESH,XLAT,ALATL,
C                                 AND ALONL TO CALL, MODIFIED INFO
C                                 READ FROM RADII FILE, AND DID SOME
C                                 CHECKING OF CONSISTENCY OF INPUTS.
C        JULY      2009   GLAHN   MODIFIED TO TRANSFORM 5 KM RADII
C                                 TO 3 KM BASED ON BMESH AND BMESHI
C        SEPTEMBER 2009   GAW     MODIFIED OPEN STATEMENT FOR OPERATIONS
C
C        PURPOSE
C            READS A FILE CONTAINING CALL LETTERS, SIX RADII OF
C            INFLUENCE FOR U155, AND THE HIGHEST AND LOWEST TERRAIN
C            ELEVATIONS WITHIN THE LARGEST R FOR EACH STATION.  THE
C            ELEVATIONS ARE USED IN DETERMINING THE LAPSE RATE FROM
C            UPPER AIR DATA IN U405 AS NEEDED.  THE DATA READ ARE ASCII
C            SO THAT EDITING CAN BE DONE AS NEEDED.  
C            (REPLACES RDVRAD)
C   
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C            IP14      - UNIT NUMBER FOR WRITING LIST OF STATIONS AND
C                        ASSOCIATED RADII AND ELEVATIONS.  (OUTPUT)
C            KFILSL    - UNIT NUMBER FOR READING THE STATIONS AND
C                        ASSOCIATED RADII AND ELEVATIONS.  (INPUT)
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              KFILSL = UNIT NUMBER FOR READING THE STATIONS AND
C                       ASSOCIATED RADII AND ELEVATIONS.  (INPUT)
C              FILESL = NAME OF DATA SET FOR READING THE VARIABLE
C                       RADII AND HI AND LO ELEVATIONS.
C                       (CHARACTER*60)  (INPUT)
C                IP14 = UNIT NUMBER FOR WRITING LIST OF STATIONS AND
C                       ASSOCIATED RADII AND ELEVATIONS.  (INPUT)
C            CCALL(K) = IDENTIFIERS OF STATIONS BEING USED (K=1,NSTA).
C                       (CHARACTER*8)  (INPUT)
C             NAME(K) = NAMES OF STATIONS BEING USED (K=1,NSTA).  USED
C                       FOR PRINTING ONLY.  (CHARACTER*20)  (INPUT)
C           VRAD(K,L) = RADII READ FROM UNIT NO. KFILSL 
C                       (K=1,NSTA) (L=1,6).  (OUTPUT)
C           ELEVLO(K) = THE LOW ELEVATION ASSOCIATED WITH STATION
C                       CCALL(K) READ FROM UNIT NO. KFILSL 
C                       (K=1,NSTA).  (OUTPUT)
C           ELEVHI(K) = THE HIGH ELEVATION ASSOCIATED WITH STATION
C                       CCALL(K) READ FROM UNIT NO. KFILSL 
C                       (K=1,NSTA).  (OUTPUT)
C                R(J) = RADIUS OF INFLUENCE FOR EACH PASS J (J=1,NPASS)
C                       FOR THE FIRST GUESS OPTION BEING USED IN TERMS
C                       OF MESH GRID UNITS BEING USED ON THAT PASS.
C                       THIS IS USED IN CASE THERE IS A STATION WITH
C                       NO VARIABLE LIST IN VRAD( , ).  (INPUT)
C               NPROJ = NUMBER OF MAP PROJECTION TO WHICH THIS GRID
C                       APPLIES.
C                       3 = LAMBERT.
C                       5 = POLAR STEREOGRAPHIC.
C                       7 = MERCATOR.
C                       (INPUT)
C              ORIENT = ORIENTATION OF GRID IN WEST LONGITUDE.  (INPUT)
C               BMESH = ACTUAL MESH LENGTH CORRESPONDING TO MESHB.
C                       (INPUT)
C                XLAT = NORTH LATITUDE AT WHICH GRIDLENGTH IS SPECIFIED
C                       IN DEGREES.  (INPUT)
C               ALATL = NORTH LATITUDE OF LOWER LEFT CORNER POINT
C                       OF A GRID OF THE SIZE  NXL, NYL.  TRUNCATED
C                       TO TEN THOUSANDTHS OF DEGREES.  NOTE THAT THE
C                       MOS-2000 ARCHIVE IS ONLY TO THOUSANDTHS OF
C                       DEGREES.  (INPUT)
C               ALONL = WEST LONGITUDE OF LOWER LEFT CORNER POINT
C                       OF A  GRID OF THE SIZE  NXL, NYL.  TRUNCATED
C                       TO TEN THOUSANDTHS OF DEGREES.  NOTE THAT THE
C                       MOS-2000 ARCHIVE IS ONLY TO THOUSANDTHS OF
C                       DEGREES.  (INPUT)
C                 ND1 = DIMENSION OF CCALL( ), NAME( ), VRAD( ),
C                       ELEVLO( ), AND ELEVHI( ).  (INPUT)
C                NSTA = NUMBER OF VALUES IN CCALL( ) AND NAME( ).
C                       (INPUT)
C               ISTOP = INCREMENTED BY ONE WHEN AN ERROR OCCURS.
C                       (INPUT/OUTPUT)
C                 IER = STATUS RETURN.
C                       0   = GOOD RETURN.
C                       777 = ERROR.
C                       (OUTPUT)
C               STATE = VARIABLE SET TO STATEMENT NUMBER TO INDICATE
C                       WHERE AN ERROR OCCURRED.  (CHARACTER*4) 
C                       (INTERNAL)
C              MAXSTA = THE MAXIMUM NUMBER OF STATIONS DEFINED IN
C                       U178 TO AFFECT A SPECIFIC GRIDPOINT NO MORE
C                       THAN XDIST GRID LENGTHS AWAY.  READ FROM
C                       UNIT NO. KFILSL.  (INTERNAL)
C               XDIST = SEE MAXSTA.  (INTERNAL)
C              IFIRST = CONTROLS PRINTING AND SPACING OF DIAGNOSTICS.
C                       (INTERNAL)
C              IWRIT1 = CONTROLS WRITING OF DIAGNOSTIC AT 137.
C                       A STATION LIST MAY BE USED (ESPECIALLY WHEN
C                       MULTIPLE ELEMENTS ARE ANALYZED IN ONE RUN)
C                       THAT IS LARGER THAN THE LIST USED TO CREATE
C                       THE RADII LIST.  THIS COULD CREATE MANY WRITES.
C                       ONLY ONE INSTANCE IS WRITTEN PER ENTRY WITH
C                       A ****, AND IS NOT COUNTED AS AN ERROR.
C              ASCIFM = NAME OF STATION LIST FILE FROM WHICH THE
C                       U178 RUN WAS MADE.  READ FROM UNIT NO KFILSL.
C                       (INTERNAL)
C              IWRIT2 = CONTROLS WRITING OF DIAGNOSTIC.  (INTERNAL)         
C        1         2         3         4         5         6         7 X
C
      CHARACTER*4 STATE
      CHARACTER*8 CCALL(NSTA),CCALLD
      CHARACTER*20 NAME(NSTA)
      CHARACTER*60 FILESL,SAVFL,ASCIFM,TOSS
C
      DIMENSION VRAD(ND1,6),ELEVHI(ND1),ELEVLO(ND1)
      DIMENSION TRASH(6),R(6)
C
      DATA SAVFL/' '/
C
      SAVE SAVFL
C
      IER=0
      IFIRST=0
      IWRIT1=0
      IWRIT2=0
C
CD     WRITE(KFILDO,100)ND1,NSTA,KFILSL,IP14,FILESL
CD100  FORMAT(/' AT 100 IN RDVRHL--ND1,NSTA,KFILSL,IP14',4I6,1X,A60)
C
C        OPEN FILE IF NECESSARY.
C
COPS  NOTE THAT THIS CHECK CAUSES A PROBLEM IN OPERATIONS IF YOU DO
COPS  OUR STANDARD PROCEDURE OF JUST HAVING A UNIT NUMBER WITH NO FILENAME.
COPS  IT WILL NEVER OPEN ANY RADII FILE.  SO PUT SOME DUMMY STRING IN FOR THE
COPS  FILENAME SO THAT IT DOESN'T MATCH SAVFL WHICH IS SET TO BLANK
C
      IF(FILESL.EQ.SAVFL)THEN
         WRITE(KFILDO,101)FILESL
 101     FORMAT(/' SAME RADII FILE USED AS PREVIOUS = ',A60)
         GO TO 160
      ENDIF
C
      STATE='102 '
COPS      OPEN(UNIT=KFILSL,FILE=FILESL,STATUS='OLD',
      OPEN(UNIT=KFILSL,STATUS='OLD',
     1     IOSTAT=IOS,ERR=900)
      SAVFL(1:60)=FILESL(1:60)
C        FILE NAME IS SAVED SO THAT RDVRHL WON'T HAVE TO BE EXECUTED
C        AGAIN UNLESS A DIFFERENT RADII FILE IS USED AS FOR THE
C        PREVIOUS ENTRY.
      WRITE(KFILDO,102)KFILSL,FILESL
 102  FORMAT(/,' OPENING OLD FILE ON UNIT NO.',I3,' FILE = ',A60)
C
C        INITIALIZE VRAD( , ), ELEVHI( ), AND ELEVLO( ).
C
      DO 105 K=1,NSTA
C
      DO 104 L=1,6
      VRAD(K,L)=0.
 104  CONTINUE
C
      ELEVHI(K)=999999.
      ELEVLO(K)=999999.      
 105  CONTINUE
C
C        SKIP FILE IDENTIFICATION.
C
      STATE=' 109'
      READ(KFILSL,109,IOSTAT=IOS,ERR=900,END=1200)TOSS
 109  FORMAT(A60)
CCC      WRITE(KFILDO,1090)TOSS
CCC 1090 FORMAT(/' TOSS = ',A60)
      
C        THE ABOVE SKIPS THE DATE TIME STAMP.  
C
      STATE=' 110'
      READ(KFILSL,110,IOSTAT=IOS,ERR=900,END=1200)NPROJI,ORIENTI,
     1         BMESHI,XLATI,ALATLI,ALONLI,NXI,NYI,MAXSTA,XDIST,ASCIFM
 110  FORMAT(20X,I8/
     1       20X,F8.0/
     2       20X,F8.0/
     3       20X,F8.0/
     4       20X,F8.0/
     5       20X,F8.0/
     5       20X,I8/
     6       20X,I8/
     7       20X,I8/
     8       20X,F8.0/
     9       12X,A60)  
C
      WRITE(KFILDO,112)MAXSTA,XDIST,BMESHI,ASCIFM
 112  FORMAT(/,' VARIABLE RADIUS LIST READ FOR MAXSTA =',I6,
     1         '  AND MAXIMUM DISTANCE =',F10.1,
     2         '  AT ACTUAL GRID LENGTH =',F9.4,/,
     3         ' PREPARED BY U178 FROM STATION LIST FILE NAME  ',A60)
C
C        CHECK INPUTS WITH WHAT IS EXPECTED.
C
      IF(NPROJ.NE.NPROJI.OR.
     1   ORIENT.NE.ORIENTI.OR.
     2   XLAT.NE.XLATI.OR.
     3   ABS(ALATL-ALATLI).GT..001.OR.
     4   ABS(ALONL-ALONLI).GT..001)THEN
         WRITE(KFILDO,1125)NPROJ,NPROJI,ORIENT,ORIENTI,BMESH,BMESHI,
     1                    XLAT,XLATI,ALATL,ALATLI,ALONL,ALONLI
 1125    FORMAT(/' ****VALUES READ FROM RADII FILE IN RDVRHL ARE NOT',
     1           '     WHAT ARE EXPECTED.'/
     2           '     NPROJ  =',I10,  '    NPROJI  FROM FILE =',I10/
     3           '     ORIENT =',F10.4,'    ORIENTI FROM FILE =',F10.4/
     4           '     BMESH  =',F10.4,'    BMESHI  FROM FILE =',F10.4/
     5           '     XLAT   =',F10.4,'    XLATI   FROM FILE =',F10.4/
     6           '     ALATL  =',F10.4,'    ALATLI  FROM FILE =',F10.4/
     7           '     ALONL  =',F10.4,'    ALONLI  FROM FILE =',F10.4)
         ISTOP=ISTOP+1
         IER=777
C           THIS IS CONSIDERED A FATAL ERROR.
         GO TO 160
      ELSEIF(ABS(BMESH-BMESHI).GT..001)THEN
         WRITE(KFILDO,1127)BMESH
 1127    FORMAT(' VARIABLE RADII BEING INCREASED.  RADII COMPUTED',
     1          ' ON BASIS OF NOMINAL 5 KM ACCEPTED FOR',F8.4,' KM.')
      ENDIF
C         
      IF(IP14.NE.0)THEN
         WRITE(IP14,113)
 113     FORMAT(' ')
         WRITE(IP14,114)MAXSTA,XDIST
 114     FORMAT(' RADII AND MAXIMUM AND MINIMUM ELEVATIONS',
     1          ' FOR STATIONS FOR EACH GRIDPOINT TO BE AFFECTED BY',
     2          ' UP TO',I5,' STATIONS',/,
     3          ' WITHIN A RADIUS OF',F7.0,' GRIDLENGTHS.',/)
         WRITE(IP14,115)
 115     FORMAT('             STATION',51X,
     1          'RADII',25X,'MAX ELEVATION   MIN ELEVATION')
      ENDIF
C
      ISTART=1
      IEND=NSTA
C        ISTART AND IEND ARE FOR SEARCHING IN LOOP BELOW.
C
      STATE=' 119'
 119  READ(KFILSL,120,IOSTAT=IOS,ERR=900,END=1200)CCALLD,
     1                         (TRASH(J),J=1,6),TRLO,TRHI
 120  FORMAT('   ',A8,2X,6F12.2,2F8.0)
      GO TO 122
C
 1200 WRITE(KFILDO,1201)KFILSL
 1201 FORMAT(/,' ****PREMATURE END OF VARIABLE RADIUS AND',
     1         '  ELEVATION LIST.  NO TERMINATOR FOUND ON',
     2         ' FILE KFILSL =',I4)
      ISTOP=ISTOP+1
      IER=777
C        THIS IS CONSIDERED A FATAL ERROR.
      GO TO 160
C
C        FIND STATION IN LIST.
C
 122  IF(CCALLD.EQ.'99999999')GO TO 140
C        THIS IS THE TERMINATOR.
C
 130  DO 135 K=ISTART,IEND
C
CCC      WRITE(KFILDO,1300)ISTART,IEND,K,CCALLD,CCALL(K)
CCC 1300 FORMAT(' AT 1300 IN RDVRHL--ISTART,IEND,K,CCALLD,CCALL(K)',
CCC     1       3I6,2X,A8,2X,A8)
C
      IF(CCALLD.EQ.CCALL(K))THEN
C
         DO 131 L=1,6
         VRAD(K,L)=TRASH(L)
 131     CONTINUE
C
         ELEVLO(K)=TRLO
         ELEVHI(K)=TRHI
         ISTART=K
C           SET TO K NOT K+1, TO AVOID DIFFICULTY AT END OF LIST.
C
         IF(IP14.NE.0)THEN
            WRITE(IP14,132)CCALL(K),NAME(K),(VRAD(K,L),L=1,6),
     1                     ELEVHI(K),ELEVLO(K)
 132        FORMAT('   ',A8,2X,A20,2X,6F12.2,2F8.0)
         ENDIF
C
         GO TO 119
      ENDIF
C
 135  CONTINUE
C
      IF(ISTART.NE.1)THEN
         ISTART=1
         GO TO 130
      ELSE
         IF(IWRIT1.EQ.0)THEN
            IWRIT1=IWRIT1+1
            WRITE(KFILDO,137)CCALLD
 137        FORMAT(/' ****IDENTIFIER ',A8,'IN RADII FILE NOT FOUND IN',
     2              ' STATION LIST.',
     3              '  MAY NOT BE AN ERROR.  THIS DIAGNOSTIC WILL NOT',
     4              ' BE PRINTED AGAIN.')
         ELSE
            IWRIT1=IWRIT1+1
         ENDIF        
C
         GO TO 119
      ENDIF
C
 140  IF(IWRIT1.GT.0)THEN
         WRITE(KFILDO,141)IWRIT1
 141     FORMAT('     THERE WERE ',I7,' SUCH IDENTIFIERS NOT FOUND.')
      ENDIF
C
CD     DO 143 K=1,NSTA
CD     WRITE(KFILDO,142)K,CCALL(K),(VRAD(K,J),J=1,6),ELEVLO(K),ELEVHI(K)
CD142  FORMAT(' RDVRHL-K,CCALL(K),(VRAD(K,J),J=1,6),ELEVLO(K),ELEVHI(K)',
CD    1        I6,2X,A8,F5.1,5F7.1,2F8.0)
CD143  CONTINUE
C
C        CHECK TO SEE WHETHER ALL STATIONS HAVE DATA READ IN.
C
      DO 150 K=1,NSTA
C
      IF(ELEVLO(K).GT.999998.)THEN
         IWRIT2=IWRIT2+1
C
         IF(IWRIT2.LE.2)THEN
            IF(IFIRST.EQ.0)THEN
               WRITE(KFILDO,144)CCALL(K),NAME(K)
 144           FORMAT(/' ****STATION  ',A8,2X,A20,'  DOES NOT HAVE',
     1                 ' RADII AND HI AND LO ELEVATIONS ON FILE.',
     2                 '  USE R( ) INSTEAD.')
               IFIRST=IFIRST+1
               ISTOP=ISTOP+1
            ELSE
               WRITE(KFILDO,145)CCALL(K),NAME(K)
 145           FORMAT(' ****STATION  ',A8,2X,A20,'  DOES NOT HAVE',
     1                ' RADII AND HI AND LO ELEVATIONS ON FILE.',
     2                '  USE R( ) INSTEAD.',/,
     3                '     THIS DIAGNOSTIC WILL NOT PRINT AGAIN.',
     4                '  COUNTED AS ONE ISTOP ERROR.')
            ENDIF
C
         ENDIF
C
         IF(IP14.NE.0.AND.IP14.NE.KFILDO)THEN
C 
            IF(IFIRST.LE.1)THEN
               WRITE(IP14,144)CCALL(K),NAME(K)
               IFIRST=IFIRST+1
            ELSE
               WRITE(IP14,144)CCALL(K),NAME(K)
            ENDIF
C
         ENDIF
C
C          IF THE STATION IN THE CCALL( ) LIST DOES NOT HAVE A
C          VARIABLE RADIUS, USE THE GENERIC ONE IN R( ).
C
         DO 148 L=1,6
         VRAD(K,L)=R(L)
 148     CONTINUE
C
      ELSE
C    
C           IT IS ASSUMED FOR THE CONUS AREA (NPROJ = 3 FOR LAMBERT),
C           THE RADII ARE CALCULATED BY U178 AT NOMINAL 5 KM, AND IF
C           THIS RUN IS FOR 3 OR 1.5 KM, THE RADII ARE MULTIPLIED
C           2 OR 4, RESPECTIVELY.  THIS KEEPS RUNNING OF U178 SIMPLER.
C           NOTE THAT THE DEFAULT RADII FROM U405A.CN ARE NOT MODIFIED.
C 
         IF(NPROJ.EQ.3.AND.(BMESHI.GT.4.AND.BMESHI.LT.6))THEN
C           THIS IS CONUS LAMBERT AND THE U178 INPUT IS 5 KM.
C           THE U178 INPUT COULD BE AT 3 OR 1.5 KM, AND THEN THE
C           RADII WOULD NOT BE MODIFIED.
C
            IF(BMESH.GT.2.AND.BMESH.LT.3)THEN
C
               DO 149 L=1,6
               VRAD(K,L)=VRAD(K,L)*2.
 149           CONTINUE
C
            ELSEIF(BMESH.GT.1.AND.BMESH.LT.2)THEN
C
               DO 1490 L=1,6
               VRAD(K,L)=VRAD(K,L)*4.
 1490          CONTINUE
C
            ENDIF
C
         ENDIF
C
      ENDIF
C
 150  CONTINUE
C
      IF(IWRIT2.GT.0)THEN
         WRITE(KFILDO,151)IWRIT2
 151     FORMAT('     THERE WERE ',I7,' SUCH IDENTIFIERS NOT FOUND',
     1          ' WITH RADII.')
      ENDIF
C
 160  CLOSE(UNIT=KFILSL)
C        FILE IS CLOSED BECAUSE ROUTINE MAY BE ENTERED AGAIN TO
C        USE THE SAME UNIT NUMBER.
      RETURN
C 
C        ERROR STOP BELOW IS FOR ERRORS OF CONTROL INFORMATION INPUT.
C
 900  CALL IERX(KFILDO,KFILDO,IOS,'RDVRHL',STATE)
      WRITE(KFILDO,901)
 901  FORMAT('     FATAL SYSTEM ROUTINE ERROR IN RDVRHL.')
      ISTOP=ISTOP+1
      IER=777
      GO TO 160
      END
