      SUBROUTINE OBSTWET(KFILDO,KFIL10,IDPARS,JD,NDATE,
     1                   SDATA,ND1,NSTA,
     2                   IPACK,IWORK,FD1,FD2,FD3,FD4,ND2X3,
     3                   LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,NFETCH,
     4                   IS0,IS1,IS2,IS4,ND7,
     5                   ISTAV,L3264B,IER)
C
C        SEPTEMBER  1999   ALLEN   TDL   MOS-2000
C        MAY        2000   ALLEN   CHANGED THE SUBROUTINE TO USE THE
C			           CORRECTED DEWPOINT RATHER THAN THE
C                                  OBSERVED DEWPOINT.
C        NOVEMBER   2000   RUDACK  MODIFIED CODE TO CONFORM WITH 
C                                  MDL FORMAT SPECIFICATIONS
C        DECEMBER   2002   WEISS   CHANGED ND5 TO ND2X3
C        MAY        2003   GLAHN   CHANGED .AND. TEST TO .OR. ON 
C                                  IDPARS( ) ABOVE 100; CHANGED FORMAT
C                                  100; INDENTATION
C
C        PURPOSE 
C            THIS SUBROUTINE WILL COMPUTE THE AVERAGE OF THE  
C            OBSERVED TEMPERATURE AND THE OBSERVED DEWPOINT.  
C            IT IS MEANT TO SERVE AS A SURROGATE FOR AN 
C            OBSERVED WETBULB TEMPERATURE.
C
C            THE FOLLOWING IDPARS(1) AND IDPARS(2) ARE ACCOMMODATED:
C
C               703 103 - AVG. OF TEMP. AND DEW POINT      
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE 
C                     (OUTPUT). 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS
C                     (INPUT-OUTPUT). 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE
C                       (INPUT). 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM 
C                       ACCESS (INPUT).
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID(J) (J=1,15)
C                       DEFINED IN THE CALLING PROGRAM (INPUT).
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 1 
C                            LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET -- PREVIOUS CYCLE.  
C                            ALWAYS + AND COUNTED BACKWARDS IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C               JD(J) = THE BASIC INTEGER PREDICTOR ID (J=1,4).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE 
C                       PORTIONS PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND THRESH.
C                       JD( ) IS USED TO IDENTIFY THE BASIC MODEL 
C                       FIELDS AS READ FROM THE ARCHIVE (INPUT).
C               NDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED
C                       (INPUT).
C            SDATA(K) = DATA TO RETURN (K=1,NSTA) (OUTPUT) 
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  (INPUT).
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT WITH
C                       (INPUT).
C            IPACK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).  
C            IWORK(J) = WORK ARRAY (J=1,ND2X3) (INTERNAL).  
C              FD1(J) = WORK ARRAY (J=1,ND2X3).  FD1 CONTAINS THE 
C                       OBSERVED TEMPERATURE. (INTERNAL)
C              FD2(J) = WORK ARRAY (J=1,ND2X3). FD2 CONTAINS THE
C                       OBSERVED DEWPOINT. (INTERNAL)
C              FD3(J) = WORK ARRAY (J=1,ND2X3).  USED IN CORDP.
C              FD4(J) = WORK ARRAY (J=1,ND2X3).  USED IN CORDP.
C               ND2X3 = DIMENSION OF IPACK( ), IWORK( ), FD1( ),
C                       FD2( ), FD3( ), AND FD4( ).  (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS), (INPUT-OUTPUT).
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK, 
C                              THIS IS MINUS THE RECORD NUMBER WHERE 
C                              THE DATA START.
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDL GRIB, 1 FOR 
C                              NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN 
C                              RETRIEVED.
C                       L=10 --NUMBER OF THE SLAB IN DIR( , ,L) AND
C                              IN NGRIDC( ,L) DEFINING THE 
C                              CHARACTERISTICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE 
C                              SORTED LIST IN ID( ,N) (N=1,NPRED)
C                              FOR WHICH THIS VARIABLE IS NEEDED, WHEN 
C                              IT IS NEEDED ONLY ONCE FROM 
C                              LSTORE( , ).  WHEN IT IS NEEDED MORE
C                              THAN ONCE, THE VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING 
C                              MSTORE( , ). LATER USED AS A WAY OF 
C                              DETERMINING WHETHER TO KEEP THIS 
C                              VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ), (INPUT). 
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN (INPUT).  
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA 
C                       IDENTIFIED IN LSTORE( , ) (J=1,ND10). WHEN
C                       CORE( ) IS FULL, DATA ARE STORED ON DISK
C                       (OUTPUT).
C                ND10 = DIMENSION OF CORE( ), (INPUT).
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE (INPUT).  
C              NFETCH = INCREMENTED EACH TIME GFETCH IS ENTERED.
C                       IT IS A RUNNING  COUNT FROM THE BEGINNING OF 
C                       THE PROGRAM.  THIS COUNT IS MAINTAINED IN 
C                       CASE THE USER NEEDS IT (DIAGNOSTICS, ETC.). 
C                       (OUTPUT).  
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3)
C                       (INTERNAL).
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+)  
C                       (INTERNAL).
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12)
C                       (INTERNAL).
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4)
C                       (INTERNAL).
C                 ND7 = DIMENSION OF IS0, IS1, IS2, AND IS4. NOT ALL
C                       LOCATIONS ARE USED (INPUT).
C               ISTAV = 1 WHEN THE DATA RETURNED ARE STATION DATA
C                       0 THE DATA RETURNED ARE GRID DATA OR
C                       DATA ARE NOT AVAILABLE FOR RETURN. 
C                       (OUTPUT).
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING 
C                       USED (EITHER 32 OR 64) (INPUT).
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        52 = NWORDS DOES NOT EQUAL NSTA.
C                       103 = IDPARS(1) AND IDPARS(2) NOT ACCOMMODATED
C                             IN THIS SUBROUTINE.
C                       SEE GFETCH FOR OTHER VALUES WHEN IER.NE.0 AND
C                       DATA ARE RETURNED AS MISSING (INTERNAL-OUTPUT)
C
C      ADDITIONAL VARIABLES 
C              IENTER = NUMBER OF TIMES THIS SUBROUTINE IS ENTERED.
C                       (INTERNAL).
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD1( ).  (INTERNAL)
C               MD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       FD2( ).  (INTERNAL)
C           MDPARS(J) = THE PARSED COMPONENTS OF THE PREDICTOR ID USED
C                       IN THE CALL TO CORDP.  SEE IDPARS FOR 
C                       COMPLETE DESCRIPTION.
C               MISSP = PRIMARY MISSING VALUE INDICATOR.  RETURNED AS
C                       ZERO FROM CALLING GFETCH WHEN THERE IS NO
C                       SECONDARY MISSING VALUE (INTERNAL)
C               MISSS = SECONDARY MISSING VALUE INDICATOR.  RETURNED AS
C                       ZERO FROM CALLING GFETCH WHEN THERE IS NO 
C                       SECONDARY MISSING VALUE. (INTERNAL)
C               NPACK = 2 FOR TDL GRIB PACKED DATA; 1 FOR NOT PACKED.
C                       THIS IS RETURNED FROM CALLING GFETCH (INTERNAL)
C               NSLAB = RETURNED FROM GFETCH AS THE VALUE STORED IN
C                       LSTORE(10, ) FOR THE FIRST FIELD.  THIS IS THE
C                       VALUE OF NSLAB RETURNED.  WHEN IER NE 0, THIS
C                       VALUE SHOULD NOT BE USED (OUTPUT).  
C              NTIMES = THE NUMBER OF TIMES, INCLUDING THIS ONE, THAT 
C                       THE RECORD HAS BEEN FETCHED. THIS IS STORED 
C                       IN LSTORE(9, ) (INTERNAL).
C              NWORDS = NUMBER OF WORDS RETURNED IN FD1( ), AND FD2( ).
C                       (INTERNAL).
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            GFETCH,PRSID1,CORDP
C
      IMPLICIT NONE
C
      INTEGER JD(4),IDPARS(15)
      INTEGER IPACK(ND2X3),IWORK(ND2X3)
      INTEGER IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      INTEGER LSTORE(12,ND9)
      INTEGER LD(4),MD(4),MDPARS(15)
      INTEGER KFILDO,KFIL10,NDATE,ND1,NSTA,ISTAV,IENTER,L3264B,
     1        ND2X3,IER,ND9,LITEMS,ND10,NBLOCK,NFETCH,NSLAB,
     2        NTIMES,NWORDS,ND7,MISSP,MISSS,NPACK,J,I 
C
      REAL SDATA(ND1)
      REAL FD1(ND2X3),FD2(ND2X3),FD3(ND2X3),FD4(ND2X3)
      REAL CORE(ND10)
C
      DATA IENTER/0/
      SAVE IENTER
C
      IER=0
      ISTAV=1
C
C        INITIALIZE SDATA( ).
C
      DO I=1,ND1
         SDATA(I)=9999.
      ENDDO
C
C        CHECK IF THE VARIABLE ID REQUESTED IN THE CALL IS 
C        THE TEMP/DEWPOINT AVG  
C
      IF((IDPARS(1).NE.703).OR.(IDPARS(2).NE.103))THEN
	 IER=103
	 WRITE(KFILDO,100)(JD(J),J=1,4),IER
 100     FORMAT(/' ****IDPARS(1) AND IDPARS(2) DO NOT INDICATE ',
     1           'TEMP/DEWPT AVG.',
     2          /'     PREDICTOR ',I9.9,2I10.9,I4.3,
     3           ' NOT ACCOMMODATED IN SUBROUTINE ',
     4           'OBSTWET.  IER=',I4)
         GO TO 900
      ENDIF
C
C           INCREMENT IENTER VARIABLE BY 1.
C
      IENTER=IENTER+1
C
C        SET UP THE LD ARRAY TO HOLD THE ID FOR THE  
C        OBSERVED TEMPERATURE
C
      LD(1)=702000000 + IDPARS(4)
      LD(2)=IDPARS(7)
      LD(3)=IDPARS(9) * 1000000 +IDPARS(12)
      LD(4)=0
C
C        CALL GFETCH TO GET THE OBSERVED TEMPERATURE AND STORE IT
C        IN FD1.
C
      CALL GFETCH(KFILDO,KFIL10,LD,7777,LSTORE,ND9,LITEMS,
     1            IS0,IS1,IS2,IS4,ND7,IPACK,IWORK,FD1,ND2X3,
     2            NWORDS,NPACK,NDATE,NTIMES,CORE,ND10,
     3            NBLOCK,NFETCH,NSLAB,MISSP,MISSS,L3264B,1,
     4            IER)
C
C         CHECK TO SEE IF IER NE 0, AND IF THIS IS THE FIRST
C         PROCESS DATE.  PRINT ERROR MESSAGE IF NEEDED.
C
       IF((IER.NE.0).AND.(IENTER.EQ.1)) THEN
          WRITE(KFILDO,140)NDATE
 140      FORMAT(/' ****ERROR FROM GFETCH OCCURRED ON',
     1            ' 1ST PROCESS DATE - ',I12,
     2            '.  OBSTWET CANNOT RUN, PLEASE READ WRITE UP.')
	  GO TO 900
       ENDIF
C
       IF(IER.NE.0) GO TO 900
C
C      IF NWORDS DOES NOT EQUAL NSTA, SET ALL VALUES TO MISSING.
C
       IF(NWORDS.NE.NSTA)THEN
          IER=52
	  WRITE(KFILDO,160)NWORDS,NSTA,IER
 160      FORMAT(/' ****NWORDS =',I6,' RETURNED FROM GFETCH',
     1            ' NOT EQUAL TO NSTA =',I6,
     2            ' IN OBSTWET.  DATA SET TO MISSING.',
     3            ' IER =',I4)
	  GO TO 900
       ENDIF
C
C        SET UP THE MD ARRAY TO HOLD THE ID FOR THE  
C        CORRECTED OBSERVED DEWPOINT
C
      MD(1)=703102000 + IDPARS(4)
      MD(2)=IDPARS(7)
      MD(3)=IDPARS(9) * 1000000 +IDPARS(12)
      MD(4)=0
C
C        CALL CORDP TO GET THE CORRECTED OBSERVED DEWPOINT AND 
C        STORE IT IN FD2.
C
      CALL PRSID1(KFILDO,MD,MDPARS)
      CALL CORDP(KFILDO,KFIL10,MDPARS,MD,
     1	         NDATE,FD2,ND1,NSTA,
     2           IPACK,IWORK,FD3,FD4,ND2X3,
     3           LSTORE,ND9,LITEMS,CORE,ND10,NBLOCK,
     4           NFETCH,IS0,IS1,IS2,IS4,ND7,ISTAV,
     5           L3264B,IER)
C
C         CHECK TO SEE IF IER NE 0.
C
      IF(IER.NE.0) THEN
         WRITE(KFILDO,141)NDATE
 141     FORMAT(/' ****ERROR FROM CORDP OCCURRED ON',I12,
     1           '.  OBSTWET CANNOT RUN, PLEASE READ WRITE UP.')
	 GO TO 900
      ENDIF
C
C        COMPUTE THE AVERAGE OF THE TEMPERATURE AND THE DEWPOINT AND
C        PUT IT IN SDATA
C
      DO J=1,NSTA
C
         IF((NINT(FD1(J)).EQ.9999).OR.(NINT(FD2(J)).EQ.9999))THEN
	    SDATA(J)=9999.
         ELSE
	    SDATA(J)=(FD1(J)+FD2(J))/2.
	 ENDIF 
C
      ENDDO
C
      GO TO 910
C
C        IF THERE WAS AN ERROR IN GFETCH, THE CODE WILL JUMP TO
C        HERE.  SET SDATA TO MISSING BEFORE RETURNING. 
C
 900  DO J=1,ND1
	 SDATA(J)=9999.
      END DO
C
 910  RETURN
      END     
