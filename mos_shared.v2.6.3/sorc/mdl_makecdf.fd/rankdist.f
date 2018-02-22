      SUBROUTINE RANKDIST(KFILDO,KFIL10,KFILAO,IP12,IP15,
     1                  KFILRA,RACESS,NUMRA,
     2                  ID,IDPARS,JD,TRESHL,TRESHU,ITAU,NVRBL,
     3                  NDATE,CCALL,ISDATA,SDATA,ND1,NSTA,
     4                  XDATA,SDDATA,ND2,KER,NN,M,
     5                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     6                  LSTORE,ND9,LITEMS,CORE,ND10,
     7                  NBLOCK,NFETCH,CDF,ND11,NPCDF,CDFTH,
     8                  XCDF,NCDFTH,
     9                  IS0,IS1,IS2,IS4,ND7,
     A                  L3264B,L3264W,ISTOP,IER)
C
C        NOVEMBER  2011   VEENHUIS    MDL - CREATED 
C        FEBRUARY  2014   RUDACK      MDL - ADDED WIND SPEED 
C        AUGUST    2014   RUDACK      ADDED TEMPERATURE AND DEWPOINT
C        PURPOSE
C            USED TO COMPUTE THE CDF FROM THE RAW ENSEMBLES SO WE MAKE A 
C            COMPARISON WITH EKDMOS FORECASTS.
C   
C        DATA SET USE 
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C            KFIL10    - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM
C                        ACCESS.  (INPUT-OUTPUT) 
C            KFILAO    - UNIT NUMBER OF ASCII OUTPUT FILE.  ZERO MEANS
C                        OUTPUT WILL NOT BE WRITTEN.  (OUTPUT)
C            IP12      - LIST OF STATIONS ON THE INPUT FILES.  (OUTPUT)
C            IP15      - LIST OF DATA IN DIST.  (OUTPUT)
C            KFILRA(J) - UNIT NUMBERS FOR EXTERNAL RANDOM ACCESS FILES
C                        (J=1,5).  (INPUT)
C 
C        VARIABLES 
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C              KFILAO = UNIT NUMBER OF ASCII OUTPUT FILE.
C                       ZERO MEANS OUTPUT WILL NOT BE WRITTEN.  (INPUT)
C                IP12 = INDICATES WHETHER (>0) OR NOT (=0) THE LIST OF
C                       STATIONS ON THE EXTERNAL RANDOM ACCESS FILES
C                       WILL BE LISTED TO UNIT IP12.  (INPUT)
C                IP15 = LIST OF DATA IN DIST.  (INPUT)
C           KFILRA(J) = THE UNIT NUMBERS FOR THE MOS-2000 EXTERNAL
C                       RANDOM ACCESS FILES (J=1,NUMRA)
C           RACESS(J) = THE FILE NAME FOR THE MOS-2000 EXTERNAL RANDOM
C                       ACCESS FILE (J=1,NUMRA).  (CHARACTER*60)
C               NUMRA = THE NUMBER OF VALUES IN KFILRA( ) AND RACESS( ).
C                       (INPUT)
C             ID(J,N) = THE VARIABLE ID (J=1,4) (N=1,NVRBL).  (INPUT)
C         IDPARS(J,N) = THE PARSED, INDIVIDUAL COMPONENTS OF THE
C                       VARIABLE ID CORRESPONDING TO ID( ) (J=1,15)
C                       (N=1,NVRBL).
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY
C                            1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK
C                            IN TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C                       (INPUT)
C             JD(J,N) = THE BASIC INTEGER VARIABLE ID (J=1,4)
C                       (N=1,NVRBL).  THIS IS THE SAME AS ID(J), EXCEPT
C                       THAT THE FOLLOWING PORTIONS ARE OMITTED:
C                       B = IDPARS(3),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       (INPUT)
C           TRESHL(N) = THE LOWER BINARY THRESHOLD ASSOCIATED WITH 
C                       IDPARS( ,N) (N=1,NVRBL).  (INPUT)
C           TRESHU(N) = THE UPPER BINARY THRESHOLD ASSOCIATED WITH
C                       IDPARS( ,N) (N=1,NVRBL).  (INPUT)
C             ITAU(N) = THE NUMBER OF HOURS AHEAD TO FIND A VARIABLE
C                       (N=1,NVRBL).  THIS DOES NOT APPLY TO ALL
C                       SUBROUTINES.  NO PRESENT USE; SHOULD BE ZERO.
C                       (INPUT)
C               NVRBL = THE NUMBER OF VARIABLES IN ID( , ), ETC.
C                       (INPUT)
C               NDATE = THE DATE/TIME FOR WHICH VARIABLE IS NEEDED.
C                       (INPUT)
C            CCALL(K) = 8-CHARACTER STATION CALL LETTERS (OR GRIDPOINT
C                       LOCATIONS FOR GRID DEVELOPMENT) TO PROVIDE
C                       OUTPUT FOR (K=1,NSTA).  ALL STATION DATA ARE
C                       KEYED TO THIS LIST.  (CHARACTER*8)  (INPUT)
C           ISDATA(K) = WORK ARRAY (K=1,ND1). (INTERNAL)
C            SDATA(K) = WORK ARRAY (K=1,ND1).  (INTERNAL)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT
C                       WITH.  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C          XDATA(K,L) = THE ARRAY USED FOR VECTOR VALUES (K=1,ND1) 
C                       (L=1,ND2).  THE COLUMNS HOLD THE VALUES FOR
C                       THE M MEMBERS (SEE M BELOW).
C                       (INTERNAL/OUTPUT)
C         SDDATA(K,L) = USED FOR THE STANDARD ERRORS (K=1,ND1)
C                       (L=1,ND2).  THE COLUMNS HOLD THE SD'S FOR THE
C                       M MEMBERS (SEE M BELOW).  (INTERNAL/OUTPUT)
C                 ND2 = MAXIMUM NUMBER OF ENSEMBLE MEMBERS.  (INPUT)
C              KER(N) = DESIGNATES THE KERNAL TO BE USED FOR VARIABLE N
C                       (N=1,ND4).
C                       1 = NORMAL (GAUSIAN).
C                       (INPUT)
C                  NN = ON INPUT, THE FIRST VARIABLE IN THE ID LIST NOT
C                       ALREADY USED.  THIS IS THE VARIABLE TO PROCESS.
C                       ON OUTPUT, THE FIRST VARIABLE IN THE ID LIST NOT
C                       USED.  WHEN ALL VARIABLES HAVE BEEN PROCESSED,
C                       NN IS RETURNED = 9999.  (INPUT/OUTPUT)
C                   M = THE NUMBER OF MEMBERS AVERAGED IN DISTF.
C                       (OUTPUT)
C         ICALLD(L,K) = 8 STATION CALL LETTERS AS CHARACTERS IN AN 
C                       INTEGER VARIABLE (L=1,L3264W) (K=1,ND1).
C                       NOTE THAT THIS REQUIRES TWO 32-BIT WORDS TO HOLD
C                       THE DESCRIPTION BUT ONLY ONE 64-BIT WORD.
C                       NEEDED IN CONST FOR ARGUMENT TO RDTDLM.
C                       EQUIVALENCED TO CCALLD( ).
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,ND5).  EQUIVALENCED
C                       TO ICALLD( , ).  (INTERNAL)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ), WORK( ), DATA( ), AND
C                       CALLD( ), AND SECOND DIMENSION OF ICALLD( , ).
C                       (INPUT)
C         LSTORE(L,J) = THE ARRAY TO HOLD INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS).
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK, 
C                              THIS IS US THE RECORD NUMBER WHERE 
C                              THE DATA START.  NOTE THAT WHEN A FIELD 
C                              CANNOT BE STORED IN CORE( ), IT IS PUT
C                              ON DISK.  IT MAY BE THAT A LATER FIELD 
C                              WILL FIT, AND IT IS PUT IN CORE( ).
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDLPACK, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --THE NUMBER IN THE LIST OF INPUT SOURCES
C                              THIS VARIABLE CAME FROM.
C                       L=11 --FOR U715, THIS WILL BE 7777, INDICATING
C                              THE VARIABLE IS ALWAYS STORED IN THE
C                              INTERNAL STORAGE FACILITY.
C                       L=12 --US THE NUMBER OF HOURS THIS VARIABLE
C                              MUST BE KEPT.  LATER SET TO A DATE WHEN
C                              THIS VARIABLE CAN BE DISCARDED.
C                       (INPUT)
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       FILLED.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETRIEVE THE DATA
C                       IDENTIFIED IN LSTORE( , ) (J=1,ND10).  WHEN
C                       CORE( ) IS FULL DATA ARE STORED ON DISK.
C                       (INPUT)
C                ND10 = DIMENSION OF CORE( ).  (INPUT)
C              NBLOCK = THE BLOCK SIZE IN WORDS OF THE MOS-2000 RANDOM
C                       DISK FILE.  (INPUT)
C              NFETCH = THE NUMBER OF TIMES GFETCH HAS BEEN ENTERED.
C                       GFETCH KEEPS TRACK OF THIS AND RETURNS THE
C                       VALUE.  (OUTPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,3).
C                       (INTERNAL)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,22+).
C                       (INTERNAL)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).
C                       (INTERNAL)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4). 
C                       (INTERNAL)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  (INPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C              L3264W = NUMBER OF WORDS IN 64 BITS (EITHER 1 OR 2).  
C                       CALCULATED BY PARAMETER, BASED ON L3464B.
C                       (INPUT)
C                NDIM = THE SIZE OF ITABLE.
C        ITABLE(NDIM) = CONTAINS A LIST OF VALID IDS FOR THIS SUBROUTINE.
C            ISTOP(J) = FOR J=1, ISTOP IS INCREMENTED BY 1 EACH TIME
C                       AN ERROR OCCURS THAT MAY BE FATAL.
C                       FOR J=2, ISTOP IS INCREMENTED BY 1 WHENEVER AN
C                       INPUT DATA RECORD IS NOT FOUND.
C                       FOR J=3, ISTOP IS INCREMENTED BY 1 WHENEVER AN
C                       AN UNUSUAL CIRCUMSTANCE OCCURS WHICH IS NOT FATAL.
C                       (INPUT/OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                         3 = ATTEMPTED TO PROCESS ID NOT ITABLE.
C                             MOST LIKELY THE KERNAL FLAG WAS SET
C                             INCORRECTLY.
C                       777 = SD = 0 FOUND IN KERNEL.
C                       OTHER VALUES CAN COME FROM CALLED SUBROUTINES.
C                       (OUTPUT)
C               MDATE = NDATE UPDATED WITH ITAU( ).  NO REASON FOR
C                       ITAU( ) TO BE OTHER THAN ZERO.  NEEDED FOR 
C                       RETVEC.  (INTERNAL)
C                  MM = THE NUMBER OF VALUES IN RDATA( ) AND SDATA( ).
C                       THIS IS THE NUMBER OF ENSEMBLES BEING 
C                       PROCESSED.  (INTERNAL)
C 
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES USED 
C            TIMPR, PRSID1, RETVEC, KERNEL
C
      PARAMETER(NDIM=7)
      CHARACTER*8 CCALL(ND1)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
      INTEGER TID
C
      DIMENSION ISDATA(ND1),SDATA(ND2),SUMPRB(ND1),MEMCNT(ND1)
      DIMENSION XDATA(ND1,ND2),SDDATA(ND1,ND2)
      DIMENSION ID(4,NVRBL),IDPARS(15,NVRBL),
     1          TRESHL(NVRBL),TRESHU(NVRBL),JD(4,NVRBL),ITAU(NVRBL),
     2          KER(NVRBL)
      DIMENSION ICALLD(L3264W,ND5),IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION CDFTH(NCDFTH),XCDF(ND1,NCDFTH)
      DIMENSION CDF(ND11) 
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10)
      DIMENSION KFILRA(5),LD(4),LDPARS(15),ISTOP(3)
      DIMENSION ITABLE(NDIM),IFLAG(ND2),XSORTED(ND2)
      DATA JFIRST/0/
      SAVE JFIRST,NDATES
C        NDATE IS SAVED IN NDATES ON THE FIRST ENTRY SO THAT
C        DIAGNOSTIC 212 WON'T PRINT AFTER THE FIRST DAY.
C
      IF(JFIRST.EQ.0)THEN
         NDATES=NDATE
         JFIRST=1
      ENDIF
C
C        ITABLE IS THE LIST OF IDS THAT CAN BE HANDLED.
C        THIS IS FOR SAFETY
C
      DATA ITABLE /2020200,2021400,2022400,2030200,2033301,2034301,
     1             2045300/

      IER=0
      IFIRST=0
      NNSAVE=9999
      MEMCNT(:)=0
      SUMPRB(:)=0.
      XDATA(1:ND1,1:ND2)=9999.
      SDDATA(1:ND1,1:ND2)=9999.
      XCDF(1:ND1,1:NCDFTH)=9999.
C            
C        INITIALIZE NSAVE IN CASE ALL DATA RETRIEVES ARE SUCCESSFUL.
      M=1
C        M IS THE COLUMN IN XDATA( , ) TO RETRIEVE THE DATA.
C
      DO 200 N=NN,NVRBL
C          NVRBL IS THE NUMBER OF VALUES IN THE ID( , ) LIST.
C          NN IS THE LOCATION OF THE FIRST ONE NOT USED.
        MDATE=NDATE+ITAU(N)
C          GET THE SINGLE VALUE FORECAST FOR IDS THAT ARE THE SAME
C          EXCEPT FOR THE DD.
C   
C         FIND THE ID IN ITABLE.  IF NOT THEN WRITE AN ERROR AND
C         ABORT
C
        IDFOUND=0
        DO 120 I=1,NDIM
          IF(ID(1,NN)/100 .EQ. ITABLE(I)) THEN
            IDFOUND=1
          ENDIF  
 120    CONTINUE
C
        IF(IDFOUND .EQ. 0) THEN
          WRITE(KFILDO,*) '****ERROR: ENTERED RANKDIST FOR VARIALBE',
     1     ' THAT IS NOT ACCOMODATED.'
          WRITE(KFILDO,121) (ID(J,NN),J=1,4)
 121      FORMAT(/,' ****VRBL ',
     1         I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
          WRITE(KFILDO,*) '****WAS SET WITH KERNAL=3 HOWEVER', 
     1     ' RANKDIST WILL ONLY ACCEPT IDS LISTED IN ITABLE'
          WRITE(KFILDO,*) '****CHECK ITABLE IN RANKDIST FOR VALID IDS.'
          WRITE(KFILDO,*) '****THIS VARIABLE WILL BE SKIPPED IN', 
     1     ' THE ID LIST.'
C
C          INCREMENT M TO MOVE TO THE NEXT VARIABLE
          M=M+1
          IER=3
        ELSE
C 
        IF(ID(1,NN)/100.EQ.ID(1,N)/100.AND.
     1     ID(2,NN).EQ.ID(2,N).AND.
     2     IDPARS(8,NN).EQ.IDPARS(8,N).AND.
     3     IDPARS(10,NN).EQ.IDPARS(10,N).AND.
     4     IDPARS(11,NN).EQ.IDPARS(11,N).AND.
     5     IDPARS(12,NN)-IDPARS(9,NN).EQ.IDPARS(12,N)-IDPARS(9,N).AND.
     6     (ABS(ID(4,N)-ID(4,NN)).EQ.
     7      ABS(IDPARS(15,N)-IDPARS(15,NN))))THEN
C
C            THE ID OF VARIABLE N AGREES WITH THAT OF THE BASE VARIABLE
C            NN, EXCEPT DD, R, AND TAU.  THE TEST ASSURES THAT THE DATES
C            AND PROJECTIONS ARE SUCH THAT THE FORECASTS VERIFY AT THE
C            SAME TIME.  NORMALLY, IDPARS(9,NN) WILL BE ZERO.
C
C            RETRIEVE THE SINGLE VALUE FORECASTS.
C
             LD(1)=ID(1,N)
             LD(2)=ID(2,N)
             LD(3)=ID(3,N)
             LD(4)=ID(4,N)
C 
             CALL PRSID1(KFILDO,LD,LDPARS)
             ITIME=IDPARS(9,N)
C            ITIME IS USED IN GFETCH.  IT IS CALLED ITAU IN RETVEC.         
C            
             CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                   LD,LDPARS,JD(1,N),ITIME,
     2                   NDATE,MDATE,CCALL,ISDATA,XDATA(1,M),ND1,NSTA,
     3                   ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                   LSTORE,ND9,LITEMS,CORE,ND10,
     5                   NBLOCK,NFETCH,
     6                   IS0,IS1,IS2,IS4,ND7,
     7                   L3264B,L3264W,IER)
C           JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C           IN CALL TO CONST, BUT IS NOT USED THERE EITHER.
C
            IF(IER.NE.0)THEN
            WRITE(KFILDO,125)(LD(J),J=1,4),NDATE
  125       FORMAT(/,' ****VARIABLE NOT RETRIEVED BY RETVEC',
     1               ' IN RANKDIST',
     2               2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,' FOR DATE',I12)
            ELSE
C
              IF(LDPARS(8).EQ.4)THEN
                 CALL OPTX(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                LD,LDPARS,TRESHL(N),JD(1,N),ITAU(N),
     2                NDATE,MDATE,CCALL,ISDATA,XDATA(1,M),ND1,NCAT,NSTA,
     3                ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                LSTORE,ND9,LITEMS,CORE,ND10,
     5                LASTL,LASTD,NBLOCK,NSTORE,NFETCH,
     6                IS0,IS1,IS2,IS4,ND7,
     7                L3264B,L3264W,ISTAB,IER)
C              IF IER IS RETRUNED NON-ZERO, THE DATA IN XDATA( ,M)
C              WILL BE SET TO MISSING, BUT THE PROCESS WILL CONTINUE.
              ENDIF
C
           ENDIF
C
         IF(IER.EQ.0)THEN
            M=M+1     
         ENDIF
C
C          CHECK TO SEE IF WE REACHED THE LAST VARIABLE.
         IF(N.EQ.NVRBL)THEN
            NNSAVE=9999
         ENDIF
      ELSE
C
         IF(IFIRST.EQ.0)THEN
            NNSAVE=N
C               NNSAVE IS NOW THE FIRST VARIABLE IN THE ID( , ) LIST
C               NOT USED.  LATER, TRANSFER NNSAVE TO NN TO BE USED
C               ON NEXT ENTRY.
            IFIRST=1
         ENDIF
C
      ENDIF
C
C       FOR THE ITABLE CHECK
      ENDIF
  200 CONTINUE
C
C      DECREASE M BY 1 OTHERWISE IT WILL MISCOUNT THE NUMBER
C      OF ENSEMBLE MEMBERS
      M=M-1
C
C      DATA HAS BEEN RETRIEVED FOR ALL ENSEMBLE MEMEBERS NOW CHECK
C      FOR MISSING VALUES AND RANK SORT THE FORECASTS. 
C    
C      THE LOOP 250 COULD BE MADE PARALLEL IF DESIRED.
C
      DO 250 K=1,NSTA
C
         IFLAG(1:M)=0
         XSORTED(1:ND2)=9999.
         NVALID=0      
         DO 240 J=1,M 
C
           ILOC=MINLOC(XDATA(K,1:M),DIM=1,MASK=IFLAG(1:M).EQ.0)
           XMIN=MINVAL(XDATA(K,1:M),MASK=IFLAG(1:M).EQ.0)

           IFLAG(ILOC)=1
           XSORTED(J)=XMIN
           IF((NINT(XMIN).NE.9999).AND.(NINT(XMIN).NE.9997))THEN
             NVALID=NVALID+1
           ENDIF
C
  240    CONTINUE
C    
C         IF(CCALL(K) .EQ. "KBWI    ") THEN
C           DO J=1,NVALID
C             PRINT *,J,XSORTED(J)
C           ENDDO
C         ENDIF
C
C          MAKE SURE AT LEAST TWO GOOD MEMBERS WERE FOUND
C
         IF(NVALID.GT.1) THEN
           CDF(1:NVALID)=9999.
           DO 245 J=1,NVALID
             CDF(J)=(1.*J)/(1.*NVALID)
C             PRINT *,CDF(J),XSORTED(J)
  245      CONTINUE
C
C            FIGURE OUT THE CDF POINTS
C
           DO 247 J=1,NCDFTH

             IF(CDFTH(J).LE.CDF(1))THEN
               XCDF(K,J)=XSORTED(1)
             ELSE IF(CDFTH(J).GE.CDF(NVALID))THEN
               XCDF(K,J)=XSORTED(NVALID)
             ELSE
              DO II=1,NVALID-1
               IF((CDFTH(J).GT.CDF(II)).AND.
     1            (CDFTH(J).LE.CDF(II+1)))THEN
C
                Y0=XSORTED(II)
                Y1=XSORTED(II+1)
                X0=CDF(II)
                X1=CDF(II+1)
C
                DX=X1-X0
                DY=Y1-Y0
C
                IF(DX.NE.0) THEN
                  XM=DY/DX
                ELSE
                  XM=0.
                ENDIF
C
                XCDF(K,J)=Y0+XM*(CDFTH(J)-X0)
C
C             PRINT *,"+++++++++++++++++"
C             PRINT *,CDFTH(J),CDF(II),CDF(II+1)
C             PRINT *,XSORTED(II),XCDF(K,J),XSORTED(II+1)
C
               ENDIF
              ENDDO
             ENDIF
  247      CONTINUE
C
C            COMPUTE THE STANDARD DEVIATION
C
           XAVG=SUM(XSORTED(1:NVALID))/(1.*NVALID)
           XTERM1=SUM((XSORTED(1:NVALID)-XAVG)**2.) 
           SDDATA(K,1)=(XTERM1/(1.*NVALID-1.))**(0.5)
C
 
         ENDIF
C
  250  CONTINUE
C      
C
C      NOW SET THE COUNTER NN TO EQUAL NSAVE SO THE PROGRAM 
C      KNOWS WHERE THE NEXT SET OF VARIABLE IDS BEGIN IN THE
C      ID LIST
      NN=NNSAVE
C
C      
  300 RETURN
      END   
