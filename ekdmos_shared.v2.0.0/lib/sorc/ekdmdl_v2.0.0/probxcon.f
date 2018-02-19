      SUBROUTINE PROBXCON(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1                  ID,IDPARS,JD,ITAU,
     2                  NDATE,MDATE,CCALL,ISDATA,XDATA,ND1,NCAT,NSTA,
     3                  ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4                  LSTORE,ND9,LITEMS,CORE,ND10,
     5                  NBLOCK,NFETCH,
     6                  IS0,IS1,IS2,IS4,ND7,
     7                  L3264B,L3264W,ISTAB,IER)
C 
C        JAN       2000   SHIREY   TDL   MOS-2000
C        JUNE      2000   DALLAVALLE   REVISED FORMAT STATEMENTS
C                                      TO CONFORM TO FORTRAN90
C                                      STANDARDS ON THE IBM-SP
C        AUG       2000   SHIREY       CHANGED TO FORCE POP TO
C                                      BE BETWEEN 0 AND 1. 
C        JANUARY   2002   HUGHES/RLC   REPLACED POPXCON WITH PROBXCON TO
C                                      MAKE THE ROUTINE MORE GENERAL, SO
C                                      THAT IT COULD PRODUCE UNCOND. QPF
C                                      AND SEVERE TSTM PROBABILITIES 
C        JUNE      2003   COSGROVE     ADDED IDS TO PRODUCE UNCONDITIONAL
C                                      SNOW AMOUNTS.
C
C        PURPOSE:
C               MULTIPLIES A CONDITIONAL PQPF/SVR/SNOW FORECAST BY ITS 
C               CORRESPONDING PROBABILITY TO CREATE AN UNCONDITIONAL
C               FORECAST.
C
C            IDPARS(1) & IDPARS(2) OF PROBABILITIES  ARE MAPPED
C                          UNCOND FROM   COND     PROB
C                          203210       203215 * 203210 -- UNCOND  6-H QPF
C                          203310       203315 * 203310 -- UNCOND 12-H QPF
C                          203410       203415 * 203410 -- UNCOND 24-H QPF
C                          207270       207265 * 207220 -- UNCOND  6-H SVR
C                          207380       207375 * 207330 -- UNCOND 12-H SVR
C                          207480       207475 * 207430 -- UNCOND 24-H SVR
C                          208460       208465 * 203450 -- UNCOND 24-H SNOW
C                          208455       208445 * 208525 -- POP*CPOS FOR SNOW 
C                            (208455 IS AN INTERMEDIATE STEP FOR SNOW)
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                     (OUTPUT) 
C            KFIL10 - UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                     (INPUT-OUTPUT) 
C            IP12   - INDICATES WHETHER (>1) OR NOT (=0) THE LIST OF
C                     STATIONS ON THE INPUT FILES WILL BE PRINTED TO 
C                     THE FILE WHOSE UNIT NUMBER IS IP12.  (OUTPUT)
C         KFILRA(J) - THE UNIT NUMBERS FOR WHICH RANDOM ACCESS FILES
C                     ARE AVAILABLE (J=1,NUMRA).  (INPUT)
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       (INPUT) 
C              KFIL10 = UNIT NUMBER OF TDL MOS-2000 FILE SYSTEM ACCESS.
C                       (INPUT)
C             ID(J,L) = THE PREDICTOR IDS (J=1,4) (L=1,NCAT).  (INPUT)
C         IDPARS(J,L) = THE PARSED, INDIVIDUAL COMPONENTS OF THE 
C                       PREDICTOR ID CORRESPONDING TO ID( ) (J=1,15)
C                       (L=1,NCAT).  (INPUT)
C                       J=1--CCC (CLASS OF VARIABLE),
C                       J=2--FFF (SUBCLASS OF VARIABLE),
C                       J=3--B (BINARY INDICATOR),
C                       J=4--DD (DATA SOURCE, MODEL NUMBER),
C                       J=5--V (VERTICAL APPLICATION),
C                       J=6--LBLBLBLB (BOTTOM OF LAYER, 0 IF ONLY 
C                            1 LAYER),
C                       J=7--LTLTLTLT (TOP OF LAYER),
C                       J=8--T (TRANSFORMATION),
C                       J=9--RR (RUN TIME OFFSET, ALWAYS + AND BACK IN 
C                            TIME),
C                       J=10--OT (TIME APPLICATION),
C                       J=11--OH (TIME PERIOD IN HOURS),
C                       J=12--TAU (PROJECTION IN HOURS),
C                       J=13--I (INTERPOLATION TYPE),
C                       J=14--S (SMOOTHING INDICATOR), AND
C                       J=15--G (GRID INDICATOR).
C             JD(J,L) = THE BASIC INTEGER PREDICTOR IDS (J=1,4) (L=1,NCAT).
C                       THIS IS THE SAME AS ID(J), EXCEPT THAT THE
C                       PORTIONS PERTAINING TO PROCESSING ARE OMITTED:
C                       B = IDPARS(3),
C                       T = IDPARS(8),
C                       I = IDPARS(13),
C                       S = IDPARS(14),
C                       G = IDPARS(15), AND
C                       THRESH.
C                       JD( ) IS USED TO IDENTIFY THE BASIC MODEL FIELDS
C                       AS READ FROM THE ARCHIVE.  (INPUT)
C                ITAU = THE NUMBER OF HOURS AHEAD TO FIND A VARIABLE.
C                       THIS HAS ALREADY BEEN CONSIDERED IN MDATE, BUT
C                       IS NEEDED FOR CALL TO GFETCH.  (INPUT)
C               MDATE = THE DATE/TIME FOR WHICH PREDICTOR IS NEEDED.
C                       (INPUT)
C          XDATA(K,L) = COMPUTED VARIABLE IS RETURNED IN XDATA( , )
C                       (K=1,NSTA) (L=1,NCAT).  (OUTPUT)
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT 
C                       WITH.  FIRST DIMENSION OF XDATA( , ).  (INPUT)
C                NSTA = NUMBER OF STATIONS OR LOCATIONS BEING DEALT
C                       WITH.  (INPUT)
C                NCAT = NUMBER OF FORECAST CATEGORIES.  (INPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C            IWORK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(J) = WORK ARRAY (J=1,ND5).  (NOT ACTUALLY USED.)
C                       (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ), IWORK( ), AND DATA( ).
C                       (INPUT)
C         LSTORE(L,J) = THE ARRAY HOLDING INFORMATION ABOUT THE DATA 
C                       STORED (L=1,12) (J=1,LITEMS).  (INPUT-OUTPUT)
C                       L=1,4--THE 4 ID'S FOR THE DATA.
C                       L=5  --LOCATION OF STORED DATA.  WHEN IN CORE,
C                              THIS IS THE LOCATION IN CORE( ) WHERE
C                              THE DATA START.  WHEN ON DISK, 
C                              THIS IS MINUS THE RECORD NUMBER WHERE 
C                              THE DATA START.
C                       L=6  --THE NUMBER OF 4-BYTE WORDS STORED.
C                       L=7  --2 FOR DATA PACKED IN TDL GRIB, 1 FOR NOT.
C                       L=8  --THE DATE/TIME OF THE DATA IN FORMAT
C                              YYYYMMDDHH.
C                       L=9  --NUMBER OF TIMES DATA HAVE BEEN RETRIEVED.
C                       L=10 --NUMBER OF THE SLAB IN DIR( , ,L) AND
C                              IN NGRIDC( ,L) DEFINING THE 
C                              CHARACTERISTICS OF THIS GRID.
C                       L=11 --THE NUMBER OF THE PREDICTOR IN THE SORTED
C                              LIST IN ID( ,N) (N=1,NPRED) FOR WHICH 
C                              THIS VARIABLE IS NEEDED, WHEN IT IS 
C                              NEEDED ONLY ONCE FROM LSTORE( , ). 
C                              WHEN IT IS NEEDED MORE THAN ONCE, THE 
C                              VALUE IS SET = 7777.
C                       L=12 --USED INITIALLY IN ESTABLISHING 
C                              MOSTORE( , ).  LATER USED AS A WAY OF
C                              DETERMINING WHETHER TO KEEP THIS 
C                              VARIABLE.
C                 ND9 = THE SECOND DIMENSION OF LSTORE( , ).  (INPUT)
C              LITEMS = THE NUMBER OF ITEMS (COLUMNS) IN LSTORE( , )
C                       THAT HAVE BEEN USED IN THIS RUN.  (INPUT)
C             CORE(J) = THE ARRAY TO STORE OR RETIREVE THE DATA 
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
C               ISTAV = 1 SINCE THE DATA RETURNED ARE VECTOR.  (OUTPUT)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  (INPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                        52 = NWORDS FROM GFETCH NE NSTA.
C                       102 = ID NOT ACCOMMODATED OR NCAT LE 0.
C                       SEE GFETCH FOR OTHER VALUES.  (OUTPUT)
C               LD(J) = HOLDS THE 4 ID WORDS OF THE DATA RETRIEVED INTO
C                       XDATA( ) (J=1,4).  (INTERNAL)
C           LDPARS(J) = PARSED VALUES CORRESPONDING TO LD( ) (J=1,15)
C                       (INTERNAL)
C        ITABLE(4,JJ) = TABLE OF ID INFORMATION FOR THIS ROUTINE (JJ=1,NDIM). 
C                       (INTERNAL)
C                         1=CCCFFF OF UNCONDITIONAL PROBABILITY TO BE COMPUTED
C                         2=CCCFFF OF CONDITIONAL PROBABILITY (I.E. QPF/SVR)
C                         3=CCCFFF OF PROBABILITY OF CONDITIONING EVENT 
C                             (I.E. POP/TSTM)
C                         4=CUTOFF FOR CONDITIONING EVENT PROBABILITY
C                NDIM = NUMBER OF POSSIBLE ID'S FOR PROCESS IN ITABLE( ).  
C                       SET BY PARAMETER.  (INTERNAL)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            RETVEC, PRSID1
C
      PARAMETER(NDIM=8)
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(ND5)
      CHARACTER*60 RACESS(5)
C
      DIMENSION XDATA(ND1,NCAT),ISDATA(ND1),PROB(ND1)
      DIMENSION ID(4,NCAT),IDPARS(15,NCAT),JD(4,NCAT)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7)
      DIMENSION LSTORE(12,ND9)
      DIMENSION CORE(ND10),ICALLD(L3264W,ND5)
      DIMENSION LD(4),LDPARS(15),KFILRA(5)
      DIMENSION ITABLE(4,NDIM)
C     
      DATA ITABLE/203210,203215,203210,950052, 
     1            203310,203315,203310,950052,
     2            203410,203415,203410,950052,
     3            207270,207265,207220,950000,
     4            207380,207375,207330,950000,
     5            207480,207475,207430,950000,
     6            208460,208465,203450,950052,
     7            208455,208445,208525,500051/
C
      IER=0
      ISTAB=1
C      
C        FIND THE ID(1) IN ITABLE(1).
C
      DO 105 JJ=1,NDIM      
         IF(ITABLE(1,JJ).EQ.(IDPARS(1,1)*1000+IDPARS(2,1))) GO TO 108
 105  CONTINUE
C     
      WRITE(KFILDO,107)(JD(L,1),L=1,4)
 107  FORMAT(' ****PROBXCON ENTERED FOR VARIABLE',
     1        2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,
     2        ' NOT ACCOMMODATED.')
      IER=102
      GO TO 300
C
C        VERIFY THAT NCAT GT 0.
C
 108  IF(NCAT.LE.0)THEN
         WRITE(KFILDO,110)NCAT
 110     FORMAT(/,' ****NCAT =',I4,' NOT CORRECT IN PROBXCON.')
         IER=102
         GO TO 300
      ENDIF
C
      LD(1)=ITABLE(2,JJ)*1000+IDPARS(3,1)*100+IDPARS(4,1)
      LD(2)=JD(2,1)
      LD(3)=JD(3,1)
C          
C        GET THE NCAT CONDITIONAL PROBABILITIES 
C    
      DO 150 J=1,NCAT
         LD(4)=ID(4,J)
C
C        IN THE CASE OF THE CPOS*CSNOW (208455), THERE IS NO DATA FOR
C        THE FIRST CUTOFF OF 208445, SO SKIP THE CALL TO RETVEC FOR THIS
C        CATEGORY.
C
         IF((IDPARS(1,1).EQ.208).AND.(IDPARS(2,1).EQ.455).AND.
     1      (J.EQ.1))GO TO 150

         CALL PRSID1(KFILDO,LD,LDPARS)
         CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1               LD,LDPARS,JD,ITAU,
     2               NDATE,MDATE,CCALL,ISDATA,XDATA(1,J),ND1,NSTA,
     3               ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4               LSTORE,ND9,LITEMS,CORE,ND10,
     5               NBLOCK,NFETCH,
     6               IS0,IS1,IS2,IS4,ND7,
     7               L3264B,L3264W,IER)
C
C           JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C           IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C
         IF(IER.NE.0)THEN
            WRITE(KFILDO,145)(LD(L),L=1,4)
 145        FORMAT(' ****VARIABLE NOT RETRIEVED BY RETVEC IN PROBXCON',
     1               2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
            GO TO 300
         ENDIF
 150     CONTINUE
C
C        GET THE PROBABILITY OF THE CONDITIONING EVENT (IE. POP, TSTM)
C
      LD(1)=ITABLE(3,JJ)*1000+IDPARS(3,1)*100+IDPARS(4,1)
      LD(2)=JD(2,1)
      LD(3)=JD(3,1)
      LD(4)=ITABLE(4,JJ)*1000
C
         CALL PRSID1(KFILDO,LD,LDPARS)
         CALL RETVEC(KFILDO,KFIL10,IP12,KFILRA,RACESS,NUMRA,
     1               LD,LDPARS,JD,ITAU,
     2               NDATE,MDATE,CCALL,ISDATA,PROB,ND1,NSTA,
     3               ICALLD,CCALLD,IPACK,IWORK,DATA,ND5,
     4               LSTORE,ND9,LITEMS,CORE,ND10,
     5               NBLOCK,NFETCH,
     6               IS0,IS1,IS2,IS4,ND7,
     7               L3264B,L3264W,IER)
C
C           JD( ) IS NOT ACTUALLY USED IN RETVEC.  IT IS USED
C           IN CALL TO CONST, BUT CONST DOES NOT USE IT EITHER.
C
         IF(IER.NE.0)THEN
            WRITE(KFILDO,146)(LD(L),L=1,4)
 146        FORMAT(' ****VARIABLE NOT RETRIEVED BY RETVEC IN PROBXCON',
     1               2X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3)
            GO TO 300
         ENDIF
C 
C       MAKE CONDITIONING EVENT FCST BETWEEN 0 AND 1.  
C       THIS STEP WAS ORIGINALLY PUT IN BECAUSE THE ORIGINAL MRF QPF
C       WAS POST-PROCESSED IN SUCH A WAY THAT IT WAS NECESSARY.  OVER
C       TIME WE DID THIS ALSO FOR THE AVN QPF, EVEN THOUGH IT WAS REDUNDANT.
C       ONCE THE QPF IS REDEVELOPED, WE CAN UNDO THIS.  IT IS NOT DONE
C       FOR TSTMS.  BUT IT WILL BE USED FOR THE SNOWFALL.  IN THAT CASE
C       WE NEED TO TRUNCATE THE CPOS PROBABILITY. IT IS THEN ASSIGNED THE
C       ID FOR THE FIRST CATEGORY OF CONDITIONAL CSNOW.
C
      IF((IDPARS(1,1).EQ.203).OR.((IDPARS(1,1).EQ.208).AND.
     1   (IDPARS(2,1).EQ.455)))THEN
 181  DO 180 K=1,NSTA         
C      
C
            IF(NINT(PROB(K)).NE.9999.AND.
     1         NINT(PROB(K)).NE.9997) THEN
               IF(PROB(K).LT.0.) THEN
                 PROB(K)=0.
               ELSEIF(PROB(K).GT.1.) THEN
                 PROB(K)=1.
               ENDIF
C
            ENDIF          
C
C
 180  CONTINUE 
       ENDIF
C 
C        CREATE UNCONDITIONAL FORECASTS
C          IF THIS IS THE FIRST CATEGORY OF THE CSNOW (208455), JUST
C          SET THE OUTPUT TO BE THE TRUNCATED CONDITIONING PROBABILITY
C          (CPOS).
C
 182  DO 190 K=1,NSTA         
C      
         DO 189 J=1,NCAT   
           IF((IDPARS(1,1).EQ.208).AND.(IDPARS(2,1).EQ.455).AND.
     1         (J.EQ.1))THEN
                XDATA(K,J)=PROB(K)
           ELSE
C
            IF(NINT(XDATA(K,J)).NE.9999) THEN
               IF(NINT(XDATA(K,J)).NE.9997) THEN
		  IF(NINT(PROB(K)).NE.9999.AND.
     1               NINT(PROB(K)).NE.9997) THEN
			XDATA(K,J)=XDATA(K,J)*PROB(K)
                  ELSE
		  XDATA(K,J)=PROB(K)
                  ENDIF
C
C
               ENDIF 
C
            ENDIF  
           ENDIF        
C
 189     CONTINUE
C
 190  CONTINUE 
      GO TO 348   
C     
C        THIS PREDICTOR CANNOT BE COMPUTED.  SET THE ARRAY TO MISSING.
C        THE MAX FUNCTION GUARDS AGAINST NCAT LT 0 AND ALWAYS SETS
C        ONE COLUMN TO MISSING.  THIS IS REALTIVELY UNIMPORTANT
C        BECAUSE THE CALLING ROUTINES WILL DO THE SAME THING.
C     
 300     DO 310 K=1,NSTA
         DO 309 J=1,NCAT
            XDATA(K,J)=9999.
 309     CONTINUE
 310  CONTINUE
C
 348  RETURN
      END
