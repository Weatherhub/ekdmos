      SUBROUTINE TANDSD(KFILDO,KFILCP,IP7,IP9,
     1                  IDTAND,IDTPAR,IDTNSD,IDPRSD,ISCLSD,PLNSD,ICAT,
     2                  MTANDS,NSETS,ND3,ND11,ISTOP,IER)
C
C        DECEMBER  2006   GLAHN   MDL   MOS-2000
C                                 REVISED FROM TANDID
C        JANUARY   2007   GLAHN   REVISED FOR NEW ID STRUCTURE
C        MARCH     2007   GLAHN   INCREASED SCALING BY 1 AND INSERTED
C                                 'SD' INTO PLAIN LANGUAGE
C        APRIL     2007   GLAHN   REMOVED OLD COMMENT ABOUT CCCFF1
C        MAY       2007   GLAHN   MODIFIED LOOP DO 148 TO MODIFY
C                                 PL( ) AND ISC( ) FOR PRINT
C
C        PURPOSE
C            TO FIND PLAIN LANGUAGE AND SCALING CONSTANTS FOR
C            STANDARD DEVIATION OF THE ERRORS OF PREDICTANDS FOR
C            U705 AND U905.  THE ID'S ARE UNCHANGED, EXCEPT THE 
C            LLLL OF WORD 2 IS SET TO 0200.  IT IS BELIEVED SETPLN
C            WILL WORK OK BECAUSE THE LLLL IS MODIFIED FOR ONLY
C            SPECIFIC IDS NOT LIKELY TO BE USED (SOON) IN ENSEMBLE
C            WORK.
C   
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C            KFILCP    - UNIT NUMBER FOR VARIABLE CONSTANT FILE.
C                        (INPUT)
C            IP7       - THE UNIQUE PREDICTAND SD LIST IN SUMMARY FORM. 
C                        THIS LIST INCLUDES THE PARSED ID'S IN 
C                        IDPARS( , ).  (OUTPUT)
C            IP9       - THE PREDICTAND SD LIST IN SUMMARY FORM, NOT
C                        INCLUDING THE PARSED ID'S IN IDPARS( , ),
C                        BUT INCLUDING THE PLNSD LANGUAGE INFORMATION. 
C                        (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              KFILCP = UNIT NUMBER FOR PREDICTAND CONSTANT FILE.  THIS
C                       CONTAINS DEFAULT VALUES FOR CERTAIN CONSTANTS. 
C                       THESE INCLUDE PLNSD LANGUAGE NAMES.  (INPUT)
C                 IP7 = WHEN IP7 NE 0, THE PREDICTAND LIST 
C                       IS WRITTEN TO UNIT IP7, INCLUDING PARSED IDS.
C                       (INPUT)
C                 IP9 = WHEN IP9 NE 0, THE PREDICTAND LIST 
C                       IS WRITTEN TO UNIT IP9, NOT INCLUDING THE 
C                       PARSED ID'S IN IDPARS( , ), BUT INCLUDING 
C                       THE PLNSD LANGUAGE INFORMATION.  (INPUT)
C      IDTAND(J,NN,I) = THE PREDICTAND ID'S (J=1,4), FOR PREDICTAND NN 
C                       (NN=1,MTANDS(I)), AND EQUATION SET I (I=1,NSETS).
C                       (INPUT)
C      IDTPAR(J,NN,I) = THE PARSED PREDICTAND ID'S (J=1,15), FOR
C                       PREDICTAND NN (NN=1,MTANDS(I)), AND EQUATION
C                       SET I (I=1,NSETS).  (INPUT)
C      IDTNSD(J,NN,I) = THE PREDICTAND ID'S (J=1,4), FOR PREDICTAND NN 
C                       (NN=1,MTANDS(I)), AND EQUATION SET I (I=1,NSETS)
C                       MODIFIED AS NECESSARY.  (OUTPUT)
C      IDPRSD(J,NN,I) = THE PARSED PREDICTAND ID'S (J=1,15), FOR
C                       PREDICTAND NN (NN=1,MTANDS(I)), AND EQUATION
C                       SET I (I=1,NSETS) MODIFIED AS NECESSARY.
C                       (OUTPUT)
C        ISCLSD(NN,I) = SCALING FACTORS FOR PREDICTAND NN 
C                       (NN=1,MTANDS(I)), AND EQUATION SET I (I=1,NSETS).
C                       (OUTPUT)
C         PLNSD(NN,I) = THE PLNSD LANGUAGE DESCRIPTION OF THE PREDICTAND NN 
C                       (NN=1,MTANDS(I)), AND EQUATION SET I (I=1,NSETS).
C                       (CHARACTER*32)  (OUTPUT)
C          ICAT(NN,I) = THE POSTPROCESSING INDICATOR FOR THIS EQUATION
C                       SET I (I=1,NSETS) AND PREDICTAND (NN=1,MTANDS(I)).
C                       FOR EXAMPLE, THE VALUE 5 WOULD REFER TO 
C                       SUBROUTINE CAT5.  (INPUT)
C           MTANDS(I) = THE NUMBER OF PREDICTANDS FOR EACH EQUATION
C                       SET I (I=1,NSETS).  (OUTPUT)
C               NSETS = THE NUMBER OF SETS OF EQUATIONS.
C                       (INPUT)
C                 ND3 = MAXIMUM NUMBER OF PREDICTANDS IN EQUATION.
C                       (INPUT)
C                ND11 = MAXIMUM NUMBER OF SETS OF EQUATIONS.
C                       (INPUT)
C               ISTOP = INCREMENTED BY 1 WHEN AN ERROR OCCURS.
C                       (INPUT/OUTPUT)
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN
C         IDPARS(J,N) = WORK ARRAY FOR SETPLN.  (INTERNAL) (AUTOMATIC)
C           TRESHL(N) = WORK ARRAY FOR SETPLN.  (INTERNAL) (AUTOMATIC)
C            IDT(J,N) = WORK ARRAY FOR SETPLN.  (INTERNAL) (AUTOMATIC)
C             JD(J,N) = WORK ARRAY FOR SETPLN.  (INTERNAL) (AUTOMATIC)
C              ISC(N) = WORK ARRAY FOR SETPLN.  (INTERNAL) (AUTOMATIC)
C            SMULT(N) = WORK ARRAY FOR SETPLN.  (INTERNAL) (AUTOMATIC)
C             SADD(N) = WORK ARRAY FOR SETPLN.  (INTERNAL) (AUTOMATIC)
C           ORIGIN(N) = WORK ARRAY FOR SETPLN.  (INTERNAL) (AUTOMATIC)
C             CINT(N) = WORK ARRAY FOR SETPLN.  (INTERNAL) (AUTOMATIC)
C            ICATC(N) = WORK ARRAY FOR PRINTING.  (INTERNAL) (AUTOMATIC)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUINES USED 
C            PRSID2, SETPLN
C
      CHARACTER*12 UNITS(ND3*ND11)
C        UNITS( ) IS AN AUTOMATIC ARRAY.
      CHARACTER*32 PLNSD(ND3,ND11)
      CHARACTER*32 PL(ND3*ND11)
C        PL( ) IS AN AUTOMATIC ARRAY.
C
      DIMENSION MTANDS(ND11)
      DIMENSION IDTAND(4,ND3,ND11),IDTNSD(4,ND3,ND11),
     1          IDTPAR(15,ND3,ND11),IDPRSD(15,ND3,ND11),
     2          ISCLSD(ND3,ND11),ICAT(ND3,ND11)
      DIMENSION IDT(4,ND3*ND11),IDPARS(15,ND3*ND11),
     1          TRESHL(ND3*ND11),JD(4,ND3*ND11),
     2          ISC(ND3*ND11),SMULT(ND3*ND11),SADD(ND3*ND11),
     3          ORIGIN(ND3*ND11),CINT(ND3*ND11),ICATC(ND3*ND11)
C        IDT( , ), IDPARS( , ), TRESH( ), JD( , ), SMULT( ), ISC( ),
C        SADD( ), ORIGIN( ), CINT( ), AND ICATC( ) ARE AUTOMATIC ARRAYS.
C
D     CALL TIMPR(KFILDO,KFILDO,'START TANDSD        ')
      IER=0
      NCOUNT=0
      ND311=ND3*ND11
C
C        CHANGE PREDICTAND IDS TO SD IDS.
C
      DO 120 I=1,NSETS
C
      DO 119 NN=1,MTANDS(I)
      IDTNSD(1,NN,I)=IDTAND(1,NN,I)  
      IDTNSD(2,NN,I)=002000000 
C        THIS SETS LLLL IN WORD 2 = 0200 TO INDICATE STANDARD
C        DEVIATIONS. 
      IDTNSD(3,NN,I)=IDTAND(3,NN,I)  
      IDTNSD(4,NN,I)=IDTAND(4,NN,I)  
 119  CONTINUE
C
 120  CONTINUE
C
C        PARSE PREDICTAND IDS INTO IDPARS( , , ).
C
      DO 145 I=1,NSETS
C
      DO 144 NN=1,MTANDS(I)
      NCOUNT=NCOUNT+1
C
C        TEST ARRAY SIZE.  THIS IS A SAFETY; CHECK
C        SHOULD BE IN CALLING PROGRAM.
C
      IF(NCOUNT.GT.ND311)THEN
         WRITE(KFILDO,131)NCOUNT,ND311
 131     FORMAT(/,' ****NUMBER OF PREDICTANDS EXCEEDS',
     1            ' NUMBER ALLOWED =',I4,'.',/,
     2            '     STOP IN TANDSD AT 131.')
         STOP 131
      ENDIF
C
      IDT(1,NCOUNT)=IDTNSD(1,NN,I)
      IDT(2,NCOUNT)=IDTNSD(2,NN,I)
      IDT(3,NCOUNT)=IDTNSD(3,NN,I)
      IDT(4,NCOUNT)=IDTNSD(4,NN,I)
      CALL PRSID2(KFILDO,IDT(1,NCOUNT),IDPARS(1,NCOUNT),TRESHL(NCOUNT))
C
C        FOR ALL PROGRAMS EXCEPT U201, IDPARS(15, ) AND IT'S ROLE IN
C        ID(4) HAS NO MEANING.  THE INTERPOLATED INPUT FILE WILL NOT
C        INCLUDE IT.  THEREFORE, SET IT TO ZERO.
C
      IDPARS(15,NCOUNT)=0
      IDT(4,NCOUNT)=(IDT(4,NCOUNT)/10)*10
C
C        PREPARE "BASIC" VARIABLE ID'S.  FOR PROGRAMS READING U201
C        OUTPUT (E.G., U600), JD( , , ) = ID( , , ) EXCEPT IN
C        ID(1, , ) THE BINARY INDICATOR IS OMITTED AND IN JD(4, , )
C        THE THRESHOLDS AND IDPARS(15, ) ARE OMITTED.
c
      JD(1,NCOUNT)=IDPARS(1,NCOUNT)*1000000+
     1           IDPARS(2,NCOUNT)*1000+
     2           IDPARS(4,NCOUNT)
      JD(2,NCOUNT)=IDT(2,NCOUNT)
      JD(3,NCOUNT)=IDT(3,NCOUNT)
      JD(4,NCOUNT)=IDPARS(13,NCOUNT)*100+
     1           IDPARS(14,NCOUNT)*10
 144  CONTINUE
C
 145  CONTINUE
C
C        PUT PLNSD LANGUAGE AND PARSED IDS WITH THE PREDICTANDS.
C        THE CONSTANT FILE HAS BEEN USED BEFORE, SO MUST REWIND IT.
C
      REWIND KFILCP
C        THE FILE IS REWOUND IN CASE IT HAS BEEN READ PREVIOUSLY.
      CALL SETPLN(KFILDO,KFILCP,
     1            IDT,IDPARS,JD,ISC,SMULT,SADD,
     2            ORIGIN,CINT,PL,UNITS,ND3*ND11,NCOUNT,ISTOP,IER)
      NCOUNT=0
C
      DO 148 I=1,NSETS
      DO 147 NN=1,MTANDS(I)
      NCOUNT=NCOUNT+1
      PL(NCOUNT)(24:25)='SD'
C        ENTERED 'SD' INTO THE ID, RATHER THAN HAVING SETPLN DO IT.
      PLNSD(NN,I)=PL(NCOUNT)
      ISC(NCOUNT)=ISC(NCOUNT)+1
C        INCREASED SCALING BY ONE OVER WHAT IS IN THE DIRECTORY
C        FOR THE VARIABLE.  THE 200 IN LLLL HAS BEEN INSERTED ABOVE.
      ISCLSD(NN,I)=ISC(NCOUNT)
      ICATC(NCOUNT)=ICAT(NN,I)
C
      DO 146 J=1,15
      IDPRSD(J,NN,I)=IDPARS(J,NCOUNT)
 146  CONTINUE
C
 147  CONTINUE
 148  CONTINUE

      IF(IP7.EQ.0)GO TO 150
C
C        WRITE THE PREDICTAND ID'S TO UNIT NO. IP7.
C
      WRITE(IP7,149)NCOUNT,((IDT(J,L),J=1,4),TRESHL(L),
     1              (IDPARS(J,L),J=1,15),L=1,NCOUNT)
 149  FORMAT(/,' ',I4,
     1         ' PREDICTAND SD''S, THRESHOLDS, AND PARSED VALUES   ',
     2         '      (1)  (2)  (3)  (4)  (5)  (6)  (7)  (8)  (9)',
     3         ' (10) (11) (12)(13)(14)(15)',/,
     4       (' ',I9.9,2I10,I11,F12.4,5X,12I5,3I4))
C
 150  IF(IP9.EQ.0)GO TO 170
C
C        WRITE THE PREDICTAND ID'S INCLUDING PLNSD LANGUAGE
C        TO UNIT NO. IP9.
C
      WRITE(IP9,160)NCOUNT
 160  FORMAT(/,' ',I4,' PREDICTAND SD''S AND INFORMATION FROM VARIABLE',
     1              ' CONSTANT FILE')
      WRITE(IP9,161)(L,(IDT(J,L),J=1,4),PL(L),ICATC(L),
     1               ISC(L),L=1,NCOUNT)
 161  FORMAT('  NO.     ID(1)     ID(2)     ID(3)      ID(4)',
     1       '       PLNSD LANGUAGE ID         ICAT ISCLSD',
     2       ' (FOR PACKING)',/,
     3      (' ',I4,1X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,2X,A32,I2,I6))
C
 170  CONTINUE
      RETURN
      END
