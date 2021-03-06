      SUBROUTINE PRSID(KFILDO,ITEMP,TEMP,ID,IDPARS,THRESH,ISTOP)
C 
C        JULY    1995   GLAHN   TDL   MOS-2000 
C        MAY     1996   GLAHN   CHANGED CALCULATION OF THRESH( )
C        OCTOBER 1999   GLAHN   OMITTED CHECKING THRESHOLD WHEN NOT BINARY
C        MAY     2000   GLAHN   COMMAS INSERTED IN FORMAT STATEMENTS
C
C        PURPOSE 
C            TO FORM THE 4-WORD ID AND TO PARSE IT INTO ITS 15 INTEGER
C            PARTS PLUS A REAL THRESHOLD.  THIS IS CALLED FROM RDPRED
C            AND RDVRBL, IMMEDIATELY UPON READING THE VARIABLE
C            IDENTIFICATION.  THE IDENTIFICATION AS READ IS IN TEMPORARY
C            VARIABLES AND IS TRANSFERRED TO ID( ) AS WELL AS TO IDPARS( ).
C            IF THE FRACTIONAL PART OF THE THRESHOLD IS ZERO, THE SIGN
C            AND EXPONENT ARE SET TO ZERO.
C
C        DATA SET USE 
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT) 
C 
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT) 
C            ITEMP(J) = INPUT INFORMATION FOR ID'S (J=1,6).  THIS IS
C                       WHAT ANY PROGRAM READING THE MOS-2000 ID'S WOULD
C                       PROVIDE PLUS TEMP.  (INPUT)
C                       J=1--FIRST ID = ID(1),
C                       J=2--SECOND ID = ID(2),
C                       J=3--THIRD ID = ID(3),
C                       J=4--LAST PORTION OF ID = ID(4),
C                       J=5--FRACTIONAL PART OF THRESH, AND
C                       J=6--TEN'S EXPONENT OF ITEMP(5).
C                TEMP = SIGN OF THRESHOLD.  (CHARACTER*1)  (INPUT)
C               ID(J) = THE PREDICTOR ID'S (J=1,4).  (OUTPUT)
C           IDPARS(J) = THE PARSED, INDIVIDUAL COMPONENTS OF THE PREDICTOR
C                       ID'S CORRESPONDING TO ID( ) (J=1,15).
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
C              THRESH = THE BINARY THRESHOLD CORRESPONDING TO ID( ) AND
C                       IDPARS( ).  (OUTPUT)
C               ISTOP = INCREASED BY 1 WHENEVER AN ERROR IS ENCOUNTERED.
C                       (INPUT-OUTPUT)
C 
C        NONSYSTEM SUBROUTINES CALLED 
C            NONE
C
      CHARACTER*1 TEMP
C
      DIMENSION ID(4),IDPARS(15),ITEMP(6)
C
C        THE COMPRESSED ID IS 4 WORDS LONG, AND COMPOSED OF 15 PARTS
C        PLUS A REAL THRESHOLD.  WHEN THE FRACTIONAL PART OF THE
C        THRESHOLD IS ZERO, SET THE SIGN TO POSITIVE AND THE EXPONENT
C        TO ZERO.
C
      ID(1)=ITEMP(1)
C
      IDPARS(1)=ID(1)/1000000
      IDPARS(2)=ID(1)/1000-IDPARS(1)*1000
      IDPARS(3)=ID(1)/100-IDPARS(1)*10000-IDPARS(2)*10
      IDPARS(4)=ID(1)-(ID(1)/100)*100
C
      ID(2)=ITEMP(2)
C
      IDPARS(5)=ID(2)/100000000
      IDPARS(6)=ID(2)/10000-IDPARS(5)*10000
      IDPARS(7)=ID(2)-(ID(2)/10000)*10000
C
      ID(3)=ITEMP(3)
C
      IDPARS(9)=ID(3)/1000000
      IDPARS(10)=ID(3)/100000-IDPARS(9)*10
      IDPARS(11)=ID(3)/1000-IDPARS(9)*1000-IDPARS(10)*100
      IDPARS(12)=ID(3)-(ID(3)/1000)*1000
      IDPARS(8)=IDPARS(9)/100
      IDPARS(9)=IDPARS(9)-IDPARS(8)*100
C
      IF(ITEMP(5).EQ.0)ITEMP(6)=0
C        THE ABOVE ASSURES THAT A ZERO FRACTION WILL HAVE A ZERO
C        EXPONENT.
      IXPON=ITEMP(6)
      IF(ITEMP(6).LT.0)ITEMP(6)=ABS(ITEMP(6))+50
      ISIGN=0
      IF(TEMP.EQ.'-')ISIGN=1
      IF(ITEMP(5).EQ.0)ISIGN=0
C        THE ABOVE ASURES THAT A ZERO FRACTION WILL NOT HAVE A MINUS.
      ID(4)=ISIGN*1000000000+ITEMP(5)*100000+ITEMP(6)*1000+ITEMP(4)
C
      IDPARS(13)=ITEMP(4)/100
      IDPARS(14)=(ITEMP(4)-IDPARS(13)*100)/10
      IDPARS(15)=(ITEMP(4)-IDPARS(13)*100)-IDPARS(14)*10
C
C        THE BELOW KEEPS INTEGER VALUES INTACT BETTER THAN
C        JUST USING THE ONE EXPRESSION FOLLOWING "ELSE" BELOW.
C
      IF(IXPON.EQ.4)THEN
         THRESH=ITEMP(5)
      ELSEIF(IXPON.EQ.3)THEN
         THRESH=ITEMP(5)/10.
      ELSEIF(IXPON.EQ.2)THEN
         THRESH=ITEMP(5)/100.
      ELSEIF(IXPON.EQ.1)THEN
         THRESH=ITEMP(5)/1000.
      ELSEIF(IXPON.GT.4)THEN	
         THRESH=ITEMP(5)*10**(IXPON-4)
      ELSE
         THRESH=ITEMP(5)*10.**(IXPON-4)
C           NOTE THAT THE 10. HAS TO BE FLOATING POINT
C           BECAUSE 10 TO A NEGATIVE POWER WILL BE LT 1.
      ENDIF
      
      IF(ISIGN.EQ.1)THRESH=-THRESH
C
D     WRITE(KFILDO,140)(ID(J),J=1,4),(IDPARS(J),J=1,15),THRESH
D140  FORMAT(' IN PRSID',1X,I9.9,1X,I9.9,1X,I9.9,1X,I10.3,/,
D    1       '         '15I6,F12.4)
      RETURN
      END
