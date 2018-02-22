      SUBROUTINE IPRINT(KFILDO,IP,IUSE)
C 
C        JULY 1999   GLAHN   TDL   MOS2000 
C        MARCH 2000  DALLAVALLE    MODIFIED FORMAT STATEMENTS TO
C                                  CONFORM TO FORTRAN 90 STANDARDS
C                                  ON THE IBM SP
C
C        PURPOSE  
C            TO PRINT THE 25 POSSIBLE IP NUMBERS.  WHEN A NUMBER
C            IS USED BY THE CALLING PROGRAM, THE NUMBER IS PRINTED
C            EVEN IF IT IS 0.  IF THE NUMBER IS NOT USED BY THE
C            CALLING PROGRAM, A BLANK IS PRINTED.
C
C        DATA SET USE 
C           KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES 
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                       WHEN IP(1) NE 0, KFILDO IS SET = IP(1). 
C                       (INPUT-OUTPUT)
C               IP(J) = INDICATES WHETHER (>0) OR NOT (=0) PARTICULAR
C                       SEGMENTS OF PRINT WILL BE DONE AND IF SO, THE
C                       UNIT NUMBER FOR THE PRINT FILE (J=1,25)..  (INPUT)
C             IUSE(J) = INDICATES WHETHER (>0) OR NOT (=0) THIS JTH VALUE
C                       OF IP( ) IS USED BY THE CALLING PROGRAM.
C                       (INPUT)
C 
C        NONSYSTEM SUBROUINES USED 
C            NONE 
C
      CHARACTER*5 FMT(27)
C
      DIMENSION IP(25),IUSE(25)
C
      DATA FMT/27*'     '/ 
C
      FMT(1)='(8X  '
      FMT(27)=')    '
C
      DO 120 J=1,25
C
      IF(IUSE(J).GT.0)THEN
         FMT(J+1)=',I3  '
      ELSE
         IP(J)=0
         FMT(J+1)=',I3.0'
      ENDIF 
C   
 120  CONTINUE 
C
      WRITE(KFILDO,130)(J,J=1,25)
 130  FORMAT(' IP( ) =',25I3)
      WRITE(KFILDO,FMT)(IP(J),J=1,25)
      RETURN
      END
                                    
