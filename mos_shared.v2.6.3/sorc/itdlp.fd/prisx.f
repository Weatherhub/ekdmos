      SUBROUTINE PRISX(KFILDO,ND7,ISECT,IS0,IS1,IS2,IS4,IER)
C
C        HISTORY
C
C        MAY       2013   ENGLE       CREATED FOR ITDLP.
C
C        PURPOSE
C
C            TO PRINT TO SCREEN, THE MOS-2000 IDENTIFICATION
C            SECTIONS FOR A GIVEN TDLPACK DATA RECORD.
C
C        VARIABLES (INPUT/OUTPUT)
C
C              KFILDO = UNIT NUMBER OF DEFAULT PRINT FOR MOS-2000
C                       ROUTINES. HERE KFILDO IS A SCRATCH FILE.
C                 ND7 = DIMENSION IF IS0( ), IS1( ), IS2( ), AND IS4 ( ).
C              IS0(I) = HOLDS THE BYTES DECODED FROM SECTION 0 (I=1,3).
C              IS1(I) = HOLDS THE BYTES DECODED FROM SECTION 1
C                       (I=1,MAX OF ND7).
C              IS2(I) = HOLDS THE OCTETS DECODED FROM SECTION 2
C                       (I=1,12).  SECTION 2 IS NOT PRESENT WHEN
C                       DATA ARE NOT GRIDPOINT.
C              IS4(I) = HOLDS THE BYTES DECODED FROM SECTION 4 (I=1,4),
C                       EXCEPT FOR THE DATA THEMSELVES.
C               ISECT = CONTAINS INFORMATION AS TO WHICH IS SECTION IS
C                       TO BE PRINTED.
C                 IER = ERROR RETURN VALUE FROM SUBROUTINES.
C
C        VARIABLES (INTERNAL)
C
C               CTDLP = HOLDS THE CHARACTER VERSIONS OF IS0(1) WHICH
C                       SHOULD EQUAL 'TDLP'.
C              BTEST0 = LOGICAL VARIABLE TO DETERMINES, IF TRUE, TO
C                       PRINT CONTENTS OF IS0( ).
C              BTEST1 = LOGICAL VARIABLE TO DETERMINES, IF TRUE, TO
C                       PRINT CONTENTS OF IS1( ).
C              BTEST2 = LOGICAL VARIABLE TO DETERMINES, IF TRUE, TO
C                       PRINT CONTENTS OF IS2( ).
C              BTEST4 = LOGICAL VARIABLE TO DETERMINES, IF TRUE, TO
C                       PRINT CONTENTS OF IS4( ).
C               
      IMPLICIT NONE
C        INPUT/OUTPUT VARIABLES
      INTEGER, INTENT(IN) :: KFILDO,ND7,ISECT
      INTEGER, INTENT(IN), DIMENSION(ND7) :: IS0,IS1,IS2,IS4
      INTEGER, INTENT(INOUT) :: IER
C        INTERNAL VARIABLES
      INTEGER :: I
      CHARACTER*4 :: CTDLP
      LOGICAL :: BTEST0,BTEST1,BTEST2,BTEST4
C        INITIALIZE VARIABLES
      CTDLP='    '
      IER=0
C
C        SET CTDLP AND PERFORM A BINARY TEST ON A BIT
C        OF ISECT. THE BIT NUMBER CORRESPONDS TO EACH
C        IS NUMBER. FOR EXAMPLE, IF BIT 0 IS EQUAL TO
C        1, THEN BTEST0=.TRUE.
C
      CTDLP=TRANSFER(IS0(1),CTDLP)
      BTEST0=BTEST(ISECT,0)
      BTEST1=BTEST(ISECT,1)
      BTEST2=BTEST(ISECT,2)
      BTEST4=BTEST(ISECT,4)
C
C        PRINT CONTENTS OF IS0( ) ARRAY IF BTEST0=.TRUE.
C
      IF(BTEST0)THEN
         WRITE(6,100)CTDLP,(I,IS0(I),I=2,3)
 100     FORMAT(3X,'IS0(1) = ',A4,/
     1          3X,'IS0(',I0.1,') = ',I0.1,/
     2          3X,'IS0(',I0.1,') = ',I0.1)
      ENDIF
C
C        PRINT CONTENTS OF IS1( ) ARRAY IF BTEST1=.TRUE.
C
      IF(BTEST1)THEN
         WRITE(6,110)(I,IS1(I),I=1,22)
 110     FORMAT(3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1,/
     1          3X,'IS1(',I0.1,') = ',I0.1)
      ENDIF
C
C        PRINT CONTENTS OF IS2( ) ARRAY IF BTEST2=.TRUE.
C        AND IF IS2 SECTION EXISTS (GRIDDED RECORD).
C
      IF(BTEST2.AND.(IS1(2).EQ.1))THEN
         WRITE(6,120)(I,IS2(I),I=1,12)
 120     FORMAT(3X,'IS2(',I0.1,') = ',I0.1,/
     1          3X,'IS2(',I0.1,') = ',I0.1,/
     1          3X,'IS2(',I0.1,') = ',I0.1,/
     1          3X,'IS2(',I0.1,') = ',I0.1,/
     1          3X,'IS2(',I0.1,') = ',I0.1,/
     1          3X,'IS2(',I0.1,') = ',I0.1,/
     1          3X,'IS2(',I0.1,') = ',I0.1,/
     1          3X,'IS2(',I0.1,') = ',I0.1,/
     1          3X,'IS2(',I0.1,') = ',I0.1,/
     1          3X,'IS2(',I0.1,') = ',I0.1,/
     1          3X,'IS2(',I0.1,') = ',I0.1,/
     1          3X,'IS2(',I0.1,') = ',I0.1)
      ENDIF
C
C        PRINT CONTENTS OF IS4( ) ARRAY IF BTEST4=.TRUE.
C
      IF(BTEST4)THEN
         WRITE(6,130)(I,IS4(I),I=1,7)
 130     FORMAT(3X,'IS4(',I0.1,') = ',I0.1,/
     1          3X,'IS4(',I0.1,') = ',I0.1,/
     1          3X,'IS4(',I0.1,') = ',I0.1,/
     1          3X,'IS4(',I0.1,') = ',I0.1,/
     1          3X,'IS4(',I0.1,') = ',I0.1,/
     1          3X,'IS4(',I0.1,') = ',I0.1,/
     1          3X,'IS4(',I0.1,') = ',I0.1)
      ENDIF
C
      RETURN
      END SUBROUTINE PRISX
