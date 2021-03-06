      SUBROUTINE FILE_INFO(KFILDO,CFILP,ISSQ,ISRA,IRECTYPE,IENDIAN,
     1                     MASTER)
C
C        HISTORY
C
C        SEPTEMBER 2013   ENGLE       CREATED.
C
C        PURPOSE
C
C            TO PRINT TO SCREEN, INFORMATION ABOUT THE INPUT TDLPACK
C            FILE, MAINLY THE FILE TYPE, TDLPACK DATA TYPE, AND FILE
C            ENDIANNESS.
C
C        DATA SET USE
C            KFILDO - DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C                     (OUTPUT)
C
C        VARIABLES (INPUT/OUTPUT)
C              KFILDO = DEFAULT UNIT NUMBER FOR OUTPUT (PRINT) FILE.
C               CFILP = CHARACTER*60 VARIABLE THAT CONTAINS THE NAME OF
C                       THE TDLPACK FILE (SEQ OR RA).
C                ISSQ = VARIABLE TO STORE IF FILE IS SEQUENTIAL.
C                ISRA = VARIABLE TO STORE IF FILE IS RANDOM-ACCESS.
C             IENDIAN = HOLDS EITHER THE VALUE OF IRAEND OR IFILEND.
C            IRECTYPE = VALUE FOR EACH TYPE OF RECORD FOUND IN A TDLPACK FILE.
C           MASTER(I) = INTEGER ARRAY (I=1,6) THAT HOLDS THE RANDOM ACCESS
C                       MASTER KEY RECORD.
C
C        VARIABLES (INTERNAL)
C           CFILETYPE = CHARACTER STRING THE REPRESENTS THE FILE TYPE.
C           CTDLPTYPE = CHARACTER STRING THE REPRESENTS THE TDLPACK DATA TYPE.
C             CENDIAN = CHARACTER STRING THE REPRESENTS THE FILE ENDIANNESS.
C
C        SUBROUTINES CALLED
C           NONE
C
      IMPLICIT NONE
C        INPUT/OUTPUT VARIABLES
      INTEGER, INTENT(IN) :: KFILDO,ISSQ,ISRA,IRECTYPE,IENDIAN
      INTEGER, INTENT(IN), DIMENSION(6) :: MASTER
      CHARACTER(LEN=*), INTENT(IN) :: CFILP
C        INTERNAL VARIABLES
      CHARACTER(LEN=15) :: CFILETYPE,CTDLPTYPE,CENDIAN
C
C        SET CHARACTER STRING FOR THE FILE TYPE
C
      IF(ISSQ.EQ.1) CFILETYPE='SEQUENTIAL'
      IF(ISRA.EQ.1) CFILETYPE='RANDOM ACCESS'
C
C        SET CHARACTER STRING FOR THE TDLPACK DATA TYPE
C
      IF(IRECTYPE.EQ.1) CTDLPTYPE='VECTOR'
      IF(IRECTYPE.EQ.2) CTDLPTYPE='GRIDDED'
C
C        SET CHARACTER STRING FOR FILE ENDIANNESS
C
      IF(IENDIAN.EQ.-1) CENDIAN='LITTLE-ENDIAN'
      IF(IENDIAN.EQ.+1) CENDIAN='BIG-ENDIAN'
C
C        PRINT FILE INFORMATION TO SCREEN
C
      WRITE(6,110)TRIM(CFILP),TRIM(CFILETYPE),TRIM(CTDLPTYPE),
     1            TRIM(CENDIAN) 
 110  FORMAT(/'             File: ',A/,
     1        'Fortran File Type: ',A/,
     2        'TDLPACK File Type: ',A/,
     3        '       Byte Order: ',A)
C
C        PRINT RA MASTER KEY INFORMATION
C
      IF(ISRA.EQ.1)WRITE(6,115)MASTER(2:6)
 115  FORMAT( '     RA File Info: ',/
     1        '             NIDS: ',I0.1/,
     2        '           NWORDS: ',I0.1/,
     3        '           NKYREC: ',I0.1/,
     4        '           MAXENT: ',I0.1/,
     5        '           LASTKY: ',I0.1)
      WRITE(6,FMT='()')
C
      RETURN
      END SUBROUTINE FILE_INFO
