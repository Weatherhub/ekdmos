
      SUBROUTINE PRMSGA(MSG,N1,N2,KOUNT,M1,M2,NUNIT,NOHEAD,NTAIL,
     * NBSTA,NESTA)
C          GLAHN   FEBRUARY 1976  IBM 360/195                           
C          GILBERT MARCH 1991  NAS 9000  - COMPILED IN FORTRAN 77       
C                                        - ADDED NOHEAD PARAMETER
C          WEISS   SEPT 1995   HOBBS AND CRAY C90
C          WEISS   DEC 1995    MODIFIED VERSION FOR RFCMXMN

C          PURPOSE                                                      
C            TO PRINT MESSAGE IN FORMAT OF TELETYPE MESSAGE

CCCCCCCCC
C
C        VARIABLES                                                      
C              MSG(,) = ARRAY CONTAINING THE ALPHANUMERIC (EBCDIC) FORE-
C                       CAST MESSAGE (INPUT)                            
C                  N1 = 1ST DIM. OF MSG(,) = NO. OF CHARS IN LINE       
C                  N2 = 2ND DIM. OF MSG(,) = NUMBER OF LINES            
C            KOUNT(,) = 1, IF A FORECAST IS ON THAT LINE                
C                  M1 = 1ST DIM. OF KOUNT(,) = NO. OF LINES             
C                  M2 = 2ND DIM. OF KOUNT(,) = NO. OF STATIONS
C                NADD = NUMBER OF BLANK LINES BETWEEN BULLETINS
C              NOHEAD = NUMBER OF LINES IN HEADER, INCLUDING BLANK LINES
C                NPOS = RECORD POSITION NUMBER OF MSG BULLETIN ARRAY
C               NUNIT = DATA SET REFERENCE NUMBER FOR OUTPUT
C               NTAIL = NUMBER OF LINES IN BULLETIN TAIL, INCLUDING BLAN
C               NBSTA = STARTING STATION CALL LETTER NUMBER PER BULLETIN
C               NESTA = ENDING STATION CALL LETTER NUMBER PER BULLETIN
C
C                       NOHEAD=5  NTAIL=1
CCCCCCCCC

      INTEGER KOUNT(M1,M2)
      INTEGER NPOS
      CHARACTER*1 MSG(N1,N2)
      DATA NPOS/0/
      SAVE NPOS

      IF(NPOS.EQ.0) WRITE(NUNIT,105)
 105  FORMAT('1')
C        PRINT NOHEAD LINES OF THE BULLETIN HEADER                      
      DO 112 K=1,NOHEAD
	NPOS=NPOS+1
	WRITE(NUNIT,110) (MSG(J,NPOS),J=1,N1)
 110    FORMAT(' ',150A1)
 112  CONTINUE

C        PRINT (NESTA-NBSTA) NUMBER OF STATION OUTPUTS

      DO 131 J=NBSTA,NESTA
	NPOS=NPOS+1
        DO 130 K=1,M1
          IF (KOUNT(K,J).NE.0) THEN
            WRITE(NUNIT,111) (MSG(L,NPOS),L=1,N1)
 111        FORMAT(' ',150A1)
          ENDIF
 130    CONTINUE
 131  CONTINUE

C        PRINT NTAIL LINE OF THE TAIL PORTION OF THE BULLETIN

      DO 151 K=1,NTAIL
	NPOS=NPOS+1
	WRITE(NUNIT,110) (MSG(J,NPOS),J=1,N1)
 151  CONTINUE

      RETURN
      END
