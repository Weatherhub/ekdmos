      SUBROUTINE RDLNK(KFILD,KFILDO,NEW,LIST,CCALL,NSTA,ND1)
C
C        FEBRUARY 2000   DREWRY   TDL   MOS-2000
C        MAY 2000	 ALLEN    CREATED THIS SUBROUTINE FROM PARTS
C                                 OF U630, AND ALLOWED THE USER TO
C                                 PASS IN A STATION LIST RATHER THAN
C                                 READING IT IN FROM AN INPUT FILE. 
C        MARCH   2006    MALONEY  REMOVED "EXIT" AFTER INCREMENT OF 
C                                 IFILCNT.  IF A STATION IS DUPLICATED
C                                 IN LIST( ) (KNYL AND KYUM BOTH IN THE
C                                 BUFR) THIS WILL MAKE SURE EACH ENTRY
C                                 WILL GET THE PROPER LINKS AND DATA.
C
C        PURPOSE
C            THIS SUBROUTINE WILL TAKE AN INPUT STATION LIST THAT
C            IS SENT IN THROUGH "LIST", PLACE IT IN CCALL(,1) AND 
C            SEARCH THROUGH THE LINKS IN THE STATION DICTIONARY. 
C 
C            IF NEW=1 : IF THE STATION IN LIST
C            MATCHES ANY OF THE LINKS IN THE DICTIONARY, THE ENTRY
C            IN THE LEFT MOST COLUMN OF THE DICTIONARY WILL BE PUT 
C            INTO CCALL(,1).
C            THIS OPTION GIVES YOU THE NEWEST CALL LETTERS
C            IN CCALL(,1).
C
C            IF NEW=0 : IF THE STATION IN LIST MATCHES ANY
C            OF THE LINKS IN THE DICTIONARY, THE CCALL(,1) REMAINS
C            UNCHANGED (IT CONTAINS YOUR INPUT LIST), AND ANY
C            LINKS ARE SAVED IN COLUMNS 2-6 OF CCALL.
C            THIS OPTION KEEPS YOUR INPUT CALL LETTERS
C            IN CCALL(,1).
C
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF DIAGNOSTIC OUTPUT FILE. (OUTPUT)
C            KFILD     - UNIT NUMBER OF STATION DICTIONARY (J=2). (INPUT)
C   
C        VARIABLES
C
C        CCALL(ND1,6) = CALL LETTERS OF THE STATIONS IN THE EQUATION FILE.
C             CDICTMP = VARIABLE USED TO READ STATION IDS FROM THE STATION DICTIONARY.
C              COLDFL = FLAG READ FROM THE STATION DICTIONARY INDICATING IF THE
C                       CALL LETTERS OF THAT PARTICULAR ROW ARE THE OLDER 
C                       VERSIONS OF A DIFFERENT ROW, AND CONSEQUENTLY SHOULD NOT
C                       BE USED.
C             CTMPNAM = STORES A STATION NAME READ FROM THE STATION DICTIONARY.
C              DIRNAM = HOLDS THE NAMES OF THE MASTER STATION LIST AND 
C                       STATION DICTIONARY.
C                   I = LOOP CONTROL VARIABLE. 
C                 IOS = IOSTAT RETURN.
C                   J = LOOP CONTROL VARIABLE. 
C                LINK = STATION LIST THAT YOU WANT TO FIND LINKS FOR.
C                   K = LOOP CONTROL VARIABLE. 
C               KFILD = UNIT NUMBER OF STATION DICTIONARY.
C                 NEW = 0 WHEN THE CALL LETTERS FROM THE INPUT LIST 
C                         ARE TO BE OUTPUT/RETAINED.
C                     = 1 WHEN THE NEWEST CALL LETTERS ARE TO BE OUTPUT. 
C                NSTA = NUMBER OF STATIONS READ FROM THE EQUATION FILE.
C
C        NONSYSTEM SUBROUTINES CALLED
C
      IMPLICIT NONE
C
      INTEGER       :: KFILD,KFILDO,IFILCNT,ITMPVAR,IOS,  
     2                 I, J, K,IFOUND,NSTA,IOSTAT,NEW,ND1 
C
      CHARACTER*1    :: COLDFL
      CHARACTER*8    :: CCALL, CDICTMP, LIST
      CHARACTER*20   :: CTMPNAM
C
      DIMENSION CDICTMP(6)
      DIMENSION IFOUND(ND1)
      DIMENSION CCALL(ND1,6), LIST(ND1)
C   
C           INITIALIZE THE CCALL ARRAY.  SET CCALL(J,1) TO THE
C           LIST YOU SENT IN IN LIST
C 
         DO I=1,ND1
            CCALL(I,1)=LIST(I)
         DO J=2,6
            CCALL(I,J)=' '
         ENDDO
         ENDDO
C
C            READ IN THE REMAINING 5 FIELDS OF THE EQUATION FILE STATION
C            CALL LETTER ARRAY FROM THE DICTIONARY.
         IFILCNT = 0 
         DO I = 1,6
            CDICTMP(I) = ' '
         ENDDO
         STADIC: DO 
            READ(KFILD, '(A8,1X,A8,1X,A20,42X,A1,1X,A8,1X,A8,1X,A8,
     1         1X,A8)', IOSTAT=IOS, END=100) CDICTMP(1), CDICTMP(2),
     2         CTMPNAM, COLDFL, CDICTMP(3), CDICTMP(4), CDICTMP(5), 
     3         CDICTMP(6)
C 
C               CHECK THE VALUES READ FROM EACH LINE OF THE STATION DICTIONARY  
C               AGAINST THOSE IN THE STATION LIST UNTIL A MATCH IS FOUND OR THE 
C               STATION LIST IS FINISHED.
            J = 1
            DO 
               IF (J .GT. NSTA) EXIT
               IF (((CCALL(J,1) .EQ. CDICTMP(1)) .OR. 
     1              (CCALL(J,1) .EQ. CDICTMP(2)) .OR.
     2              (CCALL(J,1) .EQ. CDICTMP(3)) .OR. 
     3              (CCALL(J,1) .EQ. CDICTMP(4)) .OR. 
     4              (CCALL(J,1) .EQ. CDICTMP(5)) .OR. 
     5              (CCALL(J,1) .EQ. CDICTMP(6))) .AND. 
     6              (COLDFL .NE. 'O')) THEN
C
C                    FILLS IN THE FIELDS OF CCALL ACCORDING TO THE ORDER IN
C                    THE STATION DICTIONARY.
                  IF (NEW .EQ. 1) THEN
                     CCALL(J,1) = CDICTMP(1)
                     CCALL(J,2) = CDICTMP(2)
                     CCALL(J,3) = CDICTMP(3)
                     CCALL(J,4) = CDICTMP(4)
                     CCALL(J,5) = CDICTMP(5)
                     CCALL(J,6) = CDICTMP(6)
                  ELSE
C                       FILLS THE REMAINING FIELDS IN CCALL, LEAVING THE 
C                       FIRST FIELD ALONE, PRESERVING THE CALL LETTERS WHICH 
C                       WERE READ FROM THE EQUATION FILE AND WHICH WILL BE OUTPUT.
                     ITMPVAR = 2
                     DO K = 1,6
C                          IF THE STATION ID IS THE SAME AS THE ID IN THE
C                          FIRST COLUMN OF CCALL, IT IS SKIPPED OVER, BECAUSE
C                          THAT ID WILL BE LEFT IN THE FIRST COLUMN.
                        IF (CCALL(J,1) .EQ. CDICTMP(K)) THEN
                           CYCLE
                        ELSE
                           CCALL(J,ITMPVAR) = CDICTMP(K) 
                           ITMPVAR = ITMPVAR + 1
                        ENDIF
                     END DO
                  ENDIF
                  IFILCNT = IFILCNT + 1
CCCCC                  EXIT
               END IF                
               J = J + 1
            ENDDO     
C              CHECKS IF ALL OF THE STATIONS HAVE BEEN FOUND IN THE DICTIONARY.
            IF (IFILCNT .EQ. NSTA) EXIT
         ENDDO STADIC
C
 100  RETURN
      END
      
