C *********************************************************************

      SUBROUTINE CLOUD(CHCLDA,CHCLDH,CLDA,CLDH,CLOUDS,TYPE,VVISI,VVIS,
     &                 LAYER)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                                                                       
C SUBPROGRAM:  CLOUD.F       FORMATS CLOUD AMOUNTS AND HEIGHTS          
C   PRGMMR: K. HUGHES        ORG: W/OSD211     DATE: 97-17-10           
C                                                                       
C ABSTRACT: THIS SUBROUTINE CONVERTS THE CLOUD HEIGHTS FROM METERS TO
C           100'S OF FEET, AND CHANGES THE CLOUD AMOUNT FROM A NUMBER TO
C           A CHARACTER CATEGORY FOR THE HOURLY TABLE.  IT ALSO DECODES
C           OBSCURATIONS AND PARTIAL OBSCURATIONS.  THIS SUBROUTINE
C           STOPS PROCESSING ANY CLOUD LAYERS WHEN IT ENCOUNTERS AN
C           OVERCAST LAYER.                                            
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-11-21  K. HUGHES                                                 
C   97-10-17  K. HUGHES     CHANGED HEIGHTS FOR PARTIAL OBSTRUCTIONS    
C                           FROM BLANK TO 0, REMOVED ANY OBSERVATIONS
C                           ABOVE A REPORT OF OBSCURED.  CHANGED LOOP
C                           TO COUNT FROM 1,6 INSTEAD OF 1,LAYER.                        
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     CLOUD                                                             
C                                                                       
C        96-11-21  K. HUGHES      MDL   CRAY - J916                     
C                                                                       
C        PURPOSE                                                        
C            SUBROUTINE CLOUD PROCESSES CLOUD AMOUNTS AND HEIGHT OF
C            CLOUD BASES FOR UP TO 6 LAYERS.
C                                                                       
C        DATA SET USE                                                   
C            BEGIN HERE-INDICATE INPUT,OUTPUT OR INTERNAL               
C                                                                       
C        VARIABLES                                                      
C                NAME = INDICATE INPUT,OUTPUT,I/O, OR WORK              
C              CHCLDA = CLOUD AMOUNT IN HUNDREDS OF FEET(CHAR.) -OUTPUT
C              CHCLDH = CLOUD HEIGHT CATEGORY (CHAR.)           -OUTPUT
C                CLDA = CLOUD AMOUNT (INT.)                     -WORK
C                CLDH = CLOUD HEIGHT CATEGORY (INT.)            -WORK
C              CLOUDS = ARRAY CONTAINING BUFR CLOUD INFORMATION -INPUT
C                TYPE = STATION TYPE, MAY USE IN THE FUTURE FOR -INPUT
C                       QUALITY CONTROL.  ONLY MANUAL STATIONS REPORT 
C                       UP TO 6 LAYERS, ALL OTHERS REPORT 3 LAYERS   
C               VVISI = INTEGER VALUE OF VERTICAL VISIBILITY,   -INPUT
C                       USED TO DETERMINE THE CLOUD HEIGHT FOR AN
C                       OBSCURATION                           
C                VVIS = CHARACTER VALUE OF VERTICAL VISIBILITY, -INPUT
C                       USED TO PRINT THE CLOUD HEIGHT FOR AN
C                       OBSCURATION                           
C               LAYER = NUMBER OF LAYERS IN THE REPORT RETURNED -INPUT
C                       FROM CALL TO UFBINT.  
C                                                                       
C        PROGRAM STOPS                                                  
C                  NONE                                                 
C                                                                       
C        REMARKS                                                        
C            CAUTION TO THE USER, THE VALUE RETURNED IN LAYER (NRET FROM
C            UFBINT) DID NOT ALWAYS EQUAL THE ACTUAL NUMBER OF LAYERS
C            IN THE METAR REPORT.  SOMETIMES IT WAS GREATER, BUT IT
C            WAS NEVER LESS THAN THE ACTUAL NUMBER OF CLOUD LAYERS.
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90                                                 
C   MACHINE:  CRAY4                                                   
C$$$                                                                    
      IMPLICIT NONE

      REAL, INTENT(IN)  :: CLOUDS(2,255)
      INTEGER           :: CLDA(6),CLDH(6),LAYER,J,I,K,VVISI
      CHARACTER(LEN=4)  :: TYPE    
      CHARACTER(LEN=3)  :: CHCLDH(6),TMPCLDH,CHCLDA(6),TMPCLDA,VVIS
      REAL,EXTERNAL     :: METER_FT

C       WHEN LAYER WAS RETURNED FROM BUFR IT WAS NOT RELIABLE. 
C       SOMETIMES INFORMATION ABOUT A LAYER WAS MISSING - USUALLY THE 
C       FIRST HEIGHT FOR CASES WITH MORE THAN 3 LAYERS.  INSTEAD, 
C       ASSUME THERE COULD BE SIX LAYERS.   10-17-97  KKH 
      LAYER=6
      DO I=1,6
        CLDA(I)=99
        CLDH(I)=999
      END DO

      DO I=1,6
      CHCLDA(I)='  '
      CHCLDH(I)='   '
      END DO

      DO J=1,LAYER
        CLDA(J)=NINT(CLOUDS(1,J))
        CLDH(J)=NINT((METER_FT(CLOUDS(2,J)))/100)
 
      SELECT CASE (CLDA(J))

      CASE (0)     !  CLEAR
        IF (TYPE.EQ."MANU") THEN
          CHCLDA(J)="SKC"
        ELSE
          CHCLDA(J)="CLR"
        END IF
  
C          ONCE AN OVERCAST LAYER IS ENCOUNTERED, DO NOT ALLOW ADDITIONAL
C          LAYERS TO BE PROCESSED.

      CASE (8)     !  OVERCAST
        CHCLDA(J)="OVC"
        GO TO 100    
 
C       TEST FOR OBSCURATIONS AND PARTIAL OBSCURATIONS.  THEY ARE
C       FOUND IN THE CLOUD AMOUNTS.  AS WITH OVC, DO NOT ALLOW LAYERS
C       ABOVE THE OB.
C
      CASE (9)     !  TOTAL OBSCURATION
 
        IF (VVISI.LT.999) THEN
          CHCLDA(J)="OB "
          CHCLDH(J)=VVIS
        END IF
        GO TO 100

C        IF THE CATEGORY IS FOUND TO BE A PARTIAL OBSCURATION, THEN THE
C        HEIGHT OF THAT LAYER WILL BE SET TO 0.  OTHER LAYERS ARE 
C        ALLOWED ABOVE THE PARTIAL OBSURATION.

      CASE (10)    !  PARTIAL OBSCURATION
        CHCLDA(J)="POB"
          CLDH(J)=0
      CASE (11)    !  SCATTERED
        CHCLDA(J)="SCT"
      CASE (12)    !  BROKEN
        CHCLDA(J)="BKN"
      CASE (13)    !  FEW
        CHCLDA(J)="FEW"
      CASE (99:)
        CHCLDA(J)="   "
      CASE DEFAULT
        CHCLDA(J)="   "
        PRINT *,"UNRECOGNIZABLE CLOUD LAYER REPORTED ",CLDA(J)
      END SELECT

      END DO 

  100 DO K=1,6
        WRITE(TMPCLDA,150) CLDA(K)
  150   FORMAT(I2)
          IF (K.GT.LAYER) THEN 
            CHCLDA(K)=TMPCLDA
              IF (CLDA(K).GE.99) CHCLDA(K)='  '
          END IF

        WRITE(TMPCLDH,200) CLDH(K)
  200   FORMAT(I3)
        CHCLDH(K)=TMPCLDH
          IF (CLDH(K).GE.999) CHCLDH(K)='   '
      END DO

C     END SUBROUTINE CLOUD
      END
