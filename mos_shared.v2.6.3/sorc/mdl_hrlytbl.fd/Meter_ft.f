 
C *********************************************************************

      REAL FUNCTION METER_FT(H) 
        IMPLICIT NONE 
 
C       FUNTION TO CONVERT METERS TO FEET            

      REAL, INTENT(IN) :: H
      METER_FT = H/.3048       !  GEMPAK CONVERSION
C     METER_FT = H/.3050       !  BUFR FILES

C     END FUNCTION METER_FT
      END 
