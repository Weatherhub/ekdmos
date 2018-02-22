

C *********************************************************************
C                                                                     *
C                      BEGIN FUNCTIONS                                *
C                                                                     *
C *********************************************************************

      REAL FUNCTION KELVIN_F(T)
        IMPLICIT NONE 
 
C       FUNCTION TO CONVERT TEMPERATURE FROM KELVIN TO
C       DEGREES FAHRENHEIT.  IN EARLY TESTING, THE VALUES
C       WERE ROUNDING UP TOO OFTEN, SO AN EXTRA HUNDREDTH
C       WAS SUBTRACTED FROM THE KELVIN CONVERSION FACTOR OF 273.15.
C       THIS RESULTED IN VALUES CLOSER TO THE ACTUAL METAR REPORTS.
C 
C       IN AUGUST 1998, THE NCEP BUFR FILES WERE CHANGED TO INCREASE
C       THE DECIMAL PLACES STORED FOR THE TEMPERATURES.  IT BECAME
C       UNNECESSARY TO 'FUDGE' THE CONVERSION BECAUSE OF ROUNDING.

      REAL, INTENT(IN) :: T
      KELVIN_F = (T-273.15)*(9.0/5.0)+32.0
C     KELVIN_F = (T-273.16)*(9.0/5.0)+32.0

C     END FUNCTION KELVIN_F
      END 
