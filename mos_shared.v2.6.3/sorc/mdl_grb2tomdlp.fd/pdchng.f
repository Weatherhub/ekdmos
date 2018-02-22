      SUBROUTINE PDCHNG(ID68,ID63)
C
C$$$   SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: PDCHNG
C   PRGMMR: RUDACK          ORG: W/OSD21          DATE: 2003-07-01
C
C ABSTRACT: TAKES 10 VALUES IN PDS W3FI68 FORM AND PUTS THEM IN THE
C           APPROPRIATE W3FI63 LOCATIONS.  ALL OTHER VALUES HAVE BEEN
C           PREVIOUSLY INITIALIZED TO -1.
C 
C PROGRAM HISTORY LOG:
C                                                                      
C        MARCH      1997    ERICKSON  MDL
C        JULY       2003    RUDACK    MDL ADAPTED CODE TO MDL STANDARDS.
C        OCTOBER    2007    RUDACK        INCREASED DIMENSION OF ARRAY
C                                         "ID68( , )" BY FIVE TO ACCOMODATE
C                                         ELEMENTS CONTAINING THRESHOLD 
C                                         VALUES.
C        NOVEMBER   2007    JRW       MDL COMPLETED OPERATIONAL DOCUMENTATION
C
C USAGE: CALLED BY RDGRIB1
C                                                                               
C DATA SET USE:
C           NONE
C                                                                      
C VARIABLES:    
C          ID68(J,K) = ARRAY CONTAINING PDS INFO IN W3FI68 FORMAT.
C                      ONLY THE FOLLOWING ELEMENTS ARE 
C                      SET - OTHERS = -1 (J=1,30) (K=1,NFIELDS).
C                      (3)-ORIGINATING CENTER OF THE DATA (GRIB OCTET 5)
C                      (4)-GENERATION PROCESS ID NUMBER (GRIB OCTET 6)
C                      (5)-GRID IDENTIFICATION NUMBER (GRIB OCTET 7) 
C                      (8)-INDICATOR OF PARAMETER AND UNITS (GRIB OCTET 9)
C                      (9)-INDICATOR OF TYPE OF LEVEL OR LAYER (GRIB OCTET 10) 
C                      (10)-VALUE 1 OF LEVEL BEING PROCESSED (GRIB OCTET 11)
C                      (11)-VALUE 2 OF LEVEL BEING PROCESSED (GRIB OCTET 12)
C                      (18)-P1 (PERIOD OF TIME) (GRIB OCTET 19)
C                      (19)-P2 (PERIOD OF TIME) (GRIB OCTET 20)
C                      (20)-TIME RANGE INDICATOR (E.G. AVERAGE) (GRIB OCTET 21) 
C            ID63(J) = 27 INTEGER ARRAY CONTAINING PDS INFO IN W3FI63 FORMAT.
C                      GETBG1 REQUIRES THIS FORM (J=1,200).  (OUTPUT)
C                                                                       
C SUBROUTINES CALLED:  NONE
C
C ATTRIBUTES:
C   LANGUAGE:  FORTRAN 90 (xlf90 compiler)
C   MACHINE:  IBM SP
C$$$
C
      DIMENSION ID68(30),ID63(200)
C
      ID63(1)=ID68(3)
      ID63(2)=ID68(4)
      ID63(3)=ID68(5)
      ID63(5)=ID68(8)
      ID63(6)=ID68(9)
      ID63(7)=(ID68(10)*256)+ID68(11)
      ID63(14)=ID68(18)
      ID63(15)=ID68(19)
      ID63(16)=ID68(20)
C
      RETURN
      END
