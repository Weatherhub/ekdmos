      SUBROUTINE PDCHNG(ID68,ID63)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    00010000
C                                                                       00020000
C SUBPROGRAM: PDCHNG         CONVERTS FROM W3FI68 to W3FI63 PDS FORM
C   PRGMMR: ERICKSON         ORG: W/OSD211     DATE: 97-03-05           00040000
C                                                                       00050000
C ABSTRACT: TAKES 10 VALUES IN PDS W3FI68 FORM AND PUTS THEM IN THE 
C           APPROPRIATE W3FI63 LOCATIONS.  ALL OTHER VALUES START AS 
C           -1. 
C                                                                       00080000
C PROGRAM HISTORY LOG:                                                  00090000
C   97-03-05  ERICKSON
C   YY-MM-DD  MODIFIER1  DESCRIPTION OF CHANGE                          00110000
C   YY-MM-DD  MODIFIER2  DESCRIPTION OF CHANGE                          00120000
C                                                                       00130000
C USAGE:                                                                00140000
C                                                                       00150000
C   SEE BELOW FOR MDL STANDARDS                                         00160000
C                                                                       00170000
C     PROGRAM NAME                                                      00180000
C                                                                       00190000
C        DATE   PROGRAMMER NAME   MDL   NAS9000                         00200000
C                                                                       00210000
C        PURPOSE                                                        00220000
C            COMMENTS BEGIN HERE                                        00230000
C                                                                       00240000
C        VARIABLES                                                      00280000
C      ID63  = 27 integer array containing pds info in W3FI63 format.
C              GETBG1 requires this form.
C      ID68  = 25 integer array containing pds info in W3FI68 format.
C              Only the following elements are set - others = -1.
C              (3)-originating center (GRIB OCTET 5)
C              (4)-model id (GRIB OCTET 6)
C              (5)-Grid id (GRIB OCTET 7) 
C              (8)-parameter (GRIB OCTET 9)
C              (9)-Type of Level (GRIB OCTET 10) 
C              (10)-Value 1 of Level (GRIB OCTET 11)
C              (11)-Value 2 of Level (GRIB OCTET 12)
C              (18)-P1 (period of time) (GRIB OCTET 19)
C              (19)-P2 (period of time) (GRIB OCTET 20)
C              (20)-Time Range Indicator (GRIB OCTET 21)
C                                                                       00440003
C        REMARKS                                                        00450000
C            ANYTHING APPROPRIATE CAN GO HERE--LIKE BUYER BEWARE        00460000
C                                                                       00470000
C ATTRIBUTES:                                                           00480000
C   LANGUAGE: FORTRAN H EXT + (OR VS FORTRAN)                           00490000
C   MACHINE:  CRAY4                                                     00500000
C$$$                                                                    00510000
      INTEGER ID68(200),ID63(200)
      ID63(1)=ID68(3)
      ID63(2)=ID68(4)
      ID63(3)=ID68(5)
      ID63(5)=ID68(8)
      ID63(6)=ID68(9)
      ID63(7)=(ID68(10)*256)+ID68(11)
      ID63(14)=ID68(18)
      ID63(15)=ID68(19)
      ID63(16)=ID68(20)
      RETURN
      END
