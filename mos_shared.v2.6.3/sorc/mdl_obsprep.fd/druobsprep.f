C$$$  MAIN PROGRAM DOCUMENTATION BLOCK  ***                             
C                                                                       
C MAIN PROGRAM: MDL_OBSPREP
C   PRGMMR: ERICKSON         ORG: OSD211      DATE: 2000-05-22
C                                                                       
C ABSTRACT: DRIVER FOR PROGRAM OBSPREP.  AN ATTEMPT HAS  
C   BEEN MADE TO INCLUDE ALL INFORMATION IN THIS
C   DRIVER THAT THE USER OF OBSPREP MIGHT HAVE TO
C   CHANGE.  OBSPREP RETRIEVES THE METAR SURFACE 
C   OBSERVATIONS FROM SFCTBL.XX AND PUTS THEM IN 
C   MDLPACK FORMAT FOR USE BY U900. 
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   00-05-15  ALLEN                               
C                                                                       
C USAGE:                                                                
C                                                                       
C        DATA SET USE                                                   
C        INPUT FILES:
C             FORT.10 - NCEPDATE                 DATE FILE
C             FORT.20 - SFCTBL.XX                HOURLY METAR DATA FILE 
C             FORT.27 - MDL_STATION.TBL          MDL STATION DICTIONARY 
C             FORT.26 - MDL_STATIONS.1081.ALPHA  MOS STATION LIST
C             FORT.5  - MDL_OBSPREP[GFS/MRF].DAT INPUT VARIABLE LIST
C                       (KFILDI)
C
C        OUTPUT FILES:  (INCLUDING WORK FILES)
C             FORT.70 - PKOBS.$CYCLE             OBS IN MDLPACK FORM 
C             FORT.06 - KFILDO                   STANDARD OUT
C                                                                       
C        VARIABLES     
C                  
C               IPACK = THE ARRAY TO HOLD THE ACTUAL PACKED 
C                       MESSAGE (J=1,MAX OF ND5)
C               IS0() = MOS-2000 GRIB SECTION 0 ID'S (J=1,4)
C               IS1() = MOS-2000 GRIB SECTION 1 ID'S (J=1,21+)
C               IS2() = MOS-2000 GRIB SECTION 2 ID'S (J=1,12)
C               IS4() = MOS-2000 GRIB SECTION 4 ID'S (J=1,4)
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING USED
C                       (EITHER 32 OR 64). 
C              L3264W = NUMBER OF WORDS IN A 64 BITS (EITHER 1 OR 2). 
C                 ND1 = MAXIMUM NUMBER OF STATIONS THAT CAN BE DEALT WITH 
C                       IN THE FILE CREATION. 
C                 ND5 = THE MAXIMUM NUMBER OF STATION CALL LETTERS ON 
C                       THE INPUT FILES.  
C                 ND7 = DIMENSION OF IS0(), IS1(), IS2(), AND IS4().
C
C             FOR ADDITIONAL VARIABLES, SEE SUBROUTINE OBSPREP.F
C
C                              
C        SUBPROGRAMS CALLED:   
C          UNIQUE: - CNVTWX, GETOBS, OBSPREP
C
C          LIBRARY:
C            W3LIB - W3TAG
C           MDLLIB - GET_NCEPDATE, PACK1D, RDSTAD, TRAIL, UPDAT
C                                                                       
C        EXIT STATES:
C          COND =    0 - SUCCESSFUL RUN 
C                    2 - DATE IN SFCTBL.XX FILE DOES NOT MATCH
C                        THAT REQUESTED (GETOBS)
C                    6 - SFCTBL.XX FILE WAS EMPTY (GETOBS)
C                   10 - ERROR READING NCEP DATE FILE (GET_NCEPDATE)
C                   33 - ERROR READING STATION DIRECTORY (RDSTAD)
C                   34 - STATION LIST TOO LONG FOR DIMENSION ND5 (RDSTAD)
C                   35 - ONE OR MORE STATIONS NOT FOUND IN STATION DIRECTORY
C                        (RDSTAD)
C                   36 - ONE OR MORE DUPLICATE STATIONS READ IN (RDSTAD)
C                   37 - BOTH IER 35 AND 36 HAVE OCCURRED (RDSTAD)
C                  134 - XMISSP AND XMISSS INCONSISTENT (PACK1D)
C                 9101 - WEATHER ELEMENT NOT RECOGNIZED (CNVTWX)
C                 9103 - CLOUD AMOUNT NOT RECOGNIZED (CNVTWX)
C                    OTHER IER ERROR CODES MAY COME FROM INTERNALLY
C                    CALLED SUBROUTINES.
C                                                                       
C REMARKS:  
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90 (XLF90 COMPILER, QARCH=604, O3)
C   MACHINE:  IBM SP
C
C$$$                                                                    

       PROGRAM DRUOBSPREP
C
      IMPLICIT NONE
      INTEGER L3264B,L3264W,ND1,ND5,ND7
      INTEGER KFILDI,KFILDO
      PARAMETER (L3264B=32)
      PARAMETER (L3264W=64/L3264B)
      PARAMETER (ND1=3100)
      PARAMETER (ND5=3500)
      PARAMETER (ND7=100)
      INTEGER,DIMENSION(ND5)::IPACK
      INTEGER,DIMENSION(ND7)::IS0,IS1,IS2,IS4
C
      DATA KFILDI/5/,KFILDO/6/,IS0/100*0/,IS1/100*0/,IS2/100*0/
      DATA IS4/100*0/
C
      CALL W3TAGB('MDL_OBSPREP',2000,0143,0065,'OSD211')
      CALL TIMPR(KFILDO,KFILDO,'START OBSPREP       ')
C
      CALL OBSPREP(KFILDI,KFILDO,L3264B,L3264W,
     &          ND1,ND5,ND7,IS0,IS1,IS2,IS4,IPACK)
C
      CALL TIMPR(KFILDO,KFILDO,'END OBSPREP         ')
      CALL W3TAGE('MDL_OBSPREP')
C
      END
