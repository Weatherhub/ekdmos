      SUBROUTINE RDGRIB1(KFILDO,KFILIN,KFILIO,KFILIE,KFILIC,
     1                   KFILRA,RACESS,ID,IPACK,IWORK,DATA,ND5,
     2                   NAMIN,NUMIN,KFILIX,NAMIX,ND6,IS0,IS1,IS2,IS4,
     3                   ND7,IDATE,ND8,NDATES,LDATES,ND9,ND10,
     4                   L3264B,MINPK,NTOTBY,NTOTRC,PXMISS,
     5                   XMISSP,XMISSS,JFLAG,IP4,IP11,IP12,
     6                   JCONVRT,ISTOP)
C
C$$$   SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: RDGRIB1
C   PRGMMR: RUDACK          ORG: W/OSD21          DATE: 2003-06-01
C
C ABSTRACT: TO READ GRIB1 FILES AND THEN TDLPACK THE DATA.  
C           THE DATA ARE PACKED INTO FILE 'KFILIO'. 
C           THIS SUBROUTINE UTILIZES SOME NCEP ROUTINES. 
C           NOTE: THE GRID SPECIFICATIONS IN THE GRID 
C           LIST FILE ARE THOSE THAT WILL BE TDLPACKED.  
C           SO, IF THE TDLPACK GRID IS AN OFFSET OF THE 
C           ORIGINAL NCEP GRID, THE USER MUST FIND ALL THE 
C           NEW GRID SPECIFICATIONS RELATIVE TO THE NEW (1,1) 
C           GRIDPOINT OF THE TDL GRID AND PLACE THOSE VALUES 
C           INTO THE GRID LIST FILE.  MOREOVER, IF A 
C           BITMAP IS SUPPLIED, ITS GRID SPECIFICATIONS IN 
C           THE EXTERNAL RANDOM ACCESS FILE MUST ALSO MATCH 
C           THOSE FOUND IN THE GRID LIST FILE.
C
C           ABBREVIATIONS USED IN DOCUMENTATION: 
C             1.) ELEMENT LIST FILE (ELF)
C             2.) GRID LIST FILE (GLF)
C
C PROGRAM HISTORY LOG:
C
C   JUNE      2003   RUDACK   MDL   MOS-2000
C   JUNE      2004   RUDACK   MDL   ADDED SAFEGUARD SO THAT THE
C                                   GRIB1 INPUT FILES ARE OPENED
C                                   AS READ ONLY.
C   APRIL     2005   RUDACK   MDL   CORRECTED THE TALLEY OF MISSING
C                                   RECORDS WHEN A PROCESSED INPUT 
C                                   DATE IS NOT FOUND ON THE GRIB1 
C                                   FILE.
C   SEPTEMBER 2005   RUDACK   MDL   MODIFIED CODE TO MEET OPERATIONAL 
C                                   REQUIREMENTS.
C   JUNE      2006   RUDACK   MDL   ADDED CODE TO ACCOMMODATE THE INDICATOR
C                                   OF UNIT OF TIME RANGE FOR ELEMENTS OVER
C                                   A 6-H AND 12-H PERIOD.
C   JULY      2007   RUDACK   MDL   MODIFIED PORTION OF CODE THAT ASSIGNS 
C                                   '9999' TO PXMISS.  MODIFIED FORMAT
C                                   STATEMENT 131 FROM I3 TO I6. 
C   OCTOBER   2007   RUDACK   MDL   MODIFIED FORMAT STATEMENTS PERTAINING
C                                   TO THE 4-WORD MOS-2000 IDS.  RDGRIB1
C                                   WILL NOW PRINT THE ENTIRE FOURTH WORD
C                                   MOS-2000 ID.  ELEMENTS WITH THRESHOLD
C                                   VALUES CANNOT BE ACCOMMODATED BY 
C                                   RDGRIB1.F. THE DATA MUST FIRST BE 
C                                   CONVERTED TO GRIB2.  THE DATA CAN THEN
C                                   BE PROCESSED BY RDGRIB2.  INCREASED
C                                   DIMENSION OF ID68( , ) BY FIVE IN ORDER
C                                   TO READ IN GRIB IDENTIFIERS FOR SECTION 4.
C   NOVEMBER 2007    JRW      MDL   MERGED 10/07 AND 9/05 VERSIONS.
C                                   COMPLETED OPERATIONAL DOCUMENTATION
C   OCTOBER  2012   ENGLE     MDL   JPLAIN IS NOW DEFINED AS CHARACTER*1;
C                                   FUNCTION IACHAR IS NOW USED TO PACK
C                                   PLAIN LANGUAGE; CHANGED ENNVAR TO
C                                   'FORT  '
C
C USAGE: CALLED BY U130
C
C DATA SET USE:
C         INPUT FILES:
C           FORT.KFILIN(J) - UNIT NUMBERS FOR INPUT DATA (J=1,NUMIN).  (INPUT) 
C           FORT.KFILIE    - UNIT NUMBER OF FILE CONTAINING THE ELEMENT 
C                            LIST.  (INPUT)
C           FORT.KFILIC    - UNIT NUMBER OF FILE CONTAINING THE GRID LIST. (INPUT)
C           FORT.KFILRA    - UNIT NUMBER OF GRIDDED RANDOM ACCESS FILE. (INPUT)
C           FORT.KFILIX(J) - UNIT NUMBERS OF GRIB INDEX FILES CORRESPONDING
C                            TO DATA INPUT FILES(J=1,NUMIN).  (INPUT)
C         OUTPUT FILES:     
C           FORT.KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C           FORT.KFILIO    - UNIT NUMBER OF GRIDDED TDLPACK OUTPUT FILE.
C                           (OUTPUT)
C
C VARIABLES:
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  INITIALLY,
C                       THIS IS SET BY DATA STATEMENT.  LATER, IN 
C                       IPOPEN, IF IP(1) NE 0, KFILDO IS SET = IP(1).
C                       THIS ALLOWS CHANGING THE "DEFAULT" PRINT FILE ON 
C                       THE FLY.  OTHERWISE, ON SOME SYSTEMS, THE OUTPUT
C                       FILE MIGHT HAVE THE SAME NAME AND BE
C                       OVERWRITTEN.  WHEN THE OUTPUT FILE IS NOT THE
C                       ORIGINAL DEFAULT, THE NAME IS GENERATED AND CAN
C                       BE DIFFERENT FOR EACH RUN.  (INPUT)
C           KFILIN(J) = UNIT NUMBERS FOR INPUT DATA, ALL IN GRIB1
C                       FORMAT (J=1,NUMIN). (INPUT)
C              KFILIO = UNIT NUMBER OF TDLPACK OUTPUT FILE.  (INPUT)
C              KFILIE = UNIT NUMBER OF ELEMENT LIST FILE.  (INPUT)
C              KFILIC = UNIT NUMBER OF GRID LIST FILE.  (INPUT)
C              KFILRA = UNIT NUMBER OF TDLPACK GRIDDED RANDOM ACCESS
C                       FILE; MAXIMUM NUMBER OF FILES IS ONE.  (INPUT) 
C              RACESS = NAME OF TDLPACK GRIDDED RANDOM ACCESS FILE 
C                       DATA SET.  (CHARACTER*60)  (INPUT)
C             ID(J,N) = THE INTEGER BITMAP VARIABLE ID (J=1,4).  (INPUT)
C            IPACK(J) = WORK ARRAY THAT STORES THE METADATA AND DATA 
C                       IN TDLPACK FORM (J=1,ND5).  (INTERNAL)
C            IWORK(J) = A WORK ARRAY (J=1,ND5).  (INTERNAL)
C             DATA(J) = CONTAINS THE BITMAP FIELD (J=1,ND5).  (INTERNAL) 
C                 ND5 = DIMENSION OF IPACK( ) AND IWORK( ).  SINCE IT 
C                       CANNOT BE KNOWN THE SIZE OF EACH RECORD BEFORE 
C                       READING IT, ND5 SHOULD BE AT LEAST AS LARGE AS 
C                       THE GRID DIMENSION (I.E., NX*NY) OF THE DATA
C                       ON THE GRIB1 FILE.  SET BY PARAMETER IN 
C                       DRU130.  (INPUT)
C            NAMIN(J) = HOLDS DATA SET NAMES FOR THE UNIT NUMBERS IN 
C                       KFILIN(J) (J=1,NUMIN).  (CHARACTER*60) (INPUT)
C               NUMIN = THE NUMBER OF VALUES IN KFILIN( ), NAMES IN
C                       NAMIN( ), ETC.  MAXIMUM OF ND6.  THIS IS REDUCED
C                       IF THERE IS NO VARIABLE WITH A PARTICULAR
C                       MODEL NUMBER.  (INPUT)
C           KFILIX(J) = UNIT NUMBERS OF GRIB INDEX FILES CORRESPONDING
C                       TO DATA INPUT FILES(J=1,NUMIN).  (INPUT)
C            NAMIX(J) = HOLDS DATA SET NAMES FOR UNIT NUMBERS KFILIX(J)
C                       (J=1,NUMIN).  (INPUT)
C                 ND6 = MAXIMUM NUMBER OF INPUT FILES THAT CAN BE DEALT
C                       WITH IN ONE RUN. DIMENSION OF KFILIN( ) AND
C                       NAMIN( ).  SET BY PARAMETER IN DRU130.  (INPUT)
C              IS0(J) = MOS-2000 GRIB SECTION 0 ID'S (J=1,4).  (INPUT)
C              IS1(J) = MOS-2000 GRIB SECTION 1 ID'S (J=1,21+). (INPUT)
C              IS2(J) = MOS-2000 GRIB SECTION 2 ID'S (J=1,12).  (INPUT)
C              IS4(J) = MOS-2000 GRIB SECTION 4 ID'S (J=1,4).  (INPUT)
C                 ND7 = DIMENSION OF IS0( ), IS1( ), IS2( ), AND IS4( ).
C                       NOT ALL LOCATIONS ARE USED.  MAXIMUM SIZE IS FOR
C                       IS1( ) = 22 PLUS 32 CHARACTERS (ONE CHARACTER
C                       PER WORD) OF PLAIN TEXT = 54.  SET BY PARAMETER
C                       IN DRU130.  (INPUT)
C            IDATE(J) = INITIAL DATE LIST (J=1,NDATES) WHICH MAY CONTAIN
C                       NEGATIVE VALUES INDICATING A DATE SPAN.
C                       THIS IS MODIFIED IN DATPRO TO CONTAIN THE
C                       COMPLETE DATE LIST WITH THE DATES IN THE SPANS
C                       FILLED IN (J=1,NDATES), WHERE NDATES HAS BEEN
C                       INCREASED IF NECESSARY.  DATES ARE INPUT AS
C                       YYMMDDHH AND MODIFIED TO YYYYMMDDHH.  ZEROS IN 
C                       THE INPUT ARE ELIMINATED.  TERMINATOR IS 
C                       99999999.  MAXIMUM NUMBER OF DATES IS ND8. (INPUT)
C                 ND8 = DIMENSION OF IDATE( ).  SET BY PARAMETER IN DRU130.  
C                       (INPUT)
C              NDATES = NUMBER OF VALUES IN IDATE( ).  MODIFIED AS 
C                       NECESSARY IN DATPRO.  (INPUT)
C           LDATES(J) = CONTAINS ALL THE DATES ON THE GRIB1 FILE IN
C                       CHRONOLOGICAL ORDER (J=1,ND9).  (INTERNAL)
C                 ND9 = MAXIMUM NUMBER OF RECORDS THAT ARE ANTICIPATED 
C                       TO BE ON A PARTICULAR GRIB1 FILE.  SET BY PARAMETER
C                       IN DRU130.  (INPUT)
C                ND10 = MAXIMUM NUMBER OF ELEMENTS THAT CAN BE PROCESSED  
C                       IN THE ELEMENT LIST FILE.  SET BY PARAMETER IN 
C                       DRU130.  (INPUT)  
C              L3264B = INTEGER WORD LENGTH IN BITS OF MACHINE BEING
C                       USED (EITHER 32 OR 64).  SET BY PARAMETER.  (INPUT)
C               MINPK = MINIMUM GROUP SIZE WHEN PACKING THE GRIDDED DATA
C                       VALUES.  SET IN DATA STATEMENT IN U130.  (INPUT)
C              NTOTBY = THE TOTAL NUMBER OF BYTES ON THE FILE ASSOCIATED
C                       WITH UNIT NUMBER KFILIO (THE OUTPUT TDLPACK FILE).
C                       IT IS UPDATED WHEN THE DATA IN IPACK( ) ARE
C                       WRITTEN.  (OUTPUT)
C              NTOTRC = THE TOTAL NUMBER OF RECORDS IN THE FILE.  IT IS
C                       UPDATED AS NEEDED IN WRITEP.  (OUTPUT) 
C              PXMISS = VALUE ASSIGNED TO MISSING DATA IN THE GRIB1 MESSAGE.
C                       (INPUT) 
C              XMISSP = VALUE TO PACK AS A PRIMARY MISSING. 
C                       (SET IN DATA STATEMENT IN U130.F, XMISSP=9999.). 
C                       (INPUT)
C              XMISSS = VALUE ASSIGNED TO A MASKED GRIDPOINT. 
C                       (SET IN U130.CN).  (INPUT)                   
C               JFLAG = 1 INDICATES THAT AN EXTERNAL GRIDDED RANDOM
C                       ACCESS FILE IS BEING USED TO MASK THE DATA.  
C                       (INPUT)
C                 IP4 = OUTPUT UNIT FILE THAT LIST ALL THE ELEMENTS 
C                       PROCESSED AND PACKED BY RDGRIB1.F.  (INPUT)
C                IP11 = OUTPUT UNIT FILE THAT CONTAINS THE GRIB1 GRIDDED
C                       VALUES (TO THE NEAREST THOUSANDTHS PLACE)  (INPUT)
C                IP12 = PRINT OF THE BITMAP VALUES AS READ FROM THE
C                       EXTERNAL GRIDDED RANDOM ACCESS FILE.  (INPUT)
C             JCONVRT = FLAG INDICATING IF GRIB1 (=1) OR GRIB2 (=2) DATA
C                       IS TO BE TDLPACKED.  (INPUT)
C            ISTOP(J) = ISTOP(1) IS INCREMENTED BY 1 EACH TIME
C                       AN ERROR OCCURS THAT IS NOT FATAL.  
C                       ISTOP(2) IS INCREMENTED BY 1 EACH TIME
C                       A DATA RECORD WAS NOT FOUND.  (J=1,2)
C                       (OUTPUT)
C
C INTERNAL VARIABLES:       
C                 IYR = YEAR READ FROM IDATE( ) (4 DIGIT).                        
C                 IMO = MONTH READ FROM IDATE( ).                          
C                 IDA = DAY READ FROM IDATE( ).                            
C                 IHR = HOUR (CYCLE) READ FROM IDATE( ). 
C               NDATE = THE CURRENT DATE BEING PROCESSED FROM IDATE( ).
C               KDATE = THE CURRENT DATE NUMBER IN 'NDATES' BEING PROCESSED.
C                       (E.G., DATE 1, DATE 2 ... ETC.)                         
C               IGRIB = NMC ASSIGNED GRIB VALUE IDENTIFYING TDL GRID. 
C                       READ FROM GLF.     
C               IGRID = NMC ASSIGNED VALUE OF TDL GRID.  READ FROM GLF FILE.                   
C              IPWR10 = POWER OF 10 SCALING TO USE FOR TDLPACKING. 
C                       READ FROM ELF.
C               IPWR2 = POWER OF 2 SCALING TO USE FOR TDLPACKING. 
C                       READ FROM GLF.
C             ID63(J) = ARRAY CONTAINING THE PRODUCT DEFINITION 
C                       INFORMATION IN W3FI63 FORMAT (J=1,200).  
C                       'GETBG' REQUIRES THIS FORM.                       
C           ID68(J,K) = ARRAY CONTAINING PRODUCT DEFINITION INFORMATION.
C                       ONLY THE FOLLOWING ELEMENTS ARE SET - OTHERS = -1
C                       (FOR GRIB1) OR -9999 (FOR GRIB2).
C                       (J=1,30) (K=1,NFIELDS).
C                       (3)-ORIGINATING CENTER OF THE DATA
C                       (4)-GENERATION PROCESS ID NUMBER
C                       (5)-GRID IDENTIFICATION NUMBER (FOR GRIB1 ONLY)
C                       (8)-INDICATOR OF PARAMETER AND UNITS
C                       (9)-INDICATOR OF TYPE OF LEVEL OR LAYER
C                       (10)-VALUE 1 OF LEVEL BEING PROCESSED
C                       (11)-VALUE 2 OF LEVEL BEING PROCESSED
C                       (18)-P1 FORECAST PROJECTION OR THE FORECAST PROJECTION
C                            CORRESPONDING TO THE BEGINNING OF THE FORECAST TIME
C                            PERIOD (IF AN AVERAGE OR AN ACCUMULATION VALUE
C                            IS DESIRED, FOR EXAMPLE).
C                       (19)-P2 FORECAST PROJECTION CORRESPONDING TO THE END 
C                            OF THE FORECAST TIME PERIOD.
C                       (20)-TIME RANGE INDICATOR (E.G. AVERAGE)
C                       (21)-INTEGER PRODUCT DEFINITION TEMPLATE (FOR GRIB2 ONLY)
C                       (22)-PARAMETER CATEGORY (FOR GRIB2 ONLY)
C          IDMDL(J,K) = ARRAY CONTAINING THE MOS-2000 ID TO BE 
C                       TDLPACKED (J=1,4) (K=1,ND10).  READ FROM ELF.           
C            GDATA(J) = CONTAINS THE GRIB1 FIELD BEING PROCESSED. 
C                       (DYNAMICALLY ALLOCATED ARRAY) (J=1,NX*NY) 
C             IA(J,K) = WORK ARRAY FOR 'PACK2D' (DYNAMICALLY ALLOCATED ARRAY)
C                       (J=1,NX;K=1,NY).
C             IC(J,K) = WORK ARRAY FOR 'PACK2D' (DYNAMICALLY ALLOCATED ARRAY)
C                       (J=1,NX;K=1,NY).
C              BMP(J) = LOGICAL ARRAY CONTAINING BITMAP INFORMATION 
C                       (DYNAMICALLY ALLOCATED ARRAY) (J=1,NX*NY) (NOT USED).
C             KGDS(J) = ARRAY CONTAINING THE GRID DEFINITION INFORMATION
C                       READ FROM THE GRIB1 FILE (J=1,200).
C                       LAMBERT CONFORMAL GRIDS
C                          (1) - DESIGNATED VALUE FOR LAMBERT GRID (=3)
C                          (2) - NX NR POINTS ALONG X-AXIS
C                          (3) - NY NR POINTS ALONG Y-AXIS
C                          (4) - LA1 LAT OF ORIGIN (LOWER LEFT)
C                          (5) - LO1 LON OF ORIGIN (LOWER LEFT)
C                          (6) - RESOLUTION AND COMPONENT FLAG
C                          (7) - LOV - ORIENTATION OF GRID
C                          (8) - DX - X-DIR INCREMENT
C                          (9) - DY - Y-DIR INCREMENT
C                         (10) - PROJECTION CENTER FLAG
C                         (11) - SCANNING MODE FLAG 
C                         (12) - LATIN 1 - FIRST LAT FROM POLE OF 
C                                SECANT CONE INTERSECTION
C                         (13) - LATIN 2 - SECOND LAT FROM POLE OF 
C                                SECANT CONE INTERSECTION
C                       POLAR STEREOGRAPHIC GRIDS
C                          (1) - DESIGNATED VALUE FOR POLAR STEREOGRAPHIC GRID (=5)
C                          (2) - N(I) NR POINTS ALONG LAT CIRCLE
C                          (3) - N(J) NR POINTS ALONG LON CIRCLE
C                          (4) - LA(1) LATITUDE OF ORIGIN
C                          (5) - LO(1) LONGITUDE OF ORIGIN
C                          (6) - RESOLUTION AND COMPONENT FLAG 
C                          (7) - LOV GRID ORIENTATION
C                          (8) - DX - X DIRECTION INCREMENT
C                          (9) - DY - Y DIRECTION INCREMENT
C                         (10) - PROJECTION CENTER FLAG
C                         (11) - SCANNING MODE 
C                       MERCATOR GRIDS
C                          (1) - DESIGNATED VALUE FOR MERCATOR GRID (=7)
C                          (2) - N(I) NR POINTS ON LATITUDE CIRCLE
C                          (3) - N(J) NR POINTS ON LONGITUDE MERIDIAN
C                          (4) - LA(1) LATITUDE OF ORIGIN
C                          (5) - LO(1) LONGITUDE OF ORIGIN
C                          (6) - RESOLUTION AND COMPONENT FLAG
C                          (7) - LA(2) LATITUDE OF LAST GRIDPOINT
C                          (8) - LO(2) LONGITUDE OF LAST GRIDPOINT
C                          (9) - LATIT - LATITUDE OF PROJECTION INTERSECTION
C                         (10) - RESERVED
C                         (11) - SCANNING MODE FLAG 
C                         (12) - LONGITUDINAL DIR GRID LENGTH
C                         (13) - LATITUDINAL DIR GRID LENGTH
C         JPLAIN(J,K) = PLAIN LANGUAGE IDENTIFICATION OF FIELDS READ 
C                       FROM PRODUCT DESCRIPTION FILE (J=1,NFIELDS)
C                       (K=1,ND10) (CHARACTER*1).       
C             NFIELDS = NUMBER OF ELEMENTS TO PROCESS.
C          MODELID(J) = MODEL TYPE BEING PROCESSED (E.G. RUC, ETA)  
C                       (CHARACTER*4). 
C             IMAP(J) = MAP PROJECTION TO BE TDLPACKED.  READ FROM
C                       GLF (J=1,ND10). 
C                       3 = N.H. LAMBERT 
C                       5 = N.H. POLAR STEREOGRAPHIC
C                       7 = MERCATOR
C            LPROJ(J) = MAP PROJECTION TYPE:  READ FROM THE GLF FILE 
C                       (J=1,ND10). 
C                       NPS = NORTHERN POLAR STEREOGRAPHIC
C                       LAM = LAMBERT CONICAL CONFORMAL
C                       MER = MERCATOR
C               NX(J) = X DIMENSION OF INPUT GRID (J=1,ND10).  
C                       READ FROM GLF.                          
C               NY(J) = Y DIMENSION OF INPUT GRID (J=1,ND10).  
C                       READ FROM GLF.                         
C              NXS(J) = X DIMENSION OF OUTPUT GRID (J=1,ND10).  
C                       READ FROM GLF.                        
C              NYS(J) = Y DIMENSION OF OUTPUT GRID (J=1,ND10).  
C                       READ FROM GLF.
C            IOFFX(J) = OFFSET OF THE OUTPUT GRID TO THE INPUT GRID 
C                       (X DIRECTION) (J=1,ND10).  READ FROM GLF.      
C            IOFFY(J) = OFFSET OF THE OUTPUT GRID TO THE INPUT GRID 
C                       (Y DIRECTION) (J=1,ND10).  READ FROM GLF.                  
C               DX(J) = X INCREMENT (IN KM) AT XLAT OF OUTPUT GRID. 
C                       READ FROM GLF (J=1,ND10). 
C               DY(J) = Y INCREMENT (IN KM) AT XLAT OF OUTPUT GRID.  
C                       READ FROM GLF (J=1,ND10).    
C            POLEX(J) = X COORDINATE OF NORTH POLE OF OUTPUT GRID.  
C                       READ FROM GLF (J=1,ND10).              
C            POLEY(J) = Y COORDINATE OF NORTH POLE OF OUTPUT GRID.               
C             ALAT(J) = LATITUDE OF ORIGIN (1,1) OF OUTPUT GRID 
C                       (DEGREES N) (J=1,ND10).  READ FROM GLF.    
C             ALON(J) = LONGITUDE OF ORIGIN (1,1) OF OUTPUT GRID 
C                       (DEGREES N)(J=1,ND10).  READ FROM GLF. 
C           ORIENT(J) = ORIENTATION OF OUTPUT GRID.  MERIDIAN PARALLEL TO Y    
C                       AXIS (DEGREES WEST) (J=1,ND10).  
C                       READ FROM GLF.                              
C             XLAT(J) = FOR LAMBERT CONICAL CONFORMAL, THE LATITUDE WHERE
C                       CONE IS TANGENT ON THE OUTPUT GRID.  FOR NORTH POLAR 
C                       STEREOGRAPHIC, LATITUDE WHERE GRID LENGTH APPLIES 
C                       (USUALLY 60N) (J=1,ND10).  READ FROM GLF.
C              MAXPTS = MAXIMUM NUMBER OF POSSIBLE GRIDPOINTS NEEDED IN 
C                       UNPACKING THE GRIB1 MESSAGE.
C              NUMPTS = ACTUAL NUMBER OF GRIDPOINTS IN THE GRIB1 DATA
C                       FIELD.
C                   N = TOTAL NUMBER OF DATES ON THE GRIB1 FILE BEING 
C                       PROCESSED.  THIS VALUE IS RETURNED FROM 'OVERDATE.F'. 
C               IFLAG = A VALUE OF ONE INDICATES A MATCH BETWEEN THE CURRENT
C                       DATE BEING PROCESSED AND WITH A DATE ON THE GRIB1 FILE.
C               JFLAG = VALUE INDICATING WHETHER (1) OR NOT (0) THE BITMAP
C                       DATA HAS BEEN RETRIEVED.  
C              JCLOSE = VALUE INDICATING WHETHER (1) OR NOT (0) THE CURRENT
C                       GRIB1 FILE HAS JUST BEEN CLOSED.  
C              JPRINT = FLAG (=1) INDICATING THAT THE BITMAP VARIABLE HAS
C                       ALREADY BEEN PRINTED FOR DIAGNOSTIC PURPOSES.
C          JSUBSET(J) = A VALUE OF ONE INDICATES A SUBSET GRID OF THE ORIGINAL
C                       GRIB GRID IS TO BE PROCESSED, OTHERWISE ZERO (J=1,ND10).    
C           JALLOCATE = A VALUE OF ZERO INDICATES THAT DYNAMIC MEMORY 
C                       ALLOCATION IS TO BE PERFORMED.   A VALUE OF ONE
C                       INDICATES THAT DYNAMIC MEMEORY HAS BEEN
C                       SUCCESSFULLY PERFORMED.
C         IS2_TEMP(J) = HOLDS THE GRID CHARACTERISTICS SPECIFIED IN THE GLF
C                       FOR THE GRIB1 DATA.  THIS AUTOMATIC ARRAY IS 
C                       NECESSARY BECAUSE IF A BITMAP IS USED, THE IS2( ) 
C                       VALUES ORIGINATING FROM 'CONSTG' WOULD BE USED WHEN 
C                       PACKING THE DATA RATHER THAN THE VALUES FOUND IN 
C                       THE GLF FILE.
C              JCHECK = FLAG (=1) INDICATING THAT THE BITMAP GRID
C                       CHARACTERISTICS HAVE BEEN CHECKED AGAINST THE
C                       GRID CHARACTERISTICS FOUND IN THE GRID LIST
C                       FILE.  (INTERNAL)
C                 IER = STATUS RETURN. 
C                       0 = GOOD RETURN.  SEE CALLED ROUTINES FOR OTHER
C                       VALUES.
C                       OTHER VALUES RETURNED FROM SUBROUTINES.  
C                JERR = ERROR RETURN CODE. 
C                   0 = NO ERRORS
C                   1 = MAXIMUM NUMBER OF GRIDS WERE READ.  MAY NEED TO
C                       INCREASE MAXMOD IN PARAMETER STATEMENT.
C                   2 = ERROR READING THE GRID LIST FILE.
C                   3 = REQUESTED GRID NOT FOUND IN THE GRID LIST FILE.
C 
C SUBROUTINES CALLED:
C        BAOPEN, BAOPENR, BAREAD, OVERDATE, GETGRID, PDCHNG,
C        GETGB, CONSTG, SUBGRID, GRIDASN, CHKGRID2, CHKGRID3,
C        PACK2D, WRITEP, RDELMLST, BACLOSE, SYSTEM, W3TAGE, GETENV
C
C EXIT STATES:
C   COND =   0 - SUCCESSFUL RUN
C           15 - MAX NUMBER OF FIELDS READ IN BY GETGRID.
C           17 - ERROR IN GETGRID READING GRID LIST FILE.
C           20 - REQUESTED GRID NOT FOUND IN GRID LIST FILE.
C           22 - MAX NUMBER OF FIELDS READ IN BY GETGRID.
C           24 - ERROR IN GETGRID REDING GRID LIST FILE.
C           26 - REQUESTED GRID NOT FOUND IN GRID LIST FILE.
C           32 - ERROR OPENING GRIB1 INPUT FILE.
C           42 - PROBLEM OPENING INDEX FILE
C          128 - PROBLEM ALLOCATING GRID ARRAYS
C          131 - INVALID FORECAST TIME UNIT SUPPLIED
C          132 - ERROR READING GRIB INDEX FILE
C          133 - ERROR READING GRIB INDEX FILE
C          134 - PROBLEM UNPACKING DATA POINTS.
C          136 - ERROR PROCESSING VARIABLE IN GETGB
C          138 - GRID DIMENSION LARGER THAN PROGRAM ALLOWS CHECK ND5 
C         1381 - PROBLEM RETRIEVING BITMAPA
C          281 - PROBLEM CLOSING FILE
C          290 - PROBLEM DEALLOCATING ARRAYS
C
C REMARKS:  THE GRIB1 INDEX FILE IS NOW AN INPUT IN THE U130.CN FILE.
C           EACH GRIB1 INPUT FILE HAS A CORRESPONDING GRIB1 INDEX FILE.
C
C ATTRIBUTES:
C   LANGUAGE:  FORTRAN 90 (xlf90 compiler)
C   MACHINE:  IBM SP
C$$$
C
      CHARACTER*80 ENVVAR
      CHARACTER*60 NAMIN(ND6),NAMIX(ND6)
      CHARACTER*4 MODELID(ND10)
      CHARACTER*3 LPROJ(ND10)
CINTEL
C      CHARACTER*32 JPLAIN(32,ND10)
      CHARACTER*1 JPLAIN(32,ND10)
CINTEL
      CHARACTER*60 RACESS
C
      REAL, ALLOCATABLE, DIMENSION(:) :: GDATA
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: IA,IC
      LOGICAL*1, ALLOCATABLE, DIMENSION (:) :: BMP
C
      DIMENSION ID(4)
      DIMENSION IPACK(ND5),IWORK(ND5),DATA(ND5)
      DIMENSION KFILIN(ND6),KFILIX(ND6)
      DIMENSION IS0(ND7),IS1(ND7),IS2(ND7),IS4(ND7),IS2_TEMP(ND7)
      DIMENSION IDATE(ND8)
      DIMENSION LDATES(ND9)
      DIMENSION IMAP(ND10),IPWR10(ND10),IPWR2(ND10),
     1          IGRID(ND10),IGRIB(ND10),NXS(ND10),NYS(ND10),
     2          NX(ND10),NY(ND10),IOFFX(ND10),IOFFY(ND10),POLEX(ND10),
     3          POLEY(ND10),ALAT(ND10),ALON(ND10),ORIENT(ND10),
     4          DX(ND10),DY(ND10),XLAT(ND10),
     5          IREPEAT(ND10),IDMDL(4,ND10),ID68(30,ND10),JSUBSET(ND10)
      DIMENSION ISTOP(2),KPDS(25),ID63(200),KGDS(200)
C
      LFLAG=0
      JSUBSET=0
      JALLOCATE=0
      KDATE=1
      JCLOSE=0
      JPRINT=0
      JCHECK=0
C
C        READ IN THE VALUES FOUND IN THE ELEMENT LIST FILE.
C
      CALL RDELMLST(KFILDO,KFILIE,NFIELDS,ID68,
     1              IDMDL,JPLAIN,MODELID,IMAP,
     2              IPWR10,IPWR2,IREPEAT,ND10,
     3              JCONVRT,ISTOP) 
C
C        EXTRACT FROM THE GRID FILE THE GRID SPECIFICATIONS 
C        OF THE GRIB1 DATA BEING PROCESSED. 
C
      DO 30 JJ=1,NFIELDS
C
C           EXTRACT THE GRID SPECIFICATIONS OF THE FIRST ELEMENT
C           IN THE ELEMENT LIST FILE.
C
         IF(JJ.EQ.1) THEN
C
            CALL GETGRID(MODELID(JJ),IGRID(JJ),IGRIB(JJ),NXS(JJ),
     1                   NYS(JJ),NX(JJ),NY(JJ),IOFFX(JJ),IOFFY(JJ),
     2                   POLEX(JJ),POLEY(JJ),ALAT(JJ),ALON(JJ),
     3                   ORIENT(JJ),DX(JJ),DY(JJ),LPROJ(JJ),
     4                   XLAT(JJ),KFILIC,JERR)
C
            IF(JERR.NE.0) THEN
               IF(JERR.EQ.1) THEN 
                  WRITE(KFILDO,15)
 15               FORMAT(/,' ****MAXIMUM NUMBER OF GRIDS WERE READ IN', 
     1                     ' BY GETGRID.  INCREASE PARAMETER MAXMOD',
     2                     ' IN GETGRID.',/,5X,'STOP 15 IN RDGRIB1.') 
                  CALL W3TAGE('RDGRIB1')
                  STOP 15
               ELSEIF(JERR.EQ.2) THEN
                  WRITE(KFILDO,17) (IDMDL(J,JJ),J=1,4),MODELID(JJ)
 17               FORMAT(/,' ****ERROR ENCOUNTERED BY GETGRID WHEN',
     1                     ' READING THE GRID LIST FILE FOR ELEMENT',/,
     2                     4X,3I10.9,1X,I10.10,' USING GRID IDENTIFIER',
     3                     1X,A4,'.',/,4X,' STOP 17 IN RDGRIB1.')
                  CALL W3TAGE('RDGRIB1')
                  STOP 17
               ELSEIF(JERR.EQ.3) THEN
                  WRITE(KFILDO,20) (IDMDL(J,JJ),J=1,4),MODELID(JJ)
 20               FORMAT(/,' ****ERROR ENCOUNTERED BY GETGRID WHEN',
     1                     ' OBTAINING THE GRID DEFINITION FOR',
     2                     ' ELEMENT',/,4X,3I10.9,1X,I10.10,' USING',
     3                     ' GRID IDENTIFIER ',A4,'.  STOP 20 IN',
     4                     ' RDGRIB1.') 
                  CALL W3TAGE('RDGRIB1')
                  STOP 20
               ENDIF     
            ENDIF
C
C           IF THE GRID SPECIFICATIONS FOR THE REMAINING ELEMENTS DO 
C           NOT MATCH THE GRID SPECIFICATIONS OF THE FIRST ELEMENT,
C           RETRIEVE THOSE NEW GRID SPECIFICATIONS.  
C
         ELSEIF(MODELID(JJ).NE.MODELID(1)) THEN
C
            CALL GETGRID(MODELID(JJ),IGRID(JJ),IGRIB(JJ),NXS(JJ),
     1                   NYS(JJ),NX(JJ),NY(JJ),IOFFX(JJ),IOFFY(JJ),
     2                   POLEX(JJ),POLEY(JJ),ALAT(JJ),ALON(JJ),
     3                   ORIENT(JJ),DX(JJ),DY(JJ),LPROJ(JJ),
     4                   XLAT(JJ),KFILIC,JERR)
C
            IF(JERR.NE.0) THEN
               IF(JERR.EQ.1) THEN 
                  WRITE(KFILDO,22)
 22               FORMAT(/,' ****MAXIMUM NUMBER OF GRIDS WERE READ IN', 
     1                     ' BY GETGRID.  INCREASE PARAMETER MAXMOD',
     2                     ' IN GETGRID.',/,5X,'STOP 22 IN RDGRIB1.') 
                  CALL W3TAGE('RDGRIB1')
                  STOP 22
               ELSEIF(JERR.EQ.2) THEN
                  WRITE(KFILDO,24) (IDMDL(J,JJ),J=1,4),MODELID(JJ)
 24               FORMAT(/,' ****ERROR ENCOUNTERED BY GETGRID WHEN',
     1                     ' READING THE GRID LIST FILE FOR ELEMENT',/,
     2                     4X,3I10.9,1X,I10.10,' USING GRID IDENTIFIER',
     3                     1X,A4,'.',/,4X,' STOP 24 IN RDGRIB1.')
                  CALL W3TAGE('RDGRIB1')
                  STOP 24
               ELSEIF(JERR.EQ.3) THEN
                  WRITE(KFILDO,26) (IDMDL(J,JJ),J=1,4),MODELID(JJ)
 26               FORMAT(/,' ****ERROR ENCOUNTERED BY GETGRID WHEN',
     1                     ' OBTAINING THE GRID DEFINITION FOR',
     2                     ' ELEMENT',/,4X,3I10.9,1X,I10.10,' USING',
     3                     ' GRID IDENTIFIER ',A4,'.  STOP 26 IN',
     4                     ' RDGRIB1.') 
                  CALL W3TAGE('RDGRIB1')
                  STOP 26
               ENDIF     
            ENDIF
C
C           THE GRID SPECIFICATIONS FOR THE CURRENT ELEMENT MATCHES
C           THE FIRST ELEMENT'S GRID SPECIFICATIONS.  MAP THE FIRST
C           ELEMENT'S GRID SPECIFICATIONS TO THE CURRENT ELEMENT
C           BEING EVALUATED.
C
         ELSE
C
            MODELID(JJ)=MODELID(1)
            IGRID(JJ)=IGRID(1)
            IGRIB(JJ)=IGRIB(1)
            NXS(JJ)=NXS(1)
            NYS(JJ)=NYS(1)
            NX(JJ)=NX(1)
            NY(JJ)=NY(1)
            IOFFX(JJ)=IOFFX(1)
            IOFFY(JJ)=IOFFY(1)
            POLEX(JJ)=POLEX(1)
            POLEY(JJ)=POLEY(1)
            ALAT(JJ)=ALAT(1)
            ALON(JJ)=ALON(1)
            ORIENT(JJ)=ORIENT(1)
            DX(JJ)=DX(1)
            DY(JJ)=DY(1)
            LPROJ(JJ)=LPROJ(1)
            XLAT(JJ)=XLAT(1)
C
         ENDIF
C
C           IF A SUBSET GRID IS TO BE PROCESSED SET JSUBSET( )=1.  
C           "JSUBSET( )" WILL BE USED DURING THE PROCESSING OF
C           EACH ELEMENT.
C
         IF((IOFFX(JJ).NE.0).OR.(IOFFY(JJ).NE.0)) JSUBSET(JJ)=1

 30   CONTINUE
C
C        BEGIN PROCESSING EACH GRIB1 FILE.
C
      DO 285 IN=1,NUMIN
C
         IFLAG=0
C
C           OPEN THE GRIB1 FILE USING DIRECT ACCESS.  NOTE THAT
C           THE GRIB1 FILE IS OPENED WITH READ PERMISSION ONLY.
C  
CINTEL
         ENVVAR='FORT  '
         WRITE(ENVVAR(5:6),FMT='(I2)')KFILIN(IN)
C         ENVVAR='XLFUNIT_  '
C         WRITE(ENVVAR(9:10),FMT='(I2)')KFILIN(IN)
CINTEL
         CALL GETENV(ENVVAR,NAMIN(IN))
         CALL BAOPENR(KFILIN(IN),NAMIN(IN),IRET)
C
         IF(IRET.NE.0) THEN
            WRITE(KFILDO,32) KFILIN(IN),IRET
 32         FORMAT(/,' ****PROBLEM OPENING GRIB1 INPUT FILE WITH UNIT',
     1               ' NUMBER ',I2,'.  IRET FROM BAOPENR =',I3,
     2               '.  STOP 32 IN RDGRIB1.')
            CALL W3TAGE('RDGRIB1')
            STOP 32
         ENDIF
C
C           OPEN THE GRIB1 INDEX FILE USING DIRECT ACCESS.  NOTE THAT
C           THE GRIB1 FILE IS OPENED WITH READ PERMISSION ONLY.
C
CINTEL
         ENVVAR='FORT  '
         WRITE(ENVVAR(5:6),FMT='(I2)')KFILIX(IN)
C         ENVVAR='XLFUNIT_  '
C         WRITE(ENVVAR(9:10),FMT='(I2)')KFILIX(IN)
CINTEL
         CALL GETENV(ENVVAR,NAMIX(IN))
         CALL BAOPENR(KFILIX(IN),NAMIX(IN),IRET)
C
         IF(IRET.NE.0) THEN
            WRITE(KFILDO,35) KFILIX(IN),IRET
 35         FORMAT(/,' ****PROBLEM OPENING GRIB1 INDEX FILE WITH UNIT',
     1               ' NUMBER ',I2,'.  IRET FROM BAOPENR =',I3,
     2               '.  STOP 35 IN RDGRIB1.')
            CALL W3TAGE('RDGRIB1')
            STOP 35
         ENDIF
C
         WRITE(KFILDO,37)KFILIX(IN),NAMIX(IN)
 37      FORMAT(/,' OPENING GRIB1 INDEX FILE ON UNIT NO. ',I2,
     1            ', FILE = ',A60)
C
C           FIND THE DATES 'LDATES( )' ON THE GRIB1 FILE KFILIN(IN)
C           AND PLACE THEM IN CHRONOLOGICAL ORDER.  INITIALIZE
C           ARRAY PRIOR TO USE.
C
         DO 48 J=1,ND9
            LDATES(J)=0
 48      CONTINUE
C
         CALL OVERDATE(KFILDO,KFILIN(IN),LDATES,ND9,N) 
C
C           CHECK IF ANY OF THE DATES ARE IN THE CURRENT GRIB1 FILE
C           COINCIDE WITH ANY OF THE DATES IN THE DATE LIST. IF SO,
C           SET A 'IFLAG=1'.           
C
         DO 55 LL=1,NDATES
            DO 50 JJ=1,N
               IF(LDATES(JJ).EQ.IDATE(LL)) IFLAG=1
 50         CONTINUE
 55      CONTINUE
C
C           IF THE DATES IN THE CURRENT GRIB1 FILE BEING 
C           EVALUATED DO NOT MATCH ANY OF THE DATES IN THE
C           INPUT DATE LIST, CLOSE THE CURRENT GRIB1 FILE 
C           AND BEGIN PROCESSING THE NEXT GRIB1 FILE.  
C
         IF(IFLAG.EQ.0) THEN
            WRITE(KFILDO,75) KFILIN(IN)
 75         FORMAT(/,' ****THE GRIB1 FILE WITH UNIT NUMBER ',I2,
     1               ' DOES NOT CONTAIN ANY DATA FOR THE',
     2               ' DATES IN THE INPUT DATE LIST.',/,4X,
     3               ' CONTINUE PROCESSING THE NEXT FILE.')
            ISTOP(1)=ISTOP(1)+1
            GOTO 280
         ENDIF
C
         DO 279 ND=KDATE,NDATES
C
            NDATE=IDATE(ND)
C
C              IF THE INPUT DATE BEING PROCESSED IS PRIOR TO ANY OF THE
C              DATES FOUND IN THE IN THE GRIB1 FILE,  MOVE ON TO THE NEXT 
C              DATE IN THE DATE LIST.
C
            IF(NDATE.LT.LDATES(1)) THEN
               WRITE(KFILDO,100) NDATE,KFILIN(IN) 
 100           FORMAT(/,' ****DATE ',I10,' COUND NOT BE FOUND IN INPUT',
     1                  ' FILE WITH UNIT NUMBER ',I2,'.  BEGIN',
     2                  ' PROCESSING',/,4X,' THE NEXT DATE.')
               ISTOP(1)=ISTOP(1)+1 
               ISTOP(2)=ISTOP(2)+NFIELDS
               KDATE=KDATE+1
               GOTO 279
            ENDIF
C
C              IF THE CURRENT DATE BEING PROCESSED IS LATER THAN THE 
C              DATES FOUND ON THE GRIB1 FILE, CLOSE THE GRIB1 FILE AND
C              BEGIN PROCESSING THE NEXT FILE AND MOVE ON TO THE NEXT
C              FILE. 
C
            IF(NDATE.GT.LDATES(N)) THEN 
               KDATE=KDATE+1
               GOTO 280
            ENDIF
C
C              SINCE 'KDATE' HAS ALREADY BEEN INITIALIZED TO ONE, DO
C              NOT INCREMENT 'KDATE' FOR THE FIRST DATE BEING PROCESSED
C              (I.E, ND=1).  INCREMENT 'KDATE' ONLY AFTER THE FIRST DATE 
C              HAS BEEN PROCESSED AND THE PROCESSING HAS NOT YET REACHED
C              THE END OF THE CURRENT GRIB1 FILE.
C
            IF(ND.GT.1) THEN
               IF(JCLOSE.EQ.0) KDATE=KDATE+1
            ENDIF
C
C              RESET 'JCLOSE' TO ZERO TO INDICATE THAT THE GRIB1 FILE 
C              HAS NOT BEEN CLOSED.
C
            JCLOSE=0
C
C              A DROP THROUGH HERE MEANS THAT A DATE FOUND ON THE 
C              INPUT GRIB1 FILE MATCHES A DATE FOUND IN THE INPUT 
C              DATE LIST.  BEGIN PROCESSING.
C 
            DO 250 JJ=1,NFIELDS 
C
C                 IF A PARTICULAR ELEMENT IS A DUPLICATE, DO NOT 
C                 PROCESS AND PACK THIS ELEMENT AGAIN.  MOVE ON 
C                 TO THE NEXT ELEMENT IN THE LIST.
C 
               IF(IREPEAT(JJ).EQ.1) GOTO 250   
C
C                 READ THE W3FI68 INTEGER VERSION OF THE GRIB ELF  
C                 FOR THIS SET OF FIELDS.
C              
               IF(IP4.GT.0) THEN
                  WRITE(IP4,117) NDATE 
 117              FORMAT(/,' THE FOLLOWING FIELD FOR DATE ',I10,1X,'IS',
     1                     ' CURRENTLY BEING PROCESSED.')
                  WRITE(IP4,118) (IDMDL(J,JJ),J=1,4),MODELID(JJ),
     1                            IMAP(JJ),IPWR10(JJ),IPWR2(JJ),
     2                            (JPLAIN(MM,JJ),MM=1,32)
 118              FORMAT(3I10.9,1X,I10.10,A4,1X,3(I2,1X),1X,32A1)
               ENDIF
C
C                 ALLOCATE SPACE TO THE NECESSARY ARRAYS.  ALLOCATE
C                 THE ARRAYS JUST ONCE FOR ALL ELEMENTS FOR ALL DATES
C                 FOR ALL FILES.
C               
               IF(JALLOCATE.EQ.0) THEN
                  ALLOCATE(GDATA(NX(JJ)*NY(JJ)),IA(NX(JJ),NY(JJ)),
     1                     IC(NX(JJ),NY(JJ)),BMP(NX(JJ)*NY(JJ)),STAT=IS)
C
                  IF(IS.NE.0) THEN
                     WRITE(KFILDO,128) IS
 128                 FORMAT(/,' ****PROBLEM ALLOCATING GRID ARRAYS.',
     1                        ' STOP 128 IN RDGRIB1.  IOSTAT=',I4)
                     CALL W3TAGE('RDGRIB1')
                     STOP 128
                  ENDIF
C
               ENDIF
C
C                 SET 'JALLOCATE' TO ONE TO INDICATE THAT THE PROGRAM  
C                 HAS REACHED THE POINT WHERE THE ALLOCATION OF MEMORY  
C                 HAS BEEN PERFORMED SUCCESSFULLY. 
C    
               JALLOCATE=1 

C                 ARRAY 'ID63' IS INITIALIZED TO -1.  A VALUE OF 
C                 -1 ALLOWS ANY VALUE OF THIS PARAMETER TO BE FOUND.
C     
               DO 129 J=1,200
                  ID63(J)=-1
 129           CONTINUE          
C
C                 PLACE THE DATE IN THE "ID63" ARRAY.
C
               NYR=NDATE/1000000
               IDRYR=MOD(NYR,100)
               NMO=NDATE/10000-NYR*100
               NDA=NDATE/100-NYR*10000-NMO*100
               NHR=NDATE-NYR*1000000-NMO*10000-NDA*100
C
C                 'GETGB' DEFINES THE YEAR 2000 AS THE 100TH YEAR
C                 OF THE 20TH CENTURY.  CONSEQUENTLY, IF THE YEAR
C                 BEING EVALUATED IS 2000, ID63(8) MUST BE SET TO 
C                 100 NOT 0.
C
C                 2-DIGIT YEAR OF CENTURY
               IF(IDRYR.NE.0) THEN
                  ID63(8)=IDRYR
               ELSE 
                  ID63(8)=100
               ENDIF
C                 MONTH
               ID63(9)=NMO
C                 DAY
               ID63(10)=NDA
C                 HOUR
               ID63(11)=NHR      

C                 ASSIGN THE FORECAST PROJECTION.  FOR VARIABLES THAT
C                 INVOLVE AVERAGES OR ACCUMULATIONS, THIS REPRESENTS
C                 THE PROJECTION IN HOURS (FROM THE REFERENCE TIME) THAT
C                 BEGINS THE PERIOD BEING EVALUATED.
C
               IF((ID68(17,JJ).EQ.1).OR.(ID68(17,JJ).EQ.-1)) THEN
                  ID63(14)=ID68(18,JJ)
                  ID63(15)=ID68(19,JJ)
               ELSEIF(ID68(17,JJ).EQ.11) THEN
                  ID63(14)=ID68(18,JJ)/6
                  ID63(15)=ID68(19,JJ)/6
               ELSEIF(ID68(17,JJ).EQ.12) THEN
                  ID63(14)=ID68(18,JJ)/12
                  ID63(15)=ID68(19,JJ)/12
               ELSE
                  WRITE(KFILDO,131) ID68(17,JJ)
 131              FORMAT(/,' ****U130 DOES NOT SUPPORT A FORECAST',
     1                     ' TIME UNIT OF ',I6,'.  VALUES OF 1,',
     2                     ' 11, 12, AND -1 (WILD CARD INDICATOR)',
     3                     ' ARE SUPPORTED.',/,4X,
     4                     ' STOP 131 IN RDGRIB1.')
                  STOP 131
               ENDIF
C
C                MAXIMUM NUMBER OF DATA POINTS TO UNPACK. 
C
               MAXPTS=NX(JJ)*NY(JJ)
C     
C                 ARRANGE THE W3FI68 PRODUCT DEFINITION SECTION TO A W3FI63 
C                 PRODUCT DEFINITION SECTION CONFIGURATION.
C
               CALL PDCHNG(ID68(1,JJ),ID63)
C
C                 SET 'ISRCH' TO A VALUE OF -1.  THIS CLEARS THE
C                 MEMORY BUFFER THAT STORES THE INDEX INFORMATION 
C                 FOUND IN THE CURRENT GRIB INDEX FILE.  SETTING 
C                 'ISRCH' TO A VALUE OF -1 ALLOWS THE USER TO REUSE  
C                 A DIFFERENT GRIB INDEX FILE WITH THE SAME UNIT 
C                 NUMBER AND FILE NAME FOR MULTIPLE CALLS TO "GETGB".
C
               ISRCH=-1
C               
               CALL GETGB(KFILIN(IN),KFILIX(IN),MAXPTS,ISRCH,ID63,
     1                    KGDS,NUMPTS,INUM,KPDS,KGDS,BMP,GDATA,IRET)
C
               IF(IRET.NE.0) THEN
C
                  IF(IRET.EQ.96) THEN
                     WRITE(KFILDO,132) (IDMDL(KK,JJ),KK=1,4),NDATE
 132                 FORMAT(/,' ****ERROR READING GRIB INDEX FILE FOR',
     1                        ' ELEMENT',3I10.9,1X,I10.10,1X,'FOR',
     2                        '  DATE ',I10,/,5X,'(IRET = 96 FROM',
     3                        ' GETGB).  STOP 132 IN RDGRIB1.')
                     CALL W3TAGE('RDGRIB1')
                     STOP 132
                  ELSEIF(IRET.EQ.97) THEN
                     WRITE(KFILDO,133) (IDMDL(KK,JJ),KK=1,4),NDATE
 133                 FORMAT(/,' ****ERROR READING GRIB FILE FOR',
     1                        ' ELEMENT',3I10.9,1X,I10.10,1X,'FOR',
     2                        ' DATE ',I10,/,5X,'(IRET = 97 FROM',
     3                        ' GETGB).  STOP 133 IN RDGRIB1.')
                     CALL W3TAGE('RDGRIB1')
                     STOP 133
                  ELSEIF(IRET.EQ.98) THEN
                     WRITE(KFILDO,134) (IDMDL(KK,JJ),KK=1,4),NDATE
 134                 FORMAT(/,' ****THE NUMBER OF DATA POINTS TO ',
     1                        'UNPACK FOR ELEMENT',3I10.9,1X,I10.10,/,
     2                        5X,'FOR DATE ',I10,' EXCEEDS THE NUMBER',
     3                        ' OF ANTICIPTED GRIDPOINTS SET IN THE',
     4                        ' GRID LIST FILE.',/,5X,'(IRET = 98',
     5                        ' FROM GETGB).  STOP 134 IN RDGRIB1.')
                     CALL W3TAGE('RDGRIB1')
                     STOP 134                                 
                  ELSEIF(IRET.EQ.99) THEN
                     WRITE(KFILDO,135) (IDMDL(KK,JJ),KK=1,4),NDATE
 135                 FORMAT(/,' ****VARIABLE',3I10.9,1X,I10.10,1X,'NOT',
     1                        ' FOUND FOR DATE ',I10,'.',/,4X, 
     2                        ' CONTINUE PROCESSING THE NEXT ELEMENT', 
     3                        ' (IRET=99 FROM GETGB).')
                     ISTOP(2)=ISTOP(2)+1
                     GOTO 250
                  ELSE
                     WRITE(KFILDO,136) IRET,(IDMDL(KK,JJ),KK=1,4),NDATE
 136                 FORMAT(/,' ****ERROR ',I3,' RETURNED FROM', 
     1                        ' SUBROUTINE GETGB.F WHEN ATTEMPTING TO',
     2                        ' PROCESS VARIABLE',/,4X,3I10.9,1X,I10.10,
     3                        ' FOR DATE ',I10,'.  STOP 136 IN',
     4                        ' RDGRIB1.')
                     CALL W3TAGE('RDGRIB1')
                     STOP 136
                  ENDIF
C
               ENDIF
C
C                 ENSURE THAT ND5 IS AT LEAST THE SIZE OF THE INPUT GRID. 
C                 OTHERWISE, IF THE INPUT GRID DIMENSIONS ARE LARGER THAN 
C                 ND5, A CORE DUMP COULD OCCUR IN EITHER 'CONSTG' OR 'PACK2D'.
C
               IF(ND5.LT.(KGDS(2)*KGDS(3))) THEN 
                  WRITE(KFILDO,138) ND5,KGDS(2)*KGDS(3),KFILIN(IN)
 138              FORMAT(/,' ****THE VALUE OF ND5, ',I7,', IS LESS',
     1                     ' THAN THE INPUT GRID DIMENSION (NX*NY)',I7,
     2                     ' FOUND ON THE',/,5X,'INPUT GRIB1 FILE WITH',
     3                     ' UNIT NUMBER ',I2,'.  INCREASE ND5 TO AT',
     4                     ' LEAST THE SIZE OF THE INPUT GRID. ',/,4X,
     5                     ' STOP 138 IN RDGRIB1.')     
                  CALL W3TAGE('RDGRIB1')
                  STOP 138
               ENDIF
C
C                 TRUNCATE A PORTION OF THE GRID METADATA FOR PROCESSING.
C  
               CALL GRIDASN(KGDS,IMAP(JJ),JCONVRT)
C
C                 CHECK FOR CONSISTENCY BETWEEN THE GRIB1 GRID 
C                 CHARACTERISTICS DEFINED IN THE GRIB1 FILE AND 
C                 THE GRID CHARACTERISTICS DEFINED IN THE GRID 
C                 LIST FILE.
C
               CALL CHKGRID2(KFILDO,KGDS,IMAP(JJ),NXS(JJ),NYS(JJ),
     1                       ALAT(JJ),ALON(JJ),ORIENT(JJ),XLAT(JJ),
     2                       DX(JJ),IDMDL(1,JJ),NDATE,JSUBSET(JJ),
     3                       JCONVRT)
C
C                 IF 'JFLAG' EQUALS ONE, A BITMAP IS REQUIRED FOR 
C                 PROCESSING.  IF 'LFLAG' EQUALS ZERO, FETCH THE
C                 DATA.
C  
               IF((JFLAG.EQ.1).AND.(LFLAG.EQ.0)) THEN
C
C                    RETRIEVE THE BITMAP INFORMATION FOR THE PARTICULAR 
C                    ELEMENT BEING PROCESSED.
C
                  CALL CONSTG(KFILDO,KFILRA,RACESS,ID,
     1                        IPACK,IWORK,DATA,ND5,
     2                        IS0,IS1,IS2,IS4,ND7,
     3                        ISTAV,L3264B,IER) 
C                 
                  IF(IER.NE.0) THEN
                     WRITE(KFILDO,1381) (ID(J),J=1,4)
 1381                FORMAT(/,' ****PROBLEM RETRIEVING BITMAP',
     1                      ' VARIABLE ',3(I9.9,1X),I10.10,
     2                      ' FROM THE GRIDDED',/,5X,'RANDOM',
     3                      ' ACCESS FILE.  STOP 1381 IN RDGRIB1.')
                     CALL W3TAGE('RDGRIB1')
                     STOP 1381
                  ENDIF
C
C                    SET 'LFLAG' TO A VALUE OF ONE.  THIS ENSURES THAT
C                    THE BITMAP DATA IS RETRIEVED ONLY ONCE.
C   
                  LFLAG=1
C                  
               ENDIF
C
C                 CHECK THAT THE GRID CHARACTERISTICS IN THE GRIDDED 
C                 RANDOM ACCESS FILE ARE THE SAME AS THE GRID 
C                 CHARACTERISTICS DEFINED IN THE GRID LIST FILE. 
C                 THIS CROSS-CHECK IS NECESSARY TO ENSURE THAT IF 
C                 A RANDOM ACCESS FILE IS USED, ITS GRID 
C                 CHARACTERISTICS ARE ALSO CONSISTENT WITH THE GRID 
C                 CHARACTERISTICS FOUND IN THE GRID LIST FILE.  PERFORM
C                 THIS CHECK ONCE.
C
               IF((JFLAG.EQ.1).AND.(JCHECK.EQ.0).AND.
     1                             ((JSUBSET(JJ).EQ.0).OR.
     2                             (JSUBSET(JJ).EQ.1))) THEN
                  CALL CHKGRID3(KFILDO,IS2,ND7,IMAP(JJ),NXS(JJ),
     1                          NYS(JJ),ALAT(JJ),ALON(JJ),
     2                          ORIENT(JJ),XLAT(JJ),DX(JJ))
                  JCHECK=1
               ENDIF
C
C                 IF THE USER DESIRES DIAGNOSTIC PRINT OF THE BITMAP
C                 READ FROM THE TDLPACK EXTERNAL GRIDDED RANDOM ACCESS
C                 FILE, PRINT THOSE VALUES.
C
               IF((IP12.NE.0).AND.(JPRINT.EQ.0))THEN
                  WRITE(IP12,1383) (ID(J),J=1,4)
 1383             FORMAT(/,' BITMAP VALUES FOR VARIABLE',
     1                   I11.9,3I11,/)
                  DO 1386 JY=1,NYS(JJ)
                     WRITE(IP12,1384)(DATA(K),K=(NYS(JJ)-JY)*
     1                                NXS(JJ)+1,(NYS(JJ)-JY)*
     2                                NXS(JJ)+NXS(JJ))
 1384                FORMAT(' ',10F10.1/(' ',10F10.1))
 1386             CONTINUE
                  JPRINT=1
               ENDIF
C
C                 CHECK IF A BITMAP IS PRESENT IN THE GRIB1 MESSAGE.
C                 NOTE THAT IF A BITMAP IS NOT PRESENT (I.E., BMP( )
C                 EQUALS TRUE), NONE OF THE VALUES IN GDATA( ) ARE 
C                 CHANGED.  IF A BITMAP VALUE OF 'FALSE' IS ENCOUNTERED, 
C                 THE DATA VALUE IS ASSIGNED A VALUE OF '9999'.   
C
               DO 1391 KK=1,NX(JJ)*NY(JJ)
                  IF(BMP(KK).EQ..FALSE.) GDATA(KK)=9999.
 1391          CONTINUE
C              
C                 CHECK IF A SUBSET OF THE GRID IS TO BE TDLPACKED.
C                 IF SO, MAP THE DATA TO THE SUBGRID.
C
               IF(JSUBSET(JJ).EQ.1) 
     1            CALL SUBGRID(GDATA,NX(JJ)*NY(JJ),NX(JJ),NXS(JJ),
     2                         NYS(JJ),IOFFX(JJ),IOFFY(JJ)) 
C
C                 IF THERE ARE PRIMARY MISSING VALUES IN THE DATA THAT 
C                 ARE NOT DESIGNATED BY 9999, SET THE PRIMARY MISSING
C                 VALUES TO 9999.  THIS CHECK MAKES A PROVISION SO 
C                 THAT THE MISSING VALUE CAN BE A FRACTIONAL VALUE 
C                 (E.G., .005).
C      
               IF(INT(PXMISS).NE.9999) THEN
                  DO 145 J=1,NXS(JJ)*NYS(JJ)
                     IF(ABS(NINT(GDATA(J))-PXMISS).LE..000001)
     1                  GDATA(J)=9999.
 145              CONTINUE
               ENDIF
C
C                 IF A BITMAP IS PROVIDED, MASK THE GRIDDED DATA.
C                 SINCE IT IS UNSAFE TO EQUATE REAL VALUES (A "REAL"
C                 DANGER), TAKE THE 'NINT' OF THE DATA WHICH WILL
C                 ONLY HAVE A VALUE OF ZERO OR ONE.  ASSIGN THE 
C                 SECONDARY MISSING VALUE TO THOSE POINTS ON THE 
C                 GRID CORRESPONDING TO A BITMAP VALUE OF ZERO.
C
               IF(JFLAG.EQ.1) THEN
                  DO 146 J=1,NXS(JJ)*NYS(JJ)
                     IF(NINT(DATA(J)).EQ.0) GDATA(J)=XMISSS
 146              CONTINUE
               ENDIF
C
C                 IF THE USER DESIRES A PRINT OF THE GRIDDED VALUES
C                 AS EXTRACTED FROM THE GRIB1 MESSAGE AND PROCESSED
C                 BY THE BITMAP (IF ONE WAS PRESENT ON INPUT), WRITE
C                 THE VALUES TO IP11.  THE VALUES ARE WRITTEN TO THE 
C                 NEAREST THOUSANDTHS PLACE.
C
               IF(IP11.NE.0) THEN
                  WRITE(IP11,150) (IDMDL(KK,JJ),KK=1,4),NDATE
 150              FORMAT(/,' GRIDPOINT VALUES FOR VARIABLE',I11.9,3I11,
     1            ' FOR DATE',I12/)
                  DO 152 JY=1,NYS(JJ)
                     WRITE(IP11,151)(GDATA(K),K=(NYS(JJ)-JY)*
     1                               NXS(JJ)+1,(NYS(JJ)-JY)*
     2                               NXS(JJ)+NXS(JJ))
 151                 FORMAT(' ',10F10.3/(' ',10F10.3))
 152              CONTINUE
               ENDIF
C
C                 NOW PUT DATA INTO TDLPACK FOR OUTPUT.         
C                       
C                 FIRST SET UP IS1( )
C                 IS1(2)=1 MEANS NO BIT MAP, GRID DEFINITION IS INCLUDED
C
C                 PLACE THE DATE INFORMATION IN THE IS1( ) ARRAYS.  
C                 NOTE:  THE MINUTES ARE PACKED INTO 'IS1(7)'.
C 
               IS1(2)=1
               IS1(3)=NYR
               IS1(4)=NMO
               IS1(5)=NDA
               IS1(6)=NHR
               IS1(7)=KPDS(12)
               IS1(8)=NDATE
C
C                 PLACE THE MOS-2000 IDS INTO THE IS1( ) ARRAYS.
C 
               IS1(9)=IDMDL(1,JJ)
               IS1(10)=IDMDL(2,JJ)
               IS1(11)=IDMDL(3,JJ)
               IS1(12)=IDMDL(4,JJ)
               IS1(13)=MOD(IDMDL(3,JJ),1000)
               IS1(14)=0
               IS1(15)=MOD(IDMDL(1,JJ),100)
               IS1(16)=1
               IS1(17)=IPWR10(JJ)
               IS1(18)=IPWR2(JJ)
               IS1(19)=0 
               IS1(22)=32
C
               DO 170 KK=1,IS1(22)
CINTEL
C                  IS1(22+KK)=ICHAR(JPLAIN(KK,JJ))
                  IS1(22+KK)=IACHAR(JPLAIN(KK,JJ))
CINTEL
 170           CONTINUE
C
C                 NOW SET UP IS2( ) (THE GRID SPECIFICATIONS) FOR TDLPACKING.
C
               IS2_TEMP(2)=IMAP(JJ)
               IS2_TEMP(3)=NXS(JJ)
               IS2_TEMP(4)=NYS(JJ)
               IS2_TEMP(5)=ALAT(JJ)*10000
               IS2_TEMP(6)=ALON(JJ)*10000
               IS2_TEMP(7)=ORIENT(JJ)*10000
               IS2_TEMP(8)=DX(JJ)*1000000
C
C                 NOTE THAT IS2(9) IS HARDCODED FOR 60N.  THIS IS
C                 APPLICABLE FOR ALL NORTH POLAR STEREOGRAPHIC GRIDS  
C                 ARCHIVED, BUT NOT FOR LAMBERT CONICAL OR MERCATOR.
C                 'XLAT' IS READ IN FROM THE GRID LIST FILE.
C
               IF(KGDS(1).EQ.5) THEN
                  IS2_TEMP(9)=60*10000
               ELSE
                  IS2_TEMP(9)=XLAT(JJ)*10000
               ENDIF
C
               IS2_TEMP(10)=0
C
D              WRITE(KFILDO,178) KFILDO,NXS(JJ),NYS(JJ),ND7,XMISSS,ND5,
D    1                           MINPK,L3264B
D 178          FORMAT(' PARAMETERS BEFORE CALL TO PACK2D:',/,
D    1                ' KFILDO:',I2,' NXS:',I4,' NYS:',I3,' ND7:',I8,
D    2                ' XMISSS:',F7.1,' ND5:',I6,' MINPK:',I4,
D    3                ' L3264B:',I3)
C
C                 SINCE GRIB1 DOES NOT SUPPORT A SCANNING MODE OF
C                 80 (BOUSTROPHEDONIC, BEGINNING AT THE LOWER
C                 LEFT PORTION OF THE GRID MOVING RIGHT TO LEFT),
C                 THERE IS NO NEED TO CHECK FOR THE REORDERING OF 
C                 THE DATA FIELD.  
C
C                 PACK UP THE DATA USING PACK2D. 
C
               CALL PACK2D(KFILDO,GDATA,IA,IC,NXS(JJ),NYS(JJ),IS0,IS1,
     1                     IS2_TEMP,IS4,ND7,XMISSP,XMISSS,IPACK,ND5,
     2                     MINPK,LX,IOCTET,L3264B,IER)
C
               IF(IER.NE.0) THEN
                  WRITE(KFILDO,185) (IDMDL(KK,JJ),KK=1,4),NDATE
 185              FORMAT(/,' ****VARIABLE',3I10.9,1X,I10.10,
     1                     ' COULD NOT BE PACKED BY PACK2D FOR',
     2                     ' DATE ',I10,'.',/,5X,'CONTINUE PROCESSING',
     3                     ' THE NEXT ELEMENT.')  
                  ISTOP(1)=ISTOP(1)+1 
                  GOTO 250
               ENDIF
C
C                 WRITE THE TDLPACK DATA TO OUTPUT FILE ON UNIT KFILIO.
C
               NWORDS=IOCTET*8/L3264B
C
               CALL WRITEP(KFILDO,KFILIO,IPACK,NWORDS,NTOTBY,NTOTRC,
     1                     L3264B,IER)
C
               IF(IER.EQ.0.AND.IP4.GT.0) THEN                  
                  WRITE(IP4,190) (IDMDL(KK,JJ),KK=1,4),NDATE
 190              FORMAT(/,' WRITING VARIABLE',3I10.9,1X,I10.10,
     1                     ' FOR DATE ',I10,' TO FILE KFILIO.')
               ELSEIF(IER.NE.0) THEN
                  WRITE(KFILDO,195) (IDMDL(KK,JJ),KK=1,4),NDATE
 195              FORMAT(/,' ****VARIABLE',3I10.9,1X,I10.10,
     1                     ' COULD NOT BE WRITTEN TO KFILIO FOR', 
     2                     ' DATE ',I10,'.',/,5X,'CONTINUE',
     3                     ' PROCESSING THE NEXT ELEMENT.')
                  ISTOP(1)=ISTOP(1)+1   
               ENDIF
C
 250        CONTINUE
C
 279     CONTINUE
C
C           CLOSE THE PROCESSED GRIB1 INPUT FILE.
C
 280     CALL BACLOSE(KFILIN(IN),IRET)
C
         IF(IRET.NE.0) THEN
            WRITE(KFILDO,281) IRET
 281        FORMAT(/,' ****PROBLEM CLOSING FILE WITH UNIT NUMBER ',I2,
     1               ' IN RDGRIB1.  STOP 281 IN RDGRIB1.')
            ISTOP(1)=ISTOP(1)+1
            CALL W3TAGE('RDGRIB1')
            STOP 281
         ENDIF  
C
         WRITE(KFILDO,282)KFILIN(IN),NAMIN(IN)
 282     FORMAT(/,' CLOSING FILE ON UNIT NO.',I3,', FILE = ',A60)
C
C           SET 'JCLOSE' TO ONE TO INDICATE THAT THE CURRENT GRIB1 FILE 
C           HAS BEEN CLOSED.
C
         JCLOSE=1
C
C            CLOSE THE GRIB1 INDEX FILE.
C
         CALL BACLOSE(KFILIX(IN),IRET)
C
         IF(IRET.NE.0) THEN
            WRITE(KFILDO,283) KFILIX(IN)
 283        FORMAT(/,' ****PROBLEM CLOSING INDEX FILE WITH UNIT',
     1               ' NUMBER ',I2,' IN RDGRIB1.  BEGIN',
     2               ' PROCESSING THE NEXT INPUT GRIB1 FILE.')
            ISTOP(1)=ISTOP(1)+1
         ENDIF
C
         WRITE(KFILDO,284)KFILIX(IN),NAMIX(IN)
 284     FORMAT(/,' CLOSING GRIB1 INDEX FILE ON UNIT NO.',
     1            I3,' FILE = ',A60)
C 
 285  CONTINUE
C
C        DEALLOCATE THE DYNAMIC ARRAYS.
C
      IF(JALLOCATE.EQ.1) THEN 
         DEALLOCATE(GDATA,IA,IC,BMP,STAT=IS)       
         IF(IS.NE.0) THEN
            WRITE(KFILDO,290) 
 290        FORMAT(/, ' ****PROBLEM DEALLOCATING ARRAYS IN',
     1                ' RDGRIB1.  STOP 290 IN RDGRIB1.')
            CALL W3TAGE('RDGRIB1')
            STOP 290
         ENDIF
      ENDIF
C
      RETURN
      END
 
