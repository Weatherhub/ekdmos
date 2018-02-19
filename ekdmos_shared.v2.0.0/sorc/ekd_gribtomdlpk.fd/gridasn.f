         SUBROUTINE GRIDASN(JGDS,JMAP,JCONVRT)
C
C$$$   SUBPROGRAM DOCUMENTATION BLOCK
C
C        OCTOBER    2004   RUDACK   MDL   MOS-2000          
C        SEPTEMBER  2005   RUDACK   MODIFIED CODE TO MEET
C                                   OPERATIONAL REQUIREMENTS.
C
C        PURPOSE
C           TO RE-ORDER THE GRID CHARACTERISTICS STORED 
C           IN 'JGDS( )' SO THAT THE GRIB GRID DATA 
C           CHARACTERISTICS CAN BE CHECKED AGAINST
C           THE ENTRIES FOUND IN THE GRID LIST FILE.
C
C        DATA SET USE
C              NONE
C
C        VARIABLES
C             JGDS(J) = ARRAY CONTAINING THE GRID DEFINITION INFORMATION
C                       FROM THE GRIB2 MESSAGE (J=1,200).  NOTE THAT THE
C                       ARRAY JGDS( ) FOR INPUT WHEN PROCESSING A GRIB1
C                       MESSAGE IS NOT REARRANGED.  (INPUT)
C                       LAMBERT CONFORMAL GRIDS (GRIB1)
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
C                       LAMBERT CONFORMAL GRIDS (GRIB2)
C                          (8) - NX NR POINTS ALONG X-AXIS
C                          (9) - NY NR POINTS ALONG Y-AXIS
C                         (10) - LA1 LAT OF ORIGIN (LOWER LEFT)
C                         (11) - LO1 LON OF ORIGIN (LOWER LEFT)
C                         (12) - RESOLUTION AND COMPONENT FLAG
C                         (13) - LATITUDE WHERE DX AND DY ARE SPECIFIED
C                         (14) - LOV - ORIENTATION OF GRID
C                         (15) - DX - X-DIR INCREMENT
C                         (16) - DY - Y-DIR INCREMENT
C                         (17) - PROJECTION CENTER FLAG
C                         (18) - SCANNING MODE FLAG
C                         (19) - LATIN 1 - FIRST LAT FROM POLE OF
C                                SECANT CONE INTERSECTION
C                         (20) - LATIN 2 - SECOND LAT FROM POLE OF
C                                SECANT CONE INTERSECTION
C                       POLAR STEREOGRAPHIC GRIDS (GRIB1) 
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
C                       POLAR STEREOGRAPHIC GRIDS (GRIB2)
C                          (8) - N(I) NR POINTS ALONG LAT CIRCLE
C                          (9) - N(J) NR POINTS ALONG LON CIRCLE
C                         (10) - LA(1) LATITUDE OF ORIGIN
C                         (11) - LO(1) LONGITUDE OF ORIGIN
C                         (12) - RESOLUTION AND COMPONENT FLAG
C                         (13) - LAD LATITUDE WHERE DX AND DY ARE SPECIFIED 
C                         (14) - LOV GRID ORIENTATION
C                         (15) - DX - X DIRECTION INCREMENT
C                         (16) - DY - Y DIRECTION INCREMENT
C                         (17) - PROJECTION CENTER FLAG
C                         (18) - SCANNING MODE
C                      MERCATOR GRIDS (GRIB1)
C                          (2) - N(I) NR POINTS ON LATITUDE CIRCLE
C                          (3) - N(J) NR POINTS ON LONGITUDE MERIDIAN
C                          (4) - LA(1) LATITUDE OF ORIGIN
C                          (5) - LO(1) LONGITUDE OF ORIGIN
C                          (6) - RESOLUTION FLAG
C                          (7) - LA(2) LATITUDE OF LAST GRIDPOINT
C                          (8) - LO(2) LONGITUDE OF LAST GRIDPOINT
C                          (9) - LATIT - LATITUDE OF PROJECTION INTERSECTION
C                         (10) - RESERVED
C                         (11) - SCANNING MODE FLAG
C                         (12) - LONGITUDINAL DIR GRID LENGTH
C                         (13) - LATITUDINAL DIR GRID LENGTH
C                      MERCATOR GRIDS (GRIB2)
C                          (8) - N(I) NR POINTS ON LATITUDE CIRCLE
C                          (9) - N(J) NR POINTS ON LONGITUDE MERIDIAN
C                         (10) - LA(1) LATITUDE OF ORIGIN
C                         (11) - LO(1) LONGITUDE OF ORIGIN
C                         (12) - RESOLUTION AND COMPONENT FLAG
C                         (13) - LAD LATITUDE(S) AT WHICH THE MERCATOR 
C                                PROJECTION INTERSECTS THE EARTH 
C                         (14) - LA(2) LATITUDE OF LAST GRIDPOINT
C                         (15) - LO(2) LONGITUDE OF LAST GRIDPOINT
C                         (16) - SCANNING MODE
C                         (17) - ORIENTATION OF THE GRID
C                         (18) - LONGITUDINAL DIR GRID LENGTH
C                         (19) - LATITUDINAL DIR GRID LENGTH
C                JGDS(J) = ARRAY CONTAINING THE GRID DEFINITION INFORMATION.
C                          REARRANGED GRID METADATA FOR INPUT TO SUBROUTINE 
C                          'CHKGRID2' (J=1,200).  (OUTPUT)
C                          LAMBERT CONFORMAL GRIDS
C                          (1) - DESIGNATED VALUE FOR LAMBERT GRID (=3)
C                          (2) - NX NR POINTS ALONG X-AXIS
C                          (3) - NY NR POINTS ALONG Y-AXIS
C                          (4) - LA1 LAT OF ORIGIN (LOWER LEFT)
C                          (5) - LO1 LON OF ORIGIN (LOWER LEFT)
C                          (6) - RESOLUTION AND COMPONENT FLAG
C                          (7) - LOV - ORIENTATION OF GRID
C                          (8) - DX - X-DIR INCREMENT
C                          (9) - DY - Y-DIR INCREMENT
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
C                          (6) - RESOLUTION FLAG AND COMPONENT FLAG
C                          (7) - LOV GRID ORIENTATION
C                          (8) - DX - X DIRECTION INCREMENT
C                          (9) - DY - Y DIRECTION INCREMENT
C                         (11) - SCANNING MODE 
C                         (12) - LAD - LATITUDE WHERE DX AND DY ARE SPECIFIED
C                       MERCATOR GRIDS
C                          (1) - DESIGNATED VALUE FOR MERCATOR GRID (1 OR 7)
C                          (2) - N(I) NR POINTS ON LATITUDE CIRCLE
C                          (3) - N(J) NR POINTS ON LONGITUDE MERIDIAN
C                          (4) - LA(1) LATITUDE OF ORIGIN
C                          (5) - LO(1) LONGITUDE OF ORIGIN
C                          (6) - RESOLUTION AND COMPONENT FLAG
C                          (7) - ORIENTATION OF THE GRID (NOT DEFINED FOR GRIB1)
C                          (8) - LONGITUDINAL DIR GRID LENGTH
C                          (9) - LATITUDINAL DIR GRID LENGTH
C                         (11) - SCANNING MODE FLAG
C                         (12) - LATIT - LATITUDE OF PROJECTION INTERSECTION
C             JMAP = MAP PROJECTION TO BE TDLPACKED. (INPUT)
C                          3 = N.H. LAMBERT
C                          5 = N.H. POLAR STEREOGRAPHIC
C                          7 = MERCATOR
C          JCONVRT = FLAG INDICATING IF GRIB1 (=1) OR GRIB2 (=2) DATA
C                    IS TO BE TDLPACKED.  (INPUT)
C
C SUBROUTINES CALLED:   NONE
C
C REMARKS:  NONE
C
C ATTRIBUTES:
C   LANGUAGE:  FORTRAN 90 (xlf90 compiler)
C   MACHINE:  IBM SP
C
      DIMENSION JGDS(200)
C
C        IF A GRIB1 FILE IS BEING PROCESSED, MASSAGE A PORTION OF 
C        THE GRID METADATA AS FOLLOWS:
C
      IF(JCONVRT.EQ.1) THEN
C
         IF(JMAP.EQ.3) THEN
C              IF THE MAP PROJECTION IS LAMBERT CONFORMAL (JMAP=3).
            JGDS(4)=JGDS(4)*10
            JGDS(5)=JGDS(5)*10
            JGDS(7)=JGDS(7)*10
            JGDS(8)=JGDS(8)*10
            JGDS(9)=JGDS(9)*10
            JGDS(12)=JGDS(12)*10
            JGDS(13)=JGDS(13)*10
         ELSEIF(JMAP.EQ.5) THEN
C              IF THE MAP PROJECTION IS POLAR STEREOGRAPHIC (JMAP=5).
            JGDS(4)=JGDS(4)*10
            JGDS(5)=JGDS(5)*10
            JGDS(7)=JGDS(7)*10
            JGDS(8)=JGDS(8)*10
            JGDS(9)=JGDS(9)*10
            JGDS(12)=JGDS(12)*10
         ELSEIF(JMAP.EQ.7) THEN
C              IF THE MAP PROJECTION IS MERCATOR (JMAP=7).
            JGDS(4)=JGDS(4)*10
            JGDS(5)=JGDS(5)*10
            JGDS(8)=JGDS(12)*10
            JGDS(12)=JGDS(9)*10
            JGDS(9)=JGDS(13)*10
         ENDIF
C
C        IF A GRIB2 FILE IS BEING PROCESSED, MASSAGE A PORTION OF
C        THE GRID METADATA AS FOLLOWS:
C
      ELSEIF(JCONVRT.EQ.2) THEN
C
         IF(JMAP.EQ.3) THEN
C              IF THE MAP PROJECTION IS LAMBERT CONFORMAL (JMAP=3).
            JGDS(1)=3
            JGDS(2)=JGDS(8)
            JGDS(3)=JGDS(9)
            JGDS(4)=JGDS(10)/100
            JGDS(5)=JGDS(11)/100
            JGDS(6)=JGDS(13)/100
            JGDS(7)=JGDS(14)/100 
            JGDS(8)=JGDS(15)/100
            JGDS(9)=JGDS(16)/100
            JGDS(11)=JGDS(18)
            JGDS(12)=JGDS(19)/100
            JGDS(13)=JGDS(20)/100
         ELSEIF(JMAP.EQ.5) THEN
C              IF THE MAP PROJECTION IS POLAR STEREOGRAPHIC (JMAP=5).
            JGDS(1)=5 
            JGDS(2)=JGDS(8)
            JGDS(3)=JGDS(9)
            JGDS(4)=JGDS(10)/100
            JGDS(5)=JGDS(11)/100
            JGDS(6)=JGDS(12) 
            JGDS(7)=JGDS(14)/100
            JGDS(8)=JGDS(15)/100
            JGDS(9)=JGDS(16)/100
            JGDS(11)=JGDS(18)
            JGDS(12)=JGDS(13)/100 
         ELSEIF(JMAP.EQ.7) THEN
C              IF THE MAP PROJECTION IS MERCATOR (JMAP=7). 
            JGDS(1)=7
            JGDS(2)=JGDS(8)
            JGDS(3)=JGDS(9)
            JGDS(4)=JGDS(10)/100
            JGDS(5)=JGDS(11)/100
            JGDS(6)=JGDS(12)
            JGDS(12)=JGDS(13)/100
            JGDS(11)=JGDS(16)
            JGDS(7)=JGDS(17)/100
            JGDS(8)=JGDS(18)/100
            JGDS(9)=JGDS(19)/100
         ENDIF
C
      ENDIF
C
      RETURN
      END
