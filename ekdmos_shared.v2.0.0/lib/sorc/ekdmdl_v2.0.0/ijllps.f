      SUBROUTINE IJLLPS(XI,YJ,XMESHL,ORIENT,XLAT,XNP,YNP,ALAT,ALONG)
C
C        MAY 1993   GLAHN   TDL   HP9000
C
C        PURPOSE
C            TO CONVERT FROM GRID COORDINATES ON A NORTH POLAR
C            STEROGRAPHIC PROJECTION WITH POLE AT XNP,YNP TO LATITUDE
C            AND LONGITUDE.  MESH LENGTH, ORIENTATION, AND LATITUDE AT
C            WHICH THE MESH LENGTH IS DEFINED ARE INPUT.  IT
C            IS FOR USE ONLY IN THE NORTHERN HEMISPHERE.  ADAPTED FROM
C            NMC'S W3FB05.
C
C        DATA SET USE
C            NONE.
C
C        VARIABLES
C
C            INPUT
C               XI,XJ = I (LEFT TO RIGHT) AND J (BOTTOM TO TOP)
C                       GRIDPOINT NUMBERS CONSIDERING LOWER
C                       LEFT POINT TO BE (1,1).
C              XMESHL = MESH LENGTH AT XLAT DEGREES N LATITUDE IN KM.
C                       MUST BE POSITIVE.
C              ORIENT = ORIENTATION IN DEGREES WEST LONGITUDE.
C                XLAT = LATITUDE AT WHICH XMESHL APPLIES.
C             XNP,YNP = POLE POSITION OF GRID FOR WHICH XI AND YJ
C                       ARE FURNISHED.  THIS IS THE NUMBER OF GRIDPOINTS
C                       ONE MUST COUNT IN THE IX AND JY DIRECTIONS,
C                       RESPECTIVELY, TO GO FROM THE LOWER LEFT
C                       CORNER TO THE POLE.
C
C            OUTPUT
C                ALAT = NORTH LATITUDE IN DEGREES.  MUST BE POSITIVE.
C               ALONG = WEST LONGITUDE IN DEGREES.  NEGATIVE IF EAST
C                       LONGITUDE.
C
C            INTERNAL
C              DEGPRD = DEGREES PER RADIAN.
C              RADPDG = RADIANS PER DEGREE.
C                  PI = PI
C               RERTH = RADIUS OF EARTH = 6371.2 KM.
C
C        NONSYSTEM SUBROUTINES CALLED
C            NONE.
C
      PARAMETER (PI=3.14159,
     1           RERTH=6371.2,
     2           DEGPRD=180./PI,
     3           RADPDG=PI/180.)
C
      GI2=(((1.+SIN(XLAT*RADPDG))*RERTH)/XMESHL)**2
      R2=(XI-XNP)**2+(YJ-YNP)**2
      IF(R2.NE.0.)GO TO 100
C
C         NORTH POLE POSITION
C
      ALAT=90.
      ALONG=0.
      GO TO 200
C
C         NOT POLE POSITION
C
 100  ALAT=ASIN((GI2-R2)/(GI2+R2))*DEGPRD
      ANGLE=DEGPRD*ATAN2(YJ-YNP,XI-XNP)
      IF(ANGLE.LE.0.)ANGLE=ANGLE+360.
      ALONG=270.+ORIENT-ANGLE
      IF(ALONG.LT.0.)ALONG=ALONG+360.
      IF(ALONG.GE.360.)ALONG=ALONG-360.
C
 200  RETURN
      END
