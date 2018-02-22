      SUBROUTINE BDRIVR(ID,MAXDSC,LDESC,NDESC,SCALE,REF,NBITS,
     *           IVPRJ,MAXPRJ,NVPRJ,CFLAG,IEND,IOPT,KFILDI)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BDRIVR      READS A SET OF INPUT CONTROL RECORDS       
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 92-07-13             
C                                                                       
C ABSTRACT: READS A SET OF INPUT CONTROL RECORDS.  THE SET CONSISTS OF  
C           AN ELEMENT DESCRIPTOR DESCRIBING A FORECAST ELEMENT, THE    
C           ASSOCIATED FORECAST IDENTIFIER, THE SCALING AND REFERENCE
C           VALUES, AND A LIST OF VALID PROJECTIONS FOR THE FORECAST.   
C           AN OPTION VALUE IS ALSO READ IN TO DETERMINE THE TYPE OF    
C           PROCESSING THAT NEEDS TO BE DONE.                           
C           ALSO, A LIST OF THE DESCRIPTORS IS KEPT.                    
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   92-07-13  GILBERT                                                   
C   01-02     SHIREY - CHANGED STATEMENTS TO READ IN 26 PROJECTIONS
C                      INSTEAD OF 24.  NOW CALLS MOS2000 SUB RDI INSTEAD
C                      OF OLD SUB RDLSTI.
C   04-08-23  MALONEY- CHANGED STATEMENTS TO READ IN 28 PROJECTIONS
C                      INSTEAD OF 26.  THIS WAS NEEDED TO HANDLE EVERY 
C                      3-H PROJECTION FROM 6 TO 84, INCLUSIVE.
C   05-07-22  MALONEY- INCREASED IMAX AND ITEMP( ) TO 70 FOR MEX BUFR!
C   12-10-22  ENGLE  - READ FROM KFILDI NOW USES ISCALE AND IREF WHICH
C                      ARE INTEGER VERSIONS OF SCALE AND REF. BEFORE THE
C                      SUBROUTINE RETURNS, SCALE AND REF ARE SET TO
C                      ISCALE AND IREF RESPECITVELY USING THE REAL
C                      FUNCTION.
C                                                                       
C                                                                       
C USAGE:                                                                
C                                                                       
C     JULY  1992   GILBERT    TDL   NAS9000                             
C                                                                       
C     PURPOSE                                                           
C         READS A SET OF INPUT CONTROL RECORDS.  THE SET CONSISTS OF    
C         AN ELEMENT DESCRIPTOR DESCRIBING A FORECAST ELEMENT, THE      
C         ASSOCIATED FORECAST IDENTIFIER, THE SCALING AND REFERENCE
C         VALUES, AND A LIST OF VALID PROJECTIONS FOR THE FORECAST.     
C         ALSO, A LIST OF THE DESCRIPTORS IS KEPT.                      
C                                                                       
C     DATA SET USE                                                      
C         FT(KFILDI) - INPUT CONTROL DATA SET.                           
C                                                                       
C     VARIABLES                                                         
C          IVPRJ() = LIST OF VALID PROJECTIONS FOR THE FORECAST ELEMENT 
C          LDESC() = LIST OF ELEMENT DESCRIPTORS READ.
C           MAXPRJ = MAXIMUM NUMBER OF PROJECTIONS.                     
C           MAXDSC = MAXIMUM NUMBER OF DESCRIPTORS.                     
C            ID(4) = MOS FORECAST IDENTIFIER
C            SCALE = BUFR TABLE B; SCALE VALUE                          
C           ISCALE = INTEGER VALUE OF SCALE READ FROM KFILDI            
C              REF = BUFR TABLE B; REFERENCE VALUE                      
C             IREF = INTEGER VALUE OF REF READ FROM KFILDI              
C            NBITS = BUFR TABLE B; NUMBER OF BITS                       
C               IF = 1ST PART OF DESCRIPTOR (F|X|Y)                     
C               IX = 2ND PART OF DESCRIPTOR (F|X|Y)                     
C               IY = 3RD PART OF DESCRIPTOR (F|X|Y)                     
C             IEND = FLAG INDICATING THE END OF THE INPUT CONTROL LIST  
C            ITERM = TERMINATOR FOR THE LIST OF VALID PROJECTIONS       
C             CFMT = FORMAT OF VSLID PROJECTION LIST  (CHAR*8)          
C           KFILDI = DATA SET REFERENCE NUMBER FOR THE INPUT CONTROL    
C                    LIST                                               
C                                                                       
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  NAS                                                       
C                                                                       
C$$$                                                                    
      INTEGER IVPRJ(MAXPRJ),ITEMP(70),ID(4)
      INTEGER LDESC(MAXDSC)
CINTEL
      INTEGER ISCALE,IREF
CINTEL
      REAL SCALE,REF
      CHARACTER*1 CFLAG
      CHARACTER*8 CFMT
      CHARACTER*18 CDESC
      DATA ITERM/999/,CFMT/'(28I3)  '/,IMAX/28/
      SAVE ITERM,CFMT,IMAX
ccc
ccc      write(6,*) '*************************************************'
ccc      write(6,*) 'maxprj,imax,cfmt,iterm'
ccc      write(6,*) maxprj,imax,cfmt,iterm
ccc      write(6,*) '*************************************************'
ccc 
CINTEL
C      READ(KFILDI,200) IF,IX,IY,(ID(I),I=1,4),SCALE,REF,NBITS,IOPT,
C     *                CFLAG,CDESC
C 200  FORMAT(I2,1X,I2,1X,I3,4I10,1X,F2.0,1X,F6.0,1X,I2,1X,I2,1X,
C     *       A1,1X,A18)
      READ(KFILDI,200) IF,IX,IY,(ID(I),I=1,4),ISCALE,IREF,NBITS,IOPT,
     *                CFLAG,CDESC
 200  FORMAT(I2,1X,I2,1X,I3,4I10,1X,I2,1X,I6,1X,I2,1X,I2,1X,
     *       A1,1X,A18)
CINTEL
      WRITE(6,400) IF,IX,IY,(ID(I),I=1,4),ISCALE,IREF,
     *             NBITS,IOPT,CFLAG,CDESC
 400  FORMAT(I2,1X,I2,1X,I3,4I10,1X,I2,1X,I6,1X,I2,1X,I2,1X,
     *       A1,1X,A18)
      IF (IF.EQ.9) THEN
        IEND=1
        GOTO 900
      ELSE
ccc
ccc      write(6,*) '*************************************************'
ccc      write(6,*) 'part two:  maxprj,imax,cfmt,iterm'
ccc      write(6,*) maxprj,imax,cfmt,iterm
ccc      write(6,*) '*************************************************'
ccc 
      CALL RDI(6,0,KFILDI,IVPRJ,MAXPRJ,ITEMP,IMAX,CFMT,NVPRJ,ITERM,IER)
      ENDIF
CINTEL
C
C        CONVERT ISCALE AND IREF TO REALS.  THE REST OF THE PROGRAM
C        USES SCALE AND REF (THE REAL VARIABLE VERSION).
C
      SCALE=REAL(ISCALE)
      REF=REAL(IREF)
CINTEL
      NDESC=NDESC+1
      LDESC(NDESC)=(IF*16384)+(IX*256)+IY
C
 900  RETURN
      END
