C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  
C                .      .    .                                       . 
C MAIN PROGRAM: MDL_GRB2MDLP
C   PRGMMR: MERICKSON        ORG: OSD211      DATE: 2000-05-22
C                                                                       
C ABSTRACT: PROGRAM ARCHIVES SELECTED MODEL FORECAST FIELDS FROM A      
C   SPECIFIED MODEL AND WRITES THEM TO AN OUTPUT FILE IN MDLPACK.
C   MODELS FIELDS ARE READ IN GRIB, AND ARE WRITTEN IN MDLPACK.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   94-01-10  GILBERT                                                   
C   97-03-05  ERICKSON  OVERHAULED CODE TO USE NEW FORMATS, GRIB AND
C                       MDLP.
C   97-05-07  ERICKSON  CHANGED CODE TO USE DYNAMIC ALLOCATION OF GRID
C                       ARRAYS.
C   97-06-25  ERICKSON  CHANGED CODE TO SAY STORGRID NOT STORNGM
C                       ADDED OUTPUT FILE ON UNIT 52.   
C   97-08-14  ERICKSON  ADDED IOPT VALUE FOR CONVERTING PRECIP RATE TO
C                       PRECIP AMOUNT. ADDED CK ON IER AFTER WRITEP.
C                       ADDED RECORD NUMBERS TO INVOUT FILE.
C   98-10-10  CARROLL   MODIFIED TO MAKE Y2K COMPLIANT.
C 2000-01-01  ERICKSON  MODIFIED TO HANDLE RON'S TSTM
C                       ND5=12000, BMP(200,200)
C 2000-03-22  ERICKSON  MODIFIED TO USE BAOPEN AND NEW GRIB CODES
C                       MODIFIED DIMENSIONS ID63,KGDS,KPDS TO 200
C                       CHNGD BMP FROM LOGICAL TO LOGICAL*1
C 2000-05-16  ERICKSON  ADOPTED FROM CODE STORG2000 FOR OPERATIONS
C 2002-04-15  MALONEY   MODIFIED TO UTILIZE NEW GETGRID.F FOR
C                       NEW ETA ARCHIVE IN LAMBERT PROJECTION;
C                       ADDED MAPPROJ AND TANLAT; INCREASED SIZE
C                       OF BMP AND LBMS TO 400*400 TO ALLOW FOR 
C                       LARGE AWIP32 GRID FOR ETA ARCHIVE.
C                       ADDED COMMENT FOR JUNIT; CORRECTED IUNIT CMT.
C                       INCREASED ND5,IPACK TO 100000.
C 2002-06-18  MALONEY   FIXED HARDWIRING OF IS2(9) TO USE TANLAT
C                       FROM GRIDS FILE.
C 2002-07-17  MALONEY   CHANGED 152 ERROR STATEMENT TO ONLY PRINT THE
C                       FIRST 25 VALUES OF THE ID63() ARRAY.
C 2012-06-08  ENGLE     MODIFICATION FOR WCOSS (INTEL/LINUX); ADDED CONVERT= SPECIFIER 
C                       TO OPEN STATEMENT FOR OUTPUT TDLPACK FILE; CHANDED ALL ENVVAR
C                       FROM 'XLFUNIT_  ' TO 'FORT  '; CHANGED WRITE STATEMENT TO
C                       ENNVAR FROM ENVVAR(9:10) TO ENVVAR(5:6).
C 2013-01-29  ENGLE     INITIALIZE IS0, IS1, IS2, IS4 ARRAYS TO 0.
C                                                                       
C USAGE:                                                                
C   INPUT FILES:                                                        
C     FT05F001    - CONTROL DATASET:                     
C     FT10F001    - NCEP DATE FILE: /COM/NCEPDATE                  
C     FT'NFIL'    - GRIB DATA FILES, SET BY NFIL ON CONTROL CARD
C     FT'NFILI'   - GRIB INDEX FILES, SET BY NFILI ON CONTROL CARD
C     FT30F001    - GRID IDENTIFIERS: /NWPROD/FIX/MDL_GRIDLST
C                                                                       
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)                            
C     FT51F001    - DATA TO BE ARCHIVED. MDLPACK FORMAT                       
C     FT52F001    - INVENTORY OF FIELDS ARCHIVED. (VARIABLE INVOUT)
C     FT06F001    - DATE, ROTATING FILE USED, NUMBER OF FIELDS          
C                   ARCHIVED.                                           
C                                                                       
C   VARIABLES                                                         
C      DX()     = X INCREMENT (IN KM) AT TANLAT 
C      DY()     = Y INCREMENT (IN KM).                                
C      ENVVAR   = CHARACTER REPRESENTATION OF INPUT UNIT
C      FILEN    = 8 CHARACTER NAME TO IDENTIFY INPUT GRIB FILE.
C                 ONLY USED FOR PRINTING.               
C  FILEI,FILEG  = FILENAMES FOR GRIB INDEX AND DATA FILE.
C      GRDTDL   = GRIDPOINT DATA ON MDL GRID (SUBSET OF GRDNMC).    
C      HMODL    = NAME OF MODEL DATA BEING ARCHIVED. (NGM, ETA, ETC.)
C                 READ FROM INPUT CARDS. USED TO GET ADDITIONAL GRID 
C                 INFORMATION FROM "GRIDS" FILE ON UNIT 30. 
C      IERR     = ERROR RETURN CODE FROM VARIOUS ROUTINES.            
C      IYR      = YEAR READ FROM DATE FILE (4 DIGIT).                        
C      IMO      = MONTH READ FROM DATE FILE.                          
C      IDA      = DAY READ FROM DATE FILE.                            
C      IHR      = HOUR (CYCLE) READ FROM DATE FILE.                           
C      IGRIB    = NMC ASSIGNED GRIB VALUE IDENTIFYING MDL GRID.       
C      IGRID    = NMC ASSIGNED VALUE OF MDL GRID.                     
C      IOFFX()  = OFFSET OF MDL GRID TO NMC GRID (X DIRECTION)       
C      IOFFY()  = OFFSET OF MDL GRID TO NMC GRID (Y DIRECTION)        
C      IMAP     = MAP PROJECTION FOR IS2(2)
C                 3 IS N.H. LAMBERT, 5 IS N.H. POLAR STEREOGRAPHIC
C                 7 IS MERCATOR
C      IOPT     = 1 IF CONVERSION FROM PA TO HPA (MB) IS NEEDED.
C               = 2 IF CONVERSION FROM KG/M**2 (MM) TO METERS IS NEEDED.
C               = 3 IF CONVERSION FROM PRECIP RATE TO PRECIP AMT.
C      IPWR10   = POWER OF 10 SCALING TO USE
C      IPWR2    = POWER OF 2 SCALING TO USE (MDLP VERSION)
C      IUNIT    = 10: UNIT NUMBER OF DATE FILE.                       
C      IX       = X DIMENSION OF NMC'S GRID.                          
C      IY       = Y DIMENSION OF NMC'S GRID.                          
C      II       = X DIMENSION OF MDL GRID.                            
C      JJ       = Y DIMENSION OF MDL GRID.                            
C      JUNIT    = 30: UNIT NUMBER OF GRID CONSTANT FILE.
C      ID63     = 27 INTEGER ARRAY CONTAINING PDS INFO IN W3FI63 FORMAT.
C                 GETBG1 REQUIRES THIS FORM.
C      ID68     = 25 INTEGER ARRAY CONTAINING PDS INFO IN W3FI68 FORMAT.
C                 ONLY THE FOLLOWING ELEMENTS ARE SET - OTHERS = -1.
C                 (3)-ORIGINATING CENTER (GRIB OCTET 5)
C                 (4)-MODEL ID (GRIB OCTET 6)
C                 (5)-GRID ID (GRIB OCTET 7) 
C                 (8)-PARAMETER (GRIB OCTET 9)
C                 (9)-TYPE OF LEVEL (GRIB OCTET 10) 
C                 (10)-VALUE 1 OF LEVEL (GRIB OCTET 11)
C                 (11)-VALUE 2 OF LEVEL (GRIB OCTET 12)
C                 (18)-P1 (PERIOD OF TIME) (GRIB OCTET 19)
C                 (19)-P2 (PERIOD OF TIME) (GRIB OCTET 20)
C                 (20)-TIME RANGE INDICATOR (GRIB OCTET 21)
C      KFILDO   = 06: UNIT NUMBER OF PRINT FILE.                      
C      KFILIO   = UNIT NUMBER FOR MDLP OUTPUT FILE.                      
C      MAPPROJ  = MAP PROJECTION TYPE (FROM GETGRID):
C                    NPS = NORTHERN POLAR STEREOGRAPHIC
C                    LAM = LAMBERT CONICAL CONFORMAL
C                    MER = MERCATOR
C      NCYCLE   = OPERATIONAL CYCLE TIME (00Z OR 12Z). READ FROM      
C                 INPUT CARDS.                                        
C      NRBAD    = COUNTER OF REQUIRED RECORDS WHICH ARE NOT           
C                 PROCESSED CORRECTLY                                 
C      NUMREC   = COUNTER OF REQUIRED RECORDS PROCESSED CORRECTLY     
C      NUNIT    = 05: UNIT NUMBER OF INPUT CARDS.                     
C      NFIL     = UNIT NUMBER OF GRIB DATA FILE BEING READ.
C      NFILI    = UNIT NUMBER OF GRIB INDEX FILE BEING READ.
C      NFLDS    = NUMBER OF FIELDS TO PROCESS FROM AN INPUT FILE.     
C      NPTS     = NUMBER OF GRID POINTS IN MDL GRID.                  
C      NTOTBY   = TOTAL NUMBER OF BYTES IN FILE - INCREMENTED IN
C                 WRITEP, NOT USED.
C      NTOTRC   = TOTAL NUMBER OF RECORDS WRITTEN - INCREMENTED IN 
C                 WRITEP, NOT USED BY STORGRID NOW.
C      PLAIN    = PLAIN LANGUAGE IDENTIFICATION OF FIELDS.
C      POLEX()  = X COORDINATE OF NORTH POLE (MDL GRID)               
C      POLEY()  = Y COORDINATE OF NORTH POLE (MDL GRID)               
C      ORLAT()  = LATITUDE OF ORIGIN (1,1) OF MDL GRID. (DEGREES N)   
C      ORLONG   = LONGITUDE OF ORIGIN (1,1) OF MDL GRID. (DEGREES N)  
C      ORIENT() = ORIENTATION OF MDL GRID.  MERIDIAN PARALLEL TO Y    
C                 AXIS. (DEGREES WEST)                               
C      TANLAT   = FOR LAMBERT CONICAL CONFORMAL, THE LATITUDE WHERE
C                 CONE IS TANGENT.  FOR N POLAR STEREOGRAPHIC, LATITUDE
C                 WHERE GRID LENGTH APPLIES (USUALLY 60N).  FOR MERCATOR,
C                 ???.  (READ FROM GETGRID)
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C      UNIQUE:    - PDCHNG                                      
C     LIBRARY:                                                          
C       W3LIB     - W3LOG, W3TAGB, W3TAGE, GETGB                               
C      TDLLIB     - GETDATE, GETGRID                       
C    MOS2000LIB   - PACK2D, WRITEP
C                                                                       
C   EXIT STATES:                                                        
C     COND =    0 - SUCCESSFUL RUN                                      
C          =   50 - TROUBLE READING STANDARD DATE FILE.                 
C          =   70 - GRID INFO NOT FOUND FOR REQUESTED MODEL.           
C                                                                       
C   REMARKS:                                                            
C                                                                       
C   ATTRIBUTES:                                                         
C     LANGUAGE:     FORTRAN 90                                          
C     MACHINE :     IBM
C                                                                       
C$$$                                                                    
C23456789112345678921234567893123456789412345678951234567896123456789712
      PROGRAM STORGRID
C ******* DYNAMIC ALLOCATION IS SET UP HERE ***************************
C
      INTEGER IA(:,:),IC(:,:)
      REAL FLD(:),GRDTDL(:)
      ALLOCATABLE IA,IC,FLD,GRDTDL
C
C        THE ARRAY DATA IS ONLY NEEDED WHEN CALL TO W3FI63 IS USED
C     REAL DATA(:)
C     ALLOCATABLE DATA
C *********************************************************************
      INTEGER IGRID,IGRIB,II,JJ,IX,IY,IOFFX,IOFFY,ID68(200),IDTDL(4),
     *    ID63(200),IS0(60),IS1(60),IS2(60),IS4(60),IPACK(100000),
C    *    IA(130,90),IC(130,90),KGDS(200),KPDS(200),MINPK,L3264B,IMAP,
     *    KGDS(50),KPDS(25),MINPK,L3264B,IMAP,
     *    IPWR10,IPWR2,ITST(11),LPDS(60),LGDS(50),KPTR(15),
     *    NTOTBY, NTOTRC
C     REAL POLEX,POLEY,ORLAT,ORLONG,ORIENT,DX,DY,FLD(16200),
C    *    GRDTDL(16200),XMISSP,XMISSS,DATA(16200)
      REAL POLEX,POLEY,ORLAT,ORLONG,ORIENT,DX,DY,
     *    XMISSP,XMISSS
      CHARACTER*1 PLAIN(32)
      CHARACTER*8 FILEN,END
      CHARACTER*11 ENVVAR
      CHARACTER*80 FILEI,FILEG
      CHARACTER*4 HMODL,LMODL,BLANK
      CHARACTER*3 MAPPROJ
C     CHARACTER*1 GRIB(67500)
C        ABOVE FIELD NO LONGER NEEDED WITH GETGB
      LOGICAL*1 BMP(400*400),LBMS(400*400)
C ********WORD LENGTH IS SET HERE ************************************
C
      DATA L3264B/32/
C
C ********UNIT NUMBERS ARE SET HERE **********************************
C
      DATA JUNIT/30/,IUNIT/10/,KFILIO/51/,NUNIT/5/,KFILDO/6/,
     *    INVOUT/52/
C
C ********************************************************************
      DATA NUMREC/0/,NTOTBY/0/,NTOTRC/0/,
     *    END/'99999999'/,LMODL/'    '/,ID63/200*-1/,BLANK/'    '/,
     *    ND7/60/,ND5/100000/,XMISSP/0./,XMISSS/0./,MINPK/14/,
     *    PLAIN/32*' '/,
     *    ITST/24,60,86,128,147,194,203,220,259,309,392/
C
CINTEL
C**********************************************************************
C        INITIALIZE IS* ARRAYS
C**********************************************************************
      IS0=0
      IS1=0
      IS2=0
      IS4=0
C**********************************************************************
C        OPEN TDLPACK OUTPUT FILE
C**********************************************************************
      OPEN(UNIT=KFILIO,FORM='UNFORMATTED',STATUS='NEW',
     1     IOSTAT=IOS,CONVERT='BIG_ENDIAN',ERR=500)
 500  IF(IOS.NE.0)THEN
         WRITE(KFILDO,505)KFILIO,IOS
 505     FORMAT(/,' ****TROUBLE OPENING FILE ON UNIT. ',I3,
     1            '. IOSTAT = ',I5)         
      ENDIF
CINTEL
C**********************************************************************
C        CALL LOGS / INDICATE CODE IS STARTING
C**********************************************************************
C
        CALL W3TAGB('MDL_GRB2MDLP',2000,0143,0064,'OSD211')                 
      WRITE(KFILDO,10)
 10   FORMAT('1* * * * *  STORGRID HAS BEGUN EXECUTION  * * * * *'//)
      NRBAD=0
C
C**********************************************************************
C        GET DATE FROM NMC STANDARD DATE FILE                           
C**********************************************************************
C
      CALL GET_NCEPDATE(IUNIT,IYR,IMO,IDA,IHR,KDATE,IERR)
      WRITE(6,*)IUNIT,IYR,IMO,IDA,IHR,KDATE
      IF (IERR.NE.0) THEN
        WRITE(KFILDO,50)IERR
 50     FORMAT(' PROBLEM READING NMC DATE FILE.  ERROR= ',I4)
C        CALL W3LOG('$E',50,'ERROR READING NMC STANDARD DATE FILE:')     
        CALL W3TAGE('MDL_GRB2MDLP')                                          
        STOP 50
      ENDIF
C
C**********************************************************************
C        READ CONTROL SET TO DETERMINE FILE/FIELDS INFORMATION          
C**********************************************************************
C
 100  READ(NUNIT,101) FILEN,NFLDS,NCYCLE,NFIL,NFILI
 101  FORMAT(A8,4I4)
      IF(FILEN.EQ.END)GO TO 300
      IF (NCYCLE.NE.IHR) THEN
        WRITE(KFILDO,110) NCYCLE,IHR
 110  FORMAT(' PROBLEM ENCOUNTERED, CONTROL SET INDICATES ',I2,'Z CYCLE,
     * NMC DATE INDICATES ',I2,'Z CYCLE')
        GO TO 400
      ENDIF
      WRITE(KFILDO,112) IMO,IDA,IYR,IHR
 112  FORMAT(' ARCHIVING DATA FOR ',2(I2,'/'),I4,' AT',I3,'Z.')
      WRITE(KFILDO,120) FILEN,NFLDS,NCYCLE
 120  FORMAT(1X,'STARTING ',A8,' FILE',I5,' FIELDS ',I2.2,' GMT CYCLE')
      WRITE(INVOUT,120) FILEN,NFLDS,NCYCLE
C
C        OPEN GRIB FILE FOR READING
CINTEL
C     ENVVAR='XLFUNIT_   '
      ENVVAR='FORT   '
C     WRITE(ENVVAR(9:10),FMT='(I2)') NFIL
      WRITE(ENVVAR(5:6),FMT='(I2)') NFIL
CINTEL
      CALL GETENV(ENVVAR,FILEG)
      CALL BAOPEN(NFIL,FILEG,IRET)
C     write(6,121) iret
C121  format(' iret after baopen:',i4)
C        OPEN INDEX FILE
CINTEL
C     ENVVAR='XLFUNIT   '
      ENVVAR='FORT   '
C     WRITE(ENVVAR(9:10),FMT='(I2)') NFILI
      WRITE(ENVVAR(5:6),FMT='(I2)') NFILI
CINTEL
      CALL GETENV(ENVVAR,FILEI)
      CALL BAOPEN(NFILI,FILEI,IRET)
C     write(6,121) iret
C
C**********************************************************************
C        LOOP THROUGH THE FIELDS, RETRIEVING, CONVERTING, AND WRITING
C**********************************************************************
C
      NFREC=0
      DO 200 NF=1,NFLDS
C
C**********************************************************************
C        FIRST RETRIEVE THE DESIRED DATA FROM GRIB FILE
C**********************************************************************
C23456789112345678921234567893123456789412345678951234567896123456789712
C        READ THE W3FI68 INTEGER VERSION OF THE GRIB PDS FOR THIS FIELD
      READ(NUNIT,140) (ID68(I),I=1,25),IOPT
 140  FORMAT(26I4)
C        READ THE MDL ID AND MODEL NEEDED
      READ(NUNIT,142) (IDTDL(I),I=1,4),HMODL,IMAP,IPWR10,IPWR2,
     * (PLAIN(MM),MM=1,32)
 142  FORMAT(3I10.9,I4,A4,I2,2I4,1X,32A1)
      WRITE(INVOUT,143) NF,(IDTDL(I),I=1,4),IOPT,HMODL,IMAP,IPWR10,
     * IPWR2,(PLAIN(MM),MM=1,32)
 143  FORMAT(I5,'.',3I10.9,2I4,1X,A4,I2,2I4,1X,32A1)
C        GET GRID DESCRIPTION CONSTANTS                                 
      IF(HMODL.NE.LMODL) THEN
C          CHANGED CALL FROM GTGRID TO GETGRID
C       CALL GTGRID(HMODL,IGRID,IGRIB,II,JJ,IX,IY,IOFFX,IOFFY,
C    *     POLEX,POLEY,ORLAT,ORLONG,ORIENT,DX,DY,JUNIT,JERR)
        CALL GETGRID(HMODL,IGRID,IGRIB,II,JJ,IX,IY,IOFFX,IOFFY,
     *     POLEX,POLEY,ORLAT,ORLONG,ORIENT,DX,DY,MAPPROJ,
     *     TANLAT,JUNIT,JERR)
        IF (JERR.NE.0) THEN
          WRITE(KFILDO,145) HMODL,JERR
 145      FORMAT(' PROBLEM FINDING MDL GRID FOR MODEL ',A4,'. JERR=',I4)
C         CALL W3LOG('$E',70,'ERROR READING MDL GRID FILE:')              
          CALL W3TAGE('MDL_GRB2MDLP')                                          
          STOP 145
        ENDIF
        IF(LMODL.NE.BLANK) THEN 
          DEALLOCATE (FLD,GRDTDL,IA,IC,STAT=IS)
          IF(IS.NE.0) THEN
            WRITE(KFILDO,147) IS
 147        FORMAT(' PROBLEM DEALLOCATING GRID ARRAYS, STAT=',I4)
            CALL W3TAGE('MDL_GRB2MDLP')                                          
            STOP 147
          ENDIF
        ENDIF
        ALLOCATE (FLD(IX*IY),GRDTDL(II*JJ),IA(II,JJ),IC(II,JJ),STAT=IS)
        IF(IS.NE.0) THEN
          WRITE(KFILDO,148) IS
 148      FORMAT(' PROBLEM ALLOCATING GRID ARRAYS, STAT=',I4)
          CALL W3TAGE('MDL_GRB2MDLP')                                          
          STOP 148
        ENDIF
      ENDIF
C
CCCCCCCCCC  ADD A CHECK OF MAPPROJ VS. IMAP?
C
C     WRITE(KFILDO,71) HMODL,IX,IY
C71   FORMAT(' CURRENT GRID:',A4,' DIMENSION X:',I4,' Y:',I4)
      LMODL=HMODL
C        CHANGE THE W3FI68 PDS TO A W3FI63 PDS
      CALL PDCHNG(ID68,ID63)
      ID63(8)=MOD(IYR-1,100) + 1
      ID63(9)=IMO
      ID63(10)=IDA
      ID63(11)=IHR
      MAXPTS=IX*IY
      ISRCH=0
C        CHANGED TO CALL GETGB, NEW GRIB CONVENTION
      CALL GETGB(NFIL,NFILI,MAXPTS,ISRCH,ID63,KGDS,
     *           NUMPTS,INUM,KPDS,KGDS,BMP,FLD,IRET)
C     CALL GETGB1(NFIL,NFILI,MAXPTS,ISRCH,ID63,KGDS,GRIB,NUMPTS,INUM,
C    *       KPDS,KGDS,BMP,FLD,IRET)
C     WRITE(6,147) KPDS
C147  FORMAT(' KPDS:',/,(' ',25I5))
      IF(IRET.NE.0) THEN
        WRITE(KFILDO,152) FILEN,IRET,(ID63(I),I=1,25)
 152    FORMAT(' ERROR READING FILE ',A8,' IRET= ',I4,
     *    '  REQUESTED PDS:',/,(' ',25I4))
        NRBAD=NRBAD+1
        GO TO 200
      ENDIF
C          THIS SECTION OF CODE ALLOWS YOU TO PRINT MORE
C          DETAILED INFORMATION.
C     CALL W3FI63(GRIB,LPDS,LGDS,LBMS,DATA,KPTR,KRET)
C     IF(KRET.NE.0) THEN
C       WRITE(KFILDO,1480) FILEN,IRET,LPDS
C1480   FORMAT(' ERROR FROM W3FI63 READING FILE ',A8,' IRET= ',I4,
C    *    'REQUESTED LPDS:',/,(' ',25I4))
C       GO TO 200
C     ENDIF
C     WRITE(IOUT,149) (LL,KPTR(LL),LL=1,15)
C149  FORMAT(' KPTR:',I2,I12)
C******************************************************************
C        EXTRACT MDL SUBGRID FROM NMC GRID.                             
C        USE SINGLE DIMENSIONED ARRAYS TO ALLOW FOR A                           
C        VARIETY OF GRID SIZES.  
C******************************************************************
C
      LL=0
      DO 156 J=1,JJ
        DO 155 I=1,II
        LL=LL+1
	IADD=(IOFFY+J-1)*IX + (IOFFX +I)
        GRDTDL(LL)=FLD(IADD)
 155    CONTINUE
 156  CONTINUE
C
C         THESE LINES LET YOU DUMP OUT FIELDS
C
C     DO 155 LL=1,11
C     IF(NUMREC.EQ.ITST(LL)) THEN
C     WRITE(6,152) (GRDTDL(KK),KK=1,II*JJ)
C152  FORMAT(' GRDTDL:',/,(1X,10F11.4))
C     ENDIF
C155  CONTINUE 
C
C******************************************************************
C        NOW DO CONVERSIONS IF NECESSARY
C******************************************************************
C     WRITE(INVOUT,159)
C159  FORMAT(' ENTERING IOPT TEST')
      IF (IOPT.EQ.1) THEN
        DO 160 KK=1,II*JJ
        GRDTDL(KK)=GRDTDL(KK)/100.0
 160    CONTINUE 
      ELSEIF (IOPT.EQ.2) THEN
        DO 165 KK=1,II*JJ
        GRDTDL(KK)=GRDTDL(KK)/1000.0
 165    CONTINUE 
      ELSEIF (IOPT.EQ.3) THEN
C        CHANGE PRECIP RATE TO PRECIP AMT
C       WRITE(INVOUT,167) IOPT
C167    FORMAT(' REACHED PRECIP RATE LOOP, IOPT=',I3)
        DO 168 KK=1,II*JJ
        GRDTDL(KK)=GRDTDL(KK)*43200.0
 168    CONTINUE 
      ENDIF
C           
C******************************************************************
C        NOW PUT DATA INTO MDLPACK FOR OUTPUT.         
C******************************************************************
C                    
C        FIRST SET UP IS1
C        IS1(2)=1 MEANS NO BIT MAP, GRID DEFINITION IS INCLUDED 
      IS1(2)=1
      IS1(3)=IYR
      IS1(4)=IMO
      IS1(5)=IDA
      IS1(6)=IHR
      IS1(7)=0
      IS1(8)=KDATE
      IS1(9)=IDTDL(1)
      IS1(10)=IDTDL(2)
      IS1(11)=IDTDL(3)
      IS1(12)=IDTDL(4)
      IS1(13)=MOD(IDTDL(3),1000)
      IS1(14)=0
      IS1(15)=MOD(IDTDL(1),100)
      IS1(16)=1
      IS1(17)=IPWR10
      IS1(18)=IPWR2
      IS1(22)=32
CINTEL
      DO KK=1,IS1(22)
        IS1(22+KK)=IACHAR(PLAIN(KK))
      END DO
C      DO 170 KK=1,IS1(22)
C      IS1(22+KK) = MOVA2I(PLAIN(KK))
C 170  CONTINUE
CINTEL
C
C        NOW SET UP IS2
C
      IS2(2)=IMAP
      IS2(3)=II
      IS2(4)=JJ
      IS2(5)=ORLAT*10000
      IS2(6)=ORLONG*10000
      IS2(7)=ORIENT*10000
      IS2(8)=DX*1000000
      IS2(9)=TANLAT*10000
C        NOTE THAT IS2(9) WAS HARDCODED FOR 60N, WHICH WAS
C        APPLICABLE FOR ALL N POLAR STEREOGRAPHIC GRIDS  
C        ARCHIVED, BUT NOT LAMBERT CONICAL OR MERCATOR.
C        TANLAT IS READ FROM GRID LIST FILE IN GETGRID.
C     IS2(9)=60*10000
C     WRITE(6,198) KFILDO,II,JJ,ND7,XMISSS,ND5,MINPK,L3264B
C 198 FORMAT(' PARAMETERS BEFORE CALL TO PACK2D:',/,' KFILDO:',I2,
C    *' II:',I3,' JJ:',I3,' ND7:',I8,' XMISSS:',F7.1,' ND5:',I6,
C    *' MINPK:',I4,' L3264B:',I3)
C
C**********************************************************************
C     PACK UP DATA 
C**********************************************************************
C
C23456789112345678921234567893123456789412345678951234567896123456789712
      CALL PACK2D(KFILDO,GRDTDL,IA,IC,II,JJ,IS0,IS1,IS2,IS4,ND7,XMISSP,
     *   XMISSS,IPACK,ND5,MINPK,LX,IOCTET,L3264B,IER)
      IF(IER.NE.0) THEN
        WRITE(KFILDO,180) IER,(IS1(NN),NN=8,12),LX,IOCTET
  180   FORMAT(' IER=',I5,' WHEN PACKING DATA FOR:',1X,5I10.9,/,  
     *  ' LX=',I5,' IOCTET=',I8,' AFTER PACK2D')
      ENDIF
C
C**********************************************************************
C     WRITE DATA TO OUTPUT UNIT
C**********************************************************************
C
      NWORDS=IOCTET*8/L3264B
      CALL WRITEP(KFILDO,KFILIO,IPACK,NWORDS,NTOTBY,NTOTRC,L3264B,IER)
      IF(IER.NE.0) THEN
        WRITE(KFILDO,190) IER
  190   FORMAT(' IER=',I4,' AFTER CALL TO WRITEP')
      ENDIF
      NUMREC=NUMREC+1
      NFREC=NFREC + 1
  200 CONTINUE
      WRITE(KFILDO,210) FILEN,NFREC,NFLDS
  210 FORMAT(' FILE: ',A8,2X,I4,' RECORDS WRITTEN. ',I4,' REQUESTED.')
      WRITE(INVOUT,210) FILEN,NFREC,NFLDS
      GO TO 100
  300 IF(NRBAD.NE.0) THEN
        WRITE(KFILDO,1006) NRBAD
 1006   FORMAT('0THERE WERE ',I4,' REQUIRED RECORDS THAT COULD NOT BE ',
     *  'FOUND FOR PROCESSING')
      ENDIF
      WRITE(KFILDO,1007) HMODL,IMO,IDA,IYR,IHR,NUMREC
 1007 FORMAT(//' ',A4,' DATA FOR ',2(I2,'/'),I4,' AT',I3,'Z, ARCHIVED.
     * NO. OF RECORDS=',I4)
      WRITE(INVOUT,1007) HMODL,IMO,IDA,IYR,IHR,NUMREC
      WRITE(KFILDO,1008)
 1008 FORMAT(//' * * * * *  STORGRID COMPLETED SUCCESSFULLY  * * * * *')
      WRITE(INVOUT,1008)
C     CALL W3LOG('$E')                                                  
       CALL W3TAGE('MDL_GRB2MDLP')                                          
      DEALLOCATE (FLD,GRDTDL,IA,IC)
  400 STOP
      END
