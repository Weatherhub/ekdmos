C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  
C                .      .    .                                       .  
C MAIN PROGRAM: MDL_MOS2BUFR
C   PRGMMR: ERICKSON         ORG: OST22       DATE: 2002-01-11
C                                                                       
C ABSTRACT: THIS PROGRAM GENERATES AND POSTS A BUFR MESSAGE             
C           CONTAINING MDL MOS FORECASTS FOR SPECIFIED STATIONS.        
C           THIS BUFR MESSAGE TREATS EACH PROJECTION OF EACH STATION    
C           AS A SUBSET, AND THE DATA ARE STORED IN COMPRESSED          
C           FORMAT.  WMO'S BUFR EDITION 2 IS CURRENTLY BEING USED.      
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   93-12-03  GILBERT                                                   
C   96-02-20  GILBERT  - CONVERTED TO CRAY
C   96-04-15  GILBERT  - ADDED SUBROUTINE LATLON FOR PROCESSING
C                        OF STATION LATITUDES AND LONGITUDES.
C   96-08-23  GILBERT  - FIXED PROBLEM WITH THE WMO HEADER GENERATED
C                        IN SUBROUTINE BUFRGN.
C   98-07-07  SHIREY   - CHANGED GTDATE TO GETDATE (2X) FOR Y2K.
C   98-07-23  ERICKSON - CHANGED SUBRT BUFRGN FOR Y2K
C   00-03-07  ALLEN    - CONVERTED TO RUN ON THE IBM SP.  CHANGES WERE
C                        MADE TO BUFRGN TO WRITE THE OUTPUT FILE AS A
C                        DIRECT ACCESS FILE.
C   01-03     SHIREY   - REVAMPED BUFR CODE FOR MOS-2000 SOFTWARE
C                        CHANGED SEVERAL DIMENSIONS IN ORDER TO HANDLE
C                        LARGER MESSAGES.  CHANGED READER TO READ_MOSDA.
C                        THE CODE NOW USES 32 BIT ROUTINES.  SEVERAL 
C                        TDLLIB ROUTINES WERE REPLACED WITH MOS2000
C                        ROUTINES. ALSO, SEVERAL PROCESSING ROUTINES
C                        WERE DELETED SINCE THIS PROCESSING IN NOW DONE
C                        IN U910  
C   02-01-02  COSGROVE - INCREASED MAXSTA TO ACCOMMODATE 1432 SITES. 
C                        MODIFIED BCMAP TO CORRECTLY CHANGE CATEGORICAL
C                        FORECASTS FOR AVN VISOBV.
C   02-01-02  ERICKSON - MODIFIED LATLON ROUTINE TO HANDLE SCALED VALUES 
C                        OF 9997 AND 9999 CORRECTLY.
C   03-04-16  COSGROVE - INCREASED MAXSTA TO ACCOMMODATE THE NEW MARINE
C                        SITES AND THE NEW MOS SITES WE ARE ADDING IN THE 
C                        FALL
C   04-11-18  COSGROVE - ADDED ARRAY INDEX TO CALL TO BUFOPT BECAUSE IT WAS 
C                        GETTING CLOBBERED ON BLUE BECAUSE IT WAS A LOCAL
C                        ARRAY.
C   05-07-22  MALONEY  - INCREASED MAXPRJ TO 70 TO HANDLE NEW MEX BUFR
C   06-04-25  MALONEY  - INCREASED MAXSTA TO 25000 FOR NEW SITES
C   09-12-08  MALONEY  - INCREASED MAXPRJ TO 80 TO HANDLE HPC SUPPORT
C   12-09-13  ENGLE    - CHANGED PROGRAM NAME FROM BUFRMOS2 TO MOS2BUFR
C                                                                       
C USAGE:                                                                
C                                                                       
C     PROGRAM MOS2BUFR                                                   
C                                                                       
C        JULY 1992    GILBERT    TDL   NAS9000                          
C                                                                       
C        PURPOSE                                                        
C            THIS PROGRAM GENERATES AND POSTS A BUFR MESSAGE            
C            CONTAINING TDL MOS FORECASTS FOR SPECIFIED STATIONS.       
C            THIS BUFR MESSAGE TREATS EACH PROJECTION OF EACH STATION   
C            AS A SUBSET, AND THE DATA ARE STORED IN COMPRESSED         
C            FORMAT.  WMO'S BUFR EDITION 2 IS CURRENTLY BEING USED.     
C                                                                       
C        DATA SET USE                                                   
C            FT05F001 - DATA CONTROL RECORDS (INPUT)                    
C            FT06F001 - PRINT FILE. (OUTPUT)                            
C            FT25F001 - STATION LIST & WMO HEADERS (INPUT)              
C            FT27F001 - STATION LIST (INPUT)              
C            FT60F001 - TRANSMISSION FILE (OUTPUT)                      
C            FT10F001 - NMC DATE FILE (INPUT)                           
C            MOSMATXX - MOSMAT FILE (XX = 00 OR 12)  (INPUT)            
C                                                                       
C        VARIABLES                                                      
C               MAXSTA = MAXIMUM NUMBER OF STATIONS IN STATION DICTIONARY
C                        -(MUST ACTUALLY BE # OF STAS X 2 FOR READ_MOSDA)      
C               MAXPRJ = MAXIMUM NUMBER OF PROJECTIONS                  
C               MAXDSC = MAXIMUM NUMBER OF DESCRIPTORS                  
C               MAXMES = MAXIMUM NUMBER OF BYTES IN A BUFR MESSAGE      
C               MAXBUL = MAXIMUM NUMBER OF WMO HEADERS                  
C               CSTA() = LIST OF STATION CALL LETTERS                   
C                WMO() = LIST OF WMO HEADERS                            
C              NWMO(I) = NUMBER OF STATIONS FOR HEADER WMO(I)           
C              LPROJ() = LIST OF PROJECTIONS FOR WHICH FORECASTS ARE    
C                        ENCODED                                        
C             KDATA(,) = HOLDS DATA FOR A FORECAST ELEMENT FOR EACH     
C                        STATION AND EACH PROJECTION. 1ST DIMENSION IS  
C                        # OF STATIONS, AND 2ND DIMENSION IS NUMBER OF  
C                        PROJECTIONS.  CONTAINS MISSING "9999" FOR      
C                        PROJECTION FOR WHICH FORECASTS ARE NOT VALID.  
C              MBUF(,) = HOLDS FORECASTS AFTER THEY ARE PACKED INTO BUFR
C                        FORMAT.  1ST DIMENSION IS # OF BYTES IN BUFR   
C                        MESSAGE, AND 2ND DIMENSION IS # OF BULLETINS   
C                        (WMO HEADERS).                                 
C              IVPRJ() = CONTAINS VALID PROJECTIONS FOR THE CURRENT     
C                        FORECAST ELEMENT.                              
C               NBPB() = # OF BITS IN EACH BULLETIN.                    
C               NBUL = NUMBER OF BUFR BULLETINS (WMO HEADERS)           
C                  IYR = CURRENT YEAR (4 DIGITS)                        
C                  IMO = CURRENT MONTH                                  
C                  IDA = CURRENT DAY                                    
C                  IHR = CURRENT CYCLE ( = 00 OR 12)                    
C                NDATE = CURRENT DATE                                   
C                CFLAG = SWITCH THAT PLACES MISSINGS FOR WINTER
C                        PRODUCTS IN THE SUMMER.      
C               KFILDI = DATA SET REFERNCE NUMBER FOR INPUT HEADERS     
C                        AND STATION LIST.                              
C                NUNIT = DATA SET REFERENCE NUMBER FOR INPUT CONTROL    
C                        RECORDS.                                       
C                 IEND = FLAG INDICATING END OF INPUT CONTROL RECORDS.  
C                ITERM = TERMINATOR FOR LIST OF PROJRCTIONS.            
C               NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE               
C         CCALL(ND1,6) = CALL LETTERS OF THE STATIONS
C                 CFMT = FORMAT OF INPUT CARD LINES TO READ
C               KFILDO = PRINT FILE
C                KFILX = UNIT # FOR MOS FCST FILE
C                CSEC2 = DESCRIPTION OF BUFR MESSAGE (IN SECT 2)
C           INDEX(K,L) = ARRAY CONTAINING LOCATION OF STATION K IN FILE
C                        DIRECTORY L, WHERE K=1,NSTA AND L=1,15
C                        "READ_MOSDA" (INPUT/OUTPUT)
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C     UNIQUE:    - BDRIVR, BPACK, BUFOPT, BUFRGN, RDWMO, BCATGR,        
C                  BPROB, BPROJ, BSPEC, BTMPDP, BWDIR, BWSPD, 
C                  BCLDMR, BDATE, BGENER, BNMLFQ, BCMAP, GETCON         
C     LIBRARY:                                                          
C       W3LIB    - W3TAG, W3FK40, W3AI15, W3FI62, GBYTE,        
C                  SBYTE, W3UTCDAT                                        
C       MDLLIB   - GET_NCEPDATE, RDC,RDI
C                                                                       
C   EXIT STATES:                                                        
C     COND =   0 - SUCCESSFUL RUN                                       
C          =  10 - PROBLEM READING NMC DATE FILE                        
C          = 200 - PROBLEM POSTING TRANSMISSION FILE                    
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                               
C   MACHINE:  SP                                                       
C                                                                       
C$$$                                                                    
      PROGRAM MOS2BUFR
      PARAMETER (MAXSTA=25000,MAXPRJ=80,MAXDSC=150,MAXMES=3000000)
CC      PARAMETER (MAXBUL=150)
      PARAMETER (MAXBUL=20)
      CHARACTER*1 CFLAG,MBUF(MAXMES,MAXBUL)
      CHARACTER*8 CSTA(MAXSTA),CCALL(MAXSTA,6),CFMT
      CHARACTER*11 WMO(MAXBUL)
      CHARACTER*21 CSEC2
      INTEGER KFILDO,KFILDI
      INTEGER NWMO(MAXBUL),KDATA(MAXSTA,MAXPRJ),LDESC(MAXDSC),ID(4),
     *        LPROJ(MAXPRJ),LTEMP(18),NBPB(MAXBUL),IVPRJ(MAXPRJ),
     *        INDEX(MAXSTA,15)
      DATA KFILDI/5/,NUNIT/25/,CFMT/'(18I4)  '/,ITERM/9999/,IEND/0/,
     *     NDESC/0/,NBPB/MAXBUL*0/,ICND/0/,KFILX/40/,
     *     IUNIT/10/,KFILDO/6/,KFILD/27/
      CALL W3TAGB('TDL_BUFRMOS2',2002,0011,0066,'OST22')                  
C
C        GET DESCRIPTION FOR SECTION 2 OF BUFR MESSAGE
C
      READ(NUNIT,3)CSEC2
 3    FORMAT(A21)
C
C        GET CURRENT DATE FROM NMC'S DATE FILE.                         
C
      CALL GET_NCEPDATE(IUNIT,IYR,IMO,IDA,IHR,NDATE,IERR)
      IF (IERR.NE.0) THEN
        WRITE(KFILDO,10) IERR
 10     FORMAT(' PROBLEM READING DATE FILE.  IERR = ',I4)
      CALL W3TAGE('TDL_BUFRMOS2') 
        CALL EXIT(10)
      ENDIF
      IDATE=(IMO*100)+IDA
C
C        READ WMO HEADERS AND ASSOCIATED STATION LISTS.                 
C
      CALL RDWMO(NUNIT,NSTA,CSTA,WMO,NWMO,NBUL,MAXSTA,MAXBUL)
C
      NEW=0
      CALL RDLNK(KFILD,KFILDO,NEW,CSTA,CCALL,NSTA,MAXSTA)
C
C        READ LIST OF PROJECTIONS                                       
C
      CALL RDI(KFILDO,0,KFILDI,LPROJ,MAXPRJ,LTEMP,18,CFMT,NUMPRJ,
     *         ITERM,IER)
C        READ A SET OF CONTROL RECORDS FROM THE INPUT CONTROL FILE.     
 100  CALL BDRIVR(ID,MAXDSC,LDESC,NDESC,SCALE,REF,NBITS,
     *            IVPRJ,MAXPRJ,NVPRJ,CFLAG,IEND,IOPT,KFILDI)
C        IEND = 1 INDICATES THE END OF THE INPUT CONTROL RECORDS.       
      IF (IEND.EQ.0) THEN
C        GET FORECASTS DATA AND DO NECESSARY CONVERSIONS TO PREPARE     
C        THE DATA FOR PACKING.                                          
        CALL BUFOPT(KFILDO,KFILX,NSTA,CCALL,KDATA,ID,LPROJ,
     *       SCALE,REF,NDATE,MAXSTA,IVPRJ,NVPRJ,NUMPRJ,IOPT,
     *       0,MAXSTA,54,INDEX,ICND)
C        CHECK TO SEE IF SNOW FORECASTS SHOULD BE CHECKED FOR AN        
C        INVALID SEASON.                                                
        IF (CFLAG.EQ.'*') THEN
          CALL SEACHK(IDATE,KDATA,MAXSTA,NUMPRJ,WMO,NWMO,NBUL)
        ENDIF
C        PACK CONVERTED AND SCALED DATA INTO MBUF.                      
        CALL BPACK(NSTA,KDATA,CSTA,NBITS,MBUF,NWMO,NBPB,NBUL,MAXSTA,
     *       NUMPRJ,MAXMES,IOPT)
        GOTO 100
      ENDIF
C        GENERATE ALL BUFR BULLETINS.                                   
      CALL BUFRGN(NSTA,WMO,NWMO,NBUL,LDESC,NDESC,MBUF,NBPB,IYR,IMO,
     *     IDA,IHR,MAXMES,NUMPRJ,CSTA,CSEC2)
      CALL W3TAGE('TDL_BUFRMOS2') 
      STOP
      END
