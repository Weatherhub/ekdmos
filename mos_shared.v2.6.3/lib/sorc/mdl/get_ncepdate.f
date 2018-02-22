      SUBROUTINE GET_NCEPDATE(IUNIT,IYR,IMO,IDA,IHR,NDATE,IERR)        
C                                                               
C        MARSHALL   JUNE 1983   IBM 360/195                        
C        ERICKSON   JUNE 1988   NAS9040   REVISED - VS FORTRAN    
C        GILBERT    JUNE 1995   MODIFIED TO CHANGE THE TWO DIGIT YEAR 
C                               TO THE FOUR DIGIT YEAR.  ALSO ADDED  
C                               AN ARGUMENT TO PASS BACK THE DATE IN
C                               TDL FORMAT AS WELL.       
C        GILBERT    FEB  1998   MODIFIED TO READ THE 4-DIGIT YEAR FROM
C                               NCEP'S DATE FILE INSTEAD OF USING THE
C                               WINDOWING TECHNIQUE TO CALCULATE THE
C                               4-DIGIT YEAR.
C        DREWRY     APR  2000   CHANGED NAME OF SOURCE FROM GETDATE TO
C                               GET_NCEPDATE. CHANGED ALL OCCURRANCES 
C                               OF NMC TO NCEP. 
C                                                        
C        PURPOSE                                        
C           TO READ DATE-TIME GROUP FROM NCEP'S STANDARD
C           DATE FILE AND RETURN IT TO CALLING PROGRAM IN TWO FORMATS. 
C                                                     
C        DATA SET USE                                
C            FTXX - NCEP'S STANDARD DATE FILE (XX=01-99) (INPUT)
C                                                   
C        VARIABLES                                 
C           IUNIT = 2-DIGIT UNIT NUMBER FOR DATE FILE (INPUT)   
C             IYR = 4-DIGIT YEAR (OUTPUT)         
C             IMO = 2-DIGIT MONTH (OUTPUT)       
C             IDA = 2-DIGIT DAY (OUTPUT)        
C             IHR = 2-DIGIT HOUR (OUTPUT)      
C           NDATE = DATE IN TDL FORMAT        
C            IERR = ERROR RETURN CODE        
C                   ZERO INDICATES VALID DATE RETURNED           
C                   NON-ZERO INDICATES INVALID DATE        
C                                          
      IERR=0                                
C                                            
C        READ NCEP'S STANDARD DATE FILE        
C                                              
      READ(IUNIT,100) IYR,IMO,IDA,IHR           
  100 FORMAT(6X,I4,3I2) 
C        MAKE SURE YEAR IS GT 00
      IF (IYR.LE.0) IERR=1  
C        MAKE SURE MONTH IS FROM 1 TO 12         
      IF ((IMO.LT.1).OR.(IMO.GT.12)) IERR=1       
C        MAKE SURE DAY IS FROM 1 TO 31             
      IF ((IDA.LT.1).OR.(IDA.GT.31)) IERR=1         
C        MAKE SURE HOUR IS FROM 0 TO 23    
      IF ((IHR.LT.0).OR.(IHR.GT.23)) IERR=1          
C        PUT DATE IN TDL FORMAT.                      
      NDATE=(IYR*1000000)+(IMO*10000)+(IDA*100)+IHR    
      REWIND IUNIT     
      RETURN            
      END                
