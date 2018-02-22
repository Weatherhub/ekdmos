      SUBROUTINE CHNGDATE(NDATE,KXTAU,NDATE1)                    
C                                                             
C     FEBRUARY 1974  GLAHN   IBM 360/195              
C     FEBRUARY 1977          MODIFIED FOR TAU'S OF LARGE MAGNITUDE   
C     APRIL 1978     FOSTER  MODIFIED TO RETURN HH IN THE INTERVAL
C                            0 TO 23 IF NDATE IS PASSED AS A
C                            NEGATIVE NUMBER.        
C     JUNE 1988    JENSENIUS COMPILED UNDER FORTRAN77             
C     MARCH 1998   GILBERT   ADDED CHECK FOR LEAP YEAR IN A
C                            CENTURY YEAR.
C                                                                  
C         PURPOSE                                                 
C             ADDS KXTAU HOURS TO BASIC DATE NDATE OF FORM       
C             YYYYMMDDHH AND MODIFYS YYYY, MM, DD, AND HH AS NECESSARY.
C             FINAL FORM IS SAME AS MOS PREDICTAND TAPE,              
C             HH WILL BE 1 TO 24 INCLUSIVE IF NDATE IS POSITIVE.     
C             HH WILL BE 0 TO 23 INCLUSIVE IF NDATE IS NEGATIVE.    
C                                                                  
C         VARIABLES                                               
C            NDATE = BASIC DATE IN FORM YYYYMMDDHH               
C            KXTAU = NUMBER OF HOURS TO ADD TO BASIC DATE       
C                    CAN BE A POSITIVE OR NEGATIVE NUMBER      
C           NDATE1 = RESULTING DATE                           
C               JD = DAY PORTION OF DATE                     
C               JH = HOUR PORTION OF DATE                   
C               JM = MONTH PORTION OF DATE                 
C               JY = YEAR PORTION OF DATE                 
C               JT = VARIABLE CONTAINING RESIDUAL OF DATE IN THE    
C                    CALCULATION OF THE YEAR, MONTH, DAY, AND HOUR.
C            MDATE = ABSOLUTE VALUE OF INPUT DATE                 
C             MFEB = VARIABLE USED TO TEST FOR LEAP YEAR         
C            MTEST = ARRAY CONTAINING NUMBER OF DAYS IN EACH MONTH  
C               NH = UPPER LIMIT OF ALLOWABLE HOURS (EITHER 23 OR 24) 
C                                                                    
C         REMARKS                                               
C           THIS ROUTINE ONLY WORKS WITH DATES THAT HAVE A FOUR DIGIT 
C           YEAR.  IMPROPER RESULTS MAY BE OBTAINED IF A DATE WITH A TWO
C            DIGIT YEAR IS PASSED INTO THE ROUTINE.
C                                                                   
      DIMENSION MTEST(12)                                          
      DATA MTEST/31,28,31,30,31,30,31,31,30,31,30,31/             
      MDATE=NDATE      
      NH=24             
      IF (MDATE.LT.0) THEN
        NH=23 
        MDATE=-MDATE 
      ENDIF
      JY=MDATE/1000000       
      JT=MDATE-(JY*1000000)  
      JM=JT/10000          
      JT=JT-(JM*10000)     
      JD=JT/100          
      JH=JT-(JD*100)+KXTAU
 110  MTEST(2)=28      
      MFEB=MOD(JY,4)  
      IF (MFEB.EQ.0) MTEST(2)=29   
C        IF YEAR IS A CENTURY YEAR AND NOT DIVISIBLE BY 400 THEN
C        IT IS NOT A LEAP YEAR.
      MCEN=MOD(JY,100)  
      IF (MCEN.EQ.0) THEN
        MMIL=MOD(JY,400)  
        IF (MMIL.NE.0) MTEST(2)=28
      ENDIF
 120  IF(JH-NH)140,180,130           
 130  JH=JH-24                        
      JD=JD+1                          
      IF(JD.LE.MTEST(JM)) GO TO 120     
      JD=1                               
      JM=JM+1                             
      IF(JM.LE.12) GO TO 120               
      JY=JY+1                               
      JM=1                                   
C        IF JY CHANGES, CHECK FOR LEAP YEAR.  
      GO TO 110                                
 140  IF(JH)160,150,180       
 150  IF(NH.EQ.23) GO TO 180  
 160  JH=JH+24              
      JD=JD-1                
      IF(JD.GT.0) GO TO 120   
      JM=JM-1              
      IF(JM.LE.0) GO TO 170 
      JD=MTEST(JM)           
C        IF JY CHANGES, CHECK FOR LEAP YEAR.     
      GO TO 110               
 170  JM=12                    
      JD=31                     
      JY=JY-1                    
      GO TO 120                   
 180  NDATE1=JY*1000000+JM*10000+JD*100+JH      
      RETURN                       
      END                           
