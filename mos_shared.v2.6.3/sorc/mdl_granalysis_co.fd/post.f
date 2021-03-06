      SUBROUTINE POST(KFILDO,XDATA,NVAL,TLO,SETLO,THI,SETHI,
     1                CONST,NSCAL,EX1,EX2,IER)
C
C        JUNE      2007   GLAHN   TDL   MOS-2000
C        MARCH     2008   GLAHN   CHANGED SO THAT SCALING IS
C                                 DONE AFTER TRUNCATING
C        JULY      2008   GLAHN   CORRECTED PRINT FORMAT 102
C        JULY      2008   GLAHN   CHANGED TEST ON TLO AND THI
C                                 BELOW 102
C        OCTOBER   2008   GLAHN   PUT XDATA(K)=XDATA(K)*FACTOR
C                                 AFTER ELSE ABOVE 120
C
C        PURPOSE
C            TO POSTPROCESS A VARIABLE.  THE VARIABLE IN XDATA( )
C            IS SET TO XDATA( )*CONST*10**NSCAL AFTER SETTING
C            ALL VALUES LT TLO TO SETLO AND ALL VALUES GT THI
C            TO SETHI.  EX1 AND EX2 ARE FOR POSSIBLE FUTURE USE.
C            THIS ROUTINE CAN BE USED FOR DISPOSABLE OR ARCHIVE
C            GRIDS.  WHEN TLO = -9999., THE LOW VALUE WILL NOT
C            BE MODIFIED.  WHEN THI = +9999., THE HIGH VALUE WILL
C            NOT BE MODIFIED. 
C   
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C            XDATA(K) = THE DATA TO SCALE (K=1,NVAL).  (INPUT-OUTPUT)
C                NVAL = THE NUMBER OF VALUES IN XDATA( ) BEING DEALT
C                       WITH.  (INPUT)
C                 TLO = LOW THRESHOLD.  WHEN A LAST PASS GRIDPOINT IS
C                       LT TLOD, IT IS SET TO SETLOD, THEN CONST
C                       AND NSCAL APPLIED.  (INPUT)
C               SETLO = SEE TLOD.  (INPUT)
C                 THI = HIGH THRESHOLD.  WHEN A LAST PASS  GRIDPOINT IS
C                       GT THID, IT IS SET TO SETHID, THEN CONST
C                       AND NSCAL APPLIED.  (INPUT)
C               SETHI = SEE THID.  (INPUT)
C               CONST = ADDITIVE CONSTANT TO FURNISH TO THRESHOLDING
C                       AND SCALING SUBROUTINE.  (INPUT)
C               NSCAL = SCALING CONSTANT TO FURNISH TO THRESHOLDING
C                       AND SCALING SUBROUTINE.  (INPUT)
C                 EX1 = EXTRA PARAMETER NOT YET USED FOR THRESHOLDING.
C                       (INPUT)
C                 EX2 = EXTRA PARAMETER NOT YET USED FOR THRESHOLDING.
C                       (INPUT)
C                 IER = ERROR RETURN.
C                       0 = GOOD RETURN.
C                       (OUTPUT)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES USED 
C            NONE
C
      DIMENSION XDATA(NVAL)
C
CD     CALL TIMPR(KFILDO,KFILDO,'START POST          ')
      IER=0
C
CD     WRITE(KFILDO,102)TLO,SETLO,THI,SETHI,CONST,NSCAL,EX1,EX2
CD102  FORMAT(/' AT 102 IN POST--TLO,SETLO,THI,SETHI,CONST,NSCAL,',
CD    1        'EX1,EX2',5F10.4,I4,2F10.4)
C
      FACTOR=CONST*10.**NSCAL
      SETLOF=SETLO*FACTOR
      SETHIF=SETHI*FACTOR
C
CCC      IF(TLO.LE.-99999.5.AND.
CCC     1   THI.GE.+99998.5)THEN
C        ABOVE MODIFIED AS BELOW 7/27/08.
C
      IF(TLO.LE.-9998.5.AND.
     1   THI.GE.+9998.5)THEN
C
         IF(CONST.EQ.1..AND.
     1      NSCAL.EQ.0)THEN
C              THERE IS NO CHANGE TO BE MADE TO XDATA( ).
            GO TO 150
C
         ELSE
C     
C              ONLY SCALING IS NECESSARY.
C 
            DO 110 K=1,NVAL
C
            IF(NINT(XDATA(K)).NE.9999)THEN
               XDATA(K)=XDATA(K)*FACTOR
            ENDIF
C
 110        CONTINUE
C
         ENDIF
C
      ELSE
C
C           FULL TREATMENT NECESSARY.
C
         DO 120 K=1,NVAL
C
         IF(NINT(XDATA(K)).NE.9999)THEN
C
            IF(XDATA(K).LT.TLO)THEN
               XDATA(K)=SETLOF
            ELSEIF(XDATA(K).GT.THI)THEN
               XDATA(K)=SETHIF
            ELSE
               XDATA(K)=XDATA(K)*FACTOR
            ENDIF
C
         ENDIF
C
 120     CONTINUE
C
      ENDIF
C
CD     WRITE(KFILDO,125)(XDATA(K),K=1,NVAL)
CD125  FORMAT(/,' IN POST--XDATA(K)',/,(15F8.2))
CD     CALL TIMPR(KFILDO,KFILDO,'END   POST          ')
C
 150  RETURN
      END
