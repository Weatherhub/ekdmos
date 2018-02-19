      SUBROUTINE SKPWR1(KFILDO,KFILIO,KSKIP,KWRITE,KCHECK,
     1                  CCALL,ND1,NSTA,
     2                  CCALLD,NDX,IPACK,ND5,
     3                  NTOTBY,NTOTRC,L3264B,L3264W,IER)
C
C        MAY       2006   GLAHN   MOS-2000
C                                 ADAPTED FROM SKIPWR;  CALL 
C                                 SEQUENCE LEFT INTACT, ALTHOUGH
C                                 SOME VARIABLES NOT USED
C
C        PURPOSE
C           TO SKIP RECORDS ON A VECTOR FILE.  CALL LETTERS AND
C           TRAILER RECORDS ARE READ WHEN THEY EXIST, AND ALL DATA
C           FOR THE DATE KSKIP ARE BYPASSED.  IF SOME DATA IN A 
C           PSEUDO FILE (THE DATA INCLUDING A DIRECTORY, DATA, AND
C           TRAILER) IS SKIPPED, BUT NOT ALL OF IT, A TRAILER IS
C           WRITTEN SO THAT THE END OF THE GOOD DATA IS ALWAYS
C           FOLLOWED BY A TRAILER.  THE CALL SEQUENCE USED IN SKIPWR
C           WAS LEFT INTACT, ALTHOUGH KWRITE AND KCHECK ARE NOT
C           USED.      
C
C        COMMENT
C           UNUSUAL CONDITIONS CAUSE A **** DIAGNOSTIC.  USUAL
C           CONDITIONS CAUSE A **** DIAGNOSTIC ONLY WITH /D COMPILE.
C   
C        DATA SET USE
C            KFILDO    - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (OUTPUT)
C            KFILIO    - UNIT NUMBER OF OUTPUT FILE.  (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C              KFILIO = UNIT NUMBER OF OUTPUT FILE.  IF KFILIO = 0,
C                       THIS IS A DO NOTHING ROUTINE.  (INPUT)
C               KSKIP = 0 IF THE CALL LETTERS RECORD IS TO BE WRITTEN
C                       AS THE FIRST RECORD ON THE FILE.
C                       NE 0 INDICATES THAT THE OUTPUT FILE
C                       IS TO BE MOVED FORWARD UNTIL ALL DATA FOR
C                       DATE/TIME KSKIP HAVE BEEN SKIPPED.  KSKIP IS 
C                       INPUT AS YYYYMMDDHH.  (INPUT)
C              KWRITE = NOT USED.  (INPUT)
C              KCHECK = NOT USED.  (INPUT)
C          CCALL(K,J) = 8 STATION CALL LETTERS (K=1,NSTA).  THE PRIMARY
C                       STATIONS ARE IN CCALL( ,1), BUT ANY SUBSTITUTE
C                       STATION IN J=2,5 WILL BE ACCEPTED.
C                       (CHARACTER*8)  (INPUT)
C                 ND1 = 1ST DIMENSION OF CCALL( , ).  (INPUT)
C                NSTA = THE NUMBER OF CALL LETTERS IN CCALL( , ).  (INPUT)
C           CCALLD(K) = 8 STATION CALL LETTERS (K=1,NDX).  THIS VARIABLE 
C                       HOLDS THE LIST OF STATIONS ALREADY ON THE OUTPUT
C                       FILE WHEN DATA ARE SKIPPED ON IT.  IT IS USED
C                       TO CHECK THE LIST TO WRITE WHEN DESIRED.  WHEN
C                       THE LISTS DO NOT AGREE, THE PROGRAM RESPONDS
C                       TO KWRITE.  (CHARACTER*8)  (INTERNAL)
C                 NDX = DIMENSION OF CCALLD( ).  THIS MAY BE ND1, ND5, OR
C                       SOME OTHER VALUE IN CALLING PROGRAMS.  GENERALLY,
C                       PROGRAMS WILL HAVE CCALLD( ), IPACK( ) AND 
C                       OTHER VARIABLES DIMENSIONED ND5.  (INPUT)
C            IPACK(J) = WORK ARRAY (J=1,ND5).  (INTERNAL)
C                 ND5 = DIMENSION OF IPACK( ).  (INPUT)
C              NTOTBY = THE TOTAL NUMBER OF BYTES ON THE FILE.  IT IS
C                       UPDATED WHEN SKIPPING RECORDS AND WRITING CALL
C                       LETTERS RECORDS.  (INPUT/OUTPUT)
C              NTOTRC = THE TOTAL NUMBER OF RECORDS ON THE FILE.  IT IS
C                       UPDATED WHEN SKIPPING RECORDS AND WRITING CALL
C                       LETTERS RECORDS.  (INPUT/OUTPUT)
C              L3264B = WORD LENGTH IN BITS OF MACHINE BEING USED.
C                       (INPUT)
C              L3264W = THE NUMBER OF 32-BIT "WORDS" TO CONTAIN 64 BITS.
C                       2 FOR 32-BIT MACHINE, 1 FOR 64-BIT MACHINE.
C                 IER = STATUS RETURN.
C                         0 = GOOD RETURN.
C                       140 = ERROR READING CALL LETTERS RECORD.
C                       141 = CCALLD( ) ARRAY NOT LARGE ENOUGH.
C                       142 = CALL LETTERS READ FROM FILE DO NOT MATCH
C                             CCALL( , ).
C                       143 = ERROR WHEN SKIPPING RECORDS.
C                       144 = ERROR WRITING CALL LETTERS RECORD.
C                       147 = IMPOSSIBLE SITUATION.
C                       148 = ND5 TOO SMALL.
C                       199 = ERROR IN TRAIL.
C                       OTHER RETURNS FROM UNPKBG OR PKBG.
C                       (OUTPUT)
C           NBYTES(L) = USED TO ACCESS DATA AS EITHER 2 WORDS FOR A 32-BIT
C                       MACHINE OR 1 WORD FOR A 64-BIT MACHINE (L=1,2).
C                       (INTERNAL)
C               IDATE = DATE/TIME OF RECORD READ.  (INTERNAL)
C               NSTA1 = NUMBER OF STATIONS IN THE LIST READ FROM THE
C                       OLD OUTPUT FILE.  (INTERNAL)
C              BLANK8 = 8 BLANKS.  USED TO BLANK OUT ARRAY SO IF
C                       THE LISTS DON'T MATCH AND ARE PRINTED, THE
C                       CHARACTERS WILL BE PRINTABLE.  (CHARACTER*8)
C                       (INTERNAL)
C              NTOTCL = COUNTS NUMBER OF CALL LETTERS RECORDS READ. 
C                       (INTERNAL)
C              ISHORT = THE NUMBER OF WORDS IN A TRAILER RECORD,
C                       FOLLOWING THE INITIAL 8 BYTES.  (INTERNAL)
C                IWRT = 0 WHEN CALL LETTERS RECORD IS CHECKED AND
C                         AGREES WITH THE ONE PROVIDED IN CCALL( , ).
C                       1 WHEN THERE IS NOT AGREEMENT, OR DIRECTORY
C                         IS TO BE WRITTEN.
C                       (INTERNAL)
C           CCALLX(K) = 8 STATION CALL LETTERS (K=1,NDX).  THIS VARIABLE 
C                       HOLDS THE PREVIOUSLY READ LIST OF STATIONS
C                       ALREADY ON THE OUTPUT FILE WHEN DATA ARE SKIPPED 
C                       ON IT.  IT IS USED TO CHECK THE LIST TO 
C                       WRITE WHEN DESIRED.  WHEN THE LISTS DO NOT
C                       AGREE, THE PROGRAM RESPONDS TO KWRITE.  THIS
C                       VARIABLE, IN ADDITION TO CCALLD( ), IS NECESSARY
C                       TO KEEP A DIRECTORY AND TRAILER TO BE KEPT
C                       WITH NO DATA BETWEEN.  THIS MIGHT HAPPEN WITH
C                       MULTIPLE RUNS WHERE THE SAME KSKIP WERE USED WITH
C                       KWRITE = 1 AND THE STATIONS IN CCALL( , )
C                       DIFFERENT ON THE RUNS.
C                       (CHARACTER*8)  (AUTOMATIC-INTERNAL)
C               NSTAP = THE NUMBER OF CALL LETTERS IN CCALLX( ).
C                       (INTERNAL)
C                LAST = 9999 WHEN THE LAST RECORD READ WAS A TRAILER.
C                       0 OTHERWISE.  (INTERNAL)
C
C        NONSYSTEM SUBROUTINES USED 
C            UNPKBG, TRAIL
C
      CHARACTER*8 CCALL(ND1,6)
      CHARACTER*8 CCALLD(NDX)
      CHARACTER*8 BLANK8
      CHARACTER*8 CCALLX(NDX)
C        CCALLX( ) IS AN AUTOMATIC ARRAY.
C
      DIMENSION IPACK(ND5)
      DIMENSION NBYTES(2)
C
      IER=0
C
C        SET NBYTES( ) = 0 AND COUNT RECORDS.
C
      NBYTES(1)=0
      NBYTES(2)=0
C
C        INITIALIZE NTOTCL AND ISHORT.
C
      NTOTCL=0
C        NTOTCL IS THE TOTAL NUMBER OF CALL LETTERS RECORDS.
      ISHORT=192/L3264B
C        ISHORT IS THE NUMBER OF MACHINE WORDS IN A TRAILER,
C        AFTER THE INITIAL 8 BYTES.  IT IS ALSO THE NUMBER OF
C        BYTES THAT MUST BE READ TO FIND THE DATE IN SKIPPING
C        RECORDS.
C
      IF(ISHORT.GT.ND5)THEN
         WRITE(KFILDO,101)ND5,ISHORT
 101     FORMAT(/,' ****ND5 =',I4,' TOO SMALL IN SKPWR1.',
     1            '  INCREASE TO GE',I4)
         IER=148
         GO TO 300
      ENDIF
C
      LAST=0
      NSTA1=0
      JBYTES=0
C
      IF(KFILIO.EQ.0)GO TO 300
      IF(KSKIP.EQ.0)GO TO 222
C
C        INITIALIZE CCALLD( ).
C
      DO 102 K=1,NDX
      CCALLD(K)=BLANK8
 102  CONTINUE
C
C        THIS SECTION FOR READING CALL LETTERS RECORD.
C        FIRST, TRANSFER CALL LETTERS IN CCALLD( ) TO
C        CCALLX( ) AND BLANK OUT CCALLD( ).
C
 105  DO 106 K=1,NDX
      CCALLX(K)=CCALLD(K)
      CCALLD(K)=BLANK8
 106  CONTINUE
C
      NSTAP=NSTA1
C        NSTAP IS THE NUMBER OF CALL LETTERS IN CCALLX( ).
      READ(KFILIO,IOSTAT=IOS,ERR=110,END=115)(NBYTES(J),J=1,L3264W),
     1                   (CCALLD(K),K=1,MIN(NDX,NBYTES(L3264W)/8))
      ICOUNT=0
C         ICOUNT WILL COUNT THE NUMBER OF DATA RECORDS TO KEEP
C         FOLLOWING THIS DIRECTORY RECORD.
      NTOTRC=NTOTRC+1
C        NTOTRC IS THE TOTAL NUMBER OF DIRECTORY RECORDS READ.
      NTOTCL=NTOTCL+1
C        NTOTCL IS THE TOTAL NUMBER OF RECORDS READ.
      LSTCLB=NBYTES(L3264W)+8
C        LSTCLB IS THE BYTES IN THIS DIRECTORY, SAVED IN CASE
C        IT HAS TO BE BACKSPACED OVER.
      NTOTBY=NTOTBY+LSTCLB
      GO TO 120
C
 110  NTOTP1=NTOTCL+1
      WRITE(KFILDO,111)NTOTP1,IOS
 111  FORMAT(/,' ****ERROR READING CALL LETTERS RECORD, NO. ',I5,
     1         ' IN SKPWR1 AT 110, IOSTAT =',I5)
      IER=140
      GO TO 300
C
C     *********************************************************
C
C        THIS SECTION FOR AN END OF FILE WHEN READING DIRECTORY.
C
C        THIS CAN HAPPEN ONLY AT THE BEGINNING OR FOLLOWING A 
C        TRAILER.
C
C     *********************************************************
C
 115  IF(NTOTRC.EQ.0)THEN
         WRITE(KFILDO,116)KFILIO
 116     FORMAT(/,' THIS IS A NEW DATA SET ON UNIT ',I2,
     1            '.  START WRITING AT THE BEGINNING.')
         REWIND KFILIO
         IWRT=1
         GO TO 222
C
      ELSE
         BACKSPACE KFILIO
C           IN ORDER TO WRITE, MUST BACKSPACE OVER THE EOF.
C
         IF(LAST.NE.9999)THEN
C              THE LAST RECORD SHOULD HAVE BEEN A TRAILER.
D           WRITE(KFILDO,1165)KFILIO,KSKIP
D1165       FORMAT(/,' ****SKIPPED ALL DATA ON UNIT ',I2,
D    1               ' LOOKING FOR DATE/TIME',I12)   
            WRITE(KFILDO,117)
C              THIS SHOULD BE AN IMPOSSIBLE SITUATION.
 117        FORMAT(/,' ****CODE ERROR IN SKPWR1 AT 117.')
            IER=147
            GO TO 300
         ENDIF             
C
C           REINITIALIZE CCALLD( ).
C
         DO 118 K=1,NDX
         CCALLD(K)=CCALLX(K)
 118     CONTINUE
C
         NSTA1=NSTAP
C***D        WRITE(KFILDO,119)LAST,NTOTRC,NTOTBY,(CCALL(K,1),CCALLD(K),
C***D    1                    K=1,MIN(NDX,ND1))
C***D119     FORMAT(/,' LAST,NTOTRC,NTOTBY,CCALL(K,1),CCALLD(K)',
C***D    1           3I8,/,(' ',2A8))
C           THE ABOVE MAY PRINT A LARGE NUMBER OF BLANKS.
         GO TO 200
C
      ENDIF
C
C        **********************************************************
C
C        GOOD READ.  CHECK FOR SUFFICIENCY OF ARRAY SIZE. 
C        WITH THE READ ABOVE, OVERFLOW WILL NOT OCCUR.
C
 120  NSTA1=NBYTES(L3264W)/8
      IF(NDX.GE.NSTA1)GO TO 150
      WRITE(KFILDO,121)NDX,NSTA1
 121  FORMAT(/,' ****ARRAY CCALLD( ) NOT LARGE ENOUGH TO HOLD CALL',
     1         ' LETTERS IN FILE.',/,
     2         '     INCREASE NDX IN SKPWR1 FROM',I5,' TO GE',I5)
      IER=141
      GO TO 300
C
C        READ PACKED RECORD.  THE CALL LETTERS READ ARE RETAINED
C        IN CCALLD( ) FOR LATER CHECKING AS NECESSARY.  ONLY
C        THE LAST CALL LETTERS RECORD READ NEED BE CHECKED.
C
 150  READ(KFILIO,IOSTAT=IOS,ERR=151,END=155)(NBYTES(J),J=1,L3264W),
     1                                       (IPACK(J),J=1,ISHORT)
      NTOTRC=NTOTRC+1
      JBYTES=NBYTES(L3264W)+8
      NTOTBY=NTOTBY+JBYTES
      GO TO 160
C
C        ERROR READING FILE WHEN SKIPPING RECORDS.
C
 151  WRITE(KFILDO,152)KSKIP,IOS
 152  FORMAT(/,' ****ERROR IN SKPWR1 AT 151 WHEN SKIPPING RECORDS',
     1         ' ON OUTPUT FILE WHILE LOOKING FOR DATE KSKIP =',
     2       I12,'.',/,'     IOSTAT =',I5,'.')
C        IF PROCESSING WERE TO CONTINUE, THE OUTPUT FILE MIGHT NOT
C        BE POSITIONED PROPERLY AND DATA WOULD BE OVERWRITTEN.
      IER=143
      GO TO 300
C
C     *********************************************************
C
C        THIS SECTION FOR AN END OF FILE WHEN SKIPPING RECORDS.
C
C     *********************************************************
C
C        END OF FILE ENCOUNTERED WHEN SKIPPING RECORDS.  THIS MAY BE OK.
C        THAT IS, THE DATE TO BE SKIPPED MAY BE THE LAST ONE ON THE FILE.
C
 155  BACKSPACE KFILIO
C        THIS BACKSPACES OVER THE EOF.
C
      IF(ICOUNT.EQ.0)THEN
C           THERE WERE NO DATA AFTER THE LAST DIRECTORY.  THIS IS
C           NOT NORMAL.
C
         IF(NTOTCL.EQ.1)THEN
C              THERE WAS ONLY ONE DIRECTORY RECORD AND NO DATA READ.
C              START WRITING AT THE BEGINNING.
            WRITE(KFILDO,156)KFILIO,KSKIP
 156        FORMAT(/,' ****EOF FOUND.  NO DATA TO SKIP ON UNIT ',I2,
     1              ' FOLLOWING A SINGLE DIRECTORY WHILE LOOKING FOR',
     2              ' DATE/TIME',I12,'.',/,
     3              '     TREATING AS A NEW FILE.')
            REWIND KFILIO
            IWRT=1
            NTOTRC=0
            NTOTCL=0
            NTOTBY=0
            GO TO 222
         ELSE
            WRITE(KFILDO,157)KFILIO,KSKIP
 157        FORMAT(/,' ****EOF FOUND.  NO DATA TO SKIP ON UNIT ',I2,
     1               ' AFTER THE LAST DIRECTORY WHILE LOOKING FOR',
     2               ' DATE/TIME',I12,'.',/,
     3               '     CHECKING THE PREVIOUS DIRECTORY.')
            BACKSPACE KFILIO
C              BACKSPACE OVER DIRECTORY.
            NTOTBY=NTOTBY-LSTCLB
            NTOTRC=NTOTRC-1
            NTOTCL=NTOTCL-1
C
C              TRANSFER THE PREVIOUS DIRECTORY INTO CCALLD( ).
C
            DO 158 K=1,NDX
            CCALLD(K)=CCALLX(K)
 158        CONTINUE
C
            NSTA1=NSTAP
            GO TO 200
         ENDIF
C
      ELSE
         IF(LAST.EQ.9999)THEN
D           WRITE(KFILDO,1165)KFILIO,KSKIP
         ENDIF
C              THIS WOULD HAPPEN ONLY ON UNSUCCESSFUL COMPLETION
C              OF PREVIOUS RUN.  THE TRAILER SHOULD ALWAYS HAVE
C              BEEN WRITTEN.
            WRITE(KFILDO,159)KFILIO
 159        FORMAT(/,' ****NO TRAILER AFTER DATA ON UNIT ',I2,
     1               '.  PREVIOUS RUN MUST HAVE NOT COMPLETED',
     2               ' SUCCESSFULLY.  BETTER RERUN THE RUN',
     3               ' BEING SKIPPED.')
            IER=143
            GO TO 200
      ENDIF
C
      GO TO 200
C
C     *********************************************************
C
 160  IF(L3264B.EQ.32)THEN
         IDATE=IPACK(5)
C           IPACK(5) HOLDS THE DATE/TIME OF THE RECORD FOR A 32-BIT
C           MACHINE.
      ELSE
         LOC=3
         IPOS=1
         CALL UNPKBG(KFILDO,IPACK,ND5,LOC,IPOS,IDATE,32,L3264B,IER,*170)
C           THE LEFT HALF OF IPACK(3) HOLDS THE DATE/TIME OF THE RECORD
C           FOR A 64-BIT MACHINE.
      ENDIF
C
      GO TO 180
C
 170  WRITE(KFILDO,171)
 171  FORMAT(/,' ****ERROR IN UNPKBG IN SKPWR1 AT 171.')
      GO TO 300
C
 180  CONTINUE
C
D     WRITE(KFILDO,181)KSKIP,IDATE,NTOTRC,NTOTCL,NTOTBY,
D    1                 ICOUNT,NSTA,NSTA1,NSTAP,LAST
D181  FORMAT(' KSKIP,IDATE,NTOTRC,NTOTCL,NTOTBY,',
D    1       'ICOUNT,NSTA,NSTA1,NSTAP,LAST',2I11,8I6)
C
      LAST=0
      IF(IDATE.EQ.9999)THEN
C           WHEN THE ABOVE TEST IS MET, THE RECORD JUST READ WAS A
C           TRAILER, AND ANOTHER CALL LETTERS RECORD MAY FOLLOW.
         LAST=9999
         GO TO 105
      ENDIF
C 
C        PROCEED WHEN THIS IS NOT A TRAILER.
C
      IF(IDATE.LE.KSKIP)THEN
         ICOUNT=ICOUNT+1
C           ICOUNT GT 0 INDICATES DATA RECORDS TO BE SKIPPED
C           FOLLOWING LAST DIRECTORY RECORD.
         GO TO 150
C
C     *********************************************************
C
C        THIS SECTION FOR A GOOD RECORD (NON TRAILER) READ PAST
C        THE RECORD TO SKIP.
C
C     *********************************************************
C
      ELSE
         IF(ICOUNT.EQ.0)THEN
C              WHEN ICOUNT = 0, A DIRECTORY WAS READ BUT NO DATA
C              RECORDS TO SKIP FOLLOWED.
C
            IF(NTOTCL.EQ.1)THEN
C                 THERE WAS NO PREVIOUS DIRECTORY.  REWIND.
               WRITE(KFILDO,184)KFILIO,KSKIP
 184           FORMAT(/,' ****NO DATA TO SKIP ON UNIT ',I2,
     1                  ' FOLLOWING A SINGLE DIRECTORY LOOKING',
     2                  ' FOR DATE/TIME',I12,'.',
     3                  '  TREATING AS A NEW FILE.')

               REWIND KFILIO
               IWRT=1
               NTOTRC=0
               NTOTCL=0
               NTOTBY=0
               GO TO 222
C
            ELSE
C                 THERE WAS A PREVIOUS DIRECTORY.
               WRITE(KFILDO,185)KFILIO,KSKIP
 185           FORMAT(/,' ****NO DATA TO SKIP ON UNIT ',I2,
     1                  ' AFTER THE LAST DIRECTORY LOOKING FOR',
     2                  ' DATE/TIME',I12,'.')
C
               DO 186 K=1,NDX
               CCALLD(K)=CCALLX(K)
 186           CONTINUE
C
               NSTA1=NSTAP
               BACKSPACE KFILIO
C                 BACKSPACE OVER RECORD JUST READ.
               NTOTBY=NTOTBY-JBYTES
               BACKSPACE KFILIO
C                 BACKSPACE OVER THE LAST DIRECTORY.
               NTOTBY=NTOTBY-LSTCLB
               NTOTRC=NTOTRC-2
               NTOTCL=NTOTCL-1
            ENDIF
C
         ELSE
D           WRITE(KFILDO,167)KFILIO,KSKIP
D167        FORMAT(/,' ****SUCCESSFUL SKIP ON UNIT ',I2,
D    1               ' PAST DATE/TIME',I12)
            BACKSPACE KFILIO
C              MUST BACKSPACE OVER THE RECORD JUST READ.
            NTOTRC=NTOTRC-1
            NTOTBY=NTOTBY-JBYTES
C
C              SOME DATA EXISTS AFTER THE DIRECTORY.  ADD A TRAILER.
C
            CALL TRAIL(KFILDO,KFILIO,L3264B,L3264W,NTOTBY,NTOTRC,IER)
C              TRAIL UPDATES NTOTBY AND NTOTRC.
            IF(IER.NE.0)IER=199
         ENDIF
C
      ENDIF
C
 200  CONTINUE
 222  CONTINUE
C
C        DOCUMENT RESULTS.
C
      WRITE(KFILDO,235)KSKIP,NTOTCL,NTOTRC,KFILIO,NTOTBY
 235  FORMAT(/,' RECORDS PREVIOUS TO AND INCLUDING DATE/TIME',I12,
     1         ' SKIPPED ON OUTPUT FILE.',/,
     2         '    NUMBER OF CALL LETTERS RECORDS SKIPPED =   ',I11,/,
     3         '    NUMBER OF RECORDS SKIPPED =                ',I11,/,
     4         '    TOTAL BYTES IN FILE UNIT NO.',I4,' =         ',I11)
C
300   RETURN
      END
