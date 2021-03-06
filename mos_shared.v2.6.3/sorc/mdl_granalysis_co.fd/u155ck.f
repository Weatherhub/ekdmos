      SUBROUTINE U155CK(KFILDO,KFILIO,KFILDI,KFILIN,NUMIN,
     1                  KFILD,KFILRA,NUMRA,IP,IPIN,KFIL10,
     2                  KFILVO,KFILOG,KFILAN)
C
C        JUNE      2004   GLAHN   MDL MOS-2000
C                                 ADAPTED FROM U600CK 
C        OCTOBER   2004   GLAHN   ADDED SOME CHECKS 
C        SEPTEMBER 2005   GLAHN   ADDED A FEW COMMAS PER TRIMARCO 
C        NOVEMBER  2007   GLAHN   CORRECTED KFILOG TO KFILVO IN
C                                 DO 220 LOOP
C
C        PURPOSE
C            U155CK CHECKS INCONSISTENCIES OF UNIT NUMBERS FOR U155.
C********************************************************************
C            NOTE THAT KFILAN( ) IN CALLING PROGRAM IS NOT DIMENSIONED 
C            HERE, SO ONLY KFILAN(1) IS CHECKED.  THE DIMENSIONING WAS
C            ADDED FOR THE IBM.  IF CHECKING FOR THE IBM IS NECESSARY,
C            IT CAN BE ADDED.
C********************************************************************
C   
C        DATA SET USE
C               KFILDO - UNIT NUMBER OF OUTPUT (PRINT) FILE.  (INPUT)
C
C        VARIABLES
C               KFILDO = UNIT NUMBER OF OUTPUT (PRINT) FILE.
C                        (INPUT)
C               KFILIO = UNIT NUMBER FOR PRIMARY GRIDDED OUTPUT.
C                        (INPUT)
C               KFILDI = UNIT NUMBER OF INPUT FILE.  SET BY DATA
C                        STATEMENT.  (INPUT)  
C            KFILIN(J) = UNIT NUMBERS OF TDLPACK INPUT (J=1,NUMIN).
C                        (INPUT)
C                NUMIN = NUMBER OF VALUES IN KFILIN( ).  (INPUT)
C             KFILD(J) = UNIT NUMBER FOR STATION LIST AND DIRECTORY
C                        (J=1,2).  (INPUT)
C            KFILRA(J) = UNIT NUMBERS FOR READING RANDOM ACCESS DATA
C                        (J=1,NUMRA).  (INPUT)
C                NUMRA = NUMBER OF VALUES IN KFILRA( ).  (INPUT)
C                IP(J) = UNIT NUMBERS FOR OPTIONAL OUTPUT (J=1,IPIN)  
C                        (INPUT)
C                 IPIN = NUMBER OF VALUES IN IP( ).  (INPUT)
C               KFIL10 = UNIT NUMBER FOR INTERNAL MOS-2000 STORAGE.
C                        (INPUT)
C               KFILVO = UNIT NUMBER FOR GRIDDED DATA IN VECTOR FORMAT.
C                        (INPUT)
C               KFILOG = UNIT NUMBER FOR DISPOSABLE GIIDDED OUTPUT.
C                        (INPUT)
C               KFILAN = UNIT NUMBER FOR READING THE INDIVIDUAL .CN
C                        ANALYSIS FILES.  (INPUT)
C        1         2         3         4         5         6         7 X
C 
C        NONSYSTEM SUBROUTINES USED 
C            NONE
C
      DIMENSION IP(IPIN),KFILRA(NUMRA),KFILIN(NUMIN),KFILD(2)
C
C        CHECK POSSIBLE INCONSISTENCY OF INPUT UNIT NUMBERS WITH
C        OTHERS USED BY THE PROGRAM.
C   
      DO 120 J=1,NUMIN
      DO 119 L=1,NUMRA
C
      IF(KFILIN(J).EQ.KFILRA(L))THEN
         WRITE(KFILDO,115)
 115     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBERS IN', 
     1           ' KFILIN( ) WITH KFILRA( )')
         WRITE(KFILDO,116)(KFILIN(M),M=1,NUMIN)
 116     FORMAT('     KFILIN =',38I3/('             ',38I3))
         WRITE(KFILDO,117)(KFILRA(M),M=1,NUMRA)
 117     FORMAT('     KFILRA =',38I3)
         WRITE(KFILDO,118)
 118     FORMAT('     STOP IN U155CK AT 118')
         STOP 118
      ENDIF
C   
 119  CONTINUE
 120  CONTINUE   
C
      DO 140 J=1,NUMIN
      DO 139 L=1,IPIN
C
      IF(KFILIN(J).EQ.IP(L))THEN
         WRITE(KFILDO,135)
 135     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBERS IN', 
     1           ' KFILIN( ) WITH IP( )')
         WRITE(KFILDO,136)(KFILIN(M),M=1,NUMIN)
 136     FORMAT('     KFILIN =',38I3/('             ',38I3))
         WRITE(KFILDO,137)(IP(M),M=1,IPIN)
 137     FORMAT('     IP     =',38I3)
         WRITE(KFILDO,138)
 138     FORMAT('     STOP IN U155CK AT 138')
         STOP 138
      ENDIF
C   
 139  CONTINUE
 140  CONTINUE   
C
      DO 160 J=1,NUMRA
      DO 159 L=1,IPIN
C
      IF(KFILRA(J).EQ.IP(L))THEN
         WRITE(KFILDO,155)
 155     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBERS IN', 
     1           ' KFILRA( ) WITH IP( )')
         WRITE(KFILDO,156)(KFILRA(M),M=1,NUMRA)
 156     FORMAT('     KFILRA =',38I3)
         WRITE(KFILDO,157)(IP(M),M=1,IPIN)
 157     FORMAT('     IP     =',38I3)
         WRITE(KFILDO,158)
 158     FORMAT('     STOP IN U155CK AT 158')
         STOP 158
      ENDIF
C   
 159  CONTINUE
 160  CONTINUE 
C
      DO 180 J=1,NUMIN
C
      IF(KFILIN(J).EQ.KFILIO.OR.
     1   KFILIN(J).EQ.KFILDI.OR.
     2   KFILIN(J).EQ.KFILDO.OR.
     3   KFILIN(J).EQ.KFIL10.OR.
     4   KFILIN(J).EQ.KFILVO.OR.
     5   KFILIN(J).EQ.KFILOG.OR.
     6   KFILIN(J).EQ.KFILAN)THEN
         WRITE(KFILDO,175)
 175     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBERS IN', 
     1           ' KFILIN( ) WITH',
     2           ' KFILIO, KFILDI, KFILDO,',
     3           ' KFIL10, KFILVO, KFILOG, OR KFILAN')
         WRITE(KFILDO,176)(KFILIN(M),M=1,NUMIN)
 176     FORMAT('     KFILIN =',38I3/('             ',38I3))
         WRITE(KFILDO,177)KFILIO,KFILDI,KFILDO,
     1                    KFIL10,KFILVO,KFILOG,KFILAN
 177     FORMAT('     KFILIO, KFILDI, KFILDO,',
     1          ' KFIL10, KFILVO, KFILOG, KFILAN = ',7I3)
         WRITE(KFILDO,178)
 178     FORMAT('     STOP IN U155CK AT 178')
         STOP 178
      ENDIF
C   
 180  CONTINUE   
C
      DO 200 J=1,NUMRA
C
      IF(KFILRA(J).EQ.KFILIO.OR.
     1   KFILRA(J).EQ.KFILDI.OR.
     2   KFILRA(J).EQ.KFILDO.OR.
     3   KFILRA(J).EQ.KFIL10.OR.
     4   KFILRA(J).EQ.KFILVO.OR.
     5   KFILRA(J).EQ.KFILOG.OR.
     6   KFILRA(J).EQ.KFILAN)THEN
         WRITE(KFILDO,195)
 195     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBERS IN', 
     1           ' KFILRA( ) WITH',
     2           ' KFILIO, KFILDI, KFILDO,',
     3           ' KFIL10, KFILVO, KFILOG, OR KFILAN')
         WRITE(KFILDO,196)(KFILRA(M),M=1,NUMRA)
 196     FORMAT('     KFILRA =',38I3)
         WRITE(KFILDO,197)KFILIO,KFILDI,KFILDO,
     1                    KFIL10,KFILVO,KFILOG,KFILAN
 197     FORMAT('     KFILIO, KFILDI, KFILDO,',
     1          ' KFIL10, KFILVO, KFILOG, KFILAN = ',7I3)
         WRITE(KFILDO,198)
 198     FORMAT('     STOP IN U155CK AT 198')
         STOP 198
      ENDIF
C   
 200  CONTINUE 
C
      DO 220 J=1,IPIN
C
      IF((IP(J).EQ.KFILIO.AND.KFILIO.NE.0).OR.
     1   IP(J).EQ.KFILDI.OR.
     2   IP(J).EQ.KFIL10.OR.
     3   (IP(J).EQ.KFILVO.AND.KFILVO.NE.0).OR.
     4   (IP(J).EQ.KFILAN.AND.KFILAN.NE.0).OR.
     5   (IP(J).EQ.KFILOG.AND.KFILOG.NE.0))THEN
C           NOTE THAT IP( ) = KFILDO IS ALLOWED.  ALSO, BOTH
C           IP( ) AND KFILOG, KFILVO, AND KFILAN CAN BE ZERO.
         WRITE(KFILDO,215)
 215     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBERS IN', 
     1           ' IP( ) WITH',
     2           ' KFILIO, KFILDI,',
     3           ' KFIL10, KFILVO, KFILOG, OR KFILAN')
         WRITE(KFILDO,216)(IP(M),M=1,IPIN)
 216     FORMAT('     IP     =',38I3)
         WRITE(KFILDO,217)KFILIO,KFILDI,
     1                    KFIL10,KFILVO,KFILOG,KFILAN
 217     FORMAT('     KFILIO, KFILDI,',
     1          ' KFIL10, KFILVO, KFILOG, KFILAN =   ',6I3)
         WRITE(KFILDO,218)
 218     FORMAT('     STOP IN U155CK AT 218')
         STOP 218
      ENDIF
C   
 220  CONTINUE 
C
C
      IF(KFILIO.NE.0.AND.
     1  (KFILIO.EQ.KFILDI.OR.
     1   KFILIO.EQ.KFILDO.OR.
     2   KFILIO.EQ.KFIL10.OR.
     3   KFILIO.EQ.KFILVO.OR.
     4   KFILIO.EQ.KFILOG.OR.
     5   KFILIO.EQ.KFILAN))THEN
         WRITE(KFILDO,225)
 225     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBER', 
     1           ' KFILIO WITH',
     2           ' KFILDI, KFILDO,',
     3           ' KFIL10, KFILVO, KFILOG, OR KFILAN')
         WRITE(KFILDO,226)KFILIO
 226     FORMAT('     KFILIO =',I3)
         WRITE(KFILDO,227)KFILDI,KFILDO,
     1                    KFIL10,KFILVO,KFILOG,KFILAN
 227     FORMAT('     KFILDI, KFILDO,',
     1          ' KFIL10, KFILVO, KFILOG, KFILAN = ',6I3)
         WRITE(KFILDO,228)
 228     FORMAT('     STOP IN U155CK AT 228')
         STOP 228
      ENDIF
C
      IF(KFILDI.EQ.KFILDO.OR.
     1   KFILDI.EQ.KFIL10.OR.
     2   KFILDI.EQ.KFILVO.OR.
     3   KFILDI.EQ.KFILOG)THEN
C           NOTE THAT KFILDI AND KFILAN CAN BE THE SAME, 
C           BECAUSE ALL OTHER USE OF KFILDI HAS COMPLETED
C           WHEN THE INDIVIDUAL .CN FILES ARE READ.
         WRITE(KFILDO,235)
 235     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBER', 
     1           ' KFILDI WITH',
     2           ' KFILDO,',
     3           ' KFIL10, KFILVO, KFILOG, OR KFILAN')
         WRITE(KFILDO,236)KFILDI
 236     FORMAT('     KFILDI =',I3)
         WRITE(KFILDO,237)KFILDO,KFIL10,KFILVO,KFILOG,KFILAN
 237     FORMAT('     KFILDO,',
     1          ' KFIL10, KFILVO, KFILOG, KFILAN = ',5I3)
         WRITE(KFILDO,238)
 238     FORMAT('     STOP IN U155CK AT 238')
         STOP 238
      ENDIF
C
      IF(KFILDO.EQ.KFIL10.OR.
     1   KFILDO.EQ.KFILVO.OR.
     2   KFILDO.EQ.KFILOG.OR.
     3   KFILDO.EQ.KFILAN)THEN
         WRITE(KFILDO,245)
 245     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBER', 
     1           ' KFILDO WITH',
     2           ' KFIL10, KFILVO, KFILOG, OR KFILAN')
         WRITE(KFILDO,246)KFILDO
 246     FORMAT('     KFILDO =',I3)
         WRITE(KFILDO,247) KFIL10,KFILVO,KFILOG,KFILAN
 247     FORMAT('     KFIL10, KFILVO, KFILOG, KFILAN = ',4I3)
         WRITE(KFILDO,248)
 248     FORMAT('     STOP IN U155CK AT 248')
         STOP 248
      ENDIF
C
      IF(KFIL10.EQ.KFILVO.OR.
     1   KFIL10.EQ.KFILOG.OR.
     2   KFIL10.EQ.KFILAN)THEN
         WRITE(KFILDO,265)
 265     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBER', 
     1           ' KFIL10 WITH KFILVO, KFILOG, OR KFILAN')
         WRITE(KFILDO,266)KFIL10
 266     FORMAT('     KFIL10 =',I3)
         WRITE(KFILDO,267)KFILVO,KFILOG,KFILAN
 267     FORMAT('     KFILVO, KFILOG, KFILAN = ',3I3)
         WRITE(KFILDO,268)
 268     FORMAT('     STOP IN U155CK AT 268')
         STOP 268
      ENDIF
C
      IF(((KFILVO.NE.0.OR.KFILOG.NE.0).AND.KFILVO.EQ.KFILOG).OR.
     1   KFILVO.EQ.KFILAN)THEN
C           KFILVO AND KFILOG CAN BE ZERO, BUT NOT KFILAN.
         WRITE(KFILDO,275)
 275     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBER', 
     1           ' KFILVO WITH KFILOG OR KFILAN')
         WRITE(KFILDO,276)KFILVO
 276     FORMAT('     KFILVO =',I3)
         WRITE(KFILDO,277)KFILOG,KFILAN
 277     FORMAT('     KFILOG, KFILAN = ',2I3)
         WRITE(KFILDO,278)
 278     FORMAT('     STOP IN U155CK AT 278')
         STOP 278
      ENDIF
C
      IF(KFILOG.EQ.KFILAN)THEN
         WRITE(KFILDO,285)
 285     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBER', 
     1           ' KFILOG WITH KFILAN')
         WRITE(KFILDO,286)KFILOG
 286     FORMAT('     KFILOG =',I3)
         WRITE(KFILDO,287)KFILAN
 287     FORMAT('     KFILAN =',I3)
         WRITE(KFILDO,288)
 288     FORMAT('     STOP IN U155CK AT 288')
         STOP 288
      ENDIF
C
      DO 400 J=1,2
      DO 300 L=1,NUMIN
C
      IF(KFILD(J).EQ.KFILIN(L))THEN
         WRITE(KFILDO,295)
 295     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBERS IN', 
     1           ' KFILIN( ) WITH KFILD( )')
         WRITE(KFILDO,296)(KFILIN(M),M=1,NUMIN)
 296     FORMAT('     KFILIN =',38I3/('             ',38I3))
         WRITE(KFILDO,297)(KFILD(M),M=1,2)
 297     FORMAT('     KFILD  =',38I3/('             ',38I3))
         WRITE(KFILDO,298)
 298     FORMAT('     STOP IN U155CK AT 298')
         STOP 298
      ENDIF
C
 300  CONTINUE
C
      DO 310 L=1,IPIN
C
      IF(KFILD(J).EQ.IP(L))THEN
         WRITE(KFILDO,305)
 305     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBERS IN', 
     1           ' IP( ) WITH KFILD( )')
         WRITE(KFILDO,306)(KFILD(M),M=1,2)
 306     FORMAT('     KFILD  =',38I3/('             ',38I3))
         WRITE(KFILDO,307)(IP(M),M=1,IPIN)
 307     FORMAT('     IPIN   =',38I3)
         WRITE(KFILDO,308)
 308     FORMAT('     STOP IN U155CK AT 308')
         STOP 308
      ENDIF
C
 310  CONTINUE
C
      DO 320 L=1,NUMRA
C
      IF(KFILD(J).EQ.KFILRA(L))THEN
         WRITE(KFILDO,315)
 315     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBERS IN', 
     1           ' KFILRA( ) WITH KFILD( )')
         WRITE(KFILDO,316)(KFILD(M),M=1,2)
 316     FORMAT('     KFILD  =',38I3/('             ',38I3))
         WRITE(KFILDO,317)(KFILRA(M),M=1,NUMRA)
 317     FORMAT('     KFILRA =',38I3)
         WRITE(KFILDO,318)
 318     FORMAT('     STOP IN U155CK AT 318')
         STOP 318
      ENDIF
C
 320  CONTINUE
C
      IF(KFILD(J).EQ.KFILIO.OR.
     1   KFILD(J).EQ.KFILDI.OR.
     2   KFILD(J).EQ.KFILDO.OR.
     3   KFILD(J).EQ.KFIL10.OR.
     4   KFILD(J).EQ.KFILVO.OR.
     5   KFILD(J).EQ.KFILOG.OR.
     6   KFILD(J).EQ.KFILAN)THEN
         WRITE(KFILDO,325)
 325     FORMAT(/' ****INCONSISTENCY IN INPUT UNIT NUMBERS IN', 
     1           ' KFILD( ) WITH',
     2           ' KFILIO, KFILDI, KFILDO,',
     3           ' KFIL10, KFILVO, KFILOG, OR KFILAN')
         WRITE(KFILDO,326)(KFILD(M),M=1,2)
 326     FORMAT('     KFILD  =',38I3/('             ',38I3))
         WRITE(KFILDO,327)KFILIO,KFILDI,KFILDO,
     1                    KFIL10,KFILVO,KFILOG,KFILAN
 327     FORMAT('     KFILIO, KFILDI, KFILDO,',
     1          ' KFIL10, KFILVO, KFILOG, KFILAN = ',7I3)
         WRITE(KFILDO,328)
 328     FORMAT('     STOP IN U155CK AT 328')
         STOP 328
      ENDIF
C
 400  CONTINUE
C
C        CHECK RANDOM ACCESS UNITS 42-49.
C
      DO 404 L=1,NUMRA
C
      IF(KFILRA(L).LT.42.OR.KFILRA(L).GT.49)THEN
         WRITE(KFILDO,402)(KFILRA(J),J=1,NUMRA)
 402     FORMAT(/' ****RANDOM ACCESS UNIT NUMBER NOT IN RANGE 42-49.',
     1           '  NUMBERS ARE',10I6)
         WRITE(KFILDO,403)
 403     FORMAT('     STOP IN U155CK AT 403')
         STOP 403 
      ENDIF 
C
 404  CONTINUE
C
C        CHECK ALL UNIT NUMBERS EXCEPT KFILRA( ) ARE NOT 42-49.
C
      IF((KFILD(1).GE.42.AND.KFILD(1).LE.49).OR.
     1   (KFILD(2).GE.42.AND.KFILD(2).LE.49).OR.
     2   (KFILDI.GE.42.AND.KFILDI.LE.49).OR.
     3   (KFIL10.GE.42.AND.KFIL10.LE.49).OR.
     4   (KFILVO.GE.42.AND.KFILVO.LE.49).OR.
     5   (KFILOG.GE.42.AND.KFILOG.LE.49).OR.
     6   (KFILAN.GE.42.AND.KFILAN.LE.49).OR.
     7   (KFILDO.GE.42.AND.KFILDO.LE.49))THEN
         WRITE(KFILDO,405)
 405     FORMAT(/' ****ONE OR MORE UNIT NUMBERS ARE INCORRECTLY IN THE',
     1           ' RANGE 42-49 RESERVED FOR RANDOM ACCESS FILES.')
         WRITE(KFILDO,406)
 406     FORMAT('     STOP IN U155CK AT 406')
         STOP 406
      ENDIF
C
      DO 410 L=1,NUMIN
      IF(KFILIN(L).GE.42.AND.KFILIN(L).LE.49)THEN
         WRITE(KFILDO,408)(KFILIN(J),J=1,NUMIN)
 408     FORMAT(/' ****ONE OR MORE KFILIN( ) UNIT NUMBERS ARE',
     1           ' INCORRECTLY IN THE RANGE 42-49 RESERVED FOR RANDOM',
     2           ' ACCESS FILES.'/
     3           '     KFILIN( ) NUMBERS ARE:',30I3/(27X,30I3))
         WRITE(KFILDO,409)
 409     FORMAT('     STOP IN U155CK AT 409')
         STOP 409
      ENDIF
C
 410  CONTINUE
C
      DO 420 L=1,IPIN
      IF(IP(L).GE.42.AND.IP(L).LE.49)THEN
         WRITE(KFILDO,418)(IP(J),J=1,IPIN)
 418     FORMAT(/' ****ONE OR MORE IP( ) UNIT NUMBERS ARE',
     1           ' INCORRECTLY IN THE RANGE 42-49 RESERVED FOR RANDOM',
     2           ' ACCESS FILES.'/
     3           '     IP( ) NUMBERS ARE:',30I3)
         WRITE(KFILDO,419)
 419     FORMAT('     STOP IN U155CK AT 419')
         STOP 419
      ENDIF
C
 420  CONTINUE
C
      RETURN
      END
