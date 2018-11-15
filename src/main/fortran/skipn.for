      SUBROUTINE SKIPN(NF)

C***************************************************************************
C
C   Function        : skipn.f
C   Author          : Ray Bennett with minor modifications by Luis Garcia
C   Date            : May 1995
C   Purpose         : skips any number of comment cards identified
C                     as a '*'  from a data file
C   Calling program :
C   Called programs : none
C   Input arguments : none
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           :
C
C***************************************************************************

        INTEGER nf
        CHARACTER*1 REC1

 100    READ(NF,*,END=999) REC1
c        IF(REC1.EQ.'c' .OR. REC1.EQ.'C' .OR. REC1.EQ.'#') THEN
        IF(REC1.EQ.'#') THEN
           GOTO 100
        ELSE
           BACKSPACE(NF)
        ENDIF

 999    RETURN
        END

