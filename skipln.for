      SUBROUTINE SKIPLN(NF,NLINE)
C***************************************************************************
C
C   Function        : myskip.f
C   Author          : Henry B. Manguerra 
C   Date            : September 1995
C   Purpose         : skips data line entries when reading input data file.
C   Calling program :
C   Called programs : none
C   Input arguments : none
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           :
C
C***************************************************************************


      INTEGER I, NF, NLINE
      CHARACTER*200 ALINE

      DO 10 I = 1, NLINE
 10      READ(NF,100) ALINE
 
 100  FORMAT(A200)

      RETURN

      END
