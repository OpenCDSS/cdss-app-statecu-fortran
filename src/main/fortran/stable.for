      SUBROUTINE STABLE(IB)

C***************************************************************************
C
C   Function        : stable.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This prepares summary tables for water supply. 
C   Calling program : wsupply.f
C   Called programs :
C   Input arguments : none
C                   : 
C   Output arguments: none
C   Assumptions     : 
C   Limitations     : 
C   Notes           : 
C
C***************************************************************************

      INCLUDE 'gcommon.inc'

C-----Local variable declaration
      INTEGER I,J,IDUM,IANNUAL(DIM_NY),IMONTH(12),ITOTAL
      REAL ANNUAL(DIM_NY),MONTLY(12),TOTAL


C-----Initialize
      TOTAL=0
      ITOTAL=0
      DO 10 I = 1, NYRS
 10      ANNUAL(I) = 0.0
      DO 20 J = 1, 12
         IMONTH(J)=0
 20      MONTLY(J) = 0.0

C-----Calculate Annual Total (or average), Monthly Total, and Grand Total 
      DO 40 I = 1, NYRS
      IANNUAL(I)=0
      DO 30 J = 1, 12
         IF(DIVSUP(IB,I,J) .GT. -998) THEN
           ANNUAL(I) = ANNUAL(I) + DIVSUP(IB,I,J)
         ELSE
            IANNUAL(I)=1
         ENDIF
         IF(DIVSUP(IB,I,J) .GT. -998) THEN
            MONTLY(J) = MONTLY(J) + DIVSUP(IB,I,J)
            IMONTH(J) = IMONTH(J)+1
         ENDIF

 30   CONTINUE
      IF(IANNUAL(I) .EQ. 1) ANNUAL(I) = -999
      IF(ANNUAL(I) .GT. -998) THEN
        TOTAL = TOTAL+ANNUAL(I)
        ITOTAL=ITOTAL+1
      ENDIF
 40   CONTINUE

C-----Write Results
      WRITE(8,900) DLLINE
      WRITE(8,906) (QUOTE, IDUM = 1,28)

      WRITE(8,900) SLLINE
      DO 50 I = 1, NYRS
 50      WRITE(8,904) NYR1+I-1,(DIVSUP(IB,I,J),J=1,12),ANNUAL(I)
      WRITE(8,900) SLLINE

      WRITE(8,905) QUOTE,QUOTE,(MONTLY(J)/IMONTH(J), J=1,12),
     1            TOTAL/ITOTAL

 900  FORMAT(A120,1(" "))
 904  FORMAT (2x,I4,2x,12F8.2,F12.2,5(" "))
 905  FORMAT (1x,A1,'Mean',A1,1x,12F8.2,F12.2,5(" ")/121(" ")/121(" "))
 906  FORMAT (1x,A1,'Year',A1,4x,A1,'Jan',A1,3x,A1,'Feb',A1,3x,A1,'Mar',
     :A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1,'Jun',A1,3x,A1,'Jul',A1,3x,
     :A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1,'Oct',A1,3x,A1,'Nov',A1,3x,A1,
     :'Dec',A1,6x,A1,'Total',A1,4(" "))

      RETURN
      END
