c stable
c_________________________________________________________________NoticeStart_
c StateCU Consumptive Use Model
c StateCU is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2018 Colorado Department of Natural Resources
c 
c StateCU is free software:  you can redistribute it and/or modify
c     it under the terms of the GNU General Public License as published by
c     the Free Software Foundation, either version 3 of the License, or
c     (at your option) any later version.
c 
c     StateCU is distributed in the hope that it will be useful,
c     but WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU General Public License for more details.
c 
c     You should have received a copy of the GNU General Public License
c     along with StateCU.  If not, see <https://www.gnu.org/licenses/>.
c_________________________________________________________________NoticeEnd___

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
        ANNUAL(I) = 0.0
 10   CONTINUE 
      DO 20 J = 1, 12
         IMONTH(J)=0
         MONTLY(J) = 0.0
 20   CONTINUE

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
        WRITE(8,904) NYR1+I-1,(DIVSUP(IB,I,J),J=1,12),ANNUAL(I)
 50   CONTINUE
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
