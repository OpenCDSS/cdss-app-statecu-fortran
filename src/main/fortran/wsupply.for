c wsupply
c_________________________________________________________________NoticeStart_
c StateCU Consumptive Use Model
c StateCU is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2021 Colorado Department of Natural Resources
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

      SUBROUTINE WSUPPLY(IB)

C***************************************************************************
C
C   Function        : wsupply.f
C   Author          : HB Manguerra
C   Date            : February 1995 
C   Purpose         : Incorporate monthly supply data into the input summary
C                     file *.sum.
C   Calling program : statecu.f 
C   Called programs : stable.f, myexit.f
C   Input arguments : ib = current sub-basin 
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           :
C   History         :(Date, Author, Description)
C
C     11/02/95    HBM    Units changed from 1000 acre-ft to acre-ft.
C
C***************************************************************************

      INCLUDE 'gcommon.inc'
c  delete fn_len since now commoned
      INTEGER I,J,IB
      INTEGER ICNT(12)
      REAL WSUM(12), WSUMM
      CHARACTER*40 wdid

C-----Write water supply information into the summary file
      IF (SOUT.EQ.0) THEN
         IF (IB.EQ.1) THEN
            WRITE(8,903)
            WRITE(8,902) QUOTE,QUOTE
            WRITE(8,915) DLLINE
            WRITE(8,914) 
            WRITE(8,915) SLLINE
         ENDIF

         DO 174 I = 1, 12
            ICNT(I) = 0
            WSUM(I) = 0.0
 174     CONTINUE

         DO 175 J = 1, NYRS
           DO 17 I = 1, 12
              IF(DIVSUP(IB,J,I) .GT. -998) THEN
                 WSUM(I)=WSUM(I)+DIVSUP(IB,J,I)
                 ICNT(I)=ICNT(I)+1
              ENDIF
 17        CONTINUE  
 175     CONTINUE
         DO 176 I = 1, 12
            IF(ICNT(I) .EQ. 0) THEN
               WSUM(I) = -999
            ENDIF
 176     CONTINUE
         WSUMM=0
         DO 177 I=1,12
           IF(WSUM(I) .GT. -998) THEN
              WSUM(I)=WSUM(I)/ICNT(I)
              WSUMM=WSUMM+WSUM(I)
           ELSE
              WSUM(I)=-999
           ENDIF
 177     CONTINUE
         wdid=bas_id(ib)
         WRITE(8,920) wdid(1:12),(WSUM(I),I=1,12),WSUMM

         IF (IB.EQ.NBASIN) WRITE(8,915) SLLINE

      ELSE
         WRITE(8,901) QUOTE,BAS_ID(IB),QUOTE
         CALL STABLE(IB) 
      ENDIF 


 901  FORMAT(A1,'Water Supply Information for SubArea (acre-ft) = ',
     :A40,A1,30(" "))
 902  FORMAT(A1,'Water Supply Information',A1,95(" "))
 903  FORMAT(121(" "))
 914  FORMAT (1x,'WDID',12x,'Jan',5X,'Feb',5x,'Mar',5x,'Apr',5x,'May',
     :5x,'Jun',5x,'Jul',5x,'Aug',5x,'Sep',5x,'Oct',5x,'Nov',5x,'Dec',
     :5x,'Total')
 915  FORMAT(A120,1(" "))
 920  FORMAT(1x,A12,12F8.2,F11.2)

     
      RETURN

       END
