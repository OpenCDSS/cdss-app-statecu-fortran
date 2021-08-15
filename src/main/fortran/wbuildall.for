c wbuildall
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

      SUBROUTINE WBUILDALL(NDX,IB)

C***************************************************************************
C
C   Function        : wbuildall.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This calculates the winter carry over soil moisture 
C                     for all the growing seasons.  The total pptn 
C                     during the preceding non-growing period is calculated.
C                     A carry over soil moisture coefficient is multiplied
C                     to the total precipitation to get the carry over soil
C                     moisture. 
C   Calling program : mainxc.f 
C   Called programs : none 
C   Input arguments : ndx = estimation method (1 = PM, 2 = BC)
C                     ib  = current sub-basin
C   Output arguments: none 
C   Assumptions     :
C   Limitations     :
C   Notes           : 
C
C   Edited by       :
C                     10/11/06 JHB modified to be called wbuildall
C                                  (as in "all" years)
C                                  modified to remove all code inside the
C                                  year loops and instead call wbuild.for
C                                  for each year
C                     02/13/08 JHB modified to handle year 1, too (Jan to
C                                  start of growing season)
C                                  also, only calculate winter precip in months where
C                                  crop potential ET (before eff precip) is 0
C                                  to eliminate potential of months where some
C                                  crops under a structure are growing
C                                  (using precip) and others are not (creating
C                                   winter precip)
C                                  
C
C***************************************************************************

      INCLUDE 'gcommon.inc'
c      INCLUDE 'pmcommon.inc'
c      INCLUDE 'xccommon.inc'

C-----Local Variable Declaration
c      INTEGER II,IP,IY,ENDYR,NDX,M1,M2,D1,D2,IB,IRNF(DIM_NY,12)
c      REAL SUM1, SUM2

      integer ndx, ib, iy
      
      do IY=1,NYRS
        call wbuild(ndx,ib,iy)
      enddo

c         DO 2 IY=1,NYRS
c         DO 2 M=1,12
c           IRNF(IY,M) = 0
c2        CONTINUE
c
c
c      IF (NDX.EQ.1) THEN        ! using daily rainfall data
cc rb- changed order year/crop for nparce year determination
c
c         DO 5 IY = 2, NYRS
c         DO 5 IP = 1, NPARCE(IB,iy)
ccjhb=&==================================================================
cc     rewrite to change the feb count because the clndr routine was sometimes breaking here
ccjhb=&==================================================================
c           month(2)=28
c           ENDYR=365
c           IF (MOD(NYR1+IY-1,4).EQ.0) then
c             ENDYR=366
c             month(2)=29
c           endif
ccjhb=&==================================================================
c           if (JEND(IP,IY-1).lt.1) then
c             write(*,*)'wbuild ndx',ndx,'IB',ib
c             write(*,*)'IY',iy,'IP',ip
c             write(*,*)'JEND(IP,IY-1)',JEND(IP,IY-1)
c             stop
c           endif
c           DO 7 J=JEND(IP,IY-1),ENDYR
c            CALL CLNDR(J,m1,d1)    ! DETERMINE MONTH
c            WBU(IB,IY-1,M1)=WBU(IB,IY-1,M1)+TRAIN(IY-1,J)*SMEF*
c     :                   AREA(IB,IP,IY)/12
c            IF(TRAIN(IY-1,J) .LT. -998) IRNF(IY-1,M1) = 1
c7          CONTINUE
c
cC-----Sum precipitation of current year before current growing season
c          DO 20 J = 1, JBEG(IP,IY)
c            CALL CLNDR(J,m1,d1)    ! DETERMINE MONTH
c            WBU(IB,IY,M1)=WBU(IB,IY,M1)+TRAIN(IY,J)*SMEF*
c     :                   AREA(IB,IP,IY)/12
c            IF(TRAIN(IY,J) .LT. -998) IRNF(IY,M1) = 1
c20        CONTINUE
c 5       CONTINUE
c
c      ELSE                     ! using monthly rainfall data
c
c      DO 25 IY = 2, NYRS
c      DO 22 IP = 1, NPARCE(IB,iy)
c
c         MONTH(2) = 28
c         IF (MOD(NYR1+IY-1,4).EQ.0) MONTH(2) = 29 
c         CALL CLNDR(JEND(IP,IY-1),m1,d1)    ! end of nongrowing season
c         CALL CLNDR(JBEG(IP,IY),m2,d2)      ! start of nongrowing season
c       
cC-----Sum precipitation of last year after last month growing season
c
c         DO 30 IM=M1,12
c            IF(IM .EQ. M1) THEN
c               WBU(IB,IY-1,IM)=0
c               WBU(IB,IY-1,IM+1)=WBU(IB,IY-1,IM)+RNTOT(IY-1,IM)*SMEF*
c     :                   (MONTH(IM)-D1)/MONTH(IM)*AREA(IB,IP,IY)/12
c            ELSE
c               WBU(IB,IY-1,IM)=WBU(IB,IY-1,IM)+RNTOT(IY-1,IM)*SMEF*
c     :                    AREA(IB,IP,IY)/12
c            ENDIF
c            IF(RNTOT(IY-1,IM) .LT. -998) IRNF(IY-1,IM)=1
c 30      CONTINUE
c
cC-----SUm precipitation of current year before current growing season
c         DO 40 IM=1,M2
c           IF(IM .EQ. M2) THEN
c             WBU(IB,IY,IM)=WBU(IB,IY,IM)+RNTOT(IY,IM)*SMEF*
c     :            d2/month(IM)*AREA(IB,IP,IY)/12
c           ELSE
c             WBU(IB,IY,IM)= WBU(IB,IY,IM)+RNTOT(IY,IM)*SMEF*
c     :            AREA(IB,IP,IY)/12
c           ENDIF
c           IF(RNTOT(IY,IM) .LT. -998) IRNF(IY,IM)=1
c 40      CONTINUE
c 22   CONTINUE
c 25   CONTINUE
c      ENDIF
c      DO 60 IY=1,NYRS
c      DO 60 IM=1,12
c        IF(IRNF(IY,IM) .EQ. 1) WBU(IB,IY,IM) = -999
c60    CONTINUE 

      RETURN
      END
