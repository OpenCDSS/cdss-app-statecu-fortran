c readrcr
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

      SUBROUTINE READRCR(initflag,filename)

C***************************************************************************
C
C   Function        : readrcr.for
C   Author          : Jim Brannon
C   Date            : December 2011
C   Purpose         : This reads the RCR data - replacement crop requirement - 
C                     or PCR data - partial replacement crop requirement - 
C                     and overwrites the REQT array with the values it finds
C                     in the file
C   Calling program : statecu.for 
C   Called programs : none
C   Input argument  : initflag = integer flag indicating whether to initialize
C                                the REQT array with -999 values
C                                0 - do nothing
C                                1 - initialize with -999 values
C                     filename = the name of the file to open and read
C   Output arguments: none
C   Assumptions     : the file should be a STM formatted file of monthly data
C   Limitations     : 
C   Notes           :
C
C***************************************************************************

C      INCLUDE 'xccommon.inc'
      INCLUDE 'gcommon.inc'
C      INCLUDE 'pmdata.inc'

      integer initflag
      character*(*) filename
C-----Local variable declaration----------------------------------------------
C----------------------------------------------------------------------------
      character*3 idum3
      character*12 rcridt
      character*24 twdid
      integer i,j,k,m,ierr,itmp,itmp2,hnyr1,hnyr2
C  bm 09/2022 added variables associated with soil moisture initalization 
      INTEGER ip, key
      REAL scapasp
      REAL scap(DIM_NP)
      real temp(12)
c make sure we have a real file to read
      numch=len_trim(filename)
      if (numch.eq.0) then
        return
      endif
C initialize the arrays
      if(initflag.eq.1)then
        do j=1,DIM_NA
          do k=1,DIM_NY
            do i=1,12
              reqt(j,k,i)=0
              tmprcr(j,k,i)=0
            enddo
            reqt(j,k,13)=0.0
            reqt(j,k,14)=0.0
          enddo
        enddo
      endif
C all the following code and logic copied from slimit.for
      OPEN (UNIT=333,FILE=filename,Status='old',iostat=ierr)
      IF (IERR.NE.0) CALL MYEXIT(6)
      call skipn(333)
      read(333,29) hnyr1, hnyr2, idum3
29    format(6x,i4,11x,i4,7x,a3)
30    read(333,31,end = 40) itmp,rcridt,(temp(j),j=1,12)
31    format(i4,1x,a12,12(f8.0))
      if((itmp .lt. nyr1).or.(itmp .gt. nyr2)) goto 30
      do 35 i=1,nbasin
        twdid=bas_id(i)(1:12)
        if(twdid(1:12) .eq. rcridt) then
          itmp2=itmp-nyr1+1
          do 32 j=1,12
            tmprcr(i,itmp2,j)=temp(j)
32        continue
          goto 30
        endif
35    continue
      goto 30
40    close(333)
!
!----ew-convert diversions to calendar year to match all other calculations
!     (if .rcr file is in water year)
!
      do 192 i=1,nbasin
        do 191 m=1,nyrs
          do 190 j=1,12
            if(tmprcr(i,m,j).gt.-999.0)then
              if(idum3 .eq. 'WYR') then
                if(j .gt. 3) then
                  reqt(i,m,j-3)=tmprcr(i,m,j)
                else
                  if(m .eq. 1) goto 191
                  reqt(i,m-1,j+9)=tmprcr(i,m,j)
                endif
              else
                reqt(i,m,j)=tmprcr(i,m,j)
              endif
            endif
190       continue
191     continue
192   continue
!----------------------------------------------------------------------
! bm 09/2022 added in soil moisture in order to be able to avoid doing BC or PM calculations
!	when an RCR is being used. 
!--Initialize Soil Moisture Content-------------------------------------
         DO 800 i=1,nbasin
          DO 801 m=1,nyrs
           DO 802 IP = 1, nparce(i,m)
            key = bkey(i,IP,m)
            awc(key)=awcr(i)
            irz(key) = frz(key)  ! WARNING - this nullifies the fact that
                                 ! root depth increases during the growing
                                 ! season.  This capability is only used
                                 ! in the Penman-Montieth method
            rz(key) = irz(key)
            scap(IP) = 12.0*rz(key)*awc(key)  
            scapasp = (scap(IP)*area(i,IP,m))/12
            scapatot(i,m) = scapatot(i,m) + scapasp
  802      CONTINUE
  801     CONTINUE
  800    CONTINUE
!---------------------------------------------------------------------
         END