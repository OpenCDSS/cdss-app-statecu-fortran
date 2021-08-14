c readepr
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

      SUBROUTINE READEPR(initflag,filename)

C***************************************************************************
C
C   Function        : readepr.for
C   Author          : Erin Wilson
C   Date            : November 2019
C   Purpose         : This reads the EPR data - excess effective precipitation
C                     and overwrites the EXPREC array with the values it finds
C                     in the file
C   Calling program : statecu.for 
C   Called programs : none
C   Input argument  : initflag = integer flag indicating whether to initialize
C                                the REQT array with -999 values
C                                0 - do nothing
C                                1 - initialize with -999 values
C                     filename = the name of the file to open and read
C          Note, currently only option is to completely overwrite (iniflag=1)
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
C-----Local variable declaration
      character*3 idum3
      character*12 rcridt
      character*24 twdid
      integer i,j,k,m,ierr,itmp,itmp2,hnyr1,hnyr2
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
              exprec(j,k,i)=-999.0
            enddo
            exprec(j,k,13)=0.0
            exprec(j,k,14)=0.0
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
            exprec(i,itmp2,j)=temp(j)
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
                  exprec(i,m,j-3)=tmpepr(i,m,j)
                else
                  if(m .eq. 1) goto 191
                  exprec(i,m-1,j+9)=tmpepr(i,m,j)
                endif
              else
                exprec(i,m,j)=tmpepr(i,m,j)
              endif
            endif
190       continue
191     continue
192   continue
      end
