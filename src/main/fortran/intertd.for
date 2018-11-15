c intertd
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

      SUBROUTINE INTERTD(tempc,dayC,K,N,npart,selmon,midpt)

C***************************************************************************
C
C   Function        : intertd.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This computes the monthly mean temperature and 
C                     percent daylight hours for part months (beginning
C                     month in the spring and ending month in the fall).
C   Calling programs: perencrp.f, annuacrp.f 
C   Called programs : none 
C   Input arguments : K      = month index 
C                     N      = current year
C                     npart  = number of days within the growing season in
C                              beginning month in spring and ending month
C                              in fall.
C                     selmon = beginning or ending month index
C                     midpt  = midpoint day of spring part month or fall
C                               part month.
C   Output arguments: 
C                     tempc  = interpolated mean temperature for the part
C                              month
C                     dayC   = interpolated daylight hours for the part 
C                              month
C   Assumptions     :
C   Limitations     :
C   Notes           : The routines are based on USBR XCONS2 program which
C                     uses the SCS Modified Blaney-Criddle ET Estimation
C                     Method.
C
C***************************************************************************

      INCLUDE 'gcommon.inc'
      INCLUDE 'xccommon.inc'

C-----Local Variable Declaration
      INTEGER K, JULIAN, N, npart, selmon, midpt
      REAL dayC, tempc, d1, d2


      d1 = JULIAN(selmon, midpt)
      d2 = npart
C-----------------------------------------------------------------------
C              interpolating temperature data 
C-----------------------------------------------------------------------

      if(tmean(n,k-1) .lt. -998 .or. tmean(n,k) .lt. -998) then
         tempc=-999
      else
      tempc = tmean(n,k-1)+(( d1 - middle(k-1)) /(middle(k) -
     :   middle(k-1)))  *(tmean(n,k) - tmean(n,k-1))
      endif
C-----------------------------------------------------------------------
C              interpolating daylight data 
C-----------------------------------------------------------------------
C         write(*,*) k,selmon
      dayc = ( pclite(k-1)+((d1-middle(k-1)) /
     :   (middle(k) - middle(k-1))) *(pclite(k) - 
     :   pclite(k-1))) *(d2/month(selmon))
      RETURN
      END

