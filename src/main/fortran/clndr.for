c clndr
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

      SUBROUTINE CLNDR (jday, m_mon, m_day)

C***************************************************************************
C
C   Function        : clndr.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This calculate
C
C the equivalent of julian day (or day
C                     of the year) to calendar month(1-12) and day(1-31).
C   Calling programs: annuacrp.f, perencrp.f, avgmon.f, calpcrop.f, etoref.f,
C                     etrref.f, growth.f, proto.f, wbuild.f
C   Called programs : none 
C   Input arguments : jday  = julian day or day of the year (1-366) 
C   Output arguments: m_mon = calendar month (1-12) 
C                     m_day = calendar day of the month (1-31)
C   Assumptions     :
C   Limitations     :
C   Notes           : Leap years are handled IF the MONTH() array
C                     was populated with leap year corrected values
C
C***************************************************************************

      INCLUDE 'gcommon.inc'
c     jhb - rewrite to be a little more robust...
      INTEGER :: jday, m_mon, m_day
      INTEGER :: sum=0
!C-----Local Variable Declaration
!      INTEGER jday,m_mon,m_day,sum
!      sum = 0
!      m_mon  = 0
!  10  m_mon = m_mon + 1
!      sum = sum + MONTH(m_mon) 
!      IF (sum.LT.jday) GOTO 10
!      m_day = jday - sum + MONTH(m_mon)
!      
!  20  RETURN
      select case (jday)
      case (1)
        m_mon=1
        m_day=1
      case (2:365)
        m_mon=1
        sum=MONTH(m_mon)
        do while ((sum.lt.jday).and.(m_mon.lt.12))
          m_mon=m_mon+1
          sum=sum+MONTH(m_mon)
        end do
        sum=sum-MONTH(m_mon)
        m_day = max(1,min(jday-sum,MONTH(m_mon)))
      case (366)
c       note this assumes the 366 was not a mistake
        if(MONTH(2).eq.29) then
          m_mon=12
          m_day=31
        else
c         bad julian day value...can't be sure the calling program will handle exception, so exit nicely here
          write(*,*)
     &  'An invalid day of year value was passed to CLNDR() function: ',
     &    jday,'.  Exiting StateCU.  Please contact StateCU developer.'
          write(999,*)
     &  'An invalid day of year value was passed to CLNDR() function: ',
     &    jday,'.  Exiting StateCU.  Please contact StateCU developer.'
          stop
        endif
      case (-999)
        m_mon=-999
        m_day=-999
      case default
c       bad julian day value...can't be sure the calling program will handle exception, so exit nicely here
        write(*,*)
     &  'An invalid day of year value was passed to CLNDR() function: ',
     &    jday,'.  Exiting StateCU.  Please contact StateCU developer.'
        write(999,*)
     &  'An invalid day of year value was passed to CLNDR() function: ',
     &    jday,'.  Exiting StateCU.  Please contact StateCU developer.'
        stop
      end select
      END
