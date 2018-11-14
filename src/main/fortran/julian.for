c julian
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

      INTEGER FUNCTION Julian(m_mon,m_day)
C***************************************************************************
C
C   Function        : julian.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This calculates the equivalent of calendar date to
C                     julian day (day of the year).
C   Calling programs: annuacrp.f, perencrp.f, fall.f, spring.f, frost.f,
C                     intertd.f
C   Called programs : myexit.f 
C   Input arguments : m_mon = calendar month (1-12)
C                     m_day = calendar day of the month (1-31)
C   Output arguments: julian= julian day or day of the year (1-366)
C   Assumptions     :
C   Limitations     :
C   Notes           : Leap years are handled.
C
C***************************************************************************
      INCLUDE 'gcommon.inc'
C-----function arguments
      INTEGER , INTENT(IN) :: m_mon, m_day
C-----Local variable declaration
      INTEGER sum_, ii 

C-----error check for day
      IF( m_day .lt. -998) THEN
         Julian = -999
         RETURN
      ELSEIF (m_day .LT. 0 .OR. m_day .GT. 31 ) THEN
         WRITE (*,*)' THE DAY ', m_day,' IS NOT A VALID DATE'
c         m_day = 15
         call MYEXIT(71)
      ENDIF

C-----error check for month
      IF( m_mon .lt. -998) THEN
         Julian = -999
         RETURN
      ELSEIF (m_mon .LT. 1 .OR. m_mon .GT. 12) THEN
         WRITE (*,*) ' THE MONTH ',m_mon,' IS NOT A VALID MONTH'
c         m_mon = 4
         call MYEXIT(72)
      ENDIF

      sum_ = 0
      DO 10 ii = 1,m_mon-1
  10     sum_ =  sum_ + MONTH(ii)
      Julian = sum_ + m_day 

      END FUNCTION

