c fall
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

      SUBROUTINE FALL (yr,tfrost, Jdays)

C***************************************************************************
C
C   Function        : fall.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This calculates the first day of frost in
C                     fall based on monthly mean temperature.
C   Calling program : frost.f 
C   Called programs : julian.f 
C   Input arguments : yr     = current year 
C                     tfrost = frost temperature in fall below which crop
C                              will die.
C   Output arguments: jdays  = first day of frost in fall in julian day 
C   Assumptions     : The calculation assumes that the fall frost
C                     temperature occur only after July 1.  If the weather
C                     data does not satisfy this assumption, the growing
C                     season will be set to "0" days, and no ET will be calculated.
C   Limitations     :
C   Notes           : The routines are based on USBR XCONS2 program which
C                     uses the SCS Modified Blaney-Criddle ET Estimation
C                     Method.
C
C***************************************************************************


      INCLUDE 'gcommon.inc'


C-----Local Variable Declaration
      INTEGER I, Jdays, JULIAN, idiff, kdays
      INTEGER fmonth,yr
      REAL tfrost, NDAYS(11)

      DATA NDAYS /29.5,29.5,30.5,30.5,30.5,30.5,31.0,30.5,30.5,30.5,
     :30.5/

      iday=0
      do 100 i=1,12
        if(tmean(yr,i) .lt. -998.0) then
           iday=1
        endif
 100  continue
      if(iday .eq. 1) then
         jdays=-999.0
         return
      endif

      DO 200 I=7,11
         IF(tmean(yr,I+1).LT. tfrost) GO TO 300
  200 CONTINUE
      fmonth = 12
      kdays = 31
      GO TO 500
  300 if(tmean(yr,I) .lt. -998.0 .or. tmean(yr,I+1) .lt. -998.0) then
         jdays=-999.0
         goto 600
      endif
      idiff = INT(NDAYS(I)*(tmean(yr,I)-tfrost)/
     :       (tmean(yr,I)-tmean(yr,I+1))+ 0.5)                   ! 0.5 is added for rounding
      IF (idiff .LE. 15) THEN
         fmonth = I
         kdays = idiff + 15
      ELSE IF( idiff .GT. 31) THEN
         fmonth = 7
         kdays = 15
         goto 600
      ELSE
         fmonth = I+1
         kdays = idiff - 15
      ENDIF
  500 Jdays = JULIAN(fmonth,kdays)
  600 RETURN
      END

