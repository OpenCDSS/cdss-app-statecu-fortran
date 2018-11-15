c flyfillt
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

       SUBROUTINE FLYFILLT()

C***************************************************************************
C
C   Function        : flyfillt.for 
C   Author          : Jim Brannon
C   Date            : February 2008
C   Purpose         : This routine updates the temperature array by attempting
C                     to replace -999 (missing) values with the historical
C                     monthly mean temp based on data in the array.
C                     If no monthly average can be calculated then
C                     the value is left at -999
C                     This option is set via the imiss2 variable in the CCU file.
C                     imiss2=2,3,4,5 sets this option, fillonfly, to be true
C                     imiss2=2 causes the divonfly flag to be set to 1 - fill w/ avg
C                        and the climonfly flag to be set to 1 - fill w/ avg
C                     imiss2=3 causes the divonfly flag to be set to 2 - fill w/ 0
C                        and the climonfly flag to be set to 1 - fill w/ avg
C                     imiss2=4 causes the divonfly flag to be set to 0 - no fill
C                        and the climonfly flag to be set to 1 - fill w/ avg
C                     imiss2=5 causes the divonfly flag to be set to 2 - fill w/ 0
C                        and the climonfly flag to be set to 0 - no fill
C                     then imiss2 is reset to 0 so that existing code using imiss2
C                     will still work.  note that there may be
C                     -999 values still in the diversion array after running
C                     this routine (see above)
C                     of the ID string
C   Calling program : slimit.f
C   Called programs : 
C   Input arguments : none
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           : simply loop through array, calculating monthly average,
C                     then loop again replacing -999 with that average
C                     write averaging count to log file for reference
C
C   History         :(Date, Author, Description)
C
C
C***************************************************************************

C-----Argument Variable Declaration
C-----Include Global Variables and Data Defaults
      INCLUDE 'gcommon.inc'
C-----Local Variable Declarations
      integer :: i,m,j,tmpt_cnt(dim_nw,12)
      real :: tmpt_avg(dim_nw,12), sum
      do i=1,n_sta
        do j=1,12
          sum=0.0
          tmpt_cnt(i,j)=0
          do m=1,nyrs
            if(tmpt(i,m,j).eq.-999.0)then
            else
              sum = sum + tmpt(i,m,j)
              tmpt_cnt(i,j)=tmpt_cnt(i,j)+1
            endif
          enddo
          if(tmpt_cnt(i,j).ne.0)then
            do m=1,nyrs
              if(tmpt(i,m,j).eq.-999.0)then
                tmpt(i,m,j) = sum / tmpt_cnt(i,j)
              endif
            enddo
          endif
        enddo
      enddo
      end
