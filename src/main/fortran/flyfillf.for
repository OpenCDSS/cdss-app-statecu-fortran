C flyfillf
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

       SUBROUTINE FLYFILLF()

C***************************************************************************
C
C   Function        : flyfillf.for 
C   Author          : Jim Brannon
C   Date            : February 2008
C   Purpose         : This routine updates the frostdate array by attempting
C                     to replace -999 (missing) values with the historical
C                     average frost dates (julians) based on other data in the array.
C                     If no average can be calculated then
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
C   Calling program : statecu.for
C   Called programs : 
C   Input arguments : none
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           : simply loop through array (years), calculating an average for each,
C                     then loop again replacing -999 with that average
C                     write averaging count to log file for reference
C                     note that i filled AFTER the frost dates have been
C                     assigned to the structure (via station weights)
C
C   History         :(Date, Author, Description)
C
C
C***************************************************************************

C-----Argument Variable Declaration
C-----Include Global Variables and Data Defaults
      INCLUDE 'gcommon.inc'
C-----Local Variable Declarations
      integer :: i, m, j, t28_cnt(dim_nw,2), t32_cnt(dim_nw,2)
      integer :: t28_avg(dim_nw,2), t32_avg(dim_nw,2), t28_sum, t32_sum
      do i=1,n_sta
        do j=1,2 !spring and fall
          t28_sum=0
          t32_sum=0
          t28_cnt(i,j)=0
          t32_cnt(i,j)=0
          do m=1,nyrs
            if(t28(i,m,j).eq.-999.0)then
            else
              t28_sum = t28_sum + t28(i,m,j)
              t28_cnt(i,j)=t28_cnt(i,j)+1
            endif
            if(t32(i,m,j).eq.-999.0)then
            else
              t32_sum = t32_sum + t32(i,m,j)
              t32_cnt(i,j)=t32_cnt(i,j)+1
            endif
          enddo
          if(t28_cnt(i,j).ne.0)then
            do m=1,nyrs
              if(t28(i,m,j).eq.-999.0)then
                t28(i,m,j) = t28_sum / t28_cnt(i,j)
              endif
            enddo
          endif
          if(t32_cnt(i,j).ne.0)then
            do m=1,nyrs
              if(t32(i,m,j).eq.-999.0)then
                t32(i,m,j) = t32_sum / t32_cnt(i,j)
              endif
            enddo
          endif
        enddo
      enddo
      end
