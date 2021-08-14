c sb_final
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

       SUBROUTINE SB_FINAL()

C***************************************************************************
C
C   Function        : sbupdate.for 
C   Author          : Jim Brannon
C   Date            : August 2008
C   Purpose         : This routine finalizes the sub basin array variables
C                     Note: sub-basin = group structures by district (first two characters of WDID field)
C                                       districts 01 - 99
C                                       if the first two characters are numbers, create a sub-basin total
C                               basin = all structures
C                     Set the flags for the sub-basin and basin output:
C                     One structure - no sub-basin, no basin output
C                     Two structures - no sub-basin,  include basin output
C                     Three or more - use sub-basin counts to determine if necessary
C                       If any sub-basin (other than "other") has >1, then
C                         output the non empty sub-basin totals;
C                       otherwise do not output sub-basin totals.
C                     It sets the "other" group flag if it is empty.
C                     It also removes the district group if none of them
C                     have more than 1 structure in them.
C   Calling program : statecu.f
C   Called programs : 
C   Input arguments : none
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           : note that sub-basin 0 is the "other" category - this
C                     is where structures whose first two digits are not a
C                     valid district number (between 1 and 89) are grouped
C
C   History         :(Date, Author, Description)
C
C
C***************************************************************************

C-----Argument Variable Declaration
C-----Include Global Variables and Data Defaults
      INCLUDE 'gcommon.inc'
C-----Local Variable Declarations
      integer :: idistrict=0

      select case (nbasin)
      case (0)
        sb_flag=.FALSE.
        b_flag=.FALSE.
      case (1)
        sb_flag=.FALSE.
        b_flag=.FALSE.
      case (2)
        sb_flag=.FALSE.
        b_flag=.TRUE.
      case default
        sb_flag=.FALSE.
        do idistrict=1,sbcount
          if(sbstrcnt(idistrict).gt.1)then
            sb_flag=.TRUE.
          endif
        enddo
        b_flag=.TRUE.
      end select
            
c     handle other group
      if(sb_flag)then
        if(sbstrcnt(0).gt.0)then
          sb0_flag=.TRUE.
        else
          sb0_flag=.FALSE.
        endif
      else
        sb0_flag=.FALSE.
      endif

      END
