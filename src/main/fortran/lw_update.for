c lw_update
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

       SUBROUTINE LW_UPDATE(LW_NDX, STR_TOKEN)

C***************************************************************************
C
C   Function        : lw_update.for 
C   Author          : Jim Brannon
C   Date            : August 2008
C   Purpose         : This routine updates the log file warning array variables
C                     by being repeatedly called from as
C                     each new warning event occurs
C                     The warning event index and a str token is passed
C                     to this routine each time so counts can be kept.
C   Calling program : many
C   Called programs : 
C   Input arguments :
C                     LW_NDX - log file warning index - integer
C                     STR_TOKEN - ID string for keeping sub counts- CHARACTER*24
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           :
C
C   History         :(Date, Author, Description)
C
C
C***************************************************************************

C-----Argument Variable Declaration
      Integer :: LW_NDX
      Character*(*) :: STR_TOKEN
C-----Include Global Variables and Data Defaults
      INCLUDE 'gcommon.inc'
C-----Local Variable Declarations
      integer :: s_ndx=0
      logical :: new_str = .true.
c     check the passed arguments
      select case (lw_ndx)
      case (1:100)
      case default
        lw_ndx=100 !illegal log file warning value
      end select
      lgwrcnt(lw_ndx)=lgwrcnt(lw_ndx)+1 !this is the count of all times this event occurs
c     this determines if this even has occurred on this structure yet
      new_str=.TRUE.
      do s_ndx=1,lgwrscnt(lw_ndx)
        if(lgwrsid(lw_ndx,s_ndx).eq.str_token)then !existing structure for this warning
          lgwrswcnt(lw_ndx,s_ndx)=lgwrswcnt(lw_ndx,s_ndx)+1
          new_str=.FALSE.
        endif
      enddo      
      if(new_str)then !new structure for this warning
        lgwrscnt(lw_ndx)=lgwrscnt(lw_ndx)+1
        s_ndx=lgwrscnt(lw_ndx)
        lgwrsid(lw_ndx,s_ndx)=str_token
        lgwrswcnt(lw_ndx,s_ndx)=lgwrswcnt(lw_ndx,s_ndx)+1
      endif
      WARNINGS=.TRUE.
      END
