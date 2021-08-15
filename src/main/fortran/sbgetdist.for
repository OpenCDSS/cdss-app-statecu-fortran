c sbgetdist
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

       SUBROUTINE SBGETDIST(SB_ID,SB_NDX)

C***************************************************************************
C
C   Function        : sbgetdist.for 
C   Author          : Jim Brannon
C   Date            : February 2008
C   Purpose         : This routine searches the existing sub basins and
C                     finds the one with an ID that matches SB_ID
C                     and sets SB_NDX = index of that subbasin in the list
C                     if none is found, it sets SB_NDX = 0
C   Calling program : sbupdate.for
C   Called programs : 
C   Input arguments :
C                     SB_ID - character*2 id to match
C   Output arguments: 
C                     SB_NDX - index of sub basin that matched
C   Assumptions     :
C   Limitations     :
C   Notes           : 
C
C   History         :(Date, Author, Description)
C
C
C***************************************************************************

C-----Argument Variable Declaration
      Integer :: SB_NDX
      Character*(*) :: SB_ID
C-----Include Global Variables and Data Defaults
      INCLUDE 'gcommon.inc'
C-----Local Variable Declarations
      integer :: ndx
      
      SB_NDX=0
      do ndx=1,sbcount
        if(sbid(ndx).eq.sb_id)then
          sb_ndx=ndx
        endif
      enddo
      END
