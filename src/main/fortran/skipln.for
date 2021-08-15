c skipin
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

      SUBROUTINE SKIPLN(NF,NLINE)

C***************************************************************************
C
C   Function        : myskip.f
C   Author          : Henry B. Manguerra 
C   Date            : September 1995
C   Purpose         : skips data line entries when reading input data file.
C   Calling program :
C   Called programs : none
C   Input arguments : none
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           :
C
C***************************************************************************


      INTEGER I, NF, NLINE
      CHARACTER*200 ALINE

      DO 10 I = 1, NLINE
 10      READ(NF,100) ALINE
 
 100  FORMAT(A200)

      RETURN

      END
