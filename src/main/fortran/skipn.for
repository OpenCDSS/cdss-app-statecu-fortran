c skipn
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

      SUBROUTINE SKIPN(NF)

C***************************************************************************
C
C   Function        : skipn.f
C   Author          : Ray Bennett with minor modifications by Luis Garcia
C   Date            : May 1995
C   Purpose         : skips any number of comment cards identified
C                     as a '*'  from a data file
C   Calling program :
C   Called programs : none
C   Input arguments : none
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           :
C
C***************************************************************************

        INTEGER nf
        CHARACTER*1 REC1

 100    READ(NF,*,END=999) REC1
c        IF(REC1.EQ.'c' .OR. REC1.EQ.'C' .OR. REC1.EQ.'#') THEN
        IF(REC1.EQ.'#') THEN
           GOTO 100
        ELSE
           BACKSPACE(NF)
        ENDIF

 999    RETURN
        END

