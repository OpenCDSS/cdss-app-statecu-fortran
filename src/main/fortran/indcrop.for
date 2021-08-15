c indcrop
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

      SUBROUTINE INDCROP(cropn,id)

C***************************************************************************
C
C   Function        : indece.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This assigns the index of a crop given its name 
C                     The crop index is defined by the order of the crop
C                     characteristic file
C   Calling program : statecu.f
C   Called programs : none
C   Input arguments : none
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C
C  EW - 1/99  Send variable icnt to INDCROP.for so can return the crop
C             array number for a specified number of crop names, not just
C             total number of crop names
C
C
***************************************************************************

      INCLUDE 'gcommon.inc'

      INTEGER KK
      CHARACTER*30 CROPN

C-----Get Crop Index
      ID=0
      DO 20 KK= 1, DIM_NC
         IF(CPNAME(KK).EQ.CROPN) THEN
            ID = KK
            GOTO 10
         ENDIF
 20   CONTINUE
 10   RETURN
      END
          

