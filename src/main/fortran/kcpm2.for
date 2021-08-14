c kcpm2
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

      SUBROUTINE KCPM2(CID,NLEN,ISTART,DPASS)

C***************************************************************************
C
C   Function        : kcpm2.f 
C   Author          : HB Manguerra
C   Date            : November 1995 
C   Purpose         : This calculates the daily crop coefficient for Penman-
C                     Monteith method by straight line interpolation using 
C                     the crop coefficient data set provided in file *.kpm
C                     for crops other than alfalfa and pasture for the
C                     period after the effective cover.
C   Calling program : growth.f
C   Called programs : none 
C   Input arguments : cid    - crop index
C                   : nlen   - length of the period for interpolation
C                   : istart - starting crop coeff index in *.kpm file
C                   : dpass  - days after the planting (emergence)
C   Output arguments: none
C   Assumptions     :  
C   Limitations     :  
C   Notes           : The routine is still valid even if nlen is less than
C                     zero.
C
C   History         : (Date, Author, Description)
C
C   11/27/95  HBM   : Created this subroutine to accomodate the new format
C                     of PM coefficients for crops other than alfalfa and
C                     pasture grass.
C
C***************************************************************************


      INCLUDE 'pmcommon.inc'

C-----Local Variable Declaration
      INTEGER I, II, IDY
      INTEGER CID,NLEN,ISTART,DPASS
      REAL DY1, DY2

      II = ISTART
      DY1 = KCDAY(CID,II)
      DY2 = KCDAY(CID,II+1)
      DO 10 I = 1, NLEN
         IDY = JSTR + I - 1 + DPASS
         IF (IDY.GT.JSTP) GOTO 200
         IF (I.GT.DY2) THEN
            II = II + 1
            DY1 = DY2
            DY2 = KCDAY(CID,II+1)
         ENDIF
         XKCB(IDY) = KCB(CID,II)-(KCB(CID,II)-KCB(CID,II+1))*
     :      (DY1-I)/(DY1-DY2)
 10   CONTINUE   

 200  RETURN
      END
