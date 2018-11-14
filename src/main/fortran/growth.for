C growth
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

      SUBROUTINE GROWTH(CID,KEY)

C***************************************************************************
C
C   Function        : growth.f 
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This calculates the daily crop coefficient for Penman-
C                     Monteith method by straight line interpolation using 
C                     the crop coefficient data set provided in 
C                     file *.kpm.
C   Calling program : proto.f
C   Called programs : kcpm.f, kcpm2.f 
C   Input arguments : cid - crop index
C   Output arguments: none
C   Assumptions     : 
C   Limitations     : The data set of crop coefficients in file *.kpm
C                     should include exactly 33 discrete pairs of data
C                     for alfalfa, 11 for pasture and 22 for the rest of
C                     the crops.
C   Notes           : The interpolation routine is still valid even if nlen
C                     is less than zero.  If growing season is less than
C                     peak, the crop coefficient curve is still normalized
C                     with the peak rather than the shorter growing season.
C
C   History         : (Date, Author, Description)
C
C   11/27/95   HBM  : Changed to accomodate new format of PM coefficients
C                     given in ASCE Manual 70.  Calls the kcpm.f subroutine
C                     for the actual day-to-day interpolation.
C
C***************************************************************************


      INCLUDE 'gcommon.inc'
      INCLUDE 'pmcommon.inc'

C-----Local Variable Declaration
      INTEGER I,CID,KEY,NLEN,DPASS
      CHARACTER*30 CROPN

Cjhb=&==================================================================
c     CROPN='GRASS_PASTURE'
c     CALL INDCROP(CROPN,ID)
Cjhb=&==================================================================
      CROPN=CPNAME(CID)
Cjhb=&==================================================================
      DO 121  I = 1,366
 121     XKCB(I) = 0.0


C-----Calculate daily mean crop coefficients
Cjhb=&==================================================================
c     IF (CID.EQ.1) THEN               ! alfalfa
      IF (CROPN(1:7).EQ.'ALFALFA') THEN               ! alfalfa
Cjhb=&==================================================================
C--------planting to first cut
         NLEN = GDATE5(KEY)
         DPASS = 0
         CALL KCPM(CID,NLEN,1,DPASS)
C--------first to second cut
c rb- changed amin1 to min
         NLEN = MIN(CUT2(KEY),JSTP-JSTR+1-GDATE5(KEY))
         DPASS = GDATE5(KEY)
         CALL KCPM(CID,NLEN,12,DPASS)
C--------second to third cut
         NLEN = MIN(CUT3(KEY),JSTP-JSTR+1-GDATE5(KEY)-CUT2(KEY))
         DPASS = GDATE5(KEY)+CUT2(KEY) 
         CALL KCPM(CID,NLEN,23,DPASS)
C--------third cut to killing frost - use coeff from second to third cut
         NLEN = JSTP-JSTR+1-GDATE5(KEY)-CUT2(KEY)-CUT3(KEY) 
         DPASS = GDATE5(KEY)+CUT2(KEY)+CUT3(KEY)
         CALL KCPM(CID,NLEN,23,DPASS)

Cjhb=&==================================================================
C     ELSEIF (CID.EQ.ID) THEN           ! pasture
      ELSEIF (CROPN(1:13).EQ.'GRASS_PASTURE') THEN           ! pasture
Cjhb=&==================================================================
C--------planting to harvest
         NLEN = JSTP-JSTR+1
         DPASS = 0
         CALL KCPM(CID,NLEN,1,DPASS)

      ELSE                             ! other crops

C--------planting to peak
         NLEN = GDATE5(KEY)
         DPASS = 0
         CALL KCPM(CID,NLEN,1,DPASS)

C--------peak to harvest (days after peak)
         NLEN = JSTP-JSTR+1-GDATE5(KEY)
         DPASS = GDATE5(KEY)
         CALL KCPM2(CID,NLEN,12,DPASS)
	    
      ENDIF



 200  RETURN
      END
