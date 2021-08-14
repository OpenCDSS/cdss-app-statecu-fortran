c frost
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

      SUBROUTINE FROST(ib,j)

C***************************************************************************
C
C   Function        : frost.f
C   Author          : HB Manguerra
C   Date            : June 1995 
C   Purpose         : This determines the start and end of the growing season
C                     based on the user specified dates, publsihed 28 and
C                     32 degree F frost dates and calculated frost dates based
C                     on monthly mean temperature for one year only 
C   Calling program : mainxc.f, proto.f 
C   Called programs : spring.f, fall.f, myexit.f 
C   Input arguments : ib   = current sub-basin, j is the year index
C   Output arguments: none 
C   Assumptions     :
C   Limitations     :
C   Notes           : tflg1 = flag for frost method in spring
C                     tflg2 = flag for frost method in fall
C
C                     if flag is:
C                            = 0, frost date is based on monthly mean temp
C                            = 1, frost date is based on published 28 
C                                 degree F frost date
C                            = 2, frost date is based on published 32 
C                                 degree F frost date
C
C***************************************************************************

      INCLUDE 'gcommon.inc'

      INTEGER i,j,jfrost,JULIAN,ib
      CHARACTER*30 CROPN
Cjhb=&==================================================================
C-----Use mean monthly temperature for computing frost dates
Cjhb=&==================================================================
C-----loop through all the parcels for this structure (ib) in this year (j)
Cjhb=&==================================================================
      do 10 i=1, nparce(ib,j)
            key = bkey(ib,i,j)
            CROPN=cpname(key)
Cjhb=&==================================================================
C           parcel crop is fall wheat
Cjhb=&==================================================================
            if (CROPN(1:10).eq.'WHEAT_FALL') then  ! winter wheat begin
                  ! date is user specified
Cjhb=&==================================================================
C              start with crop begin season date
Cjhb=&==================================================================
               jbeg(i,j) = JULIAN(gdate1(key),gdate2(key)) 
Cjhb=&==================================================================
C              find end of season date:
Cjhb=&==================================================================
C              find first fall frost
Cjhb=&==================================================================
               if (tflg2(key).eq.0) then
                  call fall(j,tmois2(key),jfrost)
               elseif (tflg2(key).eq.1) then
                  jfrost = INT(t28(ib,j,2))
               elseif (tflg2(key).eq.2) then
                  jfrost = INT(t32(ib,j,2))
               else 
                  call MYEXIT(32) 
               endif
Cjhb=&==================================================================
C              check for impacts of missing temperature data
Cjhb=&==================================================================
               if(jfrost .lt. -998.0) then
                  jend(i,j) = -999.0
                  jbeg(i,j) = -999.0
                  goto 10
               endif
Cjhb=&==================================================================
C              calculate end of season date
Cjhb=&==================================================================
c              rb- changed amin1 to min
               jend(i,j) = min(jfrost,JULIAN(gdate3(key),gdate4(key)))
C-----         check for length of growing season
               if ((jend(i,j)-jbeg(i,j)+1).gt.gdates(key))
     :               jend(i,j) = jbeg(i,j) + gdates(key) - 1
Cjhb=&==================================================================
C           parcel crop is spring wheat
Cjhb=&==================================================================
            elseif (cropn(1:12).eq.'WHEAT_SPRING') then      ! spring wheat end 
                           ! date is user specified
Cjhb=&==================================================================
C              start with crop begin season date - starts in fall, use jend()
Cjhb=&==================================================================
               jend(i,j) = JULIAN(gdate3(key),gdate4(key)) 
Cjhb=&==================================================================
C              find end of season date:
Cjhb=&==================================================================
C              find last spring frost
Cjhb=&==================================================================
               if (tflg1(key).eq.0) then
                  call spring(j,tmois1(key),jfrost)
               elseif (tflg1(key).eq.1) then
                  jfrost = INT(t28(ib,j,1))
               elseif (tflg1(key).eq.2) then
                  jfrost = INT(t32(ib,j,1))
               else
                  call MYEXIT(33)
               endif
Cjhb=&==================================================================
C              check for impacts of missing temperature data
Cjhb=&==================================================================
               if(jfrost .lt. -998.0) then
                  jend(i,j) = -999.0
                  jbeg(i,j) = -999.0
                  goto 10
               endif
Cjhb=&==================================================================
C              calculate end of season date - ends in spring, use jbeg()
Cjhb=&==================================================================
               jbeg(i,j) = max(jfrost,JULIAN(gdate1(key),gdate2(key)))
C-----         check for length of growing season
               if ((jend(i,j)-jbeg(i,j)+1).gt.gdates(key))
     :               jbeg(i,j) = jend(i,j) - gdates(key) + 1
Cjhb=&==================================================================
C           parcel crop is any other crop besides fall/spring wheat
Cjhb=&==================================================================
            else
Cjhb=&==================================================================
C              find crop begin of season date:
Cjhb=&==================================================================
C              find last spring frost
Cjhb=&==================================================================
               if (tflg1(key).eq.0) then
                  call spring(j,tmois1(key),jfrost)
               elseif (tflg1(key).eq.1) then
                  jfrost = INT(t28(ib,j,1))
               elseif (tflg1(key).eq.2) then
                  jfrost = INT(t32(ib,j,1))
               else
                  call MYEXIT(34)
               endif
Cjhb=&==================================================================
C              check for impacts of missing temperature data
Cjhb=&==================================================================
               if(jfrost .lt. -998.0) then
                  jend(i,j) = -999.0
                  jbeg(i,j) = -999.0
                  goto 10
               endif
Cjhb=&==================================================================
C              calculate begin of season date
Cjhb=&==================================================================
               jbeg(i,j) =
     :             max(jfrost,JULIAN(gdate1(key),gdate2(key)))
Cjhb=&==================================================================
C              find crop end of season date:
Cjhb=&==================================================================
C              find first fall frost
Cjhb=&==================================================================
               if (tflg2(key).eq.0) then
                 call fall(j,tmois2(key),jfrost)
               elseif (tflg2(key).eq.1) then
                 jfrost = INT(t28(ib,j,2))
               elseif (tflg2(key).eq.2) then
                 jfrost = INT(t32(ib,j,2))
               else 
                 call MYEXIT(35)   
               endif
Cjhb=&==================================================================
C              check for impacts of missing temperature data
Cjhb=&==================================================================
               if(jfrost .lt. -998.0) then
                  jend(i,j) = -999.0
                  jbeg(i,j) = -999.0
                  goto 10
               endif
Cjhb=&==================================================================
C              calculate end of season date
Cjhb=&==================================================================
               jend(i,j) =
     :             min(jfrost,JULIAN(gdate3(key),gdate4(key)))
C-----         check for length of growing season
               if ((jend(i,j)-jbeg(i,j)+1).gt.gdates(key))
     :               jend(i,j) = jbeg(i,j) + gdates(key) - 1
Cjhb=&==================================================================
            endif
 10        continue

      RETURN
      END
