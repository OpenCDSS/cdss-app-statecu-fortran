
      SUBROUTINE FROSTALL(ib)

C***************************************************************************
C
C   Function        : frostall.f
C   Author          : HB Manguerra
C   Date            : June 1995 
C   Purpose         : This determines the start and end of the growing season
C                     based on the user specified dates, publsihed 28 and
C                     32 degree F frost dates and calculated frost dates based
C                     on monthly mean temperature. 
C   Calling program : mainxc.f, proto.f 
C   Called programs : spring.f, fall.f, myexit.f 
C   Input arguments : ib   = current sub-basin
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
C   Edited by       :
C                     10/11/06 JHB modified to be called frostall
C                                  (as in "all" years)
C                                  modified to remove all code inside the
C                                  year loop and instead call frost.for
C                                  for each year to prevent duplication 
C                                  of code in the two almost identical routines
C                                  
C
C***************************************************************************

      INCLUDE 'gcommon.inc'

c      INTEGER i,j,jfrost,JULIAN,ib,keye
c      INTEGER meth
c      Character*30 cropn
       integer j, ib
C-----Use mean monthly temperature for computing frost dates
      do 20 j=1, nyrs 
        call frost(ib,j)
c	   do 10 i=1, nparce(ib,j) 
c              key = bkey(ib,i,j)
c              CROPN=cpname(key)
c            if (cropn(1:10).eq.'WHEAT_FALL') Then  ! winter wheat begin
cc                 ! date is user specified
c               jbeg(i,j) = JULIAN(gdate1(key),gdate2(key)) 
c               if (tflg2(key).eq.0) then
c                   call fall(j,tmois2(key),jfrost)
c               elseif (tflg2(key).eq.1) then
c	            jfrost = t28(ib,j,2)
c               elseif (tflg2(key).eq.2) then
c	            jfrost = t32(ib,j,2)
c               else 
c                  call MYEXIT(32) 
c               endif
cc rb- changed amin1 to min
c               if(jfrost .lt. -998) then
c                  jend(i,j) = -999
c                  jbeg(i,j) = -999
c                  goto 10
c               endif
c               jend(i,j) =
c     :             min(jfrost,JULIAN(gdate3(key),gdate4(key)))
cC-----check for length of growing season
c               if ((jend(i,j)-jbeg(i,j)+1).gt.gdates(key))
c     :               jend(i,j) = jbeg(i,j) + gdates(key) - 1
c            elseif (cropn(1:12).eq.'WHEAT_SPRING') then      ! spring wheat end
cc                           ! date is user specified
c               jend(i,j) = JULIAN(gdate3(key),gdate4(key)) 
c               if (tflg1(key).eq.0) then
c                  call spring(j,tmois1(key),jfrost)
c               elseif (tflg1(key).eq.1) then
c                  jfrost = t28(ib,j,1)
c               elseif (tflg1(key).eq.2) then
c                  jfrost = t32(ib,j,1)
c               else
c                  call MYEXIT(33)
c               endif
c               if(jfrost .lt. -998) then
c                  jend(i,j) = -999
c                  jbeg(i,j) = -999
c                  goto 10
c               endif
c               jbeg(i,j) = 
c     :             max(jfrost,JULIAN(gdate1(key),gdate2(key)))
cC-----check for length of growing season
c               if ((jend(i,j)-jbeg(i,j)+1).gt.gdates(key))
c     :               jend(i,j) = jbeg(i,j) + gdates(key) - 1
cc               if ((jend(i,j)-jbeg(i,j)+1).gt.gdates(key))
cc     :               jbeg(i,j) = jend(i,j) - gdates(key) + 1
c            else
c               if (tflg1(key).eq.0) then
c                  call spring(j,tmois1(key),jfrost)
c               elseif (tflg1(key).eq.1) then
c                  jfrost = t28(ib,j,1)
c               elseif (tflg1(key).eq.2) then
c                  jfrost = t32(ib,j,1)
c               else
c                  call MYEXIT(34)
c               endif
c               if(jfrost .lt. -998) then
c                  jend(i,j) = -999
c                  jbeg(i,j) = -999
c                  goto 10
c               endif
c               jbeg(i,j) = 
c     :             max(jfrost,JULIAN(gdate1(key),gdate2(key)))
c               if (tflg2(key).eq.0) then
c                 call fall(j,tmois2(key),jfrost)
c               elseif (tflg2(key).eq.1) then
c                 jfrost = t28(ib,j,2)
c               elseif (tflg2(key).eq.2) then
c                 jfrost = t32(ib,j,2)
c               else 
c                 call MYEXIT(35)   
c               endif
c               if(jfrost .lt. -998) then
c                  jend(i,j) = -999
c                  jbeg(i,j) = -999
c                  goto 10
c               endif
c               jend(i,j) =
c     :             min(jfrost,JULIAN(gdate3(key),gdate4(key)))
cC-----check for length of growing season
c               if ((jend(i,j)-jbeg(i,j)+1).gt.gdates(key))
c     :               jend(i,j) = jbeg(i,j) + gdates(key) - 1
c            endif
c           if(jend(i,j).lt. jbeg(i,j)) jend(i,j)=jbeg(i,j)
c 10        continue
 20   continue
      RETURN
      END
