      SUBROUTINE READRCR(initflag,filename)

C***************************************************************************
C
C   Function        : readrcr.for
C   Author          : Jim Brannon
C   Date            : December 2011
C   Purpose         : This reads the RCR data - replacement crop requirement - 
C                     or PCR data - partial replacement crop requirement - 
C                     and overwrites the REQT array with the values it finds
C                     in the file
C   Calling program : statecu.for 
C   Called programs : none
C   Input argument  : initflag = integer flag indicating whether to initialize
C                                the REQT array with -999 values
C                                0 - do nothing
C                                1 - initialize with -999 values
C                     filename = the name of the file to open and read
C   Output arguments: none
C   Assumptions     : the file should be a STM formatted file of monthly data
C   Limitations     : 
C   Notes           :
C
C***************************************************************************

C      INCLUDE 'xccommon.inc'
      INCLUDE 'gcommon.inc'
C      INCLUDE 'pmdata.inc'

      integer initflag
      character*(*) filename
C-----Local variable declaration
      character*3 idum3
      character*12 rcridt
      character*24 twdid
      integer i,j,k,m,ierr,itmp,itmp2,hnyr1,hnyr2
      real tmprcr(DIM_NA,DIM_NY,12),temp(12)
c make sure we have a real file to read
      numch=len_trim(filename)
      if (numch.eq.0) then
        return
      endif
C initialize the arrays
      if(initflag.eq.1)then
        do j=1,DIM_NA
          do k=1,DIM_NY
            do i=1,12
              reqt(j,k,i)=-999.0
              tmprcr(j,k,i)=-999.0
            enddo
            reqt(j,k,13)=0.0
            reqt(j,k,14)=0.0
          enddo
        enddo
      else
        do j=1,DIM_NA
          do k=1,DIM_NY
            do i=1,12
              tmprcr(j,k,i)=-999.0
            enddo
            reqt(j,k,13)=0.0
            reqt(j,k,14)=0.0
          enddo
        enddo
      endif
C all the following code and logic copied from slimit.for
      OPEN (UNIT=333,FILE=filename,Status='old',iostat=ierr)
      IF (IERR.NE.0) CALL MYEXIT(6)
      call skipn(333)
      read(333,29) hnyr1, hnyr2, idum3
29    format(6x,i4,11x,i4,7x,a3)
30    read(333,31,end = 40) itmp,rcridt,(temp(j),j=1,12)
31    format(i4,1x,a12,12(f8.0))
      if((itmp .lt. nyr1).or.(itmp .gt. nyr2)) goto 30
      do 35 i=1,nbasin
        twdid=bas_id(i)
        if(twdid(1:12) .eq. rcridt) then
          itmp2=itmp-nyr1+1
          do 32 j=1,12
            tmprcr(i,itmp2,j)=temp(j)
32        continue
          goto 30
        endif
35    continue
      goto 30
40    close(333)
!
!----ew-convert diversions to calendar year to match all other calculations
!     (if .rcr file is in water year)
!
      do 191 i=1,nbasin
        do 191 m=1,nyrs
          do 191 j=1,12
            if(tmprcr(i,m,j).gt.-999.0)then
              if(idum3 .eq. 'WYR') then
                if(j .gt. 3) then
                  reqt(i,m,j-3)=tmprcr(i,m,j)
                else
                  if(m .eq. 1) goto 191
                  reqt(i,m-1,j+9)=tmprcr(i,m,j)
                endif
              else
                reqt(i,m,j)=tmprcr(i,m,j)
              endif
            endif
191   continue
      end
