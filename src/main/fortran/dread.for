      subroutine DRead()
      INCLUDE 'pmcommon.inc'
      INCLUDE 'gcommon.inc'
      INCLUDE 'xccommon.inc'
      integer i,j,k,l,sta_ndx,yr,mo
      real d_val(31)      
      character*12 tid
Cjhb====================================================================
Cjhb  modified 08/2006
Cjhb  open files and read, put necessary data into the arrays
Cjhb====================================================================
Cjhb  initialize arrays with -999
Cjhb====================================================================
      do i=1,N_STA
        do j=1,NYRS
          do k=1,12
            do l=1,31
              tmx(i,j,k,l)=-999.0
              tmn(i,j,k,l)=-999.0
              rf(i,j,k,l)=-999.0
              rs2(i,j,k,l)=-999.0
              edpt2(i,j,k,l)=-999.0
              wd2(i,j,k,l)=-999.0
            enddo
          enddo
        enddo
      enddo
Cjhb====================================================================
      Open(60,file=tmxfile,status='OLD',iostat=ierr)
      call skipn(60)
      read(60,921) idum,idum
735   READ(60,922,END=745) YR, MO, tid,(D_VAL(J),J=1,31)
      call findsta(tid,sta_ndx)
      if(sta_ndx.gt.0)then
        if((yr.ge.nyr1).and.(yr.le.nyr2))then
          do j=1,31
            tmx(sta_ndx,yr-nyr1+1,mo,j)=D_VAL(J)
          enddo    
        endif
      endif
      goto 735
745   close(60)
Cjhb====================================================================
      Open(61,file=tmnfile,status='OLD',iostat=ierr)
      call skipn(61)
      read(61,921) idum,idum
736   READ(61,922,END=746) YR, MO, tid,(D_VAL(J),J=1,31)
      call findsta(tid,sta_ndx)
      if(sta_ndx.gt.0)then
        if((yr.ge.nyr1).and.(yr.le.nyr2))then
          do j=1,31
            tmn(sta_ndx,yr-nyr1+1,mo,j)=D_VAL(J)
          enddo    
        endif
      endif
      goto 736
746   close(61)
Cjhb====================================================================
      if(ipdy .eq. 1) then
        Open(62,file=pdyfile,status='OLD',iostat=ierr)
        call skipn(62)
        read(62,921) idum,idum
737     READ(62,922,END=747) YR, MO, tid,(D_VAL(J),J=1,31)
        call findsta(tid,sta_ndx)
        if(sta_ndx.gt.0)then
            if((yr.ge.nyr1).and.(yr.le.nyr2))then
            do j=1,31
                rf(sta_ndx,yr-nyr1+1,mo,j)=D_VAL(J)
            enddo    
            endif
        endif
        goto 737
747     close(62)
      endif
Cjhb====================================================================
      Open(63,file=solfile,status='OLD',iostat=ierr)
      call skipn(63)
      read(63,921) idum,idum
738   READ(63,922,END=748) YR, MO, tid,(D_VAL(J),J=1,31)
      call findsta(tid,sta_ndx)
      if(sta_ndx.gt.0)then
        if((yr.ge.nyr1).and.(yr.le.nyr2))then
          do j=1,31
            rs2(sta_ndx,yr-nyr1+1,mo,j)=D_VAL(J)
          enddo    
        endif
      endif
      goto 738
748   close(63)
Cjhb====================================================================
      if(flag1 .eq. 3 .or. flag1 .eq. 5) then
        Open(64,file=vapfile,status='OLD',iostat=ierr)
        call skipn(64)
        read(64,921) idum,idum
739     READ(64,922,END=749) YR, MO, tid,(D_VAL(J),J=1,31)
      call findsta(tid,sta_ndx)
      if(sta_ndx.gt.0)then
            if((yr.ge.nyr1).and.(yr.le.nyr2))then
            do j=1,31
                edpt2(sta_ndx,yr-nyr1+1,mo,j)=D_VAL(J)
            enddo    
            endif
        endif
        goto 739
749     close(64)
      endif
Cjhb====================================================================
      Open(65,file=wndfile,status='OLD',iostat=ierr)
      call skipn(65)
      read(65,921) idum,idum
740   READ(65,922,END=750) YR, MO, tid,(D_VAL(J),J=1,31)
      call findsta(tid,sta_ndx)
      if(sta_ndx.gt.0)then
        if((yr.ge.nyr1).and.(yr.le.nyr2))then
          do j=1,31
            wd2(sta_ndx,yr-nyr1+1,mo,j)=D_VAL(J)
          enddo    
        endif
      endif
      goto 740
750   close(65)
Cjhb====================================================================
 921  FORMAT(6x,i4,11x,i4)
C 922  FORMAT(i4)
 922  FORMAT(i4,2x,i2,1x,a12,31F8.0)
      return
      end
