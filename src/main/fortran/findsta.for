      subroutine findsta(id_arg,ndx_arg)
      character*12 :: id_arg
      integer :: ndx_arg,i
      INCLUDE 'gcommon.inc'
      ndx_arg=0
      do i=1,n_sta
        if(trim(wsid(i)) .eq. trim(id_arg)) then
            ndx_arg=i
            exit
        endif
      enddo
      return
      end
