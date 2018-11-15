       SUBROUTINE LW_WRITE(IYOUNIT)

C***************************************************************************
C
C   Function        : lw_write.for 
C   Author          : Jim Brannon
C   Date            : August 2008
C   Purpose         : This routine write the log file warnings
C   Calling program : statecu.for
C   Called programs : 
C   Input arguments :
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           : 
C   History         :(Date, Author, Description)
C
C***************************************************************************

C-----Argument Variable Declaration
      INTEGER :: IYOUNIT
      INTEGER iorder(100)
C-----Include Global Variables and Data Defaults
      INCLUDE 'gcommon.inc'
C-----Local Variable Declarations
      integer :: i, j, j2, i2
      write(IYOUNIT,*)' '
      write(IYOUNIT,*)' '
      write(IYOUNIT,'(A34)')'StateCU log file warning messages:'
      write(IYOUNIT,'(A34)')'=================================='
      write(IYOUNIT,*)

      iorder(1)=1
      iorder(2)=2
      iorder(3)=9
      iorder(4)=13
      iorder(5)=14
      iorder(6)=62
      iorder(7)=65
      iorder(8)=63
      iorder(9)=7
      iorder(10)=8
      iorder(11)=4
      iorder(12)=58
      iorder(13)=5
      iorder(14)=6
      iorder(15)=59
      iorder(16)=15
      iorder(17)=52
      iorder(18)=64
      iorder(19)=60
      iorder(20)=61
      iorder(21)=40
      iorder(22)=41
      iorder(23)=42
      iorder(24)=43
      iorder(25)=44
      iorder(26)=45
      iorder(27)=46
      iorder(28)=55
      iorder(29)=54
      iorder(30)=48
      iorder(31)=49
      iorder(32)=51
      iorder(33)=50
      iorder(34)=53
      iorder(35)=57
      iorder(36)=66
      iorder(37)=56
      iorder(38)=47


      do i=1,38
        i2=iorder(i)
        if(lgwrmsg1(i2).eq.'')then !this is an unused log file warning index
        else
          if(lgwrcnt(i2).gt.0)then !this event occurred one or more times
            write(IYOUNIT,*)
            write(IYOUNIT,'(A50,i4,A8)')
     &      '======= LOG FILE INFORMATION/WARNING  ======='
            write(IYOUNIT,'(A150)') lgwrmsg1(i2)
            write(IYOUNIT,'(A150)') lgwrmsg2(i2)
            write(IYOUNIT,'(A150)') lgwrmsg3(i2)
            write(IYOUNIT,'(A28,i6,A51)')
     &      'This warning event occurred ',
     &      lgwrcnt(i2),
     &      ' times (total) during the model run as shown below.'
            write(IYOUNIT,'(A32)')'Count     ID/Name/Token String  '
            write(IYOUNIT,'(A32)')'-----   ------------------------'  
            do j=1,lgwrscnt(i2)
              write(IYOUNIT,'(I6,2X,A50)')lgwrswcnt(i2,j),lgwrsid(i2,j)
            enddo
          endif
        endif
      enddo
      end