       SUBROUTINE SBUPDATE(STR_NDX, STR_ID)

C***************************************************************************
C
C   Function        : sbupdate.for 
C   Author          : Jim Brannon
C   Date            : February 2008
C   Purpose         : This routine updates the sub basin array variables
C                     by being repeatedly called from statecu.for as
C                     each new structure record is read in the *.str file
C                     Each new str ID is passed to this routine and they
C                     are grouped into sub-basins accd to the first two chars
C                     of the ID string
C   Calling program : statecu.f
C   Called programs : 
C   Input arguments :
C                     STR_NDX - structure index - integer
C                     STR_ID - structure ID string - CHARACTER*12
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           : note that sub-basin 0 is the "other" category - this
C                     is where structures whose first two digits are not a
C                     valid district number (between 1 and 89) are grouped
C
C   History         :(Date, Author, Description)
C
C
C***************************************************************************

C-----Argument Variable Declaration
      Integer :: STR_NDX
      Character*(*) :: STR_ID
C-----Include Global Variables and Data Defaults
      INCLUDE 'gcommon.inc'
C-----Local Variable Declarations
      integer :: idistrict=0, sb_ndx
      character*2 tmpidnum
      character*6 tmpid
      Character*24 :: tmp_str_id
      tmp_str_id=ADJUSTL(str_id)
      select case (len(trim(tmp_str_id)))
      case (0,1) !string is too short!
c       put it in the "other" group
        sbsb(str_ndx)=0
      case default
        tmpidnum = tmp_str_id(1:2)
        tmpid="DIST" // tmpidnum
        read(tmpidnum,'(i2)',err=1111)idistrict
        if(idistrict.lt.1.or.idistrict.gt.99)then
c         put it in the "other" group
          sbsb(str_ndx)=0
        else
c         put it in a district group
c         see if a district already exists
          call sbgetdist(tmpid,sb_ndx)
          if(sb_ndx.gt.0)then
c           sb already exists in list
            sbsb(str_ndx)=sb_ndx
            sbstrcnt(sb_ndx)=sbstrcnt(sb_ndx)+1
          else
c           sb not in the list - ADD IT!
            sbcount=sbcount+1
            sbid(sbcount)=tmpid
            sbname(sbcount)="District " // tmpidnum
            sbstrcnt(sbcount)=1
            sbsb(str_ndx)=sbcount
          endif
        endif
        return
      end select
c     handle the unreadable ID (in other words - not a number)
1111  sbsb(str_ndx)=0
      sbstrcnt(0)=sbstrcnt(0)+1
      END