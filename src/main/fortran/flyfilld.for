      SUBROUTINE FLYFILLD()

C***************************************************************************
C
C   Function        : flyfilld.for 
C   Author          : Jim Brannon
C   Date            : February 2008
C   Purpose         : This routine updates the diversion array by attempting
C                     to replace -999 (missing) values with the historical
C                     monthly average based on data in the array or 0.
C                     If no monthly average can be calculated then
C                     the value is left at -999
C                     This option is set via the imiss2 variable in the CCU file.
C                     imiss2=2,3,4,5 sets this option, fillonfly, to be true
C                     imiss2=2 causes the divonfly flag to be set to 1 - fill w/ avg
C                        and the climonfly flag to be set to 1 - fill w/ avg
C                     imiss2=3 causes the divonfly flag to be set to 2 - fill w/ 0
C                        and the climonfly flag to be set to 1 - fill w/ avg
C                     imiss2=4 causes the divonfly flag to be set to 0 - no fill
C                        and the climonfly flag to be set to 1 - fill w/ avg
C                     imiss2=5 causes the divonfly flag to be set to 2 - fill w/ 0
C                        and the climonfly flag to be set to 0 - no fill
C                     then imiss2 is reset to 0 so that existing code using imiss2
C                     will still work.  note that there may be
C                     -999 values still in the diversion array after running
C                     this routine (see above)
C                     of the ID string
C   Calling program : slimit.f
C   Called programs : 
C   Input arguments : none
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           : simply loop through array, calculating monthly average,
C                     then loop again replacing -999 with that average
C                     write averaging count to log file for reference
C
C   History         :(Date, Author, Description)
C
C
C***************************************************************************

C-----Argument Variable Declaration
C-----Include Global Variables and Data Defaults
      INCLUDE 'gcommon.inc'
C-----Local Variable Declarations
      integer :: i,m,j,k,divsupcnt(dim_na,12)
      real :: divsupavg(dim_na,12), sum, fillvalue
      do i=1,nbasin
        do j=1,12
          select case (divonfly)
          case (0) !should not happen
            fillvalue = -999.0
          case (1)
            sum=0.0
            divsupcnt(i,j)=0
            do m=1,nyrs
              if(divsup(i,m,j).eq.-999.0)then
              else
                sum = sum + divsup(i,m,j)
                divsupcnt(i,j)=divsupcnt(i,j)+1
              endif
            enddo
            if(divsupcnt(i,j).eq.0)then
              fillvalue = -999.0
            else
              fillvalue = sum / divsupcnt(i,j)
            endif
          case (2)
            fillvalue = 0.0
          case default
            fillvalue = -999.0
          end select
          do m=1,nyrs
            if(divsup(i,m,j).eq.-999.0) then
              divsup(i,m,j) = fillvalue
            endif
          enddo
        enddo
      enddo
      end