c GetCommandLine
c_________________________________________________________________NoticeStart_
c StateCU Consumptive Use Model
c StateCU is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2021 Colorado Department of Natural Resources
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

      ! Return the full command line with parameters separated by a space.
      ! The command line arguments are parsed in calling code.

      SUBROUTINE GetCommandLine(Cmdln)
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      CHARACTER(*) , INTENT(OUT) :: Cmdln
!
! Local variables
!
      CHARACTER(72) :: cmd
      INTEGER :: i

      Cmdln = ''
      DO i = 1 , IARGC()
         CALL GETARG(i,cmd)
         IF ( i==1 ) THEN
            Cmdln = cmd
         ELSE
            Cmdln = TRIM(Cmdln)//' '//cmd
         ENDIF
      ENDDO
!-ELSEIF LF95
!     call getcl(cmdln)
!-ENDIF
      END SUBROUTINE GetCommandLine       
