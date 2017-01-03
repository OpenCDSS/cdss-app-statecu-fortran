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
      INTEGER :: err , i , l
      INTEGER :: IPXFARGC
!
!*** End of declarations rewritten by SPAG
!
!-IF INTEL
      Cmdln = ''
      DO i = 1 , IPXFARGC()
         CALL PXFGETARG(i,cmd,l,err)
         IF ( i==1 ) THEN
            Cmdln = cmd
         ELSE
            Cmdln = TRIM(Cmdln)//' '//cmd
         ENDIF
      ENDDO
!-IF GFORTRAN
!      Cmdln = ''
!      DO i = 1 , IARGC()
!         CALL GETARG(i,cmd)
!         IF ( i==1 ) THEN
!            Cmdln = cmd
!         ELSE
!            Cmdln = TRIM(Cmdln)//' '//cmd
!         ENDIF
!      ENDDO
!-ELSEIF LF95
!     call getcl(cmdln)
!-ENDIF
      END SUBROUTINE GetCommandLine       
