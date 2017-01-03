      SUBROUTINE PMCLIM
C***************************************************************************
C
C   Function        : pmclim.for 
C   Author          : E. Wilson
C   Date            : July 1999
C   Purpose         : This routine reads daily climate data for the 
C                     Penman-Monteith ET method.
C   Calling program : statecu.for
C   Called programs : none
C   Input arguments : none
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           : Also redefines the frost date file read
C                     next from statecu.for
C   History         :(Date, Author, Description)
C
C
C***************************************************************************
C
C-----Include Global Variables and Data Defaults
      INCLUDE 'gcommon.inc'
      INCLUDE 'pmcommon.inc'

C-----Local Variable Declaration
      CHARACTER*10 tcounty
      CHARACTER*12 tid
      INTEGER TYR,TYMO,PTYR
C
C-----Open climate files - check years stamped in file
C
      Open(60,file=tmxfile,status='OLD',iostat=ierr)
      call skipn(60)
      read(60,921) t1nyr1,t1nyr2
      t1nyrs=t1nyr1 - t1nyr2 +1
      IF((t1nyr1 .gt. nyr1) .or. (t1nyr2 .lt. nyr2)) then
         WRITE(*,*) 'STOP-Maximum temperature data not available for req
     &uested simulation period'
       WRITE(999,*) 'STOP-Maximum temperature data not available for req
     &uested simulation period'
         STOP
      ENDIF

      Open(61,file=tmnfile,status='OLD',iostat=ierr)
      call skipn(61)
      read(61,921) t2nyr1,t2nyr2
      t2nyrs=t2nyr1 - t2nyr2 +1
      IF((t2nyr1 .gt. nyr1) .or. (t2nyr2 .lt. nyr2)) then
         WRITE(*,*) 'STOP-Minimum temperature data not available for req
     &uested simulation period'
       WRITE(999,*) 'STOP-Minimum temperature data not available for req
     &uested simulation period'
         STOP
      ENDIF
c
c  read daily precipitation file if daily effective ppt method is chosen
c
      if(ipdy .eq. 1) then
      Open(62,file=pdyfile,status='OLD',iostat=ierr)
      call skipn(62)
      read(62,921) pnyr1,pnyr2
      pnyrs=pnyr1 - pnyr2 +1
      IF((pnyr1 .gt. nyr1) .or. (pnyr2 .lt. nyr2)) then
         WRITE(*,*) 'STOP-Precipitation data not available for requested 
     & simulation period'
       WRITE(999,*) 'STOP-Precipitation data not available for requested
     & simulation period'
         STOP
      ENDIF
      endif

      Open(63,file=solfile,status='OLD',iostat=ierr)
      call skipn(63)
      read(63,921) rnyr1,rnyr2
      rnyrs=rnyr1 - rnyr2 +1
      IF((rnyr1 .gt. nyr1) .or. (rnyr2 .lt. nyr2)) then
         WRITE(*,*) 'STOP-Solar radiation data not available for request
     &ed simulation period'
       WRITE(999,*) 'STOP-Solar radiation data not available for request
     &ed simulation period'
         STOP
      ENDIF

      Open(64,file=vapfile,status='OLD',iostat=ierr)
      call skipn(64)
      read(64,921) vnyr1,vnyr2
      vnyrs=vnyr1 - vnyr2 +1
      IF((vnyr1 .gt. nyr1) .or. (vnyr2 .lt. nyr2)) then
         WRITE(*,*) 'STOP-Vapor pressure data not available for requeste
     &d simulation period'
       WRITE(999,*) 'STOP-Vapor pressure data not available for requeste
     &d simulation period'
         STOP
      ENDIF

      Open(65,file=wndfile,status='OLD',iostat=ierr)
      call skipn(65)
      read(65,921) wnyr1,wnyr2
      wnyrs=wnyr1 - wnyr2 +1
      IF((wnyr1 .gt. nyr1) .or. (wnyr2 .lt. nyr2)) then
         WRITE(*,*) 'STOP-Wind data not available for requested simulati
     &on period'
       WRITE(999,*) 'STOP-Wind data not available for requested simulati
     &on period'
         STOP
      ENDIF
c
c---- Read tmax file to determine n_sta
c
      n_sta=0
105   n_sta=n_sta+1
      read(60,922) tyr,tymo,tid
      if(tymo .eq. 2) then
         n_sta = n_sta-1
         goto 106
      endif
      wsid(n_sta)=tid
      goto 105
106   close(60)
c
c---- Read 1 year of all climate files to make sure they have the same
c     number of stations in the same order!
c
      do 110 i=1,n_sta
      read(61,922) tyr,tymo, tid
      if(wsid(i) .ne. tid) goto 111
110   continue
      close(61)
      goto 114

111   write(*,*) 'Stations in minimum temperature file do not match stat
     &ions in maximum temperature file'
      write(999,*) 'Stations in minimum temperature file do not match st
     &ations in maximum temperature file'
      stop

114   if(ipdy .eq. 1) then
         do 120 i=1,n_sta
           read(62,922) tyr,tymo, tid
           if(wsid(i) .ne. tid) goto 121
120      continue
         close(62)
         goto 124

121   write(*,*) 'Stations in precipitation file do not match stations i 
     &n maximum temperature file'
      write(999,*) 'Stations in precipitation file do not match stations
     & in maximum temperature file'
      stop
      endif

124   do 130 i=1,n_sta
      read(63,922) tyr,tymo, tid
      if(wsid(i) .ne. tid) goto 131
130   continue
      close(63)
      goto 134

131   write(*,*) 'Stations in solar radiation file do not match stations 
     & in maximum temperature file'
      write(999,*) 'Stations in solar radiation file do not match statio
     &ns in maximum temperature file'
      stop

134   do 140 i=1,n_sta
      read(64,922) tyr,tymo, tid
      if(wsid(i) .ne. tid) goto 141
140   continue
      close(64)
      goto 144

141   write(*,*) 'Stations in vapor pressure file do not match stations 
     &in maximum temperature file'
      write(999,*) 'Stations in vapor pressure file do not match station
     &s in maximum temperature file'
      stop

144   do 150 i=1,n_sta
      read(65,922) tyr,tymo, tid
      if(wsid(i) .ne. tid) goto 151
150   continue
      close(65)
      goto 154

151   write(*,*) 'Stations in wind file do not match stations in maximum
     & temperature file'
      write(999,*) 'Stations in wind file do not match stations in maxim
     &um temperature file'
      stop

c

154   continue

921   FORMAT(6x,i4,11x,i4)
922   FORMAT(i4,2x,i2,1x,a12)

      RETURN
      END
