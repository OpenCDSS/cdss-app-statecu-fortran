C     STATECU.FOR
C 
C    This program was originally developed for the CRDSS Project
c    to calculate crop consumptive use under the Blaney-Criddle
c    and Penman-Monteith methods.  Original developers were the
c    Integrated Decision Support Group of Colorado State University
c   
c    Modifications have been made to this program by Leonard Rice
c    Consulting Water Engineers to enable a response file, statemod
c    formatted input files, climate weight replacment file, etc.
c
c    November, 1998 - Modified program to accept a replacmeent file
c                     of climate weights for aggregated structures,
c                     undefined structures, new climate definitions,
c                     etc.
c 
c    January, 1999  - Modified program to handle new .ddc file format,
c                     with crop names instead of crop numbers.  Tied 
c                     kbcfile and cropchar file to crop names instead
c                     of crop numbers
c
c
c NOTE:  Three crop names are "hard-wired" and need to be written the same
c        in all files - ALFALFA, WHEAT_SPRING, and WHEAT_FALL.  These
c        three crops are handled specially due to cutting, etc.
c
c    February, 1999 - Modified program to adjust crop type
c                     percentages retrieved from the database that do not
c        
c      add up to 1.000.  Also eliminated writing to and
C                     reading from temporary files to increase execution speed
C
c    May 12, 2003    Version 2.3 Revised to handle multiple subirrigated crops
c    January 26,2004 Version 2.4 Revised Wsupsum to correct errors related to 
c                    printing % Gw and Acres (were fractions) and information 
c                    related to soil mosture when acres change
C***************************************************************************
C
C     PROGRAM STATECU
C  
C***************************************************************************

      PROGRAM STATECU

c rb-  add visual fortran dflib with getarg function      
c      comment out for Lahey compiler
C     USE DFLIB

      INCLUDE 'gcommon.inc'
c      INCLUDE 'gdata.inc'
      INCLUDE 'pmcommon.inc'
      INCLUDE 'pmdata.inc'

C-----Local Variable Declarations
      INTEGER IERR
      INTEGER I, J, K, IYR
      character*200 gwfile
      CHARACTER*200 dfile1,argmet,prfile,ddcfile,croprep,setout
      INTEGER KK,iacre(dim_na)
      integer(2) nbuff,statln,ndlyt
      character*127 command,tchar,tdesc
      REAL tlat,tttwws,tttwrs,rept4,rept5
      real ttacre,tpct(20),tcdsarea(20),tcdssum
      real t21,t22,t33(12),tpct1
      real rlat(dim_na),tlo,tel,trlatd,relev(dim_na)
      real smcar
      character*200 file1,file2,fline
      Character*60 ext
      character*6 fd1,fd2,fd3,fd4
      character*12 twdid,tid,tempid
      character*12 cid(900)
      character*20 chtest
      character*30 cropn
      character*30 TMPSTR1, TMPSTR2
      character*24 tname
      character*10 tcounty,rept1,rept3,tid2
      character*40 tbasin,line,fstring
      character*3 idum3
Cjhb=&==================================================================
      character*40 OptArgs
      character*9 celev
Cjhb=&==================================================================
      integer thuc,tyr,tyr1,ptyr,ifound,if1,ttyr
      integer tread,ttcrop,istart,tncrop,tnparce
      integer t1,t2,t3,t4,t5,t6,t7,t8,iloca(7),rept2,rept6
      INTEGER tbkey(20),T28_1,T28_2,T32_1,T32_2,ttypout
      integer pyr1,pyr2,i1(700,DIM_NY),i2(700,DIM_NY)
      integer i3(700,DIM_NY), i4(700,DIM_NY)
      character*100 theCWD
      integer(4) istat, itmp
      logical bUseAcreage
      integer ArgCount

      CALL DATE_AND_TIME (CURDATE, CURTIME)
      nbuff=1
      quote = ' '
      call FNM_INIT !init the file name variables
      call lw_init !initialize the log file warning arrays
      call GDATA_INIT !init constant variables (GDATA)
      INCH=0
      ITMP=100
      ISUPLY=0
      SOUT=0
      SMEF = 0.0
      do I=1,dim_np
        do J=1,dim_ny
            jbeg(I,J) = 0
            jend(I,J) = 0
        enddo
      enddo
cjhb=&==================================================================
c     replace with something more universal
cjhb=&==================================================================
c      call getarg(nbuff,command,statln)
C      for lahey compiler use:
c       call getcl(command)
C      if (statln.eq.-1) then
C      for lahey compiler use:
cjhb=&==================================================================
      call GetCommandLine(command)
cjhb=&==================================================================
       if(command(1:1) .eq. ' ') then
        WRITE (*,100)
100     FORMAT(' Enter base name?') 
c        READ(*,101) nch,dfile
        READ(*,101) dfile
101     FORMAT(a200)
        nch=len_trim(dfile)
        write(*,*)nch,dfile
        fn_len=nch+1
          goto 104
      endif
       do 102 i12=1,200
           if (command(i12:i12).eq.' ') then
             fn_len=i12
             goto 103
           else
             dfile(i12:i12)=command(i12:i12)
           endif
102    continue
103    continue
C----------------------------------------------------------------------------
C     Read response file *.rcu
C----------------------------------------------------------------------------
104   rcufile=dfile
      rcufile(fn_len:fn_len+4)='.rcu'
      logfile=dfile
      logfile(fn_len:fn_len+4)='.log'
      idra=0
      ipdy=0
      OPEN (UNIT=999,FILE=logfile)
      ISTAT = GETCWD (theCWD)
      IF (ISTAT == 0) write (0,*) 'Current directory is ',theCWD
      open (unit=25,file=rcufile,status='old',err=945)
      write(0,908) vers, rdate
      write(999,908) vers, rdate
      write(999,1109)dfile
1109  FORMAT('Scenario name: ', a200)
      write(999,1110)CURDATE(5:6),CURDATE(7:8),CURDATE(1:4),
     &             CURTIME(1:2),CURTIME(3:4),CURTIME(5:6)
1110  FORMAT('Model simulation began at: ',A2,'-',A2,'-',A4,'  ',
     &       A2,':',A2,':',A2)
      write(999,*)
      write(999,*)'Following are brief model execution status messages:'
      write(999,*)'===================================================='
      write(*,*) 'Reading Response File: ', rcufile
      write(999,*) 'Reading Response File: ', rcufile
Cjhb=&==================================================================
Cjhb  remove this because readfn handles comments now
C      call skipn(25)
Cjhb=&==================================================================
C
       select case (scu_debug)
       case (0)
       case (1)
         write(999,*) 'Input files found in response file (.RCU):'
       case default
       end select
14    read(25,'(a200)',end=15) file1
C
Cjhb=&==================================================================
Cjhb  remove this because readfn handles blank lines now
C      if(trim(file1) .eq. '') goto 15
Cjhb=&==================================================================
C
C0000000011111111112222222222333333333344444444445555555555666666666677777777778
C2345678901234567890123456789012345678901234567890123456789012345678901234567890
Cjhb=&==================================================================
Cjhb  call the (rewritten circa 2006) routine to read the
Cjhb  filename token and filename from the current rcu record line 
Cjhb=&==================================================================
      call readfn(file1,file2,ext)
Cjhb=&==================================================================
Cjhb  based on the token, assign the filename to a variable
Cjhb=&==================================================================
       select case (trim(ext))
       case(Token1,Token41)
         ccufile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*)'  Control filename = ',trim(ccufile)
         case default
         end select
       case(Token2,Token42)
         strfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Structure filename = ',trim(strfile)
         case default
         end select
       case(Token3,Token43)
         clifile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Climate Station filename = ',trim(clifile)
         case default
         end select
       case(Token4,Token44)
         kbcfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Blaney-Criddle Crop Coefficient filename = ',
     &                trim(kbcfile)
         case default
         end select
       case(Token5,Token45)
         cdsfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Crop Distribution filename = ',trim(cdsfile)
         case default
         end select
       case(Token6,Token46)
         tmpfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Temperature Data filename = ',trim(tmpfile)
         case default
         end select
       case(Token7,Token47)
         pptfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Precipitation Data filename = ',trim(pptfile)
         case default
         end select
       case(Token8,Token48)
         fdfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Frost Date Data filename = ',trim(fdfile)
         case default
         end select
       case(Token9,Token49)
         cchfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Crop Characteristic filename = ',trim(cchfile)
         case default
         end select
       case(Token10,Token50)
         ddhfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Monthly Historic Diversion filename = ',
     &                trim(ddhfile)
         case default
         end select
       case(Token11,Token51)
         pvhfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Historic Pumping filename = ',trim(pvhfile)
         case default
         end select
       case(Token12,Token52)
         ipyfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Irrigation Parameter filename = ',trim(ipyfile)
         case default
         end select
       case(Token13,Token53)
         ddrfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Diversion Rights filename = ',trim(ddrfile)
         case default
         end select
c       case(Token14,Token54)
c         othfile=trim(file2)
c         write(999,*) '  Other Use filename = ',trim(othfile)
c       case(Token15,Token55)
c         dlafile=trim(file2)
c         write(999,*) '  Delay Assignment filename = ',trim(dlafile)
c       case(Token16,Token56)
c         dlyfile=trim(file2)
c         write(999,*) '  Delay Pattern filename = ',trim(dlyfile)
       case(Token17,Token57)
         admfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Monthly Administration Data filename = ',
     &                trim(admfile)
         case default
         end select
       case(Token18,Token58)
         dddfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Historic Daily Diversion filename = ',
     &                trim(dddfile)
         case default
         end select
       case(Token19,Token59)
         addfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Daily Administration Data filename = ',
     &                trim(addfile)
         case default
         end select
       case(Token20,Token60)
         kpmfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
       write(999,*) '  Penmon Monteith Crop Coefficient filename = ',
     &              trim(kpmfile)
         case default
         end select
       case(Token21,Token61)
         kmhfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
        write(999,*)'  Modified Hargreaves Crop Cofficient filename = ',
     &              trim(kmhfile)
         case default
         end select
       case(Token22,Token62)
         drafile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Historic Drain Data filename = ',trim(drafile)
         case default
         end select
         idra=1
C add daily files
       case(Token23,Token63)
         tmxfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Daily Maximum Temperature Data filename = ',
     &                trim(tmxfile)
         case default
         end select
       case(Token24,Token64)
         tmnfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Daily Minimum Temperature Data filename = ',
     &                trim(tmnfile)
         case default
         end select
       case(Token25,Token65)
         pdyfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Daily Precipitation Data filename = ',
     &                trim(pdyfile)
         case default
         end select
         ipdy=1
       case(Token26,Token66)
         solfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Daily Solar Radiation Data filename = ',
     &                trim(solfile)
         case default
         end select
       case(Token27,Token67)
         vapfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Daily Vapor Pressure Data filename = ',
     &                trim(vapfile)
         case default
         end select
       case(Token28,Token68)
         wndfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*) '  Daily Wind Data filename = ',trim(wndfile)
         case default
         end select
       case(Token29,Token69)
         gisfile=trim(file2)
         select case (scu_debug)
         case (0)
         case (1)
         write(999,*)'  GIS Information filename = ',trim(gisfile)
         write(999,*)'  GIS Information file type is known but unused.'
         case default
         end select
       case("comment_line")
       case("")
c        catch this warning and record in new log file format...
         call lw_update(2,'Missing token')
         select case (scu_debug)
           case (0)
           case (1)
             write(999,*) '  Missing filename type in RCU file',
     &                    '  Skipping line.'
           case default
           end select
       Case Default
c        catch this warning and record in new log file format...
         call lw_update(1,ext)
         select case (scu_debug)
           case (0)
           case (1)
             write(999,*) '  Unknown filename type in RCU file = '
             write(999,*) trim(ext)
             write(999,*) '  Skipping line.'
           case default
         end select
       end select
      goto 14



15    if(ccufile .eq. '') then
         write(999,*) 'No control file defined in the *.rcu file'
         write(*,*) 'No control file defined in the *.rcu file'
         stop
      endif   
      write(999,*)'Reading Control File:  ',ccufile
      write(*,*) 'Reading Control File:  ', ccufile
      OPEN (UNIT=1,FILE=ccufile,STATUS='OLD',IOSTAT=IERR)
      IF (IERR.NE.0) CALL MYEXIT(1)
c
c rb- read cmn file to get flag1 and isuply option
c     read the rest later
c
      call skipn(1)
      READ(1,900,ERR=909) (TITLE(I), I=1,3)
      call skipn(1)
      READ(1,*,ERR=912) NYR1,NYR2          ! subset
      NYRS = NYR2 - NYR1 + 1
      IF( NYRS .GT. DIM_NY ) CALL MYEXIT(63)
C     Flags for ET methods
C     ET Method: FLAG1  1 - Blaney-Criddle (SCS BC Original and Enhanced) basins
C                       2 - Calculate and Summarize "other uses"
C                       3 - Penman-Monteith
C                       4 - Modified Hargreaves
c                       5 - ASCE Penman-Monteith    
C----------------------------------------------------------------------------
c rb- assume for data centered method,performing blaney criddle
c
       READ(1,*,ERR=913) FLAG1
       if(flag1 .eq. 2) goto 735
c
C Effective rainfall Method:
C   monthly: RN_XCO = 1 - SCS method
C                   = 2 - usbr mehod
C   daily: RN_XCO   = 3 - Total inches per day effective (read CN(1)= total inches)
C                   = 4 - % inches per day effective (read CN(1) = fraction)
C                   = 5 - SCS NEH Method (read CN(1,3), runoff curve 1,2,3
      READ(1,'(a40)',ERR=913) fstring
        do j=1,40
          if(fstring(j:j).eq. '0') then
            RN_XCO = 0
            goto 11
          elseif(fstring(j:j).eq. '1') then
            RN_XCO = 1
            goto 11
          elseif(fstring(j:j) .eq. '2') then
            RN_XCO = 2
            goto 11
          elseif(fstring(j:j) .eq. '3') then
            RN_XCO = 3
            goto 11
          elseif(fstring(j:j) .eq. '4') then
            RN_XCO = 4
            goto 11
          elseif(fstring(j:j) .eq. '5') then
            RN_XCO = 5
            goto 11
          endif
        end do

11      j2 = j + 1
        if(RN_XCO .gt. 2) then
           if( flag1 .lt. 2) then
             call lw_update(3,'CCU: Eff precip method error')
             select case (scu_debug)
             case (0)
             case (1)
              write(999,*) 'A daily effective precipitation method canno 
     :t be selected with the Blaney-Criddle analysis. No effective preci
     :pitation will be calculated.'
             case default
             end select
             RN_XCO = 0
           endif
        endif
        if(RN_XCO .eq. 3) then
          read(fstring(j2:40),*) cn(1)
        elseif(RN_XCO .eq. 4) then
          read(fstring(j2:40),*) cn(1)
        elseif(RN_XCO .eq. 5) then
          read(fstring(j2:40),*) (cn(i),i=1,3)
        endif
c
c  ew  - iclim = 0, climate station scenarion (unit cu), = 1, structure scenario
c
      READ(1,*,ERR=913) ICLIM
c
c if climate station scenario (iclim=0) do not read remainder of *.ccu file
c
       if(iclim .eq. 0) goto 230
c
c -----Read Flag for calculating water supply, 0 = crop irrigation water reqt only
c                                              1 = water supply limited
c                                              2 = water rights considered
c                                              3 = return flows and water rights considered
c                                              4 = groundwater considered
      READ(1,*,ERR=918) ISUPLY
      select case (isuply)
      case(0)
      case(1)
      case(2)
c      note that option 3 has been removed now      
c      case(3)
      case(4)
      case default
c       illegal value for isuply      
        call myexit(20)
      end select
c
c read climate station file
c

230   if(clifile .eq. '') then
        write(999,*) 'No climate station file defined in the *.rcu file'
        write(*,*) 'No climate station file defined in the *.rcu file'
         stop
      endif   
      write(999,*) 'Reading Climate Station File:  ',clifile
      write(*,*) 'Reading Climate Station File:  ', clifile
      open(unit=28,file=clifile,status='old',iostat=ierr)
      call skipn(28)
      n_sta=0
c     make this a bit more compatible with other OS
c200   read(28, 816, end=210) tid,tlat,telev,tzh,tzm
200   read(28, '(a200)', end=210) fline
      if(len(fline).lt.3)goto 210
      read(fline, 816, end=210) tid,tlat,telev,tzh,tzm
      n_sta=n_sta+1
      if(flag1 .ge. 3) then
         wsid(n_sta)=tid
         wsid(n_sta)=tid
         wlat(n_sta)=tlat
         welev(n_sta) = telev*ft_m
       if(tzh .eq. 0 .or. tzm .eq. 0) then
         call lw_update(4,tid)
         select case (scu_debug)
         case (0)
         case (1)
        write(999,*) 'height of climate station readings for station ',
     :tid, 'was not set in climate station file, default of 1.5m for tem
     :perature height and 2m for wind height is used'
         case default
         end select
         tzh = 4.92
         tzm = 6.56
       endif
         zh(n_sta) = tzh*ft_m
         zm(n_sta)= tzm*ft_m
      else
         wsid(n_sta)=tid
         wlat(n_sta)=tlat
         welev(n_sta)=telev
      endif
      go to 200
210   close(28)

c
c ew - if flag1 .eq. 2, skip to other uses calculation
c      if flag1 .ge. 3, read penman monteith control file to
c      get the names of the climate files, then skip to read climate weights
c
       if(flag1 .ge. 3)  then
          CALL PMCLIM
          goto 231
       endif

c
c rb- extract climate station names from time series data
c
c jhb 2006 cli file now defines the master climate station list
c jhb 2006 all therefore climate stations in time series files MUST be in the cli file
c jhb 2006       write(999,*) 'Extracting climate stations from time series files'
c jhb 2006       write(*,*) 'Extracting climate stations from time series files'
      if(tmpfile .eq. '') then
       write(999,*) 'No temperature data file defined in the *.rcu file'
       write(*,*) 'No temperature data file defined in the *.rcu file'
         stop
      endif   
      write(*,*) 'Reading Monthly Temperature File: ', tmpfile
      write(999,*) 'Reading Monthly Temperature File: ', tmpfile

      open(unit=26,file=tmpfile,status='OLD',iostat=ierr)
c      call skipn(26)
c
c rb- assume years of data stamped in tmpfile is range of data
c

 1112 read(26,'(a200)',end=106) fline   !read the raw line into a string variable
      if(len(fline).eq.0)goto 1112      !if it's a blank line, read the next line
      if(fline(1:1).eq."#")goto 1112    !if it's a comment line read the next line
      read(fline,907) gnyr1,gnyr2,idum3
      GNYRS = GNYR2 - GNYR1 + 1
      if(idum3.ne.'CYR') then
         write(*,*) 'Stop-temperature data not on calendar year basis'
         write(999,*) 'Stop-temperature not on calendar year basis'
         stop
      endif
      itempt=0
      ptyr=0
105   read(26,'(a200)',end=106)fline   !read the raw line into a string variable
      if(len(fline).eq.0)goto 105      !if it's a blank line, read the next line
      if(fline(1:1).eq."#")goto 105    !if it's a comment line read the next line
      read(fline,910,end=106) tyr,tid
      itempt=itempt+1
      if(ptyr.ne.tyr.and.itempt.ne.1) goto 106
      cid(itempt)=tid
      ptyr=tyr
      goto 105
106   close(26)
      itempt=itempt-1
c jhb 2006 check the climate stations in the monthly temp file against the cli file
c jhb 2006 this will be enforced as a 1-1 relationship from now on
        if(n_sta.ne.itempt)then
          write(*,*)'Error: Monthly temperature file ',
     &'has a different number of climate stations (',itempt,
     &') than the climate station file (',n_sta,'). ',
     &'Correct the files before proceeding.  Exiting StateCU.'
          write(999,*)'Error: Monthly temperature file ',
     &'has a different number of climate stations (',itempt,
     &') than the climate station file (',n_sta,'). ',
     &'Correct the files before proceeding.  Exiting StateCU.'
          stop
        end if
        do i=1,n_sta
            imatch=0
            do j=1,itempt
              if(trim(wsid(i)).eq.trim(cid(j)))then
                imatch=1
                exit
              end if
            end do
            if(imatch.eq.0)then
              write(*,*)'Climate station ',wsid(i),
     &' in the Temperature Station data file  (*.tmp) '//
     &' is not included in the climate station file (*.cli). '//
     &' There must be a one to one correspondence between the '//
     &'two files. Exiting StateCU.'
              write(999,*)'Climate station ',wsid(i),
     &' in the Temperature Station data file  (*.tmp) '//
     &' is not included in the climate station file (*.cli). '//
     &' There must be a one to one correspondence between the '//
     &'two files. Exiting StateCU.'
              stop
            end if
        end do

      IF ((GNYR1.GT.NYR1).OR.(GNYR2.LT.NYR2)) THEN
         WRITE(*,*) 'STOP-Temperature data not available for requested s 
     &imulation period'
       WRITE(999,*) 'STOP-Temperature data not available for requested s
     &imulation period'
         STOP
      ENDIF
      
      if(rn_xco.gt.0)then
        itempp=0
        ptyr=0

      if(pptfile .eq. '') then
        write(999,*) 'No precipitation data file defined in the '//
     &'*.rcu file'
        write(*,*) 'No precipitation data file defined in the '//
     &'*.rcu file'
      endif   

        write(*,*) 'Reading Monthly Precipitation File:  ', pptfile
        write(999,*) 'Reading Monthly Precipitation File:  ', pptfile
        open(unit=227,file=pptfile,status='old',iostat=ierr)
c        call skipn(27)
 1111   read(227,'(a200)',end=109) fline !read the raw line into a string variable
        if(len(fline).eq.0)goto 1111    !if it's a blank line, read the next line
        if(fline(1:1).eq."#")goto 1111  !if it's a comment line read the next line
        read(fline,907,end=109) pyr1,pyr2,idum3
        if(idum3.ne.'CYR') then
            write(*,*) 'Stop-precip data not on calendar year basis'
            write(999,*) 'Stop-precip data not on calendar year basis'
            stop
        end if
  107   read(227,'(a200)',end=109) fline  !read the raw line into a string variable
        if(len(fline).eq.0)goto 107       !if it's a blank line, read the next line
        if(fline(1:1).eq."#")goto 107     !if it's a comment line read the next line
        read(fline,910,end=109) tyr,tid
        if (ptyr.ne.tyr.and.ptyr.ne.0) goto 109
        ptyr=tyr
        itempp=itempp+1
        cid(itempp)=tid
        go to 107
  109   close(227)
        IF ((pYR1.GT.NYR1).OR.(pYR2.LT.NYR2)) THEN
         WRITE(*,*) 'STOP-Precipitation data not available for requested
     & simulation period'
       WRITE(999,*) 'STOP-Precipitation data not available for requested
     & simulation period'
          STOP
        ENDIF
c jhb 2006 check the climate stations in the monthly precip file against the cli file
c jhb 2006 this will be enforced as a 1-1 relationship from now on
        if(n_sta.ne.itempp)then
          write(*,*)'Error: Monthly precipitation file ',
     &'has a different number of climate stations (',itempp,
     &') than the climate station file (',n_sta,'). ',
     &'Correct the files before proceeding.  Exiting StateCU.'
          write(999,*)'Error: Monthly precipitation file ',
     &'has a different number of climate stations (',itempp,
     &') than the climate station file (',n_sta,'). ',
     &'Correct the files before proceeding.  Exiting StateCU.'
          stop
        end if
        do i=1,n_sta
            imatch=0
            do j=1,itempp
              if(trim(wsid(i)).eq.trim(cid(j)))then
                imatch=1
                exit
              end if
            end do
            if(imatch.eq.0)then
              write(*,*)'Climate station ',wsid(i),
     &' in the Precipitation Station data file  (*.prc) '//
     &' is not included in the climate station file (*.cli). '//
     &' There must be a one to one correspondence between the '//
     &'two files. Exiting StateCU.'
              write(999,*)'Climate station ',wsid(i),
     &' in the Precipitation Station data file  (*.prc) '//
     &' is not included in the climate station file (*.cli). '//
     &' There must be a one to one correspondence between the '//
     &'two files. Exiting StateCU.'
              stop
            end if
        enddo
      ENDIF

      call sb_init !initialize the "sub-basin" i.e. district arrays

      if(strfile .eq. '') then
        write(999,*) 'No structure file defined in the '//
     &'*.rcu file'
        write(*,*) 'No structure file defined in the '//
     &'*.rcu file'
      endif   
    
         write(*,*) 'Reading Structure File:  ', strfile
         write(999,*) 'Reading Structure File:  ', strfile
231      open(unit=30,file=strfile,status='old',iostat=ierr)
         nbasin=0
c        skip the initial header lines         
         call skipn(30)
c        logic is to keep reading structure records
c         (and loop through associated climate station records)
c         until the end of the strfile is reached, then jump to record 445
400      read(30,906,end=445) twdid,tlat,telev,tcounty,thuc,tname,ncli,
     &                        twcr
         nbasin=nbasin+1
         IF(NBASIN .GT. DIM_NA) CALL MYEXIT(62)
         tbasin(1:12)=twdid
         tbasin(13:40)=tname
         bas_id(nbasin)=tbasin
         blat(nbasin)=tlat
         belev(nbasin)=telev
         awcr(nbasin)=twcr
c        put this new strucutre in the appropriate subbasin
         call sbupdate(nbasin,twdid)
         do i=1,n_sta
             wws(nbasin,i)=0.0
             wrs(nbasin,i)=0.0
             ota(nbasin,i)=0.0
             opa(nbasin,i)=1.0
         enddo
         rlat(nbasin)=0.0
         relev(nbasin)=0.0
Cjhb=&==================================================================
C        make sure the # climate stations is >0
Cjhb=&==================================================================
         if(ncli.eq.0)then
             write(*,*) 'Error: Structure ', twdid,
     &       ' has no assigned climate stations. Stop.'
             write(999,*) 'Error: Structure ', twdid,
     &       ' has no assigned climate stations. Stop.'
             stop
         endif
Cjhb=&==================================================================
         do 401 ii=1,ncli
          imatch=0
Cjhb=&==================================================================
C         read(30,924,end=445) tid, twt, pwt
          read(30,924,end=445) tid, twt, pwt, OptArgs
Cjhb=&==================================================================
           do 405 i=1,n_sta
           if(wsid(i) .eq. tid) then
             wws(nbasin,i)=twt
             wrs(nbasin,i)=pwt
             rlat(nbasin)=rlat(nbasin)+wlat(i)*twt
             relev(nbasin)=relev(nbasin)+welev(i)*twt
             imatch=1
Cjhb=&==================================================================
C            if this is a monthly BC analysis AND
C            if this is a structure scenario AND
C                 [jhb 11-28-06 allow oro adj on climate station scenarios]
C            if there are extra params on the climate station assignment line AND
C            if both elevs exist (<>0),
C            ONLY THEN then use oro adj if they can be read...
C            note: this logic BREAKS if a real elev=0.0 is ever used ...
C                  assumed odds of this in colorado are ZERO, so no harm?
Cjhb=&==================================================================
             if(flag1.eq.1) then
                 if(len(trim(optargs)).gt.0) then
C                   read(OptArgs,'(2F9.2)') ota(nbasin,i), opa(nbasin,i)
                    read(OptArgs,*,end=666) ota(nbasin,i), opa(nbasin,i)
                     go to 667
666          write(*,*)'Error reading orographic adjustment parameters.'
                  write(*,*)'for structure: ',twdid,' and station: ',tid
           write(999,*)'Error reading orographic adjustment parameters.'
                write(999,*)'for structure: ',twdid,' and station: ',tid
                     write(*,*)'Please correct.'
                     write(999,*)'Please correct.'
                     stop
Cjhb=&==================================================================
C                    if oro adj for precip exists on line, save it
C                    note: this logic BREAKS if a oro prec adj=0.0 is ever used ...
C                    however, oro precip adj of zero causes all
C                    precip to be multiplied by 0 (all precip zeroed out)
C                    this value is very unlikely to ever be used as an orographic adj
C                    since the user can zero out precip other ways...
C                    so no harm in this assumption
Cjhb=&==================================================================
667                  if(opa(nbasin,i).ne.0.0.and.
     &                  opa(nbasin,i).ne.1.0) then
                       call lw_update(6,bas_id(nbasin))
                       select case (scu_debug)
                       case (0)
                       case (1)
                         write(999,*)
     &'Found and used orographic precipitation adjustment ',
     &'for structure ',tbasin,
     &'.  Multiplying precipitation of station ',tid,
     &' by ', opa(nbasin,i)
                       case default
                       end select
                     endif !opa(nbasin,i).ne.0.0
                   if((belev(nbasin).ne.0.0).and.(welev(i).ne.0.0)) then
Cjhb=&==================================================================
C                    if oro adj for temp exists on line, save it
C                    use it later if it <> 0
C                    note: this logic BREAKS if a oro temp adj=0.0 is ever used ...
C                    however, temperature adj of zero deg per 1000m means NO
C                    adjustment, the same as if it is missing,
C                    so no harm in this assumption
Cjhb=&==================================================================
                     if(ota(nbasin,i).ne.0.0) then
Cjhb=&==================================================================
C                      Convert the deg F decrease per 1000' elevation increase
C                      (from climate station to structure)
C                      into the actual deg F adjustment to use later.
C                      I.e. ADD ota(nbasin,i) to the temperature values for
C                      station i to get temperature values for structure
C                      nbasin.
C                      The typical value is 3.6 deg F temperature DECREASE 
C                      for each 1000' feet of elevation INCREASE.
C                      NOTE: the value in the STR file should be POSITIVE!!!!!!!!!!!
Cjhb=&==================================================================
                       ota(nbasin,i)=(belev(nbasin)-welev(i)) / 1000.0 * 
     &                               (-ota(nbasin,i))
                       call lw_update(5,bas_id(nbasin))
                       select case (scu_debug)
                       case (0)
                       case (1)
                         write(999,*)
     &'Found and used orographic temperature adjustment ',
     &'for structure ',tbasin,
     &'.  Adding ',ota(nbasin,i),
     &' degrees F to temperatures of station ',tid
                       case default
                       end select
                     endif !ota(nbasin,i).ne.0.0
                   else 
                    read(OptArgs,*) ota(nbasin,i), opa(nbasin,i)
                    if(ota(nbasin,i) .ne. 0.0) then
                       call lw_update(58,bas_id(nbasin))
                    endif   
                   endif !(belev(nbasin).ne.0.0).and.(welev(i).ne.0.0)
                 endif !len(trim(optargs)).gt.0
             endif !flag1.eq.1
Cjhb=&==================================================================
           endif !wsid(i) .eq. tid
405        continue
           if(imatch .eq. 0) then
            write(*,*) 'Climate Station ', trim(tid), 'assigned in '//
     :'structure file does not match station in Climate Station '//
     :'file (*.cli)'
           write(999,*) 'Climate Station ', trim(tid), 'assigned in '//
     :'structure file does not match station in Climate Station '//
     :'file (*.cli)'
             stop
           endif
401     continue
c
c if latitude or elevation missing in database, use weighted temperature
c station values
c 
        if(blat(nbasin) .eq. 0) then
           call lw_update(62,bas_id(nbasin))                   
           blat(nbasin)=rlat(nbasin)
        endif
        if(belev(nbasin) .eq. 0) then
           call lw_update(65,bas_id(nbasin))                   
           belev(nbasin)=relev(nbasin)
        endif

       go to 400

445    close(30)
       call sb_final
c
c check to see if climate station weights = 1
c
      do i=1,nbasin
         testt=0
         testp=0
         do j=1,n_sta
           testt=testt+wws(i,j)
           testp=testp+wrs(i,j)
         enddo
Cjhb=&==================================================================
C        add code to also check for weights summing to 0
Cjhb=&==================================================================
         if(testt .lt. 0.01) then
            write(999,*) 'Error: Temperature weights for structure',
     :       bas_id(i), 'sum to 0. No ET can be calculated. Stop.'
            write(*,*) 'Error: Temperature weights for structure',
     :       bas_id(i), 'sum to 0. No ET can be calculated. Stop.'
            stop
         endif
Cjhb=&==================================================================
         if(testt .lt. 0.999 .or. testt .gt. 1.001) then
           call lw_update(7,bas_id(i))
           select case (scu_debug)
           case (0)
           case (1)
           write(999,*)
     &'Warning: Temperature Weights for structure',
     &bas_id(i), 'do not add up to 1.0'
           case default
           end select
         endif
         if(testp .lt. 0.999 .or. testp .gt. 1.001) then
           call lw_update(8,bas_id(i))
           select case (scu_debug)
           case (0)
           case (1)
           write(999,*)
     &'Warning: Precipitation weights for structure',
     &bas_id(i), 'do not add up to 1.0'
           case default
           end select
         endif
      enddo
c
c ew- finish reading .CCU file (if iclim=1)
c      if iclim=0, set default output to *.iwr file and *.obc file
c
      if(iclim .eq. 0) then
        do  i=1,nbasin
           typout(i)=1
        enddo
        goto 446
      endif
c
C-----Read General Output Options
c
      READ(1,*,ERR=918) SOUT
c
c-----Read flag to consider soil moisture
c      ISM = 0, do not consider, =1 consider user defined initial
c               2 consider run presimulation to initialize
c  
      READ(1,*,ERR=918) ISM
      if(ism .ne. 0) then 
         do i=1,nbasin
           if(awcr(i) .eq. 0.0) then
             call lw_update(63,bas_id(i))
           endif
         enddo
      endif   
c
C-----Read general Soil Moisture capacity for senior, junior, and other parcels
c    
c      read(1,'(3f5.0)')psenmo,pjunmo,pothmo
      read(1,*,ERR=918)psenmo,pjunmo,pothmo
c
C-----Read switch to operate soil moisture by proration(0) or priority(1)
c    
      read(1,*) iprtysm
c
C-----Read fraction of winter precip available to soil moisture
c    
c      read(1,*) ismcar
c      smef = ismcar/100.0
      read(1,*) smcar
      smef = smcar
c
c rb-   read default report level, modify with replace file 
c
cjhb &==================================================================
cjhb  add second parameter, space delimited from first, within the first 20 characters
cjhb  this is the structure ID for the four category land output file
cjhb &==================================================================
      obcout=1
c      read(1,*) ttypout
      read(1,'(A200)') fline
      read(fline(1:20),*)ttypout
c     check for sub basin output in binary output (added 10 to typout value)
      select case (ttypout)
      case (10,11,12,13,14)
        sboutput=.true.
        ttypout=ttypout-10
      case default
        sboutput=.false.
      end select
c     now handle the normal typout value (has the 10 subtracted)
      select case (ttypout)
      case (0,1,2,3)
        s4catid=""
      case (4)
        read(fline(1:20),*,ERR=454)idum,tempid
        s4catid=trim(tempid)
454     ttypout=3
      case default
        s4catid=""
      end select
      
      do 455 i=1,nbasin
455     typout(i)=ttypout 
c
c ew - output detailed info for gw modeling (gw withdrawal for sprinkler, flood)
c
      iflood=0
      READ(1,'(I6)',ERR=918) IFLOOD
c
c _________________________________________________________
c
c rrb 2003/06/20; Allow multiple subirrigated crops
c                 where the default is 1 (if on) and it is grass
c                 else read subirrigated crop types using
c                 StateCU crop names
      do if1=1,5
        subname(if1)=' '
      end do

      if(iflood.ge.1) then
        do if1=1,iflood
          read(1,'(a5)',err=918) subname(if1)
        end do
      endif
c
c     subirrigation irrigation is only compatible with BC monthly runs right now,
c     because of the gtemp file.
      if(flag1.ne.1.and.iflood.gt.0)then
        iflood=0
           call lw_update(9,'IFLOOD')
           select case (scu_debug)
           case (0)
           case (1)
           write(999,*)
     &'Warning: subirrigated crops currently not allowed ' //
     &'when using daily ET methods.  IFLOOD reset to 0.'
           case default
           end select
      endif
c
c rb-   read switch to create statemod output
c       ddcsw=0, do not write .ddc file, =1, write in CYR, =2, write in WYR

      read(1,*) ddcsw
c
c ew - read administration number option when considering water rights (isuply=2)
c
      READ(1,'(i5)',ERR=918) idaily
c
      IF(ISUPLY.ne.2 .and. isuply .ne. 3) IDAILY=0
c ew - read administration number when idaily = 3 or 5
      read(1,'(f11.0)') adminent
      if(adminent .eq. 0) then
         adminent = 1000000
      endif
c ew - read flag for drain/tailwater file
c ew - read flag for prorating aggregate structure WSL/diversions or using 
c                diversions from ddhfile
c
      idrain=0
      iagg = 0
      imiss2 = 0
      READ(1,'(i5)',ERR=918,END=446) idrain
      select case (idrain) !drain/tailwater data flag - use additional farm water supply in DRA file
      case (0) !no DRA file data
      case (1:2) !use drain file data
        select case (idra) !drain filename flag from reading RCU
        case (0) !no DRA filename
          call lw_update(13,'IDRAIN')
          select case (scu_debug)
          case (0)
          case (1)
          write(999,*)
     &'IDRAIN flag set to 1 or 2 indicating supplemental water ' //
     &'source from return flows or drains, but the supplemental ' //
     &'source file (*.DRA) was not provided in the RCU. ' //
     &'The IDRAIN value is reset to 0.'
          case default
          end select
          idrain=0
        case default !DRA filename is set
        end select
      case default !invalid IDRAIN value
        call lw_update(14,'IDRAIN')
        select case (scu_debug)
        case (0)
        case (1)
        write(999,*)
     &'Invalid IDRAIN flag value in CCU file.  Reset to 0'
        case default
        end select
        idrain=0
      end select
c
c    read flag to automatically estimate aggregate structure CU based on key structure shortages
c

Cjhb=&==================================================================
C     removed iagg and use imiss to prorate aggr data
C      (i.e. set aggr diversion to -999 to get it prorated)
Cjhb=&==================================================================
c     READ(1,'(i5)',ERR=918,END=446) iagg
c
c    read flag to automatically fill CU for structures with missing diversion records
c      based on water district shortages (=0 don't fill, =1 fill)
c      add new options - 2 = fill missing clim time series data with monthly averages, fill missing div time series data with monthly averages
c      add new options - 3 = fill missing clim time series data with monthly averages, fill missing div time series data with 0's
c      add new options - 4 = fill missing clim time series data with monthly averages, do not fill missing div time series data
c      add new options - 5 = do not fill missing clim time series data, fill missing div time series data with 0's
c      add new options - 6 = do not fill missing clim time series data, fill missing div time series data with monthly averages
c
      READ(1,'(i5)',ERR=918,END=446) imiss2
      select case (imiss2)
      case (0)
        imiss2=0
        climonfly=0
        divonfly=0
        fillonfly=.false.
      case (1)
        imiss2=1
        climonfly=0
        divonfly=0
        fillonfly=.false.
      case (2)
        imiss2=0
        climonfly=1
        divonfly=1
        fillonfly=.true.
      case (3)
        imiss2=0
        climonfly=1
        divonfly=2
        fillonfly=.true.
      case (4)
        imiss2=0
        climonfly=1
        divonfly=0
        fillonfly=.true.
      case (5)
        imiss2=0
        climonfly=0
        divonfly=2
        fillonfly=.true.
      case (6)
        imiss2=0
        climonfly=0
        divonfly=1
        fillonfly=.true.
      case default
        imiss2=0
        climonfly=0
        divonfly=0
        fillonfly=.false.
      end select

Cjhb=&==================================================================
c    read deficit irrigation fraction record.  it will default to 1
c    if the record is not in the CCU
Cjhb=&==================================================================
      def_irr = 1.0
      READ(1,'(i5)',ERR=918,END=446) itmp
      def_irr = itmp/100.0
c
446   close(1)

c
c rb- read crop characteristics file
C     Read Crop Data - assume no consideration of crop for different soil types
c
Cjhb=&==================================================================
c      k=1
      k=0
Cjhb=&==================================================================
      if(cchfile .eq. '') then
        write(999,*) 'No crop characteristic file defined in the '//
     &'*.rcu file'
        write(*,*) 'No crop characteristic file defined in the '//
     &'*.rcu file'
      endif   
      write(*,*) 'Reading Crop Characteristics File:  ', cchfile
      write(999,*) 'Reading Crop Characteristics File:  ', cchfile
      open(unit=31,file=cchfile,status='old',iostat=ierr)
      call skipn(31)
Cjhb=&==================================================================
c500   read(31,929,end=505) cropn
500   read(31,*,end=505) cropn
Cjhb=&==================================================================
C     replace the following
C     allow alfalfa coefficients to be stored like other crops
C     (i.e. don't put them in array position 1)
C     instead, check the crop name
Cjhb=&==================================================================
c      if(cropn .eq. 'ALFALFA') THEN
c        ksave=k
c        k = 1
c      else
c        k=k+1
c      endif
Cjhb=&==================================================================
      k=k+1
Cjhb=&==================================================================
      if(cropn(1:10) .eq. 'WHEAT_FALL') iwheat=k
      backspace(31)
Cjhb=&==================================================================
c     IF (k.EQ.1) THEN         ! alfalfa
      IF (cropn(1:7).EQ.'ALFALFA') THEN         ! alfalfa
Cjhb=&==================================================================
c         READ(31,936,End=505) cpname(k),CKEY(K),GDATE1(K),GDATE2(K),
          READ(31,*,End=505) cpname(k),CKEY(K),GDATE1(K),GDATE2(K),
Cjhb=&==================================================================
     :      GDATE3(K),GDATE4(K),GDATE5(K),GDATES(K),TMOIS1(K),
     :      TMOIS2(K),MAD(K),IRZ(K),FRZ(K),AWC(K),APD(K),TFLG1(K),
     :      TFLG2(K),CUT2(K),CUT3(K)
Cjhb=&==================================================================
C         hardwire alfalfa cut dates to 45 if they are missing
Cjhb=&==================================================================
          if (cut2(k).eq.0) then
             cut2(k)=45
             WARNINGS=.TRUE.
      write(999,*)'Warning: Missing alfalfa cut date 2 in CCH file. ',
     &       'Crop name: ', cpname(k),' Set to default of 45.'
          end if
          if (cut3(k).eq.0) then
             cut3(k)=45
             WARNINGS=.TRUE.
      write(999,*)'Warning: Missing alfalfa cut date 3 in CCH file. ',
     &       'Crop name: ', cpname(k),' Set to default of 45.'
          end if
Cjhb=&==================================================================
          ckey(k)=k
          soil(k)='NO SOIL'
          n_crp(k)=k
          n_soil(k)=13
Cjhb=&==================================================================
c          k=ksave
Cjhb=&==================================================================
      ELSE
Cjhb=&==================================================================
c         READ(31,936,End=505) cpname(k),CKEY(K),GDATE1(K),GDATE2(K),
          READ(31,*,End=505) cpname(k),CKEY(K),GDATE1(K),GDATE2(K),
Cjhb=&==================================================================
     :      GDATE3(K),GDATE4(K),GDATE5(K),GDATES(K),TMOIS1(K),
     :      TMOIS2(K),MAD(K),IRZ(K),FRZ(K),AWC(K),APD(K),TFLG1(K),
     :      TFLG2(K)
Cjhb=&==================================================================
c          if (cpname(k).eq.'                    ') go to 505
Cjhb=&==================================================================
          ckey(k)=k
          soil(k)='NO SOIL'
          n_crp(k)=k
          n_soil(k)=13
      ENDIF
      IRZ(K) = FRZ(K)        ! assumed no change in root depth
      goto 500

505   close(31)
      n_crps=k
c
c
510    if(isuply .gt. 0) then 
         write(*,*) 'Reading Water Supply File:  ', ddhfile
         write(999,*) 'Reading Water Supply File:  ', ddhfile
         call slimit
       endif
c
c rb-  read irrigated acreage in cds format
c ew - read irrigated acreage in new cds format
c       read crop names instead of crop numbers
c
      do j1=1,nbasin
         iacre(j1) = 0
      enddo

      if(cdsfile .eq. '') then
        write(999,*) 'No crop acreage file defined in the '//
     &'*.rcu file'
        write(*,*) 'No crop acreage file defined in the '//
     &'*.rcu file'
      endif   
      write(*,*) 'Reading Crop Acreage File:  ', cdsfile
      write(999,*) 'Reading Crop Acreage File:  ', cdsfile
      open(unit=32,file=cdsfile,status='old',iostat=ierr)
      call skipn(32)
      READ(32,907) gnyr1,gnyr2,idum3

      GNYRS = GNYR2 - GNYR1 + 1

      IF ((GNYR1.GT.NYR1).OR.(GNYR2.LT.NYR2)) THEN
         WRITE(*,*) 'STOP-Acreage data not available for requested simu 
     &lation period'
       WRITE(999,*) 'STOP-Acreage data not available for requested simu
     &lation period'
         STOP
      ENDIF


Cjhb=&==================================================================
C     change to unformatted read
Cjhb=&==================================================================
C515   READ(32,935,End=570) tyr,tid,ttacre,tncrop
515   READ(32,*,End=570) tyr,tid,ttacre,tncrop
Cjhb=&==================================================================
c     reset any negative acreages to 0
        if(ttacre.lt.0.0) then
            ttacre=0.0
            WARNINGS=.TRUE.
            write(999,*)'WARNING: CDS file: ',
     &          'Year: ',tyr,' Structure: ',tid,
     &          ' Total acreage < 0.  Reset to 0.'
        endif
Cjhb=&==================================================================
      bUseAcreage = .TRUE.
      DO 520 K1=1,tncrop
      ICROP=0
Cjhb=&==================================================================
C     change to unformatted read - note: only reads first two parameters
C     11/13/2007 change to try to read the 3rd field on the record - the acreage field
C                set acreage to -999. and see if it gets changed
C                if not, use the percentage
Cjhb=&==================================================================
C     READ(32,942,END=530) cropn,tpct(k1)
c      READ(32,*,END=530) cropn,tpct(k1)
      READ(32,'(A200)',END=530)fline
      call CountArgs(fline,ArgCount)
      select case (ArgCount)
      case (2)
        READ(fline,*) cropn,tpct(k1)
        bUseAcreage = .FALSE.
      case (3)
        READ(fline,*) cropn,tpct(k1),tcdsarea(k1)
      case default
        write(*,*)' Error reading CDS file; line = ',fline
        write(*,*)' ID = ',tid,' Year = ',tyr,' crop # ',k1
        write(*,*)' Program stopped'
        write(999,*)' Error reading CDS file; line = ',fline
        write(999,*)' ID = ',tid,' Year = ',tyr,' crop # ',k1
        write(999,*)' Program stopped'
        stop
      end select
Cjhb=&==================================================================
      DO 525 KK= 1, dim_nc
Cjhb=&==================================================================
C     change comparison to trim strings first
Cjhb=&==================================================================
C        IF(cpname(kk).EQ.cropn) THEN
         TMPSTR1=trim(cpname(kk))
         TMPSTR2=trim(cropn)
         IF(TMPSTR1.EQ.TMPSTR2) THEN
Cjhb=&==================================================================
            tbkey(K1)=KK
            ICROP=1
            goto 520
         ENDIF
525   CONTINUE
      IF(ICROP.EQ.0) then
        write(*,*) 'Stop. File *.cds crop name ',cropn,' does not match 
     :crops identified in crop characteristic *.cch file.'
        write(999,*) 'Stop. File *.cds crop name ',cropn,' does not matc
     :h cropss identified in crop characteristic *.cch file'
       stop
      endif
520   CONTINUE
      if (tyr.lt.nyr1.or.tyr.gt.nyr2) goto 515
530   do 565 j1=1,nbasin !loop through structures to find the one that matches
         twdid=bas_id(j1) 
         if(twdid(1:12).eq.tid) then !found it
           if(bUseAcreage)then !use acreage values
             iacre(j1) = 1
             !check sum of acreages against total
             tcdssum=0.0
             do 535 j2=1,tncrop
               tcdssum=tcdssum+tcdsarea(j2)
535          continue
             if(abs((ttacre-tcdssum)/ttacre).GT.0.02)then
                call lw_update(60,bas_id(j1))
             endif
             tyr1=tyr-nyr1+1
             do 536 j2=1,tncrop
               bkey(j1,j2,tyr1)=tbkey(j2)
               if(iclim .eq. 1) then
                 area(j1,j2,tyr1)=tcdsarea(j2)
               else
                 area(j1,j2,tyr1)=tcdsarea(j2)*100.0
               endif
536          continue
           else !use percentages
             tpct1=0
             iacre(j1) = 1
c            ew - If crop percentages from GIS database do not equal 1.000,
c                 adjust the crop types proportionally so total percent = 1.000.
             do 545 j2=1,tncrop
               tpct1=tpct1+tpct(j2)
545          continue
             if(tpct1 .eq. 0) goto 551 !?
             if(tpct1 .ge. 1.001 .or. tpct1 .le. 0.999) then
c              crop %s need to be adjusted to sum to 1.0 exactly
               if( (tpct1.gt.1.02) .or. (tpct1.lt.0.98) ) then
c                sum of crop %s are far off enough from 100% to warrant a warning message in the log file
                 WARNINGS=.TRUE.
                 write(999,946) tid,tpct1
               endif
               tpctadj=1.000-tpct1
               do 550 j2=1,tncrop
                 tpct(j2)=tpct(j2)+(tpct(j2)*tpctadj/tpct1)
550            continue
             endif
551          tyr1=tyr-nyr1+1
555          do 560 j2=1,tncrop
               bkey(j1,j2,tyr1)=tbkey(j2)
               if(iclim .eq. 1) then
                 area(j1,j2,tyr1)=tpct(j2)*ttacre
               else
                 area(j1,j2,tyr1)=tpct(j2)*100
               endif
560          continue
           endif
           nparce(j1,tyr1)=tncrop
         endif
565   continue
      go to 515
570   close(32)
      
      do j1=1,nbasin
        if(iacre(j1) .eq. 0) then
          twdid=bas_id(j1)
          call lw_update(61,'bas_id(j1)')
          do j2=1,nyrs
             nparce(j1,j2)=0
          enddo
        endif
      enddo

c rb- read and process frost date data
c ew- allow for missing frost date data
c

      if(fdfile .eq. '') then
        write(999,*) 'No frost date data file defined in the '//
     &'*.rcu file'
        write(*,*) 'No frost date data file defined in the '//
     &'*.rcu file'
      endif   
      open(unit=33,file=fdfile,status='old',iostat=ierr)
      
      do 600 ib=1,nbasin
        do 600 i = 1,nyrs
          t28(ib,i,1) = 0
          t28(ib,i,2) = 0
          t32(ib,i,1) = 0
          t32(ib,i,2) = 0
         i1(ib,i)=0
         i2(ib,i)=0
         i3(ib,i)=0
         i4(ib,i)=0
600   continue
      itempf=0
      write(*,*) 'Reading Frost Date File:  ', fdfile
      write(999,*) 'Reading Frost Date File:  ', fdfile
      call skipn(33)
      read(33,907) t1,t2,idum3       
      if(idum3.ne.'CYR') then
         write(*,*) 'Stop-frost data not on calendar year basis'
         write(999,*) 'Stop-frost data not on calendar year basis'
         stop
      endif
      if(t1.gt.nyr1.or.t2.lt.nyr2) then
         write(*,*) 'Stop-frost data not for full study period'
         write(999,*) 'Stop-frost data not for full study period'
         stop
      endif 
605   read(33,977,end=620) tyr,tid,fd1,fd2,fd3,fd4
c====================================================================jhb
      if ((tyr.lt.nyr1).or.(tyr.gt.nyr2)) then
        goto 605
      endif
      if(tyr .eq. nyr1) then
         itempf=itempf+1
         cid(itempf) = tid
c jhb 2006 check the climate stations in the frost date file against the cli file
c jhb 2006 this will be enforced as a 1-1 relationship from now on
        imatch=0
        do i=1,n_sta
          if(trim(wsid(i)).eq.trim(cid(itempf)))then
            imatch=1
            exit
          end if
        end do
        if(imatch.eq.0)then
              write(*,*)'Climate station ',wsid(i),
     &' in the Frost Date data file  (*.prc) '//
     &' is not included in the climate station file (*.cli). '//
     &' There must be a one to one correspondence between the '//
     &'two files. Exiting StateCU.'
              write(999,*)'Climate station ',wsid(i),
     &' in the Frost Date data file  (*.prc) '//
     &' is not included in the climate station file (*.cli). '//
     &' There must be a one to one correspondence between the '//
     &'two files. Exiting StateCU.'
          stop
        end if
      endif
      if(fd1(1:6) .eq. '-999.0') then
         t28_1 = -999
      else
        read(fd1(2:3),'(i2)') t1
        read(fd1(5:6),'(i2)') t2
        t28_1 = julian(t1,t2)
      endif
      if(fd2(1:6) .eq. '-999.0') then
         t32_1 = -999
      else
        read(fd2(2:3),'(i2)') t3
        read(fd2(5:6),'(i2)') t4
        t32_1 = julian(t3,t4)
      endif
      if(fd3(1:6) .eq. '-999.0') then
         t32_2 = -999
      else
        read(fd3(2:3),'(i2)') t5
        read(fd3(5:6),'(i2)') t6
        t32_2 = julian(t5,t6)
      endif
      if(fd4(1:6) .eq. '-999.0') then
         t28_2 = -999
      else
        read(fd4(2:3),'(i2)') t7
        read(fd4(5:6),'(i2)') t8
        t28_2 = julian(t7,t8)
      endif

      ttyr=tyr-nyr1+1
      do 615 i=1,n_sta
         if (wsid(i).ne.tid) goto 615
         do 610 ib = 1,nbasin
             if(T28_1.gt. -998) then
               T28(ib,ttyr,1) = T28(ib,ttyr,1) + WWS(IB,I) * T28_1
             elseif(wws(ib,i) .gt. 0) then
               i1(ib,ttyr)=1
             endif
             if(T28_2.gt. -998) then
               T28(ib,ttyr,2) = T28(ib,ttyr,2) + WWS(IB,I) * T28_2
             elseif(wws(ib,i) .gt. 0) then
               i2(ib,ttyr)=1
             endif
             if(T32_1.gt. -998) then
               T32(ib,ttyr,1) = T32(ib,ttyr,1) + WWS(IB,I) * T32_1
             elseif(wws(ib,i) .gt. 0) then
               i3(ib,ttyr)=1
             endif
             if(T32_2.gt. -998) then
               T32(ib,ttyr,2) = T32(ib,ttyr,2) + WWS(IB,I) * T32_2
             elseif(wws(ib,i) .gt. 0) then
               i4(ib,ttyr)=1
             endif
610      continue
         goto 605
615   continue
620   continue
      close(33)
      if(n_sta.ne.itempf)then
          write(*,*)'Error: Frost date file ',
     &'has a different number of climate stations (',itempp,
     &') than the climate station file (',n_sta,'). ',
     &'Correct the files before proceeding.  Exiting StateCU.'
          write(999,*)'Error: Frost date file ',
     &'has a different number of climate stations (',itempp,
     &') than the climate station file (',n_sta,'). ',
     &'Correct the files before proceeding.  Exiting StateCU.'
        stop
      endif
         
         do 625 i=1,nyrs
         do 625 ib=1,nbasin
           if(i1(ib,i) .eq. 1) T28(ib,i,1)=-999
           if(i2(ib,i) .eq. 1) T28(ib,i,2)=-999
           if(i3(ib,i) .eq. 1) T32(ib,i,1)=-999
           if(i4(ib,i) .eq. 1) T32(ib,i,2)=-999
625      continue

      if(climonfly.gt.0)then
        call flyfillf
      endif

      if(flag1 .ge. 3) goto 700


C-----Calculate Total Area per Basin per Year - handy if
C-----project summary are desired to be in inches rather than acre-ft
700   do 705 I=1,DIM_NA
      do 705 J=1,DIM_NY
705      T_AREA(I,J) = 0.0
      do 710 I=1,NBASIN
      do 710 J=1,NYRS
      do 710 K=1,NPARCE(I,j)
        T_AREA(I,J) = T_AREA(I,J) + AREA(I,K,J)
710   continue
C-----Calculate total area of project
      do 715 J=1,NYRS
715      PJAREA(J) = 0.0

      do 720 I=1,NBASIN
      do 720 J=1,NYRS
720     PJAREA(J) = PJAREA(J) + T_AREA(I,J)


C-----Calculate total area by crops
      do 725 J=1,DIM_NY
      do 725 K=1,DIM_NC
725      C_AREA(J,K) = 0.0
      do 730 I=1,NBASIN
      do 730 J=1,NYRS
      do 730 K=1,NPARCE(I,j)
         KK = N_CRP(BKEY(I,K,J))
         C_AREA(J,KK) = C_AREA(J,KK) + AREA(I,K,J)
730   continue


C-----Start Calculation
735   select case (flag1)
        case(1)
          do k=1,n_crps
            crptyp(k)=0
          end do
          CALL READIN(nbegyr, nendyr)
          write(*,*) 'Summarizing input to file *.sum and processing'
          write(999,*) 'Summarizing input to file *.sum and processing'
          CALL SUMMARY
          write(*,*) 'Starting Blaney Criddle Process'
          write(999,*) 'Starting Blaney Criddle Process'
          CALL MAINXC
        case(2)
          write(*,*) 'flag1=2 "other uses"is no longer a valid option 
     &in StateCU - only crop consumptive use methods are supported '//
     &'Correct in *.ccu file.'
        write(999,*) 'flag1=2 "other uses"is no longer a valid option 
     &in StateCU - only crop consumptive use methods are supported '//
     &'Correct in *.ccu file.'
          stop
        case(3)
          do k=1,n_crps
            crptyp(k)=0
          end do
          write(*,*) 'Summarizing input to file *.sum and processing'
          write(999,*) 'Summarizing input to file *.sum and processing'
          CALL SUMMARY
          CALL DREAD
          CALL DSUM
          write(*,*) 'Starting Penman-Monteith Process'
          write(999,*) 'Starting Penman-Monteith Process'
          CALL PROTO
        case(4)
          do k=1,n_crps
            crptyp(k)=0
          end do
          write(*,*) 'Summarizing input to file *.sum and processing'
          write(999,*) 'Summarizing input to file *.sum and processing'
          CALL SUMMARY
          CALL DREAD
          CALL DSUM
          write(*,*) 'Starting Modified Hargreave Process'
          write(999,*) 'Starting Modified Hargreave Process'
          CALL PROTO
        case(5)
          do k=1,n_crps
            crptyp(k)=0
          end do
          write(*,*) 'Summarizing input to file *.sum and processing'
          write(999,*) 'Summarizing input to file *.sum and processing'
          CALL SUMMARY
          CALL DREAD
          CALL DSUM
          write(*,*) 'Starting ASCE Penman-Monteith Process'
          write(999,*) 'Starting ASCE Penman-Monteith Process'
          CALL PROTO
        Case Default
      end select


Cjhb &==================================================================
Cjhb  
Cjhb &==================================================================
      IF (ISUPLY.GE.1) THEN
        write(0,*)'Preparing Water Budget:'
        write(999,*)'Preparing Water Budget:'
c       operate a presimulation to initialize soil moisture  grb 5-04-00
        ipresim=0
        if (ism.eq.2) then
          ipresim=1
          write(0,*)
     &    '  presimulation to initialize soil moisture'
          call wsupsum
          write(0,*)
     &    '  finished'
          write(0,*)
     &    '  final simulation'
          ipresim=2
          ism=1
        endif
        CALL WSUPSUM
          write(0,*)
     &    '  finished'
      ENDIF
Cjhb &==================================================================
      write(0,*) 'Writing consumptive use summary: '
      write(999,*) 'Writing consumptive use summary '
      CALL PROJ

C-----Include water supply in input summary file
       IF (ISUPLY.GE.1) THEN
         DO 740 I=1,NBASIN
740        CALL WSUPPLY(I)
       ENDIF

c rb- create statemod (.ddc) file of output
      if (ddcsw.ge.1) then           
       write(0,*) 'Creating *.ddc file (statemod formatted results)'
       write(999,*) 'Creating *.ddc file (statemod formatted results)'
       ddcfile=dfile
       ddcfile(fn_len:fn_len+4) = '.ddc'
       open(unit=33,file=ddcfile,status='unknown',iostat=ierr)
       WRITE(33,937) 
       if(ddcsw .eq. 1) then
         WRITE(33,938) 1,'/',nyr1,'  -  ',12,'/',nyr2,' ACFT','  CYR'
       else
         WRITE(33,938) 1,'/',nyr1,'  -  ',12,'/',nyr2,' ACFT','  WYR'
       endif
       if(ddcsw .eq. 1) then
         DO 800 j=1,nyrs
          DO 805 i = 1,nbasin
             tbasin=bas_id(i)
             tyr=j+nyr1-1
             write(33,939) tyr,tbasin(1:12),(reqt(i,j,k),k=1,12)
805       CONTINUE
800     CONTINUE
       else
         DO 803 j=1,nyrs-1
          DO 804 i = 1,nbasin
             tbasin=bas_id(i)
             tyr=j+nyr1
             write(33,939) tyr,tbasin(1:12),(reqt(i,j,k),k=10,12),
     1                  (reqt(i,j+1,k),k=1,9)
804       CONTINUE
803     CONTINUE
       endif
      endif
      close(33)

c ew- create ground water pumping file (.gwp)
      if (isuply .eq. 4 .and. ddcsw .gt. 0) then
       write(0,*) 'Creating *.gwp file (ground water pumping file)'
       write(999,*) 'Creating *.gwp file (ground water pumping file)'
       gwfile=dfile
       gwfile(fn_len:fn_len+4) = '.gwp'
       open(unit=33,file=gwfile,status='unknown',iostat=ierr)
       WRITE(33,940) 
       if(ddcsw .eq. 1) then
         WRITE(33,938) 1,'/',nyr1,'  -  ',12,'/',nyr2,' ACFT','  CYR'
       else
         WRITE(33,938) 1,'/',nyr1,'  -  ',12,'/',nyr2,' ACFT','  WYR'
       endif
       if(ddcsw .eq. 1) then
         DO 802 j=1,nyrs
          DO 801 i = 1,nbasin
             tbasin=bas_id(i)
             tyr=j+nyr1-1
             write(33,939) tyr,tbasin(1:12),(gwp(i,j,k),k=1,12)
801       CONTINUE
802     CONTINUE
       else
         DO 806 j=1,nyrs-1
          DO 807 i = 1,nbasin
             tbasin=bas_id(i)
             tyr=j+nyr1
             write(33,939) tyr,tbasin(1:12),(gwp(i,j,k),k=10,12),
     1                  (gwp(i,j+1,k),k=1,9)
807       CONTINUE
806     CONTINUE
       endif
      endif
      close(33)
      goto 941

C-----Read error exit calls and format statements


816   format(a12,f6.0,f9.0,56x,2f6.0)
900   FORMAT(A120)
901   FORMAT(A40)
902   FORMAT(I2,A20)
903   FORMAT(I3,A20,A15)
905   format (a40)
906   format(a12,f6.0,f9.0,2x,a20,a8,2x,a24,i4,f8.0)
907   format(6x,i4,11x,i4,7x,a3)
908   FORMAT('StateCU Version ', f5.2,2x,a16)
909   CALL MYEXIT(7)
910   format(i4,1x,a12)
912   CALL MYEXIT(15)
913   CALL MYEXIT(18)
914   CALL MYEXIT(20)
915   format(i2,6x,f5.3,18x,a10,1x,i8)
916   format(a12,f6.0,f9.0,2x,a20,a8,2x,a20)
917   format(a12,1x,f7.0,16x,a10,i8,1x,f5.0,2x,f5.0)       
918   CALL MYEXIT(21)
919   format(a12,4x,i2)
920   format(48x,f8.0,i8)
921   format(a12,1x,i5)
922   format(1x,i8,1x,a12,1x,i2,10(1x,a20,f6.0))
923   FORMAT(A12, A24)        
Cjhb=&==================================================================
C924   FORMAT(a12, f6.0, f9.0)
924   FORMAT(a12, f6.0, f9.0, a40)
Cjhb=&==================================================================
925   FORMAT(1X,'warning- Structure ', A12,' has a demsrc of '
     :           , I3, ' and a DemCode of ',I3,' and an',/,
     :           ' area of ', F8.1,'. This is inconsistent.')
927   format('warning: Default crop types used for structure ',a12)
928   format('No default crop mix found for structure ',a12)
929   format(a20)
931   format(i8,i4)
932   format(12x,12f8.0)
933   format(a12)
934   format(4x,A20,F10.0)
935   format(i4,1x,a12,8x,F10.0,i10)
936   format(a20,1x,i2,2x,i2,1x,i2,2x,i2,2x,i2,2x,i4,1x,i4,1x,f3.0,1x,
     :  f3.0,1x,f3.0,1x,f4.0,1x,f4.0,1x,f4.0,1x,f4.0,1x,i2,1x,i2,1x,i3,
     :  1x,i3)   
937   FORMAT('# File of statemod formatted CU results generated by',
     : ' run-cu',54(" "))
938   FORMAT(I5,A1,I4,A5,I5,A1,I4,A5,A5,78(" "))
939   FORMAT(I4,1x,A12,12(F8.0))
940   FORMAT('# File of statemod formatted ground water pumping results
     : generated by StateCU',34(" "))
Cjhb=&==================================================================
C942   format(5x,a20,f10.0)
942   format(5x,a30,f10.0)
Cjhb=&==================================================================
946   format(' Warning: The crop percentages do not add up to 1.000 for 
     :structure ',a12,'(total percentage= ', f5.3,'),',/, '   Adjusting 
     :crop types proportionally so total = 1.000')
977   format(i4,1x,a12,2x,a6,3(2x,a6))
999   FORMAT(1x,'program is running...')
945   WRITE(*,*) 'Response file not found.  Exiting program'
      WRITE(999,*) 'Response file not found.  Exiting program'
      close(999)
      stop
941   continue
      call lw_ccu(999)
      call lw_write(999)
      if(WARNINGS)then
        write(*,*) 'Successful Completion with warnings. See log file.'
        write(999,*)
        write(999,*)'Successful Completion with warnings. See log file.'
      else
        write(*,*) 'Successful Completion.  See log file.'
        write(999,*)
        write(999,*) 'Successful Completion.  See log file.'
      endif
      close(999)
      STOP
      END

      subroutine CountArgs(MyString, MyCount)
         character(*) :: MyString
         integer :: i, MyCount
         logical :: InArg
         MyCount=0
         InArg=.FALSE.
         do i=1,len(MyString)
           if(MyString(i:i).eq.' ')then
             if(InArg)then
               InArg=.FALSE.
             else
               !nothing
             endif
           else
             if(InArg)then
               !nothing
             else
               InArg=.TRUE.
               MyCount=MyCount+1
             endif
           endif
         end do
      end