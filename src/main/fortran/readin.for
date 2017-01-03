      SUBROUTINE READIN(nbegyr,nendyr)

C***************************************************************************
C
C   Function        : readin.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This assigns the global variables to local variables
C                     specific to Blaney-Criddle method.  It also reads
C                     the Blaney-Criddle  crop coefficient data set from
C                     *.kbc input file.
C   Calling program : mainxc.f 
C   Called programs : none
C   Input argument  : none
C   Output arguments: nbegyr = beginning year
C                     nendyr = ending year
C   Assumptions     :
C   Limitations     : 
C   Notes           :
C
C***************************************************************************

      INCLUDE 'xccommon.inc'
      INCLUDE 'gcommon.inc'
      INCLUDE 'pmdata.inc'

C-----Local variable declaration
      INTEGER i, j, nendyr, nbegyr
      INTEGER NC, ID

      INTEGER IERR
      CHARACTER*10 FLAG
      CHARACTER*30 cropn
      CHARACTER*40 NM
      CHARACTER*80 REMARK
      CHARACTER*200 dfile1
c grb 05-20-00 add variable to read in line
      character*80 line
      character*10 cnx
      
      prectype = RN_XCO
      nbegyr = NYR1
      nendyr = NYR2
      nucrps = N_CRPS    ! number of crop scenarios

C-----calculate number of years of study
      numyrs = nendyr - nbegyr + 1

C-----assign crop variables to variables familiar to XCONS2
      DO 51 i=1,nucrps
         crop(i)   = CPNAME(I)
         ncrop(i)  = N_CRP(I) 
         ngrows(i) = GDATES(I)
   51 CONTINUE

C-----read crop coefficients
c
      if(kbcfile .eq. '') then
        write(999,*) 'No crop coefficient file defined in the '//
     &'*.rcu file'
        write(*,*) 'No crop coefficient file defined in the '//
     &'*.rcu file'
      endif   
      OPEN (UNIT=4,FILE=kbcfile,STATUS='OLD',IOSTAT=IERR)
      IF (IERR.NE.0) CALL MYEXIT(5)
      call skipn(4)
      READ(4,901,ERR=101) REMARK
      READ(4,*,ERR=101) NC
c
c  ew - read KBC file with new format for day or percent (perennial
c       of annual)  Read crop name and call INDCROP.for to tie to crop 
c       array.
c      
      DO 950 i=1,NC 
c grb 05-20-00 read in original blaney-criddle switch in kbcfile
c grb 05-20-00 read two potential formats
        READ(4,*,ERR=101) ID, cropn,FLAG
        backspace(4)
        read(4,'(a80)') line
        inote=0
        ktswt=0
        ileng=len_trim(line)
        do 26 i1=1,ileng
          if(line(i1:i1).ne.' '.and.inote.eq.0) then
            inote=1
            goto 26
          endif
          if(line(i1-1:i1-1).eq.' '.and.line(i1:i1).ne.' '
     :       .and.inote.eq.1) then
      	    inote=2
            goto 26
          endif
          if(line(i1-1:i1-1).eq.' '.and.line(i1:i1).ne.' '
     :       .and.inote.eq.2)then
            inote=3
            goto 26
          endif
          if(line(i1-1:i1-1).eq.' '.and.line(i1:i1).ne.' '
     :       .and.inote.eq.3)then
            read(line(i1:i1),"(i1)") ktswt
            goto 62
          endif
26      continue  
62      CALL INDCROP(cropn,ID)
        if(ID .EQ. 0) THEN
         WRITE(*,*)'Stop:Crop characteristics not available for crop'//
     & cropn, 'read in the crop coefficient file.' 
         WRITE(*,*)'The same crops must be included in both the crop'//
     &' characteristic file and crop coefficient file.'
         WRITE(999,*)'Stop:Crop characteristics not available for '//
     &'crop type ',cropn
         WRITE(999,*)'The same crops must be included in both the '//
     &'crop characteristic file and crop coefficient file.'
          stop
        endif

c grb 05-20-00 assign kt switch to proper crop index
c        ktsw(id)=ktswt
Cjhb=&==================================================================
c jhb 07-14-06 assign kt switch based on crop name extension if possible
Cjhb=&==================================================================
        call CropExt(cropn,cnx)
        select case (cnx)
        case (CrpTkn1) !TR21
          if((ktswt.eq.0).or.(ktswt.eq.2))then
            ktsw(id)=ktswt
          else
            ktsw(id)=0
            call lw_update(59,cropn)
          endif
        case (CrpTkn2, CrpTkn3, CrpTkn4) !CCUP, CCLP, CCRG
          ktsw(id)=0
          if(ktswt.ne.ktsw(id))then
            call lw_update(59,cropn)
          endif
        case (CrpTkn5, CrpTkn6, CrpTkn8, CrpTkn9) !DWHA, UGHA, HA, CCHA
          ktsw(id)=1
          if(ktswt.ne.ktsw(id))then
            call lw_update(59,cropn)
          endif
        case (CrpTkn7) !POCHOP
          ktsw(id)=4
          if(ktswt.ne.ktsw(id))then
            call lw_update(59,cropn)
          endif
        case (CrpTkn0) !ASCEPM
        case default
          ktsw(id)=ktswt
        end select
Cjhb=&==================================================================
        if (FLAG(1:1) .eq. 'D') then                     ! perennial
          crptyp(id)=1
          do 960 j=1,25
 960        read(4,*,ERR=101) nckca(j), ckca(ID,j)
        elseif (FLAG(1:1) .eq. 'P') then
          crptyp(id)=2
          do 970 j=1,21
 970        read(4,*,ERR=101) nckcp(j), ckcp(ID,j)
        else
          write(*,*) 'Stop: A crop listed in the *.kbc file does not'
          write(*,*) 'include Day or Percent designation'
          write(999,*) 'Stop: A crop listed in the *.kbc file does not'
          write(999,*) 'include Day or Percent designation'
          STOP
        endif
 950  continue

      close(4)

 900  FORMAT(I2,A40)
 901  FORMAT(A80)


      RETURN

 101  CALL MYEXIT(12)

      END

      subroutine CropExt(crpnm,ext)
      character*30 crpnm
      character*10 ext
      iloc=scan(crpnm,'._',back=.true.)
      if(iloc.gt.0)then
        ext=trim(crpnm(iloc+1:len(crpnm)))
      else
        ext=""
      endif
      end subroutine