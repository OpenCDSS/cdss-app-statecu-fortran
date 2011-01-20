       subroutine slimit
C***************************************************************************C
C*                                                                         *C
C*    Function:  slimit.for                                                *C
C*    Author:    Joanna M. Williams, Ross Bethel, Rick Parsons, Erin Wilson*C
C*    Date:      12-20-97 (revised and renamed 3/1999 (ew)), 07-07-00 by RB*C
C*    Purpose:   reads historic diversion, water rights, processes daily   *C 
C*               or monthly or single administration number for determining*C
C*               the coloring of water supply (daily or monthly) by admin #*C
C*    Calling Program:  statecu                                            *C
C*    Called Programs:  none                                               *C
C*    Notes:                                                               *C
C***************************************************************************C 
      INCLUDE 'gcommon.inc'

      CHARACTER headdum*1
      character*24 namet,twdid
      character*16 tempc
      character*3 idum3
      character*8 dlyt
      character*16 rtsave(20)
      character*8 tempdp
c grb 06029-00 add line character variable
      character*12 ddhidt,ddridt,aspid,temp2,Line
      integer itmp,ifound(dim_na),iflag(DIM_NA,DIM_NY)
      integer t2,ndly(60)
      integer dlyid(20),rtndl(20)
      real*8 admint, rights(dim_na,31), admday(31),admno(12)
      real temp(13),rightt,twcr,eff2(20)
      real tmpsup(DIM_NA,DIM_NY,13),eff1,dlyp,dlyrat(20,60)
      real pcttot(20)
c grb 06-20-00 add variables for administration logic
      real*4 divndata(31),divnmo
      integer iadmin, numright(dim_na)
      real rightss(dim_na,31)
      character*12 wdid
      integer iyr,imo
      character*24 wdidt
      character*3 yrstr
      if(isuply .ge. 1) then
      if(ddhfile .eq. '') then
        write(999,*) 'No water supply file defined in the '//
     &'*.rcu file'
        write(*,*) 'No water supply file defined in the '//
     &'*.rcu file'
      endif   

        OPEN (UNIT=300,FILE=ddhfile,Status='old',iostat=ierr)
        IF (IERR.NE.0) CALL MYEXIT(6)
      endif
      if(isuply .eq. 2 .or. isuply .eq. 3) then
        OPEN (UNIT=400,FILE=ddrfile,Status='old',iostat=ierr)
        IF (IERR.NE.0) CALL MYEXIT(13)
      endif
c
c-----initialize variables to assure all structures are accounted for
c
      do j=1,DIM_NA
        do k=1,DIM_NY
           ifound(j)=0
           iflag(j,k)=0
           do i=1,13
              tmpsup(j,k,i)=0.0
              divsup(j,k,i)=0.0
           enddo
           divsup(j,k,14)=0.0
        enddo
      enddo
      senadmint=0
      junadmint=0

c
c read in delay table file, if required
c
      if(isuply .eq. 3) then
        ndlyt=0
        open(unit=40,file=dlyfile,status='old',iostat=ierr)
        call skipn(40)
306     ndlyt=ndlyt+1
        read(40,931,end=307) dlyid(ndlyt), i1
        ndly(ndlyt) = i1
        backspace(40)
        if(i1 .le. 12) then
          read(40,932) (dlyrat(ndlyt,j),j=1,i1)
        elseif(i1 .le. 24) then
          read(40,932) (dlyrat(ndlyt,j),j=1,12)
          read(40,932) (dlyrat(ndlyt,j),j=13,i1)
        elseif(i1 .le. 36) then
          read(40,932) (dlyrat(ndlyt,j),j=1,12)
          read(40,932) (dlyrat(ndlyt,j),j=13,24)
          read(40,932) (dlyrat(ndlyt,j),j=25,i1)
        elseif(i1 .le. 48) then
          read(40,932) (dlyrat(ndlyt,j),j=1,12)
          read(40,932) (dlyrat(ndlyt,j),j=13,24)
          read(40,932) (dlyrat(ndlyt,j),j=25,36)
          read(40,932) (dlyrat(ndlyt,j),j=37,i1)
        else
          read(40,932) (dlyrat(ndlyt,j),j=1,12)
          read(40,932) (dlyrat(ndlyt,j),j=13,24)
          read(40,932) (dlyrat(ndlyt,j),j=25,36)
          read(40,932) (dlyrat(ndlyt,j),j=37,48)
          read(40,932) (dlyrat(ndlyt,j),j=49,i1)
        endif
        goto 306

307     ndlyt=ndlyt-1
        close(40)
      endif

C-----read in delay assignment file *.dla  (isuply .eq. 3)
      if(isuply .eq. 3) then
      open (unit=100,file=dlafile,status='old',iostat=ierr)
      IF (IERR.NE.0) CALL MYEXIT(14)
      call skipn(100)
20    read (100,16,end=27) aspid,idly,(rtsave(i),i=1,idly)
16    format(a12,i2,20(a16))

      do 25 j=1,nbasin
        twdid=bas_id(j)
        if(twdid(1:12) .eq. aspid) then
            ii=0
            do 26 i=1,idly
              tempc=rtsave(i)
              read(tempc(1:8),'(f8.0)') pcttot(i)
              read(tempc(9:16),'(i8)') rtndl(i)
26          continue
200         do 313 i=1,idly
              iret=0
              do 312 k=1,ndlyt
               if(rtndl(i) .eq. dlyid(k)) then
                 iret=1
                 do 311 im=1,ndly(k)
                   retn(j,im)=retn(j,im)+pcttot(i)/100
     :                                 *dlyrat(k,im)/100
311              continue
               endif
c               goto 313
312           continue
              if(iret.eq.0) then
               write(*,*) 'Return flow delay table not found for struct
     :ure ', twdid
               write(999,*) 'Return flow delay table not found for stru
     :cture ', twdid
               stop
              endif
313         continue

          goto 20
        endif
25    continue
      goto 20
c
c  endif for read dla file
c
      endif


c
c----if consider soil moisture option is off, set awcr array to 0
c
27    if(ISM .eq. 0) then 
        do j=1,nbasin
          awcr(j)=0.0
        enddo
      endif
      if(isuply .ge. 1) then 
        write(*,*) 'Reading in historic diversion file'
C-----read in *.ddh file
        call skipn(300)
        read(300,29) gnyr1, gnyr2, idum3
        if(idum3 .eq. 'WYR') then
          gnyr1=gnyr1+1
          gnyr2=gnyr2-1
        endif
29      format(6x,i4,11x,i4,7x,a3)

        if((nyr1 .lt. gnyr1) .or. (nyr2 .gt. gnyr2)) then
          write(*,*) 'Stop-diversion data not available for all years'
          write(999,*) 'Stop-diversion data not available for all years'
          stop
        endif

30      read(300,31,end = 40) itmp,ddhidt,(temp(j),j=1,12)
31      format(i4,1x,a12,12(f8.0),f10.0)
        if((itmp .lt. nyr1).or.(itmp .gt. nyr2)) goto 30
        do 35 i=1,nbasin
          twdid=bas_id(i)
          if(twdid(1:12) .eq. ddhidt) then
            itmp2=itmp-nyr1+1
            iflag(i,itmp2) = 1
            do 32 j=1,12
Cjhb=&==================================================================
C     removed iagg and use imiss to prorate aggr data
C      (i.e. set aggr diversion to -999 externally to get it prorated)
Cjhb=&==================================================================
C              if(twdid(4:5) .eq. 'AD' .or. twdid(3:4) .eq. 'AD') then
C                if(iagg .eq. 1) then
C                   tmpsup(i,itmp2,j)=-999
C                else
C                   tmpsup(i,itmp2,j)=temp(j)
C                endif
C              else
                tmpsup(i,itmp2,j)=temp(j)
C              endif
32          continue
            goto 30
          endif
35      continue
        goto 30

40      do 41 i=1,nbasin
          do 41 j=1,nyrs
            if(iflag(i,j) .eq. 0) then
              twdid=bas_id(i)
              aspid=twdid(1:12)
              call lw_update(66,bas_id(i))
c              if(twdid(3:5) .ne. 'URF') then
c              write(*,*) 'Stop-no diversions found for structure ',aspid
c            write(999,*) 'Stop-no diversions found for structure ',aspid
c                stop
c               endif
            endif
41      continue

c
c----ew-convert diversions to calendar year to match all other calculations
c     (if .ddh file is in water year)
c
        do 191 i=1,nbasin
          do 191 m=1,nyrs
            divsup(i,m,13) = 0
          do 191 j=1,12
            if(idum3 .eq. 'WYR') then
              if(j .gt. 3) then
                divsup(i,m,j-3)=tmpsup(i,m,j)
              else
                if(m .eq. 1) goto 191
                divsup(i,m-1,j+9)=tmpsup(i,m,j)
              endif
            else
              divsup(i,m,j)=tmpsup(i,m,j)
            endif
191       continue

        if(divonfly.gt.0)then
          call flyfilld
        endif

      endif
      

c grb the following sections color the water supply by considering administration numbers
c     if a daily diversion file is considered with daily administration numbers, then the 
c     daily diversion file is first converted to direct access file (scratch)
c     Water rights by structure are then retrieved (arrays rights,rightss) and used
c     in companion with diversion records and administration numbers to determine the 
c     percent of water supply associated with the senior priorities.  The monthly summary of
c     this value is written to the senper file.  For structures without monthly diversions
c     (including aggregated structures) a negative of the senior cfs in a given monthly for a 
c     given structure is written to the senper file under monthly administration number 
c     consideration, or is written to the scratch file under daily administration # consideration
c     These cfs values for structures with missing diversion data is then used in the wsupsum subroutine
      
c
c--- Don't read .ddr file or ddd file if isuply .lt. 2 (idaily=0),
c     set senasp to large

      IF(IDAILY.EQ.0) then
        do 42 k=1,nbasin
          senasp(k) = 1000000
          junasp(k) = 0
          trights(k)= 0
          do 42 i=1, nyrs
            do 42 j=1,12
		      persen(k,i,j)=1.0
	          peroth(k,i,j)=0.0
 42     continue
        goto 551
      ENDIF

      if (idaily.Lt.4) then
        write(*,*)'Processing daily diversions-may take several minutes'
        open (538,file='SCRATCH',access='direct',recl=124)
      if(dddfile .eq. '') then
        write(999,*) 'No daily diversion file defined in the '//
     &'*.rcu file'
        write(*,*) 'No daily diversion file defined in the '//
     &'*.rcu file'
      endif   
        open(unit=501,file=dddfile,status='old',err=43)
        call skipn(501)
        READ(501,202) dbyear,deyear,yrstr
        if (yrstr.ne."CYR") then
          write(*,*)
     &"Stopping..daily diversion need to be in calendar year format ",
     &      yrstr
          stop  
        endif
        goto 44
43     write(*,*) "Daily diversion file does not exist. Please include
     : a daily diversion file or revise your water rights processing
     : option (idaily) in the *.ccu file"
       write(999,*) "Daily diversion file does not exist. Please include
     : a daily diversion file or revise your water rights processing
     : option (idaily) in the *.ccu file"
        stop

c  check that daily diversion data is available for study period
44      if((nyr1 .lt. dbyear) .or. (nyr2 .gt. deyear)) then
          write(*,*) 'STOP-Daily diversions not available for all years'
        write(999,*) 'STOP-Daily diversions not available for all years'
          stop
        endif

        ifir=0
        k=0
        ddstrctt=0
2010    READ(501,150,end=2020) IYR,imo,wdid,(divndata(k3),k3=1,31)
        k=k+1
        if (ifir.eq.0) then
          ifir=1
          twdid=wdid
          ddstruct(k)=wdid
          ddstrctt=ddstrctt+1
          go to 2010
        endif
        if (ifir.eq.1.and.wdid.eq.twdid) ifir=2
        if (ifir.eq.1) then 
          ddstruct(k)=wdid
          write(538,rec=k) (divndata(k3),k3=1,31)
          ddstrctt=ddstrctt+1
        endif 
        if (ifir.eq.2) write(538,rec=k) (divndata(k3),k3=1,31)
        goto 2010         
2020    continue
      endif

c  end if idaily .lt 4
c'
c'C-----Read in *.ddr file
c'      WRITE(*,*) 'Reading in water right information' 
c'c
c'c--- Initialize variables for water right
c'c
      senadmint=0
      junadmint=0
        call skipn(400)
450     read(400,500,end = 55) temp2,namet,ddridt,admint,rightt
500     format (a12,a24,a12,f16.0,f8.0)     
c  Determine junior and senior water right amounts from ddr list
        do 470 j=1,nbasin
         twdid=bas_id(j)
         if (twdid(1:12) .eq. ddridt) then
           ifound(j) = 1
           numright(j)=numright(j)+1
             If (numright(j).eq.31) then
              write(*,*) "No more than 30 rights allowed for each
     1 structure...stopping"
              stop
           endif
           rights(j,numright(j))=admint
           rightss(j,numright(j))=rightt
           goto 450
         endif        
 470    continue
        goto 450
      
55      senadmint = 0
        junadmint = 0
        do 56 j=1,nbasin
c         if(ifound(j) .eq. 0) then
c           twdid=bas_id(j)
c           ddridt=twdid(1:12)
c        write(*,*) 'stop-structure ',ddridt,' not in water rights file'
c       write(999,*) 'stop-structure ',ddridt,' not in water rights file'
c           stop
c         endif

c grb 09-13-00 add one location for determining total rights of structure
         trights(j)= 0
         senasp(j) = 0
         junasp(j) = 0
         do 57 i=1,numright(j)
             if(rights(j,i) .gt. 99999.0) goto 57
             trights(j)=trights(j)+rightss(j,i)
             if(idaily .eq. 3 .or. idaily .eq. 5) then
                 if(rights(j,i) .le. adminent) then
                      senasp(j) = senasp(j) + rightss(j,i)
                      senadmint = senadmint + rightss(j,i)
                 else
                      junasp(j) = junasp(j) + rightss(j,i)
                      junadmint = junadmint + rightss(j,i)
                 endif
             endif
 57      continue
         

56      continue

C     IDAILY=0      NO WATER RIGHTS CONSIDERED, NO COLORING OF DIVERSIONS
c     idaily=1      use daily diversion with daily administrtion numbers
c     idaily=2      use daily diversions with monthly administration numbers
c     idaily=3      use daily diversions with single administration numbers
c     idaily=4      use monthly diversions with montly administration numbers
c     idaily=5      use monthly diversions with single administration numbers

      open(unit=502,file="senper",status='unknown')
       
      if (idaily.le.3) then 
        write(*,*)
     1"Coloring of water supply with daily diversions using daily,"
     	write(*,*)
     1"monthly, or single administration numbers creates a file"
     	write(*,*)
     1"(SENPER) of senior water supply as a percent of monthly water"
     	write(*,*)
     1"supply, negatives reflect daily"
     	write(*,*)
     1"   admin (-999) or senior cfs with monthly admin values."
      endif

c grb add more adminsitration processing 06-29-00
cc  set iadmin flag to -1 if single administration value, no admfile
c        iadmin=-1
        if (idaily.eq.2.or.idaily.eq.4) then 
        if(admfile .eq. '') then
        write(999,*) 'No monthly administration file defined in the '//
     &'*.rcu file'
          write(*,*) 'No monthly administration file defined in the '//
     &'*.rcu file'
        endif   

          open(unit=500,file=admfile,status='old',err=203)
          call skipn(500)
          READ(500,201) LINE
          READ(500,201) LINE
201	    FORMAT(A12,A24,A12,F11.0,F13.0)
c grb iadmin=0 = monthly iadmin=1 daily admfile data available
          IF(LINE(5:8).EQ."    ") IADMIN=0
          BACKSPACE(500)
          goto 204
        else
          goto 204
        endif
203   write(*,*) "Monthly administration file does not exist. Please
     : include a monthly diversion file or revise your water rights
     : processing option (idaily) in the *.ccu file"
      write(999,*) "Monthly administration file does not exist. Please
     : include a monthly diversion file or revise your water rights
     : processing option (idaily) in the *.ccu file"
       stop
204     if (idaily.eq.1) write(*,*) 'Processing Daily
     1 Diversions with Variable Daily Admin Numbers'
        if (idaily.eq.2) write(*,*) 'Processing Daily
     1 Diversions with Variable Monthly Admin Numbers'
        if (idaily.eq.3) write(*,*) 'Processing Daily
     1 Diversions with Single Admin Number'
        if (idaily.eq.4) write(*,*) 'Processing Monthly
     1 Diversions with Variable Monthly Admin Numbers'
        if (idaily.eq.5) write(*,*) 'Processing Monthly
     1 Diversions with Single Admin Number'


            if (idaily.eq.1) then
      if(addfile .eq. '') then
        write(999,*) 'No daily administration file defined in the '//
     &'*.rcu file'
        write(*,*) 'No daily administration file defined in the '//
     &'*.rcu file'
      endif   
               open(unit=500,file=addfile,status='old',err=205)
            call skipn(500)
          READ(500,202) dbyear,deyear,yrstr
          if (yrstr.ne."CYR") then
                 write(*,*) "Stopping..daily administration numbers need
     ' to be in calendar year format  "
                  stop  
                endif
            goto 206
205   write(*,*) "Daily administration file does not exist. Please
     : include a daily diversion file or revise your water rights
     : processing option (idaily) in the *.ccu file"
      write(999,*) "Daily administration file does not exist. Please
     : include a daily diversion file or revise your water rights
     : processing option (idaily) in the *.ccu file"
       stop

            endif

206          do 550 i=1,nyrs
          if (idaily.eq.2.or.idaily.eq.4) then
c      read monthly administration values
 502        READ(500,250) IYR,(ADMnO(k),k=1,12)
      	  if (iyr.lt.(nyr1+i-1)) goto 502
	      if (iyr.gt.(nyr1+i-1)) then
                write(*,*) "missing data, year ",nyr1+i-1," in monthly 
     :adminfile"
	        stop
            endif
          endif

c  month loop
	    do 650 j=1,12

             if (idaily.eq.1) then
 503           READ(500,300) IYR,IMO,(ADMDAY(K),K=1,31)
               if (iyr.lt.(nyr1+i-1)) goto 503
               if (iyr.eq.(nyr1+i-1).and.imo.lt.j) goto 503
	         if (iyr.gt.(nyr1+i-1)) then
               write(*,*) "Missing data, year ",nyr1+i-1," in daily
     :           adminfile. Data must be included for analysis period.
     :           Please add admin data or revise your water rights
     :           processing option (idaily( in the *.ccu file,"
               write(999,*) "Missing data, year ",nyr1+i-1," in daily
     :           adminfile. Data must be included for analysis period.
     :           Please add admin data or revise your water rights
     :           processing option (idaily( in the *.ccu file,"
	           stop
	         ENDIF
             endif
250          FORMAT(I4,4X,12F14.5)
300          FORMAT(2I4,31F14.5)

505            do 750 k=1,nbasin
	        twdid=bas_id(k)
c initialize arrays of monthly values for percent senior and other diversions in a month
                persen(k,i,j)=-999
	        peroth(k,i,j)=-999
                ifound(1)=0
                imissflg=0
                do 7501 k5=1,12
                  if (divsup(k,i,k5).lt.0) imissflg=1
7501            continue           
                sendivn=0
                othdivn=0
 
                if (idaily.le.3) then
                  do 7500 k4=1,ddstrctt
	            if (trim(adjustr(ddstruct(k4))).eq.
     1		 	trim(adjustr(twdid(1:12)))) then
                       ddindex=k4
                       ifound(1)=1
                       goto 751           
                    endif            
7500              continue
                if (ifound(1).eq.0) then
          write(*,*) "stopping...structure not found in ddd file ",twdid
                  stop
                endif
 751      if (imissflg.eq.1.or.twdid(4:5).eq.'AD'.or.twdid(3:4).eq.'AD')
     1           then
              if (idaily.eq.1) then
                  do 8502 l=1,31
	            divndata(l)=0
c
c determine amount of daily diversions that is senior to admday
                  do 9502 m=1,numright(k)
                         IF(RIGHTS(K,M).LE.ADMDAY(L)) THEN
                             divndata(l)=divndata(l)-RIGHTSS(K,M)
                         ENDIF
 9502             continue
 8502             continue
                  iddrec=ddindex+((nyr1-1-dbyear+i)*(ddstrctt*12))+
     1			    ((j-1)*ddstrctt)
                 write(538,rec=iddrec) (divndata(k3),k3=1,31)
                 goto 507
              endif 
	         if (idaily.eq.2.or.idaily.eq.4) then
	            persen(k,i,j)=0
                    if(admno(j).eq.-999) goto 507
                      do 9508 m=1,numright(k)
                         IF(RIGHTS(K,m).LE.ADMnO(J)) then 
                             persen(k,i,j)=persen(k,i,j)-rightss(k,m)
                         ENDIF
9508                  continue
                 endif

	         if (idaily.eq.3.or.idaily.eq.5) then
	            persen(k,i,j)=0
    	            do 9509 m=1,numright(k)
                      IF(RIGHTS(K,m).LE.ADMinent) then 
                           persen(k,i,j)=persen(k,i,j)-rightss(k,m)
                      ENDIF
9509                continue
                 endif
                goto 507
          endif             !end of missing diversion data

              if (idaily.le.3) then
c  process daily diversion data with daily, monthly or constant admin date
              iddrec=ddindex+((nyr1-1-dbyear+i)*(ddstrctt*12))+
     1			 ((j-1)*ddstrctt)

              read(538,rec=iddrec) (divndata(k3),k3=1,31)
100	          FORMAT(6X,I4,11X,I4,7X,A3)
150	          FORMAT(2I4,1x,A12,31F8.0)
                 
               divnmo=0
                 DO 451 M=1,31
	            if (divndata(m).eq.-999) goto 507
                    DIVNMO=DIVNMO+DIVNDATA(M)*1.9835
 451             Continue  

                 do 850 L4=1,31
	            sumright=0
                    do 950 m=1,numright(k)
                    if(idaily.eq.2) then 
                        if (admno(j).eq.-999) goto 507
	                IF(RIGHTS(K,M).LE.ADMnO(J)) then 
                              sumright=sumright+rightss(k,m)
                        ENDIF
                   endif
                   if(idaily.eq.1) then
                        IF(RIGHTS(K,M).LE.ADMDAY(L4)) THEN
                             SUMRIGHT=SUMRIGHT+RIGHTSS(K,M)
                        ENDIF            
                   endif
                  if (idaily.eq.3) then
                      IF(RIGHTS(K,M).LE.ADminent) THEN
                           SUMRIGHT=SUMRIGHT+RIGHTSS(K,M)
                      ENDIF
                  endif
 950              continue
              IF ((sumright*1.9835).gE.(divndata(L4)*1.9835)) THEN
                    sendivn= sendivn+(divndata(L4)*1.9835)
              ENDIF

        if((sumright*1.9835).lt.(divndata(L4)*1.9835)) sendivn=sendivn+
     1    (sumright*1.9835)
        if (divndata(L4).gt.trights(K)) othdivn=
     1	  othdivn+((divndata(L4)-trights(K))*1.9835)

 850           Continue      !end of day loop
               if (divnmo.eq.0) persen(k,i,j)=1.0
               if (divnmo.eq.0) peroth(k,i,j)=0
               if (divnmo.ne.0) persen(k,i,j)=sendivn/divnmo
               if (divnmo.ne.0) peroth(k,i,j)=othdivn/divnmo
              endif         !end of (idaily .le. 3)

            endif !(imissflg.eq.1.or.twdid(4:5).eq.'AD'.or.twdid(3:4).eq.'AD')

c  process monthly diversion data with monthly or constant admin date
              if(idaily.eq.4.or.idaily.eq.5) then
                sumright=0
c grb 09-13-00 remove initialization here, move prior
c                trights(K)=0
                do 9500 m=1,numright(k)
c grb 09-13-00 remove calculation of trights here, move prior				
c	            trights(K)=trights(K)+rightss(k,m)
                   if (idaily.eq.4) then
	              if (admno(j).eq.-999) goto 507
	              IF(RIGHTS(K,M).LE.ADMnO(J)) then 
	     	          sumright=sumright+rightss(k,m)
		          ENDIF
                  endif
                  if(idaily.eq.5) then
                    IF(RIGHTS(K,M).LE.ADminent) THEN
		    	      SUMRIGHT=SUMRIGHT+RIGHTSS(K,M)
                    endif
                  endif
 9500           continue
                If ((sumright*1.9835*month(j)).gE.divsup(K,I,J))sendivn
     1			  =sendivn+divsup(K,I,J)
                if((sumright*1.9835*month(j)).lt.divsup(K,I,J)) sendivn
     1              =sendivn+(sumright*1.9835*month(j))
             if (divsup(K,I,J).gt.(trights(K)*1.9835*month(j))) othdivn
     1          =othdivn+(divsup(K,I,J)-(trights(K)*1.9835*month(j)))
                if (divsup(k,i,j).eq.0) persen(k,i,j)=1.0
                if (divsup(k,i,j).eq.0) peroth(k,i,j)=0
C GRB 11-08-00 MAKE SURE FOLLOWING LINES DO NOT EXTEND PAST 72 CHARACTERS
          if (divsup(k,i,j).ne.0) persen(k,i,j)=sendivn/divsup(K,I,J)
          if (divsup(k,i,j).ne.0) peroth(k,i,j)=othdivn/divsup(K,I,J)
	        endif
 
 507          if (j.eq.12) then 
                write(502,506) (I+NYR1-1),twdid,(persen(k,i,j1),j1=1,12)
 506            format(i4,2x,a12,12f8.3)
              endif
 750        continue      !end of nbasin loop


 650      continue        !end of month loop
 550    continue          !end of year loop
 
      close(500)
      close(501)

551   totadmint=senadmint+junadmint

      close(100)
      if(isuply .ge. 1) CLOSE (300)
      if(isuply .ge. 2) CLOSE (400)

202   format(6x,i4,11x,i4,7x,a3)
931   format(i8,i4)
932   format(12x,12f8.0)
        end
