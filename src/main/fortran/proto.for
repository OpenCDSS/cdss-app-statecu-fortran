c proto
c_________________________________________________________________NoticeStart_
c StateCU Consumptive Use Model
c StateCU is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2018 Colorado Department of Natural Resources
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

       SUBROUTINE PROTO

C***************************************************************************
C
C   Function        : proto.f 
C   Author          : HB Manguerra
C   Date            : May 1995 
C   Purpose         : This is the main calling subroutine for calculating
C                     crop consumptive use by Penman-Monteith ET method.
C   Calling program : statecu.f
C   Called programs : etref.f, frost2.f,
C                     kbasal.f, finput.f, growth.f, clndr.f, 
C                     rain.f, foutput.f, myexit.f supply.f
C   Input arguments : none
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           : Also reads the published 28 and 32 degree F frost
C                     dates for all years in every weather station and
C                     weigh them to get the representative frost date
C                     for the given area.
C
C   History         :(Date, Author, Description)
C
C   11/02/95   HBM  : Units changed from 1000 acre-ft to acre-ft.
C
C   11/15/95   HBM  : Basal kc is not supported anymore. 
C
C   02/02/99    EW  : Revised so uses random access arrays instead of temporary
C                     files
C
C***************************************************************************


C-----Include Global Variables and Data Defaults

      INCLUDE 'pmcommon.inc'
      INCLUDE 'gcommon.inc'
      INCLUDE 'xccommon.inc'
Cjhb====================================================================
      INCLUDE 'bindat.inc'
Cjhb====================================================================
C-----Local Variable Declaration
      CHARACTER*200 ofile5
      CHARACTER*12 tid
      INTEGER I, IY, IM, IP, IB
      INTEGER ICROP, L
      INTEGER KEY, doy10, doyh, doym, doyc
      INTEGER TDAYS
      INTEGER nbegmo(DIM_NP),nbegda(DIM_NP),nendmo(DIM_NP)
      INTEGER nendda(DIM_NP)
      INTEGER IERR,NPART(DIM_NP)
      REAL re(DIM_NP,13),cu(DIM_NP,13)
      REAL EFFPCP
      REAL incrz
! Erin test
!      REAL effp2(DIM_NP,DIM_NA,12)
!      REAL effp2(50,1200,12)      
Cjhb=&==================================================================
      CHARACTER*30 CROPN
Cjhb=&==================================================================
      REAL D_VAL(31)
      INTEGER STA_NDX, yr, mo
      LOGICAL TMPMISS(12)
      REAL :: tmpET=0.0, tmpEP=0.0
Cjhb=&==================================================================


C-----Open Output File
!      open(533,file='ETREF.log')
      ofile5=dfile
      IF(FLAG1 .EQ. 4) THEN
         ofile5(fn_len:fn_len+4) = '.omh'
      ELSE
         ofile5(fn_len:fn_len+4) = '.opm'
      ENDIF
        OPEN(9,FILE=ofile5)
        if(flag1 .eq. 3) then 
          WRITE(9,907) QUOTE,QUOTE,QUOTE,QUOTE
        elseif(flag1 .eq. 4) then
          WRITE(9,937) QUOTE,QUOTE,QUOTE,QUOTE
        else
          WRITE(9,908) QUOTE,QUOTE,QUOTE,QUOTE
        endif

Cjhb====================================================================
C     initialize some arrays and variables
Cjhb====================================================================
      do 507 k=1,n_crps
        do 506 j=1,33
           kcb(k,j) = 0
506     continue
507   continue
      do i=1,dim_np
        do j=1,dim_na
          do k=1,12
            RQT(i,j,k)=0.0
            effp2(i,j,k)=0.0
          enddo
        enddo
      enddo
      xks=0.0
Cjhb====================================================================

Cjhb====================================================================
Cjhb  for now, only output a monthly time series binary (bd1) file in this routine if:
Cjhb  1. FLAG1=5 (ASCE std PM daily)
Cjhb====================================================================
      SELECT CASE (FLAG1) !CU METHOD
      CASE(1) !Blaney-Criddle monthly
        LBD1OUT=.FALSE. !this is impossible - should not be in this routine if FLAG1=1
      CASE (3,5) !ASCE std PM
        LBD1OUT=.TRUE.  !create BD1 in this routine
      CASE DEFAULT   !invalid cu method option
        LBD1OUT=.FALSE.  !do not create BD1 in this routine
      END SELECT
      LBD2OUT=.FALSE.
      LBD3OUT=.FALSE.
      LBD4OUT=.FALSE.
      LBD5OUT=.FALSE.
Cjhb====================================================================

Cjhb====================================================================
C-----Read Coefficients for all crops in the basin.
C     This reads the Penman-Monteith crop coefficient file
Cjhb====================================================================
      CALL KBASAL
Cjhb====================================================================

Cjhb====================================================================
Cjhb  initialize arrays with -999, so if data are not input for some days,
Cjhb  they will end up with a -999 value
Cjhb====================================================================
      do i=1,N_STA
        do j=1,NYRS
          do k=1,12
            do l=1,31
              tmx(i,j,k,l)=-999.0
              tmn(i,j,k,l)=-999.0
              rf(i,j,k,l)=-999.0
              rs2(i,j,k,l)=-999.0
              edpt2(i,j,k,l)=-999.0
              wd2(i,j,k,l)=-999.0
            enddo
          enddo
        enddo
      enddo
Cjhb====================================================================
c-----Open up climate files, skip to first year of data
Cjhb====================================================================
Cjhb  modified 08/2006
Cjhb  open files and read, put necessary data into the arrays
Cjhb====================================================================
      write(*,*)'Reading Daily Maximum Temperature file'
      write(999,*) 'Reading Daily Maximum Temperature file'
      Open(60,file=tmxfile,status='OLD',iostat=ierr)
      call skipn(60)
      read(60,921) idum,idum
735   READ(60,931,END=745) YR, MO, tid,(D_VAL(J),J=1,31)
      call findsta(tid,sta_ndx)
      if(sta_ndx.gt.0)then
        if((yr.ge.nyr1).and.(yr.le.nyr2))then
          do j=1,31
            tmx(sta_ndx,yr-nyr1+1,mo,j)=D_VAL(J)
          enddo    
        endif
      endif
      goto 735
745   close(60)
Cjhb====================================================================
      write(*,*)'Reading Daily Minimum Temperature File'
      write(999,*)'Reading Daily Minimum Temperature File'
      Open(61,file=tmnfile,status='OLD',iostat=ierr)
      call skipn(61)
      read(61,921) idum,idum
736   READ(61,931,END=746) YR, MO, tid,(D_VAL(J),J=1,31)
      call findsta(tid,sta_ndx)
      if(sta_ndx.gt.0)then
        if((yr.ge.nyr1).and.(yr.le.nyr2))then
          do j=1,31
            tmn(sta_ndx,yr-nyr1+1,mo,j)=D_VAL(J)
          enddo    
        endif
      endif
      goto 736
746   close(61)
Cjhb====================================================================
      if(ipdy .eq. 1) then
        write(*,*)'Reading Daily Precipitation File'
        write(999,*)'Reading Daily Precipitation File'
        Open(62,file=pdyfile,status='OLD',iostat=ierr)
        call skipn(62)
        read(62,921) idum,idum
737     READ(62,932,END=747) YR, MO, tid,(D_VAL(J),J=1,31)
        call findsta(tid,sta_ndx)
        if(sta_ndx.gt.0)then
            if((yr.ge.nyr1).and.(yr.le.nyr2))then
            do j=1,31
                rf(sta_ndx,yr-nyr1+1,mo,j)=D_VAL(J)
            enddo    
            endif
        endif
        goto 737
747     close(62)
      endif
Cjhb====================================================================
      write(*,*)'Reading Daily Solar Radiation File'
      write(999,*)'Reading Daily Solar Radiation File'
      Open(63,file=solfile,status='OLD',iostat=ierr)
      call skipn(63)
      read(63,921) idum,idum
738   READ(63,932,END=748) YR, MO, tid,(D_VAL(J),J=1,31)
      call findsta(tid,sta_ndx)
      if(sta_ndx.gt.0)then
        if((yr.ge.nyr1).and.(yr.le.nyr2))then
          do j=1,31
            rs2(sta_ndx,yr-nyr1+1,mo,j)=D_VAL(J)
          enddo    
        endif
      endif
      goto 738
748   close(63)
Cjhb====================================================================
      if(flag1 .eq. 3 .or. flag1 .eq. 5) then
        write(*,*)'Reading Daily Vapor Pressure File'
        write(999,*)'Reading Daily Vapor Pressure File'
        Open(64,file=vapfile,status='OLD',iostat=ierr)
        call skipn(64)
        read(64,921) idum,idum
739     READ(64,932,END=749) YR, MO, tid,(D_VAL(J),J=1,31)
        call findsta(tid,sta_ndx)
        if(sta_ndx.gt.0)then
          if((yr.ge.nyr1).and.(yr.le.nyr2))then
            do j=1,31
                edpt2(sta_ndx,yr-nyr1+1,mo,j)=D_VAL(J)
            enddo    
          endif
        endif
        goto 739
749     close(64)
      endif
Cjhb====================================================================
      write(*,*)'Reading Daily Wind File'
      write(999,*)'Reading Daily Wind File'
      Open(65,file=wndfile,status='OLD',iostat=ierr)
      call skipn(65)
      read(65,921) idum,idum
740   READ(65,932,END=750) YR, MO, tid,(D_VAL(J),J=1,31)
      call findsta(tid,sta_ndx)
      if(sta_ndx.gt.0)then
        if((yr.ge.nyr1).and.(yr.le.nyr2))then
          do j=1,31
            wd2(sta_ndx,yr-nyr1+1,mo,j)=D_VAL(J)
          enddo    
        endif
      endif
      goto 740
750   close(65)
Cjhb====================================================================

C-----Initialize mean temperature

      NUMYR=0
      DO 5 IY=1,NYRS
      DO 4 IM=1,12
        reftot(iy,im)=0.0
        rntot(iy,im)=0.0
        TMEAN(IY,IM) = 0.0
 4    continue
 5    continue
 
      DO 7 IY=1,NYRS
        DO 6 IJ=1,366
         TRAIN(IY,IJ) = 0.0
 6      Continue       
 7    Continue 
Cjhb====================================================================
c     initialize some arrays
Cjhb====================================================================
      do ib=1,nbasin+1
        do iy=1,nyrs+2 !have to add 2 because wsupsum puts stats in the extra 2 columns
          scapatot(ib,iy)=0.0
          do im=1,12+2 !have to add 2 because wsupsum puts stats in the extra 2 columns
            ettot(ib,iy,im)=0.0
            effppt(ib,iy,im)=0.0
            wbu(ib,iy,im)=0.0
            reqt(ib,iy,im)=0.0
          end do
        end do
        do i=1,60
          retn(ib,i)=0.0
        end do
      end do
Cjhb====================================================================

Cjhb====================================================================
C---CALCULATE REFERENCE ET FOR EACH MONTH, FOR EACH CLIMATE STATION
Cjhb====================================================================
c     start year loop
Cjhb====================================================================
      DO 10 IY = 1, NYRS
        IYEAR=IY+NYR1-1
Cjhb====================================================================
c       correctly set # days in year and # days in february
Cjhb====================================================================
        IF ((MOD(IYEAR,4)).EQ. 0)  THEN
           MONTH(2) = 29
           TDAYS=366
        ELSE
           MONTH(2) = 28
           TDAYS=365
        ENDIF
Cjhb====================================================================
Cjhb    copy year into common block variable for arrays in ETREF to use
Cjhb    (instead of reading data a year at a time inside this loop,
Cjhb     read it all into arrays before getting into this loop)
Cjhb--  putting IY directly in a common block is probably dangerous - used in lots of places
Cjhb--  so copy the loop value into a unique new variable (IY_PM)
Cjhb    in PMCOMMON for the ETREF routine to use...
Cjhb====================================================================        
        IY_PM=IY
Cjhb====================================================================        
C-----  Calculate Reference Evapotranspiration for all Climate Stations
C       IY (ETR(N_STA,NDAYS), saves daily rainfall (PCP(N_STA,NDAYS),
C       and calculates monthly average temp (TAVE(N_STA,NMO)
Cjhb====================================================================        
        CALL ETREF
Cjhb====================================================================        

Cjhb====================================================================        
C-----  Start Structure Loop (nbasin)
Cjhb====================================================================        
        DO 25 IB = 1,NBASIN
Cjhb====================================================================        
c-----    Calculate weighted Temperature, determining beginning, ending
c         days of growing season for year IY, structure IB
Cjhb====================================================================        
          DO 35 IM=1,12
            TMEAN(IY,IM) =0.0
            TMPMISS(IM) = .FALSE.
  35      CONTINUE
Cjhb====================================================================        
C         loop through all the climate stations
C         if it is linked to the structure through the temperature weighting
C         factors, wws(ib,i), then figure out a weighted monthly
C         mean temperature, TMEAN(IY,IM), for this structure
Cjhb====================================================================        
          do 36 i=1,n_sta
            if(wws(ib,i) .eq. 0.0) goto 36
            do 40 IM=1,12
              if(tave(I,im) .lt.-998.0) then
                TMPMISS(IM) = .TRUE.
C                TMEAN(IY,IM) = -999
C                goto 43
              else
                TMEAN(IY,IM)=TMEAN(IY,IM)+WWS(IB,I)*tave(I,IM)
              endif
  40        continue
  36      continue
Cjhb====================================================================        
C         loop through all the climate stations
C         use the rainfall weighting factors, wrs(ib,i), to
C         figure out a weighted daily precip value for the structure
Cjhb====================================================================        
          DO 411 I=1,N_STA
            DO 41 IJ=1,366
              TRAIN(IY,IJ) = TRAIN(IY,IJ) + WRS(IB,I) * PCP(I,IJ)
41          Continue
411       Continue

Cjhb====================================================================        
C         if any missing data temperature values were used
C         (tave(I,IM) = -999)  then the TMEAN value will
C         probably be <-100 and therefore set the TMEAN value to -999
Cjhb====================================================================        
          do 42 IM=1,12
            if(TMPMISS(IM)) TMEAN(IY,IM)=-999.0
  42      continue
Cjhb=&==================================================================
C         frost calculates frost dates for all
C         parcels of structure, IB, for the year, IY
C         stores them in JBEG(I,IY), JEND(I,IY) where I is the parcel #
Cjhb=&==================================================================

          call frost(IB,IY)
Cjhb=&==================================================================
C-----    Calculate Carry Over Soil Moisture
Cjhb=&==================================================================
C         rewrote wbuild to calculate soil moisture carryover for one year only
C         so it would work in this routine (mainxc calls wbuildall.for)
C         1 is a flag to indicate the daily PM method is being used
C         stores the values in WBU(IB,IY,IM) (IM is the month index)
Cjhb=&==================================================================
          CALL WBUILD(1,IB,IY)
Cjhb=&==================================================================
C         Begin Crop Loop
Cjhb=&==================================================================
          DO 60 IP = 1, NPARCE(IB,IY)
            KEY = BKEY(IB,IP,iy)
            DAYONE = .TRUE.
            ICROP = N_CRP(KEY)
Cjhb=&==================================================================
            CROPN = CPNAME(ICROP)
Cjhb=&==================================================================
            JSTR = JBEG(IP,IY)          ! julian day start
            JSTP = JEND(IP,IY)          ! julian day end
            awc(key)=awcr(IB)
            IRZ(KEY) = frz(key)
            rz(key) = irz(key)
            scap = 12.0*rz(key)*awc(key)
            if(iy .eq. 1) then
              scapasp = (scap*area(ib,ip,iy))/12
              scapatot(ib,iy) = scapatot(ib,iy) + scapasp
            endif
Cjhb=&==================================================================
C           this existing code checks for whether the begin and end dates could be calculated
C           (jstr and/or jstp will be -999 if temperature data were missing)
C           if they could not, then skip this whole year
Cjhb=&==================================================================
            if(jstr .lt. -998 .or. jstp .lt. -998) then
              do 55 L=1,12
                REQT(IB,IY,L)=-999
                DEPLET(IB,IY,L) = reqt(IB,IY,L)
 55           continue
              WRITE(9,917) QUOTE,BAS_ID(IB),QUOTE,IYEAR
              WRITE(9,918)
              GOTO 25
            endif
Cjhb=&==================================================================
C           this NEW code checks for whether any ETR values are -999
C           within the season for a climate station linked to this structure
C           ETR will be -999 for any day that any climate data were
C           missing within the season.
C           if so, then do the same as if jstr or jbeg are missing
C           (skip this whole year for this structure)
Cjhb=&==================================================================
             do ID=JSTR,JSTP
               do i=1,n_sta
                 if((wws(ib,i).ne.0.0).or.(wrs(ib,i).ne.0.0)) then
                   if(etr(i,id) .lt. -998) then
                     do L=1,12
                       REQT(IB,IY,L)=-999
                       DEPLET(IB,IY,L) = reqt(IB,IY,L)
                     enddo
                     WRITE(9,917) QUOTE,BAS_ID(IB),QUOTE,IYEAR
                     WRITE(9,918)
                     GOTO 25
                   endif
                 endif
               enddo
             enddo
Cjhb=&==================================================================
C            ok, have all the data for this structure, do the calculations
Cjhb=&==================================================================
C-----       Calculate Crop Coefficients
             IF(FLAG1 .EQ. 3 .or. FLAG1 .eq. 5) CALL GROWTH(ICROP,KEY)
C ---        Initialize daily variables
             do 45 id=1,366
                refetr(id) = 0
                refeto(id) = 0
                refet(id) = 0
                rnfall(id) = 0
                et(id) = 0
                qirr(id) = 0
                epcp(id) = 0
45           continue
             do 50 L=1,12
               RQT(IP,IY,L)=0
50           continue
c
Cjhb=&==================================================================
c-----       Begin Daily loop
Cjhb=&==================================================================
             do 200 ID=JSTR,JSTP
Cjhb=&===============================================================
c-----         Calculate weighted Reference ET, Temperature, and Effective Rainfall
c              for year IY, structure IB
c              if(wws(ib,i) .eq. 0) goto 36
c              do 40 IM=1,12
c                if(tave(I,im) .lt.-998) then
c                  TMEAN(IY,IM) = -999
c                  goto 43
c                endif
c                TMEAN(IY,IM)=TMEAN(IY,IM)+WWS(IB,I)*tave(I,IM)
c  40          continue
c  36          continue
Cjhb=&==================================================================
C              because of new code above, not sure this is necessary, but leave it:
               do 15 i=1,n_sta
                 if(etr(i,id) .lt. -998) goto 15
                 REFETR(ID) = REFETR(ID) + WWS(IB,I) * ETR(I,ID)
                 REFETO(ID) = REFETO(ID) + WWS(IB,I) * ETO(I,ID)
                 RNFALL(ID) = RNFALL(ID) + WRS(IB,I) * PCP(I,ID)
  15           continue
C-----         Verify if maximum root is achieved at full cover
               IF ((ID-JSTR).GE.GDATE5(KEY)) THEN
                  incrz = 0.0
               ENDIF
C
C-----Modified Hargreaves per AGRO Engineering
C
      if(flag1 .eq. 4) then
Cjhb=&==================================================================
C       IF(ICROP .EQ. 1) THEN     !ALFALFA, 2 CUTTINGS
        IF(CROPN(1:7).EQ.'ALFALFA') THEN     !ALFALFA, 2 CUTTINGS
Cjhb=&==================================================================
          doy10= kcday(icrop,1) + jstr
          doyc = kcday(icrop,2) + jstr
          dct1 = kcday(icrop,3) + jstr
          doym = kcday(icrop,4) + jstr
          dct2 = kcday(icrop,5) + jstr
          doyh = kcday(icrop,6) + jstr
          IF(ID .LE. doy10) THEN
             XKCB = KCB(icrop,1)
          ELSEIF(ID .LE. doyc) THEN
             XKCB=KCB(icrop,1)+(KCB(icrop,2)-KCB(icrop,1))*(ID-doy10)/
     &                (doyc-doy10)
          ELSEIF(ID .LT. dct1) THEN
             XKCB = KCB(icrop,2)
          ELSEIF(ID .EQ. dct1) THEN
             XKCB = KCB(icrop,1)
          ELSEIF(ID .LE. (dct1+14)) THEN
             XKCB=KCB(icrop,1)+(KCB(icrop,2)-KCB(icrop,1))*(ID-dct1)/14
          ELSEIF(ID .LE. doym) THEN
             XKCB = KCB(icrop,2)
          ELSEIF(ID .LT. dct2) THEN
             XKCB=KCB(icrop,2)+(KCB(icrop,3)-KCB(icrop,2))*(ID-doym)/
     &                 (doyh-doym)
          ELSEIF(ID .EQ. dct2) THEN
             XKCB = KCB(icrop,1)
          ELSEIF(ID .LE. (dct2+14)) THEN
             XKCB=KCB(icrop,1)+(1.09333-KCB(icrop,1))*(ID-dct2)/14
          ELSEIF(ID .LE. doyh) THEN
             XKCB=KCB(icrop,2)+(KCB(icrop,3)-KCB(icrop,2))*(ID-doym)/
     &                 (doyh-doym)
          ELSEIF(ID .GT. doyh) THEN
             XKCB = KCB(icrop,3)
          ENDIF
        ELSE
          doy10= kcday(icrop,1) + jstr
          doyc = kcday(icrop,2) + jstr
          doym = kcday(icrop,3) + jstr
          doyh = kcday(icrop,4) + jstr
          IF(ID .LE. doy10) THEN
             XKCB = KCB(icrop,1)
          ELSEIF(ID .LE. doyc) THEN
             XKCB=KCB(icrop,1)+(KCB(icrop,2)-KCB(icrop,1))*(ID-doy10)/
     &                (doyc-doy10)
          ELSEIF(ID .LE. doym) THEN
             XKCB = KCB(icrop,2)
          ELSEIF(ID .LE. doyh) THEN
             XKCB=KCB(icrop,2)+(KCB(icrop,3)-KCB(icrop,2))*(ID-doym)/
     &                 (doyh-doym)
          ELSEIF(ID .GT. doyh) THEN
             XKCB = KCB(icrop,3)
          ENDIF
        ENDIF
        REFET(ID) = REFETR(ID)
        if(REFET(ID).lt.0.0) REFET(ID)=0.0
        XKA = 1
        GOTO 65
      endif
C-----Calculate Gross ET during Growing Season
c     (no adjustment made for effects of surface soil wetness on increased
c      evaporation), set XKA = 1
                  XKA = 1
                  REFET(ID) = REFETR(ID)
                  if(REFET(ID).lt.0.0) REFET(ID)=0.0
65                ET(ID) = REFET(ID) * XKCB(ID) * XKA

C-----Calculate Effective Precipitation for daily methods

      if(RN_XCO .ge. 3) then
            CALL RAIN (ID,EFFPCP)
            EPCP(ID) = EFFPCP
      else
             epcp(id) = 0
      endif
c
c----Calculate irrigation water requirement (account for effective precip)
c
      if(epcp(id) .gt. et(id)) epcp(id)=et(id)
      qirr(id) = et(ID) - epcp(ID)
      
c
c-----Determine monthly values for use in water budget
c
       call CLNDR (id, im, m_day)
       rqt(ip,iy,im)=rqt(ip,iy,im)+qirr(id)
       rntot(iy,im) = rntot(iy,im)+rnfall(id)
       effp2(ip,iy,im) = effp2(ip,iy,im)+epcp(id)
       reftot(iy,im)=reftot(iy,im)+refet(id)
c


C-----Write results to Output Files
             IF (m_day.EQ.MONTH(IM) .OR. ID.EQ.JSTP) THEN
                 if(RN_XCO .eq. 1 .or. RN_XCO .eq. 2) then
                   cu(ip,im)=rqt(ip,iy,im)
                   call CLNDR(jbeg(ip,iy),nbegmo(ip),nbegda(ip))
                   call CLNDR(jend(ip,iy),nendmo(ip),nendda(ip))
                   npart(IP)=month(nbegmo(ip))-nbegda(ip) + 1 
                   nyr=iy
                  CALL DXCRAIN(cu,re,ip,npart(IP),nbegmo(IP),nbegda(IP),
     :             nendmo(IP),nendda(IP),APD(key),im)
                   effp2(ip,iy,im) = min(re(ip,im),rqt(ip,iy,im))
                   rqt(ip,iy,im)= rqt(ip,iy,im) - effp2(ip,iy,im)
                 endif
              endif

             CALL FOUTPUT(IP,ID,IB,IY,re)

C-----End of daily Calculations (ID)
200          CONTINUE  
c
c-----Calculate effective precipitation for monthly methods
c
        do 110 L=1,12
         REQT(IB,IY,L)=REQT(IB,IY,L)+RQT(IP,IY,L)*AREA(IB,IP,IY)
         EFFPPT(IB,IY,L)=EFFPPT(IB,IY,L)+ effp2(ip,iy,l)*AREA(IB,IP,IY)
110     continue
C
C----End crop calculations (ip)
C
60         continue            
C
C---- End some other loop
C

        continue

        do 115 l=1,12
           REQT(IB,IY,L) = REQT(IB,IY,L) / 12.0
           DEPLET(IB,IY,L) = reqt(IB,IY,L)
           EFFPPT(IB,IY,L) = EFFPPT(IB,IY,L) / 12.0
           ETTOT(IB,IY,L) = REQT(IB,IY,L) + EFFPPT(IB,IY,L)
       
115     continue

C-----End of structure (ib) calculations
 25      CONTINUE 
C-----End of Year calculations (iy)
 10      CONTINUE
!       open(888,file='test')
!       do ib=1,nbasin
!       do  m=1,nyrs
!       do  l=1,12
!           write(888,*) '1',  ettot(ib,m,l),effppt(ib,m,l),reqt(ib,m,l)
!       enddo
!       enddo
!       enddo
Cjhb=&==================================================================
Cjhb  open binary file 1
      IF (LBD1OUT) THEN
Cjhb=&==================================================================
Cjhb	construct the name for binary file 1 output
         BD1FN = dfile
Cjhb     add file name extension
         BD1FN(fn_len:fn_len+4) = BD1EXT
Cjhb     open file in binary mode for output
         OPEN (UNIT=IBD1UN,FILE=BD1FN,STATUS='REPLACE',IOSTAT=IERR,
c     &         ACCESS='TRANSPARENT',FORM='BINARY')
c     &         ACCESS='SEQUENTIAL',FORM='BINARY')
     &         ACCESS='STREAM')
      ENDIF
Cjhb=&==================================================================
Cjhb=&==================================================================
Cjhb  open binary file 2
      IF (LBD2OUT) THEN
Cjhb=&==================================================================
Cjhb	construct the name for binary file 2 output
         BD2FN = dfile
Cjhb     add file name extension
         BD2FN(fn_len:fn_len+4) = BD2EXT
         OPEN (UNIT=IBD2UN,FILE=BD2FN,STATUS='REPLACE',IOSTAT=IERR,
c     &         ACCESS='TRANSPARENT',FORM='BINARY')
c     &         ACCESS='SEQUENTIAL',FORM='BINARY')
     &         ACCESS='STREAM')
         IBD2RN=0
      ENDIF
Cjhb=&==================================================================
Cjhb=&==================================================================
Cjhb  open binary file 3
      IF (LBD3OUT) THEN
Cjhb=&==================================================================
Cjhb	construct the name for binary file 3 output
         BD3FN = dfile
Cjhb     add file name extension
         BD3FN(fn_len:fn_len+4) = BD3EXT
Cjhb     open file in binary mode for output
         OPEN (UNIT=IBD3UN,FILE=BD3FN,STATUS='REPLACE',IOSTAT=IERR,
c     &         ACCESS='TRANSPARENT',FORM='BINARY')
c     &         ACCESS='SEQUENTIAL',FORM='BINARY')
     &         ACCESS='STREAM')
      ENDIF
Cjhb=&==================================================================
Cjhb=&==================================================================
Cjhb  open binary file 4
      IF (LBD4OUT) THEN
Cjhb=&==================================================================
Cjhb	construct the name for binary file 4 output
         BD4FN = dfile
Cjhb     add file name extension
         BD4FN(fn_len:fn_len+4) = BD4EXT
         OPEN (UNIT=IBD4UN,FILE=BD4FN,STATUS='REPLACE',IOSTAT=IERR,
c     &         ACCESS='TRANSPARENT',FORM='BINARY')
c     &         ACCESS='SEQUENTIAL',FORM='BINARY')
     &         ACCESS='STREAM')
         IBD4RN=0
      ENDIF
Cjhb=&==================================================================
Cjhb=&==================================================================
Cjhb  open binary file 5
      IF (LBD5OUT) THEN
Cjhb=&==================================================================
Cjhb	construct the name for binary file 5 output
         BD5FN = dfile
Cjhb     add file name extension
         BD5FN(fn_len:fn_len+4) = BD5EXT
         OPEN (UNIT=IBD5UN,FILE=BD5FN,STATUS='REPLACE',IOSTAT=IERR,
c     &         ACCESS='TRANSPARENT',FORM='BINARY')
c     &         ACCESS='SEQUENTIAL',FORM='BINARY')
     &         ACCESS='STREAM')
         IBD5RN=0
      ENDIF
Cjhb=&==================================================================

      IF (LBD1OUT) THEN
Cjhb=&==================================================================
Cjhb  Description of this BD1 binary file:
Cjhb  This binary file contains structure data - both monthly time series and non-time series
Cjhb  Header records are at the top of the file.
Cjhb  Then one record for every structure containing multiple fields of non time series data.
Cjhb  Then one record for each structure/time step combination containing 
Cjhb  multiple fields on each record containing the data
Cjhb  that (potentially) change from structure/timestep pair to structure/timestep pair.
Cjhb=&==================================================================
Cjhb  Binary File Overview
Cjhb  The initial records describe the organization of the monthly time series structure data.
Cjhb  These binary files are created with the FORTRAN FORM='BINARY' specification,
Cjhb    which means the data are written to the file in binary form in one long stream.
Cjhb    This accomplishes two objectives:
Cjhb    1. Allows the existing VB6 StateCUI code to flexibly read the binary file
Cjhb       one value at at time with GET() statements without having to "hardwire"
Cjhb       a fixed record structure that would cause the VB6 code to "break"
Cjhb       if the FORTRAN output changed.
Cjhb       Now new values can be added to the binary output files in the FORTRAN code
Cjhb       and the VB6 GUI will automatically accomodate them without any changes.
Cjhb       In addition, this allows the BD1 file to be different for each scenario
Cjhb       option (ISUPLY, IFLOOD, etc) without creating a nightmare for the VB6 code
Cjhb       attempting to read the file.
Cjhb    2. Prevents having to set a fixed record size large enough for the long header
Cjhb       records but that creates unused space on the many data records that follow,
Cjhb       making the file much larger than necessary containing empty space.
Cjhb  The first BD1 (monthly time series structure data) record has the format of 5 integers:
Cjhb      NBASIN-Num Structures (I), IBD1TS-Num Timesteps (I),
Cjhb      IBD1NSF-Num Structure Fields, IBD1NF-Num TimeStep Fields,
Cjhb      IBD1TSA-Num Annual Time Steps (I) 1=>annual, 12=>monthly, 52=>weekly, 365=>daily
Cjhb      The resulting number of structure data rows in the file is NBASIN
Cjhb      The resulting number of time series data rows in the file, NR, can
Cjhb        therefore be calculated as: NR-NRows (I) = NBASIN * IBD1TS
Cjhb        where IBD1TS = IBD1TSA * NYRS
Cjhb      The begin/end years can be determined from the
Cjhb        values in the header record #1 and the first time series data row.
Cjhb      The list of structures follows header record number 1.
Cjhb      This design allows the same binary file organization to be used for:
Cjhb        monthly times series for structures,
Cjhb        monthly times series for stations,
Cjhb        daily time series for structures,
Cjhb        daily time series for stations,
Cjhb        other misc time series;
Cjhb        it also allows time series to have variable start and end dates
Cjhb        (i.e. do not have to begin on Jan 1 and end on Dec 31)
Cjhb  After the first record are IBD1NSF records (one record per field) with data describing
Cjhb      the fields on the structure data records.  The format for these header records is:
Cjhb      Field Type (Character*1:'I','F','C'), Field Length in Bytes (Integer*4),
Cjhb      Field Name (Character*24), Field In Report (Integer*4:0=false, non-zero=true),
Cjhb      Field Report Header (Character*60)
Cjhb  After these records are IBD1NF records (one record per field) with data describing
Cjhb      the fields on the time series data records.  The format for these header records is:
Cjhb      Field Type (Character*1:'I','F','C'), Field Length in Bytes (Integer*4),
Cjhb      Field Name (Character*24), Field In Report (Integer*4:0=false, non-zero=true),
Cjhb      Field Report Header (Character*60)
Cjhb  Then are the NBASIN records containing the structure data
Cjhb      with the fields described above.
Cjhb  Then are the NBASIN * IBD1TS records containing the monthly time series structure data
Cjhb      with the fields described above, with the entire time series for structure 1,
Cjhb      then the entire time series for structure 2, etc.
Cjhb  The number of fields varies depending on the model run scenario options
Cjhb
Cjhb=&==================================================================
Cjhb     write BD1 header records
Cjhb=================***************====================================
           IBD1TSA=12 !# time steps per year, assumes this is monthly
           IBD1TS=IBD1TSA*NYRS !total # time steps
           IBD1NF=6 !number output values per timestep per structure
           IBD1NSF=3 !number nontime series value output per structure
           if(sboutput) then
           WRITE(UNIT=IBD1UN)NBASIN+SBCOUNT+1+1,
     &                    IBD1TS,IBD1NSF,IBD1NF,IBD1TSA
           else
           WRITE(UNIT=IBD1UN)NBASIN,
     &                    IBD1TS,IBD1NSF,IBD1NF,IBD1TSA
           endif
Cjhb=&==================================================================
Cjhb       Here are the IBD1NSF (3) values on the STRUCTURE records
Cjhb=======1============================================================
Cjhb       Integer Structure Index, I
           WRITE(UNIT=IBD1UN)'I',4,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    'Structure Index         ',0,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    'Structure Index                                             '
Cjhb=&=====2============================================================
Cjhb       Char*12 Structure ID string, BAS_ID(L)(1:12) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',12,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    'Structure ID            ',1,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    'Structure ID                                                '
Cjhb=&=====3============================================================
Cjhb       Char*12 Structure ID name, BAS_ID(L)(13:24) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',12,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    'Structure Name          ',1,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    'Structure Name                                              '
Cjhb=&==================================================================
Cjhb       Total record size = 4+12+12 = 28 bytes
Cjhb=&==================================================================
Cjhb
Cjhb=&==================================================================
Cjhb       Here are the IBD1NF (6) values on the time series records
Cjhb=======1============================================================
Cjhb       Integer Structure Index, I
           WRITE(UNIT=IBD1UN)'I',4,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    'Structure Index         ',0,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    '          '
Cjhb=&=====2============================================================
Cjhb       Integer Year, NYR1+M-1
           WRITE(UNIT=IBD1UN)'I',4,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    'Year                    ',0,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    '          '
Cjhb=&=====3============================================================
Cjhb       Integer Month Index, L
           WRITE(UNIT=IBD1UN)'I',4,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    'Month Index             ',0,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    '          '
Cjhb=&=====4============================================================
Cjhb       Real*4 Potential Crop ET  , ET
           WRITE(UNIT=IBD1UN)'R',4,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    'Potential Crop ET       ',1,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    'ACFT      '
Cjhb=&=====5============================================================
Cjhb       Real*4  Effective Precip, effppt(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    'Effective Precip        ',1,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    'ACFT      '
Cjhb=&=====6============================================================
Cjhb       Real*4 Irrigation Water Requirement IWR, reqt(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    'Irrigation Water Reqt   ',1,
Cjhb       000000000111111111122222222223333333333444444444455555555556
Cjhb       123456789012345678901234567890123456789012345678901234567890
     &    'ACFT      '
Cjhb=&==================================================================
Cjhb       Total record size = 3*4+3*4 = 24 bytes
Cjhb=&==================================================================
Cjhb
Cjhb=&==================================================================
Cjhb       Now write the NSTR (NBASIN) structure data records
Cjhb       Note: assumes these arrays are already populated, so output them now...
Cjhb====================================================================
            DO I=1,NBASIN
              CHAR12_1=BAS_ID(I)(1:12)
              CHAR12_2=BAS_ID(I)(13:24)
              WRITE(UNIT=IBD1UN)I,CHAR12_1,CHAR12_2
Cjhb          WRITE(*,*)I,CHAR12_1,CHAR12_2
            END DO
Cjhb=&==================================================================
Cjhb       Now write the sub basin (district) and basin (all structures) data records
Cjhb====================================================================
        if(sboutput) then
        DO I=0,SBCOUNT
          CHAR12_1=SBID(I)
          CHAR12_2=SBNAME(I)(1:12)
          WRITE(UNIT=IBD1UN)NBASIN+I+1,CHAR12_1,CHAR12_2
        END DO
        CHAR12_1=BID
        CHAR12_2=BNAME (1:12)
        WRITE(UNIT=IBD1UN)NBASIN+SBCOUNT+1+1,CHAR12_1,CHAR12_2
        endif
Cjhb=&==================================================================

      
c create columnar output with et,efppt,iwr
c      open(220,file='cu.dv')
c      write(220,888)
      do ib=1,nbasin
        do m=1,nyrs
          do l=1,12
            if(reqt(ib,m,l).lt.-998) then
              tmpET=-999.0
              tmpEP=-999.0
              tmpIWR=-999.0
              sbtarea(sbsb(ib),m,l)=sbtarea(sbsb(ib),m,l)+t_area(ib,m)
              btarea(m,l)=btarea(m,l)+t_area(ib,m)
            else
              if(effppt(ib,m,l).lt.-998) then
                tmpET=-999.0
                tmpEP=-999.0
                tmpIWR=-999.0
                  sbtarea(sbsb(ib),m,l)=
     &              sbtarea(sbsb(ib),m,l)+t_area(ib,m)
                  btarea(m,l)=
     &              btarea(m,l)+t_area(ib,m)
              else
                if(iclim.eq.0)then
                  tmpET=(effppt(ib,m,l)+reqt(ib,m,l))/100.0
                  tmpEP=effppt(ib,m,l)/100.0
                  tmpIWR=reqt(ib,m,l)/100.0
                    sbtarea(sbsb(ib),m,l)=
     &                sbtarea(sbsb(ib),m,l)+t_area(ib,m)
                    sbmarea(sbsb(ib),m,l)=
     &                sbmarea(sbsb(ib),m,l)+t_area(ib,m)
                    sbettot(sbsb(ib),m,l)=sbettot(sbsb(ib),m,l)+tmpET
                    sbeffppt(sbsb(ib),m,l)=sbeffppt(sbsb(ib),m,l)+tmpEP
                    sbreqt(sbsb(ib),m,l)=sbreqt(sbsb(ib),m,l)+tmpIWR
                    btarea(m,l)=
     &                btarea(m,l)+t_area(ib,m)
                    bmarea(m,l)=
     &                bmarea(m,l)+t_area(ib,m)
                    bettot(m,l)=bettot(m,l)+tmpET
                    beffppt(m,l)=beffppt(m,l)+tmpEP
                    breqt(m,l)=breqt(m,l)+tmpIWR
                else
                  tmpET=effppt(ib,m,l)+reqt(ib,m,l)
                  tmpEP=effppt(ib,m,l)
                  tmpIWR=reqt(ib,m,l)
                    sbtarea(sbsb(ib),m,l)=
     &                sbtarea(sbsb(ib),m,l)+t_area(ib,m)
                    sbmarea(sbsb(ib),m,l)=
     &                sbmarea(sbsb(ib),m,l)+t_area(ib,m)
                    sbettot(sbsb(ib),m,l)=sbettot(sbsb(ib),m,l)+tmpET
                    sbeffppt(sbsb(ib),m,l)=sbeffppt(sbsb(ib),m,l)+tmpEP
                    sbreqt(sbsb(ib),m,l)=sbreqt(sbsb(ib),m,l)+tmpIWR
                    btarea(m,l)=
     &                btarea(m,l)+t_area(ib,m)
                    bmarea(m,l)=
     &                bmarea(m,l)+t_area(ib,m)
                    bettot(m,l)=bettot(m,l)+tmpET
                    beffppt(m,l)=beffppt(m,l)+tmpEP
                    breqt(m,l)=breqt(m,l)+tmpIWR
                endif
              endif
            endif
Cjhb====================================================================
Cjhb  write a record to the binary file if binary output option selected
Cjhb  report structure:
Cjhb=&==================================================================
                  WRITE(UNIT=IBD1UN)
Cjhb====================================================================
Cjhb  write index values as the first three values and then the month string
Cjhb  (don't really need this last parameter),
Cjhb=&==================================================================
Cjhb  INTEGER IB - basin index
Cjhb  INTEGER NYR1+M-1 - year
Cjhb  INTEGER L - month index
     :            IB,NYR1+M-1,L,
Cjhb====================================================================
Cjhb  now write the monthly data (report and other) into the record
Cjhb=&==================================================================
Cjhb  REAL ETTOT(IB,IY,L)/100 - Potential Crop ET  ***REPORT***
Cjhb  REAL effppt(ib,m,l)/100 - Effective Precip  ***REPORT***
Cjhb  REAL reqt(ib,m,l)/100 - Irrigation Water Requirement IWR  ***REPORT***
     :            tmpET, tmpEP, tmpIWR
!            write(888,*) '2',  ettot(ib,m,l),effppt(ib,m,l),reqt(ib,m,l)
Cjhb====================================================================
          enddo
        enddo
      enddo
        if(sboutput) then
        do ib=0,sbcount
          do m=1,nyrs
            do l=1,12
Cjhb====================================================================
Cjhb  write a record to the binary file if binary output option selected
Cjhb  report structure:
Cjhb=&==================================================================
              WRITE(UNIT=IBD1UN)
Cjhb====================================================================
Cjhb  write index values as the first three values and then the month string
Cjhb  (don't really need this last parameter),
Cjhb=&==================================================================
Cjhb  INTEGER IB - district (sub basin) index
Cjhb  INTEGER NYR1+M-1 - year
Cjhb  INTEGER L - month index
     :          NBASIN+IB+1,NYR1+M-1,L,
Cjhb====================================================================
Cjhb  now write the monthly data (report and other) into the record
Cjhb=&==================================================================
Cjhb  REAL sbettot(ib,m,l) = district Potential Crop ET  ***REPORT***
Cjhb  REAL sbeffppt(ib,m,l) = district Effective Precip  ***REPORT***
Cjhb  REAL sbreqt(ib,m,l) = district Irrigation Water Requirement IWR  ***REPORT***
     &          sbettot(ib,m,l),sbeffppt(ib,m,l),sbreqt(ib,m,l)
Cjhb====================================================================
            enddo !l - month loop
          enddo !m - year loop
        enddo !ib - district loop
        do m=1,nyrs
          do l=1,12
Cjhb====================================================================
Cjhb  write a record to the binary file if binary output option selected
Cjhb  report structure:
Cjhb=&==================================================================
            WRITE(UNIT=IBD1UN)
Cjhb====================================================================
Cjhb  write index values as the first three values and then the month string
Cjhb  (don't really need this last parameter),
Cjhb=&==================================================================
Cjhb  INTEGER IB - district (sub basin) index
Cjhb  INTEGER NYR1+M-1 - year
Cjhb  INTEGER L - month index
     :          NBASIN+sbcount+1+1,NYR1+M-1,L,
Cjhb====================================================================
Cjhb  now write the monthly data (report and other) into the record
Cjhb=&==================================================================
Cjhb  REAL btarea(m,l) = basin total area
Cjhb  REAL bmarea(m,l) = basin modeled area
Cjhb  REAL bettot(m,l) = basin Potential Crop ET  ***REPORT***
Cjhb  REAL beffppt(m,l) = basin Effective Precip  ***REPORT***
Cjhb  REAL breqt(m,l) = basin Irrigation Water Requirement IWR  ***REPORT***
     :          btarea(m,l),bmarea(m,l),
     &          bettot(m,l),beffppt(m,l),breqt(m,l)
Cjhb====================================================================
            enddo !l - month loop
          enddo !m - year loop
      endif !sboutput
      
      close(IBD1UN)
      END IF !lbd1out
Cjhb====================================================================

      CLOSE(61)
      CLOSE(61)
      if(ipdy .ge. 1) then
         CLOSE(62)
      endif
      CLOSE(63)
      CLOSE(64)
      CLOSE(65)
      CLOSE(9)

C-----Format Statements

 
 907  FORMAT(A1,'Detailed Results of the Consumptive Use Calculation',A1
     :/A1,'Penman-Montieth Method',A1)
 908  FORMAT(A1,'Detailed Results of the Consumptive Use Calculation',A1
     :/A1,'ASCE Standardized Penman-Montieth Method',A1)
 937  FORMAT(A1,'Detailed Results of the Consumptive Use Calculation',A1
     :/A1,'Modified Hargreaves Method',A1)
 917  FORMAT(/A1,A40,A1,i4)
 918  FORMAT('Missing climate data for the year, no ET calculated')
 921  FORMAT(6x,i4,11x,i4)
931   FORMAT(i4,2x,i2,1x,a12,31F8.0)
932   FORMAT(i4,2x,i2,1x,a12,31F8.0)

      RETURN


      END


