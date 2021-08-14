c mainxc
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
 
      SUBROUTINE MAINXC

C***************************************************************************
C
C   Function        : mainxc.f
C   Author          : HB Manguerra
C   Date            : May 1995 
C   Purpose         : This is the main calling subroutine for calculating
C                     crop consumptive use by Blaney-Criddle estimation 
C                     method.
C   Calling program : statecu.f 
C   Called programs : dayhrs.f, readin.f, calpcrop.f, frost.f,
C   Input arguments : none 
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C
C***************************************************************************

      INCLUDE 'gcommon.inc'
      INCLUDE 'xccommon.inc'
Cjhb====================================================================
      INCLUDE 'bindat.inc'
Cjhb====================================================================

C-----Local variable declaration
      INTEGER IY,IM
      INTEGER IB
      real tmpET, tmpEP, tmpIWR
      CHARACTER*200 ofile1

     
C-----specify input file extension
      ofile1 = dfile
      ofile1(fn_len:fn_len+4) = '.obc'

Cjhb====================================================================
Cjhb  for now, only output a monthly time series binary (bd1) file in this routine if:
Cjhb  1. FLAG1=1 (BC monthly),it is a climate station scenario (ICLIM=0)
Cjhb  or
Cjhb  2. FLAG1=1 (BC monthly), it is a structure scenario (ICLIM=1) and ISUPLY=0 (no water supply)
Cjhb  in other words, only output the bd1 here if the WSUPSUM routine is not called
Cjhb  (since it is always output in that routine)
Cjhb====================================================================
      SELECT CASE (FLAG1) !CU METHOD
      CASE(1) !Blaney-Criddle monthly
        SELECT CASE (ICLIM) !structure vs climate station scenario
        CASE (0) !climate station scenario
          LBD1OUT=.TRUE. !create BD1 in this routine
        CASE (1) !structure scenario
          SELECT CASE (ISUPLY) !water supply options
          CASE (0) !no water supply
            LBD1OUT=.TRUE. !create BD1 in this routine
          CASE (1,2,3,4) !water supplies available
            LBD1OUT=.FALSE. !create BD1 in WSUPSUM routine, not this routine
          CASE DEFAULT  !invalid water supply option
            LBD1OUT=.FALSE. !do not create BD1 in this routine
          END SELECT
        CASE DEFAULT   !invalid scenario option
          LBD1OUT=.FALSE.  !do not create BD1 in this routine
        END SELECT
      CASE (2,5) !not a BC monthly CU method
        LBD1OUT=.FALSE.  !for now, create BD1 only in BC monthly CU method
      CASE DEFAULT   !invalid cu method option
        LBD1OUT=.FALSE.  !do not create BD1 in this routine
      END SELECT
      LBD2OUT=.FALSE.
      LBD3OUT=.FALSE.
      LBD4OUT=.FALSE.
      LBD5OUT=.FALSE.
Cjhb====================================================================



C-----open output (obcfile) file
      OPEN (UNIT=3,FILE=ofile1,STATUS='UNKNOWN')
c jhb add header lines to OBC output file
      write(3,1108)vers, rdate
1108  FORMAT('StateCU Version ', f5.2,2x,a16)
      write(3,1109)dfile
1109  FORMAT('Scenario name: ', a200)
      write(3,1110)CURDATE(5:6),CURDATE(7:8),CURDATE(1:4),
     &             CURTIME(1:2),CURTIME(3:4),CURTIME(5:6)
1110  FORMAT('Model execution time: ',A2,'-',A2,'-',A4,'  ',
     &       A2,':',A2,':',A2)
      write(3,'(A120)')TITLE(1)
      write(3,'(A120)')TITLE(2)
      write(3,'(A120)')TITLE(3)
      write(3,*)

      WRITE(3,900) QUOTE,QUOTE,QUOTE,QUOTE
C-----reinitialize month(2)
      MONTH(2) = 28

C-----assigns global variables to variables specific to Blaney-
C-----Criddle.  Also reads the *.kbc file

      write(*,*) 'Reading Crop Coefficient File'
      write(999,*) 'Reading Crop Coefficient File'
c      do 506 k=1,n_crps
c        crptyp(k)=0
c506   continue
c
      if(iflood .ge. 1) open(104,FILE="gtemp")
c     initialize some arrays
      do iy=1,dim_ny
        do im=1,12
          btarea(iy,im)=0.
          bmarea(iy,im)=0.
          do ib=1,dim_na
            m_area(ib,iy,im)=0.
          enddo
          do ib=1,dim_nsb
            sbtarea(ib,iy,im)=0.
            sbmarea(ib,iy,im)=0.
            sbettot(ib,iy,im)=0.
            sbeffppt(ib,iy,im)=0.
            sbreqt(ib,iy,im)=0.
          enddo
        enddo
      enddo
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
      
C-----Start Sub-Basin Calculations
      write(0,511)
  511 format(' Calculating consumptive use: ')
      DO IB = 1, NBASIN
        if(mod(ib,25).eq.0) then
          write(0,*)'  processed through structure #',IB
        endif
        WRITE(3,901) QUOTE,BAS_ID(IB),QUOTE
c rb- added processing of temperature and precip data into basin based arrays
c     instead of reading data again in monthly.for
        DO i = 1,nyrs
          DO j = 1,12
            tmean(i,j) = 0.0
            rntot(i,j) = 0.0
          enddo
        enddo
c               
        do i=1,n_sta
          do iy=1,nyrs
            do im = 1,12
              if (tmpr(i,iy,im).lt.0 .and. tmpr(i,iy,im) .gt. -998) then
                  call lw_update(15,stname(i))
                  tmpr(i,iy,im) = 0.   
              endif
Cjhb=&==================================================================
C             added use of orographic temp and precip adjustments
c jhb 03/08     changed missing data criteria and behavior:
c               if any data is missing for any associated weather station
c               then the entiure month's value [tmean(iy,im),rntot(iy,im)]
c               is marked missing
Cjhb=&==================================================================
C             tmean(iy,im)=tmean(iy,im)+wws(ib,i)*tmpt(i,iy,im)
C             rntot(iy,im)=rntot(iy,im)+wrs(ib,i)*tmpr(i,iy,im)
              if(wws(ib,i).gt.0.)then
                if((tmean(iy,im).lt.-998.).or.
     &             (tmpt(i,iy,im).lt.-998.))then
                  tmean(iy,im)=-999.
                else
                  tmean(iy,im)=
     &              tmean(iy,im)+wws(ib,i)*(tmpt(i,iy,im)+ota(ib,i))
                endif
              endif
              if(wrs(ib,i).gt.0.)then
                if((rntot(iy,im).lt.-998.).or.
     &             (tmpr(i,iy,im).lt.-998.))then
                  rntot(iy,im)=-999.
                else
                  rntot(iy,im)=
     &              rntot(iy,im)+wrs(ib,i)*tmpr(i,iy,im)*opa(ib,i)
                endif
              endif
Cjhb=&==================================================================
            enddo
          enddo
        enddo

!        DO 5 IY = 1,NYRS
!          DO 5 IM = 1,12
!            if(tmean(iy,im) .lt. -100) TMEAN(IY,IM) = -999
!            if(rntot(iy,im) .lt. -100) RNTOT(IY,IM) = -999
! 5      CONTINUE


C-----calculate daylight hours from basin latitude and year
        CALL DAYHRS(BLAT(IB),PCLITE)
C-----determine growing season from frost dates
   
Cjhb=&==================================================================
C     renamed frost to frostall:  frostall calculates frost dates for all
C     parcels of structure, IB, for ALL the years, IY=1 to NYRS
C     stores them in JBEG(I,IY), JEND(I,IY) where I is the parcel #
Cjhb=&==================================================================
C        CALL FROST(IB)
        CALL FROSTALL(IB)
Cjhb=&==================================================================
C     move to AFTER the BC analysis - need BC results, ETTOT(), in the new WBU technique
Cjhb=&==================================================================
C        CALL WBUILD(2,IB)
Cjhb=&==================================================================
C-----main blaney-criddle calculations
        CALL CALPCROP(IB)
Cjhb=&==================================================================
C-----calculate carry-over soil moisture
Cjhb=&==================================================================
C     renamed WBUILD to WBUILDALL:  WBUILDALL calculates carry-over soil moisture
C      for all parcels of structure, IB, for all the years, IY=1 to NYRS
C      2 is a flag to indicate the monthly BC method is being used
C      stores them in WBU(IB,IY,IM)  (IM is the month index)
Cjhb=&==================================================================
C        CALL WBUILD(2,IB)
        CALL WBUILDALL(2,IB)
Cjhb=&==================================================================
      enddo
      write(0,*)'  finished'
      CLOSE(3)

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
        IBD1NF=8 !number output values per timestep per structure
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
Cjhb    Integer Structure Index, I
        WRITE(UNIT=IBD1UN)'I',4,
     &    'Structure Index         ',0,
     &    'Structure Index                                             '
Cjhb=&=====2============================================================
Cjhb    Char*12 Structure ID string, BAS_ID(L)(1:12) ***REPORT***
        WRITE(UNIT=IBD1UN)'C',12,
     &    'Structure ID            ',1,
     &    'Structure ID                                                '
Cjhb=&=====3============================================================
Cjhb    Char*12 Structure ID name, BAS_ID(L)(13:24) ***REPORT***
        WRITE(UNIT=IBD1UN)'C',12,
     &    'Structure Name          ',1,
     &    'Structure Name                                              '
Cjhb=&==================================================================
Cjhb       Total record size = 4+12+12 = 28 bytes
Cjhb=&==================================================================
Cjhb       Here are the IBD1NF (8) values on the time series records
Cjhb=======1============================================================
Cjhb    Integer Structure Index, I
        WRITE(UNIT=IBD1UN)'I',4,
     &    'Structure Index         ',0,
     &    '          '
Cjhb=&=====2============================================================
Cjhb    Integer Year, NYR1+M-1
        WRITE(UNIT=IBD1UN)'I',4,
     &    'Year                    ',0,
     &    '          '
Cjhb=&=====3============================================================
Cjhb    Integer Month Index, L
        WRITE(UNIT=IBD1UN)'I',4,
     &    'Month Index             ',0,
     &    '          '
Cjhb=&=====4============================================================
Cjhb    Real*4 Total Acreage, t_area
        WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Acreage           ',0,
     &    'ACRE      '
Cjhb=&=====5============================================================
Cjhb    Real*4 Modeled Acreage, m_area
        WRITE(UNIT=IBD1UN)'R',4,
     &    'Modeled Acreage         ',0,
     &    'ACRE      '
Cjhb=&=====6============================================================
Cjhb    Real*4 Potential Crop ET  , ET
        WRITE(UNIT=IBD1UN)'R',4,
     &    'Potential Crop ET       ',1,
     &    'ACFT      '
Cjhb=&=====7============================================================
Cjhb    Real*4  Effective Precip, effppt(i,m,l)
        WRITE(UNIT=IBD1UN)'R',4,
     &    'Effective Precip        ',1,
     &    'ACFT      '
Cjhb=&=====8============================================================
Cjhb    Real*4 Irrigation Water Requirement IWR, reqt(i,m,l)
        WRITE(UNIT=IBD1UN)'R',4,
     &    'Irrigation Water Reqt   ',1,
     &    'ACFT      '
Cjhb=&==================================================================
Cjhb       Total record size = 3*4+5*4 = 32 bytes
Cjhb=&==================================================================
Cjhb       Now write the NSTR (NBASIN) structure data records
Cjhb       Note: assumes these arrays are already populated, so output them now...
Cjhb====================================================================
        DO I=1,NBASIN
          CHAR12_1=BAS_ID(I)(1:12)
          CHAR12_2=BAS_ID(I)(13:24)
          WRITE(UNIT=IBD1UN)I,CHAR12_1,CHAR12_2
        ENDDO
Cjhb=&==================================================================
Cjhb       Now write the sub basin (district) and basin (all structures) data records
Cjhb====================================================================
        if(sboutput) then
        DO I=0,SBCOUNT
          CHAR12_1=SBID(I)
          CHAR12_2=SBNAME(I)(13:24)
          WRITE(UNIT=IBD1UN)NBASIN+I+1,CHAR12_1,CHAR12_2
        END DO
C        CHAR12_1=BID
C        CHAR12_2=BNAME
C        WRITE(UNIT=IBD1UN)NBASIN+SBCOUNT+1+1,CHAR12_1,CHAR12_2
         WRITE(UNIT=IBD1UN)NBASIN+SBCOUNT+1+1,BID,BNAME
        endif
Cjhb=&==================================================================
      
c create columnar output with et,efppt,iwr
c      open(220,file='cu.dv')
c      write(220,888)
        do ib=1,nbasin
          do m=1,nyrs
            do l=1,12
              if(reqt(ib,m,l).lt.-998) then
                m_area(ib,m,l)=0.
                tmpET=-999.0
                tmpEP=-999.0
                tmpIWR=-999.0
                sbtarea(sbsb(ib),m,l)=sbtarea(sbsb(ib),m,l)+t_area(ib,m)
                btarea(m,l)=btarea(m,l)+t_area(ib,m)
              else
                if(effppt(ib,m,l).lt.-998) then
                  m_area(ib,m,l)=0.
                  tmpET=-999.0
                  tmpEP=-999.0
                  tmpIWR=-999.0
                  sbtarea(sbsb(ib),m,l)=
     &              sbtarea(sbsb(ib),m,l)+t_area(ib,m)
                  btarea(m,l)=
     &              btarea(m,l)+t_area(ib,m)
                else
                  if(iclim.eq.0)then
                    m_area(ib,m,l)=t_area(ib,m)
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
                    m_area(ib,m,l)=t_area(ib,m)
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
C           write(220,889) BAS_ID(ib),NYR1+m-1,l,ET,
C    :       effppt(ib,m,l),reqt(ib,m,l)
Cjhb====================================================================
Cjhb  write a record to the binary file if binary output option selected
Cjhb  report structure:
Cjhb=&==================================================================
              WRITE(UNIT=IBD1UN)
Cjhb====================================================================
Cjhb  write index values as the first three values and then the month string
Cjhb  (don't really need this last parameter),
Cjhb=&==================================================================
Cjhb  1 INTEGER IB - basin index
Cjhb  2 INTEGER NYR1+M-1 - year
Cjhb  3 INTEGER L - month index
     :          IB,NYR1+M-1,L,
Cjhb====================================================================
Cjhb  now write the monthly data (report and other) into the record
Cjhb=&==================================================================
Cjhb  4 REAL t_area(ib,m) = total area
Cjhb  5 REAL m_area(ib,m,l) = modeled area
Cjhb  6 REAL tmpET = (effppt(ib,m,l)+reqt(ib,m,l))/100 - Potential Crop ET  ***REPORT***
Cjhb  7 REAL tmpEP = effppt(ib,m,l)/100 - Effective Precip  ***REPORT***
Cjhb  8 REAL tmpIWR = reqt(ib,m,l)/100 - Irrigation Water Requirement IWR  ***REPORT***
     :          t_area(ib,m),m_area(ib,m,l),tmpET,tmpEP,tmpIWR
Cjhb====================================================================
            enddo !l - month loop
          enddo !m - year loop
        enddo !ib - structure loop
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
Cjhb  REAL sbtarea(ib,m,l) = district total area
Cjhb  REAL sbmarea(ib,m,l) = district modeled area
Cjhb  REAL sbettot(ib,m,l) = district Potential Crop ET  ***REPORT***
Cjhb  REAL sbeffppt(ib,m,l) = district Effective Precip  ***REPORT***
Cjhb  REAL sbreqt(ib,m,l) = district Irrigation Water Requirement IWR  ***REPORT***
     :          sbtarea(ib,m,l),sbmarea(ib,m,l),
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
      END IF !lbd1out

c      close(220)
c 889  format(a12,1x,i4,1x,i2,1x,3f8.2)
c 888  format('WDID         Year Mo       ET  Effppt     IWR')

      if(iflood .ge. 1) close(104)
 900  FORMAT(A1,'Detailed Results of the Consumptive Use Calculation',A1
     :,34(" ")/A1,'Blaney-Criddle Method',A1,64(" ")/87(" "))
 901  FORMAT(A1,A40,A1,45(" ")/87(" "))

      RETURN 
      END

