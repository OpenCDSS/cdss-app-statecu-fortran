c foutput
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

      SUBROUTINE FOUTPUT(IP,DOY,IB,IY,re)

C***************************************************************************
C
C   Function        : foutput.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This generates a detailed tabular ouput of the 
C                     consumptive use computation based on Penman-Monteith
C                     method.  Each subarea is assigned with a separate
C                     flag (TYPOUT) indicating whether a detailed output
C                     should be generated or not. 
C   Calling program : proto.f
C   Called programs : none
C   Input arguments : ip    = current parcel 
C                     IB    = current sub-basin
C   Output arguments: none
C   Assumptions     :
C   Limitations     : 
C   Notes           : The tabular output that are designed 120 chars
C                     long.  They should be printed in landscape format to
C                     preserve readability of the results.
C
C   History         :(Date, Author, Description)
C
C   11/15/95   HBM  : Basal kc type for Penman-Monteith is not supported 
C                     anymore.
C                      
C***************************************************************************

      INCLUDE 'gcommon.inc'
      INCLUDE 'pmcommon.inc'

C-----Local Variable Declaration
      CHARACTER*21 STRG
      INTEGER IB, IDUM ,DOY
      INTEGER IP, IC, II
      REAL re(DIM_NP,13)
      REAL SUM1, SUM2, SUM3, SUM4, inctot

c      do i=1, dim_np
c        do j=1, 13
c          re(i,j)=0.0
c        enddo
c      enddo

C-----Get Crop Scenario for Parcel
      IYEAR=IY+NYR1-1
      ic = BKEY(IB,IP,iy)
      CALL CLNDR(DOY,IMON,IDAY)

c      IF (TYPOUT(IB).EQ.3) THEN
      IF ((IDAY.EQ.1).OR.(DOY.EQ.JSTR)) THEN

       if(flag1 .eq. 3 .or. flag1 .eq. 5) then
           STRG = 'Mean KC Alfalfa Based'
       else
            STRG = '                     '
       endif

         WRITE(9,917) QUOTE,BAS_ID(IB),QUOTE
         WRITE(9,905) QUOTE,cpname(ic),QUOTE,STRG,QUOTE,QUOTE,
     :      IYEAR,QUOTE
         WRITE(9,906) 
         WRITE(9,903) (QUOTE, IDUM=1,36)
         WRITE(9,906) 
      ENDIF

      if(RN_XCO .eq. 1 .or. RN_XCO .eq. 2) then
         WRITE(9,916) DOY,QUOTE,AMN(IMON),IDAY,QUOTE,REFET(DOY),XKA,
     :             XKCB(DOY),XKCB(DOY)+XKS,
     :             ET(DOY)
      
      else
          WRITE(9,904) DOY,QUOTE,AMN(IMON),IDAY,QUOTE,REFET(DOY),XKA,
     :             XKCB(DOY),XKCB(DOY)+XKS,
     :             ET(DOY),EPCP(DOY),QIRR(DOY)
      
      endif
c      ENDIF

C-----Initialize Tabular Monthly Values
      IF (DOY.EQ.JSTR) THEN
         DO 10 II=1,12
           PNM_ET(II) = 0.0
           PNM_QR(II) = 0.0
           pnm_er(II) = 0.0
 10      CONTINUE
      ENDIF

      PNM_ET(IMON)=PNM_ET(IMON)+ET(DOY) 
      if(RN_XCO .ge. 3) then
          PNM_ER(IMON) = PNM_ER(IMON) + EPCP(DOY)
      endif

C-----Write Monthly Total
      IF ((IDAY.EQ.MONTH(IMON)).OR.(DOY.EQ.JSTP)) THEN
        if(RN_XCO .ge. 3) then
c           prevent monthly eff precip from being greater than monthly ET           
            if(PNM_ER(IMON).GT.PNM_ET(IMON)) then
                PNM_ER(IMON) = PNM_ET(IMON)
            endif
           PNM_QR(IMON)= PNM_ET(IMON)- PNM_ER(IMON)
        else
           PNM_ER(IMON) = re(ip,imon)
c           prevent monthly eff precip from being greater than monthly ET           
            if(PNM_ER(IMON).GT.PNM_ET(IMON)) then
                PNM_ER(IMON) = PNM_ET(IMON)
            endif
           PNM_QR(IMON)= PNM_ET(IMON)- PNM_ER(IMON)
        endif

         WRITE(9,906)
            IF (IDAY.NE.1) THEN
               WRITE(9,907) QUOTE,REFTOT(IY,IMON),
     :         QUOTE,QUOTE,QUOTE,QUOTE,QUOTE,PNM_ET(IMON),
     :            PNM_ER(IMON),PNM_QR(IMON)
            ELSE
               WRITE(9,907) QUOTE,REFTOT(IY,IMON),
     :         QUOTE,QUOTE,QUOTE,QUOTE,QUOTE,PNM_ET(IMON),
     :         PNM_ER(IMON),PNM_QR(IMON)
            ENDIF
      ENDIF

      IF ((IDAY.EQ.1).OR.(DOY.EQ.JSTR)) THEN
         DYBEGM = DOY
      ENDIF

C-----Write Season Total

c      IF (TYPOUT(IB).EQ.3) THEN
      IF (DOY.EQ.JSTP) THEN
         SUM1 = 0.0
         SUM2 = 0.0
         SUM3 = 0.0
         SUM4 = 0.0
         DO 20 II=1,12
            SUM1 = SUM1 + PNM_ET(II)
            SUM2 = SUM2 + PNM_ER(II)
            SUM4 = SUM4 + PNM_QR(II)
 20      CONTINUE
      
         WRITE(9,906)
         inctot = (frz(ic)-irz(ic))*12.0*awc(ic)
         WRITE(9,908) QUOTE,QUOTE,QUOTE,QUOTE,QUOTE,QUOTE,QUOTE,QUOTE,
     :   QUOTE,QUOTE,QUOTE,QUOTE,SUM1,SUM2,SUM4
         WRITE(9,906)
      ENDIF
c      ENDIF


 903  FORMAT (A1,'DOY',A1,1x,A1,'Date',A1,2x,A1,'Ref ET',A1,1x,A1,'Ka',
     :A1,2x,A1,'Kcm',A1,3x,A1,'Kc',A1,4x,A1,'ET',A1,3x,A1,
     :2x,'Re',A1,4x,A1,'IWR',A1/
     :1x,2A1,6x,2A1,4x,A1,'(in)',A1,3x,2A1,4x,2A1,6x,2A1,4x,A1,'(in)',
     :A1,3x,A1,'(in)',A1,3x,A1,'(in)',
     :A1)  
 904  FORMAT(1x,I3,1x,A1,A3,I3,A1,F7.3,3F7.2,4F9.3)
 916  FORMAT(1x,I3,1x,A1,A3,I3,A1,F7.3,3F7.2,F9.3,2('    N/A  '))
 905  FORMAT(/A1,A20,A1,A21,A1,A1,'Year = ',I4,A1)
 906  FORMAT(68('_'))
 907  FORMAT(A1,'Monthly Total',F6.3,A1,A1,A1,A1,A1,
     :16x,5F9.3)
 908  FORMAT(A1,'Season Total',A1,A1,A1,A1,A1,A1,A1,A1,A1,A1,A1,
     :17x,5F9.3) 
 917  FORMAT(/A1,A40,A1)

      RETURN
      END

