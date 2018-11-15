c pmcommon.inc
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

***************************************************************************
C
C   Include File    : pmcommon.inc
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This includes declaration of all variables used
C                     in the Penman-Monteith calculation of evapotranspi-
C                     ration outside the calulation of reference 
C                     evapotranspiration.
C   Calling program : acount.f, evap.f, finput.f, foutput.f, growth.f,
C                     irgate.f, kbasal.f, proto.f 
C   Assumptions     :
C   Limitations     :
C   Notes           :
c                    jb 08/10/07 changed PM_NY parameter from 100 to 130 to match DIM_NY
C
C***************************************************************************


C-----Dimension
      INTEGER PM_NY 
      INTEGER PM_NS
      PARAMETER (PM_NY = 130)
      PARAMETER (PM_NS = 70)

C-----Variable Declaration

      INTEGER JSTR, JSTP
      INTEGER ILAST
      INTEGER NDYR,NUMYR
      INTEGER T1NYR1,T1NYR2,T1NYRS,T2NYR1,T2NYR2,T2NYRS,PNYR1,PNYR2
      INTEGER PNYRS,RNYR1,RNYR2,RNYRS,VNYR1,VNYR2,VNYRS,WNYR1,WNYR2
      INTEGER  ETMETH(PM_NY),WNYRS
      INTEGER DYBEGM

      REAL SMBEG, SMBEGM
      REAL CAPVAP, RZONE, XKA, XKS, EVAPDT
      REAL QIRR(366),SMCAP,SMCAP0
      REAL EPCP(366), CN(3),ELEV(PM_NS),ZH(PM_NS),ZM(PM_NS)
      REAL XKCB(366),REFET(366)
      REAL ET(366),RNFALL(366)
      REAL REFETR(366),REFETO(366)
      REAL KCDAY(PM_NY,33) 
      REAL KCB(PM_NY,33)
      REAL pnm_er(12),pnm_dp(12),pnm_et(12), pnm_qr(12),reftot(50,12)

      REAL APRM, BPRM
      REAL TAVG, TAVG_1, TAVG_2, TAVG_3
      REAL EAV, EDPT, TMAX, TMIN, RS, WD
      REAL EMX, EMN, PCP(PM_NY,366)
      REAL ETR(PM_NS,366),TAVE(PM_NS,12),ETO(PM_NS,366)
c      REAL TVAVE(PM_NS,12),TSAVE(PM_NS,12),TWAVE(PM_NS,12)
c      REAL TRAVE(PM_NS,12),TTAVE(PM_NS,12)
      
      INTEGER IY_PM
      REAL TMX(PM_NS,PM_NY,12,31),TMN(PM_NS,PM_NY,12,31)
      REAL RF(PM_NS,PM_NY,12,31),RS2(PM_NS,PM_NY,12,31)
      REAL EDPT2(PM_NS,PM_NY,12,31),WD2(PM_NS,PM_NY,12,31)

      COMMON /ET3/ APRM, BPRM
      COMMON /BLOCK1/ TAVG, TAVG_1, TAVG_2, TAVG_3
      COMMON /ET1/ EAV, EDPT, TMAX, TMIN, RS, WD
      COMMON /ET2/ EMX, EMN
      COMMON /ET4/ TMX, TMN, RF, RS2, EDPT2, WD2, IY_PM

      COMMON /BLCK1/ JSTR, JSTP
      COMMON /BLCK4/ CAPVAP, RZONE, XKA, XKS, EVAPDT
      COMMON /BLCK6/ REFET, QIRR, SMCAP, SMCAP0
      COMMON /BLCK7/ ILAST
      COMMON /BLCK8/ EPCP, CN, PCP, rnfall
      COMMON /BLCK9/  XKCB
      COMMON /BLCK11/  ET,ZH,ZM,ELEV
      COMMON /BLCK12/ NDYR
      COMMON /BLCK15/ T1NYR1,T1NYR2,T1NYRS,T2NYR1,T2NYR2,T2NYRS,PNYR1
      COMMON /BLCK16/ PNYR2,PNYRS,RNYR1,RNYR2,RNYRS,VNYR1,VNYR2,VNYRS
      COMMON /BLCK17/ WNYR1,WNYR2,WNYRS
      COMMON /ADD3/ ETMETH, NUMYR
c      COMMON /ADD8/ REFETR, REFETO, ETR, ETO,TAVE,VAVE,SAVE,WAVE,RAVE
      COMMON /ADD8/ REFETR, REFETO, ETR, ETO, TAVE
      COMMON /ADD9/ KCDAY
      COMMON /ADD11/ KCB
      COMMON /ADD12/ pnm_er, pnm_dp, pnm_et, pnm_qr, reftot
      COMMON /ADD13/ SMBEG, SMBEGM, DYBEGM

