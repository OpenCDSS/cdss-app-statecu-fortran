c etrref
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

      SUBROUTINE ETRREF(IS,NMO,DOM,ETROUT)

C***************************************************************************
C
C   Function        : etrref.f
C   Author          : HB Manguerra
C   Date            : October 1994
C   Purpose         : This calculates the alfalfa-based reference 
C                     evapotranspiration.
C   Calling program : etref.f 
C   Called programs : none
C   Input arguments : is     = current weather station   
C                     dom    = current day of month
C   Output arguments: etrout = reference evapotranspiration alfalfa-based 
C                              for the current day of year
C   Assumptions     :
C   Limitations     :
C   Notes           : The equation formulations are based on ASCE Manuals
C                     and Reports No. 70 on Evapotranspiration and Irriga-
C                     tion Water Requirements by Jensen et al., 1990.  Some
C                     of the routines have been taken from SMB supporting
C                     program ETOETR.FOR
C
C   Definition of Important Variables:
C
C     Etr    =  Reference crop evapotranspiration (Alfalfa)
C     Eto    =  Referance crop evapotranspiration (Grass)
C     DELTA  =  Slope of the Saturation Vapor Pressure-Temperature curve
C     PC     =  Psychrometric Constant
C     RN     =  Net Radiation
C     G      =  Heat Flux Density to the ground
C     VPD    =  Vapor Pressure Deficit
C     Wf     =  Wind Function

C***************************************************************************

      INCLUDE 'pmdata.inc'
      INCLUDE 'pmcommon.inc'
      INCLUDE 'gcommon.inc'

C-----Local Variable Declaration
      INTEGER DOM, DOY, IS, NMO
      REAL ETROUT
      REAL DELTA, XLTHT, PC, PX, ALBEDO
      REAL XA1, A1, RBO, RSO, RATIO, A, B, outs
      REAL RB, G, UZ, VPD, XNUM, RAALF, XKK1
      REAL ETOALF1, ETOALF2, RN, FAC1, FAC2
      REAL AngRad, AngDeg

      DELTA = 4098*EAV / (TAVG+237.3)**2              ! eq. 7.13 p. 175
      PX = 101.3 - .01055 * WELEV(IS)                  ! eq. 7.4  p. 169
      XLTHT = 2.501 - .002361 * TAVG                  ! eq. 7.1  p. 169
      PC = ( .001013 * PX) / (.622 * XLTHT)           ! eq. 7.15 p. 175

C-----Calculate Solar Radiation
      DOY=JULIAN(NMO,DOM)

!     eq. 6.67 p. 137
      AngDeg = 30*(NMO+0.0333*DOM+2.25)*.0175
      AngRad = AngDeg * PI / 180.0
c      ALBEDO  =  0.29 + 0.06 * SIND(AngDeg)
      ALBEDO  =  0.29 + 0.06 * SIN(AngRad)
      AngDeg = 30.0
      AngRad = AngDeg * PI / 180.0
      outs=sin(AngRad)

      XA1 = (.0154 * (30 * NMO + DOM - 207)) **2

      A1 = .26 + .1 * EXP(-XA1)                       ! eq. 6.68 p. 137

      RBO = (A1 + B1*SQRT(EDPT)) * 4.903E-09 *        ! eq. 3.17 p. 35
     1      ((TMAX+273.15)**4 + (TMIN+273.15)**4)  * .50
C      RSO = APRM + BPRM * COS(6.28318*(DOY-170)/365)  ! eq. 6.66 p. 135
      RSO = APRM + BPRM * COS(6.28318*DOY/365 - 2.93)  ! eq. 6.66 p. 135

      RATIO = RS / RSO
      IF ((RATIO - 0.70) .LE. 0.0) THEN
         A = 1.017                                       ! p. 137
         B = -0.06                                       ! p. 137
      ELSE
         A = 1.126                                       ! p. 137
         B = -0.07                                       ! p. 137
      ENDIF

      RB = RBO * (A * (RS/RSO) + B)                   ! eq. 3.16 p. 35
      RN = (1 - ALBEDO) * RS - RB                     ! eq. 3.5  p. 30

      G = 0.377 * (TAVG - ((TAVG_1+TAVG_2+TAVG_3)*.3333))

C-----Calculate Wind Function
C-----Adjust Wind for height of Anemometer

      UZ = WD/86.4                                    ! uz in m/s   
      IF (UZ .LT. .01) UZ = .01
      VPD = .50 * (EMX+EMN) - EDPT                    ! meth 3, p. 138

C-----Penman-Montieth Alfalfa
      RAALF = (ALOG((ZM(IS)-DALF)/ZOMALF)*ALOG((ZH(IS)-DALF)/ZOHALF))
     :         /(.1681*UZ)                           ! eq. 6.18 p. 93


      XNUM = PC*(1+RCALF/RAALF)                       ! eq. 6.19 p. 93
      FAC1 = DELTA/(DELTA+XNUM)
      FAC2 = PC/(DELTA+XNUM)
      XKK1 = 1710-6.85*TAVG                           ! eq. 6.24a p. 96
      ETOALF1 = FAC1*(RN-G)
      ETOALF2 = FAC2*XKK1*VPD/RAALF                   ! eq. 6.17b p. 93
      ETROUT = (ETOALF1 + ETOALF2) / XLTHT / 25.4

c      IF ((nMo.EQ.7).AND.(DOM.EQ.22)) THEN
c        WRITE(*,*) 'EAV,TAVG,DELTA,WELEV(IS),PX,XLTHT,PC'
c        WRITE(*,*) EAV,TAVG,DELTA,WELEV(IS),PX,XLTHT,PC
c        WRITE(*,*)
c        WRITE(*,*) 'ALBEDO,XA1,A1,RBO,RSO,APRM,BPRM,RS,RATIO'
c        WRITE(*,*) ALBEDO,XA1,A1,RBO,RSO,APRM,BPRM,RS,RATIO
c        WRITE(*,*)
c        WRITE(*,*) 'A,B,RB,RN,G,UZ,EMX,EMN,EDPT,VPD'
c        WRITE(*,*) A,B,RB,RN,G,UZ,EMX,EMN,EDPT,VPD
c        WRITE(*,*)
c        WRITE(*,*) 'ZM(IS),DALF,ZOMALF,ZH(IS),ZOHALF,RAALF'
c        WRITE(*,*) ZM(IS),DALF,ZOMALF,ZH(IS),ZOHALF,RAALF
c        WRITE(*,*)
c        WRITE(*,*) 'RCALF,XNUM,XKK1,ETOALF1,ETOALF2,ETROUT'
c        WRITE(*,*) RCALF,XNUM,XKK1,ETOALF1,ETOALF2,ETROUT
c        pause
c      ENDIF
      RETURN
      END
