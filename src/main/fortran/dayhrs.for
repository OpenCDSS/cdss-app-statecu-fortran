c dayhrs
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

      SUBROUTINE DAYHRS(LT,DHRS)

C***************************************************************************
C
C   Function        : dayhrs.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This calculates the monthly percent daylight hours
C                     associated with a given latitude.  A table of monthly
C                     values pf percent daylight hours with respect to
C                     selected latitudes (0-64 degree North) is used.
C   Calling program : mainxc.f, summary.f, other.f 
C   Called programs : none 
C   Input arguments : lt   = latitude
C   Output arguments: dhrs() = monthly daylight hours
C   Assumptions     : It takes the latitude assigned to the cropped area.
C                     An alternative is to use the latitude of the weather
C                     station (or weighted average of the latitudes of
C                     several weather stations.
C   Limitations     : The table of values are only for latitudes 0-64 degree
C                     North. 
C   Notes           : The table of monthly daylight hours is taken from
C                     the ASCE Manuals and Reports No. 70 on Evapotranspi-
C                     ration and Irrigation Water Requirements by Jensen et
C                     al., 1990.
C
C***************************************************************************

      INTEGER II, L1, L2, N1, N2
      REAL LT, HRS(12,33), DHRS(12)

      DATA HRS/
     :  8.50,7.67,8.49,8.22,8.49,8.22,8.50,8.49,8.21,8.49,8.22,8.50,
     2  8.43,7.63,8.49,8.25,8.55,8.29,8.57,8.53,8.22,8.46,8.16,8.42,
     4  8.36,7.59,8.47,8.28,8.62,8.37,8.64,8.57,8.23,8.43,8.10,8.34,
     6  8.28,7.55,8.46,8.31,8.68,8.45,8.71,8.62,8.24,8.40,8.04,8.26,
     8  8.21,7.51,8.45,8.34,8.74,8.53,8.78,8.66,8.25,8.37,7.98,8.18,
     :  8.14,7.47,8.45,8.37,8.81,8.61,8.85,8.71,8.25,8.34,7.91,8.09,
     2  8.06,7.43,8.44,8.40,8.87,8.69,8.92,8.76,8.26,8.31,7.85,8.01,
     4  7.98,7.39,8.43,8.43,8.94,8.77,9.00,8.80,8.27,8.27,7.79,7.93,
     6  7.91,7.35,8.42,8.47,9.01,8.85,9.08,8.85,8.28,8.23,7.72,7.83,
     8  7.83,7.31,8.41,8.50,9.08,8.93,9.16,8.90,8.29,8.20,7.65,7.74,
     :  7.75,7.26,8.41,8.53,9.15,9.02,9.24,8.95,8.29,8.17,7.58,7.65,
     2  7.67,7.21,8.40,8.56,9.22,9.11,9.32,9.01,8.30,8.13,7.51,7.56,
     4  7.58,7.16,8.39,8.60,9.30,9.19,9.40,9.06,8.31,8.10,7.44,7.47,
     6  7.49,7.12,8.38,8.64,9.37,9.29,9.49,9.11,8.32,8.06,7.36,7.37,
     8  7.40,7.07,8.37,8.67,9.46,9.39,9.58,9.17,8.32,8.02,7.28,7.27,
     :  7.31,7.02,8.37,8.71,9.54,9.49,9.67,9.21,8.33,7.99,7.20,7.16,
     2  7.20,6.97,8.36,8.75,9.62,9.60,9.77,9.28,8.34,7.95,7.11,7.05,
     4  7.10,6.91,8.35,8.80,9.71,9.71,9.88,9.34,8.35,7.90,7.02,6.93,
     6  6.98,6.85,8.35,8.85,9.80,9.82,9.99,9.41,8.36,7.85,6.93,6.81,
     8  6.87,6.79,8.33,8.89,9.90,9.96,10.11,9.47,8.37,7.80,6.83,6.68,
     :  6.75,6.72,8.32,8.93,10.01,10.09,10.22,9.55,8.39,7.75,6.73,6.54,
     2  6.61,6.65,8.30,8.99,10.13,10.24,10.35,9.62,8.40,7.70,6.62,6.39,
     4  6.48,6.57,8.29,9.05,10.25,10.39,10.49,9.71,8.41,7.64,6.50,6.22,
     6  6.33,6.50,8.28,9.11,10.38,10.53,10.65,9.79,8.43,7.58,6.37,6.05,
     8  6.17,6.41,8.26,9.17,10.52,10.72,10.81,9.89,8.45,7.51,6.24,5.85,
     :  5.99,6.32,8.24,9.24,10.68,10.92,10.99,9.99,8.46,7.44,6.08,5.65,
     2  5.79,6.22,8.21,9.32,10.85,11.14,11.19,10.10,8.48,7.36,5.92,5.42,
     4  5.56,6.10,8.19,9.40,11.04,11.39,11.42,10.22,8.50,7.28,5.74,5.16,
     6  5.31,5.98,8.17,9.48,11.26,11.68,11.67,10.36,8.52,7.18,5.52,4.87,
     8  5.02,5.84,8.14,9.59,11.50,12.00,11.96,10.52,8.53,7.06,5.30,4.54,
     :  4.70,5.67,8.11,9.69,11.78,12.41,12.31,10.68,8.54,6.95,5.02,4.14,
     2  4.31,5.49,8.07,9.80,12.11,12.92,12.73,10.87,8.55,6.80,4.70,3.65,
     4  3.81,5.27,8.00,9.92,12.50,13.63,13.26,11.08,8.56,6.63,4.32,3.02/

      IF ( (LT .GT. 65.0) .OR. (LT .LT. 0.0)) CALL MYEXIT(59)

      L1 = NINT(LT) - MOD(NINT(LT),2) 
      N1 = INT(0.5*L1+1)
      L2 = L1 + 2
      N2 = N1 + 1

      DO 10 II = 1,12
         DHRS(II)=HRS(II,N1)+0.5*(HRS(II,N1)-HRS(II,N2))*(L1 - LT)
10    CONTINUE
      RETURN
      END



