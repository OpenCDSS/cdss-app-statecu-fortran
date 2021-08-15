c table
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

      SUBROUTINE TABLE(IFILE,IB,BODY,RIGH,DOWN,PTOT,ITIME)

C***************************************************************************
C
C   Function        : table.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This creates the project summary tables for
C                     each subarea for all years.
C   Calling program : proj.f
C   Called programs : none
C   Input arguments : ifile = number for type of summary 14=Blanney Criddle original
C   ew 2/1/99                 15=Blanney Criddle Enhanced, 16=Penman Monteith 
C                   : ib    = current basin
C                   : itime = 0, crop irrigation water requirement table out
C                           = 1, supply limited consumptive use table out
C                   jhb 5/31/07 commented out code related to itime=2 (the *.tcu file)
C                           = 2, total cu from ppt and irrigation water
C   Output arguments: body  = monthly water depletion in a basin for all
C                             years considered.
C                     righ  = annual water depletion in a basin for all
C                             years considered.
C                     down  = average monthly water depletion in the basin
C                     ptot  = avearge annual water depletion in the basin
C   Assumptions     :
C   Limitations     :
C   Notes           : The values can be provided in acre-ft or in inches.
C                     This capability is not provided to the user as an 
C                     option in the input file.  The global variable INCH
C                     should be set to 1 in this subroutine if the values
C                     are to be provided in INCHES rather than in ACRE-FT. 
C
C   History         : (Date, Author, Description)
C
C    5/16/95  HBM   : Change units to acre-ft from 1000 acre-ft
C
C   12/14/95  HBM   : Modify summary table to include area and depletion in 
C                     unit depth (AF/acre)
C
C***************************************************************************

      INCLUDE 'gcommon.inc'

C-----Local variable declaration
      INTEGER IB, IY, M, IFILE, IDUM, IDOWN(12)
      REAL RIGH(DIM_NY), DOWN(12), BODY(DIM_NY,12), PTOT
      REAL AR_AVG, UDEPTH, IRIGH(DIM_NY),NEWDOWN(12)

C-----Initialize
      PTOT = 0.0
      IPTOT=0
      DO 705 IY = 1, NYRS
         RIGH(IY) = 0.0
 705  CONTINUE
 
      DO 706 M = 1,12
         IDOWN(M) = 0
         DOWN(M) = 0.0
 706  CONTINUE

      IF (INCH.EQ.1) THEN 
      DO 715 M  = 1, 12
           IF (T_AREA(IB,IY).NE.0) BODY(IY,M) = 12000 * BODY(IY,M)
     :         / T_AREA(IB,IY)
715   CONTINUE
      ENDIF

      DO 710 IY = 1, NYRS
      IRIGH(IY)=0
      DO 720 M = 1, 12
        IF(ITIME.EQ. 1) THEN
          BODY(IY,M)=estcrpt(IB,IY,M)
        ELSEIF(ITIME .EQ. 0) THEN
          if(iclim .eq. 0) then
             if(reqt(ib,iy,m) .lt. 0) then
               BODY(IY,M)=REQT(IB,IY,M)
             else
               BODY(IY,M)=REQT(IB,IY,M)/100
             endif
          else
             BODY(IY,M)=REQT(IB,IY,M)
          endif
!        ELSEIF(ITIME .EQ. 2) THEN
!          BODY(IY,M)=EFFPPT(IB,IY,M)+estcrpt(IB,IY,M)
        ENDIF

C-----If Project Summary Output in inches
      INCH = 0

C-----Get Annual totals
      IF(BODY(IY,M) .GT. -998) THEN
        RIGH(IY) = RIGH(IY) + BODY(IY,M)
      ELSE
        IRIGH(IY)=1
      ENDIF

C-----Get Monthly Totals
      IF(BODY(IY,M) .GT. -998) THEN
        DOWN(M) = DOWN(M) + BODY(IY,M)
        IDOWN(M)=IDOWN(M)+1
      ENDIF
 720  CONTINUE

      IF(IRIGH(IY) .EQ. 1) RIGH(IY) = -999
      IF(RIGH(IY) .GT. -998) THEN
        PTOT = PTOT + RIGH(IY)
        IPTOT=IPTOT+1
      ENDIF
 710  CONTINUE


      IF (TYPOUT(IB).NE.0) THEN
      WRITE(10,907) BAS_ID(IB),QUOTE
         IF (INCH.EQ.1) THEN
           IF(ITIME .EQ. 0) THEN
             WRITE(10,905) QUOTE, QUOTE
           ELSEIF(ITIME .EQ. 1) THEN
             WRITE(10,911) QUOTE, QUOTE
!           ELSEIF(ITIME .EQ. 3) THEN
!             WRITE(10,914) QUOTE, QUOTE
           ENDIF
         ELSE
           IF(ITIME .EQ. 0) THEN
              WRITE(10,906) QUOTE, QUOTE
           ELSEIF(ITIME .EQ. 1) THEN
              WRITE(10,912) QUOTE, QUOTE
!           ELSEIF(ITIME .EQ. 3) THEN
!              WRITE(10,913) QUOTE, QUOTE
           ENDIF
         ENDIF
      IF (IFILE.EQ.14) WRITE (10,908) QUOTE,QUOTE
      IF (IFILE.EQ.16) WRITE (10,910) QUOTE,QUOTE
      IF (IFILE.EQ.18) WRITE (10,909) QUOTE,QUOTE
      IF (IFILE.EQ.20) WRITE (10,920) QUOTE,QUOTE
      WRITE(10,900) DLLINE
      WRITE(10,902) (QUOTE, IDUM = 1,31)
      WRITE(10,900) SLLINE


      AR_AVG = 0.0
      DO 750 IY = 1, NYRS
         UDEPTH = 0.0
         if(iclim .eq. 0) then
           AR_AVG = AR_AVG + T_AREA(IB,IY)/100
         else
           AR_AVG = AR_AVG + T_AREA(IB,IY)
         endif
         IF(RIGH(IY) .GT. -998) THEN
           IF (T_AREA(IB,IY).NE.0) UDEPTH = RIGH(IY)/T_AREA(IB,IY)
           if(iclim .eq. 0) udepth=udepth*100
         ELSE
           UDEPTH = -999
         ENDIF
         if(iclim.eq.0 ) then
         if(body(iy,1) .lt. 0) then
         WRITE(10,899) NYR1+IY-1,T_AREA(IB,IY)/100,(BODY(IY,M), M=1,12),
     :      RIGH(IY),UDEPTH
         else
         WRITE(10,901) NYR1+IY-1,T_AREA(IB,IY)/100,(BODY(IY,M), M=1,12),
     :      RIGH(IY),UDEPTH
         endif
         else
         WRITE(10,903) NYR1+IY-1,T_AREA(IB,IY),(BODY(IY,M), M=1,12),
     :      RIGH(IY),UDEPTH
         endif
 750  CONTINUE
      UDEPTH = 0.0
      AR_AVG = AR_AVG/NYRS
      WRITE(10,900) SLLINE
      IF(IPTOT .EQ. 0) THEN
         TTOT=-999
         UDEPTH=-999
      ELSE
         IF (AR_AVG.NE.0) UDEPTH = PTOT/IPTOT/AR_AVG
         TTOT=PTOT/IPTOT
      ENDIF
      DO 751 M=1,12
        IF(IDOWN(M) .EQ. 0) THEN
           NEWDOWN(M) = -999
        ELSE
          NEWDOWN(M) = DOWN(M)/IDOWN(M)
        ENDIF
 751  CONTINUE

      if(iclim .eq. 0) then
         WRITE(10,916) QUOTE,QUOTE, AR_AVG,(NEWDOWN(M), M=1,12),
     :      TTOT,UDEPTH
      else
          WRITE(10,904) QUOTE,QUOTE, AR_AVG,(NEWDOWN(M), M=1,12),
     :      TTOT,UDEPTH
      endif
      ENDIF

 900  FORMAT (A120)
 902  FORMAT (A1,'Year',A1,2x,A1,'Area',A1,1x,A1,'Jan',A1,2x,A1,'Feb',
     :A1,2x,A1,'Mar',
     :A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1,'Jun',A1,3x,A1,'Jul',A1,3x,
     :A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1,'Oct',A1,2x,A1,'Nov',A1,1x,A1,
     :'Dec',A1,2x,A1,'Annual',A1,1x,A1,'AF/ac')
c rb- add .0 to floating formats
 899  FORMAT (1x,I4,2x,F7.0,F6.0,2F7.0,7F8.0,F7.0,F6.0,F10.0,F7.0)
 901  FORMAT (1x,I4,2x,F7.0,F6.2,2F7.2,7F8.2,F7.2,F6.2,F10.2,F7.2)
 903  FORMAT (1x,I4,2x,F7.0,F6.0,2F7.0,7F8.0,F7.0,F6.0,F10.0,F7.2)
 916  FORMAT (A1,'Mean',A1,1x,F7.0,F6.2,2F7.2,7F8.2,F7.2,F6.2,F10.2
     : ,F7.2/120(" ")/120(" "))
 904  FORMAT (A1,'Mean',A1,1x,F7.0,F6.0,2F7.0,7F8.0,F7.0,F6.0,F10.0
     : ,F7.2/120(" ")/120(" "))
 905  FORMAT(35x,A1,'CROP IRRIGATION WATER REQUIREMENT (inches)',A1,
     : 41(" "))
 906  FORMAT(35x,A1,1x,'CROP IRRIGATION WATER REQUIREMENT (acre-ft)',
     :  A1,39(" "))
 907  FORMAT ('_',A40,A1,78(" "))
 908  FORMAT (35x,A1,5x,'Based on Blaney-Criddle Methodology',A1,
     :43(" "))
 909  FORMAT (41x,A1,'Modified Hargreaves',A1,58(" "))
 910  FORMAT (41x,A1,'    Penman-Monteith',A1,58(" "))
 920  FORMAT (41x,A1,'ASCEPenman-Monteith',A1,58(" "))
 911  FORMAT (35x,A1,' SUPPLY-LIMITED CONSUMPTIVE USE (inches)',A1,
     :43(" "))
 912  FORMAT (37x,A1,' SUPPLY-LIMITED CONSUMPTIVE USE (acre-ft)',A1,
     :40(" "))

      RETURN
      END
