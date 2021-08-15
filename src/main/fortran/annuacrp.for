c annuacrp
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

      SUBROUTINE ANNUACRP(icrop,npart,nbeg,nend,nbegmo,nbegda,nendmo,
     :                    nendda,key)

C***************************************************************************
C
C   Function        : annuacrp.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This calculates the monthly crop coefficients
C                     mean temperature and percent daylight hours for
C                     annual crops.  It includes the calculation required
C                     for part months (i.e. the beginning and ending months)
C                     of the growing season.
C   Calling program : calpcrop.f 
C   Called programs : intertd.f, interkc.f 
C   Input arguments : icrop  = crop index
C                     nbeg   = start of growing season in julian day 
C                     nend   = end of growing season in julian day
C                     nbegmo = begin month of growing season
C                     nbegda = begin day of growing season
C                     nendmo = end month of growing season
C                     nendda = end day of growing season
C                     key    = crop and soil combination index
C   Output arguments: npart  = number of days in beginning month  in spring 
C   Assumptions     :
C   Limitations     : 
C   Notes           : The main routines were taken from USBR XCONS2 program
C                     which uses the SCS Modified Blaney-Criddle ET
C                     Estimation Method.
C
C***************************************************************************


      INTEGER K, nend, icrop, JULIAN, begmon, endmon, nbeg, npart, key
      INTEGER nbegmo,nbegda,nendmo,nendda
      REAL fake3

      INCLUDE 'xccommon.inc'
      INCLUDE 'gcommon.inc'


C-----------------------------------------------------------------------
C         annual crops
C         winter wheat fall beginning date   
C         must be before December 15        
C-----------------------------------------------------------------------

      if(nbegda .lt. -998 .or. nendda .lt. -998) then
         do 78 k=1,12
           xkc(k) = -999
           xf(k) = -999
           xkt(k) = -999
 78      continue
         return
      endif
      if(nbegda .eq. nendda) then
        if(nbegmo .eq. nendmo) then
         do 79 k=1,12
            xkt(k) = 0
            xkc(k) = 0
            xf(k) = 0
  79     continue
         return
       endif
      endif
C-----adjust end of growing season from length of growing season
      IF((nend-nbeg+1).GT.ngrows(key)) nend=nbeg + ngrows(key) - 1

C-----Calculate midpoint of beginning month in spring
      midpts=((month(nbegmo)-nbegda+ 1)/2) + nbegda

C-----Calculate midpoint of ending month in spring
      midptf= (nendda + 1)/2

C-----Calculate number of days in beginning month of spring
      npart= month(nbegmo)-nbegda + 1

C-----------------------------------------------------------------------
C        spring part month computations
C        naccum = accumulative days to midpoint of month
C        nperct = naccum/growing season length
C                                                                       WNTR WHT
C  if beginning and ending month are the same,use spring numbers        WNTR WHT
C  for computations                                                     WNTR WHT
C-----------------------------------------------------------------------

      IF(nbegmo.EQ.nendmo)  midpts = ((nendda -
     :   nbegda+1)/2) + nbegda 
      naccum(nbegmo)  =  INT(JULIAN(nbegmo,midpts) - nbeg)
      fake3 = nend - nbeg
      nperct(nbegmo) =INT((naccum(nbegmo)/( fake3 ))* 100.0)

C-----interpolate temperature and dayhours for beginning month in spring
      IF(icrop .NE. iwheat) THEN
         DO 105 K = 1, 8
            IF(JULIAN(nbegmo,midpts).LT.middle(K)) then
              CALL INTERTD(temps,days,K,nyr,npart,nbegmo,
     :                    midpts)
              goto 109
            endif

            IF(JULIAN(nbegmo,midpts).EQ.middle(K)) then
             temps = tmean(nyr ,K)
             days = pclite(K)
             goto 109
            endif

  105    CONTINUE
      else
      DO 106 K = 1,12 !WNTR WHT
        IF(JULIAN(nbegmo, midpts) .LT. middle(K)) then !WNTR WHT
          CALL INTERTD(temps,days,K,nyr,npart,nbegmo,
     :                 midpts)
          goto 109
        endif

        IF(JULIAN(nbegmo, midpts) .EQ. middle(K)) then !WNTR WHT
          temps = tmean(nyr ,K) 
          days = pclite(K)
          goto 109
        endif
  106 CONTINUE  
      endif

C-----interpolate temperature and dayhours for ending month
  109 IF(nbegmo.NE.nendmo) THEN
        DO 112 K = 1,12
            IF( JULIAN(nendmo, midptf).LT. middle(K)) THEN
               CALL INTERTD(tempf,dayf,K,nyr,nendda,
     :         nendmo, midptf)
               GO TO 118
            ENDIF 
            IF( JULIAN(nendmo, midptf) .EQ. middle(K)) THEN 
               tempf = tmean(nyr, K)
               dayf  = pclite(K)
               GO TO 118
            ENDIF
  112    CONTINUE
  118   CONTINUE
      ENDIF

C-----interpolate kc for beginning month in spring

      CALL INTERKC(nbegmo, temps, days, icrop)
C-----------------------------------------------------------------------
C        full month kc computations
C-----------------------------------------------------------------------

      IF(nbegmo.NE.nendmo) THEN
         IF(nbegmo.NE.(nendmo-1)) THEN
            begmon = nbegmo + 1
            endmon =  nendmo - 1
            DO 630 K = begmon,endmon
               naccum(K)=JULIAN(K,15)-nbeg
               nperct(K)= INT((naccum(K) /( fake3 ))* 100.0)
               CALL INTERKC(k, tmean(nyr,k), pclite(k), icrop)
  630       CONTINUE
         ENDIF

C-----interpolate kc for ending of month in fall
         naccum(nendmo) = INT(nend - nbeg- (nendda + 1)/2)
         nperct(nendmo)=INT((naccum(nendmo) / (fake3))* 100)

        CALL INTERKC(nendmo, tempf, dayf, icrop)
      ENDIF




      RETURN
      END

