      SUBROUTINE INTERKC(iper,tmps,day, icrop)

C***************************************************************************
C
C   Function        : interkc.f 
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This calculates the crop coefficient kc, climatic 
C                   : coefficient kt, and (t x d) / 100 of the current
C                     month for the annual crops only. 
C   Calling program : annuacrp.f 
C   Called programs : none 
C   Input arguments : iper = current month
C                     tmps = mean monthly temperature for the month (already
C                            interpolated for part months).
C                     day  = daylight hours for the month (already interpo-
C                            lated for part months).
C                     icrop= index of current crop and soil combination.
C   Output arguments: none 
C   Assumptions     :
C   Limitations     :
C   Notes           : The routines are based on USBR XCONS2 program which
C                     uses the SCS Modified Blaney-Criddle ET Estimation
C                     Method.
C
C***************************************************************************

      INCLUDE 'xccommon.inc'
      INCLUDE 'gcommon.inc'

      INTEGER k, iper, icrop
      REAL tmps, day

C-----interporlate monthly kc (15th day of the month)
      DO 10 k=1,21
         IF(nckcp(k).GT.nperct(iper)) GO TO 11
         IF(nckcp(k).EQ.nperct(iper)) xkc(iper) = ckcp(icrop,k)
         IF(nckcp(k).EQ.nperct(iper)) GO TO 12
   10 CONTINUE
C
C     No value match so do not interpolate
C     set xkc to zero and continue
C
      xkc(iper) = 0.0
      GOTO 12

   11 xkc(iper)=ckcp(icrop,k-1)+(ckcp(icrop,k)-
     :   ckcp(icrop,k-1))* (nperct(iper)         
     :   -nckcp(k-1))/(nckcp(k)-nckcp(k-1))

C-----calculate f value of the current month.
   12  if(tmps .gt. -998) then
         xf(iper)=(tmps*day)/100.0
       else
         xf(iper)=-999
       endif
C-----calculate kt value of the current month.


c grb 05-20-00 add logic for original blaney-criddle switch (kt=1)
c emw 08/24/04 add logic for Pochop's Method for Blue Grass
       if(ktsw(icrop).eq.0 .or. ktsw(icrop).eq.2) then 
          if(tmps .gt. -998) then
            IF(tmps.LT.36.0) THEN
               xkt(iper)=0.3
            ELSE
               xkt(iper)=0.0173*tmps-0.314
            ENDIF
          else
            xkt(iper) = -999
          endif
       elseif(ktsw(icrop).eq.1 .or. ktsw(icrop).eq.3) then
          xkt(iper)=1.0
       elseif(ktsw(icrop).eq.4) then
          if(tmps .gt. -998) then
            xkt(iper)=0.00328*tmps+0.65011
          else
            xkt(iper) = -999
          endif
       endif

      RETURN
      END

