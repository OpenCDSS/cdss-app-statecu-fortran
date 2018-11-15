      SUBROUTINE FALL (yr,tfrost, Jdays)

C***************************************************************************
C
C   Function        : fall.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This calculates the first day of frost in
C                     fall based on monthly mean temperature.
C   Calling program : frost.f 
C   Called programs : julian.f 
C   Input arguments : yr     = current year 
C                     tfrost = frost temperature in fall below which crop
C                              will die.
C   Output arguments: jdays  = first day of frost in fall in julian day 
C   Assumptions     : The calculation assumes that the fall frost
C                     temperature occur only after July 1.  If the weather
C                     data does not satisfy this assumption, the growing
C                     season will be set to "0" days, and no ET will be calculated.
C   Limitations     :
C   Notes           : The routines are based on USBR XCONS2 program which
C                     uses the SCS Modified Blaney-Criddle ET Estimation
C                     Method.
C
C***************************************************************************


      INCLUDE 'gcommon.inc'


C-----Local Variable Declaration
      INTEGER I, Jdays, JULIAN, idiff, kdays
      INTEGER fmonth,yr
      REAL tfrost, NDAYS(11)

      DATA NDAYS /29.5,29.5,30.5,30.5,30.5,30.5,31.0,30.5,30.5,30.5,
     :30.5/

      iday=0
      do 100 i=1,12
        if(tmean(yr,i) .lt. -998.0) then
           iday=1
        endif
 100  continue
      if(iday .eq. 1) then
         jdays=-999.0
         return
      endif

      DO 200 I=7,11
         IF(tmean(yr,I+1).LT. tfrost) GO TO 300
  200 CONTINUE
      fmonth = 12
      kdays = 31
      GO TO 500
  300 if(tmean(yr,I) .lt. -998.0 .or. tmean(yr,I+1) .lt. -998.0) then
         jdays=-999.0
         goto 600
      endif
      idiff = NDAYS(I)*(tmean(yr,I)-tfrost)/(tmean(yr,I)-tmean(yr,I+1))
     *  + 0.5                   ! 0.5 is added for rounding
      IF (idiff .LE. 15) THEN
         fmonth = I
         kdays = idiff + 15
      ELSE IF( idiff .GT. 31) THEN
         fmonth = 7
         kdays = 15
         goto 600
      ELSE
         fmonth = I+1
         kdays = idiff - 15
      ENDIF
  500 Jdays = JULIAN(fmonth,kdays)
  600 RETURN
      END

