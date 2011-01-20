      SUBROUTINE WBUILD(NDX,IB,IY)

C***************************************************************************
C
C   Function        : wbuild.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This calculates the monthly winter carry over soil moisture 
C                     for a particular year.  The total pptn 
C                     during the non-growing period is multiplied by a
C                     a carry over soil moisture coefficient 
C                     to get the precip avail for carry over soil moisture
C                     the monthly values are stored in the WINTPREC() array
C                     this is accumulated and applied to crop CU
C                     in the water budget routine (wsupsum)
C   Calling program : proto.f, wbuildall.f 
C   Called programs : none 
C   Input arguments : ndx = estimation method (1 = PM, 2 = BC)
C                     ib  = current sub-basin
C                     iy  = current year
C   Output arguments: none 
C   Assumptions     :
C   Limitations     :
C   Notes           : 
C
C   Edited by       :
C                     10/11/06 JHB modified to calculate only ONE year's
C                                  carry over soil moisture, the iy value
C                                  passed as an argument
C                                  this is essentially the exact code that
C                                  was inside the year loops of the old
C                                  wbuild routine, which is now called wbuildall
C                                  and only has the years loops and calls this
C                                  routine instead.
C                     02/13/08 JHB modified to handle year 1 (Jan to
C                                  start of growing season)
C                                  also, changed the logic of the routine
C                                  now calculates monthly precip avail for
C                                  winter carry over for every month of the current year
C                                  outside each parcel's growing season
C                                  the parcel values are accumulated per structure,
C                                  but the monthly values in WINTPREC() are not accumulated yet.  ****important****
C                                  this will be done in the water budget routine (wsupsum)
C
C                                  for now, also, at the end of the routine,
C                                  zero out WINTPREC() in months when ANY crop under
C                                  the structure has any PET
C                                  this prevents bleed through of winter precip from one crop
C                                  that is not growing to another crop that is growing.
C                                  it also prevents bleed through from the first(second) half of hte month
C                                  to the second(first) half of the month in the month when season starts(stops) midmonth
C
C***************************************************************************

      INCLUDE 'gcommon.inc'
      INCLUDE 'pmcommon.inc'
      INCLUDE 'xccommon.inc'

C-----Local Variable Declaration
      INTEGER II,IP,IY,ENDYR,NDX,M1,M2,D1,D2,IB,IRNF(2,12), ENDSN, BEGSN
      REAL SUM1, SUM2

cjhb=&==================================================================
c     calculate WINTPREC() for ALL months, then zero out afterwards
cjhb=&==================================================================
c        if(IY.EQ.1) return 
         
         DO 2 M=1,12
           IRNF(1,M) = 0
           IRNF(2,M) = 0
2        CONTINUE


      IF (NDX.EQ.1) THEN        ! using daily rainfall data
cjhb=&==================================================================
c     NOTE!!!!!!
c     02/18/08 This needs to be rewritten using the same scheme as the
c              monthly WINTPREC() section below.  Left this undone so I could
c              get a working monthly version to Mark for testing ASAP
cjhb=&==================================================================
      
c rb- changed order year/crop for nparce year determination

         DO 5 IP = 1, NPARCE(IB,iy)
cjhb=&==================================================================
c     rewrite to change the feb count because the clndr routine was sometimes breaking here
c     this is for the PREVIOUS year (that's why it is -1-1)
cjhb=&==================================================================
           IF (MOD(NYR1+IY-1-1,4).EQ.0) then
             ENDYR=366
             month(2)=29
           ELSE
             month(2)=28
             ENDYR=365
           endif
cjhb=&==================================================================
c     handle missing end date of the previous season...
c     rather than nothing ... set it to November 1?
c     probably will not really matter since if it is missing
c     there is likely other missing data
cjhb=&==================================================================
           if (JEND(IP,IY-1).lt.1) then
c            WARNINGS=.TRUE.
c             write(999,*)'Warning: Winter carry-over (wbuild.for):',
c     &       ' season end date of previous year is missing.',
c     &       ' previous year index:',IY-1,
c     &       ' structure index:',ib,
c     &       ' parcel index:',ip,
c     &       ' Set to November 1.'
             ENDSN=julian(11,1)
           else
             ENDSN=JEND(IP,IY-1)
           endif
           DO 7 J=ENDSN,ENDYR
            CALL CLNDR(J,m1,d1)    ! DETERMINE MONTH
            WBU(IB,IY-1,M1)=WBU(IB,IY-1,M1)+TRAIN(IY-1,J)*SMEF*
     :                   AREA(IB,IP,IY)/12
            IF(TRAIN(IY-1,J) .LT. -998) IRNF(1,M1) = 1
7          CONTINUE

cjhb=&==================================================================
c     rewrite to change the feb count because the clndr routine was sometimes breaking here
c     this is reset for the CURRENT year
cjhb=&==================================================================
           IF (MOD(NYR1+IY-1,4).EQ.0) then
             ENDYR=366
             month(2)=29
           ELSE
             month(2)=28
             ENDYR=365
           endif
cjhb=&==================================================================
c     handle missing begin date of the current season...
c     rather than nothing ... set it to April 1?
c     probably will not really matter since if it is missing
c     there is likely other missing data
cjhb=&==================================================================
           if (Jbeg(IP,IY).lt.1) then
c            WARNINGS=.TRUE.
c             write(999,*)'Warning: Winter carry-over (wbuild.for):',
c     &       ' season begin date of current year is missing.',
c     &       ' current year index:',IY,
c     &       ' structure index:',ib,
c     &       ' parcel index:',ip,
c     &       ' Set to April 1.'
             BEGSN=julian(4,1)
           else
             BEGSN=Jbeg(IP,IY)
           endif
C-----Sum precipitation of current year before current growing season
          DO 20 J = 1, BEGSN
            CALL CLNDR(J,m1,d1)    ! DETERMINE MONTH
            WBU(IB,IY,M1)=WBU(IB,IY,M1)+TRAIN(IY,J)*SMEF*
     :                   AREA(IB,IP,IY)/12
            IF(TRAIN(IY,J) .LT. -998) IRNF(2,M1) = 1
20        CONTINUE
 5       CONTINUE

      ELSE                     ! using monthly rainfall data
cjhb=&==================================================================
c     monthly analysis
cjhb=&==================================================================
c     calculate WINTPREC() for current year (IY)
c       for each of this year's parcels (IP) for the current structure (IB)
cjhb=&==================================================================
        DO 22 IP = 1, NPARCE(IB,IY)
cjhb=&==================================================================
c         handle missing begin date of the current season...
c         rather than nothing ... set it to April 1 for purposes of the WINTPREC() calcs
c         probably will not really matter since if it is missing
c         there is likely other missing data
c         note that this routine assumes the FROSTALL routine has already been
c         run for this structure (IB), i.e. JEND() and JBEG() are populated
cjhb=&==================================================================
          if (Jbeg(IP,IY).lt.1) then
cjhb=&==================================================================
c           this parcel's season begin date is not set -
c           for the purposes of WINTPREC() calculation, fill it in with April 1
cjhb=&==================================================================
c            WARNINGS=.TRUE.
c             write(999,*)'Warning: Winter carry-over (wbuild.for):',
c     &       ' season begin date of current year is missing.',
c     &       ' current year index:',IY,
c     &       ' structure index:',ib,
c     &       ' parcel index:',ip,
c     &       ' Set to April 1.'
cjhb=&==================================================================
            BEGSN=julian(4,1)
          else
            BEGSN=Jbeg(IP,IY)
          endif
cjhb=&==================================================================
c        convert begin season "day of year" to month and "day of month"
cjhb=&==================================================================
          CALL CLNDR(BEGSN,M1,D1)      ! start of growing season
cjhb=&==================================================================
C-----calculate precipitation of current year before growing season of parcel begins
C     loop through months Jan-until season start month
C     prorate % of month and convert to volume (AF) using parcel acreage
C     WINTPREC() should have already been initialized to 0's since it is used in an
C     accumulation equation
cjhb=&==================================================================
          DO 40 IM=1,M1
            IF(IM .EQ. M1) THEN
              WINTPREC(IB,IY,IM)=WINTPREC(IB,IY,IM)+RNTOT(IY,IM)*SMEF*
     :          D1/month(IM)*AREA(IB,IP,IY)/12
            ELSE
              WINTPREC(IB,IY,IM)=WINTPREC(IB,IY,IM)+RNTOT(IY,IM)*SMEF*
     :          AREA(IB,IP,IY)/12
            ENDIF
            IF(RNTOT(IY,IM) .LT. -998) IRNF(1,IM)=1
 40       CONTINUE
cjhb=&==================================================================
C     end of this year's parcel loop
cjhb=&==================================================================
 22     CONTINUE
cjhb=&==================================================================
cjhb=&==================================================================
c     calculate WINTPREC() for current year (IY)
c       for each of NEXT year's parcels (IP) for the current structure (IB)
c       for all but the last year...
cjhb=&==================================================================
        IF (IY.LT.NYRS) THEN
        DO 23 IP = 1, NPARCE(IB,IY+1)
cjhb=&==================================================================
c         handle missing end date of the current season...
c         two reasons this can happen:
c         1. Next year has more parcels than this year.
c           Since we are using next year's acreage for the winter precip
c           after the season, we have to use next years parcels.
c           But the end of season date is for this year's parcels, so
c           there could be a discrepancy.
c         2. There is missing data.
c
c         Set it to Oct 31 for purposes of the WINTPREC() calcs
c         note that this routine assumes the FROSTALL routine has already been
c         run for this structure (IB), i.e. JEND() and JBEG() are populated
c         ALSO, this handles the case where the # of parcels for next year

cjhb=&==================================================================
          if (JEND(IP,IY).lt.1) then
cjhb=&==================================================================
c           this parcel's season end date is not set -
c           for the purposes of WINTPREC() calculation, fill it in with Oct 31
cjhb=&==================================================================
c            WARNINGS=.TRUE.
c             write(999,*)'Warning: Winter carry-over (wbuild.for):',
c     &       ' season end date of current year is missing.',
c     &       ' current year index:',IY,
c     &       ' structure index:',ib,
c     &       ' parcel index:',ip,
c     &       ' Set to Oct 31.'
cjhb=&==================================================================
            ENDSN=julian(10,31)
          else
            ENDSN=JEND(IP,IY)
          endif
cjhb=&==================================================================
c        convert end season "day of year" to month and "day of month"
cjhb=&==================================================================
          CALL CLNDR(ENDSN,m2,d2)      ! end of growing season
cjhb=&==================================================================
C-----calculate precipitation of current year after growing season of parcel ends
C     loop through months season end month to Dec
C     prorate % of month and convert to volume (AF) using parcel acreage
C     WINTPREC() should have already been initialized to 0's since it is used in an
C     accumulation equation
cjhb=&==================================================================
          DO 41 IM=M2,12
            IF(IM .EQ. M2) THEN
              WINTPREC(IB,IY,IM)=WINTPREC(IB,IY,IM)+RNTOT(IY,IM)*SMEF*
     :          d2/month(IM)*AREA(IB,IP,IY+1)/12
            ELSE
              WINTPREC(IB,IY,IM)= WINTPREC(IB,IY,IM)+RNTOT(IY,IM)*SMEF*
     :          AREA(IB,IP,IY+1)/12
            ENDIF
            IF(RNTOT(IY,IM) .LT. -998) IRNF(2,IM)=1
 41       CONTINUE
cjhb=&==================================================================
C     end of next year's parcel loop
cjhb=&==================================================================
 23     CONTINUE
        ENDIF
cjhb=&==================================================================

      ENDIF

      DO 60 IM=1,12
c       mark months of missing data      
        IF(IRNF(1,IM) .EQ. 1) WINTPREC(IB,IY,IM) = -999
        IF(IRNF(2,IM) .EQ. 1) WINTPREC(IB,IY,IM) = -999
c       zero out WINTPREC() in months when any crop under the structure has ET>0
        IF(ETTOT(IB,IY,IM) .GT. 0.) WINTPREC(IB,IY,IM) = 0.
60    CONTINUE 

      RETURN
      END
