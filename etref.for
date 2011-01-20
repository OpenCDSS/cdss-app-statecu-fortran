      SUBROUTINE ETREF

C***************************************************************************
C
C   Function        : etref.for
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This calculates the reference evapotranspiration at
C                     the current weather station for alfalfa
C                     based formulation.  The weather data are read from
C                     the daily weather input files opened in Proto.for
C   Calling program : proto.for 
C   Called programs : etrref.for 
C   Called programs : etoref.for
C   Input arguments : none
C   Output arguments: etr(N_STA,366)   = reference evapotranspiration alfalfa-based 
C                     PCP(N_STA,366) = daily rainfall
C                     TAVE(N_STA,12) = average monthly temperature
C   Assumptions     :
C   Limitations     :
C   Notes           : The data set has units in english.  The calculations
C                     are however, done in metric.  The results are 
C                     transformed back to english units.
C
C***************************************************************************

      INCLUDE 'pmdata.inc'
      INCLUDE 'gcommon.inc'
      INCLUDE 'pmcommon.inc'

C-----Local Variable Declaration
      character*12 etid
      INTEGER NMO,NDAYS(DIM_NA),BDAYS,YR,MFLAG(DIM_NA),FIRSTDAY(DIM_NA)
      REAL DOY
      REAL DPT, TAVED
      REAL F,TAVE2
      REAL ETVAL


Cjhb====================================================================        
C     initialize some arrays
Cjhb====================================================================        
      DO I=1,N_STA
         MFLAG(I)=0
         NDAYS(I)=0
         FIRSTDAY(I)=0
         do j=1,366
            pcp(i,j)=0.0
            eto(i,j)=0.0
         enddo
      enddo
Cjhb====================================================================        

Cjhb====================================================================        
C-----Read Data for the month (F,F,mbars,lang,mi/day,in)
Cjhb====================================================================        
C     removed the read ... data for all years is now read into the expanded
C     arrays (more dimensions) in proto before getting into this routine
Cjhb====================================================================        
      DO 100 NMO=1,12
      DO 300 I=1,N_STA
Cjhb====================================================================        
C        READ(60,922) YR, MO, tid,(TMX(J),J=1,31)
C        READ(61,922) YR, MO, tid,(TMN(J),J=1,31)
C
C        if(ipdy .eq. 1) then
C          READ(62,922) YR, MO, tid,(RF(J),J=1,31)
C        endif
C
C        READ(63,922) YR, MO, tid,(RS2(J),J=1,31)
C
C        if(flag1 .eq. 3 .or. flag1 .eq. 5) then
C          READ(64,922) YR, MO, etid,(EDPT2(J),J=1,31)
C        endif
C
C        READ(65,922) YR, MO, tid,(WD2(J),J=1,31)
C
Cjhb====================================================================        
C---Check for missing values - send back -999 for entire year
Cjhb====================================================================
C     REMOVED THE FOLLOWING CODE
C          Check for missing values for these parameters inside
C          the day loop; if any are missing, then set ETVAL=-999
C          and skip the calculations.  In that case, -999 will be stored
C          in the ETR(I,NDAYS(I)) array
C          Later, check the ETR(I,NDAYS(I)) array value before using it
C          in case it is -999
Cjhb====================================================================        
c        do 150 j=1,month(nmo)
c           if(tmx(I,IY_PM,nmo,j) .lt. -998) then
c                mflag(i)=1
c           endif
c           if(tmn(I,IY_PM,nmo,j) .lt. -998) then
c                mflag(i)=1
c           endif     
c           if(rf(I,IY_PM,nmo,j) .lt. -998) then
c                mflag(i)=1
c           endif     
c           if(rs2(I,IY_PM,nmo,j) .lt. -998) then
c                mflag(i)=1
c           endif     
c           if(edpt2(I,IY_PM,nmo,j) .lt. -998) then
c                mflag(i)=1
c           endif     
c           if(wd2(I,IY_PM,nmo,j) .lt. -998) then
c                mflag(i)=1
c           endif
c150     continue
c        if(mflag(i) .eq. 1) then
c           goto 300
c        endif
Cjhb====================================================================        
        APRM = 31.54 - .273 *wlat(I) + .00078 * welev(I)   ! eq. 6.66 p. 135
        BPRM = -.30 + .268 * wlat(I) + .00041 * welev(I)    ! eq. 6.66 p. 135
                                                           ! errata
Cjhb====================================================================        
        TAVE2=0
        DO 200 J=1,MONTH(NMO)
          NDAYS(I)=NDAYS(I)+1
Cjhb====================================================================        
C         check for missing temperature data, can't do any temperature
C         averaging if min or max is missing
Cjhb====================================================================        
          if(tmx(I,IY_PM,nmo,j) .lt. -998.0) then
            MFLAG(I)=1
            ETVAL=-999.0
            GOTO 50
          endif
          if(tmn(I,IY_PM,nmo,j) .lt. -998.0) then
            MFLAG(I)=1
            ETVAL=-999.0
            GOTO 50
          endif     
Cjhb====================================================================        
C         we at least have temperature data, do the averaging that is
C         possible with temperature only
Cjhb====================================================================        
          tave2=tave2+(tmx(I,IY_PM,nmo,j)+tmn(I,IY_PM,nmo,j))/2
          taved = (tmx(I,IY_PM,nmo,j) + tmn(I,IY_PM,nmo,j))/2
          TMAX  = .5556 * (tmx(I,IY_PM,nmo,J) - 32.0)   ! farenheit to centigrade
          TMIN  = .5556 * (tmn(I,IY_PM,nmo,J) - 32.0)
C-----    Calculate Average Temperature and Saturation V. Pressure
Cjhb====================================================================
C         REMOVED THE FOLLOWING CODE
C          it assumes jan 1 is not missing temp data
C          change it to initialize on the first non missing day
Cjhb====================================================================        
c      IF (NMO .eq. 1 .and. j .eq. 1) THEN
c       TAVG = (TMAX+TMIN) * 0.50
c       TAVG_1 = TAVG
c       TAVG_2 = TAVG
c       TAVG_3 = TAVG
c      ELSE
c       TAVG_3 = TAVG_2
c       TAVG_2 = TAVG_1
c       TAVG_1 = TAVG
c       TAVG = (TMAX+TMIN) * 0.50
c      ENDIF
Cjhb====================================================================        
          IF (FIRSTDAY(I).EQ.0) THEN
            FIRSTDAY(I)=1
            TAVG = (TMAX+TMIN) * 0.50
            TAVG_1 = TAVG
            TAVG_2 = TAVG
            TAVG_3 = TAVG
          ELSE
            TAVG_3 = TAVG_2
            TAVG_2 = TAVG_1
            TAVG_1 = TAVG
            TAVG = (TMAX+TMIN) * 0.50
          ENDIF
Cjhb====================================================================        
          EMX = EXP((16.78*TMAX-116.9)/(TMAX+237.3))    !Eqation 7.11 page 174
          EMN = EXP((16.78*TMIN-116.9)/(TMIN+237.3))
          EAV = EXP((16.78*TAVG-116.9)/(TAVG+237.3))
Cjhb====================================================================        
C         ok, check the other data, if any missing, set ETVAL=-999
C         and then jump past the calculations
Cjhb====================================================================        
          if(rf(I,IY_PM,nmo,j) .lt. -998.0) then
            ETVAL=-999.0
            GOTO 50
          endif     
          if(rs2(I,IY_PM,nmo,j) .lt. -998.0) then
            ETVAL=-999.0
            GOTO 50
          endif     
          if(edpt2(I,IY_PM,nmo,j) .lt. -998.0) then
            ETVAL=-999.0
            GOTO 50
          endif     
          if(wd2(I,IY_PM,nmo,j) .lt. -998.0) then
            ETVAL=-999.0
            GOTO 50
          endif
Cjhb====================================================================        
C         ok, we have the necessary data, do the calculations
Cjhb====================================================================        
c
c---- Modified Hargreaves per AGRO Engineering
c
        if(flag1 .eq. 4) then
           if(wd2(I,IY_PM,nmo,j) .lt. 80.0) then
              F=0.0080
           elseif(wd2(I,IY_PM,nmo,j) .lt. 120.0) then
              F=0.0085
           else
              F=0.0090
           endif
           ETR(I,NDAYS(I)) = (F * rs2(I,IY_PM,nmo,j) * taved)/1498.6
           goto 200
         endif
C-----Calculate average monthly temperature and total monthly rainfall
C-----Unit conversions
         PCP(I,NDAYS(I))=rf(I,IY_PM,nmo,J)
         RS = rs2(I,IY_PM,nmo,J) / 23.892             ! langley to MJ / m2
         WD = wd2(I,IY_PM,nmo,J) * 1.609              ! mi/day to km/day 
         EDPT = 0.1 * edpt2(I,IY_PM,nmo,j)        ! mbars to kPas
c
c---- Calculate Reference ET for Alfalfa Reference
c
      if(flag1 .eq. 3) then
         CALL ETRREF(I,NMO,J,ETVAL)
      elseif(flag1 .eq. 5) then
         CALL ETASCEr(I,NMO,J,ETVAL)
      endif
50    ETR(I,NDAYS(I)) = ETVAL
c      write(999,*) NYR1+IY_PM-1, NMO, etval
c
c---- Calculate Reference ET for Grass Reference
c
c      CALL ETOREF(I,NMO,J,ETVAL)
c      ETO(I,NDAYS(I)) = ETVAL
c       if(flag1 .eq. 5) then
c         CALL ETASCEo(I,NMO,J,ETVAL)
c       endif
c       ETO(I,NDAYS(I)) = ETVAL

200   CONTINUE
      TAVE(I,NMO)=TAVE2/MONTH(NMO)
300   CONTINUE
100   continue
C
C----IF MISSING ANY VALUES, SET ENTIRE YEAR TO -999
C    ADD UP MONTHLY VALUES TO AVERAGE FOR INPUT SUMMARY
C
Cjhb====================================================================        
C     mflag(i) only set for missing temperature data
Cjhb====================================================================        
      DO 600 I=1,N_STA
         IF(MFLAG(I) .EQ. 1) THEN
           DO 550 ID=1,366
             ETR(I,ID) = -999.0
C             ETO(I,ID) = -999
550        CONTINUE
           DO 560 J=1,12
             TAVE(I,J) = -999.0
560        CONTINUE
         ENDIF
600   CONTINUE

922   FORMAT(i4,2x,i2,1x,a12,31F8.0)

      RETURN
      END
