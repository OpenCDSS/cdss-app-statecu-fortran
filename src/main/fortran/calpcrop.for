      SUBROUTINE CALPCROP(IB,cugrass)

C***************************************************************************
C
C   Function        : calpcrop.f
C   Author          : HB Manguerra
C   Date            : May 1995 
C   Purpose         : This calculates the crop consumptive use by Blaney- 
C                     Criddle ET method, based on the SCS Modified Blaney-
C                     Criddle method. It also
C                     generates an output file *.obc that contains a 
C                     detailed results of the consumptive calculation using
C                     Blaney-Criddle.
C   Calling program : mainxc.f 
C   Called programs : clndr.f,perencrp.f, annuacrp.f, xcrain.f
C   Input arguments : ib = current basin 
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           : The routines for the original Blaney-Criddle method 
C                     are based on USBR XCONS2 program which uses the SCS 
C                     Modified Blaney-Criddle ET Estimation Method.
C
C   History         :(Date, Author, Description)
C
C     11/02/95    HBM    Units changed from 1000 acre-ft to acre-ft.
C
C     02/02/99    EW     No longer writing to tmp files, storing results of
c                        analysis in arrays.  This was done to
c                        decrease execution time.
C
C***************************************************************************

      INCLUDE 'xccommon.inc'
      INCLUDE 'gcommon.inc'
C-----------------------------------------------------------------------
C-----Local Variable Declaration
C-----------------------------------------------------------------------
      INTEGER L, K, IP , julian, IB, key, IDUM
      INTEGER nbeg, nend
      INTEGER npart(DIM_NP), icrop
      INTEGER ddiff,nbegmo(DIM_NP),nbegda(DIM_NP)
      INTEGER nendmo(DIM_NP),nendda(DIM_NP)
      REAL re2(DIM_NP,13)
      REAL re(DIM_NP,13),cu(DIM_NP,13),cuirr(DIM_NP,13)
      REAL scapasp
      REAL scap(DIM_NP),cugrass(12)
      REAL xxf(DIM_NP,12),xxkt(DIM_NP,12),xxkc(DIM_NP,12)
      REAL ttemps(DIM_NP),ttempf(DIM_NP),ddays(DIM_NP)
      REAL ddayf(DIM_NP)
      character*12 twdid, cropid
      character*40 method(DIM_NC)
      dimension icf(5)
      character rec5*5
C----------------------------------------------------------------------------
C jhb090309 for outputting season begin/end date
C   jhb111510 - re-enabled and added to official release - Erin Wilson Nov 2010
C----------------------------------------------------------------------------
      Character*10 MYCHAR1,MYCHAR2,MYCHAR3
C-----------------------------------------------------------------------
C     main loop that computes usage for all crops in the study
C-----------------------------------------------------------------------
      MONTH(2) = 28          ! reset
      DO 1000 nyr = 1, numyrs
C--------Get Growing Season (Begin and End Dates)
        do if1=1,iflood
          icf(if1)=0
        end do
        DO 799 IP = 1, nparce(IB,nyr)
          CALL CLNDR(jbeg(IP,nyr), nbegmo(IP), nbegda(IP))
          CALL CLNDR(jend(IP,nyr), nendmo(IP), nendda(IP))
  799   CONTINUE
        DO 800 IP = 1, nparce(IB,nyr)
          key = bkey(IB,IP,nyr)
          icrop = ncrop(key)
          cropid=cpname(icrop)
          call lw_update(64,cpname(icrop))
          nbeg = jbeg(IP,nyr)
          nend = jend(IP,nyr)
C----------------------------------------------------------------------------
C jhb090309 for outputting season begin/end date
C   jhb111510 - re-enabled and added to official release - Erin Wilson Nov 2010
C----------------------------------------------------------------------------
        WRITE(MYCHAR1,'(I4)') NYR
        WRITE(MYCHAR2,'(I4)') nbeg
        WRITE(MYCHAR3,'(I4)') nend
        WRITE(914,*)trim(BAS_ID(IB)),',',
     :  trim(MYCHAR1),',',
     :  trim(cpname(icrop)),',',
     :  trim(MYCHAR2),',',trim(MYCHAR3)
C----------------------------------------------------------------------------
C-----------------------------------------------------------------------
C  start calculating potential crop evapotranspiration by blaney-criddle
C-----------------------------------------------------------------------
C--------initialize monthly values to zero
          DO 60 K = 1,12
            xf(K) = 0.0
            xkt(K) = 0.0
            xkc(K) = 0.0
            nperct(K) = 0.0
            naccum(K) = 0
   60     CONTINUE
C-----------------------------------------------------------------------
C------ -perrenial or annual
C-----------------------------------------------------------------------
          IF (crptyp(key).eq.1)
     :      CALL PERENCRP(icrop,npart(IP),nbeg,nend,nbegmo(IP),
     :                    nbegda(IP),nendmo(IP),nendda(IP))
          IF (crptyp(key).eq.2)
     :      CALL ANNUACRP(icrop,npart(IP),nbeg,nend,nbegmo(IP),
     :                    nbegda(IP),nendmo(IP),nendda(IP),key)
C-----------------------------------------------------------------------
C--------compute potential crop evapotranspiration by blaney-criddle
C-----------------------------------------------------------------------
          if(crptyp(key) .eq. 0) then
            WRITE(*,*) 'Stop.',
     :    'Crop coefficients not available for crop type ',cpname(icrop)
            WRITE(999,*) 'Stop.',
     :    'Crop coefficients not available for crop type ',cpname(icrop)
            stop
          endif
C-----------------------------------------------------------------------
c  emw 8/24/04 add high altitude adjustment capabilities           
c  ktsw(icrop) = 0 SCS Modified, = 1 Original, = 2 modified w/elev
c   ktsw (icrop ) = 3 original w/elev, 4 pochop
C-----------------------------------------------------------------------
         if(ktsw(icrop).eq.0 .or. ktsw(icrop).eq.1) then 
            aadj=1.0
         elseif(ktsw(icrop).eq.2 .or. ktsw(icrop).eq.3) then
            aadj=(0.10*(belev(ib)*fT_M)/1000)+1
         endif


         DO 660 L=1,12
          !add Pochop elevation adjustment for bluegrass
          if(ktsw(icrop).eq.4) then
            if(l.eq.4 .or. l.eq.5 .or. l.eq.9 .or. l.eq.10) then
               aadj=(0.029*(belev(ib)-4429)/1000)+1
            elseif(l.eq.6 .or. l.eq.7 .or. l.eq.8) then
               aadj=(0.023*(belev(ib)-4429)/1000)+1
            endif
          endif
          !make sure elevation adjustment does not go below 1.0
          ! in other words - apply adjustment when elevation is higher than 4429', but not when it is less.
          if (aadj .lt. 1.0) then
            aadj=1.0
          endif
          if(xkt(L) .gt. -998) then
            cu(IP,L) = xf(L)*xkt(L)*xkc(L)*aadj
          else
            cu(IP,L) = -999
          endif
  660    CONTINUE
         cutot=0
         do l=1,12
            cutot=cutot+cu(ip,l)
         enddo
  662    format(i4,3x,13(f8.2))
C--------Assign variables used for generating *.obc output file
         ttemps(IP) = temps
         ddays(IP) = days
         ttempf(IP) = tempf
         ddayf(IP) = dayf
         DO 559 L=1,12
            xxf(IP,L) = xf(L)
            xxkt(IP,L) = xkt(L)
            xxkc(IP,L) = xkc(L)
  559    CONTINUE

C-----------------------------------------------------------------------
C  start calculating effective rainfall and irrigation water reqts 
C-----------------------------------------------------------------------

C--------Compute Effective Rainfall

         CALL XCRAIN(cu,re,ip,npart(IP),nbegmo(IP),nbegda(IP),
     :               nendmo(IP),nendda(IP),APD(key))
C--------initialize totals to zero

         cu(ip,13) = 0.0
         re(ip,13) = 0.0
         re2(ip,13) = 0.0
         cuirr(ip,13) = 0.0
         icuflg = 0
         ireflg = 0
         icufl2 = 0
         iflag=0



c
c ew- if any months of precip are missing, do not calculate CU for year
c
         do l=1,12
          if(re(ip,l) .lt. -998) then
             iflag=1
          endif
         enddo
         do l=1,12
           if(iflag .eq. 1) then
             re(ip,l) = -999
           endif
         enddo
         DO 661 L=1,12
            re2(ip,L) = re(ip,L)
c            write(999,*)'Rick', IB, cropid, nyr, l, re(ip,l)   'special output for rick, effective precip by crop
            if(cu(ip,L) .gt. -998 .and. re(ip,L) .gt. -998) then
              IF(re(ip,L).GT.cu(ip,L)) re(ip,L)=cu(ip,L)
              cuirr(ip,L)= cu(ip,L) - re(ip,L)
            else
              cuirr(ip,L) = -999
            endif
            if(cu(ip,L) .gt. -998) then
              cu(ip,13) = cu(ip,13) + cu(ip,L)
            else
              cu(ip,13) = -999
            endif
            if(re(ip,L) .gt. -998) then
              re(ip,13) = re(ip,13) + re(ip,L)
            else
              re(ip,13) = -999
            endif
            if(cuirr(ip,L) .gt. -998) then
              cuirr(ip,13) = cuirr(ip,13) + cuirr(ip,L)
            else
              cuirr(ip,13) = -999
            endif

            rec5=cropid(1:5)
            do if1=1,iflood
              if(rec5.eq.subname(if1)) then
                icf(if1)=1
                if(cuirr(ip,l).lt.-1.0)then
                  grass = -999.0
                else
                  grass = cuirr(ip,l)*AREA(IB,IP,nyr)/12
                endif
                area1=area(ib,ip,nyr)
                write(104,'(i5, 20f8.2)') if1, grass, area1
              endif
            end do

  661    CONTINUE
  800    CONTINUE



C--------Perform Blaney-Criddle 
         ireqt=0
         DO 200 L = 1, 12
            reqt(IB,nyr,L) = 0.0
            DO 201 IP= 1, nparce(IB,nyr)
             if(cuirr(ip,L).gt.-998) then
               reqt(IB,nyr,L)=reqt(IB,nyr,L)+cuirr(IP,L)*AREA(IB,IP,nyr)
             else
               ireqt=1
             endif
  201       CONTINUE
            if(ireqt .eq. 1) then
              reqt(ib,nyr,L)=-999
            else
              reqt(IB,nyr,L) = reqt(IB,nyr,L) / 12.0
            endif
            deplet(IB,nyr,L) = reqt(IB,nyr,L)
  200    CONTINUE
c
c --- initialize soil moisture content
c
         DO 801 IP = 1, nparce(IB,nyr)
            key = bkey(IB,IP,nyr)
            awc(key)=awcr(IB)
            irz(key) = frz(key)  ! WARNING - this nullifies the fact that
                                 ! root depth increases during the growing
                                 ! season.  This capability is only used
                                 ! in the Penman-Montieth method
            rz(key) = irz(key)
            scap(IP) = 12.0*rz(key)*awc(key)  
            scapasp = (scap(IP)*area(IB,IP,nyr))/12
            scapatot(IB,nyr) = scapatot(IB,nyr) + scapasp
  801    CONTINUE


C--------Print Results
         DO 102 IP = 1, nparce(IB,nyr) 
            key = bkey(IB,IP,nyr)
            icrop = ncrop(key)
            if(ktsw(icrop) .eq. 0) then
               method(key)='TR21 Modified Blaney-Criddle            '
            elseif(ktsw(icrop) .eq. 1) then
               method(key)='Original Blaney-Criddle                 '
            elseif(ktsw(icrop) .eq. 2) then
               method(key)='TR21 Modified BC w/Elevation Adjustment '
            elseif(ktsw(icrop) .eq. 3) then
               method(key)='Original BC w/Elevation Adjustment      '
            elseif(ktsw(icrop) .eq. 4) then
               method(key)='Pochop Bluegrass                        '
            endif

            WRITE(3,908) QUOTE,CPNAME(key),QUOTE,QUOTE,method(key),
     :         QUOTE,QUOTE,NYR1+nyr-1,QUOTE 
            WRITE(3,906) DLINE
c            IF(NBEGMO(IP) .EQ. 1) THEN
c               WRITE(3,912) 
c               GOTO 102
c            ENDIF
            WRITE(3,907) (QUOTE, IDUM=1,46)
            WRITE(3,906) SLINE

c            if(nbegmo(IP) .eq. nendmo(IP)) then
c             do l=1,12
c              ETTOT(IB,NYR,L) = -999
c              EFFPPT(IB,NYR,L) = -999
c             enddo
c            goto 106
c            endif
            if((nbegmo(IP).lt.1).or.(nendmo(IP).gt.12)) then
             do l=1,12
              ETTOT(IB,NYR,L) = -999.0
              EFFPPT(IB,NYR,L) = -999.0
             enddo
            goto 106
            endif
            DO 101 L = nbegmo(IP), nendmo(IP)
c               if(nbegmo(ip) .eq. 1) then
c                  nendmo(ip)=1
c                  WRITE(3,905) QUOTE
c                  goto 105
c               endif
               IF (L.EQ.nbegmo(IP)) THEN
                  WRITE(3,901) QUOTE,AMN(L),nbegda(IP),QUOTE,100.0*
     :          npart(IP)/
     :            month(L),ttemps(IP),ddays(IP),xxf(IP,L),xxkt(IP,L),
     :            xxkc(IP,L),xxkt(IP,L)*xxkc(IP,L),cu(IP,L),re(IP,L),
     :            cuirr(IP,L)
               ELSEIF (L.EQ.nendmo(IP)) THEN
                  WRITE(3,901) QUOTE,AMN(L),nendda(IP),QUOTE,100.0*
     :            nendda(IP)/
     :            month(L),ttempf(IP),ddayf(IP),xxf(IP,L),xxkt(IP,L),
     :            xxkc(IP,L),xxkt(IP,L)*xxkc(IP,L),cu(IP,L),re(IP,L),
     :            cuirr(IP,L)
               ELSE
                  WRITE(3,902)QUOTE,AMN(L),QUOTE,tmean(nyr,L),pclite(L),
     :            xxf(IP,L),xxkt(IP,L),xxkc(IP,L),xxkt(IP,L)*
     :            xxkc(IP,L),cu(IP,L),re(IP,L),cuirr(IP,L)
               ENDIF   
105        IF(CU(IP,L) .LT. -998) THEN
              ETTOT(IB,NYR,L) = -999
           ELSE
           ETTOT(IB,NYR,L)=ETTOT(IB,NYR,L)+CU(IP,L)*AREA(IB,IP,nyr)/12
           ENDIF
           IF(RE(IP,L) .LT. -998) THEN
             EFFPPT(IB,NYR,L) = -999
           ELSE
           EFFPPT(IB,NYR,L)=EFFPPT(IB,NYR,L)+RE(IP,L)*AREA(IB,IP,nyr)/12
           ENDIF
  101       CONTINUE
106         WRITE(3,906) SLINE
            WRITE(3,904) (QUOTE, IDUM=1,16),cu(IP,13),re(IP,13),
     :           cuirr(IP,13)
            WRITE(3,903)

  102    CONTINUE

         do if1=1,iflood
           if(icf(if1) .eq. 0) then
             do i=1,12
               write(104,'(i5, 20f8.2)') if1, 0.0, 0.0
             enddo
           endif
         end do


 1000 CONTINUE

  903 FORMAT(87(" "))
  906 FORMAT(A87)
  907 FORMAT(A1,'Month',A1,2x,A1,'percent',A1,A1,'temp',A1,A1,
     :'daylight',A1,2x,A1,'f',A1,4x,A1,'kt',A1,3x,A1,'kc',A1,5x,
     :A1,'k',A1,5x,A1,'ETp',A1,4x,A1,'Re',A1,3x,A1,'IWR'/
     :2x,A1,A1,5x,A1,'month',A1,71(" ")/          
     :2x,A1,A1,7x,A1,A1,5x,A1,'(F)',A1,3x,A1,'(%)',A1,6x,A1,A1,6x,
     :A1,A1,5x,A1,A1,7x,A1,A1,3x,A1,'(in)',A1,3x,A1,'(in)',A1,1x,A1,
     :'(in)')
  908 FORMAT(A1,A30,A1,A1,'METHOD=',A35,A1,A1,'Year=',
     :I4,A1)
  901 FORMAT(A1,A3,I2,A1,F8.1,9F8.2)
  905 FORMAT(A1,'n/a',3x,'  -999.0',9(' -999.00'))
  902 FORMAT(A1,A3,A1,2x,'   100.0',9F8.2)
  904 FORMAT(A1,'Season Total',A1,14A1,35x,3F8.2)
  912 FORMAT(1X,'Missing Climate Data, CU Not Calculated',47(" ")/
     : 87(" ")/87(" "))

      RETURN
      END
