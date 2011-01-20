      SUBROUTINE DXCRAIN(cu,re,ip,npart,nbegmo,nbegda,nendmo,nendda,
     :                  apdep,l)

C***************************************************************************
C
C   Function        : dxcrain.f 
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This calculates the monthly effective rainfall for one month only.
C   Calling program : proto.f 
C   Called programs : none 
C   Input arguments : cu    = monthly potential crop evapotranspiration 
C                     ip    = current parcel
C                     npart = number of days in beginning month  in spring
C                     nbegmo= begin month of the growing season
C                     nbegda= begin day of the growing season
C                     nendmo= end month of the growing season
C                     nendda= end day of the growing season
C                     apdep = max application depth of crop
C   Output arguments: re    = monthly effective rainfall
C   Assumptions     :
C   Limitations     :
C   Notes           : The routines are based on USBR XCONS2 program which
C                     uses the SCS Method based on a Net Application Depth
C                     (rn_xco=1); and USBR method (rn_xco=2). If prectype
C                     is not equal to 1 or 2, rainfall is not considered and
C                     effective rainfall is assumed zero.
C
C***************************************************************************

      INCLUDE 'gcommon.inc'
      INCLUDE 'xccommon.inc'

C-----Local Variable Declaration
      INTEGER l, npart, ip
      INTEGER nbegmo,nbegda,nendmo,nendda
      REAL f, X, re(DIM_NP,13), cu(DIM_NP,13), apdep
      real*8 testx

C-----------------------------------------------------------------------
C              compute effective precipitation for each crop default
C              apdep value: 3 inch net depth of application; f=1.0
C              if apdep is specified, f value is computed based on tr-21.
C-----------------------------------------------------------------------
            IF(apdep.EQ.0.) THEN
               F = 1.0
            ELSEIF(apdep.GT.0.) THEN
               F=0.531747+0.295164*apdep-0.057697*apdep**2
     :         + 0.003804*apdep**3
            ENDIF


C-----------------------------------------------------------------------
C        compute precipitation using the scs method  - monthly
C        - uses the apdep value
C-----------------------------------------------------------------------
c
      IF(rn_xco.EQ.1) THEN
         if(cu(ip,l) .gt. -998) then
          IF(l.EQ.nbegmo) cu(ip,l)=cu(ip,l)*month(nbegmo)/npart
          IF(l.EQ.nendmo) cu(ip,l)=cu(ip,l)*month(nendmo)/
     :      nendda
         else
           cu(ip,l) = -999
         endif
         if(rntot(nyr,l) .lt. -998) then
            re(ip,l) = -999
            goto 10
         endif
         if(rntot(nyr,l) .lt. 0 .and. rntot(nyr,l) .gt. -998) then
            rntot(nyr,l)=0
         endif
         if(cu(ip,l) .gt. -998) then
           re(ip,l) = (0.70917*(rntot(nyr,l)**0.82416)-0.11556)*
     :      (10**(0.02426*cu(ip,l)))*F
         else
           re(ip,l) = -999
           goto 10
         endif

         IF(l.EQ.nbegmo) cu(ip,l)=cu(ip,l)*npart/month(nbegmo)
         IF(l.EQ.nendmo) cu(ip,l)=cu(ip,l)*nendda/month(nendmo)

C-----------------------------------------------------------------------
C              compute precipitation using the usbr method
C-----------------------------------------------------------------------

      ELSEIF (rn_xco.eq.2) THEN
         if(rntot(nyr,l) .lt. -998) then
            re(ip,l) = -999
            goto 10
         ELSEIF(rntot(nyr,l).LE.1.0) THEN
            re(ip,l) = rntot(nyr,l)*0.95
         ELSEIF(rntot(nyr,l).LE.2.0) THEN
            re(ip,l) = ((rntot(nyr,l)-1.0)*0.90) + 0.95
         ELSEIF(rntot(nyr,l).LE.3.0) THEN
            re(ip,l) = ((rntot(nyr,l)-2.0)*0.82) + 1.85
         ELSEIF(rntot(nyr,l).LE.4.0) THEN
            re(ip,l) = ((rntot(nyr,l)-3.0)*0.65) + 2.67
         ELSEIF(rntot(nyr,l).LE.5.0) THEN
            re(ip,l) = ((rntot(nyr,l)-4.0)*0.45) + 3.32
         ELSEIF(rntot(nyr,l).LE.6.0) THEN
            re(ip,l) = ((rntot(nyr,l)-5.0)*0.25) + 3.77
         ELSE
            re(ip,l) = ((rntot(nyr,l)-6.0)*0.05) + 4.02
         ENDIF
      
C-----Do not consider rainfall
      ELSE
        re(ip,l) = 0.0
        goto 10
      ENDIF
C-----check values

      if (re(ip,L).lt.0) re(ip,L) = 0.0
      IF(re(ip,L).GT.rntot(nyr,L)) re(ip,L) = rntot(nyr,L)
      X = month(L)
      IF(L.EQ.nbegmo) re(ip,L) = (( X-nbegda+1.0)/X)*re(ip,L)
      IF(L.EQ.nendmo) re(ip,L) = (nendda / X ) * re(ip,L)
      IF(L.eq.nbegmo.AND.L.eq.nendmo) then 
        re(ip,L) = ((nendda - nbegda + 1.0)/X) * re(ip,L)
        nendda = nbegda
      endif
      if (re(ip,L).lt.0) re(ip,L) = 0.0

10    RETURN
      END

