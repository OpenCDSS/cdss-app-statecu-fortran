      SUBROUTINE PERENCRP(icrop,npart,nbeg,nend,nbegmo,nbegda,nendmo,
     :                    nendda)

C***************************************************************************
C
C   Function        : perencrp.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This computes the crop coefficient, mean temperature
C                     percent daylight hours and parameter f of the month
C                     for perennial crops for the Blaney-Criddle method.
C                     It includes the computation required for part months
C                     (i.e. beginning and ending months) of the growing
C                     season.
C   Calling program : calpcrop.f
C   Called programs : clndr.f, intertd.f
C   Input arguments : nend  = end of growing season in julian day
C   Input arguments : icrop = crop index
C                     nbeg  = start of growing season in julian day
C                     nend  = end of growing season in julian day
C                     nbegmo = begin month of growing season
C                     nbegda = begin day of growing season
C                     nendmo = end month of growing season
C                     nendda = end day of growing season
C   Output arguments: npart = number of days in beginning month  in spring
C   Assumptions     :
C   Limitations     :
C   Notes           : The routines are based on USBR XCONS2 program which
C                     uses the SCS Modified Blaney-Criddle ET Estimation
C                     Method.
C
C***************************************************************************

      INCLUDE 'gcommon.inc'
      INCLUDE 'xccommon.inc'

C-----Local variable declaration
      INTEGER K, nend, npart, JULIAN
      INTEGER J, midspr, nbeg, icrop
      INTEGER begmon, endmon, midfal, mid
      INTEGER nbegmo, nbegda, nendmo, nendda

C-----Calculate midpoint of beginning month in spring
      if(nbegda .lt. -998 .or. nendda .lt. -998) then
         do 78 k=1,12
            xkt(k) = -999
            xkc(k) = -999
            xf(k) = -999
  78     continue
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

      midpts=((month(nbegmo)-nbegda + 1 )/2) + nbegda

C-----Calculate midpoint of ending month in spring

      midptf= (nendda + 1)/2

C-----Calculate number of days in beginning month of spring

      npart=(month(nbegmo)-nbegda + 1 )

C-----interpolate temperature and dayhours for beginning month in spring
      DO  65 K = 1, 8
         IF(JULIAN(nbegmo, midpts) .LT. middle(K)) THEN
            CALL INTERTD(temps,days,K,nyr,npart,nbegmo,
     :         midpts)
            GO TO 69
         ENDIF
         IF(JULIAN(nbegmo, midpts) .EQ. middle(k)) THEN
            temps = tmean(nyr,k)
            days = pclite(k)
            GO TO 69
         ENDIF
   65 CONTINUE
      call MYEXIT(99)

C-----interpolate temperature and dayhours for ending month in spring
   69 DO  72 k = 1,12
         IF( JULIAN(nendmo, midptf).LT. middle(K)) THEN
            CALL INTERTD(tempf,dayf,K,nyr,nendda,
     :         nendmo, midptf)
            GO TO 77
         ENDIF
         IF( JULIAN(nendmo, midptf) .EQ. middle(K)) THEN
            tempf = tmean(nyr, K)
            dayf  = pclite(K)
            GO TO 77
         ENDIF
   72 CONTINUE
      call MYEXIT(99)

   77 CONTINUE

C-----interpolate kc for beginning month in spring

      midspr = nbeg - nbegda + midpts
      DO 80 K=1,25
         IF(nckca(K).GT.midspr) GO TO 82
         IF(nckca(K).EQ.midspr) THEN
            xkc(nbegmo) = ckca(icrop,K)
            GO TO 83
         ENDIF
   80 CONTINUE
   82 xkc(nbegmo) = ckca(icrop,K-1) + (ckca(icrop,K) 
     :   -  ckca(icrop,K-1))*((midspr- REAL(nckca(k-1)))/(nckca(K)- 
     :   REAL(nckca(k-1))))
   83 xf(nbegmo)=(temps*days)/100.0

c emw 08/24/04 add logic for Pochop's Method for Blue Grass

       if(ktsw(icrop).eq.0 .or. ktsw(icrop).eq.2) then 
        IF(temps.LT.36.0) THEN
         xkt(nbegmo)=0.3
        ELSE
         xkt(nbegmo)=0.0173*temps-0.314
        ENDIF
       elseif(ktsw(icrop).eq.1 .or. ktsw(icrop).eq.3) then
         xkt(nbegmo)=1.0
       elseif(ktsw(icrop).eq.4) then
         xkt(nbegmo)=0.00328*temps+0.65011
       endif


C-----------------------------------------------------------------------
C        full months -- kc computation
C-----------------------------------------------------------------------

      mid=15
      begmon = nbegmo + 1
      endmon= nendmo - 1
      DO 90 K = begmon,endmon
         DO 87 J=1,25
            IF(nckca(J).EQ.JULIAN(K,mid)) GO TO 88
   87    CONTINUE
         call MYEXIT(99)

   88    xkc(K)  =  ckca(icrop, J)
         xf(K)=(tmean(nyr,K)*pclite(K))/100.0

c grb 05-20-00 add logic for original blaney criddle (kt=1)          
c emw 08/24/04 add logic for Pochop's Method for Blue Grass
        if(ktsw(icrop).eq.0 .or. ktsw(icrop).eq.2) then
            IF(tmean(nyr,K).LT.36.0) THEN
              xkt(K)=0.3
            ELSE
              xkt(K)=0.0173*tmean(nyr,K)-0.314
            ENDIF
        elseif(ktsw(icrop).eq.1 .or. ktsw(icrop).eq.3) then
            xkt(k)=1.0
        elseif(ktsw(icrop).eq.4) then
            xkt(K)=0.00328*tmean(nyr,K)+0.65011
        endif

   90 CONTINUE

C-----interpolate kc for ending month in fall
      midfal = nend-midptf
      DO 95 K=1,25
         IF(nckca(K).GT.midfal) GO TO 96
         IF(nckca(K).EQ.midfal) xkc(nendmo) = 
     :      ckca(icrop,K)
         IF(nckca(K).EQ.midfal) GO TO 97
   95 CONTINUE
   96 xkc(nendmo) = ckca(icrop,K-1)+(ckca(icrop,K) 
     :   - ckca(icrop,K-1))*((midfal- REAL(nckca(k-1)))/(nckca(K)-
     :   REAL(nckca(k-1))))
   97 xf(nendmo)=(tempf*dayf)/100.0

c grb 05-20-00 add logic for original blaney criddle (kt=1)
c emw 08/24/04 add logic for Pochop's Method for Blue Grass

      if(ktsw(icrop).eq.0 .or. ktsw(icrop).eq.2) then 
      IF(tempf.LT.36.0) THEN
         xkt(nendmo) = 0.3
        ELSE
         xkt(nendmo) = 0.0173*tempf-0.314
        ENDIF
       elseif(ktsw(icrop).eq.1 .or. ktsw(icrop).eq.3) then
         xkt(nendmo)=1.0
       elseif(ktsw(icrop).eq.4) then
         xkt(nendmo)=0.00328*temps+0.65011
      endif

      RETURN
      END 

