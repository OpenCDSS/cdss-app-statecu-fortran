      SUBROUTINE KCPM(CID,NLEN,ISTART,DPASS)

C***************************************************************************
C
C   Function        : kcpm.f 
C   Author          : HB Manguerra
C   Date            : November 1995 
C   Purpose         : This calculates the daily crop coefficient for Penman-
C                     Monteith method by straight line interpolation using 
C                     the crop coefficient data set provided in file *.kpm. 
C   Calling program : growth.f
C   Called programs : none 
C   Input arguments : cid    - crop index
C                   : nlen   - length of the period for interpolation
C                   : istart - starting crop coeff index in *.kpm file
C                   : dpass  - days after the planting (emergence)
C   Output arguments: none
C   Assumptions     :  
C   Limitations     :  
C   Notes           : The routine is still valid even if nlen is less than
C                     zero.
C
C   History         : (Date, Author, Description)
C
C   11/27/95  HBM   : Created this subroutine to accomodate the new format
C                     of PM coefficients.
C
C***************************************************************************


      INCLUDE 'pmcommon.inc'
      INCLUDE 'gcommon.inc'

C-----Local Variable Declaration
      INTEGER I, II, IDY
      INTEGER CID,NLEN,ISTART,DPASS
      REAL DY1, DY2

C--------planting to peak
      II = ISTART
      DY1 = 0.01*KCDAY(CID,II)*NLEN
      DY2 = 0.01*KCDAY(CID,II+1)*NLEN
      DO 10 I = 1, NLEN
         IDY = JSTR + I - 1 + DPASS
         IF (IDY.GT.JSTP) GOTO 200
         IF (I.GT.DY2) THEN
            II = II + 1
            DY1 = DY2
            DY2 = 0.01*KCDAY(CID,II+1)*NLEN
         ENDIF
       if((i .ge. 2) .and. (kcb(cid,ii) .eq. 0)) then
         WRITE(*,*) 'Stop: Crop coefficients not available for crop type
     :',cpname(cid), 'in the *.kpm file.'    
         WRITE(999,*) 'Stop:  Crop coefficients not available for crop t
     :ype ',cpname(cid), 'in th e*.kpm file.'

          stop
       endif     
         XKCB(IDY) = KCB(CID,II)-(KCB(CID,II)-KCB(CID,II+1))*
     :	    (DY1-I)/(DY1-DY2)
 10   CONTINUE   

 200  RETURN
      END
