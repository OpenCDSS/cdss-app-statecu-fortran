      SUBROUTINE KCPM2(CID,NLEN,ISTART,DPASS)

C***************************************************************************
C
C   Function        : kcpm2.f 
C   Author          : HB Manguerra
C   Date            : November 1995 
C   Purpose         : This calculates the daily crop coefficient for Penman-
C                     Monteith method by straight line interpolation using 
C                     the crop coefficient data set provided in file *.kpm
C                     for crops other than alfalfa and pasture for the
C                     period after the effective cover.
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
C                     of PM coefficients for crops other than alfalfa and
C                     pasture grass.
C
C***************************************************************************


      INCLUDE 'pmcommon.inc'

C-----Local Variable Declaration
      INTEGER I, II, IDY
      INTEGER CID,NLEN,ISTART,DPASS
      REAL DY1, DY2

      II = ISTART
      DY1 = KCDAY(CID,II)
      DY2 = KCDAY(CID,II+1)
      DO 10 I = 1, NLEN
         IDY = JSTR + I - 1 + DPASS
         IF (IDY.GT.JSTP) GOTO 200
         IF (I.GT.DY2) THEN
            II = II + 1
            DY1 = DY2
            DY2 = KCDAY(CID,II+1)
         ENDIF
         XKCB(IDY) = KCB(CID,II)-(KCB(CID,II)-KCB(CID,II+1))*
     :	    (DY1-I)/(DY1-DY2)
 10   CONTINUE   

 200  RETURN
      END
