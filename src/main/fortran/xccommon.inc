C
C
C   Include File    : xccommon.inc
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This includes declaration of all variables used
C                     in the Blaney-Criddle calculation of evapotranspi-
C                     ration.
C   Calling program : annuacrp.f, calpcrop.f, interkc.f, intertd.f,
C                     mainxc.f, perencrp.f, readin.f, xcrain.f, crptyp.f
C   Assumptions     :
C   Limitations     :
C   Notes           :
C
C***************************************************************************

C-----Dimension (not dependent on global dimension)
      INTEGER PKCLEN    ! perrenial crop max (covers annual crops)
      PARAMETER (PKCLEN = 70)
      INTEGER XC_CRP    ! number of crops considered in xcons2
      PARAMETER (XC_CRP = 70)
      INTEGER DIM_NC1   ! number of crop-soil-date combinations 
      PARAMETER (DIM_NC1 = 999)


      REAL pclite(12), xf(12), xkt(12), xkc(12)
      REAL ckca(xc_crp,pkclen), ckcp(xc_crp,pkclen)
      REAL days, dayf, temps, tempf

      INTEGER midpts, midptf
      INTEGER nyr, prectype, numyrs, nucrps
      INTEGER nckca(pkclen), nckcp(pkclen), ncrop(DIM_NC1), 
     : ngrows(DIM_NC1)
      INTEGER naccum(12), nperct(12)
      CHARACTER*40 crop(DIM_NC1)

      COMMON /IDS01/ckca, ckcp, nckca, nckcp, prectype
      COMMON /XXX01/nyr
      COMMON /XXX02/ pclite
      COMMON /XXX04/numyrs, nucrps
      COMMON /Z2/ncrop, ngrows, xf, xkt, xkc
      COMMON /Z2a/ naccum
      COMMON /Z3/ nperct
      COMMON /CHAR1/ crop
      COMMON /CH2/ days, dayf, temps, tempf
      COMMON /CH3/ midpts, midptf
