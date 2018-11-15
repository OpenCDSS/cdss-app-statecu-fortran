      SUBROUTINE INDCROP(cropn,id)

C***************************************************************************
C
C   Function        : indece.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This assigns the index of a crop given its name 
C                     The crop index is defined by the order of the crop
C                     characteristic file
C   Calling program : statecu.f
C   Called programs : none
C   Input arguments : none
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C
C  EW - 1/99  Send variable icnt to INDCROP.for so can return the crop
C             array number for a specified number of crop names, not just
C             total number of crop names
C
C
***************************************************************************

      INCLUDE 'gcommon.inc'

      INTEGER K, KK
      CHARACTER*30 CROPN

C-----Get Crop Index
      ID=0
      DO 20 KK= 1, DIM_NC
         IF(CPNAME(KK).EQ.CROPN) THEN
            ID = KK
            GOTO 10
         ENDIF
 20   CONTINUE
 10   RETURN
      END
          

