      SUBROUTINE KBASAL

C***************************************************************************
C
C   Function        : kbasal.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This reads the Penman-Monteith crop coefficient file
C   Calling program : proto.for 
C   Called programs : none
C   Input arguments : none   
C   Output arguments: none 
C   Assumptions     :
C   Limitations     :
C   Notes           : The model does not use all available type of crop 
C                     coefficients at the same time. The user choses a 
C                     particular type to calculate crop CU.
C
C   History         : (Date, Author, Description)
C
C   11/20/95   HBM  : Support kc mean format based on ASCE Manual 70.
C
C***************************************************************************

      INCLUDE 'pmcommon.inc'
      INCLUDE 'gcommon.inc'
      INCLUDE 'xccommon.inc'

C-----Local Variable Declaration
      CHARACTER*80 remark
      CHARACTER*200 dfile1
      CHARACTER*40 NM 
      CHARACTER*30 cropn
c rb- delete fn_len since commoned
c      INTEGER NC, ID, I, fn_len, LEN, J
      INTEGER NC, ID, I,CRVLEN, J
c      INTEGER IERR
      REAL KCDUM(40,48)

C-----Read crop coefficient file (read from input as kbcfile)
c      write(*,*)'give me a break...'
c      OPEN(UNIT=1313,FILE=kpmfile,STATUS='OLD',IOSTAT=IERR)
      OPEN(UNIT=1313,FILE=kpmfile,STATUS='OLD')
c      IF (IERR.NE.0) CALL MYEXIT(4)
      call skipn(1313)
      READ(1313,900,ERR=101) REMARK
      READ(1313,*,ERR=101) NC
      IF(FLAG1 .EQ. 3 .or. FLAG1 .eq. 5) THEN
      DO 10 I=1,NC
         CRVLEN = 22
         READ(1313,*,ERR=101) ID, cropn
         CALL INDCROP(cropn,ID)
         if(id.eq.0) then
            write(*,*)'Crop name, ',cropn, ', assigned acreage in the *
     &.cds file not included in the crop characteristic and crop coeffi
     &ent files (*.cch and *.kpm). Exiting.'
          write(999,*)'Crop name, ',cropn, ', assigned acreage in the *
     &.cds file not included in the crop characteristic and crop coeffi
     &ent files (*.cch and *.kbc). Exiting.'
            stop
         endif
         IF (cropn(1:7) .eq. 'ALFALFA') CRVLEN = 33         ! alfalfa
         IF (cropn(1:13) .eq. 'GRASS_PASTURE') CRVLEN = 11  ! grass pasture

         DO 22 J=1,CRVLEN
 22         READ(1313,*,ERR=101) KCDAY(ID,J),KCB(ID,J)

 10   CONTINUE

      ELSEIF(FLAG1 .EQ. 4) THEN

      DO 20 I=1,NC
         READ(1313,*,ERR=101) ID, cropn
         CALL INDCROP(cropn,ID)
         IF(CROPN(1:7) .EQ. 'ALFALFA') THEN
           DO 28 J=1,6
             READ(1313,*,ERR=101) KCDAY(ID,J)
 28        CONTINUE
         ELSE 
           DO 24 J=1,4
 24           READ(1313,*,ERR=101) KCDAY(ID,J)
         ENDIF
         DO 25 J=1,3
 25        READ(1313,*,ERR=101) KCB(ID,J)
 20   CONTINUE

      ENDIF
      close(1313)
 900  FORMAT(A80)

      RETURN

 101  CALL MYEXIT(11)

      END
