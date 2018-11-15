c dsum
c_________________________________________________________________NoticeStart_
c StateCU Consumptive Use Model
c StateCU is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2018 Colorado Department of Natural Resources
c 
c StateCU is free software:  you can redistribute it and/or modify
c     it under the terms of the GNU General Public License as published by
c     the Free Software Foundation, either version 3 of the License, or
c     (at your option) any later version.
c 
c     StateCU is distributed in the hope that it will be useful,
c     but WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU General Public License for more details.
c 
c     You should have received a copy of the GNU General Public License
c     along with StateCU.  If not, see <https://www.gnu.org/licenses/>.
c_________________________________________________________________NoticeEnd___

               SUBROUTINE DSUM

C***************************************************************************
C
C   Function        : dsum.for
C   Author          : e wilson
C   Date            : August 1999
C   Purpose         : This summarizes input information and save them in
C                     *.sum output file for daily ET methods only.
c                     The summary can either be basic or detailed.
C   Calling program : statecu.f
C   Called programs :
C   Input arguments : none
C   Output arguments: nonee
C   Assumptions     :
C   Limitations     :
C   Notes           : Flag:
C                         SOUT = 0 - basic summary
C                         SOUT = 1 - detailed summary
C
C***************************************************************************

      INCLUDE 'gcommon.inc'
      INCLUDE 'pmcommon.inc'

C-----Local variable declaration

      INTEGER YR,TYR,IB,IY,ID,IM,IS
      REAL TAVE1(12)
      INTEGER I,J,K,Y,IDUM,ISUM(12),MFLAG(DIM_NY)
      REAL SUM,SSUMT,SSUM,SUMT(12),sumt2(12),ssumt2
      INTEGER IERR
      character*12 tid

c
c-----Open up climate files
c
c      Open(60,file=tmxfile,status='OLD',iostat=ierr)
c      Open(61,file=tmnfile,status='OLD',iostat=ierr)
c      if(ipdy .eq. 1) then 
c        Open(62,file=pdyfile,status='OLD',iostat=ierr)
c      endif
c      Open(63,file=solfile,status='OLD',iostat=ierr)
c      if(flag1 .eq. 3 .or. flag1 .eq. 5) then
c          Open(64,file=vapfile,status='OLD',iostat=ierr)
c      endif
c      Open(65,file=wndfile,status='OLD',iostat=ierr)


      ISKIP=N_STA-1

      DO 800 I=1,N_STA

C--- skip to first year of data and N-sta
c      call skipn(60)
c      read(60,921) idum,idum
c745   read(60,923) tyr,tid
c      if(tyr.lt.nyr1) goto 745
c      if(tid .ne. wsid(I)) goto 745
c      backspace(60)

c      call skipn(61)
c      read(61,921) idum,idum
c746   read(61,923) tyr,tid
c      if(tyr.lt.nyr1) goto 746   
c      if(tid .ne. wsid(I)) goto 746
c      backspace(61)

c      if(ipdy .eq. 1) then 
c        call skipn(62)
c        read(62,921) idum,idum
c747     read(62,923) tyr,tid
c        if(tyr.lt.nyr1) goto 747   
c        if(tid .ne. wsid(I)) goto 747
c        backspace(62)
c      endif

c      call skipn(63)
c      read(63,921) idum,idum
c748   read(63,923) tyr,tid
c      if(tyr.lt.nyr1) goto 748   
c      if(tid .ne. wsid(I)) goto 748
c      backspace(63)

c      if(flag1 .eq. 3 .or. flag1 .eq. 5) then
c      call skipn(64)
c      read(64,921) idum,idum
c749   read(64,923) tyr,tid
c      if(tyr.lt.nyr1) goto 749   
c      if(tid .ne. wsid(I)) goto 749
c      backspace(64)
c      endif

c      call skipn(65)
c      read(65,921) idum,idum
c750   read(65,923) tyr,tid
c      if(tyr.lt.nyr1) goto 750   
c      if(tid .ne. wsid(I)) goto 750
c      backspace(65)

      if(flag1 .eq. 3 .or. flag1 .eq. 5) then
         IF (I.LT.10) THEN
            WRITE(8,951) QUOTE,I,WSID(I),QUOTE
         ELSE
            WRITE(8,970) QUOTE,I,WSID(I),QUOTE
         ENDIF
      elseif(flag1 .eq. 4) then
         IF (I.LT.10) THEN
            WRITE(8,950) QUOTE,I,WSID(I),QUOTE
         ELSE
            WRITE(8,971) QUOTE,I,WSID(I),QUOTE
         ENDIF
      endif


         WRITE(8,999) DLLINE
         WRITE(8,998) (QUOTE, IDUM = 1,28)
         WRITE(8,999) SLLINE
         if(sout .eq. 1) WRITE(8,957) QUOTE,QUOTE
c
c---start loop for average temperature
c
        SSUMT = 0
        ISUMT=0
        do 30 im=1,12
            SUMT(IM) = 0.0
            ISUM(IM) = 0.0
30      continue

        DO 110 IY = 1,NYRS
          DO 100 IM=1,12
             TAVE1(IM) = 0.0
100       CONTINUE
          SSUM=0
          mflag(IY)=0

          DO 90 IM = 1,12
          tave2=0
c          READ(60,922) YR, MO, tid,(TMX(J),J=1,31)
c          READ(61,922) YR, MO, tid,(TMN(J),J=1,31)

c          IF(TID .NE. WSID(I)) GOTO 90
          DO 50 J=1,MONTH(IM)
            if((tmx(I,IY,IM,J).lt.-998)
     &          .or.(tmn(I,IY,IM,J).lt.-998)) mflag(IY)=1
50          tave2=tave2+(tmx(I,IY,IM,J)+tmn(I,IY,IM,J))/2

         TAVE1(IM)=TAVE2/MONTH(IM)
         SSUM = SSUM + TAVE1(IM)      !ADD UP ALL MONTHS
         IF(MFLAG(IY) .EQ. 0) THEN
            SUMT(IM)=SUMT(IM)+TAVE1(IM)
            ISUM(IM) = ISUM(IM)+1
         ENDIF
         if(iy .eq. nyrs .and. im .eq. 12) goto 94
c         CALL SKIPLN(60,iskip)
c         CALL SKIPLN(61,iskip)

90      CONTINUE

94           IF(MFLAG(IY) .EQ. 1) THEN
              DO 95 IM=1,12
                TAVE1(IM) = -999
                SSUM = -999
95            CONTINUE
             ELSE
                SSUM=SSUM/12
                SSUMT=SSUMT+SSUM
                ISUMT=ISUMT+1
             ENDIF

         if(sout .eq. 1) WRITE(8,962) IY+NYR1-1,(TAVE1(IM),IM=1,12),SSUM
110    CONTINUE

          do im=1,12
           if(isum(im) .eq. 0) then
              sumt2(im)=-999
           else
              sumt2(im)=sumt(im)/isum(im)
           endif
          enddo
           if(isumt .eq. 0) then
              ssumt2=-999
           else
              ssumt2 = ssumt/isumt
           endif

         if(sout .eq. 1) then
           WRITE(8,963) QUOTE,QUOTE,(sumt2(im),
     :            IM=1,12),ssumt2
           WRITE(8,999) SLLINE
         else
           WRITE(8,952) QUOTE,QUOTE,(sumt2(im),
     :            IM=1,12),ssumt2
         endif

c
c---start loop for total precipitation
c
        if(ipdy .eq. 1) then
        if(sout .eq. 1) WRITE(8,961) QUOTE,QUOTE
        SSUMT = 0
        ISUMT=0
        do 130 im=1,12
            SUMT(IM) = 0.0
            ISUM(IM) = 0.0
130      continue

        DO 210 IY = 1,NYRS
          DO 200 IM=1,12
             TAVE1(IM) = 0.0
200        CONTINUE
          SSUM=0
          mflag(IY)=0

          DO 190 IM = 1,12
          tave2=0
c          READ(62,922) YR, MO, tid,(TMX(J),J=1,31)

c          IF(TID .NE. WSID(I)) GOTO 190
          DO 150 J=1,MONTH(IM)
            if(rf(I,IY,IM,J) .lt. -998) mflag(IY)=1
150         tave1(IM)=tave1(IM)+rf(I,IY,IM,J)

         SSUM = SSUM + TAVE1(IM)      !ADD UP ALL MONTHS
         IF(MFLAG(IY) .EQ. 0) THEN
            SUMT(IM)=SUMT(IM)+TAVE1(IM)
            ISUM(IM) = ISUM(IM)+1
         ENDIF
         if(iy .eq. nyrs .and. im .eq. 12) goto 194
c         CALL SKIPLN(62,iskip)

190     CONTINUE

194          IF(MFLAG(IY) .EQ. 1) THEN
              DO 195 IM=1,12
                TAVE1(IM) = -999
                SSUM = -999
195           CONTINUE
             ELSE
                SSUMT=SSUMT+SSUM
                ISUMT=ISUMT+1
             ENDIF
         if(sout .eq. 1) WRITE(8,962) IY+NYR1-1,(TAVE1(IM),IM=1,12),SSUM
210    CONTINUE
       if(sout .eq. 1) then
         WRITE(8,963) QUOTE,QUOTE,(SUMT(IM)/ISUM(IM),
     :            IM=1,12),SSUMT/ISUMT
         WRITE(8,999) SLLINE
       else
         WRITE(8,956) QUOTE,QUOTE,(SUMT(IM)/ISUM(IM),
     :            IM=1,12),SSUMT/ISUMT
       endif
       endif
c
c---start loop for average solar radiation
c
        if(sout .eq. 1) WRITE(8,959) QUOTE,QUOTE
        SSUMT = 0
        ISUMT=0
        do 230 im=1,12
            SUMT(IM) = 0.0
            ISUM(IM) = 0.0
230      continue

        DO 310 IY = 1,NYRS
          DO 300 IM=1,12
             TAVE1(IM) = 0.0
300        CONTINUE
          SSUM=0
          mflag(IY)=0

          DO 290 IM = 1,12
          tave2=0
c          READ(63,922) YR, MO, tid,(TMX(J),J=1,31)

c          IF(TID .NE. WSID(I)) GOTO 290
          DO 250 J=1,MONTH(IM)
            if(rs2(I,IY,IM,J) .lt. -998) mflag(IY)=1
250         tave1(IM)=tave1(IM)+rs2(I,IY,IM,J)

         TAVE1(IM)=TAVE1(IM)/MONTH(IM)
         SSUM = SSUM + TAVE1(IM)      !ADD UP ALL MONTHS
         IF(MFLAG(IY) .EQ. 0) THEN
            SUMT(IM)=SUMT(IM)+TAVE1(IM)
            ISUM(IM) = ISUM(IM)+1
         ENDIF
         if(iy .eq. nyrs .and. im .eq. 12) goto 294
c         CALL SKIPLN(63,iskip)

290     CONTINUE

294          IF(MFLAG(IY) .EQ. 1) THEN
              DO 295 IM=1,12
                TAVE1(IM) = -999
                SSUM = -999
295           CONTINUE
             ELSE
                SSUM=SSUM/12
                SSUMT=SSUMT+SSUM
                ISUMT=ISUMT+1
             ENDIF
         if(sout .eq. 1) WRITE(8,962) IY+NYR1-1,(TAVE1(IM),IM=1,12),SSUM
310    CONTINUE
       if(sout .eq. 1) then
         WRITE(8,963) QUOTE,QUOTE,(SUMT(IM)/ISUM(IM),
     :            IM=1,12),SSUMT/ISUMT
         WRITE(8,999) SLLINE
       else
         WRITE(8,954) QUOTE,QUOTE,(SUMT(IM)/ISUM(IM),
     :            IM=1,12),SSUMT/ISUMT
       endif
c
c---start loop for average vapor pressure
c
      if(flag1.eq.3 .or. flag1 .eq. 5) then
        if(sout .eq. 1) WRITE(8,958) QUOTE,QUOTE
        SSUMT = 0
        ISUMT=0
        do 330 im=1,12
            SUMT(IM) = 0.0
            ISUM(IM) = 0.0
330      continue

        DO 410 IY = 1,NYRS
          DO 400 IM=1,12
             TAVE1(IM) = 0.0
400        CONTINUE
          SSUM=0
          mflag(IY)=0

          DO 390 IM = 1,12
          tave2=0
c          READ(64,922) YR, MO, tid,(TMX(J),J=1,31)

c          IF(TID .NE. WSID(I)) GOTO 390
          DO 350 J=1,MONTH(IM)
            if(edpt2(I,IY,IM,J) .lt. -998) mflag(IY)=1
350         tave1(IM)=tave1(IM)+edpt2(I,IY,IM,J)

         TAVE1(IM)=TAVE1(IM)/MONTH(IM)
         SSUM = SSUM + TAVE1(IM)      !ADD UP ALL MONTHS
         IF(MFLAG(IY) .EQ. 0) THEN
            SUMT(IM)=SUMT(IM)+TAVE1(IM)
            ISUM(IM) = ISUM(IM)+1
         ENDIF
         if(iy .eq. nyrs .and. im .eq. 12) goto 394
c         CALL SKIPLN(64,iskip)

390     CONTINUE

394          IF(MFLAG(IY) .EQ. 1) THEN
              DO 395 IM=1,12
                TAVE1(IM) = -999
                SSUM = -999
395           CONTINUE
             ELSE
                SSUM=SSUM/12
                SSUMT=SSUMT+SSUM
                ISUMT=ISUMT+1
             ENDIF
         if(sout .eq. 1) WRITE(8,962) IY+NYR1-1,(TAVE1(IM),IM=1,12),SSUM
410    CONTINUE
       if(sout .eq. 1) then
         WRITE(8,963) QUOTE,QUOTE,(SUMT(IM)/ISUM(IM),
     :            IM=1,12),SSUMT/ISUMT
         WRITE(8,999) SLLINE
       else
         WRITE(8,953) QUOTE,QUOTE,(SUMT(IM)/ISUM(IM),
     :            IM=1,12),SSUMT/ISUMT
       endif
      endif
c
c---start loop for wind speed
c
        if(sout .eq. 1) WRITE(8,960) QUOTE,QUOTE
        SSUMT = 0
        ISUMT=0
        do 430 im=1,12
            SUMT(IM) = 0.0
            ISUM(IM) = 0.0
430      continue

        DO 510 IY = 1,NYRS
          DO 500 IM=1,12
             TAVE1(IM) = 0.0
500        CONTINUE
          SSUM=0
          mflag(IY)=0

          DO 490 IM = 1,12
          tave2=0
c          READ(65,922) YR, MO, tid,(TMX(J),J=1,31)

c          IF(TID .NE. WSID(I)) GOTO 490
          DO 450 J=1,MONTH(IM)
            if(wd2(I,IY,IM,J) .lt. -998) mflag(IY)=1
450         tave1(IM)=tave1(IM)+wd2(I,IY,IM,J)

         TAVE1(IM)=TAVE1(IM)/MONTH(IM)
         SSUM = SSUM + TAVE1(IM)      !ADD UP ALL MONTHS
         IF(MFLAG(IY) .EQ. 0) THEN
            SUMT(IM)=SUMT(IM)+TAVE1(IM)
            ISUM(IM) = ISUM(IM)+1
         ENDIF
         if(iy .eq. nyrs .and. im .eq. 12) goto 494
c         CALL SKIPLN(65,iskip)

490     CONTINUE

494          IF(MFLAG(IY) .EQ. 1) THEN
              DO 495 IM=1,12
                TAVE1(IM) = -999
                SSUM = -999
495           CONTINUE
             ELSE
                SSUM=SSUM/12
                SSUMT=SSUMT+SSUM
                ISUMT=ISUMT+1
             ENDIF
         if(sout .eq. 1) WRITE(8,962) IY+NYR1-1,(TAVE1(IM),IM=1,12),SSUM
510    CONTINUE
       if(sout .eq. 1) then
         WRITE(8,963) QUOTE,QUOTE,(SUMT(IM)/ISUM(IM),
     :            IM=1,12),SSUMT/ISUMT
         WRITE(8,999) SLLINE
       else
         WRITE(8,955) QUOTE,QUOTE,(SUMT(IM)/ISUM(IM),
     :            IM=1,12),SSUMT/ISUMT
         WRITE(8,999) SLLINE
       endif

       
c       rewind(60)
c       rewind(61)
c       if(ipdy .eq. 1) then
c          rewind(62)
c       endif
c       rewind(63)
c       if(flag1 .eq. 3 .or. flag1 .eq. 5) rewind(64)
c       rewind(65)
       WRITE(8,898)
       WRITE(8,898)


 800   CONTINUE



C-----Write Weight Matrix for Blaney-Criddle
          WRITE(8,968) QUOTE,QUOTE
          do 600 ib=1, nbasin
            write(8,897) bas_id(ib)
            do 601 is=1, n_sta
              if(wrs(ib,is).gt.0.or.wws(ib,is).gt.0) then
                write(8,980) wsid(is),wws(ib,is),wrs(ib,is)
 980            format(4x,a8,2f6.2,97(" "))
                goto 601
              endif
 601        continue         
 600      continue
          WRITE(8,898)

c      close(60)
c      close(61)
c      if(ipdy .eq. 1) then
c         close(62)
c      endif
c      close(63)
c      if(flag1 .eq. 3 .or. flag1 .eq. 5) close(64)
c      close(65)

 897  FORMAT(A40,81(" "))
 898  FORMAT(121(" "))
 921  FORMAT(6x,i4,11x,i4)
 922  FORMAT(i4,2x,i2,1x,a12,31F8.0)
 923  FORMAT(i4,5x,a12)
 950  FORMAT(A1,'Modified Hargreaves Weather Parameters = (St',I1,')',
     :1X,A12,A1,3(" "))
 951  FORMAT(A1,'Penman-Monteith Weather Parameters = (St',I1,')',
     :1X,A12,A1,7(" "))
 952  FORMAT(A1,'Mean Temp (F) ',A1,13F8.2,1(" "))
 953  FORMAT(A1,'V. Press (mb) ',A1,13F8.2,1(" "))
 954  FORMAT(A1,'Solar (lang/d)',A1,13F8.2,1(" "))
 955  FORMAT(A1,'Wind  (mi/day)',A1,13F8.2)
 956  FORMAT(A1,'Rainfall (in) ',A1,13F8.2,1(" "))
 957  FORMAT(A1,'Mean Temperature (F)',A1,99(" "))
 958  FORMAT(A1,'Vapor Pressure (mb)',A1,103(" "))
 959  FORMAT(A1,'Solar Radiation (langley/day)',A1,90(" "))
 960  FORMAT(A1,'Wind Speed (mi/day)',A1,100(" "))
 961  FORMAT(A1,'Monthly Total Rainfall (in)',A1,92(" ")) 
 962  FORMAT(3x,I4,9x,13F8.2,1(" "))
 963  FORMAT(A1,2x,'Mean',A1,8x,13F8.2,1(" "))
 968  FORMAT(A1,'Matrix of Weights for Non-Precip Parameters (1st column
     :) and Precip (2nd Column)',A1)
 970  FORMAT(A1,'Penman-Monteith Weather Parameters = (St',I2,')',
     :1x,A12,A1)
 971  FORMAT(A1,'Modified Hargreaves Weather Parameters = (St',I2,')',
     :1x,A12,A1)
 998  FORMAT (1x,A1,'Parameter',A1,7x,A1,'Jan',A1,3x,A1,'Feb',A1,3x,A1,
     :'Mar',A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1,'Jun',A1,3x,A1,
     :'Jul',A1,3x,A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1,'Oct',A1,3x,A1,
     :'Nov',A1,3x,A1,'Dec',A1,1x,A1,'Annual',A1)

 999  FORMAT(A121)

      RETURN
      END


