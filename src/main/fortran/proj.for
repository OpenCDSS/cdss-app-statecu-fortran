c proj
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

      SUBROUTINE PROJ

C***************************************************************************
C
C   Function        : proj.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This generates the *.iwr file which is a tabular CU
C                     summary (month and year) as calculated by Penman-
C                     Monteith and/or Blaney-Criddle ET estimation methods.
C                     It also generates the *.wsl when water supply is considered.
C   Calling program : statecu.f
C   Called programs : table.f 
C   Input arguments : none
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C
C   EW - THE FOLLOWING NOTE IS NO LONGER TRUE, SEE BELOW 2/2/99
C   Notes           : To minimize the use of higher dimensional arrays, the
C                     results needed to generate the tables were stored
C                     in temporary files as soon as they were computed by
C                     the estimation methods.  This subroutine reads these
C                     temporary files and rewrite them to the desired
C                     tabular output.
C
C
C   History         : (Date, Author, Description)
C
C    5/16/95  HBM   : Change units to acre-ft from 1000 acre-ft
C
C   12/14/95  HBM   : Modify summary table to include area and depletion in 
C                     unit depth (AF/acre)
C   02/02/99    EW     No longer writing to tmp files, storing results of
C                        analysis is random access arrays.  This was done to
C                        decrease execution time.
C   jhb 5/31/07 commented out code related to itime=2 (the *.tcu file)
C   jhb 07/08   changed extension from IWR to CIR - to prevent name conflict
C               with a statemod file that uses the same extension
C
C***************************************************************************

      INCLUDE 'gcommon.inc'

C-----Local variable declarations
c rb- delete fn_len since commoned
c     INTEGER IY, M, fn_len, IB, IDUM
      INTEGER IY, M, IB, IDUM, ID
      CHARACTER*200 ofile1 
      INTEGER IBODY1(DIM_NY,12),IRIGH1(DIM_NY),IDOWN1(12)
      REAL RIGH(DIM_NY)
      REAL DOWN(12),NDOWN1(12)
      REAL PTOT, BODY(DIM_NY,12)
      REAL BODY_1(DIM_NY,12)
      REAL RIGH_1(DIM_NY)
      REAL DOWN_1(12)
      REAL PTOT_1
      REAL AR_AVG, UDEPTH 


C-----Open Project Summary Output File for Writing
      IF(FLAG1 .EQ. 1) ID = 14
      IF(FLAG1 .EQ. 3) ID = 16
      IF(FLAG1 .EQ. 4) ID = 18
      IF(FLAG1 .EQ. 5) ID = 20
      ITIME=0

 5      IF(ITIME .EQ. 0) THEN
          ofile1 = dfile
c          ofile1(fn_len:fn_len+4) = '.iwr'
c          7/2008 change iwr to cir to prevent conflict with statemod file name...
          ofile1(fn_len:fn_len+4) = '.cir'
          OPEN (UNIT=10,FILE=ofile1,STATUS='UNKNOWN')
c jhb add header lines to IWR output file
          write(10,1108)vers, rdate
1108      FORMAT('StateCU Version ', f5.2,2x,a16)
          write(10,1109)dfile
1109      FORMAT('Scenario name: ', a200)
          write(10,1110)CURDATE(5:6),CURDATE(7:8),CURDATE(1:4),
     &                  CURTIME(1:2),CURTIME(3:4),CURTIME(5:6)
1110      FORMAT('Model execution time: ',A2,'-',A2,'-',A4,'  ',
     &           A2,':',A2,':',A2)
          write(10,'(A120)')TITLE(1)
          write(10,'(A120)')TITLE(2)
          write(10,'(A120)')TITLE(3)
          write(10,*)
        ELSEIF(ITIME .EQ. 1) THEN
          ofile1 = dfile
          ofile1(fn_len:fn_len+4) = '.wsl'
          OPEN (UNIT=10,FILE=ofile1,STATUS='UNKNOWN')
!        ELSE
!          ofile1 = dfile
!          ofile1(fn_len:fn_len+4) = '.tcu'
!          OPEN (UNIT=10,FILE=ofile1,STATUS='UNKNOWN')
        ENDIF

C---------------------------------------------------------------------------
C
C    Summary Table Per SubArea
C
C---------------------------------------------------------------------------


C-----Initialize Project Variables
       DO 12 IY = 1, NYRS
         DO 11 M = 1, 12
            IBODY1(IY,M) = 0
            BODY_1(IY,M) = 0.0
11       CONTINUE
12     CONTINUE 

         DO 2 IY =  1, NYRS
            IRIGH1(IY)=0
            RIGH_1(IY) = 0.0
 2       CONTINUE

         DO 3 M = 1, 12
           DOWN_1(M) = 0.0
 3       CONTINUE 
          

         PTOT_1 = 0.0
         IPTOT1 =0
         DO 500 IB = 1,NBASIN
            write(0,*) 'Processing IB=', IB, ' of ', NBASIN
            write(0,*) 'Calling TABLE'
            CALL TABLE(ID,IB,BODY,RIGH,DOWN,PTOT,ITIME)
            write(0,*) 'Back from TABLE'

C-----Update Project Total
            DO 531 IY = 1, NYRS
               IF(RIGH(IY) .GT. -998) THEN
                 RIGH_1(IY) = RIGH_1(IY) + RIGH(IY)
               ELSE
                 IRIGH1(IY)=1
               ENDIF
            DO 530 M = 1, 12
               IF(BODY(IY,M) .GT. -998) THEN
                 BODY_1(IY,M) = BODY_1(IY,M) + BODY(IY,M)
               ELSE
                 IBODY1(IY,M) = 1
               ENDIF
 530        CONTINUE
 531        CONTINUE

 500     CONTINUE


C-----Calculate project average annual area
      AR_AVG = 0.0
      DO 10 IY = 1, NYRS
        AR_AVG = AR_AVG + PJAREA(IY)
 10   Continue 
      AR_AVG = AR_AVG/NYRS

C---------------------------------------------------------------------------
C
C     Project Summary Table 
C
C---------------------------------------------------------------------------

         if(iclim .eq. 0) goto 82
         WRITE(10,901) QUOTE, QUOTE
         IF (INCH.EQ.1) THEN
           IF(ITIME .EQ. 0) THEN
             WRITE(10,905) QUOTE, QUOTE
           ELSEIF(ITIME .EQ. 1) THEN
             WRITE(10,911) QUOTE, QUOTE
           ELSE
             WRITE(10,913) QUOTE, QUOTE
           ENDIF
         ELSE
           IF(ITIME .EQ. 0) THEN
              WRITE(10,906) QUOTE, QUOTE
           ELSEIF(ITIME .EQ. 1) THEN
              WRITE(10,912) QUOTE, QUOTE
           ELSE
              WRITE(10,907) QUOTE, QUOTE
           ENDIF
         ENDIF
         IF(FLAG1 .EQ. 1) WRITE (10,908) QUOTE, QUOTE
         IF(FLAG1 .EQ. 3) WRITE (10,910) QUOTE, QUOTE
         IF(FLAG1 .EQ. 4) WRITE (10,909) QUOTE, QUOTE
         IF(FLAG1 .EQ. 5) WRITE (10,920) QUOTE, QUOTE
         WRITE(10,900) DLLINE
         WRITE(10,902) (QUOTE,IDUM=1,31)
         WRITE(10,900) SLLINE

         IPTOT1 = 0
         IDOWN1 = 0
         PTOT_1 = 0
         DO 80 IY = 1, NYRS
            write(0,*) 'Processing IY=', IY, ' of ', NYRS
            UDEPTH = 0.0
            IF(IRIGH1(IY) .EQ. 1) RIGH_1(IY) = -999
            IF(RIGH_1(IY) .GT. -998) THEN
              IF (PJAREA(IY).NE.0) THEN
                 UDEPTH = RIGH_1(IY)/PJAREA(IY)
              ELSE
                 UDEPTH = 0
              ENDIF
              PTOT_1=PTOT_1+RIGH_1(IY)
              IPTOT1=IPTOT1+1
            ELSE
              UDEPTH = -999
            ENDIF
            DO 75 M=1,12
              IF(IBODY1(IY,M) .EQ. 1) THEN
                 BODY_1(IY,M)=-999
              ELSE
                DOWN_1(M) = DOWN_1(M)+BODY_1(IY,M)
                IDOWN1(M)=IDOWN1(M)+1
              ENDIF
 75         CONTINUE
            IDUM=INT(PJAREA(IY))
            WRITE(10,903) NYR1+IY-1,IDUM,(BODY_1(IY,M), M=1,12),
     :         RIGH_1(IY),UDEPTH
 80      CONTINUE

         UDEPTH = 0.0
         IF (AR_AVG.NE.0) UDEPTH = PTOT_1/IPTOT1/AR_AVG
         WRITE(10,900) SLLINE
         IF(IPTOT1 .EQ. 0) THEN
            TTOT1=-999
            UDEPTH=-999
         ELSE
            TTOT1=PTOT_1/IPTOT1
         ENDIF
         DO 81 M=1,12
            IF(IDOWN1(M) .EQ. 0) THEN
               NDOWN1(M)=-999
            ELSE
               NDOWN1(M)=DOWN_1(M)/IDOWN1(M)
            ENDIF
 81      CONTINUE
         IDUM=INT(AR_AVG)      
         WRITE(10,904) QUOTE,QUOTE, IDUM,(NDOWN1(M),M=1,12),
     :         TTOT1, UDEPTH

 82     CLOSE(10)
 
      IF(ISUPLY .GT. 0 .AND. ITIME .EQ. 0) THEN
         ITIME=1
         GOTO 5
!      ELSEIF(ISUPLY .GT. 0 .AND. ITIME .EQ. 1) THEN
!         ITIME=2
!         GOTO 5
      ENDIF

      write(0,*) 'At end of PROJ'


 900  FORMAT(A120)
 901  FORMAT(A1,'Project Total'A1,105(" "))
 902  FORMAT(A1,'Year',A1,2x,A1,'Area',A1,1x,A1,'Jan',A1,2x,A1,'Feb',A1,
     :2x,A1,'Mar',A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1,'Jun',A1,3x,A1,
     :'Jul',A1,3x,A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1,'Oct',A1,2x,A1,'Nov',
     :A1,1x,A1,'Dec',A1,2x,A1,'Annual',A1,1x,A1,'AF/ac')
c rb  change fx format to fx.0
 903  FORMAT(1x,I4,2x,I7,F6.0,2F7.0,7F8.0,F7.0,F6.0,F10.0,F7.2)
 904  FORMAT(A1,'Mean',A1,1x,I7,F6.0,2F7.0,7F8.0,F7.0,F6.0,F10.0,
     :F7.2/120(" ")/120(" "))
 905  FORMAT(35x,A1,'CROP IRRIGATION WATER REQUIREMENT (inches)',A1,
     :41(" "))
 906  FORMAT(35x,A1,1x,'CROP IRRIGATION WATER REQUIREMENT (acre-ft)',
     :A1,39(" "))
 907  FORMAT(35x,A1,1x,'TOTAL CROP CU FROM PPT AND IRRIGATION SUPPLY ',
     :'(acre-ft)',A1,28(" "))
 908  FORMAT(35x,A1,5x,'Based on Blaney-Criddle Methodology',A1,
     :43(" "))
 909  FORMAT(41x,A1,'Modified Hargreaves',A1,58(" "))
 910  FORMAT(41x,A1,'    Penman-Monteith',A1,58(" "))
 920  FORMAT(41x,A1,'ASCEPenman-Monteith',A1,58(" "))
 911  FORMAT(35x,A1,' SUPPLY-LIMITED CONSUMPTIVE USE (inches)',A1,
     :43(" "))
 912  FORMAT(37x,A1,' SUPPLY-LIMITED CONSUMPTIVE USE (acre-ft)',A1,
     :40(" "))
 913  FORMAT(35x,A1,1x,'TOTAL CROP CU FROM PPT AND IRRIGATION SUPPLY ',
     :'(inches)',A1,27(" "))


      RETURN
      
      END

