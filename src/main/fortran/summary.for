c summary
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

               SUBROUTINE SUMMARY

C***************************************************************************
C
C   Function        : summary.f
C   Author          : HB Manguerra
C   Date            : May 1995
C   Purpose         : This summarizes input information and save them in
C                     *.sum output file.  Depending on a user-specified 
c                     flag, the summary can either be basic or detailed.
C   Calling program : statecu.f
C   Called programs :
C   Input arguments : none
C   Output arguments: none
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
      CHARACTER*8 OPTSTR
      CHARACTER*200 ofile1
      CHARACTER*130 TITL1, TITL2, TITL3

      INTEGER I,J,K,Y,IDUM
      REAL SUM
      REAL SUM_C(DIM_NC)
      INTEGER EDAT12(DIM_NC), FDAT12(DIM_NC)
      INTEGER EDAT34(DIM_NC), FDAT34(DIM_NC)
      REAL ST1(DIM_NC), ST2(DIM_NC)
      REAL SMAD(DIM_NC), SRZ(DIM_NC), SAWC(DIM_NC), SAPD(DIM_NC)
      INTEGER NUMC(DIM_NC),TCOUNT(12),RCOUNT(12),TTOTAL
      INTEGER KK, JDAT12, JDAT34, NN
      INTEGER EP1, EP2, FP1, FP2, EH1, EH2, FH1, FH2 
      INTEGER JULIAN,RYCOUNT(DIM_NY)
      CHARACTER*10 AS(10)
      INTEGER NU_K
      INTEGER idum1,idum2
C  EW 5/31/2019  
      REAL TWWS(DIM_NA),TWRS(DIM_NA)
      REAL SUMTT(12),SUMRR(12)
      INTEGER IERR
c rb- add dimension
      integer tyr
      character*12 tid
      real ttdat(12)
      

      CHARACTER*143  TITL7
      INTEGER IB,NBAS,IY,IM,IS
      REAL SSUMT,SSUMR,SSUM,SSUMDL,SSTEMP
      REAL SUMT(12),SUMR(12),SUMDL(12),SSUMRT(12)
      REAL TMP_T(DIM_NY,12),TMP_R(DIM_NY,12)
Cjhb0000011111111112222222222333333333344444444445555555555666666666677777
Cjhb5678901234567890123456789012345678901234567890123456789012345678901234
      PARAMETER (TITL1=' Year      Crop                      Soil Type  
     :       Area  Planting  Harvest  Earliest    Latest   Root    MAD  
     :  AWC   Applic')
      PARAMETER (TITL2='                         
     :                                Moisture   Moisture  Depth        
     :        Depth')         
      PARAMETER (TITL3='                                                
     :      (acre)                      (F)        (F)     (ft)    (%)  
     :(ft/ft)  (in)')


      TITL7=' Subarea\Station   St1  St2  St3  St4  St5  St6  St7  St8  
     :St9 St10 St11 St12 St13 St14 St15 St16 St17 St18 ST19 St20    '
c rb- do not need index
      ofile1 = dfile
      ofile1(fn_len:fn_len+4) = '.sum'

      OPEN(UNIT=8,FILE=ofile1)

c jhb add header lines to SUM output file
      write(8,1108)vers, rdate
1108  FORMAT('StateCU Version ', f5.2,2x,a16)
      write(8,1109)dfile
1109  FORMAT('Scenario name: ', a200)
      write(8,1110)CURDATE(5:6),CURDATE(7:8),CURDATE(1:4),
     &             CURTIME(1:2),CURTIME(3:4),CURTIME(5:6)
1110  FORMAT('Model execution time: ',A2,'-',A2,'-',A4,'  ',
     &       A2,':',A2,':',A2)
      write(8,*)

C-----Initialize
      DO 5 I = 1, DIM_NC
         SUM_C(I) = 0.0
5     CONTINUE

C-----Write Title strings
      WRITE(8,899) QUOTE,vers,QUOTE
      WRITE(8,900) (QUOTE,TITLE(I), QUOTE, I=1,3)
      WRITE(8,898)

C-----Write begin and end years
      WRITE(8,901) QUOTE,QUOTE,NYR1,QUOTE,QUOTE,NYR2

C-----Write program main options
      IF (FLAG1.EQ.1) THEN
      OPTSTR = 'Enabled '
      WRITE(8,902) QUOTE,OPTSTR,QUOTE
      ELSE
      OPTSTR = 'Disabled'
      ENDIF

      IF (FLAG1.GE.3) THEN
         OPTSTR = 'Enabled '
         WRITE(8,903) QUOTE,OPTSTR,QUOTE
         WRITE(8,898)
      ELSE
         OPTSTR = 'Disabled'
      ENDIF

      WRITE(8,898)      

C-----Write option for monthly effective rainfall
      IF (FLAG1.EQ.1) THEN 
         IF (RN_XCO.EQ.1) THEN
           WRITE(8,905) QUOTE,QUOTE
         ELSEIF (RN_XCO.EQ.2) THEN
           WRITE(8,906) QUOTE,QUOTE
         ELSE
           WRITE(8,907) QUOTE,QUOTE
         ENDIF
         WRITE(8,898)
      ENDIF


C-----Write option for daily effective rainfall
      IF (FLAG1.GE.3) THEN
         IF (RN_XCO.eq.1) THEN
            WRITE(8,908) QUOTE,QUOTE,QUOTE,QUOTE
         ELSEIF (RN_XCO.EQ.2) THEN
            WRITE(8,909) QUOTE,QUOTE,QUOTE,cn(1),QUOTE
         ELSEIF (RN_XCO.EQ.3) THEN
            WRITE(8,910) QUOTE,QUOTE,QUOTE,cn(1),QUOTE,QUOTE,
     :           cn(2),QUOTE,QUOTE,cn(3),QUOTE
         ELSE
            WRITE(8,911) QUOTE,QUOTE
         ENDIF
         WRITE(8,898)
      ENDIF


C-----Write water supply availability
      IF (ISUPLY.GE.1) THEN
         WRITE(8,912) QUOTE,QUOTE
      ELSE
         WRITE(8,913) QUOTE,QUOTE
      ENDIF

      WRITE(8,898)

C-----Write total project area by crop
c      fact=1.0
      DO 50 J = 1, NYRS
         SUM = 0.0
         DO 60 K = 1, DIM_NC
            IF (C_AREA(J,K).GT.0) THEN
               SUM = SUM + C_AREA(J,K)
               SUM_C(K) = SUM_C(K) + C_AREA(J,K)
            ENDIF
 60      CONTINUE
         if(iclim .eq. 0) then
           do k=1,dim_nc
             SUM_C(K)=SUM_C(K)/SUM
           enddo
         endif

         if(iclim .eq. 0) then
             write(sum_y(j),'(f10.2)') 1.00
         else
             write(sum_y(j),'(f10.0)') sum
         endif
 50   CONTINUE


C-----Renumber indeces
      NU_K = 0
      DO 151 K = 1,DIM_NC
         IF (SUM_C(K).GT.0) THEN
            NU_K = NU_K +1
            NU_NME(NU_K) = CPNAME(K) (1:10)
            DO 152 J=1,NYRS
              if(iclim .eq. 0) then
                 write(nu_sum(j,nu_k),"(f10.2)") c_area(j,k)/SUM
              else
                 write(nu_sum(j,nu_k),"(f10.0)") c_area(j,k)
              endif
 152        Continue
         ENDIF
 151  CONTINUE
      NU_DIM = NU_K
      NU_NME(NU_DIM+1) = '     Total'

      if(iclim .eq. 0) then
         WRITE(8,924) QUOTE,QUOTE
      else
         WRITE(8,921) QUOTE,QUOTE
      endif
      DO K=1,10
        AS(K) = '          '
      ENDDO
      IF((NU_DIM+1) .LE. 10) THEN
        IDIM1=10-(NU_DIM+1)
        WRITE(8,922) QUOTE,QUOTE,(NU_NME(K), K=1,NU_DIM+1),
     :   (AS(K),K=1,IDIM1)
        DO 500 J=1,NYRS
           WRITE(8,923) NYR1+J-1,(NU_SUM(J,K), K=1,NU_DIM),
     :      SUM_Y(J),(AS(K),K=1,IDIM1)
500     CONTINUE

      ELSEIF((NU_DIM+1) .LE. 20) THEN
        IDIM2=20-(NU_DIM+1)
        WRITE(8,922) QUOTE,QUOTE,(NU_NME(K), K=1,10)
        WRITE(8,922) QUOTE,QUOTE,(NU_NME(K), K=11,NU_DIM+1),
     :   (AS(K),K=1,IDIM2)
        DO 501 J=1,NYRS
           WRITE(8,923) NYR1+J-1,(NU_SUM(J,K), K=1,10)
           WRITE(8,923) NYR1+J-1,(NU_SUM(J,K), K=11,NU_DIM),
     :      SUM_Y(J),(AS(K),K=1,IDIM2)
501     CONTINUE
       ELSEIF((NU_DIM+1) .LE. 30) THEN
        IDIM3=30-(NU_DIM+1)
        WRITE(8,922) QUOTE,QUOTE,(NU_NME(K), K=1,10)
        WRITE(8,922) QUOTE,QUOTE,(NU_NME(K), K=11,20)
        WRITE(8,922) QUOTE,QUOTE,(NU_NME(K), K=21,NU_DIM+1),
     :   (AS(K),K=1,IDIM3)
        DO 503 J=1,NYRS
           WRITE(8,923) NYR1+J-1,(NU_SUM(J,K), K=1,10)
           WRITE(8,923) NYR1+J-1,(NU_SUM(J,K), K=11,20)
           WRITE(8,923) NYR1+J-1,(NU_SUM(J,K), K=21,NU_DIM),
     :      SUM_Y(J),(AS(K),K=1,IDIM3)
503     CONTINUE
      ELSEIF((NU_DIM+1) .LE. 40) THEN
        IDIM4=40-(NU_DIM+1)
        WRITE(8,922) QUOTE,QUOTE,(NU_NME(K), K=1,10)
        WRITE(8,922) QUOTE,QUOTE,(NU_NME(K), K=11,20)
        WRITE(8,922) QUOTE,QUOTE,(NU_NME(K), K=21,30)
        WRITE(8,922) QUOTE,QUOTE,(NU_NME(K), K=31,NU_DIM+1),
     :   (AS(K),K=1,IDIM4)
        DO 504 J=1,NYRS
           WRITE(8,923) NYR1+J-1,(NU_SUM(J,K), K=1,10)
           WRITE(8,923) NYR1+J-1,(NU_SUM(J,K), K=11,20)
           WRITE(8,923) NYR1+J-1,(NU_SUM(J,K), K=21,30)
           WRITE(8,923) NYR1+J-1,(NU_SUM(J,K), K=31,NU_DIM),
     :      SUM_Y(J),(AS(K),K=1,IDIM4)
504     CONTINUE
      ENDIF



      WRITE(8,898)

      IF (SOUT.EQ.0) THEN
C-----Write basic crop information
      DO 52 K = 1,DIM_NC
      EDAT12(K) = 365
      FDAT12(K) = 0
      EDAT34(K) = 365
      FDAT34(K) = 0
      ST1(K) = 0
      ST2(K) = 0
      SMAD(K) = 0
      SRZ(K) = 0
      SAWC(K) = 0
      SAPD(K) = 0
      NUMC(K) = 0
 52   CONTINUE

      DO 53 K = 1,N_CRPS
      KK = N_CRP(K)
      JDAT12 = JULIAN(GDATE1(K),GDATE2(K))
      JDAT34 = JULIAN(GDATE3(K),GDATE4(K))
      IF(JDAT12.LT.EDAT12(KK)) EDAT12(KK) = JDAT12
      IF(JDAT12.GT.FDAT12(KK)) FDAT12(KK) = JDAT12
      IF(JDAT34.LT.EDAT34(KK)) EDAT34(KK) = JDAT34
      IF(JDAT34.GT.FDAT34(KK)) FDAT34(KK) = JDAT34
      ST1(KK) = ST1(KK) + TMOIS1(K)
      ST2(KK) = ST2(KK) + TMOIS2(K)
      SMAD(KK) = SMAD(KK) + MAD(K)
      SRZ(KK) = SRZ(KK) + FRZ(K)
      SAWC(KK) = SAWC(KK) + AWC(K)
      SAPD(KK) = SAPD(KK) + APD(K)
      NUMC(KK) = NUMC(KK) + 1
 53   CONTINUE      
     
      WRITE(8,999) DLLINE
      WRITE(8,926) (QUOTE, IDUM=1,48)
      WRITE(8,999) SLLINE
c      DO 54 K = 1,DIM_NC
      DO 54 K = 1,N_CRPS
         NN = NUMC(K)
         CALL CLNDR(EDAT12(K),EP1,EP2)
         CALL CLNDR(FDAT12(K),FP1,FP2)
         CALL CLNDR(EDAT34(K),EH1,EH2)
         CALL CLNDR(FDAT34(K),FH1,FH2) 
         IF (SUM_C(K).GT.0) THEN
            IF (CRPTYP(K).EQ.1) THEN   !perrenial
               WRITE(8,927) QUOTE,CPNAME(K),QUOTE,QUOTE,QUOTE,QUOTE,
     :            QUOTE,QUOTE,QUOTE,QUOTE,QUOTE,ST1(K)/NN,ST2(K)/NN,
     :            SRZ(K)/NN,SMAD(K)/NN,SAWC(K)/NN,SAPD(K)/NN
         ELSE
               WRITE(8,925) QUOTE,CPNAME(K),QUOTE,QUOTE,EP1,EP2,QUOTE,
     :            QUOTE,FP1,FP2,QUOTE,QUOTE,EH1,EH2,QUOTE,QUOTE,FH1,
     :            FH2,QUOTE,ST1(K)/NN,ST2(K)/NN,SRZ(K)/NN,SMAD(K)/NN,
     :            SAWC(K)/NN,SAPD(K)/NN
         ENDIF
         ENDIF
 54   CONTINUE
      WRITE(8,999) SLLINE
      WRITE(8,898)

      ELSE
C-----Write crop information if output <> 0
      WRITE(8,916) QUOTE,QUOTE
      DO 20 I = 1, NBASIN
         WRITE(8,917) QUOTE,BAS_ID(I),QUOTE
         WRITE(8,999) DLLINE
         WRITE(8,999) TITL1
         WRITE(8,999) TITL2
         WRITE(8,999) TITL3
         WRITE(8,999) SLLINE
         DO 40 Y = 1,NYRS 
         DO 30 J = 1, NPARCE(I,y)
            K = BKEY(I,J,y)
            IF (CRPTYP(K).EQ.1) THEN  ! perennial
            IF (J.EQ.1) THEN
               WRITE(8,935) NYR1+Y-1,QUOTE,CPNAME(K),SOIL(K),QUOTE,
     :               AREA(I,J,Y),QUOTE,QUOTE,QUOTE,QUOTE,
     :               TMOIS1(K),TMOIS2(K),IRZ(K),MAD(K),AWC(K),APD(K)
            ELSE
            WRITE(8,936) QUOTE,QUOTE,QUOTE,CPNAME(K),SOIL(K),QUOTE,
     :               AREA(I,J,Y),QUOTE,QUOTE,QUOTE,QUOTE,
     :               TMOIS1(K),TMOIS2(K),IRZ(K),MAD(K),AWC(K),APD(K)
            ENDIF

            ELSE
            IF (J.EQ.1) THEN
               WRITE(8,918) NYR1+Y-1,QUOTE,CPNAME(K),SOIL(K),QUOTE,
     :               AREA(I,J,Y),QUOTE,GDATE1(K),GDATE2(K),QUOTE,
     :               QUOTE,GDATE3(K),GDATE4(K),QUOTE,
     :               TMOIS1(K),TMOIS2(K),IRZ(K),MAD(K),AWC(K),APD(K)
            ELSE
            WRITE(8,919) QUOTE,QUOTE,QUOTE,CPNAME(K),SOIL(K),QUOTE,
     :               AREA(I,J,Y),QUOTE,GDATE1(K),GDATE2(K),QUOTE,QUOTE,
     :               GDATE3(K),GDATE4(K),QUOTE,
     :               TMOIS1(K),TMOIS2(K),IRZ(K),MAD(K),AWC(K),APD(K)
            ENDIF
            ENDIF
  30        CONTINUE
         WRITE(8,898)
         WRITE(8,920) QUOTE,QUOTE,QUOTE,QUOTE,T_AREA(I,Y)
         WRITE(8,898)
  40        CONTINUE
      WRITE(8,999) SLLINE
      WRITE(8,898)
      WRITE(8,898)
  20     CONTINUE
      ENDIF
C-----------------------------------------------------------------------------
C     Summary of Weather Parameters
C-----------------------------------------------------------------------------

      IF (FLAG1.EQ.1 .OR. FLAG1 .EQ. 2) THEN
C-----open and read input file
       open(unit=26,file=tmpfile,status='OLD',iostat=ierr)

       call skipn(26)
       read(26,940) idum1,idum2
940    format(6x,i4,11x,i4)
945    read(26,941,end=942) tyr,tid,(ttdat(jj),jj=1,12)
       if(tyr.lt.nyr1.or.tyr.gt.nyr2) goto 945         
941    format(i4,1x,a12,12(f8.0))
         do  943 i=1,n_sta
         if(tid.eq.wsid(i)) then
           do 944 j=1,12
             tmpt(i,tyr-nyr1+1,j)=ttdat(j)
944        continue
         endif
943    continue
       goto 945
942    close(26)

       if(climonfly.gt.0)then
         call flyfillt
       endif
       
       if(rn_xco.gt.0)then
       open(unit=27,file=pptfile,status='OLD',iostat=ierr)

       call skipn(27)
       read(27,940) idum1,idum2
745    read(27,941,end=742) tyr,tid,(ttdat(jj),jj=1,12)
       if(tyr.lt.nyr1.or.tyr.gt.nyr2) goto 745         
       do  743 i=1,n_sta
         if(tid.eq.wsid(i)) then
            do 744 j=1,12
              tmpr(i,tyr-nyr1+1,j)=ttdat(j)
744         continue               
         endif 
743    continue   
       goto 745
742    close(27)
       if(climonfly.gt.0)then
           call flyfillp
         endif
       endif
       nbas=nbasin

C-----Sum Weights
         DO 221 IB = 1, NBAS
            TWWS(IB) = 0.0
            TWRS(IB) = 0.0
 221     CONTINUE

         DO 211 IB = 1, NBAS
           DO 21 I = 1, N_STA
            TWWS(IB) = TWWS(IB) + WWS(IB,I)
            TWRS(IB) = TWRS(IB) + WRS(IB,I)
 21        CONTINUE
 211     CONTINUE
 

         TTOTAL = 0
         DO 205 I = 1,N_STA
            stname(i)=wsid(i)
            do 776 iy=1,nyrs
               RYCOUNT(IY)=0
              do 77 im=1,12
                 tmp_t(iy,im)=tmpt(i,iy,im) 
                 tmp_r(iy,im)=tmpr(i,iy,im)
 77           continue
 776        continue

C-----Calculate Daylight from Latitude and Tabular values
           CALL DAYHRS(wlat(I),SUMDL)
           SSUMDL = 0.0
           DO 213 IM = 1,12
             SSUMDL = SSUMDL + SUMDL(IM)
213        continue
C-----Calculate average values for the duration

           DO 209 IM = 1,12
              SUMT(IM) = 0.0
              SUMR(IM) = 0.0
              TCOUNT(IM) = 0.0
              RCOUNT(IM) = 0.0
 209       CONTINUE

           DO 212  IY = 1,NYRS
              DO 208 IM = 1,12
                 IF(TMP_T(IY,IM) .GT. -998) THEN
                     SUMT(IM) = SUMT(IM) + TMP_T(IY,IM)
                     TCOUNT(IM)=TCOUNT(IM)+1
                 ENDIF
                 IF(TMP_R(IY,IM) .GT. -998) THEN
                   SUMR(IM) = SUMR(IM) + TMP_R(IY,IM)
                   RCOUNT(IM)=RCOUNT(IM)+1
                 ENDIF
 208          CONTINUE



              SSUMT = 0.0
              SSUMR = 0.0
              TTOTAL = 0.0
              DO 210 IM = 1,12
C  ew 5/31/2019
                
                IF(TCOUNT(IM) .GT. 0) THEN
                    SSUMT = SSUMT + SUMT(IM)
                    TTOTAL = TTOTAL + TCOUNT(IM)
                ENDIF    
                IF(RCOUNT(IM) .GT. 0) THEN
                  SSUMR = SSUMR + SUMR(IM)/RCOUNT(IM)
                ELSE
                  SSUMR=-999
                ENDIF
 210          CONTINUE
              
 212       CONTINUE

C-----Write

           WRITE(8,898)

C-----Write Weather Summary Tables
           IF (I.LT.10) THEN
             WRITE(8,964) QUOTE,I,STNAME(I),QUOTE
           ELSE
             WRITE(8,971) QUOTE,I,STNAME(I),QUOTE
           ENDIF

           WRITE(8,999) DLLINE
           WRITE(8,998) (QUOTE, IDUM = 1,28)
           WRITE(8,999) SLLINE

           IF (SOUT.EQ.0) THEN
C  ew 5/31/2019
           DO 413 IM = 1,12
              IF(TCOUNT(IM).EQ.0) THEN 
                 SUMTT(IM)  = -999
              ELSE
                 SUMTT(IM) = SUMT(IM)/TCOUNT(IM)
              ENDIF   
              IF(RCOUNT(IM).EQ.0) THEN 
                 SUMRR(IM) = -999
              ELSE   
                 SUMRR(IM) = SUMR(IM)/RCOUNT(IM) 
              ENDIF
 413       CONTINUE
           IF(TTOTAL .EQ. 0) THEN 
              SSUMT = -999
           ELSE   
              SSUMT = SSUMT/TTOTAL
           ENDIF   
              WRITE(8,952) QUOTE,QUOTE,(SUMTT(IM),
     :            IM=1,12),SSUMT
              WRITE(8,956) QUOTE,QUOTE,(SUMRR(IM),
     :            IM=1,12),SSUMR
              WRITE(8,973) QUOTE,QUOTE,(SUMDL(IM),IM=1,12),SSUMDL/12 
              WRITE(8,999) SLLINE
              WRITE(8,898)

           ELSE

C-----Mean Temperature
              WRITE(8,957) QUOTE,QUOTE
              SSUMT = 0.0
              TOTALT = 0
              DO 214  IY = 1,NYRS
                 SSUM = 0.0
                 TTOTAL = 0.0
                 DO 215 IM=1,12
                    IF(TMP_T(IY,IM) .GE. -998) THEN
                        SSUM = SSUM + TMP_T(IY,IM)
                        TTOTAL=TTOTAL+1
                    ENDIF
 215             CONTINUE
C     ew 5/31/2019
                 IF(TTOTAL .EQ. 12) THEN
                     SSTEMP = SSUM/TTOTAL
                 ELSE
                     SSTEMP = -999
                 ENDIF    
                 WRITE(8,962) NYR1+IY-1,(TMP_T(IY,IM),IM=1,12),
     :                       SSTEMP
 214          CONTINUE
C     ew 5/31/2019
              DO 632 IM=1,12
                    IF(TCOUNT(IM) .EQ. 0) THEN
                        SUMTT(IM) = -999 
                    ELSE
                        SUMTT(IM) = SUMT(IM)/TCOUNT(IM)
                        SSUMT = SSUMT + SUMTT(IM)
                        TOTALT = TOTALT+1
                    ENDIF
632           CONTINUE
              IF(SSUMT .EQ. 0) THEN 
                  SSUMT = -999
              ELSE
                  SSUMT=SSUMT/TOTALT
              ENDIF    
              WRITE(8,963) QUOTE,QUOTE,(SUMTT(IM),IM=1,12),
     :                     SSUMT
              WRITE(8,999) SLLINE

C------Rainfall
              WRITE(8,961) QUOTE,QUOTE
              DO 222 IY = 1,NYRS
                 SSUM = 0.0
                 IRYEAR = 0
                 DO 223 IM=1,12
                    IF(TMP_R(IY,IM) .GE. -998) THEN
                      SSUM = SSUM + TMP_R(IY,IM)
                    ELSE
                      IRYEAR = 1
                    ENDIF
 223             CONTINUE
                 IF(IRYEAR .EQ. 1)SSUM = -999 
                 WRITE(8,962) NYR1+IY-1,(TMP_R(IY,IM),IM=1,12),SSUM
 222          CONTINUE
              DO 633 IM=1,12
                IF(RCOUNT(IM).EQ. 0) THEN
                  SSUMRT(IM) = -999
                ELSE
                  SSUMRT(IM) = SUMR(IM)/RCOUNT(IM)
                ENDIF  
633           CONTINUE
              WRITE(8,963) QUOTE,QUOTE,(SSUMRT(IM),
     :            IM=1,12),SSUMR
              WRITE(8,999) SLLINE

C------Daylight
              WRITE(8,972) QUOTE,QUOTE              
              WRITE(8,963) QUOTE,QUOTE,(SUMDL(IM),IM=1,12),SSUMDL/12 
              WRITE(8,999) SLLINE
              WRITE(8,898)
           ENDIF
           
 205     CONTINUE


C-----Write Weight Matrix for Blaney-Criddle
          WRITE(8,965) QUOTE,QUOTE
          do 300 ib=1, nbas
            write(8,897) bas_id(ib)
            do 301 is=1, n_sta
              if(wrs(ib,is).gt.0.or.wws(ib,is).gt.0) then
                write(8,980) stname(is),wws(ib,is),wrs(ib,is)
 980            format(4x,a8,2f6.2,97(" "))
                goto 301
              endif
 301        continue         
 300      continue
          WRITE(8,898)
         
      ENDIF

 897  FORMAT(A40,81(" "))
 898  FORMAT(121(" "))
 899  FORMAT(A1,'# output file generated by StateCU version ',F3.1,A1,
     :  73(" ")/121(" "))
 900  FORMAT(A1,A119,A1)
 901  FORMAT(A1,'Begin Year             :    ',A1,I4,87(" ")/
     :A1,'End Year               :    ',A1,I4,87(" ")/121(" "))
 902  FORMAT(A1,'Blaney-Criddle Method  :    ',A8,A1,83(" "))
 903  FORMAT(A1,'Penman-Monteith Method :    ',A8,A1,83(" "))
 905  FORMAT(A1,'Monthly Precip Method  :    SCS',A1,88(" "))
 906  FORMAT(A1,'Monthly Precip Method  :    USBR',A1,87(" "))
 907  FORMAT(A1,'Monthly Precip Method  :    None',A1,87(" "))
 908  FORMAT(A1,'Daily Precip Method    :    All daily total rainfall be
     :low 1.0 inch are effective.',A1,37(" ")/
     :A1,'                       :    Maximum effective rainfall = 1.0 i
     :nch',A1,54(" "))
 909  FORMAT(A1,'Daily Precip Method    :    Effective rainfall = factor 
     : x total rainfall',A1,47(" ")/
     :A1,'                       :    Factor = ',F4.2,A1,78(" "))              
 910  FORMAT(A1,'Daily Precip Method    :    Curve Number method from NE
     :H SEC 4 METHOD',A1,50(" ")/
     :A1,'                       :    Curve number 1 =',F3.1,A1,73(" ")/
     :A1,'                       :    Curve number 2 =',F3.1,A1,73(" ")/
     :A1,'                       :    Curve number 2 =',F3.1,A1,73(" "))
 911  FORMAT(A1,'Daily Precip Method    :    None',A1,87(" "))
 912  FORMAT(A1,'Water Supply Data      :    Available',A1,82(" "))
 913  FORMAT(A1,'Water Supply Data      :    None',A1,87(" "))
 916  FORMAT(A1,'Crop Information',A1,103(" ")/'----------------',
     :    105(" ")/121(" "))
 917  FORMAT(A1,A40,A1,79(" "))
c rb- change Fx formats to Fx.0
 918  FORMAT(1x,I4,1x,A1,A30,A15,A1,F7.0,1x,A1,I2,'/',I2,A1,3x,A1,I2,
     :'/',I2,A1,3x,F4.1,7x,F4.1,1x,2F8.1,2F7.1,4(" "))
 919  FORMAT(3x,2A1,1x,A1,A30,A15,A1,F7.0,1x,A1,I2,'/',I2,A1,3x,A1,
     :I2,'/',I2,A1,3x,F4.1,7x,F4.1,1x,2F8.1,2F7.1,4(" "))
 920  FORMAT(A1,A1,4x,A1,'TOTAL',A1,40x,F7.0,71(" "))
 921  FORMAT(A1,'Total Project Area by Crop (acres)',A1,85(" ")
     :/'-----------------------------------',86(" "))
 922  FORMAT(A1,'Year',A1,10(A10,1X),5(" "))
 923  FORMAT(1x,I4,1x,10(1x,A10),5(" "))
 924  FORMAT(A1,'Total Project Crop Distribution',A1,81(" ")
     :/'-----------------------------------',86(" "))
 925  FORMAT(A1,A30,A1,A1,I2,'/',I2,A1,4x,A1,I2,'/',I2,A1,4x,A1,
     :I2,'/',I2,A1,4x,A1,I2,'/',I2,A1,F9.1,3F10.1,F8.2,F9.1,3(" "))
 926  FORMAT(A1,'Crop',A1,26x,A1,'Earliest',A1,1x,A1,'Latest',A1,2x,
     :A1,'Earliest',A1,1x,A1,'Latest',A1,2x,A1,'Earliest',A1,1x,A1,
     :'Latest',A1,3x,A1,'Root',A1,3x,A1,'MAD',A1,3x,A1,'AWC',A1,
     :4x,A1,'Applic',A1,1(" ")/
     :32x,A1,'Planting',A1,A1,'Planting',A1,1x,A1,'Harvest',A1,2x,
     :A1,'Harvest',A1,1x,A1,'Moisture',A1,1x,A1,'Moisture',A1,27x,A1,
     :'Depth',A1,2(" ")/
     :76x,A1,'(F)',A1,6x,A1,'(F)',A1,4x,A1,'(ft)',A1,3x,A1,'(%)',A1,
     :2x,A1,'(ft/ft)',A1,2x,A1,'(in)',A1,2(" "))
 927  FORMAT(A1,A30,A1,A1,'--','/','--',A1,4x,A1,'--','/','--',A1,4x,A1,
     :'--','/','--',A1,4x,A1,'--','/','--',A1,F9.1,3F10.1,F8.2,F9.1,
     :  3(" "))
 935  FORMAT(1x,I4,1x,A1,A30,A15,A1,F7.0,1x,A1,'--/--',A1,3x,A1,
     :'--/--',A1,3x,F4.1,7x,F4.1,1x,2F8.1,2F7.1,4(" "))
 936  FORMAT(3x,2A1,1x,A1,A30,A15,A1,F7.0,1x,A1,'--/--',A1,3x,A1,
     :'--/--',A1,3x,F4.1,7x,F4.1,1x,2F8.1,2F7.1,4(" "))
 952  FORMAT(A1,'Mean Temp (F) ',A1,13F8.2,1(" "))
 956  FORMAT(A1,'Rainfall (in) ',A1,13F8.2,1(" "))
 957  FORMAT(A1,'Mean Temperature (F)',A1,99(" "))
 961  FORMAT(A1,'Monthly Total Rainfall (in)',A1,92(" ")) 
 962  FORMAT(3x,I4,9x,13F8.2,1(" "))
 963  FORMAT(A1,2x,'Mean',A1,8x,13F8.2,1(" "))
 964  FORMAT(A1,'Blaney-Criddle Weather Parameters = (St',I1,')',A70,A1,
     :8(" "))
 965  FORMAT(A1,'Matrix of Weights for Non-Precip Parameters (1st column
     :) and precip (2nd column)',A1,33(" "))
 971  FORMAT(A1,'Blaney-Criddle Weather Parameters = (St',I2,')',A70,A1,
     :7(" "))
 972  FORMAT(A1,'Percent Daylight Hours',A1,97(" "))
 973  FORMAT(A1,'Daylight (%)  ',A1,13F8.2,1(" "))

 998  FORMAT (1x,A1,'Parameter',A1,7x,A1,'Jan',A1,3x,A1,'Feb',A1,3x,A1,
     :'Mar',A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1,'Jun',A1,3x,A1,
     :'Jul',A1,3x,A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1,'Oct',A1,3x,A1,
     :'Nov',A1,3x,A1,'Dec',A1,1x,A1,'Annual',A1)

 999  FORMAT(A131)

      RETURN
      END