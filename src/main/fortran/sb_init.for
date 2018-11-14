c sb_init
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

       SUBROUTINE SB_INIT

C***************************************************************************
C
C   Function        : sb_init.for 
C   Author          : Jim Brannon
C   Date            : February 2008
C   Purpose         : This routine initializes the sub basin array variables
C   Calling program : statecu.for
C   Called programs : 
C   Input arguments :
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           : 
C   History         :(Date, Author, Description)
C
C***************************************************************************

C-----Argument Variable Declaration
C-----Include Global Variables and Data Defaults
      INCLUDE 'gcommon.inc'
C-----Local Variable Declarations
      integer :: i, j, k
      sbcount=0
      do i=1,dim_na
        sbsb(i)=0
        do j=1,dim_ny
          do k=1,12
            m_area(i,j,k)=0.0
          enddo
        enddo
      enddo
      do i=0,dim_nsb
        sbid(i)=''
        sbname(i)=''
        sbstrcnt(i)=0
        do j=1,dim_ny
          do k=1,12
            sbtarea(i,j,k)=0.0
            sbmarea(i,j,k)=0.0
            sbceff(i,j,k)=0.0
            sbsfeff(i,j,k)=0.0
            sbeffcu(i,j,k)=0.0
            sbseffcu(i,j,k)=0.0
            sbcloss(i,j,k)=0.0
            sbfdiv(i,j,k)=0.0
            sbtail(i,j,k)=0.0
            sbarech(i,j,k)=0.0
            sbettot(i,j,k)=0.0
            sbeffppt(i,j,k)=0.0
            sbreqt(i,j,k)=0.0
            sbwbu(i,j,k)=0.0
            sbwbused(i,j,k)=0.0
            sbreqreq(i,j,k)=0.0
            sbseniorf(i,j,k)=0.0
            sbjuniorf(i,j,k)=0.0
            sbotherf(i,j,k)=0.0
            sbdivsup(i,j,k)=0.0
            sbcrop_cus(i,j,k)=0.0
            sbcrop_cuj(i,j,k)=0.0
            sbcrop_cuo(i,j,k)=0.0
            sbcrop_cut(i,j,k)=0.0
            sbsoil_cus(i,j,k)=0.0
            sbsoil_cuj(i,j,k)=0.0
            sbsoil_cuo(i,j,k)=0.0
            sbsoil_cu(i,j,k)=0.0
            sbsoiltotts(i,j,k)=0.0
            sbsoiltottj(i,j,k)=0.0
            sbsoiltotto(i,j,k)=0.0
            sbsoiltott(i,j,k)=0.0
            sbcropcusoil(i,j,k)=0.0
            sbestcrps(i,j,k)=0.0
            sbestcrpj(i,j,k)=0.0
            sbestcrpo(i,j,k)=0.0
            sbestcrpt(i,j,k)=0.0
            sbdivcus(i,j,k)=0.0
            sbdivcuj(i,j,k)=0.0
            sbdivcuo(i,j,k)=0.0
            sbdivcu(i,j,k)=0.0
            sbgdiv(i,j,k)=0.0
            sbgsdiv(i,j,k)=0.0
            sbgfdiv(i,j,k)=0.0
            sbgwcu(i,j,k)=0.0
            sbeffgw(i,j,k)=0.0
            sbgwro(i,j,k)=0.0
            sbcutot(i,j,k)=0.0
            sbcustot(i,j,k)=0.0
            sbtdp(i,j,k)=0.0
            sbulags(i,j,k)=0.0
            sbulagj(i,j,k)=0.0
            sbulago(i,j,k)=0.0
            sbulagt(i,j,k)=0.0
            sblagrets(i,j,k)=0.0
            sblagretj(i,j,k)=0.0
            sblagreto(i,j,k)=0.0
            sblagrett(i,j,k)=0.0
            sbtotret(i,j,k)=0.0
            sblaglates(i,j,k)=0.0
            sblaglatej(i,j,k)=0.0
            sblaglateo(i,j,k)=0.0
            sblaglatet(i,j,k)=0.0
            sbdeps(i,j,k)=0.0
            sbdepj(i,j,k)=0.0
            sbdepo(i,j,k)=0.0
            sbdept(i,j,k)=0.0
            sbshortage(i,j,k)=0.0
            do l=1,10
              sbgrass(i,j,k,l)=0.0
            enddo
          enddo
        enddo
      enddo

        bid=''
        bname=''
        do j=1,dim_ny
          do k=1,12
            btarea(j,k)=0.0
            bmarea(j,k)=0.0
            bcloss(j,k)=0.0
            bceff(j,k)=0.0
            bsfeff(j,k)=0.0
            beffcu(j,k)=0.0
            bseffcu(j,k)=0.0
            bfdiv(j,k)=0.0
            btail(j,k)=0.0
            barech(j,k)=0.0
            bettot(j,k)=0.0
            beffppt(j,k)=0.0
            breqt(j,k)=0.0
            bwbu(j,k)=0.0
            bwbused(j,k)=0.0
            breqreq(j,k)=0.0
            bseniorf(j,k)=0.0
            bjuniorf(j,k)=0.0
            bdivsup(j,k)=0.0
            botherf(j,k)=0.0
            bcrop_cus(j,k)=0.0
            bcrop_cuj(j,k)=0.0
            bcrop_cuo(j,k)=0.0
            bcrop_cut(j,k)=0.0
            bsoil_cus(j,k)=0.0
            bsoil_cuj(j,k)=0.0
            bsoil_cuo(j,k)=0.0
            bsoil_cu(j,k)=0.0
            bsoiltotts(j,k)=0.0
            bsoiltottj(j,k)=0.0
            bsoiltotto(j,k)=0.0
            bsoiltott(j,k)=0.0
            bcropcusoil(j,k)=0.0
            bestcrps(j,k)=0.0
            bestcrpj(j,k)=0.0
            bestcrpo(j,k)=0.0
            bestcrpt(j,k)=0.0
            bdivcus(j,k)=0.0
            bdivcuj(j,k)=0.0
            bdivcuo(j,k)=0.0
            bdivcu(j,k)=0.0
            bgdiv(j,k)=0.0
            bgsdiv(j,k)=0.0
            bgfdiv(j,k)=0.0
            bgwcu(j,k)=0.0
            beffgw(j,k)=0.0
            bgwro(j,k)=0.0
            bcutot(j,k)=0.0
            bcustot(j,k)=0.0
            btdp(j,k)=0.0
            bulags(j,k)=0.0
            bulagj(j,k)=0.0
            bulago(j,k)=0.0
            bulagt(j,k)=0.0
            blagrets(j,k)=0.0
            blagretj(j,k)=0.0
            blagreto(j,k)=0.0
            blagrett(j,k)=0.0
            btotret(j,k)=0.0
            blaglates(j,k)=0.0
            blaglatej(j,k)=0.0
            blaglateo(j,k)=0.0
            blaglatet(j,k)=0.0
            bdeps(j,k)=0.0
            bdepj(j,k)=0.0
            bdepo(j,k)=0.0
            bdept(j,k)=0.0
            bshortage(j,k)=0.0
            do l=1,10
              bgrass(j,k,l)=0.0
            enddo
          enddo
        enddo

      SBID(0)='DIST00'
      SBNAME(0) = 'Other Structures'      
      BID='TOTAL'
      BNAME = 'Scenario total'
      END
