         subroutine WSUPSUM
C***************************************************************************
C
C   Function        : wsupsum.for
C   Author          : LRCWE
C   Date            : March 1999
C   Purpose         : This subroutine performs the water budget and prepares
C                     the *.dwb and *.swb output files when a water supply
C                     limited analysis is requested. Water supply from direct
C                     diversions and the soil moisture reservoir or tracked.
C           
c         If water rights are considered, water is colored to account
C                     for junior, senior, and other diversions. If a river depletion
C                     analysis is requested, return flow percents and timing are
C                     accounted for.
C
C   January, 2000     Subroutine updated to handle groundwater pumping estimates
c rrb* 2003/05/19; Revised dimension of grass from 3 to 4 dimensions.
c rrb* 2003/05/19; Revised dimension of variables swmet and idcnt
c                  from 12 to 14.
c rrb* 2003/05/19; Revised the read statement for the variable grass
c                  from 3 to 4 dimensions.
c rrb* 2003/05/19; Revised the write statements for the variable grass
c                  from 3 to 4 dimentions.
c ew   2004/03/12; Revised to allow drain and tail water reuse
c rrb* 2004/03/31; Revised to print SW return & drain reuse (tail) 
c                  to *.dwb
c rrb* 2004/04/06; Revised initilization of selected variables to occur
c                  outside the year loop in order to allow position
c                  14 to include the basin total.
c rrb* 2004/05/11; Minor corrections related to Sw return reuse (tail)
c ew/jhb 2005-2007; major upgrade; added binary file (BD1) output;
c                   added 4th irrigated land category (surface water-sprinkler);
c                   changed input file format to include 4 acreages;
c                   added detailed water budget output for one structure
c jhb 2007/09/25    temporarily remove warning message for extra structures in the DRA (drain) file
c jhb 2011/01/19    changed the ISUPLY=2 DWB output back to farm deliv (seniorf(), etc.).  the new values (changed in Dec 08) were not working
C
C   Calling program : statecu.f
C   Called programs : none
C   Input arguments : none 
C   Output arguments: none
C   Assumptions     :
C***************************************************************************

Cjhb====================================================================
Cjhb  note that initializing the variable values in the declarations probably causes
Cjhb  variables to be included in the executable image at compile time instead
Cjhb  of at execution time.  This potentially increases the exe file size.
Cjhb  Avoided initializing larger arrays in the declarations.
Cjhb====================================================================

Cjhb====================================================================
      INCLUDE 'gcommon.inc'
Cjhb====================================================================
      INCLUDE 'bindat.inc'
Cjhb====================================================================
Cjhb  real variable to hold calculated monthly shortage value
      REAL :: SHORTAGE=0.0
c     define common and local variables
      REAL :: divndata(31)=0.,wbu_used=0.
      character*10 :: crpname(DIM_NC,dim_ny),as(15)
      character*200 :: thefile1,thefile2,thefile3,thefile4
      character*10 :: atxt(dim_na,DIM_NC,DIM_NY),method
      character*10 :: method2='Calculated'
      character*3 :: idum3
      character*200 :: fline
      LOGICAL :: setswac=.FALSE.,bIPYNew=.TRUE.
      LOGICAL :: DistByPriority=.TRUE., FOLLOWUP=.FALSE.
c grb 4-20-00 added flength and tempwd as character variables
      INTEGER :: m1=0,IDAYS=0,i=0,j=0,k=0,l=0,m=0
      character*524 :: tempwd
      REAL :: short=0.,GRASS(DIM_NA,DIM_NY,14,10)

      character*12 :: tspid,aspid,pvhid
      character*24 :: twdid
      character*39 :: gwdesg
      character*84 :: comment(DIM_NY)
C-----Return Flows
      REAL :: ulags(DIM_NY,14),ulagj(DIM_NY,14)
      REAL :: ulago(DIM_NY,14),ulagt(DIM_NY,14)
      REAL :: lagrets(DIM_NY,14),lagretj(DIM_NY,14)
      REAL :: lagreto(DIM_NY,14)
      REAL :: laglates(DIM_NY,14),laglatej(DIM_NY,14)
      REAL :: laglateo(DIM_NY,14)
      REAL :: lagrett(DIM_NY,14),laglatet(DIM_NY,14)
      REAL :: totret(DIM_NY,14),rets=0.,retj=0.,rett=0.,reto=0.

C-----River Depletion
      REAL :: deps(DIM_NY,14),depj(DIM_NY,14)
      REAL :: depo(DIM_NY,14),dept(DIM_NY,14)
      real :: total_adj=0.0,sen_adj=0.0,jun_adj=0.0,oth_adj=0.0
C-----Crop and Soil CU
      REAL :: cropcusoils(DIM_NY,14)
      REAL :: cropcusoil(DIM_NY,14),cropcusoilj(DIM_NY,14)
      REAL :: soil_cus(DIM_NY,14),soil_cuj(DIM_NY,14)
c   rkj 4-17-00 added variable soil_cujout
      REAL :: soil_cuo(DIM_NY,14),soil_cujout(dim_ny,14)
c   jhb 5-16-07 added variable soil_cuoout
      REAL :: soil_cuoout(dim_ny,14)
      REAL :: sxs=0.,pojsm=0.,poosm=0.,poosmbj=0.
      REAL :: crop_cus(DIM_NY,14),crop_cuj(DIM_NY,14)
      REAL :: crop_cuo(DIM_NY,14)
      REAL :: cropcusoilt(DIM_NY,14),soiltot=0.
      REAL :: soil_cu(DIM_NY,14),crop_cut(DIM_NY,14)
      REAL :: estcrpj(DIM_NY,14),estcrps(DIM_NY,14)
      REAL :: divcu(DIM_NY,14)
      REAL :: crop_cu(DIM_NY,14),soiltoto=0.
      REAL :: soiltotts(DIM_NY,14),soiltottj(DIM_NY,14)
      REAL :: soiltott(DIM_NY,14),soiltotto(DIM_NY,14)
      REAL :: estcrpo(DIM_NY,14)
      REAL :: ptotmo=0.,senmo=0.,junmo=0.,othmo=0.,totmo=0.
      REAL :: cropcusoilo(DIM_NY,14)
      REAL :: soiltots=0.,soiltotj=0.,ttotmo=0.
      REAL :: tsenmo=0.,tjunmo=0.,tothmo=0.
      REAL :: nonconsumed

C-----General	
      REAL :: spcapz=0.,reqreq(DIM_NY,14),tspcapz=0.
      REAL :: effcu(DIM_NY,14),comeff(12)=0.,seffcu(DIM_NY,14)
      REAL :: sumeff=0.,sumseff=0.,sumeffcnt=0.,sumseffcnt=0.
      REAL :: sumgwcu=0.
	
C-----Summary
      REAL :: percent(DIM_NY)=0.,holdcropo=0.
      REAL :: ddhmonot(DIM_NY,14),acret(DIM_NY)
      REAL :: reqreqts(DIM_NY,14)
      REAL :: holds=0.,holdj=0.,holdps=0.,holdpj=0.
      REAL :: holds_sf=0.,holds_ss=0.,holds_sw=0.
      REAL :: holds_gf=0.,holds_gs=0.,holds_gw=0.
      REAL :: holdj_sf=0.,holdj_ss=0.,holdj_sw=0.
      REAL :: holdj_gf=0.,holdj_gs=0.,holdj_gw=0.
      REAL :: holdo_sf=0.,holdo_ss=0.,holdo_sw=0.
      REAL :: holdo_gf=0.,holdo_gs=0.,holdo_gw=0.
      REAL :: numat(DIM_NY)
      REAL :: holdj1=0.,holds1=0.,holdpo=0.,holdo=0.
      REAL :: holdo1=0.,holdt=0.,holdcrop=0.
      REAL :: holdcrops=0.,holdcropj=0.,holdt1=0.,holdlagj(DIM_NY,12)=0.
      REAL :: holdlagt(DIM_NY,12)=0.,holdlags(DIM_NY,12)=0.
      REAL :: holdlago(DIM_NY,12)=0.,P1(12)=0.
      CHARACTER*20 :: crpnamet(DIM_NC)=''
      INTEGER ::  numcrop=0,cropy=0
C---Parameters for ground water addition
      REAL :: ceff(DIM_NA,DIM_NY)
      REAL :: fleff(dim_na,dim_ny),speff(DIM_NA,DIM_NY)
      REAL :: gper(DIM_NA,DIM_NY),sper(dim_na,dim_ny)
      REAL :: mprate(dim_na,dim_ny,14)
      REAL :: sfhold=0.,sshold=0.,swhold=0.
      REAL :: sfholds=0.,sfholdj=0.,sfholdo=0.
      REAL :: ssholds=0.,ssholdj=0.,ssholdo=0.
      REAL :: gfhold=0.,gshold=0.,gwhold=0.
      REAL :: gfholds=0.,gfholdj=0.,gfholdo=0.
      REAL :: gsholds=0.,gsholdj=0.,gsholdo=0.
      REAL :: holdfs=0.,holdfj=0.,holdfo=0.
      REAL :: t1=0.,t2=0.,t3=0.,t4=0.,t5=0.,t6=0.,t7=0.,t8=0.
      REAL :: swreq(dim_ny,14)
      REAL :: sfreq(dim_ny,14),ssreq(dim_ny,14)
      REAL :: gwreq(dim_ny,14)
      REAL :: gfreq(dim_ny,14),gsreq(dim_ny,14)
      REAL :: swreqdef(dim_ny,14)
      REAL :: sfreqdef(dim_ny,14),ssreqdef(dim_ny,14)
      REAL :: gwreqdef(dim_ny,14)
      REAL :: gfreqdef(dim_ny,14),gsreqdef(dim_ny,14)
      REAL :: scalefctr=0.
      REAL :: seniorf(DIM_NY,14),juniorf(DIM_NY,14)
      REAL :: otherf(DIM_NY,14)
      REAL :: gdiv(DIM_NY,14),gwcu(DIM_NY,14),gwro(DIM_NY,14)
      REAL :: gwcuf,gwcus,gwrof,gwros
      REAL :: tdp(DIM_NY,14),closs(DIM_NY,14),fdiv(DIM_NY,14)
      REAL :: tmp1=0.,tmp2=0.
c     farm deliveries from surface sources
      REAL :: swshare=0.,sfshare=0.,ssshare=0.
      REAL :: swshares=0.,swsharej=0.,swshareo=0.
      REAL :: sfshares=0.,sfsharej=0.,sfshareo=0.
      REAL :: ssshares=0.,sssharej=0.,ssshareo=0.
      REAL :: gwshare=0.,gfshare=0.,gsshare=0.
      REAL :: gwshares=0.,gwsharej=0.,gwshareo=0.
      REAL :: gfshares=0.,gfsharej=0.,gfshareo=0.
      REAL :: gsshares=0.,gssharej=0.,gsshareo=0.
c     farm deliveries from groundwater sources
      REAL :: gwpump=0.,gfpump=0.,gspump=0.
c     cu from soil moisture sources
      REAL :: swsoilcu=0.,sfsoilcu=0.,sssoilcu=0.
      REAL :: sfsoilcus=0.,sfsoilcuj=0.,sfsoilcuo=0.
      REAL :: sssoilcus=0.,sssoilcuj=0.,sssoilcuo=0.
      REAL :: swsoilcus=0.,swsoilcuj=0.,swsoilcuo=0.
      REAL :: gwsoilcu=0.,gfsoilcu=0.,gssoilcu=0.
      REAL :: gfsoilcus=0.,gfsoilcuj=0.,gfsoilcuo=0.
      REAL :: gssoilcus=0.,gssoilcuj=0.,gssoilcuo=0.
      REAL :: gwsoilcus=0.,gwsoilcuj=0.,gwsoilcuo=0.
c     cu from all sources
      REAL :: swcu=0.,sfcu=0.,sscu=0.,gcu=0.,gscu=0.,gfcu=0.
      REAL :: swsoil=0.,sfsoil=0.,sssoil=0.
      REAL :: gwsoil=0.,gfsoil=0.,gssoil=0.
      REAL :: arech(dim_ny,14)
      REAL :: effgw(DIM_NY,14),wghteff=0.
      REAL :: swdiv(DIM_NY,14)
      REAL :: ssdiv(DIM_NY,14),sfdiv(DIM_NY,14)
      REAL :: gwdiv(DIM_NY,14)
      REAL :: gsdiv(DIM_NY,14),gfdiv(DIM_NY,14)
      REAL :: sfeff(dim_na,dim_ny)=0., sfeffcnt=0.0
c jhb 05-18-07 add a system efficiency for gw lands      
      REAL :: gfeff(dim_na,dim_ny)=0., gfeffcnt=0.0
c grb 06-05-00 change dimension to 100 from 99
      REAL :: swmet(100,dim_ny,14),met=0.,idcnt(100,dim_ny,14)
      REAL :: custot(dim_na,dim_ny,14)
      REAL :: LftOvr=0.,LftOvrSF=0.,LftOvrSS=0.
      REAL :: LftOvrGF=0.,LftOvrGS=0.
      REAL :: percenta(DIM_NY)=0.
      REAL :: holdests=0.,holdestj=0.,holdesto=0.,holdestt=0.
      REAL :: senaspt=0.,junaspt=0.
      REAL :: sfcus=0.,sfcuj=0.,sfcuo=0.
      REAL :: sscus=0.,sscuj=0.,sscuo=0.
      REAL :: gfcus=0.,gfcuj=0.,gfcuo=0.
      REAL :: gscus=0.,gscuj=0.,gscuo=0.
      REAL :: sfinefs=0.,sfinefj=0.,sfinefo=0.
      REAL :: ssinefs=0.,ssinefj=0.,ssinefo=0.
      REAL :: gfinefs=0.,gfinefj=0.,gfinefo=0.
      REAL :: gsinefs=0.,gsinefj=0.,gsinefo=0.
      REAL :: sfinefsx=0.,sfinefjx=0.,sfinefox=0.
      REAL :: ssinefsx=0.,ssinefjx=0.,ssinefox=0.
      REAL :: gfinefsx=0.,gfinefjx=0.,gfinefox=0.
      REAL :: gsinefsx=0.,gsinefjx=0.,gsinefox=0.
      REAL :: inefs=0.,inefj=0.,inefo=0.
c ew 03/12/04 set dim for tailwater variable
      REAL :: tail(dim_na,dim_ny,14) !DIVSUP(DIM_NA,DIM_NY,14)
Cjhb====================================================================
Cjhb  11/2006 variables for acreages of the four irrigated land categories
Cjhb====================================================================
      REAL :: swflac(dim_na,dim_ny),swspac(dim_na,dim_ny)
      REAL :: swgwflac(dim_na,dim_ny),swgwspac(dim_na,dim_ny)
      REAL :: SMTotAcr=0.0
Cjhb====================================================================
Cjhb  01/2007 variables for soil moisture contents of the four irrigated land categories
Cjhb  (account separately, even though they are pooled at the end)
Cjhb====================================================================
      REAL :: swflsm(dim_na,dim_ny),swspsm(dim_na,dim_ny)
      REAL :: swgwflsm(dim_na,dim_ny),swgwspsm(dim_na,dim_ny)
Cjhb====================================================================
Cjhb  01/2007 variables for cu of the four irrigated land categories
Cjhb  (account separately, even though they are pooled at the end)
Cjhb====================================================================
      REAL :: swflcu(dim_na,dim_ny),swspcu(dim_na,dim_ny)
      REAL :: swgwflcu(dim_na,dim_ny),swgwspcu(dim_na,dim_ny)
Cjhb====================================================================
      INTEGER :: itail(dim_na)=0
      INTEGER :: imonth(14)=0,iyear(DIM_NY)=0,i9=0
      INTEGER :: iflag2(DIM_NA)=0,gmode(dim_na,dim_ny)
      INTEGER :: iflag(DIM_NA,DIM_NY)
c
c-- totals for scenario water budget
c
      REAL :: treq(DIM_NY,14),tdiv(DIM_NY,14),tcus(DIM_NY,14)
      REAL :: ttail(DIM_NY,14)
      REAL :: tcuj(DIM_NY,14),tcuo(DIM_NY,14),tcut(DIM_NY,14)
      REAL :: tscus(DIM_NY,14),tscuj(DIM_NY,14)
      REAL :: tscuo(DIM_NY,14),tscu(DIM_NY,14)
      REAL :: tulags(DIM_NY,14),tulagj(DIM_NY,14)
      REAL :: tulago(DIM_NY,14),tulagt(DIM_NY,14)
      REAL :: tdivcu(DIM_NY,14),teffcu(DIM_NY,14)
      REAL :: ttotts(DIM_NY,14),ttottj(DIM_NY,14)
      REAL :: tseffcu(DIM_NY,14),ttotto(DIM_NY,14)
      REAL :: ttott(DIM_NY,14),tcusoil(DIM_NY,14)
      REAL :: tcrps(DIM_NY,14),tcrpj(DIM_NY,14)
      REAL :: tcrpo(DIM_NY,14),tcrpt(DIM_NY,14)
      REAL :: trets(DIM_NY,14),tretj(DIM_NY,14)
      REAL :: treto(DIM_NY,14),trett(DIM_NY,14)
      REAL :: tlates(DIM_NY,14),tlatej(DIM_NY,14)
      REAL :: tlateo(DIM_NY,14),tlatet(DIM_NY,14)
      REAL :: ttotret(DIM_NY,14),tdeps(DIM_NY,14)
      REAL :: tdepj(DIM_NY,14),tdepo(DIM_NY,14)
      REAL :: tdept(DIM_NY,14),tarech(dim_ny,14)
      REAL :: tsenf(DIM_NY,14),tjunf(DIM_NY,14)
      REAL :: tothf(DIM_NY,14),tgdiv(DIM_NY,14)
      REAL :: tgwcu(DIM_NY,14),tgwro(DIM_NY,14)
      REAL :: ttdp(DIM_NY,14),tcloss(DIM_NY,14)
      REAL :: tfdiv(DIM_NY,14),tgwdiv(DIM_NY,14)
      REAL :: tgsdiv(DIM_NY,14),tgfdiv(DIM_NY,14)
      REAL :: teffgw(DIM_NY,14),tsfeff(dim_ny)
      REAL :: tet(dim_ny,14),teffr(dim_ny,14)
      REAL :: treqt(dim_ny,14),twbu(dim_ny,14)

      IOUTP=0

Cjhb====================================================================
Cjhb  always output a BD1 file in this routine
      LBD1OUT=.TRUE.  !create BD1 in this routine
      LBD2OUT=.FALSE.
      LBD3OUT=.FALSE.
      LBD4OUT=.FALSE.
      LBD5OUT=.FALSE.
Cjhb====================================================================

Cjhb=&==================================================================
c     initialize some large arrays
Cjhb=&==================================================================
      do i=1,dim_na
        do j=1,dim_ny
Cjhb      ==============================================================
Cjhb      set the deficit irrigation fraction the same for all structures
Cjhb        and for all years...for now...eventually could read this from
Cjhb        the IPY input file...
          def_irr_frac(i,j)=def_irr
Cjhb      ==============================================================
          ceff(i,j)=0.0
          fleff(i,j)=0.0
          gper(i,j)=0.0
          sper(i,j)=0.0
          speff(i,j)=0.0
Cjhb      ==============================================================
          swflac(i,j)=0.0
          swspac(i,j)=0.0
          swgwflac(i,j)=0.0
          swgwspac(i,j)=0.0
Cjhb      ==============================================================
          swflsm(i,j)=0.0
          swspsm(i,j)=0.0
          swgwflsm(i,j)=0.0
          swgwspsm(i,j)=0.0
Cjhb      ==============================================================
          swflcu(i,j)=0.0
          swspcu(i,j)=0.0
          swgwflcu(i,j)=0.0
          swgwspcu(i,j)=0.0
Cjhb      ==============================================================
          iflag(i,j)=0
          gmode(i,j)=0
          do k=1,14
            tail(i,j,k)=0.0
            mprate(i,j,k)=0.0
            custot(i,j,k)=0.0
            wbu(i,j,k)=0.0
            wbused(i,j,k)=0.0
            do l=1,10
              grass(i,j,k,l)=0.0
            enddo
          enddo
        enddo
      enddo

      do j=1,dim_ny
        do k=1,14
          ulags(j,k)=0.
          ulagj(j,k)=0.
          ulago(j,k)=0.
          ulagt(j,k)=0.
          lagrets(j,k)=0.
          lagretj(j,k)=0.
          lagreto(j,k)=0.
          laglates(j,k)=0.
          laglatej(j,k)=0.
          laglateo(j,k)=0.
          lagrett(j,k)=0.
          laglatet(j,k)=0.
          totret(j,k)=0.
          deps(j,k)=0.
          depj(j,k)=0.
          depo(j,k)=0.
          dept(j,k)=0.
          cropcusoils(j,k)=0.
          cropcusoil(j,k)=0.
          cropcusoilj(j,k)=0.
          soil_cus(j,k)=0.
          soil_cuj(j,k)=0.
          soil_cuo(j,k)=0.
          soil_cujout(j,k)=0.
          soil_cuoout(j,k)=0.
          crop_cus(j,k)=0.
          crop_cuj(j,k)=0.
          crop_cuo(j,k)=0.
          cropcusoilt(j,k)=0.
          soil_cu(j,k)=0.
          crop_cut(j,k)=0.
          estcrpj(j,k)=0.
          estcrps(j,k)=0.
          divcu(j,k)=0.
          crop_cu(j,k)=0.
          soiltotts(j,k)=0.
          soiltottj(j,k)=0.
          soiltott(j,k)=0.
          soiltotto(j,k)=0.
          estcrpo(j,k)=0.
          cropcusoilo(j,k)=0.
          effcu(j,k)=0.
          seffcu(j,k)=0.
          ddhmonot(j,k)=0.
          reqreqts(j,k)=0.
          swreq(j,k)=0.
          sfreq(j,k)=0.
          ssreq(j,k)=0.
          gwreq(j,k)=0.
          gfreq(j,k)=0.
          gsreq(j,k)=0.
          swreqdef(j,k)=0.
          sfreqdef(j,k)=0.
          ssreqdef(j,k)=0.
          gwreqdef(j,k)=0.
          gfreqdef(j,k)=0.
          gsreqdef(j,k)=0.
          seniorf(j,k)=0.
          juniorf(j,k)=0.
          otherf(j,k)=0.
          gdiv(j,k)=0.
          gwcu(j,k)=0.
          gwro(j,k)=0.
          tdp(j,k)=0.
          closs(j,k)=0.
          fdiv(j,k)=0.
          arech(j,k)=0.
          effgw(j,k)=0.
          swdiv(j,k)=0.
          ssdiv(j,k)=0.
          sfdiv(j,k)=0.
          gwdiv(j,k)=0.
          gsdiv(j,k)=0.
          gfdiv(j,k)=0.
          treq(j,k)=0.
          tdiv(j,k)=0.
          ttail(j,k)=0.
          tcus(j,k)=0.
          tcuj(j,k)=0.
          tcuo(j,k)=0.
          tcut(j,k)=0.
          tscus(j,k)=0.
          tscuj(j,k)=0.
          tscuo(j,k)=0.
          tscu(j,k)=0.
          tulags(j,k)=0.
          tulagj(j,k)=0.
          tulago(j,k)=0.
          tulagt(j,k)=0.
          tdivcu(j,k)=0.
          teffcu(j,k)=0.
          ttotts(j,k)=0.
          ttottj(j,k)=0.
          tseffcu(j,k)=0.
          ttotto(j,k)=0.
          ttott(j,k)=0.
          tcusoil(j,k)=0.
          tcrps(j,k)=0.
          tcrpj(j,k)=0.
          tcrpo(j,k)=0.
          tcrpt(j,k)=0.
          trets(j,k)=0.
          tretj(j,k)=0.
          treto(j,k)=0.
          trett(j,k)=0.
          tlates(j,k)=0.
          tlatej(j,k)=0.
          tlateo(j,k)=0.
          tlatet(j,k)=0.
          ttotret(j,k)=0.
          tdeps(j,k)=0.
          tdepj(j,k)=0.
          tdepo(j,k)=0.
          tdept(j,k)=0.
          tarech(j,k)=0.
          tsenf(j,k)=0.
          tjunf(j,k)=0.
          tothf(j,k)=0.
          tgdiv(j,k)=0.
          tgwcu(j,k)=0.
          tgwro(j,k)=0.
          ttdp(j,k)=0.
          tcloss(j,k)=0.
          tfdiv(j,k)=0.
          tgwdiv(j,k)=0.
          tgsdiv(j,k)=0.
          tgfdiv(j,k)=0.
          teffgw(j,k)=0.
          tet(j,k)=0.
          teffr(j,k)=0.
          treqt(j,k)=0.
          twbu(j,k)=0.
        enddo
      enddo
            
c
c rrb 2003/06/20; Allow multiple subirrigation crop types
      iflood2=iflood*2
Cjhb=&==================================================================
c     check to see if ipy file has sufficient time seris data for the modeling period
c     exit if not
Cjhb=&==================================================================
      if(ipyfile.eq.'') goto 42
      open (unit=150,file=ipyfile,status='old',ERR=42)
      call skipn(150)
      read(150,29) jyr1, jyr2, idum3
      if((nyr1 .lt. jyr1) .or. (nyr2 .gt. jyr2)) then
        write(0,*) 'Stop-irrigation practice data (*.ipy file) not avail
     &able for all years'
      write(999,*) 'Stop-irrigation practice data (*.ipy file) not avail
     &able for all years'
        stop
      endif

c
c check to see if tsp data exists for all structures
c and read data into array
c
Cjhb=&==================================================================
Cjhb=&  IPY file format
Cjhb=&==================================================================
Cjhb=&  itmp1 - year
Cjhb=&  tspid - structure id
Cjhb=&  t1 - max delivery efficiency - fraction (0.01-1.00) or percentage (1.01 - 100.00)
Cjhb=&  t2 - max flood irrigation efficiency  - fraction (0.01-1.00) or percentage (1.01 - 100.00)
Cjhb=&  t3 - max sprinkler efficiency  - fraction (0.01-1.00) or percentage (1.01 - 100.00)
Cjhb=&==================================================================
Cjhb=&  changed to input the four irrigation type acreages directly
Cjhb=&  in order to handle a fourth irrigated land category:
Cjhb=&  t4 Surface water only source - flood irrigated acreage (acres)
Cjhb=&  t5 Surface water only source - sprinkler irrigated acreage (acres)
Cjhb=&  t6 Surface water and Groundwater source - flood irrigated acreage (acres)
Cjhb=&  t7 Surface water and Groundwater source - sprinkler irrigated acreage (acres)
Cjhb=&==================================================================
Cjhb=&  t8 Maximum pumping volume (AF per month)
Cjhb=&  i9 Ground water use mode (1=maximize supply,2=mutual ditch,3=mutual ditch w/ recharge)
Cjhb=&  last value on record is total acreage, but is not used by StateCU (StateMod uses it)
Cjhb=&  since total acreage, t_area(i,iyr), is already input from the CDS input file
Cjhb=&==================================================================
c30      read (150,31,end=32) itmp1,tspid,t1,t2,t3,t4,t5,t6,i7
c31      format(i4,1x,a12,3f6.0,2f8.0,f12.0,i4)
Cjhb=&==================================================================
Cjhb=&  read the next record in the IPY file in the new format
Cjhb=&==================================================================
c30       read (150,'(a200)',end=32) fline
Cjhb=&==================================================================
Cjhb=&  check that the trimmed line has enough characters to match the NEW IPY file format
Cjhb=&  new IPY format even without acreage is 4+1+12+6+6+6+8+8+8+8+12+3=82
Cjhb=&    but the last integer could be in column 80, so 80 is the min for a new IPY file
Cjhb=&  an OLD style IPY file, even including the acreage, is format: 4+1+12+6+6+6+8+8+12+3+8=74
Cjhb=&    so 74 would be the max.  The min would be 64 (gwmode integer in first col of field)
Cjhb=&  Therefore if a trimmed record has length 80 or greater, try reading it as NEW;
Cjhb=&    if it is between 64 and 79, try reading it as OLD;
Cjhb=&    otherwise generate and error and stop.
Cjhb=&==================================================================
Cjhb=& 03/21/07 IMPORTANT!!!!!!!
Cjhb=& 03/21/07 Some older but important and circulated IPY files (e.g. RG2004)
Cjhb=& 03/21/07 used a gwmode field width of 4.  Since the first character of the
Cjhb=& 03/21/07 next field (total acreage) is almost always blank, the
Cjhb=& 03/21/07 decision has been made to use a gwmode format spec of I4 instead of I3.
Cjhb=& 03/21/07 This allows these IPY files to be read without error.
Cjhb=& 03/21/07 Most current and ALL future IPY files use a field width of 3,
Cjhb=& 03/21/07 but as long as the first character of the next field is blank,
Cjhb=& 03/21/07 the I4 spec will work and not cause read errors...
Cjhb=&==================================================================
c      if(len_trim(fline).GE.80) then !new format
c        setswac=.FALSE.
        bIPYNew=.TRUE.
30      read (150,31,end=32) itmp1,tspid,t1,t2,t3,t4,t5,t6,t7,t8,i9
c        read(fline,31)itmp1,tspid,t1,t2,t3,t4,t5,t6,t7,t8,i9
31      format(i4,1x,a12,3f6.0,4f8.0,f12.0,i4)
Cjhb=&  ----------------------------------------------------------------

c ew  we don't know what "i" is when we're in here.  Need to determine for error messages

c       first thing - convert any negative acreages to 0
311     if(t4.lt.0.0) then
          t4=0.0
c         catch this warning and record in new log file format...

          if(ipresim .ne. 1) call lw_update(41,tspid)
          select case (scu_debug)
          case (0)
          case (1)
            write(999,2008)itmp1,tspid
          case default
          end select
        endif
        if(t5.lt.0.0) then
          t5=0.0
c         catch this warning and record in new log file format...
          if(ipresim .ne. 1) call lw_update(42,tspid)
          select case (scu_debug)
          case (0)
          case (1)
            write(999,2009)itmp1,tspid
          case default
          end select
        endif
        if(t6.lt.0.0) then
          t6=0.0
c         catch this warning and record in new log file format...
          if(ipresim .ne. 1) call lw_update(43,tspid)
          select case (scu_debug)
          case (0)
          case (1)
            write(999,2010)itmp1,tspid
          case default
          end select
        endif
        if(t7.lt.0.0) then
          t7=0.0
c         catch this warning and record in new log file format...
          if(ipresim .ne. 1) call lw_update(44,tspid)
          select case (scu_debug)
          case (0)
          case (1)
            write(999,2011)itmp1,twdid
          case default
          end select
        endif
Cjhb=&  ----------------------------------------------------------------
c      elseif(len_trim(fline).GE.64) then !old format
Cjhb=&  set a flag to assign the missing acreage to the sw flood category t4 (later in the code)
c        setswac=.TRUE.
c        bIPYNew=.FALSE.
c        read(fline,1131)itmp1,tspid,t1,t2,t3,t4,t5,t8,i9
c 1131   format(i4,1x,a12,3f6.0,2f8.0,f12.0,i3)
c 1131   format(i4,1x,a12,3f6.0,2f8.0,f12.0,i4)
Cjhb=&  ----------------------------------------------------------------
c       sometimes in the old IPY files, the sprinkler acreage is greater
c         than the groundwater acreage and sometimes the gw or sprinkler acreage (or both)
c         are greater than the total acreage from the CDS file
c       so if this is an old style IPY record, then use the old style
c         rules for handling the discrepancies in the acreage values:
c         1. urf structures are 100% gw. (handle this later)
c         2. shrink the gw and sprinkler acreages to be equal to or less than total acreage (handle this later).
c         3. increase the gw acreage (t4) to match the sprinkler acreage (t5) (do this now)
Cjhb=&  ----------------------------------------------------------------
c       first thing - convert any negative acreages to 0
c        if(t4.lt.0.0) then
c          t4=0.0
c         catch this warning and record in new log file format...
c          if(ipresim .ne. 1) call lw_update(45,bas_id(i))
c          select case (scu_debug)
c          case (0)
c          case (1)
c            write(999,2012)itmp1,twdid
c          case default
c          end select
c        endif
c        if(t5.lt.0.0) then
c          t5=0.0
cc         catch this warning and record in new log file format...
c          if(ipresim .ne. 1) call lw_update(44,bas_id(i))
c          select case (scu_debug)
c          case (0)
c          case (1)
c            write(999,2011)itmp1,twdid
c          case default
c          end select
c        endif
cCjhb=&  ----------------------------------------------------------------
c        if(t5 .gt. t4)then
cc         catch this warning and record in new log file format...
c          if(ipresim .ne. 1) call lw_update(46,bas_id(i))
c          select case (scu_debug)
c          case (0)
c            t4=t5
c          case (1)
c            write(999,2013)itmp1,twdid
c            write(999,2014)itmp1,twdid,t4,t5
c            t4=t5
c            write(999,2015)itmp1,twdid,t4,t5
c          case default
c            t4=t5
c          end select
c        endif
cCjhb=&  ----------------------------------------------------------------
ccc       assign the acreages to the four categories - note that rules 1 and 2 from above still have NOT been done yet
cCjhb=&  ----------------------------------------------------------------
c        t7=t5
c        t6=t4-t5
c        t4=0.
c        t5=0.
c      else
c        write(*,*)'Invalid IPY file record:'
c        write(*,*)fline
c        write(*,*)'See documentation for IPY file format details.'
c        write(999,*)'Invalid IPY file record:'
c        write(999,*)fline
c        write(999,*)'See documentation for IPY file format details.'
c        stop
c      endif
Cjhb=&==================================================================
Cjhb=&  see if it's in the modeled time period
Cjhb=&==================================================================
c      if(itmp1.lt.nyr1) goto 30 !read the next line in the IPY file
c      if(itmp1.gt.nyr2) goto 32 !done reading the IPY file
Cjhb=&==================================================================
Cjhb=&  it is! so loop through the structures until the ID matches
Cjhb=&==================================================================
        do 25 i=1,nbasin
          twdid=bas_id(i)
          iyr=itmp1-nyr1+1
          itmp2=iyr
          if(twdid(1:12) .eq. tspid) then
c            itmp2=itmp1-nyr1+1
Cjhb=&==================================================================
Cjhb=&      reset some acreage values in the "old way" if it was the old format IPY record
Cjhb=&==================================================================
c            if(setswac)then
Cjhb=&        ----------------------------------------------------------
c             this is an old IPY record with only a gw acreage and a sprinkler acreage in the IPY file
Cjhb=&        ----------------------------------------------------------
c             first apply rule 2 from above
Cjhb=&        ----------------------------------------------------------
c              if(t6+t7.gt.t_area(i,iyr))then
c               more gw acreage than total acreage - not good.  see how close it is...
c                FOLLOWUP=.FALSE.
c                if((abs((t6+t7-t_area(i,iyr))/t_area(i,iyr)).gt.0.02)
c     &             .AND.(abs(t6+t7-t_area(i,iyr)).GT.1.0))then
Cjhb=&            ------------------------------------------------------
c                 if the diff is bigger than 2% and larger than 1.0 AF
c                 then it is big enough to write a warning about...
Cjhb=&            ------------------------------------------------------
c                 catch this warning and record in new log file format...
c                  if(ipresim .ne. 1) call lw_update(40,bas_id(i))
c                  select case (scu_debug)
c                  case (0)
c                  case (1)
c                   write(999,2007)itmp1,twdid
c                   write(999,2005)itmp1,twdid,t_area(i,iyr),t4,t5,t6,t7
c                  case default
c                  end select
c                  FOLLOWUP=.TRUE.
c                endif
Cjhb=&          ----------------------------------------------------------
c               note that if t_area(i,iyr)=0, then t6 and t7 will =0
Cjhb=&          ----------------------------------------------------------
c                if(t7.gt.t_area(i,iyr))then
c                  t6=0.0
c                  t7=t_area(i,iyr)
c                else
c                  t6=t_area(i,iyr)-t7
c                endif
c                if(FOLLOWUP)then
c                  select case (scu_debug)
c                  case (0)
c                  case (1)
c                   write(999,2006)itmp1,twdid,t_area(i,iyr),t4,t5,t6,t7
c                  case default
c                  end select
c                endif
c              endif
Cjhb=&        ----------------------------------------------------------
c             then set the SW FLOOD acreage to be the difference between the total acreage and the (corrected) gw acreage
Cjhb=&        ----------------------------------------------------------
c              t4=t_area(i,iyr)-(t6+t7)
c            endif
Cjhb=&==================================================================
c           mark that we found data for structure, i, for year, itmp2
Cjhb=&      ------------------------------------------------------------
            iflag(i,itmp2)=1
Cjhb=&==================================================================
            if((abs(t1).lt.0.001).or.
     &         (abs(t2).lt.0.001).or.
     &         (abs(t3).lt.0.001)) then
              write(0,*) 'Stop, efficiency is equal to zero for Structur
     :e ', twdid, ' check irrigation practice (*.ipy) file'
            write(999,*) 'Stop, efficiency is equal to zero for Structur
     :e ', twdid,' check irrigation practice (*.ipy) file'
              stop
            endif
Cjhb=&      ceff(i,iyr) = delivery efficiency from sw diversion to farm headgate
            if(t1.le.1.0)then
              ceff(i,iyr)=t1 !old fraction format
            else
              ceff(i,iyr)=t1/100.0 !new percentage format
            endif
Cjhb=&      fleff(i,iyr) = flood irrigation efficiency
            if(t2.le.1.0)then
              fleff(i,iyr)=t2 !old fraction format
            else
              fleff(i,iyr)=t2/100.0 !new percentage format
            endif
Cjhb=&      speff(i,iyr) = sprinkler irrigation efficiency
            if(t3.le.1.0)then
              speff(i,iyr)=t3 !old fraction format
            else
              speff(i,iyr)=t3/100.0 !new percentage format
            endif
          if(ceff(i,iyr) .eq. 1.0) then
            if(ipresim .ne. 1) call lw_update(52,bas_id(i))
          endif  
Cjhb=&==================================================================
Cjhb=&      gmode(i,iyr) = groundwater use mode
Cjhb=&                  1=surface and GW are used to maximize supply.
Cjhb=&                  2=surface water is used 1st on all
Cjhb=&                  acreage, and then GW.
Cjhb=&                  3=GW is used first on sprinkler
Cjhb=&                  acreage and surface water shares for
Cjhb=&                  the same acreage are available for recharge.
Cjhb=&==================================================================
            select case (isuply)
              case (4) !groundwater supply available, all gmodes are available, use the value in the file
                select case (i9)
                  case (1)                  
                    gmode(i,iyr)=i9
                    if(ipresim .ne. 1) call lw_update(48,bas_id(i))
                  case (2)
                    gmode(i,iyr)=i9
                    if(ipresim .ne. 1) call lw_update(51,'gmode=2')
                  case (3)
                    gmode(i,iyr)=i9                  
                    if(ipresim .ne. 1) call lw_update(49,bas_id(i))
                  case default
                    gmode(i,iyr)=2 
                    if(ipresim .ne. 1) call lw_update(54,bas_id(i))
                end select
              case default !invalid Gmode value, reassign to 2 (mutal ditch operation) available, ignore the value in the file
                gmode(i,iyr)=2
            end select
              if(abs(t4+t5+t6+t7-t_area(i,iyr)).gt.0.00)then
c               IPY and CDS acreage values do not match.
                if((abs((t4+t5+t6+t7-t_area(i,iyr))/t_area(i,iyr))
     &              .gt.0.02) .AND.
     &             (abs(t4+t5+t6+t7-t_area(i,iyr)).GT.1.0)) then
Cjhb=&            ------------------------------------------------------
c                 if the diff is bigger than 2% and larger than 1.0 AF
c                 then it is big enough to write a warning about...
Cjhb=&            ------------------------------------------------------
                  if(ipresim .ne. 1) call lw_update(55,bas_id(i))
                 endif
Cjhb=&          --------------------------------------------------------
c               note that if t_area(i,iyr)=0 then t4,t5,t6,t7 will=0
Cjhb=&          --------------------------------------------------------
                if(abs(t4+t5+t6+t7).lt.0.01)then
c                 put it all in sw flood if nothing is entered
                  t4=t_area(i,iyr)
                  t5=0.0
                  t6=0.0
                  t7=0.0
                else
                  scalefctr=t_area(i,iyr)/(t4+t5+t6+t7)
                  t4=t4*scalefctr
                  t5=t5*scalefctr
                  t6=t6*scalefctr
                  t7=t7*scalefctr
                endif
              endif
c            endif
Cjhb=&==================================================================
Cjhb=&      now determine the four irrigated lands categories
Cjhb=&==================================================================
            swflac(i,iyr)=t4
            swspac(i,iyr)=t5
            swgwflac(i,iyr)=t6
            swgwspac(i,iyr)=t7
C            if(t5 .gt. t4) t4=t5
Cjhb=&==================================================================
Cjhb=&      groundwater percentage = gw acreage / total acreage
Cjhb=&==================================================================
            if(t_area(i,iyr) .ne. 0.0) then
             gper(i,iyr)=(swgwflac(i,iyr)+swgwspac(i,iyr))/t_area(i,iyr)
            else
             gper(i,iyr) = 0.0
            endif
Cjhb=&==================================================================
Cjhb=&      sprinkler percentage = sprinkler acreage / total acreage
Cjhb=&      note this is a NEW use of the sper() variable
Cjhb=&      but necessary since [sw only] now potentially has sprinkler acreage
Cjhb=&==================================================================
c            if(t4 .gt. 0) then
c               sper(i,iyr)=t5/t4
c            else
c               sper(i,iyr) = 0
c            endif
Cjhb=&==================================================================
            if(t_area(i,iyr) .ne. 0.0) then
               sper(i,iyr)=(swspac(i,iyr)+swgwspac(i,iyr))/t_area(i,iyr)
            else
               sper(i,iyr) = 0.0
            endif
Cjhb=&==================================================================
Cjhb=&      assign sfeff(i,iyr) = max application efficiency on sw lands
Cjhb=&      assign gfeff(i,iyr) = max application efficiency on gw lands
Cjhb=&==================================================================
            if(gmode(i,iyr) .eq. 1) then
c             gmode=1 - "maximize water supply" - actually this is a bit of a misnomer
c             the following operation partly maximizes CU (gw first to
c             gw spr acreage) but also partly matches a particular mode of
c             operation on some RG structures (gw last to gw fl acreage)
c             specifically:
c             first use gw as much as possible on sw/gw spr acreage
c             then use sw as needed on sw only acreage (fl and spr) and
c             then to sw/gw fl acreage.
c             last, use gw to meet unmet demand on sw/gw fl acreage
c             max possible sw applic efficiency = assume gw supply fully
c             satisfies demand on sw/gw spr acreage =
c             (sw.fl.acres*fl.eff+sw.spr.acres*spr.eff+sw.gw.fl.acres*fl.eff)/(sw.fl.acres+sw.spr.acres+sw.gw.fl.acres)
c             old code:
c              sfeff(i,iyr)=fleff(i,iyr)
c             new code:
              if(swflac(i,iyr)+swspac(i,iyr)+swgwflac(i,iyr).eq.0.)then
                sfeff(i,iyr)=0.0
              else
                sfeff(i,iyr)=(swflac(i,iyr)*fleff(i,iyr) +
     &                        swspac(i,iyr)*speff(i,iyr)+
     &                        swgwflac(i,iyr)*fleff(i,iyr)) /
     &                       (swflac(i,iyr)+swspac(i,iyr)
     &                        +swgwflac(i,iyr))
              endif
Cjhb          ==========================================================
Cjhb          added max system efficiency for lands served by gw, gfeff
Cjhb          ==========================================================
              if(swgwflac(i,iyr)+swgwspac(i,iyr).eq.0.)then
                gfeff(i,iyr)=0.0
              else
                gfeff(i,iyr)=(swgwflac(i,iyr)*fleff(i,iyr)+
     &                        swgwspac(i,iyr)*speff(i,iyr)) /
     &                       (swgwflac(i,iyr)+swgwspac(i,iyr))
              endif
Cjhb        ============================================================
            elseif(gmode(i,iyr) .eq. 2) then
c             gmode=2 - true mutual ditch operation mode - sw spread equally
c                        to all 4 irrig acreage categories
c             max possible sw applic efficiency = 
c             (sw.fl.acres*fl.eff+sw.spr.acres*spr.eff+sw.gw.fl.acres*fl.eff+sw.gw.spr.acres*spr.eff)/
c               (sw.fl.acres+sw.spr.acres+sw.gw.fl.acres+sw.gw.spr.acres)
c             old code:
c              sfeff(i,iyr)=t3*gper(i,iyr)*sper(i,iyr)+t2*(1-gper(i,iyr)
c     :                   *sper(i,iyr))
c             new code:
              if(swflac(i,iyr)+swspac(i,iyr)
     &           +swgwflac(i,iyr)+swgwspac(i,iyr).eq.0.)then
                sfeff(i,iyr)=0.0
              else
                sfeff(i,iyr)=(swflac(i,iyr)*fleff(i,iyr) +
     &                        swspac(i,iyr)*speff(i,iyr)+
     &                        swgwflac(i,iyr)*fleff(i,iyr)+
     &                        swgwspac(i,iyr)*speff(i,iyr)) /
     &                       (swflac(i,iyr)+swspac(i,iyr)+
     &                        swgwflac(i,iyr)+swgwspac(i,iyr))
              endif
Cjhb          ==========================================================
Cjhb          added max system efficiency for lands served by gw, gfeff
Cjhb          ==========================================================
              if(swgwflac(i,iyr)+swgwspac(i,iyr).eq.0.)then
                gfeff(i,iyr)=0.0
              else
                gfeff(i,iyr)=(swgwflac(i,iyr)*fleff(i,iyr)+
     &                        swgwspac(i,iyr)*speff(i,iyr)) /
     &                       (swgwflac(i,iyr)+swgwspac(i,iyr))
              endif
Cjhb        ============================================================
            elseif(gmode(i,iyr) .eq. 3) then
c             gmode=3 - an alternative mutual ditch mode - another RG operation
c               prorate sw to ALL 4 irrig acreage categories,
c               but use gw first to try and meet the full demand on the
c               sw/gw spr acreage.  then if not needed, use it for recharge
c             max possible sw applic efficiency = assume gw supply fully
c             satisfies demand on sw/gw spr acreage =
c             (sw.fl.acres*fl.eff+sw.spr.acres*spr.eff+sw.gw.fl.acres*fl.eff)/(sw.fl.acres+sw.spr.acres+sw.gw.fl.acres)
c             old code:
c              sfeff(i,iyr)=t3*gper(i,iyr)*sper(i,iyr)+t2*(1-gper(i,iyr)
c     :                   *sper(i,iyr))
c             new code:
              if(swflac(i,iyr)+swspac(i,iyr)+swgwflac(i,iyr).eq.0.)then
                sfeff(i,iyr)=0.0
              else
                sfeff(i,iyr)=(swflac(i,iyr)*fleff(i,iyr) +
     &                        swspac(i,iyr)*speff(i,iyr) +
     &                        swgwflac(i,iyr)*fleff(i,iyr)) /
     &                       (swflac(i,iyr)+swspac(i,iyr)
     &                        +swgwflac(i,iyr))
              endif
Cjhb          ==========================================================
Cjhb          added max system efficiency for lands served by gw, gfeff
Cjhb          ==========================================================
              if(swgwflac(i,iyr)+swgwspac(i,iyr).eq.0.)then
                gfeff(i,iyr)=0.0
              else
                gfeff(i,iyr)=(swgwflac(i,iyr)*fleff(i,iyr)+
     &                        swgwspac(i,iyr)*speff(i,iyr)) /
     &                       (swgwflac(i,iyr)+swgwspac(i,iyr))
              endif
            endif
Cjhb=&==================================================================
Cjhb=&      assign the max pumping rate (AF/month) to all months
Cjhb=&      11/2007 the value is based on 30.4 days per month (accd to the way the hydrobase dmi works)
Cjhb=&      11/2007 so convert to a daily rate and then create a new monthly based on days per month
Cjhb=&==================================================================
            do j=1,12
              mprate(i,iyr,j)=(t8/30.4)*month(j)
            enddo
            select case(isuply)
               case (4)
               if((swgwflac(i,iyr)+swgwspac(i,iyr)) .gt. 0) then    
                  if(t8 .lt. 0.001) then
                    if(ipresim .ne. 1) call lw_update(53,bas_id(i))
                  endif  
               endif   
               case default
            end select   

Cjhb=&==================================================================
Cjhb=&      read the next line
Cjhb=&==================================================================
           goto 30
Cjhb=&==================================================================
          endif
25      continue
Cjhb=&==================================================================
Cjhb=&  read the next line
Cjhb=&==================================================================
       goto 30
Cjhb=&==================================================================
Cjhb=&only get here if no ipy file is found when ISUPLY>0
Cjhb=&(goto statements jump around it)
Cjhb=&==================================================================
42    write(0,*) 'Stop - An irrigation practice file (*.ipy) must be pro
     :vided when ISUPLY is greater than 0'
      write(999,*) 'Stop - An irrigation practice file (*.ipy) must be p
     :rovided when ISUPLY is greater than 0'
      stop
Cjhb=&==================================================================
Cjhb=&check to be sure ALL structures and years were read in the IPY file
Cjhb=&iflag() should = 1 for every combination of str and year if so
Cjhb=&stop if not
Cjhb=&==================================================================
32    do i=1,nbasin
          do j=1,nyrs
            if(iflag(i,j) .eq. 0) then
              twdid=bas_id(i)
              aspid=twdid(1:12)
              itmp2=j+nyr1-1
            write(0,*) 'Stop-no irrigation parameters found for structur
     :e ',aspid, 'for year ',itmp2 
            write(999,*) 'Stop-no irrigation parameters found for struct
     :ure ',aspid, 'for year ',itmp2 
                stop
            endif
          enddo
       enddo
Cjhb=&==================================================================
Cjhb=&close the IPY file
Cjhb=&==================================================================
      close (150)
Cjhb=&==================================================================
Cjhb=&original comment - but not related to the code here:
c if all of the structure have % groundwater, do not consider
c if any of the structure have groundwater, do not consider water rights
c or return flow timing or patterns
Cjhb=&==================================================================
Cjhb=&if the PVH file exists, read it, else jump to the drafile reading code
Cjhb=&==================================================================
      if(pvhfile.eq.'') goto 43
      open (unit=180,file=pvhfile,status='old',err=43)
      call skipn(180) !skip comment records
Cjhb=&==================================================================
c check to see if gw data exists for all years
Cjhb=&==================================================================
      read(180,29) jyr1, jyr2, idum3 !read first record
      if(idum3 .eq. 'WYR') THEN 
        jyr1=jyr1+1
        jyr2=jyr2-1
      endif
29    format(6x,i4,11x,i4,7x,a3)
      if((nyr1 .lt. jyr1) .or. (nyr2 .gt. jyr2)) then
        write(0,*)'Stop.',
     :  'Monthly pumping capacity data not available for all years'
        write(999,*)'Stop.',
     :  'Monthly pumping capacity data not available for all years'
        stop
      endif
Cjhb=&==================================================================
Cjhb=& now read the PVH file pumping records and
Cjhb=& reset the max pumping rate array
Cjhb=& (note it adjusts for water year)
Cjhb=&==================================================================
33    read (180,21, end=40) itmp,pvhid,(p1(k),k=1,12)
21    format(i4,1x,a12,12f8.0)
      if(idum3 .eq. 'WYR' .and. itmp .lt. (nyr1+1)) goto 33
      if(itmp .lt. nyr1) goto 33
      if(itmp .gt. nyr2) goto 40
      iyr=itmp-nyr1+1
      do 35 i=1,nbasin
        twdid=bas_id(i)
        if(twdid(1:12) .eq. pvhid) then
          iflag2(i) = 1
          do 22 k=1,12
           if(idum3 .eq. 'WYR') then
             if(k .gt. 3) then
               mprate(i,iyr,k-3)=p1(k)
             else
               if(iyr .eq. 1) goto 33
               mprate(i,iyr-1,k+9)=p1(k)
             endif
           else
             mprate(i,iyr,k)=p1(k)
           endif
22        continue
          goto 33
        endif
35    continue
      goto 33
40    close(180)
43    continue
Cjhb=&==================================================================
c ew 03/12/04 - open and read drain/tailwater supply file
c               (here to 60)
Cjhb=&==================================================================
      if(idrain .ge. 1) then
        if(drafile.eq.'') goto 44
        write(999,*) 'Reading in drain/tailwater file ', drafile
        open (unit=190,file=drafile,status='old',err=44)
        call skipn(190)
Cjhb=&==================================================================
c check to see if drain/tailwater supply data exists for all years
Cjhb=&==================================================================
        read(190,29) jyr1, jyr2, idum3
        if(idum3 .eq. 'WYR') THEN 
          jyr1=jyr1+1
          jyr2=jyr2-1
        endif
        if((nyr1 .lt. jyr1) .or. (nyr2 .gt. jyr2)) then
          write(0,*) 'Stop.',
     &'Monthly drain/tailwater supply data not available for all years.'
          write(999,*) 'Stop.',
     &'Monthly drain/tailwater supply data not available for all years.'
          stop
        endif
Cjhb=&==================================================================
Cjhb=& now read the DRA file records and
Cjhb=& set the tail array
Cjhb=& (note it adjusts for water year)
Cjhb=&==================================================================
63      read (190,21, end=44) itmp,pvhid,(p1(k),k=1,12)
        if(idum3 .eq. 'WYR' .and. itmp .lt. (nyr1+1)) goto 63
        if(itmp .lt. nyr1) goto 63
        if(itmp .gt. nyr2) goto 44
        iyr=itmp-nyr1+1
        ifound=0
        do 65 i=1,nbasin
          twdid=bas_id(i)
          if(twdid(1:12) .eq. pvhid) then
            ifound=1
            itail(i)=1
            do 62 k=1,12
              select case (idrain)
              case (2) !leave negative values alone
              case default !otherwise change them to 0
                p1(k)=max(p1(k),0.0)
              end select
              if(idum3 .eq. 'WYR') then
Cjhb=&==================================================================
crrb 2004/05/05; Allow multiple entries for the same ID & year             
Cjhb=&==================================================================
                if(k .gt. 3) then
                  tail(i,iyr,k-3)=tail(i,iyr,k-3)+ p1(k)
                else
                  if(iyr .eq. 1) goto 63
                  tail(i,iyr-1,k+9)=tail(i,iyr-1,k+9)+p1(k)                 
                endif
              else
                tail(i,iyr,k)=tail(i,iyr,k) + p1(k)
              endif
62          continue
            goto 63
          endif
65      continue
Cjhb=&==================================================================
c rrb 2004/05/05; Warn if station is not found
Cjhb=&==================================================================
        if(ifound.eq.0) then
            if(ipresim .ne. 1) call lw_update(56,pvhid)
        endif  
        goto 63
      endif
Cjhb=&==================================================================
c  open up output files (.dwb detailed water budget by structure)
c                       (.swb scenario water budget)
Cjhb=&==================================================================
44    thefile1 = dfile
      thefile1(fn_len:fn_len+4) = '.dwb'
c     close the drain file      
      close(190)

Cjhb=&==================================================================
Cjhb  open binary file 1
      IF (LBD1OUT.and.(ipresim.ne.1)) THEN
Cjhb     call the routine to initialize the bd1 binary file output
Cjhb     call CreateBD1FileHeader(dfile)

Cjhb    add file name extension
        BD1FN=dfile
        NCH=len_trim(BD1FN)
        BD1FN(NCH+1:NCH+5) = BD1EXT
Cjhb    open file in binary mode for output
        OPEN (UNIT=IBD1UN,FILE=BD1FN,STATUS='REPLACE',IOSTAT=IERR,
c     &        ACCESS='TRANSPARENT',FORM='BINARY')
c     &         ACCESS='SEQUENTIAL',FORM='BINARY')
     &         ACCESS='STREAM')
      ENDIF
Cjhb=&==================================================================
Cjhb  open binary file 2
      IF (LBD2OUT) THEN
Cjhb     call the routine to initialize the bd2 binary file output
Cjhb     call CreateBD2FileHeader(dfile)
      ENDIF
Cjhb=&==================================================================
Cjhb  open binary file 3
      IF (LBD3OUT) THEN
Cjhb     call the routine to initialize the bd3 binary file output
Cjhb     call CreateBD3FileHeader(dfile)
      ENDIF
Cjhb=&==================================================================
Cjhb  open binary file 4
      IF (LBD4OUT) THEN
Cjhb     call the routine to initialize the bd4 binary file output
Cjhb     call CreateBD4FileHeader(dfile)
      ENDIF
Cjhb=&==================================================================
Cjhb  open binary file 5
      IF (LBD5OUT) THEN
Cjhb     call the routine to initialize the bd5 binary file output
Cjhb     call CreateBD5FileHeader(dfile)
      ENDIF
Cjhb=&==================================================================


      IF (LBD1OUT.and.(ipresim.ne.1)) THEN
Cjhb=&==================================================================
Cjhb  Description of this BD1 binary file:
Cjhb  This binary file contains structure data - both monthly time series and non-time series
Cjhb  Header records are at the top of the file.
Cjhb  Then one record for every structure containing multiple fields of non time series data.
Cjhb  Then one record for each structure/time step combination containing 
Cjhb  multiple fields on each record containing the data
Cjhb  that (potentially) change from structure/timestep pair to structure/timestep pair.
Cjhb=&==================================================================
Cjhb  Binary File Overview
Cjhb  The initial records describe the organization of the monthly time series structure data.
Cjhb  These binary files are created with the FORTRAN FORM='BINARY' specification,
Cjhb    which means the data are written to the file in binary form in one long stream.
Cjhb    This accomplishes two objectives:
Cjhb    1. Allows the existing VB6 StateCUI code to flexibly read the binary file
Cjhb       one value at at time with GET() statements without having to "hardwire"
Cjhb       a fixed record structure that would cause the GUI (VB6) code to "break"
Cjhb       if the FORTRAN output changed.
Cjhb       So new values can be added to the binary output files in the FORTRAN code
Cjhb       and the VB6 GUI will automatically accomodate them without any changes.
Cjhb       In addition, this allows the BD1 file to be different for each scenario
Cjhb       option (ISUPLY, IFLOOD, etc) without creating a nightmare for the GUI (VB6) code
Cjhb       attempting to read the file.
Cjhb    2. Prevents having to set a fixed record size large enough for the long header
Cjhb       records which creates unused space on the many data records that follow,
Cjhb       making the file much larger than necessary containing empty space.
Cjhb  The first BD1 (monthly time series structure data) record has the format of 5 integers:
Cjhb      NBASIN-Num Structures (I), IBD1TS-Num Timesteps (I),
Cjhb      IBD1NSF-Num Structure Fields, IBD1NF-Num TimeStep Fields,
Cjhb      IBD1TSA-Num Annual Time Steps (I) 1=>annual, 12=>monthly, 52=>weekly, 365=>daily
Cjhb      The resulting number of structure data rows in the file is NBASIN
Cjhb      The resulting number of time series data rows in the file, NR, can
Cjhb        therefore be calculated as: NR-NRows (I) = NBASIN * IBD1TS
Cjhb        where IBD1TS = IBD1TSA * NYRS
Cjhb      The begin/end years can be determined from the
Cjhb        values in the header record #1 and the first time series data row.
Cjhb      The list of structures follows header record number 1.
Cjhb      This design allows the same binary file organization to be used for:
Cjhb        monthly times series for structures,
Cjhb        monthly times series for stations,
Cjhb        daily time series for structures,
Cjhb        daily time series for stations,
Cjhb        other misc time series;
Cjhb        it also allows time series to have variable start and end dates
Cjhb        (i.e. do not have to begin on Jan 1 and end on Dec 31)
Cjhb  After the first record are IBD1NSF records (one record per field) with data describing
Cjhb      the fields on the structure data records.  The format for these header records is:
Cjhb      Field Type (Character*1:'I','F','C'), Field Length in Bytes (Integer*4),
Cjhb      Field Name (Character*24), Field In Report (Integer*4:0=false, non-zero=true),
Cjhb      Field Report Header (Character*60)
Cjhb  After these records are IBD1NF records (one record per field) with data describing
Cjhb      the fields on the time series data records.  The format for these header records is:
Cjhb      Field Type (Character*1:'I','F','C'), Field Length in Bytes (Integer*4),
Cjhb      Field Name (Character*24), Field In Report (Integer*4:0=false, non-zero=true),
Cjhb      Field Report Header (Character*60)
Cjhb  Then are the NBASIN records containing the structure data
Cjhb      with the fields described above.
Cjhb  Then are the NBASIN * IBD1TS records containing the monthly time series structure data
Cjhb      with the fields described above, with the entire time series for structure 1,
Cjhb      then the entire time series for structure 2, etc.
Cjhb  The number of fields varies depending on the model run scenario options
Cjhb
Cjhb=&==================================================================
Cjhb     write BD1 header records
Cjhb=================***************====================================
         IF(ISUPLY .EQ. 1) THEN
           IBD1TSA=12 !# time steps per year, assumes this is monthly
           IBD1TS=IBD1TSA*NYRS !total # time steps
           IBD1NF=29 !number output values per timestep per structure
           IBD1NSF=3 !number nontime series value output per structure
           if(sboutput) then
           WRITE(UNIT=IBD1UN)NBASIN+SBCOUNT+1+1,
     &                       IBD1TS,IBD1NSF,IBD1NF,IBD1TSA
           else
           WRITE(UNIT=IBD1UN)NBASIN,
     &                       IBD1TS,IBD1NSF,IBD1NF,IBD1TSA
           endif
Cjhb=&==================================================================
Cjhb       Here are the IBD1NSF (3) values on the STRUCTURE records for ISUPLY=1
Cjhb=======1============================================================
Cjhb       Integer Structure Index, I
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Structure Index         ',0,
     &    'Structure Index                                             '
Cjhb=&=====2============================================================
Cjhb       Char*12 Structure ID string, BAS_ID(L)(1:12) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',12,
     &    'Structure ID            ',1,
     &    'Structure ID                                                '
Cjhb=&=====3============================================================
Cjhb       Char*12 Structure ID name, BAS_ID(L)(13:24) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',12,
     &    'Structure Name          ',1,
     &    'Structure Name                                              '
Cjhb=&==================================================================
Cjhb       Total record size = 4+12+12 = 28 bytes
Cjhb=&==================================================================
Cjhb
Cjhb=&==================================================================
Cjhb       Here are the IBD1NF (29) values on the time series records for ISUPLY=1
Cjhb=======1============================================================
Cjhb       Integer Structure Index, I
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Structure Index         ',0,
     &    '          '
Cjhb=&=====2============================================================
Cjhb       Integer Year, NYR1+M-1
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Year                    ',0,
     &    '          '
Cjhb=&=====3============================================================
Cjhb       Integer Month Index, L
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Month Index             ',0,
     &    '          '
Cjhb=&=====4============================================================
Cjhb       Char*3 abbr month string, AMN(L) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',3,
     &    'Month Name              ',1,
     &    '          '
Cjhb=&=====5============================================================
Cjhb       Real*4 Total Crop Acreage - t_area(i,m)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Irrigated Acreage ',1,
     &    'ACRE      '
Cjhb=&=====6============================================================
Cjhb       Real*4 Modeled Crop Acreage - m_area(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Modeled Irrig Acreage   ',0,
     &    'ACRE      '
Cjhb=&=====7============================================================
Cjhb       Char*10 Analysis method - "Calculated" vs. "Prorated" - METHOD
           WRITE(UNIT=IBD1UN)'C',10,
     &    'Analysis Method         ',1,
     &    '          '
Cjhb=&=====8============================================================
Cjhb       Real*4 Potential Crop ET  , ettot(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Potential Crop ET       ',1,
     &    'ACFT      '
Cjhb=&=====9============================================================
Cjhb       Real*4  Effective Precip, effppt(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Effective Precip        ',1,
     &    'ACFT      '
Cjhb=&=====10============================================================
Cjhb       Real*4 Irrigation Water Requirement IWR, reqt(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Irrigation Water Reqt   ',1,
     &    'ACFT      '
Cjhb=&=====11===========================================================
Cjhb       Real*4 EOM Winter Precip Carryover, wbu(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Wint Prec Soil Content  ',1,
     &    'ACFT      '
Cjhb=&=====12===========================================================
Cjhb       Real*4 IWR After Winter Precip, reqreqts(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'IWR After Winter Precip ',1,
     &    'ACFT      '
Cjhb=&=====13===========================================================
Cjhb       Real*4 River Diversion Acct. - Historic Diversion, ddhmonot(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'River Diversion         ',1,
     &    'ACFT      '
Cjhb=&=====14===========================================================
Cjhb       Real*4 River Diversion Acct. - conveyance Efficiency, ceff(i,m)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Conveyance Efficiency   ',0,
     &    'PERCENT   '
Cjhb=&=====15===========================================================
Cjhb       Real*4 River Diversion Acct. - conveyance Efficiency, closs(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Conveyance Loss         ',0,
     &    'ACFT      '
Cjhb=&=====16===========================================================
Cjhb       Real*4 River Diversion Acct. - Farm Headgate Diversion, fdiv(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Farm Headgate Delivery  ',0,
     &    'ACFT      '
Cjhb=&=====17===========================================================
Cjhb       Real*4 Tail water, tail(i,m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Supply_Tail Water_Drains',1,
     &    'ACFT      '
C removed Cjhb=&=====16===========================================================
C removed Cjhb       Real*4 River Diversion Acct. - Sprinkler FHG (Not Applied), arech(m,l)
C removed            WRITE(UNIT=IBD1UN)'R',4,
C removed      &    'Sprinkler FHG (Not Appl)',0,
C removed      &    'ACFT      '
Cjhb=&=====18===========================================================
Cjhb       Real*4 River Diversion Acct. - SW to CU                , crop_cut(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to CU                ',1,
     &    'ACFT      '
Cjhb=&=====19===========================================================
Cjhb       Real*4 River Diversion Acct. - SW to Soil              , soil_cu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to Soil              ',1,
     &    'ACFT      '
Cjhb=&=====21===========================================================
Cjhb       Real*4 River Diversion Acct. - SW to CU & Soil         , divcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to CU & Soil         ',1,
     &    'ACFT      '
Cjhb=&=====21===========================================================
Cjhb       Real*4 River Diversion Acct. - SW_Non_Consumed         , ulagt(m,l)-closs(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW_Non_Consumed         ',1,
     &    'ACFT      '
Cjhb=&=====22===========================================================
Cjhb       Real*4 River Diversion Acct. - Max Application Effic   , sfeff(i,m)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Max Application Effic   ',0,
     &    'PERCENT   '
Cjhb=&=====23===========================================================
Cjhb       Real*4 River Diversion Acct. - Calc SW Applic Effic    , effcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Calc SW Applic Effic    ',0,
     &    'PERCENT   '
Cjhb=&=====24===========================================================
Cjhb       Real*4 River Diversion Acct. - Calc SW System Effic    , seffcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Calc SW System Effic    ',1,
     &    'PERCENT   '
Cjhb=&=====25===========================================================
Cjhb       Real*4 Soil Moisture Contents, soiltott(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW Soil Content         ',1,
     &    'ACFT      '
Cjhb=&=====26===========================================================
Cjhb       Real*4 Crop CU from SW         , crop_cut(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Crop CU from SW         ',1,
     &    'ACFT      '
Cjhb=&=====27===========================================================
Cjhb       Real*4 Crop CU from Soil       , cropcusoil(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Crop CU from Soil       ',1,
     &    'ACFT      '
Cjhb=&=====28===========================================================
Cjhb       Real*4 Total Crop CU           , estcrpt(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Crop CU           ',1,
     &    'ACFT      '
Cjhb=&=====29===========================================================
Cjhb       Real*4 CU Shortage             , SHORTAGE
Cjhb       = IWR reqreqts(m,l) - SWCU estcrpt(i,m,l) - GWCU gwcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'CU Shortage             ',0,
     &    'ACFT      '
Cjhb=&==================================================================
Cjhb       Total record size = 3*4+3+4+4+10+22*4 = 121 bytes
Cjhb=&==================================================================
Cjhb
Cjhb=&==================================================================
Cjhb       Now write the NSTR (NBASIN) structure data records for ISUPLY=1
Cjhb       Note: assumes these arrays are already populated, so output them now...
Cjhb====================================================================
            DO I=1,NBASIN
              CHAR12_1=BAS_ID(I)(1:12)
              CHAR12_2=BAS_ID(I)(13:24)
              WRITE(UNIT=IBD1UN)I,CHAR12_1,CHAR12_2
Cjhb          WRITE(*,*)I,CHAR12_1,CHAR12_2
            END DO
           if(sboutput) then
            DO I=0,SBCOUNT
                CHAR12_1=SBID(I)
                CHAR12_2=SBNAME(I)
                WRITE(UNIT=IBD1UN)NBASIN+I+1,CHAR12_1,CHAR12_2
            END DO
            CHAR12_1=BID
            CHAR12_2=BNAME
            WRITE(UNIT=IBD1UN)NBASIN+SBCOUNT+1+1,CHAR12_1,CHAR12_2
           endif
Cjhb=&==================================================================
Cjhb       ISUPLY=2 has a different set of output fields for the time series data
Cjhb====================================================================
         ELSEIF(ISUPLY .EQ. 2) THEN
           IBD1TSA=12 !# time steps per year, assumes this is monthly
           IBD1TS=IBD1TSA*NYRS !total # time steps
           IBD1NF=48 !number output values per timestep per structure
           IBD1NSF=3 !number nontime series value output per structure
           if(sboutput) then
           WRITE(UNIT=IBD1UN)NBASIN+SBCOUNT+1+1,
     &                       IBD1TS,IBD1NSF,IBD1NF,IBD1TSA
           else
           WRITE(UNIT=IBD1UN)NBASIN,
     &                       IBD1TS,IBD1NSF,IBD1NF,IBD1TSA
           endif
Cjhb=&==================================================================
Cjhb       Here are the IBD1NSF (3) values on the STRUCTURE records for ISUPLY=2
Cjhb=======1============================================================
Cjhb       Integer Structure Index, I
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Structure Index         ',0,
     &    'Structure Index                                             '
Cjhb=&=====2============================================================
Cjhb       Char*12 Structure ID string, BAS_ID(L)(1:12) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',12,
     &    'Structure ID            ',1,
     &    'Structure ID                                                '
Cjhb=&=====3============================================================
Cjhb       Char*12 Structure ID name, BAS_ID(L)(13:24) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',12,
     &    'Structure Name          ',1,
     &    'Structure Name                                              '
Cjhb=&==================================================================
Cjhb       Total record size = 4+12+12 = 28 bytes
Cjhb=&==================================================================
Cjhb=&==================================================================
Cjhb       Here are the IBD1NF (48) values on the time series records for ISUPLY=2
Cjhb=======1============================================================
Cjhb       Integer Structure Index, I
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Structure Index         ',0,
     &    '          '
Cjhb=&=====2============================================================
Cjhb       Integer Year, NYR1+M-1
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Year                    ',0,
     &    '          '
Cjhb=&=====3============================================================
Cjhb       Integer Month Index, L
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Month Index             ',0,
     &    '          '
Cjhb=&=====4============================================================
Cjhb       Char*3 abbr month string, AMN(L) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',3,
     &    'Month Name              ',1,
     &    '          '
Cjhb=&=====5============================================================
Cjhb       Real*4 Total Irrigated Acreage  - t_area(i,m)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Irrigated Acreage ',1,
     &    'ACRE      '
Cjhb=&=====6============================================================
Cjhb       Real*4 Modeled Crop Acreage - m_area(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Modeled Irrig Acreage   ',0,
     &    'ACRE      '
Cjhb=&=====7============================================================
Cjhb       Char*10 Analysis Method         - "Calculated" vs. "Prorated" - METHOD
           WRITE(UNIT=IBD1UN)'C',10,
     &    'Analysis Method         ',1,
     &    '          '
Cjhb=&=====8============================================================
Cjhb       Real*4 Potential Crop ET  , ettot(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Potential Crop ET       ',1,
     &    'ACFT      '
Cjhb=&=====9============================================================
Cjhb       Real*4 Effective Precip, effppt(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Effective Precip        ',1,
     &    'ACFT      '
Cjhb=&=====10===========================================================
Cjhb       Real*4 Irrigation Water Requirement IWR, reqt(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Irrigation Water Reqt   ',1,
     &    'ACFT      '
Cjhb=&=====11===========================================================
Cjhb       Real*4 EOM Winter Precip Carryover, wbu(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Wint Prec Soil Content  ',1,
     &    'ACFT      '
Cjhb=&=====12===========================================================
Cjhb       Real*4 IWR After Winter Precip, reqreqts(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'IWR After Winter Precip ',1,
     &    'ACFT      '
Cjhb=&=====13===========================================================
Cjhb       Real*4 River Diversion Acct - Div By Priority - Senior, seniorf(m,l), ***REPORT*** - 12/19/08 replaced with holdps - 01/19/11 changed back to seniorf(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'River Diversion_Senior  ',1,
     &    'ACFT      '
Cjhb=&=====14===========================================================
Cjhb       Real*4 River Diversion Acct - Div By Priority - Junior, juniorf(m,l), ***REPORT*** - 12/19/08 replaced with holdpj - 01/19/11 changed back to juniorf(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'River Diversion_Junior  ',1,
     &    'ACFT      '
Cjhb=&=====15===========================================================
Cjhb       Real*4 River Diversion Acct - Div By Priority - other, otherf(m,l), ***REPORT*** - 12/19/08 replaced with holdpo - 01/19/11 changed back to otherf(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'River Diversion_Other   ',1,
     &    'ACFT      '
Cjhb=&=====16===========================================================
Cjhb       Real*4 River Diversion Acct - Div By Priority - Total, ddhmonot(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'River Diversion_Total   ',1,
     &    'ACFT      '
Cjhb=&=====17===========================================================
Cjhb       Real*4 Conveyance Efficiency   , ceff(i,m)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Conveyance Efficiency   ',0,
     &    'PERCENT   '
Cjhb=&=====18===========================================================
Cjhb       Real*4 Conveyance Loss         , closs(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Conveyance Loss         ',0,
     &    'ACFT      '
Cjhb=&=====19===========================================================
Cjhb       Real*4 Farm Headgate Delivery  , fdiv(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Farm Headgate Delivery  ',0,
     &    'ACFT      '
Cjhb=&=====20===========================================================
Cjhb       Real*4 River Diversion Acct. - Diversion to CU - Senior, crop_cus(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to CU_Senior         ',1,
     &    'ACFT      '
Cjhb=&=====21===========================================================
Cjhb       Real*4 River Diversion Acct. - Diversion to CU - Junior, crop_cuj(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to CU_Junior         ',1,
     &    'ACFT      '
Cjhb=&=====22===========================================================
Cjhb       Real*4 River Diversion Acct. - Diversion to CU - Other, crop_cuo(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to CU_Other          ',1,
     &    'ACFT      '
Cjhb=&=====23===========================================================
Cjhb       Real*4 River Diversion Acct. - Diversion to CU - Total, crop_cut(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to CU_Total          ',1,
     &    'ACFT      '
Cjhb=&=====24===========================================================
Cjhb       Real*4 River Diversion Acct. - Add to Soil Moisture - Senior, soil_cus(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to Soil_Senior       ',1,
     &    'ACFT      '
Cjhb=&=====25===========================================================
Cjhb       Real*4 River Diversion Acct. - Add to Soil Moisture - Junior, soil_cuj(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to Soil_Junior       ',1,
     &    'ACFT      '
Cjhb=&=====26===========================================================
Cjhb       Real*4 River Diversion Acct. - Add to Soil Moisture - Other, soil_cuo(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to Soil_Other        ',1,
     &    'ACFT      '
Cjhb=&=====27===========================================================
Cjhb       Real*4 River Diversion Acct. - Add to Soil Moisture - Total, soil_cu(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to Soil_Total        ',1,
     &    'ACFT      '
Cjhb=&=====28===========================================================
Cjhb       Real*4 River Diversion Acct. - Total Div to CU & Soil  , divcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to CU & Soil_Total   ',1,
     &    'ACFT      '
Cjhb=&=====29===========================================================
Cjhb       Real*4 River Diversion Acct. - Non-Consumed - Senior, ulags(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW_Non_Consumed_Senior  ',1,
     &    'ACFT      '
Cjhb=&=====30===========================================================
Cjhb       Real*4 River Diversion Acct. - Non-Consumed - Junior, ulagj(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW_Non_Consumed_Junior  ',1,
     &    'ACFT      '
Cjhb=&=====31===========================================================
Cjhb       Real*4 River Diversion Acct. - Non-Consumed - Other, ulago(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW_Non_Consumed_Other   ',1,
     &    'ACFT      '
Cjhb=&=====32===========================================================
Cjhb       Real*4 River Diversion Acct. - Non-Consumed - Total, ulagt(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW_Non_Consumed_Total   ',1,
     &    'ACFT      '
Cjhb=&=====33===========================================================
Cjhb       Real*4 Tail water, tail(i,m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Supply_Tail Water_Drains',1,
     &    'ACFT      '
C removed Cjhb=&=====31===========================================================
C removed Cjhb       Real*4 River Diversion Acct. - Sprinkler FHG (Not Applied), arech(m,l)
C removed            WRITE(UNIT=IBD1UN)'R',4,
C removed      &    'Sprinkler FHG (Not Appl)',0,
C removed      &    'ACFT      '
Cjhb=&=====34===========================================================
Cjhb       Real*4 River Diversion Acct. - Max Applic Effic, sfeff(i,m)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Max Application Effic   ',0,
     &    'PERCENT   '
Cjhb=&=====35===========================================================
Cjhb       Real*4 River Diversion Acct. - Calc Surface Water Applic Effic (%), effcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Calc SW Applic Effic    ',0,
     &    'PERCENT   '
Cjhb=&=====36===========================================================
Cjhb       Real*4 River Diversion Acct. - Calc SW System Effic    , seffcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Calc SW System Effic    ',1,
     &    'PERCENT   '
Cjhb=&=====37===========================================================
Cjhb       Real*4 EOM Soil Moisture Contents - Senior, soiltotts(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW Soil Content_Senior  ',1,
     &    'ACFT      '
Cjhb=&=====38===========================================================
Cjhb       Real*4 EOM Soil Moisture Contents - Junior, soiltottj(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW Soil Content_Junior  ',1,
     &    'ACFT      '
Cjhb=&=====39===========================================================
Cjhb       Real*4 EOM Soil Moisture Contents - Other, soiltotto(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW Soil Content_Other   ',1,
     &    'ACFT      '
Cjhb=&=====40===========================================================
Cjhb       Real*4 EOM Soil Moisture Contents - Total, soiltott(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW Soil Content_Total   ',1,
     &    'ACFT      '
Cjhb=&=====41===========================================================
Cjhb       Real*4 Tot Crop CU from Div    , crop_cut(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Crop CU from SW         ',1,
     &    'ACFT      '
Cjhb=&=====42===========================================================
Cjhb       Real*4 Tot Crop CU from Soil   , cropcusoil(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Crop CU from Soil       ',1,
     &    'ACFT      '
Cjhb=&=====43===========================================================
Cjhb       Real*4 Estimated Crop CU - By Water Rights - Senior, estcrps(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Crop CU_Senior    ',1,
     &    'ACFT      '
Cjhb=&=====44===========================================================
Cjhb       Real*4 Estimated Crop CU - By Water Rights - Junior, estcrpj(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Crop CU_Junior    ',1,
     &    'ACFT      '
Cjhb=&=====45===========================================================
Cjhb       Real*4 Estimated Crop CU - By Water Rights - Other, estcrpo(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Crop CU_Other     ',1,
     &    'ACFT      '
Cjhb=&=====46===========================================================
Cjhb       Real*4 Estimated Crop CU - By Water Rights - Total, estcrpt(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Crop CU           ',1,
     &    'ACFT      '
Cjhb=&=====47===========================================================
Cjhb       Real*4 Replacement Requirement, estcrpj(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Replacement Requirement ',1,
     &    'ACFT      '
Cjhb=&=====48===========================================================
Cjhb       Real*4 Calculated CU shortage, SHORTAGE
Cjhb       = IWR reqreqts(m,l) - SWCU estcrpt(i,m,l) - GWCU gwcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'CU Shortage             ',0,
     &    'ACFT      '
Cjhb=&==================================================================
Cjhb       Total record size = 3*4+3+4+4+10+41*4 = 197 bytes
Cjhb=&==================================================================
Cjhb
Cjhb=&==================================================================
Cjhb       Now write the NSTR (NBASIN) structure data records for ISUPLY=2
Cjhb       Note: assumes these arrays are already populated, so output them now...
Cjhb=&==================================================================
            DO I=1,NBASIN
              CHAR12_1=BAS_ID(I)(1:12)
              CHAR12_2=BAS_ID(I)(13:24)
              WRITE(UNIT=IBD1UN)I,CHAR12_1,CHAR12_2
Cjhb          WRITE(*,*)I,CHAR12_1,CHAR12_2
            END DO
           if(sboutput) then
            DO I=0,SBCOUNT
                CHAR12_1=SBID(I)
                CHAR12_2=SBNAME(I)
                WRITE(UNIT=IBD1UN)NBASIN+I+1,CHAR12_1,CHAR12_2
            END DO
            CHAR12_1=BID
            CHAR12_2=BNAME
            WRITE(UNIT=IBD1UN)NBASIN+SBCOUNT+1+1,CHAR12_1,CHAR12_2
           endif
Cjhb=&==================================================================
         ELSEIF(ISUPLY .EQ. 3) THEN
Cjhb=&==================================================================
           IBD1TSA=12 !# time steps per year
           IBD1TS=IBD1TSA*NYRS !total # time steps
           IBD1NF=61 !number output values per timestep per structure
           IBD1NSF=3 !number nontime series value output per structure
           if(sboutput) then
           WRITE(UNIT=IBD1UN)NBASIN+SBCOUNT+1+1,
     &                       IBD1TS,IBD1NSF,IBD1NF,IBD1TSA
           else
           WRITE(UNIT=IBD1UN)NBASIN,
     &                       IBD1TS,IBD1NSF,IBD1NF,IBD1TSA
           endif
Cjhb=&==================================================================
Cjhb       Here are the IBD1NSF (3) values on the STRUCTURE records for ISUPLY=3
Cjhb=======1============================================================
Cjhb       Integer Structure Index, I
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Structure Index         ',0,
     &    'Structure Index                                             '
Cjhb=&=====2============================================================
Cjhb       Char*12 Structure ID string, BAS_ID(L)(1:12) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',12,
     &    'Structure ID            ',1,
     &    'Structure ID                                                '
Cjhb=&=====3============================================================
Cjhb       Char*12 Structure ID name, BAS_ID(L)(13:24) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',12,
     &    'Structure Name          ',1,
     &    'Structure Name                                              '
Cjhb=&==================================================================
Cjhb       Total record size = 4+12+12 = 28 bytes
Cjhb=&==================================================================
Cjhb=&==================================================================
Cjhb       Here are the IBD1NF (60) values on the time series records for ISUPLY=3
Cjhb=======1============================================================
Cjhb       Integer Structure Index, I
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Structure Index         ',0,
     &    '          '
Cjhb=&=====2============================================================
Cjhb       Integer Year, NYR1+M-1
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Year                    ',0,
     &    '          '
Cjhb=&=====3============================================================
Cjhb       Integer Month Index, L
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Month Index             ',0,
     &    '          '
Cjhb=&=====4============================================================
Cjhb       Char*3 abbr month string, AMN(L) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',3,
     &    'Month Name              ',1,
     &    '          '
Cjhb=&=====5============================================================
Cjhb       Real*4 Total Crop Acreage - t_area(i,m)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Irrigated Acreage ',1,
     &    'ACRE      '
Cjhb=&=====6============================================================
Cjhb       Real*4 Modeled Crop Acreage - m_area(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Modeled Irrig Acreage   ',0,
     &    'ACRE      '
Cjhb=&=====7============================================================
Cjhb       Char*10 Analysis method - "Calculated" vs. "Prorated" - METHOD
           WRITE(UNIT=IBD1UN)'C',10,
     &    'Analysis Method         ',1,
     &    '          '
Cjhb=&=====8============================================================
Cjhb       Real*4 Potential Crop ET  , ettot(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Potential Crop ET       ',1,
     &    'ACFT      '
Cjhb=&=====9============================================================
Cjhb       Real*4 Effective Precip, effppt(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Effective Precip        ',1,
     &    'ACFT      '
Cjhb=&=====10===========================================================
Cjhb       Real*4 Irrigation Water Requirement IWR, reqt(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Irrigation Water Reqt   ',1,
     &    'ACFT      '
Cjhb=&=====11===========================================================
Cjhb       Real*4 EOM Winter Precip Carryover, wbu(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Wint Prec Soil Content  ',1,
     &    'ACFT      '
Cjhb=&=====12===========================================================
Cjhb       Real*4 IWR After Winter Precip, reqreqts(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'IWR After Winter Precip ',1,
     &    'ACFT      '
Cjhb=&=====13===========================================================
Cjhb       Real*4 River Diversion Acct - Div By Priority - Senior, seniorf(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'River Diversion - Senior',1,
     &    'ACFT      '
Cjhb=&=====14===========================================================
Cjhb       Real*4 River Diversion Acct - Div By Priority - Junior, juniorf(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'River Diversion - Junior',1,
     &    'ACFT      '
Cjhb=&=====15===========================================================
Cjhb       Real*4 River Diversion Acct - Div By Priority - other, otherf(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'River Diversion - Other ',1,
     &    'ACFT      '
Cjhb=&=====16===========================================================
Cjhb       Real*4 River Diversion Acct - Div By Priority - Total, ddhmonot(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'River Diversion - Total ',1,
     &    'ACFT      '
Cjhb=&=====17===========================================================
Cjhb       Real*4 River Diversion Acct. - Conveyance Efficiency   , ceff(i,m)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Conveyance Efficiency   ',0,
     &    'PERCENT   '
Cjhb=&=====18===========================================================
Cjhb       Real*4 River Diversion Acct. - Conveyance Loss         , closs(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Conveyance Loss         ',0,
     &    'ACFT      '
Cjhb=&=====19===========================================================
Cjhb       Real*4 River Diversion Acct. - Farm Headgate Delivery  , fdiv(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Farm Headgate Delivery  ',0,
     &    'ACFT      '
Cjhb=&=====20===========================================================
Cjhb       Real*4 River Diversion Acct. - Diversion to CU - Senior, crop_cus(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Diversion to CU - Senior',1,
     &    'ACFT      '
Cjhb=&=====21===========================================================
Cjhb       Real*4 River Diversion Acct. - Diversion to CU - Junior, crop_cuj(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Diversion to CU - Junior',1,
     &    'ACFT      '
Cjhb=&=====22===========================================================
Cjhb       Real*4 River Diversion Acct. - Diversion to CU - Other, crop_cuo(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Diversion to CU - Other ',1,
     &    'ACFT      '
Cjhb=&=====23===========================================================
Cjhb       Real*4 River Diversion Acct. - Diversion to CU - Total, crop_cut(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Diversion to CU - Total ',1,
     &    'ACFT      '
Cjhb=&=====24===========================================================
Cjhb       Real*4 River Diversion Acct. - Add to Soil Moisture - Senior, soil_cus(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Div to Soil Moist-Senior',1,
     &    'ACFT      '
Cjhb=&=====25===========================================================
Cjhb       Real*4 River Diversion Acct. - Add to Soil Moisture - Junior, soil_cuj(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Div to Soil Moist-Junior',1,
     &    'ACFT      '
Cjhb=&=====26===========================================================
Cjhb       Real*4 River Diversion Acct. - Add to Soil Moisture - Other, soil_cuo(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Div to Soil Moist-Other ',1,
     &    'ACFT      '
Cjhb=&=====27===========================================================
Cjhb       Real*4 River Diversion Acct. - Add to Soil Moisture - Total, soil_cu(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Div to Soil Moist-Total ',1,
     &    'ACFT      '
Cjhb=&=====28===========================================================
Cjhb       Real*4 River Diversion Acct. - Total Div to CU & Soil  , divcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Div to CU & Soil  ',1,
     &    'ACFT      '
Cjhb=&=====29===========================================================
Cjhb       Real*4 River Diversion Acct. - Non-Consumed - Senior, ulags(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Div Non-Consumed-Senior ',1,
     &    'ACFT      '
Cjhb=&=====30===========================================================
Cjhb       Real*4 River Diversion Acct. - Non-Consumed - Junior, ulagj(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Div Non-Consumed-Junior ',1,
     &    'ACFT      '
Cjhb=&=====31===========================================================
Cjhb       Real*4 River Diversion Acct. - Non-Consumed - Other, ulago(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Div Non-Consumed-Other  ',1,
     &    'ACFT      '
Cjhb=&=====32===========================================================
Cjhb       Real*4 River Diversion Acct. - Non-Consumed - Total, ulagt(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Div Non-Consumed-Total  ',1,
     &    'ACFT      '
C removed Cjhb=&=====31===========================================================
C removed Cjhb       Real*4 River Diversion Acct. - Sprinkler FHG (Not Applied), arech(m,l)
C removed            WRITE(UNIT=IBD1UN)'R',4,
C removed      &    'Sprinkler FHG (Not Appl)',0,
C removed      &    'ACFT      '
Cjhb=&=====33===========================================================
Cjhb       Real*4 Tail water, tail(i,m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Supply_Tail Water_Drains',1,
     &    'ACFT      '
Cjhb=&=====34===========================================================
Cjhb       Real*4 River Diversion Acct. - Max Applic Effic, sfeff(i,m)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Max Application Effic   ',0,
     &    'PERCENT   '
Cjhb=&=====35===========================================================
Cjhb       Real*4 River Diversion Acct. - Calc Surface Water Applic Effic (%), effcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Calc SW Applic Effic    ',0,
     &    'PERCENT   '
Cjhb=&=====36===========================================================
Cjhb       Real*4 River Diversion Acct. - Calc SW System Effic    , seffcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Calc SW System Effic    ',1,
     &    'PERCENT   '
Cjhb=&=====37===========================================================
Cjhb       Real*4 EOM Soil Moisture Contents - Senior, soiltotts(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'EOM Soil Moist - Senior ',1,
     &    'ACFT      '
Cjhb=&=====38===========================================================
Cjhb       Real*4 EOM Soil Moisture Contents - Junior, soiltottj(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'EOM Soil Moist - Junior ',1,
     &    'ACFT      '
Cjhb=&=====39===========================================================
Cjhb       Real*4 EOM Soil Moisture Contents - Other, soiltotto(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'EOM Soil Moist - Other  ',1,
     &    'ACFT      '
Cjhb=&=====40===========================================================
Cjhb       Real*4 EOM Soil Moisture Contents - Total, soiltott(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'EOM Soil Moist - Total  ',1,
     &    'ACFT      '
Cjhb=&=====41===========================================================
Cjhb       Real*4 Tot Crop CU from Div    , crop_cut(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Tot Crop CU from Div    ',1,
     &    'ACFT      '
Cjhb=&=====42===========================================================
Cjhb       Real*4 Total Crop from Soil    , cropcusoil(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Crop from Soil    ',1,
     &    'ACFT      '
Cjhb=&=====43===========================================================
Cjhb       Real*4 Total Crop CU - Senior  , estcrps(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Crop CU - Senior  ',1,
     &    'ACFT      '
Cjhb=&=====44===========================================================
Cjhb       Real*4 Total Crop CU - Junior  , estcrpj(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Crop CU - Junior  ',1,
     &    'ACFT      '
Cjhb=&=====45===========================================================
Cjhb       Real*4 Total Crop CU - Other   , estcrpo(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Crop CU - Other   ',1,
     &    'ACFT      '
Cjhb=&=====46===========================================================
Cjhb       Real*4 Total Crop CU           , estcrpt(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Crop CU           ',1,
     &    'ACFT      '
Cjhb=&=====47===========================================================
Cjhb       Real*4 Months Return Flows - From This Months Div - Senior, lagrets(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Ret From This Mo Div-Sen',1,
     &    'ACFT      '
Cjhb=&=====48===========================================================
Cjhb       Real*4 Months Return Flows - From This Months Div - Junior, lagretj(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Ret From This Mo Div-Jun',1,
     &    'ACFT      '
Cjhb=&=====49===========================================================
Cjhb       Real*4 Months Return Flows - From This Months Div - Other, lagreto(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Ret From This Mo Div-Oth',1,
     &    'ACFT      '
Cjhb=&=====50===========================================================
Cjhb       Real*4 Months Return Flows - From This Months Div - Total, lagrett(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Ret From This Mo Div-Tot',1,
     &    'ACFT      '
Cjhb=&=====51===========================================================
Cjhb       Real*4 Months Return Flows - From Prev Months Div - Senior, laglates(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Ret From Prev Mo Div-Sen',1,
     &    'ACFT      '
Cjhb=&=====52===========================================================
Cjhb       Real*4 Months Return Flows - From Prev Months Div - Junior, laglatej(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Ret From Prev Mo Div-Jun',1,
     &    'ACFT      '
Cjhb=&=====53===========================================================
Cjhb       Real*4 Months Return Flows - From Prev Months Div - Other, laglateo(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Ret From Prev Mo Div-Oth',1,
     &    'ACFT      '
Cjhb=&=====54===========================================================
Cjhb       Real*4 Months Return Flows - From Prev Months Div - Total, laglatet(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Ret From Prev Mo Div-Tot',1,
     &    'ACFT      '
Cjhb=&=====55===========================================================
Cjhb       Real*4 Months Return Flows - Total, totret(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Return Flow - Total     ',1,
     &    'ACFT      '
Cjhb=&=====56===========================================================
Cjhb       Real*4 River Depl(+)/Accr(-) - By Priority - Senior, deps(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Riv Deplete/Accrete-Sen ',1,
     &    'ACFT      '
Cjhb=&=====57===========================================================
Cjhb       Real*4 River Depl(+)/Accr(-) - By Priority - Junior, depj(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Riv Deplete/Accrete-Jun ',1,
     &    'ACFT      '
Cjhb=&=====58===========================================================
Cjhb       Real*4 River Depl(+)/Accr(-) - By Priority - Other, depo(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Riv Deplete/Accrete-Oth ',1,
     &    'ACFT      '
Cjhb=&=====59===========================================================
Cjhb       Real*4 River Depl(+)/Accr(-) - Total, dept(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Riv Deplete/Accrete-Tot ',1,
     &    'ACFT      '
Cjhb=&=====60===========================================================
Cjhb       Real*4 Replacement Requirement, depj(m,l),***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Replacement Requirement ',1,
     &    'ACFT      '
Cjhb=&=====61===========================================================
Cjhb       Real*4 Calculated CU shortage, SHORTAGE
Cjhb       = IWR reqreqts(m,l) - SWCU estcrpt(i,m,l) - GWCU gwcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'CU Shortage             ',0,
     &    'ACFT      '
Cjhb=&==================================================================
Cjhb       Total record size = 3*4+3+4+4+10+54*4 = 249 bytes
Cjhb=&==================================================================
Cjhb
Cjhb=&==================================================================
Cjhb       Now write the NSTR (NBASIN) structure data records for ISUPLY=3
Cjhb       Note: assumes these arrays are already populated, so output them now...
Cjhb=&==================================================================
            DO I=1,NBASIN
              CHAR12_1=BAS_ID(I)(1:12)
              CHAR12_2=BAS_ID(I)(13:24)
              WRITE(UNIT=IBD1UN)I,CHAR12_1,CHAR12_2
Cjhb          WRITE(*,*)I,CHAR12_1,CHAR12_2
            END DO
           if(sboutput) then
            DO I=0,SBCOUNT
                CHAR12_1=SBID(I)
                CHAR12_2=SBNAME(I)
                WRITE(UNIT=IBD1UN)NBASIN+I+1,CHAR12_1,CHAR12_2
            END DO
            CHAR12_1=BID
            CHAR12_2=BNAME
            WRITE(UNIT=IBD1UN)NBASIN+SBCOUNT+1+1,CHAR12_1,CHAR12_2
           endif
Cjhb=&==================================================================
         ELSEIF(ISUPLY .EQ. 4) THEN
           IF(IFLOOD .EQ. 0) THEN
Cjhb=&==================================================================
           IBD1TSA=12 !# time steps per year
           IBD1TS=IBD1TSA*NYRS !total # time steps
           IBD1NF=34 !number output values per timestep per structure
           IBD1NSF=3 !number nontime series value output per structure
           if(sboutput) then
           WRITE(UNIT=IBD1UN)NBASIN+SBCOUNT+1+1,
     &                       IBD1TS,IBD1NSF,IBD1NF,IBD1TSA
           else
           WRITE(UNIT=IBD1UN)NBASIN,
     &                       IBD1TS,IBD1NSF,IBD1NF,IBD1TSA
           endif
Cjhb=&==================================================================
Cjhb       Here are the IBD1NSF (3) values on the STRUCTURE records for ISUPLY=4 and IFLOOD=0
Cjhb=======1============================================================
Cjhb       Integer Structure Index, I
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Structure Index         ',0,
     &    'Structure Index                                             '
Cjhb=&=====2============================================================
Cjhb       Char*12 Structure ID string, BAS_ID(L)(1:12) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',12,
     &    'Structure ID            ',1,
     &    'Structure ID                                                '
Cjhb=&=====3============================================================
Cjhb       Char*12 Structure ID name, BAS_ID(L)(13:24) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',12,
     &    'Structure Name          ',1,
     &    'Structure Name                                              '
Cjhb=&==================================================================
Cjhb       Total record size = 4+12+12 = 28 bytes
Cjhb=&==================================================================
Cjhb=&==================================================================
Cjhb       Here are the IBD1NF (34) values on the time series records for ISUPLY=4 and IFLOOD=0
Cjhb=======1============================================================
Cjhb       Integer Structure Index, I
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Structure Index         ',0,
     &    '          '
Cjhb=&=====2============================================================
Cjhb       Integer Year, NYR1+M-1
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Year                    ',0,
     &    '          '
Cjhb=&=====3============================================================
Cjhb       Integer Month Index, L
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Month Index             ',0,
     &    '          '
Cjhb=&=====4============================================================
Cjhb       Char*3 abbr month string, AMN(L) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',3,
     &    'Month Name              ',1,
     &    '          '
Cjhb=&=====5============================================================
Cjhb       Real*4 'Total Irrigated Acreage ' - t_area(i,m)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Irrigated Acreage ',1,
     &    'ACRE      '
Cjhb=&=====6============================================================
Cjhb       Real*4 Modeled Crop Acreage - m_area(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Modeled Irrig Acreage   ',0,
     &    'ACRE      '
Cjhb=&=====7============================================================
Cjhb       Char*10 Analysis method - "Calculated" vs. "Prorated" - METHOD
           WRITE(UNIT=IBD1UN)'C',10,
     &    'Analysis Method         ',1,
     &    '          '
Cjhb=&=====8============================================================
Cjhb       Real*4 Potential Crop ET  , ettot(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Potential Crop ET       ',1,
     &    'ACFT      '
Cjhb=&=====9============================================================
Cjhb       Real*4 Effective Precip, effppt(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Effective Precip        ',1,
     &    'ACFT      '
Cjhb=&=====10===========================================================
Cjhb       Real*4 Irrigation Water Requirement IWR, reqt(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Irrigation Water Reqt   ',1,
     &    'ACFT      '
Cjhb=&=====11===========================================================
Cjhb       Real*4 EOM Winter Precip Carryover, wbu(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Wint Prec Soil Content  ',1,
     &    'ACFT      '
Cjhb=&=====12===========================================================
Cjhb       Real*4 IWR After Winter Precip, reqreqts(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'IWR After Winter Precip ',1,
     &    'ACFT      '
Cjhb=&=====13===========================================================
Cjhb       Real*4 River Diversion Acct - Div By Priority - Total, ddhmonot(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'River Diversion         ',1,
     &    'ACFT      '
Cjhb=&=====14===========================================================
Cjhb       Real*4 River Diversion Acct. - conveyance Efficiency, ceff(i,m)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Conveyance Efficiency   ',0,
     &    'PERCENT   '
Cjhb=&=====15===========================================================
Cjhb       Real*4 River Diversion Acct. - conveyance Efficiency, closs(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Conveyance Loss         ',0,
     &    'ACFT      '
Cjhb=&=====16===========================================================
Cjhb       Real*4 River Diversion Acct. - Farm Headgate Diversion, fdiv(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Farm Headgate Delivery  ',0,
     &    'ACFT      '
Cjhb=&=====17===========================================================
Cjhb       Real*4 Tail water, tail(i,m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Supply_Tail Water_Drains',1,
     &    'ACFT      '
Cjhb=&=====18===========================================================
Cjhb       Real*4 River Diversion Acct. - 'Farm Deliver to Recharge', arech(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to Recharge          ',0,
     &    'ACFT      '
Cjhb=&=====19===========================================================
Cjhb       Real*4 River Diversion Acct. - Max Applic Effic, sfeff(i,m)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Max Application Effic   ',0,
     &    'PERCENT   '
Cjhb=&=====20===========================================================
Cjhb       Real*4 River Diversion Acct. - Destination: CU, crop_cut(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to CU                ',1,
     &    'ACFT      '
Cjhb=&=====21===========================================================
Cjhb       Real*4 River Diversion Acct. - Destination: Soil Zone, soil_cu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to Soil              ',1,
     &    'ACFT      '
Cjhb=&=====22===========================================================
Cjhb       Real*4 River Diversion Acct. - Destination: Non-Consumed, ulagt(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW_Non_Consumed         ',1,
     &    'ACFT      '
Cjhb=&=====23===========================================================
Cjhb       Real*4 River Diversion Acct. - Calc Surface Water Applic Effic (%), effcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Calc SW Applic Effic    ',0,
     &    'PERCENT   '
Cjhb=&=====24===========================================================
Cjhb       Real*4 River Diversion Acct. - Calc SW System Effic    , seffcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Calc SW System Effic    ',1,
     &    'PERCENT   '
Cjhb=&=====25===========================================================
Cjhb       Real*4 GW Diversion Acct. - 'GW Diversion            ', gdiv(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'GW Diversion            ',0,
     &    'ACFT      '
Cjhb=&=====26===========================================================
Cjhb       Real*4 GW Diversion Acct. - 'Calc GW Application Eff ', effgw(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Calc GW Application Eff ',0,
     &    'PERCENT   '
Cjhb=&=====27===========================================================
Cjhb       Real*4 GW Diversion Acct. - 'GW CU                   ', gwcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'GW CU                   ',0,
     &    'ACFT      '
Cjhb=&=====28===========================================================
Cjhb       Real*4 GW Diversion Acct. - 'GW - Non-Consumed       ', gwro(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'GW_Non-Consumed         ',0,
     &    'ACFT      '
Cjhb=&=====29===========================================================
Cjhb       Real*4 EOM Soil Moisture Contents - Total, soiltott(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW Soil Content         ',1,
     &    'ACFT      '
Cjhb=&=====30===========================================================
Cjhb       Real*4 Estimated Crop CU - From SW/GW Diversion, cutot, ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Crop CU from SW and GW  ',1,
     &    'ACFT      '
Cjhb=&=====31===========================================================
Cjhb       Real*4 Estimated Crop CU - From Soil Moisture, cropcusoil(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Crop CU from Soil       ',1,
     &    'ACFT      '
Cjhb=&=====32===========================================================
Cjhb       Real*4 Estimated Crop CU - Total, custot(i,m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Crop CU           ',1,
     &    'ACFT      '
Cjhb=&=====33===========================================================
Cjhb       Real*4 Total Month Non-Consumed, tdp(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW & GW Non_Consumed    ',1,
     &    'ACFT      '
Cjhb=&=====34===========================================================
Cjhb       Real*4 Calculated CU shortage, SHORTAGE
Cjhb       = IWR reqreqts(m,l) - SWCU estcrpt(i,m,l) - GWCU gwcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'CU Shortage             ',0,
     &    'ACFT      '
Cjhb=&==================================================================
Cjhb       Total record size = 3*4+3+4+4+10+27*4 = 137 bytes
Cjhb=&==================================================================
Cjhb
Cjhb=&==================================================================
Cjhb       Now write the NSTR (NBASIN) structure data records for ISUPLY=4 and IFLOOD=0
Cjhb       Note: assumes these arrays are already populated, so output them now...
Cjhb=&==================================================================
            DO I=1,NBASIN
              CHAR12_1=BAS_ID(I)(1:12)
              CHAR12_2=BAS_ID(I)(13:24)
              WRITE(UNIT=IBD1UN)I,CHAR12_1,CHAR12_2
Cjhb          WRITE(*,*)I,CHAR12_1,CHAR12_2
            END DO
           if(sboutput) then
            DO I=0,SBCOUNT
                CHAR12_1=SBID(I)
                CHAR12_2=SBNAME(I)
                WRITE(UNIT=IBD1UN)NBASIN+I+1,CHAR12_1,CHAR12_2
            END DO
            CHAR12_1=BID
            CHAR12_2=BNAME
            WRITE(UNIT=IBD1UN)NBASIN+SBCOUNT+1+1,CHAR12_1,CHAR12_2
           endif
Cjhb=&==================================================================
           ELSE !IFLOOD .NE. 0
Cjhb=&==================================================================
           IBD1TSA=12 !# time steps per year
           IBD1TS=IBD1TSA*NYRS !total # time steps
           IBD1NF=36+IFLOOD2 !number output values per timestep per structure
           IBD1NSF=3 !number nontime series value output per structure
           if(sboutput) then
           WRITE(UNIT=IBD1UN)NBASIN+SBCOUNT+1+1,
     &                       IBD1TS,IBD1NSF,IBD1NF,IBD1TSA
           else
           WRITE(UNIT=IBD1UN)NBASIN,
     &                       IBD1TS,IBD1NSF,IBD1NF,IBD1TSA
           endif
Cjhb=&==================================================================
Cjhb       Here are the IBD1NSF (3) values on the STRUCTURE records for ISUPLY=4 and IFLOOD<>0
Cjhb=======1============================================================
Cjhb       Integer Structure Index, I
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Structure Index         ',0,
     &    'Structure Index                                             '
Cjhb=&=====2============================================================
Cjhb       Char*12 Structure ID string, BAS_ID(L)(1:12) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',12,
     &    'Structure ID            ',1,
     &    'Structure ID                                                '
Cjhb=&=====3============================================================
Cjhb       Char*12 Structure ID name, BAS_ID(L)(13:24) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',12,
     &    'Structure Name          ',1,
     &    'Structure Name                                              '
Cjhb=&==================================================================
Cjhb       Total record size = 4+12+12 = 28 bytes
Cjhb=&==================================================================
Cjhb=&==================================================================
Cjhb       Here are the IBD1NF (33+IFLOOD2+1) values on the time series records for ISUPLY=4 and IFLOOD<>0
Cjhb=======1============================================================
Cjhb       Integer Structure Index, I
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Structure Index         ',0,
     &    '          '
Cjhb=&=====2============================================================
Cjhb       Integer Year, NYR1+M-1
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Year                    ',0,
     &    '          '
Cjhb=&=====3============================================================
Cjhb       Integer Month Index, L
           WRITE(UNIT=IBD1UN)'I',4,
     &    'Month Index             ',0,
     &    '          '
Cjhb=&=====4============================================================
Cjhb       Char*3 abbr month string, AMN(L) ***REPORT***
           WRITE(UNIT=IBD1UN)'C',3,
     &    'Month Name              ',1,
     &    '          '
Cjhb=&=====5============================================================
Cjhb       Real*4 'Total Irrigated Acreage ' - t_area(i,m)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Irrigated Acreage ',1,
     &    'ACRE      '
Cjhb=&=====6============================================================
Cjhb       Real*4 Modeled Crop Acreage - m_area(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Modeled Irrig Acreage   ',0,
     &    'ACRE      '
Cjhb=&=====7============================================================
Cjhb       Char*10 Analysis method - "Calculated" vs. "Prorated" - METHOD
           WRITE(UNIT=IBD1UN)'C',10,
     &    'Analysis Method         ',1,
     &    '          '
Cjhb=&=====8============================================================
Cjhb       Real*4 Potential Crop ET  , ettot(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Potential Crop ET       ',1,
     &    'ACFT      '
Cjhb=&=====9============================================================
Cjhb       Real*4 Effective Precip, effppt(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Effective Precip        ',1,
     &    'ACFT      '
Cjhb=&=====10===========================================================
Cjhb       Real*4 Irrigation Water Requirement IWR, reqt(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Irrigation Water Reqt   ',1,
     &    'ACFT      '
Cjhb=&=====11===========================================================
Cjhb       Real*4 EOM Winter Precip Carryover, wbu(i,m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Wint Prec Soil Content  ',1,
     &    'ACFT      '
Cjhb=&=====12===========================================================
Cjhb       Real*4 IWR After Winter Precip, reqreqts(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'IWR After Winter Precip ',1,
     &    'ACFT      '
Cjhb=&=====13===========================================================
Cjhb       Real*4 River Diversion Acct - River Diversion         , ddhmonot(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'River Diversion         ',1,
     &    'ACFT      '
Cjhb=&=====14===========================================================
Cjhb       Real*4 River Diversion Acct. - Conveyance Efficiency   , ceff(i,m)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Conveyance Efficiency   ',0,
     &    'PERCENT   '
Cjhb=&=====15===========================================================
Cjhb       Real*4 River Diversion Acct. - Conveyance Loss         , closs(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Conveyance Loss         ',0,
     &    'ACFT      '
Cjhb=&=====16===========================================================
Cjhb       Real*4 River Diversion Acct. - 'Farm Headgate Delivery  ', fdiv(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Farm Headgate Delivery  ',0,
     &    'ACFT      '
Cjhb=&=====17===========================================================
Cjhb       Real*4 Supply_Tail Water_Drains, tail(i,m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Supply_Tail Water_Drains',1,
     &    'ACFT      '
Cjhb=&=====18===========================================================
Cjhb       Real*4 River Diversion Acct. - 'Farm Deliver to Recharge', arech(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to Recharge          ',0,
     &    'ACFT      '
Cjhb=&=====19===========================================================
Cjhb       Real*4 River Diversion Acct. - 'Max Application Effic   ', sfeff(i,m)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Max Application Effic   ',0,
     &    'PERCENT   '
Cjhb=&=====20===========================================================
Cjhb       Real*4 River Diversion Acct. - SW to CU         , crop_cut(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to CU                ',1,
     &    'ACFT      '
Cjhb=&=====21===========================================================
Cjhb       Real*4 River Diversion Acct. - SW to Soil              , soil_cu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW to Soil              ',1,
     &    'ACFT      '
Cjhb=&=====22===========================================================
Cjhb       Real*4 River Diversion Acct. - 'SW_Non_Consumed         ', ulagt(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW_Non_Consumed         ',1,
     &    'ACFT      '
Cjhb=&=====23===========================================================
Cjhb       Real*4 River Diversion Acct. - Calc Surface Water Applic Effic (%), effcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Calc SW Applic Effic    ',0,
     &    'PERCENT   '
Cjhb=&=====24===========================================================
Cjhb       Real*4 River Diversion Acct. - Calc SW System Effic    , seffcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Calc SW System Effic    ',1,
     &    'PERCENT   '
Cjhb=&=====25===========================================================
Cjhb       Real*4 GW Diversion Acct. - 'GW Diversion            ', gdiv(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'GW Diversion            ',0,
     &    'ACFT      '
Cjhb=&=====26===========================================================
Cjhb       Real*4 GW Diversion Acct. - 'Calc GW Application Eff ', effgw(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Calc GW Application Eff ',0,
     &    'PERCENT   '
Cjhb=&=====27===========================================================
Cjhb       Real*4 GW Diversion Acct. - 'GW CU                   ', gwcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'GW CU                   ',0,
     &    'ACFT      '
Cjhb=&=====28===========================================================
Cjhb       Real*4 GW Diversion Acct. - 'GW - Non-Consumed       ', gwro(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'GW_Non-Consumed         ',0,
     &    'ACFT      '
Cjhb=&=====29===========================================================
Cjhb       Real*4 EOM SW Soil Content - Total, soiltott(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW Soil Content         ',1,
     &    'ACFT      '
Cjhb=&=====30===========================================================
Cjhb       Real*4 Estimated Crop CU - From SW/GW Diversion, cutot, ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Crop CU from SW and GW  ',1,
     &    'ACFT      '
Cjhb=&=====31===========================================================
Cjhb       Real*4 Estimated Crop CU - From Soil Moisture, cropcusoil(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Crop CU from Soil       ',1,
     &    'ACFT      '
Cjhb=&=====32===========================================================
Cjhb       Real*4 Estimated Crop CU - Total, custot(i,m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'Total Crop CU           ',1,
     &    'ACFT      '
Cjhb=&=====33===========================================================
Cjhb       Real*4 Total Month Non-Consumed, tdp(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'SW & GW Non_Consumed    ',1,
     &    'ACFT      '
Cjhb=&=====34===========================================================
Cjhb       Real*4 Calculated CU shortage, SHORTAGE
Cjhb       = IWR reqreqts(m,l) - SWCU estcrpt(i,m,l) - GWCU gwcu(m,l)
           WRITE(UNIT=IBD1UN)'R',4,
     &    'CU Shortage             ',0,
     &    'ACFT      '
Cjhb=&=====35===========================================================
Cjhb       Real*4 GW div to Sprinkler, gsdiv(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'GW Applied by Sprinklers',1,
     &    'ACFT      '
Cjhb=&=====36===========================================================
Cjhb       Real*4 GW div to Flood, gfdiv(m,l), ***REPORT***
           WRITE(UNIT=IBD1UN)'R',4,
     &    'GW Applied by Flood Irr ',1,
     &    'ACFT      '
Cjhb=&=====37-??========================================================
           DO I=1,IFLOOD
           CHTMP1=subname(i) // ' IWR               '
           CHTMP1=CHTMP1 // REPEAT(' ',24-LEN(CHTMP1))
Cjhb       Real*4 Grass Pasture IWR, grass(i,m,l,ifx)
           WRITE(UNIT=IBD1UN)'R',4,
     &     CHTMP1,0,
     &    'ACFT      '
           CHTMP1=subname(i) // ' Acreage            '
           CHTMP1=CHTMP1//REPEAT(' ',24-LEN(CHTMP1))
Cjhb       Real*4 Grass Pasture IWR, grass(i,m,l,ifx)
           WRITE(UNIT=IBD1UN)'R',4,
     &     CHTMP1,0,
     &    'ACFT      '
           END DO
Cjhb=&==================================================================
Cjhb       Total record size = 3*4+3+4+4+10+29*4+IFLOOD2*4 = ?? bytes
Cjhb=&==================================================================
Cjhb
Cjhb=&==================================================================
Cjhb       Now write the NSTR (NBASIN) structure data records for ISUPLY=4 and IFLOOD<>0
Cjhb       Note: assumes these arrays are already populated, so output them now...
Cjhb=&==================================================================
            DO I=1,NBASIN
              CHAR12_1=BAS_ID(I)(1:12)
              CHAR12_2=BAS_ID(I)(13:24)
              WRITE(UNIT=IBD1UN)I,CHAR12_1,CHAR12_2
Cjhb          WRITE(*,*)I,CHAR12_1,CHAR12_2
            END DO
           if(sboutput) then
            DO I=0,SBCOUNT
                CHAR12_1=SBID(I)
                CHAR12_2=SBNAME(I)
                WRITE(UNIT=IBD1UN)NBASIN+I+1,CHAR12_1,CHAR12_2
            END DO
            CHAR12_1=BID
            CHAR12_2=BNAME
            WRITE(UNIT=IBD1UN)NBASIN+SBCOUNT+1+1,CHAR12_1,CHAR12_2
           endif
Cjhb=&==================================================================
           ENDIF !IFLOOD .EQ. 0
         ELSE !ISUPLY .NE. 1,2,3,4
            WRITE(*,*)'SHOULD NEVER GET HERE!!!!!!!!!'
         ENDIF !ISUPLY .EQ. 1,2,3,4
      ENDIF ! (LBD1OUT)
Cjhb=&==================================================================
c grb 4-20-00 Assign temp1 and temp2 as file names for use in temporary creation of DWB files
c     Open and close temp2 to avoid error if no aggregates are in data set - next 3 lines
Cjhb=&==================================================================
      OPEN (UNIT=256,FILE="temp2",STATUS='Unknown',IOSTAT=IERR)
      Close (256)
      OPEN (UNIT=256,FILE="temp1",STATUS='Unknown',IOSTAT=IERR)
      thefile2 = dfile
      thefile2(fn_len:fn_len+4)= '.swb'
      OPEN (UNIT=800,FILE=thefile2,STATUS='Unknown')
Cjhb=&==================================================================
Cjhb  open the 4 land category output file if necessary
Cjhb=&==================================================================
      if((trim(s4catid).ne."").and.(ipresim.ne.1))then
        thefile2 = dfile
        thefile2(fn_len:fn_len+4)= '.4wb'
        OPEN (UNIT=413,FILE=thefile2,STATUS='Unknown')

c jhb add header lines to 4WB output file
        write(413,1108)vers, rdate
1108    FORMAT('StateCU Version ', f5.2,2x,a16)
        write(413,1109)dfile
1109    FORMAT('Scenario name: ', a200)
        write(413,1110)CURDATE(5:6),CURDATE(7:8),CURDATE(1:4),
     &               CURTIME(1:2),CURTIME(3:4),CURTIME(5:6)
1110    FORMAT('Model execution time: ',A2,'-',A2,'-',A4,'  ',
     &         A2,':',A2,':',A2)
        write(413,'(A120)')TITLE(1)
        write(413,'(A120)')TITLE(2)
        write(413,'(A120)')TITLE(3)
        write(413,*)
        write(413,*)"Four Land Category Report for Structure ",s4catid
c     1111X2223333333344444444555555556666666677777777888888889999999900000000111111112222222233333333444444445555555566666666777777778888888899999999000000001111111122222222333333334444444455555555666666667777777788888888999999990000000011111111222222223333333344444444555555556666666677777777888888889999999900000000111111112222222233333333444444445555555566666666777777778888888899999999000000001111111122222222333333334444444455555555666666667777777788888888999999990000000011111111222222223333333344444444555555556666666677777777888888889999999900000000111111112222222233333333444444445555555566666666777777778888888899999999000000001111111122222222333333334444444455555555666666667777777788888888999999990000000011111111222222223333333344444444555555556666666677777777888888889999999900000000"
c     ----|---|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------|-----------------------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-----------------------|-----------------------|---------------------------------------|---------------------------------------|
c         |   |           Irrigated Acreage           |          Potential Crop ET            |           Effective Precip            |     Irrigation Water Requirement      |    Winter Precip Carryover Used       |           IWR after WCO               |      SW River Diversion       |       SW River Delivery       |  Tail |                                  SW Farm Headgate Delivery                    |                                   SW to CU                            |     SW to Soil Moisture       |   *Pushed Out* Soil Moisture  |Non-consumed SW (incl sm rel) |                            Soil Moisture To CU                        |      Groundwater Pumping      |   GW Pumping To CU    |Non-Consumed GW Pumping|              Total CU                 |          Total CU Shortage            |
c     ----|---|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------|-----------------------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-----------------------|-----------------------|---------------------------------------|---------------------------------------|
c         |   |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total | senior| junior| other | Total | senior| junior| other | Total | Total |SW Only|SW Only|SW & GW|SW & GW|  re-  | Total | senior| junior| other | Total |SW Only|SW Only|SW & GW|SW & GW| Total | senior| junior| other | Total | senior| junior| other | Total | junior| other | other | Total | senior| junior| other | Total |SW Only|SW Only|SW & GW|SW & GW| Total | senior| junior| other | Total |SW & GW|SW & GW| Total |  Max  |SW & GW|SW & GW| Total |SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |
c         |   |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |       |       |       |       |       |       |       |       |       |  Flood|Sprnklr|  Flood|Sprnklr| charge|       | sw div| sw div| sw div|       |  Flood|Sprnklr|  Flood|Sprnklr|       |       |       |       |       |       |       |       |       | by sr | by sr | by jr |       |       |       |       |       |  Flood|Sprnklr|  Flood|Sprnklr|       |       |       |       |       |  Flood|Sprnklr|       |  Rate |  Flood|Sprnklr|       |  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |
c     Year|Mon|(Acres)|(Acres)|(Acres)|(Acres)|(Acres)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|
c     ----|---|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------|-----------------------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-----------------------|-----------------------|---------------------------------------|---------------------------------------|
Cjhb=&==================================================================
        write(413,'(a801)')
     &"----|---|---------------------------------------|----------------
     &-----------------------|---------------------------------------|--
     &-------------------------------------|----------------------------
     &-----------|---------------------------------------|--------------
     &-----------------|-------------------------------|-------|--------
     &---------------------------------------|--------------------------
     &-----|---------------------------------------|--------------------
     &-----------|-------------------------------|----------------------
     &---------|-------------------------------|------------------------
     &---------------|-------------------------------|------------------
     &-------------|-----------------------|-----------------------|----
     &-----------------------------------|------------------------------
     &---------|"
        write(413,'(a801)')
     &"    |   |           Irrigated Acreage           |          Potent
     &ial Crop ET            |           Effective Precip            |  
     &   Irrigation Water Requirement      |    Winter Precip Carryover 
     &Used       |           IWR after WCO               |      SW River
     & Diversion       |       SW River Delivery       |  Tail |        
     &                          SW Farm Headgate Delivery               
     &     |                                   SW to CU                 
     &           |     SW to Soil Moisture       |   *Pushed Out* Soil M
     &oisture  | Non-consumed SW (incl sm rel) |                        
     &    Soil Moisture To CU                        |      Groundwater 
     &Pumping      |   GW Pumping To CU    |Non-Consumed GW Pumping|    
     &          Total CU                 |          Total CU Shortage   
     &         |"
        write(413,'(a801)')
     &"----|---|---------------------------------------|----------------
     &-----------------------|---------------------------------------|--
     &-------------------------------------|----------------------------
     &-----------|---------------------------------------|--------------
     &-----------------|-------------------------------|-------|--------
     &---------------------------------------|--------------------------
     &-----|---------------------------------------|--------------------
     &-----------|-------------------------------|----------------------
     &---------|-------------------------------|------------------------
     &---------------|-------------------------------|------------------
     &-------------|-----------------------|-----------------------|----
     &-----------------------------------|------------------------------
     &---------|"
        write(413,'(a801)')
     &"    |   |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|
     &SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW
     & Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW &
     & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total | senior| junio
     &r| other | Total | senior| junior| other | Total | Total |SW Only|
     &SW Only|SW & GW|SW & GW|  re-  | Total | senior| junior| other | T
     &otal |SW Only|SW Only|SW & GW|SW & GW| Total | senior| junior| oth
     &er | Total | senior| junior| other | Total | junior| other | other
     & | Total | senior| junior| other | Total |SW Only|SW Only|SW & GW|
     &SW & GW| Total | senior| junior| other | Total |SW & GW|SW & GW| T
     &otal |  Max  |SW & GW|SW & GW| Total |SW & GW|SW & GW| Total |SW O
     &nly|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & G
     &W| Total |"
        write(413,'(a801)')
     &"    |   |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|
     &  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  
     &Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprn
     &klr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |       |      
     & |       |       |       |       |       |       |       |  Flood|
     &Sprnklr|  Flood|Sprnklr| charge|       | sw div| sw div| sw div|  
     &     |  Flood|Sprnklr|  Flood|Sprnklr|       |       |       |    
     &   |       |       |       |       |       | by sr | by sr | by jr
     & |       |       |       |       |       |  Flood|Sprnklr|  Flood|
     &Sprnklr|       |       |       |       |       |  Flood|Sprnklr|  
     &     |  Rate |  Flood|Sprnklr|       |  Flood|Sprnklr|       |  Fl
     &ood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnkl
     &r|       |"
        write(413,'(a801)')
     &"Year|Mon|(Acres)|(Acres)|(Acres)|(Acres)|(Acres)|   (AF)|   (AF)|
     &   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|  
     & (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (
     &AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF
     &)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|
     &   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|  
     & (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (
     &AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF
     &)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|
     &   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|  
     & (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (
     &AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF
     &)|   (AF)|"
        write(413,'(a801)')
     &"----|---|---------------------------------------|----------------
     &-----------------------|---------------------------------------|--
     &-------------------------------------|----------------------------
     &-----------|---------------------------------------|--------------
     &-----------------|-------------------------------|-------|--------
     &---------------------------------------|--------------------------
     &-----|---------------------------------------|--------------------
     &-----------|-------------------------------|----------------------
     &---------|-------------------------------|------------------------
     &---------------|-------------------------------|------------------
     &-------------|-----------------------|-----------------------|----
     &-----------------------------------|------------------------------
     &---------|"
      endif
Cjhb=&==================================================================
c     open efficiency output files for StateMod
Cjhb=&==================================================================
      if(ddcsw .eq. 1) then
        thefile3 = dfile
        thefile3(fn_len:fn_len+4)= '.def'
        thefile4 = dfile
        thefile4(fn_len:fn_len+4)= '.wef'
        open(5,file=thefile3,status='unknown')
        write(5,661) '*.def: Diversion'
        if(isuply .eq. 4) then
           open(6,file=thefile4,status='unknown')
           write(6,661) '*.wef:      Well'
        endif
      endif
Cjhb=&==================================================================
c Initialize data for project summaries
Cjhb=&==================================================================
      nyrs1=nyrs+1
      nyrs2=nyrs+2
      DO K=1,15
        AS(K) = '           '
      ENDDO
      do m=1,dim_ny
        numat(m)=0.
        acret(m)=0.
        tsfeff(m)=0.
C       ==================================================================
c       reset the area arrays - in case this routine is called twice (soil moisture presimulation)
C       ==================================================================
        do j=1,12
          btarea(m,j)=0.
          bmarea(m,j)=0.
        enddo
        do j=1, dim_na
          do k=1,12
            m_area(j,m,k)=0.
          enddo
        enddo
        do j=1, dim_nsb
          do k=1,12
            sbtarea(j,m,k)=0.
            sbmarea(j,m,k)=0.
          enddo
        enddo
C       ==================================================================
        do j=1,14
          treq(m,j)=0
          treqt(m,j)=0
          tet(m,j)=0
          teffr(m,j)=0
          tdiv(m,j)=0
          ttail(m,j)=0
          tcus(m,j)=0
          tcuj(m,j)=0
          tcuo(m,j)=0
          tcut(m,j)=0
          tscus(m,j)=0
          tscuj(m,j)=0
          tscuo(m,j)=0
          tscu(m,j)=0
          tulags(m,j)=0
          tulagj(m,j)=0
          tulago(m,j)=0
          tulagt(m,j)=0
          tdivcu(m,j)=0
          ttotts(m,j)=0
          ttottj(m,j)=0
          ttotto(m,j)=0
          ttott(m,j)=0
          twbu(m,j)=0
          tcusoil(m,j)=0
          tcrps(m,j)=0
          tcrpj(m,j)=0
          tcrpo(m,j)=0
          tcrpt(m,j)=0
          trets(m,j)=0
          tretj(m,j)=0
          treto(m,j)=0
          trett(m,j)=0
          tlates(m,j)=0
          tlatej(m,j)=0
          tlateo(m,j)=0
          tlatet(m,j)=0
          ttotret(m,j)=0
          tdeps(m,j)=0
          tdepj(m,j)=0
          tdepo(m,j)=0
          tdept(m,j)=0
          tsenf(m,j)=0
          tjunf(m,j)=0
          tothf(m,j)=0
          tarech(m,j)=0
          tgdiv(m,j)=0
          tgsdiv(m,j)=0
          tgfdiv(m,j)=0
          tgwcu(m,j)=0
          tgwro(m,j)=0
          tfdiv(m,j)=0
          ttdp(m,j)=0
          tcloss(m,j)=0
          do jj=1,100
            idcnt(jj,m,j) = 0
c grb 4-28-00 initialize swmet variable
            swmet(jj,m,j)=0.0
          enddo
        enddo
      enddo
Cjhb=&==================================================================
Cjhb  allows multiple flood irrigated crops...
Cjhb=&==================================================================
      if(iflood .ge. 1) then
        open(104,FILE="gtemp")
        do 444 i=1,nbasin
          do 444 iy=1,nyrs
            do if1=1,iflood
              do im=1,12
                read(104,'(i5, 20f8.2)') ig, gr, ar
                ifx=ig*2-1
                ify=ig*2
                grass(i,iy,im,ifx)=gr
                grass(i,iy,im,ify)=ar
              enddo
            enddo
444     continue
      endif
c     if this is a soil moisture presimulation, will need to keep this file (gtemp) around for a second pass
      if(ipresim.eq.1)then
        close(104,status='KEEP')
      else
        close(104,status='DELETE')
      endif
c
c
Cjhb=&==================================================================
Cjhb  initialize a flag to track whether this is the first or second time through this code
Cjhb  (first time through determines values to use as average values for missing data)
Cjhb  itime is count of an internal loop
Cjhb=&==================================================================
Cjhb  note: the ipresim variable is for soil moisture initialization - determines whether
Cjhb  this is first or second call of wsupsum - wsupsum is called twice from statecu.for if ism=2 in the ccu
Cjhb  this is one way to initialize the sm: run entire wsupsum water supply accounting routine,
Cjhb  take ending sm and use it as initial sm, then run entire wsupsum water supply accounting routine again
Cjhb=&==================================================================
      itime=1 !first time through this code (this is for missing data fill, NOT for ipresim!!!)
Cjhb=&==================================================================
Cjhb  jhb/emw Feb 17, 2011
Cjhb  might have jumped back here after setting itime=2
Cjhb  now handle the isuply 2,3 cases - water rights - call slimit again
Cjhb  to re-prorate sr, jr, other
Cjhb=&==================================================================
45    if (itime.eq.2.and.(isuply.eq.2.or.isuply.eq.3)) then
        eyetime=2
        call slimit
        eyetime=0
      endif
c     begin nbasin loop (structure loop)
Cjhb=&==================================================================
      do 578 i=1,nbasin
Cjhb=&==================================================================
        id=99
c       grb 5-20-00 define aggregate and missing data structures up front in 1st pass for explicit structures
        twdid=bas_id(i)
c       add error and reset of id if no numerical value in first two characters (i.e. old gunnison runs)
        read(twdid(1:2),'(i2)',err=4490) ID
 4490   if (id.eq.0) id=99
c       ----------------------------------------------------------------  
c       grb 09-10-00 skip reset of flag if presim already performed 
c       ----------------------------------------------------------------
c        if (ipresim.eq.2) goto 452
c       ----------------------------------------------------------------  
c       first time through wsupsum - initialize missing data flag arrays to 0
c         missflg1(i) - any missing data for structure i, ANY year
c         missflag(i,m1) - any missing data for structure i in year m1
c         added by jhb:
c         missiwr(i,m1) - missing iwr data for structure i in year m1
c         missdiv(i,m1) - missing div data for structure i in year m1 (monthly - divsup() from DDH file)
c       ----------------------------------------------------------------  
        if (itime.eq.1) then
          missflg1(i)=0 
          do m1=1,nyrs
            missflag(i,m1)=0
            missiwr(i,m1)=0
            missdiv(i,m1)=0
          enddo
Cjhb=&==================================================================
C     removed iagg and use imiss to prorate aggr data
C      (i.e. set aggr diversion to -999 externally to get it prorated)
Cjhb=&==================================================================
C            if(twdid(4:5) .eq. 'AD' .or. twdid(3:4) .eq. 'AD') then
C               if(iagg .eq. 1) then
C                 do 450 m1=1,nyrs
C 450             missflag(i,m1)=1
C                 missflg1(i)=1
C               else
C                 do 448 m1=1,nyrs
C                 do 448,j1=1,12
C                  if(divsup(i,m1,j1).lt. -998 .or.
C     &             reqt(i,m1,j1) .lt. -998) then
C                     missflag(i,m1)=1
C                    missflg1(i)=1
C                  endif
C 448            continue
C               endif
C            else
Cjhb=&==================================================================
c         --------------------------------------------------------------
c         check for missing data - structure and structure/year flags are
c         set to 1 (true) if EITHER irrigation water requirement (IWR) or
c         diversion data is -999 (missing data value) in any month of the year
c         --------------------------------------------------------------
          do m1=1,nyrs
            do j1=1,12
              if(reqt(i,m1,j1) .lt. -998.) then
c               there is no IWR              
                missflag(i,m1)=1
                missflg1(i)=1
                missiwr(i,m1)=1
              endif
              if(divsup(i,m1,j1).lt. -998.) then
c               there are missing diversions
                missflag(i,m1)=1
                missflg1(i)=1
                missdiv(i,m1)=1
              endif
            enddo
          enddo
C          endif
c         --------------------------------------------------------------
c         reset all monthly diversion data for structure i and year m1
c         to -999 (missing data value) if the missing data flag is true for that year
c         --------------------------------------------------------------
          do m1=1,nyrs
            if(missflag(i,m1) .eq. 1) then
              do j1=1,12
                divsup(i,m1,j1) = -999.
              enddo
            endif
c           --------------------------------------------------------
c           calculate the total and modeled areas for the structures, basin and sub basins
c           --------------------------------------------------------
            do j1=1,12
              sbtarea(sbsb(i),m1,j1)=sbtarea(sbsb(i),m1,j1)+t_area(i,m1)
              btarea(m1,j1)=btarea(m1,j1)+t_area(i,m1)
            enddo
            if( (missiwr(i,m1).eq.1) .or.
     &          ( (missdiv(i,m1).eq.1) .and. (imiss2.eq.0) ) ) then
              do j1=1,12
                m_area(i,m1,j1)=0.0
              enddo
            else
              do j1=1,12
                m_area(i,m1,j1)=t_area(i,m1)
                sbmarea(sbsb(i),m1,j1)=
     &            sbmarea(sbsb(i),m1,j1)+t_area(i,m1)
                bmarea(m1,j1)=bmarea(m1,j1)+t_area(i,m1)
              enddo
            endif
c           --------------------------------------------------------
          enddo
        endif
c       ----------------------------------------------------------------  
452     continue
Cjhb=&==================================================================
Cjhb=&  note this has changed from original comments:
Cjhb=&  skip the rest of the structure loop (year loop, month loop, etc) on first pass
Cjhb=&      if structure i is flagged for missing data, missflg(i)=1
Cjhb=&  skip the rest of the structure loop (year loop, month loop, etc) on second pass if structure has data (i.e.
Cjhb=&      structure i is not flagged for missing data, missflg(i)=0)
Cjhb=&==================================================================
c       grb 05-20-00 skip processing if first pass and
c          1) iagg= 1, will fill entire period of record
c          2) imiss2=1 and need to fill that year
c       skip processing on 2nd pass if no missing data
Cjhb=&==================================================================
        if(itime .eq. 1 .and. missflg1(i) .eq. 1) goto 578
        if(itime .eq. 2 .and. missflg1(i) .eq. 0) goto 578
Cjhb=&==================================================================
        iyct=0
        iyct2=0
        do 200 j=1,nyrs1
          iyear(j)=0
          comment(j)=''
          do 100 k=1,13
            imonth(k)=0
            crop_cus(j,k)=0
            crop_cuj(j,k)=0
            crop_cuo(j,k)=0
            crop_cut(j,k)=0
            soil_cus(j,k)=0
            soil_cuj(j,k)=0
            soil_cuo(j,k)=0
            soil_cu(j,k)=0
            cropcusoils(j,k)=0
            soiltott(j,k)=0
            soiltotts(j,k)=0
            soiltottj(j,k)=0
            soiltotto(j,k)=0	
            cropcusoilj(j,k)=0
            cropcusoilo(j,k)=0
            cropcusoil(j,k)=0
            estcrps(j,k)=0
            estcrpj(j,k)=0
            estcrpo(j,k)=0
            estcrpt(i,j,k)=0
            ulags(j,k)=0
            ulagj(j,k)=0
            ulago(j,k)=0
            ulagt(j,k)=0
            lagrets(j,k)=0
            lagretj(j,k)=0
            lagreto(j,k)=0
            lagrett(j,k)=0
            divcu(j,k)=0
            laglates(j,k)=0
            laglatej(j,k)=0
            laglateo(j,k)=0
            laglatet(j,k)=0
            totret(j,k)=0
            deps(j,k)=0
            depj(j,k)=0
            depo(j,k)=0
            dept(j,k)=0
            seniorf(j,k)=0
            juniorf(j,k)=0
            otherf(j,k)=0
            arech(j,k)=0
            gdiv(j,k)=0
            gsdiv(j,k)=0
            gfdiv(j,k)=0
            gwcu(j,k)=0
            gwro(j,k)=0
            fdiv(j,k)=0
            tdp(j,k)=0
            closs(j,k)=0
            ddhmonot(j,k)=0
            reqreqts(j,k)=0
c           grb 5-20-00 initialize variable soil_cujout
            soil_cujout(j,k)=0
            soil_cuoout(j,k)=0
100       continue
200     continue
        holdests=0
        holdcrop=0
        holds=0
        holds_sf=0
        holds_ss=0
        holds_sw=0
        holds_gf=0
        holds_gs=0
        holds_gw=0
        holdj=0
        holdj_sf=0
        holdj_ss=0
        holdj_sw=0
        holdj_gf=0
        holdj_gs=0
        holdj_gw=0
        holdo=0
        holdo_sf=0
        holdo_ss=0
        holdo_sw=0
        holdo_gf=0
        holdo_gs=0
        holdo_gw=0
        holdt=0
        holdfs=0
        holdfj=0
        holdfo=0
        senaspt=senasp(i)
        junaspt=junasp(i)
Cjhb=&==================================================================
c       grb 5-20-00 move some sections of following code to determine missing structures up front
c       determine if this structure has any annual positive diversions(sumit=1)
c       set first month diversion to -999 if any missing data for that year
Cjhb=&==================================================================
        do 150 m=1, nyrs
          do 20 j=1,nparce(i,m)+1
            if(j .eq. nparce(i,m)+1) then
                crpname(j,m)= '    Total           '
              write(atxt(i,j,m),'(f10.0)') t_area(i,m)
            else
              key=bkey(i,j,m)
              crpname(j,m) = cpname(key)
              write(atxt(i,j,m),'(f10.0)') area(i,j,m)
            endif
20        continue
150     continue
Cjhb=&==================================================================
c       begin year loop
Cjhb=&==================================================================
        depfact=1.0
        do 400 m=1,nyrs
Cjhb=&==================================================================
c         ew 11/00 = if "maximize supply" option, do not account for
c         soil moisture reservoir under sprinkler lands
Cjhb=&==================================================================
Cjhb=&    11/06 Modified soil moisture capacity code.
Cjhb=&          Now that there is a new irrig land category (sw only sprinkler) to consider.
Cjhb=&          The objective is to exclude the swgw spr acreage from the soil moisture capacity
Cjhb=&          in a certain case: isuply=4 (gw) and gmode=1 ("max supply")
Cjhb=&          Set spcapz to non-swgw-spr percentage of total soil moisture, scapatot(i,m)
Cjhb=&==================================================================
c         if(isuply .eq. 4 .and. gmode(i,m) .eq. 1) then
          select case (isuply)
            case (4)
              select case (gmode(i,m))
                case (1)
c                 if(gper(i,m).eq. 0 .or. sper(i,m) .eq. 0) then
c                   spcapz=scapatot(i,m)
c                 else
c                   spcapz = scapatot(i,m)*gper(i,m)*sper(i,m)
c                 endif
                  if(swgwspac(i,m).gt.0.0)then
                    spcapz=scapatot(i,m)*(t_area(i,m)-swgwspac(i,m))
     &                     /t_area(i,m)
                  else
                    spcapz=scapatot(i,m)
                  endif
                case default
                  spcapz=scapatot(i,m)
              end select
c           else
            case default
              spcapz=scapatot(i,m)
          end select
c         endif
Cjhb=&==================================================================
Cjhb=&    initialize the current structure's soil moisture contents in year 1
Cjhb=&      soiltots = senior soil moisture pool
Cjhb=&      soiltotj = junior soil moisture pool
Cjhb=&      soiltoto = other soil moisture pool
Cjhb=&==================================================================
c         if(m .eq. 1) then !year 1
          select case (m)
            case (1) !year 1
c             initialize soil moisture contents for this parcel
c             grb 05-04-00 add logic to skip initiatlization if in presim mode
              select case (ipresim)
                case (1) !(ipresim)
c                 if (ipresim.eq.1) then 
C                 jhb| start soil moisture at 0 in year 1 of the first time
C                 jhb| through when running twice (ism=2, ipresim=1)
                  soiltots=0
                  soiltotj=0
                  soiltoto=0
c                 endif
Cjhb=&==================================================================
c               grb 05-04-00 add logic to initiatlize soil moisture from pre sim if in simulation mode with presim
                case (2) !(ipresim)
c                 if (ipresim.eq.2) then
C                 jhb| set soil moisture in year 1 of the second time through to 
C                 jhb| the ending soil contents of the first time through when
C                 jhb| running twice (ism=2, ipresim=2)
c                 grb 05-31-00 reduce initialization if ending contents exceed beginning capacity
                  if ((endsoils(i)+endsoilj(i)+endsoilo(i)).gt.spcapz)
     &            then
                    soilfac=spcapz/(endsoils(i)+endsoilj(i)+endsoilo(i))
                  else
                    soilfac=1.0
                  endif
                  soiltots=endsoils(i)*soilfac
                  soiltotj=endsoilj(i)*soilfac
                  soiltoto=endsoilo(i)*soilfac
c                endif
Cjhb=&==================================================================
                case (0) !(ipresim)
c                 if (ipresim.eq.0) then
c                 grb 4-30-00 make following lines execute for all supply options
C                 jhb| set soil moisture to user entered percentage of soil capacity
C                 jhb| when ism=1, ipresim=0
                  if(isuply .eq.1.or. isuply .eq. 4) then
                    pjunmo=0
                    pothmo=0
                  endif
                  soiltots = spcapz*psenmo
                  soiltotj = spcapz*pjunmo
                  soiltoto = spcapz*pothmo
c                 endif
                case default !(ipresim)
                  write(*,*)'invalid ipresim value, ',ipresim
                  stop
              end select ! (ipresim)
Cjhb=&==================================================================
              soiltot = soiltotj + soiltots +soiltoto
c             remember initial soil capacities 
              senmo=soiltots
              junmo=soiltotj
              othmo=soiltoto
              totmo=soiltot
Cjhb=&==================================================================
            case default !(m) - years 2 to nyrs; Reduce current contents by reduction in acreage
c             else   !years 2 to nyrs; Reduce current contents by reduction in acreage
c             ew 5/24  changed ratio from t_area to scapatot because soil moisture
c                    capacity not only function of area, but also max root depth
              if(scapatot(i,m) .lt. scapatot(i,m-1)) then
                iduct=scapatot(i,m)/scapatot(i,m-1)*100
                write(comment(m),53) iduct
53              format(
     &           'Note: Soil Moisture Contents reduced to ',
     &           i2,' % of previous due to acreage reduction.')
                soiltots=soiltots*scapatot(i,m)/scapatot(i,m-1)
                soiltotj=soiltotj*scapatot(i,m)/scapatot(i,m-1)
                soiltoto=soiltoto*scapatot(i,m)/scapatot(i,m-1)
                soiltot= soiltotj + soiltots + soiltoto
              endif
          end select
c         endif
Cjhb=&==================================================================
c         begin month loop
Cjhb=&==================================================================
          do 300 l=1,12
            if(missflag(i,m) .eq. 1) then
Cjhb=&==================================================================
Cjhb=&        this structure (i) and year (m) is flagged to have missing data
Cjhb=&========original comments=========================================
C             removed iagg and use imiss to prorate aggr data
C             (i.e. set aggr diversion to -999 externally to get it prorated)
C             if(twdid(4:5) .eq. 'AD' .or. twdid(3:4) .eq. 'AD') then
C               if(iagg .eq. 1) goto 301
C             endif
Cjhb=&==================================================================
Cjhb=&        if user specified to fill in missing data (imiss2=1) then jump to 301
Cjhb=&==================================================================
              if(imiss2 .eq. 1) goto 301
Cjhb=&==================================================================
Cjhb=&        else set all monthly values to -999, set soil moisture values
Cjhb=&          as if there was no CU, and then jump to the end of the month
Cjhb=&          loop (go to the next month)
Cjhb=&==================================================================
              iyear(m)=1
              effcu(m,l) = -999
              seffcu(m,l) = -999
              seniorf(m,l)=-999
              juniorf(m,l)=-999
              otherf(m,l)=-999
              fdiv(m,l) = -999
              closs(m,l) = -999
              arech(m,l) = -999
              gdiv(m,l) = -999
              gsdiv(m,l) = -999
              gfdiv(m,l) = -999
              gwcu(m,l) = -999
              gwro(m,l) = -999
              tdp(m,l) = -999
              ettot(i,m,l) = -999
              effppt(i,m,l) = -999
              reqt(i,m,l) = -999
              reqreqts(m,l)=-999
              ddhmonot(m,l)=-999
              crop_cus(m,l)=-999
              crop_cuj(m,l)=-999
              crop_cuo(m,l)=-999
              crop_cut(m,l)=-999
              soil_cus(m,l)=-999
              soil_cuj(m,l)=-999
              soil_cuo(m,l)=-999
              soil_cu(m,l)=-999
              ulags(m,l)=-999
              ulagj(m,l)=-999
              ulago(m,l)=-999
              ulagt(m,l)=-999
              divcu(m,l)=-999
              if(m .eq. 1 .and. l .eq. 1) then
                soiltotts(m,l)=senmo
                soiltottj(m,l)=junmo
                soiltotto(m,l)=othmo
                soiltott(m,l)=totmo
                wbu(i,m,l)=0.0
                wbused(i,m,l)=0.0
              elseif(l .eq. 1) then
                soiltotts(m,l)=soiltotts(m-1,12)
                soiltottj(m,l)=soiltottj(m-1,12)
                soiltotto(m,l)=soiltotto(m-1,12)
                soiltott(m,l)=soiltott(m-1,12)
                wbu(i,m,l)=wbu(i,m-1,12) !carry last year's value through
                wbused(i,m,l)=0.0
              else
                soiltotts(m,l)=soiltotts(m,l-1)
                soiltottj(m,l)=soiltottj(m,l-1)
                soiltotto(m,l)=soiltotto(m,l-1)
                soiltott(m,l)=soiltott(m,l-1)
                wbu(i,m,l)=wbu(i,m,l-1) !carry last year's value through
                wbused(i,m,l)=0.0
              endif
              soiltotts(m,13)=soiltotts(m,13)+soiltotts(m,l)
              soiltottj(m,13)=soiltottj(m,13)+soiltottj(m,l)
              soiltotto(m,13)=soiltotto(m,13)+soiltotto(m,l)
              soiltott(m,13)=soiltott(m,13)+soiltott(m,l)
              wbu(i,m,13)=wbu(i,m,13)+wbu(i,m,l)
              soiltotts(nyrs1,l)=soiltotts(nyrs1,l)+soiltotts(m,l)
              soiltottj(nyrs1,l)=soiltottj(nyrs1,l)+soiltottj(m,l)
              soiltotto(nyrs1,l)=soiltotto(nyrs1,l)+soiltotto(m,l)
              soiltott(nyrs1,l)=soiltott(nyrs1,l)+soiltott(m,l)
              wbu(i,nyrs1,l)=wbu(i,nyrs1,l)+wbu(i,m,l)
              wbu(i,nyrs1,13)=wbu(i,nyrs1,13)+wbu(i,m,l)
              cropcusoil(m,l)=-999
              custot(i,m,l)=-999
              estcrps(m,l)=-999
              estcrpj(m,l)=-999
              estcrpo(m,l)=-999
              estcrpt(i,m,l)=-999
              lagrets(m,l)=-999
              lagretj(m,l)=-999
              lagreto(m,l)=-999
              lagrett(m,l)=-999
              laglates(m,l)=-999
              laglatej(m,l)=-999
              laglateo(m,l)=-999
              laglatet(m,l)=-999
              totret(m,l)=-999
              deps(m,l)=-999
              depj(m,l)=-999
              depo(m,l)=-999
              dept(m,l)=-999
c               grb 5--5-00 initialize soil_cujout
              soil_cujout(m,l)=-999
              soil_cuoout(m,l)=-999
              goto 300
            endif
Cjhb=&==================================================================
Cjhb=&      to get here -
Cjhb=&        either there IS NO missing data for structure i in year m (missflag(i,m) = 0)
Cjhb=&          (and note that therefore this must be the first time through, itime=1,
Cjhb=&           since the year loop is skipped when there is no missing data on the second time through)
Cjhb=&        or there IS missing data for structure i in year m (missflag(i,m) = 1),
Cjhb=&          and the user specified to FILL in missing data (imiss2=1)
Cjhb=&          (and note that therefore this must be the second time through, itime=2,
Cjhb=&           since the year loop is skipped when there is missing data on the first time through)
Cjhb=&==================================================================
C           jhb | keep a count of the number of times through the month loop (for monthly averaging purposes later)
301         imonth(l)=imonth(l)+1
C           jhb | initialize the variables containing soil moisture contributions to crop cu
            holdcrops=0
            holdcropj=0
            holdcropo=0
            holdcrop=0
Cjhb=&==================================================================
Cjhb=&      initialize the winter carryover soil moisture contents for structure i, year m, month l
Cjhb=&        here is where the winter precip values are accumulated
Cjhb=&==================================================================
            if(l .eq. 1) then
c               month 1 - check to see if year 1
                if(m .eq. 1) then
c                   year 1, month 1 - start with initial wint prec value
                    wbu(i,m,l)=WINTPREC(i,m,l)
                else
c                   year m>1, month l=1 - accumulate, add last month's wbu to this month's wintprec
                    wbu(i,m,l)=wbu(i,m-1,12)+WINTPREC(i,m,l)
                endif
            else
c               year m, month l>1 - accumulate, add last month's wbu to this month's wintprec
                wbu(i,m,l)=wbu(i,m,l-1)+WINTPREC(i,m,l)
            endif
Cjhb=&==================================================================
Cjhb=&      if wbu > soil space avail after initial soil contents,
Cjhb=&      then set wbu=avail space in soil
Cjhb=&      I think this needs to be corrected to account for isuply=4
Cjhb=&      and gmode=1 since the soil moisture capacity is shrunk in that case...
Cjhb=&==================================================================
            wbu(i,m,l)=max(min(wbu(i,m,l),scapatot(i,m)-soiltot),0.)
Cjhb=&==================================================================
            if(reqt(i,m,l) .gt. -998) then
Cjhb=&==================================================================
Cjhb=&        if the crop cu requirement is not missing (-999)
Cjhb=&==================================================================
c              reqreq(m,l)=max((reqt(i,m,l)-wbu(i,m,l)),0.)
c              add this for central only
c              if(m .le. 24) depfact=1.0
c              if(m .gt. 24 .and. l .eq. 1) depfact=depfact-(.15/45)
c              reqt(i,m,l)=depfact*reqt(i,m,l)
c              reqt(i,m,l)=depfact*reqt(i,m,l)
c              reqreq(m,l)=max(reqt(i,m,l),0.)
Cjhb=&==================================================================
c             apply winter carryover soil moisture to the crop IWR
Cjhb=&==================================================================
              wbu_used = max(min(reqt(i,m,l),wbu(i,m,l)),0.)
              wbused(i,m,l)=wbu_used
              reqreq(m,l) = reqt(i,m,l) - wbu_used
Cjhb=&==================================================================
c             adjust the winter carryover soil moisture storage to acct for water used by crop
Cjhb=&==================================================================
              wbu(i,m,l) = wbu(i,m,l) - wbu_used
c              swreq(m,l)=reqreq(m,l)*(1-gper(i,m))
c              gsreq(m,l)=reqreq(m,l)*gper(i,m)*sper(i,m)
c              gfreq(m,l)=reqreq(m,l)*gper(i,m)*(1-sper(i,m))
Cjhb=&==================================================================
C             DISTRIBUTE CROP IWR TO THE FOUR LAND CATEGORIES
C             sfreq - surface water only, flood irrigated lands
C             ssreq - surface water only, sprinkler irrigated lands
C             swreq - surface water only, all lands
C             gfreq - surface water and groundwater, flood irrigated lands
C             gsreq - surface water and groundwater, sprinkler irrigated lands
C             gwreq - surface water and groundwater, all lands
Cjhb=&==================================================================
              if(t_area(i,m).gt.0.0)then
                sfreq(m,l)=reqreq(m,l)*swflac(i,m)/t_area(i,m)
                ssreq(m,l)=reqreq(m,l)*swspac(i,m)/t_area(i,m)
                gfreq(m,l)=reqreq(m,l)*swgwflac(i,m)/t_area(i,m)
                gsreq(m,l)=reqreq(m,l)*swgwspac(i,m)/t_area(i,m)
              else
                sfreq(m,l)=0.0
                ssreq(m,l)=0.0
                gfreq(m,l)=0.0
                gsreq(m,l)=0.0
              endif
              swreq(m,l)=sfreq(m,l)+ssreq(m,l)
              gwreq(m,l)=gfreq(m,l)+gsreq(m,l)
            else
              reqreq(m,l)=-999
              ettot(i,m,l)=-999
              effppt(i,m,l)=-999
              reqreqts(m,l)=-999
              swreq(m,l)=0.
              ssreq(m,l)=0.
              sfreq(m,l)=0.
              gwreq(m,l)=0.
              gsreq(m,l)=0.
              gfreq(m,l)=0.
            endif
            swreqdef(m,l)=swreq(m,l)*def_irr_frac(i,m)
            ssreqdef(m,l)=ssreq(m,l)*def_irr_frac(i,m)
            sfreqdef(m,l)=sfreq(m,l)*def_irr_frac(i,m)
            gwreqdef(m,l)=gwreq(m,l)*def_irr_frac(i,m)
            gsreqdef(m,l)=gsreq(m,l)*def_irr_frac(i,m)
            gfreqdef(m,l)=gfreq(m,l)*def_irr_frac(i,m)
Cjhb=&==================================================================
Cjhb=&      set spcapz to portion of soil moisture not occupied by winter carryover...
Cjhb=&      I think this needs to be corrected to account for isuply=4
Cjhb=&      and gmode=1 since the soil moisture capacity is shrunk in that case...
Cjhb=&==================================================================
Cjhb=&      comment this out - we do not want to use up soil moist space with wbu
Cjhb=&==================================================================
c           spcapz=scapatot(i,m)-wbu(i,m,l)
Cjhb=&==================================================================
c grb  05-20-00 add execution of following code if missing diversion data for a year or for aggregated structure
Cjhb=&==================================================================
            if(itime .eq. 2 .and. missflag(i,m) .eq. 1) then !second pass and filling missing data...
              select case (gmode(i,m))
              case (3)
c              if(gmode(i,m) .eq. 3) then
c                this is gmode 3 where sw for gw sp lands goes to recharge
c                modify to handle all four categories
                sfcu=sfreq(m,l)*swmet(id,m,l)
                sscu=ssreq(m,l)*swmet(id,m,l)
                swcu=sfcu+sscu
                gfcu=gfreq(m,l)*swmet(id,m,l)
                gscu=gsreq(m,l)*swmet(id,m,l)
                gcu=gfcu+gscu
c                 fdiv(m,l)=swcu/fleff(i,m)+gfcu/fleff(i,m)+
c     :                   gscu
                fdiv(m,l)=sfcu/fleff(i,m)+gfcu/fleff(i,m)+
     :                     sscu/speff(i,m)+gscu
                arech(m,l)=gscu
                arech(m,13)=arech(m,13)+arech(m,l)
                arech(nyrs1,l)=arech(nyrs1,l)+arech(m,l)
                gsshare=0
              case (1)
c                else
c                this is gmode 1 - maximize supply mode
c                (don't put sw on gw spr lands, put supply on spr lands first)
                sfcu=sfreq(m,l)*swmet(id,m,l)
                sscu=ssreq(m,l)*swmet(id,m,l)
                swcu=sfcu+sscu
                gfcu=gfreq(m,l)*swmet(id,m,l)
                gscu=gsreq(m,l)*swmet(id,m,l)
c               don't put gw spr IWR in fdiv
                gscu=0
                gcu=gfcu+gscu
c                 fdiv(m,l)=swcu/fleff(i,m)+gfcu/fleff(i,m)+
c     :                   gscu/speff(i,m)
                fdiv(m,l)=sfcu/fleff(i,m)+gfcu/fleff(i,m)+
     :                     sscu/speff(i,m)+gscu/speff(i,m)
              case default ! 2 or any other value of gmode
c              elseif(gmode(i,m) .eq. 2) then
c                this is gmode 2 - mutual ditch mode
                sfcu=sfreq(m,l)*swmet(id,m,l)
                sscu=ssreq(m,l)*swmet(id,m,l)
                swcu=sfcu+sscu
                gfcu=gfreq(m,l)*swmet(id,m,l)
                gscu=gsreq(m,l)*swmet(id,m,l)
                gcu=gfcu+gscu
c                 fdiv(m,l)=swcu/fleff(i,m)+gfcu/fleff(i,m)+
c     :                   gscu/speff(i,m)
                fdiv(m,l)=sfcu/fleff(i,m)+gfcu/fleff(i,m)+
     :                     sscu/speff(i,m)+gscu/speff(i,m)
              end select
c              endif
              divsup(i,m,l) = fdiv(m,l)/ceff(i,m)
cjhb &         ---------------------------------------------------------
cjhb &         remove these lines since we are going to rederive the gfcu and gscu (and hopefully get the same numbers)
cjhb &         and therefore remove the surf div cu from the req on gw lands later
cjhb &         ---------------------------------------------------------
c              gsreq(m,l) = gsreq(m,l) - gscu
c              gfreq(m,l) = gfreq(m,l) - gfcu
            endif
Cjhb=&==================================================================
Cjhb=&      end of missing data filling
Cjhb=&==================================================================
c
c  ew 3/12/04  here is where we want to read suppy from drains/tailwater
c  add drain/tailwater supply to fdiv for accounting (but do not consider losses) 
Cjhb=&==================================================================
C           SURFACE WATER farm deliveries = diversions - canal losses + tailwater
C           note that tail() can be negative
C           ew/jhb 09/2008 - tail() can be negative but constrain it
C                            to the value of fdiv()=divsup()*ceff()
C                            make a note to the log file if abs(tail)>fdiv
C                            and then change the value of tail()=-fdiv
Cjhb=&==================================================================
            fdiv(m,l)=divsup(i,m,l)*ceff(i,m)+tail(i,m,l)
            if(fdiv(m,l).lt.0.0)then
c             why do it this way?
c             because it not only adjusts when tail() is too negative
c             it also fixes the problem if a negative divsup slips through
c             should not happen, but better to catch it and create a warning than crash...
              tail(i,m,l)=tail(i,m,l)-fdiv(m,l)
              fdiv(m,l)=0.0
c             catch this warning and record in new log file format...
              if(fdiv(m,l) .lt. -2.0) then
                if(ipresim .ne. 1) call lw_update(47,bas_id(i))
              endif  
              select case (scu_debug)
              case (0)
              case (1)
                WARNINGS=.TRUE.
                write(999,*)'Warning: Farm delivery < 0. ' //
     &          'Reset tail(i,m,l) to ', tail(i,m,l),
     &          ' and fdiv(m,l) to 0.0 for structure: ',
     &          bas_id(i),' year:',m,' month:',l
              case default
              end select
            endif
            fdiv(m,13)=fdiv(m,13)+fdiv(m,l)
            fdiv(nyrs1,l)=fdiv(nyrs1,l)+fdiv(m,l)
Cjhb=&==================================================================
C           canal losses = diversions - farm deliveries + tailwater
Cjhb=&==================================================================
c           jhb: i am not sure why it was calculated this way previously,
c           but change it to divsup*(1-canal eff)
c            closs(m,l)=divsup(i,m,l)-fdiv(m,l)+tail(i,m,l)
Cjhb=&==================================================================
            closs(m,l)=divsup(i,m,l)*(1.0-ceff(i,m))
            closs(m,13)=closs(m,13)+closs(m,l)
            closs(nyrs1,l)=closs(nyrs1,l)+closs(m,l)
Cjhb=&==================================================================
C           Split SURFACE WATER farm deliveries into senior, other and junior deliveries
Cjhb=&==================================================================
C           Senior Diversions
Cjhb=&==================================================================
            IF (missflg1(i).EQ.0) THEN
Cjhb=&==================================================================
C             No missing data
Cjhb=&==================================================================
              if(idaily.eq.0) holdps=
     1          min(divsup(i,m,l),senasp(i)*1.9835*month(l))
              if (idaily.gt.0) then
                if(persen(i,m,l) .lt. -998) then
                  HOLDPS=MIN(DIVSUP(I,M,L),SENASP(I)*1.9835*month(L))
                else
                  holdps=divsup(i,m,l)*persen(i,m,l)
                endif
              endif
Cjhb=&==================================================================
            ELSE
Cjhb=&==================================================================
C             Missing data
Cjhb=&==================================================================
              if (idaily.le.3) then
                do 7500 k4=1,ddstrctt
                  if (trim(adjustr(ddstruct(k4))).eq.
     &              trim(adjustr(twdid(1:12)))) then
                    ddindex=k4
                    goto 754             
                  endif            
7500            continue
              endif
754           IF (IDAILY.EQ.1 .and. missflag(i,m) .eq. 1) THEN
                ddrec=ddindex+((nyr1-1-dbyear+M)*(ddstrctt*12))+
     1                ((L-1)*ddstrctt)
                iddrec=ddrec
                read(538,rec=iddrec) (divndata(k4),k4=1,31)
                IDAYS=31
                IF (L.EQ.2) IDAYS=28
                IF (L.EQ.4.OR.L.EQ.6.OR.L.EQ.9.OR.L.EQ.11) IDAYS=30
c               grb 09-13-00 initialize holdps
                holdps=0                        
                DO 755 I6=1,IDAYS
                  HOLDPS=HOLDPS+MIN(-DIVNDATA(I6)*1.9835,
     1            DIVSUP(I,M,L)/IDAYS)
755             CONTINUE
                goto 756
              ENDIF
              IF (IDAILY.ge.2.and.idaily.le.5) THEN
                if(persen(i,m,l) .lt. 0) then
                  HOLDPS=MIN(DIVSUP(I,M,L),SENASP(I)*1.9835*month(L))
                else
                  HOLDPS=PERSEN(I,M,L)*divsup(i,m,l)
                endif
              ENDIF
              if (idaily.eq.0 .or. idaily .eq. 1) THEN
                if(persen(i,m,l) .lt. -998) then
                  HOLDPS=MIN(DIVSUP(I,M,L),SENASP(I)*1.9835*month(L))
                else
                  holdps=divsup(i,m,l)*persen(i,m,l)
                endif
c                  HOLDPS=MIN(DIVSUP(I,M,L),SENASP(I)*1.9835*month(L))
              ENDIF
            ENDIF
Cjhb=&==================================================================
C           now we have senior diversions - HOLDPS
Cjhb=&==================================================================
C           convert it to senior farm deliveries - HOLDFS, SENIORF(m,l)
Cjhb=&==================================================================
756         continue
            holdfs=holdps*ceff(i,m)
Cjhb=&==================================================================
C-----------Other Diversions                   
C           grb 06-29-00 add consideration of variable administration dates                          
Cjhb=&==================================================================
            IF (MISSFLG1(I).EQ.0) THEN
Cjhb=&==================================================================
C             no missing data
Cjhb=&==================================================================
C              rewrite the following code because it does not work
C                sufficiently for the 4 category design, even though the
C                errors did not affect the 3 category output
C                (essentially the incorrect holdpo and holdpj canceled each
C                 other when added together)
Cjhb=&=========old code=================================================
c               the following calculates the max possible "other" diversions
c               based on the total sr and jr rights added up from the ddr file
c               --------------------------------------------------------
c               however, note that it IGNORES the diversions classified
c               as senior calculated above!  so it is double accounting that
c               water since trights(i) is going to be 0 when idaily=0
c               bottom line, this code sets holdpo to divsup, when it should be 0
c               so, if divsup(i,m,l)=100, then holdps=100, holdpo=100 and holdpj=-100
c               --------------------------------------------------------
c              if(idaily.eq.0) holdpo=
c     &           max(divsup(i,m,l)-(trights(i)*month(l)*1.9835),0.0)
Cjhb=&=========new code=================================================
              if(idaily.eq.0) then
c               --------------------------------------------------------
c               no priority modeling
c               --------------------------------------------------------
c               the following calculates the max possible "other" diversions
c               based on the total sr and jr rights added up from the ddr file
c               --------------------------------------------------------
c               however, note that it IGNORES the diversions classified
c               as senior calculated above!  so it is double accounting that
c               water since trights(i) is going to be 0 when idaily=0
c               bottom line, this code sets holdpo to divsup, when it should be 0
c               --------------------------------------------------------
                holdpo=max(divsup(i,m,l)-trights(i)*month(l)*1.9835,
     &                     0.0)
c               --------------------------------------------------------
c               therefore add the following correction
c               this also fixes the anomolous result for holdpj calculated
c               next that an incorrect value of holdpo causes
c               --------------------------------------------------------
                holdpo=min(holdpo, divsup(i,m,l)-holdps)
Cjhb=&==================================================================
c              if (idaily.gt.0) then
              else
c               --------------------------------------------------------
c               priority modeling
c               --------------------------------------------------------
                if(peroth(i,m,l) .lt. -998) then
                  holdpo=max(0.0,divsup(i,m,l)-((senasp(i)+junasp(i))
     &                   *month(l)*1.9835))
                else
                  holdpo =divsup(i,m,l)*peroth(i,m,l)
                endif
              endif
            ELSE
Cjhb=&==================================================================
C             missing data
Cjhb=&==================================================================
              IDAYS=31
              IF (L.EQ.2) IDAYS=28
              IF (L.EQ.4.OR.L.EQ.6.OR.L.EQ.9.OR.L.EQ.11) IDAYS=30
c             grb 09-13-00 initialize holdpo                   
              holdpo=0                       
              DO 753 I6=1,IDAYS
                HOLDPO=HOLDPO+MAX((DIVSUP(I,M,L)/idays-(TRIGHTS(I)*
     1                  1.9835)),0.0)
753           CONTINUE 
c             --------------------------------------------------------
c             add the following correction for the same reason explained above
c             --------------------------------------------------------
              holdpo=min(holdpo, divsup(i,m,l)-holdps)
            ENDIF
Cjhb=&==================================================================
C           now we have other diversions - HOLDPO
Cjhb=&==================================================================
C           convert it to other farm deliveries - HOLDFO, OTHERF(m,l)
Cjhb=&==================================================================
            holdfo = holdpo*ceff(i,m)
Cjhb=&==================================================================
C-----------------Junior Diversions
Cjhb=&==================================================================
            IF (MISSFLG1(I).EQ.0.and.idaily.ne.0) then
C             no missing data AND using daily admin #
              holdpj = divsup(i,m,l)*(1.0-persen(i,m,l)-peroth(i,m,l))
            ELSE
C             missing data OR not using daily admin #
              HOLDPJ=(DIVSUP(I,M,L)-HOLDPO-HOLDPS)
            ENDIF
            HOLDPJ=(DIVSUP(I,M,L)-HOLDPO-HOLDPS)
Cjhb=&==================================================================
C           now we have junior diversions - HOLDPJ
Cjhb=&==================================================================
C           convert it to junior farm deliveries - HOLDFJ, JUNIORF(m,l)
Cjhb=&==================================================================
            holdfj=holdpj*ceff(i,m)
Cjhb=&==================================================================
C           Now reflect the effects of the drain file values
C           Because these can can be negative,
C             had to wait until last to make the adjustments
C           The drain file only effects FARM deliveries (holdfs, holdfj, holdfo)
C            and not river headgate values (holdps, holdpj, holdpo)
C           Also, if the structure/year has missing data, do not add it in at all.
            if(missflg1(i).eq.0)then !no missing data
              if(tail(i,m,l).ge.0.0)then !drain value is positive
c               add it to other farm deliveries and be done with it
                holdfo = holdfo + tail(i,m,l)
              else !it is negative
c               reduce other farm deliveries, then junior, then senior
c               leave tail() negative so the efficiencies will calculate correctly
                total_adj=tail(i,m,l)
                oth_adj=max(-holdfo,total_adj)
                holdfo=holdfo+oth_adj
                total_adj=total_adj-oth_adj
                jun_adj=max(-holdfj,total_adj)
                holdfj=holdfj+jun_adj
                total_adj=total_adj-jun_adj
                sen_adj=max(-holdfs,total_adj)
                holdfs=holdfs+sen_adj
              endif
            endif
C           now we have the final categorized farm delivery values to use
            seniorf(m,l)=seniorf(m,l)+holdfs
            seniorf(m,13)=seniorf(m,13)+holdfs
            seniorf(nyrs1,l)=seniorf(nyrs1,l)+holdfs
            juniorf(m,l)=juniorf(m,l)+holdfj
            juniorf(m,13)=juniorf(m,13)+holdfj
            juniorf(nyrs1,l)=juniorf(nyrs1,l)+holdfj
            otherf(m,l)=otherf(m,l)+holdfo
            otherf(m,13)=otherf(m,13)+holdfo
            otherf(nyrs1,l)=otherf(nyrs1,l)+holdfo
C
Cjhb=&==================================================================
C           DISTRIBUTE FARM DELIVERIES FROM SURFACE SOURCES TO THE FOUR LAND CATEGORIES
C             sfshare - surface water only, flood irrigated lands
C             ssshare - surface water only, sprinkler irrigated lands
C             swshare - surface water only, all lands
C             gfshare - surface water and groundwater, flood irrigated lands
C             gsshare - surface water and groundwater, sprinkler irrigated lands
C             gwshare - surface water and groundwater, all lands
C           THEN CALCULATE CU FROM SURFACE SOURCES ON THE FOUR LAND CATEGORIES
C             sfcu - surface water only, flood irrigated lands
C             sscu - surface water only, sprinkler irrigated lands
C             swcu - surface water only, all lands
C             gfcu - surface water and groundwater, flood irrigated lands
C             gscu - surface water and groundwater, sprinkler irrigated lands
C             gcu  - surface water and groundwater, all lands
Cjhb=&==================================================================
Cjhb=&      on second pass, if this is structure and year with filled data,
Cjhb=&      then we STARTED with IWR (sfreq, ssreq, gfreq, gsreq) and the
Cjhb=&      division average shortage (swmet),
Cjhb=&      and then back-calculated the CU and then the farm deliveries (fdiv) ,
Cjhb=&      and then the stream diversions (divsup)
Cjhb=&      Therefore we do NOT want to recalculate the CU, again!
Cjhb=&==================================================================
C            if(itime .eq. 2 .and. missflag(i,m) .eq. 1) goto 46
Cjhb=&==================================================================
            select case (isuply)
c             if (isuply.eq.2.or.isuply.eq.3) then
              case (2,3) !(isuply)
Cjhb=&==================================================================
c               isuply mode 2 (water rights) or 3 (water rights and return flows)
c               (but no groundwater supply)
c               -----------------------------------------------
c               in these isuply scenarios (2 and 3), there are water right priorities
c               so the farm deliveries are stored in the senior, other, and junior variables: holdfs, holdfo, holdfj
c               add them up to get the total sw source farm deliveries, swshare
Cjhb=&==================================================================
c               -----------------------------------------------
c               prorate the 3 "colors" of farm deliv (sr-holdfs, jr-holdfj,
c               oth-holdfo) to the four irrig land categories based on acreage
c               -----------------------------------------------
                if(t_area(i,m).gt.0.0)then
                  sfshares=holdfs*swflac(i,m)/t_area(i,m)
                  sfsharej=holdfj*swflac(i,m)/t_area(i,m)
                  sfshareo=holdfo*swflac(i,m)/t_area(i,m)
                else
                  sfshares=holdfs
                  sfsharej=holdfj
                  sfshareo=holdfo
                endif
                sfshare=sfshares+sfsharej+sfshareo
c               -----------------------------------------------
                if(t_area(i,m).gt.0.0)then
                  ssshares=holdfs*swspac(i,m)/t_area(i,m)
                  sssharej=holdfj*swspac(i,m)/t_area(i,m)
                  ssshareo=holdfo*swspac(i,m)/t_area(i,m)
                else
                  ssshares=0.0
                  sssharej=0.0
                  ssshareo=0.0
                endif
                ssshare=ssshares+sssharej+ssshareo
c               -----------------------------------------------
                swshare=sfshare+ssshare
c               -----------------------------------------------
                if(t_area(i,m).gt.0.0)then
                  gfshares=holdfs*swgwflac(i,m)/t_area(i,m)
                  gfsharej=holdfj*swgwflac(i,m)/t_area(i,m)
                  gfshareo=holdfo*swgwflac(i,m)/t_area(i,m)
                else
                  gfshares=0.0
                  gfsharej=0.0
                  gfshareo=0.0
                endif
                gfshare=gfshares+gfsharej+gfshareo
c               -----------------------------------------------
                if(t_area(i,m).gt.0.0)then
                  gsshares=holdfs*swgwspac(i,m)/t_area(i,m)
                  gssharej=holdfj*swgwspac(i,m)/t_area(i,m)
                  gsshareo=holdfo*swgwspac(i,m)/t_area(i,m)
                else
                  gsshares=0.0
                  gssharej=0.0
                  gsshareo=0.0
                endif
                gsshare=gsshares+gssharej+gsshareo
c               -----------------------------------------------
                gwshare=gfshare+gsshare
c               -----------------------------------------------
c               calculate actual cu for each land category (up
c               to max of the irrig water requirement for that category)
c               -----------------------------------------------
c               swcu=min(swshare*fleff(i,m),swreq(m,l))
                sfcu=min(max(sfshare*fleff(i,m),0.),sfreq(m,l))
                sscu=min(max(ssshare*speff(i,m),0.),ssreq(m,l))
                swcu=sfcu+sscu
                gfcu=min(max(gfshare*fleff(i,m),0.),gfreq(m,l))
                gscu=min(max(gsshare*speff(i,m),0.),gsreq(m,l))
                gcu=gfcu+gscu
c               -----------------------------------------------
c             endif
Cjhb=&==================================================================
c             if (isuply.eq.1.or.isuply.eq.4) then
              case (1,4)
c               isuply mode 1 (supply limited) or 4 (groundwater)
c               -----------------------------------------------
c               note that in these isuply scenarios (1 and 4), there are no
c               water right priorities, so all the farm deliveries from
c               surface stream diversions are stored in the senior variable, holdfs
c               however, holdfo still contains any tailwater input data
Cjhb=&==================================================================
c               if(gmode(i,m) .eq. 2) then
                select case (gmode(i,m))
                  case (2)
Cjhb=&==================================================================
c                   gw mode 2 - mutual ditch operation
Cjhb=&==================================================================
c                   swshare=holdfs*(1-gper(i,m))
c                   gsshare=holdfs*gper(i,m)*sper(i,m)
c                   gfshare=holdfs*gper(i,m)*(1-sper(i,m))
c                   -----------------------------------------------
c                   prorate the 3 "colors" of farm deliv (sr-holdfs, jr-holdfj,
c                   oth-holdfo) to the four irrig land categories based on acreage
c                   -----------------------------------------------
                    if(t_area(i,m).gt.0.0)then
                      sfshares=holdfs*swflac(i,m)/t_area(i,m)
                      sfsharej=holdfj*swflac(i,m)/t_area(i,m)
                      sfshareo=holdfo*swflac(i,m)/t_area(i,m)
                    else
                      sfshares=holdfs
                      sfsharej=holdfj
                      sfshareo=holdfo
                    endif
                    sfshare=sfshares+sfsharej+sfshareo
c                   -----------------------------------------------
                    if(t_area(i,m).gt.0.0)then
                      ssshares=holdfs*swspac(i,m)/t_area(i,m)
                      sssharej=holdfj*swspac(i,m)/t_area(i,m)
                      ssshareo=holdfo*swspac(i,m)/t_area(i,m)
                    else
                      ssshares=0.0
                      sssharej=0.0
                      ssshareo=0.0
                    endif
                    ssshare=ssshares+sssharej+ssshareo
c                   -----------------------------------------------
                    swshare=sfshare+ssshare
c                   -----------------------------------------------
                    if(t_area(i,m).gt.0.0)then
                      gfshares=holdfs*swgwflac(i,m)/t_area(i,m)
                      gfsharej=holdfj*swgwflac(i,m)/t_area(i,m)
                      gfshareo=holdfo*swgwflac(i,m)/t_area(i,m)
                    else
                      gfshares=0.0
                      gfsharej=0.0
                      gfshareo=0.0
                    endif
                    gfshare=gfshares+gfsharej+gfshareo
c                   -----------------------------------------------
                    if(t_area(i,m).gt.0.0)then
                      gsshares=holdfs*swgwspac(i,m)/t_area(i,m)
                      gssharej=holdfj*swgwspac(i,m)/t_area(i,m)
                      gsshareo=holdfo*swgwspac(i,m)/t_area(i,m)
                    else
                      gsshares=0.0
                      gssharej=0.0
                      gsshareo=0.0
                    endif
                    gsshare=gsshares+gssharej+gsshareo
c                   -----------------------------------------------
                    gwshare=gfshare+gsshare
c                   -----------------------------------------------
c                   swcu=min(swshare*fleff(i,m),swreq(m,l))
c                   gscu=min(gsshare*speff(i,m),gsreq(m,l))
c                   gfcu=min(gfshare*fleff(i,m),gfreq(m,l))
c                   -----------------------------------------------
c                   now convert the farm deliveries into cu values for each category
c                   -----------------------------------------------
                    sfcu=min(max(sfshare*fleff(i,m),0.),sfreq(m,l))
                    sscu=min(max(ssshare*speff(i,m),0.),ssreq(m,l))
                    swcu=sfcu+sscu
c                   -----------------------------------------------
                    gfcu=min(max(gfshare*fleff(i,m),0.),gfreq(m,l))
                    gscu=min(max(gsshare*speff(i,m),0.),gsreq(m,l))
                    gcu=gfcu+gscu
c                   -----------------------------------------------
                  case (3)
c                 elseif(gmode(i,m) .eq. 3) then
Cjhb=&==================================================================
c                   gw mode 3 - mutual ditch w/ sw for gw spr acreage to recharge and not CU
Cjhb=&==================================================================
c                   prorate the 3 "colors" of farm deliv (sr-holdfs, jr-holdfj,
c                   oth-holdfo) to the four irrig land categories based on acreage
c                   -----------------------------------------------
                    if(t_area(i,m).gt.0.0)then
                      sfshares=holdfs*swflac(i,m)/t_area(i,m)
                      sfsharej=holdfj*swflac(i,m)/t_area(i,m)
                      sfshareo=holdfo*swflac(i,m)/t_area(i,m)
                    else
                      sfshares=holdfs
                      sfsharej=holdfj
                      sfshareo=holdfo
                    endif
                    sfshare=sfshares+sfsharej+sfshareo
c                   -----------------------------------------------
                    if(t_area(i,m).gt.0.0)then
                      ssshares=holdfs*swspac(i,m)/t_area(i,m)
                      sssharej=holdfj*swspac(i,m)/t_area(i,m)
                      ssshareo=holdfo*swspac(i,m)/t_area(i,m)
                    else
                      ssshares=0.0
                      sssharej=0.0
                      ssshareo=0.0
                    endif
                    ssshare=ssshares+sssharej+ssshareo
c                   -----------------------------------------------
                    swshare=sfshare+ssshare
c                   -----------------------------------------------
                    if(t_area(i,m).gt.0.0)then
                      gfshares=holdfs*swgwflac(i,m)/t_area(i,m)
                      gfsharej=holdfj*swgwflac(i,m)/t_area(i,m)
                      gfshareo=holdfo*swgwflac(i,m)/t_area(i,m)
                    else
                      gfshares=0.0
                      gfsharej=0.0
                      gfshareo=0.0
                    endif
                    gfshare=gfshares+gfsharej+gfshareo
c                   -----------------------------------------------
                    if(t_area(i,m).gt.0.0)then
                      gsshares=holdfs*swgwspac(i,m)/t_area(i,m)
                      gssharej=holdfj*swgwspac(i,m)/t_area(i,m)
                      gsshareo=holdfo*swgwspac(i,m)/t_area(i,m)
                    else
                      gsshares=0.0
                      gssharej=0.0
                      gsshareo=0.0
                    endif
                    gsshare=gsshares+gssharej+gsshareo
c                   -----------------------------------------------
                    gwshare=gfshare+gsshare
c                   -----------------------------------------------
c                   sw for gw spr acreage goes to recharge, not cu
c                   -----------------------------------------------
                    arech(m,l)=gsshare
                    arech(m,13)=arech(m,13)+arech(m,l)
                    arech(nyrs1,l)=arech(nyrs1,l)+arech(m,l)
c                   -----------------------------------------------
c                   swcu=min(swshare*fleff(i,m),swreq(m,l))
c                   gscu=min(gsshare*speff(i,m),gsreq(m,l))
c                   gfcu=min(gfshare*fleff(i,m),gfreq(m,l))
c                   -----------------------------------------------
                    sfcu=min(max(sfshare*fleff(i,m),0.),sfreq(m,l))
                    sscu=min(max(ssshare*speff(i,m),0.),ssreq(m,l))
                    swcu=sfcu+sscu
c                   -----------------------------------------------
c                   reset the gw spr deliveries to 0 (went to recharge)
c                   -----------------------------------------------
                    gsshare=0.0
                    gsshares=0.0
                    gssharej=0.0
                    gsshareo=0.0
c                   -----------------------------------------------
                    gwshare=gfshare+gsshare
c                   -----------------------------------------------
                    gfcu=min(max(gfshare*fleff(i,m),0.),gfreq(m,l))
                    gscu=0.0 !no cu from sw since no deliv from sw
                    gcu=gfcu+gscu
c                   -----------------------------------------------
                  case (1)
c                 else
Cjhb=&==================================================================
c                   gw mode 1 - maximize supply - no sw for gw spr acreage,
c                   -----------------------------------------------
c                   note this is different than all the other scenarios;
c                   the combined farm deliveries are not spread over all
c                   the structure's acreage.  Instead they are first put on
c                   sw only sprinkler lands (typically more efficient), then on
c                   sw only flood lands, then the remaining on sw/gw flood
c                   lands.  No surface source water is put on sw/gw sprinkler lands.
Cjhb=&==================================================================
c                   swshare=holdfs
c                   swcu=min(swshare*fleff(i,m),swreq(m,l))
c                   -------------------------
c                   sw only sprinkler lands first
c                   -------------------------
                    ssshare=holdfs+holdfj+holdfo
                    sscu=min(max(ssshare*speff(i,m),0.),ssreq(m,l))
                    ssshare=sscu/speff(i,m)
c                   -------------------------
c                   split the sw only spr deliv into sr, jr, oth
c                   note that this applies all 3 classes to each
c                   land type in sequence as opposed to sr first, etc.
c                   -------------------------
                    if(holdfs+holdfj+holdfo.gt.0.0)then
                      ssshares=ssshare*holdfs/(holdfs+holdfj+holdfo)
                      sssharej=ssshare*holdfj/(holdfs+holdfj+holdfo)
                      ssshareo=ssshare*holdfo/(holdfs+holdfj+holdfo)
                    else
                      ssshares=0.0
                      sssharej=0.0
                      ssshareo=0.0
                    endif
c                   -------------------------
c                   then sw only flood lands
c                   -------------------------
                    sfshare=holdfs+holdfj+holdfo-ssshare
                    sfcu=min(max(sfshare*fleff(i,m),0.),sfreq(m,l))
                    sfshare=sfcu/fleff(i,m)
c                   -------------------------
                    if(holdfs+holdfj+holdfo.gt.0.0)then
                      sfshares=sfshare*holdfs/(holdfs+holdfj+holdfo)
                      sfsharej=sfshare*holdfj/(holdfs+holdfj+holdfo)
                      sfshareo=sfshare*holdfo/(holdfs+holdfj+holdfo)
                    else
                      sfshares=0.0
                      sfsharej=0.0
                      sfshareo=0.0
                    endif
c                   -------------------------
                    swshare=sfshare+ssshare
                    swcu=sscu+sfcu
c                   -------------------------
c                   then sw and gw flood lands - gets all that is left
c                   -------------------------
                    gfshare=holdfs+holdfj+holdfo-ssshare-sfshare
                    gfcu=min(max(gfshare*fleff(i,m),0.),gfreq(m,l))
c                   -------------------------
                    if(holdfs+holdfj+holdfo.gt.0.0)then
                      gfshares=gfshare*holdfs/(holdfs+holdfj+holdfo)
                      gfsharej=gfshare*holdfj/(holdfs+holdfj+holdfo)
                      gfshareo=gfshare*holdfo/(holdfs+holdfj+holdfo)
                    else
                      gfshares=0.0
                      gfsharej=0.0
                      gfshareo=0.0
                    endif
c                   -------------------------
c                   sw and gw sprinkler lands get no sw (will get pumping supply later)
c                   -------------------------
                    gsshare=0.
                    gscu=0.
c                   -------------------------
                    if(holdfs+holdfj+holdfo.gt.0.0)then
                      gsshares=gsshare*holdfs/(holdfs+holdfj+holdfo)
                      gssharej=gsshare*holdfj/(holdfs+holdfj+holdfo)
                      gsshareo=gsshare*holdfo/(holdfs+holdfj+holdfo)
                    else
                      gsshares=0.0
                      gssharej=0.0
                      gsshareo=0.0
                    endif
c                   -------------------------
                    gwshare=gfshare+gsshare
                    gcu=gfcu+gscu
c                   -------------------------
c                 endif
c                   -------------------------
                  case default
                    write (*,*)'invalid value of gmode(i,m)',
     &                         i,m,gmode(i,m)
                    stop
                end select !(gmode(i,m))
c               -------------------------
              case default
                write (*,*)'invalid value of isuply, ',isuply
                stop
            end select !(isuply)
c           endif
Cjhb=&==================================================================
C           Now that the CU from surface sources has been determined...
C           RESET IWR ON LANDS THAT HAVE GW SUPPLIES TO BE THE UNMET IWR
Cjhb=&==================================================================
            gsreq(m,l) = gsreq(m,l) - gscu
            gsreqdef(m,l) = max(gsreqdef(m,l)-gscu,0.0)
            gfreq(m,l) = gfreq(m,l) - gfcu
            gfreqdef(m,l) = max(gfreqdef(m,l)-gfcu,0.0)
            gwreqdef(m,l) = gsreqdef(m,l) + gfreqdef(m,l)
Cjhb=&==================================================================
C          DISTRIBUTE THE CALCULATED CU TO THE SR, JR and OTH FARM DELIVERIES
C          note that so far we are still only dealing with farm deliveries
C          from surface water diversions
Cjhb=&==================================================================
46          continue
            if(DistByPriority)then
c             ----------------------------------------------------------
c             distribute the cu by priority - meet IWR first from sr deliv,
c             then from jr then last from other
c             this is the default behavior
c             ----------------------------------------------------------
              sfcus=min(sfcu,max(sfshares*fleff(i,m),0.))
              sfcuj=min(sfcu-sfcus,max(sfsharej*fleff(i,m),0.))
              sfcuo=min(sfcu-sfcus-sfcuj,max(sfshareo*fleff(i,m),0.)) !sfshareo could be negative - don't let sfcuo be neg though!
c             ----------------------------------------------------------
              sscus=min(sscu,max(ssshares*speff(i,m),0.))
              sscuj=min(sscu-sscus,max(sssharej*speff(i,m),0.))
              sscuo=min(sscu-sscus-sscuj,max(ssshareo*speff(i,m),0.)) !ssshareo could be negative - don't let sscuo be neg though!
c             ----------------------------------------------------------
              gfcus=min(gfcu,max(gfshares*fleff(i,m),0.))
              gfcuj=min(gfcu-gfcus,max(gfsharej*fleff(i,m),0.))
              gfcuo=min(gfcu-gfcus-gfcuj,max(gfshareo*fleff(i,m),0.)) !gfshareo could be negative - don't let gfcuo be neg though!
c             ----------------------------------------------------------
              gscus=min(gscu,max(gsshares*speff(i,m),0.))
              gscuj=min(gscu-gscus,max(gssharej*speff(i,m),0.))
              gscuo=min(gscu-gscus-gscuj,max(gsshareo*speff(i,m),0.)) !gsshareo could be negative - don't let gscuo be neg though!
c             ----------------------------------------------------------
            else
c             ----------------------------------------------------------
c             distribute the cu to each priority
c             prorate equally based on the delivery
c             ----------------------------------------------------------
c             this method is NOT used in any scenario,
c               so make sure it stops the code if it ends up here...
              write(*,*)
     &          'invalid value of DistByPriority ',DistByPriority
              write(999,*)
     &          'invalid value of DistByPriority ',DistByPriority
              stop
c             ----------------------------------------------------------
              if(sfshare.eq.0.0)then
                sfcus=0.0
                sfcuj=0.0
                sfcuo=0.0
              else
                sfcus=sfcu*sfshares/sfshare
                sfcuj=sfcu*sfsharej/sfshare
                sfcuo=sfcu*sfshareo/sfshare
              endif               
c             ----------------------------------------------------------
              if(ssshare.eq.0.0)then
                sscus=0.0
                sscuj=0.0
                sscuo=0.0
              else
                sscus=sscu*ssshares/ssshare
                sscuj=sscu*sssharej/ssshare
                sscuo=sscu*ssshareo/ssshare
              endif               
c             ----------------------------------------------------------
              if(gfshare.eq.0.0)then
                gfcus=0.0
                gfcuj=0.0
                gfcuo=0.0
              else
                gfcus=gfcu*gfshares/gfshare
                gfcuj=gfcu*gfsharej/gfshare
                gfcuo=gfcu*gfshareo/gfshare
              endif               
c             ----------------------------------------------------------
              if(gsshare.eq.0.0)then
                gscus=0.0
                gscuj=0.0
                gscuo=0.0
              else
                gscus=gscu*gsshares/gsshare
                gscuj=gscu*gssharej/gsshare
                gscuo=gscu*gsshareo/gsshare
              endif               
c             ----------------------------------------------------------
            endif
Cjhb=&==================================================================
C           ADD UP THE CU VALUES TO GET A TOTAL SR, JR AND OTHER CU
Cjhb=&==================================================================
            holds=sfcus+sscus+gfcus+gscus
            holdj=sfcuj+sscuj+gfcuj+gscuj
            holdo=sfcuo+sscuo+gfcuo+gscuo
c           ------------------------------------------------------------
            crop_cus(m,l)=crop_cus(m,l)+holds
            crop_cus(m,13)=crop_cus(m,13)+holds
            crop_cus(nyrs1,l)=crop_cus(nyrs1,l)+holds
c           ------------------------------------------------------------
            crop_cuj(m,l)=crop_cuj(m,l)+holdj
            crop_cuj(m,13)=crop_cuj(m,13)+holdj
            crop_cuj(nyrs1,l)=crop_cuj(nyrs1,l)+holdj
c           ------------------------------------------------------------
            crop_cuo(m,l) = crop_cuo(m,l)+holdo
            crop_cuo(m,13)=crop_cuo(m,13)+holdo
            crop_cuo(nyrs1,l)=crop_cuo(nyrs1,l)+holdo
Cjhb=&==================================================================
C           TOTAL CU FROM SURFACE SOURCES
Cjhb=&==================================================================
            holdt = holds+holdj+holdo
            crop_cut(m,l) =crop_cut(m,l)+ holdt
            crop_cut(m,13)=crop_cut(m,13)+holdt
            crop_cut(nyrs1,l)=crop_cut(nyrs1,l)+holdt
Cjhb=&==================================================================
Cjhb Feb 2011 Ack - this was bad, bad nomenclature!!
Cjhb Feb 2011   this is actually EXCESS EFFICIENT DELIVERIES, so the
Cjhb Feb 2011   variables are BADLY named.  Should have been xseff or something similar
C           Determine unused (inefficient) farm deliveries from surface water sources
C           sfinefs - 's'w only, 'f'lood irrigated, 's'enior - 'inef'ficient farm deliveries
C           sfinefsx- sw only, flood irrigated, senior - inef soil deliveries
C           etc.
Cjhb=&==================================================================
            sfinefs=max(0.0,sfshares-sfcus/fleff(i,m))
            sfinefsx=sfinefs*fleff(i,m)
            sfinefj=max(0.0,sfsharej-sfcuj/fleff(i,m))
            sfinefjx=sfinefj*fleff(i,m)
            sfinefo=max(0.0,sfshareo-sfcuo/fleff(i,m))
            sfinefox=sfinefo*fleff(i,m)
            sfinef =sfinefs +sfinefj +sfinefo
            sfinefx=sfinefsx+sfinefjx+sfinefox
c           ------------------------------------------------------------
            ssinefs=max(0.0,ssshares-sscus/speff(i,m))
            ssinefsx=ssinefs*speff(i,m)
            ssinefj=max(0.0,sssharej-sscuj/speff(i,m))
            ssinefjx=ssinefj*speff(i,m)
            ssinefo=max(0.0,ssshareo-sscuo/speff(i,m))
            ssinefox=ssinefo*speff(i,m)
            ssinef =ssinefs +ssinefj +ssinefo
            ssinefx=ssinefsx+ssinefjx+ssinefox
c           ------------------------------------------------------------
            gfinefs=max(0.0,gfshares-gfcus/fleff(i,m))
            gfinefsx=gfinefs*fleff(i,m)
            gfinefj=max(0.0,gfsharej-gfcuj/fleff(i,m))
            gfinefjx=gfinefj*fleff(i,m)
            gfinefo=max(0.0,gfshareo-gfcuo/fleff(i,m))
            gfinefox=gfinefo*fleff(i,m)
            gfinef =gfinefs +gfinefj +gfinefo
            gfinefx=gfinefsx+gfinefjx+gfinefox
c           ------------------------------------------------------------
            gsinefs=max(0.0,gsshares-gscus/speff(i,m))
            gsinefsx=gsinefs*speff(i,m)
            gsinefj=max(0.0,gssharej-gscuj/speff(i,m))
            gsinefjx=gsinefj*speff(i,m)
            gsinefo=max(0.0,gsshareo-gscuo/speff(i,m))
            gsinefox=gsinefo*speff(i,m)
            gsinef =gsinefs +gsinefj +gsinefo
            gsinefx=gsinefsx+gsinefjx+gsinefox
c           ------------------------------------------------------------
            inefs=sfinefsx+ssinefsx+gfinefsx+gsinefsx
            inefj=sfinefjx+ssinefjx+gfinefjx+gsinefjx
            inefo=sfinefox+ssinefox+gfinefox+gsinefox
Cjhb=&==================================================================
C           CALCULATE SENIOR DIVERSIONS INTO SOIL MOISTURE
Cjhb=&==================================================================
C           current soil moisture space available
C           soil moisture calculations for whole structure
Cjhb=&==================================================================
            if(iprtysm.eq.1)then !use priorities in soil moisture accounting
              smspc=max(0.0,spcapz-soiltots) !move out old jr and old other water to make room for new senior water
            else !do not use priorities in soil moisture accounting
              smspc=max(0.0,spcapz-soiltot) !don't move out any water old water; only fill empty space
            endif
Cjhb=&==================================================================
            holds1=min(inefs,smspc) !minimum of inef senior soil deliv and available soil space
Cjhb=&==================================================================
c           grb 5-7-00 add following test to prevent small rounding errors
            if (holds1.gt.-.1.and.holds1.lt.0.1) holds1=0 
Cjhb=&==================================================================
            soil_cus(m,l)     = soil_cus(m,l)     + holds1
            soil_cus(m,13)    = soil_cus(m,13)    + holds1
            soil_cus(nyrs1,l) = soil_cus(nyrs1,l) + holds1
Cjhb=&==================================================================
C           calculate existing junior and other water that have been pushed out
Cjhb=&==================================================================
            if(iprtysm.eq.1)then !use priorities in soil moisture accounting
c             pushed out jr sm = jr before - max possible jr after
c             pushed out jr sm = jr before - (spcapz-new sr total sm)
c             pushed out jr sm = soiltotj - (spcapz-(soiltots+holds1))
              pojsm=max(0.0,soiltotj-(spcapz-(soiltots+holds1)))          !pushed out junior water by senior
c             pushed out oth sm = oth before - max possible oth after
cjhb Feb 2011 FIXED a logic problem in the following code that occasionally caused
cjhb Feb 2011   an errant "pushed out other" value that resulted in
cjhb Feb 2011   negative soil moisture values
c             pushed out oth sm = oth before - (spcapz-(new sr total sm+new jr total)
c             pushed out oth sm = soiltoto - (spcapz-(soiltots+holds1+(soiltotj-pojsm))
              poosm=max(0.0,
     &                  soiltoto-(spcapz-(soiltots+holds1+
     &                                            (soiltotj-pojsm))))    !pushed out other water by senior
            else
              pojsm=0.0                                                  !pushed out junior water by senior
              poosm=0.0                                                  !pushed out other water by senior
            endif
            soiltotj = soiltotj-pojsm                                    !this is ok, since either pojsm or holdj1 is >0 but not both
            soiltoto = soiltoto-poosm                                    !this is ok, since either poosm or holdo1 is >0 but not both
Cjhb=&==================================================================
C           CALCULATE JUNIOR DIVERSIONS INTO SOIL MOISTURE
Cjhb=&==================================================================
C           current soil moisture space available
C           soil moisture calculations for whole structure
Cjhb=&==================================================================
            if(iprtysm.eq.1)then !use priorities in soil moisture accounting
              smspc=max(0.0,spcapz-(soiltots+holds1)-(soiltotj))        !move out other water to make room for new junior water
            else !do not use priorities in soil moisture accounting
              smspc=max(0.0,spcapz-(soiltot+holds1))
            endif
Cjhb=&==================================================================
            holdj1=min(inefj,smspc) !minimum of inef junior soil deliv and soil space
Cjhb=&==================================================================
c           grb 5-7-00 add following test to prevent small rounding errors
            if (holdj1.gt.-.1.and.holdj1.lt.0.1) holdj1=0 
Cjhb=&==================================================================
            soil_cuj(m,l)     = soil_cuj(m,l)     + holdj1
            soil_cuj(m,13)    = soil_cuj(m,13)    + holdj1
            soil_cuj(nyrs1,l) = soil_cuj(nyrs1,l) + holdj1
Cjhb=&==================================================================
C           calculate additional existing other water that has been pushed out by junior
Cjhb=&==================================================================
            if(iprtysm.eq.1)then !use priorities in soil moisture accounting
c             pushed out oth sm = oth before - max possible oth after
c             pushed out oth sm = oth before - (total sm space - (new sr total sm + new jr total))
c             pushed out oth sm = soiltoto - (spcapz-(soiltots+holds1+soiltotj+holdj1))
              poosmbj=max(0.0,
     &              soiltoto-(spcapz-(soiltots+holds1+soiltotj+holdj1))) !pushed out other water by junior
            else
              poosmbj=0.0                                                !pushed out other water by junior
            endif
            soiltoto = soiltoto-poosmbj                                  !this is ok, since either poosm or holdo1 is >0 but not both (if water was pushed out, then there is no room to add!)
Cjhb=&==================================================================
C           CALCULATE OTHER DIVERSIONS INTO SOIL MOISTURE
Cjhb=&==================================================================
C           current soil moisture space available
C           soil moisture calculations for whole structure
Cjhb=&==================================================================
            if(iprtysm.eq.1)then !use priorities in soil moisture accounting
              smspc=max(0.0,spcapz-(soiltots+holds1)
     &                            -(soiltotj+holdj1)
     &                            - soiltoto)                            !calculate remaining soil moisture space for new other water, if any
            else !do not use priorities in soil moisture accounting, but its the same in this case...
              smspc=max(0.0,spcapz-(soiltots+holds1)
     &                            -(soiltotj+holdj1)
     &                            - soiltoto)                            !calculate remaining soil moisture space for new other water, if any
            endif
Cjhb=&==================================================================
            holdo1=min(inefo,smspc) !minimum of inef junior soil deliv and soil space
Cjhb=&==================================================================
c           grb 5-7-00 add following test to prevent small rounding errors
            if (holdo1.gt.-.1.and.holdo1.lt.0.1) holdo1=0 
Cjhb=&==================================================================
            soil_cuo(m,l)     = soil_cuo(m,l)     + holdo1
            soil_cuo(m,13)    = soil_cuo(m,13)    + holdo1
            soil_cuo(nyrs1,l) = soil_cuo(nyrs1,l) + holdo1
Cjhb=&==================================================================
Cjhb          create new array storing pushed out "other" soil moisture
Cjhb=&==================================================================
            soil_cujout(m,l)=soil_cujout(m,l)+pojsm
            soil_cujout(m,13)=soil_cujout(m,13)+pojsm
           soil_cujout(nyrs1,1)=soil_cujout(nyrs1,l)+pojsm
            soil_cuoout(m,l)=soil_cuoout(m,l)+poosm+poosmbj
            soil_cuoout(m,13)=soil_cuoout(m,13)+poosm+poosmbj
           soil_cuoout(nyrs1,1)=soil_cuoout(nyrs1,l)+poosm+poosmbj
Cjhb=&==================================================================

Cjhb=&==================================================================
C           OLD CODE
cCjhb=&==================================================================
cC-----------------Senior Diversion to Soil Zone
c             if(gmode(i,m) .eq. 1) then
cCjhb=&==================================================================
cc              gw mode 1 - maximize supply - no sw for gw spr acreage,
cc              put on spr acreage first
cCjhb=&==================================================================
cc              LftOvr=[senior farm deliv] - [total used deliv]
cc              LftOvr=[left-over senior water]
c               LftOvr=holdfs-(sfcu+gfcu)/fleff(i,m)-sscu/speff(i,m)
cc              LftOvrSF=[left-over senior water]*[sw fl acres]/([sw fl acres]+[sw sp acres]+[gw fl acres])
cc              LftOvrSF=excess senior water distr to sw fl, sw sp, gw fl, but NOT gw sp 
c               LftOvrSF=LftOvr*(swflac(i,m))
c     &                 /(swflac(i,m)+swgwflac(i,m)+swspac(i,m))
cc              LftOvrSS=[left-over senior water]*[sw sp acres]/([sw fl acres]+[sw sp acres]+[gw fl acres])
c               LftOvrSS=LftOvr*(swspac(i,m))
c     &                 /(swflac(i,m)+swgwflac(i,m)+swspac(i,m))
c               sfshare=max(0.,LftOvrSF*fleff(i,m))
c               ssshare=max(0.,LftOvrSS*speff(i,m))
c               swshare=sfshare+ssshare
cc              LftOvrGF=[left-over senior water]*[gw fl acres]/([sw fl acres]+[sw sp acres]+[gw fl acres])
c               LftOvrGF=LftOvr*(swgwflac(i,m))
c     &                 /(swflac(i,m)+swgwflac(i,m)+swspac(i,m))
c               gfshare=max(0.,LftOvrGF*fleff(i,m))
c               gsshare=0.
c               gwshare=gfshare+gsshare
c             else
cCjhb=&==================================================================
cc              gw mode 2 or 3 - mutual ditch
cCjhb=&==================================================================
c               sfshare=max(0.,(sfshare-sfcu/fleff(i,m))*fleff(i,m))
c               ssshare=max(0.,(ssshare-sscu/speff(i,m))*speff(i,m))
c               swshare=sfshare+ssshare
c               gfshare=max(0.,(gfshare-gfcu/fleff(i,m))*fleff(i,m))
c               gsshare=max(0.,(gsshare-gscu/speff(i,m))*speff(i,m))
c               gwshare=gfshare+gsshare
c             endif
cCjhb=&==================================================================
c             if(isuply.eq.1.or.isuply.eq.4) holds1 =
c     :		   min(swshare+gfshare+gsshare,(spcapz-soiltot))
c             if((isuply.eq.2.or.isuply.eq.3).and.iprtysm.eq.0) holds1= 
c     :		   min(holdfs*wghteff-holds,(spcapz-soiltot))
cc grb 5-9-00 add following line for senior water to soil zone if water rights operated
c             If((isuply.eq.2.or.isuply.eq.3).and.iprtysm.eq.1) holds1=
c     :          min(holdfs*wghteff-holds,(spcapz-soiltots-soiltoto))
cCjhb=&==================================================================
cc grb 5-7-00 add following test to prevent small rounding errors
c             if (holds1.gt.-.1.and.holds1.lt.0.1) holds1=0 
cCjhb=&==================================================================
c             soil_cus(m,l) = soil_cus(m,l) +holds1
c             soil_cus(m,13)=soil_cus(m,13)+holds1
c             soil_cus(nyrs1,l)=soil_cus(nyrs1,l)+holds1
cCjhb=&==================================================================
cc grb add logic for senior water pushing out junior water - following 10 lines
c             pojsm=0
c             if (iprtysm.gt.0.and.(isuply.eq.2.or.isuply.eq.3)) then
cc grb 05-25-01 add hold1 to following statement to avoid exceed soil capacity
c                if ((soiltot+holds1).gt.spcapz) then
c                     pojsm=soiltot+holds1-spcapz
c                endif
c                if (pojsm.le.0.and.pojsm.ge.-.1) pojsm=0
c                soil_cujout(m,l)=soil_cujout(m,l)+pojsm
c                soil_cujout(m,13)=soil_cujout(m,13)+pojsm
c                soil_cujout(nyrs1,1)=soil_cujout(nyrs1,l)+pojsm
c                soiltotj=soiltotj-pojsm
c                soiltot=soiltot-pojsm
c             endif
cC-----------------Junior Diversion to Soil Zone                               
c             holdj1=min(holdfj*wghteff-holdj,spcapz-
c     :                  (soiltot+ holds1))
c             if (holdj1.gt.-.1.and.holdj1.lt.0.1) holdj1=0 
c             soil_cuj(m,l) = soil_cuj(m,l)+holdj1
c             soil_cuj(m,13)=soil_cuj(m,13)+holdj1
c             soil_cuj(nyrs1,l)=soil_cuj(nyrs1,l)+holdj1
cC-----------------Other Diversion to Soil Zone              
c             holdo1 = min(holdfo*wghteff-holdo,spcapz - (soiltot+ 
c     :               holds1 + holdj1))
c             if (holdo1.gt.-.1.and.holdo1.lt.0.1) holdo1=0 
c             soil_cuo(m,l) = soil_cuo(m,l)+holdo1
c             soil_cuo(m,13)=soil_cuo(m,13)+holdo1
c             soil_cuo(nyrs1,l)=soil_cuo(nyrs1,l)+holdo1
cCjhb=&==================================================================

Cjhb=&==================================================================
C           CALCULATE TOTAL DIVERSIONS INTO SOIL MOISTURE
Cjhb=&==================================================================
C-----------------Total Diversion to Soil Zone                       
            holdt1 = holds1+holdj1+holdo1
            soil_cu(m,l)     = soil_cus(m,l)+soil_cuj(m,l)+soil_cuo(m,l)
            soil_cu(m,13)    = soil_cu(m,13)    + holdt1
            soil_cu(nyrs1,l) = soil_cu(nyrs1,l) + holdt1
Cjhb        ------------------------------------------------------------
c           grb 05-11-00 adding additional check on soil moisture contents - next six lines
            if(soil_cu(m,l).lt.(-0.05)) then
              write(0,*)
     &          "Stopping in WSUPSUM-soil moisture less than zero"
              write(0,*) "i=",i,"m=",m,"l=",l
              write(0,*)
     &          "itime=",itime,"imiss2=",imiss2,"ipresim=",ipresim
              write(0,*) "soil_cu(m,l)=",soil_cu(m,l)
              write(0,*) "soil_cus(m,l)=",soil_cus(m,l)
              write(0,*) "soil_cuj(m,l)=",soil_cuj(m,l)
              write(0,*) "soil_cuo(m,l)=",soil_cuo(m,l)
              write(0,*) "holdfj=",holdfj
              write(0,*) "holdj=",holdj
              write(0,*) "holds1=",holds1
              write(0,*) "holdj1=",holdj1
              write(0,*) "fleff(i,m)=",fleff(i,m)
              write(0,*) "spcapz=",spcapz
              write(0,*) "soiltot=",soiltot
              stop
            endif       
Cjhb        ------------------------------------------------------------
C--         update soil moisture contents for diversions to soil
Cjhb        ------------------------------------------------------------
            soiltots = soiltots + holds1
            soiltotj = soiltotj + holdj1
            soiltoto = soiltoto + holdo1
            soiltot  = soiltots + soiltotj + soiltoto
Cjhb=&==================================================================

Cjhb=&==================================================================
C           CALCULATE DIVERSIONS TO UNMET IWR FROM SOIL MOISTURE
Cjhb=&==================================================================
c           grb 5-05-2000 need initialization of swhold and other variables in next 3 lines
            sfhold=0.0
            sshold=0.0
            swhold=0.0
            gfhold=0.0
            gshold=0.0
Cjhb        ------------------------------------------------------------
Cjhb        check for unmet IWR
Cjhb        ------------------------------------------------------------
            IF (holds+holdj+holdo.lt.reqreq(m,l)) then !there is unmet IWR
Cjhb          ----------------------------------------------------------
Cjhb          old way
Cjhb          ----------------------------------------------------------
Cjhb          use soil moisture water to meet unmet IWR
Cjhb          first on sw fl, then on sw spr, then on gw fl, then on gw spr
Cjhb          gmode=1 or gmode=3, no soil moisture to gw spr land
Cjhb          ----------------------------------------------------------
c              sfhold=min(sfreq(m,l)-sfcu,
c     &                   soiltot)
Cjhb          ----------------------------------------------------------
c              sshold=min(ssreq(m,l)-sscu,
c     &                   soiltot-sfhold)
Cjhb          ----------------------------------------------------------
c              swhold=sfhold+sshold
Cjhb          ----------------------------------------------------------
Cjhb          note that gfreq(m,l) and gsreq(m,l) have ALREADY been reduced by the CU from sw sources
Cjhb          ----------------------------------------------------------
c              gfhold=min(gfreq(m,l),
c     &                   soiltot-sfhold-sshold)
Cjhb          ----------------------------------------------------------
c              select case (gmode(i,m))
c                case (1,3) !no soil moisture for gw spr cu
c                  gshold=0.0
c                case default
c                  gshold=min(gsreq(m,l),
c     &                       soiltot-sfhold-sshold-gfhold)
c              end select
Cjhb          ----------------------------------------------------------
Cjhb          new way
Cjhb          ----------------------------------------------------------
Cjhb          total soil moisture water available is prorated to meet
Cjhb            unmet IWR on each land category by acreage
Cjhb          note: if gmode=1 or gmode=3, no soil moisture to gw spr
Cjhb            land at all- split the total between the other three
Cjhb          ----------------------------------------------------------
              select case (gmode(i,m))
                case (1,3) !no soil moisture for gw spr cu
                  SMTotAcr=t_area(i,m)-swgwspac(i,m)
                case default
                  SMTotAcr=t_area(i,m)
              end select
Cjhb          ----------------------------------------------------------
              if(SMTotAcr.gt.0.0)then
Cjhb          ----------------------------------------------------------
Cjhb            sfhold=min(unmet IWR, avail soil moisture)
                sfhold=min(sfreq(m,l)-sfcu,soiltot*swflac(i,m)/SMTotAcr)
Cjhb            sshold=min(unmet IWR, avail soil moisture)
                sshold=min(ssreq(m,l)-sscu,soiltot*swspac(i,m)/SMTotAcr)
                swhold=sfhold+sshold
Cjhb            gfhold=min(unmet IWR, avail soil moisture)
                gfhold=min(gfreq(m,l),soiltot*swgwflac(i,m)/SMTotAcr)
Cjhb            --------------------------------------------------------
                select case (gmode(i,m))
                  case (1,3) !no soil moisture for gw spr cu
Cjhb                gshold=0.0
                    gshold=0.0
                  case default
Cjhb                gshold=min(unmet IWR, avail soil moisture)
                    gshold=min(gsreq(m,l),
     &                         soiltot*swgwspac(i,m)/SMTotAcr)
                end select
Cjhb            --------------------------------------------------------
c               old code
Cjhb            --------------------------------------------------------
c               withdraw water from soil zone, soiltot=contents       
c                swhold=min((swreq(m,l) - swcu),soiltots)
c                if (isuply.eq.2.or.isuply.eq.3) swhold=
c     :            min(swreq(m,l)-swcu,soiltot) 
c                gfhold=min(gfreq(m,l),(soiltots-swhold))
c                if(gmode(i,m) .ne. 2) then
c                  gshold = 0
c                else
c                  gshold=min(gsreq(m,l),(soiltots-swhold-gfhold))
c                endif
Cjhb            --------------------------------------------------------
              else
                sfhold=0.0
                sshold=0.0
                swhold=sfhold+sshold
                gfhold=0.0
                gshold=0.0
              endif
Cjhb          ----------------------------------------------------------
Cjhb          reset the IWR on gw lands to account for soil moisture meeting some CU
Cjhb          the remaining CU on gw lands (gfreq and gsreq) will be met by pumping (if possible) later in the code
Cjhb          ----------------------------------------------------------
              gfreq(m,l) = gfreq(m,l) - gfhold
              gfreqdef(m,l) = max(gfreqdef(m,l)-gfhold,0.0)
              gsreq(m,l) = gsreq(m,l) - gshold
              gsreqdef(m,l) = max(gsreqdef(m,l)-gshold,0.0)
              gwreqdef(m,l) = gfreqdef(m,l) + gsreqdef(m,l)
Cjhb          ----------------------------------------------------------
Cjhb          holdcrop = total CU from soil moisture
Cjhb          ----------------------------------------------------------
              holdcrop = sfhold + sshold + gfhold + gshold
Cjhb          ----------------------------------------------------------
cc             grb 5-10-00 include total soil moisture if operating water rights - correction to LRCWE - next two lines
c              if (isuply.eq.2.or.isuply.eq.3) holdcrop=
c     &          min(swreq(m,l)-swcu,soiltot) 
Cjhb          ----------------------------------------------------------
              cropcusoil(m,l)     = cropcusoil(m,l)     + holdcrop
              cropcusoil(m,13)    = cropcusoil(m,13)    + holdcrop
              cropcusoil(nyrs1,l) = cropcusoil(nyrs1,l) + holdcrop
Cjhb          ----------------------------------------------------------
c              if (soiltot.gt. 0) then ! this was to prevent division by 0 i think
Cjhb          --------------------------------------------------------
Cjhb          now split the total cu from soil moisture over the priority soil moisture pools
Cjhb          notice the order is now SENIOR then JUNIOR then OTHER if soil moisture priority accounting is on
Cjhb          (it used to be sr then other then jr)
Cjhb          otherwise just prorate accd to the relative size of the various pools
Cjhb          --------------------------------------------------------
              select case (iprtysm)
                case (1)
                  holdcrops = min(holdcrop,
     &                            soiltots)
                  holdcropj = min(holdcrop-holdcrops,
     &                            soiltotj)
                  holdcropo = min(holdcrop-holdcrops-holdcropj,
     &                            soiltoto)
                  if(holdcrop.gt.0.0)then
                    sfholds = holdcrops * (sfhold/holdcrop)
                    sfholdj = holdcropj * (sfhold/holdcrop)
                    sfholdo = holdcropo * (sfhold/holdcrop)
                    ssholds = holdcrops * (sshold/holdcrop)
                    ssholdj = holdcropj * (sshold/holdcrop)
                    ssholdo = holdcropo * (sshold/holdcrop)
                    gfholds = holdcrops * (gfhold/holdcrop)
                    gfholdj = holdcropj * (gfhold/holdcrop)
                    gfholdo = holdcropo * (gfhold/holdcrop)
                    gsholds = holdcrops * (gshold/holdcrop)
                    gsholdj = holdcropj * (gshold/holdcrop)
                    gsholdo = holdcropo * (gshold/holdcrop)
                  else
                    sfholds = 0.0
                    sfholdj = 0.0
                    sfholdo = 0.0
                    ssholds = 0.0
                    ssholdj = 0.0
                    ssholdo = 0.0
                    gfholds = 0.0
                    gfholdj = 0.0
                    gfholdo = 0.0
                    gsholds = 0.0
                    gsholdj = 0.0
                    gsholdo = 0.0
                  endif
                case default
                  if(soiltot.gt.0.0)then
                    holdcrops = holdcrop * (soiltots/soiltot)
                    holdcropj = holdcrop * (soiltotj/soiltot)
                    holdcropo = holdcrop * (soiltoto/soiltot)
                    sfholds = sfhold * (soiltots/soiltot)
                    sfholdj = sfhold * (soiltotj/soiltot)
                    sfholdo = sfhold * (soiltoto/soiltot)
                    ssholds = sshold * (soiltots/soiltot)
                    ssholdj = sshold * (soiltotj/soiltot)
                    ssholdo = sshold * (soiltoto/soiltot)
                    gfholds = gfhold * (soiltots/soiltot)
                    gfholdj = gfhold * (soiltotj/soiltot)
                    gfholdo = gfhold * (soiltoto/soiltot)
                    gsholds = gshold * (soiltots/soiltot)
                    gsholdj = gshold * (soiltotj/soiltot)
                    gsholdo = gshold * (soiltoto/soiltot)
                  else
                    holdcrops = 0.0
                    holdcropj = 0.0
                    holdcropo = 0.0
                    holdcrop = 0.0
                    sfholds = 0.0
                    sfholdj = 0.0
                    sfholdo = 0.0
                    ssholds = 0.0
                    ssholdj = 0.0
                    ssholdo = 0.0
                    gfholds = 0.0
                    gfholdj = 0.0
                    gfholdo = 0.0
                    gsholds = 0.0
                    gsholdj = 0.0
                    gsholdo = 0.0
                  endif
              end select
              cropcusoils(m,l)     = cropcusoils(m,l)     + holdcrops
              cropcusoils(m,13)    = cropcusoils(m,13)    + holdcrops
              cropcusoils(nyrs1,l) = cropcusoils(nyrs1,l) + holdcrops
              cropcusoilj(m,l)     = cropcusoilj(m,l)     + holdcropj
              cropcusoilj(m,13)    = cropcusoilj(m,13)    + holdcropj
              cropcusoilj(nyrs1,l) = cropcusoilj(nyrs1,l) + holdcropj
              cropcusoilo(m,l)     = cropcusoilo(m,l)     + holdcropo
              cropcusoilo(m,13)    = cropcusoilo(m,13)    + holdcropo
              cropcusoilo(nyrs1,l) = cropcusoilo(nyrs1,l) + holdcropo
c              endif
Cjhb          ----------------------------------------------------------
Cjhb          reset soil moisture pool values after being used to for unmet IWR
Cjhb          ----------------------------------------------------------
              soiltot  = soiltot  - holdcrop
              soiltots = soiltots - holdcrops
              soiltotj = soiltotj - holdcropj
              soiltoto = soiltoto - holdcropo
              if(soiltot  .gt. -0.001.and.soiltot .lt.0.001)soiltot =0.0
              if(soiltots .gt. -0.001.and.soiltots.lt.0.001)soiltots=0.0
              if(soiltotj .gt. -0.001.and.soiltotj.lt.0.001)soiltotj=0.0
              if(soiltoto .gt. -0.001.and.soiltoto.lt.0.001)soiltoto=0.0
            endif
Cjhb        ------------------------------------------------------------
Cjhb        soil moisture error checking
Cjhb        ------------------------------------------------------------
            if (soiltot.lt.(-.05)) then
              write(0,*) "error-soil<0",soiltot,holdcrop,spcapz
            endif
            if (soiltot.gt.(spcapz+.05)) then 
              write(0,*)"error-soil>cap",soiltot,holdt1,spcapz
              write(0,*) 'structure = ', twdid
c              pause
            endif
Cjhb        ------------------------------------------------------------
            soiltotts(m,l)      = soiltotts(m,l)      + soiltots
            soiltotts(m,13)     = soiltotts(m,13)     + soiltotts(m,l)
            soiltotts(nyrs1,l)  = soiltotts(nyrs1,l)  + soiltots
            soiltotts(nyrs1,13) = soiltotts(nyrs1,13) + soiltots
            soiltottj(m,l)      = soiltottj(m,l)      + soiltotj
            soiltottj(m,13)     = soiltottj(m,13)     + soiltottj(m,l)
            soiltottj(nyrs1,l)  = soiltottj(nyrs1,l)  + soiltotj
            soiltottj(nyrs1,13) = soiltottj(nyrs1,13) + soiltotj
            soiltotto(m,l)      = soiltotto(m,l)      + soiltoto
            soiltotto(m,13)     = soiltotto(m,13)     + soiltotto(m,l)
            soiltotto(nyrs1,l)  = soiltotto(nyrs1,l)  + soiltoto
            soiltotto(nyrs1,13) = soiltotto(nyrs1,13) + soiltoto
            soiltott(m,l)       = soiltott(m,l)       + soiltot
            soiltott(m,13)      = soiltott(m,13)      + soiltott(m,l) 
            soiltott(nyrs1,l)   = soiltott(nyrs1,l)   + soiltot
            soiltott(nyrs1,13)  = soiltott(nyrs1,13)  + soiltot
            wbu(i,nyrs1,13)     = wbu(i,nyrs1,13)     + wbu(i,m,l)
            wbu(i,nyrs1,l)      = wbu(i,nyrs1,l)      + wbu(i,m,l)
Cjhb        ------------------------------------------------------------
Cjhb        now estimate cu from sw source and soil moisture
Cjhb        ------------------------------------------------------------
C-----------------Senior Estimated Crop CU
47          holdests = holdcrops + holds
            estcrps(m,l)     = estcrps(m,l)     + holdests
            estcrps(m,13)    = estcrps(m,13)    + holdests
            estcrps(nyrs1,l) = estcrps(nyrs1,l) + holdests
C-----------------Junior Estimated Crop CU
            holdestj = holdcropj + holdj
            estcrpj(m,l)     = estcrpj(m,l)     + holdestj
            estcrpj(m,13)    = estcrpj(m,13)    + holdestj
            estcrpj(nyrs1,l) = estcrpj(nyrs1,l) + holdestj
C-----------------Other Estimated Crop CU
            holdesto = holdcropo + holdo
            estcrpo(m,l)     = estcrpo(m,l)     + holdesto
            estcrpo(m,13)    = estcrpo(m,13)    + holdesto
            estcrpo(nyrs1,l) = estcrpo(nyrs1,l) + holdesto
C-----------------Total Estimated Crop CU
            holdestt = holdcrops + holdcropj + holdcropo + holdt
            estcrpt(i,m,l)     = estcrpt(i,m,l)     + holdestt  
            estcrpt(i,m,13)    = estcrpt(i,m,13)    + holdestt
            estcrpt(i,nyrs1,l) = estcrpt(i,nyrs1,l) + holdestt

Cjhb=&==================================================================
C           CALCULATE PUMPING TO UNMET IWR
Cjhb=&==================================================================
c           now try to make up deficit by pumping
            if(isuply .eq. 4) then
Cjhb          ----------------------------------------------------------
Cjhb          groundwater scenario
Cjhb          ----------------------------------------------------------
              if(iflag2(i) .eq. 1) then
Cjhb            --------------------------------------------------------
Cjhb            has historical pumping data in the PVH file
Cjhb            --------------------------------------------------------
                if((gsreq(m,l)+gfreq(m,l)) .gt. 0) then
Cjhb              ------------------------------------------------------
Cjhb              still has unmet IWR
Cjhb                gfreq and gsreq were reduced by cu from sw sources and cu from soil moisture
Cjhb              ------------------------------------------------------
Cjhb                old way
Cjhb              ------------------------------------------------------
Cjhb                prorate historical pumping to fl and spr lands by unmet IWR
Cjhb              ------------------------------------------------------
Cjhb                12/07 change back to old method
Cjhb              ------------------------------------------------------
!                  gsdiv(m,l)=
!     &              mprate(i,m,l)*gsreq(m,l)/(gsreq(m,l)+gfreq(m,l))
!                  gsdiv(m,l)=max(gsdiv(m,l),0.)
!                  gfdiv(m,l)=
!     &              mprate(i,m,l)*gfreq(m,l)/(gsreq(m,l)+gfreq(m,l))
!                  gfdiv(m,l)=max(gfdiv(m,l),0.)
Cjhb              ------------------------------------------------------
Cjhb 2/17/2011    distribute pvh (preset) pumping to spr first, then fl
Cjhb              ------------------------------------------------------
                  gsdiv(m,l)=min(mprate(i,m,l), gsreq(m,l)/speff(i,m))
                  gfdiv(m,l)=mprate(i,m,l)-gsdiv(m,l)
Cjhb              ------------------------------------------------------
Cjhb                an unused way
Cjhb              ------------------------------------------------------
Cjhb                prorate historical pumping to fl and spr lands by acreage
Cjhb              ------------------------------------------------------
!                  if((swgwflac(i,m)+swgwspac(i,m)).gt.0.0)then
!                    gsdiv(m,l)=mprate(i,m,l)*swgwspac(i,m)/
!     &                           (swgwflac(i,m)+swgwspac(i,m))
!                    gsdiv(m,l)=max(gsdiv(m,l),0.)
!                    gfdiv(m,l)=mprate(i,m,l)*swgwflac(i,m)/
!     &                           (swgwflac(i,m)+swgwspac(i,m))
!                    gfdiv(m,l)=max(gfdiv(m,l),0.)
!                  else
!                    gsdiv(m,l)=0.0
!                    gfdiv(m,l)=0.0
!                  endif
                else
Cjhb              ------------------------------------------------------
Cjhb              no unmet IWR
Cjhb              ------------------------------------------------------
Cjhb                old way
Cjhb              ------------------------------------------------------
Cjhb                 put all historical pumping on flood land
Cjhb              ------------------------------------------------------
Cjhb                12/07 change back to old method
Cjhb              ------------------------------------------------------
                  gfdiv(m,l)=mprate(i,m,l)
                  gfdiv(m,l)=max(gfdiv(m,l),0.)
                  gsdiv(m,l)=0
Cjhb              ------------------------------------------------------
Cjhb                new way
Cjhb              ------------------------------------------------------
Cjhb                prorate historical pumping to fl and spr lands by acreage
Cjhb              ------------------------------------------------------
!                  if((swgwflac(i,m)+swgwspac(i,m)).gt.0.0)then
!                    gsdiv(m,l)=mprate(i,m,l)*swgwspac(i,m)/
!     &                           (swgwflac(i,m)+swgwspac(i,m))
!                    gsdiv(m,l)=max(gsdiv(m,l),0.)
!                    gfdiv(m,l)=mprate(i,m,l)*swgwflac(i,m)/
!     &                           (swgwflac(i,m)+swgwspac(i,m))
!                    gfdiv(m,l)=max(gfdiv(m,l),0.)
!                  else
!                    gsdiv(m,l)=0.0
!                    gfdiv(m,l)=0.0
!                  endif
                endif
              else 
Cjhb            --------------------------------------------------------
Cjhb            does NOT have historical pumping data in the PVH file
Cjhb              meet unmet IWR on spr lands first, fl lands last, up to max pumping rate, mprate
Cjhb            --------------------------------------------------------
cjhb            add check for deficit irrigation parameter
                if(def_irr_frac(i,m).lt.1.0)then
Cjhb              --------------------------------------------------------
Cjhb              old way
Cjhb              --------------------------------------------------------
                  if((gsreqdef(m,l)/speff(i,m)) .gt. mprate(i,m,l)) then
                     if(ipresim .ne. 1) call lw_update(57,bas_id(i))
                  endif 
                  gsdiv(m,l)=min(gsreqdef(m,l)/speff(i,m),mprate(i,m,l))
                  gsdiv(m,l)=max(gsdiv(m,l),0.)
                  if((gfreqdef(m,l)/fleff(i,m)) .gt. (mprate(i,m,l)-
     &               gsdiv(m,l))) then
                     if(ipresim .ne. 1) call lw_update(57,bas_id(i))
                  endif 
                  gfdiv(m,l)=min(gfreqdef(m,l)/fleff(i,m),
     &                         mprate(i,m,l)-gsdiv(m,l))
                  gfdiv(m,l)=max(gfdiv(m,l),0.)
                else
Cjhb              --------------------------------------------------------
Cjhb              old way
Cjhb              --------------------------------------------------------
                  if((gsreq(m,l)/speff(i,m)) .gt. mprate(i,m,l)) then
                     if(ipresim .ne. 1) call lw_update(57,bas_id(i))
                  endif 
                  gsdiv(m,l)=min(gsreq(m,l)/speff(i,m),mprate(i,m,l))
                  gsdiv(m,l)=max(gsdiv(m,l),0.)
                  if((gfreq(m,l)/fleff(i,m)) .gt. (mprate(i,m,l)-
     &                gsdiv(m,l))) then
                     if(ipresim .ne. 1) call lw_update(57,bas_id(i))
                  endif 
                  gfdiv(m,l)=min(gfreq(m,l)/fleff(i,m),
     &                         mprate(i,m,l)-gsdiv(m,l))
                  gfdiv(m,l)=max(gfdiv(m,l),0.)
Cjhb              --------------------------------------------------------
Cjhb              new way
Cjhb              --------------------------------------------------------
Cjhb              prorate max pumping to fl and spr lands by acreage
Cjhb              --------------------------------------------------------
!                  if((swgwflac(i,m)+swgwspac(i,m)).gt.0.0)then
!                    gsdiv(m,l)=min(gsreq(m,l)/speff(i,m),
!     &                           mprate(i,m,l)*swgwspac(i,m)/
!     &                            (swgwflac(i,m)+swgwspac(i,m)))
!                    gsdiv(m,l)=max(gsdiv(m,l),0.)
!                    gfdiv(m,l)=min(gfreq(m,l)/fleff(i,m),
!     &                           mprate(i,m,l)*swgwflac(i,m)/
!     &                            (swgwflac(i,m)+swgwspac(i,m)))
!                    gfdiv(m,l)=max(gfdiv(m,l),0.)
!                  else
!                    gsdiv(m,l)=0.0
!                    gfdiv(m,l)=0.0
!                  endif
                endif
              endif
Cjhb          ----------------------------------------------------------
              gdiv(m,l)      = gsdiv(m,l)     + gfdiv(m,l)
              gdiv(m,13)     = gdiv(m,13)     + gdiv(m,l)
              gdiv(nyrs1,l)  = gdiv(nyrs1,l)  + gdiv(m,l)
              gsdiv(m,13)    = gsdiv(m,13)    + gsdiv(m,l)
              gsdiv(nyrs1,l) = gsdiv(nyrs1,l) + gsdiv(m,l)
              gfdiv(m,13)    = gfdiv(m,13)    + gfdiv(m,l)
              gfdiv(nyrs1,l) = gfdiv(nyrs1,l) + gfdiv(m,l)
Cjhb          ----------------------------------------------------------
Cjhb          determine CU met by groundwater pumping
Cjhb          ----------------------------------------------------------
              if(iflag2(i) .eq. 1) then
Cjhb            --------------------------------------------------------
Cjhb            has historical pumping data in the PVH file
Cjhb            --------------------------------------------------------
c                gwtemp=gsdiv(m,l)*speff(i,m)+gfdiv(m,l)*fleff(i,m)
c                gwcu(m,l)=min(gwtemp,(gsreq(m,l)+gfreq(m,l)))
c                if(gwcu(m,l) .lt. 1) gwcu(m,l)=0.
                gwtempf=gfdiv(m,l)*fleff(i,m)
                gwcuf=min(gwtempf,gfreq(m,l))
                gwtemps=gsdiv(m,l)*speff(i,m)
                gwcus=min(gwtemps,gsreq(m,l))
                gwcu(m,l)=gwcuf+gwcus
                if(gwcu(m,l) .lt. 1) gwcu(m,l)=0.
              else
Cjhb            --------------------------------------------------------
Cjhb            does NOT have historical pumping data in the PVH file
Cjhb            --------------------------------------------------------
                gwcu(m,l)=gsdiv(m,l)*speff(i,m) + gfdiv(m,l)*fleff(i,m)
                gwtempf=gfdiv(m,l)*fleff(i,m)
                gwcuf=min(gwtempf,gfreq(m,l))
                gwtemps=gsdiv(m,l)*speff(i,m)
                gwcus=min(gwtemps,gsreq(m,l))
                gwcu(m,l)=gwcuf+gwcus
                if(gwcu(m,l) .lt. 1) gwcu(m,l)=0.
              endif
Cjhb          ----------------------------------------------------------
              gwcu(m,13)    = gwcu(m,13)    + gwcu(m,l)
              gwcu(nyrs1,l) = gwcu(nyrs1,l) + gwcu(m,l)
Cjhb          ----------------------------------------------------------
Cjhb          total gw pumping that did not go to CU on gw lands
Cjhb          ----------------------------------------------------------
              gwrof         = gfdiv(m,l)    - gwcuf
              gwros         = gsdiv(m,l)    - gwcus
              gwro(m,l)     = gwrof         + gwros
              gwro(m,13)    = gwro(m,13)    + gwro(m,l)
              gwro(nyrs1,l) = gwro(nyrs1,l) + gwro(m,l)
            endif

Cjhb=&==================================================================
C           after pass 1 through wsupsum (itime=1)
C           and if the remaining surface water IWR (swreq) is nonzero
C           only  calculate and sum (for an average later)
C           the percentage shortage on sw only lands
Cjhb=&==================================================================
c
c Determine surface water only land shortage by WD
c  check here
             if(itime .eq. 1) then
               if(swreq(m,l) .gt. 0) then
                 met=(swcu+swhold)/swreq(m,l)
                 if (swcu+swhold.gt.swreq(m,l)+.05) then
                   write(0,*)"swmet imbalance:"
                   write(0,*)"swcu+swhold.gt.swreq(m,l)+.05"
                   write(0,*)"swflac(i,m)",swflac(i,m)
                   write(0,*)"swspac(i,m)",swspac(i,m)
                   write(0,*)"swgwflac(i,m)",swgwflac(i,m)
                   write(0,*)"swgwspac(i,m)",swgwspac(i,m)
                   write(0,*)"t_area(i,m)",t_area(i,m)
                   write(0,*)"sfcu",sfcu
                   write(0,*)"sscu",sscu
                   write(0,*)"swcu",swcu
                   write(0,*)"gfcu",gfcu
                   write(0,*)"gscu",gscu
                   write(0,*)"gcu",gcu
                   write(0,*)"swhold",swhold
                   write(0,*)"sfreq(m,l)",sfreq(m,l)
                   write(0,*)"ssreq(m,l)",ssreq(m,l)
                   write(0,*)"swreq(m,l)",swreq(m,l)
                   write(0,*)"gfreq(m,l)",gfreq(m,l)
                   write(0,*)"gsreq(m,l)",gsreq(m,l)
                   write(0,*)"gwreq(m,l)",gwreq(m,l)
                   write(0,*)"met",met
                   write(0,*)"nbasin",nbasin
                   write(0,*)"nyrs",nyrs
                   write(0,*)"bas_id(i)",bas_id(i)
                   write(0,*)"i,m,l,id",i,m,l,id
                   stop
                 endif
                 swmet(id,m,l)=swmet(id,m,l)+met
                 idcnt(id,m,l)=idcnt(id,m,l)+1
               endif
             endif
C-----------------Senior Unlagged Return Flow
Cjhb=&==================================================================
c jhb 5-16-07 tailwater is now grouped with other water (not senior)
c jhb 5-16-07 so it should be lagged with it as well
c             holdlags(m,l) = holdps-holds-holds1-arech(m,l)+tail(i,m,l)
c jhb 8-09-07 ray b rquest: include the recharge water in the unused (lagged) portion
c             holdlags(m,l) = holdps-holds-holds1-arech(m,l)
             holdlags(m,l) = holdps-holds-holds1
c            ew/jhb 09/2008 - these calculations will not be correct when tail() < 0
             if(tail(i,m,l).lt.0.0)then
c              fix this value to account for negative tailwater adjustments to farm deliveries
c              negative tailwater adjustments should NOT show up in unused water
               holdlags(m,l)=holdlags(m,l)+sen_adj
             endif
Cjhb=&==================================================================
             ulags(m,l) = ulags(m,l)+ holdlags(m,l)
             ulags(m,13)=ulags(m,13)+holdlags(m,l)
             ulags(nyrs1,l) =ulags(nyrs1,l)+ holdlags(m,l)
C-----------------Junior Unlagged Return Flow

c grb 4-17-00 add pushed out junior water from soil moisture to unlagged return flows
Cjhb=&==================================================================
c jhb 5-16-07 new soil moisture accounting has three kinds of "pushed out" water
c             pojsm - junior soil moisture pushed out by senior water
c             poosm - other soil moisture pushed out by senior water
c             poosmbj - other soil moisture pushed out by junior water
Cjhb=&==================================================================
             holdlagj(m,l) = holdpj-holdj-holdj1+pojsm
c            ew/jhb 09/2008 - these calculations will not be correct when tail() < 0
             if(tail(i,m,l).lt.0.0)then
c              fix this value to account for negative tailwater adjustments to farm deliveries
c              negative tailwater adjustments should NOT show up in unused water
               holdlagj(m,l)=holdlagj(m,l)+jun_adj
             endif
             ulagj(m,l) =ulagj(m,l)+ holdlagj(m,l)
             ulagj(m,13)=ulagj(m,13)+holdlagj(m,l)
             ulagj(nyrs1,l) =ulagj(nyrs1,l)+holdlagj(m,l)
C-----------------Other Unlagged Return Flow						  
Cjhb=&==================================================================
c jhb 5-16-07 add pushed out "other" soil moisture
c jhb 5-16-07 tailwater is now grouped with other water (not senior)
c jhb 5-16-07 so it should be lagged with it as well
c             holdlago(m,l)=holdpo-holdo-holdo1
             holdlago(m,l)=holdpo-holdo-holdo1+poosm+poosmbj
     &                     +tail(i,m,l)
Cjhb=&==================================================================
c            ew/jhb 09/2008 - these calculations will not be correct when tail() < 0
             if(tail(i,m,l).lt.0.0)then
c              fix this value to account for negative tailwater adjustments to farm deliveries
c              negative tailwater adjustments should NOT show up in unused water
               holdlago(m,l)=holdlago(m,l)-tail(i,m,l)+oth_adj
             endif
             ulago(m,l) = ulago(m,l)+ holdlago(m,l)
             ulago(m,13)=ulago(m,13)+holdlago(m,l)
             ulago(nyrs1,l) =ulago(nyrs1,l)+holdlago(m,l)
C-----------------Total Unlagged Return Flow						 
             holdlagt(m,l)= holdlags(m,l)+holdlagj(m,l)+holdlago(m,l)-
     :                      closs(m,l)
             ulagt(m,l)=ulagt(m,l)+ holdlagt(m,l)
             ulagt(m,13)=ulagt(m,13)+holdlagt(m,l)
             ulagt(nyrs1,l) =ulagt(nyrs1,l)+holdlagt(m,l)
             tdp(m,l) = gwro(m,l)+ulagt(m,l)
             tdp(m,13) = tdp(m,13)+gwro(m,l)+ulagt(m,l)
             tdp(nyrs1,l)=tdp(nyrs1,l)+gwro(m,l)+ulagt(m,l)
C-----------------Senior Surface WAter Return Flow 1st Month
             holdlgs=holdlags(m,l)*retn(i,1)
             lagrets(m,l) = lagrets(m,l)+holdlgs
             lagrets(m,13)=lagrets(m,13)+holdlgs
             lagrets(nyrs1,l)=lagrets(nyrs1,l)+holdlgs
C-----------------Junior Surface Water Return Flow 1st Month        
             holdlgj=holdlagj(m,l)*retn(i,1)
             lagretj(m,l) = lagretj(m,l)+holdlgj
             lagretj(m,13)=lagretj(m,13)+holdlgj
             lagretj(nyrs1,l)=lagretj(nyrs1,l)+holdlgj
C-----------------Other Surface Water Return Flow 1st Month
             holdlgo=holdlago(m,l)*retn(i,1)
             lagreto(m,l) = lagreto(m,l)+holdlgo
             lagreto(m,13)=lagreto(m,13)+holdlgo
             lagreto(nyrs1,l)=lagreto(nyrs1,l)+holdlgo
C-----------------Total Surface Water Return Flow 1st Month        
             holdlgt=holdlagt(m,l)*retn(i,1)
             lagrett(m,l) = lagrett(m,l)+holdlgt
             lagrett(m,13)=lagrett(m,13)+holdlgt
             lagrett(nyrs1,l)=lagrett(nyrs1,l)+holdlgt
C-----------------Surface Diversion to CU and SM
             holddiv=holdt+holdt1
             divcu(m,l)=divcu(m,l)+holddiv
             divcu(m,13)=divcu(m,13)+holddiv
             divcu(nyrs1,l)=divcu(nyrs1,l)+holddiv

c  process delayed return flows
             rets = 0
             retj = 0
             reto = 0
             rett = 0
           do j=1,59
             isoyr=m
             isomo=l-j
             jj=j+1
             if((l-j).lt.1 .and. (l-j) .gt. -12) then
                   isomo=12+(l-j) 
             isoyr=m-1
             endif
             if((l-j) .lt. -11 .and. (l-j) .gt. -24) then
               isomo=24+(l-j)
               isoyr=m-2
             endif
             if((l-j) .lt. -23 .and. (l-j) .gt. -36) then
               isomo=36+(l-j)
               isoyr=m-3
             endif
             if((l-j) .lt. -35 .and. (l-j) .gt. -48) then
               isomo=48+(l-j)
               isoyr=m-4
             endif
             if((l-j) .lt. -47 .and. (l-j) .gt. -60) then
               isomo=60+(l-j)
               isoyr=m-5
             endif
             if (isoyr.gt.0) then
               rets = rets + holdlags(isoyr,isomo)*retn(i,j+1)
               retj = retj + holdlagj(isoyr,isomo)*retn(i,j+1)
               reto = reto + holdlago(isoyr,isomo)*retn(i,j+1)
               rett = rett + holdlagt(isoyr,isomo)*retn(i,j+1)
              endif
111        enddo
C-----------------Senior Lagged Return Flow
              laglates(m,l) = laglates(m,l)+rets
              laglates(m,13) = laglates(m,13)+rets
              laglates(nyrs1,l) = laglates(nyrs1,l)+rets
C-----------------Junior Lagged Return Flow		        				
              laglatej(m,l) = laglatej(m,l)+retj
              laglatej(m,13) = laglatej(m,13)+retj
              laglatej(nyrs1,l) = laglatej(nyrs1,l)+retj
C-----------------Other Lagged Return Flow		        				
              laglateo(m,l) = laglateo(m,l)+reto
              laglateo(m,13) = laglateo(m,13)+reto
              laglateo(nyrs1,l) = laglateo(nyrs1,l)+reto
C-----------------Total Lagged Return Flow		        				
              laglatet(m,l) = laglatet(m,l)+rett
              laglatet(m,13) = laglatet(m,13)+rett
              laglatet(nyrs1,l) = laglatet(nyrs1,l)+rett
C-----------------Total Returns
              holdtret=holdlgt+rett		        
              totret(m,l) = totret(m,l)+holdtret
              totret(m,13) = totret(m,13)+holdtret
              totret(nyrs1,l) = totret(nyrs1,1)+holdtret
C----Senior River Depletion		   
c grb 5-12-00 change sign of depletion
              holddeps=-(holdlgs+rets-holdps)
	        deps(m,l) = deps(m,l)+holddeps
              deps(m,13)=deps(m,13)+holddeps
              deps(nyrs1,l)=deps(nyrs1,l)+holddeps
C-----------------Junior River Depletion
c grb 5-12-00 change sign of depletion			
              holddepj=-(holdlgj+retj-holdpj)
              depj(m,l) = depj(m,l)+holddepj
              depj(m,13)=depj(m,13)+holddepj
              depj(nyrs1,l)=depj(nyrs1,l)+holddepj
C-----------------Other River Depletion  
c grb 5-12-00 change sign of depletion
	        holddepo=-(holdlgo+reto-holdpo)
              depo(m,l) = depo(m,l)+holddepo
              depo(m,13)=depo(m,13)+holddepo
              depo(nyrs1,l)=depo(nyrs1,l)+holddepo
C-----------------Total River Depletion
c grb 5-12-00 change sign of depletion	
              holddept=-(holdlgt+rett-divsup(i,m,l))                 
              dept(m,l) = dept(m,l)+holddept
              dept(m,13)=dept(m,13)+holddept
              dept(nyrs1,l)=dept(nyrs1,l)+holddept
455           continue
cjhb          ==========================================================
cjhb 05-18-07 modified to better check the divisors...
cjhb          ==========================================================
C-----------------Application Efficiency and "system" efficiency
c              if (divcu(m,l).ge.0.and.(seniorf(m,l)+juniorf(m,l)+
c     :            otherf(m,l)-arech(m,l)).gt.0.) then
c                 effcu(m,l)=(divcu(m,l)/(seniorf(m,l)+
c     :           juniorf(m,l)+otherf(m,l)-arech(m,l)))*100
c                 seffcu(m,l)=(divcu(m,l)/(divsup(i,m,l)
c     :           -arech(m,l)))*100
c              else
c
c ew 8/23/00 if not diversions or demands, set eff = maximum
c                 effcu(m,l) =0
c                 seffcu(m,l) =0
c              endif
cjhb          ==========================================================
              if (divcu(m,l).ge.0.and.(seniorf(m,l)+juniorf(m,l)+
     :            otherf(m,l)-arech(m,l)).gt.0.) then
                 effcu(m,l)=(divcu(m,l)/(seniorf(m,l)+
     :           juniorf(m,l)+otherf(m,l)-arech(m,l)))*100.0
              else
                 effcu(m,l)=0
              endif
              if (divcu(m,l).ge.0.and.
     &            (divsup(i,m,l)+tail(i,m,l)-arech(m,l)).gt.0.) then
                 seffcu(m,l)=(divcu(m,l)/
     &           (divsup(i,m,l)+tail(i,m,l)-arech(m,l)))*100.0
              else
                 seffcu(m,l)=0.0
              endif
cjhb          ==========================================================
              if(gdiv(m,l) .gt. 0) then
                 effgw(m,l)=(gwcu(m,l)/gdiv(m,l))
              else
                 effgw(m,l)=0.0
              endif
cjhb          ==========================================================
c    &"1111X22233333333444444445555555566666666777777778888888899999999000000001111111122222222333333334444444455555555666666667777777788888888999999990000000011111111222222223333333344444444555555556666666677777777888888889999999900000000111111112222222233333333444444445555555566666666777777778888888899999999000000001111111122222222333333334444444455555555666666667777777788888888999999990000000011111111222222223333333344444444555555556666666677777777888888889999999900000000111111112222222233333333444444445555555566666666777777778888888899999999000000001111111122222222333333334444444455555555666666667777777788888888999999990000000011111111222222223333333344444444555555556666666677777777888888889999999900000000"
c    &"----|---|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|-----------------------------------------------|---------------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-----------------------|-----------------------|---------------------------------------|---------------------------------------|"
c    &"    |   |           Irrigated Acreage           |          Potential Crop ET            |           Effective Precip            |     Irrigation Water Requirement      |    Winter Precip Carryover Used       |           IWR after WCO               |                                  SW Farm Headgate Delivery                            |                                   SW to CU                            |     SW to Soil Moisture       |   *Pushed Out* Soil Moisture  | Non-consumed SW (incl sm rel) |                            Soil Moisture To CU                        |      Groundwater Pumping      |   GW Pumping To CU    |Non-Consumed GW Pumping|              Total CU                 |          Total CU Shortage            |"
c    &"    |   |---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|-----------------------------------------------|---------------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-----------------------|-----------------------|---------------------------------------|---------------------------------------|"
c    &"    |   |swflac |swspac |swgwfl.|swgwsp.| t_area| ettot | ettot | ettot | ettot | ettot |effppt |effppt |effppt |effppt |effppt | reqt  | reqt  | reqt  | reqt  | reqt  |wbused |wbused |wbused |wbused |wbused |reqreq |reqreq |reqreq |reqreq |reqreq |sfshare|ssshare|gfshare|gsshare| arech | fdiv  | holdfs| holdfj| holdfo| tail  |  fdiv | sfcu  | sscu  | gfcu  | gscu  | holdt | holds | holdj | holdo | holdt | holds1| holds1| holdo1| holdt1| pojsm | poosm |poosmbj|       | ulags | ulagj | ulago | ulagt | sfhold| sshold| gfhold| gshold|holdcrp holdcrpsholdcrpjholdcrpoholdcrp| gfdiv | gsdiv | gdiv  | mprate| gwcuf | gwcus |  gwcu | gwrof | gwros |  gwro |   *   |   *   |   *   |   *   |       |   *   |   *   |   *   |   *   |       |"
c    &"    |   |---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|-----------------------------------------------|---------------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-----------------------|-----------------------|---------------------------------------|---------------------------------------|"
c    &"    |   |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW|  re-  | Total | senior| junior| other | other | Total |SW Only|SW Only|SW & GW|SW & GW| Total | senior| junior| other | Total | senior| junior| other | Total | junior| other | other | Total | senior| junior| other | Total |SW Only|SW Only|SW & GW|SW & GW| Total | senior| junior| other | Total |SW & GW|SW & GW| Total |  Max  |SW & GW|SW & GW| Total |SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |"
c    &"    |   |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr| charge|       | sw div| sw div| sw div|  tail |       |  Flood|Sprnklr|  Flood|Sprnklr|       |       |       |       |       |       |       |       |       | by sr | by sr | by jr |       |       |       |       |       |  Flood|Sprnklr|  Flood|Sprnklr|       |       |       |       |       |  Flood|Sprnklr|       |  Rate |  Flood|Sprnklr|       |  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |"
c    &"Year|Mon|(Acres)|(Acres)|(Acres)|(Acres)|(Acres)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|  (AF) |   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|"
c    &"----|---|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|-----------------------------------------------|---------------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-----------------------|-----------------------|---------------------------------------|---------------------------------------|"

c     ----|---|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------|-----------------------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-----------------------|-----------------------|---------------------------------------|---------------------------------------|
c         |   |           Irrigated Acreage           |          Potential Crop ET            |           Effective Precip            |     Irrigation Water Requirement      |    Winter Precip Carryover Used       |           IWR after WCO               |      SW River Diversion       |       SW River Delivery       |  Tail |                                  SW Farm Headgate Delivery                    |                                   SW to CU                            |     SW to Soil Moisture       |   *Pushed Out* Soil Moisture  | Non-consumed SW (incl sm rel) |                            Soil Moisture To CU                        |      Groundwater Pumping      |   GW Pumping To CU    |Non-Consumed GW Pumping|              Total CU                 |          Total CU Shortage            |
c     ----|---|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------|-----------------------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-----------------------|-----------------------|---------------------------------------|---------------------------------------|
c         |   |swflac |swspac |swgwfl.|swgwsp.| t_area| ettot | ettot | ettot | ettot | ettot |effppt |effppt |effppt |effppt |effppt | reqt  | reqt  | reqt  | reqt  | reqt  |wbused |wbused |wbused |wbused |wbused |reqreq |reqreq |reqreq |reqreq |reqreq | holdps| holdpj| holdpo| divsup| holdps| holdpj| holdpo| divsup|  tail |sfshare|ssshare|gfshare|gsshare| arech | fdiv  | holdfs| holdfj| holdfo|  fdiv | sfcu  | sscu  | gfcu  | gscu  | holdt | holds | holdj | holdo | holdt | holds1| holds1| holdo1| holdt1| pojsm | poosm |poosmbj|       | ulags | ulagj | ulago | ulagt | sfhold| sshold| gfhold| gshold|holdcrp holdcrpsholdcrpjholdcrpoholdcrp| gfdiv | gsdiv | gdiv  | mprate| gwcuf | gwcus |  gwcu | gwrof | gwros |  gwro |   *   |   *   |   *   |   *   |       |   *   |   *   |   *   |   *   |       |"
c         |   |swflac |swspac |swgwfl.|swgwsp.| t_area| ettot | ettot | ettot | ettot | ettot |effppt |effppt |effppt |effppt |effppt | reqt  | reqt  | reqt  | reqt  | reqt  |wbused |wbused |wbused |wbused |wbused |reqreq |reqreq |reqreq |reqreq |reqreq | holdps| holdpj| holdpo| divsup| *ceff | *ceff | *ceff | *ceff |       |sfshare|ssshare|gfshare|gsshare| arech | fdiv  | holdfs| holdfj| holdfo|  fdiv | sfcu  | sscu  | gfcu  | gscu  | holdt | holds | holdj | holdo | holdt | holds1| holds1| holdo1| holdt1| pojsm | poosm |poosmbj|       | ulags | ulagj | ulago | ulagt | sfhold| sshold| gfhold| gshold|holdcrp holdcrpsholdcrpjholdcrpoholdcrp| gfdiv | gsdiv | gdiv  | mprate| gwcuf | gwcus |  gwcu | gwrof | gwros |  gwro |   *   |   *   |   *   |   *   |       |   *   |   *   |   *   |   *   |       |"
c     ----|---|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------|-----------------------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-----------------------|-----------------------|---------------------------------------|---------------------------------------|
c         |   |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total | senior| junior| other | Total | senior| junior| other | Total | Total |SW Only|SW Only|SW & GW|SW & GW|  re-  | Total | senior| junior| other | Total |SW Only|SW Only|SW & GW|SW & GW| Total | senior| junior| other | Total | senior| junior| other | Total | junior| other | other | Total | senior| junior| other | Total |SW Only|SW Only|SW & GW|SW & GW| Total | senior| junior| other | Total |SW & GW|SW & GW| Total |  Max  |SW & GW|SW & GW| Total |SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |SW Only|SW Only|SW & GW|SW & GW| Total |
c         |   |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |       |       |       |       |       |       |       |       |       |  Flood|Sprnklr|  Flood|Sprnklr| charge|       | sw div| sw div| sw div|       |  Flood|Sprnklr|  Flood|Sprnklr|       |       |       |       |       |       |       |       |       | by sr | by sr | by jr |       |       |       |       |       |  Flood|Sprnklr|  Flood|Sprnklr|       |       |       |       |       |  Flood|Sprnklr|       |  Rate |  Flood|Sprnklr|       |  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |  Flood|Sprnklr|  Flood|Sprnklr|       |
c     Year|Mon|(Acres)|(Acres)|(Acres)|(Acres)|(Acres)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|   (AF)|
c     ----|---|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------|-----------------------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-------------------------------|-------------------------------|---------------------------------------|-------------------------------|-------------------------------|-----------------------|-----------------------|---------------------------------------|---------------------------------------|
cjhb &==================================================================
cjhb &        OUTPUT THE DETAILS OF THE FOUR LAND CATEGORY
cjhb &        WATER BUDGET ACCOUNTING
cjhb &==================================================================
              if((trim(s4catid).ne."").and.(ipresim.ne.1))then
                if(trim(twdid(1:12)) .eq. trim(s4catid))then
                if(t_area(i,m).eq.0.0)then
c                  write(413,'(2I4,5F8.1,5F8.1,5F8.1,5F8.1,10F8.1,9F8.1,32X,5F8.1,4F8.1,32X,5F8.1,9F8.1,16X,3F8.1,16X,3F8.1,16X,3F8.1,5F8.1,5F8.1)')
                  write(413,'(I4,1X,I3,5F8.1,5F8.1,5F8.1,5F8.1,5F8.1,5F8
     &.1,4F8.1,4F8.1,F8.1,10F8.1,9F8.1,4F8.1,4F8.1,4F8.1,9F8.1,4F8.1,3F8
     &.1,3F8.1,5F8.1,5F8.1)')
     &            nyr1+m-1,l,                                           !year and month index
     &            swflac(i,m),swspac(i,m),swgwflac(i,m),swgwspac(i,m),  !irrigated acreage
     &            t_area(i,m),                                          !irrigated acreage
     &            ettot(i,m,l)*0.0,                                     !potential ET on sw fl
     &            ettot(i,m,l)*0.0,                                     !potential ET on sw sp
     &            ettot(i,m,l)*0.0,                                     !potential ET on gw fl
     &            ettot(i,m,l)*0.0,                                     !potential ET on gw sp
     &            ettot(i,m,l),                                         !potential ET total
     &            effppt(i,m,l)*0.0,                                    !eff precip on sw fl
     &            effppt(i,m,l)*0.0,                                    !eff precip on sw sp
     &            effppt(i,m,l)*0.0,                                    !eff precip on gw fl
     &            effppt(i,m,l)*0.0,                                    !eff precip on gw sp
     &            effppt(i,m,l),                                        !eff precip total
     &            reqt(i,m,l)*0.0,                                      !IWR after eff precip on sw fl
     &            reqt(i,m,l)*0.0,                                      !IWR after eff precip on sw sp
     &            reqt(i,m,l)*0.0,                                      !IWR after eff precip on gw fl
     &            reqt(i,m,l)*0.0,                                      !IWR after eff precip on gw sp
     &            reqt(i,m,l),                                          !IWR after eff precip total
     &            wbused(i,m,l)*0.0,                                    !wco on sw fl
     &            wbused(i,m,l)*0.0,                                    !wco on sw sp
     &            wbused(i,m,l)*0.0,                                    !wco on gw fl
     &            wbused(i,m,l)*0.0,                                    !wco on gw sp
     &            wbused(i,m,l),                                        !wco on gw sp
     &            reqreq(m,l)*0.0,                                      !IWR after wco on sw fl
     &            reqreq(m,l)*0.0,                                      !IWR after wco on sw sp
     &            reqreq(m,l)*0.0,                                      !IWR after wco on gw fl
     &            reqreq(m,l)*0.0,                                      !IWR after wco on gw sp
     &            reqreq(m,l),                                          !IWR after wco total
     &            holdps,holdpj,holdpo,divsup(i,m,l),                   !river diversion
     &            holdps*ceff(i,m),holdpj*ceff(i,m),                    !river delivery
     &            holdpo*ceff(i,m),divsup(i,m,l)*ceff(i,m),             !river delivery
     &            tail(i,m,l),                                          !tailwater delivery
     &            sfshare,ssshare,gfshare,gsshare,arech(m,l),           !farm deliv
     &            fdiv(m,l),                                            !farm deliv
     &            holdfs,holdfj,holdfo,fdiv(m,l),                       !farm deliv
     &            sfcu,sscu,gfcu,gscu,holdt,                            !sw to cu
     &            holds,holdj,holdo,holdt,                              !sw to cu
     &            holds1,holdj1,holdo1,holdt1,                          !sw to s.m.
     &            pojsm,poosm,poosmbj,pojsm+poosm+poosmbj,              !pushed out soil moisture
     &            ulags(m,l)-holdps*(1.-ceff(i,m)),                     !nonconsumed sw
     &            ulagj(m,l)-holdpj*(1.-ceff(i,m)),                     !nonconsumed sw
     &            ulago(m,l)-holdpo*(1.-ceff(i,m)),                     !nonconsumed sw
     &            ulagt(m,l),                                           !nonconsumed sw
     &            sfhold,sshold,gfhold,gshold,holdcrop,                 !sm to cu
     &            holdcrops,holdcropj,holdcropo,holdcrop,               !sm to cu
     &            gfdiv(m,l),gsdiv(m,l),gdiv(m,l),mprate(i,m,l),        !gw pumping
     &            gwcuf,gwcus,gwcu(m,l),                                !gw pumping to cu
     &            gwrof,gwros,gwro(m,l),                                !unconsumed gw pumping
     &            sfcu+sfhold,                                          !total cu on sw fl
     &            sscu+sshold,                                          !total cu on sw sp
     &            gfcu+gfhold+gwcuf,                                    !total cu on gw fl
     &            gscu+gshold+gwcus,                                    !total cu on gw sp
     &            holdt+holdcrop+gwcu(m,l),                             !total cu
     &            reqreq(m,l)*0.0-sfcu-sfhold,                          !total shortage on sw fl
     &            reqreq(m,l)*0.0-sscu-sshold,                          !total shortage on sw sp
     &          reqreq(m,l)*0.0-gfcu-gfhold-gwcuf,                      !total shortage on gw fl
     &          reqreq(m,l)*0.0-gscu-gshold-gwcus,                      !total shortage on gw sp
     &            reqreq(m,l)-holdt-holdcrop-gwcu(m,l)                  !total shortage
                else
c                  write(413,'(2I4,5F8.1,5F8.1,5F8.1,5F8.1,10F8.1,9F8.1,32X,5F8.1,4F8.1,32X,5F8.1,9F8.1,16X,3F8.1,16X,3F8.1,16X,3F8.1,5F8.1,5F8.1)')
                  write(413,'(I4,1X,I3,5F8.1,5F8.1,5F8.1,5F8.1,5F8.1,5F8
     &.1,4F8.1,4F8.1,F8.1,10F8.1,9F8.1,4F8.1,4F8.1,4F8.1,9F8.1,4F8.1,3F8
     &.1,3F8.1,5F8.1,5F8.1)')
     &            nyr1+m-1,l,                                           !year and month index
     &            swflac(i,m),swspac(i,m),swgwflac(i,m),swgwspac(i,m),  !irrigated acreage
     &            t_area(i,m),                                          !irrigated acreage
     &            ettot(i,m,l)*swflac(i,m)/t_area(i,m),                 !potential ET on sw fl
     &            ettot(i,m,l)*swspac(i,m)/t_area(i,m),                 !potential ET on sw sp
     &            ettot(i,m,l)*swgwflac(i,m)/t_area(i,m),               !potential ET on gw fl
     &            ettot(i,m,l)*swgwspac(i,m)/t_area(i,m),               !potential ET on gw sp
     &            ettot(i,m,l),                                         !potential ET total
     &            effppt(i,m,l)*swflac(i,m)/t_area(i,m),                !eff precip on sw fl
     &            effppt(i,m,l)*swspac(i,m)/t_area(i,m),                !eff precip on sw sp
     &            effppt(i,m,l)*swgwflac(i,m)/t_area(i,m),              !eff precip on gw fl
     &            effppt(i,m,l)*swgwspac(i,m)/t_area(i,m),              !eff precip on gw sp
     &            effppt(i,m,l),                                        !eff precip total
     &            reqt(i,m,l)*swflac(i,m)/t_area(i,m),                  !IWR after eff precip on sw fl
     &            reqt(i,m,l)*swspac(i,m)/t_area(i,m),                  !IWR after eff precip on sw sp
     &            reqt(i,m,l)*swgwflac(i,m)/t_area(i,m),                !IWR after eff precip on gw fl
     &            reqt(i,m,l)*swgwspac(i,m)/t_area(i,m),                !IWR after eff precip on gw sp
     &            reqt(i,m,l),                                          !IWR after eff precip total
     &            wbused(i,m,l)*swflac(i,m)/t_area(i,m),                !wco on sw fl
     &            wbused(i,m,l)*swspac(i,m)/t_area(i,m),                !wco on sw sp
     &            wbused(i,m,l)*swgwflac(i,m)/t_area(i,m),              !wco on gw fl
     &            wbused(i,m,l)*swgwspac(i,m)/t_area(i,m),              !wco on gw sp
     &            wbused(i,m,l),                                        !wco total
     &            reqreq(m,l)*swflac(i,m)/t_area(i,m),                  !IWR after wco on sw fl
     &            reqreq(m,l)*swspac(i,m)/t_area(i,m),                  !IWR after wco on sw sp
     &            reqreq(m,l)*swgwflac(i,m)/t_area(i,m),                !IWR after wco on gw fl
     &            reqreq(m,l)*swgwspac(i,m)/t_area(i,m),                !IWR after wco on gw sp
     &            reqreq(m,l),                                          !IWR after wco total
     &            holdps,holdpj,holdpo,divsup(i,m,l),                   !river diversion
     &            holdps*ceff(i,m),holdpj*ceff(i,m),                    !river delivery
     &            holdpo*ceff(i,m),divsup(i,m,l)*ceff(i,m),             !river delivery
     &            tail(i,m,l),                                          !tailwater delivery
     &            sfshare,ssshare,gfshare,gsshare,arech(m,l),           !farm deliv
     &            fdiv(m,l),                                            !farm deliv
     &            holdfs,holdfj,holdfo,fdiv(m,l),                       !farm deliv
     &            sfcu,sscu,gfcu,gscu,holdt,                            !sw to cu
     &            holds,holdj,holdo,holdt,                              !sw to cu
     &            holds1,holdj1,holdo1,holdt1,                          !sw to s.m.
     &            pojsm,poosm,poosmbj,pojsm+poosm+poosmbj,              !pushed out soil moisture
     &            ulags(m,l)-holdps*(1.-ceff(i,m)),                     !nonconsumed sw
     &            ulagj(m,l)-holdpj*(1.-ceff(i,m)),                     !nonconsumed sw
     &            ulago(m,l)-holdpo*(1.-ceff(i,m)),                     !nonconsumed sw
     &            ulagt(m,l),                                           !nonconsumed sw
     &            sfhold,sshold,gfhold,gshold,holdcrop,                 !sm to cu
     &            holdcrops,holdcropj,holdcropo,holdcrop,               !sm to cu
     &            gfdiv(m,l),gsdiv(m,l),gdiv(m,l),mprate(i,m,l),        !gw pumping
     &            gwcuf,gwcus,gwcu(m,l),                                !gw pumping to cu
     &            gwrof,gwros,gwro(m,l),                                !unconsumed gw pumping
     &            sfcu+sfhold,                                          !total cu on sw fl
     &            sscu+sshold,                                          !total cu on sw sp
     &            gfcu+gfhold+gwcuf,                                    !total cu on gw fl
     &            gscu+gshold+gwcus,                                    !total cu on gw sp
     &            holdt+holdcrop+gwcu(m,l),                             !total cu
     &            reqreq(m,l)*swflac(i,m)/t_area(i,m)-sfcu-sfhold,      !total shortage on sw fl
     &            reqreq(m,l)*swspac(i,m)/t_area(i,m)-sscu-sshold,      !total shortage on sw sp
     &          reqreq(m,l)*swgwflac(i,m)/t_area(i,m)-gfcu-gfhold-gwcuf,!total shortage on gw fl
     &          reqreq(m,l)*swgwspac(i,m)/t_area(i,m)-gscu-gshold-gwcus,!total shortage on gw sp
     &            reqreq(m,l)-holdt-holdcrop-gwcu(m,l)                  !total shortage
                endif
                endif
              endif
cjhb &==================================================================
300         continue
c    end of month loop
        do l=1,12
           gwp(i,m,l)=gdiv(m,l)
        enddo

              if (divcu(m,13).ge.0.and.(seniorf(m,13)+
     :         juniorf(m,13)+otherf(m,13)-arech(m,13)).gt.0.) then
                effcu(m,13)=(divcu(m,13)/(seniorf(m,13)+
     :          juniorf(m,13)+otherf(m,13)-arech(m,13)))*100
                seffcu(m,13)=(divcu(m,13)/(seniorf(m,13)+
     :          juniorf(m,13)+otherf(m,13)+closs(m,13)-arech(m,13)))*100
              else
c
c ew 8/23/00 if not diversions or demands, set eff = maximum
                 effcu(m,13) =0
                 seffcu(m,13) =0
              endif
              if(gdiv(m,13) .gt. 0) then
                 effgw(m,13)=(gwcu(m,13)/gdiv(m,13))
              else
                 effgw(m,13)=0
              endif
400     continue
c    end of year loop

c grb 05-04-2000 added return if in presimulation- following line
c       ----------------------------------------------------------------
        if (ipresim.eq.1)  goto 5780
c       ----------------------------------------------------------------  

        ceff(i,nyrs1)=0.0
        sfeff(i,nyrs1)=0.0
        gfeff(i,nyrs1)=0.0
        fleff(i,nyrs1)=0.0
        speff(i,nyrs1)=0.0
        sfeffcnt=0.0
        gfeffcnt=0.0
        do m=1,nyrs
          acret(m)=acret(m)+t_area(i,m)
          ceff(i,nyrs1)=ceff(i,nyrs1)+ceff(i,m)
          if(sfeff(i,m).gt.0.0)then
            sfeff(i,nyrs1)=sfeff(i,nyrs1)+sfeff(i,m)
            sfeffcnt=sfeffcnt+1
          endif
          if(gfeff(i,m).gt.0.0)then
            gfeff(i,nyrs1)=gfeff(i,nyrs1)+gfeff(i,m)
            gfeffcnt=gfeffcnt+1
          endif
          fleff(i,nyrs1)=fleff(i,nyrs1)+fleff(i,m)    
          speff(i,nyrs1)=speff(i,nyrs1)+speff(i,m)
          if(iyear(m) .eq. 0) then
            iyct=iyct+1
          endif
           do l=1,12
             if((reqreq(m,l).gt.-998.0).and.
     &          (divsup(i,m,l).gt.-998.0))then
               divsup(i,m,13) = divsup(i,m,13) + divsup(i,m,l)
               divsup(i,nyrs1,l)=divsup(i,nyrs1,l)+divsup(i,m,l)
               effppt(i,m,13)=effppt(i,m,13)+effppt(i,m,l)
               effppt(i,nyrs1,l)=effppt(i,nyrs1,l)+effppt(i,m,l)
               ettot(i,m,13)=ettot(i,m,13)+ettot(i,m,l)
               ettot(i,nyrs1,l)=ettot(i,nyrs1,l)+ettot(i,m,l)
               reqt(i,m,13) = reqt(i,m,13)+reqt(i,m,l)
               reqt(i,nyrs1,l)=reqt(i,nyrs1,l)+reqt(i,m,l)
               reqreqts(m,l) = reqreq(m,l)
               reqreqts(m,13) = reqreqts(m,13) + reqreq(m,l)
               reqreqts(nyrs1,l) = reqreqts(nyrs1,l)+reqreq(m,l)
               ddhmonot(m,l)=ddhmonot(m,l)+divsup(i,m,l)
             elseif(reqreq(m,l) .lt. -998) then
               ettot(i,m,l)=-999
               effppt(i,m,l)=-999
               wbu(i,m,l)=-999
               wbused(i,m,l)=-999
               reqt(i,m,l)=-999
               reqreqts(m,l)=-999
               ddhmonot(m,l)=-999
             endif
             tail(i,m,13) = tail(i,m,13) + tail(i,m,l)
             tail(i,nyrs1,l)=tail(i,nyrs1,l)+tail(i,m,l)


             do if1=1,iflood
               ifx=if1*2-1
               ify=if1*2
c
               if(grass(i,m,l,ifx).lt.-1.0)then
                 grass(i,m,13,ifx) = -999.0
               else
                 grass(i,m,13,ifx)=grass(i,m,13,ifx) + grass(i,m,l,ifx)
                 grass(i,nyrs1,l,ifx)=grass(i,nyrs1,l,ifx)+
     1                                grass(i,m,l,ifx)
               endif
c
c rrb 2003/06/20; Do not sum area
               grass(i,m,13,ify) =  grass(i,m,l,ify)
               grass(i,nyrs1,l,ify)=grass(i,nyrs1,l,ify)+
     1                              grass(i,m,l,ify)

             end do
           enddo
          enddo
c
c -- calculate yearly total - if missing months, total = -999
c
      do 405 m=1,nyrs   
          if(iyear(m) .eq. 1) then
             ettot(i,m,13)= -999
             effppt(i,m,13)= -999
             reqt(i,m,13)= -999
             reqreqts(m,13) = -999
             divsup(i,m,13) = -999
             ddhmonot(m,13)= -999
             do l=1,13
                wbu(i,m,l) = -999
                wbused(i,m,l) = -999
             enddo
             seniorf(m,13) = -999
             juniorf(m,13) = -999
             otherf(m,13) = -999
             closs(m,13) = -999
             fdiv(m,13) = -999
             gwro(m,13) = -999
             gdiv(m,13) = -999
             gsdiv(m,13) = -999
             gfdiv(m,13) = -999
             arech(m,13) = -999
             gwcu(m,13) = -999
             tdp(m,13) = -999
             crop_cus(m,13)= -999
             crop_cuj(m,13)= -999
             crop_cuo(m,13)= -999
             crop_cut(m,13)= -999
             soil_cus(m,13)= -999
             soil_cuj(m,13)= -999
             soil_cuo(m,13)= -999
             soil_cu(m,13)= -999
             ulags(m,13)= -999
             ulagj(m,13)= -999
             ulago(m,13)= -999
             ulagt(m,13)= -999
             divcu(m,13)= -999
             effcu(m,13)= -999
             seffcu(m,13)= -999
             effgw(m,13)= -999
             cropcusoil(m,13)= -999
             estcrps(m,13)= -999
             estcrpj(m,13)= -999
             estcrpo(m,13)= -999
             estcrpt(i,m,13)= -999
             lagrets(m,13)= -999
             lagretj(m,13)= -999
             lagreto(m,13)= -999
             lagrett(m,13)= -999
             laglates(m,13)= -999
             laglatej(m,13)= -999
             laglateo(m,13)= -999
             laglatet(m,13)= -999
             totret(m,13)= -999
             deps(m,13)= -999
             depj(m,13)= -999
             depo(m,13)= -999
             dept(m,13)= -999
             effcu(m,13)= -999
             seffcu(m,13)= -999
             effgw(m,13)= -999
        else
             ettot(i,nyrs1,13)=ettot(i,nyrs1,13)+ettot(i,m,13)
             effppt(i,nyrs1,13)=effppt(i,nyrs1,13)+effppt(i,m,13)
             reqt(i,nyrs1,13)=reqt(i,nyrs1,13)+reqt(i,m,13)
             reqreqts(nyrs1,13) = reqreqts(nyrs1,13)+reqreqts(m,13)
             divsup(i,nyrs1,13) = divsup(i,nyrs1,13)+divsup(i,m,13)
             wbu(i,nyrs1,13) =wbu(i,nyrs1,13)+wbu(i,m,13)
c
c
c _________________________________________________________
c
c rrb 2003/05/19; Allow up to 5 subirrigated crops
c            grass(i,nyrs1,13) = grass(i,nyrs1,13)+grass(i,m,13)
             tail(i,nyrs1,13) = tail(i,nyrs1,13) + tail(i,m,13)
             do if1=1,iflood
               ifx=if1*2-1
               ify=if1*2
               if(grass(i,m,13,ifx).lt.-1.0)then
               else
                 grass(i,nyrs1,13,ifx) = grass(i,nyrs1,13,ifx)+
     1                                   grass(i,m,13,ifx)
               endif
               grass(i,nyrs1,13,ify) = grass(i,nyrs1,13,ify)+
     1                                 grass(i,m,13,ify)
             end do
             ddhmonot(nyrs1,13)=ddhmonot(nyrs1,13)+ddhmonot(m,13)
             seniorf(nyrs1,13)=seniorf(nyrs1,13)+seniorf(m,13)
             juniorf(nyrs1,13)=juniorf(nyrs1,13)+juniorf(m,13)
             otherf(nyrs1,13)=otherf(nyrs1,13)+otherf(m,13)
             closs(nyrs1,13)=closs(nyrs1,13)+closs(m,13)
             fdiv(nyrs1,13)=fdiv(nyrs1,13)+fdiv(m,13)
             gwro(nyrs1,13)=gwro(nyrs1,13)+gwro(m,13)
             gdiv(nyrs1,13)=gdiv(nyrs1,13)+gdiv(m,13)
             gsdiv(nyrs1,13)=gsdiv(nyrs1,13)+gsdiv(m,13)
             gfdiv(nyrs1,13)=gfdiv(nyrs1,13)+gfdiv(m,13)
             arech(nyrs1,13)=arech(nyrs1,13)+arech(m,13)
             gwcu(nyrs1,13)=gwcu(nyrs1,13)+gwcu(m,13)
             tdp(nyrs1,13)=tdp(nyrs1,13)+tdp(m,13)
             crop_cus(nyrs1,13)=crop_cus(nyrs1,13)+crop_cus(m,13)
             crop_cuj(nyrs1,13)=crop_cuj(nyrs1,13)+crop_cuj(m,13)
             crop_cuo(nyrs1,13)=crop_cuo(nyrs1,13)+crop_cuo(m,13) 
             crop_cut(nyrs1,13)=crop_cut(nyrs1,13)+crop_cut(m,13) 
             soil_cus(nyrs1,13)=soil_cus(nyrs1,13)+soil_cus(m,13) 
             soil_cuj(nyrs1,13)=soil_cuj(nyrs1,13)+soil_cuj(m,13) 
             soil_cuo(nyrs1,13)=soil_cuo(nyrs1,13)+soil_cuo(m,13) 
             soil_cu(nyrs1,13)=soil_cu(nyrs1,13)+soil_cu(m,13)  
             ulags(nyrs1,13)=ulags(nyrs1,13)+ulags(m,13) 
             ulagj(nyrs1,13)=ulagj(nyrs1,13)+ulagj(m,13) 
             ulago(nyrs1,13)=ulago(nyrs1,13)+ulago(m,13) 
             ulagt(nyrs1,13)=ulagt(nyrs1,13)+ulagt(m,13) 
             divcu(nyrs1,13)=divcu(nyrs1,13)+divcu(m,13) 
             cropcusoil(nyrs1,13)=cropcusoil(nyrs1,13)+cropcusoil(m,13)
             estcrps(nyrs1,13)=estcrps(nyrs1,13)+estcrps(m,13) 
             estcrpj(nyrs1,13)=estcrpj(nyrs1,13)+estcrpj(m,13) 
             estcrpo(nyrs1,13)=estcrpo(nyrs1,13)+estcrpo(m,13) 
             estcrpt(i,nyrs1,13)=estcrpt(i,nyrs1,13)+estcrpt(i,m,13) 
             lagrets(nyrs1,13)=lagrets(nyrs1,13)+lagrets(m,13) 
             lagretj(nyrs1,13)=lagretj(nyrs1,13)+lagretj(m,13) 
             lagreto(nyrs1,13)=lagreto(nyrs1,13)+lagreto(m,13) 
             lagrett(nyrs1,13)=lagrett(nyrs1,13)+lagrett(m,13) 
             laglates(nyrs1,13)=laglates(nyrs1,13)+laglates(m,13) 
             laglatej(nyrs1,13)=laglatej(nyrs1,13)+laglatej(m,13) 
             laglateo(nyrs1,13)=laglateo(nyrs1,13)+laglateo(m,13) 
             laglatet(nyrs1,13)=laglatet(nyrs1,13)+laglatet(m,13) 
             totret(nyrs1,13)=totret(nyrs1,13)+totret(m,13) 
             deps(nyrs1,13)=deps(nyrs1,13)+deps(m,13) 
             depj(nyrs1,13)=depj(nyrs1,13)+depj(m,13) 
             depo(nyrs1,13)=depo(nyrs1,13)+depo(m,13) 
             dept(nyrs1,13)=dept(nyrs1,13)+dept(m,13) 
        endif
405   continue
           if(iyct .ge. 1) then

             ettot(i,nyrs1,13)=ettot(i,nyrs1,13)/iyct
             effppt(i,nyrs1,13)=effppt(i,nyrs1,13)/iyct
             reqt(i,nyrs1,13)=reqt(i,nyrs1,13)/iyct
             reqreqts(nyrs1,13) = reqreqts(nyrs1,13)/iyct
             wbu(i,nyrs1,13)= wbu(i,nyrs1,13)/iyct
             divsup(i,nyrs1,13) = divsup(i,nyrs1,13)/iyct
c _________________________________________________________
c
c rrb 2003/05/19; Allow up to 5 subirrigated crops
c            grass(i,nyrs1,13) = grass(i,nyrs1,13)/iyct
             tail(i,nyrs1,13)=tail(i,nyrs1,13)/iyct
            
             do if1=1,iflood
               ifx=if1*2-1
               ify=if1*2
               grass(i,nyrs1,13,ifx) = grass(i,nyrs1,13,ifx)/iyct
               grass(i,nyrs1,13,ify) = grass(i,nyrs1,13,ify)/iyct
             end do
c
c _________________________________________________________
c
             ddhmonot(nyrs1,13)=ddhmonot(nyrs1,13)/iyct
             seniorf(nyrs1,13)=seniorf(nyrs1,13)/iyct
             juniorf(nyrs1,13)=juniorf(nyrs1,13)/iyct
             otherf(nyrs1,13)=otherf(nyrs1,13)/iyct
             closs(nyrs1,13)=closs(nyrs1,13)/iyct
             fdiv(nyrs1,13)=fdiv(nyrs1,13)/iyct
             gwro(nyrs1,13)=gwro(nyrs1,13)/iyct
             gdiv(nyrs1,13)=gdiv(nyrs1,13)/iyct
             gsdiv(nyrs1,13)=gsdiv(nyrs1,13)/iyct
             gfdiv(nyrs1,13)=gfdiv(nyrs1,13)/iyct
             arech(nyrs1,13)=arech(nyrs1,13)/iyct
             gwcu(nyrs1,13)=gwcu(nyrs1,13)/iyct
             tdp(nyrs1,13)=tdp(nyrs1,13)/iyct
             crop_cus(nyrs1,13)=crop_cus(nyrs1,13)/iyct
             crop_cuj(nyrs1,13)=crop_cuj(nyrs1,13)/iyct
             crop_cuo(nyrs1,13)=crop_cuo(nyrs1,13)/iyct 
             crop_cut(nyrs1,13)=crop_cut(nyrs1,13)/iyct 
             soil_cus(nyrs1,13)=soil_cus(nyrs1,13)/iyct 
             soil_cuj(nyrs1,13)=soil_cuj(nyrs1,13)/iyct 
             soil_cuo(nyrs1,13)=soil_cuo(nyrs1,13)/iyct 
             soil_cu(nyrs1,13)=soil_cu(nyrs1,13)/iyct  
             ulags(nyrs1,13)=ulags(nyrs1,13)/iyct 
             ulagj(nyrs1,13)=ulagj(nyrs1,13)/iyct 
             ulago(nyrs1,13)=ulago(nyrs1,13)/iyct 
             ulagt(nyrs1,13)=ulagt(nyrs1,13)/iyct 
             divcu(nyrs1,13)=divcu(nyrs1,13)/iyct 
             cropcusoil(nyrs1,13)=cropcusoil(nyrs1,13)/iyct
             estcrps(nyrs1,13)=estcrps(nyrs1,13)/iyct 
             estcrpj(nyrs1,13)=estcrpj(nyrs1,13)/iyct 
             estcrpo(nyrs1,13)=estcrpo(nyrs1,13)/iyct 
             estcrpt(i,nyrs1,13)=estcrpt(i,nyrs1,13)/iyct 
             lagrets(nyrs1,13)=lagrets(nyrs1,13)/iyct 
             lagretj(nyrs1,13)=lagretj(nyrs1,13)/iyct 
             lagreto(nyrs1,13)=lagreto(nyrs1,13)/iyct 
             lagrett(nyrs1,13)=lagrett(nyrs1,13)/iyct 
             laglates(nyrs1,13)=laglates(nyrs1,13)/iyct 
             laglatej(nyrs1,13)=laglatej(nyrs1,13)/iyct 
             laglateo(nyrs1,13)=laglateo(nyrs1,13)/iyct 
             laglatet(nyrs1,13)=laglatet(nyrs1,13)/iyct 
             totret(nyrs1,13)=totret(nyrs1,13)/iyct 
             deps(nyrs1,13)=deps(nyrs1,13)/iyct 
             depj(nyrs1,13)=depj(nyrs1,13)/iyct 
             depo(nyrs1,13)=depo(nyrs1,13)/iyct 
             dept(nyrs1,13)=dept(nyrs1,13)/iyct 
             if(divcu(nyrs1,13).ge.0.and.(seniorf(nyrs1,13)+
     :        juniorf(nyrs1,13)+otherf(nyrs1,13)-arech(nyrs1,13)).gt.0.)
     :        then
               effcu(nyrs1,13) =(divcu(nyrs1,13)/(seniorf(nyrs1,13)+
     :         juniorf(nyrs1,13)+otherf(nyrs1,13)-arech(nyrs1,13)))
     :          *100
               seffcu(nyrs1,13) =(divcu(nyrs1,13)/
     :        (divsup(i,nyrs1,13)+tail(i,nyrs1,13)-arech(nyrs1,13)))*100
             else
c
c ew 8/23/00 if not diversions or demands, set eff = maximum
                 effcu(nyrs1,13) =0
                 seffcu(nyrs1,13) =0
             endif
             if (gdiv(nyrs1,13).gt.0) then
                effgw(nyrs1,13) =gwcu(nyrs1,13)/gdiv(nyrs1,13)
             else
                effgw(nyrs1,13)=0
             endif
         else
             ettot(i,nyrs1,13) = -999
             effppt(i,nyrs1,13) = -999
             reqt(i,nyrs1,13) = -999
             reqreqts(nyrs1,13) = -999
             divsup(i,nyrs1,13) = -999
             wbu(i,nyrs1,13)= -999
c
c _________________________________________________________
c
c
c rrb 2003/05/19; Allow up to 5 subirrigated crops
c 
             tail(i,nyrs1,13)=-999
             do if1=1,iflood
               ifx=if1*2-1
               ify=if1*2
               grass(i,nyrs1,13,ifx) = -999
               grass(i,nyrs1,13,ify) = -999
             end do
c
c _________________________________________________________

             ddhmonot(nyrs1,13)= -999
             seniorf(nyrs1,13)= -999
             juniorf(nyrs1,13)= -999
             otherf(nyrs1,13)= -999
             closs(nyrs1,13)= -999
             fdiv(nyrs1,13)= -999
             gwro(nyrs1,13)= -999
             gdiv(nyrs1,13)= -999
             gsdiv(nyrs1,13)= -999
             gfdiv(nyrs1,13)= -999
             arech(nyrs1,13)= -999
             gwcu(nyrs1,13)= -999
             tdp(nyrs1,13)= -999
             crop_cus(nyrs1,13)= -999
             crop_cuj(nyrs1,13)= -999
             crop_cuo(nyrs1,13)= -999
             crop_cut(nyrs1,13)= -999
             soil_cus(nyrs1,13)= -999
             soil_cuj(nyrs1,13)= -999
             soil_cuo(nyrs1,13)= -999
             soil_cu(nyrs1,13)= -999
             ulags(nyrs1,13)= -999
             ulagj(nyrs1,13)= -999
             ulago(nyrs1,13)= -999
             ulagt(nyrs1,13)= -999
             divcu(nyrs1,13)= -999
             cropcusoil(nyrs1,13)= -999
             estcrps(nyrs1,13)= -999
             estcrpj(nyrs1,13)= -999
             estcrpo(nyrs1,13)= -999
             estcrpt(i,nyrs1,13)= -999
             lagrets(nyrs1,13)= -999
             lagretj(nyrs1,13)= -999
             lagreto(nyrs1,13)= -999
             lagrett(nyrs1,13)= -999
             laglates(nyrs1,13)= -999
             laglatej(nyrs1,13)= -999
             laglateo(nyrs1,13)= -999
             laglatet(nyrs1,13)= -999
             totret(nyrs1,13)= -999
             deps(nyrs1,13)= -999
             depj(nyrs1,13)= -999
             depo(nyrs1,13)= -999
             dept(nyrs1,13)= -999
             effcu(nyrs1,13)= -999
             seffcu(nyrs1,13)= -999
             effgw(nyrs1,13)= -999
           endif
c
c -- calculate average monthly values for all years - divide total by # values
c
         do 410 l=1,12
           if(imonth(l) .ge. 1) then
             ettot(i,nyrs1,l)=ettot(i,nyrs1,l)/imonth(l)
             effppt(i,nyrs1,l)=effppt(i,nyrs1,l)/imonth(l)
             reqt(i,nyrs1,l)=reqt(i,nyrs1,l)/imonth(l)
             reqreqts(nyrs1,l) = reqreqts(nyrs1,l)/imonth(l)
             divsup(i,nyrs1,l) = divsup(i,nyrs1,l)/imonth(l)
c
c _________________________________________________________
c
c
c rrb 2003/05/19; Allow up to 5 subirrigated crops
c 
             tail(i,nyrs1,l)=tail(i,nyrs1,l)/imonth(l)
             do if1=1,iflood
               ifx=if1*2-1
               ify=if1*2
               grass(i,nyrs1,l,ifx) = grass(i,nyrs1,l,ifx)/imonth(l)
               grass(i,nyrs1,l,ify) = grass(i,nyrs1,l,ify)/imonth(l)
             end do
c
c _________________________________________________________

             ddhmonot(nyrs1,l)=ddhmonot(nyrs1,l)/imonth(l)
             seniorf(nyrs1,l)=seniorf(nyrs1,l)/imonth(l)
             juniorf(nyrs1,l)=juniorf(nyrs1,l)/imonth(l)
             otherf(nyrs1,l)=otherf(nyrs1,l)/imonth(l)
             closs(nyrs1,l)=closs(nyrs1,l)/imonth(l)
             fdiv(nyrs1,l)=fdiv(nyrs1,l)/imonth(l)
             gwro(nyrs1,l)=gwro(nyrs1,l)/imonth(l)
             gdiv(nyrs1,l)=gdiv(nyrs1,l)/imonth(l)
             gsdiv(nyrs1,l)=gsdiv(nyrs1,l)/imonth(l)
             gfdiv(nyrs1,l)=gfdiv(nyrs1,l)/imonth(l)
             arech(nyrs1,l)=arech(nyrs1,l)/imonth(l)
             gwcu(nyrs1,l)=gwcu(nyrs1,l)/imonth(l)
             tdp(nyrs1,l)=tdp(nyrs1,l)/imonth(l)
             crop_cus(nyrs1,l)=crop_cus(nyrs1,l)/imonth(l)
             crop_cuj(nyrs1,l)=crop_cuj(nyrs1,l)/imonth(l)
             crop_cuo(nyrs1,l)=crop_cuo(nyrs1,l)/imonth(l)
             crop_cut(nyrs1,l)=crop_cut(nyrs1,l)/imonth(l)
             soil_cus(nyrs1,l)=soil_cus(nyrs1,l)/imonth(l)
             soil_cuj(nyrs1,l)=soil_cuj(nyrs1,l)/imonth(l)
             soil_cuo(nyrs1,l)=soil_cuo(nyrs1,l)/imonth(l)
             soil_cu(nyrs1,l)=soil_cu(nyrs1,l)/imonth(l)
             ulags(nyrs1,l)=ulags(nyrs1,l)/imonth(l)
             ulagj(nyrs1,l)=ulagj(nyrs1,l)/imonth(l)
             ulago(nyrs1,l)=ulago(nyrs1,l)/imonth(l)
             ulagt(nyrs1,l)=ulagt(nyrs1,l)/imonth(l)
             divcu(nyrs1,l)=divcu(nyrs1,l)/imonth(l)
             soiltotts(nyrs1,l)=soiltotts(nyrs1,l)/nyrs
             soiltottj(nyrs1,l)=soiltottj(nyrs1,l)/nyrs
             soiltotto(nyrs1,l)=soiltotto(nyrs1,l)/nyrs
             soiltott(nyrs1,l)=soiltott(nyrs1,l)/nyrs
             wbu(i,nyrs1,l)=wbu(i,nyrs1,l)/nyrs
             cropcusoil(nyrs1,l)=cropcusoil(nyrs1,l)/imonth(l)
             estcrps(nyrs1,l)=estcrps(nyrs1,l)/imonth(l)
             estcrpj(nyrs1,l)=estcrpj(nyrs1,l)/imonth(l)
             estcrpo(nyrs1,l)=estcrpo(nyrs1,l)/imonth(l)
             estcrpt(i,nyrs1,l)=estcrpt(i,nyrs1,l)/imonth(l)
             lagrets(nyrs1,l)=lagrets(nyrs1,l)/imonth(l)
             lagretj(nyrs1,l)=lagretj(nyrs1,l)/imonth(l)
             lagreto(nyrs1,l)=lagreto(nyrs1,l)/imonth(l)
             lagrett(nyrs1,l)=lagrett(nyrs1,l)/imonth(l)
             laglates(nyrs1,l)=laglates(nyrs1,l)/imonth(l)
             laglatej(nyrs1,l)=laglatej(nyrs1,l)/imonth(l)
             laglateo(nyrs1,l)=laglateo(nyrs1,l)/imonth(l)
             laglatet(nyrs1,l)=laglatet(nyrs1,l)/imonth(l)
             totret(nyrs1,l)=totret(nyrs1,l)/imonth(l)
             deps(nyrs1,l)=deps(nyrs1,l)/imonth(l)
             depj(nyrs1,l)=depj(nyrs1,l)/imonth(l)
             depo(nyrs1,l)=depo(nyrs1,l)/imonth(l)
             dept(nyrs1,l)=dept(nyrs1,l)/imonth(l)
c             if (divcu(nyrs1,l).ge.0.and.(seniorF(nyrs1,l)+
c     :       juniorf(nyrs1,l)+otherf(nyrs1,l)-arech(nyrs1,l)).gt.0) then
cjhb 051807    =========================================================
cjhb 092307    change average monthly efficiency to be a simple average of the non zero monthly efficiencies
cjhb 051807    =========================================================
c                 effcu(nyrs1,l) =(divcu(nyrs1,l)/(seniorf(nyrs1,l)+
c     :           juniorf(nyrs1,l)+otherf(nyrs1,l)-arech(nyrs1,l)))*100
c                 seffcu(nyrs1,l) =(divcu(nyrs1,l)/(divsup(i,nyrs1,l)
c     :           -arech(nyrs1,l)))*100
cjhb 051807    =========================================================
               sumeff=0.0
               sumeffcnt=0.0
               sumseff=0.0
               sumseffcnt=0.0
               do m=1,nyrs
                 if(effcu(m,l).gt.0.0)then
                   sumeff=sumeff+effcu(m,l)
                   sumeffcnt=sumeffcnt+1.0
                 endif
                 if(seffcu(m,l).gt.0.0)then
                   sumseff=sumseff+seffcu(m,l)
                   sumseffcnt=sumseffcnt+1.0
                 endif
               enddo
               if(sumeffcnt.gt.0.0)then
                 effcu(nyrs1,l) = sumeff/sumeffcnt
               else
                 effcu(nyrs1,l) = 0.0
               endif
               if(sumseffcnt.gt.0.0)then
                 seffcu(nyrs1,l) = sumseff/sumseffcnt
               else
                 seffcu(nyrs1,l) = 0.0
               endif
cjhb 051807    =========================================================
c             else
cc ew 8/23/00 if no diversions or demands, set eff = 0 for now, but be sure
c            to set to maximum when printed to def or wef file
c                 effcu(nyrs1,l) = 0.
c                 seffcu(nyrs1,l) = 0.
c             endif
c             if (gdiv(nyrs1,l).gt.0) then
cjhb 051807    =========================================================
cjhb 092307    change average monthly efficiency to be a simple average of the non zero monthly efficiencies
cjhb 051807    =========================================================
c                 effgw(nyrs1,l) = (gwcu(nyrs1,l)/gdiv(nyrs1,l))
cjhb 051807    =========================================================
               sumeff=0.0
               sumeffcnt=0.0
               do m=1,nyrs
                 if(effgw(m,l).gt.0.0)then
                   sumeff=sumeff+effgw(m,l)
                   sumeffcnt=sumeffcnt+1.0
                 endif
               enddo
               if(sumeffcnt.gt.0.0)then
                 effgw(nyrs1,l) = sumeff/sumeffcnt
               else
                 effgw(nyrs1,l) = 0.0
               endif
cjhb 051807    =========================================================
c             else
c                 effgw(nyrs1,l) = 0.
c             endif
          else
             ettot(i,nyrs1,l) = -999
             effppt(i,nyrs1,l) = -999
             reqt(i,nyrs1,l) = -999
             reqreqts(nyrs1,l) = -999
             divsup(i,nyrs1,l) = -999
c
c _________________________________________________________
c
c
c rrb 2003/05/19; Allow up to 5 subirrigated crops
c 
             tail(i,nyrs1,l)=-999
             do if1=1,iflood
               ifx=if1*2-1
               ify=if1*2
               grass(i,nyrs1,l,ifx) = -999
               grass(i,nyrs1,l,ify) = -999
             end do
c
c _________________________________________________________
c
             ddhmonot(nyrs1,l)= -999
             seniorf(nyrs1,l)= -999
             juniorf(nyrs1,l)= -999
             otherf(nyrs1,l)= -999
             closs(nyrs1,l)= -999
             fdiv(nyrs1,l)= -999
             gwro(nyrs1,l)= -999
             gdiv(nyrs1,l)= -999
             gsdiv(nyrs1,l)= -999
             gfdiv(nyrs1,l)= -999
             arech(nyrs1,l)= -999
             gwcu(nyrs1,l)= -999
             tdp(nyrs1,l)= -999
             crop_cus(nyrs1,l)= -999
             crop_cuj(nyrs1,l)= -999
             crop_cuo(nyrs1,l)= -999
             crop_cut(nyrs1,l)= -999
             soil_cus(nyrs1,l)= -999
             soil_cuj(nyrs1,l)= -999
             soil_cuo(nyrs1,l)= -999
             soil_cu(nyrs1,l)= -999
             ulags(nyrs1,l)= -999
             ulagj(nyrs1,l)= -999
             ulago(nyrs1,l)= -999
             ulagt(nyrs1,l)= -999
             divcu(nyrs1,l)= -999
             soiltotts(nyrs1,l)=soiltotts(nyrs1,l)/nyrs
             soiltottj(nyrs1,l)=soiltottj(nyrs1,l)/nyrs
             soiltotto(nyrs1,l)=soiltotto(nyrs1,l)/nyrs
             soiltott(nyrs1,l)=soiltott(nyrs1,l)/nyrs
             wbu(i,nyrs1,l)=wbu(i,nyrs1,l)/nyrs
             cropcusoil(nyrs1,l)= -999
             estcrps(nyrs1,l)= -999
             estcrpj(nyrs1,l)= -999
             estcrpo(nyrs1,l)= -999
             estcrpt(i,nyrs1,l)= -999
             lagrets(nyrs1,l)= -999
             lagretj(nyrs1,l)= -999
             lagreto(nyrs1,l)= -999
             lagrett(nyrs1,l)= -999
             laglates(nyrs1,l)= -999
             laglatej(nyrs1,l)= -999
             laglateo(nyrs1,l)= -999
             laglatet(nyrs1,l)= -999
             totret(nyrs1,l)= -999
             deps(nyrs1,l)= -999
             depj(nyrs1,l)= -999
             depo(nyrs1,l)= -999
             dept(nyrs1,l)= -999
             effcu(nyrs1,l)= -999
             seffcu(nyrs1,l)= -999
             effgw(nyrs1,l)= -999
           endif
410     continue

        ceff(i,nyrs1)=ceff(i,nyrs1)/nyrs
        if(sfeffcnt.gt.0)then
            sfeff(i,nyrs1)=sfeff(i,nyrs1)/sfeffcnt
        else
            sfeff(i,nyrs1)=0.0
        endif
        if(gfeffcnt.gt.0)then
            gfeff(i,nyrs1)=gfeff(i,nyrs1)/gfeffcnt
        else
            gfeff(i,nyrs1)=0.0
        endif
        fleff(i,nyrs1)=fleff(i,nyrs1)/nyrs
        speff(i,nyrs1)=speff(i,nyrs1)/nyrs
c
c if typout(i) .lt. 3, do not write out detailed water budget
c
c        if(typout(i) .lt. 3) goto 577     
        IOUTP=1
c
c--write out header information depending on isuply option
c
Cjhb=&==================================================================
Cjhb      the following works now with new version of sper (but how did it work before??!)
Cjhb=&==================================================================
        eff(i)=ceff(i,1)*(sper(i,1)*speff(i,1)+(1-sper(i,1))*fleff(i,1))
Cjhb=&==================================================================
Cjhb      structure data when ISUPLY=1
Cjhb=&==================================================================
        if(isuply .eq. 1) then
          write(256,719) bas_id(i)
          write(256,751)
          write(256,708) scapatot(i,1)
          write(256,781) totmo
          write(256,709) eff(i)
c
c ew 03/12/04 include note in header if drain/tailwater is available to structure
          if(itail(i) .eq. 1) then
             write(256,722)
          else
             write(256,751)
          endif
          write(256,721) nyr1, nyr2
          write(256,700)
          write(256,701)
          write(256,702)
          write(256,703)
          write(256,704)
          write(256,705)
          write(256,706)
          write(256,700)
Cjhb=&==================================================================
Cjhb      writing the binary file data record for this structure
Cjhb=&==================================================================
          IF(LBD3OUT .AND. ITIME.EQ.2) THEN
          ENDIF
Cjhb=&==================================================================
        endif

Cjhb=&==================================================================
Cjhb      structure data when ISUPLY=2
Cjhb=&==================================================================
        if (isuply .eq. 2) then
          write(256,819) bas_id(i)
          write(256,851)
          write(256,808) scapatot(i,1)
          write(256,883) senmo,junmo,othmo
          write(256,809) eff(i)
c
c ew 03/12/04 include note in header if drain/tailwater is available to structure
          if(itail(i) .eq. 1) then
             write(256,822) 
          else
             write(256,851)
          endif
c grb 06-29-00 add administration processing type
          if (IDAILY.EQ.3.OR.IDAILY.EQ.5) write(256,824) adminent
c grb 11-08-00 correct print of senior and junior amount to subscripted values
c          if (IDAILY.EQ.3.OR.IDAILY.EQ.5) write(256,810) senaspt+junaspt
c	    if (IDAILY.EQ.3.OR.IDAILY.EQ.5) write(256,811) senaspt
c          if (IDAILY.EQ.3.OR.IDAILY.EQ.5) write(256,812) junaspt
      if (IDAILY.EQ.3.OR.IDAILY.EQ.5) write(256,810) senasp(I)+junasp(I)
	    if (IDAILY.EQ.3.OR.IDAILY.EQ.5) write(256,811) senasp(I)
          if (IDAILY.EQ.3.OR.IDAILY.EQ.5) write(256,812) junasp(I)
	    if (IDAILY.NE.3.and.IDAILY.NE.5) write(256,*) 'Variable
     1		 administration dates'
	    if (IDAILY.NE.3.and.IDAILY.NE.5) write (256,*) ''
	    if (IDAILY.NE.3.and.IDAILY.NE.5) write (256,*) ''
          if (IDAILY.NE.3.and.IDAILY.NE.5) write (256,*) ''
          write(256,851)
          write(256,821) nyr1, nyr2
          write(256,800)
          write(256,801)
          write(256,802)
          write(256,803)
          write(256,804)
          write(256,805)
          write(256,806)
          write(256,800)
Cjhb=&==================================================================
Cjhb      writing the binary file data record for this structure
Cjhb=&==================================================================
          IF(LBD3OUT .AND. ITIME.EQ.2) THEN
            IF(IDAILY.EQ.3 .OR. IDAILY.EQ.5) THEN
            ELSEIF(IDAILY.NE.3 .AND. IDAILY.NE.5) THEN
            ELSE
            ENDIF
          ENDIF
Cjhb=&==================================================================
        endif

Cjhb=&==================================================================
Cjhb      structure data when ISUPLY=3
Cjhb=&==================================================================
        if (isuply .eq. 3) then
          write(256,919) bas_id(i)
          write(256,951)
          write(256,908) scapatot(i,1)
          write(256,983) senmo,junmo,othmo
          write(256,909) eff(i)
c
c ew 03/12/04 include note in header if drain/tailwater is available to structure
          if(itail(i) .eq. 1) then
             write(256,922) 
          else
             write(256,951)
          endif
          write(256,914)
          write(256,970)
          write(256,960)
          write(256,970)
          write(256,980) (retn(i,j)*100,j=1,24)
          write(256,970)
          write(256,951)
c grb 06-29-00 add variable administration processes
          if (IDAILY.EQ.3.OR.IDAILY.EQ.5)write(256,824) adminent
          if (IDAILY.EQ.3.OR.IDAILY.EQ.5) write(256,810) senaspt+junaspt
	    if (IDAILY.EQ.3.OR.IDAILY.EQ.5) write(256,811) senaspt
          if (IDAILY.EQ.3.OR.IDAILY.EQ.5) write(256,812) junaspt
	    if (IDAILY.NE.3.and.IDAILY.NE.5) write(256,*) 'Variable
     1		 administration dates'
	    if (IDAILY.NE.3.and.IDAILY.NE.5) write (256,*) ''
	    if (IDAILY.NE.3.and.IDAILY.NE.5) write (256,*) ''
          if (IDAILY.NE.3.and.IDAILY.NE.5) write (256,*) ''
          write(256,951)
          write(256,921) nyr1, nyr2
          write(256,900)
          write(256,901)
          write(256,902)
          write(256,903)
          write(256,904)
          write(256,905)
          write(256,906)
          write(256,900)
Cjhb=&==================================================================
Cjhb      writing the binary file data record for this structure
Cjhb=&==================================================================
          IF(LBD3OUT .AND. ITIME.EQ.2) THEN
            IF(IDAILY.EQ.3 .OR. IDAILY.EQ.5) THEN
            ELSEIF(IDAILY.NE.3 .AND. IDAILY.NE.5) THEN
            ELSE
            ENDIF
          ENDIF
Cjhb=&==================================================================
        endif

Cjhb=&==================================================================
Cjhb      structure data when ISUPLY=4
Cjhb=&==================================================================
        if(isuply .eq. 4) then
          write(256,619) bas_id(i)
          write(256,651)
          if(gmode(i,m) .eq. 1) then
           if(gper(i,m).eq. 0. .or. sper(i,m) .eq. 0.) then
              write(256,608) scapatot(i,1)
           else
c              write(256,608) scapatot(i,1)*gper(i,1)*sper(i,1)
              if(t_area(i,m).gt.0.0)then
                write(256,608) scapatot(i,1)*
     &            (swflac(i,m)+swspac(i,m)+swgwflac(i,m))/t_area(i,m)
              else
                write(256,608) scapatot(i,1)*
     &            0.0
              endif
           endif
          else
           write(256,608) scapatot(i,1)
          endif
          write(256,681) totmo
          write(256,609) eff(i)
          write(256,651)
          if(itail(i) .eq. 1) then
             write(256,646) 
          else
             write(256,651) 
          endif
          write(256,621) nyr1, nyr2
          select case (iflood)
            case (0)
              write(256,600)
              write(256,601)
              write(256,602)
              write(256,603)
              write(256,604)
              write(256,605)
              write(256,606)
              write(256,600)
            case (1)
              write(256,2600)
              write(256,2601)
              write(256,2602)
              write(256,2603)
              write(256,2604)
              write(256,2605)
              write(256,2606)
              write(256,2600)
            case (2)
              write(256,3600)
              write(256,3601)
              write(256,3602)
              write(256,3603)
              write(256,3604)
              write(256,3605)
              write(256,3606)
              write(256,3600)
            case (3)
              write(256,4600)
              write(256,4601)
              write(256,4602)
              write(256,4603)
              write(256,4604)
              write(256,4605)
              write(256,4606)
              write(256,4600)
            case (4)
              write(256,5600)
              write(256,5601)
              write(256,5602)
              write(256,5603)
              write(256,5604)
              write(256,5605)
              write(256,5606)
              write(256,5600)
          end select
Cjhb=&==================================================================
Cjhb      writing the binary file data record for this structure
Cjhb        note: I replaced M with 1 in the following lines...
Cjhb=&==================================================================
          IF(LBD3OUT .AND. ITIME.EQ.2) THEN
            IF(GMODE(I,1) .EQ. 1) THEN
              IF(GPER(I,1).EQ.0 .OR. SPER(I,1).EQ.0) THEN
              ELSE
              ENDIF
            ELSE
            ENDIF
          ENDIF
        endif !isuply .eq. 4

C-----Report forms (isuply=1, 2, 3, 4) for detailed water budget
***********************************************************************
*  Report Form for ISUPLY = 1 - Supply Limited, no water rights considered
***********************************************************************
        if (isuply .eq. 1) then
c
c write totals for each year (for structure i)
c
          imiss = 0
          do 7065 m=1,nyrs
c grb 05-20-00 set method descriptor
!            if (missflag(i,m).eq.1) then
!              method='Prorated  '
!              method2='Prorated  '
!              imiss=1
!            endif
!            if(missflag(i,m).eq.0) method='Calculated'
            method ='Calculated'
            if( (missdiv(i,m).eq.1) .and. (imiss2.eq.1) ) then
              method ='Prorated  '
              imiss=1
            endif
            if((ulagt(m,13).lt.-998.0).or.(closs(m,13).lt.-998.0)) then
              nonconsumed=-999.0
            else
              nonconsumed=ulagt(m,13)+closs(m,13)
            endif
            write(256,712) nyr1+m-1,method,ettot(i,m,13),effppt(i,m,13),
     :        reqt(i,m,13),wbu(i,m,12),reqreqts(m,13),divsup(i,m,13),
     :        crop_cut(m,13),soil_cu(m,13),nonconsumed,
     :        divcu(m,13),seffcu(m,13),soiltott(m,12),crop_cut(m,13),
     :        cropcusoil(m,13),estcrpt(i,m,13)
7065      continue
c
c  write annual average for all years (for structure i)
c
          write(256,751)
c grb 05-20-00 set method descriptor
!          if (imiss.eq.1) method='Prorated  '
!          if(imiss.eq.0) method='Calculated'
          method ='Calculated'
          if(imiss.eq.1) then
            method ='Prorated  '
          endif
            if((ulagt(nyrs1,13).lt.-998.0).or.
     &         (closs(nyrs1,13).lt.-998.0)) then
              nonconsumed=-999.0
            else
              nonconsumed=ulagt(nyrs1,13)+closs(nyrs1,13)
            endif
          write(256,714) method,ettot(i,nyrs1,13),effppt(i,nyrs1,13),
     :      reqt(i,nyrs1,13),wbu(i,nyrs1,13)/nyrs/12,reqreqts(nyrs1,13),
     :      divsup(i,nyrs1,13),crop_cut(nyrs1,13),soil_cu(nyrs1,13),
     :      nonconsumed,divcu(nyrs1,13),
     :      seffcu(nyrs1,13),soiltott(nyrs1,13)/nyrs/12,
     :      crop_cut(nyrs1,13),cropcusoil(nyrs1,13),estcrpt(i,nyrs1,13)
          do k1=1,3
            write(256,751) 
          enddo
c
c  write monthly average for all years (for structure i)
c
          write(256,718) nyr1, nyr2
            if((ulagt(nyrs1,l).lt.-998.0).or.
     &         (closs(nyrs1,l).lt.-998.0)) then
              nonconsumed=-999.0
            else
              nonconsumed=ulagt(nyrs1,l)+closs(nyrs1,l)
            endif
          do l=1,12
            write(256,710) amn(l),method,ettot(i,nyrs1,l),
     :        effppt(i,nyrs1,l),reqt(i,nyrs1,l),wbu(i,nyrs1,l),
     :        reqreqts(nyrs1,l),divsup(i,nyrs1,l),crop_cut(nyrs1,l),
     :        soil_cu(nyrs1,l),nonconsumed,
     :        divcu(nyrs1,l),seffcu(nyrs1,l),soiltott(nyrs1,l),
     :        crop_cut(nyrs1,l),cropcusoil(nyrs1,l),estcrpt(i,nyrs1,l)
c           comeff(l)=ceff(i,nyrs1)*(seffcu(nyrs1,l)/100)
            comeff(l)=seffcu(nyrs1,l)/100
            if(comeff(l) .lt. 0.005) then
              comeff(l)=ceff(i,nyrs1)*sfeff(i,nyrs1)
            endif
          enddo

          if(ddcsw .ge. 1) then
            twdid=bas_id(i)
            write(5,660) twdid(1:12),(comeff(l)*100,l=1,12),twdid(13:24)
          endif
c
c  write annual average for all years (for structure i)
c
            if((ulagt(nyrs1,13).lt.-998.0).or.
     &         (closs(nyrs1,13).lt.-998.0)) then
              nonconsumed=-999.0
            else
              nonconsumed=ulagt(nyrs1,13)+closs(nyrs1,13)
            endif
          write(256,751)
          write(256,723) method,ettot(i,nyrs1,13),effppt(i,nyrs1,13),
     :      reqt(i,nyrs1,13),wbu(i,nyrs1,13)/nyrs/12,reqreqts(nyrs1,13),
     :      divsup(i,nyrs1,13),crop_cut(nyrs1,13),soil_cu(nyrs1,13),
     :      nonconsumed,divcu(nyrs1,13),
     :      seffcu(nyrs1,13),soiltott(nyrs1,13)/nyrs/12,
     :      crop_cut(nyrs1,13),cropcusoil(nyrs1,13),estcrpt(i,nyrs1,13)
c
c write out monthly values for each year (for structure i)
c
          do k1=1,2
            write(256,751) 
          enddo
          do m=1,nyrs
c grb 05-20-00 set method descriptor
!            if (missflag(i,m).eq.1) method='Prorated  '
!            if(missflag(i,m).eq.0) method='Calculated'
            method ='Calculated'
            if( (missdiv(i,m).eq.1) .and. (imiss2.eq.1) ) then
              method ='Prorated  '
            endif
            write(256,751)
            write(256,752) nyr1+m-1,comment(m)
            iparce=nparce(i,m)+1
            id1=15-iparce
            write(256,744) (crpname(j,m),j=1,iparce),(as(k),k=1,id1)
            write(256,745)(atxt(i,j,m),j=1,iparce),(as(k),k=1,id1)
            if( (mod(i,25).eq.0) .and. (m.eq.1)) then
              write(0,*)'  processed through structure #',i
            endif
            do l=1,12
C=====================================================
Cjhb          add shortage calculation to bd1 output
              SHORTAGE=reqreqts(m,l)-estcrpt(i,m,l)
C=====================================================
            if((ulagt(m,l).lt.-998.0).or.(closs(m,l).lt.-998.0)) then
              nonconsumed=-999.0
            else
              nonconsumed=ulagt(m,l)+closs(m,l)
            endif
              write(256,711) amn(l),method,ettot(i,m,l),effppt(i,m,l),
     :          reqt(i,m,l),wbu(i,m,l),reqreqts(m,l),
     :          ddhmonot(m,l),crop_cut(m,l),soil_cu(m,l),
     :          nonconsumed,
     :          divcu(m,l),seffcu(m,l),soiltott(m,l),
     :          crop_cut(m,l),cropcusoil(m,l),estcrpt(i,m,l)

Cjhb====================================================================
Cjhb  write a record to the binary file if binary output option selected
Cjhb  report structure:
Cjhb=&==================================================================
                IF(LBD1OUT.and.(ipresim.ne.1)) THEN
                  WRITE(UNIT=IBD1UN)
Cjhb====================================================================
Cjhb  write index values as the first three values and then the month string
Cjhb  (don't really need this last parameter),
Cjhb=&==================================================================
Cjhb  1 INTEGER I - basin index
Cjhb  2 INTEGER NYR1+M-1 - year
Cjhb  3 INTEGER L - month index
Cjhb  4 CHAR*3 AMN(L) - abbr month string  ***REPORT***
     :            I,NYR1+M-1,L,AMN(L),
Cjhb====================================================================
Cjhb  write some annual but non-monthly time series report data
Cjhb=&==================================================================
Cjhb  CHAR*84 COMMENT(M) - annual water budget comment (REMOVED from binary file)
Cjhb  5 REAL t_area(i,m) - annual total crop acreage
Cjhb  6 REAL m_area(i,m,l) - annual modeled crop acreage (=t_area if not missing data or missing divs are filled, =0 otherwise)
Cjhb  7 CHAR*10 method - Analysis method - "Calculated" vs. "Prorated"  ***REPORT***
Cjhb :            comment(m),t_area(i,m),method,
     :            t_area(i,m),m_area(i,m,l),method,
Cjhb====================================================================
Cjhb  now write the monthly data (report and other) into the record
Cjhb=&==================================================================
Cjhb  8 REAL ettot(i,m,l) - Potential Crop ET  ***REPORT***
Cjhb  9 REAL effppt(i,m,l) - Effective Precip  ***REPORT***
     :            ettot(i,m,l),effppt(i,m,l),
Cjhb  10 REAL reqt(i,m,l) - Irrigation Water Requirement IWR  ***REPORT***
Cjhb  11 REAL wbu(i,m,l) - EOM Winter Precip Carryover  ***REPORT***
Cjhb  12 REAL reqreqts(m,l) - IWR After Winter Precip  ***REPORT***
     :            reqt(i,m,l),wbu(i,m,l),reqreqts(m,l),
Cjhb  13 REAL ddhmonot(m,l) - River Diversion Acct. - Historic Diversion  ***REPORT***
     :            ddhmonot(m,l),
Cjhb  14 REAL ceff(i,m) - River Diversion Acct. - conveyance Efficiency
Cjhb  15 REAL closs(m,l) - River Diversion Acct. - conveyance Loss
Cjhb  16 REAL fdiv(m,l) - River Diversion Acct. - Farm Headgate Diversion
C removed Cjhb  16 REAL arech(m,l) - River Diversion Acct. - Sprinkler FHG (Not Applied)
C added   Cjhb  17 REAL tail(i,m,l) - Tail water,
c removed - in here twice Cjhb  17 REAL sfeff(i,m) - River Diversion Acct. - Max Applic Effic
Cjhb :            ceff(i,m),closs(m,l),fdiv(m,l),arech(m,l),sfeff(i,m),
c     :            ceff(i,m),closs(m,l),fdiv(m,l),tail(i,m,l),sfeff(i,m),
     :            ceff(i,m),closs(m,l),fdiv(m,l),tail(i,m,l),
Cjhb  18 REAL crop_cut(m,l) - River Diversion Acct. - SW to CU                  ***REPORT***
Cjhb  19 REAL soil_cu(m,l) - River Diversion Acct. - SW to Soil                 ***REPORT***
Cjhb  20 REAL divcu(m,l) - River Diversion Acct. - SW to CU & Soil              ***REPORT***
Cjhb  21 REAL ulagt(m,l) - River Diversion Acct. - SW_Non_Consumed           ***REPORT***
     :            crop_cut(m,l),soil_cu(m,l),divcu(m,l),
     :            ulagt(m,l),
Cjhb  22 REAL sfeff(i,m) - River Diversion Acct. - Max Application Effic (%)
Cjhb  23 REAL effcu(m,l) - River Diversion Acct. - Calc SW Applic Effic    
Cjhb  24 REAL seffcu(m,l) - River Diversion Acct. - Calc SW System Effic      ***REPORT***
     :            sfeff(i,m),effcu(m,l),seffcu(m,l),
Cjhb  25 REAL soiltott(m,l) - SW Soil Content ***REPORT***
     :            soiltott(m,l),
Cjhb  26 REAL crop_cut(m,l) - Crop CU from SW           ***REPORT***
Cjhb  27 REAL cropcusoil(m,l) - Crop CU from Soil         ***REPORT***
Cjhb  28 REAL estcrpt(i,m,l) - Total Crop CU             ***REPORT***
Cjhb  29 REAL SHORTAGE - CU Shortage             
     :            crop_cut(m,l),cropcusoil(m,l),estcrpt(i,m,l),SHORTAGE
Cjhb====================================================================
c                 update the "district" (sb = sub basin) totals that change by month
Cjhb====================================================================
                  if(ettot(i,m,l).gt.-999.0)then
                    sbettot(sbsb(i),m,l)=
     :                sbettot(sbsb(i),m,l)+ettot(i,m,l)
                    bettot(m,l)=
     :                bettot(m,l)+ettot(i,m,l)
                  endif
                  if(effppt(i,m,l).gt.-999.0)then
                    sbeffppt(sbsb(i),m,l)=
     &                sbeffppt(sbsb(i),m,l)+effppt(i,m,l)
                    beffppt(m,l)=
     &                beffppt(m,l)+effppt(i,m,l)
                  endif
                  if(reqt(i,m,l).gt.-999.0)then
                    sbreqt(sbsb(i),m,l)=
     &                sbreqt(sbsb(i),m,l)+reqt(i,m,l)
                    breqt(m,l)=
     &                breqt(m,l)+reqt(i,m,l)
                  endif
                  if(wbu(i,m,l).gt.-999.0)then
                    sbwbu(sbsb(i),m,l)=
     &                sbwbu(sbsb(i),m,l)+wbu(i,m,l)
                    bwbu(m,l)=
     &                bwbu(m,l)+wbu(i,m,l)
                  endif
                  if(reqreqts(m,l).gt.-999.0)then
                    sbreqreq(sbsb(i),m,l)=
     &                sbreqreq(sbsb(i),m,l)+reqreqts(m,l)
                    breqreq(m,l)=
     &                breqreq(m,l)+reqreqts(m,l)
                  endif
                  if(seniorf(m,l).gt.-999.0)then
                    sbseniorf(sbsb(i),m,l)=
     &                sbseniorf(sbsb(i),m,l)+seniorf(m,l)
                    bseniorf(m,l)=
     &                bseniorf(m,l)+seniorf(m,l)
                  endif
                  if(juniorf(m,l).gt.-999.0)then
                    sbjuniorf(sbsb(i),m,l)=
     &                sbjuniorf(sbsb(i),m,l)+juniorf(m,l)
                    bjuniorf(m,l)=
     &                bjuniorf(m,l)+juniorf(m,l)
                  endif
                  if(otherf(m,l).gt.-999.0)then
                    sbotherf(sbsb(i),m,l)=
     &                sbotherf(sbsb(i),m,l)+otherf(m,l)
                    botherf(m,l)=
     &                botherf(m,l)+otherf(m,l)
                  endif
                  if(divsup(i,m,l).gt.-999.0)then
                    sbdivsup(sbsb(i),m,l)=
     &                sbdivsup(sbsb(i),m,l)+divsup(i,m,l)
                    bdivsup(m,l)=
     &                bdivsup(m,l)+divsup(i,m,l)
                  endif
                  if(closs(m,l).gt.-999.0)then
                    sbcloss(sbsb(i),m,l)=
     &                sbcloss(sbsb(i),m,l)+closs(m,l)
                    bcloss(m,l)=
     &                bcloss(m,l)+closs(m,l)
                  endif
                  if(fdiv(m,l).gt.-999.0)then
                    sbfdiv(sbsb(i),m,l)=
     &                sbfdiv(sbsb(i),m,l)+fdiv(m,l)
                    bfdiv(m,l)=
     &                bfdiv(m,l)+fdiv(m,l)
                  endif
                  sbtail(sbsb(i),m,l)=
     &              sbtail(sbsb(i),m,l)+tail(i,m,l)
                  btail(m,l)=
     &              btail(m,l)+tail(i,m,l)
                  if(arech(m,l).gt.-999.0)then
                    sbarech(sbsb(i),m,l)=
     &                sbarech(sbsb(i),m,l)+arech(m,l)
                    barech(m,l)=
     &                barech(m,l)+arech(m,l)
                  endif
                  if(crop_cus(m,l).gt.-999.0)then
                    sbcrop_cus(sbsb(i),m,l)=
     &                sbcrop_cus(sbsb(i),m,l)+crop_cus(m,l)
                    bcrop_cus(m,l)=
     &                bcrop_cus(m,l)+crop_cus(m,l)
                  endif
                  if(crop_cuj(m,l).gt.-999.0)then
                    sbcrop_cuj(sbsb(i),m,l)=
     &                sbcrop_cuj(sbsb(i),m,l)+crop_cuj(m,l)
                    bcrop_cuj(m,l)=
     &                bcrop_cuj(m,l)+crop_cuj(m,l)
                  endif
                  if(crop_cuo(m,l).gt.-999.0)then
                    sbcrop_cuo(sbsb(i),m,l)=
     &                sbcrop_cuo(sbsb(i),m,l)+crop_cuo(m,l)
                    bcrop_cuo(m,l)=
     &                bcrop_cuo(m,l)+crop_cuo(m,l)
                  endif
                  if(crop_cut(m,l).gt.-999.0)then
                    sbcrop_cut(sbsb(i),m,l)=
     &                sbcrop_cut(sbsb(i),m,l)+crop_cut(m,l)
                    bcrop_cut(m,l)=
     &                bcrop_cut(m,l)+crop_cut(m,l)
                  endif
                  if(soil_cus(m,l).gt.-999.0)then
                    sbsoil_cus(sbsb(i),m,l)=
     &                sbsoil_cus(sbsb(i),m,l)+soil_cus(m,l)
                    bsoil_cus(m,l)=
     &                bsoil_cus(m,l)+soil_cus(m,l)
                  endif
                  if(soil_cuj(m,l).gt.-999.0)then
                    sbsoil_cuj(sbsb(i),m,l)=
     &                sbsoil_cuj(sbsb(i),m,l)+soil_cuj(m,l)
                    bsoil_cuj(m,l)=
     &                bsoil_cuj(m,l)+soil_cuj(m,l)
                  endif
                  if(soil_cuo(m,l).gt.-999.0)then
                    sbsoil_cuo(sbsb(i),m,l)=
     &                sbsoil_cuo(sbsb(i),m,l)+soil_cuo(m,l)
                    bsoil_cuo(m,l)=
     &                bsoil_cuo(m,l)+soil_cuo(m,l)
                  endif
                  if(soil_cu(m,l).gt.-999.0)then
                    sbsoil_cu(sbsb(i),m,l)=
     &                sbsoil_cu(sbsb(i),m,l)+soil_cu(m,l)
                    bsoil_cu(m,l)=
     &                bsoil_cu(m,l)+soil_cu(m,l)
                  endif
                  if(soiltotts(m,l).gt.-999.0)then
                    sbsoiltotts(sbsb(i),m,l)=
     &                sbsoiltotts(sbsb(i),m,l)+soiltotts(m,l)
                    bsoiltotts(m,l)=
     &                bsoiltotts(m,l)+soiltotts(m,l)
                  endif
                  if(soiltottj(m,l).gt.-999.0)then
                    sbsoiltottj(sbsb(i),m,l)=
     &                sbsoiltottj(sbsb(i),m,l)+soiltottj(m,l)
                    bsoiltottj(m,l)=
     &                bsoiltottj(m,l)+soiltottj(m,l)
                  endif
                  if(soiltotto(m,l).gt.-999.0)then
                    sbsoiltotto(sbsb(i),m,l)=
     &                sbsoiltotto(sbsb(i),m,l)+soiltotto(m,l)
                    bsoiltotto(m,l)=
     &                bsoiltotto(m,l)+soiltotto(m,l)
                  endif
                  if(soiltott(m,l).gt.-999.0)then
                    sbsoiltott(sbsb(i),m,l)=
     &                sbsoiltott(sbsb(i),m,l)+soiltott(m,l)
                    bsoiltott(m,l)=
     &                bsoiltott(m,l)+soiltott(m,l)
                  endif
                  if(estcrps(m,l).gt.-999.0)then
                    sbestcrps(sbsb(i),m,l)=
     &                sbestcrps(sbsb(i),m,l)+estcrps(m,l)
                    bestcrps(m,l)=
     &                bestcrps(m,l)+estcrps(m,l)
                  endif
                  if(estcrpj(m,l).gt.-999.0)then
                    sbestcrpj(sbsb(i),m,l)=
     &                sbestcrpj(sbsb(i),m,l)+estcrpj(m,l)
                    bestcrpj(m,l)=
     &                bestcrpj(m,l)+estcrpj(m,l)
                  endif
                  if(estcrpo(m,l).gt.-999.0)then
                    sbestcrpo(sbsb(i),m,l)=
     &                sbestcrpo(sbsb(i),m,l)+estcrpo(m,l)
                    bestcrpo(m,l)=
     &                bestcrpo(m,l)+estcrpo(m,l)
                  endif
                  if(estcrpt(i,m,l).gt.-999.0)then
                    sbestcrpt(sbsb(i),m,l)=
     &                sbestcrpt(sbsb(i),m,l)+estcrpt(i,m,l)
                    bestcrpt(m,l)=
     &                bestcrpt(m,l)+estcrpt(i,m,l)
                  endif
                  if(cropcusoil(m,l).gt.-999.0)then
                    sbcropcusoil(sbsb(i),m,l)=
     &                sbcropcusoil(sbsb(i),m,l)+cropcusoil(m,l)
                    bcropcusoil(m,l)=
     &                bcropcusoil(m,l)+cropcusoil(m,l)
                  endif
                  if(divcu(m,l).gt.-999.0)then
                    sbdivcu(sbsb(i),m,l)=
     :                sbdivcu(sbsb(i),m,l)+divcu(m,l)
                    bdivcu(m,l)=
     :                bdivcu(m,l)+divcu(m,l)
                  endif
                  if(ulags(m,l).gt.-999.0)then
                    sbulags(sbsb(i),m,l)=sbulags(sbsb(i),m,l)+
     &                ulags(m,l)-holdps*(1.-ceff(i,m))
                    bulags(m,l)=bulags(m,l)+
     &                ulags(m,l)-holdps*(1.-ceff(i,m))
                  endif
                  if(ulagj(m,l).gt.-999.0)then
                    sbulagj(sbsb(i),m,l)=sbulagj(sbsb(i),m,l)+
     &                ulagj(m,l)-holdpj*(1.-ceff(i,m))
                    bulagj(m,l)=bulagj(m,l)+
     &                ulagj(m,l)-holdpj*(1.-ceff(i,m))
                  endif
                  if(ulago(m,l).gt.-999.0)then
                    sbulago(sbsb(i),m,l)=sbulago(sbsb(i),m,l)+
     &                ulago(m,l)-holdpo*(1.-ceff(i,m))
                    bulago(m,l)=bulago(m,l)+
     &                ulago(m,l)-holdpo*(1.-ceff(i,m))
                  endif
                  if(ulagt(m,l).gt.-999.0)then
                    sbulagt(sbsb(i),m,l)=
     &                sbulagt(sbsb(i),m,l)+ulagt(m,l)
                    bulagt(m,l)=
     &                bulagt(m,l)+ulagt(m,l)
                  endif
                  sbsfeff(sbsb(i),m,l)=sbsfeff(sbsb(i),m,l)+
     &              sfeff(i,m)*t_area(i,m)
                  bsfeff(m,l)=bsfeff(m,l)+
     &              sfeff(i,m)*t_area(i,m)
                  if(shortage.gt.-999.0)then
                    sbshortage(sbsb(i),m,l)=
     :                sbshortage(sbsb(i),m,l)+shortage
                    bshortage(m,l)=
     :                bshortage(m,l)+shortage
                  endif
                  
Cjhb====================================================================
                ENDIF !(LBD1OUT)
Cjhb====================================================================
              enddo !l=1,12
Cjhb====================================================================
c             update the district totals that only change annually
Cjhb====================================================================
              IF(LBD1OUT.and.(ipresim.ne.1)) THEN
              ENDIF
Cjhb====================================================================
         enddo !do m=1,nyrs
Cjhb====================================================================
           write(256,751)
           write(256,751)
           write(256,751)
      endif !(isuply .eq. 1)

C**********************************************************************
C  Report Form for ISUPLY = 2 - Supply Limited, water rights considered
C**********************************************************************
        if (isuply .eq. 2) then
c
c write totals for each year (for structure i)
c
          imiss=0
          do m=1,nyrs

c grb 05-20-00 set method descriptor
!         if (missflag(i,m).eq.1) then
!	      method='Prorated  '
!              method2='Prorated  '
!	      imiss=1
!	      endif
!         if(missflag(i,m).eq.0) method='Calculated'
            method ='Calculated'
            if( (missdiv(i,m).eq.1) .and. (imiss2.eq.1) ) then
              method ='Prorated  '
              imiss=1
            endif
            if((ulagt(m,13).lt.-998.0).or.(closs(m,13).lt.-998.0)) then
              nonconsumed=-999.0
            else
              nonconsumed=ulagt(m,13)+closs(m,13)
            endif
        write(256,835) nyr1+m-1,method,ettot(i,m,13),effppt(i,m,13),
     :   reqt(i,m,13),wbu(i,m,12),reqreqts(m,13),
     :    seniorf(m,13),juniorf(m,13),otherf(m,13),divsup(i,m,13),
     :    crop_cus(m,13),crop_cuj(m,13),crop_cuo(m,13),crop_cut(m,13),
     :    soil_cus(m,13),soil_cuj(m,13),soil_cuo(m,13),soil_cu(m,13),
c     :    ulags(m,13),ulagj(m,13),ulago(m,13),ulagt(m,13),divcu(m,13),
     :    ulags(m,13),ulagj(m,13),ulago(m,13),nonconsumed,divcu(m,13),
     :    seffcu(m,13),soiltotts(m,12),soiltottj(m,12),
     :    soiltotto(m,12),soiltott(m,12),crop_cut(m,13),
     :    cropcusoil(m,13),estcrps(m,13),estcrpj(m,13),estcrpo(m,13),
     :    estcrpt(i,m,13),estcrpj(m,13)
        enddo
c
c  write annual average for all years (for structure i)
c
          write(256,851) 

c grb 05-20-00 set method descriptor
!         if (imiss.eq.1) method='Prorated  '
!         if(imiss.eq.0) method='Calculated'
          method ='Calculated'
          if(imiss.eq.1) then
            method ='Prorated  '
          endif
            if((ulagt(nyrs1,13).lt.-998.0).or.
     &         (closs(nyrs1,13).lt.-998.0)) then
              nonconsumed=-999.0
            else
              nonconsumed=ulagt(nyrs1,13)+closs(nyrs1,13)
            endif
          write(256,836) method,ettot(i,nyrs1,13),effppt(i,nyrs1,13),
     :    reqt(i,nyrs1,13),wbu(i,nyrs1,13)/nyrs/12, reqreqts(nyrs1,13),
     :    seniorf(nyrs1,13),juniorf(nyrs1,13),
     :    otherf(nyrs1,13),divsup(i,nyrs1,13),
     :    crop_cus(nyrs1,13),crop_cuj(nyrs1,13),
     :    crop_cuo(nyrs1,13),crop_cut(nyrs1,13),
     :    soil_cus(nyrs1,13),soil_cuj(nyrs1,13),
     :    soil_cuo(nyrs1,13),soil_cu(nyrs1,13),
     :    ulags(nyrs1,13),ulagj(nyrs1,13),
c     :    ulago(nyrs1,13),ulagt(nyrs1,13),
     :    ulago(nyrs1,13),nonconsumed,
     :    divcu(nyrs1,13),seffcu(nyrs1,13),
     :    soiltotts(nyrs1,13)/nyrs/12,soiltottj(nyrs1,13)/nyrs/12,
     :    soiltotto(nyrs1,13)/nyrs/12,soiltott(nyrs1,13)/nyrs/12,
     :    crop_cut(nyrs1,13),cropcusoil(nyrs1,13),
     :    estcrps(nyrs1,13),estcrpj(nyrs1,13),
     :    estcrpo(nyrs1,13),estcrpt(i,nyrs1,13),
     :    estcrpj(nyrs1,13)
c
c  write monthly average for all years (for structure i)
c
         do k1=1,3
            write(256,851) 
         enddo
         write(256,818) nyr1, nyr2
            if((ulagt(nyrs1,l).lt.-998.0).or.
     &         (closs(nyrs1,l).lt.-998.0)) then
              nonconsumed=-999.0
            else
              nonconsumed=ulagt(nyrs1,l)+closs(nyrs1,l)
            endif
         do l=1,12
           write(256,833) amn(l),method,ettot(i,nyrs1,l),
     :	 effppt(i,nyrs1,l),
     :    reqt(i,nyrs1,l),wbu(i,nyrs1,l),reqreqts(nyrs1,l),
     :    seniorf(nyrs1,l),juniorf(nyrs1,l),otherf(nyrs1,l),
     :    divsup(i,nyrs1,l),crop_cus(nyrs1,l),crop_cuj(nyrs1,l),
     :    crop_cuo(nyrs1,l),crop_cut(nyrs1,l),soil_cus(nyrs1,l),
     :    soil_cuj(nyrs1,l),soil_cuo(nyrs1,l),soil_cu(nyrs1,l),
     :    ulags(nyrs1,l),ulagj(nyrs1,l),ulago(nyrs1,l),
c     :    ulagt(nyrs1,l),divcu(nyrs1,l),seffcu(nyrs1,l),
     :    nonconsumed,divcu(nyrs1,l),seffcu(nyrs1,l),
     :    soiltotts(nyrs1,l),soiltottj(nyrs1,l),soiltotto(nyrs1,l),
     :    soiltott(nyrs1,l),crop_cut(nyrs1,l),cropcusoil(nyrs1,l),
     :    estcrps(nyrs1,l),
     :    estcrpj(nyrs1,l),estcrpo(nyrs1,l),estcrpt(i,nyrs1,l),
     :    estcrpj(nyrs1,l)
        enddo



c
c  write annual average for all years (for structure i)
c
            if((ulagt(nyrs1,13).lt.-998.0).or.
     &         (closs(nyrs1,13).lt.-998.0)) then
              nonconsumed=-999.0
            else
              nonconsumed=ulagt(nyrs1,13)+closs(nyrs1,13)
            endif
          write(256,851)
          write(256,837) method,ettot(i,nyrs1,13),effppt(i,nyrs1,13),
     :    reqt(i,nyrs1,13),wbu(i,nyrs1,13)/nyrs/12,reqreqts(nyrs1,13),
     :    seniorf(nyrs1,13),juniorf(nyrs1,13),
     :    otherf(nyrs1,13),divsup(i,nyrs1,13),
     :    crop_cus(nyrs1,13),crop_cuj(nyrs1,13),
     :    crop_cuo(nyrs1,13),crop_cut(nyrs1,13),
     :    soil_cus(nyrs1,13),soil_cuj(nyrs1,13),
     :    soil_cuo(nyrs1,13),soil_cu(nyrs1,13),
     :    ulags(nyrs1,13),ulagj(nyrs1,13),
c     :    ulago(nyrs1,13),ulagt(nyrs1,13),
     :    ulago(nyrs1,13),nonconsumed,
     :    divcu(nyrs1,13),seffcu(nyrs1,13),
     :    soiltotts(nyrs1,13)/nyrs/12,soiltottj(nyrs1,13)/nyrs/12,
     :    soiltotto(nyrs1,13)/nyrs/12,soiltott(nyrs1,13)/nyrs/12,
     :    crop_cut(nyrs1,13),cropcusoil(nyrs1,13),
     :    estcrps(nyrs1,13),estcrpj(nyrs1,13),
     :    estcrpo(nyrs1,13),estcrpt(i,nyrs1,13),
     :    estcrpj(nyrs1,13)
c write out monthly values for each year (for structure i)
          do k1=1,2
            write(256,851) 
          enddo
          do m=1,nyrs
c grb 05-20-00 set method descriptor
!         if (missflag(i,m).eq.1) method='Prorated  '
!         if(missflag(i,m).eq.0) method='Calculated'
            method ='Calculated'
            if( (missdiv(i,m).eq.1) .and. (imiss2.eq.1) ) then
              method ='Prorated  '
            endif
           write(256,851) 
           write(256,852) nyr1+m-1, comment(m)
          iparce=nparce(i,m)+1
          id1=15-iparce
         write(256,844) (crpname(j,m),j=1,iparce),(as(k),k=1,id1)
         write(256,845)(atxt(i,j,m),j=1,iparce),(as(k),k=1,id1)
           if( (mod(i,25).eq.0) .and. (m.eq.1)) then
             write(0,*)'  processed through structure #',i
           endif
           do l=1,12
C=====================================================
Cjhb          add shortage calculation to bd1 output
              SHORTAGE=reqreqts(m,l)-estcrpt(i,m,l)
C=====================================================
            if((ulagt(m,l).lt.-998.0).or.(closs(m,l).lt.-998.0)) then
              nonconsumed=-999.0
            else
              nonconsumed=ulagt(m,l)+closs(m,l)
            endif
              write(256,829) amn(l),method,ettot(i,m,l),effppt(i,m,l),
     :           reqt(i,m,l),wbu(i,m,l),reqreqts(m,l)
c                12/18/08 these are farm deliveries, do not match the label
c                12/18/08 change to river diversions
c                01/19/11 change back to farm deliveries
     :          ,seniorf(m,l),juniorf(m,l),otherf(m,l)
     :          ,ddhmonot(m,l),crop_cus(m,l),crop_cuj(m,l)
     :          ,crop_cuo(m,l),crop_cut(m,l),soil_cus(m,l)
     :          ,soil_cuj(m,l),soil_cuo(m,l),soil_cu(m,l)
c     :          ,ulags(m,l),ulagj(m,l),ulago(m,l),ulagt(m,l)
     :          ,ulags(m,l),ulagj(m,l),ulago(m,l),nonconsumed
     :          ,divcu(m,l),seffcu(m,l),soiltotts(m,l),soiltottj(m,l)
     :          ,soiltotto(m,l),soiltott(m,l),crop_cut(m,l)
     :          ,cropcusoil(m,l),estcrps(m,l),estcrpj(m,l)
     :          ,estcrpo(m,l),estcrpt(i,m,l),estcrpj(m,l)

Cjhb====================================================================
Cjhb  write a record to the binary file if binary output option selected
Cjhb  report structure:
Cjhb=&==================================================================
                IF(LBD1OUT.and.(ipresim.ne.1)) THEN
                  WRITE(UNIT=IBD1UN)
Cjhb====================================================================
Cjhb  write index values as the first three values and then the month string
Cjhb  (don't really need this last parameter),
Cjhb=&==================================================================
Cjhb  1 INTEGER I - basin index
Cjhb  2 INTEGER NYR1+M-1 - year
Cjhb  3 INTEGER L - month index
Cjhb  4 CHAR*3 AMN(L) - abbr month string  ***REPORT***
     :            I,NYR1+M-1,L,AMN(L),
Cjhb====================================================================
Cjhb  write some annual but non-monthly time series report data
Cjhb=&==================================================================
Cjhb  CHAR*84 COMMENT(M) - annual water budget comment (REMOVED from binary file)
Cjhb  5 REAL t_area(i,m) - annual Total Irrigated Acreage 
Cjhb  6 REAL m_area(i,m,l) - monthly modeled crop acreage (=t_area if not missing data or missing divs are filled, =0 otherwise)
Cjhb  7 CHAR*10 method - Analysis Method          - "Calculated" vs. "Prorated"  ***REPORT***
Cjhb :            comment(m),t_area(i,m),method,
     :            t_area(i,m), m_area(i,m,l), method,
Cjhb====================================================================
Cjhb  now write the monthly data (report and other) into the record
Cjhb=&==================================================================
Cjhb  8 REAL ettot(i,m,l) - Potential Crop ET  ***REPORT***
Cjhb  9 REAL effppt(i,m,l) - Effective Precip  ***REPORT***
     :            ettot(i,m,l),effppt(i,m,l),
Cjhb  10 REAL reqt(i,m,l) - Irrigation Water Requirement IWR  ***REPORT***
Cjhb  11 REAL wbu(i,m,l) - EOM Winter Precip Carryover  ***REPORT***
Cjhb  12 REAL reqreqts(m,l) - IWR After WInter Precip  ***REPORT***
     :            reqt(i,m,l),wbu(i,m,l),reqreqts(m,l),
Cjhb  13 REAL seniorf(m,l) - River Diversion Acct. - Div By Priority - Senior  ***REPORT*** - 12/19/08 replaced with holdps - 01/19/11 changed back to seniorf(m,l)
Cjhb  14 REAL juniorf(m,l) - River Diversion Acct. - Div By Priority - Junior  ***REPORT*** - 12/19/08 replaced with holdpj - 01/19/11 changed back to juniorf(m,l)
Cjhb  15 REAL otherf(m,l) - River Diversion Acct. - Div By Priority - Other  ***REPORT*** - 12/19/08 replaced with holdpo - 01/19/11 changed back to otherf(m,l)
Cjhb  16 REAL ddhmonot(m,l) - River Diversion Acct. - Div By Priority - Total  ***REPORT***
c                12/18/08 these are farm deliveries, do not match the label
c                12/18/08 change to river diversions
c                01/19/11 changed back to river diversions
     :            seniorf(m,l),juniorf(m,l),otherf(m,l),ddhmonot(m,l),
Cjhb  17 REAL ceff(i,m) - River Diversion Acct. - Conveyance Efficiency   
Cjhb  18 REAL closs(m,l) - River Diversion Acct. - Conveyance Loss         
Cjhb  19 REAL fdiv(m,l) - River Diversion Acct. - Farm Headgate Delivery  
     :            ceff(i,m),closs(m,l),fdiv(m,l),
Cjhb  20 REAL crop_cus(m,l) - River Diversion Acct. - Diversion to CU - Senior ***REPORT***
Cjhb  21 REAL crop_cuj(m,l) - River Diversion Acct. - Diversion to CU - Junior ***REPORT***
     :            crop_cus(m,l),crop_cuj(m,l),
Cjhb  22 REAL crop_cuo(m,l) - River Diversion Acct. - Diversion to CU - Other ***REPORT***
Cjhb  23 REAL crop_cut(m,l) - River Diversion Acct. - Diversion to CU - Total ***REPORT***
     :            crop_cuo(m,l),crop_cut(m,l),
Cjhb  24 REAL soil_cus(m,l) - River Diversion Acct. - Add to Soil Moisture - Senior ***REPORT***
Cjhb  25 REAL soil_cuj(m,l) - River Diversion Acct. - Add to Soil Moisture - Junior ***REPORT***
     :            soil_cus(m,l),soil_cuj(m,l),
Cjhb  26 REAL soil_cuo(m,l) - River Diversion Acct. - Add to Soil Moisture - Other ***REPORT***
Cjhb  27 REAL soil_cu(m,l) - River Diversion Acct. - Add to Soil Moisture - Total ***REPORT***
     :            soil_cuo(m,l),soil_cu(m,l),
Cjhb  28 REAL divcu(m,l) - River Diversion Acct. - Total Div to CU & Soil  ***REPORT***
     :            divcu(m,l),
Cjhb  29 REAL ulags(m,l) - River Diversion Acct. - Non-Consumed - Senior ***REPORT*** includes canal losses - needs to be adjusted!!
Cjhb  30 REAL ulagj(m,l) - River Diversion Acct. - Non-Consumed - Junior ***REPORT*** includes canal losses - needs to be adjusted!!
Cjhb  31 REAL ulago(m,l) - River Diversion Acct. - Non-Consumed - Other ***REPORT*** includes canal losses - needs to be adjusted!!
Cjhb  32 REAL ulagt(m,l) - River Diversion Acct. - Non-Consumed - Total ***REPORT*** does NOT include canal losses
     :            ulags(m,l)-holdps*(1.-ceff(i,m)),
     :            ulagj(m,l)-holdpj*(1.-ceff(i,m)),
     :            ulago(m,l)-holdpo*(1.-ceff(i,m)),
     :            ulagt(m,l),
C removed Cjhb  31 REAL arech(m,l) - River Diversion Acct. - Eff Calc: Sprinkler FHG (Not Applied)
C added   Cjhb  33 REAL tail(i,m,l) - River Diversion Acct. - Supply_Tail Water_Drains
Cjhb  34 REAL sfeff(i,m) - River Diversion Acct. - Eff Calc: Max Application Effic   
C    :            arech(m,l),sfeff(i,m),
     :            tail(i,m,l),sfeff(i,m),
Cjhb  35 REAL effcu(m,l) - River Diversion Acct. - Eff Calc: Calc Surface Water Applic Effic (%)
Cjhb  36 REAL seffcu(m,l) - River Diversion Acct. - Calc SW System Effic      ***REPORT***
     :            effcu(m,l),seffcu(m,l),
Cjhb  37 REAL soiltotts(m,l) - EOM SW Soil Content - Senior ***REPORT***
Cjhb  38 REAL soiltottj(m,l) - EOM SW Soil Content - Junior ***REPORT***
Cjhb  39 REAL soiltotto(m,l) - EOM SW Soil Content - Other ***REPORT***
Cjhb  40 REAL soiltott(m,l) - EOM SW Soil Content - Total ***REPORT***
     :            soiltotts(m,l),soiltottj(m,l),soiltotto(m,l),
     :            soiltott(m,l),
Cjhb  41 REAL crop_cut(m,l) - Tot Crop CU from Div      ***REPORT***
Cjhb  42 REAL cropcusoil(m,l) - Tot Crop CU from Soil     ***REPORT***
     :            crop_cut(m,l),cropcusoil(m,l),
Cjhb  43 REAL estcrps(m,l) - Estimated Crop CU - By Water Rights - Senior  ***REPORT***
Cjhb  44 REAL estcrpj(m,l) - Estimated Crop CU - By Water Rights - Junior  ***REPORT***
Cjhb  45 REAL estcrpo(m,l) - Estimated Crop CU - By Water Rights - Other  ***REPORT***
Cjhb  46 REAL estcrpt(i,m,l) - Estimated Crop CU - By Water Rights - Total  ***REPORT***
     :            estcrps(m,l),estcrpj(m,l),estcrpo(m,l),estcrpt(i,m,l),
Cjhb  47 REAL estcrpj(m,l) - Replacement Requirement  ***REPORT***
Cjhb  48 REAL SHORTAGE - Total Shortage
     :            estcrpj(m,l),SHORTAGE
Cjhb====================================================================
c                 update the district totals that change by month
Cjhb====================================================================
                  if(ettot(i,m,l).gt.-999.0)then
                    sbettot(sbsb(i),m,l)=
     :                sbettot(sbsb(i),m,l)+ettot(i,m,l)
                    bettot(m,l)=
     :                bettot(m,l)+ettot(i,m,l)
                  endif
                  if(effppt(i,m,l).gt.-999.0)then
                    sbeffppt(sbsb(i),m,l)=
     &                sbeffppt(sbsb(i),m,l)+effppt(i,m,l)
                    beffppt(m,l)=
     &                beffppt(m,l)+effppt(i,m,l)
                  endif
                  if(reqt(i,m,l).gt.-999.0)then
                    sbreqt(sbsb(i),m,l)=
     &                sbreqt(sbsb(i),m,l)+reqt(i,m,l)
                    breqt(m,l)=
     &                breqt(m,l)+reqt(i,m,l)
                  endif
                  if(wbu(i,m,l).gt.-999.0)then
                    sbwbu(sbsb(i),m,l)=
     &                sbwbu(sbsb(i),m,l)+wbu(i,m,l)
                    bwbu(m,l)=
     &                bwbu(m,l)+wbu(i,m,l)
                  endif
                  if(reqreqts(m,l).gt.-999.0)then
                    sbreqreq(sbsb(i),m,l)=
     &                sbreqreq(sbsb(i),m,l)+reqreqts(m,l)
                    breqreq(m,l)=
     &                breqreq(m,l)+reqreqts(m,l)
                  endif
                  if(seniorf(m,l).gt.-999.0)then
                    sbseniorf(sbsb(i),m,l)=
     &                sbseniorf(sbsb(i),m,l)+seniorf(m,l)
                    bseniorf(m,l)=
     &                bseniorf(m,l)+seniorf(m,l)
                  endif
                  if(juniorf(m,l).gt.-999.0)then
                    sbjuniorf(sbsb(i),m,l)=
     &                sbjuniorf(sbsb(i),m,l)+juniorf(m,l)
                    bjuniorf(m,l)=
     &                bjuniorf(m,l)+juniorf(m,l)
                  endif
                  if(otherf(m,l).gt.-999.0)then
                    sbotherf(sbsb(i),m,l)=
     &                sbotherf(sbsb(i),m,l)+otherf(m,l)
                    botherf(m,l)=
     &                botherf(m,l)+otherf(m,l)
                  endif
                  if(divsup(i,m,l).gt.-999.0)then
                    sbdivsup(sbsb(i),m,l)=
     &                sbdivsup(sbsb(i),m,l)+divsup(i,m,l)
                    bdivsup(m,l)=
     &                bdivsup(m,l)+divsup(i,m,l)
                  endif
                  if(closs(m,l).gt.-999.0)then
                    sbcloss(sbsb(i),m,l)=
     &                sbcloss(sbsb(i),m,l)+closs(m,l)
                    bcloss(m,l)=
     &                bcloss(m,l)+closs(m,l)
                  endif
                  if(fdiv(m,l).gt.-999.0)then
                    sbfdiv(sbsb(i),m,l)=
     &                sbfdiv(sbsb(i),m,l)+fdiv(m,l)
                    bfdiv(m,l)=
     &                bfdiv(m,l)+fdiv(m,l)
                  endif
                  if(crop_cus(m,l).gt.-999.0)then
                    sbcrop_cus(sbsb(i),m,l)=
     &                sbcrop_cus(sbsb(i),m,l)+crop_cus(m,l)
                    bcrop_cus(m,l)=
     &                bcrop_cus(m,l)+crop_cus(m,l)
                  endif
                  if(crop_cuj(m,l).gt.-999.0)then
                    sbcrop_cuj(sbsb(i),m,l)=
     &                sbcrop_cuj(sbsb(i),m,l)+crop_cuj(m,l)
                    bcrop_cuj(m,l)=
     &                bcrop_cuj(m,l)+crop_cuj(m,l)
                  endif
                  if(crop_cuo(m,l).gt.-999.0)then
                    sbcrop_cuo(sbsb(i),m,l)=
     &                sbcrop_cuo(sbsb(i),m,l)+crop_cuo(m,l)
                    bcrop_cuo(m,l)=
     &                bcrop_cuo(m,l)+crop_cuo(m,l)
                  endif
                  if(crop_cut(m,l).gt.-999.0)then
                    sbcrop_cut(sbsb(i),m,l)=
     &                sbcrop_cut(sbsb(i),m,l)+crop_cut(m,l)
                    bcrop_cut(m,l)=
     &                bcrop_cut(m,l)+crop_cut(m,l)
                  endif
                  if(soil_cus(m,l).gt.-999.0)then
                    sbsoil_cus(sbsb(i),m,l)=
     &                sbsoil_cus(sbsb(i),m,l)+soil_cus(m,l)
                    bsoil_cus(m,l)=
     &                bsoil_cus(m,l)+soil_cus(m,l)
                  endif
                  if(soil_cuj(m,l).gt.-999.0)then
                    sbsoil_cuj(sbsb(i),m,l)=
     &                sbsoil_cuj(sbsb(i),m,l)+soil_cuj(m,l)
                    bsoil_cuj(m,l)=
     &                bsoil_cuj(m,l)+soil_cuj(m,l)
                  endif
                  if(soil_cuo(m,l).gt.-999.0)then
                    sbsoil_cuo(sbsb(i),m,l)=
     &                sbsoil_cuo(sbsb(i),m,l)+soil_cuo(m,l)
                    bsoil_cuo(m,l)=
     &                bsoil_cuo(m,l)+soil_cuo(m,l)
                  endif
                  if(soil_cu(m,l).gt.-999.0)then
                    sbsoil_cu(sbsb(i),m,l)=
     &                sbsoil_cu(sbsb(i),m,l)+soil_cu(m,l)
                    bsoil_cu(m,l)=
     &                bsoil_cu(m,l)+soil_cu(m,l)
                  endif
                  if(soiltotts(m,l).gt.-999.0)then
                    sbsoiltotts(sbsb(i),m,l)=
     &                sbsoiltotts(sbsb(i),m,l)+soiltotts(m,l)
                    bsoiltotts(m,l)=
     &                bsoiltotts(m,l)+soiltotts(m,l)
                  endif
                  if(soiltottj(m,l).gt.-999.0)then
                    sbsoiltottj(sbsb(i),m,l)=
     &                sbsoiltottj(sbsb(i),m,l)+soiltottj(m,l)
                    bsoiltottj(m,l)=
     &                bsoiltottj(m,l)+soiltottj(m,l)
                  endif
                  if(soiltotto(m,l).gt.-999.0)then
                    sbsoiltotto(sbsb(i),m,l)=
     &                sbsoiltotto(sbsb(i),m,l)+soiltotto(m,l)
                    bsoiltotto(m,l)=
     &                bsoiltotto(m,l)+soiltotto(m,l)
                  endif
                  if(soiltott(m,l).gt.-999.0)then
                    sbsoiltott(sbsb(i),m,l)=
     &                sbsoiltott(sbsb(i),m,l)+soiltott(m,l)
                    bsoiltott(m,l)=
     &                bsoiltott(m,l)+soiltott(m,l)
                  endif
                  if(cropcusoil(m,l).gt.-999.0)then
                    sbcropcusoil(sbsb(i),m,l)=
     &                sbcropcusoil(sbsb(i),m,l)+cropcusoil(m,l)
                    bcropcusoil(m,l)=
     &                bcropcusoil(m,l)+cropcusoil(m,l)
                  endif
                  if(estcrps(m,l).gt.-999.0)then
                    sbestcrps(sbsb(i),m,l)=
     &                sbestcrps(sbsb(i),m,l)+estcrps(m,l)
                    bestcrps(m,l)=
     &                bestcrps(m,l)+estcrps(m,l)
                  endif
                  if(estcrpj(m,l).gt.-999.0)then
                    sbestcrpj(sbsb(i),m,l)=
     &                sbestcrpj(sbsb(i),m,l)+estcrpj(m,l)
                    bestcrpj(m,l)=
     &                bestcrpj(m,l)+estcrpj(m,l)
                  endif
                  if(estcrpo(m,l).gt.-999.0)then
                    sbestcrpo(sbsb(i),m,l)=
     &                sbestcrpo(sbsb(i),m,l)+estcrpo(m,l)
                    bestcrpo(m,l)=
     &                bestcrpo(m,l)+estcrpo(m,l)
                  endif
                  if(estcrpt(i,m,l).gt.-999.0)then
                    sbestcrpt(sbsb(i),m,l)=
     &                sbestcrpt(sbsb(i),m,l)+estcrpt(i,m,l)
                    bestcrpt(m,l)=
     &                bestcrpt(m,l)+estcrpt(i,m,l)
                  endif
                  if(divcu(m,l).gt.-999.0)then
                    sbdivcu(sbsb(i),m,l)=
     &                sbdivcu(sbsb(i),m,l)+divcu(m,l)
                    bdivcu(m,l)=
     &                bdivcu(m,l)+divcu(m,l)
                  endif
                  if(ulags(m,l).gt.-999.0)then
                    sbulags(sbsb(i),m,l)=sbulags(sbsb(i),m,l)+
     &                ulags(m,l)-holdps*(1.-ceff(i,m))
                    bulags(m,l)=bulags(m,l)+
     &                ulags(m,l)-holdps*(1.-ceff(i,m))
                  endif
                  if(ulagj(m,l).gt.-999.0)then
                    sbulagj(sbsb(i),m,l)=sbulagj(sbsb(i),m,l)+
     &                ulagj(m,l)-holdpj*(1.-ceff(i,m))
                    bulagj(m,l)=bulagj(m,l)+
     &                ulagj(m,l)-holdpj*(1.-ceff(i,m))
                  endif
                  if(ulago(m,l).gt.-999.0)then
                    sbulago(sbsb(i),m,l)=sbulago(sbsb(i),m,l)+
     &                ulago(m,l)-holdpo*(1.-ceff(i,m))
                    bulago(m,l)=bulago(m,l)+
     &                ulago(m,l)-holdpo*(1.-ceff(i,m))
                  endif
                  if(ulagt(m,l).gt.-999.0)then
                    sbulagt(sbsb(i),m,l)=
     &                sbulagt(sbsb(i),m,l)+ulagt(m,l)
                    bulagt(m,l)=
     &                bulagt(m,l)+ulagt(m,l)
                  endif
                  sbtail(sbsb(i),m,l)=
     &              sbtail(sbsb(i),m,l)+tail(i,m,l)
                  btail(m,l)=
     &              btail(m,l)+tail(i,m,l)
                  if(arech(m,l).gt.-999.0)then
                    sbarech(sbsb(i),m,l)=
     &                sbarech(sbsb(i),m,l)+arech(m,l)
                    barech(m,l)=
     &                barech(m,l)+arech(m,l)
                  endif
                  sbsfeff(sbsb(i),m,l)=sbsfeff(sbsb(i),m,l)+
     &              sfeff(i,m)*t_area(i,m)
                  bsfeff(m,l)=bsfeff(m,l)+
     &              sfeff(i,m)*t_area(i,m)
                  if(shortage.gt.-999.0)then
                    sbshortage(sbsb(i),m,l)=
     :                sbshortage(sbsb(i),m,l)+shortage
                    bshortage(m,l)=
     :                bshortage(m,l)+shortage
                  endif
Cjhb====================================================================
                ENDIF !(LBD1OUT)
Cjhb====================================================================
              enddo !l=1,12
Cjhb====================================================================
c             update the district totals that only change annually
Cjhb====================================================================
              IF(LBD1OUT.and.(ipresim.ne.1)) THEN
              ENDIF
Cjhb====================================================================
7062       enddo !m=1,nyrs
         write(256,851)                          
         write(256,851)
         write(256,851)
         endif !(isuply .eq. 2)

C********************************************************************************************
C  Report Form for ISUPLY = 3 - Supply Limited, water rights and return flows considered
C********************************************************************************************	
        if (isuply .eq. 3) then
c
c write totals for each year (for structure i)
c
          imiss=0
          do m=1,nyrs
c grb 05-20-00 set method descriptor
!         if (missflag(i,m).eq.1) then
!	      method='Prorated  '
!              method2='Prorated  '
!	      imiss=1
!	     endif
!         if(missflag(i,m).eq.0) method='Calculated'
            method ='Calculated'
            if( (missdiv(i,m).eq.1) .and. (imiss2.eq.1) ) then
              method ='Prorated  '
              imiss=1
            endif
        write(256,935) nyr1+m-1,method,ettot(i,m,13),effppt(i,m,13),
     :    reqt(i,m,13),wbu(i,m,12),reqreqts(m,13),
     :    seniorf(m,13),juniorf(m,13),otherf(m,13),divsup(i,m,13),
     :    crop_cus(m,13),crop_cuj(m,13),crop_cuo(m,13),crop_cut(m,13),
     :    soil_cus(m,13),soil_cuj(m,13),soil_cuo(m,13),soil_cu(m,13),
     :    ulags(m,13),ulagj(m,13),ulago(m,13),ulagt(m,13),divcu(m,13),
     :    seffcu(m,13),soiltotts(m,12),soiltottj(m,12),
     :    soiltotto(m,12),soiltott(m,12),crop_cut(m,13)
     :    ,cropcusoil(m,13),estcrps(m,13),estcrpj(m,13),estcrpo(m,13)
     :    ,estcrpt(i,m,13),lagrets(m,13),lagretj(m,13)
     :    ,lagreto(m,13),lagrett(m,13),laglates(m,13),laglatej(m,13)
     :    ,laglateo(m,13),laglatet(m,13),totret(m,13),deps(m,13)
     :    ,depj(m,13),depo(m,13),dept(m,13),depj(m,13)
       enddo
c
c  write annual average for all years (for structure i)
c
          write(256,951)
c grb 5-20-00 set method
!         if (imiss.gt.0) method='Prorated  '
!         if (imiss.eq.0) method='Calculated'
          method ='Calculated'
          if(imiss.eq.1) then
            method ='Prorated  '
          endif
          write(256,936) method,ettot(i,nyrs1,13),effppt(i,nyrs1,13),
     :    reqt(i,nyrs1,13),wbu(i,nyrs1,13)/nyrs/12,reqreqts(nyrs1,13),
     :    seniorf(nyrs1,13),
     :    juniorf(nyrs1,13),otherf(nyrs1,13),
     :    divsup(i,nyrs1,13),crop_cus(nyrs1,13),
     :    crop_cuj(nyrs1,13),crop_cuo(nyrs1,13),
     :    crop_cut(nyrs1,13),soil_cus(nyrs1,13),
     :    soil_cuj(nyrs1,13),soil_cuo(nyrs1,13),
     :    soil_cu(nyrs1,13),ulags(nyrs1,13),
     :    ulagj(nyrs1,13),ulago(nyrs1,13),
     :    ulagt(nyrs1,13),divcu(nyrs1,13),seffcu(nyrs1,13),
     :    soiltotts(nyrs1,13)/nyrs/12,soiltottj(nyrs1,13)/nyrs/12,
     :    soiltotto(nyrs1,13)/nyrs/12,soiltott(nyrs1,13)/nyrs/12,
     :    crop_cut(nyrs1,13),cropcusoil(nyrs1,13),
     :    estcrps(nyrs1,13),estcrpj(nyrs1,13),
     :    estcrpo(nyrs1,13),estcrpt(i,nyrs1,13),
     :    lagrets(nyrs1,13),lagretj(nyrs1,13),
     :    lagreto(nyrs1,13),lagrett(nyrs1,13),
     :    laglates(nyrs1,13),laglatej(nyrs1,13),
     :    laglateo(nyrs1,13),laglatet(nyrs1,13),
     :    totret(nyrs1,13),deps(nyrs1,13),
     :    depj(nyrs1,13),depo(nyrs1,13),dept(nyrs1,13),
     :    depj(nyrs1,13)
c
c  write monthly average for all years (for structure i)
c
          do k1=1,3
              write(256,951) 
          enddo 
        write(256,918) nyr1, nyr2
        do l=1,12
          write(256,933) amn(l),method,ettot(i,nyrs1,l),
     :	effppt(i,nyrs1,l),
     :    reqt(i,nyrs1,l),wbu(i,nyrs1,l),reqreqts(nyrs1,l),
     :    seniorf(nyrs1,l),juniorf(nyrs1,l),otherf(nyrs1,l),
     :    divsup(i,nyrs1,l),crop_cus(nyrs1,l),crop_cuj(nyrs1,l),
     :    crop_cuo(nyrs1,l),crop_cut(nyrs1,l),soil_cus(nyrs1,l),
     :    soil_cuj(nyrs1,l),soil_cuo(nyrs1,l),soil_cu(nyrs1,l),
     :    ulags(nyrs1,l),ulagj(nyrs1,l),ulago(nyrs1,l),
     :    ulagt(nyrs1,l),divcu(nyrs1,l),seffcu(nyrs1,l),
     :    soiltotts(nyrs1,l),soiltottj(nyrs1,l),soiltotto(nyrs1,l),
     :    soiltott(nyrs1,l),crop_cut(nyrs1,l),cropcusoil(nyrs1,l),
     :    estcrps(nyrs1,l),
     :    estcrpj(nyrs1,l),estcrpo(nyrs1,l),estcrpt(i,nyrs1,l),
     :    lagrets(nyrs1,l),lagretj(nyrs1,l),lagreto(nyrs1,l),
     :    lagrett(nyrs1,l),laglates(nyrs1,l),laglatej(nyrs1,l),
     :    laglateo(nyrs1,l),laglatet(nyrs1,l),totret(nyrs1,l),
     :    deps(nyrs1,l),depj(nyrs1,l),depo(nyrs1,l),dept(nyrs1,l),
     :    depj(nyrs1,l)
        enddo

c
c  write annual average for all years (for structure i)
c
          write(256,951)
          write(256,937) method,ettot(i,nyrs1,13),effppt(i,nyrs1,13),
     :    reqt(i,nyrs1,13),wbu(i,nyrs1,13)/nyrs/12,reqreqts(nyrs1,13),
     :    seniorf(nyrs1,13),
     :    juniorf(nyrs1,13),otherf(nyrs1,13),
     :    divsup(i,nyrs1,13),crop_cus(nyrs1,13),
     :    crop_cuj(nyrs1,13),crop_cuo(nyrs1,13),
     :    crop_cut(nyrs1,13),soil_cus(nyrs1,13),
     :    soil_cuj(nyrs1,13),soil_cuo(nyrs1,13),
     :    soil_cu(nyrs1,13),ulags(nyrs1,13),
     :    ulagj(nyrs1,13),ulago(nyrs1,13),
     :    ulagt(nyrs1,13),divcu(nyrs1,13),seffcu(nyrs1,13),
     :    soiltotts(nyrs1,13)/nyrs/12,soiltottj(nyrs1,13)/nyrs/12,
     :    soiltotto(nyrs1,13)/nyrs/12,soiltott(nyrs1,13)/nyrs/12,
     :    crop_cut(nyrs1,13),cropcusoil(nyrs1,13),
     :    estcrps(nyrs1,13),estcrpj(nyrs1,13),
     :    estcrpo(nyrs1,13),estcrpt(i,nyrs1,13),
     :    lagrets(nyrs1,13),lagretj(nyrs1,13),
     :    lagreto(nyrs1,13),lagrett(nyrs1,13),
     :    laglates(nyrs1,13),laglatej(nyrs1,13),
     :    laglateo(nyrs1,13),laglatet(nyrs1,13),
     :    totret(nyrs1,13),deps(nyrs1,13),
     :    depj(nyrs1,13),depo(nyrs1,13),dept(nyrs1,13),
     :    depj(nyrs1,13)

c
c write out monthly values for each year (for structure i)
c
          do k1=1,2
            write(256,951) 
          enddo
          do m=1,nyrs
c grb 05-20-00 set method descriptor
!         if (missflag(i,m).eq.1) method='Prorated  '
!         if(missflag(i,m).eq.0) method='Calculated'
            method ='Calculated'
            if( (missdiv(i,m).eq.1) .and. (imiss2.eq.1) ) then
              method ='Prorated  '
            endif
          write(256,951) 
          write(256,952) nyr1+m-1,comment(m)
          iparce=nparce(i,m)+1
          id1=15-iparce
        write(256,944) (crpname(j,m),j=1,iparce),(as(k),k=1,id1)
        write(256,945)(atxt(i,j,m),j=1,iparce),(as(k),k=1,id1)
           if( (mod(i,25).eq.0) .and. (m.eq.1)) then
             write(0,*)'  processed through structure #',i
           endif
          do l=1,12
C==================================================
Cjhb        add shortage calculation to bd1 output
            SHORTAGE=reqreqts(m,l)-estcrpt(i,m,l)
C==================================================
            write(256,929)  amn(l),method,ettot(i,m,l),effppt(i,m,l),
     :          reqt(i,m,l),wbu(i,m,l),reqreqts(m,l)
     :          ,seniorf(m,l),juniorf(m,l),otherf(m,l)
     :          ,ddhmonot(m,l),crop_cus(m,l),crop_cuj(m,l)
     :          ,crop_cuo(m,l),crop_cut(m,l),soil_cus(m,l)
     :          ,soil_cuj(m,l),soil_cuo(m,l),soil_cu(m,l)
     :          ,ulags(m,l),ulagj(m,l),ulago(m,l),ulagt(m,l)
     :          ,divcu(m,l),seffcu(m,l),soiltotts(m,l),soiltottj(m,l)
     :          ,soiltotto(m,l),soiltott(m,l),crop_cut(m,l)
     :          ,cropcusoil(m,l),estcrps(m,l),estcrpj(m,l)
     :          ,estcrpo(m,l),estcrpt(i,m,l),lagrets(m,l),
     :          lagretj(m,l),lagreto(m,l),lagrett(m,l),
     :          laglates(m,l),laglatej(m,l),laglateo(m,l),
     :          laglatet(m,l),totret(m,l),deps(m,l),
     :          depj(m,l),depo(m,l),dept(m,l),depj(m,l)

Cjhb====================================================================
Cjhb  write a record to the binary file if binary output option selected
Cjhb  report structure:
Cjhb=&==================================================================
                IF(LBD1OUT.and.(ipresim.ne.1)) THEN
                  WRITE(UNIT=IBD1UN)
Cjhb====================================================================
Cjhb  write index values as the first three values and then the month string
Cjhb  (don't really need this last parameter),
Cjhb=&==================================================================
Cjhb  (1) INTEGER I - basin index
Cjhb  (2) INTEGER NYR1+M-1 - year
Cjhb  (3) INTEGER L - month index
Cjhb  (4) CHAR*3 AMN(L) - abbr month string  ***REPORT***
     :            I,NYR1+M-1,L,AMN(L),
Cjhb====================================================================
Cjhb  write some annual but non-monthly time series report data
Cjhb=&==================================================================
Cjhb  CHAR*84 COMMENT(M) - annual water budget comment (REMOVED from binary file)
Cjhb  (5) REAL t_area(i,m) - annual total crop acreage
Cjhb  (6) REAL m_area(i,m,l) - monthly modeled crop acreage (=t_area if not missing data or missing divs are filled, =0 otherwise)
Cjhb  (7) CHAR*10 method - Analysis method - "Calculated" vs. "Prorated"  ***REPORT***
Cjhb :            comment(m),t_area(i,m),method,
     :            t_area(i,m), m_area(i,m,l), method,
Cjhb====================================================================
Cjhb  now write the monthly data (report and other) into the record
Cjhb=&==================================================================
Cjhb  (8) REAL ettot(i,m,l) - Potential Crop ET  ***REPORT***
Cjhb  (9) REAL effppt(i,m,l) - Effective Precip  ***REPORT***
     :            ettot(i,m,l),effppt(i,m,l),
Cjhb  (10) REAL reqt(i,m,l) - Irrigation Water Requirement IWR  ***REPORT***
Cjhb  (11) REAL wbu(i,m,l) - EOM Winter Precip Carryover  ***REPORT***
Cjhb  (12) REAL reqreqts(m,l) - IWR After WInter Precip  ***REPORT***
     :            reqt(i,m,l), wbu(i,m,l), reqreqts(m,l),
Cjhb  (13) REAL seniorf(m,l) - River Diversion Acct. - Div By Priority - Senior  ***REPORT***
Cjhb  (14) REAL juniorf(m,l) - River Diversion Acct. - Div By Priority - Junior  ***REPORT***
Cjhb  (15) REAL otherf(m,l) - River Diversion Acct. - Div By Priority - Other  ***REPORT***
     :            seniorf(m,l), juniorf(m,l), otherf(m,l),
Cjhb  (16) REAL ddhmonot(m,l) - River Diversion Acct. - Div By Priority - Total  ***REPORT***
     :            ddhmonot(m,l),
Cjhb  (17) REAL ceff(i,m) - River Diversion Acct. - Eff Calc: conveyance Efficiency
Cjhb  (18) REAL closs(m,l) - River Diversion Acct. - Eff Calc: conveyance Loss
Cjhb  (19) REAL fdiv(m,l) - River Diversion Acct. - Eff Calc: Farm Headgate Delivery  
     :            ceff(i,m), closs(m,l), fdiv(m,l),
Cjhb  (20) REAL crop_cus(m,l) - River Diversion Acct. - Diversion to CU - Senior ***REPORT***
Cjhb  (21) REAL crop_cuj(m,l) - River Diversion Acct. - Diversion to CU - Junior ***REPORT***
Cjhb  (22) REAL crop_cuo(m,l) - River Diversion Acct. - Diversion to CU - Other ***REPORT***
Cjhb  (23) REAL crop_cut(m,l) - River Diversion Acct. - Diversion to CU - Total ***REPORT***
     :            crop_cus(m,l), crop_cuj(m,l),
     :            crop_cuo(m,l), crop_cut(m,l),
Cjhb  (24) REAL soil_cus(m,l) - River Diversion Acct. - Add to Soil Moisture - Senior ***REPORT***
Cjhb  (25) REAL soil_cuj(m,l) - River Diversion Acct. - Add to Soil Moisture - Junior ***REPORT***
Cjhb  (26) REAL soil_cuo(m,l) - River Diversion Acct. - Add to Soil Moisture - Other ***REPORT***
Cjhb  (27) REAL soil_cu(m,l) - River Diversion Acct. - Add to Soil Moisture - Total ***REPORT***
     :            soil_cus(m,l), soil_cuj(m,l),
     :            soil_cuo(m,l), soil_cu(m,l),
Cjhb  (28) REAL divcu(m,l) - River Diversion Acct. - Total Div to CU & Soil    ***REPORT***
     :            divcu(m,l),
Cjhb  (29) REAL ulags(m,l) - River Diversion Acct. - Non-Consumed - Senior ***REPORT***
Cjhb  (30) REAL ulagj(m,l) - River Diversion Acct. - Non-Consumed - Junior ***REPORT***
Cjhb  (31) REAL ulago(m,l) - River Diversion Acct. - Non-Consumed - Other ***REPORT***
Cjhb  (32) REAL ulagt(m,l) - River Diversion Acct. - Non-Consumed - Total ***REPORT***
     :            ulags(m,l), ulagj(m,l), ulago(m,l), ulagt(m,l),
C removed Cjhb  (31) REAL arech(m,l) - River Diversion Acct. - Eff Calc: Sprinkler FHG (Not Applied)
C     :            arech(m,l),sfeff(i,m),
Cjhb  (33) REAL tail(i,m,l) - Tail water
Cjhb  (34) REAL sfeff(i,m) - River Diversion Acct. - Eff Calc: Max Applic Effic
Cjhb  (35) REAL effcu(m,l) - River Diversion Acct. - Eff Calc: Calc Surface Water Applic Effic (%)
Cjhb  (36) REAL seffcu(m,l) - River Diversion Acct. - Eff Calc: System Effic (%)  ***REPORT***
     :            tail(i,m,l), sfeff(i,m), effcu(m,l), seffcu(m,l),
Cjhb  (37) REAL soiltotts(m,l) - EOM SW Soil Content- Senior ***REPORT***
Cjhb  (38) REAL soiltottj(m,l) - EOM SW Soil Content- Junior ***REPORT***
Cjhb  (39) REAL soiltotto(m,l) - EOM SW Soil Content- Other ***REPORT***
Cjhb  (40) REAL soiltott(m,l) - EOM SW Soil Content- Total ***REPORT***
     :            soiltotts(m,l), soiltottj(m,l),
     :            soiltotto(m,l), soiltott(m,l),
Cjhb  (41) REAL crop_cut(m,l) - Tot Crop CU from Div    ***REPORT***
Cjhb  (42) REAL cropcusoil(m,l) - Total Crop from Soil    ***REPORT***
     :            crop_cut(m,l), cropcusoil(m,l),
Cjhb  (43) REAL estcrps(m,l) - Total Crop CU - Senior  ***REPORT***
Cjhb  (44) REAL estcrpj(m,l) - Total Crop CU - Junior  ***REPORT***
Cjhb  (45) REAL estcrpo(m,l) - Total Crop CU - Other   ***REPORT***
Cjhb  (46) REAL estcrpt(i,m,l) - Total Crop CU           ***REPORT***
     &            estcrps(m,l), estcrpj(m,l),
     &            estcrpo(m,l), estcrpt(i,m,l),
Cjhb  (47) REAL lagrets(m,l) - Months Return Flows - From This Months Div - Senior  ***REPORT***
Cjhb  (48) REAL lagretj(m,l) - Months Return Flows - From This Months Div - Junior  ***REPORT***
Cjhb  (49) REAL lagreto(m,l) - Months Return Flows - From This Months Div - Other  ***REPORT***
Cjhb  (50) REAL lagrett(m,l) - Months Return Flows - From This Months Div - Total  ***REPORT***
     &            lagrets(m,l), lagretj(m,l),
     &            lagreto(m,l), lagrett(m,l),
Cjhb  (51) REAL laglates(m,l) - Months Return Flows - From Prev Months Div - Senior  ***REPORT***
Cjhb  (52) REAL laglatej(m,l) - Months Return Flows - From Prev Months Div - Junior  ***REPORT***
Cjhb  (53) REAL laglateo(m,l) - Months Return Flows - From Prev Months Div - Other  ***REPORT***
     &            laglates(m,l), laglatej(m,l), laglateo(m,l),
Cjhb  (54) REAL laglatet(m,l) - Months Return Flows - From Prev Months Div - Total  ***REPORT***
Cjhb  (55) REAL totret(m,l) - Months Return Flows - Total  ***REPORT***
     &            laglatet(m,l), totret(m,l),
Cjhb  (56) REAL deps(m,l) - River Depl(+)/Accr(-) - By Priority - Senior  ***REPORT***
Cjhb  (57) REAL depj(m,l) - River Depl(+)/Accr(-) - By Priority - Junior  ***REPORT***
Cjhb  (58) REAL depo(m,l) - River Depl(+)/Accr(-) - By Priority - Other  ***REPORT***
Cjhb  (59) REAL dept(m,l) - River Depl(+)/Accr(-) - Total  ***REPORT***
Cjhb  (60) REAL depj(m,l) - Replacement Requirement  ***REPORT***
     &            deps(m,l), depj(m,l), depo(m,l), dept(m,l),
     &            depj(m,l),
Cjhb  (61) REAL SHORTAGE - Total CU Shortage
     &            SHORTAGE
Cjhb====================================================================
c                 update the district totals that change by month
Cjhb====================================================================
                  if(ettot(i,m,l).gt.-999.0)then
                    sbettot(sbsb(i),m,l)=
     :                sbettot(sbsb(i),m,l)+ettot(i,m,l)
                    bettot(m,l)=
     :                bettot(m,l)+ettot(i,m,l)
                  endif
                  if(effppt(i,m,l).gt.-999.0)then
                    sbeffppt(sbsb(i),m,l)=
     &                sbeffppt(sbsb(i),m,l)+effppt(i,m,l)
                    beffppt(m,l)=
     &                beffppt(m,l)+effppt(i,m,l)
                  endif
                  if(reqt(i,m,l).gt.-999.0)then
                    sbreqt(sbsb(i),m,l)=
     &                sbreqt(sbsb(i),m,l)+reqt(i,m,l)
                    breqt(m,l)=
     &                breqt(m,l)+reqt(i,m,l)
                  endif
                  if(wbu(i,m,l).gt.-999.0)then
                    sbwbu(sbsb(i),m,l)=
     &                sbwbu(sbsb(i),m,l)+wbu(i,m,l)
                    bwbu(m,l)=
     &                bwbu(m,l)+wbu(i,m,l)
                  endif
                  if(reqreqts(m,l).gt.-999.0)then
                    sbreqreq(sbsb(i),m,l)=
     &                sbreqreq(sbsb(i),m,l)+reqreqts(m,l)
                    breqreq(m,l)=
     &                breqreq(m,l)+reqreqts(m,l)
                  endif
                  if(seniorf(m,l).gt.-999.0)then
                    sbseniorf(sbsb(i),m,l)=
     &                sbseniorf(sbsb(i),m,l)+seniorf(m,l)
                    bseniorf(m,l)=
     &                bseniorf(m,l)+seniorf(m,l)
                  endif
                  if(juniorf(m,l).gt.-999.0)then
                    sbjuniorf(sbsb(i),m,l)=
     &                sbjuniorf(sbsb(i),m,l)+juniorf(m,l)
                    bjuniorf(m,l)=
     &                bjuniorf(m,l)+juniorf(m,l)
                  endif
                  if(otherf(m,l).gt.-999.0)then
                    sbotherf(sbsb(i),m,l)=
     &                sbotherf(sbsb(i),m,l)+otherf(m,l)
                    botherf(m,l)=
     &                botherf(m,l)+otherf(m,l)
                  endif
                  if(divsup(i,m,l).gt.-999.0)then
                    sbdivsup(sbsb(i),m,l)=
     &                sbdivsup(sbsb(i),m,l)+divsup(i,m,l)
                    bdivsup(m,l)=
     &                bdivsup(m,l)+divsup(i,m,l)
                  endif
                  if(closs(m,l).gt.-999.0)then
                    sbcloss(sbsb(i),m,l)=
     &                sbcloss(sbsb(i),m,l)+closs(m,l)
                    bcloss(m,l)=
     &                bcloss(m,l)+closs(m,l)
                  endif
                  if(fdiv(m,l).gt.-999.0)then
                    sbfdiv(sbsb(i),m,l)=
     &                sbfdiv(sbsb(i),m,l)+fdiv(m,l)
                    bfdiv(m,l)=
     &                bfdiv(m,l)+fdiv(m,l)
                  endif
                  if(crop_cus(m,l).gt.-999.0)then
                    sbcrop_cus(sbsb(i),m,l)=
     &                sbcrop_cus(sbsb(i),m,l)+crop_cus(m,l)
                    bcrop_cus(m,l)=
     &                bcrop_cus(m,l)+crop_cus(m,l)
                  endif
                  if(crop_cuj(m,l).gt.-999.0)then
                    sbcrop_cuj(sbsb(i),m,l)=
     &                sbcrop_cuj(sbsb(i),m,l)+crop_cuj(m,l)
                    bcrop_cuj(m,l)=
     &                bcrop_cuj(m,l)+crop_cuj(m,l)
                  endif
                  if(crop_cuo(m,l).gt.-999.0)then
                    sbcrop_cuo(sbsb(i),m,l)=
     &                sbcrop_cuo(sbsb(i),m,l)+crop_cuo(m,l)
                    bcrop_cuo(m,l)=
     &                bcrop_cuo(m,l)+crop_cuo(m,l)
                  endif
                  if(crop_cut(m,l).gt.-999.0)then
                    sbcrop_cut(sbsb(i),m,l)=
     &                sbcrop_cut(sbsb(i),m,l)+crop_cut(m,l)
                    bcrop_cut(m,l)=
     &                bcrop_cut(m,l)+crop_cut(m,l)
                  endif
                  if(soil_cus(m,l).gt.-999.0)then
                    sbsoil_cus(sbsb(i),m,l)=
     &                sbsoil_cus(sbsb(i),m,l)+soil_cus(m,l)
                    bsoil_cus(m,l)=
     &                bsoil_cus(m,l)+soil_cus(m,l)
                  endif
                  if(soil_cuj(m,l).gt.-999.0)then
                    sbsoil_cuj(sbsb(i),m,l)=
     &                sbsoil_cuj(sbsb(i),m,l)+soil_cuj(m,l)
                    bsoil_cuj(m,l)=
     &                bsoil_cuj(m,l)+soil_cuj(m,l)
                  endif
                  if(soil_cuo(m,l).gt.-999.0)then
                    sbsoil_cuo(sbsb(i),m,l)=
     &                sbsoil_cuo(sbsb(i),m,l)+soil_cuo(m,l)
                    bsoil_cuo(m,l)=
     &                bsoil_cuo(m,l)+soil_cuo(m,l)
                  endif
                  if(soil_cu(m,l).gt.-999.0)then
                    sbsoil_cu(sbsb(i),m,l)=
     &                sbsoil_cu(sbsb(i),m,l)+soil_cu(m,l)
                    bsoil_cu(m,l)=
     &                bsoil_cu(m,l)+soil_cu(m,l)
                  endif
                  if(soiltotts(m,l).gt.-999.0)then
                    sbsoiltotts(sbsb(i),m,l)=
     &                sbsoiltotts(sbsb(i),m,l)+soiltotts(m,l)
                    bsoiltotts(m,l)=
     &                bsoiltotts(m,l)+soiltotts(m,l)
                  endif
                  if(soiltottj(m,l).gt.-999.0)then
                    sbsoiltottj(sbsb(i),m,l)=
     &                sbsoiltottj(sbsb(i),m,l)+soiltottj(m,l)
                    bsoiltottj(m,l)=
     &                bsoiltottj(m,l)+soiltottj(m,l)
                  endif
                  if(soiltotto(m,l).gt.-999.0)then
                    sbsoiltotto(sbsb(i),m,l)=
     &                sbsoiltotto(sbsb(i),m,l)+soiltotto(m,l)
                    bsoiltotto(m,l)=
     &                bsoiltotto(m,l)+soiltotto(m,l)
                  endif
                  if(soiltott(m,l).gt.-999.0)then
                    sbsoiltott(sbsb(i),m,l)=
     &                sbsoiltott(sbsb(i),m,l)+soiltott(m,l)
                    bsoiltott(m,l)=
     &                bsoiltott(m,l)+soiltott(m,l)
                  endif
                  if(cropcusoil(m,l).gt.-999.0)then
                    sbcropcusoil(sbsb(i),m,l)=
     &                sbcropcusoil(sbsb(i),m,l)+cropcusoil(m,l)
                    bcropcusoil(m,l)=
     &                bcropcusoil(m,l)+cropcusoil(m,l)
                  endif
                  if(estcrps(m,l).gt.-999.0)then
                    sbestcrps(sbsb(i),m,l)=
     &                sbestcrps(sbsb(i),m,l)+estcrps(m,l)
                    bestcrps(m,l)=
     &                bestcrps(m,l)+estcrps(m,l)
                  endif
                  if(estcrpj(m,l).gt.-999.0)then
                    sbestcrpj(sbsb(i),m,l)=
     &                sbestcrpj(sbsb(i),m,l)+estcrpj(m,l)
                    bestcrpj(m,l)=
     &                bestcrpj(m,l)+estcrpj(m,l)
                  endif
                  if(estcrpo(m,l).gt.-999.0)then
                    sbestcrpo(sbsb(i),m,l)=
     &                sbestcrpo(sbsb(i),m,l)+estcrpo(m,l)
                    bestcrpo(m,l)=
     &                bestcrpo(m,l)+estcrpo(m,l)
                  endif
                  if(estcrpt(i,m,l).gt.-999.0)then
                    sbestcrpt(sbsb(i),m,l)=
     &                sbestcrpt(sbsb(i),m,l)+estcrpt(i,m,l)
                    bestcrpt(m,l)=
     &                bestcrpt(m,l)+estcrpt(i,m,l)
                  endif
                  if(divcu(m,l).gt.-999.0)then
                    sbdivcu(sbsb(i),m,l)=
     &                sbdivcu(sbsb(i),m,l)+divcu(m,l)
                    bdivcu(m,l)=
     &                bdivcu(m,l)+divcu(m,l)
                  endif
                  if(ulags(m,l).gt.-999.0)then
                    sbulags(sbsb(i),m,l)=sbulags(sbsb(i),m,l)+
     &                ulags(m,l)-holdps*(1.-ceff(i,m))
                    bulags(m,l)=bulags(m,l)+
     &                ulags(m,l)-holdps*(1.-ceff(i,m))
                  endif
                  if(ulagj(m,l).gt.-999.0)then
                    sbulagj(sbsb(i),m,l)=sbulagj(sbsb(i),m,l)+
     &                ulagj(m,l)-holdpj*(1.-ceff(i,m))
                    bulagj(m,l)=bulagj(m,l)+
     &                ulagj(m,l)-holdpj*(1.-ceff(i,m))
                  endif
                  if(ulago(m,l).gt.-999.0)then
                    sbulago(sbsb(i),m,l)=sbulago(sbsb(i),m,l)+
     &                ulago(m,l)-holdpo*(1.-ceff(i,m))
                    bulago(m,l)=bulago(m,l)+
     &                ulago(m,l)-holdpo*(1.-ceff(i,m))
                  endif
                  if(ulagt(m,l).gt.-999.0)then
                    sbulagt(sbsb(i),m,l)=
     &                sbulagt(sbsb(i),m,l)+ulagt(m,l)
                    bulagt(m,l)=
     &                bulagt(m,l)+ulagt(m,l)
                  endif
                  if(lagrets(m,l).gt.-999.0)then
                    sblagrets(sbsb(i),m,l)=
     &                sblagrets(sbsb(i),m,l)+lagrets(m,l)
                    blagrets(m,l)=
     &                blagrets(m,l)+lagrets(m,l)
                  endif
                  if(lagretj(m,l).gt.-999.0)then
                    sblagretj(sbsb(i),m,l)=
     &                sblagretj(sbsb(i),m,l)+lagretj(m,l)
                    blagretj(m,l)=
     &                blagretj(m,l)+lagretj(m,l)
                  endif
                  if(lagreto(m,l).gt.-999.0)then
                    sblagreto(sbsb(i),m,l)=
     &                sblagreto(sbsb(i),m,l)+lagreto(m,l)
                    blagreto(m,l)=
     &                blagreto(m,l)+lagreto(m,l)
                  endif
                  if(lagrett(m,l).gt.-999.0)then
                    sblagrett(sbsb(i),m,l)=
     &                sblagrett(sbsb(i),m,l)+lagrett(m,l)
                    blagrett(m,l)=
     &                blagrett(m,l)+lagrett(m,l)
                  endif
                  if(totret(m,l).gt.-999.0)then
                    sbtotret(sbsb(i),m,l)=
     &                sbtotret(sbsb(i),m,l)+totret(m,l)
                    btotret(m,l)=
     &                btotret(m,l)+totret(m,l)
                  endif
                  if(laglates(m,l).gt.-999.0)then
                    sblaglates(sbsb(i),m,l)=
     &                sblaglates(sbsb(i),m,l)+laglates(m,l)
                    blaglates(m,l)=
     &                blaglates(m,l)+laglates(m,l)
                  endif
                  if(laglatej(m,l).gt.-999.0)then
                    sblaglatej(sbsb(i),m,l)=
     &                sblaglatej(sbsb(i),m,l)+laglatej(m,l)
                    blaglatej(m,l)=
     &                blaglatej(m,l)+laglatej(m,l)
                  endif
                  if(laglateo(m,l).gt.-999.0)then
                    sblaglateo(sbsb(i),m,l)=
     &                sblaglateo(sbsb(i),m,l)+laglateo(m,l)
                    blaglateo(m,l)=
     &                blaglateo(m,l)+laglateo(m,l)
                  endif
                  if(laglatet(m,l).gt.-999.0)then
                    sblaglatet(sbsb(i),m,l)=
     &                sblaglatet(sbsb(i),m,l)+laglatet(m,l)
                    blaglatet(m,l)=
     &                blaglatet(m,l)+laglatet(m,l)
                  endif
                  if(deps(m,l).gt.-999.0)then
                    sbdeps(sbsb(i),m,l)=
     &                sbdeps(sbsb(i),m,l)+deps(m,l)
                    bdeps(m,l)=
     &                bdeps(m,l)+deps(m,l)
                  endif
                  if(depj(m,l).gt.-999.0)then
                    sbdepj(sbsb(i),m,l)=
     &                sbdepj(sbsb(i),m,l)+depj(m,l)
                    bdepj(m,l)=
     &                bdepj(m,l)+depj(m,l)
                  endif
                  if(depo(m,l).gt.-999.0)then
                    sbdepo(sbsb(i),m,l)=
     &                sbdepo(sbsb(i),m,l)+depo(m,l)
                    bdepo(m,l)=
     &                bdepo(m,l)+depo(m,l)
                  endif
                  if(dept(m,l).gt.-999.0)then
                    sbdept(sbsb(i),m,l)=
     &                sbdept(sbsb(i),m,l)+dept(m,l)
                    bdept(m,l)=
     &                bdept(m,l)+dept(m,l)
                  endif
                  sbtail(sbsb(i),m,l)=
     &              sbtail(sbsb(i),m,l)+tail(i,m,l)
                  btail(m,l)=
     &              btail(m,l)+tail(i,m,l)
                  if(arech(m,l).gt.-999.0)then
                    sbarech(sbsb(i),m,l)=
     &                sbarech(sbsb(i),m,l)+arech(m,l)
                    barech(m,l)=
     &                barech(m,l)+arech(m,l)
                  endif
                  sbsfeff(sbsb(i),m,l)=sbsfeff(sbsb(i),m,l)+
     &              sfeff(i,m)*t_area(i,m)
                  bsfeff(m,l)=bsfeff(m,l)+
     &              sfeff(i,m)*t_area(i,m)
                  if(shortage.gt.-999.0)then
                    sbshortage(sbsb(i),m,l)=
     :                sbshortage(sbsb(i),m,l)+shortage
                    bshortage(m,l)=
     :                bshortage(m,l)+shortage
                  endif
Cjhb====================================================================
                ENDIF !(LBD1OUT)
Cjhb====================================================================
              enddo !l=1,12
Cjhb====================================================================
c             update the district totals that only change annually
Cjhb====================================================================
              IF(LBD1OUT.and.(ipresim.ne.1)) THEN
              ENDIF
Cjhb====================================================================
            enddo !m=1,nyrs
            write(256,951)
            write(256,951)
            write(256,951)
      endif !(isuply .eq. 3)
***********************************************************************
*  Report Form for ISUPLY = 4 - Supply Limited, ground water considered
***********************************************************************
      if (isuply .eq. 4) then
c
c write totals for each year (for structure i)
c
        imiss=0
        do 6065 m=1,nyrs
c grb 05-20-00 set method descriptor
!         if (missflag(i,m).eq.1) then
!	      method='Prorated  '
!          method2='Prorated  '
!	      imiss=1
!         endif
!         if(missflag(i,m).eq.0) method='Calculated'
            method ='Calculated'
            if( (missdiv(i,m).eq.1) .and. (imiss2.eq.1) ) then
              method ='Prorated  '
              imiss=1
            endif
          if(crop_cut(m,13) .gt. -998) then
             cutot=crop_cut(m,13)+gwcu(m,13)
             cust=estcrpt(i,m,13)+gwcu(m,13)
          else
             cutot=-999
             cust=-999
             effgw(m,13)=-999
          endif
          short = reqreqts(m,13)-cust
          if(iflood .eq. 0) then
          write(256,612) nyr1+m-1,method,ettot(i,m,13),effppt(i,m,13),
     :    reqt(i,m,13),wbu(i,m,12),reqreqts(m,13),
     :    divsup(i,m,13),ceff(i,m),closs(m,13),fdiv(m,13),arech(m,13),
     :    sfeff(i,m),crop_cut(m,13),soil_cu(m,13),ulagt(m,13),
     :    effcu(m,13),gdiv(m,13),effgw(m,13),gwcu(m,13),gwro(m,13),
     :    soiltott(m,12),cutot,cropcusoil(m,13),cust,tdp(m,13)
          else
          write(256,632) nyr1+m-1,method,ettot(i,m,13),effppt(i,m,13),
     :    reqt(i,m,13),wbu(i,m,12),reqreqts(m,13),
     :    divsup(i,m,13),ceff(i,m),closs(m,13),fdiv(m,13),arech(m,13),
     :    sfeff(i,m),crop_cut(m,13),soil_cu(m,13),ulagt(m,13),
     :    effcu(m,13),gdiv(m,13),effgw(m,13),gwcu(m,13),gwro(m,13),
     :    soiltott(m,12),cutot,cropcusoil(m,13),cust,tdp(m,13),
     :    gsdiv(m,13),gfdiv(m,13),short,
     :    (grass(i,m,13,ifx), ifx=1,iflood2),
     :     tail(i,m,13)
          endif

6065    continue
c
c  write annual average for all years (for structure i)
c
        write(256,651)
          if(crop_cut(nyrs1,13) .gt. -998) then
             cutot=crop_cut(nyrs1,13)+gwcu(nyrs1,13)
             cust=estcrpt(i,nyrs1,13)+gwcu(nyrs1,13)
             short = reqreqts(nyrs1,13)-cust
          else
             cutot=-999
             cust=-999
             short=-999
          endif

c grb 05-20-00 set method descriptor
!         if (imiss.eq.1) method='Prorated  '
!         if(imiss.eq.0) method='Calculated'
          method ='Calculated'
          if(imiss.eq.1) then
            method ='Prorated  '
          endif
        if(iflood .eq. 0) then
        write(256,614) method,ettot(i,nyrs1,13),effppt(i,nyrs1,13),
     :  reqt(i,nyrs1,13),wbu(i,nyrs1,13)/nyrs/12,reqreqts(nyrs1,13),
     :  divsup(i,nyrs1,13),
     :  ceff(i,nyrs1),closs(nyrs1,13),fdiv(nyrs1,13),arech(nyrs1,13),
     :  sfeff(i,nyrs1),crop_cut(nyrs1,13),soil_cu(nyrs1,13),
     :  ulagt(nyrs1,13),effcu(nyrs1,13),gdiv(nyrs1,13),effgw(nyrs1,13),
     :  gwcu(nyrs1,13),gwro(nyrs1,13),
     :  soiltott(nyrs1,13)/nyrs/12,cutot,
     :  cropcusoil(nyrs1,13),cust,tdp(nyrs1,13)
        else
        write(256,634) method,ettot(i,nyrs1,13),effppt(i,nyrs1,13),
     :  reqt(i,nyrs1,13),wbu(i,nyrs1,13)/nyrs/12,reqreqts(nyrs1,13),
     :  divsup(i,nyrs1,13),
     :  ceff(i,nyrs1),closs(nyrs1,13),fdiv(nyrs1,13),arech(nyrs1,13),
     :  sfeff(i,nyrs1),crop_cut(nyrs1,13),soil_cu(nyrs1,13),
     :  ulagt(nyrs1,13),effcu(nyrs1,13),gdiv(nyrs1,13),effgw(nyrs1,13),
     :  gwcu(nyrs1,13),gwro(nyrs1,13),
     :  soiltott(nyrs1,13)/nyrs/12,cutot,
     :  cropcusoil(nyrs1,13),cust,tdp(nyrs1,13),gsdiv(nyrs1,13),
     :  gfdiv(nyrs1,13),short,(grass(i,nyrs1,13,ifx), ifx=1,iflood2),
     :     tail(i,nyrs1,13)

        endif
        do k1=1,3
           write(256,651) 
        enddo
c
c  write monthly average for all years (for structure i)
c
        write(256,618) nyr1, nyr2
        do l=1,12
          if(crop_cut(nyrs1,l) .gt. -998) then
             cutot=crop_cut(nyrs1,l)+gwcu(nyrs1,l)
             cust=estcrpt(i,nyrs1,l)+gwcu(nyrs1,l)
             short = reqreqts(nyrs1,l)-cust
          else
             cutot=-999
             cust=-999
             short=-999
          endif
          if(iflood .eq. 0) then
            write(256,610)
     &    amn(l),method,ettot(i,nyrs1,l),effppt(i,nyrs1,l),
     :    reqt(i,nyrs1,l),wbu(i,nyrs1,l),reqreqts(nyrs1,l),
     :    divsup(i,nyrs1,l),ceff(i,nyrs1),closs(nyrs1,l),fdiv(nyrs1,l),
     :    arech(nyrs1,l),sfeff(i,nyrs1),crop_cut(nyrs1,l),
     :    soil_cu(nyrs1,l),ulagt(nyrs1,l),effcu(nyrs1,l),gdiv(nyrs1,l),
     :    effgw(nyrs1,l),gwcu(nyrs1,l),gwro(nyrs1,l),
     :    soiltott(nyrs1,l),cutot,cropcusoil(nyrs1,l),cust,
     :    tdp(nyrs1,l)
          else
            write(256,630)
     &      amn(l),method,ettot(i,nyrs1,l),effppt(i,nyrs1,l),
     :    reqt(i,nyrs1,l),wbu(i,nyrs1,l),reqreqts(nyrs1,l),
     :    divsup(i,nyrs1,l),ceff(i,nyrs1),closs(nyrs1,l),fdiv(nyrs1,l),
     :    arech(nyrs1,l),sfeff(i,nyrs1),crop_cut(nyrs1,l),
     :    soil_cu(nyrs1,l),ulagt(nyrs1,l),effcu(nyrs1,l),gdiv(nyrs1,l),
     :    effgw(nyrs1,l),gwcu(nyrs1,l),gwro(nyrs1,l),
     :    soiltott(nyrs1,l),cutot,cropcusoil(nyrs1,l),cust,tdp(nyrs1,l),
     :    gsdiv(nyrs1,l),gfdiv(nyrs1,l),short,
     :    (grass(i,nyrs1,l,ifx), ifx=1, iflood2),
     :     tail(i,nyrs1,l)
          endif
c          comeff(l)=ceff(i,nyrs1)*(effcu(nyrs1,l)/100)
          comeff(l)=seffcu(nyrs1,l)/100.0
          if(comeff(l) .lt. 0.005) then
            comeff(l)=ceff(i,nyrs1)*sfeff(i,nyrs1)
          endif
          if(effgw(nyrs1,l) .lt. 0.005) then
            effgw(nyrs1,l)=gfeff(i,nyrs1)
          endif
        enddo

        if(ddcsw .ge. 1) then
          twdid=bas_id(i)
          write(5,660)twdid(1:12),(comeff(l)*100.0,l=1,12),twdid(13:24)
          write(6,660)twdid(1:12),(effgw(nyrs1,l)*100.0,l=1,12),
     :     twdid(13:24)
        endif
c
c  write annual average for all years (for structure i)
c
        write(256,651)
          if(crop_cut(nyrs1,13) .gt. -998) then
             cutot=crop_cut(nyrs1,13)+gwcu(nyrs1,13)
             cust=estcrpt(i,nyrs1,13)+gwcu(nyrs1,13)
             short = reqreqts(nyrs1,13)-cust
          else
             cutot=-999
             cust=-999
             short=-999
          endif
        if(iflood .eq. 0) then
        write(256,626) method,ettot(i,nyrs1,13),effppt(i,nyrs1,13),
     :  reqt(i,nyrs1,13),wbu(i,nyrs1,13)/nyrs/12,reqreqts(nyrs1,13),
     :  divsup(i,nyrs1,13),
     :  ceff(i,nyrs1),closs(nyrs1,13),fdiv(nyrs1,13),arech(nyrs1,13),
     :  sfeff(i,nyrs1),crop_cut(nyrs1,13),soil_cu(nyrs1,13),
     :  ulagt(nyrs1,13),effcu(nyrs1,13),gdiv(nyrs1,13),effgw(nyrs1,13),
     :  gwcu(nyrs1,13),gwro(nyrs1,13),
     :  soiltott(nyrs1,13)/nyrs/12,cutot,
     :  cropcusoil(nyrs1,13),cust,tdp(nyrs1,13)
        else
        write(256,634) method,ettot(i,nyrs1,13),effppt(i,nyrs1,13),
     :  reqt(i,nyrs1,13),wbu(i,nyrs1,13)/nyrs/12,reqreqts(nyrs1,13),
     :  divsup(i,nyrs1,13),
     :  ceff(i,nyrs1),closs(nyrs1,13),fdiv(nyrs1,13),arech(nyrs1,13),
     :  sfeff(i,nyrs1),crop_cut(nyrs1,13),soil_cu(nyrs1,13),
     :  ulagt(nyrs1,13),effcu(nyrs1,13),gdiv(nyrs1,13),effgw(nyrs1,13),
     :  gwcu(nyrs1,13),gwro(nyrs1,13),soiltott(nyrs1,13)/nyrs/12,
     :  cutot,cropcusoil(nyrs1,13),cust,tdp(nyrs1,13),gsdiv(nyrs1,13),
     :  gfdiv(nyrs1,13),short,(grass(i,nyrs1,13,ifx), ifx=1,iflood2),
     :     tail(i, nyrs1,13)
        endif
c
c write out monthly values for each year (for structure i)
c
          do k1=1,2
           write(256,651) 
          enddo
Cjhb=&==================================================================
Cjhb  Start M (year) DO loop
Cjhb=&==================================================================
          do m=1,nyrs

c grb 05-20-00 set method descriptor
!         if (missflag(i,m).eq.1) method='Prorated  '
!         if(missflag(i,m).eq.0) method='Calculated'
            method ='Calculated'
            if( (missdiv(i,m).eq.1) .and. (imiss2.eq.1) ) then
              method ='Prorated  '
            endif
          write(256,651)
          write(256,652) nyr1+m-1,comment(m)
          if(t_area(i,m).gt.0.0)then
            write(256,622)
     &        (swgwspac(i,m)+swgwflac(i,m))/t_area(i,m)*100.0,
     &        swgwspac(i,m)+swgwflac(i,m)
          else
            write(256,622)
     &        0.0,
     &        swgwspac(i,m)+swgwflac(i,m)
          endif
c          write(256,623) sper(i,m)*100,sper(i,m)*t_area(i,m)
          if(swgwspac(i,m)+swgwflac(i,m).gt.0.0)then
            write(256,623)
     &        swgwspac(i,m)/(swgwspac(i,m)+swgwflac(i,m))*100.0,
     &        swgwspac(i,m)
          else
            write(256,623)
     &        0.0,
     &        swgwspac(i,m)
          endif
          write(256,625) ceff(i,m), fleff(i,m),speff(i,m)
          if(gmode(i,m) .eq. 1) then
             gwdesg='Ground Water First on Sprinklered Lands'
          else
             gwdesg='Surface Water as Primary Use           '
          endif
          write(256,624) gwdesg
          iparce=nparce(i,m)+1
          id1=15-iparce
        write(256,644) (crpname(j,m),j=1,iparce),(as(k),k=1,id1)
        write(256,645)(atxt(i,j,m),j=1,iparce),(as(k),k=1,id1)
          if( (mod(i,25).eq.0) .and. (m.eq.1)) then
            write(0,*)'  processed through structure #',i
          endif
Cjhb=&==================================================================
Cjhb  Start L (month) DO loop
Cjhb=&==================================================================
              do l=1,12
              if(crop_cut(m,l) .gt. -998) then
                 cutot=crop_cut(m,l)+gwcu(m,l)
                 custot(i,m,l)=estcrpt(i,m,l)+gwcu(m,l)
                 short=reqreqts(m,l)-custot(i,m,l)
              else
                 cutot=-999
                 cust=-999
                 short=-999
              endif
C==================================================
Cjhb        add shortage calculation to bd1 output
            SHORTAGE=short
C==================================================
Cjhb=&==================================================================
Cjhb  Start IFLOOD IF block
Cjhb=&==================================================================
                if(iflood .eq. 0) then
                write(256,611) amn(l),method,ettot(i,m,l),
     :          effppt(i,m,l),reqt(i,m,l),wbu(i,m,l),
     :          reqreqts(m,l),
     :          ddhmonot(m,l),ceff(i,m),closs(m,l),fdiv(m,l),arech(m,l),
     :          sfeff(i,m),crop_cut(m,l),soil_cu(m,l),ulagt(m,l),
     :          effcu(m,l),gdiv(m,l),effgw(m,l),gwcu(m,l),gwro(m,l),
     :          soiltott(m,l),cutot,cropcusoil(m,l),
     :          custot(i,m,l),tdp(m,l)

Cjhb====================================================================
Cjhb  write index values as the first three values and then the month string
Cjhb=&==================================================================
                IF(LBD1OUT.and.(ipresim.ne.1)) THEN
                  WRITE(UNIT=IBD1UN)
Cjhb====================================================================
Cjhb  write index values as the first three values and then the month string
Cjhb  (don't really need this last parameter),
Cjhb=&==================================================================
Cjhb  (1) INTEGER I - basin index
Cjhb  (2) INTEGER NYR1+M-1 - year
Cjhb  (3) INTEGER L - month index
Cjhb  (4) CHAR*3 AMN(L) - abbr month string  ***REPORT***
     :            I,NYR1+M-1,L,AMN(L),
Cjhb====================================================================
Cjhb  write some annual but non-monthly time series report data
Cjhb=&==================================================================
Cjhb  CHAR*84 COMMENT(M) - annual water budget comment (REMOVED from binary file)
Cjhb  (5) REAL t_area(i,m) - annual 'Total Irrigated Acreage '
Cjhb  (6) REAL m_area(i,m,l) - monthly modeled crop acreage (=t_area if not missing data or missing divs are filled, =0 otherwise)
Cjhb  (7) CHAR*10 method - Analysis method - "Calculated" vs. "Prorated"  ***REPORT***
Cjhb :            comment(m),t_area(i,m),method,
     :            t_area(i,m),m_area(i,m,l),method,
Cjhb====================================================================
Cjhb  now write the monthly data (report and other) into the record
Cjhb=&==================================================================
Cjhb  (8) REAL ettot(i,m,l) - Potential Crop ET  ***REPORT***
Cjhb  (9) REAL effppt(i,m,l) - Effective Precip  ***REPORT***
     :            ettot(i,m,l),effppt(i,m,l),
Cjhb  (10) REAL reqt(i,m,l) - Irrigation Water Requirement IWR  ***REPORT***
Cjhb  (11) REAL wbu(i,m,l) - EOM Winter Precip Carryover  ***REPORT***
Cjhb  (12) REAL reqreqts(m,l) - IWR After WInter Precip  ***REPORT***
     :            reqt(i,m,l),wbu(i,m,l),reqreqts(m,l),
Cjhb  (13) REAL ddhmonot(m,l) - River Diversion (SW) Acct. - Historic Diversion  ***REPORT***
     :            ddhmonot(m,l),
Cjhb  (14) REAL ceff(i,m) - River Diversion (SW) Acct. - conveyance Efficiency  ***REPORT***
Cjhb  (15) REAL closs(m,l) - River Diversion (SW) Acct. - conveyance Loss  ***REPORT***
Cjhb  (16) REAL fdiv(m,l) - River Diversion (SW) Acct. - Farm Headgate Diversion  ***REPORT***
Cjhb  (17) REAL tail(i,m,l) - Tail water
     :            ceff(i,m),closs(m,l),fdiv(m,l),tail(i,m,l),
Cjhb  (18) REAL arech(m,l) - River Diversion (SW) Acct. - Farm Deliver to Recharge  ***REPORT***
Cjhb  (19) REAL sfeff(i,m) - River Diversion (SW) Acct. - Max Applic Effic  ***REPORT***
     :            arech(m,l),sfeff(i,m),
Cjhb  (20) REAL crop_cut(m,l) - River Diversion (SW) Acct. - SW to CU           ***REPORT***
Cjhb  (21) REAL soil_cu(m,l) - River Diversion (SW) Acct. - SW to Soil              ***REPORT***
Cjhb  (22) REAL ulagt(m,l) - River Diversion (SW) Acct. - 'SW_Non_Consumed         '  ***REPORT***
     :            crop_cut(m,l),soil_cu(m,l),ulagt(m,l),
Cjhb  (23) REAL effcu(m,l) - River Diversion (SW) Acct. - Calc Surface Water Applic Effic (%)  ***REPORT***
Cjhb  (24) REAL seffcu(m,l) - River Diversion (SW) Acct. - Calc SW System Effic      ***REPORT***
     :            effcu(m,l),seffcu(m,l),
Cjhb  (25) REAL gdiv(m,l) - GW Diversion Acct. - 'GW Diversion            ' ***REPORT***
Cjhb  (26) REAL effgw(m,l) - GW Diversion Acct. - 'Calc GW Application Eff '  ***REPORT***
Cjhb  (27) REAL gwcu(m,l) - GW Diversion Acct. - 'GW CU                   ' ***REPORT***
Cjhb  (28) REAL gwro(m,l) - GW Diversion Acct. - 'GW - Non-Consumed       ' ***REPORT***
     :            gdiv(m,l),effgw(m,l),gwcu(m,l),gwro(m,l),
Cjhb  (29) REAL soiltott(m,l) - Total Soil Moisture EOM Contents  ***REPORT***
     :            soiltott(m,l),
Cjhb  (30) REAL cutot - Estimated Crop CU - From SW/GW Diversion  ***REPORT***
Cjhb  (31) REAL cropcusoil(m,l) - Estimated Crop CU - From Soil Moisture  ***REPORT***
Cjhb  (32) REAL custot(i,m,l) - Estimated Crop CU - Total  ***REPORT***
     :            cutot,cropcusoil(m,l),custot(i,m,l),
Cjhb  (33) REAL tdp(m,l) - Total Month Non-Consumed  ***REPORT***
Cjhb  (34) REAL SHORTAGE - Total Shortage
     :            tdp(m,l),SHORTAGE
Cjhb====================================================================
c                 update the district totals that change by month
Cjhb====================================================================
                  if(ettot(i,m,l).gt.-999.0)then
                    sbettot(sbsb(i),m,l)=
     :                sbettot(sbsb(i),m,l)+ettot(i,m,l)
                    bettot(m,l)=
     :                bettot(m,l)+ettot(i,m,l)
                  endif
                  if(effppt(i,m,l).gt.-999.0)then
                    sbeffppt(sbsb(i),m,l)=
     &                sbeffppt(sbsb(i),m,l)+effppt(i,m,l)
                    beffppt(m,l)=
     &                beffppt(m,l)+effppt(i,m,l)
                  endif
                  if(reqt(i,m,l).gt.-999.0)then
                    sbreqt(sbsb(i),m,l)=
     &                sbreqt(sbsb(i),m,l)+reqt(i,m,l)
                    breqt(m,l)=
     &                breqt(m,l)+reqt(i,m,l)
                  endif
                  if(wbu(i,m,l).gt.-999.0)then
                    sbwbu(sbsb(i),m,l)=
     &                sbwbu(sbsb(i),m,l)+wbu(i,m,l)
                    bwbu(m,l)=
     &                bwbu(m,l)+wbu(i,m,l)
                  endif
                  if(reqreqts(m,l).gt.-999.0)then
                    sbreqreq(sbsb(i),m,l)=
     &                sbreqreq(sbsb(i),m,l)+reqreqts(m,l)
                    breqreq(m,l)=
     &                breqreq(m,l)+reqreqts(m,l)
                  endif
                  if(seniorf(m,l).gt.-999.0)then
                    sbseniorf(sbsb(i),m,l)=
     &                sbseniorf(sbsb(i),m,l)+seniorf(m,l)
                    bseniorf(m,l)=
     &                bseniorf(m,l)+seniorf(m,l)
                  endif
                  if(juniorf(m,l).gt.-999.0)then
                    sbjuniorf(sbsb(i),m,l)=
     &                sbjuniorf(sbsb(i),m,l)+juniorf(m,l)
                    bjuniorf(m,l)=
     &                bjuniorf(m,l)+juniorf(m,l)
                  endif
                  if(otherf(m,l).gt.-999.0)then
                    sbotherf(sbsb(i),m,l)=
     &                sbotherf(sbsb(i),m,l)+otherf(m,l)
                    botherf(m,l)=
     &                botherf(m,l)+otherf(m,l)
                  endif
                  if(divsup(i,m,l).gt.-999.0)then
                    sbdivsup(sbsb(i),m,l)=
     &                sbdivsup(sbsb(i),m,l)+divsup(i,m,l)
                    bdivsup(m,l)=
     &                bdivsup(m,l)+divsup(i,m,l)
                  endif
                  if(closs(m,l).gt.-999.0)then
                    sbcloss(sbsb(i),m,l)=
     &                sbcloss(sbsb(i),m,l)+closs(m,l)
                    bcloss(m,l)=
     &                bcloss(m,l)+closs(m,l)
                  endif
                  if(fdiv(m,l).gt.-999.0)then
                    sbfdiv(sbsb(i),m,l)=
     &                sbfdiv(sbsb(i),m,l)+fdiv(m,l)
                    bfdiv(m,l)=
     &                bfdiv(m,l)+fdiv(m,l)
                  endif
                  sbtail(sbsb(i),m,l)=
     &              sbtail(sbsb(i),m,l)+tail(i,m,l)
                  btail(m,l)=
     &              btail(m,l)+tail(i,m,l)
                  if(arech(m,l).gt.-999.0)then
                    sbarech(sbsb(i),m,l)=
     &                sbarech(sbsb(i),m,l)+arech(m,l)
                    barech(m,l)=
     &                barech(m,l)+arech(m,l)
                  endif
                  sbsfeff(sbsb(i),m,l)=sbsfeff(sbsb(i),m,l)+
     &              sfeff(i,m)*t_area(i,m)
                  bsfeff(m,l)=bsfeff(m,l)+
     &              sfeff(i,m)*t_area(i,m)
                  if(crop_cus(m,l).gt.-999.0)then
                    sbcrop_cus(sbsb(i),m,l)=
     &                sbcrop_cus(sbsb(i),m,l)+crop_cus(m,l)
                    bcrop_cus(m,l)=
     &                bcrop_cus(m,l)+crop_cus(m,l)
                  endif
                  if(crop_cuj(m,l).gt.-999.0)then
                    sbcrop_cuj(sbsb(i),m,l)=
     &                sbcrop_cuj(sbsb(i),m,l)+crop_cuj(m,l)
                    bcrop_cuj(m,l)=
     &                bcrop_cuj(m,l)+crop_cuj(m,l)
                  endif
                  if(crop_cuo(m,l).gt.-999.0)then
                    sbcrop_cuo(sbsb(i),m,l)=
     &                sbcrop_cuo(sbsb(i),m,l)+crop_cuo(m,l)
                    bcrop_cuo(m,l)=
     &                bcrop_cuo(m,l)+crop_cuo(m,l)
                  endif
                  if(crop_cut(m,l).gt.-999.0)then
                    sbcrop_cut(sbsb(i),m,l)=
     &                sbcrop_cut(sbsb(i),m,l)+crop_cut(m,l)
                    bcrop_cut(m,l)=
     &                bcrop_cut(m,l)+crop_cut(m,l)
                  endif
                  if(soil_cus(m,l).gt.-999.0)then
                    sbsoil_cus(sbsb(i),m,l)=
     &                sbsoil_cus(sbsb(i),m,l)+soil_cus(m,l)
                    bsoil_cus(m,l)=
     &                bsoil_cus(m,l)+soil_cus(m,l)
                  endif
                  if(soil_cuj(m,l).gt.-999.0)then
                    sbsoil_cuj(sbsb(i),m,l)=
     &                sbsoil_cuj(sbsb(i),m,l)+soil_cuj(m,l)
                    bsoil_cuj(m,l)=
     &                bsoil_cuj(m,l)+soil_cuj(m,l)
                  endif
                  if(soil_cuo(m,l).gt.-999.0)then
                    sbsoil_cuo(sbsb(i),m,l)=
     &                sbsoil_cuo(sbsb(i),m,l)+soil_cuo(m,l)
                    bsoil_cuo(m,l)=
     &                bsoil_cuo(m,l)+soil_cuo(m,l)
                  endif
                  if(soil_cu(m,l).gt.-999.0)then
                    sbsoil_cu(sbsb(i),m,l)=
     &                sbsoil_cu(sbsb(i),m,l)+soil_cu(m,l)
                    bsoil_cu(m,l)=
     &                bsoil_cu(m,l)+soil_cu(m,l)
                  endif
                  if(soiltotts(m,l).gt.-999.0)then
                    sbsoiltotts(sbsb(i),m,l)=
     &                sbsoiltotts(sbsb(i),m,l)+soiltotts(m,l)
                    bsoiltotts(m,l)=
     &                bsoiltotts(m,l)+soiltotts(m,l)
                  endif
                  if(soiltottj(m,l).gt.-999.0)then
                    sbsoiltottj(sbsb(i),m,l)=
     &                sbsoiltottj(sbsb(i),m,l)+soiltottj(m,l)
                    bsoiltottj(m,l)=
     &                bsoiltottj(m,l)+soiltottj(m,l)
                  endif
                  if(soiltotto(m,l).gt.-999.0)then
                    sbsoiltotto(sbsb(i),m,l)=
     &                sbsoiltotto(sbsb(i),m,l)+soiltotto(m,l)
                    bsoiltotto(m,l)=
     &                bsoiltotto(m,l)+soiltotto(m,l)
                  endif
                  if(soiltott(m,l).gt.-999.0)then
                    sbsoiltott(sbsb(i),m,l)=
     &                sbsoiltott(sbsb(i),m,l)+soiltott(m,l)
                    bsoiltott(m,l)=
     &                bsoiltott(m,l)+soiltott(m,l)
                  endif
                  if(cropcusoil(m,l).gt.-999.0)then
                    sbcropcusoil(sbsb(i),m,l)=
     &                sbcropcusoil(sbsb(i),m,l)+cropcusoil(m,l)
                    bcropcusoil(m,l)=
     &                bcropcusoil(m,l)+cropcusoil(m,l)
                  endif
                  if(estcrps(m,l).gt.-999.0)then
                    sbestcrps(sbsb(i),m,l)=
     &                sbestcrps(sbsb(i),m,l)+estcrps(m,l)
                    bestcrps(m,l)=
     &                bestcrps(m,l)+estcrps(m,l)
                  endif
                  if(estcrpj(m,l).gt.-999.0)then
                    sbestcrpj(sbsb(i),m,l)=
     &                sbestcrpj(sbsb(i),m,l)+estcrpj(m,l)
                    bestcrpj(m,l)=
     &                bestcrpj(m,l)+estcrpj(m,l)
                  endif
                  if(estcrpo(m,l).gt.-999.0)then
                    sbestcrpo(sbsb(i),m,l)=
     &                sbestcrpo(sbsb(i),m,l)+estcrpo(m,l)
                    bestcrpo(m,l)=
     &                bestcrpo(m,l)+estcrpo(m,l)
                  endif
                  if(estcrpt(i,m,l).gt.-999.0)then
                    sbestcrpt(sbsb(i),m,l)=
     &                sbestcrpt(sbsb(i),m,l)+estcrpt(i,m,l)
                    bestcrpt(m,l)=
     &                bestcrpt(m,l)+estcrpt(i,m,l)
                  endif
                  if(gdiv(m,l).gt.-999.0)then
                    sbgdiv(sbsb(i),m,l)=
     &                sbgdiv(sbsb(i),m,l)+gdiv(m,l)
                    bgdiv(m,l)=
     &                bgdiv(m,l)+gdiv(m,l)
                  endif
                  if(gsdiv(m,l).gt.-999.0)then
                    sbgsdiv(sbsb(i),m,l)=
     &                sbgsdiv(sbsb(i),m,l)+gsdiv(m,l)
                    bgsdiv(m,l)=
     &                bgsdiv(m,l)+gsdiv(m,l)
                  endif
                  if(gfdiv(m,l).gt.-999.0)then
                    sbgfdiv(sbsb(i),m,l)=
     &                sbgfdiv(sbsb(i),m,l)+gfdiv(m,l)
                    bgfdiv(m,l)=
     &                bgfdiv(m,l)+gfdiv(m,l)
                  endif
                  if(gwcu(m,l).gt.-999.0)then
                    sbgwcu(sbsb(i),m,l)=
     &                sbgwcu(sbsb(i),m,l)+gwcu(m,l)
                    bgwcu(m,l)=
     &                bgwcu(m,l)+gwcu(m,l)
                  endif
                  if(gwro(m,l).gt.-999.0)then
                    sbgwro(sbsb(i),m,l)=
     &                sbgwro(sbsb(i),m,l)+gwro(m,l)
                    bgwro(m,l)=
     &                bgwro(m,l)+gwro(m,l)
                  endif
                  if(cutot.gt.-999.0)then
                    sbcutot(sbsb(i),m,l)=
     &                sbcutot(sbsb(i),m,l)+cutot
                    bcutot(m,l)=
     &                bcutot(m,l)+cutot
                  endif
                  if(custot(i,m,l).gt.-999.0)then
                    sbcustot(sbsb(i),m,l)=
     &                sbcustot(sbsb(i),m,l)+custot(i,m,l)
                    bcustot(m,l)=
     &                bcustot(m,l)+custot(i,m,l)
                  endif
                  if(tdp(m,l).gt.-999.0)then
                    sbtdp(sbsb(i),m,l)=
     &                sbtdp(sbsb(i),m,l)+tdp(m,l)
                    btdp(m,l)=
     &                btdp(m,l)+tdp(m,l)
                  endif
                  if(ulags(m,l).gt.-999.0)then
                    sbulags(sbsb(i),m,l)=sbulags(sbsb(i),m,l)+
     &                ulags(m,l)-holdps*(1.-ceff(i,m))
                    bulags(m,l)=bulags(m,l)+
     &                ulags(m,l)-holdps*(1.-ceff(i,m))
                  endif
                  if(ulagj(m,l).gt.-999.0)then
                    sbulagj(sbsb(i),m,l)=sbulagj(sbsb(i),m,l)+
     &                ulagj(m,l)-holdpj*(1.-ceff(i,m))
                    bulagj(m,l)=bulagj(m,l)+
     &                ulagj(m,l)-holdpj*(1.-ceff(i,m))
                  endif
                  if(ulago(m,l).gt.-999.0)then
                    sbulago(sbsb(i),m,l)=sbulago(sbsb(i),m,l)+
     &                ulago(m,l)-holdpo*(1.-ceff(i,m))
                    bulago(m,l)=bulago(m,l)+
     &                ulago(m,l)-holdpo*(1.-ceff(i,m))
                  endif
                  if(ulagt(m,l).gt.-999.0)then
                    sbulagt(sbsb(i),m,l)=
     &                sbulagt(sbsb(i),m,l)+ulagt(m,l)
                    bulagt(m,l)=
     &                bulagt(m,l)+ulagt(m,l)
                  endif
                  if(shortage.gt.-999.0)then
                    sbshortage(sbsb(i),m,l)=
     :                sbshortage(sbsb(i),m,l)+shortage
                    bshortage(m,l)=
     :                bshortage(m,l)+shortage
                  endif
Cjhb====================================================================
                ENDIF !(LBD1OUT)
Cjhb====================================================================

Cjhb=&==================================================================
Cjhb  Continue IFLOOD IF block
Cjhb=&==================================================================
                else ! NOT (iflood .eq. 0)
                write(256,631) amn(l),method,ettot(i,m,l),
     :          effppt(i,m,l),reqt(i,m,l),wbu(i,m,l),
     :          reqreqts(m,l),
     :          ddhmonot(m,l),ceff(i,m),closs(m,l),fdiv(m,l),arech(m,l),
     :          sfeff(i,m),crop_cut(m,l),soil_cu(m,l),ulagt(m,l),
     :          effcu(m,l),gdiv(m,l),effgw(m,l),gwcu(m,l),gwro(m,l),
     :          soiltott(m,l),cutot,cropcusoil(m,l),custot(i,m,l),
     :          tdp(m,l),gsdiv(m,l),gfdiv(m,l),short,
     :          (grass(i,m,l,ifx), ifx=1,iflood2),
     :          tail(i,m,l)

Cjhb====================================================================
Cjhb  write a record to the binary file if binary output option selected
Cjhb  report structure:
Cjhb  (note ITIME is necessary because this code is looped through twice
Cjhb   first time is to determine avg values for filling in missing data)
Cjhb=&==================================================================
                IF(LBD1OUT.and.(ipresim.ne.1)) THEN
                  WRITE(UNIT=IBD1UN)
Cjhb====================================================================
Cjhb  write index values as the first three values and then the month string
Cjhb  (don't really need this last parameter),
Cjhb=&==================================================================
Cjhb  (1) INTEGER I - basin index
Cjhb  (2) INTEGER NYR1+M-1 - year
Cjhb  (3) INTEGER L - month index
Cjhb  (4) CHAR*3 AMN(L) - abbr month string  ***REPORT***
     :            I,NYR1+M-1,L,AMN(L),
Cjhb====================================================================
Cjhb  write some annual but non-monthly time series report data
Cjhb=&==================================================================
Cjhb  CHAR*84 COMMENT(M) - annual water budget comment (REMOVED from binary file)
Cjhb  (5) REAL t_area(i,m) - annual 'Total Irrigated Acreage '
Cjhb  (6) REAL m_area(i,m,l) - monthly modeled crop acreage (=t_area if not missing data or missing divs are filled, =0 otherwise)
Cjhb  (7) CHAR*10 method - Analysis method - "Calculated" vs. "Prorated"  ***REPORT***
Cjhb :            comment(m),t_area(i,m),method,
     :            t_area(i,m),m_area(i,m,l),method,
Cjhb====================================================================
Cjhb  now write the monthly data (report and other) into the record
Cjhb=&==================================================================
Cjhb  (8) REAL ettot(i,m,l) - Potential Crop ET  ***REPORT***
Cjhb  (9) REAL effppt(i,m,l) - Effective Precip  ***REPORT***
     :            ettot(i,m,l),effppt(i,m,l),
Cjhb  (10) REAL reqt(i,m,l) - Irrigation Water Requirement IWR  ***REPORT***
Cjhb  (11) REAL wbu(i,m,l) - EOM Winter Precip Carryover  ***REPORT***
Cjhb  (12) REAL reqreqts(m,l) - IWR After WInter Precip  ***REPORT***
     :            reqt(i,m,l),wbu(i,m,l),reqreqts(m,l),
Cjhb  (13) REAL ddhmonot(m,l) - River Diversion (SW) Acct. - River Diversion           ***REPORT***
     :            ddhmonot(m,l),
Cjhb  (14) REAL ceff(i,m) - River Diversion (SW) Acct. - conveyance Efficiency  ***REPORT***
Cjhb  (15) REAL closs(m,l) - River Diversion (SW) Acct. - conveyance Loss  ***REPORT***
Cjhb  (16) REAL fdiv(m,l) - River Diversion (SW) Acct. - 'Farm Headgate Delivery  '  ***REPORT***
     :            ceff(i,m),closs(m,l),fdiv(m,l),
Cjhb  (17) REAL tail(i,m,l) - Supply_Tail Water_Drains
Cjhb  (18) REAL arech(m,l) - River Diversion (SW) Acct. - 'Farm Deliver to Recharge'  ***REPORT***
Cjhb  (19) REAL sfeff(i,m) - River Diversion (SW) Acct. - 'Max Application Effic   '  ***REPORT***
     :            tail(i,m,l),arech(m,l),sfeff(i,m),
Cjhb  (20) REAL crop_cut(m,l) - River Diversion (SW) Acct. - SW to CU           ***REPORT***
Cjhb  (21) REAL soil_cu(m,l) - River Diversion (SW) Acct. - SW to Soil               ***REPORT***
Cjhb  (22) REAL ulagt(m,l) - River Diversion (SW) Acct. - 'SW_Non_Consumed         ' ***REPORT***
     :            crop_cut(m,l),soil_cu(m,l),ulagt(m,l),
Cjhb  (23) REAL effcu(m,l) - River Diversion (SW) Acct. - Calc Surface Water Applic Effic (%)  ***REPORT***
Cjhb  (24) REAL seffcu(m,l) - River Diversion (SW) Acct. - Calc SW System Effic      ***REPORT***
     :            effcu(m,l),seffcu(m,l),
Cjhb  (25) REAL gdiv(m,l) - GW Diversion Acct. - 'GW Diversion            ' ***REPORT***
Cjhb  (26) REAL effgw(m,l) - GW Diversion Acct. - 'Calc GW Application Eff '  ***REPORT***
Cjhb  (27) REAL gwcu(m,l) - GW Diversion Acct. - 'GW CU                   ' ***REPORT***
Cjhb  (28) REAL gwro(m,l) - GW Diversion Acct. - 'GW - Non-Consumed       ' ***REPORT***
     :            gdiv(m,l),effgw(m,l),gwcu(m,l),gwro(m,l),
Cjhb  (29) REAL soiltott(m,l) - Total Soil Moisture EOM Contents  ***REPORT***
     :            soiltott(m,l),
Cjhb  (30) REAL cutot - Estimated Crop CU - From SW/GW Diversion  ***REPORT***
Cjhb  (31) REAL cropcusoil(m,l) - Estimated Crop CU - From Soil Moisture  ***REPORT***
Cjhb  (32) REAL custot(i,m,l) - Estimated Crop CU - Total  ***REPORT***
     :            cutot,cropcusoil(m,l),custot(i,m,l),
Cjhb  (33) REAL tdp(m,l) - Total Month Non-Consumed  ***REPORT***
     :            tdp(m,l),
Cjhb  (34) REAL SHORTAGE - Total Shortage  ***REPORT***
Cjhb  (35) REAL gsdiv(m,l) - GW div to Sprinkler  ***REPORT***
Cjhb  (36) REAL gfdiv(m,l) - GW div to Flood  ***REPORT***
     :            SHORTAGE,gsdiv(m,l),gfdiv(m,l),
Cjhb  2*IFLOOD REAL (grass(i,m,l,ifx),ifx=1,iflood2) - IWR Pasture  ***REPORT***
Cjhb  REAL tail(i,m,l) - ???  ***REPORT***
     :            (grass(i,m,l,ifx),ifx=1,iflood2)
Cjhb====================================================================
c                 update the district totals that change by month
Cjhb====================================================================
                  if(ettot(i,m,l).gt.-999.0)then
                    sbettot(sbsb(i),m,l)=
     :                sbettot(sbsb(i),m,l)+ettot(i,m,l)
                    bettot(m,l)=
     :                bettot(m,l)+ettot(i,m,l)
                  endif
                  if(effppt(i,m,l).gt.-999.0)then
                    sbeffppt(sbsb(i),m,l)=
     &                sbeffppt(sbsb(i),m,l)+effppt(i,m,l)
                    beffppt(m,l)=
     &                beffppt(m,l)+effppt(i,m,l)
                  endif
                  if(reqt(i,m,l).gt.-999.0)then
                    sbreqt(sbsb(i),m,l)=
     &                sbreqt(sbsb(i),m,l)+reqt(i,m,l)
                    breqt(m,l)=
     &                breqt(m,l)+reqt(i,m,l)
                  endif
                  if(wbu(i,m,l).gt.-999.0)then
                    sbwbu(sbsb(i),m,l)=
     &                sbwbu(sbsb(i),m,l)+wbu(i,m,l)
                    bwbu(m,l)=
     &                bwbu(m,l)+wbu(i,m,l)
                  endif
                  if(reqreqts(m,l).gt.-999.0)then
                    sbreqreq(sbsb(i),m,l)=
     &                sbreqreq(sbsb(i),m,l)+reqreqts(m,l)
                    breqreq(m,l)=
     &                breqreq(m,l)+reqreqts(m,l)
                  endif
                  if(seniorf(m,l).gt.-999.0)then
                    sbseniorf(sbsb(i),m,l)=
     &                sbseniorf(sbsb(i),m,l)+seniorf(m,l)
                    bseniorf(m,l)=
     &                bseniorf(m,l)+seniorf(m,l)
                  endif
                  if(juniorf(m,l).gt.-999.0)then
                    sbjuniorf(sbsb(i),m,l)=
     &                sbjuniorf(sbsb(i),m,l)+juniorf(m,l)
                    bjuniorf(m,l)=
     &                bjuniorf(m,l)+juniorf(m,l)
                  endif
                  if(otherf(m,l).gt.-999.0)then
                    sbotherf(sbsb(i),m,l)=
     &                sbotherf(sbsb(i),m,l)+otherf(m,l)
                    botherf(m,l)=
     &                botherf(m,l)+otherf(m,l)
                  endif
                  if(divsup(i,m,l).gt.-999.0)then
                    sbdivsup(sbsb(i),m,l)=
     &                sbdivsup(sbsb(i),m,l)+divsup(i,m,l)
                    bdivsup(m,l)=
     &                bdivsup(m,l)+divsup(i,m,l)
                  endif
                  if(closs(m,l).gt.-999.0)then
                    sbcloss(sbsb(i),m,l)=
     &                sbcloss(sbsb(i),m,l)+closs(m,l)
                    bcloss(m,l)=
     &                bcloss(m,l)+closs(m,l)
                  endif
                  if(fdiv(m,l).gt.-999.0)then
                    sbfdiv(sbsb(i),m,l)=
     &                sbfdiv(sbsb(i),m,l)+fdiv(m,l)
                    bfdiv(m,l)=
     &                bfdiv(m,l)+fdiv(m,l)
                  endif
                  sbtail(sbsb(i),m,l)=
     &              sbtail(sbsb(i),m,l)+tail(i,m,l)
                  btail(m,l)=
     &              btail(m,l)+tail(i,m,l)
                  if(arech(m,l).gt.-999.0)then
                    sbarech(sbsb(i),m,l)=
     &                sbarech(sbsb(i),m,l)+arech(m,l)
                    barech(m,l)=
     &                barech(m,l)+arech(m,l)
                  endif
                  sbsfeff(sbsb(i),m,l)=sbsfeff(sbsb(i),m,l)+
     &              sfeff(i,m)*t_area(i,m)
                  bsfeff(m,l)=bsfeff(m,l)+
     &              sfeff(i,m)*t_area(i,m)
                  if(crop_cus(m,l).gt.-999.0)then
                    sbcrop_cus(sbsb(i),m,l)=
     &                sbcrop_cus(sbsb(i),m,l)+crop_cus(m,l)
                    bcrop_cus(m,l)=
     &                bcrop_cus(m,l)+crop_cus(m,l)
                  endif
                  if(crop_cuj(m,l).gt.-999.0)then
                    sbcrop_cuj(sbsb(i),m,l)=
     &                sbcrop_cuj(sbsb(i),m,l)+crop_cuj(m,l)
                    bcrop_cuj(m,l)=
     &                bcrop_cuj(m,l)+crop_cuj(m,l)
                  endif
                  if(crop_cuo(m,l).gt.-999.0)then
                    sbcrop_cuo(sbsb(i),m,l)=
     &                sbcrop_cuo(sbsb(i),m,l)+crop_cuo(m,l)
                    bcrop_cuo(m,l)=
     &                bcrop_cuo(m,l)+crop_cuo(m,l)
                  endif
                  if(crop_cut(m,l).gt.-999.0)then
                    sbcrop_cut(sbsb(i),m,l)=
     &                sbcrop_cut(sbsb(i),m,l)+crop_cut(m,l)
                    bcrop_cut(m,l)=
     &                bcrop_cut(m,l)+crop_cut(m,l)
                  endif
                  if(soil_cus(m,l).gt.-999.0)then
                    sbsoil_cus(sbsb(i),m,l)=
     &                sbsoil_cus(sbsb(i),m,l)+soil_cus(m,l)
                    bsoil_cus(m,l)=
     &                bsoil_cus(m,l)+soil_cus(m,l)
                  endif
                  if(soil_cuj(m,l).gt.-999.0)then
                    sbsoil_cuj(sbsb(i),m,l)=
     &                sbsoil_cuj(sbsb(i),m,l)+soil_cuj(m,l)
                    bsoil_cuj(m,l)=
     &                bsoil_cuj(m,l)+soil_cuj(m,l)
                  endif
                  if(soil_cuo(m,l).gt.-999.0)then
                    sbsoil_cuo(sbsb(i),m,l)=
     &                sbsoil_cuo(sbsb(i),m,l)+soil_cuo(m,l)
                    bsoil_cuo(m,l)=
     &                bsoil_cuo(m,l)+soil_cuo(m,l)
                  endif
                  if(soil_cu(m,l).gt.-999.0)then
                    sbsoil_cu(sbsb(i),m,l)=
     &                sbsoil_cu(sbsb(i),m,l)+soil_cu(m,l)
                    bsoil_cu(m,l)=
     &                bsoil_cu(m,l)+soil_cu(m,l)
                  endif
                  if(soiltotts(m,l).gt.-999.0)then
                    sbsoiltotts(sbsb(i),m,l)=
     &                sbsoiltotts(sbsb(i),m,l)+soiltotts(m,l)
                    bsoiltotts(m,l)=
     &                bsoiltotts(m,l)+soiltotts(m,l)
                  endif
                  if(soiltottj(m,l).gt.-999.0)then
                    sbsoiltottj(sbsb(i),m,l)=
     &                sbsoiltottj(sbsb(i),m,l)+soiltottj(m,l)
                    bsoiltottj(m,l)=
     &                bsoiltottj(m,l)+soiltottj(m,l)
                  endif
                  if(soiltotto(m,l).gt.-999.0)then
                    sbsoiltotto(sbsb(i),m,l)=
     &                sbsoiltotto(sbsb(i),m,l)+soiltotto(m,l)
                    bsoiltotto(m,l)=
     &                bsoiltotto(m,l)+soiltotto(m,l)
                  endif
                  if(soiltott(m,l).gt.-999.0)then
                    sbsoiltott(sbsb(i),m,l)=
     &                sbsoiltott(sbsb(i),m,l)+soiltott(m,l)
                    bsoiltott(m,l)=
     &                bsoiltott(m,l)+soiltott(m,l)
                  endif
                  if(cropcusoil(m,l).gt.-999.0)then
                    sbcropcusoil(sbsb(i),m,l)=
     &                sbcropcusoil(sbsb(i),m,l)+cropcusoil(m,l)
                    bcropcusoil(m,l)=
     &                bcropcusoil(m,l)+cropcusoil(m,l)
                  endif
                  if(estcrps(m,l).gt.-999.0)then
                    sbestcrps(sbsb(i),m,l)=
     &                sbestcrps(sbsb(i),m,l)+estcrps(m,l)
                    bestcrps(m,l)=
     &                bestcrps(m,l)+estcrps(m,l)
                  endif
                  if(estcrpj(m,l).gt.-999.0)then
                    sbestcrpj(sbsb(i),m,l)=
     &                sbestcrpj(sbsb(i),m,l)+estcrpj(m,l)
                    bestcrpj(m,l)=
     &                bestcrpj(m,l)+estcrpj(m,l)
                  endif
                  if(estcrpo(m,l).gt.-999.0)then
                    sbestcrpo(sbsb(i),m,l)=
     &                sbestcrpo(sbsb(i),m,l)+estcrpo(m,l)
                    bestcrpo(m,l)=
     &                bestcrpo(m,l)+estcrpo(m,l)
                  endif
                  if(estcrpt(i,m,l).gt.-999.0)then
                    sbestcrpt(sbsb(i),m,l)=
     &                sbestcrpt(sbsb(i),m,l)+estcrpt(i,m,l)
                    bestcrpt(m,l)=
     &                bestcrpt(m,l)+estcrpt(i,m,l)
                  endif
                  if(gdiv(m,l).gt.-999.0)then
                    sbgdiv(sbsb(i),m,l)=
     &                sbgdiv(sbsb(i),m,l)+gdiv(m,l)
                    bgdiv(m,l)=
     &                bgdiv(m,l)+gdiv(m,l)
                  endif
                  if(gsdiv(m,l).gt.-999.0)then
                    sbgsdiv(sbsb(i),m,l)=
     &                sbgsdiv(sbsb(i),m,l)+gsdiv(m,l)
                    bgsdiv(m,l)=
     &                bgsdiv(m,l)+gsdiv(m,l)
                  endif
                  if(gfdiv(m,l).gt.-999.0)then
                    sbgfdiv(sbsb(i),m,l)=
     &                sbgfdiv(sbsb(i),m,l)+gfdiv(m,l)
                    bgfdiv(m,l)=
     &                bgfdiv(m,l)+gfdiv(m,l)
                  endif
                  if(gwcu(m,l).gt.-999.0)then
                    sbgwcu(sbsb(i),m,l)=
     &                sbgwcu(sbsb(i),m,l)+gwcu(m,l)
                    bgwcu(m,l)=
     &                bgwcu(m,l)+gwcu(m,l)
                  endif
                  if(gwro(m,l).gt.-999.0)then
                    sbgwro(sbsb(i),m,l)=
     &                sbgwro(sbsb(i),m,l)+gwro(m,l)
                    bgwro(m,l)=
     &                bgwro(m,l)+gwro(m,l)
                  endif
                  if(cutot.gt.-999.0)then
                    sbcutot(sbsb(i),m,l)=
     &                sbcutot(sbsb(i),m,l)+cutot
                    bcutot(m,l)=
     &                bcutot(m,l)+cutot
                  endif
                  if(custot(i,m,l).gt.-999.0)then
                    sbcustot(sbsb(i),m,l)=
     &                sbcustot(sbsb(i),m,l)+custot(i,m,l)
                    bcustot(m,l)=
     &                bcustot(m,l)+custot(i,m,l)
                  endif
                  if(tdp(m,l).gt.-999.0)then
                    sbtdp(sbsb(i),m,l)=
     &                sbtdp(sbsb(i),m,l)+tdp(m,l)
                    btdp(m,l)=
     &                btdp(m,l)+tdp(m,l)
                  endif
                  if(ulags(m,l).gt.-999.0)then
                    sbulags(sbsb(i),m,l)=sbulags(sbsb(i),m,l)+
     &                ulags(m,l)-holdps*(1.-ceff(i,m))
                    bulags(m,l)=bulags(m,l)+
     &                ulags(m,l)-holdps*(1.-ceff(i,m))
                  endif
                  if(ulagj(m,l).gt.-999.0)then
                    sbulagj(sbsb(i),m,l)=sbulagj(sbsb(i),m,l)+
     &                ulagj(m,l)-holdpj*(1.-ceff(i,m))
                    bulagj(m,l)=bulagj(m,l)+
     &                ulagj(m,l)-holdpj*(1.-ceff(i,m))
                  endif
                  if(ulago(m,l).gt.-999.0)then
                    sbulago(sbsb(i),m,l)=sbulago(sbsb(i),m,l)+
     &                ulago(m,l)-holdpo*(1.-ceff(i,m))
                    bulago(m,l)=bulago(m,l)+
     &                ulago(m,l)-holdpo*(1.-ceff(i,m))
                  endif
                  if(ulagt(m,l).gt.-999.0)then
                    sbulagt(sbsb(i),m,l)=
     &                sbulagt(sbsb(i),m,l)+ulagt(m,l)
                    bulagt(m,l)=
     &                bulagt(m,l)+ulagt(m,l)
                  endif
                  if(shortage.gt.-999.0)then
                    sbshortage(sbsb(i),m,l)=
     :                sbshortage(sbsb(i),m,l)+shortage
                    bshortage(m,l)=
     :                bshortage(m,l)+shortage
                  endif
                  do ifx=1,iflood2
                    if(grass(i,m,l,ifx).gt.-999.0)then
                      sbgrass(sbsb(i),m,l,ifx)=
     :                  sbgrass(sbsb(i),m,l,ifx)+grass(i,m,l,ifx)
                      bgrass(m,l,ifx)=
     :                  bgrass(m,l,ifx)+grass(i,m,l,ifx)
                    endif
                  enddo
Cjhb====================================================================
                ENDIF !(LBD1OUT)
Cjhb====================================================================
Cjhb=&==================================================================
Cjhb  End IFLOOD IF block
Cjhb=&==================================================================
                endif !(iflood .eq. 0)
Cjhb=&==================================================================
Cjhb  End L (month) DO loop
Cjhb=&==================================================================
              enddo !l=1,12
Cjhb====================================================================
c             update the district totals that only change annually
Cjhb====================================================================
              IF(LBD1OUT.and.(ipresim.ne.1)) THEN
              ENDIF
Cjhb====================================================================
Cjhb  End M (year) DO loop
Cjhb=&==================================================================
          enddo !m=1,nyrs

           write(256,651)
           write(256,651)
           write(256,651)
                                                      
Cjhb=&==================================================================
Cjhb  End ISUPLY IF block
Cjhb=&==================================================================
      endif !(isuply .eq. 4)

*********************************************************************************************
*  Keep Track of Project totals, averages
*********************************************************************************************	
c      
c Total by year for all structures - if any structures missing, no total!!!
c

577   do m=1,nyrs+1
        if(reqreqts(m,13) .gt. -998 .and. divsup(i,m,13) .gt. -998) then
          numat(m)=numat(m)+t_area(i,m)
          reqreqts(m,14) = reqreqts(m,14)+reqreqts(m,13)
          ettot(nbasin+1,m,14)=ettot(nbasin+1,m,14)+ettot(i,m,13)
          effppt(nbasin+1,m,14)=effppt(nbasin+1,m,14)+effppt(i,m,13)
          reqt(nbasin+1,m,14)=reqt(nbasin+1,m,14)+reqt(i,m,13)
          divsup(nbasin+1,m,14)=divsup(nbasin+1,m,14)+divsup(i,m,13)
          tail(nbasin+1,m,14)=tail(nbasin+1,m,14)+tail(i,m,13)
          seniorf(m,14)=seniorf(m,14)+seniorf(m,13)
          juniorf(m,14)=juniorf(m,14)+juniorf(m,13)
          otherf(m,14)=otherf(m,14)+otherf(m,13)
          closs(m,14)=closs(m,14)+closs(m,13)
          fdiv(m,14)=fdiv(m,14)+fdiv(m,13)
          gwro(m,14)=gwro(m,14)+gwro(m,13)
          gdiv(m,14)=gdiv(m,14)+gdiv(m,13)
          gsdiv(m,14)=gsdiv(m,14)+gsdiv(m,13)
          gfdiv(m,14)=gfdiv(m,14)+gfdiv(m,13)
          arech(m,14)=arech(m,14)+arech(m,13)
          gwcu(m,14)=gwcu(m,14)+gwcu(m,13)
          tdp(m,14)=tdp(m,14)+tdp(m,13)
          crop_cus(m,14) = crop_cus(m,14)+crop_cus(m,13)
          crop_cuj(m,14) = crop_cuj(m,14)+crop_cuj(m,13)
          crop_cuo(m,14) = crop_cuo(m,14)+crop_cuo(m,13)
          crop_cut(m,14) = crop_cut(m,14)+crop_cut(m,13)
          soil_cus(m,14) = soil_cus(m,14)+soil_cus(m,13) 
          soil_cuj(m,14) = soil_cuj(m,14)+soil_cuj(m,13)
          soil_cuo(m,14) = soil_cuo(m,14)+soil_cuo(m,13)
          soil_cu(m,14) = soil_cu(m,14)+soil_cu(m,13)
          ulags(m,14) = ulags(m,14)+ulags(m,13)
          ulagj(m,14) = ulagj(m,14)+ulagj(m,13)
          ulago(m,14) = ulago(m,14)+ulago(m,13)
          ulagt(m,14) = ulagt(m,14)+ulagt(m,13)
          divcu(m,14) = divcu(m,14)+divcu(m,13)
          if(m .lt. nyrs1) then
           soiltotts(m,14) = soiltotts(m,14) + soiltotts(m,12)
           soiltottj(m,14) = soiltottj(m,14) + soiltottj(m,12)
           soiltotto(m,14) = soiltotto(m,14) + soiltotto(m,12)
           soiltott(m,14) = soiltott(m,14) + soiltott(m,12)
           wbu(nbasin+1,m,14)=wbu(nbasin+1,m,14) + wbu(i,m,12)
          else
           soiltotts(m,14) = soiltotts(m,14) + soiltotts(m,13)
           soiltottj(m,14) = soiltottj(m,14) + soiltottj(m,13)
           soiltotto(m,14) = soiltotto(m,14) + soiltotto(m,13)
           soiltott(m,14) = soiltott(m,14) + soiltott(m,13)
           wbu(nbasin+1,m,14)=wbu(nbasin+1,m,14) + wbu(i,m,13)
          endif
          cropcusoil(m,14) = cropcusoil(m,14) + cropcusoil(m,13)
          estcrps(m,14) = estcrps(m,14) + estcrps(m,13)
          estcrpj(m,14) = estcrpj(m,14) + estcrpj(m,13)
          estcrpo(m,14) = estcrpo(m,14) + estcrpo(m,13)
          tcrpt(m,14)=tcrpt(m,14)+estcrpt(i,m,13)
          lagrets(m,14) = lagrets(m,14) + lagrets(m,13)
          lagretj(m,14) = lagretj(m,14) + lagretj(m,13)
          lagreto(m,14) = lagreto(m,14) + lagreto(m,13)
          lagrett(m,14) = lagrett(m,14) + lagrett(m,13)
          laglates(m,14) = laglates(m,14) + laglates(m,13)
          laglatej(m,14) = laglatej(m,14) + laglatej(m,13)
          laglateo(m,14) = laglateo(m,14) + laglateo(m,13)
          laglatet(m,14) = laglatet(m,14) + laglatet(m,13)
          totret(m,14) = totret(m,14) + totret(m,13)
          deps(m,14) = deps(m,14) + deps(m,13)
          depj(m,14) = depj(m,14) + depj(m,13)
          depo(m,14) = depo(m,14) + depo(m,13)
          dept(m,14) = dept(m,14) + dept(m,13)
        endif
      enddo

c
c -- add average monthly values for all structures (already accounts for missing values)
c
         do l=1,13
           if(reqreqts(nyrs1,l) .gt. -998) then
             reqreqts(nyrs2,l) = reqreqts(nyrs2,l)+reqreqts(nyrs1,l)
             ettot(nbasin+1,nyrs2,l) = ettot(nbasin+1,nyrs2,l)
     :           +ettot(i,nyrs1,l)
             effppt(nbasin+1,nyrs2,l) = effppt(nbasin+1,nyrs2,l)
     :           +effppt(i,nyrs1,l)
             reqt(nbasin+1,nyrs2,l) = reqt(nbasin+1,nyrs2,l)
     :           +reqt(i,nyrs1,l)
             divsup(nbasin+1,nyrs2,l) = divsup(nbasin+1,nyrs2,l)
     :           +divsup(i,nyrs1,l)
             tail(nbasin+1,nyrs2,l) = tail(nbasin+1,nyrs2,l)
     :           +tail(i,nyrs1,l)
             seniorf(nyrs2,l)=seniorf(nyrs2,l)+seniorf(nyrs1,l)
             juniorf(nyrs2,l)=juniorf(nyrs2,l)+juniorf(nyrs1,l)
             otherf(nyrs2,l)=otherf(nyrs2,l)+otherf(nyrs1,l)
             closs(nyrs2,l)=closs(nyrs2,l)+closs(nyrs1,l)
             fdiv(nyrs2,l)=fdiv(nyrs2,l)+fdiv(nyrs1,l)
             gwro(nyrs2,l)=gwro(nyrs2,l)+gwro(nyrs1,l)
             gdiv(nyrs2,l)=gdiv(nyrs2,l)+gdiv(nyrs1,l)
             gsdiv(nyrs2,l)=gsdiv(nyrs2,l)+gsdiv(nyrs1,l)
             gfdiv(nyrs2,l)=gfdiv(nyrs2,l)+gfdiv(nyrs1,l)
             arech(nyrs2,l)=arech(nyrs2,l)+arech(nyrs1,l)
             gwcu(nyrs2,l)=gwcu(nyrs2,l)+gwcu(nyrs1,l)
             tdp(nyrs2,l)=tdp(nyrs2,l)+tdp(nyrs1,l)
             ddhmonot(nyrs2,l)=ddhmonot(nyrs2,l)+ddhmonot(nyrs1,l)
             crop_cus(nyrs2,l)=crop_cus(nyrs2,l)+crop_cus(nyrs1,l)
             crop_cuj(nyrs2,l)=crop_cuj(nyrs2,l)+crop_cuj(nyrs1,l)
             crop_cuo(nyrs2,l)=crop_cuo(nyrs2,l)+crop_cuo(nyrs1,l)
             crop_cut(nyrs2,l)=crop_cut(nyrs2,l)+crop_cut(nyrs1,l)
             soil_cus(nyrs2,l)=soil_cus(nyrs2,l)+soil_cus(nyrs1,l)
             soil_cuj(nyrs2,l)=soil_cuj(nyrs2,l)+soil_cuj(nyrs1,l)
             soil_cuo(nyrs2,l)=soil_cuo(nyrs2,l)+soil_cuo(nyrs1,l)
             soil_cu(nyrs2,l)=soil_cu(nyrs2,l)+soil_cu(nyrs1,l)
             ulags(nyrs2,l)=ulags(nyrs2,l)+ulags(nyrs1,l)
             ulagj(nyrs2,l)=ulagj(nyrs2,l)+ulagj(nyrs1,l)
             ulago(nyrs2,l)=ulago(nyrs2,l)+ulago(nyrs1,l)
             ulagt(nyrs2,l)=ulagt(nyrs2,l)+ulagt(nyrs1,l)
             divcu(nyrs2,l)=divcu(nyrs2,l)+divcu(nyrs1,l)
             soiltotts(nyrs2,l)=soiltotts(nyrs2,l)+soiltotts(nyrs1,l)
             soiltottj(nyrs2,l)=soiltottj(nyrs2,l)+soiltottj(nyrs1,l)
             soiltotto(nyrs2,l)=soiltotto(nyrs2,l)+soiltotto(nyrs1,l)
             soiltott(nyrs2,l)=soiltott(nyrs2,l)+soiltott(nyrs1,l)
             cropcusoil(nyrs2,l)=cropcusoil(nyrs2,l)+cropcusoil(nyrs1,l)
             estcrps(nyrs2,l)=estcrps(nyrs2,l)+estcrps(nyrs1,l)
             estcrpj(nyrs2,l)=estcrpj(nyrs2,l)+estcrpj(nyrs1,l)
             estcrpo(nyrs2,l)=estcrpo(nyrs2,l)+estcrpo(nyrs1,l)
             tcrpt(nyrs2,l)=tcrpt(nyrs2,l)+estcrpt(i,nyrs1,l)
             lagrets(nyrs2,l)=lagrets(nyrs2,l)+lagrets(nyrs1,l)
             lagretj(nyrs2,l)=lagretj(nyrs2,l)+lagretj(nyrs1,l)
             lagreto(nyrs2,l)=lagreto(nyrs2,l)+lagreto(nyrs1,l)
             lagrett(nyrs2,l)=lagrett(nyrs2,l)+lagrett(nyrs1,l)
             laglates(nyrs2,l)=laglates(nyrs2,l)+laglates(nyrs1,l)
             laglatej(nyrs2,l)=laglatej(nyrs2,l)+laglatej(nyrs1,l)
             laglateo(nyrs2,l)=laglateo(nyrs2,l)+laglateo(nyrs1,l)
             laglatet(nyrs2,l)=laglatet(nyrs2,l)+laglatet(nyrs1,l)
             totret(nyrs2,l)=totret(nyrs2,l)+totret(nyrs1,l)
             deps(nyrs2,l)=deps(nyrs2,l)+deps(nyrs1,l)
             depj(nyrs2,l)=depj(nyrs2,l)+depj(nyrs1,l)
             depo(nyrs2,l)=depo(nyrs2,l)+depo(nyrs1,l)
             dept(nyrs2,l)=dept(nyrs2,l)+dept(nyrs1,l)
            endif
         enddo
  
      tspcapz=tspcapz+spcapz
      ttotmo=ttotmo+totmo
      tsenmo=tsenmo+senmo
      tjunmo=tjunmo+junmo
      tothmo=tothmo+othmo
      do m=1,nyrs
        tsfeff(m)=tsfeff(m) + sfeff(i,m)
        do j=1,12
        if(missflag(i,m) .eq. 0) then
           reqt(i,m,j)=reqreqts(m,j)
        endif
        if(reqreqts(m,j) .gt. -998 .and. divsup(i,m,j) .gt. -998) then
          treq(m,j) = treq(m,j)+reqreqts(m,j)
          tet(m,j) = tet(m,j)+ettot(i,m,j)
          teffr(m,j) = teffr(m,j)+effppt(i,m,j)
          treqt(m,j) = treqt(m,j)+reqt(i,m,j)
          tdiv(m,j) = tdiv(m,j)+divsup(i,m,j)
          ttail(m,j) = ttail(m,j)+tail(i,m,j)
          tsenf(m,j)=tsenf(m,j)+seniorf(m,j)
          tjunf(m,j)=tjunf(m,j)+juniorf(m,j)
          tothf(m,j)=tothf(m,j)+otherf(m,j)
          tgdiv(m,j)=tgdiv(m,j)+gdiv(m,j)
          tgsdiv(m,j)=tgsdiv(m,j)+gsdiv(m,j)
          tgfdiv(m,j)=tgfdiv(m,j)+gfdiv(m,j)
          tarech(m,j)=tarech(m,j)+arech(m,j)
          tgwcu(m,j)=tgwcu(m,j)+gwcu(m,j)
          tgwro(m,j)=tgwro(m,j)+gwro(m,j)
          tfdiv(m,j)=tfdiv(m,j)+fdiv(m,j)
          ttdp(m,j)=ttdp(m,j)+tdp(m,j)
          tcloss(m,j)=tcloss(m,j)+closs(m,j)
          tcus(m,j) = tcus(m,j)+crop_cus(m,j)
          tcuj(m,j) = tcuj(m,j)+crop_cuj(m,j)
          tcuo(m,j) = tcuo(m,j)+crop_cuo(m,j)
          tcut(m,j) = tcut(m,j)+crop_cut(m,j)
          tscus(m,j) = tscus(m,j)+soil_cus(m,j) 
          tscuj(m,j) = tscuj(m,j)+soil_cuj(m,j)
          tscuo(m,j) = tscuo(m,j)+soil_cuo(m,j)
          tscu(m,j) = tscu(m,j)+soil_cu(m,j)
          tulags(m,j) = tulags(m,j)+ulags(m,j)
          tulagj(m,j) = tulagj(m,j)+ulagj(m,j)
          tulago(m,j) = tulago(m,j)+ulago(m,j)
          tulagt(m,j) = tulagt(m,j)+ulagt(m,j)
          tdivcu(m,j) = tdivcu(m,j)+divcu(m,j)
          ttotts(m,j) = ttotts(m,j) + soiltotts(m,j)
          ttottj(m,j) = ttottj(m,j) + soiltottj(m,j)
          ttotto(m,j) = ttotto(m,j) + soiltotto(m,j)
          ttott(m,j) = ttott(m,j) + soiltott(m,j)
          twbu(m,j) = twbu(m,j) + wbu(i,m,j)
          tcusoil(m,j) = tcusoil(m,j) + cropcusoil(m,j)
          tcrps(m,j) = tcrps(m,j) + estcrps(m,j)
          tcrpj(m,j) = tcrpj(m,j) + estcrpj(m,j)
          tcrpo(m,j) = tcrpo(m,j) + estcrpo(m,j)
          tcrpt(m,j) = tcrpt(m,j) + estcrpt(i,m,j)
          trets(m,j) = trets(m,j) + lagrets(m,j)
          tretj(m,j) = tretj(m,j) + lagretj(m,j)
          treto(m,j) = treto(m,j) + lagreto(m,j)
          trett(m,j) = trett(m,j) + lagrett(m,j)
          tlates(m,j) = tlates(m,j) + laglates(m,j)
          tlatej(m,j) = tlatej(m,j) + laglatej(m,j)
          tlateo(m,j) = tlateo(m,j) + laglateo(m,j)
          tlatet(m,j) = tlatet(m,j) + laglatet(m,j)
          ttotret(m,j) = ttotret(m,j) + totret(m,j)
          tdeps(m,j) = tdeps(m,j) + deps(m,j)
          tdepj(m,j) = tdepj(m,j) + depj(m,j)
          tdepo(m,j) = tdepo(m,j) + depo(m,j)
          tdept(m,j) = tdept(m,j) + dept(m,j)
          if(isuply .eq. 4) then
            estcrpt(i,m,j) = custot(i,m,j)
          endif
        endif
       enddo
      enddo

c grb 05-05-2000 save soil moisture at end of presim period for start of simulation - next five lines
5780  if (ipresim.eq.1) then
        endsoils(i)=soiltots
        endsoilj(i)=soiltotj
        endsoilo(i)=soiltoto
      endif

578   continue    !end nbasin loop
c
c Determine average shortages for surface water only lands by water district
c    set itime .eq. 2 and start nbasin loop again for aggregate structures 
c
      if(itime .eq. 1)  then
         do id=1,99
          do m=1,nyrs
            do j=1,12
              if(idcnt(id,m,j) .gt. 0) then
                swmet(id,m,j) = swmet(id,m,j)/idcnt(id,m,j)
c grb   add check on swmet       
                if (swmet(id,m,j).gt. (1.001)) then
                   write(0,*) "swmet>1",swmet(id,m,j),id,m,j
                   stop
                endif
              else
                swmet(id,m,j) = 1.0
              endif
            enddo
          enddo
         enddo
      endif
Cjhb=&==================================================================
Cjhb  the following code checks to see if this is first or second time
Cjhb  through the water supply accounting code.
Cjhb  if this was first time through, then close and reopen file unit 256,
Cjhb  reset itime flag =2 and run through the entire nbasin loop again
Cjhb=&==================================================================
      if(itime .eq. 1) then
        itime = 2
        close(256)
        OPEN (UNIT=256,FILE="temp2",STATUS='Unknown',IOSTAT=IERR)
        goto 45
      endif
Cjhb=&==================================================================
c       now write the subbasin and basin totals
c       we are outside the structure and imiss2 (itime) caused loops
c       so we can just check the LBD1OUT and ipresim flags
c       ipresim can't be 1
c       i.e. we either need to be here without soil moisture presimulation
c       (in which case ipresim=0)
c       or else we need to be here on the soil moisture presimulation second pass
c       (in which case ipresim=2)
Cjhb=&==================================================================
      if(sboutput) then
      if(LBD1OUT.and.(ipresim.ne.1)) then
        do i=0,sbcount
          do m=1,nyrs
            do l=1,12
Cjhb=&==================================================================
c             here are some values that could not be calculated until now
Cjhb=&==================================================================
c             first is conveyance efficiency, ceff.
c               set it equal to the total farm deliveries minus the
c               total tail water deliveries divided by the total diversions
c               note that this still works even (and especially) when the
c               tailwater values are negative - as often happens in SPDSS
              tmp1=sbfdiv(i,m,l)-sbtail(i,m,l)
              tmp2=sbdivsup(i,m,l)
              sbceff(i,m,l)=0.0
              if(tmp2.gt.0.0)then
                sbceff(i,m,l)=tmp1/tmp2
              endif

c             next is maximum application efficiency, sfeff.
c               it is already calculated per structure as an area (within the structure) prorated value
c               I have extended this to be an area prorated value over
c               groups of structures.
c               I sum the sfeff*area values for each structure in sbsfeff,
c               and now divide by the total sbarea
c               note that this is over ALL years in the analysis; since
c               this is a theoretical maximum, it did not make sense to
c               let missing data affect its value.
              tmp1=sbsfeff(i,m,l)
              tmp2=sbtarea(i,m,l)
              sbsfeff(i,m,l)=0.0
              if(tmp2.gt.0.0)then
                sbsfeff(i,m,l)=tmp1/tmp2
              endif

              tmp1=sbdivcu(i,m,l)
              tmp2=sbseniorf(i,m,l)+sbjuniorf(i,m,l)+sbotherf(i,m,l)
     &             -sbarech(i,m,l)
              sbeffcu(i,m,l)=0.0
              if(tmp2.gt.0.0)then
                sbeffcu(i,m,l)=tmp1/tmp2
              endif

              tmp1=sbdivcu(i,m,l)
              tmp2=sbdivsup(i,m,l)+sbtail(i,m,l)-sbarech(i,m,l)
              sbseffcu(i,m,l)=0.0
              if(tmp2.gt.0.0)then
                sbseffcu(i,m,l)=tmp1/tmp2
              endif

              tmp1=sbgwcu(i,m,l)
              tmp2=sbgdiv(i,m,l)
              sbeffgw(i,m,l)=0.0
              if(tmp2.gt.0.0)then
                sbeffgw(i,m,l)=tmp1/tmp2
              endif
Cjhb=&==================================================================
              select case (isuply)
                case (1)
                  WRITE(UNIT=IBD1UN)
     :              NBASIN+I+1,NYR1+M-1,L,AMN(L),
     :              sbtarea(i,m,l),sbmarea(i,m,l),'N.A.      ',
     :              sbettot(i,m,l),sbeffppt(i,m,l),
     :              sbreqt(i,m,l),sbwbu(i,m,l),sbreqreq(i,m,l),
c     :            ddhmonot(m,l),
     :              sbdivsup(i,m,l),
c     :            ceff(i,m),closs(m,l),
     :              sbceff(i,m,l), sbcloss(i,m,l),
c     :            fdiv(m,l),tail(i,m,l),
     :              sbfdiv(i,m,l), sbtail(i,m,l),
c     :            crop_cut(m,l),soil_cu(m,l),
     :              sbcrop_cut(i,m,l), sbsoil_cu(i,m,l),
c     :            divcu(m,l),ulagt(m,l),
     :              sbdivcu(i,m,l), sbulagt(i,m,l),
c     :            sfeff(i,m),effcu(m,l),seffcu(m,l),
     :              sbsfeff(i,m,l),sbeffcu(i,m,l),sbseffcu(i,m,l),
c     :            soiltott(m,l),
     :              sbsoiltott(i,m,l),
c     :            crop_cut(m,l),cropcusoil(m,l),estcrpt(i,m,l),SHORTAGE
     :              sbcrop_cut(i,m,l), sbcropcusoil(i,m,l),
     &              sbestcrpt(i,m,l), sbshortage(i,m,l)
Cjhb====================================================================
                case (2)
                  WRITE(UNIT=IBD1UN)
!     :            I,NYR1+M-1,L,AMN(L),
!     :            t_area(i,m),method,
     :              NBASIN+I+1,NYR1+M-1,L,AMN(L),
     :              sbtarea(i,m,l),sbmarea(i,m,l),'N.A.      ',
!     :            ettot(i,m,l),effppt(i,m,l),
     :              sbettot(i,m,l),sbeffppt(i,m,l),
!     :            reqt(i,m,l),wbu(i,m,l),reqreqts(m,l),
     :              sbreqt(i,m,l),sbwbu(i,m,l),sbreqreq(i,m,l),
c     :            seniorf(m,l),juniorf(m,l),otherf(m,l),ddhmonot(m,l),
     &              sbseniorf(i,m,l), sbjuniorf(i,m,l),
     &              sbotherf(i,m,l), sbdivsup(i,m,l),
!     :            ceff(i,m),closs(m,l),fdiv(m,l),
     :              sbceff(i,m,l), sbcloss(i,m,l),
     :              sbfdiv(i,m,l),
c     :            crop_cus(m,l),crop_cuj(m,l),
     :              sbcrop_cus(i,m,l), sbcrop_cuj(i,m,l),
c     :            crop_cuo(m,l),crop_cut(m,l),
     :              sbcrop_cuo(i,m,l), sbcrop_cut(i,m,l),
c     :            soil_cus(m,l),soil_cuj(m,l),
     :              sbsoil_cus(i,m,l), sbsoil_cuj(i,m,l),
c     :            soil_cuo(m,l),soil_cu(m,l),
     :              sbsoil_cuo(i,m,l), sbsoil_cu(i,m,l),
c     :            divcu(m,l),
     :              sbdivcu(i,m,l),
c     :           ulags(m,l),ulagj(m,l),ulago(m,l),ulagt(m,l),
     :              sbulags(i,m,l),sbulagj(i,m,l),
     &              sbulago(i,m,l),sbulagt(i,m,l),
c     :            tail(i,m,l),sfeff(i,m),
     :               sbtail(i,m,l), sbsfeff(i,m,l),
c     :            effcu(m,l),seffcu(m,l),
     :              sbeffcu(i,m,l),sbseffcu(i,m,l),
c     :            soiltotts(m,l),soiltottj(m,l),soiltotto(m,l),
c     :            soiltott(m,l),
     :              sbsoiltotts(i,m,l), sbsoiltottj(i,m,l),
     :              sbsoiltotto(i,m,l), sbsoiltott(i,m,l),
c     :            crop_cut(m,l),cropcusoil(m,l),
     :              sbcrop_cut(i,m,l), sbcropcusoil(i,m,l),
c     :            estcrps(m,l),estcrpj(m,l),estcrpo(m,l),estcrpt(i,m,l),
     :              sbestcrps(i,m,l), sbestcrpj(i,m,l),
     &              sbestcrpo(i,m,l), sbestcrpt(i,m,l),
c     :            estcrpj(m,l),SHORTAGE
     :              sbestcrpj(i,m,l), sbshortage(i,m,l)
                case (3)
                  WRITE(UNIT=IBD1UN)
c     :            I,NYR1+M-1,L,AMN(L),
c     :            t_area(i,m),method,
     :              NBASIN+I+1,NYR1+M-1,L,AMN(L),
     :              sbtarea(i,m,l),sbmarea(i,m,l),'N.A.      ',
c     :            ettot(i,m,l),effppt(i,m,l),
     :              sbettot(i,m,l),sbeffppt(i,m,l),
c     :            reqt(i,m,l),wbu(i,m,l),reqreqts(m,l),
     :              sbreqt(i,m,l),sbwbu(i,m,l),sbreqreq(i,m,l),
c     :            seniorf(m,l),juniorf(m,l),otherf(m,l),
c     :            ddhmonot(m,l),
     :              sbseniorf(i,m,l), sbjuniorf(i,m,l),
     &              sbotherf(i,m,l), sbdivsup(i,m,l),
c     :            ceff(i,m),closs(m,l),fdiv(m,l),
     :              sbceff(i,m,l), sbcloss(i,m,l),
     :              sbfdiv(i,m,l),
c     :            crop_cus(m,l),crop_cuj(m,l),
c     :            crop_cuo(m,l),crop_cut(m,l),
     :              sbcrop_cus(i,m,l), sbcrop_cuj(i,m,l),
     :              sbcrop_cuo(i,m,l), sbcrop_cut(i,m,l),
c     :            soil_cus(m,l),soil_cuj(m,l),
c     :            soil_cuo(m,l),soil_cu(m,l),
     :              sbsoil_cus(i,m,l), sbsoil_cuj(i,m,l),
     :              sbsoil_cuo(i,m,l), sbsoil_cu(i,m,l),
c     :            divcu(m,l),
     :              sbdivcu(i,m,l),
c     :            ulags(m,l),ulagj(m,l),ulago(m,l),ulagt(m,l),
     :              sbulags(i,m,l),sbulagj(i,m,l),
     &              sbulago(i,m,l),sbulagt(i,m,l),
c     :            tail(i,m,l),sfeff(i,m),effcu(m,l),seffcu(m,l),
     :              sbtail(i,m,l), sbsfeff(i,m,l),
     &              sbeffcu(i,m,l),sbseffcu(i,m,l),
c     :            soiltotts(m,l),soiltottj(m,l),soiltotto(m,l),
c     :            soiltott(m,l),
     :              sbsoiltotts(i,m,l), sbsoiltottj(i,m,l),
     :              sbsoiltotto(i,m,l), sbsoiltott(i,m,l),
c     :            crop_cut(m,l),cropcusoil(m,l),
     :              sbcrop_cut(i,m,l), sbcropcusoil(i,m,l),
c     :            estcrps(m,l),estcrpj(m,l),estcrpo(m,l),estcrpt(i,m,l),
     :              sbestcrps(i,m,l), sbestcrpj(i,m,l),
     &              sbestcrpo(i,m,l), sbestcrpt(i,m,l),
c     :            lagrets(m,l),lagretj(m,l),lagreto(m,l),lagrett(m,l),
     :              sblagrets(i,m,l),sblagretj(i,m,l),
     &              sblagreto(i,m,l),sblagrett(i,m,l),
c     :            laglates(m,l),laglatej(m,l),
c     :            laglateo(m,l),laglatet(m,l),
c     :            totret(m,l),
     :              sblaglates(i,m,l),sblaglatej(i,m,l),
     &              sblaglateo(i,m,l),sblaglatet(i,m,l),
     &              sbtotret(i,m,l),
c     :            deps(m,l),depj(m,l),
c     :            depo(m,l),dept(m,l),depj(m,l),
     :              sbdeps(i,m,l),sbdepj(i,m,l),
     &              sbdepo(i,m,l),sbdept(i,m,l),sbdepj(i,m,l),
c     :            SHORTAGE
     :              sbshortage(i,m,l)
                case (4)
                  select case (iflood)
                    case (0)
                  WRITE(UNIT=IBD1UN)
c     :            I,NYR1+M-1,L,AMN(L),
c     :            t_area(i,m),method,
     :              NBASIN+I+1,NYR1+M-1,L,AMN(L),
     :              sbtarea(i,m,l),sbmarea(i,m,l),'N.A.      ',
c     :            ettot(i,m,l),effppt(i,m,l),
     :              sbettot(i,m,l),sbeffppt(i,m,l),
c     :            reqt(i,m,l),wbu(i,m,l),reqreqts(m,l),
     :              sbreqt(i,m,l),sbwbu(i,m,l),sbreqreq(i,m,l),
c     :            ddhmonot(m,l),
     :              sbdivsup(i,m,l),
c     :            ceff(i,m),closs(m,l),fdiv(m,l),tail(i,m,l),
     :              sbceff(i,m,l), sbcloss(i,m,l),
     :              sbfdiv(i,m,l),sbtail(i,m,l),
c     :            arech(m,l),sfeff(i,m),
     :              sbarech(i,m,l), sbsfeff(i,m,l),
c     :            crop_cut(m,l),soil_cu(m,l),ulagt(m,l),
     &              sbcrop_cut(i,m,l),sbsoil_cu(i,m,l),sbulagt(i,m,l),
c     :            effcu(m,l),seffcu(m,l),
     :              sbeffcu(i,m,l),sbseffcu(i,m,l),
c     :            gdiv(m,l),effgw(m,l),gwcu(m,l),gwro(m,l),
     :              sbgdiv(i,m,l),sbeffgw(i,m,l),
     &              sbgwcu(i,m,l),sbgwro(i,m,l),
c     :            soiltott(m,l),
     :              sbsoiltott(i,m,l),
c     :            cutot,cropcusoil(m,l),custot(i,m,l),
     :              sbcutot(i,m,l),sbcropcusoil(i,m,l),sbcustot(i,m,l),
c     :            tdp(m,l),SHORTAGE
     :              sbtdp(i,m,l),sbshortage(i,m,l)
                    case default
                  WRITE(UNIT=IBD1UN)
c     :            I,NYR1+M-1,L,AMN(L),
c     :            t_area(i,m),method,
     :              NBASIN+I+1,NYR1+M-1,L,AMN(L),
     :              sbtarea(i,m,l),sbmarea(i,m,l),'N.A.      ',
c     :            ettot(i,m,l),effppt(i,m,l),
     :              sbettot(i,m,l),sbeffppt(i,m,l),
c     :            reqt(i,m,l),wbu(i,m,l),reqreqts(m,l),
     :              sbreqt(i,m,l),sbwbu(i,m,l),sbreqreq(i,m,l),
c     :            ddhmonot(m,l),
     :              sbdivsup(i,m,l),
c     :            ceff(i,m),closs(m,l),fdiv(m,l),
     :              sbceff(i,m,l), sbcloss(i,m,l),
     :              sbfdiv(i,m,l),
c     :            tail(i,m,l),arech(m,l),sfeff(i,m),
     :              sbtail(i,m,l), sbarech(i,m,l), sbsfeff(i,m,l),
c     :            crop_cut(m,l),soil_cu(m,l),ulagt(m,l),
     &              sbcrop_cut(i,m,l),sbsoil_cu(i,m,l),sbulagt(i,m,l),
c     :            effcu(m,l),seffcu(m,l),
     :              sbeffcu(i,m,l),sbseffcu(i,m,l),
c     :            gdiv(m,l),effgw(m,l),gwcu(m,l),gwro(m,l),
     :              sbgdiv(i,m,l),sbeffgw(i,m,l),
     &              sbgwcu(i,m,l),sbgwro(i,m,l),
c     :            soiltott(m,l),
     :              sbsoiltott(i,m,l),
c     :            cutot,cropcusoil(m,l),custot(i,m,l),
     :              sbcutot(i,m,l),sbcropcusoil(i,m,l),sbcustot(i,m,l),
c     :            tdp(m,l),
     :              sbtdp(i,m,l),
c     :            SHORTAGE,gsdiv(m,l),gfdiv(m,l),
     :              sbshortage(i,m,l),sbgsdiv(i,m,l),sbgfdiv(i,m,l),
c     :            (grass(i,m,l,ifx),ifx=1,iflood2)
     :              (sbgrass(i,m,l,ifx),ifx=1,iflood2)
                  end select !iflood
                case default
              end select !isuply
            end do !l, month
          end do !m, year
        end do !i, sub basin
          do m=1,nyrs
            do l=1,12
Cjhb=&==================================================================
c             here are some values that could not be calculated until now
c             first is conveyance efficiency, ceff.
c               set it equal to the total farm deliveries minus the
c               total tail water deliveries divided by the tital diversions
c               note that this still works even (and especially) when the
c               tailwater values are negative - as often happens in SPDSS
              tmp1=bfdiv(m,l)-btail(m,l)
              tmp2=bdivsup(m,l)
              bceff(m,l)=0.0
              if(tmp2.gt.0.0)then
                bceff(m,l)=tmp1/tmp2
              endif

c             next is maximum application efficiency, sfeff.
c               it is already calculated per structure as an area (within the structure) prorated value
c               I have extended this to be an area prorated value over
c               groups of structures.
c               I sum the sfeff*area values for each structure in bsfeff,
c               and now divide by the total sbarea
c               note that this is over ALL years in the analysis; since
c               this is a theoretical maximum, it did not make sense to
c               let missing data affect its value.
              tmp1=bsfeff(m,l)
              tmp2=btarea(m,l)
              bsfeff(m,l)=0.0
              if(tmp2.gt.0.0)then
                bsfeff(m,l)=tmp1/tmp2
              endif

              tmp1=bdivcu(m,l)
              tmp2=bseniorf(m,l)+bjuniorf(m,l)+botherf(m,l)-barech(m,l)
              beffcu(m,l)=0.0
              if(tmp2.gt.0.0)then
                beffcu(m,l)=tmp1/tmp2
              endif

              tmp1=bdivcu(m,l)
              tmp2=bdivsup(m,l)+btail(m,l)-barech(m,l)
              bseffcu(m,l)=0.0
              if(tmp2.gt.0.0)then
                bseffcu(m,l)=tmp1/tmp2
              endif

              tmp1=bgwcu(m,l)
              tmp2=bgdiv(m,l)
              beffgw(m,l)=0.0
              if(tmp2.gt.0.0)then
                beffgw=tmp1/tmp2
              endif
Cjhb=&==================================================================
              select case (isuply)
                case (1)
                  WRITE(UNIT=IBD1UN)
     :              NBASIN+SBCOUNT+1+1,NYR1+M-1,L,AMN(L),
     :              btarea(m,l),bmarea(m,l),'N.A.      ',
     :              bettot(m,l),beffppt(m,l),
     :              breqt(m,l),bwbu(m,l),breqreq(m,l),
c     :            ddhmonot(m,l),
     :              bdivsup(m,l),
c     :            ceff(i,m),closs(m,l),fdiv(m,l),tail(i,m,l),
     :              bceff(m,l),bcloss(m,l),
     :              bfdiv(m,l),btail(m,l),
c     :            crop_cut(m,l),soil_cu(m,l),divcu(m,l),
     :              bcrop_cut(m,l),bsoil_cu(m,l),bdivcu(m,l),
c     :            ulagt(m,l),
     :              bulagt(m,l),
c     :            sfeff(i,m),effcu(m,l),seffcu(m,l),
     :              bsfeff(m,l),beffcu(m,l),bseffcu(m,l),
c     :            soiltott(m,l),
     :              bsoiltott(m,l),
c     :            crop_cut(m,l),cropcusoil(m,l),estcrpt(i,m,l),SHORTAGE
     :              bcrop_cut(m,l),bcropcusoil(m,l),
     &              bestcrpt(m,l), bshortage(m,l)
Cjhb====================================================================
                case (2)
                  WRITE(UNIT=IBD1UN)
!     :            I,NYR1+M-1,L,AMN(L),
!     :            t_area(i,m),method,
     :              NBASIN+SBCOUNT+1+1,NYR1+M-1,L,AMN(L),
     :              btarea(m,l),bmarea(m,l),'N.A.      ',
!     :            ettot(i,m,l),effppt(i,m,l),
     :              bettot(m,l),beffppt(m,l),
!     :            reqt(i,m,l),wbu(i,m,l),reqreqts(m,l),
     :              breqt(m,l),bwbu(m,l),breqreq(m,l),
c     :            seniorf(m,l),juniorf(m,l),otherf(m,l),ddhmonot(m,l),
     &              bseniorf(m,l), bjuniorf(m,l),
     &              botherf(m,l), bdivsup(m,l),
!     :            ceff(i,m),closs(m,l),fdiv(m,l),
     :              bceff(m,l), bcloss(m,l),
     :              bfdiv(m,l),
c     :            crop_cus(m,l),crop_cuj(m,l),
     :              bcrop_cus(m,l),bcrop_cuj(m,l),
c     :            crop_cuo(m,l),crop_cut(m,l),
     :              bcrop_cuo(m,l),bcrop_cut(m,l),
c     :            soil_cus(m,l),soil_cuj(m,l),
     :              bsoil_cus(m,l),bsoil_cuj(m,l),
c     :            soil_cuo(m,l),soil_cu(m,l),
     :              bsoil_cuo(m,l),bsoil_cu(m,l),
c     :            divcu(m,l),
     :              bdivcu(m,l),
c     :           ulags(m,l),ulagj(m,l),ulago(m,l),ulagt(m,l),
     :              bulags(m,l),bulagj(m,l),bulago(m,l),bulagt(m,l),
c     :            tail(i,m,l),sfeff(i,m),
     :              btail(m,l), bsfeff(m,l),
c     :            effcu(m,l),seffcu(m,l),
     :              beffcu(m,l),bseffcu(m,l),
c     :            soiltotts(m,l),soiltottj(m,l),soiltotto(m,l),
c     :            soiltott(m,l),
     &              bsoiltotts(m,l),bsoiltottj(m,l),
     &              bsoiltotto(m,l),bsoiltott(m,l),
c     :            crop_cut(m,l),cropcusoil(m,l),
     :              bcrop_cut(m,l), bcropcusoil(m,l),
c     :            estcrps(m,l),estcrpj(m,l),estcrpo(m,l),estcrpt(i,m,l),
     :              bestcrps(m,l),bestcrpj(m,l),
     &              bestcrpo(m,l),bestcrpt(m,l),
c     :            estcrpj(m,l),SHORTAGE
     :              bestcrpj(m,l), bshortage(m,l)
Cjhb====================================================================
                case (3)
                  WRITE(UNIT=IBD1UN)
c     :            I,NYR1+M-1,L,AMN(L),
c     :            t_area(i,m),method,
     :              NBASIN+SBCOUNT+1+1,NYR1+M-1,L,AMN(L),
     :              btarea(m,l),bmarea(m,l),'N.A.      ',
c     :            ettot(i,m,l),effppt(i,m,l),
     :              bettot(m,l),beffppt(m,l),
c     :            reqt(i,m,l),wbu(i,m,l),reqreqts(m,l),
     :              breqt(m,l),bwbu(m,l),breqreq(m,l),
c     :            seniorf(m,l),juniorf(m,l),otherf(m,l),
c     :            ddhmonot(m,l),
     &              bseniorf(m,l), bjuniorf(m,l),
     &              botherf(m,l), bdivsup(m,l),
c     :            ceff(i,m),closs(m,l),fdiv(m,l),
     :              bceff(m,l), bcloss(m,l),
     :              bfdiv(m,l),
c     :            crop_cus(m,l),crop_cuj(m,l),
c     :            crop_cuo(m,l),crop_cut(m,l),
     :              bcrop_cus(m,l),bcrop_cuj(m,l),
     :              bcrop_cuo(m,l),bcrop_cut(m,l),
c     :            soil_cus(m,l),soil_cuj(m,l),
c     :            soil_cuo(m,l),soil_cu(m,l),
     :              bsoil_cus(m,l),bsoil_cuj(m,l),
     :              bsoil_cuo(m,l),bsoil_cu(m,l),
c     :            divcu(m,l),
     :              bdivcu(m,l),
c     :            ulags(m,l),ulagj(m,l),ulago(m,l),ulagt(m,l),
     :              bulags(m,l),bulagj(m,l),bulago(m,l),bulagt(m,l),
c     :            tail(i,m,l),sfeff(i,m),effcu(m,l),seffcu(m,l),
     :              btail(m,l),bsfeff(m,l),beffcu(m,l),bseffcu(m,l),
c     :            soiltotts(m,l),soiltottj(m,l),soiltotto(m,l),
c     :            soiltott(m,l),
     &              bsoiltotts(m,l),bsoiltottj(m,l),
     &              bsoiltotto(m,l),bsoiltott(m,l),
c     :            crop_cut(m,l),cropcusoil(m,l),
     :              bcrop_cut(m,l), bcropcusoil(m,l),
c     :            estcrps(m,l),estcrpj(m,l),estcrpo(m,l),estcrpt(i,m,l),
     :              bestcrps(m,l),bestcrpj(m,l),
     &              bestcrpo(m,l),bestcrpt(m,l),
c     :            lagrets(m,l),lagretj(m,l),lagreto(m,l),lagrett(m,l),
     :              blagrets(m,l),blagretj(m,l),
     &              blagreto(m,l),blagrett(m,l),
c     :            laglates(m,l),laglatej(m,l),
c     :            laglateo(m,l),laglatet(m,l),
c     :            totret(m,l),
     :              blaglates(m,l),blaglatej(m,l),
     &              blaglateo(m,l),blaglatet(m,l),
     &              btotret(m,l),
c     :            deps(m,l),depj(m,l),
c     :            depo(m,l),dept(m,l),depj(m,l),
     :              bdeps(m,l),bdepj(m,l),
     &              bdepo(m,l),bdept(m,l),bdepj(m,l),
c     :            SHORTAGE
     :              bshortage(m,l)
Cjhb====================================================================
                case (4)
                  select case (iflood)
Cjhb====================================================================
                    case (0)
                  WRITE(UNIT=IBD1UN)
c     :            I,NYR1+M-1,L,AMN(L),
c     :            t_area(i,m),method,
     :              NBASIN+SBCOUNT+1+1,NYR1+M-1,L,AMN(L),
     :              btarea(m,l),bmarea(m,l),'N.A.      ',
c     :            ettot(i,m,l),effppt(i,m,l),
     :              bettot(m,l),beffppt(m,l),
c     :            reqt(i,m,l),wbu(i,m,l),reqreqts(m,l),
     :              breqt(m,l),bwbu(m,l),breqreq(m,l),
c     :            ddhmonot(m,l),
     :              bdivsup(m,l),
c     :            ceff(i,m),closs(m,l),fdiv(m,l),tail(i,m,l),
     :              bceff(m,l), bcloss(m,l),
     :              bfdiv(m,l),btail(m,l),
c     :            arech(m,l),sfeff(i,m),
     :              barech(m,l), bsfeff(m,l),
c     :            crop_cut(m,l),soil_cu(m,l),ulagt(m,l),
     :              bcrop_cut(m,l),bsoil_cu(m,l),bulagt(m,l),
c     :            effcu(m,l),seffcu(m,l),
     :              beffcu(m,l),bseffcu(m,l),
c     :            gdiv(m,l),effgw(m,l),gwcu(m,l),gwro(m,l),
     :              bgdiv(m,l),beffgw(m,l),
     &              bgwcu(m,l),bgwro(m,l),
c     :            soiltott(m,l),
     :              bsoiltott(m,l),
c     :            cutot,cropcusoil(m,l),custot(i,m,l),
     :              bcutot(m,l),bcropcusoil(m,l),bcustot(m,l),
c     :            tdp(m,l),SHORTAGE
     :              btdp(m,l), bshortage(m,l)
Cjhb====================================================================
                    case default
                  WRITE(UNIT=IBD1UN)
c     :            I,NYR1+M-1,L,AMN(L),
c     :            t_area(i,m),method,
     :              NBASIN+SBCOUNT+1+1,NYR1+M-1,L,AMN(L),
     :              btarea(m,l),bmarea(m,l),'N.A.      ',
c     :            ettot(i,m,l),effppt(i,m,l),
     :              bettot(m,l),beffppt(m,l),
c     :            reqt(i,m,l),wbu(i,m,l),reqreqts(m,l),
     :              breqt(m,l),bwbu(m,l),breqreq(m,l),
c     :            ddhmonot(m,l),
     :              bdivsup(m,l),
c     :            ceff(i,m),closs(m,l),fdiv(m,l),
     :              bceff(m,l), bcloss(m,l),
     :              bfdiv(m,l),
c     :            tail(i,m,l),arech(m,l),sfeff(i,m),
     :              btail(m,l), barech(m,l), bsfeff(m,l),
c     :            crop_cut(m,l),soil_cu(m,l),ulagt(m,l),
     :              bcrop_cut(m,l),bsoil_cu(m,l),bulagt(m,l),
c     :            effcu(m,l),seffcu(m,l),
     :              beffcu(m,l),bseffcu(m,l),
c     :            gdiv(m,l),effgw(m,l),gwcu(m,l),gwro(m,l),
     :              bgdiv(m,l),beffgw(m,l),
     &              bgwcu(m,l),bgwro(m,l),
c     :            soiltott(m,l),
     :              bsoiltott(m,l),
c     :            cutot,cropcusoil(m,l),custot(i,m,l),
     :              bcutot(m,l),bcropcusoil(m,l),bcustot(m,l),
c     :            tdp(m,l),
     :              btdp(m,l),
c     :            SHORTAGE,gsdiv(m,l),gfdiv(m,l),
     :              bshortage(m,l),bgsdiv(m,l),bgfdiv(m,l),
c     :            (grass(i,m,l,ifx),ifx=1,iflood2)
     :              (bgrass(m,l,ifx),ifx=1,iflood2)
                  end select !iflood
                case default
              end select !isuply
            end do !l, month
          end do !m, year
      endif !LBD1OUT.and.(ipresim.ne.1)
      endif !SBOUTPUT
Cjhb=&==================================================================
c     close the binary output file.
c       note: the binary output is written INSIDE the nbasin loop, just like the DWB file
c             however - it is NOT reordered like the DWB file is.
c             leave it in the same order - will take care of ordering in the GUI
Cjhb=&==================================================================
      close(UNIT=IBD1UN)
c      close(33)
Cjhb=&==================================================================
Cjhb  note: at this point itime MUST = 2 since its only other value is 1 and
Cjhb  that was handled in the previous code.  the following if statement then
Cjhb  has a redundant check on itime, but that's ok ... does not hurt anything
Cjhb=&==================================================================
Cjhb  the following code checks to see if this is first or second call of
Cjhb   wsupsum from statecu.for for soil moisture initialization
Cjhb   ipresim=0 - first and ONLY call of WSUPSUM from statecu.for
Cjhb   ipresim=1 - first of two calls of WSUPSUM from statecu.for
Cjhb   ipresim=2 - second of two calls of WSUPSUM from statecu.for
Cjhb  if ipresim=1 then exit wsupsum now, else go on to output the water budget...
Cjhb=&==================================================================
cc grb 04-20-00 if in presimulation mode, return, added next four statements
Cjhb=&==================================================================
      if (itime.eq.2. and.ipresim.eq.1) then
        close(256)
        return
      endif
Cjhb=&==================================================================
c     grb next lines down to 461 added to reorganize dwb file     4-20-00
c     grb 04-20-00 reorder dwb file with next approx 40 lines
Cjhb=&==================================================================
      close(256)
Cjhb=&==================================================================
Cjhb  reorganize the data for the final dwb file.
Cjhb  read from the temp1 and temp2 files and output to the dwb file (thefile1)
Cjhb=&==================================================================
c      if(iagg .eq. 1 .or. imiss2 .eq. 1) then
Cjhb=&==================================================================
      OPEN (UNIT=256,FILE=thefile1,STATUS='Unknown',IOSTAT=IERR)
      open (unit=257, file="temp1")
      read(257,'(a520)',end=4664 ) tempwd
      backspace(257)
4664  open (unit=258,file="temp2")      
      write(0,513)
      write(999,513)
 513  format(' Writing water budget output: ')
      do 461 i=1,nbasin
        if(mod(i,25).eq.0)then
          write(0,*)'  processed through structure #',i
        endif
        twdid=bas_id(i)
        read(257,'(a520)',end=464 ) tempwd
        if (tempwd(12:23).eq.twdid(1:12)) then
          m1=257
          goto 462
        endif
        backspace(257)
Cjhb=&==================================================================
464     read(258,'(a523)',end=463) tempwd
        if (tempwd(12:23).eq.twdid(1:12)) then
          m1=258
          goto 462 
        endif
Cjhb=&==================================================================
463     write(0,*)
     &  "Error - structure not found in temp dwb files",twdid
        write(999,*)
     &  "Error - structure not found in temp dwb files",twdid
        stop
Cjhb=&==================================================================
c       transfer line with ID
462     IF (ISUPLY.EQ.1) write(256,'(a179)') tempwd(1:179) 
        IF (ISUPLY.EQ.2) write(256,'(a359)') tempwd(1:359) 
        IF (ISUPLY.EQ.3) write(256,'(a522)') tempwd(1:522) 
        IF (ISUPLY.EQ.4) write(256,'(a372)') tempwd(1:372)
c       ----------------------------------------------------------------
c       jhb | did this to get rid of all those unnecessary trailing spaces...
c       jhb | but some fortran compilers treat * formats differently (e.g. wrap) ... so take it out
c       ----------------------------------------------------------------
!462     IF (ISUPLY.EQ.1) write(256,*) trim(tempwd(1:179))
!        IF (ISUPLY.EQ.2) write(256,*) trim(tempwd(1:359))
!        IF (ISUPLY.EQ.3) write(256,*) trim(tempwd(1:522))
!        IF (ISUPLY.EQ.4) write(256,*) trim(tempwd(1:372))
Cjhb=&==================================================================
c       transfer lines following ID down to next id line
465     read(m1,'(a523)',end=461) tempwd
        if (tempwd(5:7).eq."ID:") then 
          backspace(m1)
          goto 461
        endif
Cjhb=&==================================================================
        IF (ISUPLY.EQ.1) write(256,'(a179)') tempwd(1:179)
        IF (ISUPLY.EQ.2) write(256,'(a359)') tempwd(1:359) 
        IF (ISUPLY.EQ.3) write(256,'(a522)') tempwd(1:522) 
        IF (ISUPLY.EQ.4) write(256,'(a372)') tempwd(1:372) 
c       ----------------------------------------------------------------
c       jhb | did this to get rid of all those unnecessary trailing spaces...
c       jhb | but some fortran compilers treat * formats differently (e.g. wrap at 80) ... so take it out
c       ----------------------------------------------------------------
!        IF (ISUPLY.EQ.1) write(256,*) trim(tempwd(1:179))
!        IF (ISUPLY.EQ.2) write(256,*) trim(tempwd(1:359))
!        IF (ISUPLY.EQ.3) write(256,*) trim(tempwd(1:522))
!        IF (ISUPLY.EQ.4) write(256,*) trim(tempwd(1:372))
        goto 465
461   continue
Cjhb=&==================================================================
c466     CLOSE(258,STATUS='DELETE')
c        CLOSE(257,STATUS='DELETE')
c      endif
Cjhb=&==================================================================


Cjhb=&==================================================================
Cjhb  the remainder of this file is to create the SWB file -
Cjhb  the scenario water budget report - for the whole basin - no individual structures
Cjhb=&==================================================================
ccccccccccccccccccccccccccccccccccccccccccccccccc
c  write out scenario water budget
ccccccccccccccccccccccccccccccccccccccccccccccccc
c  grb 5-8-00  general modification made is to add calculated or prorated method
c   to dwb and swb output in isuply =2 or isuply=4 options  changed write statements
c   and corresponding format statements
ccccccccccccccccccccccccccccccccccccccccccccccccc
      do m=1,nyrs
        if(acret(m) .gt. 0.0) then
          percenta(m)=numat(m)/acret(m)*100.0
        else
          percenta(m)=0.0
        endif
      enddo
      do l=1,13
        if (divcu(nyrs2,l).gt.0.and.(seniorf(nyrs2,l)+
     :      juniorf(nyrs2,l)+otherf(nyrs2,l)-arech(nyrs1,l)).gt.0.) then
          effcu(nyrs2,l) =(divcu(nyrs2,l)/(seniorf(nyrs2,l)+
     :      juniorf(nyrs2,l)+otherf(nyrs2,l)-
     :      arech(nyrs2,l)))*100
          seffcu(nyrs2,l) =(divcu(nyrs2,l)/(seniorf(nyrs2,l)+
     :      juniorf(nyrs2,l)+otherf(nyrs2,l)+closs(nyrs2,l)-
     :      arech(nyrs2,l)))*100
        else
          effcu(nyrs2,l)=0.0
          seffcu(nyrs2,l)=0.0
        endif
        if (gdiv(nyrs2,l).gt.0.0) then
          effgw(nyrs2,l) =(gwcu(nyrs2,l)/gdiv(nyrs2,l))
        else
          effgw(nyrs2,l)=0
        endif
      enddo
c
c --write out header information depending on option
c
        if(isuply .eq. 1)  then
          write(800,1708) tspcapz
          write(800,1781) ttotmo
          write(800,1751)
          write(800,1721) nyr1, nyr2
          write(800,1700)
          write(800,1701)
          write(800,1702)
          write(800,1703)
          write(800,1704)
          write(800,1705)
          write(800,1706)
          write(800,1700)
        endif

        if (isuply .eq. 2) then
          write(800,1808) tspcapz
          write(800,1883) tsenmo,tjunmo,tothmo
          write(800,1851)
          write(800,1821) nyr1, nyr2
          write(800,1800)
          write(800,1801)
          write(800,1802)
          write(800,1803)
          write(800,1804)
          write(800,1805)
          write(800,1806)
          write(800,1800)
        endif

        if (isuply .eq. 3) then
          write(800,1908) tspcapz
          write(800,1983) tsenmo,tjunmo,tothmo
          write(800,1951)
          write(800,1921) nyr1, nyr2
          write(800,1900)
          write(800,1901)
          write(800,1902)
          write(800,1903)
          write(800,1904)
          write(800,1905)
          write(800,1906)
          write(800,1900)
        endif

        if(isuply .eq. 4)  then
          write(800,1608) tspcapz
          write(800,1681) ttotmo
          write(800,1651)
          write(800,1621) nyr1, nyr2
          if(iflood .eq. 0) then
            write(800,1600)
            write(800,1601)
            write(800,1602)
            write(800,1603)
            write(800,1604)
            write(800,1605)
            write(800,1606)
            write(800,1600)
          else
            write(800,6600)
            write(800,6601)
            write(800,6602)
            write(800,6603)
            write(800,6604)
            write(800,6605)
            write(800,6606)
            write(800,6600)
          endif
        endif

        do m=1,nyrs
          if (divcu(m,14).ge.0.and.(seniorf(m,14)+
     :         juniorf(m,14)+otherf(m,14)-arech(m,14)).gt.0) then
                effcu(m,14) =(divcu(m,14)/(seniorf(m,14)+
     :          juniorf(m,14)+otherf(m,14)-arech(m,14)))*100
                seffcu(m,14) =(divcu(m,14)/(seniorf(m,14)+
     :          juniorf(m,14)+otherf(m,14)+closs(m,13)-arech(m,14)))*100
          else
            effcu(m,14) = 0
            seffcu(m,14) = 0
          endif
          if (gdiv(m,14).gt.0) then
                effgw(m,14) =(gwcu(m,14)/gdiv(m,14))
          else
            effgw(m,14) = 0
          endif
        do l=1,12
          if (tdivcu(m,l).ge.0.and.(tsenf(m,l)+
     :      tjunf(m,l)+tothf(m,l)-tarech(m,l)).gt.0) then
             teffcu(m,l) =(tdivcu(m,l)/(tsenf(m,l)+
     :       tjunf(m,l)+tothf(m,l)-tarech(m,l)))*100
             tseffcu(m,l) =(tdivcu(m,l)/(tsenf(m,l)+
     :       tjunf(m,l)+tothf(m,l)+tcloss(m,l)-tarech(m,l)))*100
          else
            teffcu(m,l) = 0
            tseffcu(m,l) = 0
          endif
          if (tgdiv(m,l).gt.0) then
             teffgw(m,l) =(tgwcu(m,l)/tgdiv(m,l))
          else
            teffgw(m,l) = 0
          endif
        enddo
        tsfeff(m)=tsfeff(m)/nbasin*100
        tsfeff(nyrs1)=tsfeff(nyrs1)+tsfeff(m)
      enddo

        tsfeff(nyrs1)=tsfeff(nyrs1)/nyrs
        if (tdivcu(nyrs1,13).ge.0.and.(tsenf(nyrs1,13)+
     :     tjunf(nyrs1,13)+tothf(nyrs1,13)-tarech(nyrs1,13)).gt.0) then
           teffcu(nyrs1,13) =(tdivcu(nyrs1,13)/(tsenf(nyrs1,13)+
     :     tjunf(nyrs1,13)+tothf(nyrs1,13)-tarech(nyrs1,13)))*100
           tseffcu(nyrs1,13) =(tdivcu(nyrs1,13)/(tsenf(nyrs1,13)+
     :     tjunf(nyrs1,13)+tothf(nyrs1,13)+tcloss(nyrs1,13)-
     :     tarech(nyrs1,13)))*100
        else
          teffcu(nyrs1,13) = 0
          tseffcu(nyrs1,13) = 0
        endif
        if (tgdiv(nyrs1,13).gt.0) then
          teffgw(nyrs1,13) =(tgwcu(nyrs1,13)/tgdiv(nyrs1,13))
        else
          teffgw(nyrs1,13) = 0
        endif
c
c write annual totals for all structures
c 
      if(isuply .eq. 1) then
        do m = 1,nyrs
        if((ulagt(m,14).lt.-998.0).or.(closs(m,14).lt.-998.0)) then
          nonconsumed=-999.0
        else
          nonconsumed=ulagt(m,14)+closs(m,14)
        endif
       write(800,1712) percenta(m),method2,nyr1+m-1,ettot(nbasin+1,m,14)
     :  ,effppt(nbasin+1,m,14),reqt(nbasin+1,m,14),wbu(nbasin+1,m,14),
     :   reqreqts(m,14),divsup(nbasin+1,m,14),
c     :    crop_cut(m,14),soil_cu(m,14),ulagt(m,14),divcu(m,14),
     :    crop_cut(m,14),soil_cu(m,14),nonconsumed,divcu(m,14),
     :    seffcu(m,14),soiltott(m,14),crop_cut(m,14),
     :    cropcusoil(m,14),
     :    tcrpt(m,14)
        enddo
        write(800,1751) 
c
c write average for all years for all structures
c
        if((ulagt(nyrs2,13).lt.-998.0).or.
     &     (closs(nyrs2,13).lt.-998.0)) then
          nonconsumed=-999.0
        else
          nonconsumed=ulagt(nyrs2,13)+closs(nyrs2,13)
        endif
         write(800,1713) method2,ettot(nbasin+1,nyrs2,13),
     :    effppt(nbasin+1,nyrs2,13),reqt(nbasin+1,nyrs2,13),
     :    wbu(nbasin+1,nyrs2,13),reqreqts(nyrs2,13),
     :    divsup(nbasin+1,nyrs2,13),
c     :    crop_cut(nyrs2,13),soil_cu(nyrs2,13),ulagt(nyrs2,13),
     :    crop_cut(nyrs2,13),soil_cu(nyrs2,13),nonconsumed,
     :    divcu(nyrs2,13),seffcu(nyrs2,13),soiltott(nyrs,14),
     :    crop_cut(nyrs2,13),cropcusoil(nyrs2,13),tcrpt(nyrs2,13)

        do k1=1,3
           write(800,1751) 
        enddo
c
c  write monthly average for all years for all structures
c
        write(800,1718) nyr1, nyr2
        do l=1,12
         if((ulagt(nyrs2,l).lt.-998.0).or.
     &      (closs(nyrs2,l).lt.-998.0)) then
           nonconsumed=-999.0
         else
           nonconsumed=ulagt(nyrs2,l)+closs(nyrs2,l)
         endif
         write(800,1710) method2,amn(l),ettot(nbasin+1,nyrs2,l),
     :    effppt(nbasin+1,nyrs2,l),reqt(nbasin+1,nyrs2,l),
     :    wbu(nbasin+1,nyrs2,l),reqreqts(nyrs2,l),
     :    divsup(nbasin+1,nyrs2,l),
c     :    crop_cut(nyrs2,l),soil_cu(nyrs2,l),ulagt(nyrs2,l),
     :    crop_cut(nyrs2,l),soil_cu(nyrs2,l),nonconsumed,
     :    divcu(nyrs2,l),seffcu(nyrs2,l),soiltott(nyrs2,l),
     :    crop_cut(nyrs2,l),cropcusoil(nyrs2,l),tcrpt(nyrs2,l)
        enddo
        write(800,1751) 

c
c write average for all years for all structures
c
        if((ulagt(nyrs2,13).lt.-998.0).or.
     &     (closs(nyrs2,13).lt.-998.0)) then
          nonconsumed=-999.0
        else
          nonconsumed=ulagt(nyrs2,13)+closs(nyrs2,13)
        endif
         write(800,1714) method2,ettot(nbasin+1,nyrs2,13),
     :    effppt(nbasin+1,nyrs2,13),reqt(nbasin+1,nyrs2,13),
     :    wbu(nbasin+1,nyrs2,13),reqreqts(nyrs2,13),
     :    divsup(nbasin+1,nyrs2,13),
c     :    crop_cut(nyrs2,13),soil_cu(nyrs2,13),ulagt(nyrs2,13),
     :    crop_cut(nyrs2,13),soil_cu(nyrs2,13),nonconsumed,
     :    divcu(nyrs2,13),seffcu(nyrs2,13),soiltott(nyrs,14),
     :    crop_cut(nyrs2,13),cropcusoil(nyrs2,13),tcrpt(nyrs2,13)
        write(800,1751) 
        write(800,1751) 
c
c write out monthly values for each year for all structures combined
c
          if(nu_dim+1 .gt. 10) nu_dim=9
          id1=15-nu_dim-1
          do m=1,nyrs
          write(800,1751)
          write(800,1752) percenta(m),nyr1+m-1
          write(800,1744) (nu_nme(i),i=1,nu_dim+1),(as(k),k=1,id1)
          write(800,1745) (nu_sum(m,i),i=1,nu_dim),sum_y(m),
     :                    (as(k),k=1,id1)
            do l=1,12
             if((tulagt(m,l).lt.-998.0).or.
     &          (tcloss(m,l).lt.-998.0)) then
               nonconsumed=-999.0
             else
               nonconsumed=tulagt(m,l)+tcloss(m,l)
             endif
            write(800,1711) method2,amn(l),tet(m,l),teffr(m,l),
     :        treqt(m,l),twbu(m,l),treq(m,l),
c     :        tdiv(m,l),tcut(m,l),tscu(m,l),tulagt(m,l),
     :        tdiv(m,l),tcut(m,l),tscu(m,l),nonconsumed,
     :        tdivcu(m,l),tseffcu(m,l),ttott(m,l),
     :        tcut(m,l),tcusoil(m,l),tcrpt(m,l)
            enddo
          enddo
      endif

      if(isuply .eq. 2) then
c
c write annual totals for all structures combined
c
        do m=1,nyrs
        if((ulagt(m,14).lt.-998.0).or.(closs(m,14).lt.-998.0)) then
          nonconsumed=-999.0
        else
          nonconsumed=ulagt(m,14)+closs(m,14)
        endif
        write(800,1835) percenta(m),method2,nyr1+m-1,
     :	  ettot(nbasin+1,m,14),
     :  effppt(nbasin+1,m,14),reqt(nbasin+1,m,14),wbu(nbasin+1,m,14),
     :  reqreqts(m,14),
     :    seniorf(m,14),juniorf(m,14),otherf(m,14),divsup(nbasin+1,m,14)
     :,crop_cus(m,14),crop_cuj(m,14),crop_cuo(m,14),crop_cut(m,14),
     :    soil_cus(m,14),soil_cuj(m,14),soil_cuo(m,14),soil_cu(m,14),
c     :    ulags(m,14),ulagj(m,14),ulago(m,14),ulagt(m,14),divcu(m,14),
     :    ulags(m,14),ulagj(m,14),ulago(m,14),nonconsumed,divcu(m,14),
     :    seffcu(m,14),soiltotts(m,14),soiltottj(m,14),
     :    soiltotto(m,14),soiltott(m,14),crop_cut(m,14),
     :    cropcusoil(m,14),estcrps(m,14),estcrpj(m,14),estcrpo(m,14),
     :    tcrpt(m,14),estcrpj(m,14)
        enddo
        write(800,1851) 
c
c write average for all years for all structures
c
        if((ulagt(nyrs2,13).lt.-998.0).or.
     &     (closs(nyrs2,13).lt.-998.0)) then
          nonconsumed=-999.0
        else
          nonconsumed=ulagt(nyrs2,13)+closs(nyrs2,13)
        endif
         write(800,1813) method2,ettot(nbasin+1,nyrs2,13),
     :    effppt(nbasin+1,nyrs2,13),reqt(nbasin+1,nyrs2,13),
     :    wbu(nbasin+1,nyrs2,13),reqreqts(nyrs2,13),
     :    seniorf(nyrs2,13),juniorf(nyrs2,13),otherf(nyrs2,13),
     :    divsup(nbasin+1,nyrs2,13),crop_cus(nyrs2,13),
     :    crop_cuj(nyrs2,13),
     :    crop_cuo(nyrs2,13),crop_cut(nyrs2,13),soil_cus(nyrs2,13),
     :    soil_cuj(nyrs2,13),soil_cuo(nyrs2,13),soil_cu(nyrs2,13),
     :    ulags(nyrs2,13),ulagj(nyrs2,13),ulago(nyrs2,13),
c     :    ulagt(nyrs2,13),divcu(nyrs2,13),seffcu(nyrs2,13),
     :    nonconsumed,divcu(nyrs2,13),seffcu(nyrs2,13),
     :    soiltotts(nyrs,14),soiltottj(nyrs,14),soiltotto(nyrs,14),
     :    soiltott(nyrs,14),crop_cut(nyrs2,13),cropcusoil(nyrs2,13),
     :    estcrps(nyrs2,13),
     :    estcrpj(nyrs2,13),estcrpo(nyrs2,13),tcrpt(nyrs2,13),
     :    estcrpj(nyrs2,13)

         do k1=1,3
            write(800,1851) 
         enddo
c
c  write monthly average for all years for all structures
c
         write(800,1818) nyr1, nyr2
         do l=1,12
         if((ulagt(nyrs2,l).lt.-998.0).or.
     &      (closs(nyrs2,l).lt.-998.0)) then
           nonconsumed=-999.0
         else
           nonconsumed=ulagt(nyrs2,l)+closs(nyrs2,l)
         endif
         write(800,1833) method2,amn(l),ettot(nbasin+1,nyrs2,l),
     :    effppt(nbasin+1,nyrs2,l),reqt(nbasin+1,nyrs2,l),
     :    wbu(nbasin+1,nyrs2,l),reqreqts(nyrs2,l),
     :    seniorf(nyrs2,l),juniorf(nyrs2,l),otherf(nyrs2,l),
     :    divsup(nbasin+1,nyrs2,l),crop_cus(nyrs2,l),crop_cuj(nyrs2,l),
     :    crop_cuo(nyrs2,l),crop_cut(nyrs2,l),soil_cus(nyrs2,l),
     :    soil_cuj(nyrs2,l),soil_cuo(nyrs2,l),soil_cu(nyrs2,l),
     :    ulags(nyrs2,l),ulagj(nyrs2,l),ulago(nyrs2,l),
c     :    ulagt(nyrs2,l),divcu(nyrs2,l),seffcu(nyrs2,l),
     :    nonconsumed,divcu(nyrs2,l),seffcu(nyrs2,l),
     :    soiltotts(nyrs2,l),soiltottj(nyrs2,l),soiltotto(nyrs2,l),
     :    soiltott(nyrs2,l),crop_cut(nyrs2,l),cropcusoil(nyrs2,l),
     :    estcrps(nyrs2,l),
     :    estcrpj(nyrs2,l),estcrpo(nyrs2,l),tcrpt(nyrs2,l),
     :    estcrpj(nyrs2,l)
        enddo
c
c write average for all years for all structures
c
        if((ulagt(nyrs2,13).lt.-998.0).or.
     &     (closs(nyrs2,13).lt.-998.0)) then
          nonconsumed=-999.0
        else
          nonconsumed=ulagt(nyrs2,13)+closs(nyrs2,13)
        endif
         write(800,1814) method2,ettot(nbasin+1,nyrs2,13),
     :    effppt(nbasin+1,nyrs2,13),reqt(nbasin+1,nyrs2,13),
     :    wbu(nbasin+1,nyrs2,13),reqreqts(nyrs2,13),
     :    seniorf(nyrs2,13),juniorf(nyrs2,13),otherf(nyrs2,13),
     :    divsup(nbasin+1,nyrs2,13),crop_cus(nyrs2,13),
     :    crop_cuj(nyrs2,13),
     :    crop_cuo(nyrs2,13),crop_cut(nyrs2,13),soil_cus(nyrs2,13),
     :    soil_cuj(nyrs2,13),soil_cuo(nyrs2,13),soil_cu(nyrs2,13),
     :    ulags(nyrs2,13),ulagj(nyrs2,13),ulago(nyrs2,13),
c     :    ulagt(nyrs2,13),divcu(nyrs2,13),seffcu(nyrs2,13),
     :    nonconsumed,divcu(nyrs2,13),seffcu(nyrs2,13),
     :    soiltotts(nyrs,14),soiltottj(nyrs,14),soiltotto(nyrs,14),
     :    soiltott(nyrs,14),crop_cut(nyrs2,13),cropcusoil(nyrs2,13),
     :    estcrps(nyrs2,13),
     :    estcrpj(nyrs2,13),estcrpo(nyrs2,13),tcrpt(nyrs2,13),
     :    estcrpj(nyrs2,13)
          write(800,1851) 
          write(800,1851) 
c
c write out monthly values for each year for all structures combined
c
          if(nu_dim+1 .gt. 10) nu_dim=9
          id1=15-nu_dim-1
          do m=1,nyrs
           write(800,1851) 
           write(800,1852) percenta(m),nyr1+m-1
          write(800,1844) (nu_nme(i),i=1,nu_dim+1),(as(k),k=1,id1)
          write(800,1845) (nu_sum(m,i),i=1,nu_dim),sum_y(m),
     :                     (as(k),k=1,id1)
           do l=1,12
           if (tdivcu(m,l).ge.0.and.(tsenf(m,l)+
     :         tjunf(m,l)+tothf(m,l)-tarech(m,l)).gt.0) then
                teffcu(m,l) =(tdivcu(m,l)/(tsenf(m,l)+
     :          tjunf(m,l)+tothf(m,l)-tarech(m,l)))*100
                tseffcu(m,l) =(tdivcu(m,l)/(tsenf(m,l)+
     :          tjunf(m,l)+tothf(m,l)+tcloss(m,l)-tarech(m,l)))*100
           else
             teffcu(m,l) = 0
             tseffcu(m,l) = 0
           endif
             if((tulagt(m,l).lt.-998.0).or.
     &          (tcloss(m,l).lt.-998.0)) then
               nonconsumed=-999.0
             else
               nonconsumed=tulagt(m,l)+tcloss(m,l)
             endif
              write(800,1829) method2, amn(l),tet(m,l),teffr(m,l),
     :		treqt(m,l),
     :         twbu(m,l),treq(m,l)
     :          ,tsenf(m,l),tjunf(m,l),tothf(m,l)
     :          ,tdiv(m,l),tcus(m,l),tcuj(m,l)
     :          ,tcuo(m,l),tcut(m,l),tscus(m,l)
     :          ,tscuj(m,l),tscuo(m,l),tscu(m,l)
c     :          ,tulags(m,l),tulagj(m,l),tulago(m,l),tulagt(m,l)
     :          ,tulags(m,l),tulagj(m,l),tulago(m,l),nonconsumed
     :          ,tdivcu(m,l),tseffcu(m,l),ttotts(m,l),ttottj(m,l)
     :          ,ttotto(m,l),ttott(m,l),tcut(m,l)
     :          ,tcusoil(m,l),tcrps(m,l),tcrpj(m,l)
     :          ,tcrpo(m,l),tcrpt(m,l),tcrpj(m,l)
           enddo
          enddo
      endif
      if(isuply .eq. 3) then
c
c write annual totals for all structures combined
c
        do m=1,nyrs
        write(800,1935) percenta(m),method2,nyr1+m-1,
     :	  ettot(nbasin+1,m,14),
     :    effppt(nbasin+1,m,14),reqt(nbasin+1,m,14),wbu(nbasin+1,m,14),
     :   reqreqts(m,14),
     :seniorf(m,14),juniorf(m,14),otherf(m,14),divsup(nbasin+1,m,14),
     :    crop_cus(m,14),crop_cuj(m,14),crop_cuo(m,14),crop_cut(m,14),
     :    soil_cus(m,14),soil_cuj(m,14),soil_cuo(m,14),soil_cu(m,14),
     :    ulags(m,14),ulagj(m,14),ulago(m,14),ulagt(m,14),divcu(m,14),
     :    seffcu(m,14),soiltotts(m,14),soiltottj(m,14),
     :    soiltotto(m,14),soiltott(m,14),crop_cut(m,14)
     :    ,cropcusoil(m,14),estcrps(m,14),estcrpj(m,14),estcrpo(m,14)
     :    ,tcrpt(m,14),lagrets(m,14),lagretj(m,14)
     :    ,lagreto(m,14),lagrett(m,14),laglates(m,14),laglatej(m,14)
     :    ,laglateo(m,14),laglatet(m,14),totret(m,14),deps(m,14)
     :    ,depj(m,14),depo(m,14),dept(m,14),depj(m,14)
       enddo
       write(800,1951) 
c
c write average for all years for all structures
c
            write(800,1913) method2,ettot(nbasin+1,nyrs2,13),
     :    effppt(nbasin+1,nyrs2,13),reqt(nbasin+1,nyrs2,13),
     :    wbu(nbasin+1,nyrs2,13),reqreqts(nyrs2,13),
     :    seniorf(nyrs2,13),juniorf(nyrs2,13),otherf(nyrs2,13),
     :    divsup(nbasin+1,nyrs2,13),crop_cus(nyrs2,13),
     :    crop_cuj(nyrs2,13),
     :    crop_cuo(nyrs2,13),crop_cut(nyrs2,13),soil_cus(nyrs2,13),
     :    soil_cuj(nyrs2,13),soil_cuo(nyrs2,13),soil_cu(nyrs2,13),
     :    ulags(nyrs2,13),ulagj(nyrs2,13),ulago(nyrs2,13),
     :    ulagt(nyrs2,13),divcu(nyrs2,13),seffcu(nyrs2,13),
     :    soiltotts(nyrs,14),soiltottj(nyrs,14),soiltotto(nyrs,14),
     :    soiltott(nyrs,14),crop_cut(nyrs2,13),cropcusoil(nyrs2,13),
     :    estcrps(nyrs2,13),
     :    estcrpj(nyrs2,13),estcrpo(nyrs2,13),tcrpt(nyrs2,13),
     :    lagrets(nyrs2,13),lagretj(nyrs2,13),lagreto(nyrs2,13),
     :    lagrett(nyrs2,13),laglates(nyrs2,13),laglatej(nyrs2,13),
     :    laglateo(nyrs2,13),laglatet(nyrs2,13),totret(nyrs2,13),
     :    deps(nyrs2,13),depj(nyrs2,13),depo(nyrs2,13),dept(nyrs2,13),
     :    depj(nyrs2,13)
c
c  write monthly average for all years for all structures
c
          do k1=1,3
              write(800,1951) 
          enddo 
        write(800,1918) nyr1, nyr2
        do l=1,12
            write(800,1933) method2,amn(l),ettot(nbasin+1,nyrs2,l),
     :    effppt(nbasin+1,nyrs2,l),reqt(nbasin+1,nyrs2,l),
     :    wbu(nbasin+1,nyrs2,l),reqreqts(nyrs2,l),
     :    seniorf(nyrs2,l),juniorf(nyrs2,l),otherf(nyrs2,l),
     :    divsup(nbasin+1,nyrs2,l),crop_cus(nyrs2,l),crop_cuj(nyrs2,l),
     :    crop_cuo(nyrs2,l),crop_cut(nyrs2,l),soil_cus(nyrs2,l),
     :    soil_cuj(nyrs2,l),soil_cuo(nyrs2,l),soil_cu(nyrs2,l),
     :    ulags(nyrs2,l),ulagj(nyrs2,l),ulago(nyrs2,l),
     :    ulagt(nyrs2,l),divcu(nyrs2,l),seffcu(nyrs2,l),
     :    soiltotts(nyrs2,l),soiltottj(nyrs2,l),soiltotto(nyrs2,l),
     :    soiltott(nyrs2,l),crop_cut(nyrs2,l),cropcusoil(nyrs2,l),
     :    estcrps(nyrs2,l),
     :    estcrpj(nyrs2,l),estcrpo(nyrs2,l),tcrpt(nyrs2,l),
     :    lagrets(nyrs2,l),lagretj(nyrs2,l),lagreto(nyrs2,l),
     :    lagrett(nyrs2,l),laglates(nyrs2,l),laglatej(nyrs2,l),
     :    laglateo(nyrs2,l),laglatet(nyrs2,l),totret(nyrs2,l),
     :    deps(nyrs2,l),depj(nyrs2,l),depo(nyrs2,l),dept(nyrs2,l),
     :    depj(nyrs2,l)
        enddo
        write(800,1951) 
c
c write average for all years for all structures
c
            write(800,1914) method2, ettot(nbasin+1,nyrs2,13),
     :    effppt(nbasin+1,nyrs2,13),reqt(nbasin+1,nyrs2,13),
     :    wbu(nbasin+1,nyrs2,13),reqreqts(nyrs2,13),
     :    seniorf(nyrs2,13),juniorf(nyrs2,13),otherf(nyrs2,13),
     :    divsup(nbasin+1,nyrs2,13),crop_cus(nyrs2,13),
     :    crop_cuj(nyrs2,13),
     :    crop_cuo(nyrs2,13),crop_cut(nyrs2,13),soil_cus(nyrs2,13),
     :    soil_cuj(nyrs2,13),soil_cuo(nyrs2,13),soil_cu(nyrs2,13),
     :    ulags(nyrs2,13),ulagj(nyrs2,13),ulago(nyrs2,13),
     :    ulagt(nyrs2,13),divcu(nyrs2,13),seffcu(nyrs2,13),
     :    soiltotts(nyrs,14),soiltottj(nyrs,14),soiltotto(nyrs,14),
     :    soiltott(nyrs,14),crop_cut(nyrs2,13),cropcusoil(nyrs2,13),
     :    estcrps(nyrs2,13),
     :    estcrpj(nyrs2,13),estcrpo(nyrs2,13),tcrpt(nyrs2,13),
     :    lagrets(nyrs2,13),lagretj(nyrs2,13),lagreto(nyrs2,13),
     :    lagrett(nyrs2,13),laglates(nyrs2,13),laglatej(nyrs2,13),
     :    laglateo(nyrs2,13),laglatet(nyrs2,13),totret(nyrs2,13),
     :    deps(nyrs2,13),depj(nyrs2,13),depo(nyrs2,13),dept(nyrs2,13),
     :    depj(nyrs2,13)
          write(800,1951) 
          write(800,1951) 
c
c write out monthly values for each year for all structures combined
c
          if(nu_dim+1 .gt. 10) nu_dim=9
          id1=15-nu_dim-1
          do m=1,nyrs
          write(800,1951) 
          write(800,1952) percenta(m), nyr1+m-1
          write(800,1944) (nu_nme(i),i=1,nu_dim+1),(as(k),k=1,id1)
          write(800,1945) (nu_sum(m,i),i=1,nu_dim),sum_y(m),
     :                       (as(k),k=1,id1)
          do l=1,12
            if (tdivcu(m,l).ge.0.and.(tsenf(m,l)+
     :         tjunf(m,l)+tothf(m,l)-tarech(m,l)).gt.0) then
                teffcu(m,l) =(tdivcu(m,l)/(tsenf(m,l)+
     :          tjunf(m,l)+tothf(m,l)-tarech(m,l)))*100
                tseffcu(m,l) =(tdivcu(m,l)/(tsenf(m,l)+
     :          tjunf(m,l)+tothf(m,l)+tcloss(m,l)-tarech(m,l)))*100
            else
             teffcu(m,l) = 0
             tseffcu(m,l) = 0
            endif
            write(800,1929) method2, amn(l),tet(m,l),teffr(m,l),
     :		  treqt(m,l),
     :           twbu(m,l),treq(m,l)
     :          ,tsenf(m,l),tjunf(m,l),tothf(m,l)
     :          ,tdiv(m,l),tcus(m,l),tcuj(m,l)
     :          ,tcuo(m,l),tcut(m,l),tscus(m,l)
     :          ,tscuj(m,l),tscuo(m,l),tscu(m,l)
     :          ,tulags(m,l),tulagj(m,l),tulago(m,l),tulagt(m,l)
     :          ,tdivcu(m,l),tseffcu(m,l),ttotts(m,l),ttottj(m,l)
     :          ,ttotto(m,l),ttott(m,l),tcut(m,l)
     :          ,tcusoil(m,l),tcrps(m,l),tcrpj(m,l)
     :          ,tcrpo(m,l),tcrpt(m,l),trets(m,l),
     :          tretj(m,l),treto(m,l),trett(m,l),
     :          tlates(m,l),tlatej(m,l),tlateo(m,l),
     :          tlatet(m,l),ttotret(m,l),tdeps(m,l),
     :          tdepj(m,l),tdepo(m,l),tdept(m,l),tdepj(m,l)
           enddo
           enddo
      endif

      if(isuply .eq. 4) then
        do m=1,nyrs
          cutot=crop_cut(m,14)+gwcu(m,14)
          cust=cutot+cropcusoil(m,14)
          if(divsup(nbasin+1,m,14) .gt. 0) then
            conv=fdiv(m,14)/(divsup(nbasin+1,m,14)+tail(nbasin+1,m,14))
          else
            conv=0
          endif
          if(iflood .eq. 0) then
          write(800,1612) percenta(m),method2,nyr1+m-1,
     :   ettot(nbasin+1,m,14),
     :   effppt(nbasin+1,m,14),reqt(nbasin+1,m,14),wbu(nbasin+1,m,14),
     :    reqreqts(m,14),
     :    divsup(nbasin+1,m,14),conv,closs(m,14),fdiv(m,14),arech(m,14),
     :    tsfeff(m),crop_cut(m,14),soil_cu(m,14),ulagt(m,14),
     :    effcu(m,14),gdiv(m,14),effgw(m,14),gwcu(m,14),gwro(m,14),
     :    soiltott(m,14),cutot,cropcusoil(m,14),cust,tdp(m,14)
          else
          write(800,1632) percenta(m),method2,nyr1+m-1,
     :   ettot(nbasin+1,m,14),
     :   effppt(nbasin+1,m,14),reqt(nbasin+1,m,14),wbu(nbasin+1,m,14),
     :    reqreqts(m,14),
     :    divsup(nbasin+1,m,14),conv,closs(m,14),fdiv(m,14),arech(m,14),
     :    tsfeff(m),crop_cut(m,14),soil_cu(m,14),ulagt(m,14),
     :    effcu(m,14),gdiv(m,14),effgw(m,14),gwcu(m,14),gwro(m,14),
     :    soiltott(m,14),cutot,cropcusoil(m,14),cust,tdp(m,14),
     :    gsdiv(m,14),gfdiv(m,14)
          endif
        enddo
        write(800,1651) 
c
c write average for all years for all structures
c
          cutot=crop_cut(nyrs2,13)+gwcu(nyrs2,13)
          cust=cutot+cropcusoil(nyrs2,13)
          if( divsup(nbasin+1,nyrs2,13) .gt. 0) then
            conv=fdiv(nyrs2,13)/
     &       (divsup(nbasin+1,nyrs2,13)+tail(nbasin+1,nyrs2,13))
          else
            conv=0
          endif
          if(iflood .eq. 0) then
          write(800,1613) method2,ettot(nbasin+1,nyrs2,13),
     :   effppt(nbasin+1,nyrs2,13),reqt(nbasin+1,nyrs2,13),
     :   wbu(nbasin+1,nyrs2,13),reqreqts(nyrs2,13),
     :   divsup(nbasin+1,nyrs2,13),conv,closs(nyrs2,13),fdiv(nyrs2,13),
     :   arech(nyrs2,13),tsfeff(nyrs1),crop_cut(nyrs2,13),
     :   soil_cu(nyrs2,13),ulagt(nyrs2,13),effcu(nyrs2,13),
     :   gdiv(nyrs2,13),effgw(nyrs2,13),gwcu(nyrs2,13),gwro(nyrs2,13),
     :   soiltott(nyrs,14),cutot,cropcusoil(nyrs2,13),cust,
     :   tdp(nyrs2,13)
         else
          write(800,1633) method2,ettot(nbasin+1,nyrs2,13),
     :   effppt(nbasin+1,nyrs2,13),reqt(nbasin+1,nyrs2,13),
     :   wbu(nbasin+1,nyrs2,13),reqreqts(nyrs2,13),
     :   divsup(nbasin+1,nyrs2,13),conv,closs(nyrs2,13),fdiv(nyrs2,13),
     :   arech(nyrs2,13),tsfeff(nyrs1),crop_cut(nyrs2,13),
     :   soil_cu(nyrs2,13),ulagt(nyrs2,13),effcu(nyrs2,13),
     :   gdiv(nyrs2,13),effgw(nyrs2,13),gwcu(nyrs2,13),gwro(nyrs2,13),
     :   soiltott(nyrs,14),cutot,cropcusoil(nyrs2,13),cust,
     :   tdp(nyrs2,13),gsdiv(nyrs2,13),gfdiv(nyrs2,13)
         endif
        do k1=1,3
           write(800,1651) 
        enddo

c
c  write monthly average for all years for all structures
c
        write(800,1618) nyr1, nyr2
        do l=1,12
        cutot=crop_cut(nyrs2,l)+gwcu(nyrs2,l)
        cust=cutot+cropcusoil(nyrs2,l)
        if(divsup(nbasin+1,nyrs2,l) .gt. 0) then
          conv=fdiv(nyrs2,l)/
     &    (divsup(nbasin+1,nyrs2,l)+tail(nbasin+1,nyrs2,l))
        else
          conv=0
        endif
        if(iflood .eq. 0) then
         write(800,1610) method2,amn(l),ettot(nbasin+1,nyrs2,l),
     :effppt(nbasin+1,nyrs2,l),reqt(nbasin+1,nyrs2,l),
     :wbu(nbasin+1,nyrs2,l),reqreqts(nyrs2,l),
     :divsup(nbasin+1,nyrs2,l),conv,closs(nyrs2,l),fdiv(nyrs2,l),
     :arech(nyrs2,l),tsfeff(nyrs1),crop_cut(nyrs2,l),
     :soil_cu(nyrs2,l),
     :ulagt(nyrs2,l),effcu(nyrs2,l),gdiv(nyrs2,l),effgw(nyrs2,l),
     :gwcu(nyrs2,l),gwro(nyrs2,l),soiltott(nyrs2,l),
     :cutot,cropcusoil(nyrs2,l),cust,tdp(nyrs2,l)
        else
         write(800,1630) method2,amn(l),ettot(nbasin+1,nyrs2,l),
     :effppt(nbasin+1,nyrs2,l),reqt(nbasin+1,nyrs2,l),
     :wbu(nbasin+1,nyrs2,l),reqreqts(nyrs2,l),
     :divsup(nbasin+1,nyrs2,l),conv,closs(nyrs2,l),fdiv(nyrs2,l),
     :arech(nyrs2,l),tsfeff(nyrs1),crop_cut(nyrs2,l),
     :soil_cu(nyrs2,l),ulagt(nyrs2,l),effcu(nyrs2,l),gdiv(nyrs2,l),
     :effgw(nyrs2,l),gwcu(nyrs2,l),gwro(nyrs2,l),soiltott(nyrs2,l),
     :cutot,cropcusoil(nyrs2,l),cust,tdp(nyrs2,l),gsdiv(nyrs2,l),
     :gfdiv(nyrs2,l)
        endif
        enddo
      write(800,1651)
c
c write average for all years for all structures
c
          cutot=crop_cut(nyrs2,13)+gwcu(nyrs2,13)
          cust=cutot+cropcusoil(nyrs2,13)
          if(divsup(nbasin+1,m,14) .gt. 0) then
            conv=fdiv(nyrs2,13)/
     &      (divsup(nbasin+1,nyrs2,13)+tail(nbasin+1,nyrs2,13))
          else
            conv=0
          endif
         if(iflood .eq. 0) then
          write(800,1614) method2,ettot(nbasin+1,nyrs2,13),
     :   effppt(nbasin+1,nyrs2,13),reqt(nbasin+1,nyrs2,13),
     :   wbu(nbasin+1,nyrs2,13),reqreqts(nyrs2,13),
     :   divsup(nbasin+1,nyrs2,13),conv,closs(nyrs2,13),fdiv(nyrs2,13),
     :   arech(nyrs2,13),tsfeff(nyrs1),crop_cut(nyrs2,13),
     :   soil_cu(nyrs2,13),ulagt(nyrs2,13),effcu(nyrs2,13),
     :   gdiv(nyrs2,13),effgw(nyrs2,13),gwcu(nyrs2,13),gwro(nyrs2,13),
     :   soiltott(nyrs,14),cutot,cropcusoil(nyrs2,13),cust,
     :   tdp(nyrs2,13)
         else
          write(800,1633) method2,ettot(nbasin+1,nyrs2,13),
     :   effppt(nbasin+1,nyrs2,13),reqt(nbasin+1,nyrs2,13),
     :   wbu(nbasin+1,nyrs2,13),reqreqts(nyrs2,13),
     :   divsup(nbasin+1,nyrs2,13),conv,closs(nyrs2,13),fdiv(nyrs2,13),
     :   arech(nyrs2,13),tsfeff(nyrs1),crop_cut(nyrs2,13),
     :   soil_cu(nyrs2,13),ulagt(nyrs2,13),effcu(nyrs2,13),
     :   gdiv(nyrs2,13),effgw(nyrs2,13),gwcu(nyrs2,13),gwro(nyrs2,13),
     :   soiltott(nyrs,14),cutot,cropcusoil(nyrs2,13),cust,
     :   tdp(nyrs2,13),gsdiv(nyrs2,13),gfdiv(nyrs2,13)
         endif

          write(800,1651)
          write(800,1651)
c
c write out monthly values for each year for all structures combined
c

          if(nu_dim+1 .gt. 10) nu_dim=9
          id1=15-nu_dim-1
          do m=1,nyrs
            write(800,1651)
            write(800,1652) percenta(m),nyr1+m-1
            write(800,1644) (nu_nme(i),i=1,nu_dim+1),(as(k),k=1,id1)
            write(800,1645) (nu_sum(m,i),i=1,nu_dim),sum_y(m),
     :                      (as(k),k=1,id1)
             do l=1,12
                cutot=tcut(m,l)+tgwcu(m,l)
                cust=cutot+tcusoil(m,l)
                if(tdiv(m,l)+ttail(m,l) .gt. 0) then
                   conv=tfdiv(m,l)/(tdiv(m,l)+ttail(m,l))
                else
                   conv=0
                endif
                if(iflood .eq. 0) then
                write(800,1611) method2,amn(l),tet(m,l),teffr(m,l),
     :           treqt(m,l),twbu(m,l),treq(m,l),
     :          tdiv(m,l),conv,tcloss(m,l),tfdiv(m,l),tarech(m,l),
     :       tsfeff(m),tcut(m,l),tscu(m,l),tulagt(m,l),teffcu(m,l),
     :          tgdiv(m,l),teffgw(m,l),tgwcu(m,l),tgwro(m,l),
     :          ttott(m,l),cutot,tcusoil(m,l),
     :          cust,ttdp(m,l)
                else
                write(800,1631) method2,amn(l),tet(m,l),teffr(m,l),
     :           treqt(m,l),twbu(m,l),treq(m,l),
     :          tdiv(m,l),conv,tcloss(m,l),tfdiv(m,l),tarech(m,l),
     :       tsfeff(m),tcut(m,l),tscu(m,l),tulagt(m,l),teffcu(m,l),
     :          tgdiv(m,l),teffgw(m,l),tgwcu(m,l),tgwro(m,l),
     :          ttott(m,l),cutot,tcusoil(m,l),
     :          cust,ttdp(m,l),tgsdiv(m,l),tgfdiv(m,l)
                endif
            enddo
         enddo        
      endif

c  format statements
c  600 series for when groundwater is considered
600   Format (256('-'))
601   Format ('|','Year/| Analysis | Potential | Effect| Irrigation|',
     :3x,'EOM',3x,'|    IWR    |',
     :24x,'River Diversion (Surface Water) Accounting',
     :25x,'|',3x,'Ground Water Diversion Accounting',2x,'|Delivered',
     :'|',7x,'Estimated Crop CU',7x,'|',10x,'|')
602   Format('|',5x,'|',10x,'|   Crop    |',7x,'|',3x,'Water',3x,
     :'|  Winter |',3x,'After',3x,'|',91('-'),'|',38('-'),
     :'|',2x,'Soil',3x,'|',31('-'),'|',10x,'|')
603   Format ('|','Month','|  Method  |',4x,'ET',5x,'| Precip|Requiremen
     :t|  Precip |   Winter  |',2x,
     : 'Historic',1x,'|',5x,'|',7x,'|',3x,'Farm',3x,'|Sprnklr|','Maxim',
     :1x,'|',5x,'Farm Diversion to',6x,'|','Calculated|Groundwater|',6x,
     :'|',
     :3x,'Diversion to',4x,'|',1x,'Moisture|',4x,'From',4x,'|',2x,'From'
     :,3x,'|',8x,'|',2x,'Total',3x,'|')
604   Format ('|',5x,'|',10x,'|',11x,'|',7x,'|   (IWR)   |Carryover|'
     :,3x,'Precip  |',3x,'River',3x,'|Conv',
     :1x,'|',1x,'Conv ',1x,'|',1x,'Headgate',1x,'|FHG(Not|Applic|',
     :28('-'),'|',1x,'Surface',2x,'|',11x,'|Calcd',1x,'|',19('-'),'|',
     :1x,'  EOM   |',
     :2x,'Surface/',2x,'|',2x,'Soil',3x,'|',8x,'|',2x,'Month',3x,'|')
605   Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,'|',
     :1x,'Diversion',1x,'|Effic|',
     :1x,'Loss',2x,'|',1x,'Diversion|Applied|Effic |',3x,'CU',3x,'|',
     :1x,'Soil Zone|  Non-  |','Water Appl','|',1x,'Diversion',1x,
     :'|Applic|',4x,'CU',4x,'|  Non-  |',' Contents|',1x,'Groundwater|',
     :1x,'Moisture|',2x,'Total',1x,'|',1x,'  Non-   |')
606   Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,'|',
     :11x,'|',5x,'|',7x,'|',
     :10x,'|',7x,'|'6x,'|',8x,'|',10x,'|Consumed|Effic (%)',
     :1x,'|',11x,'|Effic', 1x, '|',10x,'|Consumed|',9x,'|',
     :2x,'Diversion',1x,'|',9x,'|',8x,'| Consumed |')
c jhb 09-06-07 add variable length column headers for different values of iflood (1 and 2 for now)
c jhb 09-06-07 iflood = 1
2600  Format (310('-'))
2601  Format ('|','Year/| Analysis | Potential | Effect| Irrigation|',
     :3x,'EOM',3x,'|    IWR    |',
     :24x,'River Diversion (Surface Water) Accounting',
     :25x,'|',3x,'Ground Water Diversion Accounting',2x,'|Delivered',
     :'|',7x,'Estimated Crop CU',7x,'|',10x,'|',
     :'  Ground Water   |        |   SubIrrigated  | Tail-  |')
2602  Format('|',5x,'|',10x,'|   Crop    |',7x,'|',3x,'Water',3x,
     :'|  Winter |',3x,'After',3x,'|',91('-'),'|',38('-'),
     :'|',2x,'Soil',3x,'|',31('-'),'|',10x,'|',
     :'-----------------|        |      Crop 1     | Water  |')
2603  Format ('|','Month','|  Method  |',4x,'ET',5x,'| Precip|Requiremen
     :t|  Precip |   Winter  |',2x,
     : 'Historic',1x,'|',5x,'|',7x,'|',3x,'Farm',3x,'|Sprnklr|','Maxim',
     :1x,'|',5x,'Farm Diversion to',6x,'|','Calculated|Groundwater|',6x,
     :'|',
     :3x,'Diversion to',4x,'|',1x,'Moisture|',4x,'From',4x,'|',2x,'From'
     :,3x,'|',8x,'|',2x,'Total',3x,'|',
     :'  Diversion To   |        |                 |        |')
2604  Format ('|',5x,'|',10x,'|',11x,'|',7x,'|   (IWR)   |Carryover|'
     :,3x,'Precip  |',3x,'River',3x,'|Conv',
     :1x,'|',1x,'Conv ',1x,'|',1x,'Headgate',1x,'|FHG(Not|Applic|',
     :28('-'),'|',1x,'Surface',2x,'|',11x,'|Calcd',1x,'|',19('-'),'|',
     :1x,'  EOM   |',
     :2x,'Surface/',2x,'|',2x,'Soil',3x,'|',8x,'|',2x,'Month',3x,'|',
     :'-----------------| Total  |-----------------|--------|')
2605  Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,'|',
     :1x,'Diversion',1x,'|Effic|',
     :1x,'Loss',2x,'|',1x,'Diversion|Applied|Effic |',3x,'CU',3x,'|',
     :1x,'Soil Zone|  Non-  |','Water Appl','|',1x,'Diversion',1x,
     :'|Applic|',4x,'CU',4x,'|  Non-  |',' Contents|',1x,'Groundwater|',
     :1x,'Moisture|',2x,'Total',1x,'|',1x,'  Non-   |',
     :'  Spr   | Flood  |Shortage|   IWR  | Acreage| Diver- |')
2606  Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,'|',
     :11x,'|',5x,'|',7x,'|',
     :10x,'|',7x,'|'6x,'|',8x,'|',10x,'|Consumed|Effic (%)',
     :1x,'|',11x,'|Effic', 1x, '|',10x,'|Consumed|',9x,'|',
     :2x,'Diversion',1x,'|',9x,'|',8x,'| Consumed |',
     :' Acreage| Acreage|        |        |        |  sion  |')
c jhb 09-06-07 iflood = 2
3600  Format (328('-'))
3601  Format ('|','Year/| Analysis | Potential | Effect| Irrigation|',
     :3x,'EOM',3x,'|    IWR    |',
     :24x,'River Diversion (Surface Water) Accounting',
     :25x,'|',3x,'Ground Water Diversion Accounting',2x,'|Delivered',
     :'|',7x,'Estimated Crop CU',7x,'|',10x,'|',
     :'  Ground Water   |        |   SubIrrigated  |',
     :'   SubIrrigated  | Tail-  |')
3602  Format('|',5x,'|',10x,'|   Crop    |',7x,'|',3x,'Water',3x,
     :'|  Winter |',3x,'After',3x,'|',91('-'),'|',38('-'),
     :'|',2x,'Soil',3x,'|',31('-'),'|',10x,'|',
     :'-----------------|        |      Crop 1     |',
     :'      Crop 2     | Water  |')
3603  Format ('|','Month','|  Method  |',4x,'ET',5x,'| Precip|Requiremen
     :t|  Precip |   Winter  |',2x,
     : 'Historic',1x,'|',5x,'|',7x,'|',3x,'Farm',3x,'|Sprnklr|','Maxim',
     :1x,'|',5x,'Farm Diversion to',6x,'|','Calculated|Groundwater|',6x,
     :'|',
     :3x,'Diversion to',4x,'|',1x,'Moisture|',4x,'From',4x,'|',2x,'From'
     :,3x,'|',8x,'|',2x,'Total',3x,'|',
     :'  Diversion To   |        |                 |',
     :'                 |        |')
3604  Format ('|',5x,'|',10x,'|',11x,'|',7x,'|   (IWR)   |Carryover|'
     :,3x,'Precip  |',3x,'River',3x,'|Conv',
     :1x,'|',1x,'Conv ',1x,'|',1x,'Headgate',1x,'|FHG(Not|Applic|',
     :28('-'),'|',1x,'Surface',2x,'|',11x,'|Calcd',1x,'|',19('-'),'|',
     :1x,'  EOM   |',
     :2x,'Surface/',2x,'|',2x,'Soil',3x,'|',8x,'|',2x,'Month',3x,'|',
     :'-----------------| Total  |-----------------|',
     :'-----------------|--------|')
3605  Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,'|',
     :1x,'Diversion',1x,'|Effic|',
     :1x,'Loss',2x,'|',1x,'Diversion|Applied|Effic |',3x,'CU',3x,'|',
     :1x,'Soil Zone|  Non-  |','Water Appl','|',1x,'Diversion',1x,
     :'|Applic|',4x,'CU',4x,'|  Non-  |',' Contents|',1x,'Groundwater|',
     :1x,'Moisture|',2x,'Total',1x,'|',1x,'  Non-   |',
     :'  Spr   | Flood  |Shortage|   IWR  | Acreage|',
     :'   IWR  | Acreage| Diver- |')
3606  Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,'|',
     :11x,'|',5x,'|',7x,'|',
     :10x,'|',7x,'|'6x,'|',8x,'|',10x,'|Consumed|Effic (%)',
     :1x,'|',11x,'|Effic', 1x, '|',10x,'|Consumed|',9x,'|',
     :2x,'Diversion',1x,'|',9x,'|',8x,'| Consumed |',
     :' Acreage| Acreage|        |        |        |',
     :'        |        |  sion  |')
c jhb 09-06-07 iflood = 3
4600  Format (346('-'))
4601  Format ('|','Year/| Analysis | Potential | Effect| Irrigation|',
     :3x,'EOM',3x,'|    IWR    |',
     :24x,'River Diversion (Surface Water) Accounting',
     :25x,'|',3x,'Ground Water Diversion Accounting',2x,'|Delivered',
     :'|',7x,'Estimated Crop CU',7x,'|',10x,'|',
     :'  Ground Water   |        |   SubIrrigated  |',
     :'   SubIrrigated  |   SubIrrigated  | Tail-  |')
4602  Format('|',5x,'|',10x,'|   Crop    |',7x,'|',3x,'Water',3x,
     :'|  Winter |',3x,'After',3x,'|',91('-'),'|',38('-'),
     :'|',2x,'Soil',3x,'|',31('-'),'|',10x,'|',
     :'-----------------|        |      Crop 1     |',
     :'      Crop 2     |      Crop 3     | Water  |')
4603  Format ('|','Month','|  Method  |',4x,'ET',5x,'| Precip|Requiremen
     :t|  Precip |   Winter  |',2x,
     : 'Historic',1x,'|',5x,'|',7x,'|',3x,'Farm',3x,'|Sprnklr|','Maxim',
     :1x,'|',5x,'Farm Diversion to',6x,'|','Calculated|Groundwater|',6x,
     :'|',
     :3x,'Diversion to',4x,'|',1x,'Moisture|',4x,'From',4x,'|',2x,'From'
     :,3x,'|',8x,'|',2x,'Total',3x,'|',
     :'  Diversion To   |        |                 |',
     :'                 |                 |        |')
4604  Format ('|',5x,'|',10x,'|',11x,'|',7x,'|   (IWR)   |Carryover|'
     :,3x,'Precip  |',3x,'River',3x,'|Conv',
     :1x,'|',1x,'Conv ',1x,'|',1x,'Headgate',1x,'|FHG(Not|Applic|',
     :28('-'),'|',1x,'Surface',2x,'|',11x,'|Calcd',1x,'|',19('-'),'|',
     :1x,'  EOM   |',
     :2x,'Surface/',2x,'|',2x,'Soil',3x,'|',8x,'|',2x,'Month',3x,'|',
     :'-----------------| Total  |-----------------|',
     :'-----------------|-----------------|--------|')
4605  Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,'|',
     :1x,'Diversion',1x,'|Effic|',
     :1x,'Loss',2x,'|',1x,'Diversion|Applied|Effic |',3x,'CU',3x,'|',
     :1x,'Soil Zone|  Non-  |','Water Appl','|',1x,'Diversion',1x,
     :'|Applic|',4x,'CU',4x,'|  Non-  |',' Contents|',1x,'Groundwater|',
     :1x,'Moisture|',2x,'Total',1x,'|',1x,'  Non-   |',
     :'  Spr   | Flood  |Shortage|   IWR  | Acreage|',
     :'   IWR  | Acreage|   IWR  | Acreage| Diver- |')
4606  Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,'|',
     :11x,'|',5x,'|',7x,'|',
     :10x,'|',7x,'|'6x,'|',8x,'|',10x,'|Consumed|Effic (%)',
     :1x,'|',11x,'|Effic', 1x, '|',10x,'|Consumed|',9x,'|',
     :2x,'Diversion',1x,'|',9x,'|',8x,'| Consumed |',
     :' Acreage| Acreage|        |        |        |',
     :'        |        |        |        |  sion  |')
c jhb 09-06-07 iflood = 4 or more
5600  Format (364('-'))
5601  Format ('|','Year/| Analysis | Potential | Effect| Irrigation|',
     :3x,'EOM',3x,'|    IWR    |',
     :24x,'River Diversion (Surface Water) Accounting',
     :25x,'|',3x,'Ground Water Diversion Accounting',2x,'|Delivered',
     :'|',7x,'Estimated Crop CU',7x,'|',10x,'|',
     :'  Ground Water   |        |   SubIrrigated  |',
     :'   SubIrrigated  |   SubIrrigated  |   SubIrrigated  | Tail-  |')
5602  Format('|',5x,'|',10x,'|   Crop    |',7x,'|',3x,'Water',3x,
     :'|  Winter |',3x,'After',3x,'|',91('-'),'|',38('-'),
     :'|',2x,'Soil',3x,'|',31('-'),'|',10x,'|',
     :'-----------------|        |      Crop 1     |',
     :'      Crop 2     |      Crop 3     |      Crop 4     | Water  |')
5603  Format ('|','Month','|  Method  |',4x,'ET',5x,'| Precip|Requiremen
     :t|  Precip |   Winter  |',2x,
     : 'Historic',1x,'|',5x,'|',7x,'|',3x,'Farm',3x,'|Sprnklr|','Maxim',
     :1x,'|',5x,'Farm Diversion to',6x,'|','Calculated|Groundwater|',6x,
     :'|',
     :3x,'Diversion to',4x,'|',1x,'Moisture|',4x,'From',4x,'|',2x,'From'
     :,3x,'|',8x,'|',2x,'Total',3x,'|',
     :'  Diversion To   |        |                 |',
     :'                 |                 |                 |        |')
5604  Format ('|',5x,'|',10x,'|',11x,'|',7x,'|   (IWR)   |Carryover|'
     :,3x,'Precip  |',3x,'River',3x,'|Conv',
     :1x,'|',1x,'Conv ',1x,'|',1x,'Headgate',1x,'|FHG(Not|Applic|',
     :28('-'),'|',1x,'Surface',2x,'|',11x,'|Calcd',1x,'|',19('-'),'|',
     :1x,'  EOM   |',
     :2x,'Surface/',2x,'|',2x,'Soil',3x,'|',8x,'|',2x,'Month',3x,'|',
     :'-----------------| Total  |-----------------|',
     :'-----------------|-----------------|-----------------|--------|')
5605  Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,'|',
     :1x,'Diversion',1x,'|Effic|',
     :1x,'Loss',2x,'|',1x,'Diversion|Applied|Effic |',3x,'CU',3x,'|',
     :1x,'Soil Zone|  Non-  |','Water Appl','|',1x,'Diversion',1x,
     :'|Applic|',4x,'CU',4x,'|  Non-  |',' Contents|',1x,'Groundwater|',
     :1x,'Moisture|',2x,'Total',1x,'|',1x,'  Non-   |',
     :'  Spr   | Flood  |Shortage|   IWR  | Acreage|',
     :'   IWR  | Acreage|   IWR  | Acreage|   IWR  | Acreage| Diver- |')
5606  Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,'|',
     :11x,'|',5x,'|',7x,'|',
     :10x,'|',7x,'|'6x,'|',8x,'|',10x,'|Consumed|Effic (%)',
     :1x,'|',11x,'|Effic', 1x, '|',10x,'|Consumed|',9x,'|',
     :2x,'Diversion',1x,'|',9x,'|',8x,'| Consumed |',
     :' Acreage| Acreage|        |        |        |',
     :'        |        |        |        |        |        |  sion  |')

c grb 05-11-00 remove separate print without soil moisture
c607   Format (1x,i4,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
c     :1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.2,
c     :f9.0,8x,'NA',1x,f9.0,f11.0,f12.0,
c     :f7.0,f11.0,f9.0,7x,'NA',1x,10x,'NA',1x,7x,'NA',1x,f9.0,f10.0,1x)
608   Format (T5, 'Soil Moisture Capacity:',11x,f11.2,' af',203(" "))
c grb 05-11-00 remove separate print without soil moisture
609   Format (4x,'Maximum Irrigation Efficiency:',9x,F6.2,206(" "))
c609   Format (2x,'Ave',2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,
c     :f11.0,1x,f11.0,f6.2,f8.0,f11.0,f8.0,
c     :f7.2,f9.0,8x,'NA',1x,f9.0,f11.0,f12.0,
c     :f7.2,f11.0,f9.0,7x,'NA',1x,10x,'NA',1x,7x,'NA',1x,f9.0,f10.0,1x)
610   Format (2x,a3,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.2,
     :f9.0,f11.0,
     : f9.0,f11.0,f12.0,f7.2,f11.0,f9.0,f10.0,f13.0,f10.0,f9.0,f11.0)      
611   Format (2x,a3,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.2,
     :f9.0,f11.0,
     : f9.0,f11.0,f12.0,f7.2,f11.0,f9.0,f10.0,f13.0,f10.0,f9.0,f11.0)      
612   Format (1x,i4,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.2,
     :f9.0,f11.0,
     : f9.0,f11.0,f12.0,f7.2,f11.0,f9.0,f10.0,f13.0,f10.0,f9.0,f11.0)      
613   Format('    % of structure diversions considered:',f8.2,206(" "))
614   Format (2x,'Ave',2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,
     :f11.0,1x,f11.0,f6.2,f8.0,f11.0,f8.0,
     :f7.2,f9.0,f11.0,
     :f9.0,f11.0,f12.0,f7.2,f11.0,f9.0,f10.0,f13.0,f10.0,f9.0,f11.0)
626   Format (2x,'Tot',2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,
     :f11.0,1x,f11.0,f6.2,f8.0,f11.0,f8.0,
     :f7.2,f9.0,f11.0,
     :f9.0,f11.0,f12.0,f7.2,f11.0,f9.0,f10.0,f13.0,f10.0,f9.0,f11.0)
615   Format('    % of structure acreage considered:   ',f8.2,206(" "))
c grb 05-11-00 remove separate print without soil moisture
c616   Format (2x,a3,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
c     :1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.2,
c     :f9.0,8x,'NA',1x,f9.0,f11.0,f12.0,
c     :f7.2,f11.0,f9.0,7x,'NA',1x,10x,'NA',1x,7x,'NA',1x,f9.0,f11.0)
c617   Format (2x,a3,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
c     :1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.2,
c     :f9.0,8x,'NA',1x,f9.0,f11.0,f12.0,
c     :f7.2,f11.0,f9.0,7x,'NA',1x,10x,'NA',1x,7x,'NA',1x,f9.0,f11.0)
618   Format('Monthly Averages ', i4,' - ',i4,227(" "))
619   Format (T5, 'ID:    ',a24,220(" "))
621   Format('Yearly Totals   ', i4,' - ',i4,228(" "))
622   Format ('% of Acreage with Ground Water:   ',f6.2,"  ",f12.0,
     :        ' acres', 195(" "))
623   Format ('% of GW Acreage w/Sprinklers:     ',f6.2,"  ",f12.0,
     :        ' acres', 195(" "))
624   Format ('Analysis Uses ',a39,202(" "))
625   Format ('Conveyance Efficiency = ',f6.2,2x,'Flood Efficiency = ',
     :         f6.2,2x,'Sprinkler Efficiency = ',f6.2,167(" "))
630   Format (2x,a3,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.2,f9.0,f11.0,f9.0,f11.0,f12.0,
     :f7.2,f11.0,f9.0,f10.0,f13.0,f10.0,f9.0,f11.0,20f9.0)
631   Format (2x,a3,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.2,f9.0,f11.0,f9.0,f11.0,f12.0,
     :f7.2,f11.0,f9.0,f10.0,f13.0,f10.0,f9.0,f11.0,20f9.0)
632   Format (1x,i4,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.2,f9.0,f11.0,f9.0,f11.0,f12.0,
     :f7.2,f11.0,f9.0,f10.0,f13.0,f10.0,f9.0,f11.0,20f9.0)
634   Format (2x,'Ave',2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,
     :f11.0,1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.2,f9.0,f11.0,f9.0,f11.0,
     :f12.0,f7.2,f11.0,f9.0,f10.0,f13.0,f10.0,f9.0,f11.0,20f9.0)
c636   Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,'|',
c     :11x,'|',5x,'|',7x,'|',
c     :10x,'|',7x,'|'6x,'|',8x,'|',10x,'|Consumed|Effic (%)',
c     :1x,'|',11x,'|Effic', 1x, '|',10x,'|Consumed|',9x,'|',2x,
c     :'Diversion',1x,'|',9x,'|',8x,'| Consumed|',
c     :' gw spr   gw flood','totshort IWRpastur')
644   format ('Crops',1x,15(1x,a10),84(" "))
645   format ('Acres',1x,15(1x,a10),84(" "))
646   Format(4x,'Note, Drain/Tailwater Supply Available to this Ditch',
     :198(" "))
651   Format (255(" "))
652   Format (i4,4x,a84,163(" "))
660   Format(5x,a12,12(f8.0),1x,a12)
661   Format('# ',a16,' Station Efficiency file for STATEMOD'/
     :'#'/
     :'#     Card 1   Control'/
     :'#     format:  (Free)'/                   
     :'#     NOTE EFF1 IS JANUARY, EFF2 IS FEBRUARY, ETC.'/
     :'#'/
     :'#     ID       cwelid:   Well ID'/
     :'#     Eff1     eff(1)    Efficiency in month 1'/
     :'#     Eff1     eff(2)    Efficiency in month 2'/
     :'#     ...      ....      ...'/
     :'#     Eff1     eff(12)   Efficiency in month 12'/
     :'#'/
     :'#'/                                                      
     :'#1 ID           Eff1    Eff2    Eff3    Eff4    Eff5    Eff6'
     :'Eff7    Eff8    Eff9   Eff10   Eff11   Eff12'/
     :'#----------eb------eb------eb------eb------eb------eb------eb---'
     :'---eb------eb------eb------eb------eb------exb----------eb-----'/
     :'#')
681   Format (t5, 'Starting Soil Moisture:',12x,f10.2,' af',203(" "))
c
c - 700 series write format statements for isuply=1 (126 characters per line)
c

700   Format (179('-'))
701   Format ('|','Year/| Analysis | Potential | Effect| Irrigation|',
     :3x,'EOM',3x,'|    IWR    |',19x,'River Diversion Accounti',
     :'ng',20x,'|',3x,'Soil',2x,'|',7x,'Estimated Crop CU',6x,'|') 
702   Format('|',5x,'|',10x,'|   Crop    |',7x,'|',3x,'Water',3x,
     :'|  Winter |',3x,'After',3x,'|',65('-'),'|',1x,
     : 'Moisture','|',30('-'),'|')
703   Format ('|','Month','|  Method  |',4x,'ET',5x,'| Precip|Requiremen
     :t|  Precip |   Winter  |',2x,
     : 'Historic',1x,'|',3x,'River Diversion To',4x
     :,'|',1x,'Efficiency Calc.',2x,'|',1x,'Contents','|',4x,'From'
     :,3x,'|',2x,'From',3x,'|',2x,'Total',1x,'|')
704   Format ('|',5x,'|',10x,'|',11x,'|',7x,'|   (IWR)   |Carryover|'
     :,3x,'Precip  |',1x,'Diversion',1x
     : ,'|',33('-'),'|',19('-'),'|',9x,'|',1x,'Diversion',1x,'|',3x,
     : 'Soil',2x,'|',8x,'|')
705   Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,'|',
     :11x,'|',5x,'CU',4x,'|','Soil Zone','|   Non-    |',2x,'Div to',1x,
     :'|',1x,'System',2x,'|',9x,'|',11x,'|',1x,'Moisture','|',8x,'|')
706   Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,'|',
     :11x,'|',11x,'|',9x,'| Consumed  |CU and SM|Effic (%)|',9x,'|',11x,
     :'|',9x,'|',8x,'|')
c grb 05-11-00 remove separate print without soil moisture
c707   Format (1x,i4,2x,a10,1x,f11.0,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,1x,
c     :f11.0,1x,f11.0,1x,7x,'NA',1x,
c     : f11.0,1x,f9.0,1x,f9.0,1x,7x,'NA',1x,f11.0,1x,7x,'NA',1x,f9.0)         
708   Format (T5, 'Soil Moisture Capacity:',11x,f11.2,' af',127(" "))
709   Format (4x,'Maximum Irrigation Efficiency:',9x,F6.2,130(" "))
710   Format (2x,a3,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,1x,f11.0,1x,f9.0,1x,
     : f11.0,1x,f9.0,1x,f9.0,1x,f9.0,1x,f11.0,1x,f9.0,1x,f9.0)   
711   Format (2x,a3,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,1x,f11.0,1x,f9.0,1x
     :,f11.0,1x,f9.0,1x,f9.0,1x,f9.0,1x,f11.0,1x,f9.0,1x,f9.0)
712   Format (1x,i4,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,1x,f11.0,1x,f9.0,1x,
     : f11.0,1x,f9.0,1x,f9.0,1x,f9.0,1x,f11.0,1x,f9.0,1x,f9.0)         
713   Format('    % of structure diversions considered:',f8.2,130(" "))
714   Format (2x,'Ave',2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f9.0,1x,
     :f11.0,1x,f9.0,1x,f9.0,1x,f9.0,1x,f11.0,1x,f9.0,1x,f9.0)     
723   Format (2x,'Tot',2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f9.0,1x,
     :f11.0,1x,f9.0,1x,f9.0,1x,f9.0,1x,f11.0,1x,f9.0,1x,f9.0)     
715   Format('    % of structure acreage considered:   ',f8.2,130(" "))
c grb 05-11-00 remove separate print without soil moisture
c716   Format (2x,a3,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
c     :1x,f11.0,1x,f11.0,1x,7x,'NA',1x,
c     : f11.0,1x,f9.0,1x,f9.0,1x,7x,'NA',1x,f11.0,1x,7x,'NA',1x,f9.0)   
c717   Format (2x,a3,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
c     :1x,f11.0,1x,f11.0,1x,7x,'NA',1x
c     :,f11.0,1x,f9.0,1x,f9.0,1x,7x,'NA',1x,f11.0,1x,7x,'NA',1x,f9.0)
718   Format('Monthly Averages ', i4,' - ',i4,151(" "))
719   Format (T5, 'ID:    ',a24,144(" "))
c grb 05-11-00 remove separate print without soil moisture
c720   Format (2x,'Ave',2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,1x,
c     :f11.0,1x,f11.0,1x,f11.0,1x,7x,'NA',1x,
c     :f11.0,1x,f9.0,1x,f9.0,1x,7x,'NA',1x,f11.0,1x,7x,'NA',1x,f9.0)     
721   Format('Yearly Totals   ', i4,' - ',i4,152(" "))
722   Format(4x,'Note, Drain/Tailwater Supply Available to this Ditch',
     :122(" "))
744   format ('Crops',1x,15(1x,a10),8(" "))
745   format ('Acres',1x,15(1x,a10),8(" "))
751   Format (179(" "))
752   Format (i4,4x,a84,87(" "))
781   Format (t5, 'Starting Soil Moisture:',12x,f10.2,' af',127(" "))
c
c - 800 series write format statements for isuply=2 (306 characters per line)
c
800   format(359("-"))
801   Format ('|','Year/|          | Potential | Effect| Irrigation|',
     :3x,'EOM',3x,'|    IWR    |',75x,'River Diversion Accounting',
     :75x,'|Delivered EOM Soil Moisture Content|',23x,'Estimated Crop 
     : CU',21x,'|','Replacement','|') 
802   Format('|',5x,'|          |','   Crop    |',7x,'|',3x,'Water',3x,
     :'|  Winter |',3x,'After',3x,'|',176('-'),
     : '|',35('-'),'|',61('-'),'|','Requirement','|')
803   Format('|','Month','|','  Method  ','|',4x,'ET',5x,'| Precip|
     :Requirement|  Precip |
     :   Winter  |',13x,'Diversion By Priority'
     :,13x,'|',10x,'Diversion to CU',10x,'|',8x,'Add To Soil Moisture',
     :7x,'|',3x,'Non-Consumed River Diversion',4x,'|',2x,'Efficiency Cal
     :c.',
     :2x,'|',1x,'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',2x,
     :'|',1x,'Total',2x,'|'3x,'From',2x,'|',3x,'From',2x,'|',9x,'By wate
     :r rights',8x,'|',2x,'Total',1x,'|',11x,'|')
804   Format ('|',5x,'|',10x,'|',11x,'|',7x,'|   (IWR)   |Carryover|'
     :,3x,'Precip  |',47('-'),'|',35('-'),
     : '|',35('-'),'|',35('-'),'|',20('-'),'|',8x,'|',8x,'|',8x,'|',
     : 8x,'|',2x,'Diver',2x,'|',3x,'Soil',2x,'|',32('-'),'|',8x,'|',
     : 11x,'|')
805    Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,
     :'|',2x,
     :'Senior',3x,'|',2x,'Junior'
     :,3x,'|',3x,'Other',3x,'|',3x,'Total',3x,'|',1x,
     :'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',2x,'|',1x,'Total',2x
     :,'|',1x,'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',2x,'|',1x,
     :'Total',2x,'|',1x,'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',2x
     :,'|',1x,'Total',2x'|',2x,'Div to',1x,'|','Calculated','|'8x'|'
     :,8x,'|',8x,'|',8x,'|',9x,'|',9x,'|',2x,'Senior',2x,'|',2x,'Junior'
     :,2x,'|',3x,'Other',2x,'|'
     :,8x,'|',11x,'|')
806    Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,
     :'|',11x,'|',
     :11x,'|',11x,'|'
     :,11x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|'
     :,8x,'|',8x,'|',8x,'|',8x,'|','CU and SM','|',1x,'Effic (%)','|',
     :8x,'|',8x,'|',8x,'|',8x,'|',9x
     :,'|',9x,'|',10x,'|',10x,'|',10x,'|',8x,'|',11x,'|')
808   Format (T5,'Soil Moisture Capacity:',11x,f11.2,' af',307(" "))
809   Format (4x,'Maximum Irrigation Efficiency:',9x,F6.2,310(" "))
810   Format (T5,'Total Rights :',17x,20x,f10.2,1x,'cfs',290(" "))
811   Format (T5, 'Water rights senior or equal to Cutoff Admin Date: '
     :  ,f10.2,1x,'cfs',290(" "))
812   Format (T5, 'Water rights junior to Cutoff Admin Date:          '
     :  ,f10.2,1x,'cfs',290(" "))
813   Format('    % of structure diversions considered:',f8.2,310(" "))
815   Format('    % of structure acreage considered:   ',f8.2,310(" "))
818   Format('Monthly Averages ', i4,' - ',i4,331(" "))
819   Format (T5, 'ID:    ',a24,323(" "))
821   Format('Yearly Totals   ', i4,' - ',i4,332(" "))
822   Format(4x,'Note, Drain/Tailwater Supply Available to this Ditch',
     : 347(" "))
824   Format (T5, 'Administration Cutoff:  ',26x,f14.5,291(" "))
829   Format (2x,a3,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,1x
     :,f11.0,
     :1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,f9.0,2x,f11.0)
833   Format (2x,a3,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,1x
     :,f11.0,
     :1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,f9.0,2x,f11.0)
834   Format(1x,i4,' No diversion records available for this year',
     :  309(" "))
835   Format (1x,i4,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     : 1x,f11.0,
     :1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,f9.0,2x,f11.0)
836   Format (2x,'Ave',2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0
     :,1x,
     :f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,f9.0,2x,f11.0)
837   Format (2x,'Tot',2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0
     :,1x,
     :f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,f9.0,2x,f11.0)
844   format ('Crops',1x,15(1x,a10),188(" "))
845   format ('Acres',1x,15(1x,a10),188(" "))
851   Format (359(" "))
852   Format (i4,4x,a84,267(" "))
870   Format (T5,110('-'),234(" "))
883   Format (t5, 'Starting Soil Moisture:  Senior = ', f8.2,'   Junior
     := ', f8.2, '   Other = ',f8.2,264(" "))
c
c - 900 series write format statements for isuply=3 (468 characters per line)
c

900   format(522("-"))
901   Format ('|','Year/|          | Potential | Effect| Irrigation|',3x
     :,'EOM',3x,'|    IWR    |',  75x,'River Diversion Accounting',75x,
     :  '|Delivered EOM Soil Moisture Content|',23x,'Estimated Crop C
     :U',22x,'|',45x,'Months Return Flows',
c grb 5-12-00 revise format to reflect sign of depletions and accretions
     :  44x,'|',8x,' River Depletions(+)/Accretions(-)',9x,'|',1x
     :     ,'Replacement','|') 
902   Format('|',5x,'|          |   Crop    |',7x,'|',3x,'Water',3x,
     :'|  Winter |',3x,'After',3x,'|',176('-'),
     : '|',35('-'),'|',62('-'),'|',108('-'),'|',51('-'),'|',1x,
     : 'Requirement','|')
903    Format('|','Month','|  Method  ', '|',4x,'ET',5x,'| Precip|
     :Requiremen
     :t|  Precip |   Winter  |',13x,'Diversion By Priority',
     :13x,'|',10x,'Diversion to CU',10x,'|',8x,'Add To Soil 
     :Moisture',7x,'|',3x,'Non-Consumed River Diversion',4x,'|',2x,
     :'Efficiency 
     :Calc.',2x,'|',1x,'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',2x
     :,'|',1x,'Total',2x,'|'3x,'From',2x,'|',3x,'From',2x,'|',9x,'By 
     :water rights',8x,'|',2x,'Total',2x,'|',11x,'From this 
     :Months Diversion',10x,'|',14x,'From Previous Months',13x
     :,'|',4x,'Total',3x,'|',14x,'By Priority',13x,'|',4x,'Total',3x,
     :'|',12x,'|')
904   Format  ('|',5x,'|',10x,'|',11x,'|',7x,'|   (IWR)   |Carryover|'
     :,3x,'Precip  |',47('-'),'|',35('-'),
     : '|',35('-'),'|',35('-'),'|',20('-'),'|',8x,'|',8x,'|',8x,'|',
     : 8x,'|',2x,'Diver',2x,'|',3x,'Soil',2x,'|',32('-'),'|',9x,'|',
     : 47('-'),'|',47('-'),'|',12x,'|',38('-'),'|',12x,'|',12x,'|')
905   Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,'|'
     :,2x,
     :'Senior',3x,'|',2x,'Junior'
     :,3x,'|',3x,'Other',3x,'|',3x,'Total',3x,'|',1x,
     :'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',2x,'|',1x,'Total',2x
     :,'|',1x,'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',2x,'|',1x,
     :'Total',2x,'|',1x,'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',2x
     :,'|',1x,'Total',2x'|',2x,'Div to',1x,'|','Calculated','|'8x'|'
     :,8x,'|',8x,'|',8x,'|',9x,'|',9x,'|',2x,'Senior',2x,'|',2x,'Junior'
     :,2x,'|',3x,'Other',2x,'|'
     :,9x,'|',2x,
     :'Senior',3x,'|',2x,'Junior',3x,'|',3x,'Other',3x,'|',3x,'Total',
     :3x,'|',2x,'Senior',3x,'|',2x,'Junior',3x,'|',3x,'Other',3x,'|',
     :3x,'Total',3x,'|',12x,'|'3x,'Senior',3x,'|',3x,'Junior',3x,'|'
     :,4x,'Other',3x,'|',12x,'|',12x,'|')
c grb 5-11-00 corrected following overflow line
906    Format ('|',5x,'|',10x,'|',11x,'|',7x,'|',11x,'|',9x,'|',11x,
     :'|',11x,'|',
     :11x,'|',11x,'|'
     :,11x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|'
     :,8x,'|',8x,'|',8x,'|',8x,'|','CU and SM','|',1x,'Effic (%)','|',
     :8x,'|',8x,'|',8x,'|',8x,'|',9x
     :,'|',9x,'|',10x,'|',10x,'|',10x,'|',9x,'|',11x,'|',11x,
     :'|',11x,'|',11x
     :,'|',11x,'|',11x,'|',11x,'|',11x,'|',12x,'|',12x,'|',12x,'|',12x,
     :'|',12x,'|',12x,'|')
908   Format (T5,'Soil Moisture Capacity:',11x,f11.2,' af',470(" "))
909   Format (4x,'Maximum Irrigation Efficiency:',9x,F6.2,473(" "))
910   Format (T5,'Total Rights :',17x,20x,f10.2,1x,'cfs',453(" "))
911   Format (T5, 'Water rights senior or equal to Cutoff Admin Date: '
     :  ,f10.2,1x,'cfs',453(" "))
912   Format (T5, 'Water rights junior to Cutoff Admin Date:          '
     :  ,f10.2,1x,'cfs',453(" "))
913   Format('    % of structure diversions considered:',f8.2,473(" "))
914   Format (T5, 'Return Flow Lag Pattern (%) on next line (only first
     :24 months):',455(" "))
915   Format('    % of structure acreage considered:   ',f8.2,473(" "))
918   Format('Monthly Averages ', i4,' - ',i4,494(" "))
919   Format (T5, 'ID:    ',a24,476(" "))
921   Format('Yearly Totals   ', i4,' - ',i4,495(" "))
922   Format(4x," Note, Dr
     :ain/Tailwater Supply Available to this Ditch",420(" "))
924   Format (T5, 'Administration Cutoff:  ',26x,f14.5,454(" "))
929   Format (2x,a3,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,
     :1x,f11.0,1x,f11.0
     :,1x,f1 1.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,1x,f9.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,f12.0,3x ,f12.0,1x,f12.0,1x,
     :f12.0,1x,f12.0,2x,f12.0,3x,f12.0)
932   Format ('No Diversions for this Structure',491(" "))
933   Format (2x,a3,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,
     :1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,1x,f9.0,1x,f11.0, 1x,f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,f12.0,3x ,f12.0,1x,f12.0,1x,
     :f12.0,1x,f12.0,2x,f12.0,3x,f12.0)
934   Format(1x,i4,' No diversion records available for this year',
     :  461(" "))
935   Format (1x,i4,2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,
     :1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,1x,f9.0,1x,f11.0, 1x,f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,f12.0,3x ,f12.0,1x,f12.0,1x,
     :f12.0,1x,f12.0,2x,f12.0,3x,f12.0)
936   Format (2x,'Ave',2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,
     :f11.0,1x,
     :f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,1x,f9.0,1x,f11.0, 1x,f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,f12.0,3x ,f12.0,1x,f12.0,1x,
     :f12.0,1x,f12.0,2x,f12.0,3x,f12.0)
937   Format (2x,'Tot',2x,a10,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,
     :f11.0,1x,
     :f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,1x,f9.0,1x,f11.0, 1x,f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,f12.0,3x ,f12.0,1x,f12.0,1x,
     :f12.0,1x,f12.0,2x,f12.0,3x,f12.0)
944   format ('Crops',1x,15(1x,a10),351(" "))
945   format ('Acres',1x,15(1x,a10),351(" "))
951   Format (522(" "))
952   Format (i4,4x,a84,430(" "))
960   Format (T5,'|Diversion|Month Following|3rd Mon|4th Mon|5th Mon|6th
     : Mon|7th Mon|8th Mon|9th Mon|10th Mon|11th Mon|12th Mon|13th Mon|1
     :4th Mon|15th Mon|16th Mon|17th Mon|18th Mon|19th Mon|20th Mon|21st
     : Mon|22nd Mon|23rd Mon|24th Mon|',300(" "))
970   Format (T5,218('-'),300(" "))
980   Format (T5,'|',2x,F6.2,1x,'|',5x,F6.2,4x,'|',1x,f6.2,'|',1x,f6.2,
     :  '|',1x,f6.2,'|',1x,f6.2,'|',1x,f6.2,'|',1x,f6.2,'|',1x,
     :  f6.2,'|',15(1x,f6.2,1x,'|'),300(" "))
983   Format (t5, 'Starting Soil Moisture:  Senior = ', f8.2,'   Junior
     := ', f8.2, '   Other = ',f8.2,438(" "))
1600  Format (264('-'))
1601  Format ('|   %   | Analysis |','Year/| Potential | Effect|',
     :' Irrigation|',3x,'EOM',3x,'|    IWR    |',
     :24x,'River Diversion (Surface Water) Accounting',
     :25x,'|',3x,'Ground Water Diversion Accounting',2x,'|Delivered|',
     :7x,'Estimated Crop CU',7x,'|',10x,'|')
1602  Format('|Project|',10x,'|',5x,'|   Crop    |',7x,'|',3x,'Water',
     :3x,'|  Winter |',3x,'After',3x,'|',91('-'),'|',
     :38('-'),'|',2x,'Soil',3x,'|',31('-'),'|',10x,'|')
1603  Format ('| Calcd |  Method  |','Month','|',4x,'ET',5x,'| Precip|Re
     :quirement|  Precip |   Winter  |',2x,
     : 'Historic',1x,'|',5x,'|',7x,'|',3x,'Farm   |Sprnklr|Maxim',1x,'|'
     :,5x,'Farm Diversion to',6x,'|','Calculated|Groundwater|',6x,'|',3x
     :,'Diversion to',4x,'|',1x,'Moisture|',4x,'From',4x,'|',2x,'From',
     :3x,'|',8x,'|',2x,'Total',3x,'|')
1604  Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|   (IWR)   |Carryov
     :er|',3x,'Precip  |',3x,'River',3x,'|Conv',
     :1x,'|',1x,'Conv ',1x,'|',1x,'Headgate |FHG-Not|Applic|',28('-'),
     :'|',1x,'Surface',2x,'|',11x,'|Calcd',1x,'|',19('-'),'|',1x,
     :'  EOM   |',2x,'Surface/',2x,'|',2x,'Soil',3x,'|',8x,'|',2x,
     :'Month',3x,'|')
1605  Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|',11x,'|',9x,'|',
     :11x,'|',1x,'Diversion',1x,'|Effic|',
     :1x,'Loss',2x,'|',1x,'Diversion|Applied|Effic',1x,'|',3x,'CU',3x,
     :'|',1x,'Soil Zone|  Non-  |','Water Appl','|',1x,'Diversion',1x,
     :'|Applic|',4x,'CU',4x,'|  Non-  | Contents|',1x,'Groundwater|',1x,
     :'Moisture|',2x,'Total',1x,'|',1x,'  Non-   |')
1606  Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|',11x,'|',9x,
     :'|',11x,'|',11x,'|',5x,'|',7x,'|',
     :10x,'|',7x,'|',6x,'|',8x,'|',10x,'|Consumed|Effic (%)',
     :1x,'|',11x,'|Effic', 1x, '|',10x,'|Consumed|',9x,'|',
     :2x,'Diversion',1x,'|',9x,'|',8x,'| Consumed |')
6600  Format (282('-'))
6601  Format ('|   %   | Analysis |','Year/| Potential | Effect|',
     :' Irrigation|',3x,'EOM',3x,'|    IWR    |',
     :24x,'River Diversion (Surface Water) Accounting',
     :25x,'|',3x,'Ground Water Diversion Accounting',2x,'|Delivered|',
     :7x,'Estimated Crop CU',7x,'|',10x,'|',
     :'  Ground Water   |')
6602  Format('|Project|',10x,'|',5x,'|   Crop    |',7x,'|',3x,'Water',
     :3x,'|  Winter |',3x,'After',3x,'|',91('-'),'|',
     :38('-'),'|',2x,'Soil',3x,'|',31('-'),'|',10x,'|',
     :'-----------------|')
6603  Format ('| Calcd |  Method  |','Month','|',4x,'ET',5x,'| Precip|Re
     :quirement|  Precip |   Winter  |',2x,
     : 'Historic',1x,'|',5x,'|',7x,'|',3x,'Farm   |Sprnklr|Maxim',1x,'|'
     :,5x,'Farm Diversion to',6x,'|','Calculated|Groundwater|',6x,'|',3x
     :,'Diversion to',4x,'|',1x,'Moisture|',4x,'From',4x,'|',2x,'From',
     :3x,'|',8x,'|',2x,'Total',3x,'|',
     :'  Diversion To   |')
6604  Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|   (IWR)   |Carryov
     :er|',3x,'Precip  |',3x,'River',3x,'|Conv',
     :1x,'|',1x,'Conv ',1x,'|',1x,'Headgate |FHG-Not|Applic|',28('-'),
     :'|',1x,'Surface',2x,'|',11x,'|Calcd',1x,'|',19('-'),'|',1x,
     :'  EOM   |',2x,'Surface/',2x,'|',2x,'Soil',3x,'|',8x,'|',2x,
     :'Month',3x,'|',
     :'-----------------|')
6605  Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|',11x,'|',9x,'|',
     :11x,'|',1x,'Diversion',1x,'|Effic|',
     :1x,'Loss',2x,'|',1x,'Diversion|Applied|Effic',1x,'|',3x,'CU',3x,
     :'|',1x,'Soil Zone|  Non-  |','Water Appl','|',1x,'Diversion',1x,
     :'|Applic|',4x,'CU',4x,'|  Non-  | Contents|',1x,'Groundwater|',1x,
     :'Moisture|',2x,'Total',1x,'|',1x,'  Non-   |',
     :'  Spr   | Flood  |')
6606  Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|',11x,'|',9x,
     :'|',11x,'|',11x,'|',5x,'|',7x,'|',
     :10x,'|',7x,'|',6x,'|',8x,'|',10x,'|Consumed|Effic (%)',
     :1x,'|',11x,'|Effic', 1x, '|',10x,'|Consumed|',9x,'|',
     :2x,'Diversion',1x,'|',9x,'|',8x,'| Consumed |',
     :' Acreage| Acreage|')
c grb 5-11-00 following format not used so commented out
c1607  Format (f6.1,'%',2x,a10,2x,i4,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,
c     :1x,f11.0,1x,f11.0,f6.2,f8.0,f11.0,
c     :f8.0,5x,'NA',f9.0,8x,'NA',1x,f9.0,f11.0,f12.0,f7.2,f11.0,f9.0,
c     :7x,'NA',1x,10x,'NA',1x,7x,'NA',1x,f9.0,f10.0,1x)
1608  Format (T5, 'Soil Moisture Capacity:',11x,f11.2,' af',211(" "))
1610  Format (9x,a10,2x,a3,3x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.0,f9.0
     :,f11.0,f9.0,f11.0,f12.0,f7.2,f11.0,f9.0,f10.0,f13.0,f10.0,f9.0,
     :f10.0,1x)
1611  Format (9x,a10,2x,a3,3x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.0,f9.0
     :,f11.0,f9.0,f11.0,f12.0,f7.2,f11.0,f9.0,f10.0,f13.0,f10.0,f9.0,
     :f10.0,1x)
1612  Format (f6.1,'%',2x,a10,2x,i4,2x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,
     :1x,f11.0,1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.0,
     :f9.0,f11.0,f9.0,f11.0,f12.0,f7.2,f11.0,f9.0,f10.0,f13.0,f10.0
     :,f9.0,f10.0,1x)
1613  Format (9x,a10,2x,'Ave.',2x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,
     :1x,f11.0,1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.0,
     :f9.0,f11.0,f9.0,f11.0,f12.0,f7.2,f11.0,f9.0,f10.0,f13.0,f10.0
     :,f9.0,f10.0,1x)
1614  Format (9x,a10,2x,'Tot.',2x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,
     :1x,f11.0,1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.0,
     :f9.0,f11.0,f9.0,f11.0,f12.0,f7.2,f11.0,f9.0,f10.0,f13.0,f10.0
     :,f9.0,f10.0,1x)
1630  Format (9x,a10,2x,a3,3x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.0,f9.0
     :,f11.0,f9.0,f11.0,f12.0,f7.2,f11.0,f9.0,f10.0,f13.0,f10.0,f9.0,
     :f10.0,f9.0,f9.0,1x)
1631  Format (9x,a10,2x,a3,3x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.0,f9.0
     :,f11.0,f9.0,f11.0,f12.0,f7.2,f11.0,f9.0,f10.0,f13.0,f10.0,f9.0,
     :f10.0,f9.0,f9.0,1x)
1632  Format (f6.1,'%',2x,a10,2x,i4,2x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,
     :1x,f11.0,1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.0,
     :f9.0,f11.0,f9.0,f11.0,f12.0,f7.2,f11.0,f9.0,f10.0,f13.0,f10.0
     :,f9.0,f10.0,f9.0,f9.0,1x)
1633  Format (9x,a10,2x,'Ave.',2x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,
     :1x,f11.0,1x,f11.0,f6.2,f8.0,f11.0,f8.0,f7.0,
     :f9.0,f11.0,f9.0,f11.0,f12.0,f7.2,f11.0,f9.0,f10.0,f13.0,f10.0
     :,f9.0,f10.0,f9.0,f9.0,1x)
c1636  Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|',11x,'|',9x,
c     :'|',11x,'|',11x,'|',5x,'|',7x,'|',
c     :10x,'|',7x,'|',6x,'|',8x,'|',10x,'|Consumed|Effic (%)',
c     :1x,'|',11x,'|Effic', 1x, '|',10x,'|Consumed|',9x,'|',2x,
c     :'Diversion',1x,'|',9x,'|',8x,'| Consumed|',' gw spr   gw flood')

1618  Format('Project Monthly Averages ', i4,' - ',i4,227(" "))
1621  Format('Yearly Totals for Scenario   ', i4,' - ',i4,223(" "))
1644  format ('Crops',1x,15(1x,a10),92(" "))
1645  format ('Acres',1x,15(1x,a10),92(" "))
1651  Format (263(" "))
1652  Format (f6.1,'%',1x,i4,251(" "))
1681  Format (t5, 'Starting Soil Moisture:',12x,f10.2,' af',211(" "))

1700  Format (187('-'))
1701  Format ('|   %   | Analysis |','Year/| Potential | Effect| Irrigat
     :ion|',3x,'EOM',3x,'|    IWR    |',19x,'River Diversion Accounting'
     :,20x,'|',3x,'Soil',2x,'|',7x,'Estimated Crop CU',6x,'|') 
1702  Format('|Project|',10x,'|',5x,'|   Crop    |',7x,'|',3x,'Water',
     :3x,'|  Winter |',3x,'After',3x,'|'  ,65('-'),'|',
     : 1x,'Moisture','|',30('-'),'|')
1703  Format('| Calcd |  Method  |','Month','|',4x,'ET',5x,'| Precip|Req
     :uirement|  Precip |   Winter  |',2x,
     : 'Historic',1x,'|',3x,'River Diversion To',4x
     :,'|',1x,'Efficiency Calc.',2x,'|',1x,'Contents','|',4x,'From'
     :,3x,'|',2x,'From',3x,'|',2x,'Total',1x,'|')
1704  Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|   (IWR)   |Carryov
     :er|',3x,'Precip  |',1x,'Diversion',1x,
     :'|',33('-'),'|',19('-'),'|',9x,'|',1x,'Diversion',1x,'|',3x,
     : 'Soil',2x,'|',8x,'|')
1705  Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|',11x,'|',9x,'|',
     :11x,'|',11x,'|',5x,'CU',4x,
     :'|','Soil Zone','|',1x,'  Non-  ',2x,'|',2x,'Div to',1x,'|',1x,
     :'System',2x,'|',9x,'|',11x,'|',1
     :x,'Moisture','|',8x,'|')
1706   Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|',11x,'|',9x,'|',
     :11x,'|',11x,'|',11x,'|',9x,'| Consumed  |CU and SM|',
     :'Effic (%)','|',9x,'|',11x,'|',9x,'|',8x,'|')
1708  Format (T5, 'Soil Moisture Capacity:',11x,f11.2,' af',135(" "))
1710  Format (9x,a10,2x,a3,2x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,1x,f11.0,1x,f9.0,1x,
     : f11.0,1x,f9.0,1x,f9.0,1x,f9.0,1x,f11.0,1x,f9.0,1x,f9.0)   
1711  Format (9x,a10,2x,a3,2x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,f11.0,1x,f11.0,1x,f9.0,1x
     :,f11.0,1x,f9.0,1x,f9.0,1x,f9.0,1x,f11.0,1x,f9.0,1x,f9.0)
1712  Format (f6.1,'%',2x,a10,2x,i4,1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,
     :1x,f11.0,1x,f11.0,1x,f11.0,1x,f9.0,1x,
     : f11.0,1x,f9.0,1x,f9.0,1x,f9.0,1x,f11.0,1x,f9.0,1x,f9.0)         
1713  Format (9x,a10,2x,'Ave.',1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,
     :1x,f11.0,1x,f11.0,1x,f11.0,1x,f9.0,1x,
     : f11.0,1x,f9.0,1x,f9.0,1x,f9.0,1x,f11.0,1x,f9.0,1x,f9.0)         
1714  Format (9x,a10,2x,'Tot.',1x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,
     :1x,f11.0,1x,f11.0,1x,f11.0,1x,f9.0,1x,
     : f11.0,1x,f9.0,1x,f9.0,1x,f9.0,1x,f11.0,1x,f9.0,1x,f9.0)         
1718  Format('Project Monthly Averages ', i4,' - ',i4,151(" "))
1721  Format('Yearly Totals for Scenario   ', i4,' - ',i4,147(" "))
1744  format ('Crops',1x,15(1x,a10),16(" "))
1745  format ('Acres',1x,15(1x,a10),16(" "))
1751  Format (187(" "))
1752  Format (f6.1,'%',1x,i4,175(" "))
1781  Format (t5, 'Starting Soil Moisture:',12x,f10.2,' af',135(" "))

1800  format(367("-"))
1801  Format ('|   %   |          |','Year/| Potential | Effect|
     : Irrigation|',
     :3x,'EOM',3x,'|    IWR    |',75x,'River Diversion Accounting',
     :75x,'|Deliverd EOM Soil Moisture Content|',23x,'Estimated Crop
     : CU',21x,'|','Replacement','|') 
1802  Format('|Project|',10x,'|','month','|   Crop    |',7x,'|',3x,
     :'Water',3x,
     :'|  Winter |',3x,'After',3x,'|',176('-'),
     : '|',35('-'),'|',61('-'),'|','Requirement','|')
1803  Format ('| Calcd |  Method  |',5x,'|',4x,'ET',5x,'| Precip|
     :Requirement|  Prec
     :ip |   Winter  |',13x,'Diversion By Priority'
     :,13x,'|',10x,'Diversion to CU',10x,'|',8x,'Add To Soil Moisture',
     :7x,'|',3x,'Non-Consumed River Diversion',4x,'|',2x,'Efficiency Cal
     :c.',
     :2x,'|',1x,'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',2x,
     :'|',1x,'Total',2x,'|'3x,'From',2x,'|',3x,'From',2x,'|',9x,'By wate
     :r rights',8x,'|',2x,'Total',1x,'|',11x,'|')
1804  Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|   (IWR)   |
     :Carryover|'
     :,3x,'Precip  |',47('-'),'|',35('-'),
     : '|',35('-'),'|',35('-'),'|',20('-'),'|',8x,'|',8x,'|',8x,'|',
     : 8x,'|',2x,'Diver',2x,'|',3x,'Soil',2x,'|',32('-'),'|',8x,'|',
     : 11x,'|')
1805  Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|',11x,'|',9x,'|'
     :,11x,'|',
     :2x,'Senior',3x,'|',2x,'Junior'
     :,3x,'|',3x,'Other',3x,'|',3x,'Total',3x,'|',1x,
     :'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',2x,'|',1x,'Total',2x
     :,'|',1x,'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',2x,'|',1x,
     :'Total',2x,'|',1x,'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',2x
     :,'|',1x,'Total',2x'|',2x,'Div to',1x,'|','Calculated','|'8x'|'
     :,8x,'|',8x,'|',8x,'|',9x,'|',9x,'|',2x,'Senior',2x,'|',2x,'Junior'
     :,2x,'|',3x,'Other',2x,'|'
     :,8x,'|',11x,'|')
1806   Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|',11x,'|',9x,
     :'|',11x,'|',
     :11x,'|',11x,'|',11x,'|'
     :,11x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|'
     :,8x,'|',8x,'|',8x,'|',8x,'|','CU and SM','|',1x,'Effic (%)','|',
     :8x,'|',8x,'|',8x,'|',8x,'|',9x
     :,'|',9x,'|',10x,'|',10x,'|',10x,'|',8x,'|',11x,'|')
1808  Format (T5,'Soil Moisture Capacity:',11x,f11.2,' af',315(" "))
1813  Format (9x,a10,1x,'Ave.',2x,f11.0,1x,f7.0,1x,f10.0,1x,f10.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,f9.0,2x,f11.0)
1814  Format (9x,a10,1x,'Tot.',2x,f11.0,1x,f7.0,1x,f10.0,1x,f10.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,f9.0,2x,f11.0)
1818  Format('Project Monthly Averages ', i4,' - ',i4,331(" "))
1821  Format('Yearly Totals for Scenario  ', i4,' - ',i4,328(" "))
1829  Format (9x,a10,2x,a3,2x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     :1x,
     :f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,f9.0,2x,f11.0)
1833  Format (9x,a10,2x,a3,2x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,f11.0,
     : 1x,
     :f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,f9.0,2x,f11.0)
1835  Format (f6.1,'%',2x,a10,1x,i4,2x,f11.0,1x,f7.0,1x,f10.0,1x,f10.0,
     :1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,f9.0,2x,f11.0)
1836  Format (2x,1x,10x,'Ave',10x,f11.0,1x,f7.0,1x,f11.0,1x,f9.0,1x,
     :f11.0,1x,
     :f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,f9.0,2x,f11.0)
1844  format ('Crops',1x,15(1x,a10),196(" "))
1845  format ('Acres',1x,15(1x,a10),196(" "))
1851  Format (367(" "))
1852  Format (f6.1,'%',1x,i4,355(" "))
1883  Format (t5, 'Starting Soil Moisture:  Senior = ', f8.2,'   Junior
     := ', f8.2, '   Other = ',f8.2,283(" "))



1900  format(530("-"))
1901  Format ('|   %   |          |','Year/| Potential | Effect| 
     :Irrigation|',3x
     :,'EOM',3x,'|    IWR    |',  75x,'River Diversion Accounting',75x,
     :  '|Delivered EOM Soil Moisture Content|',23x,'Estimated Crop C
     :U',22x,'|',45x,'Months Return Flows',
     :  44x,'|',17x,' River Depletions',17x,'|',1x,'Replacement','|') 
1902   Format('|Project|',10x,'|','month','|   Crop    |',7x,'|',3x,
     :'Water',3x,
     :'|  Winter |',3x,'After',3x,'|',176('-'),
     : '|',35('-'),'|',62('-'),'|',108('-'),'|',51('-'),'|',1x,
     : 'Requirement','|')
1903  Format ('| Calcd |          |',5x,'|',4x,'ET',5x,'| Precip
     :|Requiremen
     :t|  Precip |   Winter  |',13x,'Diversion By Priority',
     :13x,'|',10x,'Diversion to CU',10x,'|',8x,'Add To Soil', 
     :' Moisture',7x,'|',3x,'Non-Consumed River Diversion',4x,'|',2x,'Ef
     :ficienc 
     :y Calc.',2x,'|',1x,'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',
     :2x,'|',1x,'Total',2x,'|'3x,'From',2x,'|',3x,'From',2x,'|',9x,'By 
     :water rights',8x,'|',2x,'Total',2x,'|',11x,'From this 
     :Months Diversion',10x,'|',14x,'From Previous Months',13x
     :,'|',4x,'Total',3x,'|',14x,'By Priority',13x,'|',4x,'Total',3x,
     :'|',12x,'|')
1904   Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|   (IWR)   |
     :Carryover|'
     :,3x,'Precip  |',47('-'),'|',35('-'),
     : '|',35('-'),'|',35('-'),'|',20('-'),'|',8x,'|',8x,'|',8x,'|',
     : 8x,'|',2x,'Diver',2x,'|',3x,'Soil',2x,'|',32('-'),'|',9x,'|',
     : 47('-'),'|',47('-'),'|',12x,'|',38('-'),'|',12x,'|',12x,'|')
1905   Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|',11x,'|',9x,'|'
     :,11x,'|',
     :2x,'Senior',3x,'|',2x,'Junior'
     :,3x,'|',3x,'Other',3x,'|',3x,'Total',3x,'|',1x,
     :'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',2x,'|',1x,'Total',2x
     :,'|',1x,'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',2x,'|',1x,
     :'Total',2x,'|',1x,'Senior',1x,'|',1x,'Junior',1x,'|',1x,'Other',2x
     :,'|',1x,'Total',2x'|',2x,'Div to',1x,'|','Calculated','|'8x'|'
     :,8x,'|',8x,'|',8x,'|',9x,'|',9x,'|',2x,'Senior',2x,'|',2x,'Junior'
     :,2x,'|',3x,'Other',2x,'|'
     :,9x,'|',2x,
     :'Senior',3x,'|',2x,'Junior',3x,'|',3x,'Other',3x,'|',3x,'Total',
     :3x,'|',2x,'Senior',3x,'|',2x,'Junior',3x,'|',3x,'Other',3x,'|',
     :3x,'Total',3x,'|',12x,'|'3x,'Senior',3x,'|',3x,'Junior',3x,'|'
     :,4x,'Other',3x,'|',12x,'|',12x,'|')
1906   Format ('|',7x,'|',10x,'|',5x,'|',11x,'|',7x,'|',11x,'|',9x,'|'
     :,11x,'|',
     :11x,'|',11x,'|',11x,'|'
     :,11x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|',8x,'|'
     :,8x,'|',8x,'|',8x,'|',8x,'|','CU and SM','|',1x,'Effic (%)','|',
     :8x,'|',8x,'|',8x,'|',8x,'|',9x
     :,'|',9x,'|',10x,'|',10x,'|',10x,'|',9x,'|',11x,'|',11x,
     :'|',11x,'|',11x
     :,'|',11x,'|',11x,'|',11x,'|',11x,'|',12x,'|',12x,'|',12x,'|',12x,
     :'|',12x,'|',12x,'|')
1908  Format (T5,'Soil Moisture Capacity:',11x,f11.2,' af',478(" "))
1913  Format (9x,a10,2x,'Ave.',1x,f11.0,1x,f7.0,1x,f11.0,1x,f10.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,1x,f9.0,1x,f11.0, 1x,f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,f12.0,2x ,f12.0,1x,f12.0,1x,
     :f12.0,1x,f12.0)
1914  Format (9x,a10,2x,'Tot.',1x,f11.0,1x,f7.0,1x,f11.0,1x,f10.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,1x,f9.0,1x,f11.0, 1x,f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,f12.0,2x ,f12.0,1x,f12.0,1x,
     :f12.0,1x,f12.0)
1918  Format('Project Monthly Averages ', i4,' - ',i4,494(" "))
1921  Format('Yearly Totals for Scenario   ', i4,' - ',i4,490(" "))
1929  Format (9x,a10,2x,a3,2x,f11.0,1x,f7.0,1x,f11.0,1x,f10.0,1x,f11.0
     :,1x,
     :f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,1x,f9.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,f12.0,2x ,f12.0,1x,f12.0,1x,
     :f12.0,1x,f12.0)
1933  Format (9x,a10,2x,a3,2x,f11.0,1x,f7.0,1x,f11.0,1x,f10.0,1x,f11.0,
     :1x,
     :f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,1x,f9.0,1x,f11.0, 1x,f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,f12.0,2x ,f12.0,1x,f12.0,1x,
     :f12.0,1x,f12.0)
1935  Format (f6.1,'%',2x,a10,1x,i4,2x,f11.0,1x,f7.0,1x,f11.0,1x,f10.0
     :,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,1x,f9.0,1x,f11.0, 1x,f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,f12.0,2x ,f12.0,1x,f12.0,1x,
     :f12.0,1x,f12.0)
1936  Format (13x,'Ave',10x,f11.0,1x,f7.0,1x,f11.0,1x,f10.0,1x,f11.0,1x,
     :f11.0,1x,f11.0,1x,f11.0
     :,1x,f11.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f10.0,1x,f8.0
     :,1x,f8.0,1x,f8.0,1x,f8.0,1x,f9.0,1x,f9.0,1x,f10.0,1x,f10.0,1x
     :,f10.0,1x,f9.0,1x,f11.0, 1x,f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,
     :f11.0,1x,f11.0,1x,f11.0,1x,f11.0,1x,f12.0,2x ,f12.0,1x,f12.0,1x,
     :f12.0,1x,f12.0)
1944  format ('Crops',1x,15(1x,a10),359(" "))
1945  format ('Acres',1x,15(1x,a10),359(" "))
1951  Format(530(" "))
1952  Format (f6.1,'%',1x,i4,518(" "))

1983  Format (t5, 'Starting Soil Moisture:  Senior = ', f8.2,'   Junior
     := ', f8.2, '   Other = ',f8.2,446(" "))
2001  FORMAT('WARNING: IPY file acreage: Year: ',I4,' Structure: ',A12,
     &' URF structure SW-Only acreage must=0. Moving SW-Only acreage to 
     &GW acreage.')
2002  FORMAT('WARNING: IPY file acreage: Year: ',I4,' Structure: ',A12,
     &' Original IPY File Values: SW-Only flood = ',F9.2,
     &' SW-Only sprinkler = ',F9.2,' SW and GW flood = ',F9.2,
     &' SW and GW sprinkler = ',F9.2)
2003  FORMAT('WARNING: IPY file acreage: Year: ',I4,' Structure: ',A12,
     &'  Updated IPY File Values: SW-Only flood = ',F9.2,
     &' SW-Only sprinkler = ',F9.2,' SW and GW flood = ',F9.2,
     &' SW and GW sprinkler = ',F9.2)
2004  FORMAT('WARNING: IPY file acreage: Year: ',I4,' Structure: ',A12,
     &' IPY acreage does not sum to the CDS total acreage. IPY acreage v
     &alues scaled to match CDS total.')
2005  FORMAT('WARNING: IPY file acreage: Year: ',I4,' Structure: ',A12,
     &' CDS File Total Acreage = ',F9.2,
     &' Original IPY File Values: SW-Only flood = ',F9.2,
     &' SW-Only sprinkler = ',F9.2,' SW and GW flood = ',F9.2,
     &' SW and GW sprinkler = ',F9.2)
2006  FORMAT('WARNING: IPY file acreage: Year: ',I4,' Structure: ',A12,
     &' CDS File Total Acreage = ',F9.2,
     &'  Updated IPY File Values: SW-Only flood = ',F9.2,
     &' SW-Only sprinkler = ',F9.2,' SW and GW flood = ',F9.2,
     &' SW and GW sprinkler = ',F9.2)
2007  FORMAT('WARNING: IPY file acreage: Year: ',I4,' Structure: ',A12,
     &'GW acreage > CDS file total acreage. Adjusted GW acreage values t
     &o match.')
2008  FORMAT('WARNING: IPY file acreage: Year: ',I4,' Structure: ',A12,
     &'SW flood acreage < 0.  Reset to 0.')
2009  FORMAT('WARNING: IPY file acreage: Year: ',I4,' Structure: ',A12,
     &'SW sprinkler acreage < 0.  Reset to 0.')
2010  FORMAT('WARNING: IPY file acreage: Year: ',I4,' Structure: ',A12,
     &'GW flood acreage < 0.  Reset to 0.')
2011  FORMAT('WARNING: IPY file acreage: Year: ',I4,' Structure: ',A12,
     &'GW sprinkler acreage < 0.  Reset to 0.')
2012  FORMAT('WARNING: IPY file acreage: Year: ',I4,' Structure: ',A12,
     &'GW total acreage < 0.  Reset to 0.')
2013  FORMAT('WARNING: IPY file acreage: Year: ',I4,' Structure: ',A12,
     &'GW sprinkler acreage > GW acreage. Increasing GW acreage to match
     & GW sprinkler acreage.')
2014  FORMAT('WARNING: IPY file acreage: Year: ',I4,' Structure: ',A12,
     &' Original IPY File Values: SW & GW Total = ',F9.2,
     &' SW and GW sprinkler = ',F9.2)
2015  FORMAT('WARNING: IPY file acreage: Year: ',I4,' Structure: ',A12,
     &'  Updated IPY File Values: SW & GW Total = ',F9.2,
     &' SW and GW sprinkler = ',F9.2)

579   IF(IOUTP .EQ. 1) THEN
         close(256)
      ELSE
         close(256)
c         close(256,status='delete')
      ENDIF
      open(unit=257,file="temp1",status='unknown')
      open(unit=258,file="temp2",status='unknown')
      close(257,status='delete')
      close(258,status='delete')
      close(unit=583)
      open (538,file='SCRATCH',access='direct',recl=124)
      close(538,status='delete')
      close(800)
      close(6)
      close(5)
      if((trim(s4catid).ne."").and.(ipresim.ne.1)) close(413)
      RETURN
      END
