***************************************************************************
C
C   Include File    : pmdata.inc
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This includes declaration of all variables and 
C                     parameters used in the Penman-Monteith calculation
C                     of reference evapotranspiration (soil moisture budget
C                     not included.
C   Calling program : etref.f, etrref.f, etoref.f 
C   Assumptions     :
C   Limitations     :
C   Notes           :
C
C***************************************************************************


      REAL HCALF, LHCALF,LAIALF,ZOMALF,ZOHALF,DALF,RCALF
      REAL HCGRS, LAIGRS, ZOMGRS, ZOHGRS, DGRS, RCGRS, B1, PI

      PARAMETER(HCALF=0.5)
      PARAMETER(LHCALF=-0.69314718)                      ! ln(hcalf)
      PARAMETER(LAIALF = 5.5 + 1.5*LHCALF)
      PARAMETER(ZOMALF = .123*HCALF)
      PARAMETER(ZOHALF = .0123*HCALF)
      PARAMETER(DALF = .67*HCALF)
      PARAMETER(RCALF = 200/LAIALF)
      PARAMETER(HCGRS = .12)
      PARAMETER(LAIGRS = 24*HCGRS)
      PARAMETER(ZOMGRS = .123*HCGRS)
      PARAMETER(ZOHGRS = .0123*HCGRS)
      PARAMETER(DGRS = .67*HCGRS)
      PARAMETER(RCGRS = 200/LAIGRS)
      PARAMETER(B1 = -0.139)     
      PARAMETER(PI = 3.14159)     

C0000000011111111112222222222333333333344444444445555555555666666666677777777778
C2345678901234567890123456789012345678901234567890123456789012345678901234567890
Cjhb=&==================================================================
Cjhb  add the named constants for tokens for rcu file reading
Cjhb=&==================================================================
      character(len=*),parameter :: Token1="ccu"
      character(len=*),parameter :: Token41="CUControl"
      character(len=*),parameter :: Token2="str"
      character(len=*),parameter :: Token42="Structure"
      character(len=*),parameter :: Token3="cli"
      character(len=*),parameter :: Token43="ClimateStation"
      character(len=*),parameter :: Token4="kbc"
      character(len=*),parameter :: 
     & Token44="Blaney-Criddle_CropCoefficient"
      character(len=*),parameter :: Token5="cds"
      character(len=*),parameter :: Token45="CropDistribution_Yearly"
      character(len=*),parameter :: Token6="tmp"
      character(len=*),parameter :: Token46="MeanTemperature_Monthly"
      character(len=*),parameter :: Token7="ppt"
      character(len=*),parameter :: Token47="Precipitation_Monthly"
      character(len=*),parameter :: Token8="fd"
      character(len=*),parameter :: Token48="FrostDate_Yearly"
      character(len=*),parameter :: Token9="cch"
      character(len=*),parameter :: Token49="CropCharacteristic"
      character(len=*),parameter :: Token10="ddh"
      character(len=*),parameter :: Token50="Diversion_Historic_Monthly"
      character(len=*),parameter :: Token11="pvh"
      character(len=*),parameter :: Token51="Well_Historic_Monthly"
      character(len=*),parameter :: Token12="ipy"
      character(len=*),parameter :: Token52="IrrigationParameter_Yearly"
      character(len=*),parameter :: Token13="ddr"
      character(len=*),parameter :: Token53="Diversion_Right"
      character(len=*),parameter :: Token14="oth"
      character(len=*),parameter :: Token54="OtherUses"
      character(len=*),parameter :: Token15="dla"
      character(len=*),parameter :: Token55="DelayAssignment"
      character(len=*),parameter :: Token16="dly"
      character(len=*),parameter :: Token56="DelayTable_Monthly"
      character(len=*),parameter :: Token17="adm"
      character(len=*),parameter :: Token57="AdministrationDate_Monthly"
      character(len=*),parameter :: Token18="ddd"
      character(len=*),parameter :: Token58="Diversion_Historic_Daily"
      character(len=*),parameter :: Token19="add"
      character(len=*),parameter :: Token59="AdministrationDate_Daily"
      character(len=*),parameter :: Token20="kpm"
      character(len=*),parameter :: Token60="Penman_CropCoefficient"
      character(len=*),parameter :: Token21="kmh"
      character(len=*),parameter :: 
     & Token61="ModifiedHargreaves_CropCoefficient"
      character(len=*),parameter :: Token22="dra"
      character(len=*),parameter :: Token62="Drain_Historic_Monthly"
      character(len=*),parameter :: Token23="tmx"
      character(len=*),parameter :: Token63="MaxTemperature_Daily"
      character(len=*),parameter :: Token24="tmn"
      character(len=*),parameter :: Token64="MinTemperature_Daily"
      character(len=*),parameter :: Token25="pdy"
      character(len=*),parameter :: Token65="Precipitation_Daily"
      character(len=*),parameter :: Token26="sol"
      character(len=*),parameter :: Token66="SolarRadiation_Daily"
      character(len=*),parameter :: Token27="vap"
      character(len=*),parameter :: Token67="VaporPressure_Daily"
      character(len=*),parameter :: Token28="wnd"
      character(len=*),parameter :: Token68="Wind_Daily"
Cjhb=&==================================================================
Cjhb  the following is recognized, but ignored by StateCU (used by StateCUI)
Cjhb=&==================================================================
      character(len=*),parameter :: Token29="gis"
      character(len=*),parameter :: Token69="GeographicInformation"
Cjhb=&==================================================================
Cjhb  the following are used by StateCU, but not StateCUI (yet!)
Cjhb=&==================================================================
      character(len=*),parameter :: Token30="rcr"
      character(len=*),parameter :: 
     & Token70="Replacement_Crop_Requirement"
      character(len=*),parameter :: Token31="pcr"
      character(len=*),parameter :: 
     & Token71="PartialReplacement_Crop_Requirement"
C0000000011111111112222222222333333333344444444445555555555666666666677777777778
C2345678901234567890123456789012345678901234567890123456789012345678901234567890
Cjhb=&==================================================================
Cjhb  add the named constants for tokens for gis file reading
Cjhb  however, not used in StateCU at all - just here for FYI
Cjhb  and synchronization between StateCU and StateCUI
Cjhb=&==================================================================
      character(len=*),parameter :: Token80="gis_state"
      character(len=*),parameter :: Token81="stateboundary"
      character(len=*),parameter :: Token82="stategis"
      character(len=*),parameter :: Token83="gis_div"
      character(len=*),parameter :: Token84="basin"
      character(len=*),parameter :: Token85="divgis"
      character(len=*),parameter :: Token86="divisionboundary"
      character(len=*),parameter :: Token87="gis_hydro"
      character(len=*),parameter :: Token88="hydrogis"
      character(len=*),parameter :: Token89="rivers"
      character(len=*),parameter :: Token90="gis_str"
      character(len=*),parameter :: Token91="strucgis"
      character(len=*),parameter :: Token92="diversion"
      character(len=*),parameter :: Token93="gis_clim"
      character(len=*),parameter :: Token94="climgis"
      character(len=*),parameter :: Token95="climatestation"
Cjhb=&==================================================================
Cjhb=&==================================================================
Cjhb  add the named constants for crop name tokens
Cjhb=&==================================================================
      character(len=*),parameter :: CrpTkn1="TR21"
      character(len=*),parameter :: CrpTkn2="CCUP"
      character(len=*),parameter :: CrpTkn3="CCLP"
      character(len=*),parameter :: CrpTkn4="CCRG"
      character(len=*),parameter :: CrpTkn5="DWHA"
      character(len=*),parameter :: CrpTkn6="UGHA"
      character(len=*),parameter :: CrpTkn7="POCHOP"
      character(len=*),parameter :: CrpTkn8="HA"
      character(len=*),parameter :: CrpTkn9="CCHA"
      character(len=*),parameter :: CrpTkn0="ASCEPM"
