       SUBROUTINE LW_INIT

C***************************************************************************
C
C   Function        : lwinit.for 
C   Author          : Jim Brannon
C   Date            : August 2008
C   Purpose         : This routine initializes the log file warning array variables
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
      integer :: i, j
      scu_debug=0
      do i=1,dim_nlw
        lgwrmsg1(i)=''
        lgwrmsg2(i)=''
        lgwrcnt(i)=0
        lgwrscnt(i)=0
        do j=1,dim_nlws
          lgwrsid(i,j)=''
          lgwrswcnt(i,j)=0
        enddo
      enddo
c     set the log file warning message text strings
      lgwrmsg1(1)=
     &'Response File (*.RCU): Unknown file type description.'
      lgwrmsg2(1)=
     &'An unknown file type description was read in the RCU file. ' //
     &'See the StateCU documentation for allowable file types.'
      lgwrmsg3(1)=
     &'Record ignored.'

      lgwrmsg1(2)=
     &'Response File (*.RCU): Missing file type description.'
      lgwrmsg2(2)=
     &'A record with a missing file type description was read in ' //
     &'the RCU file. See StateCU documentation for RCU file format.'
      lgwrmsg3(2)=
     &'Record ignored.'

      lgwrmsg1(3)=
     &'Control File (*.CCU): ' //
     &'Invalid effective precipitation method selected.'
      lgwrmsg2(3)=
     &'A daily effective precipitation method (RN_XCO>2) can not be ' //
     &'used in a monthly Blaney Criddle analysis.'
      lgwrmsg3(3)=
     &'Effective precipitation is turned off. (RN_XCO=0)'

      lgwrmsg1(4)=
     &'Climate Station File (*.CLI): ' //
     &'Missing instrument height values.'
      lgwrmsg2(4)=
     &'The instrument height for climate station readings is missing.'
      lgwrmsg3(4)=
     &'Default of 1.5m for temperature height and 2m for wind height' //
     &' is used'

      lgwrmsg1(62)=
     &'Structure File (*.STR): ' //
     &'The following structures do not have latitude assigned in the'
      lgwrmsg2(62)=
     &'structure file. Latitudes from associated climate stations '
      lgwrmsg3(62)=
     &'will be used.'

      lgwrmsg1(65)=
     &'Structure File (*.STR): ' //
     &'The following structures do not have elevation assigned in the'
      lgwrmsg2(65)=
     &'structure file. If crops under this acreage are assigned an '//
     &'ET method that includes an elevation adjustment, the elevation'
      lgwrmsg3(65)=
     &'from associated climate stations will be used'

      lgwrmsg1(63)=
     &'Structure File (*.STR): ' //
     &'The following structures do not have Available Water '
      lgwrmsg2(63)=
     &'Content (AWC) values assigned in the structure file.'
      lgwrmsg3(63)=
     &'No soil moisture will be considered for these structures.'

      lgwrmsg1(5)=
     &'Structure File (*.STR): ' //
     &'Using orographic temperature data adjustments.'
      lgwrmsg2(5)=
     &'The following structures use climate station temperature data '//
     &'that has been adjusted'
      lgwrmsg3(5)=
     &'using orographic adjustments set in the structure file.'


      lgwrmsg1(58)=
     &'Structure File (*.STR): ' //
     &'Temperature Orographic Adjustments were requested for the '//
     &'following structures, however they have no elevations' 
      lgwrmsg2(58)=
     &'assigned in the structure file.'
      lgwrmsg3(58)=
     &'No temperature orographic adjustment will be made.'

      lgwrmsg1(6)=
     &'Structure File (*.STR): ' //
     &'Using orographic precipitation data adjustments.'
      lgwrmsg2(6)=
     &'The following structures use climate station precipitation '//
     &'data that has been adjusted'
      lgwrmsg3(6)=
     &'using orographic adjustments set in the structure file.'     

      lgwrmsg1(7)=
     &'Structure File (*.STR): ' //
     &'Climate station temperature weights do not sum to 1.0'
      lgwrmsg2(7)=
     &'The climate station temperature data weighting values ' //
     &'under most circumstances should sum to 1.0 for a structure.'
      lgwrmsg3(7)=
     &'No change was made. Values used as they were found in STR file.'

      lgwrmsg1(8)=
     &'Structure File (*.STR): ' //
     &'Climate station precipitation weights do not sum to 1.0'
      lgwrmsg2(8)=
     &'The climate station precipitation data weighting values ' //
     &'under most circumstances should sum to 1.0 for a structure.'
      lgwrmsg3(8)=
     &'No change was made. Values used as they were found in STR file.'

      lgwrmsg1(9)=
     &'Control File (*.CCU): ' //
     &'Invalid subirrigation flag (IFLOOD>0)'
      lgwrmsg2(9)=
     &'Subirrigated crops currently not allowed when using ' //
     &'daily ET methods.'
      lgwrmsg3(9)=
     &'IFLOOD reset to 0'

      lgwrmsg1(12)=
     &'Control File (*.CCU): ' //
     &'Invalid IDAILY flag value'
      lgwrmsg2(12)=
     &'Water rights calculations are done when ISUPLY = 2 ' //
     &' and the only valid IDAILY values are 1,2,3,4 or 5.'
      lgwrmsg3(12)=
     &'IDAILY reset to 5'

      lgwrmsg1(13)=
     &'Control File (*.CCU): ' //
     &'Missing tailwater/drain file record (*.DRA) in RCU file'
      lgwrmsg2(13)=
     &'IDRAIN is set to 1 or 2, but a drain file is not included' //
     &' in the response (RCU) file.'
      lgwrmsg3(13)=
     &'IDRAIN reset to 0'

      lgwrmsg1(14)=
     &'Control File (*.CCU): ' //
     &'Invalid IDRAIN flag.'
      lgwrmsg2(14)=
     &'The only valid values of the IDRAIN flag are 0,1 or 2'
      lgwrmsg3(14)=
     &'IDRAIN reset to 0'

      lgwrmsg1(59)=
     &'Monthly Crop Coefficient File (*.KBC): ' //
     &'Monthly ET method defined in the *.kbc file for the ' //
     &'following crops do not match the crop extension.'
      lgwrmsg2(59)=
     &'For instance, crops with TR21 extension (eg ALFALFA.TR21) '//
     &'should be assigned ET method of SCS TR21 (ktswt = 0 or 2)'
      lgwrmsg3(59)=
     &'ET method flag reset.'
     
      lgwrmsg1(15)=
     &'Precipitation Data file (*.prc): ' //
     &'The following climate stations have one or more ' //
     &'months of negative precipitation.'
      lgwrmsg2(15)=
     &'This could be the result of ' //
     &'filling using regression with another climate station.'
      lgwrmsg3(15)=
     &'Precipitation for those months is set to 0.'

      lgwrmsg1(52)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'The following structure have conveyance ' //
     &'efficiency set to 1.0 for one or more years.'
      lgwrmsg2(52)=
     &'May be okay if application efficiency represents '//
     &'"system" efficiency or if conveyance efficiency'
      lgwrmsg3(52)=
     &'is negligible.'

      lgwrmsg1(64)=
     &'Crop Distribution File (*.CDS): ' //
     &'The following crop types are assigned' 
      lgwrmsg2(64)=
     &'irrigated acreage in the analysis. '
      lgwrmsg3(64)=
     &' '

      lgwrmsg1(60)=
     &'Crop Distribution File (*.CDS): ' //
     &'Sum of crop acreage does not match total acreage for the ' //
     &'following structures for one or more years.'
      lgwrmsg2(60)=
     &'Crop acreage will be prorated to match the'
      lgwrmsg3(60)=
     &'CDS file total acreage.'

      lgwrmsg1(61)=
     &'Crop Distribution File (*.CDS): ' //
     &'The following structures listed in the *.STR structure ' //
     &'are not included in the crop distribution file.'
      lgwrmsg2(61)=
     &'Crop acreage will be set to 0.'
      lgwrmsg3(61)=
     &'No ET will be calculated'

      lgwrmsg1(40)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'GW acreage > CDS file total acreage for the following ' //
     &'structures for one or more years.'
      lgwrmsg2(40)=
     &'The GW acreage values in the IPY file are ' //
     &'greater than the total acres in the CDS file.'
      lgwrmsg3(40)=
     &'The GW acres are reduced to match the CDS file total.'
     
      lgwrmsg1(41)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'SW flood acreage < 0 for the following structures ' //
     &'for one or more years.'
      lgwrmsg2(41)=
     &'Negative acreages are not allowed. ' //
     &'(Sometimes older IPY files imply negative acreages)'
      lgwrmsg3(41)=
     &'SW flood acreage reset to 0'

      lgwrmsg1(42)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'SW sprinkler acreage < 0 for the following structures ' //
     &'for one or more years.'
      lgwrmsg2(42)=
     &'Negative acreages are not allowed. ' //
     &'(Sometimes older IPY files imply negative acreages)'
      lgwrmsg3(42)=
     &'SW sprinkler acreage reset to 0'

      lgwrmsg1(43)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'GW flood acreage < 0 for the following structures ' //
     &'for one or more years.'
      lgwrmsg2(43)=
     &'Negative acreages are not allowed. ' //
     &'(Sometimes older IPY files imply negative acreages)'
      lgwrmsg3(43)=
     &'GW flood acreage reset to 0'

      lgwrmsg1(44)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'GW sprinkler acreage < 0 for the following structures ' //
     &'for one or more years.'
      lgwrmsg2(44)=
     &'Negative acreages are not allowed. ' //
     &'(Sometimes older IPY files imply negative acreages)'
      lgwrmsg3(44)=
     &'GW sprinkler acreage reset to 0'

      lgwrmsg1(45)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'GW total acreage < 0 for the following structures ' //
     &'for one or more years.'
      lgwrmsg2(45)=
     &'Negative acreages are not allowed. ' //
     &'(Sometimes older IPY files imply negative acreages)'
      lgwrmsg3(45)=
     &'GW acreage reset to 0'

      lgwrmsg1(46)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'GW sprinkler acreage > GW total acreage for the following ' //
     &'structures for one or more years.'
      lgwrmsg2(46)=
     &'(Sometimes older IPY files imply negative acreages)'
      lgwrmsg3(46)=
     &'Increasing GW total acreage to match GW sprinkler acreage'

      lgwrmsg1(55)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'Total Acreage by source and irrigation method does not equal'
      lgwrmsg2(55)=
     &'Total Crop Acreage defined in the crop distribution file ' //
     &'(*.CDS) for the following structures for one or more years.'
      lgwrmsg3(55)=
     &'Scaling IPY acreage to match CDS acreage.'

      lgwrmsg1(48)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'The following structures use the Maximum Supply Approach'
      lgwrmsg2(48)=
     &'to distribute surface water and estimate pumping'
      lgwrmsg3(48)=
     &'for one or more years. GMODE = 1'

      lgwrmsg1(49)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'The following structures use the Mutual Ditch Approach with'
      lgwrmsg2(49)=
     &'Ground Water Pumped to Meet Sprinkler Acreage IWR to  '//
     &'distribute surface water and estimate pumping'
      lgwrmsg3(49)=
     &'for one or more years. GMODE = 3'

      lgwrmsg1(54)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'The following structures are assigned an invalid GMODE value'
      lgwrmsg2(54)=
     &'GMODE must be 1, 2, or 3. GMODE value has been re-assigned  '//
     &'to 2, Mutual Ditch Approach '
      lgwrmsg3(54)=
     &'for one or more years'

      lgwrmsg1(51)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'Stuctures not listed as GMODE = 1 or GMODE = 3 '//
     &'use the Mutual Ditch Approach to' 
      lgwrmsg2(51)=
     &'distribute surface water and estimate pumping'
      lgwrmsg3(51)=
     &'GMODE = 2'

      lgwrmsg1(50)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'IPY total acreage does not match CDS total acreage.'
      lgwrmsg2(50)=
     &'The total acreage defined by irrig method (IPY) does not ' //
     &'match the total acreage defined by crop (CDS).'
      lgwrmsg3(50)=
     &'The IPY acreage will be scaled to match the CDS acreage.'

      lgwrmsg1(53)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'The following structures have ground water acreage but.'
      lgwrmsg2(53)=
     &'estimated pumping capacity is "0" for one or more years' 
      lgwrmsg3(53)=
     &'No pumping will be estimated for those years'

      lgwrmsg1(57)=
     &'Structure Irrigation Parameter File (*.IPY): ' //
     &'The following structures estimated pumping was'
      lgwrmsg2(57)=
     &'limited by pumping capacity defined in the Irrigation ' 
      lgwrmsg3(57)=
     &'Parameter File for one or more years.'

      lgwrmsg1(66)=
     &'Surface Water Supply File(*.DDH): ' //
     &'The following structures are not included in the '
      lgwrmsg2(66)=
     &'surface water supply file. May be okay if only '// 
     &'source is ground water and acreage has been assigned'
      lgwrmsg3(66)=
     &'to SW/GW flood or SW/GW sprinkler in the *.ipy file. '

      lgwrmsg1(56)=
     &'Tailwater/Drain File (*.DRA): ' //
     &'The following structures are included in the Tailwater '
      lgwrmsg2(56)=
     &'/Drain File but are not included in the analysis. The supply' 
      lgwrmsg3(56)=
     &'associated with these structures will be ignored.'

      lgwrmsg1(47)=
     &'Tailwater/Drain File (*.DRA): Negative value too large for '//
     &'the following structures in one or more years.'
      lgwrmsg2(47)=
     &'Value can not be larger (neg) than sw farm delivery or else ' //
     &' farm delivery will become negative.'
      lgwrmsg3(47)=
     &'Drain value reset to -(farm deliv) and farm deliv set to 0.0'
     
      lgwrmsg1(100)=
     &'Log file warning 100: Unknown warning index detected.'
      lgwrmsg2(100)='Warning index should be between 1 and 100'
      lgwrmsg3(100)=''

      END
