       SUBROUTINE LW_CCU(IYOUNIT)

C***************************************************************************
C
C   Function        : lw_ccu.for 
C   Author          : Erin Wilson
C   Date            : August 2008
C   Purpose         : This routine echos the control options to the log file
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
      INTEGER :: IYOUNIT
      character*60 cuopt
      character*5 ioptcar
C-----Include Global Variables and Data Defaults
      INCLUDE 'gcommon.inc'
C-----Local Variable Declarations
      integer :: i, j

      write(IYOUNIT,*)' '
      write(IYOUNIT,*)' '
      write(IYOUNIT,*)'StateCU Control File Simulation Information '//
     &'Options:'
      write(IYOUNIT,*)'--------------------------------------------'//
     &'--------'
      write(IYOUNIT,900) (TITLE(I), I=1,3)
      write(IYOUNIT,*)'--------------------------------------------'//
     &'--------'
      write(IYOUNIT,200)'Simulation Begin Year                       '//
     &'        ', nyr1
      write(IYOUNIT,200)'Simulation End Year                         '//
     &'        ', nyr2
200   format(A53,I4)
c CU method
      select case (flag1)
      case (1)
         cuopt='Monthly Blaney-Criddle    '
      case (3)   
         cuopt='Daily Penman Monteith     '
      case (4)
         cuopt='Daily Modified Hargreaves '
      case (5)   
         cuopt='Daily ASCE Penman Monteith'
      case default
         cuopt='                          '
      end select
      write(IYOUNIT,*)'Consumptive Use Option                      '//
     &'        ', cuopt
c effective precip method
      select case (rn_xco)   
      case (0)
         cuopt='No Effective Precipitation'
      case (1)   
         cuopt='SCS Monthly Method        '
      case (2)
         cuopt='USBR Monthly Method       '
      case (3)   
         cuopt='Daily Maximum Total       '
      case (4)   
         cuopt='Fraction Daily Total      '
      case (5)   
         cuopt='Daily SCS NEH Method      '
      case default
         cuopt='                          '
      end select
      write(IYOUNIT,*)'Effective Precipitation Method              '//
     &'        ', cuopt  
c analysis type
      select case (iclim)
      case (0)
         cuopt='Climate Station Scenario  '
      case (1)   
         cuopt='Structure Scenario        '
      case default
         cuopt='                          '
      end select
      write(IYOUNIT,*)'Model Analysis Type                         '//
     &'        ', cuopt
      if(iclim .eq. 0) go to 950
c supply option
      select case (isuply)
      case (0)
         cuopt='No Water Supply, IWR Only '
      case (1)   
         cuopt='Surface Supply by Structure '
      case (2)   
         cuopt='Surface Supply and Rights by Structure '
      case (4)   
         cuopt='Surface and Ground Water Supply'
      case default
         cuopt='                          '
      end select
      write(IYOUNIT,*)'Water Supply Option                         '//
     &'        ', cuopt

      IF (ISUPLY .GT. 0) THEN
c soil moisture initialization
      select case (ism)
      case (0)
         cuopt='Do Not Consider Soil Moisture '
      write(IYOUNIT,*)'Soil Moisture Option                        '//
     &'        ', cuopt 
      case (1)   
        cuopt='Consider Soil Moisture, Percent of Capacity = '
      write(IYOUNIT,240)'Soil Moisture Option                       '//
     &'         ', cuopt, psenmo*100 
240   format(A53,A46,F5.2)
      case (2)   
        cuopt='Consider Soil Moisture, Run Presimulation to Initialize'
      write(IYOUNIT,*)'Soil Moisture Option                        '//
     &'        ', cuopt 
      case default
      end select
c winter carry-over precip
      if(smef .eq. 0.) then
         cuopt='Do Not Consider Winter Carry-over Precipitation'
      write(IYOUNIT,*)'Winter Precipitation                        '//
     &'        ', cuopt  
      else
       cuopt='Percent of Winter Precipitation Available to Soil = '
       smef=smef*100
      write(IYOUNIT,250)'Winter Precipitation                       '//
     &'         ', cuopt, ismef            
250   format(A53,A52,I3)
      endif
c Water Rights Options
      if(isuply .eq. 2) then
      select case (idaily)
      case (1)
         cuopt='Use Daily Diversions with Daily Administration Numbers'
      case (2)   
         cuopt='Use Daily diversions with Monthly Administration '//
     &'Numbers'
      case (3)   
         cuopt='Use Daily Diversions with Single Administration  '//
     &'Number'
      case (4)   
         cuopt='Use Monthly Diversions with Monthly Administration '//
     &'Numbers'
      case (5)   
         cuopt='Use Monthly Diversions with Single Administration '//
     &'Number'
      end select
      write(IYOUNIT,*)'Water Rights Analysis Options               '//
     &'        ', cuopt
      endif
c consider supply from Drains      
      select case (idrain)
      case (0)
         cuopt='Do not Consider On-farm Supplies'
      case (1)   
         cuopt='Consider On-farm Supplies'
      case (2)
         cuopt='Use Tailwater/Drain to Offset Diversion to Storage '//
     &'or Recharge'
      end select
      write(IYOUNIT,*)'Supplemental Tailwater/Drain Supply         '//
     &'        ', cuopt
c imiss option
      select case (imiss2)
      case (0)
        select case (climonfly)
        case (0)
          select case (divonfly)
          case (0) !imiss=0
            cuopt='No Filling of Missing Climate Data or Diversions'
          case (1) !imiss=6 - not documented yet
            cuopt='Fill Missing Diversions with Average Monthly '//
     &            'Values; Do Not Fill Missing Climate Data'
          case (2) !imiss=5
            cuopt='Fill Missing Diversions with 0; Do Not Fill'//
     &            'Missing Climate Data'
          end select
        case (1)
          select case (divonfly)
          case (0) !imiss=4
            cuopt='Fill Missing Climate Data with Average Monthly '//
     &            'Values; Do Not Fill Diversions'
          case (1) !imiss=2
            cuopt='Fill Missing Climate and Diversion Data with '//
     &            'Average Monthly Values'
          case (2) !imiss=3
            cuopt='Fill Missing Climate Data with Average Monthly '//
     &            'Values; Missing Diversions with 0'
          end select
        end select
         cuopt='No Filling of Missing Climate Data or Diversions'
      case (1) !imiss=1
         cuopt='Fill Missing Supply Based on Water District '//
     &         'Shortages'
      end select
      write(IYOUNIT,*)'Missing Data Filling Options                '//
     &'        ', cuopt
      
      ENDIF   

100   format(A42,A26)  
900   format(A120)
950   continue
       end