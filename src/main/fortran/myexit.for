c myexit - print a suitable warning and exit the program
c_________________________________________________________________NoticeStart_
c StateCU Consumptive Use Model
c StateCU is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2021 Colorado Department of Natural Resources
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

      SUBROUTINE MYEXIT(ERRNDX)

C***************************************************************************
C
C   Function        : myexit.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This implements the exit routines when the program is 
C                     about to stop.  The history of execution is saved
C                     in *.log file.
C   Calling program : annuacrp.f, frost.f, kbasal.f, monthly.f, perencrp.f,
C                     proto.f, rain.f, distr.f, indece.f, supply.f, julian.f
C   Called programs : none 
C   Input arguments : errndx = error code 
C                     Use a specific code to output a message and then exit.
C                     Use a large number such as 999 to print a generic
C                     message and exit, such as when error message is output
C                     elsewhere.
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Notes           : Currently, error code is limited to two: run is
C                     successful; and errors are detected.  Eventually, the
C                     program should be able to specifically identify what
C                     kind of error was detected.
C
C   History         : (Date, Author, Description)
C      11/01/95   HBM      Error messages 1-13 incorporated.     
C
C***************************************************************************

      INCLUDE 'gcommon.inc'

      INTEGER errndx
      CHARACTER*100 errmsg(99)

      errmsg(1) = 'Scenario Control File is not found!'
      errmsg(2) = 'File *.wbc is not found!'
      errmsg(3) = 'File *.wpm is not found!'
      errmsg(4) = 'File *.kpm is not found!'
      errmsg(5) = 'Crop Coefficient File is not found!'
      errmsg(6) = 'Water Supply File is not found!'
      errmsg(7) = 'File *.ctl is empty!'
      errmsg(8) = 'File *.ctl does not have correct data format!'
      errmsg(9) = 'File *.wbc does not have correct data format!'
      errmsg(10) = 'File *.wpm does not have correct data format!'
      errmsg(11) = 'File *.kpm does not have correct data format!'
      errmsg(12) = 'Crop Coefficient File does not have correct data for
     :mat!'
      errmsg(13) = 'Water Rights File is not found!'

      errmsg(14) = 'Delay Assignment File is not found!'
      errmsg(15) = 'Scenario Control File problem with simulation years'
      errmsg(16) = 'File *.cmn problem with number of basins'
      errmsg(17) = 'File *.cmn problem with ET methods flags'
      errmsg(18) = 'Scenario Control File problem with effective rainfal
     :l flag'
      errmsg(19) = 'File *.cmn problem with effective rainfall param.'
      errmsg(20) = 'Scenario Control File problem with supply flag'
      errmsg(21) = 'Scenario Control File problem with output options'
      errmsg(22) = 'File *.cmn problem with snow melt efficiency'
      errmsg(23) = 'File *.cds crop name does not match crops identified
     : in crop characteristic *.cch file'
      errmsg(24) = 'File *.cmn problem with number of crops'
      errmsg(25) = 'File *.cmn problem with crop name or soil name'
      errmsg(26) = 'File *.cmn problem with crop data title'
      errmsg(27) = 'File *.cmn problem with alfalfa crop data'
      errmsg(28) = 'File *.cmn problem with crop data not alfalfa'
      errmsg(29) = 'File *.cmn problem with basin data title'
      errmsg(30) = 'File *.cmn problem with basin id data'
      errmsg(31) = 'File *.cmn problem with basin data'

      errmsg(32) = 'Crop Characteristic File wrong fall frost method for
     : winter wheat'
      errmsg(33) = 'Crop Characteristic File wrong spring frost method f
     :or spg wheat'
      errmsg(34) = 'Crop Characteristic File wrong spring frost method'
      errmsg(35) = 'Crop Characteristic File wrong fall frost method'

C23456789012345678901234567890123456789012345678901234567890123456789012

      errmsg(36) = 'File *.wbc simulation years exceed .cmn years'
      errmsg(37) = 'File *.wbc # of basins does not match .cmn # basins'
      errmsg(38) = 'File *.wbc problem with sim. years or # basins'
      errmsg(39) = 'File *.wbc problem remarks for temperature weights'
      errmsg(40) = 'File *.wbc problem temperature weights' 
      errmsg(41) = 'File *.wbc problem remarks for precipiation weights'
      errmsg(42) = 'File *.wbc problem precipitation weights' 
      errmsg(43) = 'File *.wbc problem temperature station name' 
      errmsg(44) = 'File *.wbc problem temperature station remark' 
      errmsg(45) = 'File *.wbc problem temperature stat. lat,long,elev' 
      errmsg(46) = 'File *.wbc problem weather data year' 
      errmsg(47) = 'File *.wbc problem weather data comment' 
      errmsg(48) = 'File *.wbc problem weather data' 

      errmsg(49) = 'File *.wpm problem with sim. years or # basins'
      errmsg(50) = 'File *.wpm simulation years exceed .cmn years'
      errmsg(51) = 'File *.wpm # of basins does not match .cmn # basins'
      errmsg(52) = 'File *.wpm problem remarks for temperature weights'
      errmsg(53) = 'File *.wpm problem temperature weights' 
      errmsg(54) = 'File *.wpm problem remarks for precipiation weights'
      errmsg(55) = 'File *.wpm problem precipitation weights' 
      errmsg(56) = 'File *.wpm problem temperature station name' 
      errmsg(57) = 'File *.wpm problem temperature station remark' 
      errmsg(58) = 'File *.wpm problem temperature stat. lat,long,elev' 

      errmsg(59) = 'Latitude of a structure is not valid '

      errmsg(60) = 'File *.sup does not match .cmn # sub-basin '
      errmsg(61) = 'File *.sup problem with simulation years'

      errmsg(62) = 'The number of structures exceeds the dimensions of
     :DIM_NA.'
      errmsg(63) = 'The number of years of simulation exceeds the 
     :dimensions of DIM_NY.'
      errmsg(64) = 'The number of parcels for a subarea exceeds the 
     :dimensions of DIM_NP.' 
      errmsg(65) = 'The number of weather stations exceeds the 
     :dimensions of DIM_NW.' 

      errmsg(70) = 'Error with weather data. No temperature for a month 
     : needed to compute the fall cutoff'
      errmsg(71) = 'Error with converting a day to julian date'
      errmsg(72) = 'Error with converting a month to julian date'
      errmsg(73) = 'Error with weather data. No temperature for a month 
     : needed to compute the spring cutoff'

      errmsg(80) = 'Error with water balance in original BC'

      errmsg(99) = 'Type of error not identified!'

      ! If the error code is outside the known range, output a general message.
      if(errndx.ge.100) then
            write(*,*) 'Stopping - see log file.'
            call exit(1)
      endif

      IF (errndx.eq.0) then
         ! Output a general message and exit the program with status 0 indicating success.
         WRITE(999,*)' run successful - program ended normally'
         WRITE(*,*)' run successful - program ended normally'

         call exit(0)
      else
         ! Output a specific message and exit the program with status 1 indicating failure.
         WRITE(999,*)' error encountered - program aborted'
         WRITE(*,*)' error encountered - program aborted'
         WRITE(999,*)'Error ',errndx,'. ->  ',errmsg(errndx)
         WRITE(*,*)'Error ',errndx,'. ->  ',errmsg(errndx)

         call exit(1)
      endif

      END SUBROUTINE
