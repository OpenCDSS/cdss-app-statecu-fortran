c init
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

      subroutine fnm_init()
      INCLUDE 'gcommon.inc'
      dfile=''
      rcufile=''
      ccufile=''
      kbcfile=''
      tmpfile=''
      pptfile=''
      ddhfile=''
      othfile=''
      logfile=''
      cdsfile=''
      fdfile=''
      cchfile=''
      kpmfile=''
      strfile=''
      ddsfile=''
      kmhfile=''
      ddrfile=''
      penfile=''
      tmxfile=''
      tmnfile=''
      pdyfile=''
      solfile=''
      vapfile=''
      wndfile=''
      dlyfile=''
      ipyfile=''
      pvhfile=''
      dlafile=''
      clifile=''
      admfile=''
      dddfile=''
      addfile=''
      dcufile=''
      gisfile=''
      swdfile=''
      rcrfile='xxx'
      pcrfile='xxx'
      end subroutine
      
      subroutine gdata_init()
      INCLUDE 'gcommon.inc'
      month(1)=31
      month(2)=28
      month(3)=31
      month(4)=30
      month(5)=31
      month(6)=30
      month(7)=31
      month(8)=31
      month(9)=30
      month(10)=31
      month(11)=30
      month(12)=31
      middle(1)=16
      middle(2)=45
      middle(3)=75
      middle(4)=105
      middle(5)=136
      middle(6)=166
      middle(7)=197
      middle(8)=228
      middle(9)=258
      middle(10)=289
      middle(11)=319
      middle(12)=350
      SLINE='-----------------------------------------------------------
     :----------------------------'
      DLINE='===========================================================
     :============================'
      AMN(1)='Jan'
      AMN(2)='Feb'
      AMN(3)='Mar'
      AMN(4)='Apr'
      AMN(5)='May'
      AMN(6)='Jun'
      AMN(7)='Jul'
      AMN(8)='Aug'
      AMN(9)='Sep'
      AMN(10)='Oct'
      AMN(11)='Nov'
      AMN(12)='Dec'
      SNAME(1)='CLAY'
      SNAME(2)='SILTY_CLAY'
      SNAME(3)='SANDY_CLAY'
      SNAME(4)='SILTY_CLAY_LOAM'
      SNAME(5)='SANDY_CLAY_LOAM'
      SNAME(6)='CLAY_LOAM'
      SNAME(7)='SILT'
      SNAME(8)='SILT_LOAM'
      SNAME(9)='LOAM'
      SNAME(10)='SANDY_LOAM'
      SNAME(11)='LOAMY_SAND'
      SNAME(12)='SAND'
      SNAME(13)='NO_SOIL'
      SNAME(14)='NO_SOIL_EVAP'
      SLLINE='----------------------------------------------------------
     :------------------------------------------------------------------
     :----'
      DLLINE='==========================================================
     :==================================================================
     :===='
      ddstrctt=0
      WARNINGS=.FALSE.
      end subroutine    
