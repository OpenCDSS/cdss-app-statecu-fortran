C***************************************************************************
C
C   Include File    : bindat.inc
C   Author          : Jim Brannon
C   Date            : February 2008
C   Purpose         : keeps binary data file related variables organized
C   Calling program : wsupsum.for, mainxc.for, proto.for
C   Assumptions     : 
C   Limitations     :

Cjhb=&==================================================================
Cjhb  Define parameters and variables for the binary files
Cjhb=&==================================================================
Cjhb  IBD#UN = unit number for the binary file number #
      INTEGER IBD1UN,IBD2UN,IBD3UN,IBD4UN,IBD5UN
Cjhb  BD#EXT = file name extension for the binary file number #
      CHARACTER*4 BD1EXT,BD2EXT,BD3EXT,BD4EXT,BD5EXT
Cjhb=&==================================================================
Cjhb  BINARY DATA 1, *.BD1 - Monthly Time Series Data
Cjhb  any number of monthly time series per structure, per sub-basin, per basin, etc.
Cjhb  but also contains some structure info (non time series data) at the top of the file
Cjhb  BD1 UNIT NUMBER, IBD1UN = 1111
      PARAMETER (IBD1UN = 1111)
Cjhb  BD1 (DOS 8.3) FILE EXTENSION, BD1EXT = '.BD1'
      PARAMETER (BD1EXT = '.BD1')
Cjhb=&==================================================================
Cjhb  BINARY DATA 2, *.BD2 - UNSPECIFIED
Cjhb  BD2 UNIT NUMBER, IBD2UN = 1112
      PARAMETER (IBD2UN = 1112)
Cjhb  BD2 (DOS 8.3) FILE EXTENSION, BD2EXT = '.BD2'
      PARAMETER (BD2EXT = '.BD2')
Cjhb=&==================================================================
Cjhb  BINARY DATA 3, *.BD3 - UNSPECIFIED
Cjhb  BD3 UNIT NUMBER, IBD3UN = 1113
      PARAMETER (IBD3UN = 1113)
Cjhb  BD3 (DOS 8.3) FILE EXTENSION, BD3EXT = '.BD3'
      PARAMETER (BD3EXT = '.BD3')
Cjhb=&==================================================================
Cjhb  BINARY DATA 4, *.BD4 - UNSPECIFIED
Cjhb  BD4 UNIT NUMBER, IBD4UN = 1114
      PARAMETER (IBD4UN = 1114)
Cjhb  BD4 (DOS 8.3) FILE EXTENSION, BD4EXT = '.BD4'
      PARAMETER (BD4EXT = '.BD4')
Cjhb=&==================================================================
Cjhb  BINARY DATA 5, *.BD5 - UNSPECIFIED
Cjhb  BD5 UNIT NUMBER, IBD5UN = 1115
      PARAMETER (IBD5UN = 1115)
Cjhb  BD5 (DOS 8.3) FILE EXTENSION, BD5EXT = '.BD5'
      PARAMETER (BD5EXT = '.BD5')
Cjhb=&==================================================================

Cjhb  BD#FN = file name for the binary file number #
      CHARACTER*200 BD1FN,BD2FN,BD3FN,BD4FN,BD5FN
Cjhb  FLDDSCR = text array to store the field descriptions on the
Cjhb     header records in the file.
      CHARACTER*15 :: FLDDSCR(8)
      CHARACTER*29 :: CHARMSG
      CHARACTER*40 :: COLNAME1(100)
      CHARACTER*15 :: COLNAME2(100)
      CHARACTER*55 :: COLTEXT2(100)
      CHARACTER*12 :: CHAR12_1, CHAR12_2
      CHARACTER*24 :: CHTMP1
      CHARACTER*76 :: CHTMP2
Cjhb  IBD#RN = binary file # record number
      INTEGER IBD1RN,IBD2RN,IBD3RN,IBD4RN,IBD5RN
Cjhb  IBD#NF = number of time series data fields in binary file #
      INTEGER IBD1NF,IBD2NF,IBD3NF,IBD4NF,IBD5NF
Cjhb  IBD#NSF = number of structure data fields in binary file #
      INTEGER IBD1NSF,IBD2NSF,IBD3NSF,IBD4NSF,IBD5NSF
Cjhb  IBD#TS = number of total time steps in binary file #
      INTEGER IBD1TS,IBD2TS,IBD3TS,IBD4TS,IBD5TS
Cjhb  IBD#TSA = number of annual time steps in binary file #
      INTEGER IBD1TSA,IBD2TSA,IBD3TSA,IBD4TSA,IBD5TSA
Cjhb  IBD#NFD = number of data field descriptors in binary file #
      INTEGER IBD1NFD,IBD2NFD,IBD3NFD,IBD4NFD,IBD5NFD
Cjhb  ICOLRPT2(I) = integer array flag to determine whether the Ith field
Cjhb                in the binary file is in the standard dwb report
      INTEGER :: ICOLRPT2(100)
Cjhb  LBD#OUT = logical variable to indicate whether to output
Cjhb            the binary file
      LOGICAL :: LBD1OUT,LBD2OUT,LBD3OUT,LBD4OUT,LBD5OUT

      COMMON /BDa/ BD1FN,BD2FN,BD3FN,BD4FN,BD5FN,FLDDSCR,CHARMSG,
     &COLNAME1,COLNAME2,CHAR12_1,CHAR12_2,CHTMP1,CHTMP2
      COMMON /BDb/ IBD1RN,IBD2RN,IBD3RN,IBD4RN,IBD5RN,
     &IBD1NF,IBD2NF,IBD3NF,IBD4NF,IBD5NF,
     &IBD1NSF,IBD2NSF,IBD3NSF,IBD4NSF,IBD5NSF,
     &IBD1TS,IBD2TS,IBD3TS,IBD4TS,IBD5TS,
     &IBD1TSA,IBD2TSA,IBD3TSA,IBD4TSA,IBD5TSA,
     &IBD1NFD,IBD2NFD,IBD3NFD,IBD4NFD,IBD5NFD,
     &ICOLRPT2,LBD1OUT,LBD2OUT,LBD3OUT,LBD4OUT,LBD5OUT