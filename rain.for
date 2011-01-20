      SUBROUTINE RAIN (DOY,EFFPCP)

C***************************************************************************
C
C   Function        : rain.f 
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This calculates the daily effective rainfall used with
C                     the Penman-Monteith ET method.  
C   Calling program : proto.f
C   Called programs : myexit.f 
C   Input arguments : doy      = day of the year
C                     RN_XCO   = method for calculating effective rainfall
C                     rnfac    = factor to be multiplied to total rainfall
C                                to get effective when irn = 2
C                     cn(3)    = curve number parameters when irn = 3
C                     ET(366) = daily irrigation amount when irn = 3. Only
C                                irrigation of the past 5 days is used.
C                     rnfall(366)= daily total rainfall.  Only the total
C                                rainfall of the current day pcp(doy) is
C                                used.
C   Output arguments: EFFPCP   = effective daily rainfall of the current
C                                day. 
C   Assumptions     :
C   Limitations     :
C   Notes           : Daily effective rainfall is calculated using three
C                     methods namely:
C                     1 = all daily total rainfall below 1.0 in is effective
C                         with maximum effective = 1.0 in.
C                     2 = effective rainfall = factor x total rainfall
C                     3 = curve number method from NEH SEC 4 METHOD
C                     A choice of irn = 0 causes the program to ignore 
C                     rainfall in the soil moisture budget.
C                     Routines for the Curve Number Method were taken from 
C                     the rain.f subroutine of SMB program developed by
C                     Wheeler and Associates.
C
C***************************************************************************

C-----Include Global Variables and Data Defaults
      INCLUDE 'pmcommon.inc'
      INCLUDE 'gcommon.inc'

C-----Local Variable Declaration
      INTEGER  DOY
      REAL EFFPCP, AM, S, X, RO

C-----Do not consider rainfall
      IF (RN_XCO.LT.3) THEN
         EFFPCP = 0.0
C-----Maximum Effective Precipitation (define as CN(1))
      ELSEIF (RN_XCO.EQ.3) THEN
         EFFPCP = AMIN1(RNFALL(DOY), CN(1))
C-----Effective Precipitation is specified from a factor
      ELSEIF (RN_XCO.EQ.4) THEN
         EFFPCP = RNFALL(DOY) * cn(1)
C-----Effective Precipitation is estimated using NEH Curve number method 
      ELSEIF (RN_XCO.EQ.5) THEN
        IF (DOY.EQ.1) THEN
	     AM = 0
	     ELSEIF (DOY.EQ.2) THEN
            AM = RNFALL(1)
         ELSEIF (DOY.EQ.3) THEN
           AM = RNFALL(2)+RNFALL(1)
         ELSEIF (DOY.EQ.4) THEN
           AM = RNFALL(3)+RNFALL(2)+RNFALL(1)
         ELSEIF (DOY.EQ.5) THEN
           AM = RNFALL(4)+RNFALL(3)+RNFALL(2)+RNFALL(1)
         ELSE
            AM = RNFALL(DOY-1)+RNFALL(DOY-2)+RNFALL(DOY-3)+RNFALL(DOY-4)
     !           +RNFALL(DOY-5)
         ENDIF
         IF (AM .LT. 1.4)  IAMC = 1
         IF (AM .GE. 1.4 .AND. AM .LT. 2.1)  IAMC = 2
         IF (AM .GE. 2.1)  IAMC = 3


	     IF (DOY.GE.5) THEN
            IF (ET(DOY-1) .GT. 0 .OR. ET(DOY-2) .GT. 0 .OR.
     :         ET(DOY-3) .GT. 0 .OR. ET(DOY-4) .GT. 0 .OR.
     :         ET(DOY) .GT. 0)  IAMC = 3
         ELSEIF (DOY.EQ.4) THEN
            IF (ET(DOY-1) .GT. 0 .OR. ET(DOY-2) .GT. 0 .OR.
     :         ET(DOY-3) .GT. 0 .OR. ET(DOY) .GT. 0)  IAMC = 3
    	 ELSEIF (DOY.EQ.3) THEN
            IF (ET(DOY-1) .GT. 0 .OR. ET(DOY-2) .GT. 0 .OR.
     :         ET(DOY) .GT. 0)  IAMC = 3
	     ELSEIF (DOY.EQ.2) THEN
            IF (ET(DOY-1) .GT. 0 .OR. ET(DOY) .GT. 0)  IAMC = 3
         ELSE
            IF (ET(DOY) .GT. 0) IAMC = 3
         ENDIF


         S = 1000/CN(IAMC) - 10
         X = RNFALL(DOY) - 0.2 * S
         IF (X .LE. 0)  X = 0
         RO = X**2 / (RNFALL(DOY) + 0.8 * S)
         EFFPCP = RNFALL(DOY) - RO
C-----Error - no method is chosen - abort program
      ELSE
         CALL MYEXIT(99)
      ENDIF

      RETURN
      END

