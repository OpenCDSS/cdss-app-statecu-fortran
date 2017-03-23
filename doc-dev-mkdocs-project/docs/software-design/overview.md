# Software Design / Overview

**TODO smalers 2016-12-31 This is a very rough outline and needs work to educate developers about the StateCU code organization.** 


The StateCU Fortran code is divided into separate files with .for extension.
Each subroutine is in a separate file and the name of the file matches the subroutine (is this globally true? a grep on code seemed to indicate so).
>_[JHB] It is my observation, too, that this rule was followed by previous developers, and I think (90% sure) I did as well.  However I have a niggling memory of once or twice putting a couple of tiny subroutines into one file at some point, somewhere in StateCU or StateMod, so perhaps qualify the statement with __"generally"__ somewhere in there._

Need to describe code modules at a high level:
>_[JHB] Wow, lots of work to do below.  A decade or more ago there were several flow charts and diagrams of StateCU code logic and organization.  At some point it will be worth a question to LRE to see if my paper files and computer files are still available.  Maybe Erin kept all this stuff, she also spent a lot of time coding StateCU back then.  I made diagrams and used them when coding.  Also, on the GUI side, updates to the GUI - Wizard and Excel graphing features and etc - (PM was Erin and Kara) was also carefully documented and prototyped and demonstrated to the State. Some of that info hopefully is still around and could be useful._

* Main entry point (see: [`statecu.for` main program documentation](http://software.openwaterfoundation.org/cdss/statecu/13.10/doc-api/statecu_8for.html))
* Code organization:  include files, parameter data, etc.
* Input
* Initialization
	+ Global data
	+ Main program data
* Calculations
* Output
* Data checks
* Logging
* Clean-up
* Integration with StateCU GUI
>_[JHB] As we discussed the GUI is VB code, rewritten __ only where it had to be__ from VB6. In other words, a lot of the original VB6 code is still in there - but it compiled, so it was left alone.  It is not modern era VB.NET at all. Therefore it can't be compiled __except__ on the LRE computer given to DWR. Kelley T or Mary H has it._

>>_[JHB] When I started working on StateCU, we moved both the FORTRAN and VB code into the Visual Studio environment using Intel FORTRAN.  Later I separated the FORTRAN StateCU code from Visual Studio, and modified it to be amenable to other compilers like gfortran. At that time at LRE, we were not using a version control system, so complete sets of StateCU code for each version were kept in carefully named folders.  StateCU FORTRAN code was finally added to a version control system (git) much later, but StateCU code was changing infrequently by that time._

* [StateMod 13.10 code documentation](http://software.openwaterfoundation.org/cdss/statecu/13.10/doc-api/index.html)

## Fortran Conventions

StateCU has been in existence for many years and the code as evolved through different versions of the Fortran language.
The following is a summary of current conventions:

* Generally adheres to Fortran 77/?? conventions - does not utilize newer Fortran ?? features (need to explain).
>_[JHB] Generally, yes.  But I don't remember ever explicitly limiting changes I made to FORTRAN 77, it wasn't an objective.  Nor have I ever tried using compiler switches to test which FORTRAN version the code is actually compantible with.  A particular FORTRAN version was not an objective, but cross-compiler and cross-platform compatibility was.  In other words trying to write code that compiles cleanly with recent compilers - such as Intel Fortran and gfortran and sometimes Lahey - whether in Windows or Linux._ 
* List naming conventions
* Other conventions

## Data Passing

As per typical Fortran conventions, some data values are passed to subroutines and functions via parameters
and other data are shared between subroutines via common blocks.

Need to list common blocks and explain purpose.

Need to explain global/static data, dimensions, etc.
