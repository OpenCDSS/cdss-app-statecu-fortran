# Software Design / Overview

**This is a very rough outline and needs work to educate developers about the StateCU code organization.**

The StateCU Fortran code is divided into separate files with .for extension.
Each subroutine is in a separate file and the name of the file matches the subroutine (is this globally true? a grep on code seemed to indicate so).

Need to describe code modules at a high level:

* Main entry point (see: [`statecu.for` main program documentation](https://github.com/OpenCDSS/cdss-app-statecu-fortran/blob/master/src/main/fortran/statecu.for).
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

-------------

## StateCU Code API

The StateCU code has been processed with Doxygen software to produce HTML documentation.
Additional code formatting can occur to enable more complete documentation.
See the [Development Tasks / Documenation section](../dev-tasks/documenting)
for information on using Doxygen to generate code documentation.

## Fortran Conventions

StateCU has been in existence for many years and the code as evolved through different versions of the Fortran language.
The following is a summary of current conventions:

* Generally adheres to Fortran 77/?? conventions - does not utilize newer Fortran ?? features (need to explain).
* List naming conventions
* Other conventions

## Data Passing

As per typical Fortran conventions, some data values are passed to subroutines and functions via parameters
and other data are shared between subroutines via common blocks.

Need to list common blocks and explain purpose.

Need to explain global/static data, dimensions, etc.
