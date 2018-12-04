# Initial Project Setup / Build Utility Scripts

This documentation references a number of build utility scripts.
These scripts are saved in the repository to help developers be efficient,
assuming that they have set up the development environment as per this documentation.

This documentation includes the following sections:

* [Build Utility Script Location](#build-utility-script-location)
* [Script to Configure MinGW Environment - `setup-mingw-env`](#script-to-configure-mingw-environment-setup-mingw-env)
* [Script to Run Eclipse - `run-eclipse-statecu`](#script-to-run-eclipse-run-eclipse-statecu-mingw)

-------------------

## Build Utility Script Location

The build utilities are located in the `build-util` folder in the repository, for example:

For Linux:  `~/cdss-dev/StateCU/git-repos/cdss-app-statecu-fortran/build-util`.

For Windows:  `C:\Users\user\cdss-dev\StateCU\git-repos\cdss-app-statecu-fortran\build-util`.

## Script to Configure MinGW Environment - `setup-mingw-env`

Use this script to configure the MinGW environment so that compilers can be found in the command shell and in eclipse.
This script is called by other scripts that use the MinGW environment, such as `run-eclipse-statecu-mingw.bat`.

This script is described first in [Develompent Environment / Machine](dev-env/machine/) and is located in `build-util/mingw`.

## Script to Run Eclipse - `run-eclipse-statecu-mingw`

Use this script to start Eclipse in the MinGW environment.
This script calls `setup-mingw-env` before calling Eclipse.

This script is described first in [Initial Project Setup / Eclipse Run Script](eclipse-run-script/) and is located in `build-util/eclipse`.
