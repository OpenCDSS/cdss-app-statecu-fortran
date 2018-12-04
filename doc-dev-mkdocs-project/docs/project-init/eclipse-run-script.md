# Initial Project Setup / Eclipse Run Script

It is helpful to run Eclipse with a script to ensure that proper versions of Eclipse and Java are used.

The following sections are included in this documentation:

* [Prerequisites](#prerequisites)
* [Default Eclipse Run Script](#default-eclipse-run-script) - run script provided in repository for standard project configuration
	+ [Linux](#linux)
	+ [Windows](#windows)
* [Developer-specific Run Script](#developer-specific-run-script) - modify the default for a specific user
* [Next Step](#next-step)

----------------

## Prerequisites

This step requites that Eclipse/Photran was previously installed as per the [Developer Environment / Eclipse and Photran](../dev-env/eclipse/) documentation.

## Default Eclipse Run Script

### Linux

A run script for Linux Eclipse has not yet been created.

### Windows

The following batch file was created in the repository to run Eclipse and is suitable if the project
is configured as per this documentation.
It is recommended that developers use the script if possible to run from a Windows command shell that has
been initialized with the proper MinGW environment variables.
Additional scripts can be created as Eclipse is updated in the future.

```text
C:\Users\user\cdss-dev\StateCU\git-repos\cdss-app-statecu-fortran\build-util\eclipse\run-eclipse-statecu.bat
```

See the [script in the repository](https://github.com/OpenCDSS/cdss-app-statecu-fortran/blob/master/build-util/eclipse/run-eclipse-statecu-mingw.bat).

Note that the command line parameters passed to Eclipse are an alternative to changing the `eclipse.ini` file 
distributed with the Eclipse software.
If the above is not suitable, it is recommended that the above script is copied as a developer-specific version as described in the next section.

## Developer-specific Run Script

If the above script is not appropriate for the specific developer, make a copy and modify.
For example, save in a `cdss-dev\StateCU\build-util` folder.

## Next Step

Use the run script to start Eclipse.  The next step is to set up an initial Eclipse workspace.
