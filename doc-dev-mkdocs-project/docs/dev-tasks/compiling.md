# Development Tasks / Compiling

StateCU code is compiled using a "makefile", which defines rules for detecting when a file needs to be recompiled,
based on code dependencies.
The code can be compiled from command line or from the Eclipse IDE.
The Eclipse IDE provides benefits during development but it may be necessary or useful to compile on the command line,
for example, if automating the build process.

This documentation contains the following sections:

* [Compile StateCU on Command Line](#compile-statecu-on-command-line)
* [Compile StateCU in Eclipse](#compile-statecu-in-eclipse)

## Compile StateCU on Command Line

### Linux

### Windows - MinGW

To compile StateCU on the command line, open a Windows command prompt window.
Then run the following batch file to configure the MinGW environment:

```
> C:\Users\user\cdss-dev\StateCU\git-repos\cdss-app-statecu-fortran\build-util\mingw\setup-mingw-env.bat`
```

Then change to the code location and run the makefile:

```
> C:\Users\user\cdss-dev\StateCU\git-repos\cdss-app-statecu-fortran\src\main\fortran
> make clean
> make statecu
```

The executable is created in the same folder.

## Compile StateCU in Eclipse

### Linux

### Windows - MinGW

To compile StateCU in Eclipse, start Eclipse with the run script:

```
> C:\Users\user\cdss-dev\StateCU\git-repos\cdss-app-statecu-fortran\build-util\eclipse\run-eclipse-statecu-mingw.bat`
```

Then right-click in the ***Project Explorer*** area and select ***Make / Targets***.  Then select ***Build...***.  Then select a target and press the ***Build*** button.

Review the output in the ***Console*** area to see if any errors occurred.
