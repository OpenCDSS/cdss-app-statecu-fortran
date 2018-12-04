# Development Tasks / Compiling

StateCU code is compiled using a "makefile", which defines rules for detecting when a file needs to be recompiled,
based on code dependencies.
The code can be compiled from command line or from the Eclipse IDE.
The Eclipse IDE provides benefits during development such as debugging and code completion
but does require more training/learning to use.

This documentation contains the following sections:

* [Compile StateCU on Command Line](#compile-statecu-on-command-line)
* [Compile StateCU in Eclipse](#compile-statecu-in-eclipse)

-----------------

## Compile StateCU on Command Line

Compiling on the command line uses the `make` command and `makefile`.

### Linux

Compiling on Linux is similar to Windows.  Use the `make` command targets.

### Windows - MinGW

To compile StateCU on the command line it is first necessary to configure the environment to run the compiler.
Open a Windows command prompt window and change to the folder where the setup script exists.
Then run the `build-util/mingw/setup-mingw-env.bat` batch file to configure the MinGW environment (note that setting up the environment in the window only needs
to be done once after the window is opened).

```
> C:
> cd \Users\user\cdss-dev\StateCU\git-repos\cdss-app-statecu-fortran\build-util\mingw
> setup-mingw-env.bat
```

Then change to the code location and run the makefile:

```
> C:
> cd \Users\user\cdss-dev\StateCU\git-repos\cdss-app-statecu-fortran\src\main\fortran
> make clean
> make statecu
```

The executable `statecu.exe` is created in the same folder and can be run with model input (in a test folder separate from the code).

Use the `make help` command to list available `makefile` targets.

## Compile StateCU in Eclipse

StateCU can also be compiled using Eclipse, which relies on the `make` command and `makefile`.

### Linux

This documentation will be completed when resources are available for Linux development and testing.

### Windows - MinGW

To compile StateCU in Eclipse, start Eclipse with the run script `run-eclipse-statecu-mingw.bat` as shown below.
This script automatically runs the MinGW setup script described in the previous section,
which will configure the compiler environment if necessary.


```
> C:
> cd \Users\user\cdss-dev\StateCU\git-repos\cdss-app-statecu-fortran\build-util\eclipse
> run-eclipse-statecu-mingw.bat
```

Then right-click in the ***Project Explorer*** area and select ***Make / Targets***.  Then select ***Build...***.  Then select a target and press the ***Build*** button.

Review the output in the ***Console*** area to see if any errors occurred.
