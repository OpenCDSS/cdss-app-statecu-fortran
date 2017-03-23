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

>_[JHB] Rather than deal with DOS shell environment issues, I found myself always using the special mingw shell where everything (git, make, gfortran, etc.) just works like it does in linux bash shells.  The problem is that it puts your "home" in a deep dark path somewhere hard to find.  And path separators are the linux style.  Which brings up another issue...there are some Windows only and Linux only coding situations...which can be handled with custom make files, or code that you have to adjust internally or compiler level instructions.  None of these are happy solutions to me.  In statemod, I have linux only and windows only .for files, and let the make file handle it.  In other words, in windows, USE THE WINDOWS SPECIFIC MAKE FILE!_

>_[JHB] And I generally used the command line compile method, rather than the built in Eclipse features.  Since I have only been compiling StateMod in recent years, this is the example I am talking mostly about in my comments here._ 

To compile StateCU on the command line it is first necessary to configure the environment to run the compiler.
Open a Windows command prompt window and change to the folder where the setup script exists.
Then run the `setup-mingw-env.bat` batch file to configure the MinGW environment (note that setting up the environment in the window only needs
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

## Compile StateCU in Eclipse

### Linux

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
