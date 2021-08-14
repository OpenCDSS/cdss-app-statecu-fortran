# src/main/fortran

This folder contains the StateCU Fortran source code.

## Folder Contents 

The following table provides additional information.

| **File** | **Description** |
| -- | -- |
| `*.for` | Fortran source files. |
| `*.inc` | Include files, used to share code such as parameter variables and common blocks with multiple code files. |
| `*.o` | Object files produced by the compiler, ignored from the repository. |
| `count-warnings.bash` | Used to count compiler warnings of different type, used to clean up code for warnings during migration to the `gfortran` 10 compiler. |
| `gcommon.inc` | Common block include file that includes the program version. |
| `get-statecu-version.bash` | Simple script to print the StateCU version to stdout, useful for scripting. |
| `statecu-13.11-gfortran-win-64bit-check.exe` (and similar) | 64-bit windows executable created using compiler options that perform more checks.  The executables are larger and slower. |
| `statecu-13.11-gfortran-win-64bit-o3.exe` (and similar) | 64-bit windows executable created using compiler options for optimized executable.  The executable have fewer checks and are smaller and faster. |
| `statecu-13.11-gfortran-win-64bit.exe` (and similar) | Copy of the `-o3` executable, for distribution and use in production work. |
| Similar to above with `32bit` | 32-bit executables, should not be needed. |
| `statecu.for` | Main program entry point. |

## Compiling StateCU

The MinGW Fortran development environment is used to compile StateCU.
See the main README file for information about the development environment.

Use `make help` to see the available makefile targets.

During development, the `make statecu_prog` target is typically used.

The `make statecu_release` target can be used to do a full build for check and optimized executables.

The following are important considerations related to the `makefile` and executable file names.

1. **Operating System**
	1. The makefile is designed to work for the `gfortran` compiler running on Windows and Linux.
	2. Testing has primarily focused on Windows and additional cleanup may be needed on Linux.
	3. To avoid confusion, the executable name for Windows includes `win` and `linux` for Linux.
	Note that compiling the program in a linux-like environment on Windows (MSys MinGW, Cygwin, etc.)
	creates output with `win` since the executables run on Windows.
2. **Compiler** 
	1. The `makefile` is primarily configured for `gfortran` compiler but could be updated for use with other compilers.
	2. A separate compile process is used for Lahey compiler, which was used for development in the past.
	3. To avoid confusion, the executable name includes `gfortran`.
2. **Software Version**
	1. The `makefile` automatically determines the program version from `gcommon.inc`.
	2. The version controls filenames produced by the build process,
	software packaging, and uploads to the cloud.
	3. The `get-statecu-version.bash` script can be used to determine the version.
3. **Chip Archtecture**
	1. The `makefile` automatically compiles the appropriate 64-bit or 32-bit executables.
	2. Normally, only 64-bit executables are of interest.
	The ability to create 32-bit executable is retained for comparison and transition from 32-bit to 64-bit.
	3. Use the appropriate MSys2 MinGW window to select 64-bit or 32-bit development environment.
	4. The `makefile` includes checks to avoid cross-linking 64-bit and 32-bit object files.
	5. To avoid confusion, the executable name includes `32bit` or `64bit`.
4. **Runtime Variants**
	1. In addition to the software versions, two "variant" executables are created.
	2. The `check` variant uses compiler options to include more run-time checks.
	This variant can be run, if necessary, to troubleshoot a problem.
	It should be used for final testing prior to a software release.
	3. The `o3` variant uses compiler options to produce an optimized executable,
	which is smaller and faster.
	This executable should be used in production and should also be used
	in automated testing prior to a software release.
	5. To avoid confusion, the executable name includes `-o3` or `-check`.
	However, a copy of the `o3` variant is copied to a filename without this designation, for distribution.
	6. Experience has shown that doing development with the check version is slow,
	in particular running automated tests.
	Therefore, the default `makefile` targets compile the optimized version in working files.
