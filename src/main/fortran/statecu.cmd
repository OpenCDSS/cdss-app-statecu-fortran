@echo off
rem
rem Run StateCU for the dataset using the latest StateCU executable that is found,
rem based on sorting the executable name.
rem This scripts provides a standard command so that users don't need to type the verbose
rem executable names.
rem
rem version 1.0.0 2021-09-05

rem Turn on delayed expansion so that loops work:
rem - See:  https://ss64.com/nt/delayedexpansion.html
rem - Otherwise, for loops and ( ) blocks don't work because variables are not set as expected
rem - Such variables must be surrounded by ! !, rather than % %
setlocal EnableDelayedExpansion

rem Save the folder where the script is run so can return after running.
set currentFolder=%CD%

rem Determine the folder that the script exists in.
rem This should typically be the 'StateCU' folder of a dataset.
set scriptFolder=%~dp0
rem Remove trailing \ from scriptFolder.
set scriptFolder=%scriptFolder:~0,-1%

rem Change to the script folder, which should be the dataset folder.
rem This is done just in case the script is run by typing a path to the script.
echo.
echo Changing to:  %scriptFolder%
cd %scriptFolder%

rem List executables:
rem - alphabetical, not as smart as sorting by version parts, but OK in most cases
rem - newest first
rem - see the following recommendation:  https://stackoverflow.com/questions/13601015/how-can-i-get-the-first-line-of-the-output-for-given-command-in-dos
rem - see the following documentation for 'for':  https://ss64.com/nt/for_cmd.html
rem - TODO smalers 2021-09-04 tried to redirect error but did not work:  'dir /b /o:-n statecu*.exe 2>nul'
rem - if no executables a warning will be shown and will be handled below
echo Determining StateCU executable to run using: dir /b /o:-n statecu*.exe
for /F "delims=" %%A in ('dir /b /o:-n statecu*.exe') do (
  rem After setting the first value, break out of the loop with a goto.
  rem Could also list alphabetically and reset for all items and then put goto outside of the loop.
  set statecuExe=%%A
  goto runStatecu
)
rem Will get to here if 'dir' command found no files.
rem Check for existence of the response file below.
goto runStatecu

rem Should never get here?
goto exit0

rem ========================================================================
rem Below here are 'goto' labels, in alphabetical order.
rem ========================================================================

:exit1
rem Exit the program with status 1.
rem Change back to original folder before exiting.
rem This is not a function.  Do not "call".  Use goto.
echo.
echo Changing back to starting folder:  %currentFolder%
cd %currentFolder%
rem Exit with success code
echo Exiting with status 1
exit /b 1
rem End of :exit1

:exit0
rem Exit the program with status 0.
rem Change back to original folder before exiting.
rem This is not a function.  Do not "call".  Use goto.
echo.
echo Changing back to starting folder:  %currentFolder%
cd %currentFolder%
rem Exit with success code
echo Exiting with status 0
exit /b 0
rem End of :exit0

:nostatecuexe
rem Print an error when no executable found:
rem - could happen if executable was not packaged with the dataset
echo.
echo No StateCU executables found.
echo Exiting without running StateCU.
echo.
goto exit1
rem End of :nostatecuexe

:notexiststatecuexe
rem Print an error if the executable does not exist:
rem - an executable was found but the file does not exist
rem - should not happen but might help with logic problems
echo.
echo StateCU executable does not exist:  %statecuExe%
echo Exiting without running StateCU.
echo.
goto exit1
rem End of :noexiststatecuexe

:runStatecu
rem Function to run the found StateCU executable:
rem - first program parameter is the simulate option
rem - second program parameter is the response file name
rem   (*.rcu, file only, no path, with or without extension)
rem - it is assumed the current folder is the StateCU dataset folder
if [%statecuExe%] == [] goto nostatecuexe
if not exist "%statecuExe%" goto notexiststatecuexe
set rspFile=%1%
set runArg=%2%
set runArg2=%3%
echo Running: %statecuExe% %rspFile% %runArg% %runArg2%
time /t
%statecuExe% %rspFile% %runArg% %runArg2%
set errorLevel2=%ERRORLEVEL%
time /t
echo Finished running: %statecuExe% %rspFile% %runArg% %runArg2%
if not %errorLevel2%==0 goto statecuerror
rem If here, StateCU exited with status 0, which is success.
echo Success running StateCU.
echo.
exit /b %errorLevel2%
rem End of :runStatecu

:statecuerror
echo Error running StateCU, exit status: %errorLevel2%
echo.
exit /b %errorLevel2%
rem End of :statecuerror
