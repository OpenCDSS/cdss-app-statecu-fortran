@echo off
rem
rem Copy the site/* contents to the CO DNR GCP bucket.
rem - replace all the files on the web with local files
rem - TODO smalers 2018-11-28 need to implement option to indicate if latest version

rem Set -n to test before actually doing
set dryrun=""
rem set dryrun="-n"
set gsFolderLatest="gs://static-cdss-state-co-us/statecu/latest/doc-api"

rem TODO smalers 2018-11-28 need to look up latest StateCU version
rem and also upload to that folder, similar to how Linux scripts work
rem - For now hard-code
set statecuVersion=13.11
set gsFolderVersion="gs://static-cdss-state-co-us/statecu/%statecuVersion%/doc-api"
set docFolder=doxygen-statecu-%statecuVersion%/html

rem Make sure that a Google Cloud SDK window is being used.
set pathWithoutGoogle=%PATH:google-cloud-sdk=%
if "%PATH%" == "%pathWithoutGoogle%" (
	rem Two strings are the same so no Google Cloud SDK in PATH
        echo.
        echo Must run from Google Cloud SDK Shell.
        echo.
        exit /b 1
)

rem Make sure that this batch file is being run from the doc-doxygen-project folder
rem Get current folder, see: https://superuser.com/questions/160702/get-current-folder-name-by-a-dos-command
for %%* in (.) do set curDirName=%%~nx*
if NOT "%curDirName%" == "doc-dev-doxygen-project" (
        echo.
        echo Must run from doc-dev-doxygen-project folder.
        echo.
        exit /b 1
)

set showHelp="no"
if "%1%" == "-h" set showHelp="yes"
if "%1%" == "/h" set showHelp="yes"
if %showHelp% == "yes" (
	echo .
	echo Usage:  copy-to-co-dnr-gcp
	echo .
	echo Copy the site files to the GCP static website folder:  %gsFolderLatest%
	echo Copy the site files to the GCP static website folder:  %gsFolderVersion%
	echo .
	exit /b 0
)

rem Make sure that the Doxygen output actually exists.
if not exist %docFolder%\NUL (
	echo .
	echo Doxygen output folder does not exist:  %docFolder%
	echo Run the main build-util/run-doxygen.sh script to create documentation.
	echo .
	exit /b 1
)

rem Now sync the local files up to Amazon S3
rem - apparently can't pass an empty argument so add %dryrun% only if testing
rem - the -m option causes operations to run in parallel, which can be much faster
rem - the -d option means delete extra files in destination
rem - the -r option means recursive to sync the whole folder tree
@echo on
gsutil -m rsync -d -r %docFolder% %gsFolderLatest%
rem gsutil -m rsync -d -r %docFolder% %gsFolderVersion%
@echo off

exit /b 0
