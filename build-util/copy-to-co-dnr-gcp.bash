#!/bin/bash
#
# Create and copy the StateCU executable zip file installer to the CO DNR GCP website:
# - replace all the files on the web for the specific version with local files

# Supporting functions, alphabetized.

# Create a zip file containing the StateCU files:
# - the ${statecuVersion} must have been set
createZip() {
  local sevenZip

  if [ -z "${statecuVersion}" ]; then
    echoStderr "[ERROR] StateCU version is not set.  Cannot create zip file."
    return 1
  fi

  # Create a temporary folder to hold the files.
  echoStderr "[INFO] Building zip file in:"
  echoStderr "[INFO]   ${buildFolder}"
  if [ -d "${buildFolder}" ]; then
    # Remove it so that the contents will be current.
    rm -rf ${buildFolder}
    if [ -d "${buildFolder}" ]; then
      # Unsuccessful removing.
      echoStderr "[ERROR] Could not remove old build folder:"
      echoStderr "[ERROR]   ${buildFolder}"
      return 1
    fi
  fi
  # The folder was removed.  Recreate it.
  mkdir -p ${buildFolder2}
  # Copy files into the folder:
  # - rename the bash script to remove the extension 
  cp ${srcMainFolder}/statecu-${statecuVersion}-gfortran-win-64bit.exe ${buildFolder2}
  cp ${srcMainFolder}/statecu-${statecuVersion}-gfortran-win-64bit-check.exe ${buildFolder2}
  cp ${srcMainFolder}/statecu.cmd ${buildFolder2}
  cp ${srcMainFolder}/statecu.bash ${buildFolder2}/statecu
  # Zip the file using 7zip.
  sevenZip="/C/Program Files/7-Zip/7z.exe"
  if [ ! -f "${sevenZip}" ]; then
    # Major problem.
    echoStderr "[ERROR] Can't find 7-zip software to create zip file:"
    echoStderr "[ERROR]   ${sevenZip}"
    return 1
  fi
  cd ${buildFolder}
  # Zip the distribution folder that contains software files.
  "${sevenZip}" a -tzip ${zipFile} $(basename ${buildFolder2})
  if [ $? -ne 0 ]; then
    echoStderr "[ERROR] Error running 7-zip."
    return 1
  else
    echoStderr "[INFO] Created zip file:"
    echoStderr "[INFO]   ${zipFile}"
  fi

  return 0
}

# Echo a string to standard error (stderr).
# This is done so that output printed to stdout is not mixed with stderr.
echoStderr() {
  echo "$@" >&2
}

# Parse the command parameters:
# - use the getopt command line program so long options can be handled
parseCommandLine() {
  local optstring optstringLong
  local exitCode
  local GETOPT_OUT

  # Single character options.
  optstring="dhlv"
  # Long options.
  optstringLong="copy-to-latest,debug,dryrun,help,version"
  # Parse the options using getopt command.
  GETOPT_OUT=$(getopt --options ${optstring} --longoptions ${optstringLong} -- "$@")
  exitCode=$?
  if [ ${exitCode} -ne 0 ]; then
    # Error parsing the parameters such as unrecognized parameter.
    echoStderr ""
    printUsage
    exit 1
  fi
  # The following constructs the command by concatenating arguments.
  eval set -- "${GETOPT_OUT}"
  # Loop over the options.
  while true; do
    #echo "Command line option is ${opt}"
    case "${1}" in
      --debug) # --debug  Indicate to output debug messages.
        echoStderr "--debug detected - will print debug messages."
        debug="true"
        shift 1
        ;;
      -d|--dryrun) # -d or --dryrun  Indicate to do a dryrun but not actually upload.
        echoStderr "--dryrun detected - will not change files on GCP"
        dryrun="-n"
        shift 1
        ;;
      -h|--help) # -h or --help  Print the program usage.
        printUsage
        exit 0
        ;;
      --l,--copy-to-latest) # --l or --copy-to-latest  Also copy to "latest".
        echoStderr "--copy-to-latest detected."
        copyToLatest="yes"
        shift 1
        ;;
      -v|--version) # -v or --version  Print the program version.
        printVersion
        exit 0
        ;;
      --) # No more arguments.
        shift
        break
        ;;
      *) # Unknown option.
        echoStderr ""
        echoStderr "Invalid option: ${1}" >&2
        printUsage
        exit 1
        ;;
    esac
  done
}

# Print the usage.
printUsage() {
  echoStderr ""
  echoStderr "Usage: ${scriptName} [options]"
  echoStderr ""
  echoStderr "Copy the StateCU installer (zip file) to the versioned website folder:"
  echoStderr "  ${gsFolderVersion}"
  echoStderr "Optionally, copy the StateCU installer (zip file) to 'latest' website folder if -l specified:"
  echoStderr "  ${gsFolderLatest}"
  echoStderr ""
  echoStderr "--debug                Turn on debug for troubleshooting."
  echoStderr "-d, --dryrun           Dry run (print actions but don't execute upload)."
  echoStderr "-h, --help             Print usage."
  echoStderr "-l, --copy-to-latest   Copy to 'latest' folder in addition to auto-detected version folder."
  echoStderr "-v, --version          Print the program version."
  echoStderr ""
}

# Print the version.
printVersion() {
  echoStderr "${version}"
}

# Set the location of the 'gsutil' script:
# - Google provides a linux shell script and 'gsutil.cmd' is available for Windows
# - MinGW does not seem to inherit the full Windows PATH so find the script in typical location
# - set the global ${gsutilCommand} variable
setGsutilCommand() {
  local gsutilPath

  echoStderr "Check if 'gsutil' is in the PATH."
  gsutilCommand=$(which gsutil)
  if [ $? -eq 0 ]; then
    # Found it.
    return 0
  else
    # Look in the standard location.
    gsutilPath="/c/Users/${USER}/AppData/Local/Google/Cloud SDK/google-cloud-sdk/bin/gsutil"
    echoStderr "Check whether 'gsutil' is in the normal location:"
    echoStderr "  ${gsutilPath}"
    if [ ! -f "${gsutilPath}" ]; then
      # Error is printed in calling code.
      echoStderr "[ERROR] Unable to find 'gsutil' - check that it is installed."
      gsutilCommand=""
      return 1
    else
      gsutilCommand="${gsutilPath}"
      echoStderr "[INFO] Will use gsutil:"
      echoStderr "[INFO]   ${gsutilCommand}"
    fi
  fi
}

# Synchronize the installer to the cloud.
syncFiles() {
  # Copy the local files up to Google cloud:
  # - the -m option causes operations to run in parallel, which can be much faster
  # - the -d option means delete extra files in destination
  # - the -r option means recursive to sync the whole folder tree
  # TODO smalers 2021-09-02 actually, only need "latest" for documentation.
  #if [ ${copyToLatest} = "yes" ]; then
  #  gsutil.cmd cp ${dryrun} ${exeFileGfortran32} ${gsFileLatestZip}
  #fi
  # Upload to the versioned copy.
  echoStderr "Confirm copy:"
  echoStderr "  from: ${zipFile}"
  echoStderr "    to: ${gsFileVersionZip}"
  read -p "Continue with upload (Y/n/q)? " answer
  if [ -z "${answer}" -o "${answer}" = "y" -o "$answer" = "Y" ]; then
    "${gsutilCommand}" cp ${dryrun} ${zipFile} ${gsFileVersionZip}
    return $?
  elif [ "${answer}" = "q" -o "$answer" = "Q" ]; then
    exit 0
  else
    # Can still rebuild index.
    return 0
  fi
}

# Update the GCP index that lists files.
updateIndex() {
  local answer
  echo ""
  read -p "Do you want to update the GCP index file [Y/n]? " answer
  if [ -z "${answer}" -o "${answer}" = "y" -o "$answer" = "Y" ]; then
    ${scriptFolder}/create-gcp-statecu-index.bash
  fi
  return 0
}

# Entry point for the script.

# Get the location where this script is located since it may have been run from any folder.
scriptFolder=$(cd $(dirname "$0") && pwd)
scriptName=$(basename $0)
version="1.0.0 (2021-09-05)"

repoFolder=$(dirname "${scriptFolder}")
srcFolder="${repoFolder}/src"
srcMainFolder="${srcFolder}/main/fortran"

gcommonFile="${srcMainFolder}/gcommon.inc"
if [ -f "${gcommonFile}" ]; then
  # Code looks like:
  #   CHARACTER*16, PARAMETER :: VERS = '14.0.0'
  statecuVersion=$(cat ${gcommonFile} | grep -i 'PARAMETER' | grep -i 'VERS' | grep -v '!' | cut -d '=' -f 2 | tr -d ' ' | tr -d ')' | tr -d "'")
else
  echoStderr "[ERROR] Cannot determine StateCU version because file not found:"
  echoStderr "[ERROR]   ${gcommonFile}"
  exit 1
fi
if [ -z "${statecuVersion}" ]; then
  echoStderr "[ERROR] Cannot determine StateCu version from:"
  echoStderr "[ERROR]   ${gcommonFile}"
  exit 1
fi

buildFolder="/tmp/statecu-build"
buildFolder2="/tmp/statecu-build/statecu-cdss-${statecuVersion}"
zipFile="${buildFolder}/statecu-cdss-${statecuVersion}.zip"

#exeFileGfortran32="${srcMainFolder}/statecu-${statecuVersion}-gfortran-32bit.exe"
echoStderr "scriptFolder=${scriptFolder}"
echoStderr "repoFolder=${repoFolder}"
echoStderr "srcFolder=${srcFolder}"
echoStderr "srcMainFolder=${srcMainFolder}"
echoStderr "gcommonFile=${gcommonFile}"
echoStderr "statecuVersion=${statecuVersion}"
echoStderr "buildFolder=${buildFolder}"
echoStderr "buildFolder2=${buildFolder2}"
echoStderr "zipFile=${zipFile}"

# Initialize controlling data.
dryrun=""
# TODO smalers 2021-09-02 latest is only for documentation.
#gsFolderLatest="gs://opencdss.state.co.us/statecu/latest/software"
#gsFileLatestZip="${gsFolderLatest}/statecu-cdss-${statecuVersion}.zip"
gsFolderVersion="gs://opencdss.state.co.us/statecu/${statecuVersion}/software"
gsFileVersionZip="${gsFolderVersion}/statecu-cdss-${statecuVersion}.zip"

# Whether to copy to latest in addition to the specific version:
# - default to no because the script can be run on any version, and can't assume latest
copyToLatest="no"

# Parse the command line.
parseCommandLine $@

# Set the location of gsutil command:
# - MinGW uses the linux script rather than Windows gsutil.cmd
setGsutilCommand
if [ $? -ne 0 -o -z "${gsutilCommand}" ]; then
  echoStderr "[ERROR] Unable to find 'gsutil'.  Exiting."
  exit 1
fi

# Create zip file:
# - this is needed because unlike TSTool, the build process does not currently create the installer
createZip
exitStatus=$?

# Sync the files to the cloud.
if [ ${exitStatus} -eq 0 ]; then
  syncFiles
  exitStatus=$?
fi

# Also update the index.
if [ ${exitStatus} -eq 0 ]; then
  updateIndex
  exitStatus=$?
fi

# Exist with the status from the most recent call above.
exit ${exitStatus}
