#!/bin/sh
#
# Copy the executable to the CO DNR GCP website
# - replace all the files on the web with local files

# Supporting functions

# Print the usage
printUsage() {
	echo ""
	echo "Usage:  $0"
	echo ""
	echo "Copy the StateCu executable file to the latest website folder if -l specified:  $gsFolderLatest"
	echo "Copy the StateCu executable file to the versioned website folder:  $gsFolderVersion"
	echo ""
	echo "-d dry run (print actions but don't execute upload)"
	echo "-h print usage"
	echo "-l copy to latest folder in addition to auto-detected version folder"
	echo ""
}

# Entry point for the script

# Get the location where this script is located since it may have been run from any folder
scriptFolder=`cd $(dirname "$0") && pwd`
repoFolder=$(dirname "$scriptFolder")
srcFolder="$repoFolder/src"
srcMainFolder="${srcFolder}/main/fortran"
gcommonFile="${srcMainFolder}/gcommon.inc"
if [ -f "${gcommonFile}" ]; then
	statecuVersion=$(cat ${gcommonFile} | grep 'PARAMETER' | grep 'VERS' | cut -d '(' -f 2 | cut -d '=' -f 2 | tr -d ' ' | tr -d ')')
else
	echo "Cannot determine StateCU version because file not found:  ${gcommonFile}"
	exit 1
fi
exeFile="${srcMainFolder}/statecu-${statecuVersion}.exe"
echo "scriptFolder=$scriptFolder"
echo "repoFolder=$repoFolder"
echo "srcFolder=$srcFolder"
echo "srcMainFolder=$srcMainFolder"
echo "gcommonFile=$gcommonFile"
echo "statecuVersion=$statecuVersion"
echo "exeFile=$exeFile"

dryrun=""
gsFolderLatest="gs://static-cdss-state-co-us/statecu/latest/software"
gsFileLatest="$gsFolderLatest/statecu-${statecuVersion}.exe"
gsFolderVersion="gs://static-cdss-state-co-us/statecu/${statecuVersion}/software"
gsFileVersion="$gsFolderVersion/statecu-${statecuVersion}.exe"

# Whether to copy to latest in addition to the specific version
# - default to no because the script can be run on any version, and can't assume latest
copyToLatest="no"

# Parse the command parameters
while getopts :dhl opt; do
	#echo "Command line option is ${opt}"
	case $opt in
		d) # Indicate that this should be copied to the latest release and version
			dryrun="-n"
			;;
		h) # Usage
			printUsage
			exit 0
			;;
		l) # Indicate that this should be copied to the latest release and version
			copyToLatest="yes"
			;;
		\?)
			echo "Invalid option:  -$OPTARG" >&2
			exit 1
			;;
		:)
			echo "Option -$OPTARG requires an argument" >&2
			exit 1
			;;
	esac
done

# Make sure that this is being run from the build-util folder
pwd=`pwd`
dirname=`basename ${pwd}`
if [ ! ${dirname} = "build-util" ]; then
	echo "Must run from build-util folder"
	exit 1
fi

# Copy the local files up to Google Cloud
# - the -m option causes operations to run in parallel, which can be much faster
# - the -d option means delete extra files in destination
# - the -r option means recursive to sync the whole folder tree
if [ ${copyToLatest} = "yes" ]; then
	gsutil.cmd cp ${dryrun} $exeFile ${gsFileLatest}
fi
# For now always upload to the versioned copy
gsutil.cmd cp ${dryrun} $exeFile ${gsFileVersion}

exit $?
