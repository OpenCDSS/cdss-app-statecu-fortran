#!/bin/sh
(set -o igncr) 2>/dev/null && set -o igncr; # This comment is required.
# The above line ensures that the script can be run on Cygwin/Linux even with Windows CRNL.
#
# git-tag-all-statecu - tag all StateCU repositories:
# - this script calls the general git utilities script

# Get the location where this script is located since it may have been run from any folder.
scriptFolder=$(cd $(dirname "$0") && pwd)

# Git utilities folder is relative to the user's files in a standard development files location:
# - determine based on location relative to the script folder
# Specific repository folder for this repository.
repoFolder=$(dirname ${scriptFolder})
# Want the parent folder to the specific Git repository folder.
gitReposFolder=$(dirname ${repoFolder})

# Main StateCU repository
mainRepo="cdss-app-statecu-fortran"
mainRepoFolder="${gitReposFolder}/${mainRepo}"

# Determine the version from the software product
# - code line looks like:
#     CHARACTER*16, PARAMETER :: VERS = '14.0.0'
# - this is used as information to help the user specify an intelligent tag name and commit message
# - grep -m 1 means stop after first occurrence
productVersion=$(cat ${mainRepoFolder}/src/main/fortran/gcommon.inc | grep -i 'PARAMETER' | grep -i 'VERS' | grep -v '!' | cut -d '=' -f 2 | tr -d ' ' | tr -d ')' | tr -d "'")
if [ -z "${productVersion}" ]; then
  echo "Could not determine StateCU version.  Exiting."
  exit 1
fi
productName="StateCU"

# Run the generic utility script.
${scriptFolder}/git-util/git-tag-all.sh -m "${mainRepo}" -g "${gitReposFolder}" -N "${productName}" -V "${productVersion}" $@
