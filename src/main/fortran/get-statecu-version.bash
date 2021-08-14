#!/bin/bash

# Script to determine the StateCU version:
# - this can be used on the command line if necessary, such as by other scripts
# - the same code is embedded in the makefile

# Determine the StateCU version:
# - used in the executable name
# - also used for installer upload folder, etc.
# - must handle version syntax that has changed over time
# - through version 13, code used a floating point version that could only handle two parts:
#      PARAMETER (VERS = 13.12)
# - as of version 14, uses a character string that can handle semantic versioning:
#      PARAMETER (VERS = '14.0.0')
# - "dev" at the end of the version (e.g., 14.0.0.dev) indicates the software is under development

# Get the location of this script so that paths can be absolute and avoid file location issues:
# - this script lives in the same folder as the source code
scriptFolder=$(cd $(dirname "$0") && pwd)

codeFile="${scriptFolder}/gcommon.inc"

# Explanation of the following command:
# - (grep) search for lines with 'PARAMETER', ignoring case
# - (grep) filter to lines with 'VERS', ignoring case
# - (grep) ignore lines with ! since comment
# - (cut) cut out the part after (
# - (cut) cut out the part after the =
# - (tr) remove spaces
# - (tr) remove )
# - (tr) remove '
cat ${codeFile} | grep -i 'PARAMETER' | grep -i 'VERS' | grep -v '!' | cut -d '(' -f 2 | cut -d '=' -f 2 | tr -d ' ' | tr -d ')' | tr -d "'"
