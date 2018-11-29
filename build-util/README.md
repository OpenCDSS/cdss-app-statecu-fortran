# build-util

This folder contains utility scripts (or batch files if Windows) that are used in the build process:

* `copy-to-co-dnr-gcp.sh` - copy the StateCU executable to Google Cloud platform, for public access
* `eclipse/`
	+ `run-eclipse-statecu-mingw.bat` - batch file to start correct version of Eclipse with proper Java version
	(use if Eclipse/Photran environment is used for development rather than text editor and command line compiler)
* `git-check-statecu.sh` - script to indicate status of all StateCU repositories
* `git-util/` - generalized Git utility scripts
* `mingw/' - scripts used with MINGW environment
	+ `setup-mingw-env.bat` - batch file to configure Windows command shell environment for compiling with MINGW `gfortran`
* `product-repo-list.txt` - repository list used by Git utility scripts
* `run-doxygen.sh` - run Doxygen to autogenerate code documentation in `doc-dev-doxygen-project` folder
* `search-for-edits.sh` - used to determine historical code authors
