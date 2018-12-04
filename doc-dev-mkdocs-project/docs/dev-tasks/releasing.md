# Development Tasks / Releasing

Releasing the software consists of compiling StateCU into an executable and packaging into an installer that can be distributed.

**Need to coordinate with WWG on how StateCU should be released... with the GUI, separate, etc.
Also need to evaluate whether to release from the Git Project page, GCP website and/or CDSS website.**

This documentation contains the following sections:

* [Build Checklist](#build-checklist)
* [Creating StateCU Installer](#creating-statecu-installer)
* [Releasing StateCU](#releasing-statecu)

--------------------

## Build Checklist

Currently there is not a need for StateCU to be built in a continuous integration process because the development team is small.
Consequently the build process is done by the primary developer(s).

The full build process checklist is as follows:

1. The software version currently has 2 parts such as 13.11.
The version should be incremented accordingly as public releases are made.
**This needs to be expanded to Major.Minor.BugFix version convention.**
2. Issues in the GitHub repository should be coordinated to decide when a public versioned release should occur.
Normally a release will be made to fix bugs only and/or to introduce one or more new features.
3. Recompile the program:
	1. Run `make clean`.
	2. Run `make statecu`.
4. Run tests to confirm program accuracy.
5. Update the documentation, in particular user documentation and release notes.
6. Create the installer.  Currently there is no installer for the model executable and the StateCU GUI software is out of date
and needs to be updated.  The [Creating StateCU Installer](#creating-statecu-installer)
section below discusses the installer in more detail.
7. Publish the new version.  The compiled executable can be distributed.  See the [Releasing StateCU](#releasing-statecu) section below.

## Creating StateCU Installer ##

**Include here details of creating the installer and bundling with the GUI when the GUI is updated.**

## Releasing StateCU

As indicated above, there is currently no installer. Therefore the executable can be distributed without additional packaging.
To do so, run the `build-util/copy-to-co-dnr-gcp.sh` script from Git Bash.
This will copy the latest compiled executable to the State's Google Cloud Platform storage location,
for the detected and latest version.  Run the script with `-h` to see usage.
