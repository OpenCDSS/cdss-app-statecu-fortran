# New Developer Setup / Overview

This documentation is for software developers that are members of the core StateCU team and others who
have an interest in contributing to the StateCU software project.
It is recommended that the StateCU development environment should follow these setup instructions, which are
consistent with the [Development Environment](../dev-env/overview/) and [Initial Project Setup](../project-init/overview/) documentation.
The [standard development folder structure](../project-init/overview#development-folder-structure) should be followed to minimize potential for issues,
especially given the number of components and setup steps.
All of this documentation is consistent with the recommended development environment.

The intent of this documentation is to completely document setup steps and allow new developers to comment on this documentation
so that it can be improved for other developers. The following steps need to occur to set up a new developer's environment.
Links to other documentation are included to provide more information and "see details below" is used to indicate that
specific instructions are included below (rather than linking to other pages from the following outline).
After reading instructions for a step, use "back" to return to this outline so that setup is done in the proper sequence.

1. [Create folder for development files](#create-folder-for-development-files) - where development will occur (**see details below**), **required**
2. Development Environment software install part 1 (version control)
	* [Development Environment / Git](../dev-env/git/) - install Git software so the repository can be cloned, **required, if not already installed**
3. [Clone Git Repository](#clone-git-repository) - clone the repository to get access to all files (**see details below**), **required**
4. Development Environment software install part 2 (documentation tools), **optional, install if will edit and view documentation**
	* [Development Environment / Python and pip](../dev-env/python/) - install Python, which is needed by MkDocs
	* [Development Environment / MkDocs](../dev-env/mkdocs/) - install MkDocs to view/edit full documentation locally.
	See [Development Tasks / Documenting](../dev-tasks/documenting#developer-documentation-using-mkdocs)
	for instructions on viewing documentation.
5. Development Environment software install part 3 (Fortran development tools)
	* [Development Environment / Machine](../dev-env/machine/) - configure machine for development, **required, to provide environment**
	* [Development Environment / Java 8](../dev-env/java8/) - make sure Java 8 is available on system, **optional, not needed if command line compile is used**
	* [Development Environment / gfortran](../dev-env/gfortran/) - install `gfortran` Fortran compiler, **required**
	* [Development Environment / Eclipse and Photran](../dev-env/eclipse/) - install Eclipse for use as IDE, **optional, not needed if command line compile is used**
	* [Development Environment / Doxygen](../dev-env/doxygen/) - install Doxygen to auto-generate code API documentation,
	**optional, not needed if code documentation map is not created**
	* [Development Environment / pytest](../dev-env/pytest/) - install to facilitate automated testing (being evaluated), **optional, still experimental**
	* [Development Environment / KDiff3](../dev-env/kdiff3/) - install software to facilitate comparing files, **optional, useful for file comparison**
6. Eclipse Workspace Setup (interactive development environment), **optional, only do if Eclipse/Photran was installed**
	* [Create Eclipse Workspace Folder](#create-eclipse-workspace-folder) - simple manual step (***see details below***)
	* [Import the Existing Eclipse StateCU Project from the Git Repository Folder](#import-the-existing-eclipse-statecu-project-from-the-git-repository-folder) - import
	from Git repository working files (**see details below**)
7. [Next Steps - Development Tasks](#next-steps-development-tasks) - be productive!

The following sections are referenced from the above outline.

## Create Folder for Development Files

Create a development home folder consistent with the [initial project setup](../project-init/home-folder/) - this
is an umbrella folder for all StateCU development files,
including software that is installed locally (as appropriate).
It is assumed that development will occur in a developer's home folder on the computer in order to provide separation from the
work of other developers on the computer.
Tools such as Git rely on a unique identity for developers in order to properly track edits to files.
After the folder is created, additional instructions will describe how to install development files into the folder.

### Linux

Do the following using a terminal window. Note that the syntax `~` indicates the home folder and is equivalent to the `$HOME` environment
variable location.

```bash
$ cd
$ mkdir cdss-dev
$ cd ~/cdss-dev/
$ mkdir StateCU
```

### Windows

Do the following in a Windows command shell, or perform the equivalent actions in file explorer.

```com
> C:
> cd \Users\userName
> mkdir cdss-dev
> cd cdss-dev
> mkdir StateCU
```

*Press back in the browser to return to the outline.*

## Clone Git Repository

The [cdss-app-statecu-fortran Git repository hosted on GitHub](https://github.com/OpenWaterFoundation/cdss-app-statecu-fortran)
contains the StateCU software and configuration files.
It also contains the most recent version of this documentation.
The repository will be imported into the Eclipse/Photran workspace as a Fortran project.

### Clone the repository files (Linux)

```bash
$ cd ~/cdss-dev/StateCU
$ mkdir git-repos
$ cd git-repos
> git clone https://github.com/OpenWaterFoundation/cdss-app-statecu-fortran.git
```

### Clone the repository files (Windows)

```com
> C:
> cd \Users\user\cdss-dev\StateCU
> mkdir git-repos
> cd git-repos
> git clone https://github.com/OpenWaterFoundation/cdss-app-statecu-fortran.git
```

If prompted, specify the GitHub account credentials.
The repository will include the Fortran StateCU project.

The resulting files should match the [Development Files Structure](../project-init/overview#development-folder-structure).

*Press back in the browser to return to the outline.*

## Create Eclipse Workspace Folder

This step is the same as the [Initial Project Setup](../project-init/eclipse-workspace/) so follow those instructions with the following input:

* Linux:  the workspace folder is `~/cdss-dev/StateCU/eclipse-workspace`
* Windows: the workspace folder is `C:\Users\user\cdss-dev\StateCU\eclipse-workspace`

Start Eclipse by running the [Eclipse run script](../project-init/eclipse-run-script) as shown below.
This script can be used any time to run Eclipse for this project.
If it is necessary to modify this script,
[see recommendations for a developer-specific run script](../project-init/eclipse-run-script#developer-specific-run-script).

Open the workspace in Eclipse in preparation of adding the code project from the Git repository in the next step.

### Linux

```bash
$ cd ~/cdss-dev/StateCU/git-repos/cdss-app-statecu-fortran/build-util/eclipse
$ ./run-eclipse-statecu.sh
```

### Windows

```bash
> C:
> cd \Users\user\cdss-dev\StateCU\git-repos\cdss-app-statecu-fortran\build-util\eclipse
> .\run-eclipse-statecu-mingw.bat
```

*Press back in the browser to return to the outline.*

## Import the Existing Eclipse StateCU Project from the Git Repository Folder

The [Initial Project Setup](../project-init/overview/) process performed by the software team leads
did extensive work to set up the Eclipse project 
and these files were saved to the Git repository as an Eclipse/Photran Fortran project.

New developers need to import the project into the empty workspace as follows.

Use ***File / Import*** and then follow the sequence below.

![Import project](overview-images/eclipse-import-project-1.png)

The first step allows browsing to an existing project (the user name will be different).

![Import project](overview-images/eclipse-import-project-2.png)

Press ***Finish*** in the initial import dialog.

The resulting Eclipse workspace is shown as below.
Note that Eclipse recognizes the associated Git repository indicated by the repository name/branch next to the Eclipse project.

![Import project](overview-images/eclipse-import-project-3.png)

## Next Steps - Development Tasks

At this point it should be possible to [compile and run StateCU within the Eclipse interface](../dev-tasks/compiling).
See also:

* [Deployed Environment / Overview](../deployed-env/overview/) - for an overview of the deployed software
* [Software Design / Overview](../software-design/overview/) - to understand software structure and logic
* [Development Tasks / Overview](../dev-tasks/overview/) - common development tasks
