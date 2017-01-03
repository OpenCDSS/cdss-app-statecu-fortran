# New Developer Setup / Overview

A new developer that will contribute to StateCU must configure a development environment
consistent with the [Development Environment](../dev-env/overview/) and [Initial Project Setup](../project-init/overview/) documentation.
The [standard development folder structure](../project-init/overview#development-folder-structure) should be followed to minimize potential for issues,
especially given the number of components and setup steps.

The intent of this documentation is to completely document setup steps and allow new developers to comment on this documentation
so that it can be improved for other developers. The following steps need to occur to set up a new developer's eenvironment.
Links to other documentation or are included to provide more information and "see details below" is used to indicate that
specific instructions are included below (rather than simply linking to other pages).

1. [Create folder for development files](#create-folder-for-development-files) - where development will occur (**see details below**)
2. Development Environment software install part 1
	+ [Development Environment / Git](../dev-env/git/) - install Git software so can clone the repository
	+ [Development Environment / Python and pip](../dev-env/python/) - install Python, which is needed by MkDocs
	+ [Development Environment / pytest](../dev-env/pytest/) - install to facilitate automated testing
	+ [Development Environment / MkDocs](../dev-env/mkdocs/) - install MkDocs so can view/edit full documentation locally
3. [Clone Git Repository](#clone-git-repository) - clone the repository to get access to all files (**see details below**)
4. [View Developer Documentation with MkDocs](#view-developer-documentation-with-mkdocs) - view full developer documentation locally (**see details below**)
5. Development Environment software install part 2
	+ [Development Environment / Machine](../dev-env/machine/) - configure machine for development
	+ [Development Environment / Java 8](../dev-env/java8/) - make sure Java 8 is available on system
	+ [Development Environment / gfortran](../dev-env/gfortran/) - install `gfortran` Fortran compiler
	+ [Development Environment / Eclipse and Photran](../dev-env/eclipse/) - install Eclipse for use as IDE
	+ [Development Environment / Doxygen](../dev-env/doxygen/) - install Doxygen to auto-generate code API documentation
	+ [Development Environment / KDiff3](../dev-env/kdiff3/) - install software to facilitate comparing files
6. Eclipse Workspace Setup
	+ [Create Eclipse Workspace Folder](#create-eclipse-workspace-folder) - simple manual step (***see details below***)
	+ [Import the Existing Eclipse/Maven Project from the Git Repository Folder](#import-the-existing-eclipsemaven-project-from-the-git-repository-folder) - import
	from Git repository working files (**see details below**)
	+ [Configure Java Compiler for Java 8](#configure-java-compiler-for-java-8) - configure Java settings (***see details below***)
7. [Next Steps - Development Tasks](#next-steps-development-tasks) - be productive!

The following sections are new content that does not exist anywhere else and is linked to from above.

## Create Folder for Development Files

[Create a development home folder consistent with the initial project setup](../project-init/home-folder/) - this
is an umbrella folder for all StateCU development files,
including software that is installed locally (as appropriate).
After the folder is created, the following sections will install software and code into the development folder.

### Linux

Do the following using a terminal window:

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

## Clone Git Repository

The Git repository contains the StateCU software and configuration files.
It also contains this documentation.
The repository will be imported into the Eclipse/Photran workspace as a Fortran project.

### Clone the repository files (Linux)

```com
> C:
> cd \Users\user\cdss-dev\StateCU
> mkdir git-repos
> cd git-repos
> git clone https://bitbucket.org/trilynxsystems/trilynx-ws-novastar.git
```

### Clone the repository files (Windows)

```bash
$ cd ~/trilynx-dev/NovaStarREST
$ mkdir git-repos
$ cd git-repos
$ git clone https://bitbucket.org/trilynxsystems/trilynx-ws-novastar.git
```

If prompted, specify the Bitbucket account credentials.
The repository will include the Java dynamic web project as a Maven project.

## View Developer Documentation with MkDocs

Developer documentation is created with MkDocs, which uses Markdown to create a static HTML website.
The [latest published documentation](http://learn.openwaterfoundation.org/owf-learn-cdss-statecu-dev/) is available on the web
but may not contain the latest content. **TODO smalers 2017-01-01 need to change URL to the OpenCDSS location when available.**

The MkDocs server is run locally to view the full documentation from the repository, as shown below.
Then view the [`localhost:8000`](https://localhost:8000) web page in a web browser.
This developer documentation and all linked documents will then be available in fully-navigable form to facilitate remaining
new developer setup steps.

### Run MkDocs Server (Linux)

```bash
$ cd ~/cdss-dev/StateCU/git-repos/cdss-app-statecu-fortran/doc-dev-mkdocs-project
$ mkdocs serve
```

### Run MkDocs Server (Windows)

```com
> C:
> cd \Users\user\cdss-dev\StateCU\git-repos\cdss-app-statecu-fortran\doc-dev-mkdocs-project
> mkdocs serve
```

## Create Eclipse Workspace Folder

This step is the same as the [Initial Project Setup](../project-init/eclipse-workspace/) so follow those instructions.

The workspace folder on Linux is `~/cdss-dev/StateCU/eclipse-workspace`.

The workspace folder on Windows is `C:\Users\user\cdss-dev\StateCU\eclipse-workspace`.

Start Eclipse by running the [Eclipse run script](../project-init/eclipse-run-script) shown below.  This script can be used any time to run Eclipse for this project.
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
> .\run-eclipse-statecu.bat
```

## Import the Existing Eclipse/Maven Project from the Git Repository Folder

The [Initial Project Setup](../project-init/overview/) process performed by the software team leads
did extensive work to get the Eclipse project set up
and these files were saved to the Git repository as an Eclipse/Photran Fortran project.

New developers need to import the project into the empty workspace as follows.

Use ***File / Import...*** and then follow the sequence below.

![Import Maven project](overview-images/eclipse-import-maven-project1.png)

The first step allows browsing to an existing project.

![Import Maven project](overview-images/eclipse-import-maven-project2.png)

Browse to and select the Git repository folder (same as for initial project setup),
as shown below.

![Import Maven project](overview-images/eclipse-import-maven-project3.png)

Press ***OK*** and then press ***Finish*** in the initial import dialog.

The resulting Eclipse workspace is shown as below.
Note that Eclipse recognizes the associated Git repository indicated by the repository name/branch next to the Eclipse project.

![Import Maven project](overview-images/eclipse-import-maven-project4.png)

## Configure Java Compiler for Java 8

This step is the same as the initial project setup, although less probably needs to be done
because Maven project indicated Java version preference.

* [Configure Java Compiler](../project-init/java-compiler/) - follow the documentation and confirm that the configuration is correct
	+ There should be no need to create a folder for Java code and tests because this exists in the repository
	+ Very little if anything needs to be done
	+ **TODO smalers 2016-12-20 need to verify how Java compliance is determined - does execution environment name get saved in Git files?**

## Next Steps - Development Tasks

At this point it should be possible to compile and run StateCU within the Eclipse interface.
See the documentation for [Development Tasks / Overview](../dev-tasks/overview/).
