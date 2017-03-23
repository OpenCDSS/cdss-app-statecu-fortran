# Initial Project Setup / Overview

The Initial Project Setup documentation is a record of how the project was set up the first time.
It is assumed that all software listed in the [Development Environment](../dev-env/overview/) has been installed,
although the project initialization steps will list each setup step.

This documentation is a useful reference in case the project needs to be reinitialized or other similar projects need to be configured.
Sections of this documentation are also referenced by the [New Developer](../dev-new/overview/) documentation,
such as configuring the Eclipse workspace.

>_[JHB] I think most developers who are reading this will be savvy enough to know that with FORTRAN, any decent editor along with a compiler and make system should be sufficient, so they will be wondering "why eclipse and photran?".  This does not have to be addressed here, but somewhere.  Essentially, for me Ecipse provides the capability to have a graphical debugging environment integrated with the editor.  Photran adds FORTRAN language syntax intelligence to Eclipse.  But some users (including me some days and on small projects) will not be willing to deal with Eclipse issues in order to get integrated debugging, so we should probably say Eclipse is optional __for advanced users__._

This documentation includes the following sections:

* [Background on Code Versions](#background-on-code-versions) - recent code history
* [Development Folder Structure](#development-folder-structure) - overview of the development files
	+ [Linux](#linux)
	+ [Windows](#windows)
* [Eclipse File Location Overview](#eclipse-file-location-overview) - locations of various Eclipse files
* [Project Initialization Steps](#project-initialization-steps) - steps to initialize the StateCU software project,
done once and thereafter [New Developer](../dev-new/overview/) instructions apply

## Background on Code Versions

The StateCU code has been under development for over 20 years as part of [CDSS](http://cdss.state.co.us).
However, few changes have occurred since 2012 and the following discussion focuses on recent history.

**TODO smalers 2016-12-31 need to decide if need to explain earlier history**

The StateCU 13.00 code was loaded into Jim Brannon's private `statecu-project` GitHub repository on January 20, 2011.
This repository only contains source code, with no documentation or tests, and all source code files are
located in the main repository folder.
The repository does not contain any Eclipse files, although Eclipse/Photran was used for recent development.
Several code changes occurred on a limited number of files since that time,
mainly to support compiling on Linux with `gfortran`.

>_[JHB] at the time, I recall that various StateCU FORTRAN development environments were still in use on a number of platforms by multiple people, so any IDE project files were carefully kept separate from the StateCU code_

The Open Water Foundation (OWF) compared the repository code with the latest code provided by Erin Wilson of Wilson Water Group (WWG),
as of Nov 18, 2016, and found only minor differences due to end-of-line characters.
Consequently, Jim Brannon's GitHub repository contains the latest version of the code, marked as version 13.10.
The latest StateCU version on the CDSS website as of Dec 31, 2016, is 13.03.

The OpenCDSS project, led by OWF, is establishing a more comprehensive StateCU repository and development environment,
in particular to establish a development environment that can support multiple developers.
Consequently, the code from Jim Brannon's repository was migrated to the OWF private repository named `cdss-app-statecu-fortran`,
retaining the code history. The repository was cloned to the OWF repository and the remote reference was deleted so that no
link to the Jim Brannon repository remains.
This is appropriate because the OpenCDSS repository will be the main repository going forward.

The code organization was restructured as per the folder structure discussed in the
following section and additional content was added, including this developer documentation.
The goal is to allow a new developer to clone the repository and get up and running as quickly as possible,
while also avoiding user-specific files in the repository, which would cause ongoing conflicts in file content.

## Development Folder Structure

This documentation assumes that files are configured according to the following folder structure.
It is recommended that StateCU developers follow this folder structure closely because if they do not,
the documentation will be less helpful and troubleshooting will require additional resources.
Feedback on the structure can be discussed with the development team leads.
There does not seem to be a clear folder structure standard for Fortran projects and consequently the following
builds upon [Maven Java Standard Directory Layout](https://maven.apache.org/guides/introduction/introduction-to-the-standard-directory-layout.html).

**TODO smalers 2016-12-31 need to flesh out how automated testing could work using Python pytest and Fortran unit tests - will
perhaps result in "fortran" and "python" folders under "test".**

### Linux

The Linux development environment relies on compilers being installed on the operating system (whereas Windows development environment
relies on additional layer of MinGW or Cygwin).
The following folder structure is similar to the Windows version other than different conventions for Linux file locations.

```text
/usr/... etc.                                      (TODO smalers 2016-12-31 need to document where compilers install)

/home/user/                                        (Software developer's home folder)
    cdss-dev/                                      (CDSS software projects)
        StateCU/                                   (CDSS StateCU software project)
            eclipse-workspace/                     (Eclipse workspace to organize Eclipse projects)
            git-repos/                             (Git repositories that comprise StateCU)
                cdss-app-statecu-fortran/          (StateCU Git main program repository)
                    .git/                          (Git local repository - DO NOT TOUCH DIRECTLY)
                    .gitattributes                 (Git repository properties)
                    .gitignore                     (Git repository global ignore list)
                    doc/                           (Legacy documentation, such as Word user manual)
                    doc-dev-mkdocs-project/        (MkDocs project for the developer documentation)
                    doc-user-mkdocs-project/       (MkDocs project for the user documentation)
                    src/                           (StateCU source code, main and tests)
                        main/                      (StateCU program code)
                            fortran/               (StateCU Fortran code)
                                *.for
                                *.inc
                                makefile 
                            resources/             (Envisioned as non-Fortan code that may be needed - supporting files)
                        test/                      (StateCU tests)
                            fortran/               (Envisioned for Fortran unit tests)
                                *.for
                            python/                (Envisioned for pytest functional tests)
                                *.py
```
>_[JHB] what files would be in the eclipse_workspace and not the git repo?_

### Windows

The following assumes that MinGW is used to provide the compilers.

**TODO smalers 2017-01-01 need to add description for Cygwin.**


```text
C:\MinGW\                                          (Install home for MinGW, which includes gfortran)

C:\Users\user\                                     (Software developer's home folder)
    cdss-dev\                                      (CDSS software projects)
        StateCU\                                   (CDSS StateCU software project)
            eclipse-workspace\                     (Eclipse workspace to organize Eclipse projects)
            git-repos\                             (Git repositories that comprise StateCU)
                cdss-app-statecu-fortran\          (StateCU Git main program repository)
                    .git\                          (Git local repository - DO NOT TOUCH DIRECTLY)
                    .gitattributes                 (Git repository properties)
                    .gitignore                     (Git repository global ignore list)
                    doc\                           (Legacy documentation, such as Word user manual)
                    doc-dev-mkdocs-project\        (MkDocs project for the developer documentation)
                    doc-user-mkdocs-project\       (MkDocs project for the user documentation)
                    src\                           (StateCU source code, main and tests)
                        main\                      (StateCU program code)
                            fortran\               (StateCU Fortran code)
                                *.for
                                *.inc
                                makefile 
                            resources\             (Envisioned as non-Fortan code that may be needed - supporting files)
                        test\                      (StateCU tests)
                            fortran\               (Envisioned for Fortran unit tests)
                                *.for
                            python\                (Envisioned for pytest functional tests)
                                *.py
```

## Eclipse File Location Overview

**TODO smalers 2016-12-31 Need to fill this in to explain how various levels of Eclipse files are used.**

## Project Initialization Steps

The following are the project initialization steps in the recommended order,
although some steps were actually implemented in slightly different order for practical reasons.

* Initialize software development folder and Git repository to receive files
	+ [Create Development Home Folder](home-folder/) - manually create project home folder that will hold all the software development files
	+ [GitHub Git Repository](github/) - create the GitHub repository from the legacy repository
* Initialize Eclipse development environment
	+ [Eclipse Run Script](eclipse-run-script/) - the script ensures that the proper version of Eclipse and Java are used
	+ [Eclipse Workspace](eclipse-workspace/) - create an Eclipse workspace for the StateCU software project
	+ [Eclipse StateCU Project](eclipse-statecu-project/) - configure the project connected to the Git repository files
	+ [Eclipse Folder Structure](eclipse-folder-structure/) - implement the development folder structure
	+ [Eclipse Make Targets](eclipse-make-targets/) - implement make targets to run makefile
* Initialize default utility scripts
	+ [Build Utility Scripts](build-util/) - helpful build utility scripts for developers 
* Initialize documentation
	+ [Documentation, Developer (MkDocs)](doc-dev/) - create initial MkDocs project for developer documentation
	+ [Documentation, User (MkDocs)](doc-user/) - create initial MkDocs project for user documentation
	+ [Doxygen](doc-doxygen/) - configure Doxygen project to generate API documentation
