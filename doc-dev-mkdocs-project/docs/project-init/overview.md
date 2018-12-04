# Initial Project Setup / Overview

The Initial Project Setup documentation is a record of how the project was set up the first time.
It is assumed that all software listed in the [Development Environment](../dev-env/overview/) has been installed,
although the project initialization steps will list each setup step.

This documentation is a useful reference in case the project needs to be reinitialized or other similar projects need to be configured.
Sections of this documentation are also referenced by the [New Developer](../dev-new/overview/) documentation,
such as configuring the Eclipse workspace.

This documentation includes the following sections:

* [Background on Code Versions](#background-on-code-versions) - recent code history
* [Development Folder Structure](#development-folder-structure) - overview of the development files
* [Eclipse File Location Overview](#eclipse-file-location-overview) - locations of various Eclipse files
* [Project Initialization Steps](#project-initialization-steps) - steps to initialize the StateCU software project,
done once and thereafter [New Developer](../dev-new/overview/) instructions apply

--------------------

## Background on Code Versions

The StateCU code has been under development for over 20 years as part of
[Colorado's Decision Support Systems (CDSS)](https://www.colorado.gov/cdss).
However, few changes have occurred since 2012 and the following discussion focuses on recent history.

The StateCU 13.00 code was loaded into Jim Brannon's private `statecu-project` GitHub repository on January 20, 2011.
This repository only contains source code, with no documentation or tests, and all source code files are
located in the main repository folder.
The repository does not contain any Eclipse files, although Eclipse/Photran was used for recent development.
Several code changes occurred on a limited number of files since that time,
mainly to support compiling on Linux with `gfortran`.

The Open Water Foundation (OWF) compared the repository code with the latest code provided by Erin Wilson of Wilson Water Group (WWG),
as of Nov 18, 2016, and found only minor differences due to end-of-line characters.
Consequently, Jim Brannon's GitHub repository contains the latest version of the code, marked as version 13.10.
The latest StateCU version on the CDSS website as of Dec 31, 2016, is 13.03.

The OpenCDSS project established a more comprehensive StateCU repository and development environment,
in particular to establish a development environment that can support multiple developers.
Consequently, the code from Jim Brannon's repository was migrated to the OWF private repository named `cdss-app-statecu-fortran`,
retaining the code history. The repository was cloned to the OWF repository and the remote reference was deleted so that no
link to the Jim Brannon repository remains.
This is appropriate because the OpenCDSS repository will be the main repository going forward.

The code organization was restructured as per the folder structure discussed in the
following section and additional content was added, including this developer documentation.
The goal is to allow a new developer to clone the repository and get up and running as quickly as possible,
while also avoiding user-specific files in the repository, which would cause ongoing conflicts in file content.

OWF transferred the repository from its GitHub account to the
[OpenCDSS GitHub account](https://github.com/OpenCDSS/cdss-app-statecu-fortran)
in November, 2018 and implemented the GPLv3 open source license.
The software version was updated to 13.11.

## Development Folder Structure

This documentation assumes that files are configured according to the following folder structure.
It is recommended that StateCU developers follow this folder structure closely because if they do not,
the documentation will be less helpful and troubleshooting will require additional resources.
There does not seem to be a clear folder structure standard for Fortran projects and consequently the following
builds upon [Maven Java Standard Directory Layout](https://maven.apache.org/guides/introduction/introduction-to-the-standard-directory-layout.html).

```text
/c/users/user/                           Git Bash user files (same as Windows location).
/home/user/                              Linux user files.
C:\Users\user\                           Windows user files (same as Git Bash location).
  cdss-dev/                              CDSS software development.
    StateCU/                             CDSS StateCU software project.
      eclipse-workspace/                 Eclipse workspace to organize Eclipse projects (not in Git repository).
      git-repos/                         Git repositories that comprise StateCU.
        cdss-app-statecu-fortran/        StateCU Git main program repository.
          .cproject/                     Eclipse project files (C project is used for Fortran).
          .git/                          Git local repository - DO NOT EDIT DIRECTLY.
          .gitattributes                 Git repository properties.
          .github/                       Template for GitHub issues, etc.
          .gitignore                     Git repository global ignore list.
          .project/                      Eclipse project settings.
          .setttings/                    Eclipse project settings.
          build-util/                    Useful scripts for development/build environment.
          doc-dev-doxygen-project/       Auto-generated software code documentation, using Doxygen.
          doc-dev-mkdocs-project/        Markdown/MkDocs project for the developer documentation.
          doc-user-mkdocs-project/       Markdown/MkDocs project for the user documentation.
          LICENSE.md                     License file.
          README.md                      This file.
          resources/                     Additional resources.
          src/                           StateCU source code, main and tests.
            main/                        StateCU program code.
              fortran/                   StateCU Fortran code.
                *.for                    Fortran source files.
                *.inc                    Fortran include files (common blocks).
                makefile                 Used with make program to compile software.
              test/                      StateCU unit tests, possible for future.
                fortran/
                  *.for
                python/                  Envisioned for pytest functional tests, future.
                  *.py
        cdss-app-statecu-fortran-test/   StateCU functional tests.
```

## Eclipse File Location Overview

The following summarizes Eclipse files, which are used if Eclipse/Photran software are used (optional).
These files are not stored in the repository because they are software files or
dynamically change for each developer.

```
/c/users/user/                           Git Bash user files (same as Windows location).
/home/user/                              Linux user files.
C:\Users\user\                           Windows user files (same as Git Bash location).
  cdss-dev/                              CDSS software development.
    StateCU/                             CDSS StateCU software project.
      eclipse-workspace/                 Eclipse workspace to organize Eclipse projects (see above).
        .metadata/                       Eclipse workspace files (updated for each developer).
          .plugins/                      Eclipse files for each software plugin.

  .eclipse/                              Eclipse files in user files.

C:\Program Files\Eclipse\                Eclipse software installation folder.
  eclipse-parallel-mars-64/              Software files, as per installation instructions.
```

## Project Initialization Steps

The following are the project initialization steps in the recommended order,
although some steps were actually implemented in slightly different order for practical reasons.

1. Initialize software development folder and Git repository to receive files
	1. [Create Development Home Folder](home-folder/) - manually create project home folder that will hold all the software development files
	2. [GitHub Git Repository](github/) - create the GitHub repository from the legacy repository
2. Initialize Eclipse development environment
	3. [Eclipse Run Script](eclipse-run-script/) - the script ensures that the proper version of Eclipse and Java are used
	4. [Eclipse Workspace](eclipse-workspace/) - create an Eclipse workspace for the StateCU software project
	5. [Eclipse StateCU Project](eclipse-statecu-project/) - configure the project connected to the Git repository files
	6. [Eclipse Folder Structure](eclipse-folder-structure/) - implement the development folder structure
	7. [Eclipse Make Targets](eclipse-make-targets/) - implement make targets to run makefile
3. Initialize default utility scripts
	1. [Build Utility Scripts](build-util/) - helpful build utility scripts for developers 
4. Initialize documentation
	1. [Documentation, Developer](doc-dev/) - create initial MkDocs project for developer documentation
	2. [Documentation, User](doc-user/) - create initial MkDocs project for user documentation
	3. [Doxygen](doc-doxygen/) - configure Doxygen project to generate API documentation
