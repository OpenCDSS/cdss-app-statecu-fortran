# Development Environment / Overview

This Development Environment documentation is intended to be used as a reference by a software developer that
sets up the project for the first time or initializes a new development environment to contribute to the project.
Specific sections are referenced by the [Initial Project Setup](../project-init/overview/),
[Deployed Environment](../deployed-env/overview/), and
[New Developer](../dev-new/overview/) sections.

This documentation includes the following sections:

* [Development Environment Software Requirements](#development-environment-software-requirements)
* [Software Install Location Considerations](#software-install-location-considerations)
* [Portability Considerations](#portability-considerations)

------------------

## Development Environment Software Requirements

The following software are needed in the StateCU development environment and should be installed before doing [Initial Project Setup](../project-init/overview/),
although the Initial Project Setup documentation generally indicates prerequisites for software that needs to be installed.
Steps can be skipped if they have been completed previously as part of operating system setup or
during setup of other software development projects.
Some software and scripts may have been installed during project initialization
and will exist in the repository that is cloned from GitHub when setting up the development environment.

The development environment as described provides a full technology stack for development.
It is possible to use a subset of the components (text editor and compiler rather than Eclipse/Photran IDE, for example);
however, the full stack is described in order to provide a fully-integrated development environment.
Software developers need to invest in training to be competent with the various tools,
although this documentation is intended to help facilitate development by providing useful information.

The following software are required for StateCU development:

1. [Machine](machine/) - minimal GNU for Windows (MinGW), Cygwin, or Linux virtual machine (VM), to support `gfortran` compiler
(**MinGW on Windows is the initial focus and Linux has been tested to a lesser degree**)
2. [Git](git/) - needed to perform command line version control operations
3. [Python and pip](python/) - needed by MkDocs and pytest, and useful general tool
4. [pytest](pytest/) - an option being evaluated for automated testing
5. [MkDocs](mkdocs/) - MkDocs is used for developer and user documentation static websites, including this documentation
6. [Java 8](java8/) - used to run Eclipse, and can be used to write utility programs
7. [gfortran](gfortran/) - compiler for the StateCU Fortran software
8. [Eclipse/Photran](eclipse/) - IDE used for interactive Fortran software development
9. [Doxygen](doxygen/) - documentation tool
10. [KDiff3](kdiff3/) - tool for comparing files

-------------------

## Software Install Location Considerations

The software development environment must be appropriately configured to effectively develop StateCU and support collaboration.
Development environment setup is actually a major effort before any code can be written
and is typically done the first time by someone that has a good grasp of the technologies.
Failing to understand how to set up the development environment will likely lead to wasted time
and possibly back-tracking on previous software development work.
Using a consistent development environment for all developers ensures that documentation is applicable and troubleshooting is consistent.
Therefore, the software installation locations described in this documentation are highly recommended to avoid issues.

See the discussion of [Initial Project Setup / Development Folder Structure](../project-init/overview#development-folder-structure)
for a folder structure that is assumed in this documentation.
It is important for software developers to understand the software tools and configuration so that they can troubleshoot configuration issues.

## Portability Considerations

The StateCU development environment is intended to be as portable as possible,
in order to allow multiple software developers to contribute to the software on multiple operating systems.
Portable configuration minimizes day-to-day conflicts in developer environment that result in lost productivity and software quality.
Portability considerations are discussed where appropriate.
