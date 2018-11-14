# cdss-app-statecu-fortran #

This repository contains the source code and source files for developer and user documentation
for the StateCU consumptive use model.
StateCU is part of [Colorado's Decision Support Systems (CDSS)](http://cdss.state.co.us).
Open source CDSS is referred to as OpenCDSS.
CDSS is supported by the [Colorado Water Conservation Board](http://cwcb.state.co.us) and
[Colorado Division of Water Resources](http://water.state.co.us).

The developer documentation and guidelines will be updated as the development environment is used.

* [Background](#background)
* [Repository Folder Structure](#repository-folder-structure)
* [Development Environment](#development-environment)
* [Compiling](#compiling)
* [License](#license)
* [Contributing](#contributing)
* [Contact](#contact)
* [Release Notes](#release-notes)

-----

## Background ##

The StateCU software estimates crop consumptive use.
Input for StateCU consists of locations (stations or structures), crop characteristics,
crop acreage, irrigation method, climate data, and other data.
Output is irrigation water requirement and other information,
which can be used as input to the StateMod water allocation model and other analysis tools.

## Repository Folder Structure ##

The following folder structure is used for repository files.
It is assumed that the repository folder name is the same as the repository name.

```
cdss-app-statecu-fortran/           Repository folder.
  build-util/                       Useful utility scripts for software developers.
  doc-dev-mkdocs-project/           Markdown/MkDocs project for developer documentation.
  doc-doxygen-project/              Doxygen project to auto-generate code documentation.
  doc-user-mkdocs-project/          Markdown/MkDocs project for user documentation.
  .git                              Git repository internal files (do not modify directly!).
  .gitattributes                    Standard Git configuration file for repository properties.
  .gitignore                        Standard Git configuration file for ignoring files.
  .github                           Files used by GitHub, such as issues template.
  LICENSE.md                        Software license.
  README.md                         This file.
  resources/                        Various resources used by this project.
  src/                              StateCU source code.
    main/                           Source code for main program.
      fortran/                      Fortran source code for main program.
```

The following folder structure is recommended for top-level folders to organize OpenCDSS software projects
and is consistent with other OpenCDSS projects.
Top-level folders should be created as necessary.

```

C:\User\users\                       Windows user files.
/home/user/                          Linux user files.
  cdss-dev/                          Software for Colorado's Decision Support Systems.
    StateCU/                         Work related to the StateCU product.
      git-repos/                     Git repositories for StateCU.
        cdss-app-statecu-fortran/    The StateCU code and documentation repository (see above).
```

## Development Environment ##

Software for the development environment should be installed and configured as per the
[StateCU Developer Documentation](http://learn.openwaterfoundation.org/owf-learn-cdss-statecu-dev/index.html).

## Compiling ##

The Eclipse Photran integrated development environment can be used to develop StateCU.
However, the basic instructions to compile StateCU on the command line are as follows:

1. Open a Windows command prompt window.
2. In that window cd to `build-util\mingw` and run `setup-mingw-env.bat`.
3. In that window, change to the source code folder `src\main\fortran`.
4. View `make` command targets:  `make help`
5. Compile StateCU using the `make` command:  `make statecu`
6. The executable program will have a name `statecu-version.exe` on Windows and `statecu-version` on Linux, where `version` is a numeric version.
7. Use the executable in testing.

## License ##

The software is licensed under GPL v3+.  See the [LICENSE.md](LICENSE.md) file.

## Contributing ##

Contributions to this project can be submitted using the following options:

1. StateCU software developers with commit privileges can write to this repository
as per normal CDSS development protocols.
2. Post an issue on GitHub with suggested change (preferred for small changes).
3. Fork the repository, make changes, and do a pull request (preferred for large changes).
Contents of the current master branch should be merged with the fork to minimize
code review before committing the pull request.

## Contact ##

The lead developers/maintainers for StateCU that actively monitor repository issues are listed below.
Questions can be posed using the GitHub issues feature.

* @kelleythompson, Kelley Thompson, Colorado Division of Water Resources
* @macphersonbr, Brian Macpherson, Colorado Water Conservation Board
* Erin Wilson, Wilson Water Group (StateCU modeler)
* @smalers, Steve Malers, Open Water Foundation

## Release Notes ##

Release note comments are currently included in the `gcommon.inc` source file.
