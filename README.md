# cdss-app-statecu-fortran

This repository contains the source code for the StateCU consumptive use model,
which is part of [Colorado's Decision Support Systems (CDSS)](http://cdss.state.co.us).
CDSS is supported by the [Colorado Water Conservation Board](http://cwcb.state.co.us) and
[Colorado Division of Water Resources](http://water.state.co.us).

The StateCU software is being migrated to an open source software project as part of the OpenCDSS project
led by the [Open Water Foundation](http://openwaterfoundation.org).

See the following online developer documentation to get started as a StateCU developer:

* [Learn StateCU (for Developers)](http://learn.openwaterfoundation.org/owf-learn-cdss-statecu-dev/index.html)

The developer documentation and guidelines will be updated as the development environment is proven out.

* [StateCU Repository Folder Structure](#statecu-repository-folder-structure)
* [Cloning this Repository](#cloning-this-repository)
* [Contributing](#contributing)
* [License](#license)
* [Contact](#contact)

-----

<a name="statecu-repository-folder-structure"></a>
## StateCU Repository Folder Structure ##

The following folder structure is recommended for StateCU development including this documentation.
Top-level folders should be created as necessary.

### Linux ###

```
/home/user/                                 (user's home folder)
    cdss-dev/                               (work done for Colorado's Decision Support Systems)
        StateCU/                            (work related to the StateCU product)
           git-repos/                       (Git repositories for StateCU)
               cdss-app-statemod-fortran/   (the StateCU code and documentation repository)
```

### Windows ####

```
C:\Users\user\                              (user's home folder)
    cdss-dev\                               (work done for Colorado's Decision Support Systems)
        StateCU\                            (work related to the StateCU product)
           git-repos\                       (Git repositories for StateCU)
               cdss-app-statemod-fortran\   (the StateCU code and documentation repository)
```

<a name="cloning-this-repository"></a>
## Cloning this Repository ##

Clone this repository using a Git software client, for example using Git command line:

### Linux ###

```sh
> cd /home/user/cdss-dev/StateCU/git-repos
> git clone https://github.com/OpenWaterFoundation/owf-learn-cdss-statecu-dev.git
```

### Windows ###

```sh
> C:
> cd \Users\user\cdss-dev\StateCU\git-repos
> git clone https://github.com/OpenWaterFoundation/owf-learn-cdss-statecu-dev.git
```

<a name="contributing"></a>
## Contributing ##

Contributions to this project can be submitted using the following options:

1. StateCU software developers with commit privileges can write to this repository
as per normal CDSS development protocols.
2. Post an issue on GitHub with suggested change (preferred for small changes).
3. Email the contact.
4. Fork the repository, make changes, and do a pull request (preferred for large changes).
Contents of the current master branch should be merged with the fork to minimize
code review before committing the pull request.

<a name="license"></a>
## License ##

A license for the software is being determined as part of the OpenCDSS project.

<a name="contact"></a>
## Contact ##

The lead developers/maintainers for StateCU are:

* Erin Wilson, Wilson Water Group, [erin.wilson@wilsonwatergroup.com](mailto:erin.wilson@wilsonwatergroup.com) (StateCU subject matter expert during transition to open source project)
* Steve Malers, Open Water Foundation, [steve.malers@wilsonwatergroup.com](mailto:steve.malers@openwaterfoundation.org) (initial author of developer documentation and architect of development/test environment)
* Andy Moore, Colorado Water Conservation Board, [andy.moore@state.co.us](mailto:andy.moore@state.co.us) (CDSS lead at the CWCB)
