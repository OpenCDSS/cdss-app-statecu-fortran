# StateCU (for Developers) #

This documentation is the developer manual for Colorado's Decision Support Systems (CDSS) StateCU consumptive use model software.

If you are reading this documentation, you have an interest in learning how StateCU is designed,
are a member of the software development team,
or perhaps wish to contribute software code enhancements or otherwise provide input to the project.
This documentation is intended to provide sufficient information to software developers
to streamline understanding of the StateCU code and developer environment.
It is expected that software developers are technically competent and
follow conventions of the open source StateCU project.

This documentation page includes the following sections:

* [How to Use this Documentation](#how-to-use-this-documentation) - guidance and list of main documentation sections
* [Colorado's Decision Support Systems](#colorados-decision-support-systems) - the system under which the software is maintained
* [License](#license) - license for software and this documentation
* [Source Repository on GitHub](#source-repository-on-github) - location of StateCU repository in GitHub
* [MkDocs](#mkdocs) - the tool used to create this documentation

## How to Use this Documentation ##

This website is a companion to the StateCU source code and provides guidance for
software developers that modify and support StateCU.

The documentation is organized with the first sections focusing on setup for a new developer and common development tasks.
The reference sections at the end provide information that may be of use but are typically not used day to day.
Use the search feature of this website to find specific information.

* [New Developer Setup](dev-new/overview/) - **new StateCU software developers should start here**
* [Development Tasks](dev-tasks/overview/) - describes comment development tasks - **refer to this after new development environment is configured**
* [REFERENCE: Software Design](software-design/overview/) - provides details about the software code design
* [REFERENCE: Deployed Environment](deployed-env/overview/) - describes the deployed environment after software is installed
* [REFERENCE: Development Environment](dev-env/overview/) - describes development environment software installation (some tools are shared between CDSS software projects)
* [REFERENCE: Initial Project Setup](project-init/overview/) - describes how the StateCU software project was initially configured

The navigation menu on the left provides access to pages in the documentation.
The navigation menu on the right provides access to sections on the page.
If the page is viewed in a narrow window the navigation menus may be compressed into an icon.

## Colorado's Decision Support Systems ##

Colorado's Decision Support Systems ([CDSS, cdss.state.co.us](https://www.colorado.gov/cdss))
has been developed to answer important questions about Colorado's water resources.
CDSS efforts are led by the [Colorado Water Conservation Board (CWCB)](http://cwcb.state.co.us)
and [Colorado Division of Water Resources (DWR)](http://water.state.co.us).

![CDSS Website](index-images/CDSS-website.png)

One component of CDSS is the StateCU consumptive use model, which estimates irrigation water requirements and other demands.
StateCU results are used as input to the StateMod water allocation model.

In late 2016, the CWCB funded the OpenCDSS project to move StateCU and other CDSS software to open source licensing
and establish open source software projects.
The [Open Water Foundation](http://openwaterfoundation.org) was contracted to lead the OpenCDSS project.

## License ##

The license for this documentation is the [Creative Commons CC-BY 4.0 license](https://creativecommons.org/licenses/by/4.0/).

The StateCU software is licensed using the GPL v3 license.

## Source Repository on GitHub ##

The source files for this documentation are maintained in the
[StateCU GitHub repository](https://github.com/OpenCDSS/cdss-app-statecu-fortran/tree/master/doc-dev-mkdocs-project/).

## MkDocs ##

This documentation is prepared using MkDocs.
For full MkDocs documentation visit [mkdocs.org](http://mkdocs.org).
