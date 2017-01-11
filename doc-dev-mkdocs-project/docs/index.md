# Learn StateCU (for Software Developers)

This documentation is the developer manual for Colorado's Decision Support Systems (CDSS) StateCU consumptive use model software.

**This documentation is a work in progress and will contain notes for inserts until resources can
be devoted to filling in blanks.  Comments in the code with text "TODO smalers 2017-01-01 some text" (for example)
are included to indicate work that needs to be completed.**

If you are reading this documentation, you have an interest in learning how StateCU is designed,
are a member of the software development team,
or perhaps wish to contribute software code enhancements or otherwise provide input to the project.
This documentation is intended to provide sufficient information to software developers
to streamline understanding of the StateCU code and developer environment.
It is expected that software developers are technically competant and
follow conventions of the open source StateCU project.

This documentation includes the following sections:

* [Colorado's Decision Support Systems](#colorados-decision-support-systems) - the system under which the software is maintained
* [Open Water Foundation](#open-water-foundation) - 
* [How to Use this Documentation](#how-to-use-this-documentation)
* [License](#license)
* [Source Repository on GitHub](#source-repository-on-github)
* [MkDocs](#mkdocs)

## Colorado's Decision Support Systems

Colorado's Decision Support Systems ([CDSS, cdss.state.co.us](http://cdss.state.co.us))
has been developed to answer important questions about Colorado's water resources.
CDSS efforts are led by the [Colorado Water Conservation Board (CWCB)](http://cwcb.state.co.us)
and [Colorado Division of Water Resources (DWR)](http://water.state.co.us).

![CDSS Website](images/CDSS-website.png)

One component of CDSS is the StateCU consumptive use model, which estimates irrigation water requirements and other demands.
StateCU results are used as input to the StateMod water allocation model.

In late 2016, the Open Water Foundation began the effort to move StateCU and other CDSS software to open source licensing
and establish open source software projects, referred to as "OpenCDSS", and this documentation is one project outcome.

## Open Water Foundation

The Open Water Foundation (OWF, [openwaterfoundation.org](http://openwaterfoundation.org)) is a 501(c)3 social enterprise
nonprofit that focuses on developing and supporting open source software to make better
decisions about water resources.  OWF is providing technical resources and management to
transition StateCU and other CDSS software to a sustainable open source project.

See also other [OWF learning resources](http://learn.openwaterfoundation.org).

## How to Use this Documentation

This website is intended as a companion to the StateCU source code and is the source of
information for software developers that support and enhance StateCU.

The documentation is organized somewhat chronologically for creating and maintaining the StateCU software, as follows,
with later sections referring to earlier sections as appropriate:

* [Development Environment](dev-env/overview/) - describes development environment software installation (some tools are shared between CDSS software projects)
* [Initial Project Setup](project-init/overview/) - describes how the StateCU software project was initially configured
* [Deployed Environment](deployed-env/overview/) - describes the deployed environment after software is installed
* [New Developer Setup](dev-new/overview/) - **new StateCU software developers should start here**
* [Software Design](software-design/overview/) - provides details about the software code design
* [Development Tasks](dev-tasks/overview/) - describes comment development tasks - **refer to this after new development environment is configured**

## License

The license for this documentation is being determined in the CDSS open source project.
More information will be provided later.

**TODO smalers 2017-01-09 need to determine softare and documentation license.**

## Source Repository on GitHub

**TODO smalers 2016-12-31 this information will be finalized once the public code repository and open source project website is put into place.**

The source files for this documentation are maintained in the private GitHub repository for StateCU: [cdss-app-statecu-fortran](https://github.com/OpenWaterFoundation/cdss-app-statecu-fortran) in the `doc-dev-mkdocs-project` folder.
These files are copied to the Open Water Foundation [Learn StateCU (for Software Developers)](http://learn.openwaterfoundation.org/owf-learn-cdss-statecu-dev/index.html) website.

## MkDocs

This documentation is prepared using MkDocs.
For full MkDocs documentation visit [mkdocs.org](http://mkdocs.org).
