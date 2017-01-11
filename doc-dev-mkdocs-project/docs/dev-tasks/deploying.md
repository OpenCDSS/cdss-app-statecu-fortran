# Development Tasks / Deploying

Deploying the software consists of compiling StateCU into an executable and packaging into an installer that can be distributed.

**TODO smalers 2017-01-02 Need to coordinate with Erin Wilson on how StateCU should be deployed... with the GUI, separate, etc.
Also need to evaluate whether to deploy from the Git Project page and/or CDSS website.**

This documentation contains the following sections:

* [Build Checklist](#build-checklist)
* [Creating StateCU Installer](#creating-statecu-installer)
* [Releasing StateCU](#releasing-statecu)

## Build Checklist

Currently there is not a need for StateCU to be built in a continuous integration process because the development team is small.
Consequently the build process is done by the "buildmaster".

The full build process checklist is as follows:

1. **TODO smalers 2017-01-10 need to discuss software version number and release notes.**
2. **TODO smalers 2017-01-10 need to discuss cleaning up branches into master so there are no loose ends, tagging release, etc..**
3. Run `make clean`.
4. Run `make statecu`.
5. **TODO smalers 2017-01-10 need make targets for creating installer, etc.... does this depend on a StateCU GUI release? link to below.**
6. **TODO smalers 2017-01-10 need to discuss deploying installer to GitHub pages, CDSS website, etc. link to below.**

## Creating StateCU Installer ##

**TODO smalers 2017-01-10 include here details of creating the installer**

## Releasing StateCU

**TODO smalers 2017-01-10 include here details of public release**
