# Development Tasks / Documenting

Documenting software for developers and users is one of the most important software developer tasks,
in particular for open source projects that rely on collaboration to ensure a sustainable project.
Without documentation, software can be confusing to understand and code may be rewritten when it does not need to be.

This documentation includes the following documentation:

* [Developer Documentation Using MkDocs](#developer-documentation-using-mkdocs)
* [User Documentation Using MkDocs](#user-documentation-using-mkdocs)
* [Fortran Code API Documentation Using Doxygen](#fortran-code-api-documentation-using-doxygen)
* [Fortran Code Internal Documentation Using Comments](#fortran-code-internal-documentation-using-comments)

## Developer Documentation Using MkDocs

This developer documentation uses MkDocs and should be updated appropriately to keep the documentation current.
See the [Initial Project Setup / Documentation, Develper (MkDocs)](../project-init/doc-dev/)
for background on how the documentation is configured.
If using the standard development files structure,
the developer documentation is located in `~/cdss-dev/StateCU/git-repos/cdss-app-statecu-fortran/doc-dev-markdown-project`.
See the following resources for information on MkDocs and Markdown:

* [MkDocs - Writing your docs](http://www.mkdocs.org/user-guide/writing-your-docs/)
* [Markdown on Wikipedia](https://en.wikipedia.org/wiki/Markdown)
* [commonmark.org Markdown reference](http://commonmark.org/help/)

## User Documentation Using MkDocs

The user documentation for StateCU currently uses Microsoft Word and is distributed as PDF.
That documentation has not yet been included in the StateCU repository.

**TODO smalers 2017-01-10 Need to discuss with WWG what should be done with existing Word documentation - could
add to `doc-user-manual` or `doc/UserManual` folder in repository, for example.**

An option for going forward is to use MkDocs for user documentation, and a placeholder folder has been added to
the repository.
See the [Initial Project Setup / Documentation, User (MkDocs)](../project-init/doc-user/)
for background on how the documentation is configured.
If using the standard development files structure,
the user documentation is located in `~/cdss-dev/StateCU/git-repos/cdss-app-statecu-fortran/doc-user-markdown-project`.

## Fortran Code API Documentation Using Doxygen

The StateCU subroutines, functions, and modules should be documented using Doxygen-style comments.
See the following resources:

* [Doxygen - Comment blocks in Fortran](http://www.stack.nl/~dimitri/doxygen/manual/docblocks.html#fortranblocks)
* [NASA Modeling Guru:  Using Doxygen with Fortran soruce code](https://modelingguru.nasa.gov/docs/DOC-1811)

**TODO smalers 2017-01-10 need to discuss with the team how agressively to add new comments to StateCU code as part of OpenCDSS project,
in order to make it easier for new developers to understand the code.**

Refer to the [Initial Project Setup / Documentation, API (Doxygen)](../project-init/doc-doxygen/) documentation for how to run Doxygen.

Currently, Doxygen is intended as a local developer tool.
If StateCU code is packaged into a library, then the Doxygen-generated API code can be published for developers that use the library.

## Fortran Code Internal Documentation Using Comments

The Fortran code files that comprise the StateCU software should be documented with in-line comments
using Fortran conventions to facilitate understanding of variables, data structures, modules, functions,
subroutines, etc.

**TODO smalers 2017-01-10 need to provide some specific examples for guidance.**