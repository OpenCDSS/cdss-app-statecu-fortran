# Development Tasks / Documenting

Documenting software for developers and users is one of the most important software developer tasks,
in particular for open source projects that rely on collaboration to ensure a sustainable project.
Without documentation, software can be confusing to understand and code may be rewritten when it does not need to be.

This documentation includes the following documentation:

* [Developer Documentation Using MkDocs](#developer-documentation-using-mkdocs)
* [User Documentation Using MkDocs](#user-documentation-using-mkdocs)
* [Fortran Code API Documentation Using Doxygen](#fortran-code-api-documentation-using-doxygen)
* [Fortran Code Internal Documentation Using Comments](#fortran-code-internal-documentation-using-comments)

-----------------

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

### Run MkDocs Server to View Documentation

MkDocs runs a local Python web server that allows the browser to view the documentation.
To start the server, change to the folder where the documentation configuration file exists and start the server.
This should work for Windows command shell, Git Bash running on Windows or Linux, and Linux command shell,
depending on the development environment.

```
> cd \Users\userName\cdss-dev\StateCU\git-repos\cdss-app-statecu-fortran\doc-dev-mkdocs-project
> mkdocs serve
```

The following indicates that the server is running.  If an error is shown, it is usually because the
`mkdocs.yml` file lists a file that does not yet exist or the file has a syntax problem such as mis-matched quotes.
If a problem occurs, fix the problem and try restarting the server.

![mkdocs serve](documenting-images/mkdocs-serve.png)

Then view the documentation in a web browser using the address `http://localhost:8000`.
The MkDocs server will generally auto-detect changes to files and the browser will refresh.
When auto-refresh does not happen, manually refresh to see changes.

Stop the server with `Ctrl-C` in the command shell window.

### Publish the documentation.

Run the `doc-dev-mkdocs-project\build-util\copy-to-co-dnr-gcp.sh` script to publish the documentation
to the State of Colorado's Google Cloud Platform website.
The documentation will be installed into a folder corresponding to the software version.

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

Doxygen is currently used mainly to graph the calling sequence of functions.

Refer to the [Initial Project Setup / Documentation, API (Doxygen)](../project-init/doc-doxygen/) documentation for how to run Doxygen.

Doxygen output can be copied to the State of Colorado's Google Cloud Platform storage site for public access using the
`doc-dev-doxygen-project/copy-to-co-dnr-gcp.sh` script.

## Fortran Code Internal Documentation Using Comments

The Fortran code files that comprise the StateCU software should be documented with in-line comments
using Fortran conventions to facilitate understanding of variables, data structures, modules, functions,
subroutines, etc.
