# Development Tasks / Version Control with Git

The OpenCDSS project is using Git and GitHub for version control.
Git protocols for StateCU follow OpenCDSS protocols and are summarized here for reference.

* [Git Training](#git-training)
* [Git Tools in Repository](#git-tools-in-repository)

---------------

## Git Training 

There are numerous resources on the internet for Git training.
See also the [CDSS / Learn Git](http://learn.openwaterfoundation.org/cdss-learn-git/) documentation.
**Need to update this link to point to the Google Cloud location when it is finalized.**

## Git Tools in Repository

Several scripts are installed in the `build-util` folder and can be used for StateCU development.
Run these scripts from the command line:

* `git-check-statecu.sh` - check the status of StateCU repositories
* `git-clone-all-statecu.sh` - clone all repositories, if not already cloned, after cloning the main repository
* `git-tag-all-statecu.sh` - tag all repositories for a StateCU version
