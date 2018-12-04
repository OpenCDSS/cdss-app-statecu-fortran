# Initial Project Setup / Create Development Home Folder

See the [discussion of development project folder structure](overview#development-folder-structure).

The following sections are included in this documentation:

* [Prerequisites](#prerequisites)
* [Create Home Folder for Development Files](#create-home-folder-for-development-files)
	+ [Linux and Git Bash](#linux-and-git-bash)
	+ [Windows](#windows)

----------------------

## Prerequisites

All setup for the project should occur as a normal user (not root or other special user) in the software developer's home folder.
Most of the contents of this folder will be backed up during GitHub repository commits.
Additional backups can be made by periodically copying files to a backup device or configuring automated backups of the folder.

## Create Home Folder for Development Files

Assuming that the folder structure is as documented for the project folder structure,
create a folder to serve as an umbrella for the StateCU development.
This folder may have already been created as part of software tool installation,
for when creating script(s) to run and configure development tools.

Additional folders will be created as specific software tools are installed and configured,
and software files are added, as per other documentation sections.
If documentation refers to a folder that has not been created, it is generally OK to create it manually
because few if any of the top-level folders are created with scripts.

### Linux and Git Bash

```bash
$
$ cd
$ # Create a folder for all CDSS development projects, to differentiate from other user files
$ mkdir cdss-dev
$ cd cdss-dev
$ # Create a folder for the StateCU software project development files
$ mkdir StateCU
```

### Windows

Do the following in a Windows command shell or the equivalent in Windows file explorer.


```com
>
> C
> cd \Users\user
> # Create a folder for all CDSS development projects, to differentiate from other user files
> mkdir cdss-dev
> cd cdss-dev
> # Create a folder for the NovaStar REST web services software project development files
> mkdir StateCU
```

