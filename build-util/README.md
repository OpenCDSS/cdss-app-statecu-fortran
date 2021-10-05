# build-util

This folder contains utility scripts (or batch files if Windows) that are used in the build process.

| **File/Folder** | **Description** |
| -- | -- |
| `copy-to-co-dnr-gcp.bash` | Create and copy the StateCU zip file installer to Google Cloud platform, for public access. |
| `create-gcp-statecu-index.bash` | Create an `index.html` file on GCP that lists the installer downloads and associated documentation. |
| `eclipse/`| Contains batch file to run the correct version of Eclipse with proper Java version, if using Eclipse Photran for development. |
| `git-check-statecu.sh` | Script to indicate status of all StateCU repositories. |
| `git-clone-all-statecu.sh` | Script to clone all StateCU repisitories, if they have not yet been cloned. |
| `git-tag-all-statecu.sh` | Script to tag all related StateCU repisitories with the same tag. |
| `git-util/` | Generalized Git utility scripts. |
| `product-repo-list.txt` | Repository list used by Git utility scripts. |
| `run-doxygen.sh` | Run Doxygen to autogenerate code documentation in `doc-dev-doxygen-project` folder. |
| `search-for-edits.sh` | Used to determine historical code authors. |
| `x-mingw/' | Obsolete scripts used with MinGW environment. | 
