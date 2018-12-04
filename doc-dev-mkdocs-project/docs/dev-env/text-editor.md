# Development Environment / Text Editor #

The [Eclipse/Photran IDE](eclipse) is one option for editing code and performing other development tasks.
However, some developers may prefer to avoid Eclipse and instead use a test editor and the command-line compiler.
Text editors are also useful in general for some operations.
The StateCU development environment is configured to accommodate both options.
Text editors that are known to be used with StateCU are discussed below.

* [Prerequisites](#prerequisites)
* [General Considerations](#general-considerations)
* [Known Text Editors and Issues](#known-text-editors-and-issues)
	+ [Atom](#atom)
	+ [Notepad](#notepad)
	+ [Notepad++ ("notepad plus-plus")](#notepad-notepad-plus-plus)
	+ [Sublime](#sublime)
	+ [UltraEdit](#ultraedit)
	+ [vim](#vim)
	+ [Other Editors](#other-editors)

-------------

## Prerequisites ##

Text editors don't typically have prerequisites although some may have a cost.

## General Considerations ##

Text editors are generally comparable for basic features.
The following are important considerations:

1. Does the editor automatically detect line ending character (Windows or Linux) and use that
convention when adding new lines.
Most editors handle this and may show an indicator for the line ending type.
It is important that a file, when saved, has consistent line endings
so as to not confuse editors, compilers, Git, and other tools.
2. Does the editor use tabs or spaces for indentation.
Ideally it can be configured to display tabs.
Spaces should be used in code if possible because tab spacing may vary between developers and tools.
Otherwise, formatting issues may arise.
3. Some editors create temporary and backup files.
These files need to be ignored using Git `.gitignore` file so as to not commit to the repository.
4. Many editors include spell checkers.
Avoiding typos and misspelled words in code is desirable.

## Known Text Editors and Issues ##

The following are text editors that are known to have been used with StateCU development
or are likely to be used.

### Atom ###

The [GitHub Atom editor](https://atom.io/)
can be used to edit files and is particularly useful because it has a built-in Markdown file viewer
(use ***Ctrl-Shift-m*** to open the Markdown viewer for a file).

### Notepad ###

The Windows notepad editor has limitations and should not typically be used as text editor for coding.

### Notepad++ ("notepad plus-plus") ###

The [Notepad++](https://notepad-plus-plus.org/) editor is a good general purpose text editor.
The following are useful Notepad++ features:

* use the ***View / Show Symbol / Show End of Line*** menu to toggle the display
of end of line character, useful to determine whether Windows or Linux end of line is being used

### Sublime ###

The [Sublime editor](https://www.sublimetext.com/) is a full-featured editor used by many programmers.

### UltraEdit ###

The [UltraEdit editor](https://www.ultraedit.com/)
has been used by the developer in the past.

### vim ###

The [vim editor](https://www.vim.org/) is widely available on many operating systems and
is available by default in Git Bash.
A visual version of `vim` is available and keyboard driven version is available by default on many systems.
The following are useful `vim` features:

* use `vim -b` to check for `^M` end of line, indicating Windows-style line endings
* `vim` can be also be configured via its `~/.vimrc` file to clearly show when tabs are used in files:

```
" Displays tabs as >-, where first character indicates tab and following - fill in tab space
" see:  https://vi.stackexchange.com/questions/422/displaying-tabs-as-characters
set list
set listchars=tab:>-
```

### Other Editors ###

Other text editors can also be used and are a preference of the software developer.
See the [General Considerations](#general-considerations).
