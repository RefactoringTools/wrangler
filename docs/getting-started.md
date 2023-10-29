# Getting started

## How do I get started with Wrangler?

First you need to download and install Wrangler: details of this are on the [home page](index.html). All the details of getting Wrangler running are further down this page.

The system is documented: this tells you about all the refactorings and other facilities. You can read it [here](https://refactoringtools.github.io/wrangler/). Installation instructions are further down this page.

You can see some introductory presentations about using Wrangler here.
- [Improving your (test) code with Wrangler](http://www.cs.kent.ac.uk/people/staff/sjt/presentations/PerthFeb2010.ppt)
- [Hands-on Refactoring with Wrangler](http://www.cs.kent.ac.uk/people/staff/sjt/presentations/ErlangFactory09.ppt)

Finally, you can take a look at the videos about Wrangler, available from the [home page](index.html).


## Installation

### With Erlang-LS (VS Code, Sublime, IntelliJ and others)

Follow the guide of the [Wrangler Language Server](wls/wrangler_language_server)

### Mac OS X and Linux

Follow the INSTALL file from the repository.

### Windows

[Download installer](https://github.com/downloads/RefactoringTools/wrangler/Wrangler_Setup.exe) 

(currently outdated)

### Running Wrangler in Emacs

Load a `.erl` file. 

Start/stop using 
    `Ctrl-C, Ctrl-R`

To undo any refactoring type 
    `Ctrl-C, Ctrl-_`

To configure the search directories, select the `Customize Wrangler` menu option.

On Windows, Erlang release R14/above are required 

### Running Wrangler in Eclipse + ErlIDE

On Windows systems, use a path with no spaces in it.

Install Eclipse, if you didn't already. 

All the details [here](http://erlide.org/index.html).