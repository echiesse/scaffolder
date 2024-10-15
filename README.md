# Scaffolder
A general purpose scaffolder

# Overview
The goal here is to be able to build a directory structure (with some empty files if needed) in order to maintain structure consistency over parts of a project or among projects of the same type.

# Availability

Scaffolder has been developed and tested in Windows 10/11, but there should be no problem running on Linux and Unix like systems as it does not depend on anything specific to the Windows platform.

# Installation

Pre compiled binaries and installers are not available at the moment, so the recommended way to install it is to build the project from source locally. Check the build instructions further down in this document.

After building the software, copy the generated `scaffolder.exe` (or `scaffolder`) to `%HOME%\bin` (`$HOME/bin` on Linux) and configure this folder to be in the executables search path of your platform (most Linuxes will already have it). Of course, if you prefer a different location go ahead and do it. Just make it is in your `PATH`.

# Usage
## Straight to the Point

- Create a template by either creating a file by hand or using `scaffolder reverse`
- Register the template using `scaffolder register <template-name> <template-path>`
- Use the template to create scaffolds with `scaffolder run <template-name> [target-dir]`

And that's it.

Continue reading for detailed information about the commands.

## Defining the Directory Tree Template

First you have to define the structure of your project in a plain text file. The file consists of a list of items (one item per line) starting with a plus (`+`) or minus (`-`) sign. Items starting with a `+` are directories and items starting with a `-` are files. Hard and symbolic links are not supported.

If a directory has subitems they must be indented by one level in the next lines. Unindenting a line means you are coming back to the previous level.

Line comments are supported using the `#` character.

Take the following description as an example:

```
+ Project
    + docs
        - manual.tex
    + plan
        - overview.md
    + src
        - main.py
        - setup.py
        - settings.py
```

It corresponds to a tree starting from the directory `Project`, which contains 3 subdirs: `docs`, `plan` and `src`. The directory `Project/docs` contains the file `manual.tex`, the directory `Project/plan` contains the file `overview.md` and the directory `Project/src` contains the fils `main.py`, `setup.py` and `settings.py`.

There's no need to define a single directory as a single root (`Project` could have a sibling file or directory).

### Creating Templates from Existing Directory Tree

Scaffolder has a command that allows you to use an existing filesystem tree to produce a template.

Run
```bash
$ scaffolder reverse <path>
```

and Scaffolder will spit the contents of the directory designated by `<path>` to stdout in the correct format. In order to save it to a file you can simply redirect the output.

For instance, this command line will take the current dir and save its structure to `struct.txt`:

```bash
$ scaffolder reverse . > struct.txt
```

## Registering Templates

After your template is created you must register it for later use. To register your template use the command:

```bash
$ scaffolder template-register <template-name> <template-path>
```

Where `template-name` is the name you want to give your template and `template-path` is the path to the template file.

To save the `struct.txt` created above in a template called `struct` we run the command:

```bash
$ scaffolder template-register struct struct.txt
```

You can see the list of registered templates running

```bash
$ scaffolder template-list
```

Here is an example of the output it produces:
```bash
$ scaffolder template-list
struct
```


## Generating Scaffolds

To generate a scafflod, simply call scaffolder using the subcommand `run` and pass in a template name

For example, to create a new scaffold using the `struct` template created above call:

```bash
$ scaffolder run struct
```

Scaffolder will create the structure and placeholder files (empty files) in the current directory.

Note that the current directory must be empty.

If you would like to point to a different location, pass it as a second argument to the subcommand:

Linux:
```bash
$ scaffolder run struct /CodeProjects/MyNewProject
```
Windows:
```batch
$ scaffolder run struct C:\CodeProjects\MyNewProject
```

In this example `MyNewProject` will be created if it does not exist. If it already exists, it must be empty.


# Building

## Requirements

Scaffolder is written in Haskell and compiles/builds with GHC.
It requires GHC >= 7.10.3 and the Haskell platform installed.

> Tip: The best way to have a working Haskell environment is by using [GHCUp](https://www.haskell.org/ghcup/)


## Building

To build just call

```bash
$ ghc --make scaffolder.hs
```

It will produce the executable `scaffolder` (or `scaffolder.exe`) which you can install in your system.

# Future
## Features
- Include sub template (`include` command)
- Definition of constants
- File templates
- Read template from a zip file?
