# Scaffolder
A general purpose scaffolder

# Overview
The goal here is to be able to build a directory structure (with some empty files if needed) in order to maintain structure consistency over parts of the project or among projects of the same type.

# Base Usage

Given a file `struct.txt` with tree structure described like this:
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
Running the command

```
scaffolder run struct.txt
```

Scaffolder will create the structure and placeholder files (empty files) under the current directory.

# Building

```
$> ghc --make scaffolder.hs
```
Requires ghc >= 7.10.3

# Future
## Features
- Include sub template (`include` command)
- Definition of constants
- Per user persistent templates
    - Templates will be registered and called by name
- File templates
- Create scaffolding from an existing directory structure
- Read template from a zip file?

## Commands
- `scaffolder run <template-name>` - Create a dir struct based on a registered template
- `scaffolder template-register <template-name> <template-path>` - Register a new template
- `scaffolder template-list` - Lists existing templates
- `scaffolder template-show <template-name>` - Prints the specified template to the console.
