# Scaffolder
A general purpose scaffolder

# Overview
The goal here is to be able to build a directory structure (with some empty files if needed) in order to maintain strcture consistency over parts of the project or among projects of the same type.

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

```scaffolder run struct.txt```

Scaffolder will create the structure and placeholder files (empty files) under the current directory.

# Building

```
$> ghc --make scaffolder.hs
```
Requires ghc >= 7.10.3

