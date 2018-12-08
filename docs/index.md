[smk (SmartMake)](http://lionel.draghi.free.fr/smk/index.html)
==============================================================

`smk` is an attempt to realize the simplest and smartest possible make.

It was created by Lionel Draghi, is released under [![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0), and is under active development.

------------------------------------------------------------------------

Table of contents  
- [smk (SmartMake)](#smk-smartmake)
  - [Overview](#overview)
  - [Usage](#usage)
  - [Downloading and building](#downloading-and-building)
  - [Portability](#portability)
  - [Further reading](#further-reading)
  
------------------------------------------------------------------------

## Overview

Imagine that your usual Makefile :

``` Makefile
hello: hello.o main.o
	gcc -o hello hello.o main.o

hello.o: hello.c
	gcc -o hello.o -c hello.c

main.o: main.c hello.h
	gcc -o main.o -c main.c
```

could be, while keeping the same behavior, as simple as the command list :

``` Makefile
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```

that is, with **no more rules, dependencies, target, etc.**

This would require a tool able to observes the execution of the various command lines, and smart enough to understand for example that the first command is reading `hello.c`, and thus depending on this file, and producing `hello.o`, and to understand that if both `hello.c` and `hello.o` didn't change since last run, it don't have to re-run this command.

> this is what `Smk` do!

## Usage

A short, but highly recommended, [`smk` tutorial](tutorial.md) is the best way to quickly start.  

And don't forget [`smk --help`](cmd_line.md).


## Downloading and building

1. First, get the sources
   
   - Either donwload it on [the GitHub project page](https://github.com/LionelDraghi/smk)  

   - or directly with `git`:  
     > `git clone https://github.com/LionelDraghi/smk.git`

2. Go to the created `smk` directory

3. Ensure you have a gnat compiler (gcc Ada)  
   On Debian family:  
   >  `apt install gnat make`

4. Build it:  
   > `gprbuild -P smk.gpr`


## Portability

This tool heavily rely on [`strace`](https://en.wikipedia.org/wiki/Strace) utility, and thus is dependent on Linux for now.

I keep OS dependencies as small and localized as possible to ease porting.  
Currently, both dependencies to GNAT for the Spawn procedure, and to the
OS (the strace call himself) are in a short separate procedure in smk-main-run_command.adb.

That said, I do not entend to port smk to other platforms myself, but many OSes provide [utilities similar to strace](https://en.wikipedia.org/wiki/Strace#Similar_tools), so it may be done.  
And obviously, any contribution, including ports, is welcomed.

`smk` is curently only tested on my Debian x86_64 box.


## Further reading

- [More on the `smkfile` format](smkfile_format.md)
- [Not sure to understand what is the difference with `make`...](compare_with_make.md)
- [Limitations and bugs](compare_with_make.md)
- [Changelog](changelog.md)
- [Build Dashboard](dashboard.md)
- Tests  
    * [Tests status](tests/tests_status.md)
    * [Tests results](tests/testrec.md)
- [About](about.md)

Lionel
