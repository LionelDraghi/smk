[smk (SmartMake)](http://lionel.draghi.free.fr/smk/index.html)
==============================================================

`smk` is an attempt to realize the smartest possible make.

**Please note that the current 0.0.2 version is the first MVP of smk**

------------------------------------------------------------------------

## Table of Contents

<!-- TOC -->

- [Table of Contents](#table-of-contents)
- [Overview](#overview)
- [Usage](#usage)
- [About](#about)
- [Building](#building)
- [Portability](#portability)

<!-- /TOC -->

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


## Usage

Create a `MyBuild` file with your favorite editor containing just your commands:
```
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
etc.
```

I want to | Command
----------|--------
Check what `smk` undestand from my file | `smk -lm MyBuild`
Run it | `smk MyBuild`
Run it with explanations | `smk -e MyBuild`
Check what would be run wihtout running it | `smk -n MyBuild`
See what has understud `smk` regarding sources and targets from last run | `smk -ls MyBuild`
What else? | `smk -h`


## About

This package was created by Lionel Draghi, and is released under [![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)


## Building

Get the sources [on the GitHub project page](https://github.com/LionelDraghi/smk)  

To build and run the tests, just :
> make

## Portability

This tool heavily rely on `strace` utility, and thus is dependent on Linux for now.

I keep OS dependencies as small and localized as possible to ease porting (many OSes provide utilities similar to strace), but do not entend to port smk to other platforms myself.
Any contribution, including ports, is welcomed.

`smk` is curently only tested on my Debian x86_64 box.

Lionel
