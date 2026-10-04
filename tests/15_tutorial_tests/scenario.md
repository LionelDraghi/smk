# Tutorial

This document is the smk tutorial, written as bbt scenarios: each command
of the tutorial is run, and its output is checked. Running `bbt` on this
file guarantees that the tutorial remains true.

_Table of Contents:_
- [Scenario: create the sources for this test case](#scenario-create-the-sources-for-this-test-case)
- [Scenario: first run](#scenario-first-run)
- [Scenario: what are those new files in the current dir?](#scenario-what-are-those-new-files-in-the-current-dir)
- [Scenario: second smk run](#scenario-second-smk-run)
- [Scenario: let's remove a file](#scenario-lets-remove-a-file)
- [Scenario: let's modify a source](#scenario-lets-modify-a-source)
- [Scenario: another smk run](#scenario-another-smk-run)

## Scenario : create the sources for this test case

Create the three C files, and a `MyBuild` file with your favorite editor
containing just your commands:

- Given the file `hello.c`
```c
#include <stdio.h>
#include <stdlib.h>

void Hello(void)
{
	printf("Hello World\n");
}
```
- Given the file `main.c`
```c
#include <stdio.h>
#include <stdlib.h>
#include "hello.h"

int main(void)
{
	Hello();
	return EXIT_SUCCESS;
}
```
- Given the file `hello.h`
```c
#ifndef H_GL_HELLO
#define H_GL_HELLO

void Hello(void);

#endif
```
- Given the file `MyBuild`
```shell
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```

## Scenario : first run

`smk MyBuild`
(it's equivalent to `smk build MyBuild`)
all commands should be executed:

- Given I run `rm -f hello hello.o main.o`
- Given I run `../../smk -q reset`
- When I run `../../smk MyBuild`
- Then I get
```
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```

From now on, smk knows sources and targets for each command.
Let's see what smk retains from the last run:

`smk status -l`

For convenience, when there is one (and only one) smk file in the dir,
you don't have to give it on the command line. So, from now on, `smk`
is equivalent to `smk MyBuild`. Note that by default `smk` does not
display system files (for example `/usr/include/stdio.h`), otherwise the
output would be flooded!

- When I run `sh -c "../../smk status -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
Command "gcc -o hello hello.o main.o", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (2)
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] hello.o
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] main.o
  Targets: (1)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] hello

Command "gcc -o hello.o -c hello.c", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (1)
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] hello.c
  Targets: (1)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] hello.o

Command "gcc -o main.o -c main.c", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (2)
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] hello.h
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] main.c
  Targets: (1)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] main.o
```

## Scenario : what are those new files in the current dir?

Smk stores information in local hidden .smk.* files, one per smk file:

- When I run `ls .smk.MyBuild`
- Then I get `.smk.MyBuild`

## Scenario : second smk run

`smk MyBuild`, or just `smk`

Sources are unchanged, target is up to date, nothing to run:

- When I run `../../smk`
- Then I get `Nothing to run`

## Scenario : let's remove a file

`rm main.o`
`smk`

Note that by default, `smk` does not rebuild a target just because
it is missing, only because a source is updated:

- When I run `rm main.o`
- When I run `../../smk`
- Then I get `Nothing to run`

Unless you use the `-mt` / `--missing-targets` option:

- When I run `sleep 1`
- When I run `sh -c "../../smk -mt -e | sed 's/[0-9]//g'"`
- Then I get
```
run "gcc -o main.o -c main.c" because Target file main.o is missing
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because Source file main.o has been updated (-- ::.)
gcc -o hello hello.o main.o
```

`smk` provides an equivalent to the classical `make clean`: the `clean`
command. You may try it, with or without the `--dry-run` (short form `-n`)
option if you don't want to effectively remove files:

- When I run `../../smk clean`
- Then I get
```
Deleting file hello
Deleting file hello.o
Deleting file main.o
```

This option will not remove `smk` internal files. If you want to do that,
use `smk reset`, and check it with `smk lr`:

- When I run `../../smk reset`
- Then I get `Deleting .smk.MyBuild`
- When I run `../../smk lr`
- Then I get `No run file`

And then, rebuild with `smk MyBuild`:

- When I run `../../smk MyBuild`
- Then I get
```
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```

## Scenario : let's modify a source

`touch hello.c`

To get more info on why are commands executed, just add the `--explain`
option (short form: `-e`)

Once more, only the two commands needed to get `hello` updated are run:

- When I run `sleep 1`
- When I run `touch hello.c`
- When I run `sh -c "../../smk -e | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
run "gcc -o hello.o -c hello.c" because Source file hello.c has been updated (YYYY:MM:DD HH:MM:SS.SS)
gcc -o hello.o -c hello.c
run "gcc -o hello hello.o main.o" because Source file hello.o has been updated (YYYY:MM:DD HH:MM:SS.SS)
gcc -o hello hello.o main.o
```

## Scenario : another smk run

No file changes, nothing to run:

- When I run `../../smk`
- Then I get `Nothing to run`

## Command summary

What have we seen in this tutorial?

| I want to                                  | Command              |
| ------------------------------------------ | -------------------- |
| Run MyBuild                                | `smk MyBuild`        |
| Check what `smk` knows from the last run   | `smk status -l`      |
| See runfiles in the current directory      | `smk lr`             |
| Run it with explanations                   | `smk -e`             |
| Check what would be run, without running it | `smk -n`            |
| Rebuild missing targets                    | `smk -mt`            |
| Cleanup all targets                        | `smk clean`          |
| Reset smk internal files                   | `smk reset`          |
| Get the full picture of command line       | `smk -h`             |
