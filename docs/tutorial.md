Tutorial
========

Table of contents
- [Tutorial](#tutorial)
  - [Create the sources for this test case](#create-the-sources-for-this-test-case)
  - [First run](#first-run)
  - [What are those new files in the the current dir?](#what-are-those-new-files-in-the-the-current-dir)
  - [Second smk run](#second-smk-run)
  - [Let's remove a file](#lets-remove-a-file)
  - [Let's modify a source](#lets-modify-a-source)
  - [Another smk run](#another-smk-run)
  - [Command summary](#command-summary)


## Create the sources for this test case  

Create and move to a tmp dir, and then:

```C
echo '
#include <stdio.h>
#include <stdlib.h>

void Hello(void)
{
	printf("Hello World\n");
}
' > hello.c
```

```C
echo '
#include <stdio.h>
#include <stdlib.h>
#include "hello.h"

int main(void)
{
	Hello();
	return EXIT_SUCCESS;
}
' > main.c
```

```C
echo '
#ifndef H_GL_HELLO
#define H_GL_HELLO

void Hello(void);

#endif
' > hello.h
```

Create a `MyBuild` file with your favorite editor containing just your commands:  

```shell
echo '
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
' > MyBuild
```

## First run

`smk MyBuild`  
(it's equivalent to `smk build MyBuild`)  
all commands should be executed:

```
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```  

> From now on, smk knows sources and targets for each command:  

let's see what smk retains from the last run:

`smk status -l`

For convenience, when there is one (and only one) smk file in the dir, you don't have to give it on the command line.  
So, from now on, `smk` is equivalent to `smk MyBuild` 

```
2018-12-07 23:53:06.50 [] gcc -o hello hello.o main.o
  Sources (2) :
  - 2018-12-07 23:53:06.00:/home/lionel/Proj/smk/tests/hello.c/hello.o
  - 2018-12-07 23:53:06.00:/home/lionel/Proj/smk/tests/hello.c/main.o
  Targets (1) :
  - 2018-12-07 23:53:06.00:/home/lionel/Proj/smk/tests/hello.c/hello

2018-12-07 23:53:06.24 [] gcc -o hello.o -c hello.c
  Sources (1) :
  - 2018-12-07 23:17:03.00:/home/lionel/Proj/smk/tests/hello.c/hello.c
  Targets (1) :
  - 2018-12-07 23:53:06.00:/home/lionel/Proj/smk/tests/hello.c/hello.o

2018-12-07 23:53:06.40 [] gcc -o main.o -c main.c
  Sources (2) :
  - 2018-11-14 23:14:17.00:/home/lionel/Proj/smk/tests/hello.c/hello.h
  - 2018-12-07 00:41:31.00:/home/lionel/Proj/smk/tests/hello.c/main.c
  Targets (1) :
  - 2018-12-07 23:53:06.00:/home/lionel/Proj/smk/tests/hello.c/main.o

```

> Note that by default `smk` do not display system files (for exemple `/usr/include/stdio.h`), otherwise the output would be flooded!  
> If you want to know on which system files you depends on, use the `--show-all-files` options (short form `-sa`).  

To see what I mean by flooded, try it :

`smk status -sa -l MyBuild` 


## What are those new files in the the current dir?

Smk stores information in local hidden .smk.* files, one per smk file :

`ls -l .smk*`

## Second smk run

`smk MyBuild`, or just `smk`

Sources are unchanged, target is up to date, smk exit silently.  

To get more info, you may try the `--verbose` option (short form : `-v`)

`smk -v`


## Let's remove a file

`rm main.o`  
`smk`  

Only the two commands needed to get `hello` updated are run:  

```
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```

`smk` provides an equivalent to the classical `make clean` : the `clean` command.  
You may try it, with or without the `--dry-run` (short form `-n`) option if you don't want to effectively remove files :  
`smk -n clean`  

This option will not remove `smk` internal files. If you want to do that:  
`smk reset`  
Check it with : `ls -l .smk*`  
Or with `smk -lr` (long form : `--list-runs`)

And then, rebuild with `smk MyBuild`.

## Let's modify a source

`touch hello.c`  

Got get more info on why are commands executed, just add the `--explain` option (short form : `-e`)  

`smk -e`

Once more, only the two commands needed to get `hello` updated are run:  
```
gcc -o hello.o -c hello.c
gcc -o hello hello.o main.o
```

## Another smk run

`smk`

No file changes, nothing's done.


## Command summary

What have we seen in this tutorial?  

| I want to                                                             | Command           |
| --------------------------------------------------------------------- | ----------------- |
| Run MyBuild                                                           | `smk MyBuild`     |
| Check what `smk` understand from MyBuild                               | `smk status MyBuild` |
| See runfiles in the current directory                                 | `smk -lr`         |
| Run it with explanations                                              | `smk -e MyBuild`  |
| Check what would be run, without running it                           | `smk -n MyBuild`  |
| cleanup all targets                                                   | `smk clean`       |
| show system files on status (and -lt, ls, etc.)                       | `smk -sa -rl`     |
| Get the full picture of command line                                  | `smk -h`          |

What else have I to explore?  

| I want to                                                             | Command               |
| --------------------------------------------------------------------- | --------------------- |
| Check what `smk` understand from my smkfile                            | `smk read-smkfile smkfile`     |
| do as much as possible                                                | `smk -k`              |
| ignore build errors (return no error)                                 | `smk -i`              |
| See identified sources / targets                                      | `smk -ls` / `smk -lt` |


