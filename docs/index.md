[smk (SmartMake)](http://lionel.draghi.free.fr/smk/index.html)
==============================================================

`smk` is an attempt to realize the simplest and smartest possible make.

------------------------------------------------------------------------

Table of contents
- [Overview](#overview)
- [Usage](#usage)
- [More options](#more-options)
- [Downloading and building](#downloading-and-building)
- [Portability](#portability)
- [About](#about)
  
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

This would require a tool able to observe the execution of the various command lines, and smart enough to understand for example that the first command is reading `hello.c`, and thus depending on this file, and producing `hello.o`, and to understand that if both `hello.c` and `hello.o` didn't change since last run, it doesn't have to re-run this command.

> this is what `Smk` does!

## Usage

- Create a `MyBuild` file with your favorite editor containing just your commands:  
```shell
echo '
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
' > MyBuild
```

- Create the sources for this test case:  
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

- First run, all commands are executed:
> smk MyBuild  

```
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```  

- From now on, smk knows sources and targets for each command:  
> smk -ls MyBuild  

```
2018-11-30 00:42:23.47 []gcc -o hello hello.o main.o
  Sources (20) :
  - 2018-11-30 00:42:23.00:hello.o
  - 2018-11-30 00:42:10.00:main.o
  Targets (1) :
  - 2018-11-30 00:42:23.00:hello

2018-11-30 00:42:23.40 []gcc -o hello.o -c hello.c
  Sources (55) :
  - 2018-11-30 00:42:19.00:hello.c
  Targets (1) :
  - 2018-11-30 00:42:23.00:hello.o

2018-11-30 00:42:10.20 []gcc -o main.o -c main.c
  Sources (56) :
  - 2018-11-14 23:14:17.00:hello.h
  - 2018-11-24 18:55:04.00:main.c
  Targets (1) :
  - 2018-11-30 00:42:10.00:main.o
```

- Second smk run. Sources are unchanged, target is up to date, so nothing is done:  
> smk MyBuild  

```
```

- Let's remove a file. Only the two commands needed to get `hello` updated are run:  

> rm main.o  
> smk MyBuild  

```
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```

- Let's modify a source. Once more, only the two commands needed to get `hello` updated are run:  

> touch hello.c    
> smk MyBuild  

```
gcc -o hello.o -c hello.c
gcc -o hello hello.o main.o
```

- Another smk run. No file changes, nothing is done:  
> smk MyBuild  

```
```


## More options

| I want to                                                             | Command           |
| --------------------------------------------------------------------- | ----------------- |
| Check what `smk` undestand from MyBuild                               | `smk -lm MyBuild` |
| Run it with explanations                                              | `smk -e MyBuild`  |
| Check what would be run, without running it                           | `smk -n MyBuild`  |
| See what knows `smk` from previous runs regarding sources and targets | `smk -ls MyBuild` |
| What else?                                                            | `smk -h`          |


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

This tool heavily relies on [`strace`](https://en.wikipedia.org/wiki/Strace) utility, and thus is dependent on Linux for now.

I keep OS dependencies as small and localized as possible to ease porting.  
Currently, both dependencies to GNAT for the Spawn procedure, and to the
OS (the strace call itself) are in a short separate procedure in smk-main-run_command.adb.

That said, I do not intend to port smk to other platforms myself, but many OSes provide [utilities similar to strace](https://en.wikipedia.org/wiki/Strace#Similar_tools), so it may be done.  
And obviously, any contribution, including ports, is welcome.

`smk` is currently only tested on my Debian x86_64 box.

## About

This package was created by Lionel Draghi, and is released under [![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

Lionel
