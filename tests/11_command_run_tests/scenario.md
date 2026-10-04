# Command Run features

Test the `add` and `run` commands, that respectively add a command to
`default.smk`, and add it then run it.

The tests are run in a local `hello.c` directory, created by the background,
and containing a tiny C program.

_Table of Contents:_
- [Scenario: add and build](#scenario-add-and-build)
- [Scenario: run](#scenario-run)

### Background:

- Given the directory `hello.c`
- Given the file `hello.c/hello.c`
```c
#include <stdio.h>
#include <stdlib.h>

void Hello(void)
{
	printf("Hello World\n");
}
```
- Given the file `hello.c/main.c`
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
- Given the file `hello.c/hello.h`
```c
#ifndef H_GL_HELLO
#define H_GL_HELLO

void Hello(void);

#endif
```

## Scenario : add and build

Commands are added to `default.smk`, then run by `smk build`:

- Given I run `rm -f *.o hello default.smk`
- Given I run `../../smk -q reset`
- Given I run `../../smk add gcc -c hello.c/main.c`
- Given I run `../../smk add gcc -c hello.c/hello.c`
- Given I run `../../smk add gcc -o hello hello.o main.o`
- When I run `../../smk build`
- Then I get
```
gcc -c hello.c/main.c
gcc -c hello.c/hello.c
gcc -o hello hello.o main.o
```

## Scenario : run

Each command is added to `default.smk` and immediately run:

- Given I run `rm -f *.o hello default.smk`
- Given I run `../../smk -q reset`
- When I run `../../smk run gcc -c hello.c/main.c`
- Then I get `gcc -c hello.c/main.c`
- When I run `../../smk run gcc -c hello.c/hello.c`
- Then I get `gcc -c hello.c/hello.c`
- When I run `../../smk run gcc -o hello hello.o main.o`
- Then I get `gcc -o hello hello.o main.o`
