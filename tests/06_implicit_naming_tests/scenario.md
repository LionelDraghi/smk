# Implicit naming

Test that when there is only one run file in the directory, `smk` assumes
it without giving it on the command line.

The tests are run in a local `hello.c` directory, created by the background,
and containing a tiny C program built with the `Makefile.2` and `Makefile.3`
smkfiles.

_Table of Contents:_
- [Scenario: no run file in the directory](#scenario-no-run-file-in-the-directory)
- [Scenario: one run file, implicit run](#scenario-one-run-file-implicit-run)
- [Scenario: more than one run file](#scenario-more-than-one-run-file)

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
- Given the file `hello.c/Makefile.2`
```makefile
all: hello

hello.o: hello.c
	gcc -o hello.o -c hello.c

main.o: main.c hello.h
	gcc -o main.o -c main.c

hello: hello.o main.o
	gcc -o hello hello.o main.o
```
- Given the file `hello.c/Makefile.3`
```makefile
all: hello

hello.o: hello.c
	gcc -o hello.o -c hello.c

main.o: main.c hello.h
	gcc -o main.o -c main.c

hello: hello.o main.o
	gcc -o hello hello.o main.o

# let's add some section that should not be run with the rest
clean:
	rm -rf *.o

mrproper: clean
	rm -rf hello
```

## Scenario : no run file in the directory

Help message, as there is nothing in the directory:

- Given I run `../../smk -q reset`
- When I run `../../smk`
- Then I get `Error : No smkfile given, and no existing runfile in dir`
- Then I get error

## Scenario : one run file, implicit run

`smk` re-runs `Makefile.2`, as it is the only one run file in the dir:

- Given I run `../../smk -q hello.c/Makefile.2`
- When I run `sleep 1`
- When I run `touch hello.c/hello.c`
- When I run `../../smk`
- Then I get
```
gcc -o hello.o -c hello.c
gcc -o hello hello.o main.o
```

## Scenario : more than one run file

There is more than one possible run: `smk` displays the list but don't
do anything else.

- Given I run `../../smk -q hello.c/Makefile.3`
- When I run `../../smk`
- Then I get `Error : No smkfile given, and more than one runfile in dir`
- Then I get error
