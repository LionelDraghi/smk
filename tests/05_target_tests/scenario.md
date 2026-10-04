# Targets related functions

Those tests check the functions related to targets: cleaning them
(dry run and real), building a selected target, and error on unknown target.

The tests are run in a local `hello.c` directory, created by the background,
and containing a tiny C program built with the `Makefile.1` and `Makefile.2`
smkfiles.

_Table of Contents:_
- [Scenario: dry-run clean](#scenario-dry-run-clean)
- [Scenario: real clean](#scenario-real-clean)
- [Scenario: build selected target](#scenario-build-selected-target)
- [Scenario: build unknown target](#scenario-build-unknown-target)

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
- Given the file `hello.c/Makefile.1`
```makefile
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
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

## Scenario : dry-run clean

Test targets cleaning in dry run: nothing shall be actually deleted.

- Given I run `../../smk -q reset`
- Given I run `../../smk -q build hello.c/Makefile.2`
- When I run `../../smk clean --dry-run`
- Then I get
```
Deleting file hello.c/hello
Deleting file hello.c/hello.o
Deleting file hello.c/main.o
```

`--explain` checks that nothing was actually deleted:

- When I run `../../smk --explain`
- Then I get `Nothing to run`

## Scenario : real clean

- When I run `../../smk clean`
- Then I get
```
Deleting file hello.c/hello
Deleting file hello.c/hello.o
Deleting file hello.c/main.o
```

`smk -e -mt` checks the effective cleaning:

- When I run `../../smk -e -mt`
- Then I get
```
run "gcc -o hello.o -c hello.c" because Target file hello.c/hello.o is missing
gcc -o hello.o -c hello.c
run "gcc -o main.o -c main.c" because Target file hello.c/main.o is missing
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because Target file hello.c/hello is missing
gcc -o hello hello.o main.o
```

## Scenario : build selected target

Build one of the targets of the previous run. Note that to avoid any
confusion, `Makefile.1` does not contain any target named `main.o`.

- Given I run `../../smk -q reset`
- Given I run `../../smk -q hello.c/Makefile.1`
- When I run `../../smk build main.o`
- Then I get `Nothing to run`

After updating `main.c`, the command building `main.o`, and the commands
using it, are run:

- When I run `sleep 1`
- When I run `touch hello.c/main.c`
- When I run `../../smk build main.o`
- Then I get
```
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```

## Scenario : build unknown target

- When I run `../../smk build mainzzzzz.o`
- Then I get
```
Target "mainzzzzz.o" not found
run "smk list-targets" to get a list of possible target
```
