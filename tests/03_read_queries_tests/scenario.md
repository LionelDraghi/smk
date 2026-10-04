# Read queries

Those tests check the queries that read and display what `smk` knows:
the content of a smkfile (`read-smkfile`), and the dump of the previous
run (`status`).

The tests are run in a local `hello.c` directory, created by the background,
and containing a tiny C program built with the `Makefile.2` and `Makefile.3`
smkfiles.

_Table of Contents:_
- [Scenario: read-smkfile](#scenario-read-smkfile)
- [Scenario: status](#scenario-status)
- [Scenario: status, long listing and system files](#scenario-status-long-listing-and-system-files)
- [Scenario: status, no previous run](#scenario-status-no-previous-run)
- [Scenario: status, no smkfile](#scenario-status-no-smkfile)

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

## Scenario : read-smkfile

Read a smkfile and shows what is understud by smk:

- When I run `sh -c "../../smk read-smkfile hello.c/Makefile.3 | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
hello.c/Makefile.3 (YYYY:MM:DD HH:MM:SS.SS) :
4: [hello.o] gcc -o hello.o -c hello.c
7: [main.o] gcc -o main.o -c main.c
10: [hello] gcc -o hello hello.o main.o
14: [clean] rm -rf *.o
17: [mrproper] rm -rf hello
```

## Scenario : status

Read the previous run dump and shows sources and targets.
Dates are neutralized to ease the comparison.

- Given I run `rm -f hello.c/hello hello.c/hello.o hello.c/main.o`
- Given I run `../../smk -q reset`
- When I run `../../smk -q build hello.c/Makefile.2`
- When I run `sh -c "../../smk status | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
Command "gcc -o hello hello.o main.o" in section [hello], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (2)
  - hello.c/hello.o
  - hello.c/main.o
  Targets: (1)
  - hello.c/hello

Command "gcc -o hello.o -c hello.c" in section [hello.o], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (1)
  - hello.c/hello.c
  Targets: (1)
  - hello.c/hello.o

Command "gcc -o main.o -c main.c" in section [main.o], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (2)
  - hello.c/hello.h
  - hello.c/main.c
  Targets: (1)
  - hello.c/main.o
```

## Scenario : status, long listing and system files

Same as previously, with system files not ignored and long form.
The expected listing contains many system files, and is machine dependent:
it is compared to the `expected_lpr1.txt` golden file.

- Given I run `../../smk -q reset`
- Given I run `../../smk -q build hello.c/Makefile.2`
- When I run `sh -c "../../smk st -l -sa | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g' > out.lpr1.txt"`
- Then the file `out.lpr1.txt` is equal to file `expected_lpr1.txt`

## Scenario : status, no previous run

With a runfile for this smkfile, but no previous run:

- Given I run `../../smk -q reset`
- When I run `../../smk status hello.c/Makefile.2`
- Then I get `Error : No previous run found.`
- Then I get error

## Scenario : status, no smkfile

With no smkfile given, and no runfile in the current dir:

- Given I run `../../smk -q reset`
- When I run `../../smk status`
- Then I get `Error : No smkfile given, and no existing runfile in dir`
- Then I get error
