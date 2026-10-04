# Queries



Those features describe the queries that display what
smk knows: the content of a smkfile, the previous runs, the sources and targets.



_Table of Contents:_
- Feature: Read queries
  - [Scenario: read-smkfile](#scenario-read-smkfile)
  - [Scenario: status](#scenario-status)
  - [Scenario: status, long listing and system files](#scenario-status-long-listing-and-system-files)
  - [Scenario: status, no previous run](#scenario-status-no-previous-run)
  - [Scenario: status, no smkfile](#scenario-status-no-smkfile)
- Feature: List queries
  - [Scenario: lr, list-runs](#scenario-lr-list-runs)
  - [Scenario: lt, list-targets](#scenario-lt-list-targets)
  - [Scenario: ls, list-sources](#scenario-ls-list-sources)
  - [Scenario: list-sources, show all files](#scenario-list-sources-show-all-files)

# Feature : Read queries

Those tests check the queries that read and display what `smk` knows:
the content of a smkfile (`read-smkfile`), and the dump of the previous
run (`status`).

The tests are run in a local `hello_c` directory, created by the background,
and containing a tiny C program built with the `Makefile.2` and `Makefile.3`
smkfiles.


### Background:

- Given the directory `hello_c`
- And the file `hello_c/hello.c`
```c
#include <stdio.h>
#include <stdlib.h>

void Hello(void)
{
	printf("Hello World\n");
}
```
- And the file `hello_c/main.c`
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
- And the file `hello_c/hello.h`
```c
#ifndef H_GL_HELLO
#define H_GL_HELLO

void Hello(void);

#endif
```
- And the file `hello_c/Makefile.2`
```makefile
all: hello

hello.o: hello.c
	gcc -o hello.o -c hello.c

main.o: main.c hello.h
	gcc -o main.o -c main.c

hello: hello.o main.o
	gcc -o hello hello.o main.o
```
- And the file `hello_c/Makefile.3`
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

- When I run `sh -c "smk read-smkfile hello_c/Makefile.3 | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
hello_c/Makefile.3 (YYYY:MM:DD HH:MM:SS.SS) :
4: [hello.o] gcc -o hello.o -c hello.c
7: [main.o] gcc -o main.o -c main.c
10: [hello] gcc -o hello hello.o main.o
14: [clean] rm -rf *.o
17: [mrproper] rm -rf hello
```

## Scenario : status

Read the previous run dump and shows sources and targets.
Dates are neutralized to ease the comparison.

- Given I run `rm -f hello_c/hello hello_c/hello.o hello_c/main.o`
- And I run `smk -q reset`
- When I run `smk -q build hello_c/Makefile.2`
- And I run `sh -c "smk status | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
Command "gcc -o hello hello.o main.o" in section [hello], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (2)
  - hello_c/hello.o
  - hello_c/main.o
  Targets: (1)
  - hello_c/hello

Command "gcc -o hello.o -c hello.c" in section [hello.o], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (1)
  - hello_c/hello.c
  Targets: (1)
  - hello_c/hello.o

Command "gcc -o main.o -c main.c" in section [main.o], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (2)
  - hello_c/hello.h
  - hello_c/main.c
  Targets: (1)
  - hello_c/main.o
```

## Scenario : status, long listing and system files

Same as previously, with system files not ignored and long form.
The expected listing contains many system files, and is machine dependent:
it is compared to the `../data/expected_lpr1.txt` golden file.

- Given I run `smk -q reset`
- And I run `smk -q build hello_c/Makefile.2`
- When I run `sh -c "smk st -l -sa | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g' > out.lpr1.txt"`
- Then the file `out.lpr1.txt` is equal to file `../data/expected_lpr1.txt`

## Scenario : status, no previous run

With a runfile for this smkfile, but no previous run:

- Given I run `smk -q reset`
- When I run `smk status hello_c/Makefile.2`
- Then I get `Error : No previous run found.`
- And I get error
## Scenario : status, no smkfile

With no smkfile given, and no runfile in the current dir:

- Given I run `smk -q reset`
- When I run `smk status`
- Then I get `Error : No smkfile given, and no existing runfile in dir`
- And I get error
# Feature : List queries

Those tests check the queries that list what `smk` knows:
the previous runs (`list-runs`), the targets (`list-targets`),
and the sources (`list-sources`).

The tests are run in a local `hello_c` directory, created by the background,
and containing a tiny C program built with the `Makefile.2` and `Makefile.3`
smkfiles.


### Background:

- Given the directory `hello_c`
- And the file `hello_c/hello.c`
```c
#include <stdio.h>
#include <stdlib.h>

void Hello(void)
{
	printf("Hello World\n");
}
```
- And the file `hello_c/main.c`
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
- And the file `hello_c/hello.h`
```c
#ifndef H_GL_HELLO
#define H_GL_HELLO

void Hello(void);

#endif
```
- And the file `hello_c/Makefile.2`
```makefile
all: hello

hello.o: hello.c
	gcc -o hello.o -c hello.c

main.o: main.c hello.h
	gcc -o main.o -c main.c

hello: hello.o main.o
	gcc -o hello hello.o main.o
```
- And the file `hello_c/Makefile.3`
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

## Scenario : lr, list-runs

Test available previous runs. After a reset, there is no run file:

- Given I run `smk -q reset`
- When I run `smk lr`
- Then I get `No run file`

After building `Makefile.2` and `Makefile.3`, both runfiles are listed:

- Given I run `smk -q build hello_c/Makefile.2`
- And I run `smk -q build hello_c/Makefile.3`
- When I run `smk list-runs`
- Then I get
```
Makefile.2
Makefile.3
```

## Scenario : lt, list-targets

Long form (dates are neutralized to ease the comparison):

- When I run `sh -c "smk list-targets --long-listing hello_c/Makefile.2 | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
"gcc -o hello hello.o main.o" [hello] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] hello_c/hello
"gcc -o hello.o -c hello.c" [hello.o] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] hello_c/hello.o
"gcc -o main.o -c main.c" [main.o] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] hello_c/main.o
```

Short form:

- When I run `smk lt hello_c/Makefile.2`
- Then I get
```
hello_c/hello
hello_c/hello.o
hello_c/main.o
```

## Scenario : ls, list-sources

Without shortening file names (`-ds`), the paths are absolute, and machine
dependent: the output is compared to the `../data/expected_ls1.txt` golden file.

- When I run `sh -c "smk ls -ds hello_c/Makefile.2 > out.ls1.txt"`
- Then the file `out.ls1.txt` is equal to file `../data/expected_ls1.txt`

Short form:

- When I run `smk list-sources hello_c/Makefile.2`
- Then I get
```
hello_c/hello.o
hello_c/main.o
hello_c/hello.c
hello_c/hello.h
hello_c/main.c
```

## Scenario : list-sources, show all files

With `--show-all-files`, system files are also listed, and the output is
machine dependent: it is compared to the `../data/expected_las1.txt`
and `../data/expected_las2.txt` golden files.

Short form:

- When I run `sh -c "smk list-sources --show-all-files hello_c/Makefile.2 > out.las1.txt"`
- Then the file `out.las1.txt` is equal to file `../data/expected_las1.txt`

Long form, sorted:

- When I run `sh -c "smk -l ls -sa hello_c/Makefile.2 | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g' | sort > out.las2.txt"`
- Then the file `out.las2.txt` is equal to file `../data/expected_las2.txt`
