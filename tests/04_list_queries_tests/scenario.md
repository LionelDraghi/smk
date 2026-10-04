# List queries

Those tests check the queries that list what `smk` knows:
the previous runs (`list-runs`), the targets (`list-targets`),
and the sources (`list-sources`).

The tests are run in a local `hello.c` directory, created by the background,
and containing a tiny C program built with the `Makefile.2` and `Makefile.3`
smkfiles.

_Table of Contents:_
- [Scenario: lr, list-runs](#scenario-lr-list-runs)
- [Scenario: lt, list-targets](#scenario-lt-list-targets)
- [Scenario: ls, list-sources](#scenario-ls-list-sources)
- [Scenario: list-sources, show all files](#scenario-list-sources-show-all-files)

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

## Scenario : lr, list-runs

Test available previous runs. After a reset, there is no run file:

- Given I run `../../smk -q reset`
- When I run `../../smk lr`
- Then I get `No run file`

After building `Makefile.2` and `Makefile.3`, both runfiles are listed:

- Given I run `../../smk -q build hello.c/Makefile.2`
- Given I run `../../smk -q build hello.c/Makefile.3`
- When I run `../../smk list-runs`
- Then I get
```
Makefile.2
Makefile.3
```

## Scenario : lt, list-targets

Long form (dates are neutralized to ease the comparison):

- When I run `sh -c "../../smk list-targets --long-listing hello.c/Makefile.2 | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
"gcc -o hello hello.o main.o" [hello] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] hello.c/hello
"gcc -o hello.o -c hello.c" [hello.o] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] hello.c/hello.o
"gcc -o main.o -c main.c" [main.o] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] hello.c/main.o
```

Short form:

- When I run `../../smk lt hello.c/Makefile.2`
- Then I get
```
hello.c/hello
hello.c/hello.o
hello.c/main.o
```

## Scenario : ls, list-sources

Without shortening file names (`-ds`), the paths are absolute, and machine
dependent: the output is compared to the `expected_ls1.txt` golden file.

- When I run `sh -c "../../smk ls -ds hello.c/Makefile.2 > out.ls1.txt"`
- Then the file `out.ls1.txt` is equal to file `expected_ls1.txt`

Short form:

- When I run `../../smk list-sources hello.c/Makefile.2`
- Then I get
```
hello.c/hello.o
hello.c/main.o
hello.c/hello.c
hello.c/hello.h
hello.c/main.c
```

## Scenario : list-sources, show all files

With `--show-all-files`, system files are also listed, and the output is
machine dependent: it is compared to the `expected_las1.txt`
and `expected_las2.txt` golden files.

Short form:

- When I run `sh -c "../../smk list-sources --show-all-files hello.c/Makefile.2 > out.las1.txt"`
- Then the file `out.las1.txt` is equal to file `expected_las1.txt`

Long form, sorted:

- When I run `sh -c "../../smk -l ls -sa hello.c/Makefile.2 | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g' | sort > out.las2.txt"`
- Then the file `out.las2.txt` is equal to file `expected_las2.txt`
