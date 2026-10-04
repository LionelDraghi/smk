# Build



Those features describe the build related functions: target
cleaning and selection, sections, the implicit smkfile naming, and the
add and run commands.



_Table of Contents:_
- Feature: Targets
  - [Scenario: dry-run clean](#scenario-dry-run-clean)
  - [Scenario: real clean](#scenario-real-clean)
  - [Scenario: build selected target](#scenario-build-selected-target)
  - [Scenario: build unknown target](#scenario-build-unknown-target)
- Feature: Implicit smkfile naming
  - [Scenario: no run file in the directory](#scenario-no-run-file-in-the-directory)
  - [Scenario: one run file, implicit run](#scenario-one-run-file-implicit-run)
  - [Scenario: more than one run file](#scenario-more-than-one-run-file)
- Feature: Sections
  - [Scenario: specific section building](#scenario-specific-section-building)
  - [Scenario: unknown section](#scenario-unknown-section)
  - [Scenario: smkfile,section notation](#scenario-smkfilesection-notation)
  - [Scenario: unknown smkfile with section](#scenario-unknown-smkfile-with-section)
- Feature: Add and run commands
  - [Scenario: add and build](#scenario-add-and-build)
  - [Scenario: run](#scenario-run)

# Feature : Targets

Those tests check the functions related to targets: cleaning them
(dry run and real), building a selected target, and error on unknown target.

The tests are run in a local `hello_c` directory, created by the background,
and containing a tiny C program built with the `Makefile.1` and `Makefile.2`
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
- And the file `hello_c/Makefile.1`
```makefile
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
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

## Scenario : dry-run clean

Test targets cleaning in dry run: nothing shall be actually deleted.

- Given I run `smk -q reset`
- And I run `smk -q build hello_c/Makefile.2`
- When I run `smk clean --dry-run`
- Then I get
```
Deleting file hello_c/hello
Deleting file hello_c/hello.o
Deleting file hello_c/main.o
```

`--explain` checks that nothing was actually deleted:

- When I run `smk --explain`
- Then I get `Nothing to run`

## Scenario : real clean

- When I run `smk clean`
- Then I get
```
Deleting file hello_c/hello
Deleting file hello_c/hello.o
Deleting file hello_c/main.o
```

`smk -e -mt` checks the effective cleaning:

- When I run `smk -e -mt`
- Then I get
```
run "gcc -o hello.o -c hello.c" because Target file hello_c/hello.o is missing
gcc -o hello.o -c hello.c
run "gcc -o main.o -c main.c" because Target file hello_c/main.o is missing
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because Target file hello_c/hello is missing
gcc -o hello hello.o main.o
```

## Scenario : build selected target

Build one of the targets of the previous run. Note that to avoid any
confusion, `Makefile.1` does not contain any target named `main.o`.

- Given I run `smk -q reset`
- And I run `smk -q hello_c/Makefile.1`
- When I run `smk build main.o`
- Then I get `Nothing to run`

After updating `main.c`, the command building `main.o`, and the commands
using it, are run:

- When I run `sleep 1`
- And I run `touch hello_c/main.c`
- And I run `smk build main.o`
- Then I get
```
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```

## Scenario : build unknown target

- When I run `smk build mainzzzzz.o`
- Then I get
```
Target "mainzzzzz.o" not found
run "smk list-targets" to get a list of possible target
```

# Feature : Implicit smkfile naming

Test that when there is only one run file in the directory, `smk` assumes
it without giving it on the command line.

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

## Scenario : no run file in the directory

Help message, as there is nothing in the directory:

- Given I run `smk -q reset`
- When I run `smk`
- Then I get `Error : No smkfile given, and no existing runfile in dir`
- And I get error
## Scenario : one run file, implicit run

`smk` re-runs `Makefile.2`, as it is the only one run file in the dir:

- Given I run `smk -q hello_c/Makefile.2`
- When I run `sleep 1`
- And I run `touch hello_c/hello.c`
- And I run `smk`
- Then I get
```
gcc -o hello.o -c hello.c
gcc -o hello hello.o main.o
```

## Scenario : more than one run file

There is more than one possible run: `smk` displays the list but don't
do anything else.

- Given I run `smk -q hello_c/Makefile.3`
- When I run `smk`
- Then I get `Error : No smkfile given, and more than one runfile in dir`
- And I get error
# Feature : Sections

Test the building of a specific section, the `smkfile:section` notation,
and the error cases on unknown section or smkfile.

The tests are run in a local `hello_c` directory, created by the background,
and containing a tiny C program built with the `Makefile.2` and `Makefile.4`
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
- And the file `hello_c/Makefile.4`
```makefile
all: hello

# line are not in the natural build order
hello: hello.o main.o
	gcc -o hello hello.o main.o

hello.o: hello.c
	gcc -o hello.o -c hello.c

main.o: main.c hello.h
	gcc -o main.o -c main.c

clean:
	rm -rf *.o

mrproper: clean
	rm -rf hello
```

## Scenario : specific section building

`smk :main.o` with `main.o` up to date:

- Given I run `smk -q reset`
- And I run `smk build -q hello_c/Makefile.2`
- When I run `smk :main.o`
- Then I get `Nothing to run`

After `touch main.c`, only the `main.o` section is run:

- When I run `sleep 1`
- And I run `touch hello_c/main.c`
- And I run `smk :main.o`
- Then I get
```
gcc -o main.o -c main.c
```

## Scenario : unknown section

- When I run `smk :qzdsqdq.o`
- Then I get `No section "qzdsqdq.o" in hello_c/Makefile.2`

## Scenario : smkfile,section notation

Building a section of a given smkfile:

- Given I run `smk build -q hello_c/Makefile.4`
- When I run `touch hello_c/hello.c`
- And I run `smk hello_c/Makefile.4:hello.o`
- Then I get
```
gcc -o hello.o -c hello.c
```

Running the `mrproper` section:

- When I run `smk -a hello_c/Makefile.4:mrproper`
- Then I get
```
rm -rf hello
```

## Scenario : unknown smkfile with section

- When I run `smk -a hello_c/Makezzzzzfile.4:mrproper`
- Then I get
```
Error : Unknown Smkfile hello_c/Makezzzzzfile.4 in hello_c/Makezzzzzfile.4:mrproper
Error : No smkfile given, and more than one runfile in dir
```
- And I get error
# Feature : Add and run commands

Test the `add` and `run` commands, that respectively add a command to
`default.smk`, and add it then run it.

The tests are run in a local `hello_c` directory, created by the background,
and containing a tiny C program.


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

## Scenario : add and build

Commands are added to `default.smk`, then run by `smk build`:

- Given I run `rm -f hello.o main.o hello default.smk`
- And I run `smk -q reset`
- And I run `smk add gcc -c hello_c/main.c`
- And I run `smk add gcc -c hello_c/hello.c`
- And I run `smk add gcc -o hello hello.o main.o`
- When I run `smk build`
- Then I get
```
gcc -c hello_c/main.c
gcc -c hello_c/hello.c
gcc -o hello hello.o main.o
```

## Scenario : run

Each command is added to `default.smk` and immediately run:

- Given I run `rm -f hello.o main.o hello default.smk`
- And I run `smk -q reset`
- When I run `smk run gcc -c hello_c/main.c`
- Then I get `gcc -c hello_c/main.c`
- When I run `smk run gcc -c hello_c/hello.c`
- Then I get `gcc -c hello_c/hello.c`
- When I run `smk run gcc -o hello hello.o main.o`
- Then I get `gcc -o hello hello.o main.o`
