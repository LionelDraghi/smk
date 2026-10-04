# Sections related functions

Test the building of a specific section, the `smkfile:section` notation,
and the error cases on unknown section or smkfile.

The tests are run in a local `hello.c` directory, created by the background,
and containing a tiny C program built with the `Makefile.2` and `Makefile.4`
smkfiles.

_Table of Contents:_
- [Scenario: specific section building](#scenario-specific-section-building)
- [Scenario: unknown section](#scenario-unknown-section)
- [Scenario: smkfile,section notation](#scenario-smkfile-section-notation)
- [Scenario: unknown smkfile with section](#scenario-unknown-smkfile-with-section)

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
- Given the file `hello.c/Makefile.4`
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

- Given I run `../../smk -q reset`
- Given I run `../../smk build -q hello.c/Makefile.2`
- When I run `../../smk :main.o`
- Then I get `Nothing to run`

After `touch main.c`, only the `main.o` section is run:

- When I run `sleep 1`
- When I run `touch hello.c/main.c`
- When I run `../../smk :main.o`
- Then I get
```
gcc -o main.o -c main.c
```

## Scenario : unknown section

- When I run `../../smk :qzdsqdq.o`
- Then I get `No section "qzdsqdq.o" in hello.c/Makefile.2`

## Scenario : smkfile,section notation

Building a section of a given smkfile:

- Given I run `../../smk build -q hello.c/Makefile.4`
- When I run `touch hello.c/hello.c`
- When I run `../../smk hello.c/Makefile.4:hello.o`
- Then I get
```
gcc -o hello.o -c hello.c
```

Running the `mrproper` section:

- When I run `../../smk -a hello.c/Makefile.4:mrproper`
- Then I get
```
rm -rf hello
```

## Scenario : unknown smkfile with section

- When I run `../../smk -a hello.c/Makezzzzzfile.4:mrproper`
- Then I get
```
Error : Unknown Smkfile hello.c/Makezzzzzfile.4 in hello.c/Makezzzzzfile.4:mrproper
Error : No smkfile given, and more than one runfile in dir
```
- Then I get error
