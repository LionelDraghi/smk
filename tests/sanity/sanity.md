# Sanity tests

Those tests check the basic smk behavior: sources and targets identification,
and the re-run or not of the commands. They are the non regression tests of
the core algorithm.

_Table of Contents:_
- [Scenario: First `smk`, after `make`, should run no command](#scenario-first-smk-after-make-should-run-no-command)
- [Scenario: Second `smk`, should not run any command](#scenario-second-smk-should-not-run-any-command)
- [Scenario: `smk reset`, no more history, should run all commands](#scenario-smk-reset-no-more-history-should-run-all-commands)
- [Scenario: `smk -a`, should run all commands even if not needed](#scenario-smk-a-should-run-all-commands-even-if-not-needed)
- [Scenario: `rm main.o` (missing file)](#scenario-rm-maino-missing-file)
- [Scenario: `touch hello.c` (updated file)](#scenario-touch-helloc-updated-file)
- [Scenario: `touch hello.c` and dry run](#scenario-touch-helloc-and-dry-run)

Those tests check the basic smk behavior: sources and targets identification,
re-run or not of the commands.

The tests are run in a local `hello_c` directory, created by the background,
and containing a tiny C program built with the `Makefile.2` smkfile.


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

## Scenario : First `smk`, after `make`, should run no command

Run `smk -q hello_c/Makefile.2` (all commands are run), then
`smk -e hello_c/Makefile.2`: nothing should be left to do.

- Given I run `rm -f hello_c/hello hello_c/hello.o hello_c/main.o`
- And I run `smk reset --quiet`
- When I run `smk -q hello_c/Makefile.2`
- And I run `smk -e hello_c/Makefile.2`
- Then I get
```
Nothing to run
```

## Scenario : Second `smk`, should not run any command

- When I run `smk -e hello_c/Makefile.2`
- Then I get
```
Nothing to run
```

## Scenario : `smk reset`, no more history, should run all commands

- When I run `smk reset --quiet`
- And I run `smk -e hello_c/Makefile.2`
- Then I get
```
run "gcc -o hello.o -c hello.c" because it was not run before
gcc -o hello.o -c hello.c
run "gcc -o main.o -c main.c" because it was not run before
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because it was not run before
gcc -o hello hello.o main.o
```

## Scenario : `smk -a`, should run all commands even if not needed

- When I run `smk -e -a hello_c/Makefile.2`
- Then I get
```
run "gcc -o hello.o -c hello.c" because -a option is set
gcc -o hello.o -c hello.c
run "gcc -o main.o -c main.c" because -a option is set
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because -a option is set
gcc -o hello hello.o main.o
```

## Scenario : `rm main.o` (missing file)

A sleep is needed before this test because of close consecutive smk runs
that disrupt the algorithm (time stamp resolution).

- When I run `sleep 1`
- And I run `rm hello_c/main.o`
- And I run `sh -c "smk -e --missing-targets hello_c/Makefile.2 | sed 's/[0-9]//g'"`
- Then I get
```
run "gcc -o main.o -c main.c" because Target file hello_c/main.o is missing
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because Source file hello_c/main.o has been updated (-- ::.)
gcc -o hello hello.o main.o
```

## Scenario : `touch hello.c` (updated file)

- When I run `sleep 1`
- And I run `touch hello_c/hello.c`
- And I run `sh -c "smk -e hello_c/Makefile.2 | sed 's/[0-9]//g'"`
- Then I get
```
run "gcc -o hello.o -c hello.c" because Source file hello_c/hello.c has been updated (-- ::.)
gcc -o hello.o -c hello.c
run "gcc -o hello hello.o main.o" because Source file hello_c/hello.o has been updated (-- ::.)
gcc -o hello hello.o main.o
```

## Scenario : `touch hello.c` and dry run

`whatsnew` lists the changes since the last run:

- When I run `sleep 1.0`
- And I run `touch hello_c/hello.c`
- And I run `smk whatsnew hello_c/Makefile.2`
- Then I get
```
[Updated] [Source] hello_c/hello.c
```

`smk -e -n` is a dry run: the commands are printed, but not executed:

- When I run `sh -c "smk -e -n | sed 's/[0-9]//g'"`
- Then I get
```
run "gcc -o hello.o -c hello.c" because Source file hello_c/hello.c has been updated (-- ::.)
> gcc -o hello.o -c hello.c
```

`whatsnew` should still return the same, as nothing was changed by the
previous run with `-n`:

- When I run `sh -c "smk wn | sed 's/[0-9]//g'"`
- Then I get
```
[Updated] [Source] hello_c/hello.c
```

Run the same without `-n`:

- When I run `sh -c "smk -e hello_c/Makefile.2 | sed 's/[0-9]//g'"`
- Then I get
```
run "gcc -o hello.o -c hello.c" because Source file hello_c/hello.c has been updated (-- ::.)
gcc -o hello.o -c hello.c
run "gcc -o hello hello.o main.o" because Source file hello_c/hello.o has been updated (-- ::.)
gcc -o hello hello.o main.o
```

And now `whatsnew` returns null:

- When I run `sh -c "smk wn | sed 's/[0-9]//g'"`
- Then I get
```
Nothing new
```
