# Run errors

Test the `-k` (keep going) and `-i` (ignore errors) behavior when a run
command fails.

The tests are run in a local `hello.c` directory, created by the background,
and containing a tiny C program. The `Wrong_Makefile` smkfile contains a
command with an invalid option, making it fail.

Tool messages are checked with the `C` locale, so that they don't depend
on the machine language settings.

_Table of Contents:_
- [Scenario: no option](#scenario-no-option)
- [Scenario: keep going](#scenario-keep-going)
- [Scenario: ignore errors](#scenario-ignore-errors)
- [Scenario: keep going and ignore errors](#scenario-keep-going-and-ignore-errors)
- [Scenario: run command fails](#scenario-run-command-fails)
- [Scenario: other commands after a failed run](#scenario-other-commands-after-a-failed-run)
- [Scenario: debug option](#scenario-debug-option)

### Background:

- Given the environment variable `LC_ALL` is `C`
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
- Given the file `hello.c/Wrong_Makefile`
```makefile
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
gcc -o hello hello.o main.o
```

## Scenario : no option

Without option, `smk` stops on the first command that fails, and returns
an error code:

- Given I run `../../smk -q reset`
- When I run `../../smk hello.c/Wrong_Makefile`
- Then I get
```
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
Error : Spawn failed for gcc -o main.o -c main.c --WTF
```
- Then the error output is
```
gcc: error: unrecognized command-line option '--WTF'
```
- Then I get error

## Scenario : keep going

With `-k`, `smk` runs the other commands, and returns an error code:

- Given I run `../../smk -q reset`
- When I run `../../smk -k hello.c/Wrong_Makefile`
- Then I get
```
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
Error : Spawn failed for gcc -o main.o -c main.c --WTF
gcc -o hello hello.o main.o
Error : Spawn failed for gcc -o hello hello.o main.o
```
- Then the error output is
```
gcc: error: unrecognized command-line option '--WTF'
/usr/bin/x86_64-linux-gnu-ld.bfd: cannot find main.o: No such file or directory
collect2: error: ld returned 1 exit status
```
- Then I get error

## Scenario : ignore errors

Same as with `-k`, but without returning an error code:

- Given I run `../../smk -q reset`
- When I run `../../smk -i hello.c/Wrong_Makefile`
- Then I get
```
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
Error : Spawn failed for gcc -o main.o -c main.c --WTF
```
- Then the error output is
```
gcc: error: unrecognized command-line option '--WTF'
```
- Then I get no error

## Scenario : keep going and ignore errors

With both! Same as with `-k`, but without returning an error code:

- Given I run `../../smk -q reset`
- When I run `../../smk --keep-going --ignore-errors hello.c/Wrong_Makefile`
- Then I get
```
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
Error : Spawn failed for gcc -o main.o -c main.c --WTF
gcc -o hello hello.o main.o
Error : Spawn failed for gcc -o hello hello.o main.o
```
- Then the error output is
```
gcc: error: unrecognized command-line option '--WTF'
/usr/bin/x86_64-linux-gnu-ld.bfd: cannot find main.o: No such file or directory
collect2: error: ld returned 1 exit status
```
- Then I get no error

## Scenario : run command fails

- Given I run `../../smk -q reset`
- When I run `../../smk run non_existing_command`
- Then I get
```
non_existing_command
Error : Spawn failed for non_existing_command
```
- Then the error output is
```
/usr/bin/strace: Cannot find executable 'non_existing_command'
```
- Then I get error

`default.smk` should nevertheless contains the failed command:

- Then the file `default.smk` contains `non_existing_command`

## Scenario : other commands after a failed run

- When I run `sh -c "../../smk read-smkfile | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
default.smk (YYYY:MM:DD HH:MM:SS.SS) :
1: [] non_existing_command
```

- When I run `../../smk status`
- Then I get `No recorded run`

Other commands should return nothing, except `whatsnew`:

- When I run `../../smk whatsnew`
- Then I get `Nothing new`
- When I run `../../smk list-sources`
- Then there is no output
- When I run `../../smk list-targets`
- Then there is no output
- When I run `../../smk list-unused`
- Then there is no output

## Scenario : debug option

The `-d` debug option dumps the settings, that contain machine dependent
absolute paths: the output is compared to the `expected.13` golden file.

- Given I run `../../smk -q reset`
- Given I run `rm -f default.smk`
- When I run `sh -c "../../smk -d dump > out.13 2>&1"`
- Then the file `out.13` is equal to file `expected.13`
- Then I get error
