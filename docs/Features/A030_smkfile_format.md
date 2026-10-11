# Smkfile format

Those features describe how smk parses the smkfiles,
beyond the simple one command per line.

_Table of Contents:_
- Feature: Multiline commands
  - [Scenario: multiline single command](#scenario-multiline-single-command)
  - [Scenario: multiline with more commands and pipes](#scenario-multiline-with-more-commands-and-pipes)
  - [Scenario: hill formatted multiline](#scenario-hill-formatted-multiline)

# Feature : Multiline commands

Test the processing of multiline commands, with the backslash
continuation, comments in the middle, and hill formatted files.

The tests are run on smkfiles containing multiline commands with
`gcc` and `wc`, and in a local `hello_c` directory, created by the
background, and containing a tiny C program.


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
- And the file `multiline_smkfile1.txt`
```makefile
	gcc -E -P -xc /dev/null \
		-o out.sloc.png \
# comment in the middle should not get in the way
		-Wall \
		 -O2
```
- And the file `multiline_smkfile2.txt`
```makefile
// multiline with command and pipes
wc -l hello_c/*.c hello_c/*.h | \
grep "hello" \
|sed "s/hello/HELLO/"
		-- comment at the end
```
- And the file `hill_multiline_smkfile.txt`
```makefile
# Hill formatted multiline command:

	gcc -E -P -xc /dev/null \
		-o out.sloc.png \
		-Wall \
// the end of the command is missing

-- Note that the comment immediatly following the command
-- should not be considered as the end of the command, neither
-- should the following blank line or any of the following lines.
```

## Scenario : multiline single command

The multiline `gcc` command, with a comment in the middle,
is run as a single command:

- Given I run `smk -q reset`
- And I run `touch out.sloccount`
- When I run `smk multiline_smkfile1.txt`
- Then I get
```
gcc -E -P -xc /dev/null -o out.sloc.png -Wall -O2
```

## Scenario : multiline with more commands and pipes

The multiline `wc` command, with pipes and a comment at the end:

- Given I run `smk -q reset`
- When I run `smk multiline_smkfile2.txt`
- Then I get
```
wc -l hello_c/*.c hello_c/*.h | grep "hello" |sed "s/hello/HELLO/"
  7 hello_c/hello.c
  9 hello_c/main.c
  6 hello_c/hello.h
```

## Scenario : hill formatted multiline

A hill formatted multiline command, where the end of the command is
missing: the last command is ignored.

- Given I run `smk -q reset`
- When I run `smk hill_multiline_smkfile.txt`
- Then I get
```
Error : hill_multiline_smkfile.txt ends with incomplete multine, last command ignored
Nothing to run
```
- And I get error
