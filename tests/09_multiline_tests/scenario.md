# Multiline commands

Test the processing of multiline commands, with the backslash
continuation, comments in the middle, and hill formatted files.

The tests are run on smkfiles containing `ploticus` and `sloccount`
multiline commands, and in a local `hello.c` directory, created by the
background, and containing a tiny C program.

_Table of Contents:_
- [Scenario: multiline single command](#scenario-multiline-single-command)
- [Scenario: multiline with more commands and pipes](#scenario-multiline-with-more-commands-and-pipes)
- [Scenario: hill formatted multiline](#scenario-hill-formatted-multiline)

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
- Given the file `multiline_smkfile1.txt`
```makefile
	ploticus -prefab pie 	\
		data=out.sloccount labels=2 colors="blue red green orange"	\
# comment in the middle should not get in the way
		explode=0.1 values=1 title="Ada sloc `date +%x`"	\
		 -png -o out.sloc.png
```
- Given the file `multiline_smkfile2.txt`
```makefile
// multiline with command and pipes
sloccount hello.c/* | 	\
grep "ansic=" 			\
|sed "s/ansic/C/"
		-- comment at the end
```
- Given the file `hill_multiline_smkfile.txt`
```makefile
# Hill formatted multiline command:

	ploticus -prefab pie 	\
		data=out.sloccount labels=2 colors="blue red green orange"	\
		explode=0.1 values=1 title="Ada sloc `date +%x`"	\
// the end of the command is missing

-- Note that the comment immediatly following the command
-- should not be considered as the end of the command, neither
-- should the following blank line or any of the following lines.
```

## Scenario : multiline single command

The multiline `ploticus` command, with a comment in the middle,
is run as a single command. `out.sloccount` is the ploticus data file:

- Given I run `../../smk -q reset`
- Given I run `sh -c "sloccount hello.c/* | grep 'ansic=' > out.sloccount"`
- When I run `../../smk multiline_smkfile1.txt`
- Then I get
```
ploticus -prefab pie data=out.sloccount labels=2 colors="blue red green orange" explode=0.1 values=1 title="Ada sloc `date +%x`" -png -o out.sloc.png
```

## Scenario : multiline with more commands and pipes

The multiline `sloccount` command, with pipes and a comment at the end:

- Given I run `../../smk -q reset`
- When I run `../../smk multiline_smkfile2.txt`
- Then I get
```
sloccount hello.c/* | grep "ansic=" |sed "s/ansic/C/"
18      top_dir         C=18
```

## Scenario : hill formatted multiline

A hill formatted multiline command, where the end of the command is
missing: the last command is ignored.

- Given I run `../../smk -q reset`
- When I run `../../smk hill_multiline_smkfile.txt`
- Then I get
```
Error : hill_multiline_smkfile.txt ends with incomplete multine, last command ignored
Nothing to run
```
- Then I get error
