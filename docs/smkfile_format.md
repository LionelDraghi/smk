"Smkfile" format
================

- ["Smkfile" format](#%22smkfile%22-format)
  - [Overview](#overview)
  - [Comments](#comments)
  - [Sections](#sections)
  - [Commands](#commands)
  - [Compatibility with Makefile](#compatibility-with-makefile)
    
## Overview

A `Smkfile` is a simple text file, containing commands.

Here is a simple first example of a `Smkfile`:  

```
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```

The `Smkfile` name is derived from the `Makefile` example.  
Both terms will be used hereafter to avoid any ambiguity regarding the intended file format. 

As the project goal is to greatly simplify the make process, the `Smkfile` format is logically much simpler, and thus not fully compatible with the `Makefile` format.

As an illustration, consider the functionally equivalent `Makefile` of the previous example of a `Smkfile`:  

```Makefile
hello.o: hello.c
	gcc -o hello.o -c hello.c

main.o: main.c hello.h
	gcc -o main.o -c main.c

hello: hello.o main.o
	gcc -o hello hello.o main.o
```

A lot more verbose, isn't it?  

The good news is that `Smk` can run both files the same way, so as to avoid rewriting (simple) existing `Makefiles`.

More generally speaking, a `Smkfile` may contain:  

- [Comment lines](#comments)
- [Section lines](#sections)
- [Command lines](#commands)

(Blank lines are ignored).

## Comments

`Smk` recognizes several comment formats (but single line comments only), so keep your own habits:

- the Ada way, that is starting with `--`, 
- the Java / C# way, starting with `//`, 
- the Shell and Make way, starting with `#`.

(Those formats seem to be the most popular for single line comments in programming languages, according to [Rosetta Code](https://rosettacode.org/wiki/Comments))

Comments following a command are not removed, and will be passed to the shell during the spawn:
```
# this comment will be ignored 
gcc -o hello.o -c hello.c # this comment will be passed to gcc!
```

Note that this format is compatible with [Shebang](https://en.wikipedia.org/wiki/Shebang_%28Unix%29) notation, so your `Smkfile` may perfectly be a shell script like this:
```
#!/bin/bash

gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```


## Sections

Sections are labels in the file that specifically designate the following commands (until the next section). This allows for instance to run only the `clean` section of a Makefile. **Fixme: Not yet implemented, as of v0.0.4**  
Refer to the [Compatibility with Makefile](#compatibility-with-makefile) chapter.


## Commands

A command line is whatever line that is neither a blank, Section or Comment line. Once heading blanks, tabs and '@' characters are removed, the rest of the line is passed 'as is' to the shell to be run.
Shell or Make variables, pattern rules, constructs, etc. are not interpreted. So, don't expect `$(MAKE)`, `%.d: %.c`, `if [ $1 -gt 100 ]`, etc. to have the right behavior.

## Compatibility with Makefile

A makefile consists of [“rules” in the following form:](https://en.wikipedia.org/wiki/Makefile#rules)

```Makefile
target: dependencies
    system command(s)
```

1. Makefile command lines start with a tab, and that's OK for `Smk`.
A line may also start with '@' (in that case, `Make` suppresses the echoing of that line).  
Like `Make`, `Smk` discards the '@' character before the line is passed to the shell (but `smk` will not suppress the echoing).

2. Dependencies are determined automatically by `Smk` so dependencies following `target:` are ignored. 

3. Targets are also determined automatically by `Smk`, but the target also plays another role in Makefiles, like in this example:  

```Makefile
all: hello

hello: hello.o main.o
    gcc -o hello hello.o main.o

clean:
    rm -rf *.o

mrproper: clean
    rm -rf hello
```  

A user calling `make` doesn't expect `mrproper` to be run immediately after the build. As of version v0.0.2, this is the behavior of `Smk`: all lines are run if needed sequentially, forcing the user to split incompatible sequences of commands in several files (for example here build.txt, clean.txt and mrproper.txt).
   
To avoid this burden, `Smk` will provide in a future version the ability to run only the commands following the target. To avoid confusion with targets in the more general meaning, an identifier followed by a colon (':') will be called a *Section* instead.
   
In this example:  
   
```Makefile
build: 
    gcc -o hello hello.o main.o
```

`build` is the *section name*, `hello` is the *target*, and both `.o` (and possibly included `.h`) are the *sources*.

4. `Makefile` targets may also be invoked to ask for a particular intermediate production.  
   
   For example, in:  

```Makefile
hello: hello.o main.o
    gcc -o hello hello.o main.o

hello.o: hello.c
    gcc -o hello.o -c hello.c

main.o: main.c hello.h
    gcc -o main.o -c main.c
```  

One could invoke `make hello.o`.

But in `Smkfile`, targets are not explicitly stated.  
This is why `smk` provides a way to:  

- Ask for the list of possible targets:  
   `smk --list-targets Makefile` (or `smk -lt Makefile`)

- Build a specific file from the listed targets:  
   `smk hello.o`  
   **Fixme: Not yet implemented, as of v0.0.4**
   

5. Multiline commands (split with a backslash) are processed.




