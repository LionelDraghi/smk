"Smkfile" format
================

A `Smkfile` is a simple text file, containing commands.

Here is a simple first exemple of `Smkfile`:
```
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```

`Smkfile` is named after `Makefile` exemple.  
Both term will be used hereafter to avoid any ambiguity regarding the intended file format. 

As the project goal is to greatly simplify the make process, the `Smkfile` format is logically simpler and not fully compatible with the `Makefile` format.

As an illustration, consider the functionnaly equivalent `Makefile` of the previous exemple of `Smkfile`:
```Makefile
hello.o: hello.c
	gcc -o hello.o -c hello.c

main.o: main.c hello.h
	gcc -o main.o -c main.c

hello: hello.o main.o
	gcc -o hello hello.o main.o
```
A lot more verbose, isn't it?  

The good news is that `Smk` can run both file in the same way, so that to avoid rewriting (simple) existing `Makefiles`.

More generally speaking, a `Smkfile` may contains:  

- [Comment lines](#comments)
- [Section lines](#sections)
- [Command lines](#commands)

(Blank lines are ignored).

Comments
--------

`Smk` recognize several comments formats (but single line comments only), so keep your own habit :

- the Ada way, that is starting with `--`, 
- the Java / C# way, starting with `//`, 
- the Shell and Make way, starting with `#`.

(Those format seems to be the most popular for single line comments in programming languages, according to [Rosetta Code](https://rosettacode.org/wiki/Comments))

Comments following a command are not removed, and will be passed to the shell during the spawn :
```
# this comment will be ignored 
gcc -o hello.o -c hello.c # this comment will be passed to gcc!
```

Note that this format is compatible with [Shebang](https://en.wikipedia.org/wiki/Shebang_%28Unix%29) notation, so your `Smkfile` may perfectly be a shell script like this :
```
#!/bin/bash

gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```


Sections
--------
Sections are labels in the file that specificaly designate the following commands (until next section). It allows for exemple to run only the `clean` section of a Makefile.  
Refer to the [Compatibility with Makefile](#compatibility-with-makefile) chapter.


Commands
--------
A command line is whatever line that is neither a blank, Section or Comment line. Once removed heading blanks, tabs and '@' characters, the rest of the line passed 'as is' to the shell to be run.
Shell or Make variable, pattern rule, constructs, etc. are not interpreted. So, don't expect `$(MAKE)`, `%.d: %.c`, `if [ $1 -gt 100 ]`, etc. to have the right behavior.

Compatibility with Makefile
---------------------------

A makefile consists of [“rules” in the following form:](https://en.wikipedia.org/wiki/Makefile#rules)

```Makefile
target: dependencies
    system command(s)
```

1. Makefile command lines start with a tab, and that's OK for `Smk`.
A line may also starts with ‘@’ (in that case, Make suppress the echoing of that line).  
Like `Make`, `Smk` discards the '@' character before the line is passed to the shell (but `smk` will not suppress the echoing).

2. Dependencies are determined automatically by `Smk` so dependencies following `target:` are ignored. 

3. Targets are also determined automatically by `Smk`, but the target also plays another role in Makefile, like in this exemple:
   ```Makefile
   all: hello

   hello: hello.o main.o
       gcc -o hello hello.o main.o

   clean:
       rm -rf *.o

   mrproper: clean
       rm -rf hello
   ```
   A user calling `make` don't expect `mrproper` to be run immediatly after the build. As of version v0.0.2, this is the behavior of `Smk` : all lines are run if needed sequentially, forcing the user to split incompatible sequences of commands in several files (for exemple here build.txt, clean.txt and mrproper.txt).
   
   To avoid this burden, `Smk` will provide in future version the hability to run only the commands following the target. To avoid confusion with targets in the more general meaning, identifier followed by colon (':') will be called *Section* instead.
   
   In this exemple :
   ```
   build: 
       gcc -o hello hello.o main.o
   ```
   `build` is the *section name*, `hello` is the *target*, and both `.o` (and possibly included `.h`) are the *sources*.

4. Targets may also be invoked to to ask for a particular intermediate production.  
   For exemple, in:
   ```
   hello: hello.o main.o
    gcc -o hello hello.o main.o

   hello.o: hello.c
	gcc -o hello.o -c hello.c

   main.o: main.c hello.h
	gcc -o main.o -c main.c
   ```
   One could invoke `make hello.o`.

   In `Smkfile`, targets are not explicitly stated.  
   Future version will provide the hability to:
   1. Ask for the possible target list:  
      `smk --list_target Makefile` (or `smk -lt Makefile`)
   2. Build specifically one of the listed target:  
      `smk Makefile hello.o`

5. Multilines (splitted with backslash) are not yet processed.




