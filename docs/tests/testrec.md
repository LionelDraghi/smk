
# Command line checks



##  Command line checks / Help options


  Test that the -h, --help or no command line will output :

  Run:  
  `smk `  
  `smk --help`  

  Expected:

```

Usage : smk [Options] make_file

Options :
   -a   | --always-make    : unconditionally make all targets
   -e   | --explain        : explain why each target is made
   -n   | --dry-run        : print the commands that would be executed, but do not execute them
   -i   | --ignore-errors  : ignore all errors in commands executed to remake files

   -lm  | --list-makefile  : dump Smk understanding of a Makefile
   -ls  | --list-saved-run : dump what Smk stored of the previous run of this Makefile

          --clean          : remove all local Smk files (equivalent to rm .smk.*)

   -We  | --Warnings=error : treat warnings as errors
   -v   | --verbose
   -q   | --quiet          : no message unless error.
                             Warning are also ignored.
          --version        : Smk version
   -h   | --help           : this message

http://lionel.draghi.free.fr/smk/

```


Command line checks / Help options [Successful](tests_status.md#successful)

##  Command line checks / Version option


  Test that the --version will put :

  Run:  
  `smk --version`

  Expected:

```
0.0.4
```


Command line checks / Version option [Successful](tests_status.md#successful)

##  Command line checks / Unknow Makefile


  Test the error message if an unknow MakeFile is given

  Run:  
  `smk My_Makefile`

  Expected:

```
Error : Unknown Makefile or unknow option My_Makefile
```


Command line checks / Unknow Makefile [Successful](tests_status.md#successful)

# List functions checks



##  List functions checks / --list-makefile


  Test the MakeFile dump

  Makefile:
```
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

  Run:  
  `smk -lm hello.c/Makefile.3`

  Expected:
```
hello.c/Makefile.3 (2018-11-24 03:31:18.00) :
4: [hello.o] gcc -o hello.o -c hello.c
7: [main.o] gcc -o main.o -c main.c
10: [hello] gcc -o hello hello.o main.o
14: [clean] rm -rf *.o
17: [mrproper] rm -rf hello
```


List functions checks / --list-makefile [Successful](tests_status.md#successful)

##  List functions checks / -ls | --list-saved-run


  Test the previous run dump

  Run:  
  `smk -q hello.c/Makefile.2`  
  `smk -ls hello.c/Makefile.2`  

  Expected:
  (note that to ease comparison, dates are removed)
```
YYYY:MM:DD HH:MM:SS.SS [hello] gcc -o hello hello.o main.o
  Sources (20) :
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/hello.o
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/main.o
  Targets (1) :
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/hello

YYYY:MM:DD HH:MM:SS.SS [hello.o] gcc -o hello.o -c hello.c
  Sources (55) :
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/hello.c
  Targets (1) :
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/hello.o

YYYY:MM:DD HH:MM:SS.SS [main.o] gcc -o main.o -c main.c
  Sources (56) :
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/hello.h
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/main.c
  Targets (1) :
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/main.o

```


List functions checks / -ls | --list-saved-run [Successful](tests_status.md#successful)

# Sanity checks



 Makefile:
```
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```

##  Sanity checks / First `smk`, after `make`, should run no command


  Run:  
  `smk -e hello.c/Makefile.2`

  Expected:
```
```


Sanity checks / First `smk`, after `make`, should run no command [Successful](tests_status.md#successful)

##  Sanity checks / Second `smk`, should not run any command


  Run:  
  `smk -e hello.c/Makefile.2`

  Expected:
```
```


Sanity checks / Second `smk`, should not run any command [Successful](tests_status.md#successful)

##  Sanity checks / smk --clean, no more history, should run all commands


  Run:  
  `smk --clean`  
  `smk -e hello.c/Makefile.2`

  Expected:
```
run gcc -o hello.o -c hello.c because it was not run before
gcc -o hello.o -c hello.c
run gcc -o main.o -c main.c because it was not run before
gcc -o main.o -c main.c
run gcc -o hello hello.o main.o because it was not run before
gcc -o hello hello.o main.o
```


Sanity checks / smk --clean, no more history, should run all commands [Successful](tests_status.md#successful)

##  Sanity checks / `smk -a`, should run all commands even if not needed


  Run:  
  `smk -e -a hello.c/Makefile.2`

  Expected:
```
run gcc -o hello.o -c hello.c because -a option is set
gcc -o hello.o -c hello.c
run gcc -o main.o -c main.c because -a option is set
gcc -o main.o -c main.c
run gcc -o hello hello.o main.o because -a option is set
gcc -o hello hello.o main.o
```


Sanity checks / `smk -a`, should run all commands even if not needed [Successful](tests_status.md#successful)

##  Sanity checks / `rm main.o` (missing file)


  Run:  
  `rm hello.c/main.o`  
  `smk -e hello.c/Makefile.2`  

  Expected:
```
run gcc -o main.o -c main.c because /home/lionel/Proj/smk/tests/hello.c/main.o is missing
gcc -o main.o -c main.c
run gcc -o hello hello.o main.o because /home/lionel/Proj/smk/tests/hello.c/main.o (-- ::.) has been updated since last run (-- ::.)
gcc -o hello hello.o main.o
```


Sanity checks / `rm main.o` (missing file) [Successful](tests_status.md#successful)

##  Sanity checks / `touch hello.c` (updated file)


  Run:  
  `rm hello.c/main.o`  
  `smk -e hello.c/Makefile.2`  

  Expected:
```
run gcc -o hello.o -c hello.c because /home/lionel/Proj/smk/tests/hello.c/hello.c (-- ::.) has been updated since last run (-- ::.)
gcc -o hello.o -c hello.c
run gcc -o hello hello.o main.o because /home/lionel/Proj/smk/tests/hello.c/hello.o (-- ::.) has been updated since last run (-- ::.)
gcc -o hello hello.o main.o
```


Sanity checks / `touch hello.c` (updated file) [Successful](tests_status.md#successful)
