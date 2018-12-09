
# Sanity



 Makefile:  
```  
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```  

##  Sanity / First `smk`, after `make`, should run no command


  Run:  
  `smk -q hello.c/Makefile.2`  
  `smk -e hello.c/Makefile.2`  

  Expected:  
```  
Nothing to run
```  


Sanity / First `smk`, after `make`, should run no command [Successful](tests_status.md#successful)

##  Sanity / Second `smk`, should not run any command


  Run:  
  `smk -e hello.c/Makefile.2`  

  Expected:  
```  
Nothing to run
```  


Sanity / Second `smk`, should not run any command [Successful](tests_status.md#successful)

##  Sanity / `smk --reset`, no more history, should run all commands


  Run:  
  `smk --reset --quiet`  
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


Sanity / `smk --reset`, no more history, should run all commands [Successful](tests_status.md#successful)

##  Sanity / `smk -a`, should run all commands even if not needed


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


Sanity / `smk -a`, should run all commands even if not needed [Successful](tests_status.md#successful)

##  Sanity / `rm main.o` (missing file)


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


Sanity / `rm main.o` (missing file) [Successful](tests_status.md#successful)

##  Sanity / `touch hello.c` (updated file)


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


Sanity / `touch hello.c` (updated file) [Successful](tests_status.md#successful)

# Command line



##  Command line / Help options


  Test the -h and --help output :  

  Run:  
  `smk `  
  `smk --help`  

  Expected:  

```  

Usage : smk Query [Options]* [Smkfile]

Usual use example:
   when run the first time:
   > smk MyBuild.txt
   and then just:
   > smk
   to rebuild

Queries :
   -rs  | --read-smkfile  : shows Smk understanding of a Smkfile
   -rl  | --read-last-run : shows what smk knows about the previous runs
                               (commands, sources and targets)
   -lr  | --list-runs     : list runfiles in current directory
   -ls  | --list-sources  : list sources, except system files
   -lt  | --list-targets
          --clean         : remove all targets files
          --reset         : remove all local Smk files (equivalent to rm .smk.*)
          --version       : Smk version
   -h   | --help          : this message
   -b   | --build         : run the build
   NB : when no query is given, --build is assumed

Options :
   -a   | --always-make     : unconditionally make all targets
   -e   | --explain         : explain why each target is made
   -n   | --dry-run         : print the commands that would be executed, but do not execute them
   -sa  | --shows-all-files : prevent -ls and -rl to ignore system files
   -i   | --ignore-errors   : ignore all errors in commands executed to remake files
   -k   | --keep-going      : Do as much work as possible
   -We  | --Warnings=error  : treat warnings as errors
   -v   | --verbose
   -q   | --quiet           : no message unless error,
                              Warning are also ignored

http://lionel.draghi.free.fr/smk/

```  


Command line / Help options [Successful](tests_status.md#successful)

##  Command line / Version option


  Test that the --version will put :  

  Run:  
  `smk --version`  

  Expected:  

```  
0.0.5
```  


Command line / Version option [Successful](tests_status.md#successful)

##  Command line / Illegal cmd lines


  Run:  
  `smk -rs -rl`  

  Expected:  

```  
Error : More than one query on command line : READ_LAST_RUN and READ_SMKFILE

Usage : smk Query [Options]* [Smkfile]

Usual use example:
   when run the first time:
   > smk MyBuild.txt
   and then just:
   > smk
   to rebuild

Queries :
   -rs  | --read-smkfile  : shows Smk understanding of a Smkfile
   -rl  | --read-last-run : shows what smk knows about the previous runs
                               (commands, sources and targets)
   -lr  | --list-runs     : list runfiles in current directory
   -ls  | --list-sources  : list sources, except system files
   -lt  | --list-targets
          --clean         : remove all targets files
          --reset         : remove all local Smk files (equivalent to rm .smk.*)
          --version       : Smk version
   -h   | --help          : this message
   -b   | --build         : run the build
   NB : when no query is given, --build is assumed

Options :
   -a   | --always-make     : unconditionally make all targets
   -e   | --explain         : explain why each target is made
   -n   | --dry-run         : print the commands that would be executed, but do not execute them
   -sa  | --shows-all-files : prevent -ls and -rl to ignore system files
   -i   | --ignore-errors   : ignore all errors in commands executed to remake files
   -k   | --keep-going      : Do as much work as possible
   -We  | --Warnings=error  : treat warnings as errors
   -v   | --verbose
   -q   | --quiet           : no message unless error,
                              Warning are also ignored

http://lionel.draghi.free.fr/smk/

```  


Command line / Illegal cmd lines [Successful](tests_status.md#successful)

##  Command line / Unknow Makefile


  Test the error message if an unknow MakeFile is given  

  Run:  
  `smk My_Makefile`  

  Expected:  

```  
Error : Unknown Makefile or unknow option My_Makefile
```  


Command line / Unknow Makefile [Successful](tests_status.md#successful)

# Read queries



##  Read queries / -rs | --read-smkfile


  Read a smkfile and shows what is understud by smk  

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
  `smk -rs hello.c/Makefile.3`  

  Expected:  
```  
hello.c/Makefile.3 (YYYY:MM:DD HH:MM:SS.SS) :
4: [hello.o] gcc -o hello.o -c hello.c
7: [main.o] gcc -o main.o -c main.c
10: [hello] gcc -o hello hello.o main.o
14: [clean] rm -rf *.o
17: [mrproper] rm -rf hello
```  


Read queries / -rs | --read-smkfile [Successful](tests_status.md#successful)

##  Read queries / -rl | --read-last-run


  Read the previous run dump and shows sources and targets  

  Run:  
  `smk -q --reset`  
  `smk -q --build hello.c/Makefile.2`  
  `smk -rl hello.c/Makefile.2`  

  Expected:  
  (note that to ease comparison, dates are removed)  
```  
YYYY:MM:DD HH:MM:SS.SS [hello] gcc -o hello hello.o main.o
  Sources (2) :
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/hello.o
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/main.o
  Targets (1) :
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/hello

YYYY:MM:DD HH:MM:SS.SS [hello.o] gcc -o hello.o -c hello.c
  Sources (1) :
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/hello.c
  Targets (1) :
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/hello.o

YYYY:MM:DD HH:MM:SS.SS [main.o] gcc -o main.o -c main.c
  Sources (2) :
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/hello.h
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/main.c
  Targets (1) :
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/main.o

```  

  Run: (same with system files not ignored)   
  `smk -q --reset`  
  `smk -q --build hello.c/Makefile.2`  
  `smk -rl -sa hello.c/Makefile.2`  

  Expected:  
  (note that to ease comparison, dates are removed)  
```  
YYYY:MM:DD HH:MM:SS.SS [hello] gcc -o hello hello.o main.o
  Sources (20) :
  - YYYY:MM:DD HH:MM:SS.SS:/etc/ld.so.cache
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/hello.o
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/main.o
  - YYYY:MM:DD HH:MM:SS.SS:/lib/x86_64-linux-gnu/ld-2.27.so
  - YYYY:MM:DD HH:MM:SS.SS:/lib/x86_64-linux-gnu/libc-2.27.so
  - YYYY:MM:DD HH:MM:SS.SS:/lib/x86_64-linux-gnu/libdl-2.27.so
  - YYYY:MM:DD HH:MM:SS.SS:/lib/x86_64-linux-gnu/libm-2.27.so
  - YYYY:MM:DD HH:MM:SS.SS:/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/crtbegin.o
  - YYYY:MM:DD HH:MM:SS.SS:/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/crtend.o
  - YYYY:MM:DD HH:MM:SS.SS:/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/libgcc.a
  - YYYY:MM:DD HH:MM:SS.SS:/opt/GNAT/2018/lib64/libgcc_s.so
  - YYYY:MM:DD HH:MM:SS.SS:/opt/GNAT/2018/lib64/libgcc_s.so.1
  - YYYY:MM:DD HH:MM:SS.SS:/opt/GNAT/2018/libexec/gcc/x86_64-pc-linux-gnu/7.3.1/liblto_plugin.so.0.0.0
  - YYYY:MM:DD HH:MM:SS.SS:/usr/lib/locale/locale-archive
  - YYYY:MM:DD HH:MM:SS.SS:/usr/lib/x86_64-linux-gnu/crt1.o
  - YYYY:MM:DD HH:MM:SS.SS:/usr/lib/x86_64-linux-gnu/crti.o
  - YYYY:MM:DD HH:MM:SS.SS:/usr/lib/x86_64-linux-gnu/crtn.o
  - YYYY:MM:DD HH:MM:SS.SS:/usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
  - YYYY:MM:DD HH:MM:SS.SS:/usr/lib/x86_64-linux-gnu/libc.so
  - YYYY:MM:DD HH:MM:SS.SS:/usr/lib/x86_64-linux-gnu/libc_nonshared.a
  Targets (1) :
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/hello

YYYY:MM:DD HH:MM:SS.SS [hello.o] gcc -o hello.o -c hello.c
  Sources (55) :
  - YYYY:MM:DD HH:MM:SS.SS:/etc/ld.so.cache
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/hello.c
  - YYYY:MM:DD HH:MM:SS.SS:/lib/x86_64-linux-gnu/libc-2.27.so
  - YYYY:MM:DD HH:MM:SS.SS:/lib/x86_64-linux-gnu/libdl-2.27.so
  - YYYY:MM:DD HH:MM:SS.SS:/lib/x86_64-linux-gnu/libm-2.27.so
  - YYYY:MM:DD HH:MM:SS.SS:/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stdarg.h
  - YYYY:MM:DD HH:MM:SS.SS:/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stddef.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/alloca.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/endian.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/features.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/stdc-predef.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/stdio.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/stdlib.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/_G_config.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/byteswap-16.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/byteswap.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/endian.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/floatn-common.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/floatn.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/libc-header-start.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/libio.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/long-double.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/select.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/stdint-intn.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/stdio_lim.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/stdlib-float.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/sys_errlist.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/sysmacros.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/FILE.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/__FILE.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/clock_t.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/time_t.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/timer_t.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/typesizes.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/uintn-identity.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/waitflags.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/waitstatus.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/wordsize.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/gnu/stubs-64.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/gnu/stubs.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/sys/cdefs.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/sys/select.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/sys/sysmacros.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/sys/types.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/lib/locale/locale-archive
  Targets (1) :
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/hello.o

YYYY:MM:DD HH:MM:SS.SS [main.o] gcc -o main.o -c main.c
  Sources (56) :
  - YYYY:MM:DD HH:MM:SS.SS:/etc/ld.so.cache
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/hello.h
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/main.c
  - YYYY:MM:DD HH:MM:SS.SS:/lib/x86_64-linux-gnu/libc-2.27.so
  - YYYY:MM:DD HH:MM:SS.SS:/lib/x86_64-linux-gnu/libdl-2.27.so
  - YYYY:MM:DD HH:MM:SS.SS:/lib/x86_64-linux-gnu/libm-2.27.so
  - YYYY:MM:DD HH:MM:SS.SS:/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stdarg.h
  - YYYY:MM:DD HH:MM:SS.SS:/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stddef.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/alloca.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/endian.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/features.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/stdc-predef.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/stdio.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/stdlib.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/_G_config.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/byteswap-16.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/byteswap.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/endian.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/floatn-common.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/floatn.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/libc-header-start.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/libio.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/long-double.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/select.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/stdint-intn.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/stdio_lim.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/stdlib-float.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/sys_errlist.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/sysmacros.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/FILE.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/__FILE.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/clock_t.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/time_t.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/types/timer_t.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/typesizes.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/uintn-identity.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/waitflags.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/waitstatus.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/bits/wordsize.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/gnu/stubs-64.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/gnu/stubs.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/sys/cdefs.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/sys/select.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/sys/sysmacros.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/include/x86_64-linux-gnu/sys/types.h
  - YYYY:MM:DD HH:MM:SS.SS:/usr/lib/locale/locale-archive
  Targets (1) :
  - YYYY:MM:DD HH:MM:SS.SS:/home/lionel/Proj/smk/tests/hello.c/main.o

```  


  Run:  
  `smk -q --reset`  
  `smk -rl hello.c/Makefile.2`  

  Expected:  
```  
Error : No previous run found.
```  


  Run:  
  `smk --read-last-run`  

  Expected:  
```  
Error : No smkfile given, and no existing runfile in dir
```  


Read queries / -rl | --read-last-run [Successful](tests_status.md#successful)

# List queries



##  List queries / -lr | --list-runs


  Test available previous runs  

  Run:  
  `smk -q --reset`  
  `smk -lr`  

  Expected:  
```  
No run file
```  

  Run:  
  `smk -q -b hello.c/Makefile.2`  
  `smk -q hello.c/Makefile.3`  
  `smk --list-runs`  

  Expected:  
```  
Makefile.2
Makefile.3
```  


List queries / -lr | --list-runs [Successful](tests_status.md#successful)

##  List queries / -lt | --list-targets

  Run:  
  `smk -lt hello.c/Makefile.2`  
  `smk --list-targets hello.c/Makefile.2`  

  Expected:  
```  
[hello]gcc -o hello hello.o main.o:/home/lionel/Proj/smk/tests/hello.c/hello
[hello.o]gcc -o hello.o -c hello.c:/home/lionel/Proj/smk/tests/hello.c/hello.o
[main.o]gcc -o main.o -c main.c:/home/lionel/Proj/smk/tests/hello.c/main.o
```  


List queries / -lt | --list-targets [Successful](tests_status.md#successful)

##  List queries / -ls | --list-sources


  Run:  
  `smk -ls hello.c/Makefile.2`  
  `smk --list-sources hello.c/Makefile.2`  

  Expected:  
```  
[hello]gcc -o hello hello.o main.o:/home/lionel/Proj/smk/tests/hello.c/hello.o
[hello]gcc -o hello hello.o main.o:/home/lionel/Proj/smk/tests/hello.c/main.o
[hello.o]gcc -o hello.o -c hello.c:/home/lionel/Proj/smk/tests/hello.c/hello.c
[main.o]gcc -o main.o -c main.c:/home/lionel/Proj/smk/tests/hello.c/hello.h
[main.o]gcc -o main.o -c main.c:/home/lionel/Proj/smk/tests/hello.c/main.c
```  


List queries / -ls | --list-sources [Successful](tests_status.md#successful)

##  List queries / -ls | --list-sources --show-all-files


  Run:  
  `smk -las hello.c/Makefile.2`  
  `smk --list-sources --shows-system-files hello.c/Makefile.2`  

  Expected:  
```  
[hello]gcc -o hello hello.o main.o:/etc/ld.so.cache
[hello]gcc -o hello hello.o main.o:/home/lionel/Proj/smk/tests/hello.c/hello.o
[hello]gcc -o hello hello.o main.o:/home/lionel/Proj/smk/tests/hello.c/main.o
[hello]gcc -o hello hello.o main.o:/lib/x86_64-linux-gnu/ld-2.27.so
[hello]gcc -o hello hello.o main.o:/lib/x86_64-linux-gnu/libc-2.27.so
[hello]gcc -o hello hello.o main.o:/lib/x86_64-linux-gnu/libdl-2.27.so
[hello]gcc -o hello hello.o main.o:/lib/x86_64-linux-gnu/libm-2.27.so
[hello]gcc -o hello hello.o main.o:/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/crtbegin.o
[hello]gcc -o hello hello.o main.o:/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/crtend.o
[hello]gcc -o hello hello.o main.o:/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/libgcc.a
[hello]gcc -o hello hello.o main.o:/opt/GNAT/2018/lib64/libgcc_s.so
[hello]gcc -o hello hello.o main.o:/opt/GNAT/2018/lib64/libgcc_s.so.1
[hello]gcc -o hello hello.o main.o:/opt/GNAT/2018/libexec/gcc/x86_64-pc-linux-gnu/7.3.1/liblto_plugin.so.0.0.0
[hello]gcc -o hello hello.o main.o:/usr/lib/locale/locale-archive
[hello]gcc -o hello hello.o main.o:/usr/lib/x86_64-linux-gnu/crt1.o
[hello]gcc -o hello hello.o main.o:/usr/lib/x86_64-linux-gnu/crti.o
[hello]gcc -o hello hello.o main.o:/usr/lib/x86_64-linux-gnu/crtn.o
[hello]gcc -o hello hello.o main.o:/usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
[hello]gcc -o hello hello.o main.o:/usr/lib/x86_64-linux-gnu/libc.so
[hello]gcc -o hello hello.o main.o:/usr/lib/x86_64-linux-gnu/libc_nonshared.a
[hello.o]gcc -o hello.o -c hello.c:/etc/ld.so.cache
[hello.o]gcc -o hello.o -c hello.c:/home/lionel/Proj/smk/tests/hello.c/hello.c
[hello.o]gcc -o hello.o -c hello.c:/lib/x86_64-linux-gnu/libc-2.27.so
[hello.o]gcc -o hello.o -c hello.c:/lib/x86_64-linux-gnu/libdl-2.27.so
[hello.o]gcc -o hello.o -c hello.c:/lib/x86_64-linux-gnu/libm-2.27.so
[hello.o]gcc -o hello.o -c hello.c:/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stdarg.h
[hello.o]gcc -o hello.o -c hello.c:/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stddef.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/alloca.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/endian.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/features.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/stdc-predef.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/stdio.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/stdlib.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/_G_config.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/byteswap-16.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/byteswap.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/endian.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/floatn-common.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/floatn.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/libc-header-start.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/libio.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/long-double.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/select.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/stdint-intn.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/stdio_lim.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/stdlib-float.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/sys_errlist.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/sysmacros.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/types.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/types/FILE.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/types/__FILE.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/types/clock_t.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/types/time_t.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/types/timer_t.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/typesizes.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/uintn-identity.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/waitflags.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/waitstatus.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/bits/wordsize.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/gnu/stubs-64.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/gnu/stubs.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/sys/cdefs.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/sys/select.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/sys/sysmacros.h
[hello.o]gcc -o hello.o -c hello.c:/usr/include/x86_64-linux-gnu/sys/types.h
[hello.o]gcc -o hello.o -c hello.c:/usr/lib/locale/locale-archive
[main.o]gcc -o main.o -c main.c:/etc/ld.so.cache
[main.o]gcc -o main.o -c main.c:/home/lionel/Proj/smk/tests/hello.c/hello.h
[main.o]gcc -o main.o -c main.c:/home/lionel/Proj/smk/tests/hello.c/main.c
[main.o]gcc -o main.o -c main.c:/lib/x86_64-linux-gnu/libc-2.27.so
[main.o]gcc -o main.o -c main.c:/lib/x86_64-linux-gnu/libdl-2.27.so
[main.o]gcc -o main.o -c main.c:/lib/x86_64-linux-gnu/libm-2.27.so
[main.o]gcc -o main.o -c main.c:/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stdarg.h
[main.o]gcc -o main.o -c main.c:/opt/GNAT/2018/lib/gcc/x86_64-pc-linux-gnu/7.3.1/include/stddef.h
[main.o]gcc -o main.o -c main.c:/usr/include/alloca.h
[main.o]gcc -o main.o -c main.c:/usr/include/endian.h
[main.o]gcc -o main.o -c main.c:/usr/include/features.h
[main.o]gcc -o main.o -c main.c:/usr/include/stdc-predef.h
[main.o]gcc -o main.o -c main.c:/usr/include/stdio.h
[main.o]gcc -o main.o -c main.c:/usr/include/stdlib.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/_G_config.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/byteswap-16.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/byteswap.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/endian.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/floatn-common.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/floatn.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/libc-header-start.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/libio.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/long-double.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/select.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/stdint-intn.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/stdio_lim.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/stdlib-float.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/sys_errlist.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/sysmacros.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/types.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/types/FILE.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/types/__FILE.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/types/clock_t.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/types/time_t.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/types/timer_t.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/typesizes.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/uintn-identity.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/waitflags.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/waitstatus.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/bits/wordsize.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/gnu/stubs-64.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/gnu/stubs.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/sys/cdefs.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/sys/select.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/sys/sysmacros.h
[main.o]gcc -o main.o -c main.c:/usr/include/x86_64-linux-gnu/sys/types.h
[main.o]gcc -o main.o -c main.c:/usr/lib/locale/locale-archive
```  


List queries / -ls | --list-sources --show-all-files [Successful](tests_status.md#successful)

# Implicit naming



##  Implicit naming / Implicit naming


  Test that when there is only one run file in the directory, smk assume it without giving it on the command line.  


  Run:  
  `smk -q --reset`  
  `smk`  

  Expected: help message as there is nothing in the directory  

```  
Error : No smkfile given, and no existing runfile in dir
```  

  Run:  
  `smk -q hello.c/Makefile.2`  
  `touch hello.c/hello.c`  
  `smk`  

  Expected: smk re-run Makefile.2, as it is the only one in the dir  
```  
gcc -o hello.o -c hello.c
gcc -o hello hello.o main.o
```  

  Run:  
  `smk -q hello.c/Makefile.3`  
  `smk `  

  Expected:  
     There is more than one possible run, smk display the list but don't do anything else   
```  
Error : No smkfile given, and more than one runfile in dir
```  


Implicit naming / Implicit naming [Successful](tests_status.md#successful)

# Targets related functions



##  Targets related functions / --clean


  Test targets cleaning  

  Run:  
  `smk --reset`  
  `smk -q -b hello.c/Makefile.2`  
  `smk --clean --dry-run`  
  `smk --explain`  (to check that nothing was actually deleted)  

  Expected:  
```  
Deleting /home/lionel/Proj/smk/tests/hello.c/hello
Deleting /home/lionel/Proj/smk/tests/hello.c/hello.o
Deleting /home/lionel/Proj/smk/tests/hello.c/main.o
```  

  Run:  
  `smk --clean`  

  Expected:  
```  
Deleting /home/lionel/Proj/smk/tests/hello.c/hello
Deleting /home/lionel/Proj/smk/tests/hello.c/hello.o
Deleting /home/lionel/Proj/smk/tests/hello.c/main.o
```  

  Run:  
  `smk -e`  (to check effective cleaning)  

  Expected:  
```  
run gcc -o hello.o -c hello.c because /home/lionel/Proj/smk/tests/hello.c/hello.o is missing
gcc -o hello.o -c hello.c
run gcc -o main.o -c main.c because /home/lionel/Proj/smk/tests/hello.c/main.o is missing
gcc -o main.o -c main.c
run gcc -o hello hello.o main.o because /home/lionel/Proj/smk/tests/hello.c/hello is missing
gcc -o hello hello.o main.o
```  


Targets related functions / --clean [Successful](tests_status.md#successful)

# Multiline commands in smkfile



 cat `multiline_smkfile.txt`:  
```  
	ploticus -prefab pie 	\
		data=out.sloccount labels=2 colors="blue red green orange"	\
# comment in the middle should not get in the way
		explode=0.1 values=1 title="Ada sloc `date +%x`"	\
		 -png -o out.sloc.png 

```  

 Run:  
 `smk -q --reset`  
 `smk multiline_smkfile.txt`  

 Expected:  
```  
ploticus -prefab pie data=out.sloccount labels=2 colors="blue red green orange" explode=0.1 values=1 title="Ada sloc `date +%x`" -png -o out.sloc.png
```  

  Hill formatted multiline:  

  cat `hill_multiline_smkfile.txt`:  
```  
# Hill formatted multiline command:

	ploticus -prefab pie 	\
		data=out.sloccount labels=2 colors="blue red green orange"	\
		explode=0.1 values=1 title="Ada sloc `date +%x`"	\
// the end of the command is missing 

-- Note that the comment immediatly following the command 
-- should not be considered as the end of the command, neither 
-- should the following blank line or any of the following lines.
```  

  Run:  
  `smk -q --reset`  
  `smk hill_multiline_smkfile.txt`  

  Expected:  
```  
Error : hill_multiline_smkfile.txt ends with incomplete multine, last command ignored
Nothing to run
```  


Multiline commands in smkfile /  [Successful](tests_status.md#successful)

# `-k` and `-i` behavior



 Run:  
 `smk -q --reset`  
 `smk hello.c/Wrong_Makefile`  

 Expected:  
```  
gcc --not-an-option -o hello.o -c hello.c
Error : Spawn failed for /usr/bin/strace -y -q -qq -f -e trace=file -o /tmp/Wrong_Makefile.strace_output gcc --not-an-option -o hello.o -c hello.c
```  


  Run:  
  `smk -q --reset`  
  `smk -k hello.c/Wrong_Makefile`  

  Expected:  
```  
gcc --not-an-option -o hello.o -c hello.c
Error : Spawn failed for /usr/bin/strace -y -q -qq -f -e trace=file -o /tmp/Wrong_Makefile.strace_output gcc --not-an-option -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
Error : Spawn failed for /usr/bin/strace -y -q -qq -f -e trace=file -o /tmp/Wrong_Makefile.strace_output gcc -o main.o -c main.c --WTF
gcc -o hello hello.o main.o
gcc --not-an-option -o hello.o -c hello.c
Error : Spawn failed for /usr/bin/strace -y -q -qq -f -e trace=file -o /tmp/Wrong_Makefile.strace_output gcc --not-an-option -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
Error : Spawn failed for /usr/bin/strace -y -q -qq -f -e trace=file -o /tmp/Wrong_Makefile.strace_output gcc -o main.o -c main.c --WTF
```  

  Note that the two command that fail are rerun  
  during a second loop in case the command  
  that run successfully during the first loop  
  changed the the situation.  
  And that is not the case here, nothing is run with  
  success during the second loop, so full stop.  


  Run:  
  `smk -q --reset`  
  `smk -i hello.c/Wrong_Makefile`  

  Expected:  
     Same as without nothing, but without returning an error code  
```  
gcc --not-an-option -o hello.o -c hello.c
Error : Spawn failed for /usr/bin/strace -y -q -qq -f -e trace=file -o /tmp/Wrong_Makefile.strace_output gcc --not-an-option -o hello.o -c hello.c
```  


  Run: with both!  
  `smk -q --reset`  
  `smk --keep-going --ignore-errors hello.c/Wrong_Makefile`  

  Expected:  
     Same as with -k, but without returning an error code  
```  
gcc --not-an-option -o hello.o -c hello.c
Error : Spawn failed for /usr/bin/strace -y -q -qq -f -e trace=file -o /tmp/Wrong_Makefile.strace_output gcc --not-an-option -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
Error : Spawn failed for /usr/bin/strace -y -q -qq -f -e trace=file -o /tmp/Wrong_Makefile.strace_output gcc -o main.o -c main.c --WTF
gcc -o hello hello.o main.o
gcc --not-an-option -o hello.o -c hello.c
Error : Spawn failed for /usr/bin/strace -y -q -qq -f -e trace=file -o /tmp/Wrong_Makefile.strace_output gcc --not-an-option -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
Error : Spawn failed for /usr/bin/strace -y -q -qq -f -e trace=file -o /tmp/Wrong_Makefile.strace_output gcc -o main.o -c main.c --WTF
```  


`-k` and `-i` behavior /  [Successful](tests_status.md#successful)
