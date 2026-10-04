
# Sanity



 Makefile:  
```  
all: hello

hello.o: hello.c
	gcc -o hello.o -c hello.c

main.o: main.c hello.h
	gcc -o main.o -c main.c

hello: hello.o main.o
	gcc -o hello hello.o main.o

```  

##  Sanity / First `smk`, after `make`, should run no command


  Run:  
  `smk -q ../hello.c/Makefile.2`  
  `smk -e ../hello.c/Makefile.2`  

  Expected:  
```  
Nothing to run
```  


Sanity / First `smk`, after `make`, should run no command [Successful](tests_status.md#successful)

##  Sanity / Second `smk`, should not run any command


  Run:  
  `smk -e ../hello.c/Makefile.2`  

  Expected:  
```  
Nothing to run
```  


Sanity / Second `smk`, should not run any command [Successful](tests_status.md#successful)

##  Sanity / `smk reset`, no more history, should run all commands


  Run:  
  `smk reset --quiet`  
  `smk -e ../hello.c/Makefile.2`  

  Expected:  
```  
run "gcc -o hello.o -c hello.c" because it was not run before
gcc -o hello.o -c hello.c
run "gcc -o main.o -c main.c" because it was not run before
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because it was not run before
gcc -o hello hello.o main.o
```  


Sanity / `smk reset`, no more history, should run all commands [Successful](tests_status.md#successful)

##  Sanity / `smk -a`, should run all commands even if not needed


  Run:  
  `smk -e -a ../hello.c/Makefile.2`  

  Expected:  
```  
run "gcc -o hello.o -c hello.c" because -a option is set
gcc -o hello.o -c hello.c
run "gcc -o main.o -c main.c" because -a option is set
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because -a option is set
gcc -o hello hello.o main.o
```  


Sanity / `smk -a`, should run all commands even if not needed [Successful](tests_status.md#successful)

##  Sanity / `rm main.o` (missing file)


  Run:  
  `rm ../hello.c/main.o`  
  `smk -e --missing-targets ../hello.c/Makefile.2`  

  Expected:  
```  
run "gcc -o main.o -c main.c" because Target file ../hello.c/main.o is missing
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because Source file ../hello.c/main.o has been updated (-- ::.)
gcc -o hello hello.o main.o
```  


Sanity / `rm main.o` (missing file) [Successful](tests_status.md#successful)

##  Sanity / `touch hello.c` (updated file)


  Run:  
  `touch ../hello.c/hello.c`  
  `smk -e ../hello.c/Makefile.2`  

  Expected:  
```  
run "gcc -o hello.o -c hello.c" because Source file ../hello.c/hello.c has been updated (-- ::.)
gcc -o hello.o -c hello.c
run "gcc -o hello hello.o main.o" because Source file ../hello.c/hello.o has been updated (-- ::.)
gcc -o hello hello.o main.o
```  


Sanity / `touch hello.c` (updated file) [Successful](tests_status.md#successful)

##  Sanity / `touch hello.c` and dry run


  Run:  
  `touch ../hello.c/hello.c`  
  `smk whatsnew ../hello.c/Makefile.2`  

  Expected:  
```  
[Updated] [Source] ../hello.c/hello.c
```  


  Run:  
  `smk -e -n`  

  Expected:  
```  
run "gcc -o hello.o -c hello.c" because Source file ../hello.c/hello.c has been updated (-- ::.)
> gcc -o hello.o -c hello.c
```  


  whatsnew should still return the same, as nothing was changed by previous run with -n) :  
  `smk wn`  

  Expected:  
```  
[Updated] [Source] ../hello.c/hello.c
```  


  Run same without -n :  
  `smk -e ../hello.c/Makefile.2`  

  Expected:  
```  
run "gcc -o hello.o -c hello.c" because Source file ../hello.c/hello.c has been updated (-- ::.)
gcc -o hello.o -c hello.c
run "gcc -o hello hello.o main.o" because Source file ../hello.c/hello.o has been updated (-- ::.)
gcc -o hello hello.o main.o
```  


  and now whatsnew returns null:  
  `smk wn`  

  Expected:  
```  
Nothing new
```  


Sanity / `touch hello.c` and dry run [Successful](tests_status.md#successful)

# Read queries



##  Read queries / read-smkfile


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
  `smk read-smkfile ../hello.c/Makefile.3`  

  Expected:  
```  
../hello.c/Makefile.3 (YYYY:MM:DD HH:MM:SS.SS) :
4: [hello.o] gcc -o hello.o -c hello.c
7: [main.o] gcc -o main.o -c main.c
10: [hello] gcc -o hello hello.o main.o
14: [clean] rm -rf *.o
17: [mrproper] rm -rf hello
```  


Read queries / read-smkfile [Successful](tests_status.md#successful)

##  Read queries / status


  Read the previous run dump and shows sources and targets  

  Run:  
  `smk -q reset`  
  `smk -q build ../hello.c/Makefile.2`  
  `smk status ../hello.c/Makefile.2`  

  Expected:  
  (note that to ease comparison, dates are removed)  
```  
Command "gcc -o hello hello.o main.o" in section [hello], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (2)
  - ../hello.c/hello.o
  - ../hello.c/main.o
  Targets: (1)
  - ../hello.c/hello

Command "gcc -o hello.o -c hello.c" in section [hello.o], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (1)
  - ../hello.c/hello.c
  Targets: (1)
  - ../hello.c/hello.o

Command "gcc -o main.o -c main.c" in section [main.o], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (2)
  - ../hello.c/hello.h
  - ../hello.c/main.c
  Targets: (1)
  - ../hello.c/main.o

```  

  Run: (same with system files not ignored and long form)   
  `smk -q reset`  
  `smk -q build ../hello.c/Makefile.2`  
  `smk st -l -sa ../hello.c/Makefile.2`  

  Expected:  
  (note that to ease comparison, dates are removed)  
```  
Command "gcc -o hello hello.o main.o" in section [hello], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (30)
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.o
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/main.o
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/crtbeginS.o
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/crtendS.o
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/libatomic_asneeded.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/libgcc.a
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/libgcc_s.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/libgcc_s_asneeded.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/C.utf8/LC_CTYPE
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/fr_CA.utf8/LC_MESSAGES/SYS_LC_MESSAGES
  - [If update  ] [Dir] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/fr_FR.utf8/LC_MESSAGES
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/Scrt1.o
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crti.o
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crtn.o
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libatomic.so.1.2.0
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libbfd-2.47-system.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc.so.6
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc_nonshared.a
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libctf.so.0.0.0
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libgcc_s.so.1
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libjansson.so.4.15.1
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libsframe.so.3.0.0
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libz.so.1.3.2
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libzstd.so.1.5.7
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/libexec/gcc/x86_64-linux-gnu/16/liblto_plugin.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/locale/fr/LC_MESSAGES/bfd.mo
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/locale/fr/LC_MESSAGES/ld.mo
  Targets: (1)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello

Command "gcc -o hello.o -c hello.c" in section [hello.o], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (71)
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.c
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/alloca.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/endian.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features-time64.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdc-predef.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdio.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdlib.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/atomic_wide_counter.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/byteswap.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endian.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endianness.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn-common.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/long-double.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/select.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/time64.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/timesize.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/FILE.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/cookie_io_functions_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/once_flag.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/time_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/typesizes.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitflags.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitstatus.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/wordsize.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/cdefs.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/select.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/types.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/include/stdarg.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/include/stddef.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/C.utf8/LC_CTYPE
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/fr_CA.utf8/LC_MESSAGES/SYS_LC_MESSAGES
  - [If update  ] [Dir] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/fr_FR.utf8/LC_MESSAGES
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libbfd-2.47-system.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc.so.6
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libgmp.so.10.5.0
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libisl.so.23.5.0
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libm.so.6
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libmpc.so.3.3.1
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libmpfr.so.6.2.2
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libsframe.so.3.0.0
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libz.so.1.3.2
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libzstd.so.1.5.7
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/locale/fr/LC_MESSAGES/gas.mo
  Targets: (1)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.o

Command "gcc -o main.o -c main.c" in section [main.o], last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (72)
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.h
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/main.c
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/alloca.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/endian.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features-time64.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdc-predef.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdio.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdlib.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/atomic_wide_counter.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/byteswap.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endian.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endianness.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn-common.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/long-double.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/select.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/time64.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/timesize.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/FILE.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/cookie_io_functions_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/once_flag.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/time_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/typesizes.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitflags.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitstatus.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/wordsize.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/cdefs.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/select.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/types.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/include/stdarg.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/include/stddef.h
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/C.utf8/LC_CTYPE
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/fr_CA.utf8/LC_MESSAGES/SYS_LC_MESSAGES
  - [If update  ] [Dir] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/fr_FR.utf8/LC_MESSAGES
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libbfd-2.47-system.so
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc.so.6
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libgmp.so.10.5.0
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libisl.so.23.5.0
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libm.so.6
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libmpc.so.3.3.1
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libmpfr.so.6.2.2
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libsframe.so.3.0.0
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libz.so.1.3.2
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libzstd.so.1.5.7
  - [If update  ] [Fil] [System] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/locale/fr/LC_MESSAGES/gas.mo
  Targets: (1)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/main.o

```  


  Run:  
  `smk -q reset`  
  `smk status ../hello.c/Makefile.2`  

  Expected:  
```  
Error : No previous run found.
```  


  Run:  
  `smk status`  

  Expected:  
```  
Error : No smkfile given, and no existing runfile in dir
```  


Read queries / status [Successful](tests_status.md#successful)

# List queries



##  List queries / lr | list-runs


  Test available previous runs  

  Run:  
  `smk -q reset`  
  `smk lr`  

  Expected:  
```  
No run file
```  

  Run:  
  `smk -q build ../hello.c/Makefile.2`  
  `smk -q ../hello.c/Makefile.3`  
  `smk list-runs`  

  Expected:  
```  
Makefile.2
Makefile.3
```  


List queries / lr | list-runs [Successful](tests_status.md#successful)

##  List queries / lt | list-targets

  Run:  
  `smk list-targets --long-listing ../hello.c/Makefile.2`  

  Expected: (long form)  
```  
"gcc -o hello hello.o main.o" [hello] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello
"gcc -o hello.o -c hello.c" [hello.o] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.o
"gcc -o main.o -c main.c" [main.o] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/main.o
```  

  Run:  
  `smk lt ../hello.c/Makefile.2`  

  Expected: (short form)  
```  
../hello.c/hello
../hello.c/hello.o
../hello.c/main.o
```  


List queries / lt | list-targets [Successful](tests_status.md#successful)

##  List queries / ls | list-sources


  Run: ls without shortening file names  
  `smk ls -ds ../hello.c/Makefile.2`  

  Expected:  
```  
/home/lionel/prj/smk/tests/hello.c/hello.o
/home/lionel/prj/smk/tests/hello.c/main.o
/home/lionel/prj/smk/tests/hello.c/hello.c
/home/lionel/prj/smk/tests/hello.c/hello.h
/home/lionel/prj/smk/tests/hello.c/main.c
```  


  Run:  
  `smk list-sources ../hello.c/Makefile.2`  

  Expected:  
```  
../hello.c/hello.o
../hello.c/main.o
../hello.c/hello.c
../hello.c/hello.h
../hello.c/main.c
```  


List queries / ls | list-sources [Successful](tests_status.md#successful)

##  List queries / ls | list-sources --show-all-files


  Run: (short form)  
  `smk list-sources --shows-system-files ../hello.c/Makefile.2`  

  Expected:  
```  
../hello.c/hello.o
../hello.c/main.o
/usr/lib/gcc/x86_64-linux-gnu/16/crtbeginS.o
/usr/lib/gcc/x86_64-linux-gnu/16/crtendS.o
/usr/lib/gcc/x86_64-linux-gnu/16/libatomic_asneeded.so
/usr/lib/gcc/x86_64-linux-gnu/16/libgcc.a
/usr/lib/gcc/x86_64-linux-gnu/16/libgcc_s.so
/usr/lib/gcc/x86_64-linux-gnu/16/libgcc_s_asneeded.so
/usr/lib/locale/C.utf8/LC_CTYPE
/usr/lib/locale/fr_CA.utf8/LC_MESSAGES/SYS_LC_MESSAGES
/usr/lib/locale/fr_FR.utf8/LC_MESSAGES
/usr/lib/x86_64-linux-gnu/Scrt1.o
/usr/lib/x86_64-linux-gnu/crti.o
/usr/lib/x86_64-linux-gnu/crtn.o
/usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
/usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2
/usr/lib/x86_64-linux-gnu/libatomic.so.1.2.0
/usr/lib/x86_64-linux-gnu/libbfd-2.47-system.so
/usr/lib/x86_64-linux-gnu/libc.so
/usr/lib/x86_64-linux-gnu/libc.so.6
/usr/lib/x86_64-linux-gnu/libc_nonshared.a
/usr/lib/x86_64-linux-gnu/libctf.so.0.0.0
/usr/lib/x86_64-linux-gnu/libgcc_s.so.1
/usr/lib/x86_64-linux-gnu/libjansson.so.4.15.1
/usr/lib/x86_64-linux-gnu/libsframe.so.3.0.0
/usr/lib/x86_64-linux-gnu/libz.so.1.3.2
/usr/lib/x86_64-linux-gnu/libzstd.so.1.5.7
/usr/libexec/gcc/x86_64-linux-gnu/16/liblto_plugin.so
/usr/share/locale/fr/LC_MESSAGES/bfd.mo
/usr/share/locale/fr/LC_MESSAGES/ld.mo
../hello.c/hello.c
/usr/include/alloca.h
/usr/include/endian.h
/usr/include/features-time64.h
/usr/include/features.h
/usr/include/stdc-predef.h
/usr/include/stdio.h
/usr/include/stdlib.h
/usr/include/x86_64-linux-gnu/bits/atomic_wide_counter.h
/usr/include/x86_64-linux-gnu/bits/byteswap.h
/usr/include/x86_64-linux-gnu/bits/endian.h
/usr/include/x86_64-linux-gnu/bits/endianness.h
/usr/include/x86_64-linux-gnu/bits/floatn-common.h
/usr/include/x86_64-linux-gnu/bits/floatn.h
/usr/include/x86_64-linux-gnu/bits/libc-header-start.h
/usr/include/x86_64-linux-gnu/bits/long-double.h
/usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
/usr/include/x86_64-linux-gnu/bits/select.h
/usr/include/x86_64-linux-gnu/bits/stdint-intn.h
/usr/include/x86_64-linux-gnu/bits/stdio_lim.h
/usr/include/x86_64-linux-gnu/bits/stdlib-float.h
/usr/include/x86_64-linux-gnu/bits/struct_mutex.h
/usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
/usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
/usr/include/x86_64-linux-gnu/bits/time64.h
/usr/include/x86_64-linux-gnu/bits/timesize.h
/usr/include/x86_64-linux-gnu/bits/types.h
/usr/include/x86_64-linux-gnu/bits/types/FILE.h
/usr/include/x86_64-linux-gnu/bits/types/__FILE.h
/usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
/usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
/usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
/usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
/usr/include/x86_64-linux-gnu/bits/types/clock_t.h
/usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
/usr/include/x86_64-linux-gnu/bits/types/cookie_io_functions_t.h
/usr/include/x86_64-linux-gnu/bits/types/once_flag.h
/usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
/usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
/usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
/usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
/usr/include/x86_64-linux-gnu/bits/types/time_t.h
/usr/include/x86_64-linux-gnu/bits/types/timer_t.h
/usr/include/x86_64-linux-gnu/bits/typesizes.h
/usr/include/x86_64-linux-gnu/bits/uintn-identity.h
/usr/include/x86_64-linux-gnu/bits/waitflags.h
/usr/include/x86_64-linux-gnu/bits/waitstatus.h
/usr/include/x86_64-linux-gnu/bits/wordsize.h
/usr/include/x86_64-linux-gnu/gnu/stubs-64.h
/usr/include/x86_64-linux-gnu/gnu/stubs.h
/usr/include/x86_64-linux-gnu/sys/cdefs.h
/usr/include/x86_64-linux-gnu/sys/select.h
/usr/include/x86_64-linux-gnu/sys/types.h
/usr/lib/gcc/x86_64-linux-gnu/16/include/stdarg.h
/usr/lib/gcc/x86_64-linux-gnu/16/include/stddef.h
/usr/lib/locale/C.utf8/LC_CTYPE
/usr/lib/locale/fr_CA.utf8/LC_MESSAGES/SYS_LC_MESSAGES
/usr/lib/locale/fr_FR.utf8/LC_MESSAGES
/usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
/usr/lib/x86_64-linux-gnu/libbfd-2.47-system.so
/usr/lib/x86_64-linux-gnu/libc.so.6
/usr/lib/x86_64-linux-gnu/libgmp.so.10.5.0
/usr/lib/x86_64-linux-gnu/libisl.so.23.5.0
/usr/lib/x86_64-linux-gnu/libm.so.6
/usr/lib/x86_64-linux-gnu/libmpc.so.3.3.1
/usr/lib/x86_64-linux-gnu/libmpfr.so.6.2.2
/usr/lib/x86_64-linux-gnu/libsframe.so.3.0.0
/usr/lib/x86_64-linux-gnu/libz.so.1.3.2
/usr/lib/x86_64-linux-gnu/libzstd.so.1.5.7
/usr/share/locale/fr/LC_MESSAGES/gas.mo
../hello.c/hello.h
../hello.c/main.c
/usr/include/alloca.h
/usr/include/endian.h
/usr/include/features-time64.h
/usr/include/features.h
/usr/include/stdc-predef.h
/usr/include/stdio.h
/usr/include/stdlib.h
/usr/include/x86_64-linux-gnu/bits/atomic_wide_counter.h
/usr/include/x86_64-linux-gnu/bits/byteswap.h
/usr/include/x86_64-linux-gnu/bits/endian.h
/usr/include/x86_64-linux-gnu/bits/endianness.h
/usr/include/x86_64-linux-gnu/bits/floatn-common.h
/usr/include/x86_64-linux-gnu/bits/floatn.h
/usr/include/x86_64-linux-gnu/bits/libc-header-start.h
/usr/include/x86_64-linux-gnu/bits/long-double.h
/usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
/usr/include/x86_64-linux-gnu/bits/select.h
/usr/include/x86_64-linux-gnu/bits/stdint-intn.h
/usr/include/x86_64-linux-gnu/bits/stdio_lim.h
/usr/include/x86_64-linux-gnu/bits/stdlib-float.h
/usr/include/x86_64-linux-gnu/bits/struct_mutex.h
/usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
/usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
/usr/include/x86_64-linux-gnu/bits/time64.h
/usr/include/x86_64-linux-gnu/bits/timesize.h
/usr/include/x86_64-linux-gnu/bits/types.h
/usr/include/x86_64-linux-gnu/bits/types/FILE.h
/usr/include/x86_64-linux-gnu/bits/types/__FILE.h
/usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
/usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
/usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
/usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
/usr/include/x86_64-linux-gnu/bits/types/clock_t.h
/usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
/usr/include/x86_64-linux-gnu/bits/types/cookie_io_functions_t.h
/usr/include/x86_64-linux-gnu/bits/types/once_flag.h
/usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
/usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
/usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
/usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
/usr/include/x86_64-linux-gnu/bits/types/time_t.h
/usr/include/x86_64-linux-gnu/bits/types/timer_t.h
/usr/include/x86_64-linux-gnu/bits/typesizes.h
/usr/include/x86_64-linux-gnu/bits/uintn-identity.h
/usr/include/x86_64-linux-gnu/bits/waitflags.h
/usr/include/x86_64-linux-gnu/bits/waitstatus.h
/usr/include/x86_64-linux-gnu/bits/wordsize.h
/usr/include/x86_64-linux-gnu/gnu/stubs-64.h
/usr/include/x86_64-linux-gnu/gnu/stubs.h
/usr/include/x86_64-linux-gnu/sys/cdefs.h
/usr/include/x86_64-linux-gnu/sys/select.h
/usr/include/x86_64-linux-gnu/sys/types.h
/usr/lib/gcc/x86_64-linux-gnu/16/include/stdarg.h
/usr/lib/gcc/x86_64-linux-gnu/16/include/stddef.h
/usr/lib/locale/C.utf8/LC_CTYPE
/usr/lib/locale/fr_CA.utf8/LC_MESSAGES/SYS_LC_MESSAGES
/usr/lib/locale/fr_FR.utf8/LC_MESSAGES
/usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
/usr/lib/x86_64-linux-gnu/libbfd-2.47-system.so
/usr/lib/x86_64-linux-gnu/libc.so.6
/usr/lib/x86_64-linux-gnu/libgmp.so.10.5.0
/usr/lib/x86_64-linux-gnu/libisl.so.23.5.0
/usr/lib/x86_64-linux-gnu/libm.so.6
/usr/lib/x86_64-linux-gnu/libmpc.so.3.3.1
/usr/lib/x86_64-linux-gnu/libmpfr.so.6.2.2
/usr/lib/x86_64-linux-gnu/libsframe.so.3.0.0
/usr/lib/x86_64-linux-gnu/libz.so.1.3.2
/usr/lib/x86_64-linux-gnu/libzstd.so.1.5.7
/usr/share/locale/fr/LC_MESSAGES/gas.mo
```  


  Run: (long form)  
  `smk -l ls -sa ../hello.c/Makefile.2`  

  Expected:  
```  
"gcc -o hello hello.o main.o" [hello] [If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/fr_FR.utf8/LC_MESSAGES
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.o
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/main.o
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/libexec/gcc/x86_64-linux-gnu/16/liblto_plugin.so
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/crtbeginS.o
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/crtendS.o
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/libatomic_asneeded.so
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/libgcc.a
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/libgcc_s_asneeded.so
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/libgcc_s.so
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/C.utf8/LC_CTYPE
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/fr_CA.utf8/LC_MESSAGES/SYS_LC_MESSAGES
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crti.o
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/crtn.o
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libatomic.so.1.2.0
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libbfd-2.47-system.so
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc_nonshared.a
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc.so
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc.so.6
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libctf.so.0.0.0
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libgcc_s.so.1
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libjansson.so.4.15.1
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libsframe.so.3.0.0
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libz.so.1.3.2
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libzstd.so.1.5.7
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/Scrt1.o
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/locale/fr/LC_MESSAGES/bfd.mo
"gcc -o hello hello.o main.o" [hello] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/locale/fr/LC_MESSAGES/ld.mo
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/fr_FR.utf8/LC_MESSAGES
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.c
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/alloca.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/endian.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features-time64.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdc-predef.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdio.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdlib.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/atomic_wide_counter.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/byteswap.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endian.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endianness.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn-common.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/long-double.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/select.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/time64.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/timesize.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/cookie_io_functions_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/FILE.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/typesizes.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/once_flag.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/time_t.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitflags.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitstatus.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/wordsize.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/cdefs.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/select.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/types.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/include/stdarg.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/include/stddef.h
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/C.utf8/LC_CTYPE
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/fr_CA.utf8/LC_MESSAGES/SYS_LC_MESSAGES
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libbfd-2.47-system.so
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc.so.6
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libgmp.so.10.5.0
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libisl.so.23.5.0
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libmpc.so.3.3.1
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libmpfr.so.6.2.2
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libm.so.6
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libsframe.so.3.0.0
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libz.so.1.3.2
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libzstd.so.1.5.7
"gcc -o hello.o -c hello.c" [hello.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/locale/fr/LC_MESSAGES/gas.mo
"gcc -o main.o -c main.c" [main.o] [If update  ] [Dir] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/fr_FR.utf8/LC_MESSAGES
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/hello.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [Normal] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] ../hello.c/main.c
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/alloca.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/endian.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/features-time64.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdc-predef.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdio.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/stdlib.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/atomic_wide_counter.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/byteswap.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endian.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/endianness.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn-common.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/floatn.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/long-double.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/select.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/struct_mutex.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/struct_rwlock.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/time64.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/timesize.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/cookie_io_functions_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/FILE.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos64_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__fpos_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/typesizes.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/once_flag.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_FILE.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/types/time_t.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitflags.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/waitstatus.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/bits/wordsize.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/gnu/stubs.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/cdefs.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/select.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/include/x86_64-linux-gnu/sys/types.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/include/stdarg.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/gcc/x86_64-linux-gnu/16/include/stddef.h
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/C.utf8/LC_CTYPE
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/locale/fr_CA.utf8/LC_MESSAGES/SYS_LC_MESSAGES
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libbfd-2.47-system.so
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libc.so.6
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libgmp.so.10.5.0
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libisl.so.23.5.0
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libmpc.so.3.3.1
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libmpfr.so.6.2.2
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libm.so.6
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libsframe.so.3.0.0
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libz.so.1.3.2
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/lib/x86_64-linux-gnu/libzstd.so.1.5.7
"gcc -o main.o -c main.c" [main.o] [If update  ] [Fil] [System] [Source] [New    ] [YYYY:MM:DD HH:MM:SS.SS] /usr/share/locale/fr/LC_MESSAGES/gas.mo
```  


List queries / ls | list-sources --show-all-files [Successful](tests_status.md#successful)

# Targets related functions



##  Targets related functions / dry-run clean


  Test targets cleaning (dry run and real)  

  Run:  
  `smk reset`  
  `smk -q build ../hello.c/Makefile.2`  
  `smk clean --dry-run`  
  `smk --explain`  (to check that nothing was actually deleted)  

  Expected:  
```  
Deleting file ../hello.c/hello
Deleting file ../hello.c/hello.o
Deleting file ../hello.c/main.o
```  


##  Targets related functions / real clean

  Run:  
  `smk clean`  

  Expected:  
```  
Deleting file ../hello.c/hello
Deleting file ../hello.c/hello.o
Deleting file ../hello.c/main.o
```  

  Run:  
  `smk -e -mt`  (to check effective cleaning)  

  Expected:  
```  
run "gcc -o hello.o -c hello.c" because Target file ../hello.c/hello.o is missing
gcc -o hello.o -c hello.c
run "gcc -o main.o -c main.c" because Target file ../hello.c/main.o is missing
gcc -o main.o -c main.c
run "gcc -o hello hello.o main.o" because Target file ../hello.c/hello is missing
gcc -o hello hello.o main.o
```  


Targets related functions / real clean [Successful](tests_status.md#successful)

##  Targets related functions / Build selected target


  Run:  
  `smk -q reset`  
  `smk ../hello.c/Makefile.1`  
  `smk build main.o`  
  Note that to avoid any confusion, this smkfile do not contain any target named main.o  

  Expected:  
```  
Nothing to run
```  

  Run:  
  `touch ../hello.c/main.c`  
  `smk build main.o`  

  Expected:  
```  
gcc -o main.o -c main.c
gcc -o hello hello.o main.o
```  


Targets related functions / Build selected target [Successful](tests_status.md#successful)

##  Targets related functions / Build unknown target


  Run:  
  `smk build mainzzzzz.o`  

  Expected:  
```  
Target "mainzzzzz.o" not found
run "smk list-targets" to get a list of possible target
```  


Targets related functions / Build unknown target [Successful](tests_status.md#successful)

# Implicit naming



##  Implicit naming / Implicit naming


  Test that when there is only one run file in the directory, smk assume it without giving it on the command line.  


  Run:  
  `smk -q reset`  
  `smk`  

  Expected: help message as there is nothing in the directory  

```  
Error : No smkfile given, and no existing runfile in dir
```  

  Run:  
  `smk -q ../hello.c/Makefile.2`  
  `touch ../hello.c/hello.c`  
  `smk`  

  Expected: smk re-run Makefile.2, as it is the only one in the dir  
```  
gcc -o hello.o -c hello.c
gcc -o hello hello.o main.o
```  

  Run:  
  `smk -q ../hello.c/Makefile.3`  
  `smk `  

  Expected:  
     There is more than one possible run, smk display the list but don't do anything else   
```  
Error : No smkfile given, and more than one runfile in dir
```  


Implicit naming / Implicit naming [Successful](tests_status.md#successful)

# Run errors


 test -k and -i behavior  

##  Run errors / no option


  Run:  
  `smk -q reset`  
  `smk ../hello.c/Wrong_Makefile`  

  Expected:  
```  
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
gcc: error: unrecognized command-line option ‘--WTF’
Error : Spawn failed for gcc -o main.o -c main.c --WTF
```  


##  Run errors / -k


  Run:  
  `smk -q reset`  
  `smk -k ../hello.c/Wrong_Makefile`  

  Expected:  
```  
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
gcc: error: unrecognized command-line option ‘--WTF’
Error : Spawn failed for gcc -o main.o -c main.c --WTF
gcc -o hello hello.o main.o
/usr/bin/x86_64-linux-gnu-ld.bfd : ne peut pas trouver main.o : Aucun fichier ou dossier de ce nom
collect2: error: ld returned 1 exit status
Error : Spawn failed for gcc -o hello hello.o main.o
```  


##  Run errors / -i


  Run:  
  `smk -q reset`  
  `smk -i ../hello.c/Wrong_Makefile`  

  Expected:  
     Same as with -k, but without returning an error code  
```  
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
gcc: error: unrecognized command-line option ‘--WTF’
Error : Spawn failed for gcc -o main.o -c main.c --WTF
```  


##  Run errors / -k -i


  Run: with both!  
  `smk -q reset`  
  `smk --keep-going --ignore-errors ../hello.c/Wrong_Makefile`  

  Expected:  
     Same as with -k, but without returning an error code  
```  
gcc -o hello.o -c hello.c
gcc -o main.o -c main.c --WTF
gcc: error: unrecognized command-line option ‘--WTF’
Error : Spawn failed for gcc -o main.o -c main.c --WTF
gcc -o hello hello.o main.o
/usr/bin/x86_64-linux-gnu-ld.bfd : ne peut pas trouver main.o : Aucun fichier ou dossier de ce nom
collect2: error: ld returned 1 exit status
Error : Spawn failed for gcc -o hello hello.o main.o
```  


Run errors / -k -i [Successful](tests_status.md#successful)

##  Run errors / `run` command fails


  Run:  
  `smk -q reset`  
  `rm *.smk`  
  `smk run non_existing_command`  

  Expected:  

```  
non_existing_command
/usr/bin/strace: Cannot find executable 'non_existing_command'
Error : Spawn failed for non_existing_command
```  


  default.smk should nevertheless contains the failed command  

```  
non_existing_command
```  

  Run:  
  `smk read-smkfile`  

  Expected:  

```  
default.smk (YYYY:MM:DD HH:MM:SS.SS) :
1: [] non_existing_command
```  

  Run:  
  `smk status`  

  Expected:  

```  
No recorded run
```  

  Other commands should return nothing:  
  `smk whatsnew`  
  `smk list-sources`  
  `smk list-targets`  
  `smk list-unused`  


Run errors / `run` command fails [Successful](tests_status.md#successful)

##  Run errors / debug option


  Run:  
  `smk -d dump`  

  Expected:  

```  
Error : No smkfile given, and no existing runfile in dir

Settings / Command line analysis:
---------------------------------

   Verbosity         : DEBUG
   Command           : DUMP
   Smkfile name      : 
   Runfile name      : 
   Strace out file   : 
   Section name      : 
   Cmd Line          : 
   Target name       : 
   Unidentified Opt  : 
   Initial directory : /home/lionel/prj/smk/tests/07_run_error_tests

   System Files      : 
   - /usr/*
   - /lib/*
   - /etc/*
   - /opt/*

   Ignore list       : 
   - /sys/*
   - /proc/*
   - /dev/*
   - /tmp/*
   - /etc/ld.so.cache

   [ ] Build_Missing_Targets
   [ ] Always_Make
   [ ] Explain
   [ ] Dry_Run
   [ ] Keep_Going
   [ ] Ignore_Errors
   [ ] Long_Listing_Format
   [ ] Warnings_As_Errors
   [X] Shorten_File_Names
   [X] Filter_Sytem_Files

---------------------------------

```  


Run errors / debug option [Successful](tests_status.md#successful)

# Command line



##  Command line / Help options


  Test the -h and help output :  

  Run:  
  `smk `  
  `smk help`  

  Expected:  

```  

Usage : smk [Options]* Command [Smkfile][:target]

Use example :
   when run the first time   : smk MyBuild.txt
   and then,to rebuild, just : smk
   to run a specific target  : smk MyBuild.txt:target
   or just                   : smk :target

Commands :
   build             : run the build
   status       | st : shows what smk knows about the previous
                       runs (commands, sources and targets)
   read-smkfile | rs : shows smk understanding of a Smkfile
   whatsnew     | wn : list changes since last run
   add               : add the rest of the command line to
                       default.smk
   run               : equivalent to `add` followed by `build`
   clean             : remove all targets files
   reset             : remove all local Smk files
                       (equivalent to rm .smk.*)
   version           : put Smk version
   help              : this message
   dump              : list all smk known info on files,
                       including unused and dir
   list-runs    | lr : list runfiles in current directory
   list-sources | ls : list sources, except system files
   list-targets | lt : list targets, except system files
   list-unused  | lu : list files not involved in build

   NB : when no command is given, build is assumed

Options :
   -mt  | --missing-targets : build if missing targets
                              (default is to build only if
                               sources are updated)
   -a   | --always-make     : unconditionally make all targets
   -e   | --explain         : explain why each target is made
   -n   | --dry-run         : print the commands that would be
                              executed, but do not execute them
   -sa  | --show-all-files  : show also system files
   -ds  | --dont-shorten    : print files with full path
   -i   | --ignore-errors   : ignore all errors in commands
                              executed to remake files
   -l   | --long-listing    : use a long listing format when
                              listing files
   -k   | --keep-going      : Do as much work as possible
   -We  | --Warnings=error  : treat warnings as errors
   -v   | --verbose
   -q   | --quiet           : no message unless error,
                              Warning are also ignored
   -h   | --help            : this message

https://github.com/LionelDraghi/smk

```  


Command line / Help options [Successful](tests_status.md#successful)

##  Command line / Version option


  Test that the version command will put :  

  Run:  
  `smk version`  

  Expected:  

```  
0.4.1-dev
```  


Command line / Version option [Successful](tests_status.md#successful)

##  Command line / Illegal cmd lines


  Run:  
  `smk read-smkfile status`  

  Expected:  

```  
Error : More than one command on command line : STATUS and READ_SMKFILE

Usage : smk [Options]* Command [Smkfile][:target]

Use example :
   when run the first time   : smk MyBuild.txt
   and then,to rebuild, just : smk
   to run a specific target  : smk MyBuild.txt:target
   or just                   : smk :target

Commands :
   build             : run the build
   status       | st : shows what smk knows about the previous
                       runs (commands, sources and targets)
   read-smkfile | rs : shows smk understanding of a Smkfile
   whatsnew     | wn : list changes since last run
   add               : add the rest of the command line to
                       default.smk
   run               : equivalent to `add` followed by `build`
   clean             : remove all targets files
   reset             : remove all local Smk files
                       (equivalent to rm .smk.*)
   version           : put Smk version
   help              : this message
   dump              : list all smk known info on files,
                       including unused and dir
   list-runs    | lr : list runfiles in current directory
   list-sources | ls : list sources, except system files
   list-targets | lt : list targets, except system files
   list-unused  | lu : list files not involved in build

   NB : when no command is given, build is assumed

Options :
   -mt  | --missing-targets : build if missing targets
                              (default is to build only if
                               sources are updated)
   -a   | --always-make     : unconditionally make all targets
   -e   | --explain         : explain why each target is made
   -n   | --dry-run         : print the commands that would be
                              executed, but do not execute them
   -sa  | --show-all-files  : show also system files
   -ds  | --dont-shorten    : print files with full path
   -i   | --ignore-errors   : ignore all errors in commands
                              executed to remake files
   -l   | --long-listing    : use a long listing format when
                              listing files
   -k   | --keep-going      : Do as much work as possible
   -We  | --Warnings=error  : treat warnings as errors
   -v   | --verbose
   -q   | --quiet           : no message unless error,
                              Warning are also ignored
   -h   | --help            : this message


https://github.com/LionelDraghi/smk

```  


Command line / Illegal cmd lines [Successful](tests_status.md#successful)

##  Command line / Option given after a command


  Run:  
  `smk reset -l`  

  Expected:  

```  
```  


Command line / Option given after a command [Successful](tests_status.md#successful)

##  Command line / Unknow Makefile


  Test the error message if an unknow MakeFile is given  

  Run:  
  `smk My_Makefile`  

  Expected:  

```  
Error : No smkfile given, and no existing runfile in dir
```  


Command line / Unknow Makefile [Successful](tests_status.md#successful)

# Multiline Commands



##  Multiline Commands / multiline single command


  cat `multiline_smkfile1.txt`:  
```  
	ploticus -prefab pie 	\
		data=out.sloccount labels=2 colors="blue red green orange"	\
# comment in the middle should not get in the way
		explode=0.1 values=1 title="Ada sloc `date +%x`"	\
		 -png -o out.sloc.png 
```  

  Run:  
  `smk -q reset`  
  `smk multiline_smkfile1.txt`  

  Expected:  
```  
ploticus -prefab pie data=out.sloccount labels=2 colors="blue red green orange" explode=0.1 values=1 title="Ada sloc `date +%x`" -png -o out.sloc.png
```  


Multiline Commands / multiline single command [Successful](tests_status.md#successful)

##  Multiline Commands / multiline with more commands and pipes


  cat `multiline_smkfile2.txt`:  
```  
// multiline with command and pipes
sloccount ../hello.c/* | 	\
grep "ansic=" 			\
|sed "s/ansic/C/"
		-- comment at the end
```  

  Run:  
  `smk -q reset`  
  `smk multiline_smkfile2.txt`  

  Expected:  
```  

sloccount ../hello.c/* | grep "ansic=" |sed "s/ansic/C/"
18      top_dir         C=18
```  


Multiline Commands / multiline with more commands and pipes [Successful](tests_status.md#successful)

##  Multiline Commands / Hill formatted multiline


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
  `smk -q reset`  
  `smk hill_multiline_smkfile.txt`  

  Expected:  
```  
Error : hill_multiline_smkfile.txt ends with incomplete multine, last command ignored
Nothing to run
```  


Multiline Commands / Hill formatted multiline [Successful](tests_status.md#successful)

# Sections related functions



##  Sections related functions / specific section building

  Run:  
  `smk :main.o` with main.o up to date  

  Expected:  
```  
Nothing to run
```  

  Run:  
  `touch main.c`  
  `smk :main.o`  

  Expected:  
```  
gcc -o main.o -c main.c
```  


Sections related functions / specific section building [Successful](tests_status.md#successful)

##  Sections related functions / unknow section

  Run:  
  `smk :qzdsqdq.o`  

  Expected:  
```  
No section "qzdsqdq.o" in ../hello.c/Makefile.2
```  


Sections related functions / unknow section [Successful](tests_status.md#successful)

##  Sections related functions / smkmfile:section notation

  Run:  
  `smk build -q ../hello.c/Makefile.4`  
  `touch ../hello.c/hello.c`  
  `smk ../hello.c/Makefile.4:hello.o`  

  Expected:  
```  
gcc -o hello.o -c hello.c
```  

  Run:  
  `smk -a ../hello.c/Makefile.4:mrproper`  

  Expected:  
```  
rm -rf hello
```  

  Run:  
  `smk -a ../hello.c/Makezzzzzfile.4:mrproper`  

  Expected:  
```  
Error : Unknown Smkfile ../hello.c/Makezzzzzfile.4 in ../hello.c/Makezzzzzfile.4:mrproper
Error : No smkfile given, and more than one runfile in dir
```  


Sections related functions / smkmfile:section notation [Successful](tests_status.md#successful)

# Command Run features



##  Command Run features / Add and build


  Run:  
  `smk -q clean`  
  `smk -q reset`  
  `rm default.smk`  
  `smk add gcc -c ../hello.c/main.c`  
  `smk add gcc -c ../hello.c/hello.c`  
  `smk add gcc -o hello hello.o main.o`  
  `smk build`  

  Expected:  
```  
gcc -c ../hello.c/main.c
gcc -c ../hello.c/hello.c
gcc -o hello hello.o main.o
```  


Command Run features / Add and build [Successful](tests_status.md#successful)

##  Command Run features / Run


  Run:  
  `smk -q clean`  
  `smk -q reset`  
  `rm default.smk`  
  `smk run gcc -c ../hello.c/main.c`  
  `smk run gcc -c ../hello.c/hello.c`  
  `smk run gcc -o hello hello.o main.o`  

  Expected:  
```  

gcc -c ../hello.c/main.c
gcc -c ../hello.c/hello.c
gcc -o hello hello.o main.o
```  


Command Run features / Run [Successful](tests_status.md#successful)

# Directory update tests



##  Directory update tests / start conversion


  Run:  
  `rm default.smk`  
  `smk -q reset`  
  `smk run ./ogg-to-mp3.sh`  

  Expected:  
```  
./ogg-to-mp3.sh

```  

  Run:  
  `smk st -l`  

  Expected:  
```  
Command "./ogg-to-mp3.sh", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (5)
  - [If update  ] [Dir] [Normal] [Source] [Updated] [YYYY:MM:DD HH:MM:SS.SS] ./
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ogg-to-mp3.sh
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] to-mp3.sh
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] x.ogg
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] y.ogg
  Targets: (2)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] x.mp3
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] y.mp3

```  


Directory update tests / start conversion [Successful](tests_status.md#successful)

##  Directory update tests / new ogg in dir


  Run:  
  `cp x.ogg z.ogg`  
  `smk whatsnew`  

  Expected:  
```  
[Updated] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests
[Missing] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/x.mp3
[Updated] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/y.ogg
[Created] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/z.ogg
```  


Directory update tests / new ogg in dir [Successful](tests_status.md#successful)

##  Directory update tests / ogg-to-mp3 is modified


  Run:  
  `smk -q run ./ogg-to-mp3.sh`  
  `touch ogg-to-mp3`  
  `smk -e`  

  Expected:  
```  
run "./ogg-to-mp3.sh" because Source dir ./ has been updated (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```  


Directory update tests / ogg-to-mp3 is modified [Successful](tests_status.md#successful)

##  Directory update tests / adding a .ogg file in a subdir


  Run:  
  `mkdir dir1`  
  `smk wn -l`  

  Expected:  
```  
[Dir] [Normal] [Source] [Updated] [YYYY:MM:DD HH:MM:SS.SS] ./
```  

  Run:  
  `smk -e run ./ogg-to-mp3.sh`  

  Expected:  
```  
run "./ogg-to-mp3.sh" because Source dir ./ has been updated (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```  

  Run:  
  `cp x.ogg dir1/t.ogg`  
  `smk -e run ./ogg-to-mp3.sh`  

  Expected:  
```  
run "./ogg-to-mp3.sh" because Source dir ./ has been updated (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```  


Directory update tests / adding a .ogg file in a subdir [Successful](tests_status.md#successful)

##  Directory update tests / smk clean


  Run:  
  `smk clean`  

  Expected:  
```  
Deleting file dir1/t.mp3
Deleting file x.mp3
Deleting file y.mp3
Deleting file z.mp3
```  


Directory update tests / smk clean [Successful](tests_status.md#successful)
# File_Utilities.Short_Path unit tests

1. Subdir with default Prefix : OK
Short_Path (From_Dir => "/home/tests",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

2. Dir with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

3. subdir with Prefix : OK
Short_Path (From_Dir => "/home/tests",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "./") = ./mysite/site/d1/idx.txt

4. Sibling subdir : OK
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = ../../mysite/site/d1/idx.txt

5. Parent dir : OK
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/idx.txt") = ../../idx.txt

6. Other Prefix : OK
Short_Path (From_Dir => "/home/tests/12/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "$PWD/") = $PWD/../mysite/site/d1/idx.txt

7. Root dir : OK
Short_Path (From_Dir => "/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = /home/tests/mysite/site/d1/idx.txt

8. File is over dir : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1",
            To_File  => "/home/readme.txt") = ../../../../readme.txt

9. File is over Dir, Dir with final / : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1/",
            To_File  => "/home/readme.txt") = ../../../../readme.txt

10. File is the current dir : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests") = ./

11. File is over Dir, Dir and File with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/") = ./

12. No common part : OK
Short_Path (From_Dir => "/home/toto/src/tests/",
            To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so") = /opt/GNAT/2018/lib64/libgcc_s.so


All tests OK [Successful](tests_status.md#successful)
# File_Utilities.Short_Path unit tests

1. Subdir with default Prefix : OK
Short_Path (From_Dir => "/home/tests",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

2. Dir with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

3. subdir with Prefix : OK
Short_Path (From_Dir => "/home/tests",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "./") = ./mysite/site/d1/idx.txt

4. Sibling subdir : OK
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = ../../mysite/site/d1/idx.txt

5. Parent dir : OK
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/idx.txt") = ../../idx.txt

6. Other Prefix : OK
Short_Path (From_Dir => "/home/tests/12/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "$PWD/") = $PWD/../mysite/site/d1/idx.txt

7. Root dir : OK
Short_Path (From_Dir => "/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = /home/tests/mysite/site/d1/idx.txt

8. File is over dir : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1",
            To_File  => "/home/readme.txt") = ../../../../readme.txt

9. File is over Dir, Dir with final / : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1/",
            To_File  => "/home/readme.txt") = ../../../../readme.txt

10. File is the current dir : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests") = ./

11. File is over Dir, Dir and File with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/") = ./

12. No common part : OK
Short_Path (From_Dir => "/home/toto/src/tests/",
            To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so") = /opt/GNAT/2018/lib64/libgcc_s.so


All tests OK [Successful](tests_status.md#successful)
# File_Utilities.Short_Path unit tests

1. Subdir with default Prefix : OK
Short_Path (From_Dir => "/home/tests",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

2. Dir with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

3. subdir with Prefix : OK
Short_Path (From_Dir => "/home/tests",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "./") = ./mysite/site/d1/idx.txt

4. Sibling subdir : OK
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = ../../mysite/site/d1/idx.txt

5. Parent dir : OK
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/idx.txt") = ../../idx.txt

6. Other Prefix : OK
Short_Path (From_Dir => "/home/tests/12/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "$PWD/") = $PWD/../mysite/site/d1/idx.txt

7. Root dir : OK
Short_Path (From_Dir => "/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = /home/tests/mysite/site/d1/idx.txt

8. File is over dir : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1",
            To_File  => "/home/readme.txt") = ../../../../readme.txt

9. File is over Dir, Dir with final / : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1/",
            To_File  => "/home/readme.txt") = ../../../../readme.txt

10. File is the current dir : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests") = ./

11. File is over Dir, Dir and File with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/") = ./

12. No common part : OK
Short_Path (From_Dir => "/home/toto/src/tests/",
            To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so") = /opt/GNAT/2018/lib64/libgcc_s.so


All tests OK [Successful](tests_status.md#successful)

# Analyze_Line unit tests


## execve, should be ignored
   Line: 11750 execve("/opt/GNAT/2018/bin/gcc", ["gcc", "-o", "hello", "hello.o", "main.o"], 0x7ffd629baf60 /* 45 vars */) = 0
   - Expected Call_Type: "ignored", OK

## SIGCHLD line
   Line: 11751 --- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, si_pid=11752, si_uid=1000, si_status=0, si_utime=0, si_stime=0} ---
   - Expected Call_Type: "ignored", OK

## Read openat
   Line: 11750 openat(AT_FDCWD, "/etc/ld.so.cache", O_RDONLY|O_CLOEXEC) = 3</etc/ld.so.cache>
   - Expected Read file: "/etc/ld.so.cache", OK

## Write openat
   Line: 11750 openat(AT_FDCWD, "/tmp/ccvHeGYq.res", O_RDWR|O_CREAT|O_EXCL, 0600) = 3</tmp/ccvHeGYq.res>
   - Expected Write file: "/tmp/ccvHeGYq.res", OK

## Dir openat 
   Line: 2918  openat(AT_FDCWD, "./site/about", O_RDONLY|O_NOCTTY|O_NONBLOCK|O_NOFOLLOW|O_CLOEXEC|O_DIRECTORY) = 3</home/lionel/Proj/smk/tests/mysite/site/about>
   - Expected Read file: "/home/lionel/Proj/smk/tests/mysite/site/about", OK

## Dir openat without AT_FDCWD
   Line: 904   openat(5</home/lionel/Proj/smk/tests/12_mp3_conversions_tests>, "dir1", O_RDONLY|O_NOCTTY|O_NONBLOCK|O_NOFOLLOW|O_CLOEXEC|O_DIRECTORY) = 6</home/lionel/Proj/smk/tests/12_mp3_conversions_tests/dir1>
   - Expected Read file: "/home/lionel/Proj/smk/tests/12_mp3_conversions_tests/dir1", OK

## Access Error (EACCES)
   Line: 25242 mkdir("/usr/lib/python3/dist-packages/click/__pycache__", 0777) = -1 EACCES (Permission denied)
   - Expected Call_Type: "ignored", OK

## File not found (ENOENT)
   Line: 11751 openat(AT_FDCWD, "/tmp/ccQ493FX.ld", O_RDONLY) = -1 ENOENT (No such file or directory)
   - Expected Call_Type: "ignored", OK

## access for exec
   Line: 11750 access("/opt/GNAT/2018/bin/gcc", X_OK) = 0
   - Expected Call_Type: "Ignored", OK

## access file with no dir
   Line: 11750 access("hello.o", F_OK)           = 0
   - Expected Call_Type: "Ignored", OK

## RW access to a dir
   Line: 11750 access("/tmp", R_OK|W_OK|X_OK)    = 0
   - Expected Call_Type: "Ignored", OK

## unlink (rm)
   Line: 11750 unlink("/tmp/ccvHeGYq.res")       = 0
   - Expected Call_Type: "Ignored", OK

## unlinkat AT_REMOVEDIR
   Line: 29164 unlinkat(AT_FDCWD, "./site/about", AT_REMOVEDIR) = 0
   - Expected Call_Type: "Ignored", OK

## Set a current directory for process 30461
   Line: 30461 getcwd("/dir1/dir2", 4096) = 36
   - Expected Call_Type: "ignored", OK

## Set a current directory for process 15232 with final /
   Line: 15232 getcwd("/dir3/dir4/", 4096) = 36
   - Expected Call_Type: "ignored", OK

## Read AND Write test
   Line: 30461 rename("x.mp3", "unknown-unknown.mp3") = 0
   - Expected Source file: "/dir1/dir2/x.mp3", OK
   - Expected Target file: "/dir1/dir2/unknown-unknown.mp3", OK

## Rename with two AT_FDCWD
   Line: 15232 renameat2(AT_FDCWD, "all.filecount.new", AT_FDCWD, "all.filecount", RENAME_NOREPLACE) = 0
   - Expected Source file: "/dir3/dir4/all.filecount.new", OK
   - Expected Target file: "/dir3/dir4/all.filecount", OK

## renameat with explicit dir (and not AT_FDCWD), with and without final /
   Line: 15165 renameat(5</home/lionel/.slocdata>, "old", 5</home/lionel/.slocdata/>, "new")...
   - Expected Source file: "/home/lionel/.slocdata/old", OK
   - Expected Target file: "/home/lionel/.slocdata/new", OK

All tests OK [Successful](tests_status.md#successful)

# Analyze_Line unit tests


## execve, should be ignored
   Line: 11750 execve("/opt/GNAT/2018/bin/gcc", ["gcc", "-o", "hello", "hello.o", "main.o"], 0x7ffd629baf60 /* 45 vars */) = 0
   - Expected Call_Type: "ignored", OK

## SIGCHLD line
   Line: 11751 --- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, si_pid=11752, si_uid=1000, si_status=0, si_utime=0, si_stime=0} ---
   - Expected Call_Type: "ignored", OK

## Read openat
   Line: 11750 openat(AT_FDCWD, "/etc/ld.so.cache", O_RDONLY|O_CLOEXEC) = 3</etc/ld.so.cache>
   - Expected Read file: "/etc/ld.so.cache", OK

## Write openat
   Line: 11750 openat(AT_FDCWD, "/tmp/ccvHeGYq.res", O_RDWR|O_CREAT|O_EXCL, 0600) = 3</tmp/ccvHeGYq.res>
   - Expected Write file: "/tmp/ccvHeGYq.res", OK

## Dir openat 
   Line: 2918  openat(AT_FDCWD, "./site/about", O_RDONLY|O_NOCTTY|O_NONBLOCK|O_NOFOLLOW|O_CLOEXEC|O_DIRECTORY) = 3</home/lionel/Proj/smk/tests/mysite/site/about>
   - Expected Read file: "/home/lionel/Proj/smk/tests/mysite/site/about", OK

## Dir openat without AT_FDCWD
   Line: 904   openat(5</home/lionel/Proj/smk/tests/12_mp3_conversions_tests>, "dir1", O_RDONLY|O_NOCTTY|O_NONBLOCK|O_NOFOLLOW|O_CLOEXEC|O_DIRECTORY) = 6</home/lionel/Proj/smk/tests/12_mp3_conversions_tests/dir1>
   - Expected Read file: "/home/lionel/Proj/smk/tests/12_mp3_conversions_tests/dir1", OK

## Access Error (EACCES)
   Line: 25242 mkdir("/usr/lib/python3/dist-packages/click/__pycache__", 0777) = -1 EACCES (Permission denied)
   - Expected Call_Type: "ignored", OK

## File not found (ENOENT)
   Line: 11751 openat(AT_FDCWD, "/tmp/ccQ493FX.ld", O_RDONLY) = -1 ENOENT (No such file or directory)
   - Expected Call_Type: "ignored", OK

## access for exec
   Line: 11750 access("/opt/GNAT/2018/bin/gcc", X_OK) = 0
   - Expected Call_Type: "Ignored", OK

## access file with no dir
   Line: 11750 access("hello.o", F_OK)           = 0
   - Expected Call_Type: "Ignored", OK

## RW access to a dir
   Line: 11750 access("/tmp", R_OK|W_OK|X_OK)    = 0
   - Expected Call_Type: "Ignored", OK

## unlink (rm)
   Line: 11750 unlink("/tmp/ccvHeGYq.res")       = 0
   - Expected Call_Type: "Ignored", OK

## unlinkat AT_REMOVEDIR
   Line: 29164 unlinkat(AT_FDCWD, "./site/about", AT_REMOVEDIR) = 0
   - Expected Call_Type: "Ignored", OK

## Set a current directory for process 30461
   Line: 30461 getcwd("/dir1/dir2", 4096) = 36
   - Expected Call_Type: "ignored", OK

## Set a current directory for process 15232 with final /
   Line: 15232 getcwd("/dir3/dir4/", 4096) = 36
   - Expected Call_Type: "ignored", OK

## Read AND Write test
   Line: 30461 rename("x.mp3", "unknown-unknown.mp3") = 0
   - Expected Source file: "/dir1/dir2/x.mp3", OK
   - Expected Target file: "/dir1/dir2/unknown-unknown.mp3", OK

## Rename with two AT_FDCWD
   Line: 15232 renameat2(AT_FDCWD, "all.filecount.new", AT_FDCWD, "all.filecount", RENAME_NOREPLACE) = 0
   - Expected Source file: "/dir3/dir4/all.filecount.new", OK
   - Expected Target file: "/dir3/dir4/all.filecount", OK

## renameat with explicit dir (and not AT_FDCWD), with and without final /
   Line: 15165 renameat(5</home/lionel/.slocdata>, "old", 5</home/lionel/.slocdata/>, "new")...
   - Expected Source file: "/home/lionel/.slocdata/old", OK
   - Expected Target file: "/home/lionel/.slocdata/new", OK

All tests OK [Successful](tests_status.md#successful)

# Analyze_Line unit tests


## execve, should be ignored
   Line: 11750 execve("/opt/GNAT/2018/bin/gcc", ["gcc", "-o", "hello", "hello.o", "main.o"], 0x7ffd629baf60 /* 45 vars */) = 0
   - Expected Call_Type: "ignored", OK

## SIGCHLD line
   Line: 11751 --- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, si_pid=11752, si_uid=1000, si_status=0, si_utime=0, si_stime=0} ---
   - Expected Call_Type: "ignored", OK

## Read openat
   Line: 11750 openat(AT_FDCWD, "/etc/ld.so.cache", O_RDONLY|O_CLOEXEC) = 3</etc/ld.so.cache>
   - Expected Read file: "/etc/ld.so.cache", OK

## Write openat
   Line: 11750 openat(AT_FDCWD, "/tmp/ccvHeGYq.res", O_RDWR|O_CREAT|O_EXCL, 0600) = 3</tmp/ccvHeGYq.res>
   - Expected Write file: "/tmp/ccvHeGYq.res", OK

## Dir openat 
   Line: 2918  openat(AT_FDCWD, "./site/about", O_RDONLY|O_NOCTTY|O_NONBLOCK|O_NOFOLLOW|O_CLOEXEC|O_DIRECTORY) = 3</home/lionel/Proj/smk/tests/mysite/site/about>
   - Expected Read file: "/home/lionel/Proj/smk/tests/mysite/site/about", OK

## Dir openat without AT_FDCWD
   Line: 904   openat(5</home/lionel/Proj/smk/tests/12_mp3_conversions_tests>, "dir1", O_RDONLY|O_NOCTTY|O_NONBLOCK|O_NOFOLLOW|O_CLOEXEC|O_DIRECTORY) = 6</home/lionel/Proj/smk/tests/12_mp3_conversions_tests/dir1>
   - Expected Read file: "/home/lionel/Proj/smk/tests/12_mp3_conversions_tests/dir1", OK

## Access Error (EACCES)
   Line: 25242 mkdir("/usr/lib/python3/dist-packages/click/__pycache__", 0777) = -1 EACCES (Permission denied)
   - Expected Call_Type: "ignored", OK

## File not found (ENOENT)
   Line: 11751 openat(AT_FDCWD, "/tmp/ccQ493FX.ld", O_RDONLY) = -1 ENOENT (No such file or directory)
   - Expected Call_Type: "ignored", OK

## access for exec
   Line: 11750 access("/opt/GNAT/2018/bin/gcc", X_OK) = 0
   - Expected Call_Type: "Ignored", OK

## access file with no dir
   Line: 11750 access("hello.o", F_OK)           = 0
   - Expected Call_Type: "Ignored", OK

## RW access to a dir
   Line: 11750 access("/tmp", R_OK|W_OK|X_OK)    = 0
   - Expected Call_Type: "Ignored", OK

## unlink (rm)
   Line: 11750 unlink("/tmp/ccvHeGYq.res")       = 0
   - Expected Call_Type: "Ignored", OK

## unlinkat AT_REMOVEDIR
   Line: 29164 unlinkat(AT_FDCWD, "./site/about", AT_REMOVEDIR) = 0
   - Expected Call_Type: "Ignored", OK

## Set a current directory for process 30461
   Line: 30461 getcwd("/dir1/dir2", 4096) = 36
   - Expected Call_Type: "ignored", OK

## Set a current directory for process 15232 with final /
   Line: 15232 getcwd("/dir3/dir4/", 4096) = 36
   - Expected Call_Type: "ignored", OK

## Read AND Write test
   Line: 30461 rename("x.mp3", "unknown-unknown.mp3") = 0
   - Expected Source file: "/dir1/dir2/x.mp3", OK
   - Expected Target file: "/dir1/dir2/unknown-unknown.mp3", OK

## Rename with two AT_FDCWD
   Line: 15232 renameat2(AT_FDCWD, "all.filecount.new", AT_FDCWD, "all.filecount", RENAME_NOREPLACE) = 0
   - Expected Source file: "/dir3/dir4/all.filecount.new", OK
   - Expected Target file: "/dir3/dir4/all.filecount", OK

## renameat with explicit dir (and not AT_FDCWD), with and without final /
   Line: 15165 renameat(5</home/lionel/.slocdata>, "old", 5</home/lionel/.slocdata/>, "new")...
   - Expected Source file: "/home/lionel/.slocdata/old", OK
   - Expected Target file: "/home/lionel/.slocdata/new", OK

All tests OK [Successful](tests_status.md#successful)

# Tutorial


 This test ensure that the current version  
 of smk behave as described in the tutorial  

##  Tutorial / start conversion


  This is the "Quick Start" part of the tutorial  

  Run:  
  # converting ogg to mp3:  
  `sox x.ogg x.mp3`  

  Expected:  
```  
run "sox x.ogg x.mp3" because it was not run before
sox x.ogg x.mp3
```  

  # setting Artist and Tittle tags:  
  `id3v2 -a Luke -t Sentinelle x.mp3`  

  Expected:  
```  
run "id3v2 -a Luke -t Sentinelle x.mp3" because it was not run before
id3v2 -a Luke -t Sentinelle x.mp3
```  

  # renaming according to tags:  
  `id3ren -quiet -template=%a  

  Expected:  
```  
run "id3ren -quiet -template=%a-%s.mp3 x.mp3" because it was not run before
id3ren -quiet -template=%a-%s.mp3 x.mp3
```  


Tutorial / start conversion [Successful](tests_status.md#successful)

##  Tutorial / second run


  Run:  
  `smk`  

  Expected: nothing, situation is up to date  
```  
Nothing to run
```  


Tutorial / second run [Successful](tests_status.md#successful)

##  Tutorial / smk do not rebuild if a target is missing!!!

  Run:  
  `rm Luke-Sentinelle.mp3`  
  `smk`  

  Expected:  
```  
Nothing to run
```  


Tutorial / smk do not rebuild if a target is missing!!! [Successful](tests_status.md#successful)

##  Tutorial / unless using the `-mt` / `--build-missing-target` option

  Run:  
  `smk -mt -e`  

  Expected:  
```  
run "sox x.ogg x.mp3" because Target file x.mp3 is missing
sox x.ogg x.mp3
run "id3v2 -a Luke -t Sentinelle x.mp3" because Source file x.mp3 has been updated (YYYY:MM:DD HH:MM:SS.SS)
id3v2 -a Luke -t Sentinelle x.mp3
run "id3ren -quiet -template=%a-%s.mp3 x.mp3" because Target file Luke-Sentinelle.mp3 is missing
id3ren -quiet -template=%a-%s.mp3 x.mp3
```  


Tutorial / unless using the `-mt` / `--build-missing-target` option [Successful](tests_status.md#successful)

##  Tutorial / touch x.ogg

  Run:  
  `touch x.ogg`  
  `rm Luke-Sentinelle.mp3`  
  `smk`  

  Expected:  
```  
run "sox x.ogg x.mp3" because Source file x.ogg has been updated (YYYY:MM:DD HH:MM:SS.SS)
sox x.ogg x.mp3
run "id3v2 -a Luke -t Sentinelle x.mp3" because Source file x.mp3 has been updated (YYYY:MM:DD HH:MM:SS.SS)
id3v2 -a Luke -t Sentinelle x.mp3
run "id3ren -quiet -template=%a-%s.mp3 x.mp3" because Source file x.mp3 is present
id3ren -quiet -template=%a-%s.mp3 x.mp3
```  


##  Tutorial / smk do rebuild if you give the target

  Run:  
  `smk lt -l > out.20`  
  `rm Luke-Sentinelle.mp3`  
  `smk Luke-Sentinelle.mp3`  

  Expected:  
```  
"id3ren -quiet -template=%a-%s.mp3 x.mp3" [] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] Luke-Sentinelle.mp3
"id3v2 -a Luke -t Sentinelle x.mp3" [] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] x.mp3
"sox x.ogg x.mp3" [] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] x.mp3
id3ren -quiet -template=%a-%s.mp3 x.mp3
```  


Tutorial / smk do rebuild if you give the target [Successful](tests_status.md#successful)

# Directory tests



##  Directory tests / mkdir dir1


  Run:  
  `smk run mkdir dir1`  

  Expected:  
```  
mkdir dir1
```  

  Run:  
  `smk st -l`  

  Expected:  
```  
Command "mkdir dir1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - [If absence ] [Dir] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir1

```  

  Run:  
  `smk ls`  

  Expected:  
```  
```  

  Run:  
  `smk lt`  

  Expected:  
```  
dir1
```  

  Run:  
  `smk lu`  

  Expected:  
```  
/usr/lib/locale/fr_FR.utf8/LC_MESSAGES/SYS_LC_MESSAGES
```  

  Run:  
  `smk wn`  

  Expected:  
```  
Nothing new
```  


Directory tests / mkdir dir1 [Successful](tests_status.md#successful)

##  Directory tests / updating dir1


  Run:  
  `smk run touch dir1/f1`  

  Expected:  
```  
touch dir1/f1
```  

  Run:  
  `smk st -l`  

  Expected:  
```  
Command "mkdir dir1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - [If absence ] [Dir] [Normal] [Target] [Updated] [YYYY:MM:DD HH:MM:SS.SS] dir1

Command "touch dir1/f1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir1/f1

```  

  Run:  
  `smk ls`  

  Expected:  
```  
```  

  Run:  
  `smk lt`  

  Expected:  
```  
dir1
dir1/f1
```  

  Run:  
  `smk lu`  

  Expected:  
```  
/usr/lib/locale/fr_FR.utf8/LC_MESSAGES/SYS_LC_MESSAGES
```  

  Run:  
  `smk wn`  

  Expected:  
```  
[Updated] [Target] dir1
```  

  Run:  
  `smk`  

  Expected:  
```  
Nothing to run
```  

  Run:  
  `touch dir1`  

  Expected:  
  It's a target, so touching it should not run the command  
```  
Nothing to run
```  


Directory tests / updating dir1 [Successful](tests_status.md#successful)

##  Directory tests / cleaning dir1


  This test check that smk correctly identify target files,  
  and remove it with "clean", and preserve "unused" files,  
  even if those are in a target dir.  

  Run:  
  `smk run mkdir dir1`  
  `smk run touch dir1/f1`  
  `smk run mkdir dir2`  
  `smk run mv dir1/* dir2`  
  `touch dir1/f5`  
  `mkdir dir2/dir3`  
  `smk st`  

  Expected:  
```  
Command "mkdir dir1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - dir1

Command "mkdir dir2", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - dir2

Command "mv dir1/f1 dir2", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (2)
  - dir2
  - AT_FDCWD</home/lionel/prj/smk/tests/16_dir_ops_tests/dir1/f1
  Targets: (1)
  - dir2/f1

Command "touch dir1/f1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - dir1/f1

```  

  Run:  
  `smk lt -l`  
  (files that should be erased when cleaning)  

  Expected:  
```  
"mkdir dir1" [] [If absence ] [Dir] [Normal] [Target] [Updated] [YYYY:MM:DD HH:MM:SS.SS] dir1
"mkdir dir2" [] [If absence ] [Dir] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir2
"mv dir1/f1 dir2" [] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] dir2/f1
"touch dir1/f1" [] [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir1/f1
```  

  Run:  
  `smk lu -l`  
  (files that should not be erased when cleaning)  

  Expected:  
```  
/home/lionel/prj/smk/tests/16_dir_ops_tests/dir1/f5
/home/lionel/prj/smk/tests/16_dir_ops_tests/dir2/dir3
/usr/lib/locale/fr_FR.utf8/LC_MESSAGES/SYS_LC_MESSAGES
```  

  Run:  
  `smk clean`  

  Expected:  
```  
Deleting file dir2/f1
Deleting dir dir1
Deleting dir dir2
```  


Directory tests / cleaning dir1 [Successful](tests_status.md#successful)

##  Directory tests / accessing dir1 contents, write access

  Run:  
  `mkdir -p dir1`  
  `smk run mkdir dir1/dir2`  
  `smk st -l`  

  Expected:  
```  
Command "mkdir dir1/dir2", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - [If absence ] [Dir] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir1/dir2

```  

  Run:  
  `smk wn`  

  Expected:  
```  
Nothing new
```  

  Run:  
  `smk`  

  Expected:  
```  
Nothing to run
```  

  Run:  
  `touch dir1/f2`  
  `smk wn`  
  nothing expected as dir1 is not involved in a known command  

  Expected:  
```  
Nothing new
```  

  Run:  
  `touch dir1/dir2/f3`  
  `smk wn`  
  dir2 update should be reported, as dir2 is involved in a known command  

  Expected:  
```  
[Updated] [Target] dir1/dir2
```  

  but there is nothing to run  
  Run:  
  `smk`  

  Expected:  
```  
Nothing to run
```  


##  Directory tests / accessing dir1 contents, read access

  `let s now add a command reading dir1`  
  Run:  
  `smk run ls -1 dir1`  

  Expected:  
```  
ls -1 dir1
dir2
f2

```  

  `if nothing changes, nothing to run`  
  Run:  
  `smk`  

  Expected:  
```  
Nothing to run
```  

  `but if we add a file in dir1, ls should re-run`  
  Run:  
  `touch dir1/f4`  
  `smk -e`  

  Expected:  
```  
run "ls -1 dir1" because Source dir dir1 has been updated (YYYY:MM:DD HH:MM:SS.SS)
ls -1 dir1
dir2
f2
f4

```  


Directory tests / accessing dir1 contents, read access [Successful](tests_status.md#successful)

##  Directory tests / removing dir1

  Run:  
  `rm -rf dir1`  

  Expected:  
```  
[Missing] [Target] dir1
[Missing] [Target] dir1/dir2
[Missing] [Target] dir1/f1
```  

  Run:  
  `smk -e`  

  Expected:  
  Missing target, should not run without -mt  
```  
Nothing to run
```  

  Run:  
  `smk -e -mt`  

  Expected:  
  should cause the command to rebuild missing targets  
```  
run "mkdir dir1" because Target dir dir1 is missing
mkdir dir1
run "touch dir1/f1" because Target file dir1/f1 is missing
touch dir1/f1
run "mkdir dir1/dir2" because Target dir dir1/dir2 is missing
mkdir dir1/dir2
```  


Directory tests / removing dir1 [Successful](tests_status.md#successful)
