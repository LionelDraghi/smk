
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
