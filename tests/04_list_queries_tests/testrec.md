
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
