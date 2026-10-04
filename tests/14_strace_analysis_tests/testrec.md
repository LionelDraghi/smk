
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
