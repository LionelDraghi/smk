
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
