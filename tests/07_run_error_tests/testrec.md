
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
