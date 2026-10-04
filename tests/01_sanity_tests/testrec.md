
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
