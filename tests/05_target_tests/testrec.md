
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
