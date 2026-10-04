
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
