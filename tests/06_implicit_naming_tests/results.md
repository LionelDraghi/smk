
# Document: [scenario.md](scenario.md)  
   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.3`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [no run file in the directory](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk`  
   - OK : Then I get `Error : No smkfile given, and no existing runfile in dir`  
   - OK : Then I get error  
   - [X] scenario   [no run file in the directory](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.3`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [one run file, implicit run](scenario.md): 
   - OK : Given I run `../../smk -q hello.c/Makefile.2`  
   - OK : When I run `sleep 1`  
   - OK : When I run `touch hello.c/hello.c`  
   - OK : When I run `../../smk`  
   - OK : Then I get  
   - [X] scenario   [one run file, implicit run](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.3`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [more than one run file](scenario.md): 
   - OK : Given I run `../../smk -q hello.c/Makefile.3`  
   - OK : When I run `../../smk`  
   - OK : Then I get `Error : No smkfile given, and more than one runfile in dir`  
   - OK : Then I get error  
   - [X] scenario   [more than one run file](scenario.md) pass  


## Summary : **Success**, 3 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 3     |
| Empty      | 0     |
| Not Run    | 0     |

