
# Document: [scenario.md](scenario.md)  
   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.4`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [specific section building](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : Given I run `../../smk build -q hello.c/Makefile.2`  
   - OK : When I run `../../smk :main.o`  
   - OK : Then I get `Nothing to run`  
   - OK : When I run `sleep 1`  
   - OK : When I run `touch hello.c/main.c`  
   - OK : When I run `../../smk :main.o`  
   - OK : Then I get  
   - [X] scenario   [specific section building](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.4`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [unknown section](scenario.md): 
   - OK : When I run `../../smk :qzdsqdq.o`  
   - OK : Then I get `No section "qzdsqdq.o" in hello.c/Makefile.2`  
   - [X] scenario   [unknown section](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.4`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [smkfile,section notation](scenario.md): 
   - OK : Given I run `../../smk build -q hello.c/Makefile.4`  
   - OK : When I run `touch hello.c/hello.c`  
   - OK : When I run `../../smk hello.c/Makefile.4:hello.o`  
   - OK : Then I get  
   - OK : When I run `../../smk -a hello.c/Makefile.4:mrproper`  
   - OK : Then I get  
   - [X] scenario   [smkfile,section notation](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.4`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [unknown smkfile with section](scenario.md): 
   - OK : When I run `../../smk -a hello.c/Makezzzzzfile.4:mrproper`  
   - OK : Then I get  
   - OK : Then I get error  
   - [X] scenario   [unknown smkfile with section](scenario.md) pass  


## Summary : **Success**, 4 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 4     |
| Empty      | 0     |
| Not Run    | 0     |

