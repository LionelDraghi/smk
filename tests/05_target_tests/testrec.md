
# Document: [scenario.md](scenario.md)  
   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.1`  
   - OK : Given the file `hello.c/Makefile.2`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [dry-run clean](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : Given I run `../../smk -q build hello.c/Makefile.2`  
   - OK : When I run `../../smk clean --dry-run`  
   - OK : Then I get  
   - OK : When I run `../../smk --explain`  
   - OK : Then I get `Nothing to run`  
   - [X] scenario   [dry-run clean](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.1`  
   - OK : Given the file `hello.c/Makefile.2`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [real clean](scenario.md): 
   - OK : When I run `../../smk clean`  
   - OK : Then I get  
   - OK : When I run `../../smk -e -mt`  
   - OK : Then I get  
   - [X] scenario   [real clean](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.1`  
   - OK : Given the file `hello.c/Makefile.2`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [build selected target](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : Given I run `../../smk -q hello.c/Makefile.1`  
   - OK : When I run `../../smk build main.o`  
   - OK : Then I get `Nothing to run`  
   - OK : When I run `sleep 1`  
   - OK : When I run `touch hello.c/main.c`  
   - OK : When I run `../../smk build main.o`  
   - OK : Then I get  
   - [X] scenario   [build selected target](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.1`  
   - OK : Given the file `hello.c/Makefile.2`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [build unknown target](scenario.md): 
   - OK : When I run `../../smk build mainzzzzz.o`  
   - OK : Then I get  
   - [X] scenario   [build unknown target](scenario.md) pass  


## Summary : **Success**, 4 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 4     |
| Empty      | 0     |
| Not Run    | 0     |

