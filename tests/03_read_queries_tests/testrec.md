
# Document: [scenario.md](scenario.md)  
   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.3`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [read-smkfile](scenario.md): 
   - OK : When I run `sh -c "../../smk read-smkfile hello.c/Makefile.3 | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [read-smkfile](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.3`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [status](scenario.md): 
   - OK : Given I run `rm -f hello.c/hello hello.c/hello.o hello.c/main.o`  
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk -q build hello.c/Makefile.2`  
   - OK : When I run `sh -c "../../smk status | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [status](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.3`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [status, long listing and system files](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : Given I run `../../smk -q build hello.c/Makefile.2`  
   - OK : When I run `sh -c "../../smk st -l -sa | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g' > out.lpr1.txt"`  
   - OK : Then the file `out.lpr1.txt` is equal to file `expected_lpr1.txt`  
   - [X] scenario   [status, long listing and system files](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.3`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [status, no previous run](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk status hello.c/Makefile.2`  
   - OK : Then I get `Error : No previous run found.`  
   - OK : Then I get error  
   - [X] scenario   [status, no previous run](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.3`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [status, no smkfile](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk status`  
   - OK : Then I get `Error : No smkfile given, and no existing runfile in dir`  
   - OK : Then I get error  
   - [X] scenario   [status, no smkfile](scenario.md) pass  


## Summary : **Success**, 5 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 5     |
| Empty      | 0     |
| Not Run    | 0     |

