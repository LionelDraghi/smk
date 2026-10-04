
# Document: [scenario.md](scenario.md)  
   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [First `smk`, after `make`, should run no command](scenario.md): 
   - OK : Given I run `rm -f hello.c/hello hello.c/hello.o hello.c/main.o`  
   - OK : Given I run `../../smk reset --quiet`  
   - OK : When I run `../../smk -q hello.c/Makefile.2`  
   - OK : When I run `../../smk -e hello.c/Makefile.2`  
   - OK : Then I get  
   - [X] scenario   [First `smk`, after `make`, should run no command](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [Second `smk`, should not run any command](scenario.md): 
   - OK : When I run `../../smk -e hello.c/Makefile.2`  
   - OK : Then I get  
   - [X] scenario   [Second `smk`, should not run any command](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [`smk reset`, no more history, should run all commands](scenario.md): 
   - OK : When I run `../../smk reset --quiet`  
   - OK : When I run `../../smk -e hello.c/Makefile.2`  
   - OK : Then I get  
   - [X] scenario   [`smk reset`, no more history, should run all commands](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [`smk -a`, should run all commands even if not needed](scenario.md): 
   - OK : When I run `../../smk -e -a hello.c/Makefile.2`  
   - OK : Then I get  
   - [X] scenario   [`smk -a`, should run all commands even if not needed](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [`rm main.o` (missing file)](scenario.md): 
   - OK : When I run `sleep 1`  
   - OK : When I run `rm hello.c/main.o`  
   - OK : When I run `sh -c "../../smk -e --missing-targets hello.c/Makefile.2 | sed 's/[0-9]//g'"`  
   - OK : Then I get  
   - [X] scenario   [`rm main.o` (missing file)](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [`touch hello.c` (updated file)](scenario.md): 
   - OK : When I run `sleep 1`  
   - OK : When I run `touch hello.c/hello.c`  
   - OK : When I run `sh -c "../../smk -e hello.c/Makefile.2 | sed 's/[0-9]//g'"`  
   - OK : Then I get  
   - [X] scenario   [`touch hello.c` (updated file)](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [`touch hello.c` and dry run](scenario.md): 
   - OK : When I run `sleep 1.0`  
   - OK : When I run `touch hello.c/hello.c`  
   - OK : When I run `../../smk whatsnew hello.c/Makefile.2`  
   - OK : Then I get  
   - OK : When I run `sh -c "../../smk -e -n | sed 's/[0-9]//g'"`  
   - OK : Then I get  
   - OK : When I run `sh -c "../../smk wn | sed 's/[0-9]//g'"`  
   - OK : Then I get  
   - OK : When I run `sh -c "../../smk -e hello.c/Makefile.2 | sed 's/[0-9]//g'"`  
   - OK : Then I get  
   - OK : When I run `sh -c "../../smk wn | sed 's/[0-9]//g'"`  
   - OK : Then I get  
   - [X] scenario   [`touch hello.c` and dry run](scenario.md) pass  


## Summary : **Success**, 7 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 7     |
| Empty      | 0     |
| Not Run    | 0     |

