
# Document: [scenario.md](scenario.md)  
   ### Background: [](scenario.md): 
   - OK : Given the environment variable `LC_ALL` is `C`  
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Wrong_Makefile`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [no option](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk hello.c/Wrong_Makefile`  
   - OK : Then I get  
   - OK : Then the error output is  
   - OK : Then I get error  
   - [X] scenario   [no option](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the environment variable `LC_ALL` is `C`  
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Wrong_Makefile`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [keep going](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk -k hello.c/Wrong_Makefile`  
   - OK : Then I get  
   - OK : Then the error output is  
   - OK : Then I get error  
   - [X] scenario   [keep going](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the environment variable `LC_ALL` is `C`  
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Wrong_Makefile`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [ignore errors](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk -i hello.c/Wrong_Makefile`  
   - OK : Then I get  
   - OK : Then the error output is  
   - OK : Then I get no error  
   - [X] scenario   [ignore errors](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the environment variable `LC_ALL` is `C`  
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Wrong_Makefile`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [keep going and ignore errors](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk --keep-going --ignore-errors hello.c/Wrong_Makefile`  
   - OK : Then I get  
   - OK : Then the error output is  
   - OK : Then I get no error  
   - [X] scenario   [keep going and ignore errors](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the environment variable `LC_ALL` is `C`  
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Wrong_Makefile`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [run command fails](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk run non_existing_command`  
   - OK : Then I get  
   - OK : Then the error output is  
   - OK : Then I get error  
   - OK : Then the file `default.smk` contains `non_existing_command`  
   - [X] scenario   [run command fails](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the environment variable `LC_ALL` is `C`  
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Wrong_Makefile`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [other commands after a failed run](scenario.md): 
   - OK : When I run `sh -c "../../smk read-smkfile | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `../../smk status`  
   - OK : Then I get `No recorded run`  
   - OK : When I run `../../smk whatsnew`  
   - OK : Then I get `Nothing new`  
   - OK : When I run `../../smk list-sources`  
   - OK : Then there is no output  
   - OK : When I run `../../smk list-targets`  
   - OK : Then there is no output  
   - OK : When I run `../../smk list-unused`  
   - OK : Then there is no output  
   - [X] scenario   [other commands after a failed run](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the environment variable `LC_ALL` is `C`  
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Wrong_Makefile`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [debug option](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : Given I run `rm -f default.smk`  
   - OK : When I run `sh -c "../../smk -d dump > out.13 2>&1"`  
   - OK : Then the file `out.13` is equal to file `expected.13`  
   - OK : Then I get error  
   - [X] scenario   [debug option](scenario.md) pass  


## Summary : **Success**, 7 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 7     |
| Empty      | 0     |
| Not Run    | 0     |

