scenario.md:52: Warning: the command contains a shell metacharacter ('*'), but commands are not run through a shell: a command shall not contain pipes, redirections, or command substitutions; '*' will be passed as an argument to the command
scenario.md:69: Warning: the command contains a shell metacharacter ('*'), but commands are not run through a shell: a command shall not contain pipes, redirections, or command substitutions; '*' will be passed as an argument to the command

# Document: [scenario.md](scenario.md)  
   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [add and build](scenario.md): 
   - OK : Given I run `rm -f *.o hello default.smk`  
   - OK : Given I run `../../smk -q reset`  
   - OK : Given I run `../../smk add gcc -c hello.c/main.c`  
   - OK : Given I run `../../smk add gcc -c hello.c/hello.c`  
   - OK : Given I run `../../smk add gcc -o hello hello.o main.o`  
   - OK : When I run `../../smk build`  
   - OK : Then I get  
   - [X] scenario   [add and build](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [run](scenario.md): 
   - OK : Given I run `rm -f *.o hello default.smk`  
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk run gcc -c hello.c/main.c`  
   - OK : Then I get `gcc -c hello.c/main.c`  
   - OK : When I run `../../smk run gcc -c hello.c/hello.c`  
   - OK : Then I get `gcc -c hello.c/hello.c`  
   - OK : When I run `../../smk run gcc -o hello hello.o main.o`  
   - OK : Then I get `gcc -o hello hello.o main.o`  
   - [X] scenario   [run](scenario.md) pass  


## Summary : **Success**, 2 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 2     |
| Empty      | 0     |
| Not Run    | 0     |

