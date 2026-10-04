
# Document: [scenario.md](scenario.md)  
   ### Scenario: [create the sources for this test case](scenario.md): 
   - OK : Given the file `hello.c`  
   - OK : Given the file `main.c`  
   - OK : Given the file `hello.h`  
   - OK : Given the file `MyBuild`  
   - [X] scenario   [create the sources for this test case](scenario.md) pass  

   ### Scenario: [first run](scenario.md): 
   - OK : Given I run `rm -f hello hello.o main.o`  
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk MyBuild`  
   - OK : Then I get  
   - OK : When I run `sh -c "../../smk status -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [first run](scenario.md) pass  

   ### Scenario: [what are those new files in the current dir?](scenario.md): 
   - OK : When I run `ls .smk.MyBuild`  
   - OK : Then I get `.smk.MyBuild`  
   - [X] scenario   [what are those new files in the current dir?](scenario.md) pass  

   ### Scenario: [second smk run](scenario.md): 
   - OK : When I run `../../smk`  
   - OK : Then I get `Nothing to run`  
   - [X] scenario   [second smk run](scenario.md) pass  

   ### Scenario: [let's remove a file](scenario.md): 
   - OK : When I run `rm main.o`  
   - OK : When I run `../../smk`  
   - OK : Then I get `Nothing to run`  
   - OK : When I run `sleep 1`  
   - OK : When I run `sh -c "../../smk -mt -e | sed 's/[0-9]//g'"`  
   - OK : Then I get  
   - OK : When I run `../../smk clean`  
   - OK : Then I get  
   - OK : When I run `../../smk reset`  
   - OK : Then I get `Deleting .smk.MyBuild`  
   - OK : When I run `../../smk lr`  
   - OK : Then I get `No run file`  
   - OK : When I run `../../smk MyBuild`  
   - OK : Then I get  
   - [X] scenario   [let's remove a file](scenario.md) pass  

   ### Scenario: [let's modify a source](scenario.md): 
   - OK : When I run `sleep 1`  
   - OK : When I run `touch hello.c`  
   - OK : When I run `sh -c "../../smk -e | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [let's modify a source](scenario.md) pass  

   ### Scenario: [another smk run](scenario.md): 
   - OK : When I run `../../smk`  
   - OK : Then I get `Nothing to run`  
   - [X] scenario   [another smk run](scenario.md) pass  


## Summary : **Success**, 7 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 7     |
| Empty      | 0     |
| Not Run    | 0     |

