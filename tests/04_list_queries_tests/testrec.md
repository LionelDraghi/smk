
# Document: [scenario.md](scenario.md)  
   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.3`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [lr, list-runs](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk lr`  
   - OK : Then I get `No run file`  
   - OK : Given I run `../../smk -q build hello.c/Makefile.2`  
   - OK : Given I run `../../smk -q build hello.c/Makefile.3`  
   - OK : When I run `../../smk list-runs`  
   - OK : Then I get  
   - [X] scenario   [lr, list-runs](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.3`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [lt, list-targets](scenario.md): 
   - OK : When I run `sh -c "../../smk list-targets --long-listing hello.c/Makefile.2 | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `../../smk lt hello.c/Makefile.2`  
   - OK : Then I get  
   - [X] scenario   [lt, list-targets](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.3`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [ls, list-sources](scenario.md): 
   - OK : When I run `sh -c "../../smk ls -ds hello.c/Makefile.2 > out.ls1.txt"`  
   - OK : Then the file `out.ls1.txt` is equal to file `expected_ls1.txt`  
   - OK : When I run `../../smk list-sources hello.c/Makefile.2`  
   - OK : Then I get  
   - [X] scenario   [ls, list-sources](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `hello.c/Makefile.2`  
   - OK : Given the file `hello.c/Makefile.3`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [list-sources, show all files](scenario.md): 
   - OK : When I run `sh -c "../../smk list-sources --show-all-files hello.c/Makefile.2 > out.las1.txt"`  
   - OK : Then the file `out.las1.txt` is equal to file `expected_las1.txt`  
   - OK : When I run `sh -c "../../smk -l ls -sa hello.c/Makefile.2 | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g' | sort > out.las2.txt"`  
   - OK : Then the file `out.las2.txt` is equal to file `expected_las2.txt`  
   - [X] scenario   [list-sources, show all files](scenario.md) pass  


## Summary : **Success**, 4 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 4     |
| Empty      | 0     |
| Not Run    | 0     |

