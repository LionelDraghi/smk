
# Document: [scenario.md](scenario.md)  
   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `multiline_smkfile1.txt`  
   - OK : Given the file `multiline_smkfile2.txt`  
   - OK : Given the file `hill_multiline_smkfile.txt`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [multiline single command](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : Given I run `sh -c "sloccount hello.c/* | grep 'ansic=' > out.sloccount"`  
   - OK : When I run `../../smk multiline_smkfile1.txt`  
   - OK : Then I get  
   - [X] scenario   [multiline single command](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `multiline_smkfile1.txt`  
   - OK : Given the file `multiline_smkfile2.txt`  
   - OK : Given the file `hill_multiline_smkfile.txt`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [multiline with more commands and pipes](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk multiline_smkfile2.txt`  
   - OK : Then I get  
   - [X] scenario   [multiline with more commands and pipes](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the directory `hello.c`  
   - OK : Given the file `hello.c/hello.c`  
   - OK : Given the file `hello.c/main.c`  
   - OK : Given the file `hello.c/hello.h`  
   - OK : Given the file `multiline_smkfile1.txt`  
   - OK : Given the file `multiline_smkfile2.txt`  
   - OK : Given the file `hill_multiline_smkfile.txt`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [hill formatted multiline](scenario.md): 
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk hill_multiline_smkfile.txt`  
   - OK : Then I get  
   - OK : Then I get error  
   - [X] scenario   [hill formatted multiline](scenario.md) pass  


## Summary : **Success**, 3 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 3     |
| Empty      | 0     |
| Not Run    | 0     |

