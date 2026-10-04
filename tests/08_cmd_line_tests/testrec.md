scenario.md:25: Warning: the command contains a shell metacharacter ('>'), but commands are not run through a shell: a command shall not contain pipes, redirections, or command substitutions; '>' will be passed as an argument to the command

# Document: [scenario.md](scenario.md)  
   ### Scenario: [help options](scenario.md): 
   - OK : When I run `sh -c "../../smk -h > out.help1.txt"`  
   - OK : When I run `sh -c "../../smk help > out.help2.txt"`  
   - OK : Then the file `out.help1.txt` is equal to file `out.help2.txt`  
   - [X] scenario   [help options](scenario.md) pass  

   ### Scenario: [version option](scenario.md): 
   - OK : Given I run `sh -c "grep ^version ../../alire.toml | cut -d'\"' -f2 > out.expected_version.txt"`  
   - OK : When I run `../../smk version`  
   - OK : Then the output is equal to file `out.expected_version.txt`  
   - [X] scenario   [version option](scenario.md) pass  

   ### Scenario: [illegal command lines](scenario.md): 
   - OK : When I run `sh -c "../../smk read-smkfile status > out.wrong_cmd_line1.txt 2>&1"`  
   - OK : Then the file `out.wrong_cmd_line1.txt` is equal to file `expected_wrong_cmd_line1.txt`  
   - OK : Then I get error  
   - [X] scenario   [illegal command lines](scenario.md) pass  

   ### Scenario: [option given after a command](scenario.md): 
   - OK : When I run `../../smk reset -l`  
   - OK : Then there is no output  
   - [X] scenario   [option given after a command](scenario.md) pass  

   ### Scenario: [unknown smkfile](scenario.md): 
   - OK : When I run `../../smk My_Makefile`  
   - OK : Then I get `Error : No smkfile given, and no existing runfile in dir`  
   - OK : Then I get error  
   - [X] scenario   [unknown smkfile](scenario.md) pass  


## Summary : **Success**, 5 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 5     |
| Empty      | 0     |
| Not Run    | 0     |

