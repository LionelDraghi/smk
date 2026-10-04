
# Document: [scenario.md](scenario.md)  
   ### Scenario: [mkdir dir1](scenario.md): 
   - OK : Given I run `rm -rf default.smk dir1 dir2`  
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk run mkdir dir1`  
   - OK : Then I get `mkdir dir1`  
   - OK : When I run `sh -c "../../smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `../../smk ls`  
   - OK : Then there is no output  
   - OK : When I run `../../smk lt`  
   - OK : Then I get `dir1`  
   - OK : When I run `../../smk lu`  
   - OK : Then I get `/usr/lib/locale/fr_FR.utf8/LC_MESSAGES/SYS_LC_MESSAGES`  
   - OK : When I run `../../smk wn`  
   - OK : Then I get `Nothing new`  
   - [X] scenario   [mkdir dir1](scenario.md) pass  

   ### Scenario: [updating dir1](scenario.md): 
   - OK : When I run `sleep 1`  
   - OK : When I run `../../smk run touch dir1/f1`  
   - OK : Then I get `touch dir1/f1`  
   - OK : When I run `sh -c "../../smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `../../smk ls`  
   - OK : Then there is no output  
   - OK : When I run `../../smk lt`  
   - OK : Then I get  
   - OK : When I run `../../smk lu`  
   - OK : Then I get `/usr/lib/locale/fr_FR.utf8/LC_MESSAGES/SYS_LC_MESSAGES`  
   - OK : When I run `../../smk wn`  
   - OK : Then I get `[Updated] [Target] dir1`  
   - OK : When I run `../../smk`  
   - OK : Then I get `Nothing to run`  
   - OK : When I run `touch dir1`  
   - OK : When I run `../../smk`  
   - OK : Then I get `Nothing to run`  
   - [X] scenario   [updating dir1](scenario.md) pass  

   ### Scenario: [cleaning dir1](scenario.md): 
   - OK : Given I run `rm -rf default.smk dir1 dir2`  
   - OK : Given I run `../../smk -q reset`  
   - OK : Given I run `../../smk -q run mkdir dir1`  
   - OK : Given I run `../../smk -q run touch dir1/f1`  
   - OK : Given I run `../../smk -q run mkdir dir2`  
   - OK : Given I run `../../smk -q run mv dir1/f1 dir2`  
   - OK : Given I run `touch dir1/f5`  
   - OK : Given I run `mkdir dir2/dir3`  
   - OK : When I run `sh -c "../../smk st | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `sleep 1`  
   - OK : When I run `sh -c "../../smk lt -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `sh -c "../../smk lu -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `../../smk clean`  
   - OK : Then I get  
   - [X] scenario   [cleaning dir1](scenario.md) pass  

   ### Scenario: [accessing dir1 contents, write access](scenario.md): 
   - OK : Given I run `rm -rf default.smk dir1 dir2`  
   - OK : Given I run `../../smk -q reset`  
   - OK : Given I run `mkdir -p dir1`  
   - OK : When I run `../../smk -q run mkdir dir1/dir2`  
   - OK : When I run `sh -c "../../smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `../../smk wn`  
   - OK : Then I get `Nothing new`  
   - OK : When I run `../../smk`  
   - OK : Then I get `Nothing to run`  
   - OK : When I run `touch dir1/f2`  
   - OK : When I run `../../smk wn`  
   - OK : Then I get `Nothing new`  
   - OK : When I run `touch dir1/dir2/f3`  
   - OK : When I run `../../smk wn`  
   - OK : Then I get `[Updated] [Target] dir1/dir2`  
   - OK : When I run `../../smk`  
   - OK : Then I get `Nothing to run`  
   - [X] scenario   [accessing dir1 contents, write access](scenario.md) pass  

   ### Scenario: [accessing dir1 contents, read access](scenario.md): 
   - OK : When I run `../../smk run ls -1 dir1`  
   - OK : Then I get  
   - OK : When I run `../../smk`  
   - OK : Then I get `Nothing to run`  
   - OK : When I run `sleep 1`  
   - OK : When I run `touch dir1/f4`  
   - OK : When I run `sh -c "../../smk -e | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [accessing dir1 contents, read access](scenario.md) pass  

   ### Scenario: [removing dir1](scenario.md): 
   - OK : Given I run `rm -rf default.smk dir1 dir2`  
   - OK : Given I run `../../smk -q reset`  
   - OK : Given I run `../../smk -q run mkdir dir1`  
   - OK : Given I run `../../smk -q run touch dir1/f1`  
   - OK : Given I run `../../smk -q run mkdir dir1/dir2`  
   - OK : When I run `rm -rf dir1`  
   - OK : When I run `../../smk wn`  
   - OK : Then I get  
   - OK : When I run `../../smk -e`  
   - OK : Then I get `Nothing to run`  
   - OK : When I run `../../smk -e -mt`  
   - OK : Then I get  
   - [X] scenario   [removing dir1](scenario.md) pass  


## Summary : **Success**, 6 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 6     |
| Empty      | 0     |
| Not Run    | 0     |

