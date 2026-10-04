scenario.md:36: Warning: the command contains a shell metacharacter ('*'), but commands are not run through a shell: a command shall not contain pipes, redirections, or command substitutions; '*' will be passed as an argument to the command

# Document: [scenario.md](scenario.md)  
   ### Background: [](scenario.md): 
   - OK : Given the executable file `to-mp3.sh`  
   - OK : Given the executable file `ogg-to-mp3.sh`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [start conversion](scenario.md): 
   - OK : Given I run `rm -f default.smk *.mp3 z.ogg`  
   - OK : Given I run `rm -rf dir1`  
   - OK : Given I run `../../smk -q reset`  
   - OK : When I run `../../smk run ./ogg-to-mp3.sh`  
   - OK : Then I get `./ogg-to-mp3.sh`  
   - OK : When I run `sh -c "../../smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [start conversion](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the executable file `to-mp3.sh`  
   - OK : Given the executable file `ogg-to-mp3.sh`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [new ogg in dir](scenario.md): 
   - OK : When I run `sleep 1`  
   - OK : When I run `cp x.ogg z.ogg`  
   - OK : When I run `../../smk whatsnew`  
   - OK : Then I get `[Updated] [Source] ./`  
   - [X] scenario   [new ogg in dir](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the executable file `to-mp3.sh`  
   - OK : Given the executable file `ogg-to-mp3.sh`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [ogg-to-mp3 is modified](scenario.md): 
   - OK : Given I run `../../smk -q run ./ogg-to-mp3.sh`  
   - OK : When I run `touch ./ogg-to-mp3.sh`  
   - OK : When I run `sh -c "../../smk -e | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [ogg-to-mp3 is modified](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the executable file `to-mp3.sh`  
   - OK : Given the executable file `ogg-to-mp3.sh`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [adding a .ogg file in a subdir](scenario.md): 
   - OK : Given I run `mkdir dir1`  
   - OK : When I run `sleep 1`  
   - OK : When I run `sh -c "../../smk wn -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `sh -c "../../smk -e run ./ogg-to-mp3.sh | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : Given I run `cp x.ogg dir1/t.ogg`  
   - OK : When I run `sh -c "../../smk -e run | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [adding a .ogg file in a subdir](scenario.md) pass  

   ### Background: [](scenario.md): 
   - OK : Given the executable file `to-mp3.sh`  
   - OK : Given the executable file `ogg-to-mp3.sh`  
   - [X] background [](scenario.md) pass  

   ### Scenario: [smk clean](scenario.md): 
   - OK : When I run `../../smk clean`  
   - OK : Then I get  
   - [X] scenario   [smk clean](scenario.md) pass  


## Summary : **Success**, 5 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 5     |
| Empty      | 0     |
| Not Run    | 0     |

