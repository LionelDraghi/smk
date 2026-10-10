../../docs/Features/A040_errors.md:223: Warning: the command contains a shell metacharacter ('>'), but commands are not run through a shell: a command shall not contain pipes, redirections, or command substitutions; '>' will be passed as an argument to the command
../../docs/Features/A050_use_cases.md:307: Warning: the command contains a shell metacharacter ('*'), but commands are not run through a shell: a command shall not contain pipes, redirections, or command substitutions; '*' will be passed as an argument to the command

# Document: [sanity.md](../../tests/sanity/sanity.md)  
   ### Background: [](../../tests/sanity/sanity.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - [X] background [](../../tests/sanity/sanity.md) pass  

   ### Scenario: [First `smk`, after `make`, should run no command](../../tests/sanity/sanity.md): 
   - OK : Given I run `rm -f hello_c/hello hello_c/hello.o hello_c/main.o`  
   - OK : And I run `smk reset --quiet`  
   - OK : When I run `smk -q hello_c/Makefile.2`  
   - OK : And I run `smk -e hello_c/Makefile.2`  
   - OK : Then I get  
   - [X] scenario   [First `smk`, after `make`, should run no command](../../tests/sanity/sanity.md) pass  

   ### Background: [](../../tests/sanity/sanity.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - [X] background [](../../tests/sanity/sanity.md) pass  

   ### Scenario: [Second `smk`, should not run any command](../../tests/sanity/sanity.md): 
   - OK : When I run `smk -e hello_c/Makefile.2`  
   - OK : Then I get  
   - [X] scenario   [Second `smk`, should not run any command](../../tests/sanity/sanity.md) pass  

   ### Background: [](../../tests/sanity/sanity.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - [X] background [](../../tests/sanity/sanity.md) pass  

   ### Scenario: [`smk reset`, no more history, should run all commands](../../tests/sanity/sanity.md): 
   - OK : When I run `smk reset --quiet`  
   - OK : And I run `smk -e hello_c/Makefile.2`  
   - OK : Then I get  
   - [X] scenario   [`smk reset`, no more history, should run all commands](../../tests/sanity/sanity.md) pass  

   ### Background: [](../../tests/sanity/sanity.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - [X] background [](../../tests/sanity/sanity.md) pass  

   ### Scenario: [`smk -a`, should run all commands even if not needed](../../tests/sanity/sanity.md): 
   - OK : When I run `smk -e -a hello_c/Makefile.2`  
   - OK : Then I get  
   - [X] scenario   [`smk -a`, should run all commands even if not needed](../../tests/sanity/sanity.md) pass  

   ### Background: [](../../tests/sanity/sanity.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - [X] background [](../../tests/sanity/sanity.md) pass  

   ### Scenario: [`rm main.o` (missing file)](../../tests/sanity/sanity.md): 
   - OK : When I run `sleep 1`  
   - OK : And I run `rm hello_c/main.o`  
   - OK : And I run `sh -c "smk -e --missing-targets hello_c/Makefile.2 | sed 's/[0-9]//g'"`  
   - OK : Then I get  
   - [X] scenario   [`rm main.o` (missing file)](../../tests/sanity/sanity.md) pass  

   ### Background: [](../../tests/sanity/sanity.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - [X] background [](../../tests/sanity/sanity.md) pass  

   ### Scenario: [`touch hello.c` (updated file)](../../tests/sanity/sanity.md): 
   - OK : When I run `sleep 1`  
   - OK : And I run `touch hello_c/hello.c`  
   - OK : And I run `sh -c "smk -e hello_c/Makefile.2 | sed 's/[0-9]//g'"`  
   - OK : Then I get  
   - [X] scenario   [`touch hello.c` (updated file)](../../tests/sanity/sanity.md) pass  

   ### Background: [](../../tests/sanity/sanity.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - [X] background [](../../tests/sanity/sanity.md) pass  

   ### Scenario: [`touch hello.c` and dry run](../../tests/sanity/sanity.md): 
   - OK : When I run `sleep 1.0`  
   - OK : And I run `touch hello_c/hello.c`  
   - OK : And I run `smk whatsnew hello_c/Makefile.2`  
   - OK : Then I get  
   - OK : When I run `sh -c "smk -e -n | sed 's/[0-9]//g'"`  
   - OK : Then I get  
   - OK : When I run `sh -c "smk wn | sed 's/[0-9]//g'"`  
   - OK : Then I get  
   - OK : When I run `sh -c "smk -e hello_c/Makefile.2 | sed 's/[0-9]//g'"`  
   - OK : Then I get  
   - OK : When I run `sh -c "smk wn | sed 's/[0-9]//g'"`  
   - OK : Then I get  
   - [X] scenario   [`touch hello.c` and dry run](../../tests/sanity/sanity.md) pass  


# Document: [A010_queries.md](../Features/A010_queries.md)  
  ## Feature: Read queries  
   ### Background: [](../Features/A010_queries.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.3`  
   - [X] background [](../Features/A010_queries.md) pass  

   ### Scenario: [read-smkfile](../Features/A010_queries.md): 
   - OK : When I run `sh -c "smk read-smkfile hello_c/Makefile.3 | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [read-smkfile](../Features/A010_queries.md) pass  

   ### Background: [](../Features/A010_queries.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.3`  
   - [X] background [](../Features/A010_queries.md) pass  

   ### Scenario: [status](../Features/A010_queries.md): 
   - OK : Given I run `rm -f hello_c/hello hello_c/hello.o hello_c/main.o`  
   - OK : And I run `smk -q reset`  
   - OK : When I run `smk -q build hello_c/Makefile.2`  
   - OK : And I run `sh -c "smk status | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [status](../Features/A010_queries.md) pass  

   ### Background: [](../Features/A010_queries.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.3`  
   - [X] background [](../Features/A010_queries.md) pass  

   ### Scenario: [status, long listing and system files](../Features/A010_queries.md): 
   - OK : Given I run `smk -q reset`  
   - OK : And I run `smk -q build hello_c/Makefile.2`  
   - OK : When I run `sh -c "smk st -l -sa | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g' > out.lpr1.txt"`  
   - OK : Then the file `out.lpr1.txt` is equal to file `../data/expected_lpr1.txt`  
   - [X] scenario   [status, long listing and system files](../Features/A010_queries.md) pass  

   ### Background: [](../Features/A010_queries.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.3`  
   - [X] background [](../Features/A010_queries.md) pass  

   ### Scenario: [status, no previous run](../Features/A010_queries.md): 
   - OK : Given I run `smk -q reset`  
   - OK : When I run `smk status hello_c/Makefile.2`  
   - OK : Then I get `Error : No previous run found.`  
   - OK : And I get error  
   - [X] scenario   [status, no previous run](../Features/A010_queries.md) pass  

   ### Background: [](../Features/A010_queries.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.3`  
   - [X] background [](../Features/A010_queries.md) pass  

   ### Scenario: [status, no smkfile](../Features/A010_queries.md): 
   - OK : Given I run `smk -q reset`  
   - OK : When I run `smk status`  
   - OK : Then I get `Error : No smkfile given, and no existing runfile in dir`  
   - OK : And I get error  
   - [X] scenario   [status, no smkfile](../Features/A010_queries.md) pass  

  ## Feature: List queries  
   ### Background: [](../Features/A010_queries.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.3`  
   - [X] background [](../Features/A010_queries.md) pass  

   ### Scenario: [lr, list-runs](../Features/A010_queries.md): 
   - OK : Given I run `smk -q reset`  
   - OK : When I run `smk lr`  
   - OK : Then I get `No run file`  
   - OK : Given I run `smk -q build hello_c/Makefile.2`  
   - OK : And I run `smk -q build hello_c/Makefile.3`  
   - OK : When I run `smk list-runs`  
   - OK : Then I get  
   - [X] scenario   [lr, list-runs](../Features/A010_queries.md) pass  

   ### Background: [](../Features/A010_queries.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.3`  
   - [X] background [](../Features/A010_queries.md) pass  

   ### Scenario: [lt, list-targets](../Features/A010_queries.md): 
   - OK : When I run `sh -c "smk list-targets --long-listing hello_c/Makefile.2 | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `smk lt hello_c/Makefile.2`  
   - OK : Then I get  
   - [X] scenario   [lt, list-targets](../Features/A010_queries.md) pass  

   ### Background: [](../Features/A010_queries.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.3`  
   - [X] background [](../Features/A010_queries.md) pass  

   ### Scenario: [ls, list-sources](../Features/A010_queries.md): 
   - OK : When I run `sh -c "smk ls -ds hello_c/Makefile.2 > out.ls1.txt"`  
   - OK : Then the file `out.ls1.txt` is equal to file `../data/expected_ls1.txt`  
   - OK : When I run `smk list-sources hello_c/Makefile.2`  
   - OK : Then I get  
   - [X] scenario   [ls, list-sources](../Features/A010_queries.md) pass  

   ### Background: [](../Features/A010_queries.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.3`  
   - [X] background [](../Features/A010_queries.md) pass  

   ### Scenario: [list-sources, show all files](../Features/A010_queries.md): 
   - OK : When I run `sh -c "smk list-sources --show-all-files hello_c/Makefile.2 > out.las1.txt"`  
   - OK : Then the file `out.las1.txt` is equal to file `../data/expected_las1.txt`  
   - OK : When I run `sh -c "smk -l ls -sa hello_c/Makefile.2 | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g' | sort > out.las2.txt"`  
   - OK : Then the file `out.las2.txt` is equal to file `../data/expected_las2.txt`  
   - [X] scenario   [list-sources, show all files](../Features/A010_queries.md) pass  


# Document: [A020_build.md](../Features/A020_build.md)  
  ## Feature: Targets  
   ### Background: [](../Features/A020_build.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.1`  
   - OK : And the file `hello_c/Makefile.2`  
   - [X] background [](../Features/A020_build.md) pass  

   ### Scenario: [dry-run clean](../Features/A020_build.md): 
   - OK : Given I run `smk -q reset`  
   - OK : And I run `smk -q build hello_c/Makefile.2`  
   - OK : When I run `smk clean --dry-run`  
   - OK : Then I get  
   - OK : When I run `smk --explain`  
   - OK : Then I get `Nothing to run`  
   - [X] scenario   [dry-run clean](../Features/A020_build.md) pass  

   ### Background: [](../Features/A020_build.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.1`  
   - OK : And the file `hello_c/Makefile.2`  
   - [X] background [](../Features/A020_build.md) pass  

   ### Scenario: [real clean](../Features/A020_build.md): 
   - OK : When I run `smk clean`  
   - OK : Then I get  
   - OK : When I run `smk -e -mt`  
   - OK : Then I get  
   - [X] scenario   [real clean](../Features/A020_build.md) pass  

   ### Background: [](../Features/A020_build.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.1`  
   - OK : And the file `hello_c/Makefile.2`  
   - [X] background [](../Features/A020_build.md) pass  

   ### Scenario: [build selected target](../Features/A020_build.md): 
   - OK : Given I run `smk -q reset`  
   - OK : And I run `smk -q hello_c/Makefile.1`  
   - OK : When I run `smk build main.o`  
   - OK : Then I get `Nothing to run`  
   - OK : When I run `sleep 1`  
   - OK : And I run `touch hello_c/main.c`  
   - OK : And I run `smk build main.o`  
   - OK : Then I get  
   - [X] scenario   [build selected target](../Features/A020_build.md) pass  

   ### Background: [](../Features/A020_build.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.1`  
   - OK : And the file `hello_c/Makefile.2`  
   - [X] background [](../Features/A020_build.md) pass  

   ### Scenario: [build unknown target](../Features/A020_build.md): 
   - OK : When I run `smk build mainzzzzz.o`  
   - OK : Then I get  
   - [X] scenario   [build unknown target](../Features/A020_build.md) pass  

  ## Feature: Implicit smkfile naming  
   ### Background: [](../Features/A020_build.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.3`  
   - [X] background [](../Features/A020_build.md) pass  

   ### Scenario: [no run file in the directory](../Features/A020_build.md): 
   - OK : Given I run `smk -q reset`  
   - OK : When I run `smk`  
   - OK : Then I get `Error : No smkfile given, and no existing runfile in dir`  
   - OK : And I get error  
   - [X] scenario   [no run file in the directory](../Features/A020_build.md) pass  

   ### Background: [](../Features/A020_build.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.3`  
   - [X] background [](../Features/A020_build.md) pass  

   ### Scenario: [one run file, implicit run](../Features/A020_build.md): 
   - OK : Given I run `smk -q hello_c/Makefile.2`  
   - OK : When I run `sleep 1`  
   - OK : And I run `touch hello_c/hello.c`  
   - OK : And I run `smk`  
   - OK : Then I get  
   - [X] scenario   [one run file, implicit run](../Features/A020_build.md) pass  

   ### Background: [](../Features/A020_build.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.3`  
   - [X] background [](../Features/A020_build.md) pass  

   ### Scenario: [more than one run file](../Features/A020_build.md): 
   - OK : Given I run `smk -q hello_c/Makefile.3`  
   - OK : When I run `smk`  
   - OK : Then I get `Error : No smkfile given, and more than one runfile in dir`  
   - OK : And I get error  
   - [X] scenario   [more than one run file](../Features/A020_build.md) pass  

  ## Feature: Sections  
   ### Background: [](../Features/A020_build.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.4`  
   - [X] background [](../Features/A020_build.md) pass  

   ### Scenario: [specific section building](../Features/A020_build.md): 
   - OK : Given I run `smk -q reset`  
   - OK : And I run `smk build -q hello_c/Makefile.2`  
   - OK : When I run `smk :main.o`  
   - OK : Then I get `Nothing to run`  
   - OK : When I run `sleep 1`  
   - OK : And I run `touch hello_c/main.c`  
   - OK : And I run `smk :main.o`  
   - OK : Then I get  
   - [X] scenario   [specific section building](../Features/A020_build.md) pass  

   ### Background: [](../Features/A020_build.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.4`  
   - [X] background [](../Features/A020_build.md) pass  

   ### Scenario: [unknown section](../Features/A020_build.md): 
   - OK : When I run `smk :qzdsqdq.o`  
   - OK : Then I get `No section "qzdsqdq.o" in hello_c/Makefile.2`  
   - [X] scenario   [unknown section](../Features/A020_build.md) pass  

   ### Background: [](../Features/A020_build.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.4`  
   - [X] background [](../Features/A020_build.md) pass  

   ### Scenario: [smkfile,section notation](../Features/A020_build.md): 
   - OK : Given I run `smk build -q hello_c/Makefile.4`  
   - OK : When I run `touch hello_c/hello.c`  
   - OK : And I run `smk hello_c/Makefile.4:hello.o`  
   - OK : Then I get  
   - OK : When I run `smk -a hello_c/Makefile.4:mrproper`  
   - OK : Then I get  
   - [X] scenario   [smkfile,section notation](../Features/A020_build.md) pass  

   ### Background: [](../Features/A020_build.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Makefile.2`  
   - OK : And the file `hello_c/Makefile.4`  
   - [X] background [](../Features/A020_build.md) pass  

   ### Scenario: [unknown smkfile with section](../Features/A020_build.md): 
   - OK : When I run `smk -a hello_c/Makezzzzzfile.4:mrproper`  
   - OK : Then I get  
   - OK : And I get error  
   - [X] scenario   [unknown smkfile with section](../Features/A020_build.md) pass  

  ## Feature: Add and run commands  
   ### Background: [](../Features/A020_build.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - [X] background [](../Features/A020_build.md) pass  

   ### Scenario: [add and build](../Features/A020_build.md): 
   - OK : Given I run `rm -f hello.o main.o hello default.smk`  
   - OK : And I run `smk -q reset`  
   - OK : And I run `smk add gcc -c hello_c/main.c`  
   - OK : And I run `smk add gcc -c hello_c/hello.c`  
   - OK : And I run `smk add gcc -o hello hello.o main.o`  
   - OK : When I run `smk build`  
   - OK : Then I get  
   - [X] scenario   [add and build](../Features/A020_build.md) pass  

   ### Background: [](../Features/A020_build.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - [X] background [](../Features/A020_build.md) pass  

   ### Scenario: [run](../Features/A020_build.md): 
   - OK : Given I run `rm -f hello.o main.o hello default.smk`  
   - OK : And I run `smk -q reset`  
   - OK : When I run `smk run gcc -c hello_c/main.c`  
   - OK : Then I get `gcc -c hello_c/main.c`  
   - OK : When I run `smk run gcc -c hello_c/hello.c`  
   - OK : Then I get `gcc -c hello_c/hello.c`  
   - OK : When I run `smk run gcc -o hello hello.o main.o`  
   - OK : Then I get `gcc -o hello hello.o main.o`  
   - [X] scenario   [run](../Features/A020_build.md) pass  


# Document: [A030_smkfile_format.md](../Features/A030_smkfile_format.md)  
  ## Feature: Multiline commands  
   ### Background: [](../Features/A030_smkfile_format.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `multiline_smkfile1.txt`  
   - OK : And the file `multiline_smkfile2.txt`  
   - OK : And the file `hill_multiline_smkfile.txt`  
   - [X] background [](../Features/A030_smkfile_format.md) pass  

   ### Scenario: [multiline single command](../Features/A030_smkfile_format.md): 
   - OK : Given I run `smk -q reset`  
   - OK : And I run `sh -c "sloccount hello_c/* | grep 'ansic=' > out.sloccount"`  
   - OK : When I run `smk multiline_smkfile1.txt`  
   - OK : Then I get  
   - [X] scenario   [multiline single command](../Features/A030_smkfile_format.md) pass  

   ### Background: [](../Features/A030_smkfile_format.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `multiline_smkfile1.txt`  
   - OK : And the file `multiline_smkfile2.txt`  
   - OK : And the file `hill_multiline_smkfile.txt`  
   - [X] background [](../Features/A030_smkfile_format.md) pass  

   ### Scenario: [multiline with more commands and pipes](../Features/A030_smkfile_format.md): 
   - OK : Given I run `smk -q reset`  
   - OK : When I run `smk multiline_smkfile2.txt`  
   - OK : Then I get  
   - [X] scenario   [multiline with more commands and pipes](../Features/A030_smkfile_format.md) pass  

   ### Background: [](../Features/A030_smkfile_format.md): 
   - OK : Given the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `multiline_smkfile1.txt`  
   - OK : And the file `multiline_smkfile2.txt`  
   - OK : And the file `hill_multiline_smkfile.txt`  
   - [X] background [](../Features/A030_smkfile_format.md) pass  

   ### Scenario: [hill formatted multiline](../Features/A030_smkfile_format.md): 
   - OK : Given I run `smk -q reset`  
   - OK : When I run `smk hill_multiline_smkfile.txt`  
   - OK : Then I get  
   - OK : And I get error  
   - [X] scenario   [hill formatted multiline](../Features/A030_smkfile_format.md) pass  


# Document: [A040_errors.md](../Features/A040_errors.md)  
  ## Feature: Run errors  
   ### Background: [](../Features/A040_errors.md): 
   - OK : Given the environment variable `LC_ALL` is `C`  
   - OK : And the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Wrong_Makefile`  
   - [X] background [](../Features/A040_errors.md) pass  

   ### Scenario: [no option](../Features/A040_errors.md): 
   - OK : Given I run `smk -q reset`  
   - OK : When I run `smk hello_c/Wrong_Makefile`  
   - OK : Then I get  
   - OK : And the error output is  
   - OK : And I get error  
   - [X] scenario   [no option](../Features/A040_errors.md) pass  

   ### Background: [](../Features/A040_errors.md): 
   - OK : Given the environment variable `LC_ALL` is `C`  
   - OK : And the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Wrong_Makefile`  
   - [X] background [](../Features/A040_errors.md) pass  

   ### Scenario: [keep going](../Features/A040_errors.md): 
   - OK : Given I run `smk -q reset`  
   - OK : When I run `smk -k hello_c/Wrong_Makefile`  
   - OK : Then I get  
   - OK : And the error output is  
   - OK : And I get error  
   - [X] scenario   [keep going](../Features/A040_errors.md) pass  

   ### Background: [](../Features/A040_errors.md): 
   - OK : Given the environment variable `LC_ALL` is `C`  
   - OK : And the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Wrong_Makefile`  
   - [X] background [](../Features/A040_errors.md) pass  

   ### Scenario: [ignore errors](../Features/A040_errors.md): 
   - OK : Given I run `smk -q reset`  
   - OK : When I run `smk -i hello_c/Wrong_Makefile`  
   - OK : Then I get  
   - OK : And the error output is  
   - OK : And I get no error  
   - [X] scenario   [ignore errors](../Features/A040_errors.md) pass  

   ### Background: [](../Features/A040_errors.md): 
   - OK : Given the environment variable `LC_ALL` is `C`  
   - OK : And the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Wrong_Makefile`  
   - [X] background [](../Features/A040_errors.md) pass  

   ### Scenario: [keep going and ignore errors](../Features/A040_errors.md): 
   - OK : Given I run `smk -q reset`  
   - OK : When I run `smk --keep-going --ignore-errors hello_c/Wrong_Makefile`  
   - OK : Then I get  
   - OK : And the error output is  
   - OK : And I get no error  
   - [X] scenario   [keep going and ignore errors](../Features/A040_errors.md) pass  

   ### Background: [](../Features/A040_errors.md): 
   - OK : Given the environment variable `LC_ALL` is `C`  
   - OK : And the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Wrong_Makefile`  
   - [X] background [](../Features/A040_errors.md) pass  

   ### Scenario: [run command fails](../Features/A040_errors.md): 
   - OK : Given I run `rm -f default.smk`  
   - OK : And I run `smk -q reset`  
   - OK : When I run `smk run non_existing_command`  
   - OK : Then I get  
   - OK : And the error output is  
   - OK : And I get error  
   - OK : And the file `default.smk` contains `non_existing_command`  
   - [X] scenario   [run command fails](../Features/A040_errors.md) pass  

   ### Background: [](../Features/A040_errors.md): 
   - OK : Given the environment variable `LC_ALL` is `C`  
   - OK : And the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Wrong_Makefile`  
   - [X] background [](../Features/A040_errors.md) pass  

   ### Scenario: [other commands after a failed run](../Features/A040_errors.md): 
   - OK : When I run `sh -c "smk read-smkfile | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `smk status`  
   - OK : Then I get `No recorded run`  
   - OK : When I run `smk whatsnew`  
   - OK : Then I get `Nothing new`  
   - OK : When I run `smk list-sources`  
   - OK : Then there is no output  
   - OK : When I run `smk list-targets`  
   - OK : Then there is no output  
   - OK : When I run `smk list-unused`  
   - OK : Then there is no output  
   - [X] scenario   [other commands after a failed run](../Features/A040_errors.md) pass  

   ### Background: [](../Features/A040_errors.md): 
   - OK : Given the environment variable `LC_ALL` is `C`  
   - OK : And the directory `hello_c`  
   - OK : And the file `hello_c/hello.c`  
   - OK : And the file `hello_c/main.c`  
   - OK : And the file `hello_c/hello.h`  
   - OK : And the file `hello_c/Wrong_Makefile`  
   - [X] background [](../Features/A040_errors.md) pass  

   ### Scenario: [debug option](../Features/A040_errors.md): 
   - OK : Given I run `smk -q reset`  
   - OK : And I run `rm -f default.smk`  
   - OK : When I run `sh -c "smk -d dump > out.13 2>&1"`  
   - OK : Then the file `out.13` is equal to file `../data/expected.13`  
   - OK : And I get error  
   - [X] scenario   [debug option](../Features/A040_errors.md) pass  

  ## Feature: Command line errors  
   ### Scenario: [help options](../Features/A040_errors.md): 
   - OK : When I run `sh -c "smk -h > out.help1.txt"`  
   - OK : And I run `sh -c "smk help > out.help2.txt"`  
   - OK : Then the file `out.help1.txt` is equal to file `out.help2.txt`  
   - [X] scenario   [help options](../Features/A040_errors.md) pass  

   ### Scenario: [version option](../Features/A040_errors.md): 
   - OK : Given I run `sh -c "grep ^version ../../alire.toml | cut -d'\"' -f2 > out.expected_version.txt"`  
   - OK : When I run `smk version`  
   - OK : Then the output is equal to file `out.expected_version.txt`  
   - [X] scenario   [version option](../Features/A040_errors.md) pass  

   ### Scenario: [illegal command lines](../Features/A040_errors.md): 
   - OK : When I run `sh -c "smk read-smkfile status > out.wrong_cmd_line1.txt 2>&1"`  
   - OK : Then the file `out.wrong_cmd_line1.txt` is equal to file `../data/expected_wrong_cmd_line1.txt`  
   - OK : And I get error  
   - [X] scenario   [illegal command lines](../Features/A040_errors.md) pass  

   ### Scenario: [option given after a command](../Features/A040_errors.md): 
   - OK : When I run `smk reset -l`  
   - OK : Then there is no output  
   - [X] scenario   [option given after a command](../Features/A040_errors.md) pass  

   ### Scenario: [unknown smkfile](../Features/A040_errors.md): 
   - OK : When I run `smk My_Makefile`  
   - OK : Then I get `Error : No smkfile given, and no existing runfile in dir`  
   - OK : And I get error  
   - [X] scenario   [unknown smkfile](../Features/A040_errors.md) pass  


# Document: [A050_use_cases.md](../Features/A050_use_cases.md)  
  ## Feature: Directory operations  
   ### Scenario: [mkdir dir1](../Features/A050_use_cases.md): 
   - OK : Given I run `rm -rf default.smk dir1 dir2`  
   - OK : And I run `smk -q reset`  
   - OK : When I run `smk run mkdir dir1`  
   - OK : Then I get `mkdir dir1`  
   - OK : When I run `sh -c "smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `smk ls`  
   - OK : Then there is no output  
   - OK : When I run `smk lt`  
   - OK : Then I get `dir1`  
   - OK : When I run `smk lu`  
   - OK : Then I get `/usr/lib/locale/fr_FR.utf8/LC_MESSAGES/SYS_LC_MESSAGES`  
   - OK : When I run `smk wn`  
   - OK : Then I get `Nothing new`  
   - [X] scenario   [mkdir dir1](../Features/A050_use_cases.md) pass  

   ### Scenario: [updating dir1](../Features/A050_use_cases.md): 
   - OK : When I run `sleep 1`  
   - OK : And I run `smk run touch dir1/f1`  
   - OK : Then I get `touch dir1/f1`  
   - OK : When I run `sh -c "smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `smk ls`  
   - OK : Then there is no output  
   - OK : When I run `smk lt`  
   - OK : Then I get  
   - OK : When I run `smk lu`  
   - OK : Then I get `/usr/lib/locale/fr_FR.utf8/LC_MESSAGES/SYS_LC_MESSAGES`  
   - OK : When I run `smk wn`  
   - OK : Then I get `[Updated] [Target] dir1`  
   - OK : When I run `smk`  
   - OK : Then I get `Nothing to run`  
   - OK : When I run `touch dir1`  
   - OK : And I run `smk`  
   - OK : Then I get `Nothing to run`  
   - [X] scenario   [updating dir1](../Features/A050_use_cases.md) pass  

   ### Scenario: [cleaning dir1](../Features/A050_use_cases.md): 
   - OK : Given I run `rm -rf default.smk dir1 dir2`  
   - OK : And I run `smk -q reset`  
   - OK : And I run `smk -q run mkdir dir1`  
   - OK : And I run `smk -q run touch dir1/f1`  
   - OK : And I run `smk -q run mkdir dir2`  
   - OK : And I run `smk -q run mv dir1/f1 dir2`  
   - OK : And I run `touch dir1/f5`  
   - OK : And I run `mkdir dir2/dir3`  
   - OK : When I run `sh -c "smk st | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `sleep 1`  
   - OK : And I run `sh -c "smk lt -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `sh -c "smk lu -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `smk clean`  
   - OK : Then I get  
   - [X] scenario   [cleaning dir1](../Features/A050_use_cases.md) pass  

   ### Scenario: [accessing dir1 contents, write access](../Features/A050_use_cases.md): 
   - OK : Given I run `rm -rf default.smk dir1 dir2`  
   - OK : And I run `smk -q reset`  
   - OK : And I run `mkdir -p dir1`  
   - OK : When I run `smk -q run mkdir dir1/dir2`  
   - OK : And I run `sh -c "smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `smk wn`  
   - OK : Then I get `Nothing new`  
   - OK : When I run `smk`  
   - OK : Then I get `Nothing to run`  
   - OK : When I run `touch dir1/f2`  
   - OK : And I run `smk wn`  
   - OK : Then I get `Nothing new`  
   - OK : When I run `touch dir1/dir2/f3`  
   - OK : And I run `smk wn`  
   - OK : Then I get `[Updated] [Target] dir1/dir2`  
   - OK : When I run `smk`  
   - OK : Then I get `Nothing to run`  
   - [X] scenario   [accessing dir1 contents, write access](../Features/A050_use_cases.md) pass  

   ### Scenario: [accessing dir1 contents, read access](../Features/A050_use_cases.md): 
   - OK : When I run `smk run ls -1 dir1`  
   - OK : Then I get  
   - OK : When I run `smk`  
   - OK : Then I get `Nothing to run`  
   - OK : When I run `sleep 1`  
   - OK : And I run `touch dir1/f4`  
   - OK : And I run `sh -c "smk -e | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [accessing dir1 contents, read access](../Features/A050_use_cases.md) pass  

   ### Scenario: [removing dir1](../Features/A050_use_cases.md): 
   - OK : Given I run `rm -rf default.smk dir1 dir2`  
   - OK : And I run `smk -q reset`  
   - OK : And I run `smk -q run mkdir dir1`  
   - OK : And I run `smk -q run touch dir1/f1`  
   - OK : And I run `smk -q run mkdir dir1/dir2`  
   - OK : When I run `rm -rf dir1`  
   - OK : And I run `smk wn`  
   - OK : Then I get  
   - OK : When I run `smk -e`  
   - OK : Then I get `Nothing to run`  
   - OK : When I run `smk -e -mt`  
   - OK : Then I get  
   - [X] scenario   [removing dir1](../Features/A050_use_cases.md) pass  

  ## Feature: mp3 conversions  
   ### Background: [](../Features/A050_use_cases.md): 
   - OK : Given the executable file `to-mp3.sh`  
   - OK : And the executable file `ogg-to-mp3.sh`  
   - [X] background [](../Features/A050_use_cases.md) pass  

   ### Scenario: [start conversion](../Features/A050_use_cases.md): 
   - OK : Given I run `rm -f default.smk *.mp3 z.ogg`  
   - OK : And I run `rm -rf dir1`  
   - OK : And I run `cp ../data/ogg/x.ogg x.ogg`  
   - OK : And I run `cp ../data/ogg/y.ogg y.ogg`  
   - OK : And I run `smk -q reset`  
   - OK : When I run `smk run ./ogg-to-mp3.sh`  
   - OK : Then I get `./ogg-to-mp3.sh`  
   - OK : When I run `sh -c "smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [start conversion](../Features/A050_use_cases.md) pass  

   ### Background: [](../Features/A050_use_cases.md): 
   - OK : Given the executable file `to-mp3.sh`  
   - OK : And the executable file `ogg-to-mp3.sh`  
   - [X] background [](../Features/A050_use_cases.md) pass  

   ### Scenario: [new ogg in dir](../Features/A050_use_cases.md): 
   - OK : When I run `sleep 1`  
   - OK : And I run `cp x.ogg z.ogg`  
   - OK : And I run `smk whatsnew`  
   - OK : Then I get `[Updated] [Source] ./`  
   - [X] scenario   [new ogg in dir](../Features/A050_use_cases.md) pass  

   ### Background: [](../Features/A050_use_cases.md): 
   - OK : Given the executable file `to-mp3.sh`  
   - OK : And the executable file `ogg-to-mp3.sh`  
   - [X] background [](../Features/A050_use_cases.md) pass  

   ### Scenario: [ogg-to-mp3 is modified](../Features/A050_use_cases.md): 
   - OK : Given I run `smk -q run ./ogg-to-mp3.sh`  
   - OK : When I run `touch ./ogg-to-mp3.sh`  
   - OK : And I run `sh -c "smk -e | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [ogg-to-mp3 is modified](../Features/A050_use_cases.md) pass  

   ### Background: [](../Features/A050_use_cases.md): 
   - OK : Given the executable file `to-mp3.sh`  
   - OK : And the executable file `ogg-to-mp3.sh`  
   - [X] background [](../Features/A050_use_cases.md) pass  

   ### Scenario: [adding a .ogg file in a subdir](../Features/A050_use_cases.md): 
   - OK : Given I run `mkdir dir1`  
   - OK : When I run `sleep 1`  
   - OK : And I run `sh -c "smk wn -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : When I run `sh -c "smk -e run ./ogg-to-mp3.sh | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - OK : Given I run `cp x.ogg dir1/t.ogg`  
   - OK : When I run `sh -c "smk -e run | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [adding a .ogg file in a subdir](../Features/A050_use_cases.md) pass  

   ### Background: [](../Features/A050_use_cases.md): 
   - OK : Given the executable file `to-mp3.sh`  
   - OK : And the executable file `ogg-to-mp3.sh`  
   - [X] background [](../Features/A050_use_cases.md) pass  

   ### Scenario: [smk clean](../Features/A050_use_cases.md): 
   - OK : When I run `smk clean`  
   - OK : Then I get  
   - [X] scenario   [smk clean](../Features/A050_use_cases.md) pass  


# Document: [tutorial.md](../tutorial.md)  
   ### Scenario: [create the sources for this test case](../tutorial.md): 
   - OK : Given the file `hello.c`  
   - OK : And the file `main.c`  
   - OK : And the file `hello.h`  
   - OK : And the file `MyBuild`  
   - [X] scenario   [create the sources for this test case](../tutorial.md) pass  

   ### Scenario: [first run](../tutorial.md): 
   - OK : Given I run `rm -f hello hello.o main.o`  
   - OK : And I run `smk -q reset`  
   - OK : When I run `smk MyBuild`  
   - OK : Then I get  
   - OK : When I run `sh -c "smk status -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [first run](../tutorial.md) pass  

   ### Scenario: [what are those new files in the current dir?](../tutorial.md): 
   - OK : When I run `ls .smk.MyBuild`  
   - OK : Then I get `.smk.MyBuild`  
   - [X] scenario   [what are those new files in the current dir?](../tutorial.md) pass  

   ### Scenario: [second smk run](../tutorial.md): 
   - OK : When I run `smk`  
   - OK : Then I get `Nothing to run`  
   - [X] scenario   [second smk run](../tutorial.md) pass  

   ### Scenario: [let's remove a file](../tutorial.md): 
   - OK : When I run `rm main.o`  
   - OK : And I run `smk`  
   - OK : Then I get `Nothing to run`  
   - OK : When I run `sleep 1`  
   - OK : And I run `sh -c "smk -mt -e | sed 's/[0-9]//g'"`  
   - OK : Then I get  
   - OK : When I run `smk clean`  
   - OK : Then I get  
   - OK : When I run `smk reset`  
   - OK : Then I get `Deleting .smk.MyBuild`  
   - OK : When I run `smk lr`  
   - OK : Then I get `No run file`  
   - OK : When I run `smk MyBuild`  
   - OK : Then I get  
   - [X] scenario   [let's remove a file](../tutorial.md) pass  

   ### Scenario: [let's modify a source](../tutorial.md): 
   - OK : When I run `sleep 1`  
   - OK : And I run `touch hello.c`  
   - OK : And I run `sh -c "smk -e | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`  
   - OK : Then I get  
   - [X] scenario   [let's modify a source](../tutorial.md) pass  

   ### Scenario: [another smk run](../tutorial.md): 
   - OK : When I run `smk`  
   - OK : Then I get `Nothing to run`  
   - [X] scenario   [another smk run](../tutorial.md) pass  


## Summary : **Success**, 62 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 62    |
| Empty      | 0     |
| Not Run    | 0     |

# File_Utilities.Short_Path unit tests

1. Subdir with default Prefix : OK
Short_Path (From_Dir => "/home/tests",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

2. Dir with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = mysite/site/d1/idx.txt

3. subdir with Prefix : OK
Short_Path (From_Dir => "/home/tests",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "./") = ./mysite/site/d1/idx.txt

4. Sibling subdir : OK
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = ../../mysite/site/d1/idx.txt

5. Parent dir : OK
Short_Path (From_Dir => "/home/tests/12/34",
            To_File  => "/home/tests/idx.txt") = ../../idx.txt

6. Other Prefix : OK
Short_Path (From_Dir => "/home/tests/12/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt",
            Prefix   => "$PWD/") = $PWD/../mysite/site/d1/idx.txt

7. Root dir : OK
Short_Path (From_Dir => "/",
            To_File  => "/home/tests/mysite/site/d1/idx.txt") = /home/tests/mysite/site/d1/idx.txt

8. File is over dir : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1",
            To_File  => "/home/readme.txt") = ../../../../readme.txt

9. File is over Dir, Dir with final / : OK
Short_Path (From_Dir => "/home/tests/mysite/site/d1/",
            To_File  => "/home/readme.txt") = ../../../../readme.txt

10. File is the current dir : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests") = ./

11. File is over Dir, Dir and File with final / : OK
Short_Path (From_Dir => "/home/tests/",
            To_File  => "/home/tests/") = ./

12. No common part : OK
Short_Path (From_Dir => "/home/toto/src/tests/",
            To_File  => "/opt/GNAT/2018/lib64/libgcc_s.so") = /opt/GNAT/2018/lib64/libgcc_s.so


All tests OK [Successful]

# Analyze_Line unit tests


## execve, should be ignored
   Line: 11750 execve("/opt/GNAT/2018/bin/gcc", ["gcc", "-o", "hello", "hello.o", "main.o"], 0x7ffd629baf60 /* 45 vars */) = 0
   - Expected Call_Type: "ignored", OK

## SIGCHLD line
   Line: 11751 --- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, si_pid=11752, si_uid=1000, si_status=0, si_utime=0, si_stime=0} ---
   - Expected Call_Type: "ignored", OK

## Read openat
   Line: 11750 openat(AT_FDCWD, "/etc/ld.so.cache", O_RDONLY|O_CLOEXEC) = 3</etc/ld.so.cache>
   - Expected Read file: "/etc/ld.so.cache", OK

## Write openat
   Line: 11750 openat(AT_FDCWD, "/tmp/ccvHeGYq.res", O_RDWR|O_CREAT|O_EXCL, 0600) = 3</tmp/ccvHeGYq.res>
   - Expected Write file: "/tmp/ccvHeGYq.res", OK

## Dir openat 
   Line: 2918  openat(AT_FDCWD, "./site/about", O_RDONLY|O_NOCTTY|O_NONBLOCK|O_NOFOLLOW|O_CLOEXEC|O_DIRECTORY) = 3</home/lionel/Proj/smk/tests/mysite/site/about>
   - Expected Read file: "/home/lionel/Proj/smk/tests/mysite/site/about", OK

## Dir openat without AT_FDCWD
   Line: 904   openat(5</home/lionel/Proj/smk/tests/12_mp3_conversions_tests>, "dir1", O_RDONLY|O_NOCTTY|O_NONBLOCK|O_NOFOLLOW|O_CLOEXEC|O_DIRECTORY) = 6</home/lionel/Proj/smk/tests/12_mp3_conversions_tests/dir1>
   - Expected Read file: "/home/lionel/Proj/smk/tests/12_mp3_conversions_tests/dir1", OK

## Access Error (EACCES)
   Line: 25242 mkdir("/usr/lib/python3/dist-packages/click/__pycache__", 0777) = -1 EACCES (Permission denied)
   - Expected Call_Type: "ignored", OK

## File not found (ENOENT)
   Line: 11751 openat(AT_FDCWD, "/tmp/ccQ493FX.ld", O_RDONLY) = -1 ENOENT (No such file or directory)
   - Expected Call_Type: "ignored", OK

## access for exec
   Line: 11750 access("/opt/GNAT/2018/bin/gcc", X_OK) = 0
   - Expected Call_Type: "Ignored", OK

## access file with no dir
   Line: 11750 access("hello.o", F_OK)           = 0
   - Expected Call_Type: "Ignored", OK

## RW access to a dir
   Line: 11750 access("/tmp", R_OK|W_OK|X_OK)    = 0
   - Expected Call_Type: "Ignored", OK

## unlink (rm)
   Line: 11750 unlink("/tmp/ccvHeGYq.res")       = 0
   - Expected Call_Type: "Ignored", OK

## unlinkat AT_REMOVEDIR
   Line: 29164 unlinkat(AT_FDCWD, "./site/about", AT_REMOVEDIR) = 0
   - Expected Call_Type: "Ignored", OK

## Set a current directory for process 30461
   Line: 30461 getcwd("/dir1/dir2", 4096) = 36
   - Expected Call_Type: "ignored", OK

## Set a current directory for process 15232 with final /
   Line: 15232 getcwd("/dir3/dir4/", 4096) = 36
   - Expected Call_Type: "ignored", OK

## Read AND Write test
   Line: 30461 rename("x.mp3", "unknown-unknown.mp3") = 0
   - Expected Source file: "/dir1/dir2/x.mp3", OK
   - Expected Target file: "/dir1/dir2/unknown-unknown.mp3", OK

## Rename with two AT_FDCWD
   Line: 15232 renameat2(AT_FDCWD, "all.filecount.new", AT_FDCWD, "all.filecount", RENAME_NOREPLACE) = 0
   - Expected Source file: "/dir3/dir4/all.filecount.new", OK
   - Expected Target file: "/dir3/dir4/all.filecount", OK

## renameat with explicit dir (and not AT_FDCWD), with and without final /
   Line: 15165 renameat(5</home/lionel/.slocdata>, "old", 5</home/lionel/.slocdata/>, "new")...
   - Expected Source file: "/home/lionel/.slocdata/old", OK
   - Expected Target file: "/home/lionel/.slocdata/new", OK

All tests OK [Successful]
