# Directory tests

Those tests check the smk behavior on directories: creation, update,
cleaning, access, and removal.

_Table of Contents:_
- [Scenario: mkdir dir1](#scenario-mkdir-dir1)
- [Scenario: updating dir1](#scenario-updating-dir1)
- [Scenario: cleaning dir1](#scenario-cleaning-dir1)
- [Scenario: accessing dir1 contents, write access](#scenario-accessing-dir1-contents-write-access)
- [Scenario: accessing dir1 contents, read access](#scenario-accessing-dir1-contents-read-access)
- [Scenario: removing dir1](#scenario-removing-dir1)

## Scenario : mkdir dir1

- Given I run `rm -rf default.smk dir1 dir2`
- Given I run `../../smk -q reset`
- When I run `../../smk run mkdir dir1`
- Then I get `mkdir dir1`

The status shows the new directory as a target:

- When I run `sh -c "../../smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
Command "mkdir dir1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - [If absence ] [Dir] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir1
```

There is no source, and `dir1` is a target:

- When I run `../../smk ls`
- Then there is no output
- When I run `../../smk lt`
- Then I get `dir1`
- When I run `../../smk lu`
- Then I get `/usr/lib/locale/fr_FR.utf8/LC_MESSAGES/SYS_LC_MESSAGES`
- When I run `../../smk wn`
- Then I get `Nothing new`

## Scenario : updating dir1

- When I run `sleep 1`
- When I run `../../smk run touch dir1/f1`
- Then I get `touch dir1/f1`

`dir1` was modified by the command, and is thus reported as updated:

- When I run `sh -c "../../smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
Command "mkdir dir1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - [If absence ] [Dir] [Normal] [Target] [Updated] [YYYY:MM:DD HH:MM:SS.SS] dir1

Command "touch dir1/f1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir1/f1
```

- When I run `../../smk ls`
- Then there is no output
- When I run `../../smk lt`
- Then I get
```
dir1
dir1/f1
```
- When I run `../../smk lu`
- Then I get `/usr/lib/locale/fr_FR.utf8/LC_MESSAGES/SYS_LC_MESSAGES`
- When I run `../../smk wn`
- Then I get `[Updated] [Target] dir1`

Nothing to run, as the update was recorded during the run:

- When I run `../../smk`
- Then I get `Nothing to run`

`dir1` is a target, so touching it should not run the command:

- When I run `touch dir1`
- When I run `../../smk`
- Then I get `Nothing to run`

## Scenario : cleaning dir1

This test checks that smk correctly identifies target files, removes
them with `clean`, and preserves "unused" files, even if those are in
a target dir.

- Given I run `rm -rf default.smk dir1 dir2`
- Given I run `../../smk -q reset`
- Given I run `../../smk -q run mkdir dir1`
- Given I run `../../smk -q run touch dir1/f1`
- Given I run `../../smk -q run mkdir dir2`
- Given I run `../../smk -q run mv dir1/f1 dir2`
- Given I run `touch dir1/f5`
- Given I run `mkdir dir2/dir3`

- When I run `sh -c "../../smk st | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
Command "mkdir dir1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - dir1

Command "mkdir dir2", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - dir2

Command "mv dir1/f1 dir2", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (2)
  - dir2
  - AT_FDCWD</home/lionel/prj/smk/tests/16_dir_ops_tests/dir1/f1
  Targets: (1)
  - dir2/f1

Command "touch dir1/f1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - dir1/f1
```

`smk lt -l`: files that should be erased when cleaning:

- When I run `sleep 1`
- When I run `sh -c "../../smk lt -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
"mkdir dir1" [] [If absence ] [Dir] [Normal] [Target] [Updated] [YYYY:MM:DD HH:MM:SS.SS] dir1
"mkdir dir2" [] [If absence ] [Dir] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir2
"mv dir1/f1 dir2" [] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] dir2/f1
"touch dir1/f1" [] [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir1/f1
```

`smk lu -l`: files that should not be erased when cleaning
(note that `dir1/f5` and `dir2/dir3` are preserved):

- When I run `sh -c "../../smk lu -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
/home/lionel/prj/smk/tests/16_dir_ops_tests/dir1/f5
/home/lionel/prj/smk/tests/16_dir_ops_tests/dir2/dir3
/usr/lib/locale/fr_FR.utf8/LC_MESSAGES/SYS_LC_MESSAGES
```

- When I run `../../smk clean`
- Then I get
```
Deleting file dir2/f1
Deleting dir dir1
Deleting dir dir2
```

## Scenario : accessing dir1 contents, write access

- Given I run `rm -rf default.smk dir1 dir2`
- Given I run `../../smk -q reset`
- Given I run `mkdir -p dir1`
- When I run `../../smk -q run mkdir dir1/dir2`
- When I run `sh -c "../../smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
Command "mkdir dir1/dir2", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - [If absence ] [Dir] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir1/dir2
```

- When I run `../../smk wn`
- Then I get `Nothing new`
- When I run `../../smk`
- Then I get `Nothing to run`

Nothing expected, as `dir1` is not involved in a known command:

- When I run `touch dir1/f2`
- When I run `../../smk wn`
- Then I get `Nothing new`

`dir2` update should be reported, as `dir2` is involved in a known
command, but there is nothing to run:

- When I run `touch dir1/dir2/f3`
- When I run `../../smk wn`
- Then I get `[Updated] [Target] dir1/dir2`
- When I run `../../smk`
- Then I get `Nothing to run`

## Scenario : accessing dir1 contents, read access

Let's now add a command reading `dir1`:

- When I run `../../smk run ls -1 dir1`
- Then I get
```
ls -1 dir1
dir2
f2
```

If nothing changes, nothing to run:

- When I run `../../smk`
- Then I get `Nothing to run`

But if we add a file in `dir1`, `ls` should re-run:

- When I run `sleep 1`
- When I run `touch dir1/f4`
- When I run `sh -c "../../smk -e | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
run "ls -1 dir1" because Source dir dir1 has been updated (YYYY:MM:DD HH:MM:SS.SS)
ls -1 dir1
dir2
f2
f4
```

## Scenario : removing dir1

- Given I run `rm -rf default.smk dir1 dir2`
- Given I run `../../smk -q reset`
- Given I run `../../smk -q run mkdir dir1`
- Given I run `../../smk -q run touch dir1/f1`
- Given I run `../../smk -q run mkdir dir1/dir2`

- When I run `rm -rf dir1`
- When I run `../../smk wn`
- Then I get
```
[Missing] [Target] dir1
[Missing] [Target] dir1/dir2
[Missing] [Target] dir1/f1
```

Missing target: should not run without `-mt`:

- When I run `../../smk -e`
- Then I get `Nothing to run`

`-mt` should cause the command to rebuild missing targets:

- When I run `../../smk -e -mt`
- Then I get
```
run "mkdir dir1" because Target dir dir1 is missing
mkdir dir1
run "touch dir1/f1" because Target file dir1/f1 is missing
touch dir1/f1
run "mkdir dir1/dir2" because Target dir dir1/dir2 is missing
mkdir dir1/dir2
```
