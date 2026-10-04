# Use cases



Those features describe real use cases, exercising smk
end to end on directory and audio file processing.



_Table of Contents:_
- Feature: Directory operations
  - [Scenario: mkdir dir1](#scenario-mkdir-dir1)
  - [Scenario: updating dir1](#scenario-updating-dir1)
  - [Scenario: cleaning dir1](#scenario-cleaning-dir1)
  - [Scenario: accessing dir1 contents, write access](#scenario-accessing-dir1-contents-write-access)
  - [Scenario: accessing dir1 contents, read access](#scenario-accessing-dir1-contents-read-access)
  - [Scenario: removing dir1](#scenario-removing-dir1)
- Feature: mp3 conversions
  - [Scenario: start conversion](#scenario-start-conversion)
  - [Scenario: new ogg in dir](#scenario-new-ogg-in-dir)
  - [Scenario: ogg-to-mp3 is modified](#scenario-ogg-to-mp3-is-modified)
  - [Scenario: adding a .ogg file in a subdir](#scenario-adding-a-ogg-file-in-a-subdir)
  - [Scenario: smk clean](#scenario-smk-clean)

# Feature : Directory operations

Those tests check the smk behavior on directories: creation, update,
cleaning, access, and removal.


## Scenario : mkdir dir1

- Given I run `rm -rf default.smk dir1 dir2`
- And I run `smk -q reset`
- When I run `smk run mkdir dir1`
- Then I get `mkdir dir1`

The status shows the new directory as a target:

- When I run `sh -c "smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
Command "mkdir dir1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - [If absence ] [Dir] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir1
```

There is no source, and `dir1` is a target:

- When I run `smk ls`
- Then there is no output
- When I run `smk lt`
- Then I get `dir1`
- When I run `smk lu`
- Then I get `/usr/lib/locale/fr_FR.utf8/LC_MESSAGES/SYS_LC_MESSAGES`
- When I run `smk wn`
- Then I get `Nothing new`

## Scenario : updating dir1

- When I run `sleep 1`
- And I run `smk run touch dir1/f1`
- Then I get `touch dir1/f1`

`dir1` was modified by the command, and is thus reported as updated:

- When I run `sh -c "smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
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

- When I run `smk ls`
- Then there is no output
- When I run `smk lt`
- Then I get
```
dir1
dir1/f1
```
- When I run `smk lu`
- Then I get `/usr/lib/locale/fr_FR.utf8/LC_MESSAGES/SYS_LC_MESSAGES`
- When I run `smk wn`
- Then I get `[Updated] [Target] dir1`

Nothing to run, as the update was recorded during the run:

- When I run `smk`
- Then I get `Nothing to run`

`dir1` is a target, so touching it should not run the command:

- When I run `touch dir1`
- And I run `smk`
- Then I get `Nothing to run`

## Scenario : cleaning dir1

This test checks that smk correctly identifies target files, removes
them with `clean`, and preserves "unused" files, even if those are in
a target dir.

- Given I run `rm -rf default.smk dir1 dir2`
- And I run `smk -q reset`
- And I run `smk -q run mkdir dir1`
- And I run `smk -q run touch dir1/f1`
- And I run `smk -q run mkdir dir2`
- And I run `smk -q run mv dir1/f1 dir2`
- And I run `touch dir1/f5`
- And I run `mkdir dir2/dir3`
- When I run `sh -c "smk st | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
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
  - AT_FDCWD</home/lionel/prj/smk/tests/run/dir1/f1
  Targets: (1)
  - dir2/f1

Command "touch dir1/f1", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - dir1/f1
```

`smk lt -l`: files that should be erased when cleaning:

- When I run `sleep 1`
- And I run `sh -c "smk lt -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
"mkdir dir1" [] [If absence ] [Dir] [Normal] [Target] [Updated] [YYYY:MM:DD HH:MM:SS.SS] dir1
"mkdir dir2" [] [If absence ] [Dir] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir2
"mv dir1/f1 dir2" [] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] dir2/f1
"touch dir1/f1" [] [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir1/f1
```

`smk lu -l`: files that should not be erased when cleaning
(note that `dir1/f5` and `dir2/dir3` are preserved):

- When I run `sh -c "smk lu -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
/home/lionel/prj/smk/tests/run/dir1/f5
/home/lionel/prj/smk/tests/run/dir2/dir3
/usr/lib/locale/fr_FR.utf8/LC_MESSAGES/SYS_LC_MESSAGES
```

- When I run `smk clean`
- Then I get
```
Deleting file dir2/f1
Deleting dir dir1
Deleting dir dir2
```

## Scenario : accessing dir1 contents, write access

- Given I run `rm -rf default.smk dir1 dir2`
- And I run `smk -q reset`
- And I run `mkdir -p dir1`
- When I run `smk -q run mkdir dir1/dir2`
- And I run `sh -c "smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
Command "mkdir dir1/dir2", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (0)
  Targets: (1)
  - [If absence ] [Dir] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] dir1/dir2
```

- When I run `smk wn`
- Then I get `Nothing new`
- When I run `smk`
- Then I get `Nothing to run`

Nothing expected, as `dir1` is not involved in a known command:

- When I run `touch dir1/f2`
- And I run `smk wn`
- Then I get `Nothing new`

`dir2` update should be reported, as `dir2` is involved in a known
command, but there is nothing to run:

- When I run `touch dir1/dir2/f3`
- And I run `smk wn`
- Then I get `[Updated] [Target] dir1/dir2`
- When I run `smk`
- Then I get `Nothing to run`

## Scenario : accessing dir1 contents, read access

Let's now add a command reading `dir1`:

- When I run `smk run ls -1 dir1`
- Then I get
```
ls -1 dir1
dir2
f2
```

If nothing changes, nothing to run:

- When I run `smk`
- Then I get `Nothing to run`

But if we add a file in `dir1`, `ls` should re-run:

- When I run `sleep 1`
- And I run `touch dir1/f4`
- And I run `sh -c "smk -e | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
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
- And I run `smk -q reset`
- And I run `smk -q run mkdir dir1`
- And I run `smk -q run touch dir1/f1`
- And I run `smk -q run mkdir dir1/dir2`
- When I run `rm -rf dir1`
- And I run `smk wn`
- Then I get
```
[Missing] [Target] dir1
[Missing] [Target] dir1/dir2
[Missing] [Target] dir1/f1
```

Missing target: should not run without `-mt`:

- When I run `smk -e`
- Then I get `Nothing to run`

`-mt` should cause the command to rebuild missing targets:

- When I run `smk -e -mt`
- Then I get
```
run "mkdir dir1" because Target dir dir1 is missing
mkdir dir1
run "touch dir1/f1" because Target file dir1/f1 is missing
touch dir1/f1
run "mkdir dir1/dir2" because Target dir dir1/dir2 is missing
mkdir dir1/dir2
```

# Feature : mp3 conversions

Those tests check the smk behavior on a real use case: the conversion of
`ogg` files into `mp3`, run by the `ogg-to-mp3.sh` script, that finds all
`*.ogg` files and converts each of them with `to-mp3.sh`.

The `x.ogg` and `y.ogg` input files are binary: they are copied from
`tests/data/ogg/` in the working directory. The scripts are given by the
background.


### Background:

- Given the executable file `to-mp3.sh`
```sh
#!/bin/sh

## echo sox "$1" "${1%.*}.mp3"
sox "$1" "${1%.*}.mp3"
# ffmpeg -y -t 1 -i "$1" "${1%.*}.mp3"
```
- And the executable file `ogg-to-mp3.sh`
```sh
#!/bin/sh

find -name "*.ogg" -exec ./to-mp3.sh '{}' \;
```

## Scenario : start conversion

- Given I run `rm -f default.smk *.mp3 z.ogg`
- And I run `rm -rf dir1`
- And I run `cp ../data/ogg/x.ogg x.ogg`
- And I run `cp ../data/ogg/y.ogg y.ogg`
- And I run `smk -q reset`
- When I run `smk run ./ogg-to-mp3.sh`
- Then I get `./ogg-to-mp3.sh`

The status shows the sources (including the directories) and the targets.
Note that the working dir is shared with the previously run features:
the `find` command also reports `hello_c` as a source dir:

- When I run `sh -c "smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
Command "./ogg-to-mp3.sh", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (6)
  - [If update  ] [Dir] [Normal] [Source] [Updated] [YYYY:MM:DD HH:MM:SS.SS] ./
  - [If update  ] [Dir] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] hello_c
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ogg-to-mp3.sh
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] to-mp3.sh
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] x.ogg
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] y.ogg
  Targets: (2)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] x.mp3
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] y.mp3
```

## Scenario : new ogg in dir

A new `z.ogg` file is added to the directory:

- When I run `sleep 1`
- And I run `cp x.ogg z.ogg`
- And I run `smk whatsnew`
- Then I get `[Updated] [Source] ./`

## Scenario : ogg-to-mp3 is modified

- Given I run `smk -q run ./ogg-to-mp3.sh`
- When I run `touch ./ogg-to-mp3.sh`
- And I run `sh -c "smk -e | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
run "./ogg-to-mp3.sh" because Source file ogg-to-mp3.sh has been updated (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```

## Scenario : adding a .ogg file in a subdir

A new directory is created, and the conversion is re-run:

- Given I run `mkdir dir1`
- When I run `sleep 1`
- And I run `sh -c "smk wn -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
[Dir] [Normal] [Source] [Updated] [YYYY:MM:DD HH:MM:SS.SS] ./
```

- When I run `sh -c "smk -e run ./ogg-to-mp3.sh | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
run "./ogg-to-mp3.sh" because Source dir ./ has been updated (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```

An ogg file is copied in the subdir, and the conversion is re-run
(the command is implicit, from `default.smk`):

- Given I run `cp x.ogg dir1/t.ogg`
- When I run `sh -c "smk -e run | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
run "./ogg-to-mp3.sh" because Source dir dir1 has been updated (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```

## Scenario : smk clean

- When I run `smk clean`
- Then I get
```
Deleting file dir1/t.mp3
Deleting file x.mp3
Deleting file y.mp3
Deleting file z.mp3
```
