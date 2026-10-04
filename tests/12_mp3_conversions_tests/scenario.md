# mp3 conversions

Those tests check the smk behavior on a real use case: the conversion of
`ogg` files into `mp3`, run by the `ogg-to-mp3.sh` script, that finds all
`*.ogg` files and converts each of them with `to-mp3.sh`.

The `x.ogg` and `y.ogg` input files are binary, and stay in the directory.
The scripts are given by the background.

_Table of Contents:_
- [Scenario: start conversion](#scenario-start-conversion)
- [Scenario: new ogg in dir](#scenario-new-ogg-in-dir)
- [Scenario: ogg-to-mp3 is modified](#scenario-ogg-to-mp3-is-modified)
- [Scenario: adding a .ogg file in a subdir](#scenario-adding-a-ogg-file-in-a-subdir)
- [Scenario: smk clean](#scenario-smk-clean)

### Background:

- Given the executable file `to-mp3.sh`
```sh
#!/bin/sh

## echo sox "$1" "${1%.*}.mp3"
sox "$1" "${1%.*}.mp3"
# ffmpeg -y -t 1 -i "$1" "${1%.*}.mp3"
```
- Given the executable file `ogg-to-mp3.sh`
```sh
#!/bin/sh

find -name "*.ogg" -exec ./to-mp3.sh '{}' \;
```

## Scenario : start conversion

- Given I run `rm -f default.smk *.mp3 z.ogg`
- Given I run `rm -rf dir1`
- Given I run `../../smk -q reset`
- When I run `../../smk run ./ogg-to-mp3.sh`
- Then I get `./ogg-to-mp3.sh`

The status shows the sources (including the directory, that contains
the newly created mp3 files) and the targets:

- When I run `sh -c "../../smk st -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
Command "./ogg-to-mp3.sh", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (5)
  - [If update  ] [Dir] [Normal] [Source] [Updated] [YYYY:MM:DD HH:MM:SS.SS] ./
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
- When I run `cp x.ogg z.ogg`
- When I run `../../smk whatsnew`
- Then I get `[Updated] [Source] ./`

## Scenario : ogg-to-mp3 is modified

- Given I run `../../smk -q run ./ogg-to-mp3.sh`
- When I run `touch ./ogg-to-mp3.sh`
- When I run `sh -c "../../smk -e | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
run "./ogg-to-mp3.sh" because Source file ogg-to-mp3.sh has been updated (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```

## Scenario : adding a .ogg file in a subdir

A new directory is created, and the conversion is re-run:

- Given I run `mkdir dir1`
- When I run `sleep 1`
- When I run `sh -c "../../smk wn -l | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
[Dir] [Normal] [Source] [Updated] [YYYY:MM:DD HH:MM:SS.SS] ./
```

- When I run `sh -c "../../smk -e run ./ogg-to-mp3.sh | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
run "./ogg-to-mp3.sh" because Source dir ./ has been updated (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```

An ogg file is copied in the subdir, and the conversion is re-run
(the command is implicit, from `default.smk`):

- Given I run `cp x.ogg dir1/t.ogg`
- When I run `sh -c "../../smk -e run | sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g' | sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'"`
- Then I get
```
run "./ogg-to-mp3.sh" because Source dir dir1 has been updated (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```

## Scenario : smk clean

- When I run `../../smk clean`
- Then I get
```
Deleting file dir1/t.mp3
Deleting file x.mp3
Deleting file y.mp3
Deleting file z.mp3
```
