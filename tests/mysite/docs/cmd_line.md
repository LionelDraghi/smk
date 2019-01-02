smk command line
----------------

```
smk -h
```

```

Usage : smk Command [Options]* [Smkfile]

Usual use example:
   when run the first time:
   > smk MyBuild.txt
   and then just:
   > smk
   to rebuild

Commands :
   build        : run the build
   status       : shows what smk knows about the previous runs
                  (commands, sources and targets)
   read-smkfile : shows Smk understanding of a Smkfile
   clean        : remove all targets files
   reset        : remove all local Smk files
                  (equivalent to rm .smk.*)
   version      : put Smk version
   help | -h    : this message
   -lr  | --list-runs    : list runfiles in current directory
   -ls  | --list-sources : list sources, except system files
   -lt  | --list-targets
   NB : when no command is given, build is assumed

Options :
   -a   | --always-make     : unconditionally make all targets
   -e   | --explain         : explain why each target is made
   -n   | --dry-run         : print the commands that would be
                              executed, but do not execute them
   -sa  | --show-all-files : prevent -ls and -rl from
                              ignoring system files
   -i   | --ignore-errors   : ignore all errors in commands
                              executed to remake files
   -k   | --keep-going      : Do as much work as possible
   -We  | --Warnings=error  : treat warnings as errors
   -v   | --verbose
   -q   | --quiet           : no message unless error,
                              Warning are also ignored

http://lionel.draghi.free.fr/smk/

```

smk current version
-------------------

```
smk version
```

```
0.2.0
```

