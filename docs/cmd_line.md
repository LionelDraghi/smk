smk command line
----------------

```
smk -h
```

```

Usage: smk [Options]* Command [Smkfile][:target]

Usage examples:
   on first use              : smk MyBuild.txt
   then, to rebuild, just    : smk
   to run a specific target  : smk MyBuild.txt:target
   or just                   : smk :target

Commands:
   build             : run the build
   status       | st : show what smk knows about the previous
                       runs (commands, sources and targets)
   read-smkfile | rs : show smk's understanding of a Smkfile
   whatsnew     | wn : list the changes since the last run
   add               : add the rest of the command line to
                       default.smk
   run               : equivalent to `add` followed by `build`
   clean             : remove all target files
   reset             : remove all local Smk files
                       (equivalent to rm .smk.*)
   version           : print the smk version
   help              : this message
   dump              : list all the info smk knows about
                       the files, including unused and dirs
   list-runs    | lr : list the runfiles in the current
                       directory
   list-sources | ls : list the sources, except system files
   list-targets | lt : list the targets, except system files
   list-unused  | lu : list the files not involved in the build

   NB: when no command is given, build is assumed

Options:
   -mt  | --missing-targets : build if missing targets
                              (default is to build only if
                               sources are updated)
   -a   | --always-make     : unconditionally make all targets
   -e   | --explain         : explain why each target is made
   -n   | --dry-run         : print the commands that would be
                              executed, but do not execute them
   -sa  | --show-all-files  : also show system files
   -ds  | --dont-shorten    : print files with their full path
   -i   | --ignore-errors   : ignore all errors in commands
                              executed to remake files
   -l   | --long-listing    : use a long listing format when
                              listing files
   -k   | --keep-going      : do as much work as possible
   -We  | --Warnings=error  : treat warnings as errors
   -v   | --verbose
   -q   | --quiet           : no messages unless error,
                              warnings are also ignored
   -h   | --help            : this message

https://github.com/LionelDraghi/smk

```

smk current version
-------------------

```
smk version
```

```
0.4.1-dev
```

