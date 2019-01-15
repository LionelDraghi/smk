smk command line
----------------

```
smk -h
```

```

Usage : smk [Options]* Command [Smkfile][:target]

Use example :
   when run the first time   : smk MyBuild.txt
   and then,to rebuild, just : smk
   to run a specific target  : smk MyBuild.txt:target
   or just                   : smk :target

Commands :
   build             : run the build
   status       | st : shows what smk knows about the previous
                       runs (commands, sources and targets)
   read-smkfile | rs : shows smk understanding of a Smkfile
   whatsnew     | wn : list changes since last run
   add               : add the rest of the command line to
                       default.smk
   run               : equivalent to `add` followed by `build`
   clean             : remove all targets files
   reset             : remove all local Smk files
                       (equivalent to rm .smk.*)
   version           : put Smk version
   help              : this message
   dump              : list all smk known info on files,
                       including unused and dir
   list-runs    | lr : list runfiles in current directory
   list-sources | ls : list sources, except system files
   list-targets | lt : list targets, except system files
   list-unused  | lu : list files not involved in build

   NB : when no command is given, build is assumed

Options :
   -mt  | --missing-targets : build if missing targets
                              (default is to build only if
                               sources are updated)
   -a   | --always-make     : unconditionally make all targets
   -e   | --explain         : explain why each target is made
   -n   | --dry-run         : print the commands that would be
                              executed, but do not execute them
   -sa  | --show-all-files  : show also system files
   -ds  | --dont-shorten    : print files with full path
   -i   | --ignore-errors   : ignore all errors in commands
                              executed to remake files
   -l   | --long-listing    : use a long listing format when
                              listing files
   -k   | --keep-going      : Do as much work as possible
   -We  | --Warnings=error  : treat warnings as errors
   -v   | --verbose
   -q   | --quiet           : no message unless error,
                              Warning are also ignored
   -h   | --help            : this message

http://lionel.draghi.free.fr/smk/

```

smk current version
-------------------

```
smk version
```

```
0.4.0
```

