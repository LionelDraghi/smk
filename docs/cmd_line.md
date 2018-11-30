smk command line
================

smk command line
----------------

```
smk -h
```

```

Usage : smk [Options] make_file

Options :
   -a   | --always-make    : unconditionally make all targets
   -e   | --explain        : explain why each target is made
   -n   | --dry-run        : print the commands that would be executed, but do not execute them
   -i   | --ignore-errors  : ignore all errors in commands executed to remake files

   -lm  | --list-makefile  : dump Smk understanding of a Makefile
   -ls  | --list-saved-run : dump what Smk stored of the previous run of this Makefile

          --clean          : remove all local Smk files (equivalent to rm .smk.*)

   -We  | --Warnings=error : treat warnings as errors
   -v   | --verbose
   -q   | --quiet          : no message unless error.
                             Warning are also ignored.
          --version        : Smk version
   -h   | --help           : this message

http://lionel.draghi.free.fr/smk/

```

smk current version
-------------------

```
smk --version
```

```
0.0.4
```

