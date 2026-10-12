# How does smk compare with make, and what are the comparable solutions

## Differences with `Make`

`Make` is an old, successful and mature tool, able to do lots of stuff that `Smk` doesn't even dream of. From that point of view, there is no comparison, full stop.

Nevertheless, this table provides some information that may help to understand in which case you should choose `Smk`!

`Smk` | `Make` 
----|-----
`Smk` aims at optimizing the run of simple processes | `Make` can do really complex things
 The command is run if at least one of the targets does not exist, or if any of the sources has been modified since the last run | The command is run if at least one of the targets does not exist, or is older than any of the files it depends on
`Smk` stores previous run information in local hidden files. This information is available thanks to `lr` | `Make` doesn't store run information.
`Smk` observes real dependencies thanks to the Linux [ptrace](https://man7.org/linux/man-pages/man2/ptrace.2.html) interface, and is unlikely to miss something | `Make` relies on the Sources and Targets description provided by users in the `Makefile`
`Smkfiles` are as stupidly simple as possible | Usual Makefiles may be easy to understand, or just a nightmare
`smk` is faster to type, it has only three letters|`make` has four :-)
If no `smkfile` is given on command line, `smk` checks for existing `runfiles`. If there is only one in the current directory, `smk` picks it up. So, feel free to use a descriptive name for your smkfiles, it will still be handy.|If you don't want to give the makefile name on command line, it has to be named `Makefile`. Full stop.

## Other solutions

Fixme: to be done
