# How does smk compare with make, and what are the comparable solutions 

## Differences with `Make` 

`Make` is an old, succesful and mature tool, able to do lots of stuff that `Smk` don't even dream of. From that point of vue, there is no comparison, full stop.

Nevertheeless, this table provides some informations that may help to understand in which case you should choose `Smk`!

`Smk` | `Make` 
----|-----
`Smk` aims at optimizing the run of simple processes | `Make` can do really complex things
 The command is run if at least one the targets does not exist, or if any of the sources has been modified since las run | The command is run if at least one the targets does not exist, or is older than any of the files it depends on
`Smk` stores previous run information in a local hidden files. Those info are available thanks to -rl | `Make` don't store run information.
`Smk` observes real dependencies thanks to [strace](https://en.wikipedia.org/wiki/Strace) / ptrace, and is unlikely to miss something | `Make` rely on Sources and Targets description provided by users in the `Makefile`
`Smkfile` are as stupidly simple as possible | Usual Makefiles may be easy to understand, or just a nightmare
`smk` is faster to type, it has only three letter|`make` has four :-)
If no `smkfile` is given on command line, `smk` checks for existing `runfiles`. If there is only one in the current directory, `smk` pick it up. So, feel free to use a descriptive name for your smkfiles, it will still be handy.|If you don't want to give the makefile name on command line, it has to be named `Makefile`. Full stop.

## other solutions

Fixme: to be done