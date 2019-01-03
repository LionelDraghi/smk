# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and version numbering adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

- [Unreleased]

- [0.3.0] - 2019-01-02
> - [Added] `list-unused` / `lu` command added
> - [Added] `dump` command added, that dump `smk` internal data without any filtering

- [0.2.0] - 2018-12-24
> - [Added] `whatsnew` / `wn` command added 
> - [Added] Hard coded ignore list because of /etc/ld.so.cache that changes at each run
> - [Fixed] line considered as section even when the identifier was not immediately followed by a colon 
> - [Added] First "add & run" implementation 
> - [Added] First "section" implementation : `smk smkfile:section`, or `smk :section`. For example `smkfile :mrproper`.
> - [Changed] major command line refactoring  
> - [Changed] line are now run in a shell, and may now include multiple commands and shell specifics constructs, like pipes
> - [Changed] list query now print only file names. To get the previous _[section]command:file_ format, use `-l` / `--long-listing` option

- [0.1.0] - 2018-12-11
> - [Added] Multiline processing, closes #5
> - [Added] -k (--keep-going) added, and previous -i semantics no more hide some -k behaviour, closes #13 

- [0.0.4] - 2018-12-07
> - [Added] `-lr` / `--list-runs` added
> - [Fixed] when using `-rl`, sources count is now coherent, closes #3
> - [Added] `-ss` / `--show-system-files` modify queries listing sources files to list also system files  
> - [Added] no more need to give the smkfile in the command line when there is one (and only one) runfile in the current dir, closes #7  
> - [Added] `--clean` now delete targets, as a `make clean`, thanks [Manuel](https://github.com/mgrojo) for the idea, closes #10  
> - [Added] `-ls` / `--list-sources` & `-la` / `--list-all-sources` added  
> - [Added] `-lt` / `--list-targets` added  
> - [Changed] using consistently `smkfile` (instead of `makefile`) and `runfile` within sources, options and docs  
> - [Changed] `--clean` renamed `--reset`  
> - [Changed] `--list_makefile_` renamed `-rs` / `--read-smkfile`
> - [Changed] `--list-saved-run` renamed `-rl` / `--read-last-run`, closes #9
> - [Fixed] using '-' instead of '_' as separator in long form options, closes #8
> - [Fixed] change in strace command line : `-e trace=%file` changed to `-e trace=file`, closes #1
> - [Fixed] Documentation improvement in Quickstart, closes #4

- [0.0.3] - 2018-11-27
> - [Added] `smkfile` format documentation 
> - [Fixed] Smk was not stopping in case of spawn error even if `-i` was not set
> - [Fixed] `/sys` and `/proc` missing in file system filtering 
> - [Fixed] When starting with a tab, Comment lines where identified as Command line instead
> - [Changed] For better compatibility with Makefile, '@' are now ignored when at the beginning of the line
> - [Changed] `-ls` now indicates how many sources and targets are identified overall (that is including system files)  

- [0.0.2] - 2018-11-24
> - [Added] The `-ls` / `--list_saved_run` option to dump what Smk stored of the previous run of this Makefile
> - [Added] The `-lm` / `--list_makefile` option to see what smk understand from the Makefile
> - [Added] The `-a` / `--always-make` that unconditionally make all targets
> - [Added] The `-e` / `--explain` option to explain why a command is run
> - [Added] The `-i` / `--ignore-errors` to ignore all errors in executed commands
> - [Added] The `-n` / `--dry-run` to print the commands instead of running them

- [0.0.1] - 2018-11-11
> - [Added] Initial prod environment
