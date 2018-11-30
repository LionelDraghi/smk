# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and version numbering adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

- [Unreleased]
> - [Fixed] #1
> - [Fixed] #4

- [0.0.3] - 2018-11-27
> - [Added] `smkfile` format documentaton 
> - [Fixed] Smk was not stopping in case of spawn error even if -i was not set
> - [Fixed] /sys and /proc missing in file system filtering 
> - [Fixed] When starting with a tab, Comment lines where identified as Command line instead
> - [Changed] For better compatibility with Makefile, '@' are now ignored when at the beginning of the line
> - [Changed] -ls now indicates how many sources and targets are identified overall (that is including system files)  

- [0.0.2] - 2018-11-24
> - [Added] The -ls / --list_saved_run option to dump what Smk stored of the previous run of this Makefile
> - [Added] The -lm / --list_makefile option to see what smk understand from the Makefile
> - [Added] The -a / --always-make that unconditionally make all targets
> - [Added] The -e / --explain option to explain why a command is run
> - [Added] The -i / --ignore-errors to ignore all errors in executed commands
> - [Added] The -n / --dry-run to print the commands instead of running them

- [0.0.1] - 2018-11-11
> - [Added] Initial prod environment
