Table of contents:

- [Limitations](#limitations)
- [Known Bugs](#known-bugs)
- [Fixed issues](#fixed-issues)
- [Submitting issues:](#submitting-issues)

## Limitations

- smk observes the commands through the Linux `ptrace` interface: it
  requires Linux kernel >= 5.3 (`PTRACE_GET_SYSCALL_INFO`), and the
  syscall numbers used by the observer are x86_64 for now
  (cf. [Design discussions](dev/design_discussions.md), D1).

- the seccomp subscription filter is installed with
  `PR_SET_NO_NEW_PRIVS`: commands run by `smk` (and their children)
  cannot gain privileges through setuid or setgid binaries anymore.
  Also, non x86_64 programs run by `smk` are not observed: they run
  at full speed, but no dependency is recorded for them.

- there is currently no internal dependency graph stored.
  After running all commands for which source files have been
  updated, smk naively rescans the whole list of commands to check
  if one of those commands has updated a source file of
  another command, and then re-runs it.
  The consequence of this is that the `--dry-run` option can't
  always show you the whole list of commands that should be rerun,
  as the commands are not really run, and so the files that would
  be updated are not.

## Known Bugs

- some runs may be missed due to the poor precision (1 second granularity)
  of the Modification_Time function in the current GNAT implementation.
  This occurs when you build, "touch" a source and immediately rebuild
  (this could happen if you run several `smk` from a Makefile, a shell,
  or when a smkfile is running `smk`!).
  This problem is more precisely described [here](dev/design_notes.md#on-file-systems-time-stamp).

## Fixed issues

For fixed issues, have a look at the [Changelog](changelog.md).


## Submitting issues:

Healthy criticism, discussion and suggestions for improvements [are welcome here](https://github.com/LionelDraghi/smk/discussions).

