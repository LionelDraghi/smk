Table of contents:

- [Limitations](#limitations)
- [Known Bugs](#known-bugs)
- [Fixed issues](#fixed-issues)
- [Submitting issues:](#submitting-issues)

## Limitations

- there is currently no internal dependencies graph build stored. 
  After running all command that for which sources files have been
  updated, smk naively rescreen the whole list of command to check
  if one of those command has updated some sources file of 
  another commands, and then re-run it.
  The consequence of this is that the `--dry-run` option can't
  always show you the whole list of command that should be rerun,
  as the command are not really run, and so all files that would 
  be updated are not.

## Known Bugs

- some run may be missed due to poor precision (1 second granularity) 
  of the Modification_Time function in the current GNAT implementation.
  This occurs when you build, "touch" a source and immediatly rebuild
  (this could happen if you run several `smk` from a Makefile, a shell,
  or when a smkfile is running `smk`!).
  This problem is more precisely described [here](design_notes.md#on-file-systems-time-stamp).

## Fixed issues

For fixed issues, have a look at the [Changelog](changelog.md).


## Submitting issues:

Healthy criticism, discussion and suggestions for improvements [are welcome here](https://github.com/LionelDraghi/smk/discussions).

