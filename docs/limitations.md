Limitations
===========

- unable to manage a relative PATH to smk on the command line:
running `/home/x/y/z/smk Makefile` is OK, but `../smk Makefile` is not.
Nota: answer to this problem is in the Compose procedure provided by `Ada.Directories.Hierachical_File_Names`

Known Bugs
==========
- some run may be missed due to poor precision (1 second granularity) 
  of the Modification_Time in the current GNAT implementation.
  This occurs when you build, "touch" a source and immediatly rebuild (this could happen if you run `smk` from a Makefile!)

> For fixed issues, have a look at the [Changelog](changelog.md)


Submitting issues:
==================

Healthy criticism, discussion and suggestions for improvements [are welcome here](https://github.com/LionelDraghi/smk/issues/new).

