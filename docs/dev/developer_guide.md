# Developer guide

Table of contents:
- [Developer guide](#developer-guide)
- [Submitting code / docs](#submitting-code--docs)
- [Design Overview](#design-overview)
- [Tests Overview](#tests-overview)
  - [Tools required to run the tests](#tools-required-to-run-the-tests)

Comments and issues are welcome [here](https://github.com/LionelDraghi/smk/issues/new).  
Feel free to suggest whatever improvement.

Use cases or features suggestions are also welcome.  

# Submitting code / docs

To propose some patch          | git command
-------------------------------|-------------------------------------
 1. Fork the project           | fork button on https://github.com/LionelDraghi/Smk
 2. Clone your own copy        | `git clone https://github.com/your_user_name/Smk.git`
 3. Create your feature branch | `git checkout -b my-new-feature`    
 4. Commit your changes        | `git commit -am 'Add some feature'` 
 5. Push to the branch         | `git push origin my-new-feature`    
 6. Create new Pull Request    | on your GitHub fork, go to "Compare & pull request".

> **NB: please note that code proposed with matching tests and doc is much appreciated!**  

# Design Overview

Main Smk components are:

- **Smk.Main** procedure is the controller, in charge (with separate units) of running operations according to the command line analysis;

- **Smk.Settings** defines various application-wide constants, and stores various parameters set on the command line;

Fixme: **TBC**

# Tests Overview

The global intent is to have tests documenting the software behavior. Test execution results in a global count of passed/failed/empty tests, and in a text output in Markdown format, integrated in this documentation.

## Tools required to run the tests

Besides `make` and the Ada toolchain (see the [Download and build section](../../README.md#downloading-and-building)), the test suite (`make check`) runs `smk` on various commands, and needs the following tools in the path:

- a C compiler (`gcc`), used by the `hello.c` based tests;
- `sox`, `id3v2` and `id3ren`, used by the audio conversion tests (test 12_);
- `sed`, used in the multiline commands tested by the smkfile format tests.

On Debian family:

> `apt install gcc sox id3v2 id3ren`

On Ubuntu, sox does not handle the mp3 format out of the box, and the
audio conversion scenarios fail with
`sox FAIL formats: no handler for file extension 'mp3'`; also install
the format handler:

> `apt install libsox-fmt-mp3`

The tests are organized as follows:

- the **features** are described by bbt scenario files in `docs/Features/`
  (one file per family, grouping several `# Feature` sections, each with
  its scenarios): they are part of the documentation;
- the **tutorial** (`docs/tutorial.md`) is itself a bbt scenario file;
- the **sanity tests** are in `tests/sanity/sanity.md`;
- the Ada **unit test** suite (`tests/unit_file_utilities/`) is driven
  by its own Makefile;
- machine dependent expected outputs and binary inputs (ogg files) are
  kept in `tests/data/`.

The scenarios are written in almost natural English (Given / When / Then),
and are intended to be readable as a documentation of the smk behavior.
A test typically documents (order may vary):

1. When running _this_ command,
2. with _those_ source files or situations (details are not always printed),
3. I should have _this_ result (on standard output, but also on error
   output, and returned code)

`make check` runs all the scenario files in a single bbt invocation,
in a fresh `tests/run/` working directory, that records the execution and
the assertions results in a single `results.md` file (the bbt `--index`
option), written directly in this documentation (see the [Tests](../tests/results.md)
page), which ends with the bbt summary of passed/failed tests. The tutorial
goes one step further: its scenario file is the tutorial itself.