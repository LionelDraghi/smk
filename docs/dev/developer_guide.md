# Developer guide

Table of contents:
- [Developer guide](#developer-guide)
- [submitting code / docs](#submitting-code--docs)
- [Design Overview](#design-overview)
- [Tests Overview](#tests-overview)

Comments and issues are welcome [here](https://github.com/LionelDraghi/smk/issues/new).  
Feel free to suggest whatever improvement.
I'm not a native english speaker. English improvements are welcome, in documentation as in the code.  
Use cases or features suggestions are also welcome.  

# submitting code / docs

To propose some patch          | git command
-------------------------------|-------------------------------------
 1. Fork the project           | fork button on https://github.com/LionelDraghi/Smk
 2. Clone your own copy        | `git clone https://github.com/your_user_name/Smk.git`
 3. Create your feature branch | `git checkout -b my-new-feature`    
 4. Commit your changes        | `git commit -am 'Add some feature'` 
 5. Push to the branch         | `git push origin my-new-feature`    
 6. Create new Pull Request    | on your GitHub fork, go to "Compare & pull request".

> **NB : please note that code proposed with matching tests and doc is very appreciated!**  

# Design Overview

Main Smk components are :

- **Smk.Main** procedure is the controller, in charge (with separate units) of running operation according to the command line analysis;

- **Smk.Settings** defines various application wide constant, and stores various parameters set on command line;

Fixme: **TBC**

# Tests Overview

The global intent is to have tests documenting the software behavior. Test execution result in a global count of passed/failed/empty tests, and in a text output in Markdown format, integrated in this documentation.

## Tools required to run the tests

Beside `make` and the Ada toolchain (see the [Download and build section](../../README.md#downloading-and-building)), the test suite (`make check`) runs `smk` on various commands, and needs the following tools in the path:

- `strace`, used by `smk` itself to trace files accesses;
- a C compiler (`gcc`), used by the `hello.c` based tests;
- `sox`, `id3v2` and `id3ren`, used by the audio conversion tests (test 12_);
- `sed`, used to neutralize dates in expected outputs.

On Debian family:

> `apt install strace gcc sox id3v2 id3ren`

The tests are organized as follows:

- the **features** are described by bbt scenario files in `docs/Features/`
  (one file per family, grouping several `# Feature` sections, each with
  its scenarios): they are part of the documentation;
- the **tutorial** (`docs/tutorial.md`) is itself a bbt scenario file;
- the **sanity tests** are in `tests/sanity/sanity.md`;
- the two Ada **unit test** suites (`tests/unit_file_utilities/` and
  `tests/unit_strace_analysis/`) are driven by their own Makefile;
- machine dependent expected outputs and binary inputs (ogg files) are
  kept in `tests/data/`.

The scenarios are written in almost natural English (Given / When / Then),
and are intended to be readable as a documentation of the smk behavior.
A test typically documents (order may vary) :

1. When running _this_ command,
2. with _those_ sources files or situation (details are not always printed),
3. I should have _this_ result (on standard output, but also on error
   output, and returned code)

`make check` runs all the scenario files in a single bbt invocation,
in a fresh `tests/run/` working directory, that records the execution and
the assertions results in a single `results.md` file (the bbt `--index`
option), written directly in this documentation (see the [Tests](../tests/results.md)
page), which ends with the bbt summary of passed/failed tests. The tutorial
goes one step further: its scenario file is the tutorial itself.