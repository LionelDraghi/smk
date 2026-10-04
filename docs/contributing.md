# Contributing

Table of contents:
- [Contributing](#contributing)
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

The project policy is that code shall be 100% covered (except debug or error specific lines).

Coverage is computed on each `make check`, and non covered code is easy to check with (for example) `chromium docs/lcov/index.html`.  

> **NB : please note that code proposed with matching tests, doc and complete coverage is very appreciated!**  

# Design Overview

Main Smk components are :

- **Smk.Main** procedure is the controller, in charge (with separate units) of running operation according to the command line analysis;

- **Smk.Settings** defines various application wide constant, and stores various parameters set on command line;

Fixme: **TBC**

# Tests Overview

The global intent is to have tests documenting the software behavior. Test execution result in a global count of passed/failed/empty tests, and in a text output in Markdown format, integrated in this documentation.

## Tools required to run the tests

Beside `make` and the Ada toolchain (see the [Download and build section](../README.md#downloading-and-building)), the test suite (`make check`) runs `smk` on various commands, and needs the following tools in the path:

- `strace`, used by `smk` itself to trace files accesses;
- a C compiler (`gcc`), used by the `hello.c` based tests;
- `sox`, `id3v2` and `id3ren`, used by the audio conversion tests (test 12_);
- `sed`, used to neutralize dates in expected outputs;
- `lcov` (providing `genhtml`), used to build the coverage report.

On Debian family:

> `apt install strace gcc sox id3v2 id3ren lcov`

Tests are defined in the `tests` dir, one [bbt](https://github.com/LionelDraghi/bbt)
scenario file (`scenario.md`) per `NN_*_tests` directory, plus two Ada unit
test suites (tests 13 and 14, driven by their own Makefile).

The scenarios are written in almost natural English (Given / When / Then),
and are intended to be readable as a documentation of the smk behavior.
A test typically documents (order may vary) :

1. When running _this_ command,
2. with _those_ sources files or situation (details are not always printed),
3. I should have _this_ result (on standard output, but also on error
   output, and returned code)

`make check` runs all the suites with bbt, that records the execution and
the assertions results in a local `results.md` file per suite. Those files
are aggregated in this documentation (see the [Tests](tests/results.md)
page), together with a global count of passed/failed tests. Tests 15
(the tutorial) goes one step further: its scenario is the tutorial itself.