# Instructions for coding agents

## Commit discipline

- never stage, commit or push without the owner's explicit consent:
  `git add` is as forbidden as `git commit` and `git push`; prepare the
  change, run a full `make` (build, check, doc), a `make clean` to check
  that there is no remaining unwanted file, and report the result;
  wait for the go-ahead before touching the index or the history
- once the owner gives the go-ahead, run the whole sequence in one go:
  `git add` the whole generated state (test results, docs/tests/,
  docs/dashboard.md, docs/cmd_line.md, docs/fixme.md, docs/tests.json,
  tests/tests_count.txt, tests/tests_status.md, updated fixtures...),
  commit, and push. Committing in the middle of the chain freezes
  inconsistent artifacts, such as a dashboard or a tests badge still
  referring to the previous version

## Build

- the project is the Alire crate `smk` (alire.toml); the binary is
  built at the repository root (`./smk`) by `make build`
  (`alr build --validation`), or `make release` (`alr build --release`)
- the generated Alire configuration (src/Alire_config/) is gitignored;
  `alr build` regenerates the gpr, and `alr update` regenerates
  Crate_Version: after a version change in alire.toml, run `alr update`,
  as `alr build` alone does not refresh it
- the tests 13 and 14 sub-projects are built through `alr exec` in their
  Makefiles, because gnatbind is not on the system PATH (the toolchain
  comes from Alire); do not revert this to a plain gprbuild call
- coverage (lcov, genhtml) is also run through `alr exec`, so that lcov
  uses the gcov of the Alire toolchain, matching the gcda format

## Test procedure

- tests are in the 15 `tests/NN_*_tests/` directories, each with its own
  Makefile, driven by `tests/Makefile`; `make check` from the root runs
  them all, plus the coverage report; `cd tests/NN_*_tests && make check`
  runs one suite
- the recording tool is `testrec` (tests/Tools): it writes a testrec.md
  per suite, aggregated in docs/tests/ by `make check`; the tests are
  also the documentation of the behavior
- the external tools required by the test suite (strace, gcc, sox,
  id3v2, id3ren, sed, sdiff, lcov...) are listed in docs/contributing.md,
  with the Debian package names
- run the tests with `LD_LIBRARY_PATH` unset (`env -u LD_LIBRARY_PATH make check`)
  if your environment pollutes it, e.g. from a VSCode extension: its
  bundled libraries break sox, python and apt, and add unwanted files
  in the smk listings, which makes the fixtures differ
- expected files (expected*.*) may legitimately differ from one machine
  to another (system libraries, locale, tool versions): before updating
  a fixture, make sure the difference comes from the environment and not
  from an smk regression; a fixture update must always be justified
  in the report to the owner
- some `sleep 1` in the test Makefiles compensate the file system time
  stamp resolution (cf. docs/design_notes.md): do not remove them
- after `make clean`, verify cleanliness on the file system, not only
  with git status: git ignored files (obj/, alire/, docs/lcov/, the
  `smk` binary, .smk.* run files, out.* files, binaries built in
  tests/hello.c/...) remain invisible

## Changing a feature or an error message format

- the tests are the specification (TDD first); the output formats
  (listings, explanations, error messages, `smk -h`) are stabilized by
  the expected* fixtures: check tests/08_cmd_line_tests and the list
  queries suites before changing any output format
- add a line in docs/changelog.md under the current -dev version,
  following the Keep a Changelog format with the
  [Added]/[Changed]/[Fixed] keywords, from a user perspective only
  (no internal changes: build, refactoring, test tooling)
- `Fixme:` comments in src/, docs/ and tests/ are indexed in docs/fixme.md
  by `make doc`: add one rather than leaving an untracked TODO
- the version is the Alire crate version, displayed through
  Smk_Config.Crate_Version: do not hard code a version in the sources
- after a version change or a `smk -h` change, run `make doc` so that
  docs/cmd_line.md, docs/dashboard.md and docs/tests.json match the
  new binary; the README tests badge reads docs/tests.json through
  raw.githubusercontent.com, so it is refreshed by the next push

## Environment specificities

- smk relies on strace to identify sources and targets (cf.
  docs/design_notes.md): the strace output format is OS and version
  dependent (PID width, localized messages), and the analyzer
  (src/smk-runs-strace_analyzer.adb) must not assume a fixed layout
- smk is currently only tested on Debian x86_64

## Pointers

- to understand smk: README.md, docs/tutorial.md, docs/cmd_line.md
- design and strace output analysis: docs/design_notes.md
- tests overview and required tools: docs/contributing.md
- to do list: docs/fixme.md, docs/limitations.md
- version history: docs/changelog.md (Keep a Changelog; every released
  version, i.e. every GitHub tag, must have its entry)
- the project URL is https://github.com/LionelDraghi/smk; the mkdocs
  web site and GitHub Pages have been removed, the README is the entry
  point and links into docs/
