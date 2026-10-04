# Instructions for coding agents

## Commit discipline

- never stage, commit or push without the owner's explicit approval:
  `git add` is as forbidden as `git commit` and `git push`
- the procedure is always, in that order:
  1. make the changes;
  2. run a full `make` (build, check, doc), then a `make clean` to check
     that the repository is clean, and verify the file system (git status
     shows the ignored files);
  3. report the result and STOP: the owner reviews the diffs (e.g. in
     the editor source control view);
  4. only on the owner's explicit go-ahead, run the whole sequence in one
     go: `git add` the whole generated state, commit, and push
- a request such as "on pousse sur GitHub" in the task is the goal, not
  the go-ahead: the go-ahead is a separate, explicit approval given by
  the owner AFTER reviewing the diffs; it may come in a later message;
  when in doubt, wait
- once the owner gives the go-ahead, `git add` the whole generated state
  (test results, docs/tests/, docs/dashboard.md, docs/cmd_line.md,
  docs/fixme.md, docs/tests.json, tests/tests_count.txt,
  tests/tests_status.md, updated fixtures...), commit, and push.
  Committing in the middle of the chain freezes inconsistent artifacts,
  such as a dashboard or a tests badge still referring to the previous
  version

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

- tests are [bbt](https://github.com/LionelDraghi/bbt) scenarios, one
  `scenario.md` per `tests/NN_*_tests/` directory, run by `tests/Makefile`
  with `bbt -k --yes --index testrec.md scenario.md`; `make check` from the
  root runs them all, plus the coverage report; `cd tests/NN_*_tests && bbt
  -k --yes scenario.md` runs one suite
- each scenario.md starts with a `_Table of Contents:_` header listing its
  scenarios with anchors, as in the bbt features files
  (../bbt/docs/features)
- tests 13 (file utilities) and 14 (strace analysis) are Ada unit tests,
  not bbt scenarios: they are still driven by their own Makefile
- the bbt run of each suite writes a `testrec.md` file, aggregated in
  docs/tests/ by `make check`; the tests are also the documentation of
  the behavior, and tests/15_tutorial_tests/scenario.md is written to
  be, at the end, the tutorial itself (docs/tutorial.md)
- the external tools required by the test suite (strace, gcc, sox,
  id3v2, id3ren, sed, lcov...) are listed in docs/contributing.md,
  with the Debian package names
- run the tests with `LD_LIBRARY_PATH` unset (`env -u LD_LIBRARY_PATH make check`)
  if your environment pollutes it, e.g. from a VSCode extension: its
  bundled libraries break sox, python and apt, and add unwanted files
  in the smk listings, which makes the fixtures differ

### bbt authoring rules

- bbt does not run commands through a shell: pipes, redirections and
  command substitutions must be wrapped in `sh -c "..."`, with double
  quotes outside (that bbt strips while grouping the argument) and
  single quotes inside; never use backticks inside the command, they
  would end the bbt code span
- the Background applies before EACH scenario (Gherkin semantics):
  use it for create-if-none inputs only (`- Given the file \`x\``), never
  for state reset, otherwise the state chaining between scenarios is
  broken; put resets and cleanups as `Given` steps of the first scenario
  of a state chain
- to create a script, use `- Given the executable file \`x\` containing`
  (create-if-none + executable bit); avoid the `new` form in a
  Background, it erases and rewrites the script before each scenario,
  and the timestamp change perturbs the run analysis
- `- Given there is no \`x\` file` prompts before deleting: use
  `- Given I run \`rm -f x\`` instead, or run bbt with `--yes`
- dates in expected outputs are neutralized by piping the command
  through `sed 's/[0-9][0-9]*-[0-9][0-9]-[0-9][0-9]/YYYY:MM:DD/g'` and
  `sed 's/[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9]/HH:MM:SS.SS/g'`
  (wrapped in `sh -c`), so that the fixtures don't depend on the run time
- when a tool message is checked (gcc, ld, strace...), set
  `- Given the environment variable \`LC_ALL\` is \`C\``, so that the
  message does not depend on the machine language settings
- expected outputs that depend on the machine (system files, absolute
  paths) are compared to golden files (`expected_*`) with
  `- Then the file \`out\` is equal to file \`expected\``; never modify
  a golden file without the owner's agreement; before updating one,
  make sure the difference comes from the environment and not from an
  smk regression, and justify the update in the report to the owner
- some `sleep 1` steps compensate the file system time stamp resolution
  (cf. docs/design_notes.md): do not remove them
- after `make clean`, verify cleanliness on the file system, not only
  with git status: git ignored files (obj/, alire/, docs/lcov/, the
  `smk` binary, .smk.* run files, out.* files, scenario.md.out, the
  local hello.c/ dirs and binaries built by the scenarios...) remain
  invisible

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
