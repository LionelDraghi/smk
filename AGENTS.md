# Instructions for coding agents

This file is the entry point for coding agents working on smk.

**Before any other action, read the shared template referenced below
and follow it.**

The shared rules for Lionel's Ada projects live in the [reference
repository on GitHub](https://github.com/LionelDraghi/ada-project-template/blob/main/AGENTS.md)
and apply here in full:
non-negotiable git rules (in brief: no `git add`, commit, push, tag,
release or Alire publish without the owner's explicit, post-review
consent; never modify a golden file, `expected_*`, without the owner's
agreement), procedure choice, reference policy, skills, Ada tooling (Serena MCP first). This file
keeps only what is specific to smk. Before any publish, the crate must
build and its tests pass on every platform declared available in
`alire.toml` (cf. `docs/dev/release_procedure.md`, section 1).

## Build pointers

The shared build pointers apply ([reference repository on GitHub](https://github.com/LionelDraghi/ada-project-template/blob/main/AGENTS.md)); `make help` is the documentation of record. smk specifics:

- `make check` runs the bbt scenario suites then the unit tests; the individual run targets (`sanity`, `features`, `tutorial`, `unit`, `benchmark`) live in `tests/`, which has its own `make help`.
- `make cmd_line.md` regenerates `docs/cmd_line.md`; `make doc` regenerates `cmd_line.md` and the fixme index.
- Deviation on `install`: `make install` builds in release mode first, then copies smk to `~/bin`.
- smk has no tools sub-project.
- Run `make` with `LD_LIBRARY_PATH` unset (`env -u LD_LIBRARY_PATH make`)
  if your environment pollutes it, e.g. from a VSCode extension.
- The GitHub Actions workflows (`.github/workflows/`) build and test
  smk on Linux on each push, and upload an AppImage to the rolling
  `latest` GitHub release. The tests results and badges are not
  published by the CI: they are part of the repository state.
- Run `make` with `LD_LIBRARY_PATH` unset (`env -u LD_LIBRARY_PATH make`)
  if your environment pollutes it, e.g. from a VSCode extension.

## Conditional readings

Read a document only when the task requires it; do not open them all up front.

- To choose the next task or find already-tracked work:
  `docs/dev/fixme_index.md`, `docs/limitations.md`,
  `docs/changelog.md` (current -dev version)
- To review or challenge the design: `docs/dev/design_discussions.md`,
  `docs/dev/design_notes.md`
- To follow the developer workflow: `docs/dev/development_workflow.md`,
  `docs/dev/developer_guide.md`
- To prepare or run a release: the [common release procedure on GitHub](https://github.com/LionelDraghi/ada-project-template/blob/main/release_procedure.md), with the smk parameters and additions in `docs/dev/release_procedure.md`
- To write, convert or debug bbt scenarios: the bbt skill, installed from GitHub (`npx skills add LionelDraghi/bbt --skill bbt-skill`)
- To understand the command line and features: `docs/cmd_line.md`,
  `docs/tutorial.md`, `docs/features/`

smk is tested mostly with bbt, on Debian x86_64.
