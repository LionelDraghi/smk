# Instructions for coding agents

This file is the entry point for coding agents working on smk.

## Non-negotiable rules

- Never stage, commit or push without the owner's explicit approval:
  `git add` is as forbidden as `git commit` and `git push`.
- Never tag, create a release or publish to Alire without the owner's
  explicit approval. Before any publish, the crate must build and its
  tests must pass on every platform declared available in alire.toml
  (cf. `docs/dev/release_procedure.md`, section 1).
- The go-ahead is a separate, explicit approval given by the owner AFTER
  reviewing the diffs; it may come in a later message. A request such as
  "on pousse sur GitHub" in the task is the goal, not the go-ahead.
  When in doubt, wait.
- Never modify a golden file (`expected_*`) without the owner's
  agreement.
- If a build, test, documentation or cleanup step fails, stop and
  report the failure.
- Do not include unrelated changes, temporary files or test artifacts.
- Check repository cleanliness on the file system, not only with
  `git status`: git ignored files remain invisible.

## Choose the right procedure

- Development work, bug fixes, doc updates, tests, cleanup, review:
  follow `docs/dev/development_workflow.md`.
- Release versioning, tagging, GitHub release, Alire publication:
  follow `docs/dev/release_procedure.md`.
- Committing and pushing after the owner's go-ahead: same document,
  section 1.

## Build pointers

- `make build` builds smk in validation mode, `make release` in
  release mode, `make install` also copies the binary to `~/bin`.
- `make check` runs the bbt scenario suites and the unit tests.
- `make doc` regenerates the generated docs (cmd_line.md, tests badge,
  fixme index).
- `make clean` removes the build and test artifacts.
- The GitHub Actions workflows (`.github/workflows/`) build and test
  smk on Linux on each push, and upload an AppImage to the rolling
  `latest` GitHub release. The tests results and badges are not
  published by the CI: they are part of the repository state.
- Run `make` with `LD_LIBRARY_PATH` unset (`env -u LD_LIBRARY_PATH make`)
  if your environment pollutes it, e.g. from a VSCode extension.

## Task triage

When the user asks "what should we do now?" or "what is the priority
right now?", check the tracked work before proposing new work:

- `docs/dev/fixme_index.md` for actionable fixes and cleanup items;
- `docs/limitations.md` for known limitations and bugs;
- `docs/dev/design_discussions.md` for design subjects under
  discussion or arbitrated, that constrain the next step;
- `docs/changelog.md` for the current -dev version and recent changes.

Use these sources to ground the answer in the repository's existing
work. Do not invent a new task before checking whether the need is
already tracked elsewhere.

## Useful references

- `docs/dev/development_workflow.md`
- `docs/dev/release_procedure.md`
- `docs/dev/design_notes.md`
- `docs/dev/design_discussions.md`
- `docs/dev/developer_guide.md`
- `docs/dev/fixme_index.md`
- `docs/cmd_line.md`, `docs/tutorial.md`, `docs/Features/`

smk is tested mostly with bbt, on Debian x86_64.
