# Release procedure

This document is the single reference for:

- selecting a release version;
- preparing and validating a release;
- committing the release state;
- creating the Git tag and GitHub release;
- publishing the release to Alire;
- returning the repository to development mode.

Do not commit, push, tag, create a GitHub release or publish to Alire
without the owner's explicit approval.

## 1. Release prerequisites

A release publishes what the current `-dev` section of
`docs/changelog.md` describes.

Before starting the release procedure, verify that:

- the intended development work is complete;
- the current `-dev` changelog section is complete and accurate;
- all development changes have been reviewed, committed and pushed;
- the working tree is clean, on the file system and not only through
  `git status`;
- the current branch is the intended release branch;
- a complete local `make` (build, check, doc) has succeeded;
- the latest GitHub Actions checks (`.github/workflows/build-test.yml`)
  have succeeded on Linux, the only platform where smk is declared
  available.

### Platform availability, and the Alire index tests

`alr publish` exposes the crate to the community index tests: Alire
builds the crate, and runs its test action (the `[[actions]]` with
`type = "test"` in alire.toml, that is `make check`), on **every
platform where the crate is declared available**.

- A publish must not be attempted until the crate **builds and its
  tests pass** on every platform declared available in the `[available]`
  section of alire.toml. A crate published while a platform build or
  test fails is published broken: this is how acc was once published
  with its three platform compilations failing.
- smk is currently declared available on Linux only (the
  `[available]` case in alire.toml), consistent with its strace
  dependency: the local full `make` on Linux, and the Linux GitHub
  Actions checks, are the platform reference.
- Never extend the `[available]` restriction to another platform
  before the build and the tests have been validated on it (through
  CI or manual runs); keep alire.toml and this section in sync.
- Note that the test action invokes `make check`, which requires the
  external tools listed in the
  [Developer guide](developer_guide.md#tools-required-to-run-the-tests):
  the community index test machines may not have them. Check that the
  test action can run there before publishing, or it will fail in the
  index.

## 2. Select the release version

Do not select the release number by simply removing the `-dev` suffix.

Review the current `-dev` section of `docs/changelog.md` and propose the
release version according to SemVer:

- an `[Added]` entry requires a MINOR version bump;
- a backward-compatible `[Changed]` entry requires a MINOR version
  bump;
- a release containing only backward-compatible bug fixes keeps the
  PATCH bump;
- any incompatible change must be assessed explicitly according to
  SemVer.

Before changing any file:

1. identify the current development version;
2. review the current changelog entries;
3. propose the release version;
4. explain the SemVer reasoning;
5. request explicit owner confirmation.

Do not change the version before it has been confirmed.

The prerelease suffix must be separated with a dash:

```text
0.4.1-dev
```

Do not use the dot-separated form: it prevents Alire from loading the
workspace.

## 3. Set the release version

After the owner confirms the version, update it in:

```text
alire.toml
```

Then run:

```sh
alr update
```

`alr update` regenerates `Crate_Version` in:

```text
src/Alire_config/smk_config.ads
```

Running `alr build` alone does not regenerate it.

Verify that the generated `Crate_Version` matches the confirmed release
version, and that `smk version` displays it (the sources must not hard
code any version: `Smk_Settings.Smk_Version` reads
`Smk_Config.Crate_Version`).

## 4. Close the release changelog section

In `docs/changelog.md`:

1. verify that the current `-dev` section accurately describes the
   release;
2. retitle that section with the confirmed release version;
3. replace the undefined date with the release date;
4. preserve the existing changelog structure and categories;
5. keep entries concise and user-oriented;
6. remove obsolete placeholders.

The release notes must reflect the actual content of this changelog
section.

## 5. Validate the release locally

Run the complete local validation:

```sh
make
```

If any part of the build, test or documentation generation fails:

1. stop;
2. do not commit, push or tag;
3. fix the issue through the normal development workflow;
4. restart release validation from a clean state.

After a successful full run, verify that `smk version` reports the
confirmed release version exactly, without the `-dev` suffix.

Also verify that the release mode builds:

```sh
make release
```

Then clean the repository and verify cleanliness on the file system,
as described in the
[development workflow](development_workflow.md#1-commit-procedure):

```sh
make clean
git status
```

## 6. Confirm and commit the release state

Before staging, present a release-preparation report containing:

- the previous development version;
- the proposed release version;
- the SemVer reasoning;
- the changelog entries included;
- the result of `alr update`;
- the version reported by `smk version`;
- the result of the local full `make`;
- the result of the cleanup;
- the files to be committed;
- the proposed commit message.

Request explicit owner approval before staging, committing or pushing.

After approval, `git add` the complete generated state, commit and push,
as described in the
[development workflow](development_workflow.md#1-commit-procedure).

## 7. Tag and publish the GitHub release

After pushing the release commit, verify that the GitHub Actions
checks of that commit have succeeded on Linux.

Do not create or push the release tag while a required check:

- is pending;
- has failed;
- has been cancelled;
- was run against a different commit: the pushed release commit must
  be exactly the locally validated one.

If a correction is required, return to the development workflow,
implement and validate the fix, then restart the release procedure
from a clean state.

The tag format is:

```text
x.y.z
```

Request explicit owner approval before creating or publishing the tag.

After approval:

```sh
git tag x.y.z
git push origin x.y.z
```

Then create the GitHub release from that tag on the
[smk releases page](https://github.com/LionelDraghi/smk/releases),
using the released changelog section as release notes.

## 8. Publish to Alire

Publish to Alire only after the GitHub release and tag are available.

Before publication, verify that:

- `alire.toml` contains the released version;
- `Crate_Version` contains the same version;
- the Git tag is available remotely;
- the GitHub release is published;
- the local repository is clean;
- the GitHub Actions checks have succeeded on the platforms where
  smk is declared available (cf. section 1);
- the crate builds and its tests pass on every platform declared
  available (cf. section 1).

The publication command is:

```sh
alr publish
```

Refer to the
[Alire project publication documentation](https://alire.ada.dev/docs/#publishing-your-projects-in-alire)
when needed.

Before running `alr publish`, present the intended action and request
explicit owner approval.

After publication, report:

- the published version;
- the Git tag used;
- the publication or pull-request reference;
- the Alire validation result, including the community index build and
  test results on the available platforms;
- any requested correction.

Do not silently alter an already published GitHub release to resolve
an Alire publication issue.

## 9. Return to development mode

After the GitHub and Alire publication steps are complete, propose the
next development version. The new version must use the dash-separated
`-dev` suffix, for example:

```text
0.4.1 -> 0.4.2-dev
```

Request explicit owner confirmation before changing it.

After confirmation:

1. update the version in `alire.toml`;
2. run `alr update`;
3. verify the regenerated `Crate_Version`;
4. open a new `[x.y.z-dev]` section in `docs/changelog.md`, with an
   undefined date;
5. run a full `make`;
6. clean and inspect the repository;
7. request approval before staging, committing and pushing.

The repository is back in development mode only when:

- the new `-dev` version has been committed and pushed;
- the working tree and file system are clean.
