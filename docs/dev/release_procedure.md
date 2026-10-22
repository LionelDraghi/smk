<!-- omit from toc -->
# Release procedure

The release procedure is common to all Lionel's Ada projects and lives
in [the reference repository on GitHub](https://github.com/LionelDraghi/ada-project-template/blob/main/release_procedure.md).
Read both documents together; on any conflict, this file wins.

This file gives the parameter values and the smk-specific additions;
all other steps are described in the common procedure.

## Parameters

- Complete local validation: `make` (build, check, doc). Release-mode
  build: `make release`.
- Version check: `smk version` (the sources must not hard code any
  version: `Smk_Settings.Smk_Version` reads
  `Smk_Config.Crate_Version`).
- Regenerated configuration: `src/Alire_config/smk_config.ads`.
- Clean command: `make clean`.
- Remote: `origin`.
- GitHub releases page:
  [LionelDraghi/smk releases](https://github.com/LionelDraghi/smk/releases).
- Required GitHub Actions checks:
  `.github/workflows/build-test.yml`, on Linux — the only platform
  where smk is declared available (consistent with its ptrace
  dependency).
- Alire platform availability: Linux only (the `[available]` case in
  `alire.toml`); the local full `make` on Linux is the platform
  reference.

## smk-specific additions

- Step 1: refresh the copyright years in the file headers
  (`scripts/update_copyright.sh`, on a clean working tree); the result
  is committed with the release.
- Step 1 (platform availability): the test action invokes `make
  check`, which requires the external tools listed in the
  [Developer guide](developer_guide.md#tools-required-to-run-the-tests);
  check that it can run on the community index machines before
  publishing.
- Step 5: after a successful full run, also verify that the release
  mode builds (`make release`), and that `smk version` reports the
  confirmed release version exactly, without the `-dev` suffix.
- Step 6: the release-preparation report includes the version
  reported by `smk version`.
