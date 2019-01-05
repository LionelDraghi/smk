Fixme in current version:
-------------------------

Location | Text
---------|-----
docs/README.md:199|  - [Fixme](fixme.md)
docs/compare_with_make.md:21|Fixme: to be done
docs/contributing.md:39|Fixme: **TBC**
docs/index.md:199|  - [Fixme](fixme.md)
docs/smkfile_format.md:81|Sections are labels in the file that specificaly designate the following commands (until next section). It allows for exemple to run only the `clean` section of a Makefile. **Fixme: Not yet implemented, as of v0.0.4**  
docs/smkfile_format.md:158|   **Fixme: Not yet implemented, as of v0.0.4**
src/smk-runfiles.adb:222|            -- Fixme: precond : Is_Dir = False
src/smk-runfiles.ads:39|   -- Fixme: File_Counts should be encapsulated within File_Lists
src/smk-runfiles.ads:109|   -- [section]Command:file name --Fixme: to be updated
src/smk-runs-analyze_run.adb:168|            else -- Fixme: partial code duplication
src/smk-runs-analyze_run.adb:198|   -- Fixme: no dir in counts   Runfiles.Update_Counts (Dirs, Counts);
src/smk-runs-run_command.adb:49|      -- Fixme: this escaping is not portable
src/smk-runs-strace_analyzer.adb:81|   -- Fixme: to increase performances, those List should be ordered with
src/smk-runs-strace_analyzer.adb:194|         -- Fixme: need cwd management
src/smk-settings.adb:40|                                              -- Fixme: can't ignore /tmp/* ???
tests/01_sanity_tests/Makefile:103|	# Fixme: sleep needed because of close consecutive smk run that disrupt the algorithm
tests/01_sanity_tests/Makefile:124|	# Fixme: sleep needed because of close consecutive smk run that disrupt the algorithm
tests/02_website_sanity_tests/Makefile:177|	sleep 1.0 # Fixme: if "touched" just after being build, the change is not detected
tests/05_target_tests/Makefile:112|	sleep 1 # Fixme:
tests/06_implicit_naming_tests/Makefile:64|	# Fixme: sleep needed because of close consecutive smk run that disrupt the algorithm
tests/10_section_tests/Makefile:54|	sleep 1.0 # Fixme: touch to close to rebuild, not detected othewise
