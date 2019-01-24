Fixme in current version:
-------------------------

Location | Text
---------|-----
docs/README.md:200|  - [Fixme](fixme.md)
docs/compare_with_make.md:21|Fixme: to be done
docs/contributing.md:39|Fixme: **TBC**
docs/index.md:200|  - [Fixme](fixme.md)
docs/smkfile_format.md:81|Sections are labels in the file that specificaly designate the following commands (until next section). It allows for exemple to run only the `clean` section of a Makefile. **Fixme: Not yet implemented, as of v0.0.4**  
docs/smkfile_format.md:158|   **Fixme: Not yet implemented, as of v0.0.4**
src/file_utilities.ads:69|   -- Fixme: this function is not portable!
src/smk-files.ads:99|   -- if False: Fixme:
src/smk-files.ads:100|   -- if True: Fixme:
src/smk-runfiles.adb:234|              and then not Settings.In_Ignore_List (+Name) -- Fixme: useful??
src/smk-runfiles.ads:87|   -- [section]Command:file name --Fixme: to be updated
src/smk-runs-analyze_run.adb:105|               -- Fixme: pas un trigger ou une condition, à réorganiser
src/smk-runs-analyze_run.adb:152|--              else -- Fixme: partial code duplication
src/smk-runs-analyze_run.adb:182|--     -- Fixme: no dir in counts   Runfiles.Update_Counts (Dirs, Counts);
src/smk-runs-must_be_run.adb:46|      -- Fixme: not sure this call is needed, why not use the Status?
src/smk-runs-strace_analyzer.adb:39|   -- Fixme: processing of unfinished line not done
src/smk-runs-strace_analyzer.adb:267|      -- Fixme: if to be ordered according to occurence frequence
src/smk-settings.adb:40|                                              -- Fixme: can't ignore /tmp/* ???
tests/01_sanity_tests/Makefile:103|	# Fixme: sleep needed because of close consecutive smk run that disrupt the algorithm
tests/01_sanity_tests/Makefile:124|	# Fixme: sleep needed because of close consecutive smk run that disrupt the algorithm
tests/02_website_sanity_tests/Makefile:165|	sleep 1.0 # Fixme: if "touched" just after being build, the change is not detected
tests/05_target_tests/Makefile:112|	sleep 1 # Fixme:
tests/06_implicit_naming_tests/Makefile:64|	# Fixme: sleep needed because of close consecutive smk run that disrupt the algorithm
tests/10_section_tests/Makefile:54|	sleep 1.0 # Fixme: touch to close to rebuild, not detected othewise
tests/12_mp3_conversions_tests/Makefile:123|	sleep 1.0 # Fixme:
