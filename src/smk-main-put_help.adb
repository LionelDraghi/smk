-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

separate (Smk.Main)

-- -----------------------------------------------------------------------------
procedure Put_Help is
begin
   New_Line;
   Put_Line ("Usage: smk [Options]* Command [Smkfile][:target]");
   New_Line;
   Put_Line ("Usage examples:");
   Put_Line ("   on first use              : smk MyBuild.txt");
   Put_Line ("   then, to rebuild, just    : smk");
   Put_Line ("   to run a specific target  : smk MyBuild.txt:target");
   Put_Line ("   or just                   : smk :target");
   New_Line;
   Put_Line ("Commands:");
   Put_Line ("   build             : run the build");
   Put_Line ("   status       | st : show what smk knows about the previous");
   Put_Line ("                       runs (commands, sources and targets)");
   Put_Line ("   read-smkfile | rs : show smk's understanding of a Smkfile");
   Put_Line ("   whatsnew     | wn : list the changes since the last run");
   Put_Line ("   add               : add the rest of the command line to");
   Put_Line ("                       default.smk");
   Put_Line ("   run               : equivalent to `add` followed by `build`");
   Put_Line ("   clean             : remove all target files");
   Put_Line ("   reset             : remove all local Smk files");
   Put_Line ("                       (equivalent to rm .smk.*)");
   Put_Line ("   version           : print the smk version");
   Put_Line ("   help              : this message");
   Put_Line ("   dump              : list all the info smk knows about");
   Put_Line ("                       the files, including unused and dirs");
   Put_Line ("   list-runs    | lr : list the runfiles in the current");
   Put_Line ("                       directory");
   Put_Line ("   list-sources | ls : list the sources, except system files");
   Put_Line ("   list-targets | lt : list the targets, except system files");
   Put_Line ("   list-unused  | lu : list the files not involved in the build");
   New_Line;
   Put_Line ("   NB: when no command is given, build is assumed");
   New_Line;
   Put_Line ("Options:");
   Put_Line ("   -mt  | --missing-targets : build if missing targets");
   Put_Line ("                              (default is to build only if");
   Put_Line ("                               sources are updated)");
   Put_Line ("   -a   | --always-make     : unconditionally make all targets");
   Put_Line ("   -e   | --explain         : explain why each target is made");
   Put_Line ("   -n   | --dry-run         : print the commands that would be");
   Put_Line ("                              executed, but do not execute them");
   Put_Line ("   -sa  | --show-all-files  : also show system files");
   Put_Line ("   -ds  | --dont-shorten    : print files with their full path");
   Put_Line ("   -i   | --ignore-errors   : ignore all errors in commands");
   Put_Line ("                              executed to remake files");
   Put_Line ("   -l   | --long-listing    : use a long listing format when");
   Put_Line ("                              listing files");
   Put_Line ("   -k   | --keep-going      : do as much work as possible");
   Put_Line ("   -We  | --Warnings=error  : treat warnings as errors");
   Put_Line ("   -v   | --verbose");
   Put_Line ("   -q   | --quiet           : no messages unless error,");
   Put_Line ("                              warnings are also ignored");
   Put_Line ("   -h   | --help            : this message");
   New_Line;
   Put_Line ("https://github.com/LionelDraghi/smk");
   New_Line;
end Put_Help;
