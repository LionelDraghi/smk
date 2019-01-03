-- -----------------------------------------------------------------------------
-- smk, the smart make (http://lionel.draghi.free.fr/smk/)
-- © 2018, 2019 Lionel Draghi <lionel.draghi@free.fr>
-- SPDX-License-Identifier: APSL-2.0
-- -----------------------------------------------------------------------------
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- http://www.apache.org/licenses/LICENSE-2.0
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- -----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

separate (Smk.Main)

-- -----------------------------------------------------------------------------
procedure Put_Help is
begin
   New_Line;
   Put_Line ("Usage : smk [Options]* Command [Smkfile][:target]");
   New_Line;
   Put_Line ("Use example :");
   Put_Line ("   when run the first time   : smk MyBuild.txt");
   Put_Line ("   and then,to rebuild, just : smk");
   Put_Line ("   to run a specific target  : smk MyBuild.txt:target");
   Put_Line ("   or just                   : smk :target");
   New_Line;
   Put_Line ("Commands :");
   Put_Line ("   build             : run the build");
   Put_Line ("   status       | st : shows what smk knows about the previous");
   Put_Line ("                       runs (commands, sources and targets)");
   Put_Line ("   read-smkfile | rs : shows Smk understanding of a Smkfile");
   Put_Line ("   whatsnew     | wn : list changes since last run");
   Put_Line ("   add               : add the rest of the command line to");
   Put_Line ("                       default.smk");
   Put_Line ("   run               : equivalent to `add` followed by `build`");
   Put_Line ("   clean             : remove all targets files");
   Put_Line ("   reset             : remove all local Smk files");
   Put_Line ("                       (equivalent to rm .smk.*)");
   Put_Line ("   version           : put Smk version");
   Put_Line ("   help              : this message");
   Put_Line ("   dump              : list all smk known info on files,");
   Put_Line ("                       including unused and dir");
   Put_Line ("   list-runs    | lr : list runfiles in current directory");
   Put_Line ("   list-sources | ls : list sources, except system files");
   Put_Line ("   list-targets | lt : list targets, except system files");
   Put_Line ("   list-unused  | lu : list files not involved in build");
   New_Line;
   Put_Line ("   NB : when no command is given, build is assumed");
   New_Line;
   Put_Line ("Options :");
   Put_Line ("   -a   | --always-make    : unconditionally make all targets");
   Put_Line ("   -e   | --explain        : explain why each target is made");
   Put_Line ("   -n   | --dry-run        : print the commands that would be");
   Put_Line ("                             executed, but do not execute them");
   Put_Line ("   -sa  | --show-all-files : prevent -ls and -rl from");
   Put_Line ("                             ignoring system files");
   Put_Line ("   -i   | --ignore-errors  : ignore all errors in commands");
   Put_Line ("                             executed to remake files");
   Put_Line ("   -l   | --long-listing   : use a long listing format when");
   Put_Line ("                             listing files");
   Put_Line ("   -k   | --keep-going     : Do as much work as possible");
   Put_Line ("   -We  | --Warnings=error : treat warnings as errors");
   Put_Line ("   -v   | --verbose");
   Put_Line ("   -q   | --quiet          : no message unless error,");
   Put_Line ("                             Warning are also ignored");
   Put_Line ("   -h   | --help           : this message");
   New_Line;
   Put_Line ("http://lionel.draghi.free.fr/smk/");
   New_Line;
end Put_Help;
