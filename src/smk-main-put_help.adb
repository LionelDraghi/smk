-- -----------------------------------------------------------------------------
-- smk, the smart make
-- © 2018 Lionel Draghi <lionel.draghi@free.fr>
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
   Put_Line ("Usage : smk Query [Options]* [Smkfile]");
   New_Line;
   Put_Line ("Usual use example:");
   Put_Line ("   when run the first time:");
   Put_Line ("   > smk MyBuild.txt");
   Put_Line ("   and then just:");
   Put_Line ("   > smk");
   Put_Line ("   to rebuild");
   New_Line;
   Put_Line ("Queries :");
   Put_Line ("   -rs  | --read-smkfile  : shows Smk understanding of a"
             & " Smkfile");
   Put_Line ("   -rl  | --read-last-run : shows what smk knows about the"
             & " previous runs");
   Put_Line ("                               (commands, sources and targets)");
   Put_Line ("   -lr  | --list-runs     : list runfiles in current"
             & " directory");
   Put_Line ("   -ls  | --list-sources  : list sources,"
             & " except system files");
   Put_Line ("   -lt  | --list-targets");
   Put_Line ("          --clean         : remove all targets files");
   Put_Line ("          --reset         : remove all local Smk files"
             & " (equivalent to rm .smk.*)");
   Put_Line ("          --version       : Smk version");
   Put_Line ("   -h   | --help          : this message");
   Put_Line ("   -b   | --build         : run the build");
   Put_Line ("   NB : when no query is given, --build is assumed");
   New_Line;
   Put_Line ("Options :");
   Put_Line ("   -a   | --always-make     : unconditionally make all targets");
   Put_Line ("   -e   | --explain         : explain why each target is made");
   Put_Line ("   -n   | --dry-run         : print the commands that would"
             & " be executed, but do not execute them");
   Put_Line ("   -sa  | --shows-all-files : prevent -ls and -rl"
             & " from ignoring system files");
   Put_Line ("   -i   | --ignore-errors   : ignore all errors in commands"
             & " executed to remake files");
   Put_Line ("   -k   | --keep-going      : Do as much work as possible");
   Put_Line ("   -We  | --Warnings=error  : treat warnings as errors");
   Put_Line ("   -v   | --verbose");
   Put_Line ("   -q   | --quiet           : no message unless error,");
   Put_Line ("                              Warning are also ignored");
   New_Line;
   Put_Line ("http://lionel.draghi.free.fr/smk/");
   New_Line;
end Put_Help;
