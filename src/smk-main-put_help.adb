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
   Put_Line ("Usage : smk [Options] make_file");
   New_Line;
   Put_Line ("Options :");
   Put_Line ("   -a  | --always-make    : unconditionally make all targets");
   Put_Line ("   -e  | --explain        : explain why each target is made");
   Put_Line ("   -n  | --dry-run        : print the commands that would"
             & " be executed, but do not execute them");
   Put_Line ("   -i  | --ignore-errors  : ignore all errors in commands"
             & " executed to remake files");
   New_Line;
   Put_Line ("   -lm | --list_makefile  : dump Smk understanding of the"
             & " Makefile");
   New_Line;
   Put_Line ("   -ls | --list_saved_run : dump what Smk stored of the previous"
             & " run of this Makefile");
   New_Line;
   Put_Line ("         --clean          : remove all local Smk files"
             & " (equivalent to rm .smk.*)");
   New_Line;
   Put_Line ("   -We | --Warnings=error : treat warnings as errors");
   Put_Line ("   -v  | --verbose");
   Put_Line ("   -q  | --quiet          : no message unless error.");
   Put_Line ("                            Warning are also ignored.");
   Put_Line ("         --version        : Smk version");
   Put_Line ("   -h  | --help           : this message");
   New_Line;
   Put_Line ("https://github.com/LionelDraghi/smk");
   New_Line;
end Put_Help;
