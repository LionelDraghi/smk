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

with Ada.Directories;
with GNAT.OS_Lib;
with Smk.IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;

separate (Smk.Main)

-- -----------------------------------------------------------------------------
procedure Run_All_Commands (The_Smkfile    : in out Smkfiles.Smkfile;
                            The_Run_List   : in out Runfiles.Run_Lists.Map;
                            No_Command_Run :    out Boolean) is
   Cmd_Run_During_Outer : Boolean := False;

begin
   No_Command_Run := True;

   Outer : for I in 1 .. The_Smkfile.Entries.Length loop
      -- This double loop is a pragmatic way to avoid a more complex
      -- dependies analysis.
      --
      -- Why: build command may appears not ordered in the Makefile.
      -- What should we do if a command invalidate another one that was
      -- before in The_Smkfile.Entries?
      -- Exemple :
      -- 1. gcc -o hello hello.o main.o
      -- 2. gcc -o hello.o -c hello.c
      -- 3. gcc -o main.o -c main.c
      -- If main.c is changed, a single loop approach will
      -- re-run 3. andexit.
      -- The double loop will re-loop, and run 1.
      -- If during the inner loop, nothing is run, then OK,
      -- we exit the outer loop.
      --
      -- The the worst order will cause as many loop as
      -- The_Smkfile.Entries. Otherwise, it means that there is
      -- a circular dependency.
      -- To avoid infinite recursion in that case, the outer loop is
      -- limited to the worst case, that is The_Smkfile.Entries.Length.

      Cmd_Run_During_Outer := False;

      Inner : for E of The_Smkfile.Entries loop

         -- declare
         --    use type Runfiles.Command_Lines;
         -- begin
         --    IO.Put_Line (Positive'Image (Positive (I)) & " "
         --              & (+E.Command)
         --              & " Was_Run = "
         --              & Boolean'Image (E.Was_Run)
         --              & ", No_Cmd_Run = "
         --                 & Boolean'Image (No_Command_Run));
         -- end;

         if not E.Was_Run then
            Run_Command (E, The_Run_List);

            if E.Was_Run then
               No_Command_Run := False;
               Cmd_Run_During_Outer := True;
            end if;

            if IO.Some_Error and not Ignore_Errors then
               exit Outer;
            end if;

            -- Cmd_Run_During_Outer := E.Was_Run;

         end if;
      end loop Inner;

      -- IO.Put_Line ("");

      -- Naive loop aproach : each time a cmd is run, and potentialy
      -- invalidate another cmd, we restart the whole cmd list, until
      -- no command is re-run.
      exit Outer when not Cmd_Run_During_Outer;

   end loop Outer;

end Run_All_Commands;
