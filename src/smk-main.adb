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

-- -----------------------------------------------------------------------------
-- Procedure: Smk.Main body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
-- -----------------------------------------------------------------------------

with Smk.IO;
with Smk.Makefiles;
with Smk.Run_Files;
with Smk.Settings;   use Smk.Settings;

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Directories;

procedure Smk.Main is

   -- Debug : constant Boolean := True;

   -- --------------------------------------------------------------------------
   procedure Put_Help is separate;
   procedure Put_Error (Msg       : in String  := "";
                        With_Help : in Boolean := False) is separate;
   -- Put_Line Utilities

   -- --------------------------------------------------------------------------
   procedure Analyze_Cmd_Line is separate;
   -- Cmd line options are then available in the Settings package.

   -- --------------------------------------------------------------------------
   procedure Analyze_Run (-- Previous_Run_Time : in     Ada.Calendar.Time;
                          Source_Files      :    out Run_Files.File_Lists.Map;
                          Target_Files      :    out Run_Files.File_Lists.Map)
   is separate;
   -- Based on the run log file (that is the strace output), and the run time,
   -- it identifies Source and Target files.
   -- Thanks to strace -y option, file names appears clearly between <>
   -- in the strace output.
   -- This output is filtered to keep only those file names, and pushed to
   -- Source_Files list output parameter if the file's time tag is older than
   -- the execution time, and to Target_Files list otherwise.

   -- --------------------------------------------------------------------------
   function Must_Be_Run (Command      : Run_Files.Command_Lines;
                         Previous_Run : in out Run_Files.Run_Lists.Map)
                         return Boolean
                         is separate;
   -- This function return True if one of the following condition is met:
   --    1. the --always-make option is set;
   --    2. the provided Command is not found in the previous run;
   --    3. one the files identified as Target during the previous run
   --       is missing;
   --    4. one the files identified as Source during the previous run
   --       has been updated after the previous run.

   -- --------------------------------------------------------------------------
   procedure Run_Command (E            : in out Makefiles.Makefile_Entry;
                          The_Run_List : in out Run_Files.Run_Lists.Map)
                          -- Was_Run      :    out Boolean)
   is separate;
   -- Run_Command is in charge of spawning the Cmd (using strace),
   -- and analysing the strace log file.
   -- The_Run_List is updated with this run results

   -- --------------------------------------------------------------------------
   procedure Clean_Files is
      use Ada.Directories;
      Search : Search_Type;
      File   : Directory_Entry_Type;
   begin
      Start_Search (Search,
                    Directory => ".",
                    Pattern   => Smk_File_Prefix & "*",
                    Filter    => (Ordinary_File => True,
                                  others        => False));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, File);
         IO.Put_Line ("Deleting " & Simple_Name (File));
         Delete_File (Simple_Name (File));
      end loop;
   end Clean_Files;

   The_Makefile : Makefiles.Makefile;
   The_Run_List : Run_Files.Run_Lists.Map;

begin
   -- --------------------------------------------------------------------------
   Analyze_Cmd_Line;
   if IO.Some_Error then
      -- If some error occurs during command line analysis, stop here.
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   if Clean_Smk_Files then Clean_Files; end if;

   if Makefile_Name = "" then return; end if;
   -- Nothing to do : useful for, e.g. -h or -v options
   -- Fixme: do we want -ls or -lm option without Makefile given that just
   --        apply to.smk.* files found in the directory?

   -- 1. Current Makefile analysis
   -- ----------------------------
   Makefiles.Analyze (Makefile_Name, The_Makefile);

   if List_Makefile then
      -- -lm option
      Makefiles.Dump (The_Makefile);
      return;
   end if;

   -- 2. Load previous run
   -- --------------------
   if Run_Files.Saved_Run_Found then
      The_Run_List := Run_Files.Get_Saved_Run;
   else
      The_Run_List := Run_Files.Run_Lists.Empty_Map;
   end if;

   if List_Saved_Run then
      -- -ls Option
      Run_Files.Dump (The_Run_List, Filter_Sytem_Files => True);
      return;
   end if;

   -- 3. Run commands found in the Makefile
   -- -------------------------------------
   declare
      A_Cmd_Was_Run : Boolean := False;
      -- use Run_Files;
   begin
      Outer : for I in 1 .. The_Makefile.Entries.Length loop
         -- This double loop is a pragmatic way to avoid a more complex
         -- dependies analysis.
         --
         -- Why: build command may appears not ordered in the Makefile.
         -- What should we do if a command invalidate another one that was
         -- before in The_Makefile.Entries?
         -- Exemple :
         -- 1. gcc -o hello hello.o main.o
         -- 2. gcc -o hello.o -c hello.c
         -- 3. gcc -o main.o -c main.c
         -- If main.c is changed, a single loop approach will re-run 3. and
         -- exit.
         -- The double loop will re-loop, and run 1.
         -- If during the inner loop, nothing is run, then OK,
         -- we exit the outer loop.
         --
         -- The the worst order will cause as many loop as
         -- The_Makefile.Entries. Otherwise, it means that there is a circular
         -- dependency.
         -- To avoid infinite recursion in that case, the outer loop is
         -- limited to the worst case, that is The_Makefile.Entries.Length.

         A_Cmd_Was_Run := False;

         Inner : for E of The_Makefile.Entries loop
            -- IO.Put_Line (Positive'Image (Positive (I)) & " " & (+E.Command)
            --              & " Already_Run = "
            --              & Boolean'Image (E.Already_Run));
            if not E.Already_Run then
               Run_Command (E, The_Run_List);
               A_Cmd_Was_Run := E.Already_Run;
            end if;
         end loop Inner;

         -- IO.Put_Line ("");

         -- Naive loop aproach : each time a cmd is run, and potentialy
         -- invalidate another cmd, we restart the whole cmd list, until
         -- no command is re-run.
         exit Outer when not A_Cmd_Was_Run;

      end loop Outer;
   end;

   -- 4. Save the updated run
   -- -----------------------
   Run_Files.Save_Run (The_Run_List);

   if IO.Some_Error then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

end Smk.Main;
