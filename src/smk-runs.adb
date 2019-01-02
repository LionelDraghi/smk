-- -----------------------------------------------------------------------------
-- smk, the smart make (http://lionel.draghi.free.fr/smk/)
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

with Smk.Definitions;      use Smk.Definitions;
with Smk.Files;            use Smk.Files;
with Smk.Files.File_Lists;
with Smk.Settings;         -- use Smk.Settings;

with Ada.Directories;      use Ada.Directories;
with Ada.Text_IO;          use Ada.Text_IO;

package body Smk.Runs is

   -- --------------------------------------------------------------------------
   procedure Analyze_Run
     (Sources_And_Targets : out Files.File_Lists.Map;
      Dirs                : out Files.File_Lists.Map;
      Counts              : out Runfiles.File_Counts) is separate;
   -- Based on the run log file (that is the strace output), and the run time,
   -- it identifies Source and Target files.
   -- Thanks to strace -y option, file names appears clearly between <>
   -- in the strace output.
   -- This output is filtered to keep only those file names, and pushed to
   -- Source_Files list output parameter if the file's time tag is older than
   -- the execution time, and to Target_Files list otherwise.

   -- --------------------------------------------------------------------------
   function Must_Be_Run (Command      : Command_Lines;
                         Previous_Run : in out Runfiles.Run_Lists.Map)
                         return Boolean is separate;
   -- This function return True if one of the following condition is met:
   --    1. the --always-make option is set;
   --    2. the provided Command is not found in the previous run;
   --    3. one the files identified as Target during the previous run
   --       is missing;
   --    4. one the files identified as Source during the previous run
   --       has been updated after the previous run.

   -- --------------------------------------------------------------------------
   procedure Run_Command (E            : in out Smkfiles.Smkfile_Entry;
                          The_Run_List : in out Runfiles.Run_Lists.Map;
                          Cmd_To_Run   :    out Boolean;
                          Error_In_Run :    out Boolean)
   is separate;
   -- Run_Command is in charge of spawning the Cmd (using strace),
   -- and analysing the strace log file.
   -- The_Run_List is updated with this run results


   -- --------------------------------------------------------------------------
   procedure Run_All (The_Smkfile   : in out Smkfiles.Smkfile;
                      The_Run_List  : in out Runfiles.Run_Lists.Map;
                      Cmd_To_Run    :    out Boolean;
                      Error_In_Run  :    out Boolean;
                      Section_Found :    out Boolean;
                      Target_Found  :    out Boolean)
   is
      -- Those booleans cumulate the result over the loop,
      -- that will be put in the matching output parameters.
      Some_Command_To_Run  : Boolean := False;
      Some_Error_In_Run    : Boolean := False;

   begin
      Cmd_To_Run    := False;
      Error_In_Run  := False;
      Section_Found := False;
      Target_Found  := False;

      for E of The_Smkfile.Entries loop

         if Settings.Section_Name = "" or else
           Settings.Section_Name = +E.Section
         then
            Section_Found := True;
            Target_Found  := Runfiles.Has_Target (The_Run_List,
                                                  Settings.Target_Name);

            if Settings.Target_Name = "" or else Target_Found then

               if not E.Was_Run then
                  Run_Command (E,
                               The_Run_List,
                               Some_Command_To_Run,
                               Some_Error_In_Run);

                  -- sum the results:
                  Cmd_To_Run   := Cmd_To_Run   or Some_Command_To_Run;
                  Error_In_Run := Error_In_Run or Some_Error_In_Run;

                  if Some_Error_In_Run and not Settings.Keep_Going then
                     exit;
                  end if;

               end if;

            end if;

         end if;

      end loop;

   end Run_All;

end Smk.Runs;
