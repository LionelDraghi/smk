-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

with Smk.Assertions;  use Smk.Assertions;
with Smk.Definitions; use Smk.Definitions;
with Smk.Files;       use Smk.Files;
with Smk.Settings;
with Smk.Runs.Observers;

with Ada.Directories; use Ada.Directories;

package body Smk.Runs is

   -- --------------------------------------------------------------------------
   procedure Analyze_Run (Events     : in     Observers.Operation_Lists.List;
                          Assertions :    out Condition_Lists.List) is separate;
   -- Based on the file operations observed during the run, it identifies
   -- the Source and Target files of the command.

   -- --------------------------------------------------------------------------
   function Must_Be_Run (Command      : Command_Lines;
                         Previous_Run : in out Runfiles.Run_Lists.Map)
                         return Boolean is separate;
   -- This function returns True if one of the following conditions is met:
   --    1. the --always-make option is set;
   --    2. the provided Command is not found in the previous run;
   --    3. one of the files identified as Target during the previous run
   --       is missing;
   --    4. one of the files identified as Source during the previous run
   --       has been updated after the previous run.

   -- --------------------------------------------------------------------------
   procedure Run_Command (E            : in out Smkfiles.Smkfile_Entry;
                          The_Run_List : in out Runfiles.Run_Lists.Map;
                          Cmd_To_Run   :    out Boolean;
                          Error_In_Run :    out Boolean)
   is separate;
   -- Run_Command is in charge of running the Cmd, observing its file
   -- accesses, and analyzing the observed operations.
   -- The_Run_List is updated with the results of this run

   -- --------------------------------------------------------------------------
   procedure Run_All (The_Smkfile   : in out Smkfiles.Smkfile;
                      The_Run_List  : in out Runfiles.Run_Lists.Map;
                      Cmd_To_Run    :    out Boolean;
                      Error_In_Run  :    out Boolean;
                      Section_Found :    out Boolean;
                      Target_Found  :    out Boolean)
   is
      -- Those booleans cumulate the results over the loop,
      -- and are then put in the matching output parameters.
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
