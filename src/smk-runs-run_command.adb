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

with File_Utilities;
with Smk.Assertions;
with Smk.IO;

with GNAT.OS_Lib;

with Ada.Calendar;
with Ada.Directories;

separate (Smk.Runs)

-- -----------------------------------------------------------------------------
procedure Run_Command (E            : in out Smkfiles.Smkfile_Entry;
                       The_Run_List : in out Runfiles.Run_Lists.Map;
                       Cmd_To_Run   :    out Boolean;
                       Error_In_Run :    out Boolean)
is
   -- --------------------------------------------------------------------------
   procedure Run (Cmd   : in     Command_Lines;
                  OK    :    out Boolean) is
      -- Spawn the Cmd under strace.
      -- OK is set to True if the spawn did it well.

      use GNAT.OS_Lib;
      Debug       : constant Boolean := False;
      Prefix      : constant String  := "";
      Opt         : constant String  := Settings.Shell_Opt
                      & File_Utilities.Escape (Settings.Strace_Cmd
                                               & Settings.Strace_Outfile_Name
                                               & " " & (+Cmd));
      Initial_Dir : constant String  := Current_Directory;
      Spawn_Arg   : constant Argument_List_Access
        := Argument_String_To_List (Opt);

   begin
      -- IO.Put_Line ("cd " & Settings.Run_Dir_Name, Level => Verbose);
      Set_Directory (Settings.Run_Dir_Name);

      IO.Put_Debug_Line
        (Msg    => "Spawn " & Settings.Shell_Cmd & " " & (Opt) & "...",
         Debug  => Debug,
         Prefix => Prefix);
      for A of Spawn_Arg.all loop
         IO.Put_Debug_Line (">" & A.all & "<", Debug, Prefix);
      end loop;

      IO.Put_Line ((+Cmd));
      Spawn (Program_Name => Settings.Shell_Cmd,
             Args         => Spawn_Arg.all,
             Success      => OK);
      if not OK then
         IO.Put_Error (Msg => "Spawn failed for " & (+Cmd));
      end if;

      Set_Directory (Initial_Dir);
      -- Fixme : ensure turning to Initial_Dir even in case of exception?

   end Run;

   use Smk.Runfiles;
   OK           : Boolean;
   New_Run_Time : Ada.Calendar.Time;
   Assertions   : Condition_Lists.List;

begin
   -- --------------------------------------------------------------------------
   if Must_Be_Run (E.Command, The_Run_List) then

      Cmd_To_Run := True;

      if Settings.Dry_Run then
         -- don't run, just print the command
         IO.Put_Line ("> " & (+E.Command));
         E.Was_Run    := True;
         Error_In_Run := False;

      else
         -- 1. Run the command
         New_Run_Time := Ada.Calendar.Clock;

         Run (E.Command, OK);

         E.Was_Run    := OK;
         Error_In_Run := not OK;

         if not OK and not Settings.Keep_Going then
            return;
         end if;

         if OK then
            -- 2. Analyze the run log
            Analyze_Run (Assertions);

            -- 3. Store the results
            Insert_Or_Update
              (The_Command => E.Command,
               The_Run     => (Section    => E.Section,
                               Run_Time   => New_Run_Time,
                               Assertions => Assertions),
               In_Run_List => The_Run_List);
         end if;

      end if;

   else
      E.Was_Run  := False;
      Cmd_To_Run := False;

      IO.Put_Line ("No need to run " & (+E.Command),
                   Level => IO.Verbose);
      Error_In_Run := False;

   end if;

end Run_Command;
