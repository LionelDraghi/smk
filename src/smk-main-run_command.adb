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
procedure Run_Command (E            : in out Smkfiles.Smkfile_Entry;
                       The_Run_List : in out Runfiles.Run_Lists.Map;
                       Cmd_To_Run   :    out Boolean;
                       Error_In_Run :    out Boolean)
is

   -- --------------------------------------------------------------------------
   procedure Run (Cmd   : in     Runfiles.Command_Lines;
                  OK    :    out Boolean) is
      -- Spawn the Cmd under strace.
      -- OK is set to True if the spawn did it well.

      use type Runfiles.Command_Lines;
      use GNAT.OS_Lib;
      use Ada.Directories;
      Debug       : constant Boolean := False;
      Prefix      : constant String  := "Run";
      Opt         : constant String
        := Settings.Strace_Opt & Settings.Strace_Outfile_Name & " " & (+Cmd);
      Initial_Dir : constant String  := Current_Directory;

   begin
      -- IO.Put_Line ("cd " & Settings.Run_Dir_Name, Level => Verbose);
      Set_Directory (Settings.Run_Dir_Name);

      IO.Put_Debug_Line
        (Msg    => " Spawn " & Strace_Cmd & " " & (Opt) & "...",
         Debug  => Debug,
         Prefix => Prefix);
      IO.Put_Line ((+Cmd));
      Spawn (Program_Name => Strace_Cmd,
             Args         => Argument_String_To_List (Opt).all,
             Success      => OK);
      if not OK then
         IO.Put_Error (Msg => "Spawn failed for " & Strace_Cmd & " " & (Opt));
      end if;

      Set_Directory (Initial_Dir);
      -- Fixme : ensure turning to Initial_Dir even in case of exception?

   end Run;

   use Smk.Runfiles;
   OK            : Boolean;
   Source_Files,
   Target_Files  : File_Lists.Map;
   New_Run_Time  : Ada.Calendar.Time;
   Source_System_File_Count,
   Target_System_File_Count : Natural;

begin
   if Must_Be_Run (E.Command, The_Run_List) then

      Cmd_To_Run := True;

      if Dry_Run then
         -- don't run, just print the command
         IO.Put_Line ("> " & (+E.Command));
         E.Was_Run    := True;
         Error_In_Run := False;

      else
         -- 1. Run the command
         New_Run_Time := Ada.Calendar.Clock;

         -- New_Run_Time := Time_Of
         --    (Year     => Year (Tmp),
         --     Month    => Month (Tmp),
         --     Day      => Day (Tmp),
         --     Seconds  => Day_Duration (Float'Floor (Float (Seconds (Tmp)))));
         -- This pretty ridiculous code is here to avoid the sub_second
         -- part that is return by Calendar.Clock, but always set
         -- to 0.0 in the Time returned by the Directories.Modification_Time.
         -- This cause files created by a command to be seeing as older than
         -- this command, and prevent the evaluation of the need to re-run a
         -- command.
         -- It's better described in Analyze_Run code.
         -- =================================================================
         -- NB: this method IS CLEARLY NOT RELIABLE
         -- =================================================================

         Run (E.Command, OK);
         E.Was_Run    := OK;
         Error_In_Run := not OK;

         if not OK and not Keep_Going then
            return;
         end if;

         if OK then
            -- 2. Analyze the run log
            Analyze_Run (Source_Files, Source_System_File_Count,
                         Target_Files, Target_System_File_Count);

            -- 3. Store the results
            Insert_Or_Update
              (The_Command => E.Command,
               The_Run     =>
                 (Section                  => E.Section,
                  Run_Time                 => New_Run_Time,
                  Sources                  => Source_Files,
                  Source_System_File_Count => Source_System_File_Count,
                  Targets                  => Target_Files,
                  Target_System_File_Count => Target_System_File_Count),
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
