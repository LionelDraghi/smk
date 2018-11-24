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
procedure Run_Command (E            : in out Makefiles.Makefile_Entry;
                       The_Run_List : in out Run_Files.Run_Lists.Map)
is

   -- --------------------------------------------------------------------------
   procedure Run (Cmd   : in     Run_Files.Command_Lines;
                  OK    :    out Boolean) is
      -- Spawn the Cmd under strace.
      -- OK is set to True if the spawn did it well.

      use type Run_Files.Command_Lines;
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

   use Smk.Run_Files;
   OK            : Boolean;
   Source_Files,
   Target_Files  : File_Lists.Map;
   New_Run_Time  : Ada.Calendar.Time;

begin
   if Must_Be_Run (E.Command, The_Run_List) then

      if Dry_Run then
         -- don't run, just print the command
         IO.Put_Line ("> " & (+E.Command));
         E.Already_Run := True;

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
         E.Already_Run := True;

         if not OK and not Ignore_Errors then
            return;
         end if;

         -- 2. Analyze the run log
         Analyze_Run (Source_Files, Target_Files);

         -- 3. Store the results
         Insert_Or_Update (The_Command => E.Command,
                           The_Run     => (Section  => E.Section,
                                           Run_Time => New_Run_Time,
                                           Sources  => Source_Files,
                                           Targets  => Target_Files),
                           In_Run_List => The_Run_List);
      end if;

   else
      E.Already_Run := False;
      IO.Put_Line ("No need to run " & (+E.Command),
                   Level => IO.Verbose);

   end if;
end Run_Command;
