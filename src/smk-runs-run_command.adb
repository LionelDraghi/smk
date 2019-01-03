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

with Smk.IO;

with GNAT.OS_Lib;

with Ada.Calendar;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings;

separate (Smk.Runs)

-- -----------------------------------------------------------------------------
procedure Run_Command (E            : in out Smkfiles.Smkfile_Entry;
                       The_Run_List : in out Runfiles.Run_Lists.Map;
                       Cmd_To_Run   :    out Boolean;
                       Error_In_Run :    out Boolean)
is
   -- --------------------------------------------------------------------------
   function Escape (Text : in String) return String is
      use Ada.Strings.Maps;
      Src_Idx       : Natural := Text'First;
      To_Be_Escaped : constant Character_Set := To_Set (' '
                                                        & '"' & '#' & '$'
                                                        & '&' & ''' & '('
                                                        & ')' & '*' & ','
                                                        & ';' & '<' & '>'
                                                        & '?' & '[' & '\'
                                                        & ']' & '^' & '`'
                                                        & '{' & '|' & '}');
      -- Refer to the "Which characters need to be escaped when using Bash?"
      -- discussion on stackoverflow.com
      -- Fixme: this escaping is not portable

      Blank_Count   : constant Natural
        := Ada.Strings.Fixed.Count (Text, Set => To_Be_Escaped);
      Out_Str       : String (Text'First .. Text'Last + Blank_Count);
   begin
      -- IO.Put_Line ("Blank_Count    =" & Natural'Image (Blank_Count));
      -- IO.Put_Line ("Out_Str'length =" & Natural'Image (Out_Str'Length));
      -- IO.Put_Line ("Text'length    =" & Natural'Image (Text'Length));

      Out_Str (Text'First .. Text'Last) := Text;

      for I in 1 .. Blank_Count loop
         -- IO.Put_Line (Integer'Image (I) & ": S >" & Text    & "<");
         -- IO.Put_Line (Integer'Image (I) & ": T >" & Out_Str & "<");
         -- IO.Put_Line (Integer'Image (I) & ": Src_Idx before search ="
         --             & Natural'Image (Src_Idx));

         Src_Idx := Ada.Strings.Fixed.Index (Out_Str (Src_Idx .. Out_Str'Last),
                                             To_Be_Escaped);
         -- IO.Put_Line (Integer'Image (I) & ": Src_Idx after search ="
         --             & Natural'Image (Src_Idx));
         Ada.Strings.Fixed.Insert (Out_Str,
                                   Before   => Src_Idx,
                                   New_Item => "\",
                                   Drop     => Ada.Strings.Right);
         Src_Idx := Src_Idx + 2;
      end loop;
      return Out_Str;
   end Escape;

   -- --------------------------------------------------------------------------
   procedure Run (Cmd   : in     Command_Lines;
                  OK    :    out Boolean) is
      -- Spawn the Cmd under strace.
      -- OK is set to True if the spawn did it well.

      use GNAT.OS_Lib;
      Debug       : constant Boolean := False;
      Prefix      : constant String  := "";
      Opt         : constant String  := Settings.Shell_Opt
                      & Escape (Settings.Strace_Cmd
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
   OK                  : Boolean;
   Sources_And_Targets : Files.File_Lists.Map;
   New_Run_Time        : Ada.Calendar.Time;
   Counts              : Runfiles.File_Counts;
   Dirs                : Files.File_Lists.Map;

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
            Analyze_Run (Sources_And_Targets, Dirs, Counts);

            -- 3. Store the results
            Insert_Or_Update
              (The_Command => E.Command,
               The_Run     =>
                 (Section  => E.Section,
                  Run_Time => New_Run_Time,
                  Files    => Sources_And_Targets,
                  Dirs     => Dirs,
                  Counts   => Counts),
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
