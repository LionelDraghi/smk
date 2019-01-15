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

with Smk.Assertions;
with Smk.Definitions;      use Smk.Definitions;
with Smk.IO;
with Smk.Files;
with Smk.Files.File_Lists;
with Smk.Smkfiles;
with Smk.Runfiles;
with Smk.Settings;         use Smk.Settings;

with Ada.Command_Line;
with Ada.Directories;
with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

procedure Smk.Main is

   -- --------------------------------------------------------------------------
   -- Put_Line Utilities:
   procedure Put_Help is separate;
   procedure Put_Error (Msg       : in String  := "";
                        With_Help : in Boolean := False) is separate;

   -- --------------------------------------------------------------------------
   procedure Analyze_Cmd_Line is separate;
   -- Cmd line options are then available in the Settings package.

   -- --------------------------------------------------------------------------
   procedure Build is separate;

begin
   -- --------------------------------------------------------------------------
   Analyze_Cmd_Line;

   if IO.Some_Error then
      -- If some error occurs during command line analysis, stop here,
      -- even if Ignore_Errors or Keep_Going is set
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   case Current_Command is
      when Read_Smkfile =>
         Smkfiles.Dump;

      when Status =>
         declare
            The_Runfile : Runfiles.Runfile;
            use Files;
         begin
            if Runfiles.Runfiles_Found then
               The_Runfile := Runfiles.Get_Saved_Run
                 (+To_Runfile_Name (Smkfile_Name));
               Runfiles.Put_Run (The_Runfile.Run_List);
            else
               Put_Error ("No previous run found.");
            end if;
         end;

      when List_Previous_Runs =>
         Runfiles.Put_Run_List;

      when List_Targets =>
         Runfiles.Put_Files (Runfiles.Load_Runfile,
                             Print_Targets => True);

      when List_Sources =>
         Runfiles.Put_Files (Runfiles.Load_Runfile,
                             Print_Sources => True);

      when List_Unused =>
         Runfiles.Put_Files (Runfiles.Load_Runfile,
                             Print_Unused => True);

      when Whatsnew =>
         declare
            use Runfiles;
            The_Runfile  : Runfile;
            Updated_List : Assertions.Condition_Lists.List;
         begin
            The_Runfile := Load_Runfile;
            for R of The_Runfile.Run_List loop
               Update_Files_Status (R.Assertions, Updated_List);
            end loop;
            Put_Updated (Updated_List);
         end;

      when Clean =>
         Runfiles.Delete_Targets (Runfiles.Load_Runfile);

      when Reset =>
         Runfiles.Clean_Run_Files;

      when Version =>
         IO.Put_Line (Settings.Smk_Version);

      when Build =>
         Build;

      when Add =>
         Smkfiles.Add_To_Smkfile (Command_Line);

      when Dump =>
         Runfiles.Dump (Runfiles.Load_Runfile);

      when Run =>
         Smkfiles.Add_To_Smkfile (Command_Line);
         Build;

      when Help =>
         Put_Help;

      when None =>
         Put_Error
           ("Internal error : exiting Analyze_Cmd_Line without Command");

   end case;

   if IO.Some_Error and not Ignore_Errors then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end Smk.Main;
