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
with Smk.IO;
with Smk.Files;
with Smk.Files.File_Lists;
with Smk.Smkfiles;
with Smk.Runfiles;
with Smk.Settings;          use Smk.Settings;

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

procedure Smk.Main is

   -- Debug : constant Boolean := True;

   -- --------------------------------------------------------------------------
   -- Put_Line Utilities:
   procedure Put_Help is separate;
   procedure Put_Error (Msg       : in String  := "";
                        With_Help : in Boolean := False) is separate;

   -- --------------------------------------------------------------------------
   procedure Analyze_Cmd_Line is separate;
   -- Cmd line options are then available in the Settings package.

   -- --------------------------------------------------------------------------
   procedure Analyze_Run
     (Source_Files             : out Files.File_Lists.Map;
      Source_System_File_Count : out Natural;
      Target_Files             : out Files.File_Lists.Map;
      Target_System_File_Count : out Natural) is separate;
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
   is separate; -- Fixme: to be moved as a Run_All_Commands separate
   -- Run_Command is in charge of spawning the Cmd (using strace),
   -- and analysing the strace log file.
   -- The_Run_List is updated with this run results

   -- --------------------------------------------------------------------------
   procedure Run_All_Commands (The_Smkfile   : in out Smkfiles.Smkfile;
                               The_Run_List  : in out Runfiles.Run_Lists.Map;
                               Cmd_To_Run    :    out Boolean;
                               Error_In_Run  :    out Boolean;
                               Section_Found :    out Boolean;
                               Target_Found  :    out Boolean)
   is separate;

   -- --------------------------------------------------------------------------
   procedure Process_Build is
      The_Smkfile   : Smkfiles.Smkfile;
      The_Runfile   : Runfiles.Runfile;
      Cmd_To_Run    : Boolean;
      Error_In_Run  : Boolean;
      Section_Found : Boolean;
      Target_Found  : Boolean;
      use Smk.Smkfiles;

   begin
      Smkfiles.Analyze (+Smkfile_Name, The_Smkfile);

      The_Runfile := Runfiles.Load_Runfile;

      Run_All_Commands (The_Smkfile,
                        The_Runfile.Run_List,
                        Cmd_To_Run,
                        Error_In_Run,
                        Section_Found,
                        Target_Found);

      if Settings.Section_Name /= "" and not Section_Found then
         IO.Put_Line ("No section """ & Settings.Section_Name &
                        """ in " & (+The_Smkfile.Name));

      elsif Settings.Target_Name /= "" and not Target_Found then
         IO.Put_Line
           ("Target """ & Settings.Target_Name & """ not found");
         IO.Put_Line
           ("run smk list-targets to get a list of possible target");

      elsif not Cmd_To_Run then
         IO.Put_Line ("Nothing to run");

      end if;

      if Error_In_Run and not Ignore_Errors then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;

      -- Save the updated run:
      Runfiles.Save_Run (The_Runfile);
   end Process_Build;

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
         declare
            The_Smkfile : Smkfiles.Smkfile;
            use Smkfiles;
         begin
            Smkfiles.Analyze (+Smkfile_Name, The_Smkfile);
            Smkfiles.Dump (The_Smkfile);
         end;

      when Status =>
         declare
            The_Runfile : Runfiles.Runfile;
            use Files;
         begin
            if Runfiles.Runfiles_Found then
               The_Runfile := Runfiles.Get_Saved_Run
                 (+To_Runfile_Name (Smkfile_Name));
               Runfiles.Dump (The_Runfile.Run_List);
            else
               Put_Error ("No previous run found.");
            end if;
         end;

      when List_Previous_Runs =>
         Runfiles.Put_Run_List; -- Fixme:

      when List_Targets =>
         Runfiles.List_Targets (Runfiles.Load_Runfile);

      when List_Sources =>
         Runfiles.List_Sources (Runfiles.Load_Runfile);

      when Whatsnew =>
         declare
            use Runfiles;
            The_Runfile  : Runfile;
            Updated_List : Files.File_Lists.Map;
         begin
            The_Runfile := Load_Runfile;
            Update_Files_Status (The_Runfile, Updated_List);
            List_Updated (Updated_List);
            Save_Run (The_Runfile);
         end;

      when Clean =>
         Runfiles.Delete_Targets (Runfiles.Load_Runfile);

      when Reset =>
         Runfiles.Clean_Run_Files;

      when Version =>
         IO.Put_Line (Settings.Smk_Version);

      when Build =>
         Process_Build;

      when Add =>
         Smkfiles.Add_To_Smkfile (Command_Line);

      when Run =>
         Smkfiles.Add_To_Smkfile (Command_Line);
         Process_Build;

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
