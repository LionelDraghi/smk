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
-- Package: Smk.Cmd_Line body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
--
-- -----------------------------------------------------------------------------

with Smk.IO;
with Smk.Settings;

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;

separate (Smk.Main)

procedure Analyze_Cmd_Line is

   -- --------------------------------------------------------------------------
   Arg_Counter : Positive := 1;

   -- --------------------------------------------------------------------------
   procedure Next_Arg is
   begin
      Arg_Counter := Arg_Counter + 1;
   end Next_Arg;

   -- --------------------------------------------------------------------------
   procedure Set_If_Not_Already_Set (New_Query : in Queries) is
   begin
      if Query = None then
         Query := New_Query;
      else
         Put_Error ("More than one query on command line : "
                    & Queries'Image (New_Query)
                    & " and " & Queries'Image (Query),
                    With_Help => True);
      end if;
   end Set_If_Not_Already_Set;

begin
   -- NB: command line, including arguments should comply with GNU Coding
   -- standards
   -- (https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html)

   while Arg_Counter <= Ada.Command_Line.Argument_Count loop

      declare
         Opt : constant String := Ada.Command_Line.Argument (Arg_Counter);

      begin
         -- 1/3 Queries:
         if Opt = "-rs" or Opt = "--read-smkfile" then
            Set_If_Not_Already_Set (Read_Smkfile);

         elsif Opt = "-rl" or Opt = "--read-last-run" then
            Set_If_Not_Already_Set (Read_Last_Run);

         elsif Opt = "-lr" or Opt = "--list-runs" then
            Set_If_Not_Already_Set (List_Previous_Runs);

         elsif Opt = "-ls" or Opt = "--list-sources" then
            Set_If_Not_Already_Set (List_Sources);

         elsif Opt = "-lt" or Opt = "--list-targets" then
            Set_If_Not_Already_Set (List_Targets);

         elsif Opt = "--clean" then
            Set_If_Not_Already_Set (Clean_Targets);

         elsif Opt = "--reset" then
            Set_If_Not_Already_Set (Clean_Smk_Files);

         elsif Opt = "--version" then
            Set_If_Not_Already_Set (Version);

         elsif Opt = "-b" or Opt = "--build" then
            Set_If_Not_Already_Set (Build);

         elsif Opt = "-h" or Opt = "--help" then
            Set_If_Not_Already_Set (Help);

            -- 2/3 Options:
         elsif Opt = "-a" or Opt = "--always-make" then
            Settings.Always_Make := True;

         elsif Opt = "-e" or Opt = "--explain" then
            Settings.Explain := True;

         elsif Opt = "-n" or Opt = "--dry-run" then
            Settings.Dry_Run := True;

         elsif Opt = "-sa" or Opt = "--show-all-files" then
            Settings.Filter_Sytem_Files := False;

         elsif Opt = "-i" or Opt = "--ignore-errors" then
            Settings.Ignore_Errors := True;

         elsif Opt = "-k" or Opt = "--keep-going" then
            Settings.Keep_Going := True;

         elsif Opt = "-We" or Opt = "--Warnings=error" then
            Settings.Warnings_As_Errors := True;

         elsif Opt = "-v" or Opt = "--verbose" then
            Settings.Verbosity := Verbose;

         elsif Opt = "-q" or Opt = "--quiet" then
            Settings.Verbosity := Quiet;

         elsif Opt = "-d" then
            -- undocumented option
            Settings.Verbosity := Debug;

            -- 3/3 Smkfile:
         elsif Ada.Directories.Exists (Opt) then
            -- should be the Makefile
            Settings.Set_Smkfile_Name (Opt);

         else
            Put_Error ("Unknown Makefile or unknow option "
                       & Opt, With_Help => False);

         end if;

         if IO.Some_Error then return; end if;
         -- No need to further analyze command line, or to do
         -- Options_Coherency_Tests.
      end;

      Next_Arg;

   end loop;

   if Query = None then
      -- default behavior:
      Set_If_Not_Already_Set (Build);
   end if;

   -- Options_Coherency_Tests;

   -- Check for inplicit Smkfile, except if the query doesn't need it
   if Smkfile_Name = "" and Query not in
     List_Previous_Runs | Clean_Smk_Files | Version | Help
   then
      -- IO.Put_Debug_Line ("no smkfile given");
      -- no smkfile given on the command line
      declare
         use type Ada.Containers.Count_Type;
         use Runfiles;
         use Runfiles.File_Lists;
         Run_List : constant File_Lists.Map := Get_Run_List;
         use Ada.Strings.Unbounded;

      begin
         if Run_List.Length = 1 then
            IO.Put_Line ("Run_List.Length = 1", Level => IO.Debug);
            -- implicit smkfile: if there's only one in the current directory,
            -- then, go with it!
            declare
               Runfile_Name : constant String
                 := To_Runfile_Name (+Key (Run_List.First));
               Run          : Runfile;
               -- use Ada.Directories;
            begin
               Settings.Set_Runfile_Name (Runfile_Name);
               IO.Put_Line ("Implicit runfile = " & (Runfile_Name),
                            Level => Debug);

               -- if Exists (Runfile_Name) then
                  Run := Get_Saved_Run (Runfile_Name);
                  Settings.Set_Smkfile_Name (To_String (Run.Smkfile_Name));
                  IO.Put_Line ("Implicit smkfile = " &
                               (To_String (Run.Smkfile_Name)),
                               Level => Debug);
--                 else
--                    Put_Error ("runfile " & Runfile_Name & " not found",
--                               With_Help => False);
--                    Put_Error ("bring it back!, or run smk --reset",
--                               With_Help => False);
--                 end if;

            end;

         elsif Run_List.Length > 1 then
            Put_Error ("No smkfile given, and more than one runfile in dir",
                       With_Help => False);

         else
            Put_Error ("No smkfile given, and no existing runfile in dir",
                       With_Help => False);

         end if;
      end;
   end if;


end Analyze_Cmd_Line;
