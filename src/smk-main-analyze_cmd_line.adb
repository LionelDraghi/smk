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
with Smk.Settings;

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Directories;
with Smk.Smkfiles;          use type Smk.Smkfiles.Smk_File_Name;


separate (Smk.Main)

procedure Analyze_Cmd_Line is

   -- --------------------------------------------------------------------------
   Arg_Counter      : Positive         := 1;
   Unidentified_Opt : Unbounded_String := Null_Unbounded_String;

   -- --------------------------------------------------------------------------
   procedure Next_Arg is
   begin
      Arg_Counter := Arg_Counter + 1;
   end Next_Arg;

   -- --------------------------------------------------------------------------
   procedure Set_If_Not_Already_Set (New_Command : in Commands) is
   begin
      if Current_Command = None then
         Current_Command := New_Command;
      else
         Put_Error ("More than one command on command line : "
                    & Commands'Image (New_Command)
                    & " and " & Commands'Image (Current_Command),
                    With_Help => True);
      end if;
   end Set_If_Not_Already_Set;

   -- --------------------------------------------------------------------------
   function Is_Section_Name (Opt : in String) return Boolean is
   begin
      return Ada.Strings.Fixed.Index (Source  => Opt, Pattern => ":") /= 0;
   end Is_Section_Name;

   -- --------------------------------------------------------------------------
   procedure Process_Section_Name (Opt : in String) is
      Index   : constant Natural := Ada.Strings.Fixed.Index (Opt, ":");
      Smkfile : constant String  := Opt (Opt'First .. Index - 1);
      Section : constant String  := Opt (Index + 1 .. Opt'Last);
   begin
      -- maybe either smkfile:section or :section
      if Smkfile = "" then
         Settings.Set_Section_Name (Section);

      else
         if Ada.Directories.Exists (Smkfile) then
            Settings.Set_Smkfile_Name (Smkfile);
            Settings.Set_Section_Name (Section);
         else
            Put_Error ("Unknown Smkfile " & Smkfile & " in " & Opt,
                       With_Help => False);
         end if;
      end if;

   end Process_Section_Name;

   -- --------------------------------------------------------------------------
   procedure Unidentified_Opt_Processing is

      procedure Reset_Unidentified_Opt is
      begin
         Unidentified_Opt := Null_Unbounded_String;
      end Reset_Unidentified_Opt;

   begin

      case Current_Command is
         -- the unrecognized argument processing depends on the
         -- ongoing command

      when Read_Smkfile | Status | List_Sources | List_Targets |
           List_Unused | Whatsnew =>
         declare
            Opt : constant String := To_String (Unidentified_Opt);

         begin
            -- should be the smkfile
            if Ada.Directories.Exists (Opt) then
               Settings.Set_Smkfile_Name (Opt);
               Reset_Unidentified_Opt;

            else
               Put_Error ("Unknown Smkfile " & Opt,
                          With_Help => False);
            end if;
         end;

      when List_Previous_Runs | Reset | Version | Help | None | Dump =>
         -- no more argument expected
         Put_Error ("Unknown option " & To_String (Unidentified_Opt),
                    With_Help => True);

      when Clean | Build =>
         declare
            Opt : constant String := To_String (Unidentified_Opt);

         begin
            -- may be smkfile or target or section
            if Ada.Directories.Exists (Opt) then
               Settings.Set_Smkfile_Name (Opt);
               Reset_Unidentified_Opt;

            elsif Is_Section_Name (Opt) then
               Process_Section_Name (Opt);
               Reset_Unidentified_Opt;

            else
               Settings.Set_Target_Name (Opt);
               Reset_Unidentified_Opt;

            end if;
         end;

         when Add | Settings.Run =>
            -- the Add case is already process at the beginning of the if,
            -- this line can't be executed!
            Put_Error ("Unknown error in command line on :" &
                         To_String (Unidentified_Opt));

      end case;

   end Unidentified_Opt_Processing;

   -- --------------------------------------------------------------------------
   procedure Implicit_Smkfile_Processing is
      -- if there is no smkfile in the command line, but there is
      -- no ambiguity because there is only one runfile in the current dir,
      -- let's load it.
   begin
      if not Is_Smkfile_Name_Set and Current_Command not in
        List_Previous_Runs | Reset | Version | Help
      then
         -- Check for implicit Smkfile, except if the command doesn't need it
         -- IO.Put_Debug_Line ("no smkfile given");
         -- No smkfile given on the command line
         declare
            use type Ada.Containers.Count_Type;
            use Runfiles;
            use Files;
            use File_Lists;
            use Smkfiles;
            Run_List : constant File_Lists.Map := Get_Run_List;

         begin
            if Run_List.Length = 1 then
               -- implicit smkfile: if there's only one in the
               -- current directory, then, go with it!
               declare
                  Runfile_Name : constant String
                    := To_Runfile_Name (+Key (Run_List.First));
                  Run          : Runfile;
               begin
                  -- Put_Error ("1 ***********************");
                  Settings.Set_Runfile_Name (Runfile_Name);
                  -- Put_Error ("2 ***********************");
                  Run := Get_Saved_Run (+Runfile_Name);
                  -- Put_Error ("3 ***********************");
                  Settings.Set_Smkfile_Name (+Run.Smkfile_Name);
                  -- Put_Error ("4 ***********************");
               end;

            elsif Run_List.Length > 1 then
               Put_Error ("No smkfile given, and more than one runfile in dir",
                          With_Help => False);

            elsif Ada.Directories.Exists (Default_Smkfile_Name) then
               Settings.Set_Smkfile_Name (Default_Smkfile_Name);
               IO.Put_Line ("Using smkfile = " & (Smkfile_Name),
                            Level => Verbose);

            else
               Put_Error ("No smkfile given, and no existing runfile in dir",
                          With_Help => False);

            end if;
         end;

         -- elsif Settings.Target_Name /= "" and Current_Command /= Build then
         --        -- Target_Name is meaningless unless building, so
         --        -- what we took for the target name was just a typo
         --        Put_Error ("Unknown smkfile or unknow option "
         --                   & Settings.Target_Name, With_Help => False);

      end if;

   end Implicit_Smkfile_Processing;

   -- --------------------------------------------------------------------------
   procedure Put_Settings is
      function Checkbox (Switch : Boolean) return String is
        (if Switch then "[X]" else "[ ]");
   begin
      IO.Put_Line ("");
      IO.Put_Line ("Settings / Command line analysis:");
      IO.Put_Line ("---------------------------------");
      IO.Put_Line ("");
      IO.Put_Line ("   Verbosity         : "
                   & Print_Out_Level'Image (Verbosity));
      IO.Put_Line ("   Command           : "
                   & Commands'Image (Current_Command));
      IO.Put_Line ("   Smkfile name      : " & Smkfile_Name);
      IO.Put_Line ("   Runfile name      : " & Runfile_Name);
      IO.Put_Line ("   Strace out file   : " & Strace_Outfile_Name);
      IO.Put_Line ("   Section name      : " & Section_Name);
      IO.Put_Line ("   Cmd Line          : " & Command_Line);
      IO.Put_Line ("   Target name       : " & Target_Name);
      IO.Put_Line ("   Unidentified Opt  : " & To_String (Unidentified_Opt));
      IO.Put_Line ("   Initial directory : " & Initial_Directory);
      IO.Put_Line ("");

      IO.Put_Line ("   System Files      : ");
      for F of System_Files loop
         IO.Put_Line ("   - " & F.all);
      end loop;
      IO.Put_Line ("");
      IO.Put_Line ("   Ignore list       : ");
      for F of Ignore_List loop
         IO.Put_Line ("   - " & F.all);
      end loop;
      IO.Put_Line ("");

      IO.Put_Line ("   " & Checkbox (Always_Make) & " Always_Make");
      IO.Put_Line ("   " & Checkbox (Explain) & " Explain");
      IO.Put_Line ("   " & Checkbox (Dry_Run) & " Dry_Run");
      IO.Put_Line ("   " & Checkbox (Keep_Going) & " Keep_Going");
      IO.Put_Line ("   " & Checkbox (Ignore_Errors) & " Ignore_Errors");
      IO.Put_Line ("   " & Checkbox (Long_Listing_Format)
                   & " Long_Listing_Format");
      -- Put_Line ("   " & Checkbox (Recursive) & " Recursive");
      IO.Put_Line ("   " & Checkbox (Warnings_As_Errors)
                   & " Warnings_As_Errors");
      IO.Put_Line ("   " & Checkbox (Shorten_File_Names)
                   & " Shorten_File_Names");
      IO.Put_Line ("   " & Checkbox (Filter_Sytem_Files)
                   & " Filter_Sytem_Files");
      IO.Put_Line ("");
      IO.Put_Line ("---------------------------------");
      IO.Put_Line ("");
   end Put_Settings;

begin
   -- --------------------------------------------------------------------------
   -- NB: command line, including arguments should comply with GNU Coding
   -- standards
   -- (https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html)

   while Arg_Counter <= Ada.Command_Line.Argument_Count loop

      declare
         Opt : constant String := Ada.Command_Line.Argument (Arg_Counter);

      begin
         -- Commands:
         if Current_Command = Add or Current_Command = Run then
            -- all parameters following "add" are considered as part of the
            -- command line, so there is no analysis of the content
            Add_To_Command_Line (Opt);

         elsif Opt = "build" then
            Set_If_Not_Already_Set (Build);

         elsif Opt = "st" or Opt = "status" then
            Set_If_Not_Already_Set (Status);

         elsif Opt = "clean" then
            Set_If_Not_Already_Set (Clean);

         elsif Opt = "reset" then
            Set_If_Not_Already_Set (Reset);

         elsif Opt = "version" then
            Set_If_Not_Already_Set (Version);

         elsif Opt = "help" or Opt = "-h" or Opt = "--help" then
            Set_If_Not_Already_Set (Help);

         elsif Opt = "rs" or Opt = "read-smkfile" then
            Set_If_Not_Already_Set (Read_Smkfile);

         elsif Opt = "lr" or Opt = "list-runs" then
            Set_If_Not_Already_Set (List_Previous_Runs);

         elsif Opt = "ls" or Opt = "list-sources" then
            Set_If_Not_Already_Set (List_Sources);

         elsif Opt = "lt" or Opt = "list-targets" then
            Set_If_Not_Already_Set (List_Targets);

         elsif Opt = "list-unused" or Opt = "lu" then
            Set_If_Not_Already_Set (List_Unused);

         elsif Opt = "wn" or Opt = "whatsnew" then
            Set_If_Not_Already_Set (Whatsnew);

         elsif Opt = "add" then
            Set_If_Not_Already_Set (Add);

         elsif Opt = "run" then
            Set_If_Not_Already_Set (Run);

         elsif Opt = "dump" then
            Set_If_Not_Already_Set (Dump);

            -- Options:
         elsif Opt = "-a" or Opt = "--always-make" then
            Settings.Always_Make := True;

         elsif Opt = "-e" or Opt = "--explain" then
            Settings.Explain := True;

         elsif Opt = "-n" or Opt = "--dry-run" then
            Settings.Dry_Run := True;

         elsif Opt = "-sa" or Opt = "--show-all-files" then
            Settings.Filter_Sytem_Files := False;

         elsif Opt = "-ds" or Opt = "--dont-shorten" then
            Settings.Shorten_File_Names := False;

         elsif Opt = "-i" or Opt = "--ignore-errors" then
            Settings.Ignore_Errors := True;

         elsif Opt = "-l" or Opt = "--long-listing" then
            Settings.Long_Listing_Format := True;

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

         else
            Unidentified_Opt := To_Unbounded_String (Opt);

         end if;

         if IO.Some_Error then return; end if;
         -- No need to further analyze command line, or to do
         -- Options_Coherency_Tests.
      end;

      Next_Arg;

   end loop;

   -- --------------------------------------------------------------------------
   if Current_Command = None then
      -- default behavior = build
      Set_If_Not_Already_Set (Build);
   end if;

   -- --------------------------------------------------------------------------
   if Unidentified_Opt /= "" then
      Unidentified_Opt_Processing;
   end if;

   -- --------------------------------------------------------------------------
   if not Is_Smkfile_Name_Set and
     (Current_Command = Add or Current_Command = Run)
   then
      -- adding to default smkfile if none given
      Set_Smkfile_Name (Default_Smkfile_Name);
   end if;

   -- --------------------------------------------------------------------------
   Implicit_Smkfile_Processing;

   -- --------------------------------------------------------------------------
   if Debug_Mode then
      Put_Settings;
   end if;

end Analyze_Cmd_Line;
