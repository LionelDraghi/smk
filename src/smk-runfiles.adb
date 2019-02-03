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

with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;

with Smk.IO;
with Smk.Settings;

package body Smk.Runfiles is

   -- --------------------------------------------------------------------------
   Run_Fl : Ada.Streams.Stream_IO.File_Type;

   -- --------------------------------------------------------------------------
   function Section_Image (Section : Section_Names) return String is
     ("[" & (+Section) & "]");
   function Command_Image (Command : Command_Lines) return String is
       ("""" & (+Command) & """");
   function Time_Image (Time : Ada.Calendar.Time) return String is
     ("[" & IO.Image (Time) & "]");

   -- --------------------------------------------------------------------------
   function Command_Image (Command : Command_Lines;
                           Run     : Runfiles.Run) return String is
   begin
      if +Run.Section = "" then
         return ("Command " & Command_Image (Command)
                 & ", last run " & Time_Image (Run.Run_Time));
      else
         return ("Command " & Command_Image (Command)
                 & " in section " & Section_Image (Run.Section)
                 & ", last run " & Time_Image (Run.Run_Time));
      end if;
   end Command_Image;

   -- --------------------------------------------------------------------------
   procedure Put_File (C             : Condition;
                       Print_Sources : Boolean := False;
                       Print_Targets : Boolean := False;
                       Print_Unused  : Boolean := False;
                       Prefix        : String := "") is
   begin
      if not (Settings.Filter_Sytem_Files and Files.Is_System (C.File))
      then
         if        (Print_Sources and then Files.Is_Source (C.File))
           or else (Print_Targets and then Files.Is_Target (C.File))
           or else (Print_Unused  and then Files.Is_Unused (C.File))
         then
            IO.Put_Line (Prefix & Image (C) & File_Image (C.Name, C.File));
         end if;
      end if;
   end Put_File;

   -- --------------------------------------------------------------------------
   procedure Put_Files (A             : Condition_Lists.List;
                        Print_Sources : Boolean := False;
                        Print_Targets : Boolean := False;
                        Print_Unused  : Boolean := False;
                        Prefix        : String := "") is
   begin
      for C of A loop
         Put_File (C,
                   Print_Sources,
                   Print_Targets,
                   Print_Unused,
                   Prefix);
      end loop;
   end Put_Files;

   -- --------------------------------------------------------------------------
   procedure Put_Files (The_Runfile   : Runfile;
                        Print_Sources : Boolean := False;
                        Print_Targets : Boolean := False;
                        Print_Unused  : Boolean := False) is
      use Run_Lists;
   begin
      for R in The_Runfile.Run_List.Iterate loop
         declare
            Prefix : constant String
              := (if Settings.Long_Listing_Format
                  then Command_Image (Key (R)) & " " &
                    Section_Image (Element (R).Section) & " "
                  else "");
         begin
            Put_Files (Element (R).Assertions,
                       Print_Sources,
                       Print_Targets,
                       Print_Unused,
                       Prefix);
         end;
      end loop;
   end Put_Files;

   -- --------------------------------------------------------------------------
   procedure Put_Updated (Cond_List : in Assertions.Condition_Lists.List) is
      Something_Updated : Boolean := False;

   begin
      for C of Cond_List loop
         declare
            Name : File_Name renames C.Name;
            File : File_Type renames C.File;
         begin
            if Role (File) /= Unused and Status (File) /= Identical then
               -- unused files are ignored
               if Settings.Long_Listing_Format then
                  -- normal behavior
                  IO.Put_Line (File_Image (Name, File));
                  Something_Updated := True;

               else
                  -- Even if in short form, we want to print the Status field
                  IO.Put_Line
                    (File_Image (Name, File,
                     Prefix => "[" & Status_Image (Status (File)) & "] "
                     & "[" & Role_Image (Role (File)) & "] "));
                  Something_Updated := True;

               end if;

            end if;
         end;
      end loop;

      if not Something_Updated then
         IO.Put_Line ("Nothing new");
      end if;

   end Put_Updated;

   -- --------------------------------------------------------------------------
   procedure Put_Run (Run_List : in Run_Lists.Map) is
      use type Ada.Containers.Count_Type;
   begin
      if Run_List.Length = 0 then
         IO.Put_Line ("No recorded run");
         return;
      end if;

      for L in Run_List.Iterate loop
         declare
            Run : constant Runfiles.Run := Run_Lists.Element (L);
            SC  : constant File_Count   := Count
              (Run.Assertions,
               Count_Sources => True,
               With_System_Files => (not Settings.Filter_Sytem_Files));
            TC  : constant File_Count   := Count
              (Run.Assertions,
               Count_Targets     => True,
               With_System_Files => (not Settings.Filter_Sytem_Files));
            SC_Image : constant String := Count_Image (SC);
            TC_Image : constant String := Count_Image (TC);

         begin
            IO.Put_Line (Command_Image (Run_Lists.Key (L), Run));
            IO.Put_Line ("  Sources: (" & SC_Image & ")");
            Put_Files (Run.Assertions,
                       Print_Sources => True,
                       Prefix        => "  - ");
            IO.Put_Line ("  Targets: (" & TC_Image & ")");
            Put_Files (Run.Assertions,
                       Print_Targets => True,
                       Prefix        => "  - ");
            IO.New_Line;

         end;
      end loop;
   end Put_Run;

   -- --------------------------------------------------------------------------
   procedure Dump (The_Runfile : Runfile) is
   begin
      -- Force long format, show all and don't shorten
      Settings.Long_Listing_Format := True;
      Settings.Shorten_File_Names  := False;
      Settings.Filter_Sytem_Files  := False;

      for R in The_Runfile.Run_List.Iterate loop
         declare
            use Run_Lists;
            Run : constant Runfiles.Run := Element (R);
         begin
            IO.Put_Line (Command_Image (Key (R), Run));
            for A of Run.Assertions loop
               IO.Put_Line (Image (A) & File_Image (A.Name, A.File));
            end loop;
         end;
      end loop;
   end Dump;

   -- --------------------------------------------------------------------------
   function Get_File_List (The_Runfile : Runfile) return File_Lists.Map is
      List : File_Lists.Map;
   begin
      for R of The_Runfile.Run_List loop
         for A of R.Assertions loop
            if not List.Contains (A.Name) then
               List.Insert (Key      => A.Name,
                            New_Item => Create (File => A.Name,
                                                Role => Unused));
               -- IO.Put_Line ("Inserting " & File_Image (A.Name, A.File));
            end if;
         end loop;
      end loop;
      return List;
   end Get_File_List;

   -- --------------------------------------------------------------------------
   function Get_Dir_List (From : File_Lists.Map) return File_Lists.Map is
      List : File_Lists.Map;
      use File_Lists;
   begin
      for F in From.Iterate loop
         declare
            Name : File_Name renames Key (F);
            File : File_Type renames Element (F);
         begin
            -- IO.Put_Line ("Get_Dir_List (" & (+Name) & ")");
            if not List.Contains (Name) and then Is_Dir (File) then
               List.Insert (Key      => Name,
                            New_Item => File);
               -- IO.Put_Line ("Extracting dir " & File_Image (Name, File));
            end if;
         end;
      end loop;
      return List;
   end Get_Dir_List;

   -- --------------------------------------------------------------------------
   procedure Update_Files_Status (Assertions   : in out Condition_Lists.List;
                                  Updated_List : in out Condition_Lists.List) is
      use Smk.Assertions.Condition_Lists;
   begin
      -- -----------------------------------------------------------------------
      for A of Assertions loop
         declare
            Previous_Status : File_Status;
            Current_Status  : File_Status;
            Name            : File_Name renames A.Name;
            File            : File_Type renames A.File;

         begin
            Files.Update_File_Status (Name, File,
                                      Previous_Status, Current_Status);
            if Current_Status /= Identical
              and then not Updated_List.Contains (A)
            then
               Updated_List.Append (A);
               if Previous_Status /= Current_Status then
                  IO.Put_Line
                    (+Name & " status changed from "
                     & Files.Status_Image (Previous_Status) & " to "
                     & Files.Status_Image (Current_Status),
                     Level => IO.Debug);
               end if;
            end if;
         end;
      end loop;
      Name_Sorting.Sort (Updated_List);
   end Update_Files_Status;

   -- --------------------------------------------------------------------------
   procedure Update_Files_Status (In_Run_List  : in out Run_Lists.Map;
                                  Updated_List : in out Condition_Lists.List) is
   begin
      for R of In_Run_List loop
         Update_Files_Status (R.Assertions, Updated_List);
      end loop;
   end Update_Files_Status;

   -- --------------------------------------------------------------------------
   procedure Delete_Targets (The_Runfile : in Runfile) is
      use Ada.Directories;
      -- -----------------------------------------------------------------------
      procedure Delete (P           : Condition;
                        Delete_Dir  : Boolean := False;
                        Delete_File : Boolean := False) is
         Name : constant String := +P.Name;
      begin
         if Is_Target (P.File) then
            if Exists (Name) then
               if Is_Dir (Name) then
                  if Delete_Dir then
                     IO.Put_Line ("Deleting dir " & Shorten (P.Name));
                     if not Settings.Dry_Run then
                        begin
                           Delete_Directory (Name);
                        exception
                           when Use_Error => null; -- the dir is not empty
                        end;
                     end if;
                  end if;
               else -- not a dir
                  if Delete_File then
                     IO.Put_Line ("Deleting file " & Shorten (P.Name));
                     if not Settings.Dry_Run then
                        Ada.Directories.Delete_File (Name);
                     end if;
                  end if;
               end if;
            end if;
         else
            IO.Put_Line ("Target to delete not found : " & (+P.Name),
                         Level => Settings.Verbose);
         end if;
      end Delete;

   begin
      -- -----------------------------------------------------------------------
      for R of The_Runfile.Run_List loop
         for P of R.Assertions loop
            Delete (P, Delete_File => True);
         end loop;
      end loop;

      -- Fixme: directories are erased after files to avoid a rmdir fail
      -- because of a file present in the dir, that will be erased after.
      -- But this is still wrong as we dont erase dir in a smart order,
      -- and we may try to delete dir1 before dir1/dir2.
      for R of The_Runfile.Run_List loop
         for P of R.Assertions loop
            Delete (P, Delete_Dir => True);
         end loop;
      end loop;

   end Delete_Targets;

   -- --------------------------------------------------------------------------
   function Has_Target (The_Run_List : Run_Lists.Map;
                        Target       : String) return Boolean is
   begin
      -- -----------------------------------------------------------------------
      if Target = "" then return False; end if;

      for R of The_Run_List loop
         for P of R.Assertions loop
            if Is_Target (P.File) and then Has_Target (P.Name, Target) then
               return True;
            end if;
         end loop;
      end loop;

      return False;

   end Has_Target;

   -- --------------------------------------------------------------------------
   procedure Insert_Or_Update (The_Command     : in     Command_Lines;
                               The_Run         : in     Run;
                               In_Run_List     : in out Run_Lists.Map) is
   begin
      if In_Run_List.Contains (The_Command) then
         In_Run_List.Replace (Key      => The_Command,
                              New_Item => The_Run);

      else
         In_Run_List.Insert (Key      => The_Command,
                             New_Item => The_Run);
      end if;
   end Insert_Or_Update;

   -- --------------------------------------------------------------------------
   function Runfiles_Found return Boolean is
   begin
      return Ada.Directories.Exists (Settings.Runfile_Name);
   end Runfiles_Found;

   -- --------------------------------------------------------------------------
   function Get_Saved_Run (Runfile_Name : in File_Name) return Runfile is
      use Ada.Streams.Stream_IO;
      The_Runfile : Runfile;
      S           : Stream_Access;
   begin
      Open (Name => +Runfile_Name,
            File => Run_Fl,
            Mode => In_File);
      S := Stream (Run_Fl);
      The_Runfile := Runfile'Input (S);
      Close (Run_Fl);
      return The_Runfile;
   end Get_Saved_Run;

   -- --------------------------------------------------------------------------
   function Load_Runfile return Runfile is
      The_Runfile : Runfile;
      use Settings;
   begin
      if Runfiles_Found then
         The_Runfile := Get_Saved_Run (+To_Runfile_Name (Smkfile_Name));
      else
         The_Runfile := (Smkfile_Name => +Smkfile_Name,
                         Run_List     => Run_Lists.Empty_Map);
      end if;
      return The_Runfile;
   end Load_Runfile;

   -- --------------------------------------------------------------------------
   procedure Save_Run (The_Run : in Runfile) is
      use Ada.Streams.Stream_IO;
      S : Stream_Access;
      use Settings;
   begin
      Create (Name => To_Runfile_Name (+The_Run.Smkfile_Name),
              File => Run_Fl,
              Mode => Out_File);
      S := Stream (Run_Fl);
      Runfile'Output (S, The_Run);
      Close (Run_Fl);
   end Save_Run;

   -- --------------------------------------------------------------------------
   procedure Clean_Run_Files is
      use Ada.Directories;
      Search : Search_Type;
      File   : Directory_Entry_Type;
   begin
      Start_Search (Search,
                    Directory => ".",
                    Pattern   => Settings.Smk_File_Prefix & "*",
                    Filter    => (Ordinary_File => True,
                                  others        => False));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, File);
         IO.Put_Line ("Deleting " & Simple_Name (File));
         Delete_File (Simple_Name (File));
      end loop;
   end Clean_Run_Files;

   -- --------------------------------------------------------------------------
   function Get_Run_List return File_Lists.Map is
      use Ada.Directories;
      Search   : Search_Type;
      File     : Directory_Entry_Type;
      The_List : File_Lists.Map;
   begin
      Start_Search (Search,
                    Directory => ".",
                    Pattern   => Settings.Smk_File_Prefix & "*",
                    Filter    => (Ordinary_File => True,
                                  others        => False));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, File);
         declare
            Prefix_Length : constant Natural := Settings.Smk_File_Prefix'Length;
            Run_Fl_Name   : constant String  := Simple_Name (File);
            Smkfile_Name  : constant String  := Ada.Strings.Fixed.Delete
              (Source  => Run_Fl_Name,
               From    => Run_Fl_Name'First,
               Through => Run_Fl_Name'First + Prefix_Length - 1);
            -- removing File_Prefix
         begin
            The_List.Insert (Key      => +Smkfile_Name,
                             New_Item => Create (+Smkfile_Name, Unused));
         end;
      end loop;
      return The_List;
   end Get_Run_List;

   -- --------------------------------------------------------------------------
   procedure Put_Run_List is
      use File_Lists;
      Run_List : constant File_Lists.Map := Runfiles.Get_Run_List;
      use type Ada.Containers.Count_Type;
   begin
      if Run_List.Length = 0 then
         IO.Put_Line ("No run file");
      else
         for F in Run_List.Iterate loop
            IO.Put_Line (+Key (F));
         end loop;
      end if;
   end Put_Run_List;

end Smk.Runfiles;
