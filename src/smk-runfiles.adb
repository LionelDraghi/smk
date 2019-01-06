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

with Smk.Files.Put;
with Smk.IO;
with Smk.Settings;

package body Smk.Runfiles is

   -- --------------------------------------------------------------------------
   Run_Fl : Ada.Streams.Stream_IO.File_Type;

   -- --------------------------------------------------------------------------
   function Count (Count             : Counts;
                   With_System_Files : Boolean := False) return Natural
   is
      Sum : Natural := 0;
   begin
      Sum := Count (False);
      if With_System_Files  then
         Sum := Sum + Count (True);
      end if;
      return Sum;
   end Count;

   function Sources_Count (Counts            : File_Counts;
                           With_System_Files : Boolean := False)
                           return Natural is
   begin
      return Count (Counts.Sources, With_System_Files);
   end Sources_Count;
   function Targets_Count (Counts            : File_Counts;
                           With_System_Files : Boolean := False)
                           return Natural is
   begin
      return Count (Counts.Targets, With_System_Files);
   end Targets_Count;

   -- --------------------------------------------------------------------------
   function Count_Image (Count : Natural) return String is
      Raw : constant String := Natural'Image (Count);
   begin
      return Raw (2 .. Raw'Last);
   end Count_Image;

   -- --------------------------------------------------------------------------
   function Counts_Image (Counts : File_Counts) return String is
     ("Sources [Ord:" & Count_Image (Sources_Count (Counts, False))
      & "/ Sys:" & Count_Image (Sources_Count (Counts, True))
      & "], Targets [Ord:" & Count_Image (Targets_Count (Counts, False))
      & "/ Sys:" & Count_Image (Targets_Count (Counts, True)) & "]");

   -- --------------------------------------------------------------------------
   function Section_Image (Section : Section_Names) return String is
      -- (if (+Section) = "" then "" else "[" & (+Section) & "]");
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
   procedure Put_Files (The_Runfile   : Runfile;
                        Print_Sources : Boolean := False;
                        Print_Targets : Boolean := False) is
      use Run_Lists;
   begin
      for R in The_Runfile.Run_List.Iterate loop
         declare
            Prefix : constant String :=
                       (if Settings.Long_Listing_Format
                        then Command_Image (Key (R)) & " " &
                          Section_Image (Element (R).Section) & " "
                        else "");
         begin
            Files.Put (File_List     => Element (R).Files,
                       Prefix        => Prefix,
                       Print_Sources => Print_Sources,
                       Print_Targets => Print_Targets);
         end;
      end loop;
   end Put_Files;

   -- --------------------------------------------------------------------------
   procedure Put_Updated (File_List : in File_Lists.Map) is
      use File_Lists;
   begin
      for I in File_List.Iterate loop
         declare
            Name : File_Name renames File_Lists.Key (I);
            File : File_Type renames File_Lists.Element (I);
         begin
            if Role (File) /= Unused and Status (File) /= Identical then
               -- unused files are ignored
               if Settings.Long_Listing_Format then
                  -- normal behavior
                  Put_File_Description (Name, File, Prefix => "");

               else
                  -- Even if in short form, we want to print the Status field
                  Put_File_Description
                    (Name, File,
                     Prefix => "[" & Status_Image (Status (File)) & "] ");

               end if;

            end if;
         end;
      end loop;
   end Put_Updated;

   -- --------------------------------------------------------------------------
   procedure Put_Run (Run_List : in Run_Lists.Map) is
      use Run_Lists;
      use type Ada.Containers.Count_Type;
   begin
      if Run_List.Length = 0 then
         IO.Put_Line ("No recorded run");
         return;
      end if;

      for L in Run_List.Iterate loop
         declare
            Run : constant Runfiles.Run := Element (L);
            SC  : constant String := Count_Image
              (Sources_Count (Run.Counts, With_System_Files =>
                        (not Settings.Filter_Sytem_Files)));
            TC  : constant String := Count_Image
              (Targets_Count (Run.Counts, With_System_Files =>
                        (not Settings.Filter_Sytem_Files)));

         begin
            IO.Put_Line (Command_Image (Key (L), Run));
            if Settings.Long_Listing_Format then
               -- long form:
               IO.Put_Line ("Sources: (" & SC & ")");
               Smk.Files.Put (Run.Files, "  - ", Print_Sources => True);
               IO.Put_Line ("Targets: (" & TC & ")");
               Smk.Files.Put (Run.Files, "  - ", Print_Targets => True);
               IO.Put_Line ("");
            end if;

         end;
      end loop;
   end Put_Run;

   -- --------------------------------------------------------------------------
   procedure Dump (The_Runfile : Runfile) is
      use type Ada.Containers.Count_Type;
   begin
      if The_Runfile.Run_List.Length = 0 then
         IO.Put_Line ("No recorded run");
         return;
      end if;

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
            for F in Run.Files.Iterate loop
               Dump_File_Description (Name   => File_Lists.Key (F),
                                      File   => File_Lists.Element (F));
            end loop;
            for F in Run.Dirs.Iterate  loop
               Dump_File_Description (Name   => File_Lists.Key (F),
                                      File   => File_Lists.Element (F));
            end loop;
         end;
      end loop;
   end Dump;

   -- --------------------------------------------------------------------------
   procedure Update_Files_Status (File_List    : in out File_Lists.Map;
                                  Updated_List : in out File_Lists.Map) is
      use Smk.Files.File_Lists;
   begin
      -- -----------------------------------------------------------------------
      for I in File_List.Iterate loop
         declare
            Previous_Status : File_Status;
            Current_Status  : File_Status;
            Name            : File_Name renames File_Lists.Key (I);
            File            : File_Type renames File_List (I);
         begin
            -- Fixme: precond : Is_Dir = False
            if Is_Dir (File) then
               IO.Put_Error ("Update_Files_Status:"  & (+Name) & " is a dir");
            end if;

            Files.Update_File_Status (Name, File,
                                      Previous_Status, Current_Status);
            if Current_Status /= Identical
              and then Role (File) /= Unused
              and then not Updated_List.Contains (Name)
              and then not Settings.In_Ignore_List (+Name)
            then
               Updated_List.Insert (Name, File);
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
   end Update_Files_Status;

   --------------------------------------------------------------------------
   procedure Update_Dirs_Status (The_Run      : in out Run;
                                 Updated_List : in out File_Lists.Map) is
      use Smk.Files.File_Lists;

      Dir_List  : File_Lists.Map renames The_Run.Dirs;
      File_List : File_Lists.Map renames The_Run.Files;

      -----------------------------------------------------------------------
      function New_Files_In_Dir (Dir : in String) return File_Lists.Map is
         -- returns the list of new files and dirs in Dir
         use Ada.Directories;
         Search    : Search_Type;
         File      : Directory_Entry_Type;
         New_Files : File_Lists.Map;
      begin
         IO.Put_Line ("Start searching new files in " & Dir,
                      Level => IO.Debug);
         Start_Search (Search,
                       Directory => Dir,
                       Pattern   => "*",
                       Filter    => (Ordinary_File => True,
                                     Directory     => True,
                                     others        => False));
         while More_Entries (Search) loop
            Get_Next_Entry (Search, File);
            declare
               Full_Name   : constant String
                 := Ada.Directories.Full_Name   (File);
               Simple_Name : constant String
                 := Ada.Directories.Simple_Name (File);
            begin
               if Simple_Name = ".." or Simple_Name = "." then
                  IO.Put_Line ("Ignoring "
                               & Simple_Name,
                               Level => IO.Debug);

               elsif Updated_List.Contains (+Full_Name) then
                  IO.Put_Line ("File "
                               & Simple_Name
                               & " already known as Updated",
                               Level => IO.Debug);

               elsif File_List.Contains (+Full_Name) then
                  IO.Put_Line ("File "
                               & Simple_Name
                               & " already known, but not Updated",
                               Level => IO.Debug);

               elsif Dir_List.Contains (+Full_Name) then
                  IO.Put_Line ("Dir "
                               & Simple_Name
                               & " already known, but not Updated",
                               Level => IO.Debug);

               else
                  -- A genuine new file:
                  IO.Put_Line ("New file " & Simple_Name,
                               Level => IO.Debug);
                  if not Settings.In_Ignore_List (Simple_Name) then
                     New_Files.Insert (+Full_Name,
                                       Create (File => +Full_Name,
                                               Role => Unused));
                  end if;
               end if;
            end;

         end loop;

         return New_Files;

      end New_Files_In_Dir;

      New_Files : File_Lists.Map;

   begin
      -----------------------------------------------------------------------
      for I in Dir_List.Iterate loop
         declare
            Previous_Status : File_Status;
            Current_Status  : File_Status;
            Name            : File_Name renames File_Lists.Key (I);
            File            : File_Type renames Dir_List (I);
         begin
            if not Is_Dir (File) then
               IO.Put_Error ("Update_Dirs_Status: " & (+Name)
                             & " is not a dir");
            end if;

            Files.Update_File_Status (Name, File,
                                      Previous_Status, Current_Status);
            if Current_Status /= Identical
            -- if Identical, don't need to look for new files
              and then Current_Status /= Missing
            -- if Missing, can't look for new files
              and then not Settings.In_Ignore_List (+Name)
            then
               New_Files := Empty_Map;
               if not Updated_List.Contains (Name) then

                  New_Files := New_Files_In_Dir (+Name);

                  if New_Files.Is_Empty then
                     -- Updated_List.Insert (Name, File);
                     IO.Put_Line ("Updated dir " & (+Name)
                                  & " but no new file",
                                  Level => IO.Debug);
                     -- Set_Status (Dir_List (I), Identical);
                     -- Updated Dir are only reported if there is a new file
                     -- (new meaning that is not in File_List).

                  else
                     -- it's a dir with new files
                     Updated_List.Insert (Name, File);
                     IO.Put_Line ("Dir " & (+Name) & " with new file(s)",
                                  Level => IO.Debug);

                  end if;
               end if;
            end if;
         end;
      end loop;

      -- New_Files are also inserted in the list of known file,
      -- with an Unused status
      for F in New_Files.Iterate loop
         if Settings.In_Ignore_List (+Key (F)) then
            IO.Put_Line ("Ignoring " & (+Key (F)),
                         Level => IO.Debug);

         elsif Is_Dir (New_Files (F)) then
            declare
               Name : constant File_Name := Key (F);
               Dir  : constant File_Type := Element (F);
            begin
               IO.Put_Line ("Adding dir " & (+Name)
                            & " to dir list",
                            Level => IO.Debug);
               Dir_List.Insert (Name, Dir);
            end;

         else
            declare
               Name : constant File_Name := Key (F);
               File : constant File_Type := Element (F);
            begin
               IO.Put_Line ("Adding file "
                            & (+Name) & " to file list",
                            Level => IO.Debug);
               -- Set_Role (File, Unused);
               File_List.Insert (Name, File);
            end;

         end if;
      end loop;

   end Update_Dirs_Status;

   -- --------------------------------------------------------------------------
   procedure Delete_Targets (The_Runfile : in Runfile) is
      use Ada.Directories;
   begin
      for R of The_Runfile.Run_List loop
         for F in R.Files.Iterate loop
            declare
               use File_Lists;
               Name : constant String := (+Key (F));
               use Settings;
            begin
               if Is_Target (R.Files (F)) and then Exists (Name) then
                  IO.Put_Line ("Deleting " & Shorten (Name));
                  if not Dry_Run then
                     Delete_File (Name);
                  end if;
               else
                  IO.Put_Line ("Target to delete not found : " & Name,
                               Level => Verbose);
               end if;
            end;
         end loop;
      end loop;
   end Delete_Targets;

   -- --------------------------------------------------------------------------
   function Has_Target (The_Run_List : Run_Lists.Map;
                        Target       : String) return Boolean is
      use File_Lists;
   begin
      if Target = "" then return False; end if;

      for R of The_Run_List loop
         for T in R.Files.Iterate loop
            if Is_Target (R.Files (T)) and then
              Ada.Strings.Fixed.Tail (+Key (T), Target'Length) = Target
            then
               IO.Put_Line ("File " & (+Key (T)) & " match Target "
                            & Target & ".", Level => IO.Verbose);
               return True;
            end if;
         end loop;
      end loop;
      return False;
   end Has_Target;

   -- --------------------------------------------------------------------------
   procedure Reset (Counts : in out Runfiles.File_Counts) is
   begin
      Counts := (others => (others => 0));
   end Reset;

   -- --------------------------------------------------------------------------
   procedure Update_Counts (File_list : in     Files.File_Lists.Map;
                            Counts    : in out Runfiles.File_Counts) is
   begin
      for F of File_list  loop
         declare
            Sys : constant Boolean := Is_System (F);
         begin
            if Is_Source (F) then
               Counts.Sources (Sys) := Counts.Sources (Sys) + 1;
            end if;
            if Is_Target (F) then
               Counts.Targets (Sys) := Counts.Targets (Sys) + 1;
            end if;
         end;
      end loop;
   end Update_Counts;

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
