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

-- -----------------------------------------------------------------------------
-- Package: Smk.Run_Files body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
-- -----------------------------------------------------------------------------

with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;

with Smk.IO;
with Smk.Settings;

package body Smk.Runfiles is

   Run_Fl : Ada.Streams.Stream_IO.File_Type;

   -- --------------------------------------------------------------------------
   function "+" (Name : File_Name) return String is
     (To_String (Name));
   function "+" (Name : String) return File_Name is
     (File_Name'(To_Unbounded_String (Name)));
   function "+" (Name : File_Name) return Unbounded_String is
     (Unbounded_String (Name));

   -- --------------------------------------------------------------------------
   procedure Dump (File_List : in File_Lists.Map) is
      use Runfiles.File_Lists;
   begin
      for F in File_List.Iterate loop
         declare
            Name : constant String            := +Key (F);
            TT   : constant Ada.Calendar.Time := Element (F).Time_Tag;
         begin
            if not (Settings.Filter_Sytem_Files and Element (F).Is_System) then
               IO.Put_Line ("  - " & IO.Image (TT) & ":" & Name);
            end if;
         end;
      end loop;
   end Dump;

   -- --------------------------------------------------------------------------
   procedure Dump (File_List : in File_Lists.Map;
                   Prefix    : in String) is
      use Runfiles.File_Lists;
      use Settings;
   begin
      for F in File_List.Iterate loop
         if not (Settings.Filter_Sytem_Files and Element (F).Is_System) then
            IO.Put_Line (Item => Prefix & (+Key (F)));
         end if;
      end loop;
   end Dump;

   -- --------------------------------------------------------------------------
   function Command_Image (Command : in String;
                           Run     : in Runfiles.Run) return String
   is
      RT_Image : constant String := IO.Image (Run.Run_Time);
      Section  : constant String := " [" & (+Run.Section) & "] ";
   begin
      return (RT_Image & Section & Command);
   end Command_Image;

   -- --------------------------------------------------------------------------
   function Source_Count (Run : in Runfiles.Run) return Natural is
   begin
      if Settings.Filter_Sytem_Files then
         return Natural (Run.Sources.Length) - Run.Source_System_File_Count;
      else
         return Natural (Run.Sources.Length);
      end if;
   end Source_Count;

   -- --------------------------------------------------------------------------
--     procedure List_Dependencies (Run_List : in Run_Lists.Map) is
--        use Run_Lists;
--     begin
--        for L in Run_List.Iterate loop
--           declare
--              Run : constant Runfiles.Run := Element (L);
--              SC  : constant String := Natural'Image (Source_Count (Run));
--           begin
--              IO.Put_Line (Command_Image ((+Key (L)), Run));
--
--              IO.Put_Line ("  Sources (" & SC (2 .. SC'Last) & ") :");
--              Dump (Run.Sources);
--
--              IO.Put_Line ("");
--           end;
--        end loop;
--     end List_Dependencies;

   -- --------------------------------------------------------------------------
   procedure List_Sources (The_Runfile : in Runfile) is
      use Run_Lists;
   begin
      for R in The_Runfile.Run_List.Iterate loop
         declare
            Run    : constant Runfiles.Run := Element (R);
         begin
            if Settings.Long_Listing_Format then
               Dump (Run.Sources,
                     "[" & (+Run.Section) & "]" & (+Key (R)) & ":");
            else
               Dump (Run.Sources, "");
            end if;
         end;
      end loop;
   end List_Sources;

   -- --------------------------------------------------------------------------
   procedure List_Targets (The_Runfile : in Runfile) is
      use Run_Lists;
   begin
      for R in The_Runfile.Run_List.Iterate loop
         declare
            Run    : constant Runfiles.Run := Element (R);
         begin
            if Settings.Long_Listing_Format then
               Dump (Run.Targets,
                     "[" & (+Run.Section) & "]" & (+Key (R)) & ":");
            else
               Dump (Run.Targets, "");
            end if;
         end;
      end loop;
   end List_Targets;

   -- --------------------------------------------------------------------------
   procedure Delete_Targets (The_Runfile : in Runfile) is
      -- use Run_Lists;
      use Ada.Directories;
   begin
      for R of The_Runfile.Run_List loop
         for F in R.Targets.Iterate loop
            declare
               use Runfiles.File_Lists;
               Name : constant String := (+Key (F));
               use Settings;
            begin
               if Exists (Name) then
                  IO.Put_Line ("Deleting " & Name);
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
         for T in R.Targets.Iterate loop
            if Tail (+Key (T), Target'Length) = Target then
               IO.Put_Line ("File " & (+Key (T)) & " match Target "
                            & Target & ".", Level => IO.Verbose);
               return True;
            end if;
         end loop;
      end loop;
      return False;
   end Has_Target;

   -- --------------------------------------------------------------------------
--     procedure Update_Time_Tag (File_List : in out File_Lists.Map) is
--        use Ada.Directories;
--        use File_Lists;
--     begin
--        for C in File_List.Iterate loop
--           declare
--              Name : constant String := +Key (C);
--           begin
--              if Exists (Name) then
--                 File_List.Replace_Element
--                   (C, (Time_Tag  => Ada.Directories.Modification_Time (Name),
--                        Is_System => Element (C).Is_System));
--                 -- Fixme : GNAT 2018 bug on this line:
--                 -- Element (C) := Ada.Directories.Modification_Time (Name);
--              end if;
--           end;
--        end loop;
--     end Update_Time_Tag;

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
   procedure Dump (Run_List : in Run_Lists.Map) is
      use Run_Lists;
      -- use Ada.Containers;
      Target_Count : Natural;
   begin
      for L in Run_List.Iterate loop
         if Settings.Filter_Sytem_Files then
            -- IO.Put_Line
            --   ("  Targets.Length = "
            --    & Natural'Image (Natural (Element (L).Targets.Length)));
            -- IO.Put_Line
            --   ("  Target_System_File_Count ="
            --    & Natural'Image (Element (L).Target_System_File_Count));

            Target_Count := Natural (Element (L).Targets.Length) -
              Element (L).Target_System_File_Count;
         else
            Target_Count := Natural (Element (L).Targets.Length);
         end if;

         declare
            Run : constant Runfiles.Run := Element (L);
            SC  : constant String := Natural'Image (Source_Count (Run));
            TC  : constant String := Natural'Image (Target_Count);
         begin
            IO.Put_Line (Command_Image ((+Key (L)), Run));

            IO.Put_Line ("  Sources (" & SC (2 .. SC'Last) & ") :");
            Dump (Run.Sources);

            IO.Put_Line ("  Targets (" & TC (2 .. TC'Last) & ") :");
            Dump (Run.Targets);

            IO.Put_Line ("");
         end;
      end loop;
   end Dump;

   -- --------------------------------------------------------------------------
   function Runfiles_Found return Boolean is
   begin
      return Ada.Directories.Exists (Settings.Runfile_Name);
   end Runfiles_Found;

   -- --------------------------------------------------------------------------
   function Get_Saved_Run (Runfile_Name : in String) return Runfile is
      use Ada.Streams.Stream_IO;
      The_Runfile : Runfile;
      S           : Stream_Access;
   begin
      Open (Name => Runfile_Name,
            File => Run_Fl,
            Mode => In_File);
      S := Stream (Run_Fl);
      The_Runfile := Runfile'Input (S);
      Close (Run_Fl);
      return The_Runfile;
   end Get_Saved_Run;

   -- --------------------------------------------------------------------------
   procedure Save_Run (The_Run : in Runfile) is
      use Ada.Streams.Stream_IO;
      S : Stream_Access;
      use Settings;
   begin
      Create (Name => To_Runfile_Name (To_String (The_Run.Smkfile_Name)),
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
      use File_Lists;
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
            -- IO.Put_Line ("Get_Run_List inserting = " & Smkfile_Name);
            -- IO.Put_Line ("          (Simple_Name = " & Run_Fl_Name & ")");

            The_List.Insert (Key      => +Smkfile_Name,
                             New_Item => (Modification_Time (File),
                                          Is_System => False));
            -- Fixme: pas compatible avec un smkfile en répertoire relatif!
         end;
      end loop;
      return The_List;
   end Get_Run_List;

   -- --------------------------------------------------------------------------
   procedure Put_Run_List is
      use Runfiles.File_Lists;
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
