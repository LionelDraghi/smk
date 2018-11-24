-- -----------------------------------------------------------------------------
-- smk, the smart make
-- Copyright 2018 Lionel Draghi
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
-- Package: Smk.Makefiles body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
-- -----------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;                use Ada.Text_IO;

with Smk.IO;

package body Smk.Makefiles is

   Debug  : constant Boolean := False;
   Prefix : constant String  := " smk-makefile.adb ";

   use Ada.Strings;
   use Ada.Strings.Fixed;

   Current_Section : Run_Files.Section_Names := Run_Files.Default_Section;

   use Ada.Strings.Maps;
   use Ada.Strings.Maps.Constants;
   Whitespace_Set : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set
       (Ada.Characters.Latin_1.HT & Ada.Characters.Latin_1.Space);
   Identifier_Set : constant Ada.Strings.Maps.Character_Set
     := Alphanumeric_Set or To_Set (".-_");

   -- --------------------------------------------------------------------------
   function Is_Empty (Line : in String) return Boolean is
     (Index_Non_Blank (Line) = 0);

   -- --------------------------------------------------------------------------
   function Is_A_Comment (Line : in String) return Boolean is
   begin
      return
        Head (Line, Count => 1) = "#"   or else -- Shell style comment
        Head (Line, Count => 2) = "--"  or else -- Ada   style comment
        Head (Line, Count => 2) = "//";         -- Java  style comment
   end Is_A_Comment;

   -- --------------------------------------------------------------------------
   function Is_A_Section (Line : in String) return Boolean is
      -- A section line is an identifier followed by a semicolon.
      -- NB: this function updates the global Current_Section variable
      First : Positive;
      Last  : Natural;
   begin
      Find_Token (Source => Line,
                  Set    => Identifier_Set,
                  Test   => Inside,
                  First  => First,
                  Last   => Last);
      if Last = 0 then
         return False;
         -- The line don't start with an identifier, can't be a section
      else
         IO.Put_Debug_Line (Line (First .. Last) & "<", Debug, Prefix);
         if Index (Source  => Line,
                   Pattern => ":",
                   From    => Last + 1,
                   Going   => Forward) /= 0
         then
            Current_Section := +(Line (First .. Last));
            return True;
         else
            return False;
         end if;
      end if;
   end Is_A_Section;

  -- --------------------------------------------------------------------------
   procedure Analyze (Makefile_Name : in     String;
                      Line_List     :    out Makefile) is
      Make_Fl    : Ada.Text_IO.File_Type;
      Entry_List : Makefile_Entry_Lists.List;

   begin
      Open (Make_Fl, Mode => In_File, Name => Makefile_Name);

      Analysis : while not End_Of_File (Make_Fl) loop
         declare
            Line : constant String := Trim (Get_Line (Make_Fl), Side => Both);
            Line_Nb : constant Integer := Integer (Ada.Text_IO.Line (Make_Fl));

         begin
            if Is_A_Comment (Line) then
               IO.Put_Debug_Line (Line & "<",
                                  Debug  => Debug,
                                  Prefix => Prefix & "Comment    >",
                                  File   => Makefile_Name,
                                  Line   => Line_Nb);

            elsif Is_Empty (Line) then
               IO.Put_Debug_Line (Line & "<",
                                  Debug  => Debug,
                                  Prefix => Prefix & "Empty line >",
                                  File   => Makefile_Name,
                                  Line   => Line_Nb);

            elsif Is_A_Section (Line) then
               IO.Put_Debug_Line (+Current_Section & "<",
                                  Debug  => Debug,
                                  Prefix => Prefix & "Section    >",
                                  File   => Makefile_Name,
                                  Line   => Line_Nb);

            else
               -- Last but not least, it's a command line.
               declare
                  First : Positive;
                  Last  : Natural;
               begin
                  -- Let's go to the first Identifier
                  Find_Token (Source => Line,
                              Set    => Whitespace_Set,
                              Test   => Outside,
                              First  => First,
                              Last   => Last);
                  IO.Put_Debug_Line (Line (First .. Line'Last) & "<",
                                     Debug  => Debug,
                                     Prefix => Prefix & "Command    >",
                                     File   => Makefile_Name,
                                     Line   => Line_Nb);
                  Entry_List.Append
                    ((Line        => Line_Nb - 1, -- why -1 ???
                      Section     => Current_Section,
                      Command     => +(Line (First .. Line'Last)),
                      Already_Run => False));
               end;

            end if;

         end;

      end loop Analysis;

      Close (Make_Fl);

      declare
         use Ada.Directories;
      begin
         Line_List := (Name     => +Makefile_Name,
                       Time_Tag => Modification_Time (Makefile_Name),
                       Entries  => Entry_List);
      end;

   end Analyze;

   -- --------------------------------------------------------------------------
   procedure Dump (The_Make_File : in Makefile) is
      Time_Tag : constant String := IO.Image (The_Make_File.Time_Tag);

   begin
      IO.Put_Line (+The_Make_File.Name & " (" & Time_Tag & ") :");

      for E of The_Make_File.Entries loop
         IO.Put_Line (Trim (Positive'Image (E.Line), Left)
                        & ": [" & (+E.Section) & "] " & (+E.Command));
      end loop;
   end Dump;

end Smk.Makefiles;
