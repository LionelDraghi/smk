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

with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;                use Ada.Text_IO;

with Smk.IO;

-- -----------------------------------------------------------------------------
package body Smk.Smkfiles is

   Debug  : constant Boolean := False;
   Prefix : constant String  := "";

   use Ada.Strings;
   use Ada.Strings.Fixed;

   Current_Section : Runfiles.Section_Names := Runfiles.Default_Section;

   use Ada.Strings.Maps;
   use Ada.Strings.Maps.Constants;
   Whitespace_Set : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set
       (Ada.Characters.Latin_1.HT & Ada.Characters.Latin_1.Space & '@');
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
      -- A section line starts with an identifier immediatly followed by
      -- a semicolon, e.g. "mrproper:"
      -- Warning: this function updates the global Current_Section variable
      First : Positive;
      Last  : Natural;
   begin
      Find_Token (Source => Line,
                  Set    => Identifier_Set,
                  Test   => Inside,
                  First  => First,
                  Last   => Last);
      if Last = 0 then
         -- The line don't start with an identifier, can't be a section
         return False;

      elsif Last = Line'Last then
         -- There is only an identifier on this line
         return False;

      else
         -- IO.Put_Debug_Line ("""" & Line (First .. Last)
         --                    & """", Debug, Prefix);
         if Line (Last + 1) = ':' then
            Current_Section := +(Line (First .. Last));
            return True;
         else
            return False;
         end if;
      end if;
   end Is_A_Section;

  -- --------------------------------------------------------------------------
   procedure Analyze (Smkfile_Name : in     String;
                      Line_List    :    out Smkfile)
   is
      Make_Fl        : Ada.Text_IO.File_Type;
      Entry_List     : Smkfile_Entry_Lists.List;
      In_A_Multiline : Boolean := False;
      Multiline      : Command_Lines := Null_Command_Line;

   begin
      Open (Make_Fl, Mode => In_File, Name => Smkfile_Name);

      Analysis : while not End_Of_File (Make_Fl) loop
         declare
            -- Line = Get_Line, but heading blanks or tabs character are removed
            -- If there is not heading blank, First_Non_Blank will be null,
            -- and Line will start at 1.
            Line : constant String := Trim (Get_Line (Make_Fl),
                                            Left  => Whitespace_Set,
                                            Right => Whitespace_Set);
            Line_Nb : constant Integer := Integer (Ada.Text_IO.Line (Make_Fl));

         begin
            if Is_A_Comment (Line) then
               IO.Put_Debug_Line (Line & "<",
                                  Debug  => Debug,
                                  Prefix => Prefix & "Comment    >",
                                  File   => Smkfile_Name,
                                  Line   => Line_Nb);

            elsif Is_Empty (Line) then
               IO.Put_Debug_Line (Line & "<",
                                  Debug  => Debug,
                                  Prefix => Prefix & "Empty line >",
                                  File   => Smkfile_Name,
                                  Line   => Line_Nb);

            elsif Is_A_Section (Line) then
               IO.Put_Debug_Line (+Current_Section & "<",
                                  Debug  => Debug,
                                  Prefix => Prefix & "Section    >",
                                  File   => Smkfile_Name,
                                  Line   => Line_Nb);

            else
               -- Last but not least, it's a command line.
               if Line (Line'Last) = '\' then
                  -- first or continuation line of the multiline
                  declare
                     Last_Non_Blank_Before_Backslash  : constant Natural
                       := Index (Source => Line (Line'First .. Line'Last - 1),
                                 Set    => Whitespace_Set,
                                 Test   => Outside,
                                 Going  => Backward);
                  begin
                     -- we replace all all blanks and tab befor '\' with
                     -- a single blank
                     Multiline := Multiline
                       & Line (Line'First .. Last_Non_Blank_Before_Backslash)
                       & " ";
                     In_A_Multiline := True;
                     IO.Put_Debug_Line
                       (To_String (Multiline) & "<",
                        Debug  => Debug,
                        Prefix => Prefix & "First or continuation    >",
                        File   => Smkfile_Name,
                        Line   => Line_Nb);
                  end;

               else
                  if In_A_Multiline then
                     -- last line of the multiline
                     Multiline := Multiline & Line;
                     Entry_List.Append ((Line    => Line_Nb - 1, -- why -1 ???
                                         Section => Current_Section,
                                         Command => Multiline,
                                         Was_Run => False));
                     IO.Put_Debug_Line (To_String (Multiline) & "<",
                                        Debug  => Debug,
                                        Prefix => Prefix & "Last line >",
                                        File   => Smkfile_Name,
                                        Line   => Line_Nb);
                     Multiline := Null_Command_Line;
                     In_A_Multiline := False;

                  else
                     -- single line command
                     Entry_List.Append ((Line    => Line_Nb - 1, -- why -1 ???
                                         Section => Current_Section,
                                         Command => +(Line),
                                         Was_Run => False));
                     IO.Put_Debug_Line (Line & "<",
                                        Debug  => Debug,
                                        Prefix => Prefix & "Single line >",
                                        File   => Smkfile_Name,
                                        Line   => Line_Nb);
                  end if;

               end if;

            end if;

         end;

      end loop Analysis;

      if In_A_Multiline then
         IO.Put_Error (Smkfile_Name
                       & " ends with incomplete multine, last command ignored");
      end if;

      Close (Make_Fl);

      declare
         use Ada.Directories;
      begin
         Line_List := (Name     => +Smkfile_Name,
                       Time_Tag => Modification_Time (Smkfile_Name),
                       Entries  => Entry_List);
      end;

   end Analyze;

   -- --------------------------------------------------------------------------
   procedure Dump (The_Smkfile : in Smkfile) is
      Time_Tag : constant String := IO.Image (The_Smkfile.Time_Tag);

   begin
      IO.Put_Line (+The_Smkfile.Name & " (" & Time_Tag & ") :");

      for E of The_Smkfile.Entries loop
         IO.Put_Line (Trim (Positive'Image (E.Line), Left)
                        & ": [" & (+E.Section) & "] " & (+E.Command));
      end loop;
   end Dump;

   -- --------------------------------------------------------------------------
   function Contains (The_Smkfile : in Smkfile;
                      The_Command : in Command_Lines)
                      return Boolean is
   begin
      for E of The_Smkfile.Entries loop
         if E.Command = The_Command then
         -- IO.Put_Line (">" & (+E.Command) & "< =  >" & (+The_Command) & "<");
            return True;
         -- else
         -- IO.Put_Line (">" & (+E.Command) & "< /= >" & (+The_Command) & "<");
         end if;
      end loop;
      return False;
   end Contains;


end Smk.Smkfiles;
