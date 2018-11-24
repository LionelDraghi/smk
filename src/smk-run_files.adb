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

with Smk.IO;
with Smk.Settings;

package body Smk.Run_Files is

   Run_Fl : Ada.Streams.Stream_IO.File_Type;

   -- --------------------------------------------------------------------------
   function "+" (Name : File_Name) return String is
     (To_String (Name));
   function "+" (Name : String) return File_Name is
     (File_Name'(To_Unbounded_String (Name)));
   function "+" (Name : File_Name) return Unbounded_String is
     (Unbounded_String (Name));

   -- --------------------------------------------------------------------------
   procedure Dump (File_List          : in File_Lists.Map;
                   Filter_Sytem_Files : in Boolean) is
      use Run_Files.File_Lists;
   begin
      for C in File_List.Iterate loop
         declare
            Name : constant String := +Key (C);
            TT   : constant Ada.Calendar.Time := Element (C);
         begin
            if not Filter_Sytem_Files or
              (Name (1 .. 4) /= "/usr"
               and Name (1 .. 4) /= "/lib"
               and Name (1 .. 4) /= "/opt"
               and Name (1 .. 4) /= "/etc")
            then
               IO.Put_Line ("  - " & IO.Image (TT) & ":" & Name);
            end if;
         end;
      end loop;
   end Dump;

   -- --------------------------------------------------------------------------
   procedure Update_Time_Tag (File_List : in out File_Lists.Map) is
      use Ada.Directories;
      use File_Lists;
   begin
      for C in File_List.Iterate loop
         declare
            Name : constant String := +Key (C);
         begin
            if Exists (Name) then
               File_List.Replace_Element
                 (C, Ada.Directories.Modification_Time (Name));
               -- Fixme : GNAT bug on this line:
               -- Element (C) := Ada.Directories.Modification_Time (Name);
            end if;
         end;
      end loop;
   end Update_Time_Tag;

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
   procedure Dump (Run_List           : in Run_Lists.Map;
                   Filter_Sytem_Files : in Boolean) is
      use Run_Lists;
   begin
      for L in Run_List.Iterate loop
         IO.Put_Line (IO.Image (Element (L).Run_Time)
                      & " [" & (+Element (L).Section) & "] " & (+Key (L)));

         IO.Put_Line ("  Sources:");
         Dump (Element (L).Sources, Filter_Sytem_Files);

         IO.Put_Line ("  Targets:");
         Dump (Element (L).Targets, Filter_Sytem_Files);

         IO.Put_Line ("");

      end loop;
   end Dump;

   -- --------------------------------------------------------------------------
   function Saved_Run_Found return Boolean is
   begin
      return Ada.Directories.Exists (Settings.Previous_Run_File_Name);
   end Saved_Run_Found;

   -- --------------------------------------------------------------------------
   function Get_Saved_Run return Run_Lists.Map is
      use Ada.Streams.Stream_IO;
      List : Run_Lists.Map;
      S    : Stream_Access;
   begin
      Open (Name => Settings.Previous_Run_File_Name,
            File => Run_Fl,
            Mode => In_File);
      S := Stream (Run_Fl);
      List := Run_Lists.Map'Input (S);
      Close (Run_Fl);
      return List;
   end Get_Saved_Run;

   -- --------------------------------------------------------------------------
   procedure Save_Run (The_Run : in Run_Lists.Map) is
      use Ada.Streams.Stream_IO;
      S : Stream_Access;
   begin
      Create (Name => Settings.Previous_Run_File_Name,
              File => Run_Fl,
              Mode => Out_File);
      S := Stream (Run_Fl);
      Run_Lists.Map'Output (S, The_Run);
      Close (Run_Fl);
   end Save_Run;

end Smk.Run_Files;
