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

with Smk.IO;

with Ada.Directories;

package body Smk.Files is

   -- --------------------------------------------------------------------------
   function "+" (Name : File_Name) return String is
     (To_String (Name));
   function "+" (Name : String) return File_Name is
     (File_Name'(To_Unbounded_String (Name)));
   function "<" (Left, Right : File_Name) return Boolean is
     ("<" (+Left, +Right));

   -- --------------------------------------------------------------------------
   function Is_System (File_Name : in String) return Boolean is
   begin
      return File_Name'Length > 5 and then
        (File_Name (File_Name'First .. File_Name'First + 4) = "/usr/"
         or else File_Name (File_Name'First .. File_Name'First + 4) = "/lib/"
         or else File_Name (File_Name'First .. File_Name'First + 4) = "/opt/"
         or else File_Name (File_Name'First .. File_Name'First + 4) = "/etc/"
         or else File_Name (File_Name'First .. File_Name'First + 5) = "/proc/"
         or else File_Name (File_Name'First .. File_Name'First + 4) = "/sys/");
   end Is_System;

   -- --------------------------------------------------------------------------
   function In_Ignore_List (File_Name : in String) return Boolean is
   begin
      return File_Name = "/etc/ld.so.cache";
   end In_Ignore_List;

   -- --------------------------------------------------------------------------
   function Is_Dir (File : in File_Name) return Boolean is
      use Ada.Directories;
   begin
      return Exists (+File) and then Kind (+File) = Directory;
   end Is_Dir;

   -- --------------------------------------------------------------------------
   function Create (File : File_Name) return File_Type is
      ((Time_Tag  => Ada.Calendar.Clock,
        Is_System => Is_System (+File),
        Is_Dir    => Is_Dir (File),
        Status    => Created));

   -- --------------------------------------------------------------------------
   function Time_Tag  (File : File_Type) return Ada.Calendar.Time is
      (File.Time_Tag);
   function Is_System (File : File_Type) return Boolean     is (File.Is_System);
   function Is_Dir    (File : File_Type) return Boolean     is (File.Is_Dir);
   function Status    (File : File_Type) return File_Status is (File.Status);

   -- --------------------------------------------------------------------------
   procedure Update_File_Status (Name            : in     File_Name;
                                 File            : in out File_Type;
                                 Previous_Status :    out File_Status;
                                 Current_Status  :    out File_Status)
   is
      Fl_Name : constant String := +Name;
      use type Ada.Calendar.Time;
   begin
      Previous_Status := File.Status;

      if not Ada.Directories.Exists (Fl_Name) then
         File.Status := Missing;

      elsif Ada.Directories.Modification_Time (Fl_Name) > File.Time_Tag then
         File.Status := Updated;

      else
         File.Status := Identical;

      end if;

      Current_Status := File.Status;

      IO.Put_Line (Item => "[" & Short_Image (Previous_Status) & " -> "
                   & Short_Image (Current_Status) & "] " & (+Name),
                   Level => IO.Debug);

   end Update_File_Status;

   -- --------------------------------------------------------------------------
   --     procedure Update_Status (File_List : in out File_Lists.Map) is
   --        use File_Lists;
   --     begin
   --        for F in File_List.Iterate loop
   --           Reference (File_List, F).Status := Missing;
   --        end loop;
   --     end Update_Status;

end Smk.Files;
