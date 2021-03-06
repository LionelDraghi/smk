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

with File_Utilities;
with Smk.IO;
with Smk.Settings;

with Ada.Calendar;    use Ada.Calendar;
with Ada.Directories;
with Ada.Strings.Fixed;

package body Smk.Files is

   -- --------------------------------------------------------------------------
   function "+" (Name : File_Name) return String is
     (To_String (Name));
   function "+" (Name : String) return File_Name is
     (File_Name'(To_Unbounded_String (Name)));

   -- --------------------------------------------------------------------------
   function Shorten (Name : String)    return String is
     (if Settings.Shorten_File_Names
      then File_Utilities.Short_Path (From_Dir => Settings.Initial_Directory,
                                      To_File  => Name)
      else Name);
   function Shorten (Name : File_Name) return String is
          (Shorten (+Name));

   -- --------------------------------------------------------------------------
   function Modification_Time (File_Name : String) return Time is
      use Ada.Directories;
   begin
      if Exists (File_Name) and then Kind (File_Name) /= Special_File
      -- Trying to get Modification_Time of /dev/urandom
      -- raise Name_Error
      then
         return Ada.Directories.Modification_Time (File_Name);
      else
         return Clock;
      end if;
   end Modification_Time;

   -- --------------------------------------------------------------------------
   function Create (File : File_Name;
                    Role : File_Role) return File_Type is
      ((Time_Tag  => Modification_Time (+File),
        Is_Dir    => Is_Dir (+File),
        Is_System => Settings.Is_System (+File),
        Is_Source => Role = Source, 
        Is_Target => Role = Target, 
        Status    => New_File));

   -- --------------------------------------------------------------------------
   function Time_Tag  (File : File_Type) return Time        is (File.Time_Tag);
   function Is_Dir    (File : File_Type) return Boolean     is (File.Is_Dir);
   function Is_System (File : File_Type) return Boolean     is (File.Is_System);
   function Role      (File : File_Type) return File_Role   is
     (if       Is_Source (File) then Source
      else (if Is_Target (File) then Target
            else                     Unused));
   function Status    (File : File_Type) return File_Status is (File.Status);
   function Is_Source (File : File_Type) return Boolean     is (File.Is_Source);
   function Is_Target (File : File_Type) return Boolean     is (File.Is_Target);

   -- -----------------------------------------------------------------------
   function Has_Target (Name   : File_Name;
                        Target : String) return Boolean is
   begin
      if Target = "" then
         return False;

      elsif Ada.Strings.Fixed.Tail (+Name, Target'Length) = Target then
         IO.Put_Line ("File " & (+Name) & " match Target "
                      & Target & ".", Level => IO.Verbose);
         return True;
      else
         return False;
      end if;
   end Has_Target;

   -- --------------------------------------------------------------------------
   function File_Image (Name   : File_Name;
                        File   : File_Type;
                        Prefix : String := "") return String is
      Left : constant String :=
               (if Settings.Long_Listing_Format
                then Prefix
                & (if Is_Dir (File)    then "[Dir] "     else "[Fil] ")
                & (if Is_System (File) then  "[System] " else "[Normal] ")
                & "[" & Role_Image (Role (File))   & "] "
                & "[" & Status_Image (File.Status) & "] "
                & "[" & IO.Image (File.Time_Tag)   & "] "
                else Prefix);
   begin
      return Left & Shorten (Name);
   end File_Image;

   -- --------------------------------------------------------------------------
   function Is_Dir (File_Name : in String) return Boolean is
      use Ada.Directories;
   begin
      return Exists (File_Name) and then Kind (File_Name) = Directory;
   end Is_Dir;

   -- --------------------------------------------------------------------------
   procedure Update_File_Status (Name            : in     File_Name;
                                 File            : in out File_Type;
                                 Previous_Status :    out File_Status;
                                 Current_Status  :    out File_Status)
   is
      Fl_Name : constant String := +Name;
      use Ada.Directories;
   begin
      Previous_Status := File.Status;

      if not Exists (Fl_Name) then
         File.Status := Missing;

      elsif Modification_Time (Fl_Name) > File.Time_Tag then
         File.Status := Updated;

      else
         File.Status := Identical;

      end if;

      Current_Status := File.Status;

      IO.Put_Line (Item => "[" & Short_Status_Image (Previous_Status) & " -> "
                   & Short_Status_Image (Current_Status) & "] "
                   & (+Name),
                   Level => IO.Debug);

   end Update_File_Status;

end Smk.Files;
