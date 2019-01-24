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

with Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

private package Smk.Files is

   -- --------------------------------------------------------------------------
   -- Purpose:
   --   This package defines a File and related operations

   -- --------------------------------------------------------------------------
   type File_Status is (New_File,
                        Identical,
                        Updated,
                        Missing,
                        Unknown) with Default_Value => Unknown;
   Status_Image : constant array (File_Status) of String (1 .. 7) :=
                    (New_File  => "New    ",
                     Identical => "Identic", -- Identical
                     Updated   => "Updated",
                     Missing   => "Missing",
                     Unknown   => "Unknown");
   Short_Status_Image : constant array (File_Status) of Character :=
                          (New_File  => 'N',
                           Identical => '=',
                           Updated   => 'U',
                           Missing   => 'M',
                           Unknown   => '?');

   -- --------------------------------------------------------------------------
   type File_Role is (Source,
                      Target,
                      Unused) with Default_Value => Unused;
   Role_Image : constant array (File_Role) of String (1 .. 6) :=
                  (Source => "Source",
                   Target => "Target",
                   Unused => "Unused");

   -- --------------------------------------------------------------------------
   type File_Name is new Unbounded_String;
   function "+" (Name : File_Name) return String;
   function "+" (Name : String)    return File_Name;
   No_File : constant File_Name
     := File_Name (Ada.Strings.Unbounded.Null_Unbounded_String);

   -- --------------------------------------------------------------------------
   function Shorten (Name : String)    return String;
   function Shorten (Name : File_Name) return String;
   -- returns Name if Settings.Shorten_File_Names = False,
   -- or a short path from current dir the the file.
   -- NB : Name must be a Full_Name (a rooted path)

   -- --------------------------------------------------------------------------
   type File_Type is private;

   -- --------------------------------------------------------------------------
   function Create (File : File_Name;
                    Role : File_Role) return File_Type;

   -- --------------------------------------------------------------------------
   function Time_Tag  (File : File_Type) return Ada.Calendar.Time;
   function Is_Dir    (File : File_Type) return Boolean;
   function Is_System (File : File_Type) return Boolean;
   -- Is_System returns True if the File_Name starts with "/usr/, "/lib/", etc.
   function Role      (File : File_Type) return File_Role
     with Pre => not (Is_Source (File) and Is_Target (File)); -- can't be both
   function Status    (File : File_Type) return File_Status;
   function Is_Source (File : File_Type) return Boolean;
   function Is_Target (File : File_Type) return Boolean;
   function Is_Unused (File : File_Type) return Boolean is
     (not Is_Source (File) and not Is_Target (File));

   -- -----------------------------------------------------------------------
   function Has_Target (Name   : File_Name;
                        Target : String) return Boolean;
   -- File is a Full_Name
   -- return True if Target match the right part of File

   -- --------------------------------------------------------------------------
   function File_Image (Name   : File_Name;
                        File   : File_Type;
                        Prefix : String := "") return String;
   -- Return a string according to Long_Listing_Format setting
   -- if False: Fixme:
   -- if True: Fixme:

   -- --------------------------------------------------------------------------
   function Is_Dir (File_Name : in String) return Boolean;

   -- --------------------------------------------------------------------------
   procedure Update_File_Status (Name            : in     File_Name;
                                 File            : in out File_Type;
                                 Previous_Status :    out File_Status;
                                 Current_Status  :    out File_Status);

private
   -- --------------------------------------------------------------------------
   type File_Type is record
      Time_Tag  : Ada.Calendar.Time;
      Is_System : Boolean;
      Is_Dir    : Boolean;
      Is_Source : Boolean;
      Is_Target : Boolean;
      Status    : File_Status;
   end record;

end Smk.Files;
