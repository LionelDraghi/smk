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
   type File_Status is (Created,
                        Identical,
                        Updated,
                        Missing,
                        Unknown) with Default_Value => Unknown;
   Status_Image : constant array (File_Status) of String (1 .. 7) :=
                    (Created   => "Created",
                     Identical => "Identic", -- Identical
                     Updated   => "Updated",
                     Missing   => "Missing",
                     Unknown   => "Unknown");
   Short_Status_Image : constant array (File_Status) of Character :=
                          (Created   => 'C',
                           Identical => '=',
                           Updated   => 'U',
                           Missing   => 'M',
                           Unknown   => '?');

   -- --------------------------------------------------------------------------
   type File_Role is (Source,
                      Target,
                      Both,
                      Unused) with Default_Value => Unused;
   Role_Image : constant array (File_Role) of String (1 .. 6) :=
                  (Source => "Source",
                   Target => "Target",
                   Both   => "Both  ",
                   Unused => "Unused");

   -- --------------------------------------------------------------------------
   type File_Name is private;
   function "+" (Name : File_Name) return String;
   function "+" (Name : String)    return File_Name;
   function "<" (Left, Right : File_Name) return Boolean;

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
   -- procedure Set_Role   (File : in out File_Type; Role   : File_Role);
   procedure Set_Status (File : in out File_Type; Status : File_Status);
   procedure Set_Source (File : in out File_Type);
   procedure Set_Target (File : in out File_Type);

   -- --------------------------------------------------------------------------
   function Time_Tag  (File : File_Type) return Ada.Calendar.Time;
   function Is_Dir    (File : File_Type) return Boolean;
   function Is_System (File : File_Type) return Boolean;
   -- Is_System returns True if the File_Name starts with "/usr/, "/lib/", etc.
   function Role      (File : File_Type) return File_Role;
   function Status    (File : File_Type) return File_Status;
   function Is_Source (File : File_Type) return Boolean;
   function Is_Target (File : File_Type) return Boolean;

   -- --------------------------------------------------------------------------
   procedure Put_File_Description (Name   : File_Name;
                                   File   : File_Type;
                                   Prefix : String := "");
   -- Print a one line File description,
   -- with a format variable according to Long_Listing_Format setting

   -- --------------------------------------------------------------------------
   procedure Dump_File_Description (Name : File_Name;
                                    File : File_Type);
   -- Print all about the File, no filtering

   -- --------------------------------------------------------------------------
   function Is_Dir (File_Name : in String) return Boolean;

   -- --------------------------------------------------------------------------
   procedure Update_File_Status (Name            : in     File_Name;
                                 File            : in out File_Type;
                                 Previous_Status :    out File_Status;
                                 Current_Status  :    out File_Status);

private
   type File_Type is record
      Time_Tag  : Ada.Calendar.Time;
      Is_System : Boolean;
      Is_Dir    : Boolean;
      Is_Source : Boolean;
      Is_Target : Boolean;
      Status    : File_Status;
   end record;

   type File_Name is new Unbounded_String;

end Smk.Files;
