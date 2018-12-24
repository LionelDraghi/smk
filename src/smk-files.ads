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

with Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

private package Smk.Files is

   -- Purpose:
   --   This package defines a File and related operations

   -- --------------------------------------------------------------------------
   type File_Status is (Created,
                        Identical,
                        Updated,
                        Missing,
                        Unknown) with Default_Value => Unknown;
   Short_Image : array (File_Status) of Character :=
                   (Created   => 'C',
                    Identical => '=',
                    Updated   => 'U',
                    Missing   => 'M',
                    Unknown   => '?');

   -- --------------------------------------------------------------------------
   type File_Name is private;
   function "+" (Name : File_Name) return String;
   function "+" (Name : String)    return File_Name;
   function "<" (Left, Right : File_Name) return Boolean;

   -- --------------------------------------------------------------------------
   type File_Type is private;

   -- --------------------------------------------------------------------------
   function Create (File : File_Name) return File_Type;

   -- --------------------------------------------------------------------------
   function Time_Tag  (File : File_Type) return Ada.Calendar.Time;
   function Is_System (File : File_Type) return Boolean;
   -- Is_System returns True if the File_Name starts with "/usr/, "/lib/", etc.
   function Is_Dir    (File : File_Type) return Boolean;
   function Status    (File : File_Type) return File_Status;

   -- --------------------------------------------------------------------------
   function Is_System      (File_Name : in String) return Boolean;

   -- --------------------------------------------------------------------------
   function In_Ignore_List (File_Name : in String) return Boolean;
   -- Return True for files like /etc/ld.so.cache that are updated
   -- on each execution

   -- --------------------------------------------------------------------------
   procedure Update_File_Status (Name            : in     File_Name;
                                 File            : in out File_Type;
                                 Previous_Status :    out File_Status;
                                 Current_Status  :    out File_Status);

private
   type File_Type is record
      Time_Tag  : Ada.Calendar.Time; -- := Ada.Calendar.Clock;
      Is_System : Boolean;           -- := False;
      Is_Dir    : Boolean;           -- := False;
      Status    : File_Status;       -- := Unknown;
   end record;

   type File_Name is new Unbounded_String;

end Smk.Files;
