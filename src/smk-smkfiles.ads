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

with Smk.Definitions; use Smk.Definitions;
with Smk.Files;

with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;

private package Smk.Smkfiles is

   -- --------------------------------------------------------------------------
   -- Purpose:
   --   This package defines and manages the Smkfile (equivalent of the
   --   Makefile for make) data structure, and provided related services.

   -- --------------------------------------------------------------------------
   type Smkfile_Entry is record
      Line    : Positive;
      Section : Section_Names := Default_Section;
      Command : Command_Lines := Null_Command_Line;
      Was_Run : Boolean       := False;
      -- used to avoid running the same command for a different reason
      -- during the analysis.
   end record;
   package Smkfile_Entry_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Smkfile_Entry, "=");

   type Smk_File_Name is new Files.File_Name;
   function "+" (Name : Smk_File_Name) return String;
   function "+" (Name : String)        return Smk_File_Name;

   -- --------------------------------------------------------------------------
   type Smkfile is record
      Name     : Smk_File_Name;
      Time_Tag : Ada.Calendar.Time;
      Entries  : Smkfile_Entry_Lists.List;
   end record;

   -- --------------------------------------------------------------------------
   function Load_Smkfile return Smkfile;
   -- Load the Makefile provided on command line.

   -- --------------------------------------------------------------------------
   procedure Dump;
   -- Dump Smk understanding of a Makefile, only the useful part of the
   -- Current Makefile, that is without any comment or blank line.

   -- --------------------------------------------------------------------------
   function Contains (The_Smkfile : in Smkfile;
                      The_Command : in Command_Lines)
                      return Boolean;

   -- --------------------------------------------------------------------------
   procedure Add_To_Smkfile (Cmd_Line : String);

end Smk.Smkfiles;
