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
-- Package: Smk.Makefiles specification
--
-- Purpose:
--   This package manages the Makefile data structure, and provided related
--   analysis and dump services.
--
-- Effects:
--
-- Performance:
--
-- -----------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;              use Ada.Strings.Unbounded;

with Smk.Run_Files;                      use Smk.Run_Files;

private package Smk.Makefiles is

   -- --------------------------------------------------------------------------
   type Makefile_Entry is record
      Line        : Positive;
      Section     : Section_Names := Default_Section;
      Command     : Command_Lines := Command_Lines (Null_Unbounded_String);
      Already_Run : Boolean       := False;
      -- used to avoid running the same command for a different reason
      -- during the analysis.
   end record;
   package Makefile_Entry_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Makefile_Entry, "=");

   -- --------------------------------------------------------------------------
   type Makefile is record
      Name     : File_Name;
      Time_Tag : Ada.Calendar.Time;
      Entries  : Makefile_Entry_Lists.List;
   end record;

   -- --------------------------------------------------------------------------
   procedure Analyze (Makefile_Name : in     String;
                      Line_List     :    out Makefile);
   -- Analyze the Makefile provided on command line.

   -- --------------------------------------------------------------------------
   procedure Dump (The_Make_File : in Makefile);
   -- Dump Smk undestanding of a Makefile, only the useful part of the
   -- Current Makefile, that is without any comment or blank line.

end Smk.Makefiles;
