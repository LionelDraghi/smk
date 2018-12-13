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

with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;              use Ada.Strings.Unbounded;

with Smk.Runfiles;                       use Smk.Runfiles;

private package Smk.Smkfiles is

   -- --------------------------------------------------------------------------
   -- Purpose:
   --   This package defines and manages the Smkfile (equivalent of the
   --   Makefile for make) data structure, and provided related services.

   -- --------------------------------------------------------------------------
   type Smkfile_Entry is record
      Line    : Positive;
      Section : Section_Names := Default_Section;
      Command : Command_Lines := Command_Lines (Null_Unbounded_String);
      Was_Run : Boolean       := False;
      -- used to avoid running the same command for a different reason
      -- during the analysis.
   end record;
   package Smkfile_Entry_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Smkfile_Entry, "=");

   -- --------------------------------------------------------------------------
   type Smkfile is record
      Name     : File_Name;
      Time_Tag : Ada.Calendar.Time;
      Entries  : Smkfile_Entry_Lists.List;
   end record;

   -- --------------------------------------------------------------------------
   procedure Analyze (Smkfile_Name : in     String;
                      Line_List    :    out Smkfile);
   -- Analyze the Makefile provided on command line.

   -- --------------------------------------------------------------------------
   procedure Dump (The_Smkfile : in Smkfile);
   -- Dump Smk understanding of a Makefile, only the useful part of the
   -- Current Makefile, that is without any comment or blank line.

end Smk.Smkfiles;
