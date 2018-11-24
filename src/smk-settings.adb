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
-- Package: Smk.Settings body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
-- -----------------------------------------------------------------------------

with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Smk.Settings is

   Make_Fl_Name         : Unbounded_String := Null_Unbounded_String;
   Previous_Run_Fl_Name : Unbounded_String := Null_Unbounded_String;

   -- --------------------------------------------------------------------------
   -- Procedure: Set_Makefile_Name
   -- --------------------------------------------------------------------------
   procedure Set_Makefile_Name (Name : in String) is
   begin
      Make_Fl_Name         := To_Unbounded_String (Name);
      Previous_Run_Fl_Name := To_Unbounded_String
        (Smk_File_Prefix & Ada.Directories.Simple_Name (Makefile_Name));
   end Set_Makefile_Name;

   -- --------------------------------------------------------------------------
   -- Function: Makefile_Name
   -- --------------------------------------------------------------------------
   function Makefile_Name          return String is
     (To_String (Make_Fl_Name));
   function Previous_Run_File_Name return String is
     (To_String (Previous_Run_Fl_Name));
   function Run_Dir_Name           return String is
     (Ada.Directories.Containing_Directory (Makefile_Name));
   function Strace_Outfile_Name   return String is
     (Ada.Directories.Full_Name (Smk_File_Prefix & Strace_Outfile_Suffix));

end Smk.Settings;
