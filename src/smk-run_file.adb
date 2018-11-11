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
-- Package: Smk.Run_File body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
-- -----------------------------------------------------------------------------

with Ada.Directories;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Smk.Settings;

package body Smk.Run_File is

   Run_Fl : Ada.Text_IO.File_Type;
   Run_Fl_Name : Unbounded_String := Null_Unbounded_String;
   Run_Fl_Exists : Boolean := True;

   -- --------------------------------------------------------------------------
   -- Procedure: Initialize
   -- --------------------------------------------------------------------------
   procedure Initialize is
      use Ada.Text_IO;
   begin
      Run_Fl_Name := To_Unbounded_String (".smk_" & Settings.Makefile_Name);
      Run_Fl_Exists := Ada.Directories.Exists (To_String (Run_Fl_Name));

      if Run_Fl_Exists then
         Open (Name => To_String (Run_Fl_Name),
               File => Run_Fl,
               Mode => In_File);
      else
         Create (Run_Fl, Out_File, To_String (Run_Fl_Name));
      end if;
   end Initialize;

   -- --------------------------------------------------------------------------
   -- Function: First_Run
   -- --------------------------------------------------------------------------
   function First_Run return Boolean is
   begin
      return not Run_Fl_Exists;
   end First_Run;

   -- --------------------------------------------------------------------------
   -- Procedure: Run_All
   -- --------------------------------------------------------------------------
   procedure Run_All is
   begin
      null;
   end Run_All;

   -- --------------------------------------------------------------------------
   -- Procedure: Update_Run_File
   -- --------------------------------------------------------------------------
   procedure Update_Run_File is
   begin
      null;
   end Update_Run_File;

end Smk.Run_File;
