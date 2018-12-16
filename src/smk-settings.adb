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

   Smkfl_Name : Unbounded_String := Null_Unbounded_String;
   Runfl_Name : Unbounded_String := Null_Unbounded_String;

   -- --------------------------------------------------------------------------
   procedure Set_Smkfile_Name (Name : in String) is
   begin
      Smkfl_Name := To_Unbounded_String (Name);
      Runfl_Name := To_Unbounded_String (To_Runfile_Name (Name));
   end Set_Smkfile_Name;

   -- --------------------------------------------------------------------------
   procedure Set_Runfile_Name (Name : in String) is
   begin
      Runfl_Name := To_Unbounded_String (Name);
   end Set_Runfile_Name;

   -- --------------------------------------------------------------------------
   function To_Runfile_Name (Smkfile_Name : in String) return String is
      (Smk_File_Prefix & Ada.Directories.Simple_Name (Smkfile_Name));

   -- --------------------------------------------------------------------------
   function Smkfile_Name return String is (To_String (Smkfl_Name));
   function Runfile_Name return String is (To_String (Runfl_Name));
   function Run_Dir_Name return String is
     (Ada.Directories.Containing_Directory (Smkfile_Name));
   function Strace_Outfile_Name return String is
     (Strace_Outfile_Prefix & Ada.Directories.Simple_Name (Smkfile_Name)
      & Strace_Outfile_Suffix);

   -- --------------------------------------------------------------------------
   function Is_System_File (File_Name : in String) return Boolean is
   begin
      return    File_Name (File_Name'First .. File_Name'First + 4) = "/usr/"
        or else File_Name (File_Name'First .. File_Name'First + 4) = "/lib/"
        or else File_Name (File_Name'First .. File_Name'First + 4) = "/opt/"
        or else File_Name (File_Name'First .. File_Name'First + 4) = "/etc/"
        or else File_Name (File_Name'First .. File_Name'First + 5) = "/proc/"
        or else File_Name (File_Name'First .. File_Name'First + 4) = "/sys/";
   end Is_System_File;

end Smk.Settings;
