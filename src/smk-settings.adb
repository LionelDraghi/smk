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

with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Smk.Settings is

   -- --------------------------------------------------------------------------
   -- Most of the variable here are "write once, read more".
   -- To avoid the cost of Unbounded strings manipulation,
   -- they are implemented as access to String
   Smkfl_Name      : access String := null;
   Runfl_Name      : access String := null;
   Current_Section : access String := null;
   Current_Target  : access String := null;

   Cmd_Line        : Unbounded_String := Null_Unbounded_String;

   -- --------------------------------------------------------------------------
   procedure Set_Smkfile_Name (Name : in String) is
   begin
      Smkfl_Name := new String'(Name);
      Runfl_Name := new String'(To_Runfile_Name (Name));
   end Set_Smkfile_Name;

   function Smkfile_Name return String is
     (if Smkfl_Name = null then "" else Smkfl_Name.all);

   function Is_Smkfile_Name_Set return Boolean is
     (Smkfl_Name /= null);

   function Run_Dir_Name return String is
     (Ada.Directories.Containing_Directory (Smkfile_Name));

   function Strace_Outfile_Name return String is
     (Strace_Outfile_Prefix & Ada.Directories.Simple_Name (Smkfile_Name)
      & Strace_Outfile_Suffix);

   -- --------------------------------------------------------------------------
   procedure Set_Runfile_Name (Name : in String) is
   begin
      Runfl_Name := new String'(Name);
   end Set_Runfile_Name;

   function Runfile_Name return String is
     (if Runfl_Name = null then "" else Runfl_Name.all);

   function To_Runfile_Name (Smkfile_Name : in String) return String is
     (Smk_File_Prefix & Ada.Directories.Simple_Name (Smkfile_Name));

   -- --------------------------------------------------------------------------
   procedure Set_Section_Name (Name : in String) is
   begin
      Current_Section := new String'(Name);
   end Set_Section_Name;

   function Section_Name return String is
     (if Current_Section = null then "" else Current_Section.all);

   -- --------------------------------------------------------------------------
   procedure Add_To_Command_Line (Text : in String) is
   begin
      if Cmd_Line = "" then
         Cmd_Line := To_Unbounded_String (Text);
      else
         Cmd_Line := Cmd_Line & " " & Text;
      end if;
   end Add_To_Command_Line;

   function Command_Line return String is (To_String (Cmd_Line));

   -- --------------------------------------------------------------------------
   procedure Set_Target_Name (Target : in String) is
   begin
      Current_Target := new String'(Target);
   end Set_Target_Name;

   function Target_Name return String is
     (if Current_Target = null then "" else Current_Target.all);

end Smk.Settings;
