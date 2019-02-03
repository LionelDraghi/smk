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

   WD              : constant access String
     := new String'(Ada.Directories.Current_Directory);

   Ignored         : constant Filter_List := (new String'("/sys/*"),
                                              new String'("/proc/*"),
                                              new String'("/dev/*"),
                                              new String'("/tmp/*"),
                                              new String'("/etc/ld.so.cache"));
   System_Dir      : constant Filter_List := (new String'("/usr/*"),
                                              new String'("/lib/*"),
                                              new String'("/etc/*"),
                                              new String'("/opt/*"));

   -- --------------------------------------------------------------------------
   function Is_File_In (File, Dir : String) return Boolean is
      Compared_Length : constant Natural := (if Dir (Dir'Last) = '*'
                                             then Dir'Length - 1
                                             else Dir'Length);
      -- return True if File is in Dir, supposing that both are full name.
      -- e.g. (Dir => /usr/*, File => /usr/lib/locale) return True
      -- e.g. (Dir => /usr/*, File => locale)          return False
   begin
      return (File'Length >= Compared_Length and then
              File (File'First .. File'First - 1 + Compared_Length)
              = Dir (Dir'First .. Dir'First  - 1 + Compared_Length));
   end Is_File_In;

   -- --------------------------------------------------------------------------
   function In_Ignore_List (File_Name : in String) return Boolean is
   begin
      for D of Ignored loop
         if Is_File_In (File => File_Name, Dir => D.all) then
            return True;
         end if;
      end loop;

      if File_Name = Settings.Smkfile_Name
        or else File_Name = Settings.Runfile_Name
      then
         return True;
      end if;

      return False;
   end In_Ignore_List;

   -- --------------------------------------------------------------------------
   function Is_System (File_Name : in String) return Boolean is
     (for some D of System_Dir =>
         Is_File_In (File => File_Name, Dir => D.all));

   -- --------------------------------------------------------------------------
   function System_Files return Filter_List is (System_Dir);
   function Ignore_List  return Filter_List is (Ignored);

   -- --------------------------------------------------------------------------
   function Initial_Directory return String is (WD.all);

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
     (if Is_Smkfile_Name_Set then
         Strace_Outfile_Prefix & Ada.Directories.Simple_Name (Smkfile_Name) &
        Strace_Outfile_Suffix
      else "");

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
