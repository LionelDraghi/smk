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

-- -----------------------------------------------------------------------------
-- Purpose:
--   This package manages global settings, hard coded or from cmd line.
-- -----------------------------------------------------------------------------

private package Smk.Settings is

   Smk_Version : constant String := "0.4.0";

   -- --------------------------------------------------------------------------
   Build_Missing_Targets : Boolean := False;
   Always_Make           : Boolean := False;
   Explain               : Boolean := False;
   Dry_Run               : Boolean := False;
   Keep_Going            : Boolean := False;
   Ignore_Errors         : Boolean := False;
   Long_Listing_Format   : Boolean := False;
   Warnings_As_Errors    : Boolean := False;
   Shorten_File_Names    : Boolean := True;
   Filter_Sytem_Files    : Boolean := True;

   type Commands is (Build,
                     Read_Smkfile,
                     Status,
                     Whatsnew,
                     List_Previous_Runs,
                     List_Sources,
                     List_Targets,
                     List_Unused,
                     Clean,
                     Reset,
                     Version,
                     Help,
                     Add,
                     Run,
                     Dump,
                     None) with Default_Value => None;
   Current_Command : Commands;

   -- --------------------------------------------------------------------------
   Smk_File_Prefix       : constant String := ".smk.";
   Strace_Outfile_Prefix : constant String := "/tmp/";
   Strace_Outfile_Suffix : constant String := ".strace_output";
   Default_Smkfile_Name  : constant String := "default.smk";
   Shell_Cmd             : constant String := "/bin/sh";
   Shell_Opt             : constant String := "-c "; -- no space before -c!
   Strace_Cmd            : constant String
     := "/usr/bin/strace -y -q -qq -f -s 100 -e trace=file -o ";
   -- -y  : print paths associated with file descriptor arguments (between <>)
   -- -q  : suppress messages about attaching, detaching, etc.
   -- -qq : suppress messages about process exit status.
   -- -f  : follow forks
   -- -s  : maximum string size to print (the default is 32)
   --       Filenames are not considered strings and are always printed in full.

   -- --------------------------------------------------------------------------
   function Initial_Directory return String;
   -- returns Ada.Directories.Current_Directory at smk launch.

   -- --------------------------------------------------------------------------
   type Print_Out_Level is (Debug, Verbose, Normal, Quiet);
   -- default: Normal messages are displayed, verbose messages are not
   --          displayed.
   -- quiet:   Neither normal messages nor verbose messages are displayed.
   --          This mode can be achieved using option --quiet.
   -- verbose: Both normal messages and verbose messages are displayed.
   --          This mode can be achieved using option --verbose.
   Verbosity : Print_Out_Level := Normal;

   -- --------------------------------------------------------------------------
   function Debug_Mode return Boolean is (Verbosity = Debug);

   -- --------------------------------------------------------------------------
   function Is_System      (File_Name : in String) return Boolean;
   function In_Ignore_List (File_Name : in String) return Boolean;
   -- Return True for files like /etc/ld.so.cache that are updated
   -- on each execution, or like /dev/* that are not "normal" files
   -- in a build context.

   type Filter_List is array (Positive range <>) of access String;
   function System_Files return Filter_List;
   function Ignore_List  return Filter_List;

   -- --------------------------------------------------------------------------
   -- Smkfile_Name = "../hello.c/Makefile.txt"
   -- Runfile_Name = ".smk.Makefile.txt"
   --    that is Runfile_Name = "Prefix + Simple_Name (Smkfile_Name)"
   procedure Set_Smkfile_Name (Name : in String);
   function Smkfile_Name return String;
   -- Smkfile_Name returns "" if Smkfile has not been set
   function Is_Smkfile_Name_Set return Boolean;
   function Run_Dir_Name return String;
   function Strace_Outfile_Name return String;

   -- --------------------------------------------------------------------------
   procedure Set_Runfile_Name (Name : in String);
   function Runfile_Name return String;
   function To_Runfile_Name (Smkfile_Name : in String) return String;

   -- --------------------------------------------------------------------------
   procedure Set_Section_Name (Name : in String);
   function Section_Name return String;

   -- --------------------------------------------------------------------------
   procedure Add_To_Command_Line (Text : in String);
   function Command_Line return String;

   -- --------------------------------------------------------------------------
   procedure Set_Target_Name (Target : in String);
   function Target_Name return String;

end Smk.Settings;
