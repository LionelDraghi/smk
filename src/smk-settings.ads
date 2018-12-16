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

-- -----------------------------------------------------------------------------
-- Package: smk.Settings specification
--
-- Purpose:
--   This package manages global settings, hard coded or from cmd line.
--
-- Effects:
--
-- Performance:
--
-- -----------------------------------------------------------------------------

private package Smk.Settings is

   Smk_Version : constant String := "0.2.0";

   -- --------------------------------------------------------------------------
   Always_Make        : Boolean := False;
   Explain            : Boolean := False;
   Dry_Run            : Boolean := False;
   Keep_Going         : Boolean := False;
   Ignore_Errors      : Boolean := False;
   Recursive          : Boolean := False;
   Warnings_As_Errors : Boolean := False;
   Create_Template    : Boolean := False;
   Filter_Sytem_Files : Boolean := True;

   type Commands is (Read_Smkfile,
                     Read_Run_Status,
                     List_Previous_Runs,
                     List_Sources,
                     List_Targets,
                     Clean_Targets,
                     Reset_Smk_Files,
                     Version,
                     Build,
                     Help,
                     None) with Default_Value => None;
   Command : Commands;

   -- --------------------------------------------------------------------------
   Smk_File_Prefix       : constant String := ".smk."; -- used for all Smk files
   Strace_Outfile_Prefix : constant String := "/tmp/";
   Strace_Outfile_Suffix : constant String := ".strace_output";
   Shell_Cmd             : constant String := "/bin/sh";
   Shell_Opt             : constant String := "-c ";
   -- don't put a space before -c!
   Strace_Cmd            : constant String
     := "/usr/bin/strace\ -y\ -q\ -qq\ -f\ -e\ trace=file\ -o\ ";
   -- Strace_Cmd : constant String := "/usr/bin/strace";
   -- Strace_Opt : constant String := "-y -q -qq -f -e trace=file -o ";
   -- -y  : print paths associated with file descriptor arguments (between <>)
   -- -q  : suppress messages about attaching, detaching, etc.
   -- -qq : suppress messages about process exit status.
   -- -f  : follow forks

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

   -- -------------------------------------------------------------------------
   -- Smkfile_Name = "../hello.c/Makefile.txt"
   -- Runfile_Name = ".smk.Makefile.txt"
   --    that is Runfile_Name = "Prefix + Simple_Name (Smkfile_Name)"
   procedure Set_Smkfile_Name (Name : in String);
   procedure Set_Runfile_Name (Name : in String);
   function To_Runfile_Name (Smkfile_Name : in String) return String;
   function Smkfile_Name        return String;
   function Runfile_Name        return String;
   function Run_Dir_Name        return String;
   function Strace_Outfile_Name return String;

   -- --------------------------------------------------------------------------
   function Is_System_File (File_Name : in String) return Boolean;
   -- return True if the string starts with "/usr/, "/lib/", etc.

end Smk.Settings;
