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

   Smk_Version : constant String := "0.0.2";

   Always_Make        : Boolean := False;
   Explain            : Boolean := False;
   Dry_Run            : Boolean := False;
   Ignore_Errors      : Boolean := False;
   List_Makefile      : Boolean := False;
   List_Saved_Run     : Boolean := False;
   List_Targets       : Boolean := False;
   Recursive          : Boolean := False;
   Warnings_As_Errors : Boolean := False;
   Create_Template    : Boolean := False;
   Clean_Smk_Files    : Boolean := False;

   -- -------------------------------------------------------------------------
   Smk_File_Prefix       : constant String := ".smk."; -- used for all Smk files
   Strace_Outfile_Suffix : constant String := "strace_output";
   Strace_Cmd            : constant String := "/usr/bin/strace";
   Strace_Opt            : constant String := "-y -q -qq -f -e trace=%file -o ";
   -- -y  : print paths associated with file descriptor arguments (between <>)
   -- -q  : suppress messages about attaching, detaching, etc.
   -- -qq : suppress messages about process exit status.
   -- -f  : follow forks

   -- -------------------------------------------------------------------------
   type Print_Out_Level is (Debug, Verbose, Normal, Quiet);
   -- default: Normal messages are displayed, verbose messages are not
   --          displayed.
   -- quiet:   Neither normal messages nor verbose messages are displayed.
   --          This mode can be achieved using option --quiet.
   -- verbose: Both normal messages and verbose messages are displayed.
   --          This mode can be achieved using option --verbose.
   Verbosity : Print_Out_Level := Normal;

   -- -------------------------------------------------------------------------
   -- Function: Debug_Mode
   -- -------------------------------------------------------------------------
   function Debug_Mode return Boolean is (Verbosity = Debug);

   -- -------------------------------------------------------------------------
   -- Procedure: Set_Makefile_Name
   -- -------------------------------------------------------------------------
   procedure Set_Makefile_Name (Name : in String);

   -- -------------------------------------------------------------------------
   -- Function: Makefile_Name
   -- -------------------------------------------------------------------------
   function Makefile_Name          return String;
   function Previous_Run_File_Name return String;
   function Run_Dir_Name           return String;
   function Strace_Outfile_Name    return String;

end Smk.Settings;
