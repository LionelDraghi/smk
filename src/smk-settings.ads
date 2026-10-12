-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Purpose:
--   This package manages global settings, hard coded or from cmd line.
-- -----------------------------------------------------------------------------

with Smk_Config;

private package Smk.Settings is

   Smk_Version : constant String := Smk_Config.Crate_Version;

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
   Default_Smkfile_Name  : constant String := "default.smk";
   Shell_Cmd             : constant String := "/bin/sh";
   -- The commands are run through this shell, and observed by
   -- Smk.Runs.Observers.Ptrace.

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

   type String_Access is access String;
   type Filter_List is array (Positive range <>) of String_Access;
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
