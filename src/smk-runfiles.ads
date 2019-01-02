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

with Smk.Definitions;      use Smk.Definitions;
with Smk.Files;            use Smk.Files;
with Smk.Files.File_Lists;

with Ada.Calendar;
with Ada.Containers.Ordered_Maps;

private package Smk.Runfiles is
   -- --------------------------------------------------------------------------
   -- Purpose:
   --   This package defines a "Run File" and it's storage.

   -- --------------------------------------------------------------------------
   type Counts is array (Boolean) of Natural;
   -- The boolean semantics is Is_System
   -- So there is 2 counters:
   -- - ordinary files
   -- - system   files
   type File_Counts is record
      Sources : Counts;
      Targets : Counts;
   end record;
   -- Fixme: File_Counts should be encapsulated within File_Lists
   function Sources_Count (Counts            : File_Counts;
                           With_System_Files : Boolean := False) return Natural;
   function Targets_Count (Counts            : File_Counts;
                           With_System_Files : Boolean := False) return Natural;

   function Count_Image  (Count  : Natural)     return String;
   function Counts_Image (Counts : File_Counts) return String;

   -- --------------------------------------------------------------------------
   type Run is record
      Section  : Section_Names  := Default_Section;
      Run_Time : Ada.Calendar.Time;
      Files    : File_Lists.Map := File_Lists.Empty_Map;
      Dirs     : File_Lists.Map := File_Lists.Empty_Map;
      Counts   : File_Counts;
   end record;
   package Run_Lists is
     new Ada.Containers.Ordered_Maps (Key_Type     => Command_Lines,
                                      Element_Type => Run);
   -- --------------------------------------------------------------------------
   type Runfile is record
      Smkfile_Name : Files.File_Name;
      Run_List     : Run_Lists.Map;
   end record;


   -- --------------------------------------------------------------------------
   -- Run manipulation
   -- --------------------------------------------------------------------------

   -- --------------------------------------------------------------------------
   procedure Insert_Or_Update (The_Command : in     Command_Lines;
                               The_Run     : in     Run;
                               In_Run_List : in out Run_Lists.Map);

   -- --------------------------------------------------------------------------
   procedure Update_Files_Status (File_List    : in out File_Lists.Map;
                                  Updated_List : in out File_Lists.Map);
   procedure Update_Dirs_Status (The_Run      : in out Run;
                                 Updated_List : in out File_Lists.Map);

   -- --------------------------------------------------------------------------
   procedure Delete_Targets (The_Runfile : in Runfile);
   -- remove all target files (to mimic a "make clean")

   -- --------------------------------------------------------------------------
   function Has_Target (The_Run_List : Run_Lists.Map;
                        Target       : String) return Boolean;
   -- Return true if Target match the right part of one of the target
   -- file name

   -- --------------------------------------------------------------------------
   procedure Reset (Counts : in out Runfiles.File_Counts);
   procedure Update_Counts (File_list : in     Files.File_Lists.Map;
                            Counts    : in out Runfiles.File_Counts);

   -- --------------------------------------------------------------------------
   -- Listing operations
   -- --------------------------------------------------------------------------

   -- --------------------------------------------------------------------------
   procedure Put_Updated (File_List : in File_Lists.Map);
   -- List files updated or missing since last run

   -- --------------------------------------------------------------------------
   procedure Put_Files (The_Runfile   : Runfile;
                        Print_Sources : Boolean := False;
                        Print_Targets : Boolean := False);
   -- List each dependeny with the format :
   -- [section]Command:file name --Fixme: to be updated
   -- ...
   -- Note that system files and directories are ignored.
   --
   -- Sources and Targets are printed according to the Booleans.
   -- Setting both to False will print unused files.

   -- --------------------------------------------------------------------------
   procedure Put_Run (Run_List : in Run_Lists.Map);
   --
   -- Print each run with the format :
   --
   -- Time_Tag [Section] Command (X source(s), Y target(s))
   -- For example:
   -- 2018-12-23 01:09:37.77 [main.o] gcc -c main.c (2 source(s), 1 target(s))
   --
   -- If --long-listing is used, sources and targets will also be listed
   -- Time_Tag [Section] Command
   --    Sources (Sources count) :
   --       Time_Tag file1
   --       Time_Tag file2
   --       ...
   --    Targets (Target count):
   --       Time_Tag file1
   --       Time_Tag file2
   --       ...

   -- --------------------------------------------------------------------------
   procedure Dump (The_Runfile : Runfile);
   -- Raw dump of all info

   -- --------------------------------------------------------------------------
   -- Run storage management
   -- --------------------------------------------------------------------------
   function Runfiles_Found return Boolean;
   function Get_Saved_Run (Runfile_Name : in File_Name) return Runfile
     with Pre => Runfiles_Found;

   function Load_Runfile return Runfile;
   -- Get_Saved_Run or create a new runfile if none saved

   procedure Save_Run (The_Run : in Runfile);
   procedure Clean_Run_Files;

   -- --------------------------------------------------------------------------
   function Get_Run_List return File_Lists.Map;
   procedure Put_Run_List;

end Smk.Runfiles;
