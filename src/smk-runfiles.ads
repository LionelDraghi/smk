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
-- Package: Smk.Run_Files specification
--
-- Purpose:
--   This package defines a "Run File" and it's storage.
--
-- Effects:
--
-- Performance:
--
-- -----------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

private package Smk.Runfiles is

   -- --------------------------------------------------------------------------
   type Section_Names is new Unbounded_String;
   type Command_Lines is new Unbounded_String;

   function "+" (Section : Section_Names) return String is
     (To_String (Section));
   function "+" (Command : Command_Lines) return String is
     (To_String (Command));
   function "+" (Section : String) return Section_Names is
     (To_Unbounded_String (Section));
   function "+" (Command : String) return Command_Lines is
     (To_Unbounded_String (Command));

   Null_Command_Line : constant Command_Lines
     := Command_Lines (Null_Unbounded_String);

   Default_Section : constant Section_Names
     := Section_Names (Null_Unbounded_String);

   -- --------------------------------------------------------------------------
   type File_Name is new Unbounded_String;
   function "+" (Name : File_Name) return String;
   function "+" (Name : String)    return File_Name;
   function "+" (Name : File_Name) return Unbounded_String;

   -- --------------------------------------------------------------------------
   type File_Type is record
      Time_Tag  : Ada.Calendar.Time;
      Is_System : Boolean;
   end record;
   use type Ada.Calendar.Time;
   package File_Lists is
     new Ada.Containers.Ordered_Maps (Key_Type     => File_Name,
                                      Element_Type => File_Type);

   -- --------------------------------------------------------------------------
   procedure Dump (File_List : in File_Lists.Map);
   -- Dump files in a one per line bulleted way.
   -- If Settings.Filter_System_Files, then ignore
   -- /lib /usr /etc /opt etc. files

   -- --------------------------------------------------------------------------
   type Run is record
      Section                  : Section_Names  := Default_Section;
      Run_Time                 : Ada.Calendar.Time;
      Sources                  : File_Lists.Map := File_Lists.Empty_Map;
      Source_System_File_Count : Natural        := 0;
      Targets                  : File_Lists.Map := File_Lists.Empty_Map;
      Target_System_File_Count : Natural        := 0;
   end record;
   package Run_Lists is
     new Ada.Containers.Ordered_Maps (Key_Type     => Command_Lines,
                                      Element_Type => Run);
   -- --------------------------------------------------------------------------
   type Runfile is record
      Smkfile_Name : Unbounded_String;
      Run_List     : Run_Lists.Map;
   end record;

   -- --------------------------------------------------------------------------
   procedure Insert_Or_Update (The_Command : in     Command_Lines;
                               The_Run     : in     Run;
                               In_Run_List : in out Run_Lists.Map);

   -- --------------------------------------------------------------------------
   procedure Dump (Run_List : in Run_Lists.Map);
   -- Dump each run with the format :
   --
   -- Time_Tag Command
   --    Sources (Sources count) :
   --       Time_Tag file1
   --       Time_Tag file2
   --       ...
   --    Targets (Target count):
   --       Time_Tag file1
   --       Time_Tag file2
   --       ...

   -- --------------------------------------------------------------------------
   -- procedure List_Dependencies (Run_List : in Run_Lists.Map);
   -- List each dependency with the format :
   --
   -- Time_Tag Command
   --    Sources (Sources count) :
   --       Time_Tag file1
   --       Time_Tag file2
   --       ...

   -- --------------------------------------------------------------------------
   procedure List_Sources (The_Runfile : in Runfile);
   -- List each dependeny with the format :
   -- [section]Command:source
   -- ...

   -- --------------------------------------------------------------------------
   procedure List_Targets (The_Runfile : in Runfile);
   -- List each dependeny with the format :
   -- [section]Command:target
   -- ...

   -- --------------------------------------------------------------------------
   procedure Delete_Targets (The_Runfile : in Runfile);
   -- remove all target files (to mimic a "make clean")

   -- --------------------------------------------------------------------------
   function Has_Target (The_Run_List : Run_Lists.Map;
                        Target       : String) return Boolean;
   -- return true if Target match the right part of one of the target
   -- file name

   -- --------------------------------------------------------------------------
   -- Run storage management
   -- --------------------------------------------------------------------------
   function Runfiles_Found return Boolean;
   function Get_Saved_Run (Runfile_Name : in String) return Runfile;

   function Load_Runfile return Runfile;
   -- Get_Saved_Run or create a new runfile if none saved

   procedure Save_Run (The_Run : in Runfile);
   procedure Clean_Run_Files;

   -- --------------------------------------------------------------------------
   function Get_Run_List return File_Lists.Map;
   procedure Put_Run_List;

end Smk.Runfiles;
