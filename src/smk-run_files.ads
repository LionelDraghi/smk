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

private package Smk.Run_Files is

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

   Default_Section : constant Section_Names
     := Section_Names (Null_Unbounded_String);

   -- --------------------------------------------------------------------------
   type File_Name is new Unbounded_String;
   function "+" (Name : File_Name) return String;
   function "+" (Name : String) return File_Name;
   function "+" (Name : File_Name) return Unbounded_String;

   -- --------------------------------------------------------------------------
   -- type File_Type is record
   --    Time_Tag : Ada.Calendar.Time;
   -- end record;
   use type Ada.Calendar.Time;
   package File_Lists is
     new Ada.Containers.Ordered_Maps (Key_Type     => File_Name,
                                      Element_Type => Ada.Calendar.Time);

   -- --------------------------------------------------------------------------
   procedure Dump (File_List          : in File_Lists.Map;
                   Filter_Sytem_Files : in Boolean);
   -- Dump files in a one per line bulleted way
   -- if Filter_System_Files, then ignore /lib /usr /etc /opt files

   -- --------------------------------------------------------------------------
   procedure Update_Time_Tag (File_List : in out File_Lists.Map);
   -- updates time_tag of each file according to the current file system state

   -- --------------------------------------------------------------------------
   type Run is record
      Section  : Section_Names := Default_Section;
      Run_Time : Ada.Calendar.Time;
      Sources  : File_Lists.Map := File_Lists.Empty_Map;
      Targets  : File_Lists.Map := File_Lists.Empty_Map;
   end record;
   package Run_Lists is
     new Ada.Containers.Ordered_Maps (Key_Type     => Command_Lines,
                                      Element_Type => Run);

   -- --------------------------------------------------------------------------
   procedure Insert_Or_Update (The_Command     : in     Command_Lines;
                               The_Run         : in     Run;
                               In_Run_List     : in out Run_Lists.Map);

   -- --------------------------------------------------------------------------
   procedure Dump (Run_List           : in Run_Lists.Map;
                   Filter_Sytem_Files : in Boolean);
   -- Dump each run with the format :
   --
   -- Time_Tag Command
   --    Sources:
   --       Time_Tag file1
   --       Time_Tag file2
   --       ...
   --    Targets:
   --       Time_Tag file1
   --       Time_Tag file2
   --       ...

   -- --------------------------------------------------------------------------
   -- Run storage management
   -- --------------------------------------------------------------------------
   function Saved_Run_Found return Boolean;
   function Get_Saved_Run return Run_Lists.Map;
   procedure Save_Run (The_Run : in Run_Lists.Map);

end Smk.Run_Files;
