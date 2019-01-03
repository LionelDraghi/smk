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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

private package Smk.Definitions is
   -- --------------------------------------------------------------------------
   -- Purpose:
   --   This package defines a "Run File" and it's storage.

   -- --------------------------------------------------------------------------
   type Section_Names is private;
   function "+" (Section : Section_Names) return String;
   function "+" (Section : String) return Section_Names;

   Default_Section : constant Section_Names;

   -- --------------------------------------------------------------------------
   type Command_Lines is private;
   function "+" (Command : Command_Lines) return String;
   function "+" (Command : String) return Command_Lines;
   function "<" (Left, Right : Command_Lines) return Boolean;
   function "&" (Left : Command_Lines; Right : String) return Command_Lines;
   Null_Command_Line : constant Command_Lines;

private
   -- --------------------------------------------------------------------------
   type Section_Names is new Unbounded_String;
   Default_Section : constant Section_Names
     := Section_Names (Null_Unbounded_String);

   -- --------------------------------------------------------------------------
   type Command_Lines is new Unbounded_String;
   Null_Command_Line : constant Command_Lines
     := Command_Lines (Null_Unbounded_String);

end Smk.Definitions;
