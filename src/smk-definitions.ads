-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

private package Smk.Definitions is
   -- --------------------------------------------------------------------------
   -- Purpose:
   --   This package defines a "Run File" and its storage.

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
