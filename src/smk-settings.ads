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

   Smk_Version : constant String := "0.0.1";

   Recursive          : Boolean := False;
   Warnings_As_Errors : Boolean := False;
   Create_Template    : Boolean := False;

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
   function Makefile_Name return String;

end Smk.Settings;
