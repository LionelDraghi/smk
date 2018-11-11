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
-- Package: smk.Run_File specification
--
-- Purpose:
--   This package manages global settings, hard coded or from cmd line.
--
-- Effects:
--
-- Performance:
--
-- -----------------------------------------------------------------------------

private package Smk.Run_File is

   -- --------------------------------------------------------------------------
   procedure Initialize;

   -- --------------------------------------------------------------------------
   -- Function: First_Run
   -- Purpose:
   --   Return True if there is no previous run file
   -- --------------------------------------------------------------------------
   function First_Run return Boolean;

   -- --------------------------------------------------------------------------
   -- Function: Run_All
   -- Purpose:
   --   Run all command in Makefile
   -- --------------------------------------------------------------------------
   procedure Run_All;

   -- --------------------------------------------------------------------------
   -- Function: Run_All
   -- Purpose:
   --   Replace the previous run file with the current one.
   -- --------------------------------------------------------------------------
   procedure Update_Run_File;

end Smk.Run_File;
