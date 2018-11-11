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
-- Package: Smk.Cmd_Line body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
--
-- -----------------------------------------------------------------------------

with Smk.IO;
with Smk.Settings;

with Ada.Command_Line;
with Ada.Directories;

separate (Smk.Main)

procedure Analyze_Cmd_Line is

   -- --------------------------------------------------------------------------
   Arg_Counter   : Positive := 1;

   -- --------------------------------------------------------------------------
   -- Procedure: Next_Arg
   -- --------------------------------------------------------------------------
   procedure Next_Arg is
   begin
      Arg_Counter := Arg_Counter + 1;
   end Next_Arg;

   -- --------------------------------------------------------------------------
   -- Procedure: Put_Version
   -- --------------------------------------------------------------------------
   procedure Put_Version is
      use Smk.IO;
   begin
      Put_Line (Settings.Smk_Version);
   end Put_Version;


   -- --------------------------------------------------------------------------
   -- Procedure: Analyze_Cmd_Line
   -- --------------------------------------------------------------------------
   use Smk.IO;

begin
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Help;
      return;
   end if;

   while Arg_Counter <= Ada.Command_Line.Argument_Count loop
      declare
         Opt : constant String := Ada.Command_Line.Argument (Arg_Counter);
      begin
         if Opt = "--version" then
            Put_Version;
            Next_Arg;

         elsif Opt = "-h" or Opt = "--help" then
            Put_Help;
            Next_Arg;

         elsif Opt = "-q" or Opt = "--quiet" then
            Settings.Verbosity := Quiet;
            Next_Arg;

         elsif Opt = "-We" or Opt = "--Warnings=error" then
            Settings.Warnings_As_Errors := True;
            Next_Arg;

         elsif Opt = "-v" or Opt = "--verbose" then
            Settings.Verbosity := Verbose;
            Next_Arg;

         elsif Opt = "-d" then
            -- undocumented option
            Settings.Verbosity := Debug;
            Next_Arg;

         elsif Ada.Directories.Exists (Opt) then
            -- should be the Makefile
            Settings.Set_Makefile_Name (Opt);
            Next_Arg;

         else
            Put_Error ("Unknown Makefile or unknow option "
                       & Opt, With_Help => False);

         end if;

         if Some_Error then return; end if;
         -- No need to further analyze command line, or to do
         -- Options_Coherency_Tests.
      end;

   end loop;

   -- Options_Coherency_Tests;

end Analyze_Cmd_Line;
