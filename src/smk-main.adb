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
-- Procedure: Smk.Main body
--
-- Implementation Notes:
--
-- Portability Issues:
--
-- Anticipated Changes:
-- -----------------------------------------------------------------------------

with Smk.IO;
with Smk.Run_File;
with Smk.Settings;

with Ada.Command_Line;

procedure Smk.Main is
   procedure Put_Help         is separate;
   procedure Put_Error (Msg       : in String  := "";
                        With_Help : in Boolean := False) is separate;
   procedure Analyze_Cmd_Line is separate;

   Debug : constant Boolean := False;

begin
   Analyze_Cmd_Line;
   if IO.Some_Error then
      -- Some error occurs during command line analisys, stop here.
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   Run_File.Initialize;

   if Run_File.First_Run then
      IO.Put_Debug_Line ("first run", Debug, "");
      Run_File.Run_All;
   else
      IO.Put_Debug_Line ("not the first run", Debug, "");
      -- else, for each command :
      --    if the command is still the same.
      --          else, run the command.
      --    else if the accessed files are identical.
      --          else run the command.
      --    else next command
      --
      -- NB : run the command => build the next run_file

      Run_File.Update_Run_File;
   end if;

   if IO.Some_Error then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

end Smk.Main;
