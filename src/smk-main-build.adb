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

with Smk.Runs;

separate (Smk.Main)

-- -----------------------------------------------------------------------------
procedure Build is
   Smkfile       : Smkfiles.Smkfile;
   Runfile       : Runfiles.Runfile;
   Cmd_To_Run    : Boolean;
   Error_In_Run  : Boolean;
   Section_Found : Boolean;
   Target_Found  : Boolean;
   use Smk.Smkfiles;

begin
   Smkfile := Smkfiles.Load_Smkfile;

   Runfile := Runfiles.Load_Runfile;

   Runs.Run_All (Smkfile,
                 Runfile.Run_List,
                 Cmd_To_Run,
                 Error_In_Run,
                 Section_Found,
                 Target_Found);

   if Settings.Section_Name /= "" and not Section_Found then
      IO.Put_Line ("No section """ & Settings.Section_Name &
                     """ in " & (+Smkfile.Name));

   elsif Settings.Target_Name /= "" and not Target_Found then
      IO.Put_Line
        ("Target """ & Settings.Target_Name & """ not found");
      IO.Put_Line
        ("run ""smk list-targets"" to get a list of possible target");

   elsif not Cmd_To_Run then
      IO.Put_Line ("Nothing to run");

   end if;

   if Error_In_Run and not Ignore_Errors then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   Runfiles.Save_Run (Runfile);

end Build;
