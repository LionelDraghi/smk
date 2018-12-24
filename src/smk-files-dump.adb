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

with Smk.IO;
with Smk.Settings;

procedure Smk.Files.Dump (File_List : in File_Lists.Map) is
   use File_Lists;
begin
   for F in File_List.Iterate loop
      declare
         Name : constant String            := +Key (F);
         TT   : constant Ada.Calendar.Time := Element (F).Time_Tag;
      begin
         if not (Settings.Filter_Sytem_Files and Element (F).Is_System) then
            IO.Put_Line ("  - " & IO.Image (TT) & ":" & Name);
         end if;
      end;
   end loop;
end Smk.Files.Dump;
