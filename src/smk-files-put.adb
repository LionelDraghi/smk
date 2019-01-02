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

with Smk.Settings;

procedure Smk.Files.Put (File_List     : File_Lists.Map;
                         Prefix        : String  := "";
                         Print_Sources : Boolean := False;
                         Print_Targets : Boolean := False) is
   use File_Lists;
   use Smk.Settings;
begin
   for F in File_List.Iterate loop
      if not (Settings.Filter_Sytem_Files and Is_System (Element (F)))
      then
         if (Print_Sources and Is_Source (File_List (F)))
           or else (Print_Targets and Is_Target (File_List (F)))
         then
            Put_File_Description (Name   => Key (F),
                                  File   => Element (F),
                                  Prefix => Prefix);
         end if;
      end if;
   end loop;
end Smk.Files.Put;
