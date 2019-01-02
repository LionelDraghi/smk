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

with Smk.Files.File_Lists;

procedure Smk.Files.Put (File_List     : File_Lists.Map;
                         Prefix        : String  := "";
                         Print_Sources : Boolean := False;
                         Print_Targets : Boolean := False);
-- Print files matching one of the boolean, in a one per line way.
-- If Settings.Filter_System_Files, then ignore
-- /lib /usr /etc /opt etc. files
-- NB : it's a OR between Sources and Targets, meaning that if Print_Sources
-- is set, files that are Source or Both will be selected.
