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

with Smk.Smkfiles;
with Smk.Runfiles;

private package Smk.Runs is

   -- --------------------------------------------------------------------------
   procedure Run_All (The_Smkfile   : in out Smkfiles.Smkfile;
                      The_Run_List  : in out Runfiles.Run_Lists.Map;
                      Cmd_To_Run    :    out Boolean;
                      Error_In_Run  :    out Boolean;
                      Section_Found :    out Boolean;
                      Target_Found  :    out Boolean);

end Smk.Runs;
