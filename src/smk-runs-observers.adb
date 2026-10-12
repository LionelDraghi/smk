-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
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

with Smk.Runs.Strace_Analyzer;

package body Smk.Runs.Observers is

   -- --------------------------------------------------------------------------
   procedure Observe (Cmd     : in     Command_Lines;
                      Events  :    out Operation_Lists.List;
                      Success :    out Boolean) is
   begin
      -- Linux backend (transitional): the command is spawned under
      -- strace, and the strace output file is analyzed to produce the
      -- events. This backend will be replaced by the direct ptrace
      -- tracer (design discussion D1).
      Strace_Analyzer.Observe (Cmd     => Cmd,
                               Events  => Events,
                               Success => Success);
   end Observe;

end Smk.Runs.Observers;
