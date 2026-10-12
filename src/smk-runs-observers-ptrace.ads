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

private package Smk.Runs.Observers.Ptrace is

   -- --------------------------------------------------------------------------
   -- Linux backend of the Observers abstraction layer: a direct ptrace
   -- tracer (design discussion D1).
   -- The command is forked under PTRACE_TRACEME, after installing a
   -- seccomp BPF filter that traps the subscribed file syscalls only,
   -- then the tracer follows the command and all its children, and
   -- turns the successful file operations into events, with no
   -- intermediate text trace.
   -- Requires Linux >= 5.3 (PTRACE_GET_SYSCALL_INFO); the syscall
   -- numbers are x86_64 for now.

   procedure Observe (Cmd     : in     Command_Lines;
                      Events  :    out Operation_Lists.List;
                      Success :    out Boolean);
   -- Same semantics as Observers.Observe.

end Smk.Runs.Observers.Ptrace;
