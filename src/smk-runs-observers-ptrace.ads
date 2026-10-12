-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
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
