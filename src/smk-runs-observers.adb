-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

with Smk.Runs.Observers.Ptrace;

package body Smk.Runs.Observers is

   -- --------------------------------------------------------------------------
   procedure Observe (Cmd     : in     Command_Lines;
                      Events  :    out Operation_Lists.List;
                      Success :    out Boolean) is
   begin
      -- Linux backend: a direct ptrace tracer (design discussion D1).
      Ptrace.Observe (Cmd     => Cmd,
                      Events  => Events,
                      Success => Success);
   end Observe;

end Smk.Runs.Observers;
