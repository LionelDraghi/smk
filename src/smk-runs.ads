-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
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
