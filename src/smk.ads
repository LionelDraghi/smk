-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Package: smk specification
--
-- Purpose:
--    This package is empty. Child units do the real job:
--
--    procedure <Smk.Main> - is in charge of
--       controlling the execution flow according to the command line
--    package <Smk.Cmd_Line> - does the command line analysis
--    package <Smk.Settings> - global settings, resulting mainly from
--       cmd line analysis (and env. variables in the future)
--    ...
--
-- Effects:
--
-- Limitations:
--
-- Performance:
--
-- -----------------------------------------------------------------------------

package Smk is

   pragma Pure;

end Smk;
