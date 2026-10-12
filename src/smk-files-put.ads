-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

with Smk.Files.File_Lists;

procedure Smk.Files.Put (File_List     : File_Lists.Map;
                         Prefix        : String  := "";
                         Print_Sources : Boolean := False;
                         Print_Targets : Boolean := False;
                         Print_Unused  : Boolean := False);
-- Print the files matching one of the booleans, one per line.
-- If Settings.Filter_System_Files, then ignore
-- /lib, /usr, /etc, /opt etc. files
