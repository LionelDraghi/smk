-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

with Ada.Containers.Ordered_Maps;

package Smk.Files.File_Lists is
  new Ada.Containers.Ordered_Maps (Key_Type     => File_Name,
                                   Element_Type => File_Type);
