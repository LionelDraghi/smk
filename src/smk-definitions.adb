-- -----------------------------------------------------------------------------
-- smk, the smart make (https://github.com/LionelDraghi/smk)
-- SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
-- SPDX-License-Identifier: Apache-2.0
-- -----------------------------------------------------------------------------

package body Smk.Definitions is

   -- --------------------------------------------------------------------------
   function "+" (Section : Section_Names) return String is
     (To_String (Section));
   function "+" (Section : String) return Section_Names is
     (To_Unbounded_String (Section));

   -- --------------------------------------------------------------------------
   function "+" (Command : Command_Lines) return String is
     (To_String (Command));
   function "+" (Command : String) return Command_Lines is
     (To_Unbounded_String (Command));
   function "<" (Left, Right : Command_Lines) return Boolean is
     (+Left < +Right);
   function "&" (Left : Command_Lines; Right : String) return Command_Lines is
      (Left & (+Right));

end Smk.Definitions;
